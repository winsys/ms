{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                 Lexical Scanner Class                  }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZScanner;

interface

uses SysUtils;

const
  { Token types definitions }
  tokUnknown   = $0000;
  tokComment   = $0001;

  tokKeyword   = $0002;
  tokType      = $0004;
  tokIdent     = $0008;
  tokAlpha     = $000E;

  tokOperator  = $0010;
  tokBrace     = $0020;
  tokSeparator = $0040;
  tokEol       = $0080;
  tokLF        = $00E0;
  tokDelim     = $00F0;

  tokInt       = $0100;
  tokFloat     = $0200;
  tokString    = $0400;
  tokBool      = $0800;
  tokConst     = $0F00;

  tokEof       = $8000;

type
  { Abstract scanner class definition }
  TZScanner = class
  protected
    FBuffer: string;
    FBufferPos, FBufferLine, FBufferLen: Integer;
    FTokenType, FNextTokenType: Integer;
    FToken, FNextToken: string;
    FLineNo, FNextLineNo: Integer;
    FPosition, FNextPosition: Integer;
    FShowComment: Boolean;
    FShowString: Boolean;
    FShowEol: Boolean;
    FShowKeyword: Boolean;
    FShowType: Boolean;
    
    procedure SetBuffer(Value: string);
    function GetNextLineNo: Integer;
    function GetNextToken: string;
    function GetNextPosition: Integer;
    function GetNextTokenType: Integer;

    function LowRunLex(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; virtual;
    function RunLex(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; virtual;

    procedure ExtractToken;
    procedure ExtractNextToken;

    function InnerStartLex(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer;
    function InnerProcLineComment(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer;
    function InnerProcCComment(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer;
    function InnerProcIdent(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer;
    function InnerProcCString(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer;
    function InnerProcPasString(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Restart;

    function WrapString(Value: string): string; virtual;
    function UnwrapString(Value: string): string; virtual;

    class function IsAlpha(Value: Char): Boolean; virtual;
    class function IsDigit(Value: Char): Boolean; virtual;
    class function IsDelim(Value: Char): Boolean; virtual;
    class function IsWhite(Value: Char): Boolean; virtual;
    class function IsEol(Value: Char): Boolean; virtual;
    class function IsQuote(Value: Char): Boolean; virtual;

    function Lex: Integer;
    function GotoNextToken: Integer;

    property ShowComment: Boolean read FShowComment write FShowComment;
    property ShowEol: Boolean read FShowEol write FShowEol;
    property ShowString: Boolean read FShowString write FShowString;
    property ShowKeyword: Boolean read FShowKeyword write FShowKeyword;
    property ShowType: Boolean read FShowType write FShowType;

    property Buffer: string read FBuffer write SetBuffer;
    property BufferPos: Integer read FBufferPos;

    property Position: Integer read FPosition;
    property LineNo: Integer read FLineNo;
    property Token: string read FToken;
    property TokenType: Integer read FTokenType;

    property NextPosition: Integer read GetNextPosition;
    property NextLineNo: Integer read GetNextLineNo;
    property NextToken: string read GetNextToken;
    property NextTokenType: Integer read GetNextTokenType;
  end;

  { Pascal-like scanner class definition }
  TZPasScanner = class (TZScanner)
  protected
    function LowRunLex(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; override;
  public
    function WrapString(Value: string): string; override;
    function UnwrapString(Value: string): string; override;
  end;

  { C-like scanner class definition }
  TZCScanner = class (TZScanner)
  protected
    function LowRunLex(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; override;
  public
    function WrapString(Value: string): string; override;
    function UnwrapString(Value: string): string; override;
  end;

implementation

{ TZScanner }

{ Class constructor }
constructor TZScanner.Create;
begin
  FBufferPos := 1;
  FBufferLine := 1;
  FShowKeyword := True;
  FShowType := True;
  FShowString := True;
end;

{ Class destructor }
destructor TZScanner.Destroy;
begin
  inherited Destroy;
end;

{ Set new string buffer }
procedure TZScanner.SetBuffer(Value: string);
begin
  FBuffer := Value;
  FBufferLen := Length(FBuffer);
  FBufferPos := 1;
  FBufferLine := 1;
  FTokenType := tokEof;
  FNextTokenType := tokUnknown;
  FToken := '';
  FNextToken := '';
  FLineNo := 0;
  FNextLineNo := 0;
  FPosition := 0;
  FNextPosition := 0;
end;

{ Get next line no }
function TZScanner.GetNextLineNo: Integer;
begin
  ExtractNextToken;
  Result := FNextLineNo;
end;

{ Get next position }
function TZScanner.GetNextPosition: Integer;
begin
  ExtractNextToken;
  Result := FNextPosition;
end;

{ Get next token value }
function TZScanner.GetNextToken: string;
begin
  ExtractNextToken;
  Result := FNextToken;
end;

{ Get next token type }
function TZScanner.GetNextTokenType: Integer;
begin
  ExtractNextToken;
  Result := FNextTokenType;
end;

{ Convert string value into string }
function TZScanner.WrapString(Value: string): string;
begin
  Result := '"' + Value + '"';
end;

{ Unconvert string into string value }
function TZScanner.UnwrapString(Value: string): string;
var
  Quote: Char;
begin
  Result := Value;
  if Result = '' then Exit;
  { Delete start and end quotes }
  Quote := Result[1];
  if Quote in ['"', ''''] then Delete(Result, 1, 1)
  else Exit;
  if (Result <> '') and (Result[Length(Result)] = Quote) then
    Delete(Result, Length(Result), 1);
end;

{ Extract next token }
procedure TZScanner.ExtractNextToken;
begin
  if (FNextToken = '') and (FNextTokenType = tokUnknown) then
    FNextTokenType := RunLex(FNextPosition, FNextLineNo, FNextToken);
end;

{ Extract current token }
procedure TZScanner.ExtractToken;
begin
  if (FNextToken <> '') and (FNextTokenType <> tokUnknown) then
  begin
    { Move next token to current token }
    FTokenType := FNextTokenType;
    FLineNo := FNextLineNo;
    FPosition := FNextPosition;
    FToken := FNextToken;
    { Clear next token }
    FNextTokenType := tokUnknown;
    FNextLineNo := 0;
    FNextPosition := 0;
    FNextToken := '';
  end else
    FTokenType := RunLex(FPosition, FLineNo, FToken);
end;

{ Get next token alias }
function TZScanner.GotoNextToken: Integer;
begin
  Result := Lex;
end;

{ Get next token }
function TZScanner.Lex: Integer;
begin
  ExtractToken;
  Result := TokenType;
end;

{ Extract token }
function TZScanner.RunLex(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
begin
  { Get current token }
  repeat
    Result := LowRunLex(CurrPos, CurrLineNo, CurrToken);
    if (Result in [tokEol, tokLF]) and FShowEol then
      Break;
    if (Result = tokComment) and FShowComment then
      Break;
  until not (Result in [tokEol, tokLF, tokComment]);
  { Convert string if needed }
  if (Result = tokString) and not ShowString then
    CurrToken := UnwrapString(CurrToken);
end;

{******************** Lexical procedures *********************}

{ Start lexical scaning }
function TZScanner.InnerStartLex(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
begin
  { Initialize values }
  Result := tokEof;
  CurrLineNo := FBufferLine;
  CurrPos := FBufferPos;
  CurrToken := '';
  { Check position }
  if FBufferPos > FBufferLen then Exit;
  { Skip whitespaces }
  while FBuffer[FBufferPos] in [' ',#9] do
  begin
    Inc(FBufferPos);
    if FBufferPos > FBufferLen then
    begin
      CurrPos := FBufferPos;
      Exit;
    end;
  end;
  CurrPos := FBufferPos;
  CurrToken := FBuffer[FBufferPos];
  Inc(FBufferPos);

  { Check for LF }
  if CurrToken[1] = #10 then
  begin
    Result := tokLF;
    Exit;
  end;

  { Check for EOL }
  if CurrToken[1] = #13 then
  begin
    Result := tokEol;
    Inc(FBufferLine);
    Exit;
  end;
  Result := tokUnknown;
end;

{ Process identificator }
function TZScanner.InnerProcIdent(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
var
  Temp: Char;
begin
  Result := tokUnknown;
  if CurrToken[1] in ['0'..'9','.','a'..'z','A'..'Z','_','$'] then
  begin
    Temp := CurrToken[1];
    if Temp = '.' then Result := tokFloat
    else if Temp in ['0'..'9'] then Result := tokInt
    else Result := tokIdent;

    while FBufferPos <= FBufferLen do
    begin
      Temp := FBuffer[FBufferPos];
      if not (Temp in ['0'..'9','.','a'..'z','A'..'Z','_','$']) then
        Break;
      if (Result = tokInt) and (Temp = '.') then
        Result := tokFloat;
      CurrToken := CurrToken + Temp;
      Inc(FBufferPos);
    end;
  end;
end;

{ Process C-like escape string }
function TZScanner.InnerProcCString(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
var
  Temp, Quote: Char;
begin
  Result := tokUnknown;
  if IsQuote(CurrToken[1]) then
  begin
    Result := tokString;
    Quote := CurrToken[1];
    while FBufferPos <= FBufferLen do
    begin
      Temp := FBuffer[FBufferPos];
      CurrToken := CurrToken + Temp;
      Inc(FBufferPos);
      if (Temp = '\') and ((FBufferPos <= FBufferLen)
        and (FBuffer[FBufferPos] = Quote)) then
      begin
        CurrToken := CurrToken + Quote;
        Inc(FBufferPos);
      end
      else if Temp = Quote then
        Break;
    end;
  end
end;

{ Process Pascal-like string }
function TZScanner.InnerProcPasString(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
var
  Temp, Quote: Char;
begin
  Result := tokUnknown;
  if IsQuote(CurrToken[1]) then
  begin
    Result := tokString;
    Quote := CurrToken[1];
    while FBufferPos <= FBufferLen do
    begin
      Temp := FBuffer[FBufferPos];
      CurrToken := CurrToken + Temp;
      Inc(FBufferPos);
      if (Temp = Quote) and ((FBufferPos <= FBufferLen)
        and (FBuffer[FBufferPos] = Quote)) then
      begin
        CurrToken := CurrToken + Quote;
        Inc(FBufferPos);
      end
      else if Temp = Quote then
        Break;
    end;
  end
end;

{ Process C-like multi-line comment }
function TZScanner.InnerProcCComment(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
var
  Temp, Temp1: Char;
begin
  Result := tokUnknown;
  if (CurrToken[1] = '/') and (FBufferPos <= FBufferLen)
    and (FBuffer[FBufferPos] = '*') then
  begin
    Result := tokComment;
    Temp1 := #0;
    while FBufferPos <= FBufferLen do
    begin
      Temp := FBuffer[FBufferPos];
      CurrToken := CurrToken + Temp;
      Inc(FBufferPos);
      if (Temp = '/') and (Temp1 = '*') then
        Break;
      if Temp = #13 then
        Inc(FBufferLine);
      Temp1 := Temp;
    end;
  end
end;

{ Process single-line comment }
function TZScanner.InnerProcLineComment(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
var
  Temp: Char;
begin
  Result := tokComment;
  while FBufferPos <= FBufferLen do
  begin
    Temp := FBuffer[FBufferPos];
    CurrToken := CurrToken + Temp;
    Inc(FBufferPos);
    if Temp = #13 then
    begin
      Inc(FBufferLine);
      Break;
    end;
  end;
end;

{ Get lowlevel token }
function TZScanner.LowRunLex(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
var
  Temp: Char;
  Quote: string;
begin
  Result := InnerStartLex(CurrPos, CurrLineNo, CurrToken);
  if Result <> tokUnknown then Exit;

  Temp := CurrToken[1];
  { Check for brace }
  if Temp in ['(', ')', '{', '}', '[', ']'] then
  begin
    Result := tokBrace;
  end
  { Check for separator }
  else if Temp in [',', ';', ':'] then
  begin
    Result := tokSeparator;
  end
  { Check for delimiters }
  else if IsDelim(Temp) then
  begin
    Result := tokOperator;
  end
  { Check for string }
  else if IsQuote(Temp) then
  begin
    Quote  := Temp;
    Result := tokString;
    while FBufferPos <= FBufferLen do
    begin
      Temp := FBuffer[FBufferPos];
      CurrToken := CurrToken + Temp;
      Inc(FBufferPos);
      if Temp = Quote then
        Break;
    end;
  end
  { Check for digits and identifiers }
  else
  begin
    if IsDigit(Temp) then
      Result := tokInt
    else Result := tokIdent;
    while FBufferPos <= FBufferLen do
    begin
      Temp := FBuffer[FBufferPos];
      if IsDelim(Temp) or (Temp in ['"', '''']) then
        Break;
      if (Result = tokInt) and (Temp = '.') then
        Result := tokFloat;
      CurrToken := CurrToken + Temp;
      Inc(FBufferPos);
    end;
  end;
end;

{ Check is value an alpha }
class function TZScanner.IsAlpha(Value: Char): Boolean;
begin
  Result := not ((Value < ' ') or IsDelim(Value) or IsDigit(Value));
end;

{ Check is value a delimiter }
class function TZScanner.IsDelim(Value: Char): Boolean;
begin
  Result := (Pos(Value, ':;,+-<>/*%^=()[]|&~@#$\`{}!? '#9#10#13) > 0);
end;

{ Check is value a digit }
class function TZScanner.IsDigit(Value: Char): Boolean;
begin
  Result := (Value in ['0'..'9']);
end;

{ Check is value EOL }
class function TZScanner.IsEol(Value: Char): Boolean;
begin
  Result := (Value = #13);
end;

{ Check is value a white space }
class function TZScanner.IsWhite(Value: Char): Boolean;
begin
  Result := (Value in [' ', #9, #10]);
end;

{ Check is value a quote }
class function TZScanner.IsQuote(Value: Char): Boolean;
begin
  Result := (Value in ['"', '''']);
end;

{ Restart lexical analyse }
procedure TZScanner.Restart;
begin
  SetBuffer(FBuffer);
end;

{ TZPasScanner }

const
  MaxPasOp       = 7;
  MaxPasType     = 8;
  MaxPasKeyword  = 33;

  PasOp: array[1..MaxPasOp] of string =
    ('and','or','not','shr','shl','div','mod');
  PasType: array[1..MaxPasType] of string =
    ('integer','longint','byte','char','string','boolean','real','double');
  PasKeyword: array[1..MaxPasKeyword] of string =
    ('with','array','function','case','var','const','until','then','set',
     'record','program','procedure','packed','nil','label','in','repeat',
     'of','goto','forward','for','while','file','else','downto','do','to',
     'type','end','begin','if','true','false');

{ Get lowlevel token }
function TZPasScanner.LowRunLex(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
var
  I: Integer;
  Temp, Temp1: Char;
  Search: string;
begin
  Result := InnerStartLex(CurrPos, CurrLineNo, CurrToken);
  if Result <> tokUnknown then Exit;
  Temp := CurrToken[1];
  { Check for multi-line comment }
  if Temp = '{' then
  begin
    Result := tokComment;
    while FBufferPos <= FBufferLen do
    begin
      Temp := FBuffer[FBufferPos];
      CurrToken := CurrToken + Temp;
      Inc(FBufferPos);
      if Temp = '}' then
        Break;
      if Temp = #13 then
        Inc(FBufferLine);
    end;
  end
  { Check for multi-line comment }
  else if (Temp = '(') and (FBufferPos <= FBufferLen)
    and (FBuffer[FBufferPos] = '*') then
  begin
    Result := tokComment;
    Temp1 := #0;
    while FBufferPos <= FBufferLen do
    begin
      Temp := FBuffer[FBufferPos];
      CurrToken := CurrToken + Temp;
      Inc(FBufferPos);
      if (Temp = ')') and (Temp1 = '*') then
        Break;
      if Temp = #13 then
        Inc(FBufferLine);
      Temp1 := Temp;
    end;
  end;

  { Check for single-line comment }
  if (Temp = '/') and (FBufferPos <= FBufferLen)
    and (FBuffer[FBufferPos] = '/') then
  begin
    Result := InnerProcLineComment(CurrPos, CurrLineNo, CurrToken);
    Exit;
  end;

  { Check for brace }
  if Temp in ['(', ')', '[', ']'] then
  begin
    Result := tokBrace;
  end
  { Check for separator }
  else if (Temp in [',', ';'])
    or ((Temp = ':') and (FBufferPos <= FBufferLen)
      and (FBuffer[FBufferPos] <> '=')) then
  begin
    Result := tokSeparator;
  end
  { Check for delimiters }
  else if Pos(Temp, ':=+-<>/*^@#') > 0 then
  begin
    Result := tokOperator;
    if FBufferPos <= FBufferLen then
      Temp1 := FBuffer[FBufferPos]
    else Temp1 := #0;
    if ((Temp in [':', '>']) and (Temp1 = '='))
      or ((Temp = '<') and (Temp1 in ['=', '>'])) then
    begin
      CurrToken := CurrToken + Temp1;
      Inc(FBufferPos);
    end;
  end;
  if Result <> tokUnknown then Exit;

  { Check for string }
  if Temp = '''' then
  begin
    Result := InnerProcPasString(CurrPos, CurrLineNo, CurrToken);
    Exit;
  end;

  { Check for digits and identifiers }
  Result := InnerProcIdent(CurrPos, CurrLineNo, CurrToken);
  { Check for operators }
  if Result = tokIdent then
  begin
    Search := LowerCase(CurrToken);
    for I := 1 to MaxPasOp do
      if PasOp[I] = Search then
      begin
        Result := tokOperator;
        Exit;
      end;
  end;
  { Check for types }
  if (Result = tokIdent) and ShowType then
  begin
    Search := LowerCase(CurrToken);
    for I := 1 to MaxPasType do
      if PasType[I] = Search then
      begin
        Result := tokType;
        Exit;
      end;
  end;
  { Check for keywords }
  if (Result = tokIdent) and ShowKeyword then
  begin
    Search := LowerCase(CurrToken);
    for I := 1 to MaxPasKeyword do
      if PasKeyword[I] = Search then
      begin
        Result := tokKeyword;
        Exit;
      end;
  end;
end;

{ Unconvert value into string value }
function TZPasScanner.UnwrapString(Value: string): string;
var
  Pos, Len: Integer;
begin
  Result := '';
  if Value = '' then Exit;
  Pos := 1;
  Delete(Value, 1, 1);
  Len := Length(Value);
  while Pos <= Len do
  begin
    if Value[Pos] <> '''' then
      Result := Result + Value[Pos]
    else if (Pos < Len) and (Value[Pos+1] = '''') then
    begin
      Result := Result + '''';
      Inc(Pos);
    end;
    Inc(Pos);
  end;
end;

{ Convert string value into string }
function TZPasScanner.WrapString(Value: string): string;
var
  Pos: Integer;
begin
  Result := '''';
  for Pos := 1 to Length(Value) do
  begin
    Result := Result + Value[Pos];
    if Value[Pos] = '''' then
      Result := Result + Value[Pos];
  end;
  Result := Result + '''';
end;

{ TZCScanner }

const
  MaxCType     = 7;
  MaxCKeyword  = 33;

  CType: array[1..MaxCType] of string =
    ('int','long','short','char','bool','float','double');
  CKeyword: array[1..MaxCKeyword] of string =
    ('with','array','function','case','var','const','until','then','set',
     'record','program','procedure','packed','nil','label','in','repeat',
     'of','goto','forward','for','while','file','else','downto','do','to',
     'type','end','begin','if','true','false');

{ Get lowlevel token }
function TZCScanner.LowRunLex(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
var
  I: Integer;
  Temp, Temp1, Temp2: Char;
  Search: string;
begin
  Result := InnerStartLex(CurrPos, CurrLineNo, CurrToken);
  if Result <> tokUnknown then Exit;

  { Check for multi-line comment }
  Result := InnerProcCComment(CurrPos, CurrLineNo, CurrToken);
  if Result <> tokUnknown then Exit;
  Temp := CurrToken[1];

  { Check for single-line comment }
  if (Temp = '/') and (FBufferPos <= FBufferLen)
    and (FBuffer[FBufferPos] = '/') then
  begin
    Result := InnerProcLineComment(CurrPos, CurrLineNo, CurrToken);
    Exit;
  end;

  { Check for brace }
  if Temp in ['{', '}', '(', ')', '[', ']'] then
  begin
    Result := tokBrace;
  end
  { Check for separator }
  else if (Temp in [',', ';'])
    or ((Temp = ':') and (FBufferPos <= FBufferLen)
      and (FBuffer[FBufferPos] <> '=')) then
  begin
    Result := tokSeparator;
  end
  { Check for delimiters }
  else if Pos(Temp, ':=+-<>/*^@#?%!|&~') > 0 then
  begin
    Result := tokOperator;
    { Check second char }
    if FBufferPos <= FBufferLen then
      Temp1 := FBuffer[FBufferPos]
    else Temp1 := #0;
    if ((Temp in ['+','-','!','|','~','&','*','/','>','%']) and (Temp1 = '='))
      or ((Temp = '<') and (Temp1 in ['=']))
      or ((Temp in ['<','>','|','&','+','-']) and (Temp1 = Temp))
      or ((Temp = '-') and (Temp1 in ['>'])) then
    begin
      CurrToken := CurrToken + Temp1;
      Inc(FBufferPos);
    end;
    { Check third char }
    if (Temp1 <> #0) and (FBufferPos <= FBufferLen) then
      Temp2 := FBuffer[FBufferPos]
    else Temp2 := #0;
    if ((Temp = '>') and (Temp1 = '>') and (Temp2 = '='))
      or ((Temp = '<') and (Temp1 = '<') and (Temp2 = '=')) then
    begin
      CurrToken := CurrToken + Temp2;
      Inc(FBufferPos);
    end;
  end;
  if Result <> tokUnknown then Exit;

  { Check for string }
  if Temp = '"' then
  begin
    Result := InnerProcCString(CurrPos, CurrLineNo, CurrToken);
    Exit;
  end;

  { Check for digits and identifiers }
  Result := InnerProcIdent(CurrPos, CurrLineNo, CurrToken);
  { Check for types }
  if (Result = tokIdent) and ShowType then
  begin
    Search := LowerCase(CurrToken);
    for I := 1 to MaxCType do
      if CType[I] = Search then
      begin
        Result := tokType;
        Exit;
      end;
  end;
  { Check for keywords }
  if (Result = tokIdent) and ShowKeyword then
  begin
    for I := 1 to MaxCKeyword do
      if CKeyword[I] = CurrToken then
      begin
        Result := tokKeyword;
        Exit;
      end;
  end;
end;

{ Unconvert value into string value }
function TZCScanner.UnwrapString(Value: string): string;
var
  N: Integer;
  Ptr1, Ptr2: PChar;
begin
  Result := '';
  if Value = '' then Exit;
  Delete(Value, 1, 1);
  Delete(Value, Length(Value), 1);

  SetLength(Result, Length(Value)+1);
  Ptr1 := PChar(Value);
  Ptr2 := PChar(Result);
  N := 0;
  while Ptr1^ <> #0 do
  begin
    if Ptr1^ <> '\' then
      Ptr2^ := Ptr1^
    else begin
      Inc(Ptr1);
      if Ptr1 = #0 then Break;
      case Ptr1^ of
        'n': Ptr2^ := #10;
        'r': Ptr2^ := #13;
        't': Ptr2^ := #9;
        '0': Ptr2^ := #0;
        else Ptr2^ := Ptr1^;
      end;
    end;
    Inc(N);
    Inc(Ptr1);
    Inc(Ptr2);
  end;
  SetLength(Result, N);
end;

{ Convert string value into string }
function TZCScanner.WrapString(Value: string): string;
var
  I, Add, Len: Integer;
  Ptr: PChar;
begin
  Add := 0;
  Len := Length(Value);
  for I := 1 to Len do
    if Value[I] in ['''','"','\',#9,#10,#13,#0] then
      Inc(Add);
  SetLength(Result, Len + Add);
  Ptr := PChar(Result);
  for I := 1 to Len do
  begin
    if Value[I] in ['''','"','\',#9,#10,#13,#0] then
    begin
      Ptr^ := '\';
      Inc(Ptr);
      case Value[I] of
        #9:  Ptr^ := 't';
        #10: Ptr^ := 'n';
        #13: Ptr^ := 'r';
        #0:  Ptr^ := '0';
        else Ptr^ := Value[I];
      end;
    end else
      Ptr^ := Value[I];
    Inc(Ptr);
  end;
  Result := '"' + Result + '"';
end;


end.
