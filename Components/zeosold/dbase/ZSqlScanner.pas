{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{               Lexical Sql-Scanner class                }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZSqlScanner;

interface

uses Classes, SysUtils, ZScanner, ZSqlTypes;

type
  { Abstract Sql-scanner class definition }
  TZSqlScanner = class (TZScanner)
  protected
    FDatabaseType: TDatabaseType;

    function LowRunLex(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; override;

    function InnerProcSqlComment(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; virtual;
    function InnerProcSqlString(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; virtual;
    function InnerProcSqlIdent(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; virtual;
    function InnerProcSqlDelim(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; virtual;
  public
    constructor Create; override;

    function WrapString(Value: string): string; override;
    function UnwrapString(Value: string): string; override;

    function ExtractSpaces: string;
    function ExtractStatement(var CurrPos, CurrLen, CurrLineNo: Integer): string;

    property DatabaseType: TDatabaseType read FDatabaseType;
  end;

  { Interbase Scanner }
  TZIbSqlScanner = class (TZSqlScanner)
  public
    constructor Create; override;
  end;

  { MS SQL Scanner }
  TZMsSqlScanner = class (TZSqlScanner)
  protected
    function InnerProcSqlComment(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; override;
  public
    constructor Create; override;
  end;

  { Oracle Scanner }
  TZOraSqlScanner = class (TZSqlScanner)
  public
    constructor Create; override;
  end;

  { PostgreSql Scanner }
  TZPgSqlScanner = class (TZSqlScanner)
  protected
    function InnerProcSqlComment(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; override;
    function InnerProcSqlString(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; override;
  public
    constructor Create; override;
    function WrapString(Value: string): string; override;
    function UnwrapString(Value: string): string; override;
  end;

  { MySql Scanner }
  TZMySqlScanner = class (TZSqlScanner)
  protected
    function InnerProcSqlComment(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; override;
    function InnerProcSqlString(var CurrPos, CurrLineNo: Integer;
      var CurrToken: string): Integer; override;
  public
    constructor Create; override;
    function WrapString(Value: string): string; override;
    function UnwrapString(Value: string): string; override;
  end;

{ Create a Sql scanner according DatabaseType }
function CreateSqlScanner(DatabaseType: TDatabaseType): TZSqlScanner;

implementation

{*********************** General constants ************************}
const
  MaxSqlKeyword  = 24;
  SqlKeyword: array[1..MaxSqlKeyword] of string =
  (
    'select','update','delete','create','drop','explain','from','where',
    'order','by','group','having','set','values','into','union','all',
    'distinct','left','right','on','join','outer','inner'
  );

{***********************  General routines ***********************}
{ Create a Sql scanner according DatabaseType }
function CreateSqlScanner(DatabaseType: TDatabaseType): TZSqlScanner;
begin
  case DatabaseType of
    dtMySql:      Result := TZMySqlScanner.Create;
    dtInterbase:  Result := TZIbSqlScanner.Create;
    dtMsSql:      Result := TZMsSqlScanner.Create;
    dtPostgreSql: Result := TZPgSqlScanner.Create;
    dtOracle:     Result := TZOraSqlScanner.Create;
    else          Result := TZMySqlScanner.Create;
  end;
end;

{ TZSqlScanner }

{ Class constructor }
constructor TZSqlScanner.Create;
begin
  inherited Create;
  FDatabaseType := dtUnknown;
end;

{ Unconvert value into pascal-like strings }
function TZSqlScanner.UnwrapString(Value: string): string;
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
    if not (Value[Pos] in ['''', '"']) then
      Result := Result + Value[Pos]
    else if (Pos < Len) and (Value[Pos+1] = Value[Pos]) then
    begin
      Result := Result + Value[Pos];
      Inc(Pos);
    end;
    Inc(Pos);
  end;
end;

{ Convert string value into pascal-like string }
function TZSqlScanner.WrapString(Value: string): string;
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

{ Get lowlevel token }
function TZSqlScanner.LowRunLex(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
begin
  { Initialize the lexing }
  Result := InnerStartLex(CurrPos, CurrLineNo, CurrToken);

  if Result <> tokUnknown then Exit;

  { Process sql comments }
  Result := InnerProcSqlComment(CurrPos, CurrLineNo, CurrToken);
  if Result <> tokUnknown then Exit;

  { Look for sql delimiters }
  Result := InnerProcSqlDelim(CurrPos, CurrLineNo, CurrToken);
  if Result <> tokUnknown then Exit;

  { Look for sql string }
  Result := InnerProcSqlString(CurrPos, CurrLineNo, CurrToken);
  if Result <> tokUnknown then Exit;

  { Look for identifiers and constants }
  Result := InnerProcSqlIdent(CurrPos, CurrLineNo, CurrToken);
end;

{ Extract leading whitespaces }
function TZSqlScanner.ExtractSpaces: string;
begin
  Result := '';
  if FBufferPos > FBufferLen then Exit;
  { Skip whitespaces }
  while (FBuffer[FBufferPos] in [' ',#9,#10])
    and (FBufferPos <= FBufferLen) do
  begin
    Result := Result + FBuffer[FBufferPos];
    Inc(FBufferPos);
  end;
end;

{ Extract an Sql statement }
function TZSqlScanner.ExtractStatement(var CurrPos, CurrLen,
  CurrLineNo: Integer): string;
var
  Token: string;
  TokenType, TokenLineNo, TokenPos: Integer;
begin
  ExtractSpaces;
  CurrPos := FBufferPos; // Added
  CurrLineNo := FBufferLine;
  Result := '';
  TokenType := RunLex(TokenPos, TokenLineNo, Token);
  while TokenType in [tokEol, tokLF] do
    TokenType := RunLex(TokenPos, TokenLineNo, Token);
//  CurrPos := TokenPos - 1;
  while (TokenType <> tokEof) and (Token <> ';') do
  begin
    { Handling LF chars  (#10) }
    if (TokenType=tokLF) and not (Result[Length(Result)] in [' ', #9]) then
      Result := Result + ' '
    else
      Result := Result + Token;
    Token := ExtractSpaces;
    if Token <> '' then
      Result := Result + ' ';
    TokenType := RunLex(TokenPos, TokenLineNo, Token);
  end;
  CurrLen := FBufferPos - CurrPos; // -1;
  if ShowComment and ShowEol then
    Result := AdjustLineBreaks(Result);
end;

{********************** Lexical procedures *********************}

{ Process sql delimiters }
function TZSqlScanner.InnerProcSqlDelim(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
var
  Temp, Temp1: Char;
begin
  Result := tokUnknown;
  Temp := CurrToken[1];
  { Check for brace }
  if Temp in ['{', '}', '(', ')', '[', ']'] then
  begin
    Result := tokBrace;
  end
  { Check for separator }
  else if Temp in [',', ';', ':'] then
  begin
    Result := tokSeparator;
  end
  { Check for delimiters }
  else if Pos(Temp, '~!#%?|=+-<>/*^@#') > 0 then
  begin
    Result := tokOperator;
    if FBufferPos <= FBufferLen then
      Temp1 := FBuffer[FBufferPos]
    else Temp1 := #0;
    if ((Temp = '>') and (Temp1 = '='))
      or ((Temp = '<') and (Temp1 in ['=', '>'])) then
    begin
      CurrToken := CurrToken + Temp1;
      Inc(FBufferPos);
    end;
  end;
end;

{ Process sql comments }
function TZSqlScanner.InnerProcSqlComment(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
begin
  { Check for single-line comment }
  if (CurrToken[1] = '/') and (FBufferPos <= FBufferLen)
    and (FBuffer[FBufferPos] = '/') then
  begin
    Result := InnerProcLineComment(CurrPos, CurrLineNo, CurrToken);
    Exit;
  end;
  { Check for multi-line comment }
  Result := InnerProcCComment(CurrPos, CurrLineNo, CurrToken);
end;

{ Process sql strings }
function TZSqlScanner.InnerProcSqlString(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
begin
  Result := InnerProcPasString(CurrPos, CurrLineNo, CurrToken);
end;

{ Process sql identifiers and constants }
function TZSqlScanner.InnerProcSqlIdent(var CurrPos, CurrLineNo: Integer;
  var CurrToken: string): Integer;
var
  I: Integer;
  Search: string;
begin
  { Look for digits and identifiers }
  Result := InnerProcIdent(CurrPos, CurrLineNo, CurrToken);
  { Look for sql keywords }
  if (Result = tokIdent) and ShowKeyword then
  begin
    Search := LowerCase(CurrToken);
    for I := 1 to MaxSqlKeyword do
      if SqlKeyword[I] = Search then
      begin
        Result := tokKeyword;
        Exit;
      end;
  end;
end;

{ TZIbSqlScanner }

{ Class constructor }
constructor TZIbSqlScanner.Create;
begin
  inherited Create;
  FDatabaseType := dtInterbase;
end;

{ TZMsSqlScanner }

{ Class constructor }
constructor TZMsSqlScanner.Create;
begin
  inherited Create;
  FDatabaseType := dtMsSql;
end;

{ Process MS SQL comments }
function TZMsSqlScanner.InnerProcSqlComment(var CurrPos,
  CurrLineNo: Integer; var CurrToken: string): Integer;
begin
  { Check for single-line comment }
  if (CurrToken[1] = '-') and (FBufferPos <= FBufferLen)
    and (FBuffer[FBufferPos] = '-') then
  begin
    Result := InnerProcLineComment(CurrPos, CurrLineNo, CurrToken);
    Exit;
  end;
  { Check for multi-line comment }
  Result := InnerProcCComment(CurrPos, CurrLineNo, CurrToken);
end;

{ TZOraSqlScanner }

{ Class constructor }
constructor TZOraSqlScanner.Create;
begin
  inherited Create;
  FDatabaseType := dtOracle;
end;

{ TZMySqlScanner }

{ Class constructor }
constructor TZMySqlScanner.Create;
begin
  inherited Create;
  FDatabaseType := dtMySql;
end;

{ Unconvert value into string value for MySql }
function TZMySqlScanner.UnwrapString(Value: string): string;
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
        'Z': Ptr2^ := #26;
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

{ Convert string value into string for MySql }
function TZMySqlScanner.WrapString(Value: string): string;
var
  I, Add, Len: Integer;
  Ptr: PChar;
begin
  Add := 0;
  Len := Length(Value);
  for I := 1 to Len do
    if Value[I] in ['''','"','\',#26,#10,#13,#0] then
      Inc(Add);
  SetLength(Result, Len + Add);
  Ptr := PChar(Result);
  for I := 1 to Len do
  begin
    if Value[I] in ['''','"','\',#26,#10,#13,#0] then
    begin
      Ptr^ := '\';
      Inc(Ptr);
      case Value[I] of
        #26: Ptr^ := 'Z';
        #10: Ptr^ := 'n';
        #13: Ptr^ := 'r';
        #0:  Ptr^ := '0';
        else Ptr^ := Value[I];
      end;
    end else
      Ptr^ := Value[I];
    Inc(Ptr);
  end;
  Result := '''' + Result + '''';
end;

{ Process MySql comment }
function TZMySqlScanner.InnerProcSqlComment(var CurrPos,
  CurrLineNo: Integer; var CurrToken: string): Integer;
begin
  Result := tokUnknown;
  { Check for -- single-line comment }
  if (CurrToken[1] = '-') and (FBufferPos <= FBufferLen)
    and (FBuffer[FBufferPos] = '-') then
  begin
    Result := InnerProcLineComment(CurrPos, CurrLineNo, CurrToken);
    Exit;
  end;
  { Check for # single-line comment }
  if CurrToken[1] = '#' then
  begin
    Result := InnerProcLineComment(CurrPos, CurrLineNo, CurrToken);
    Exit;
  end;
end;

{ Process MySql strings }
function TZMySqlScanner.InnerProcSqlString(var CurrPos,
  CurrLineNo: Integer; var CurrToken: string): Integer;
begin
  Result := InnerProcCString(CurrPos, CurrLineNo, CurrToken);
end;

{ TZPgSqlScanner }

{ Class constructor }
constructor TZPgSqlScanner.Create;
begin
  inherited Create;
  FDatabaseType := dtPostgreSql;
end;

{ Unconvert value into string value for PostgreSql }
function TZPgSqlScanner.UnwrapString(Value: string): string;
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

{ Convert string value into string for PostgreSql }
function TZPgSqlScanner.WrapString(Value: string): string;
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
  Result := '''' + Result + '''';
end;

{ Process PostgreSql comment }
function TZPgSqlScanner.InnerProcSqlComment(var CurrPos,
  CurrLineNo: Integer; var CurrToken: string): Integer;
begin
  { Check for single-line comment }
  if (CurrToken[1] = '-') and (FBufferPos <= FBufferLen)
    and (FBuffer[FBufferPos] = '-') then
  begin
    Result := InnerProcLineComment(CurrPos, CurrLineNo, CurrToken);
    Exit;
  end;
  { Check for multi-line comment }
  Result := InnerProcCComment(CurrPos, CurrLineNo, CurrToken);
end;

{ Process PostgreSql strings }
function TZPgSqlScanner.InnerProcSqlString(var CurrPos,
  CurrLineNo: Integer; var CurrToken: string): Integer;
begin
  Result := InnerProcCString(CurrPos, CurrLineNo, CurrToken);
end;

end.
