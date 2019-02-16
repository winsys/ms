{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{               Formula parser component                 }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZParser;

{$R *.dcr}

interface

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses SysUtils, Classes, ZToken, ZMatch, Math {$IFDEF VERCLX}, Variants{$ENDIF};

{$IFNDEF LINUX
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

const
  MAX_PARSE_ITEMS = 100;
  MAX_PARSE_STACK = 100;
  MAX_PARSE_VARS  = 20;
  MAX_PARSE_FUNCS = 20;

type

  TParseItemType=(ptFunction, ptVariable, ptDelim, ptString, ptInteger, ptFloat,
    ptBoolean);

  TParseItem = record
    ItemValue: Variant;
    ItemType:  TParseItemType;
  end;

  TParseStack = array[0..MAX_PARSE_STACK] of Variant;

  TParseVar = record
    VarName:  string;
    VarValue: Variant;
  end;

  TZParser = class;

  TParseFunc = function(Sender: TZParser): Variant;

  TParseFuncRec = record
    FuncName: string;
    FuncPtr: TParseFunc;
  end;

  EParseException = class(Exception);

  { Formula parser component }
  TZParser = class(TComponent)
  private
    FParseItems: array[0..MAX_PARSE_ITEMS] of TParseItem;
    FParseCount: Integer;
    FErrCheck:   Integer;
    FEquation:   string;
    FParseStack: TParseStack;
    FStackCount: Integer;
    FVars:       array[0..MAX_PARSE_VARS] of TParseVar;
    FVarCount:   Integer;
    FFuncs:      array[0..MAX_PARSE_FUNCS] of TParseFuncRec;
    FFuncCount:  Integer;

    function  ExtractTokenEx(var Buffer, Token: string): TParseItemType;
    function  OpLevel(Operat: string): Integer;
    function  Parse(Level: Integer; var Buffer: string): Integer;
    procedure SetEquation(Value: string);
    function  GetVar(VarName: string): Variant;
    procedure SetVar(VarName: string; VarValue: Variant);
    function  GetVarName(VarIndex: Integer): string;
    function  GetFunc(FuncName: string): TParseFunc;
    procedure SetFunc(FuncName: string; FuncPtr: TParseFunc);
    function  GetFuncName(FuncIndex: Integer): string;
    procedure CheckTypes(Value1: Variant; var Value2: Variant);
    function  ConvType(Value: Variant): Variant;
    function  CheckFunc(var Buffer: string): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  Evalute: Variant;
    procedure Clear;
    procedure Push(Value: Variant);
    function  Pop: Variant;

    property Variables[Index: string]: Variant read GetVar  write SetVar;
    property VarCount: Integer read FVarCount;
    property VarNames[Index: Integer]: string read GetVarName;
    property Functions[Index: string]: TParseFunc read GetFunc write SetFunc;
    property FuncCount: Integer read FFuncCount;
    property FuncNames[Index: Integer]: string read GetFuncName;
  published
    property Equation: string read FEquation write SetEquation;
  end;

procedure Register;

implementation

uses ZExtra, ZCommonConst;

const
  tokABS      = 'ABS';
  tokAND      = 'AND';
  tokCOS      = 'COS';
  tokEXP      = 'EXP';
  tokFALSE    = 'FALSE';
  tokIIF      = 'IIF';
  tokLIKE     = 'LIKE';
  tokSTARTING = 'STARTING';
  tokINLIST   = 'INLIST';
  tokBETWEEN  = 'BETWEEN';
  //tokIN       = 'IN';
  tokLN       = 'LN';
  tokMAX      = 'MAX';
  tokMIN      = 'MIN';
  tokNOT      = 'NOT';
  tokNOW      = 'NOW';
  tokOR       = 'OR';
  tokSIN      = 'SIN';
  tokSQRT     = 'SQRT';
  tokSUM      = 'SUM';
  tokTAN      = 'TAN';
  tokTRUE     = 'TRUE';
  tokXOR      = 'XOR';

{************** User functions implementation *************}

{ Get current date and time }
function FuncNow(Sender: TZParser): Variant; forward;

{ Define minimal value }
function FuncMin(Sender: TZParser): Variant; forward;

{ Define maximum value }
function FuncMax(Sender: TZParser): Variant; forward;

{ Define result by value }
function FuncIIf(Sender: TZParser): Variant; forward;

{ Calculate sum of values }
function FuncSum(Sender: TZParser): Variant; forward;

{ Evalue sinus value }
function FuncSin(Sender: TZParser): Variant; forward;

{ Evalue cosinus value }
function FuncCos(Sender: TZParser): Variant; forward;

{ Evalue tangens value }
function FuncTan(Sender: TZParser): Variant; forward;

{ Evalue exponent value }
function FuncExp(Sender: TZParser): Variant; forward;

{ Evalue natural logoriphm value }
function FuncLn(Sender: TZParser): Variant; forward;

{ Evalue square root value }
function FuncSqrt(Sender: TZParser): Variant; forward;

{ Evalue absolute value }
function FuncAbs(Sender: TZParser): Variant; forward;

{function InList}
function FuncInList(Sender: TZParser): Variant; forward;

{Sql Between function }
function FuncBetween(Sender: TZParser): Variant; forward;

{SQL IN function}
//function FuncIn(Sender: TZParser): Variant; forward;

{******************* TZParser implementation ****************}

{ Class constructor }
constructor TZParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FErrCheck   := 0;
  FStackCount := 0;
  FVarCount   := 0;
  FFuncCount  := 0;
  SetFunc(tokNOW, FuncNow);
  SetFunc(tokMAX, FuncMax);
  SetFunc(tokMIN, FuncMin);
  SetFunc(tokIIF, FuncIIf);
  SetFunc(tokSUM, FuncSum);
  SetFunc(tokSIN, FuncSin);
  SetFunc(tokCOS, FuncCos);
  SetFunc(tokTAN, FuncTan);
  SetFunc(tokEXP, FuncExp);
  SetFunc(tokLN,  FuncLn);
  SetFunc(tokABS, FuncAbs);
  SetFunc(tokSQRT,FuncSqrt);
  SetFunc(tokINLIST, FuncInList);
  SetFunc(tokBetween, FuncBetween);
  //SetFunc(tokIN, FuncIn);
end;

{ Class destructor }
destructor TZParser.Destroy;
begin
  inherited Destroy;
end;

{ Extract highlevel lexem }
function TZParser.ExtractTokenEx(var Buffer, Token: string): TParseItemType;
var
  P: Integer;
  Temp: string;
  TokenType: TTokenType;
begin
  repeat
    TokenType := ExtractToken(Buffer, Token);
  until (Token <> tokNL) and (Token <> tokCR);
  if Token = '[' then
  begin
    TokenType := ttAlpha;
    P := Pos(']',Buffer);
    Token := '';
    if P > 0 then
    begin
      Token  := Copy(Buffer, 1, P-1);
      Buffer := Copy(Buffer, P+1, Length(Buffer)-P);
    end;
  end;
  if (Buffer <> '') and (Token = '>') and (Buffer[1] = '=') then
  begin
    ExtractToken(Buffer, Temp);
    Token := Token + Temp;
  end;
  if (Buffer <> '') and (Token = '<') and ((Buffer[1] = '=')
    or (Buffer[1] = '>')) then
  begin
    ExtractToken(Buffer, Temp);
    Token := Token + Temp;
  end;
  Temp := UpperCase(Token);
  if (Temp = tokAND) or (Temp = tokNOT) or (Temp = tokOR)
    or (Temp = tokXOR) or (Temp = tokLIKE) or (Temp = tokSTARTING) then
  begin
    Token  := Temp;
    Result := ptDelim;
    Exit;
  end;
  if (Temp = tokTRUE) or (Temp = tokFALSE) then
  begin
    Token  := Temp;
    Result := ptBoolean;
    Exit;
  end;

  Result := ptString;
  case TokenType of
    ttAlpha:
      Result := ptVariable;
    ttDelim:
      Result := ptDelim;
    ttDigit:
      begin
        if (Buffer <> '') and (Buffer[1] = '.') then
        begin
          ExtractToken(Buffer, Temp);
          Token := Token + '.';
          if (Buffer <> '') and (Buffer[1] >= '0') and (Buffer[1] <= '9') then
          begin
            ExtractToken(Buffer,Temp);
            Token := Token + Temp;
          end;
          Result := ptFloat;
        end else
          Result := ptInteger;
      end;
  end;
end;

{ Get priority level of operation }
function TZParser.OpLevel(Operat: string): Integer;
var
  Temp: string;
begin
  Result := 7;
  Temp := UpperCase(Operat);
  if (Temp = tokAND) or (Temp = tokOR) or (Temp = tokXOR) then
    Result := 1;
  if Temp = tokNOT then
    Result := 2;
  if (Temp = '<') or (Temp = '>') or (Temp = '=') or (Temp = '>=')
    or(Temp = '<=') or (Temp = '<>') then
    Result := 3;
  if (Temp[1] = '+') or (Temp[1] = '-') or (Temp = tokLIKE)
      or (Temp = tokSTARTING)  then
    Result := 4;
  if (Temp[1] = '/') or (Temp[1] = '*') or (Temp[1] = '%') then
    Result := 5;
  if (Temp[1] = '^') then
    Result := 6;
end;

{ Internal convert equation from infix form to postfix }
function TZParser.Parse(Level: Integer; var Buffer: string): Integer;
var
  ParseType: TParseItemType;
  Token, FuncName: string;
  NewLevel, Params, SaveCount: Integer;
  Temp: Char;
begin
  Result := 0;
  while Buffer <> '' do
  begin
    ParseType := ExtractTokenEx(Buffer, Token);
    if Token = '' then
      Exit;
    if (Token = ')') or (Token = ',') then
    begin
      PutbackToken(Buffer, Token);
      Exit;
    end;
    if Token = '(' then
    begin
      FErrCheck := 0;
      Parse(0,Buffer);
      ExtractTokenEx(Buffer, Token);
      if Token <> ')' then
        raise EParseException.Create(SSyntaxError);
      FErrCheck := 1;
      Continue;
    end;

    if ParseType = ptDelim then
    begin
      NewLevel := OpLevel(Token);

      if (FErrCheck = 2) and (Token <> tokNOT) then
        raise EParseException.Create(SSyntaxError);
      if FErrCheck = 0 then
        if (Token <> tokNOT) and (Token <> '+') and (Token <> '-') then
          raise EParseException.Create(SSyntaxError)
        else if Token <> tokNOT then
          NewLevel := 6;

      if (Token <> tokNOT) and (NewLevel <= Level) then
      begin
        PutbackToken(Buffer, Token);
        Result := NewLevel;
        Exit;
      end else if (Token = tokNOT) and (NewLevel < Level) then
      begin
        PutbackToken(Buffer, Token);
        Result := NewLevel;
        Exit;
      end;

      if (FErrCheck = 0) and (Token = '+') then
        Continue;
      if (FErrCheck = 0) and (Token = '-') then
        Token := '~';
      FErrCheck := 2;

      while (Buffer <> '') and (Buffer[1] <> ')')
        and (Parse(NewLevel, Buffer) > NewLevel) do;
      FParseItems[FParseCount].ItemValue := Token;
      FParseItems[FParseCount].ItemType  := ptDelim;
      Inc(FParseCount);
      Result := NewLevel;
      Continue;
    end;

    if FErrCheck = 1 then
      raise EParseException.Create(SSyntaxError);
    FErrCheck := 1;

    case ParseType of
      ptVariable:
        begin
          FParseItems[FParseCount].ItemValue := Token;
          if CheckFunc(Buffer) then
            ParseType := ptFunction
          else
            SetVar(Token, Unassigned);
        end;
      ptInteger:
        FParseItems[FParseCount].ItemValue := StrToInt(Token);
      ptFloat:
        begin
          Temp := DecimalSeparator;
          DecimalSeparator := '.';
          FParseItems[FParseCount].ItemValue := StrToFloat(Token);
          DecimalSeparator := Temp;
        end;
      ptString:
        begin
          DeleteQuotes(Token);
          FParseItems[FParseCount].ItemValue := Token;
        end;
      ptBoolean:
        FParseItems[FParseCount].ItemValue := (Token = tokTRUE);
    end;

    { Process function params }
    if ParseType = ptFunction then
    begin
      FuncName  := AnsiUpperCase(Token);
      SaveCount := FParseCount;
      Params    := 0;
      repeat
        FErrCheck := 0;
        Parse(0,Buffer);
        ExtractTokenEx(Buffer, Token);
        case Token[1] of
          ',':
            begin
              Inc(Params);
              Continue;
            end;
          ')':
            begin
              if SaveCount < FParseCount then
                Inc(Params);
              FParseItems[FParseCount].ItemValue := ConvType(Params);
              FParseItems[FParseCount].ItemType  := ptInteger;
              Inc(FParseCount);
              Break;
            end;
          else
            raise EParseException.Create(SSyntaxError);
        end;
      until Buffer = '';
      FParseItems[FParseCount].ItemValue := FuncName;
    end;

    FParseItems[FParseCount].ItemValue :=
      ConvType(FParseItems[FParseCount].ItemValue);
    FParseItems[FParseCount].ItemType := ParseType;
    Inc(FParseCount);
  end;
end;

{ Split equation to stack }
procedure TZParser.SetEquation(Value: string);
begin
  FParseCount := 0;
  FErrCheck   := 0;
  FEquation   := Value;
//  while Value<>'' do
  Parse(0, Value);
end;

{ Get variable name by it index }
function TZParser.GetVarName(VarIndex: Integer): string;
begin
  if VarIndex >= FVarCount then
    raise EParseException.Create(SIncorVarIdx);
  Result := AnsiUpperCase(FVars[VarIndex].VarName);
end;

{ Get variable value }
function TZParser.GetVar(VarName: string): Variant;
var
  I: Integer;
begin
  VarName := AnsiUpperCase(VarName);
  if VarName = 'NULL' then
  begin
    Result := NULL;
    Exit;
  end;

  I := 0;
  while I < FVarCount do
  begin
    if FVars[I].VarName = VarName then
    begin
      Result := FVars[I].VarValue;
      Exit;
    end;
    Inc(I);
  end;
  Result := Unassigned;
end;

{ Set new value to variable }
procedure TZParser.SetVar(VarName: string; VarValue: Variant);
var
  I: Integer;
begin
  I := 0;
  VarName := AnsiUpperCase(VarName);
  if VarName = 'NULL' then Exit;
  while I < FVarCount do
  begin
    if FVars[I].VarName = VarName then
    begin
      if VarType(VarValue) <> varEmpty then
        FVars[I].VarValue := ConvType(VarValue);
      Exit;
    end;
    Inc(I);
  end;

  if I >= MAX_PARSE_VARS then Exit;
  FVars[I].VarName  := VarName;
  FVars[I].VarValue := ConvType(VarValue);
  Inc(FVarCount);
end;

{ Get function name by it handle }
function TZParser.GetFuncName(FuncIndex: Integer): string;
begin
  if FuncIndex >= FFuncCount then
    raise EParseException.Create(SIncorFuncIdx);
  Result := FFuncs[FuncIndex].FuncName;
end;

{ Get function handle }
function TZParser.GetFunc(FuncName: string): TParseFunc;
var
  I: Integer;
begin
  FuncName := AnsiUpperCase(FuncName);
  for I := 0 to FFuncCount-1 do
    if UpperCase(FFuncs[I].FuncName) = FuncName then
    begin
      Result := FFuncs[I].FuncPtr;
      Exit;
    end;
  Result := nil;
end;

{ Set new function handle }
procedure TZParser.SetFunc(FuncName: string; FuncPtr: TParseFunc);
var
  I: Integer;
begin
  I := 0;
  FuncName := AnsiUpperCase(FuncName);
  while I < FFuncCount do
  begin
    if FFuncs[I].FuncName = FuncName then
    begin
      if Assigned(FuncPtr) then
        FFuncs[i].FuncPtr := FuncPtr;
      Exit;
    end;
    Inc(I);
  end;
  if I >= MAX_PARSE_FUNCS then Exit;
  FFuncs[I].FuncName := FuncName;
  FFuncs[I].FuncPtr  := FuncPtr;
  Inc(FFuncCount);
end;

{ Convert types of two variant values }
function TZParser.ConvType(Value: Variant): Variant;
begin
  case VarType(Value) of
    varByte, varSmallint, varInteger:
      Result := VarAsType(Value, varInteger);
    varSingle, varDouble, varCurrency:
      Result := VarAsType(Value, varDouble);
    varDate:
      Result := DateTimeToSqlDate(Value);
    varOleStr, varString, varVariant:
      Result := VarAsType(Value, varString);
    varBoolean:
      Result := Value;
    varEmpty, varNull:
      Result := Null;
    else
      raise EParseException.Create(STypesMismatch);
  end;
end;

{ Convert types of two variant values }
procedure TZParser.CheckTypes(Value1: Variant; var Value2: Variant);
begin
  if (Value1 = Null) or (Value2 = Null) then Exit;
  case VarType(Value1) of
    varInteger:
      if VarType(Value2) = varString then
        Value2 := StrToFloatEx(Value2)
      else
        Value2 := VarAsType(Value2, varInteger);
    varString:
        Value2 := VarAsType(Value2, varString);
    varDouble:
      if VarType(Value2) = varString then
        Value2 := StrToFloatEx(Value2)
      else
        Value2 := VarAsType(Value2, varDouble);
    varBoolean:
      case VarType(Value2) of
        varInteger, varDouble:
          Value2 := (Value2 <> 0);
        varString:
          Value2 := (StrToFloatEx(Value2) <> 0);
        varBoolean:
        else
          raise EParseException.Create(STypesMismatch);
      end;
    else
      raise EParseException.Create(STypesMismatch);
  end;
end;

{ Calculate an equation }
function TZParser.Evalute: Variant;
var
  I: Integer;
  Value1, Value2, Sgn: Variant;
  Op: string;
  FuncPtr: TParseFunc;
begin
  FStackCount := 0;
  for I := 0 to FParseCount-1 do
  begin
    case FParseItems[I].ItemType of
      ptFunction:
        begin
          FuncPtr := GetFunc(FParseItems[I].ItemValue);
          if Assigned(FuncPtr) then
            Push(FuncPtr(Self))
          else
            raise EParseException.CreateFmt(SFuncNotFound,
              [FParseItems[I].ItemValue]);
        end;
      ptVariable:
        begin
          Value1 := GetVar(FParseItems[I].ItemValue);
          if VarType(Value1) = varEmpty then
            raise EParseException.CreateFmt(SVarNotFound,
              [FParseItems[I].ItemValue]);
          Push(Value1);
        end;
      ptFloat, ptInteger, ptString, ptBoolean:
        Push(FParseItems[I].ItemValue);
      ptDelim:
        begin
          Op := VarAsType(FParseItems[I].ItemValue, varString);

          if Op[1] in ['+','-','*','/','%'] then
          begin
            Value2 := Pop;
            Value1 := Pop;

            if (Value1 = Null) or (Value2 = Null) then
            begin
              Push(Null);
              Continue;
            end;

            CheckTypes(Value1, Value2);
            case Op[1] of
              '+': Push(Value1 + Value2);
              '-': Push(Value1 - Value2);
              '*': Push(Value1 * Value2);
              '/': Push(Value1 / Value2);
              '%': Push(Value1 mod Value2);
            end;
            Continue;
          end;

          if (Op = '=') or (Op = '<') or (Op = '>') then
          begin
            Value2 := Pop;
            Value1 := Pop;

            CheckTypes(Value1, Value2);
            case Op[1] of
              '=': Push(Value1 = Value2);
              '<': Push(Value1 < Value2);
              '>': Push(Value1 > Value2);
            end;
            Continue;
          end;

          if (Op = '>=') or (Op = '<=') or (Op = '<>') then
          begin
            Value2 := Pop;
            Value1 := Pop;

            CheckTypes(Value1, Value2);
            if Op = '>=' then Push(Value1 >= Value2);
            if Op = '<=' then Push(Value1 <= Value2);
            if Op = '<>' then Push(Value1 <> Value2);
            Continue;
          end;

          if (Op = tokAND) or (Op = tokOR) or (Op = tokXOR) then
          begin
            Value1 := Pop;
            Value2 := Pop;

            if (Value1 = Null) or (Value2 = Null) then
            begin
              Push(Null);
              Continue;
            end;

            if Op = tokAND then Push(Value1 and Value2);
            if Op = tokOR then  Push(Value1 or Value2);
            if Op = tokXOR then Push((not Value1 and Value2) or (Value1 and not Value2));
            Continue;
          end;

          if Op = '~' then
          begin
            Value1 := Pop;
            if Value1 <> Null then
              Push(-Value1)
            else Push(Null);
            Continue;
          end;

          if Op = tokNOT then
          begin
            Value1 := Pop;
            if Value1 <> Null then
            begin
              CheckTypes(True, Value1);
              Push(not Value1);
            end else
              Push(Null);
            Continue;
          end;

          if Op = '^' then
          begin
            Value2 := Pop;
            Value1 := Pop;

            if (Value1 = Null) or (Value2 = Null) then
            begin
              Push(Null);
              Continue;
            end;

            Value2 := VarAsType(Value2, varDouble);
            Value1 := VarAsType(Value1, varDouble);
            if (Value1 < 0) and ((Value2 mod 2) = 1) then Sgn := -1
            else Sgn := 1;
            Push(Sgn*Power(Abs(Value1),Value2));
            Continue;
          end;

          if Op = tokLIKE then
          begin
            Value2 := Pop;
            Value1 := Pop;

            if (Value1 = Null) or (Value2 = Null) then
            begin
              Push(Null);
              Continue;
            end;

            Value2 := VarAsType(Value2, varString);
            Value1 := VarAsType(Value1, varString);
            Push(IsMatch(Value2,Value1));
            Continue;
          end;

          if Op = tokSTARTING then
          begin
            Value2 := Pop;
            Value1 := Pop;

            if (Value1 = Null) or (Value2 = Null) then
            begin
              Push(Null);
              Continue;
            end;

            Value2 := VarAsType(Value2, varString);
            Value1 := VarAsType(Value1, varString);
            Push(Copy(LowerCase(Value1),1,Length(Value2)) = LowerCase(Value2));
            Continue;
          end;

          (*
          if Op = tokBETWEEN then
          begin
            Value3 := Pop;
            Value2 := Pop;
            Value1 := Pop;

            Push((Value1 >= Value2) or (Value1 <= Value3));
            Continue;
          end;
          *)

          raise EParseException.Create(SIncorOperate);
        end;
    end;
  end;

  Result := Pop;
  if FStackCount > 0 then
    raise EParseException.Create(SEvalError);
end;

{ Push value to stack }
procedure TZParser.Push(Value: Variant);
begin
  if FStackCount >= MAX_PARSE_STACK then
    raise EParseException.Create(SStackFull);
  FParseStack[FStackCount] := Value;
  Inc(FStackCount);
end;

{ Pop value from stack }
function TZParser.Pop: Variant;
begin
  if FStackCount = 0 then
    raise EParseException.Create(SStackEmpty);
  Dec(FStackCount);
  Result := FParseStack[FStackCount];
end;

{ Clear all variables and equation }
procedure TZParser.Clear;
begin
  FStackCount := 0;
  FParseCount := 0;
  FVarCount   := 0;
  FEquation   := '';
end;

{ Define function }
function TZParser.CheckFunc(var Buffer: string): Boolean;
var
  I: Integer;
  Token: string;
begin
  I := 1;
  Result := False;
  while (I <= Length(Buffer)) and (Buffer[I] in [' ',#9,#10,#13]) do
    Inc(I);
  if Buffer = '' then
    Exit;
  if Buffer[I] = '(' then
  begin
    Result := True;
    ExtractToken(Buffer, Token);
  end;
end;

{************** User functions implementation **************}

{ Get current date and time }
function FuncNow(Sender: TZParser): Variant;
begin
  Result := Sender.Pop;
  if Result <> 0 then
    EParseException.CreateFmt(SIncorFuncParam,[tokNOW]);
  //Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now()); //???
  {Format is different for other country}
  //Result := FormatDateTime(LongDateFormat+' '+LongTimeFormat, Now());
  Result := DateTimeToStr(Now());
end;

{ Get maximum value }
function FuncMax(Sender: TZParser): Variant;
var
  Count: Integer;
  Temp:  Variant;
begin
  Count := Sender.Pop;
  if Count = 0 then
    EParseException.CreateFmt(SIncorFuncParam,[tokMAX]);
  Result := Sender.Pop;
  Dec(Count);
  while Count > 0 do
  begin
    Temp := Sender.Pop;
    if Temp > Result then Result := Temp;
    Dec(Count);
  end;
end;

{ Get minimum value }
function FuncMin(Sender: TZParser): Variant;
var
  Count: Integer;
  Temp:  Variant;
begin
  Count := Sender.Pop;
  if Count = 0 then
    EParseException.CreateFmt(SIncorFuncParam,[tokMIN]);
  Result := Sender.Pop;
  Dec(Count);
  while Count > 0 do
  begin
    Temp := Sender.Pop;
    if Temp < Result then Result := Temp;
    Dec(Count);
  end;
end;

{ Calculate sum of values }
function FuncSum(Sender: TZParser): Variant;
var
  Count: Integer;
begin
  Count := Sender.Pop;
  if Count = 0 then
    EParseException.CreateFmt(SIncorFuncParam,[tokSUM]);
  Result := Sender.Pop;
  Dec(Count);
  while Count > 0 do
  begin
    Result := Result + Sender.Pop;
    Dec(Count);
  end;
end;

{ Get result by value }
function FuncIIf(Sender: TZParser): Variant;
var
  Count: Integer;
  Temp, Temp1, Temp2: Variant;
begin
  Count := Sender.Pop;
  if Count <> 3 then
    EParseException.CreateFmt(SIncorFuncParam,[tokIIF]);
  Temp2 := Sender.Pop;
  Temp1 := Sender.Pop;
  Temp := VarAsType(Sender.Pop, varBoolean);
  if Temp then Result := Temp1
  else Result := Temp2;
end;

{ Evalue sinus value }
function FuncSin(Sender: TZParser): Variant;
begin
  Result := Sender.Pop;
  if Result <> 1 then
    EParseException.CreateFmt(SIncorFuncParam,[tokSIN]);
  Result := Sin(Sender.Pop);
end;

{ Evalue cosinus value }
function FuncCos(Sender: TZParser): Variant;
begin
  Result := Sender.Pop;
  if Result <> 1 then
    EParseException.CreateFmt(SIncorFuncParam,[tokCOS]);
  Result := Cos(Sender.Pop);
end;

{ Evalue tangens value }
function FuncTan(Sender: TZParser): Variant;
begin
  Result := Sender.Pop;
  if Result <> 1 then
    EParseException.CreateFmt(SIncorFuncParam,[tokTAN]);
  Result := Tan(Sender.Pop);
end;

{ Evalue exponent value }
function FuncExp(Sender: TZParser): Variant;
begin
  Result := Sender.Pop;
  if Result <> 1 then
    EParseException.CreateFmt(SIncorFuncParam,[tokEXP]);
  Result := Exp(Sender.Pop);
end;

{ Evalue natural logoriphm value }
function FuncLn(Sender: TZParser): Variant;
begin
  Result := Sender.Pop;
  if Result <> 1 then
    EParseException.CreateFmt(SIncorFuncParam,[tokLN]);
  Result := Ln(Sender.Pop);
end;

{ Evalue square root value }
function FuncSqrt(Sender: TZParser): Variant;
begin
  Result := Sender.Pop;
  if Result <> 1 then
    EParseException.CreateFmt(SIncorFuncParam,[tokSQRT]);
  Result := Sqrt(Sender.Pop);
end;

{ Evalute absolute value }
function FuncAbs(Sender: TZParser): Variant;
begin
  Result := Sender.Pop;
  if Result <> 1 then
    EParseException.CreateFmt(SIncorFuncParam,[tokABS]);
  Result := Abs(Sender.Pop);
end;

{ Sql IN }
function FuncInList(Sender: TZParser): Variant;
var
  Count, I: Integer;
  Temp, Tempv: Variant;
begin
  Result := False;
  Count := Sender.Pop;
  if Count <3 then
    EParseException.CreateFmt(SIncorFuncParam,[tokINLIST]);

  Tempv := VarArrayCreate([0, Count-1], varVariant);

  for I := 1 to Count-1 do
  Tempv[I] := Sender.Pop;

  Temp := Sender.Pop;

  for I := 1 to Count - 1 do
  begin
    if Tempv[I]= Temp then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ Sql Between function }
function FuncBetween(Sender: TZParser): Variant;
var
  Count: Integer;
  Temp, Temp1, Temp2: Variant;
begin
  Count := Sender.Pop;
  if Count <> 3 then
    EParseException.CreateFmt(SIncorFuncParam,[tokBETWEEN]);

  Temp2 := Sender.Pop;
  Temp1 := Sender.Pop;
  Temp  := Sender.Pop;

  Result := (Temp >= Temp1) and (Temp <= Temp2);
end;

procedure Register;
begin
  RegisterComponents(ZEOS_DB_PALETTE, [TZParser]);
end;


end.
