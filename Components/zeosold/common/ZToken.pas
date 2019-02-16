{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{          Functions for lexic and syntax analyse        }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZToken;

interface

uses Classes, SysUtils;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

const
{ Special symbols }
  tokTAB   = #9;
  tokCR    = #13;
  tokNL    = #10;
  tokDELIM = ' .:;,+-<>/*%^=()[]|&~@#$\`{}!?'#10#13#9;
  tokSPACE = ' ';

type
{ Lexem types }
  TTokenType = (ttUnknown, ttDelim, ttDigit, ttAlpha, ttString, ttCommand);

{******************* Check types functions ********************}

{ Check if delimiter }
function IsDelim(Value: Char): Boolean;

{ Check if white spaces }
function IsWhite(Value: Char): Boolean;

{ Check if digit }
function IsDigit(Value: Char): Boolean;

{ Check if alpha }
function IsAlpha(Value: Char): Boolean;

{ Check if end of line }
function IsEOL(Value: Char): Boolean;

{****************** Functions for lexical analize *************}

{ Extract low level lexem }
function ExtractLowToken(var Buffer, Token: string): TTokenType;

{ Extract lexem }
function ExtractToken(var Buffer, Token: string): TTokenType;

{ Extract high level lexem }
function ExtractTokenEx(var Buffer, Token: string): TTokenType;

{ Extract high high level lexem }
function ExtractHighToken(var Buffer: string; Cmds: TStringList;
  var Token: string; var CmdNo: Integer): TTokenType;

{ Putback lexem to buffer }
procedure PutbackToken(var Buffer: string; Value: string);

{ Delete begin and end quotes }
function DeleteQuotes(var Buffer: string): string;

{ Delete name delimiters }
function DeleteQuotesEx(var Buffer: string): string;

{ Convert string to C-escape string format }
function ConvStr(Value: string): string;

{ Convert string from C-escape string format }
function UnconvStr(Value: string): string;

{ Extract substring up to delimiters }
function StrTok(var Buffer: string; Delim: string): string;

{ Extract substring up to delimiters with string processing }
function StrTokEx(var Buffer: string; Delim: string): string;

{**************** Functions for params string processing *********************}

{ Extract parameter value by it index }
function ExtractParamByNo(Buffer: string; KeyNo: Integer): string;

{ Extract parameter value by it name }
function ExtractParam(Buffer, Key: string): string;

{ Split params string }
procedure SplitParams(Buffer: string; ParamNames, ParamValues: TStringList);

implementation

{ Check if delemiter }
function IsDelim(Value: Char): Boolean;
begin
  Result := (Pos(Value,tokDELIM) <> 0);
end;

{ Check if white space }
function IsWhite(Value: Char): Boolean;
begin
  Result := Value in [tokSPACE,tokTAB,tokNL,tokCR];
end;

{ Check if digit }
function IsDigit(Value: Char): Boolean;
begin
  Result := Value in ['0'..'9'];
end;

{ Check if alpha }
function IsAlpha(Value: Char): Boolean;
begin
  Result := (not IsDelim(Value)) and (not IsDigit(Value));
end;

{ Check if quotes }
function IsQuote(Value: Char): Boolean;
begin
  Result := Value in ['"',''''];
end;

{ Check if end of line }
function IsEOL(Value: Char): Boolean;
begin
  Result := Value in [tokNL,tokCR];
end;

{ Convert string to C-escape string format }
function ConvStr(Value: string): string;
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
end;

{ Convert string from C-escape string format }
function UnconvStr(Value: string): string;
var
  N: Integer;
  Ptr1, Ptr2: PChar;
begin
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

{ Extract lower level token }
function ExtractLowToken(var Buffer, Token: string): TTokenType;
label ExitProc;
var
  P: Integer;
  Quote: string;
begin
  P := 1;
  Result := ttUnknown;
  Token  := '';
  if Buffer = '' then Exit;

  while Buffer[P] in [' ',tokTAB] do
  begin
    Inc(P);
    if Length(Buffer) < P then
      goto ExitProc;
  end;

  if IsDelim(Buffer[P]) then
  begin
    Result := ttDelim;
    Token  := Buffer[P];
    Inc(P);
    goto ExitProc;
  end;

  if IsQuote(Buffer[P]) then
  begin
    Quote  := Buffer[P];
    Result := ttString;
    Token  := Quote;
    Inc(P);
    while P <= Length(Buffer) do
    begin
      Token := Token + Buffer[P];
      Inc(P);
      if (Buffer[P-1] = Quote) and (Buffer[P-2] <> '\') then
        Break;
    end;
  end
  else
  begin
    if IsDigit(Buffer[P]) then Result := ttDigit
    else Result := ttAlpha;
    while P <= Length(Buffer) do
    begin
      Token := Token + Buffer[P];
      Inc(P);
      if (P > Length(Buffer)) or IsDelim(Buffer[P]) or IsQuote(Buffer[P]) then
        Break;
    end;
  end;

ExitProc:
  Delete(Buffer, 1, P-1);
end;

{ Extract token except CR,NL }
function ExtractToken(var Buffer, Token: string): TTokenType;
begin
  repeat
    Result := ExtractLowToken(Buffer, Token);
  until (Token = '') or not (Token[1] in [tokNL, tokCR]);
end;

{ Putback lexem to buffer }
procedure PutbackToken(var Buffer: string; Value: string);
begin
  if Value <> '' then
    Buffer := Value + {tokSPACE +} Buffer;
end;

{ Delete begin and end quotes }
function DeleteQuotes(var Buffer: string): string;
begin
  if (Buffer <> '') and (IsQuote(Buffer[1])) then
  begin
    if Buffer[1] = Buffer[Length(Buffer)] then
      Buffer := Copy(Buffer, 2, Length(Buffer)-2)
    else
      Buffer := Copy(Buffer, 2, Length(Buffer)-1);
  end;
  Result := Buffer;
end;

{ Delete name delimiters }
function DeleteQuotesEx(var Buffer: string): string;
begin
  if (Buffer <> '') and (Buffer[1] = '[') then
  begin
    if Buffer[Length(Buffer)] = ']' then
      Buffer := Copy(Buffer, 2, Length(Buffer)-2)
    else
      Buffer := Copy(Buffer, 2, Length(Buffer)-1);
  end else
    DeleteQuotes(Buffer);
  Result := Buffer;
end;

{ Extract param value by it index }
function ExtractParamByNo(Buffer: string; KeyNo: Integer): string;
var
  N: Integer;
  Token: string;
  TokenType: TTokenType;
begin
  N := -1;
  Result := '';
  while (Buffer <> '') and (N < KeyNo) do
  begin
    TokenType := ExtractToken(Buffer, Token);
    if TokenType in [ttAlpha, ttDigit] then
      Inc(n);
    if (Token = '=') and (N < KeyNo) then
      ExtractToken(Buffer, Token);
  end;

  if N <> KeyNo then Exit;

  ExtractToken(Buffer, Token);
  if Token <> '=' then Exit;

  TokenType := ExtractToken(Buffer, Token);
  if TokenType = ttString then
    DeleteQuotes(Token);
  Result := Token;
end;

{ Extract param value by it name }
function ExtractParam(Buffer, Key: string): string;
var
  Token: string;
  TokenType: TTokenType;
begin
  while Buffer <> '' do
  begin
    ExtractToken(Buffer, Token);
    if Token = Key then break;
    if Token = '=' then
      ExtractToken(Buffer, Token);
  end;

  if Buffer = '' then Exit;

  ExtractToken(Buffer, Token);
  if Token <> '=' then Exit;

  TokenType := ExtractToken(Buffer, Token);
  if TokenType = ttString then
    DeleteQuotes(Token);
  Result := Token;
end;

{ Split params string }
procedure SplitParams(Buffer: string; ParamNames, ParamValues: TStringList);
var
  Token: string;
  TokenType: TTokenType;
begin
  if Assigned(ParamNames) then  ParamNames.Clear;
  if Assigned(ParamValues) then ParamValues.Clear;

  while Buffer <> '' do
  begin
    TokenType := ExtractToken(Buffer, Token);
    if TokenType in [ttUnknown, ttDelim] then
      Continue;

    if TokenType = ttString then
      DeleteQuotes(Token);
    if Assigned(ParamNames) then
      ParamNames.Add(Token);

    ExtractToken(Buffer, Token);
    if Token <> '=' then
    begin
      PutbackToken(Buffer, Token);
      if Assigned(ParamValues) then
        ParamValues.Add('');
    end
    else
    begin
      TokenType := ExtractToken(Buffer, Token);
      if TokenType = ttString then
        DeleteQuotes(Token);

      if TokenType in [ttDelim, ttUnknown] then
      begin
        if Assigned(ParamValues) then
          ParamValues.Add('');
      end else
        if Assigned(ParamValues) then
          ParamValues.Add(Token);
    end;
  end;
end;

{ Extract high level lexem }
function ExtractHighToken(var Buffer: string; Cmds:TStringList;
  var Token: string; var CmdNo: Integer): TTokenType;
var
  I: Integer;
  TempToken: string;
  TokenType: TTokenType;
begin
  TokenType := ExtractToken(Buffer, Token);
  CmdNo := -1;

{ Extract float numbers }
  if (TokenType = ttDigit) and (Buffer <> '') and (Buffer[1] = '.') then
  begin
    ExtractToken(Buffer, TempToken);
    Token := Token + TempToken;
    if IsDigit(Buffer[1]) then
    begin
      ExtractToken(Buffer, TempToken);
      Token := Token + TempToken;
    end;
  end;

{ Define command index }
  if (TokenType = ttAlpha) and Assigned(Cmds) then
  begin
    for I := 0 to Cmds.Count-1 do
      if Cmds[I] = Token then
      begin
        CmdNo := I;
        TokenType := ttCommand;
        Break;
      end;
  end;

  Result := TokenType;
end;

{ Extract hight level lexem }
function ExtractTokenEx(var Buffer, Token: string): TTokenType;
var
  P: Integer;
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
      Delete(Buffer, 1, P);
//      Buffer := Copy(Buffer,p+1,Length(Buffer)-p);
    end;
  end;
  Result := TokenType;
end;

{ Extract substring up to delimiters }
function StrTok(var Buffer: string; Delim: string): string;
var
  S, N: Integer;
begin
  Result := '';
  if Buffer = '' then
    Exit;
  N := 1;
  while (N <= Length(Buffer)) and (Pos(Buffer[N], Delim)>0) do
    Inc(N);
  S := N;
  while (N <= Length(Buffer)) and (Pos(Buffer[N], Delim)=0) do
    Inc(N);
  Result := Copy(Buffer, S, N-S);
  Delete(Buffer, 1, N-1);
end;

{ Extract substring up to delimiters with string processing }
function StrTokEx(var Buffer: string; Delim: string): string;
var
  S, N, M: Integer;
  Quote: Char;
begin
  N := 1;
  Result := '';
  if Buffer = '' then 
    Exit;
  while (N <= Length(Buffer)) and (Pos(Buffer[N], Delim)>0) do
    Inc(N);
  S := N;
  repeat
    if Buffer[N] in ['''','"'] then
    begin
      Quote := Buffer[N];
      Inc(N);
      while (N <= Length(Buffer)) and (Buffer[N] <> Quote) do
        Inc(N);
      if N > Length(Buffer) then N := Length(Buffer);
      Result := Copy(Buffer, S, N-S+1);
      Delete(Buffer, 1, N);
    end else if Buffer[N] = '(' then
    begin
      M := 1;
      repeat
        Inc(N);
        if N > Length(Buffer) then Break;
        if Buffer[N] = '(' then Inc(M);
        if Buffer[N] = ')' then Dec(M);
      until M = 0;
      if N > Length(Buffer) then N := Length(Buffer);
      Result := Copy(Buffer, S, N-S+1);
      Delete(Buffer, 1, N);
    end else
      Result := StrTok(Buffer, Delim);
  until (Buffer = '') or (Buffer[1] <> '(');
end;

end.