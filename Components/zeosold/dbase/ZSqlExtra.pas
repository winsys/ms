{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                Extra dbware functions                  }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZSqlExtra;

interface

uses SysUtils, Classes, DB;

{$INCLUDE ../Zeos.inc}

{**************** Extra functions definition ****************}

{ Extract field from string as Db.Table.Field }
function ExtractField(Value: string): string;

{ Extract field precision from type description }
function ExtractPrecision(Value: string): Integer;

{ Replace #10#13#9 charactires with spaces }
function ClearSpaces(Value: string): string;

{ Get index of name case sensetive }
function CaseIndexOf(List: TStrings; Value: string): Integer;

{ Get index of field in list }
function FieldIndexOf(List: string; Field: string): Integer;

{ Invert list delimited with semicolon }
function InvertList(List: string): string;

implementation

uses ZDBaseConst, ZExtra, ZToken;

{************** Extra functions implementation ***************}

{ Extract field name from string as Db.Table.Field }
function ExtractField(Value: string): string;
var
  P: Integer;
begin
  Result := Value;
  P := LastDelimiter('.', Result);
  Result := Copy(Result, P+1, Length(Result)-P);
  if Pos(' ', Result) > 0 then
    Result := '"' + Result + '"';
end;

{ Extract field precision from type description }
function ExtractPrecision(Value: string): Integer;
begin
  StrTok(Value,'() ,.');
  StrTok(Value,'() ,.');
  Result := StrToIntDef(StrTok(Value,'() ,.'),4);
end;

{ Replace #10#13#9 charactires with spaces }
function ClearSpaces(Value: string): string;
var
  I: Integer;
  Prev: Char;
  Quote: Char;
begin
  Result := Value;
  Prev := #0;
  Quote := #0;
  I := 1;
  while I <= Length(Result) do
  begin
    if (Result[I] in [' ',#9,#10,#13]) and (Quote = #0) then
    begin
      if Prev = ' ' then
      begin
        Delete(Result, I, 1);
        Dec(I);
      end else
        Result[I] := ' ';
    end;
    if (Result[I] in ['"', '''']) and (Prev <> '\') then
    begin
      if Quote = #0 then
        Quote := Result[I]
      else if Quote = Result[I] then
        Quote := #0;
    end;
    Prev := Result[I];
    Inc(I);
  end;
end;

{ Get index of name }
function CaseIndexOf(List: TStrings; Value: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to List.Count-1 do
    if StrCaseCmp(List[I], Value) then
    begin
      Result := I;
      Exit;
    end;
end;

{ Get index of field in list }
function FieldIndexOf(List: string; Field: string): Integer;
var
  Temp: string;
  N: Integer;
begin
  Result := 0;
  N := 1;
  while List <> '' do
  begin
    Temp := Trim(StrTokEx(List, ';,'));
    if StrCaseCmp(Temp, Field) then
    begin
      Result := N;
      Exit;
    end;
    Inc(N);
  end;
end;

{ Invert list delimited with semicolon }
function InvertList(List: string): string;
var
  Temp: string;
begin
  Result := '';
  while List <> '' do
  begin
    Temp := Trim(StrTokEx(List, ';,'));
    if Temp = '' then Continue;
    if Result <> '' then Result := ';' + Result;
    Result := Temp + Result;
  end;
end;

end.
