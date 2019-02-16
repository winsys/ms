{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                   Extra functions                      }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZExtra;

interface

uses {$IFNDEF LINUX}Windows{$ELSE}Types{$ENDIF}, SysUtils, Classes, ZToken, Math;

{$INCLUDE ../Zeos.inc}

{**************** Extra functions definition ****************}

{ Get file version number from version resource }
//function GetFileVersion(FileName: string): string;

{ Convert string buffer into pascal string }
function MemPas(Buffer: PChar; Length: LongInt): string;

{ string compare from the end }
function StrCmpEnd(Str1, Str2: string): Boolean;

{ string compare from the begin }
function StrCmpBegin(Str1, Str2: string): Boolean;

{ Compares strings case sensitively }
function StrCaseCmp(Str1, Str2: string): Boolean;

{ Convert string value to float with '.' delimiter }
function StrToFloatEx(Value: string): Double;

{ Convert string value to float with '.' delimiter with default value }
function StrToFloatDefEx(Value: string; Default: Double): Double;

{ Convert string value to float with '.' delimiter }
function StrToFloatCom(Value: string): Double;

{ Convert float value to string with '.' delimiter }
function FloatToStrEx(Value: Double): string;

{ Convert currency value to string }
function MoneyToString(Total: Double; Currency, Coin: string): string;

{ Get maximum value }
function Max(A, B: Integer): Integer;

{ Get minimum value }
function Min(A, B: Integer): Integer;

{ Sign of value }
function Sgn(Value: Double): Integer;

{****************** Functions for SQL92 dates processing  *******************}

{ Convert MySQL Timestamp to TDateTime }
function MyTimestampToDateTime(Value: string): TDateTime;

{ Convert MySQL Timestamp to Sql Data }
function MyTimestampToSqlDate(Value: string): string;

{ Convert SQL Date to TDateTime }
function SqlDateToDateTime(Value: string): TDateTime;

{ Convert SQL Date to TDateTime with constant date part }
function SqlDateToDateTimeEx(Value: string): TDateTime;

{ Convert TDateTime to SQL Ansi-92 Date }
function DateTimeToSqlDate(Value: TDateTime): string;

{ Convert TDateTime to SQL Ansi-92 Date with constant date part}
function DateTimeToSqlDateEx(Value: TDateTime): string;

{ Format date to SQL92 standart }
function EncodeSqlDate(Year, Month, Day: Word): string;

{ Encode SQL92 date into year, month and day }
procedure DecodeSqlDate(Date: string; var Year, Month, Day: Word);

{ Format date in ISO format }
function FormatSqlDate(Value: TDateTime): string;

{ Format time in ISO format }
function FormatSqlTime(Value: TDateTime): string;

{ Define begin of a month }
function BeginMonth(Date: string): string;

{ Define last day of a month }
function LastDay(Month, Year: Word): Word;

{ Define and of a month }
function EndMonth(Date: string): string;

{ Define end of a previous month }
function PriorMonth(Date: string): string;

{ Define begin of a next month }
function NextMonth(Date: string): string;

{ Define previous day }
function PriorDay(Date: string): string;

{ Define next day }
function NextDay(Date: string): string;

implementation

{************** Extra functions implementation ***************}

{ Convert MySQL Timestamp to TDateTime }
function MyTimestampToDateTime(Value: string): TDateTime;
var
   Year, Month, Day, Hour, Min, Sec: Integer;
   LengthString, BeginMonth: Integer;
begin
   Month := 1;
   Day := 1;
   Hour := 0;
   Min := 0;
   Sec := 0;
   { only for speed reasons}
   LengthString := Length(Value);

   if (LengthString = 14) or (LengthString = 8) then
   begin
     BeginMonth := 5;
     Year := Max(1, StrToIntDef(Copy(Value, 1, 4), 1));
   end
   else
   begin
     BeginMonth := 3;
     Year  := Max(1, StrToIntDef(Copy(Value, 1, 2), 1));
   end;

   if LengthString > 2 then  {Add Month}
   begin
     Month := Max(1, StrToIntDef(Copy(Value, BeginMonth, 2), 1));
     if LengthString > 4 then {Add Day}
     begin
       Day   := Max(1, StrToIntDef(Copy(Value, BeginMonth+2, 2), 1));
       if LengthString > 6 then {Add Hour}
       begin
         Hour := StrToIntDef(Copy(Value, BeginMonth+4, 2), 0);
         if LengthString > 8 then {Add Minute}
         begin
           Min  := StrToIntDef(Copy(Value, BeginMonth+6, 2), 0);
           if LengthString > 10 then {Add Second}
           begin
             Sec  := StrToIntDef(Copy(Value, BeginMonth+8, 2), 0);
           end;
         end;
       end;
     end;
   end;

   Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, 0);
end;

{ Convert MySQL Timestamp to Sql Data }
function MyTimestampToSqlDate(Value: string): string;
var
  LengthString, BeginMonth: Integer;
begin
  Result := Value;
  LengthString := Length(Value);

  if (LengthString = 14) or (LengthString = 8) then
    BeginMonth := 5
  else BeginMonth := 3;

  if LengthString > 2 then {Add Month}
  begin
    Insert('-', Result, BeginMonth);
    if LengthString > 4 then {Add Day}
    begin
      Insert('-', Result, BeginMonth+3);
      if LengthString > 6 then {Add Hour}
      begin
        Insert(' ', Result, BeginMonth+6);
        if LengthString > 8 then {Add Minute}
        begin
          Insert(':', Result, BeginMonth+9);
          if LengthString > 10 then {Add Second}
          begin
            Insert(':', Result, BeginMonth+12);
          end;
        end;
      end;
    end;
  end;
end;

{ Convert string buffer into pascal string }
function MemPas(Buffer: PChar; Length: LongInt): string;
begin
  Result := '';
  if Assigned(Buffer) then
    SetString(Result, Buffer, Length);
end;

{ Convert SQL Date to TDateTime }
function SqlDateToDateTime(Value: string): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec: Word;
  Temp: string;
begin
  Temp   := Value;
  Result := 0;
  if Length(Temp) >= 10 then
  begin
    Year  := StrToIntDef(Copy(Temp, 1, 4), 1);
    Year := Max(Year, 1);
    Month := StrToIntDef(Copy(Temp, 6, 2), 1);
    Month := MinIntValue([MaxIntValue([Month, 1]), 12]);
    Day   := StrToIntDef(Copy(Temp, 9, 2), 1);
    Day   := MinIntValue([MaxIntValue([Day, 1]), LastDay(Month, Year)]);

    Result := EncodeDate(Year, Month, Day);
    Temp := Copy(Temp, 12, 8);
  end;
  if Length(Temp) >= 8 then
  begin
    Hour := StrToIntDef(Copy(Temp, 1, 2), 0);
    Hour := MinIntValue([MaxIntValue([Hour, 0]), 23]);
    Min  := StrToIntDef(Copy(Temp, 4, 2), 0);
    Min  := MinIntValue([MaxIntValue([Min, 0]), 59]);
    Sec  := StrToIntDef(Copy(Temp, 7, 2), 0);
    Sec  := MinIntValue([MaxIntValue([Sec, 0]), 59]);
    Result := Result + EncodeTime(Hour, Min, Sec, 0);
  end;
end;

{ Convert SQL Date to TDateTime with constant date part }
function SqlDateToDateTimeEx(Value: string): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec: Word;
  Temp: string;
begin
  Temp  := Value;
  Year  := StrToIntDef(Copy(Temp, 1, 4), 1);
  Year  := MaxIntValue([Year, 1]);
  Month := StrToIntDef(Copy(Temp, 6, 2), 1);
  Month := MinIntValue([MaxIntValue([Month, 1]), 12]);
  Day   := StrToIntDef(Copy(Temp, 9, 2), 1);
  Day   := MinIntValue([MaxIntValue([Day, 1]), LastDay(Month, Year)]);
  Result := EncodeDate(Year, Month, Day);

  if Length(Temp) > 11 then
  begin
    Temp := Copy(Temp, 12, 8);
    Hour := StrToIntDef(Copy(Temp, 1, 2), 0);
    Hour := MinIntValue([MaxIntValue([Hour, 0]), 23]);
    Min  := StrToIntDef(Copy(Temp, 4, 2), 0);
    Min  := MinIntValue([MaxIntValue([Min, 0]), 59]);
    Sec  := StrToIntDef(Copy(Temp, 7, 2), 0);
    Sec  := MinIntValue([MaxIntValue([Sec, 0]), 59]);
    Result := Result + EncodeTime(Hour, Min, Sec, 0);
  end;
end;

{ Form number with leading zeros }
function FormatNumber(Value, Width: Word): string;
begin
  Result := IntToStr(Value);
  while Length(Result) < Width do
    Result := '0' + Result;
end;

{ Format date in ISO format }
function FormatSqlDate(Value: TDateTime): string;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := FormatNumber(Year, 4) + '-' + FormatNumber(Month, 2)
    + '-' + FormatNumber(Day, 2);
end;

{ Format time in ISO format }
function FormatSqlTime(Value: TDateTime): string;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  Result := FormatNumber(Hour, 2) + ':' + FormatNumber(Min, 2)
    + ':' + FormatNumber(Sec, 2);
end;

{ Convert TDateTime to SQL Ansi-92 Date }
function DateTimeToSqlDate(Value: TDateTime): string;
begin
  Result := '';
  if Trunc(Value) <> 0 then
    Result := FormatSqlDate(Value);
  if Frac(Value) <> 0 then
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + FormatSqlTime(Value);
  end;
end;

{ Convert TDateTime to SQL Ansi-92 Date with constant date part}
function DateTimeToSqlDateEx(Value: TDateTime): string;
begin
  if Trunc(Value) <> 0 then
    Result := FormatSqlDate(Value)
  else
    Result := '0001-01-01';

  if Frac(Value) <> 0 then
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + FormatSqlTime(Value);
  end;
end;

{ string compare from the end }
function StrCmpEnd(Str1, Str2: string): Boolean;
var
  P1, P2: Integer;
begin
  Result := True;
  P1 := Length(Str1);
  P2 := Length(Str2);
  while (P1 > 0) and (P2 > 0) and Result do
  begin
    Result := (Str1[P1] = Str2[P2]);
    Dec(P1);
    Dec(P2);
  end;
end;

{ string compare from the begin }
function StrCmpBegin(Str1, Str2: string): Boolean;
begin
  if ((Str1 = '') or (Str2 = '')) and (Str1 <> Str2) then
    Result := False
  else
    Result := (StrLComp(PChar(Str1), PChar(Str2),
      Min(Length(Str1), Length(Str2))) = 0);
end;

{ Compares strings case sensitively }
function StrCaseCmp(Str1, Str2: string): Boolean;
begin
  Result := CompareText(Str1, Str2) = 0;
end;

{ Convert string value to float with '.' or ',' delimiter }
function StrToFloatEx(Value: string): Double;
var
  Ptr: PChar;
begin
  Ptr := PChar(Value);
  while Ptr^ <> #0 do
  begin
    if Ptr^ in [',','.'] then
      Ptr^ := DecimalSeparator;
    Inc(Ptr);
  end;
  if Value <> '' then
    try
      Result := StrToFloat(Value);
    except
      Result := 0;
    end
  else
    Result := 0;
end;

{ Convert string value to float with '.' delimiter }
function StrToFloatCom(Value: string): Double;
var
  Temp: Integer;
begin
  if Value <> '' then
    try
      if DecimalSeparator <> '.' then
      begin
        Temp := AnsiPos(DecimalSeparator, Value);
        if Temp <> 0 then Value[Temp] := '.';
      end;
      Result := StrToFloat(Value);
    except
      Result := 0;
    end
  else
    Result := 0;
end;

{ Convert string value to float with '.' delimiter with default value }
function StrToFloatDefEx(Value: string; Default: Double): Double;
var
  Ptr: PChar;
begin
  Ptr := PChar(Value);
  while Ptr^ <> #0
  do begin
    if Ptr^ in ['.',','] then
      Ptr^ := DecimalSeparator;
    Inc(Ptr);
  end;
  if Value <> '' then
    try
      Result := StrToFloat(Value);
    except
      Result := Default;
    end
  else
    Result := 0;
end;

{ Convert float value to string with '.' delimiter }
function FloatToStrEx(Value: Double): string;
var
  Temp: Integer;
begin
  Result := FloatToStr(Value);
  if DecimalSeparator <> '.' then
  begin
    Temp := AnsiPos(DecimalSeparator,Result);
    if Temp <> 0 then Result[Temp] := '.';
  end;
end;

{ Convert currency value to string }
function MoneyToString(Total: Double; Currency, Coin: string): string;
const
{$IFDEF RUSSIAN}
  StrTop: array[0..5] of string =
    ('','тысяч','миллион','миллиард','триллион','триллиард');
  Padeg: array[0..1,0..9] of string = (
    (''  ,'а','и','и','и',  '',  '',  '',  '',  ''),
    ('ов', '','а','а','а','ов','ов','ов','ов','ов')
  );
  NumLevel: array[0..3,0..9] of string = (
    ('','один','два','три','четыре','пять','шесть','семь','восемь','девять'),
    ('','десять','двадцать','тридцать','сорок','пятьдесят','шестьдесят',
     'семьдесят','восемьдесят','девяносто'),
    ('','сто','двести','триста','четыреста','пятьсот','шестьсот','семьсот',
     'восемьсот','девятьсот'),
    ('десять','одиннадцать','двенадцать','тринадцать','четырнадцать',
     'пятнадцать','шестнадцать','семнадцать','восемнадцать','девятнадцать')
  );
  NoRole: array[0..1] of string = ('одна','две');
{$ELSE}
  StrTop: array[0..5] of string =
    ('hundred','thousand','million','milliard','trillion','trilliard');
  Padeg: array[0..1,0..9] of string =
  (
    ('','','','','','','','','',''),
    ('','','','','','','','','','')
  );
  NumLevel: array[0..3,0..9] of string =
  (
    ('','one','two','three','four','five','six','seven','eight','nine'),
    ('','ten','twenty','thirty','fourty','fifty','sixty',
     'seventy','eighty','ninety'),
    ('','one','two','three','four','five','six','seven',
     'eight','nine'),
    ('ten','eleven','twelve','thirteen','fourteen',
     'fifteen','sixteen','seventeen','eighteen','nineteen')
  );
  NoRole: array [0..1] of string = ('one','two');
{$ENDIF}
  MAX_DIGIT_NUM = 18;
var
  MaxLevel: LongInt;
  Levels:   array [0..MAX_DIGIT_NUM] of Byte;
  TotalLow: LongInt;
  IsEmpty:  Boolean;
  Temp:     string;
begin
  Result  := '';
  IsEmpty := True;
  if Trunc(Total*100) = 0 then
    Exit;

  TotalLow := Trunc(Frac(Total)*100);
  MaxLevel := 0;
  while (Total > 0) and (MaxLevel < MAX_DIGIT_NUM) do
  begin
    Levels[MaxLevel] := Trunc(Total) mod 10;
    Total := Total / 10;
    Inc(MaxLevel);
  end;
  Dec(MaxLevel);

  while MaxLevel >= 0 do
  begin
    if IsEmpty and (Levels[MaxLevel] <> 0) then
      IsEmpty := False;
    Temp := '';
    if ((MaxLevel mod 3) = 1) and (Levels[MaxLevel] = 1) then
    begin
      Dec(MaxLevel);
      Temp := NumLevel[3][Levels[MaxLevel]];
    end
    else
    if (MaxLevel = 3) and ((Levels[MaxLevel] = 2) or (Levels[MaxLevel] = 1)) then
      Temp := NoRole[Levels[MaxLevel]-1]
    else
      Temp := NumLevel[MaxLevel mod 3][Levels[MaxLevel]];

    if Temp <> '' then
      Result := Result + Temp + ' ';

    if ((MaxLevel mod 3) = 0) and not IsEmpty then
    begin
      if Result <> '' then
      begin
        Temp := StrTop[MaxLevel div 3];
        if MaxLevel div 3 = 1 then
          Temp := Temp + Padeg[0][Levels[MaxLevel]]
        else
        if MaxLevel div 3 > 1 then
          Temp := Temp + Padeg[1][Levels[MaxLevel]];
        if Temp <> '' then
          Result := Result + Temp + ' ';
      end;
      IsEmpty := True;
    end;

    Dec(MaxLevel);
  end;
  if Trim(Result) <> '' then
    Result := Result + ' ' + Currency;
  if TotalLow < 10 then
    Result := Result + ' 0' + IntToStr(TotalLow) + ' ' + Coin
  else
    Result := Result + ' ' + IntToStr(TotalLow) + ' ' + Coin;
end;

{ Format date to SQL92 standart }
function EncodeSqlDate(Year, Month, Day: Word): string;
begin
  Result := Format('%4.4d-%2.2d-%2.2d', [Year,Month,Day]);
end;

{ Encode SQL92 date into year, month and day }
procedure DecodeSqlDate(Date: string; var Year, Month, Day: Word);
begin
  Year := StrToIntDef(Copy(Date,1,4),0);
  if Year = 0 then Year := 2000;
  Month := Max(1, StrToIntDef(Copy(Date,6,2),0));
  Day   := Max(1, StrToIntDef(Copy(Date,9,2),0));
end;

{ Define begin of a month }
function BeginMonth(Date: string): string;
var
  Year, Month, Day: Word;
begin
  DecodeSqlDate(Date,Year, Month, Day);
  Day := 1;
  Result := EncodeSqlDate(Year, Month, Day);
end;

{ Define last day of a month }
function LastDay(Month, Year: Word): Word;
begin
  Result := 30;
  case Month of
    1,3,5,7,8,10,12:
      Result := 31;
    2: begin
        Result := 28;
        if (Year mod 4) = 0 then Result := 29;
        if (Year mod 100) = 0 then Result := 28;
        if (Year mod 400) = 0 then Result := 29;
      end;
  end;
end;

{ Define and of a month }
function EndMonth(Date: string): string;
var
  Year, Month, Day: Word;
begin
  DecodeSqlDate(Date,Year, Month, Day);
  Day := LastDay(Month, Year);
  Result := EncodeSqlDate(Year, Month, Day);
end;

{ Define end of a previous month }
function PriorMonth(Date: string): string;
var
  Year, Month, Day: Word;
begin
  DecodeSqlDate(Date,Year, Month, Day);
  Dec(Month);
  if Month <= 0 then
  begin
    Month := 12;
    Dec(Year);
  end;
  Day := LastDay(Month, Year);
  Result := EncodeSqlDate(Year, Month, Day);
end;

{ Define begin of a next month }
function NextMonth(Date: string): string;
var
  Year, Month, Day: Word;
begin
  DecodeSqlDate(Date,Year, Month, Day);
  Inc(Month);
  if Month > 12 then
  begin
    Month := 1;
    Inc(Year);
  end;
  Day := 1;
  Result := EncodeSqlDate(Year, Month, Day);
end;

{ Define previous day }
function PriorDay(Date: string): string;
var
  Year, Month, Day: Word;
begin
  DecodeSqlDate(Date,Year, Month, Day);
  Dec(Day);
  if Day <= 0 then
  begin
    Dec(Month);
    if Month <= 0 then
    begin
      Month := 12;
      Dec(Year);
    end;
    Day := LastDay(Month, Year);
  end;
  Result := EncodeSqlDate(Year, Month, Day);
end;

{ Define next day }
function NextDay(Date: string): string;
var
  Year, Month, Day: Word;
begin
  DecodeSqlDate(Date,Year, Month, Day);
  Inc(Day);
  if Day > LastDay(Month, Year) then
  begin
    Day := 1;
    Inc(Month);
    if Month > 12 then
    begin
      Month := 1;
      Inc(Year);
    end;
  end;
  Result := EncodeSqlDate(Year, Month, Day);
end;

{ Get maximum value }
function Max(A, B: Integer): Integer; assembler;
asm
  CMP EAX,EDX
  JG  @Exit
  MOV EAX,EDX
@Exit:
end;

{ Get minimum value }
function Min(A, B: Integer): Integer; assembler;
asm
  CMP EAX,EDX
  JL  @Exit
  MOV EAX,EDX
@Exit:
end;

{ Sign of value }
function Sgn(Value: Double): Integer;
begin
  if Value < 0 then Result := -1
  else if Value = 0 then Result := 0
  else Result := 1;
end;
(*
{ Get file version number from version resource }
function GetFileVersion(FileName: string): string;
const
  Path: PChar = '\StringFileInfo\040904B0\FileVersion';
var
  Handle, Size: DWORD;
  Buffer: Pointer;
  Length: UINT;
  Version: PChar;
begin
  Result := '';
  Version := nil;
  Size := GetFileVersionInfoSize(PChar(FileName), Handle);
  if Size > 0 then
  begin
    GetMem(Buffer, Size);
    try
      if GetFileVersionInfo(PChar(FileName), Handle, Size, Buffer) then
        if VerQueryValue(Buffer, Path, Pointer(Version), Length) then
          Result := StrPas(Version);
    finally
      FreeMem(Buffer, Size);
    end;
  end;
end;
*)
end.

