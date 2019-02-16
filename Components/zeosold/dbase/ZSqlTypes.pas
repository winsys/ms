{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Extra sql types and structures            }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZSqlTypes;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses SysUtils, Classes, DB {$IFDEF VERCLX}, Variants{$ENDIF};

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Record status type }
  TZRecordType = (ztModified, ztInserted, ztDeleted, ztUnmodified);
  TZUpdateRecordTypes = set of TZRecordType;

  { Supported databases }
  TDatabaseType = (dtMySql, dtPostgreSql, dtInterbase, dtMsSql, dtOracle,
    dtDb2, dtUnknown);

  { Fields for any purposes }
  TFieldList = array[0..MAX_FIELD_COUNT] of Integer;

  { RowId values }
  TRowId = array[0..8] of Byte;

  { Common used storages }
  TIntArray = array[0..0] of Integer;
  PIntArray = ^TIntArray;

  TByteArray = array[0..0] of Byte;
  PByteArray = ^TByteArray;

  TBytes = array[0..1000000] of Byte;
  PBytes = ^TBytes;

  PDateTime = ^TDateTime;
  PTimeStamp = ^TTimeStamp;
  PBoolean = ^Boolean;
  PWordBool = ^WordBool;
  PVoid = ^Pointer;
  PPChar = ^PChar;
  PComp = ^Comp;

  { Sql types }
  TInt64 = packed record
    Data: LongInt;
    Pad: LongInt;
  end;
{$IFNDEF VER100}
  PInt64 = ^Int64;
{$ELSE}
  PInt64 = ^TInt64;
{$ENDIF}

  TBool = Byte;

  { Sql parameters }
  TVarRecArray = array[0..MAX_FIELD_COUNT - 1] of TVarRec;

  { Blob types }
  TBlobType = (btInternal, btExternal);

  { External Blob handle structure }
  TBlobHandle = packed record
    Ptr: LongInt;               // General blob handle
    PtrEx: Cardinal;            // External blob handle (for Interbase)
  end;
  PBlobHandle = ^TBlobHandle;

  { Blob record type }
  TRecordBlob = packed record
    BlobType: TBlobType;        // Blob type (external or internal)
    Handle: TBlobHandle;        // Handle to blob object
    Data: PByteArray;           // Data pointer
    Size: Integer;              // Size of allocated blob
  end;
  PRecordBlob = ^TRecordBlob;

  { General record buffer }
  TRecordData = packed record
    Signature: Byte;            // Signature
    Index: Integer;             // Record index
    RecordType: TZRecordType;    // Record type
    RowId: TRowId;              // RowId value
    BookmarkFlag: TBookmarkFlag; // Bookmark flag
    Bytes: TByteArray;          // Pointer to record contents
  end;
  PRecordData = ^TRecordData;

{******** Extra sql converting functions ***********}

{ Convert bytes to special string }
function BytesToSql(Value: string): string;

{ Convert string to Ansi SQL escaped string }
function StringToSql(Value: string): string;

{ Convert string to Ansi SQL escaped string }
function SqlToString(Value: string): string;

{ Convert varian value into sql value }
//function VarToSqlValue(Value: Variant; DatabaseType: TDatabaseType): string;

{ Convert Ansi Sql date to Interbase date }
function SqlDateToIbDate(Value: string): string;

{ Convert DateTime to Interbase date }
function DateTimeToIbDate(Value: TDateTime): string;

{ Convert Date to Interbase date }
function DateToSqlIbDate(Value: TDateTime): string;

{ Convert sql string to variant value according field type  }
function SqlValueToVariant(Value: string; FieldType: TFieldType;
  DatabaseType: TDatabaseType): Variant;

{ Convert variant value to sql string according field type  }
function VariantToSqlValue(Value: Variant; FieldType: TFieldType;
  DatabaseType: TDatabaseType): string;

{ Convert money values like '$123,456.789' to float }
function MoneyToFloat(Value: string): Double;

{ Convert float value tp money presentation '123456.78' }
function FloatToMoney(Value: Double): string;

implementation

uses ZDBaseConst, ZExtra, ZToken, ZSqlExtra;

{***************** Extra functions implementation ***************}

{ Convert string to escaped Ansi SQL string }
function StringToSql(Value: string): string;
var
  I, Add, Len: Integer;
  Ptr: PChar;
begin
  Add := 0;
  Len := Length(Value);
  for I := 1 to Len do
    if Value[I] in ['''', '"', '\', #26, #10, #13, #0] then
      Inc(Add);
  SetLength(Result, Len + Add);
  Ptr := PChar(Result);
  for I := 1 to Len do
  begin
    if Value[I] in ['''', '"', '\', #26, #10, #13, #0] then
    begin
      Ptr^ := '\';
      Inc(Ptr);
      case Value[I] of
        #26: Ptr^ := 'Z';
        #10: Ptr^ := 'n';
        #13: Ptr^ := 'r';
        #0: Ptr^ := '0';
        else Ptr^ := Value[I];
      end;
    end else
      Ptr^ := Value[I];
    Inc(Ptr);
  end;
end;

{ Convert bytes to special string }
function BytesToSql(Value: string): string;
var
  I: Integer;
begin
  if Value = '' then
  begin
    Result := 'NULL';
    Exit;
  end;

  Result := '0x';
  for I := 1 to Length(Value) do
    Result := Result + IntToHex(Ord(Value[I]), 2);
end;

{ Convert string to escaped Ansi SQL string }
function SqlToString(Value: string): string;
var
  N: Integer;
  Ptr1, Ptr2: PChar;
begin
  SetLength(Result, Length(Value) + 1);
  Ptr1 := PChar(Value);
  Ptr2 := PChar(Result);
  N := 0;
  while Ptr1^ <> #0 do
  begin
    if Ptr1^ <> '\' then
      Ptr2^ := Ptr1^
    else 
    begin
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

{ Convert varian value into sql value }
{
function VarToSqlValue(Value: Variant; DatabaseType: TDatabaseType): string;
begin
  case VarType(Value) of
    varEmpty, varNull:
       Result := 'NULL';
    varSmallint, varInteger, varByte:
       Result := IntToStr(Value);
    varSingle, varDouble, varCurrency:
       Result := FloatToStrEx(VarAsType(Value, varDouble));
    varDate:
       Result := '''' + DateTimeToSqlDate(Value) + '''';
    varBoolean:
      if DatabaseType = dtMySql then
      begin
        if Value then Result := '''Y'''
        else Result := '''N''';
      end
      else
      begin
        if Value then Result := 't'
        else Result := 'f';
      end;
    else
      Result := '''' + StringToSql(VarAsType(Value, varString)) + '''';
  end;
end;
}
//function VarAsFieldType(Value: Variant: FieldType: TFieldType);

{ Convert Ansi Sql date to Interbase date }
function SqlDateToIbDate(Value: string): string;
var
  Date: TDateTime;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  Date := SqlDateToDateTime(Value);
  DecodeDate(Date, Year, Month, Day);
  DecodeTime(Date, Hour, Min, Sec, MSec);
  Result := Format('%2.2d/%2.2d/%4.4d', [Month, Day, Year]);
  if Frac(Date) > 0 then
    Result := Result + Format(' %2.2d:%2.2d:%2.2d', [Hour, Min, Sec]);
end;

{ Convert DateTime to Interbase date }
function DateTimeToIbDate(Value: TDateTime): string;
begin
  if Frac(Value) <> 0 then
    Result := FormatDateTime('mm''/''dd''/''yyyy hh:nn:ss.zzz', Value)
  else
    Result := FormatDateTime('mm''/''dd''/''yyyy', Value);
end;

function DateToSqlIbDate(Value: TDateTime): string;
begin
  Result := FormatDateTime('mm''/''dd''/''yyyy', Value);
end;

{ Convert sql string to variant value according field type  }
function SqlValueToVariant(Value: string; FieldType: TFieldType;
  DatabaseType: TDatabaseType): Variant;
begin
  case FieldType of
{$IFNDEF VER100}
    ftLargeInt:
      Result := Value;
{$ENDIF}
    ftBCD:
      Result := StrToCurr(Value);
    ftInteger, ftSmallInt, ftAutoInc:
      Result := StrToIntDef(Value, 0);
    ftFloat, ftCurrency:
      Result := StrToFloatDefEx(Value, 0);
    ftBoolean:
      Result := (Value <> '') and (Value[1] in ['y', 'Y', 't', 'T']);
    ftTime:
      Result := SqlDateToDateTime(Value);
    ftDate, ftDateTime:
      Result := SqlDateToDateTimeEx(Value);
    else
      Result := Value;
  end;
end;

{ Convert variant value to sql string according field type  }
function VariantToSqlValue(Value: Variant; FieldType: TFieldType;
  DatabaseType: TDatabaseType): string;
begin
  if VarType(Value) in [varEmpty, varNull] then
    Result := 'NULL'
  else try
    case FieldType of
{$IFNDEF VER100}
      ftLargeInt:
        Result := Value;
{$ENDIF}
      ftBCD:
        Result := CurrToStr(Value);
      ftInteger, ftSmallInt, ftAutoInc:
        Result := IntToStr(Value);
      ftFloat:
        Result := FloatToStrEx(Value);
      ftCurrency:
        if DatabaseType = dtPostgreSql then
          Result := FloatToMoney(Value) + '::money'
        else Result := FloatToStrEx(Value);
      ftBoolean:
        if DatabaseType = dtPostgreSql then
        begin
          if Value then Result := 't'
          else Result := 'f';
        end
        else if DatabaseType = dtMsSql then
        begin
          if Value then Result := '1'
          else Result := '0';
        end
        else
        begin
          if Value then Result := 'Y'
          else Result := 'N';
        end;

      ftTime :
        begin
          Result := FormatSqlTime(Value);
        end;
      ftDate :
        begin
          if DatabaseType = dtInterbase then
             Result := DateToSqlIbDate(Value)
          else
             Result := FormatSqlDate(Value);
        end;
      ftDateTime:
        begin
          if DatabaseType = dtInterbase then
             Result := DateTimeToIbDate(Value)
          else
             Result := DateTimeToSqlDateEx(Value);
        end;
      else
        Result := Value;
    end;
  except
    Result := '';
  end;
end;

{ Convert money values like '$123,456.789' to float }
function MoneyToFloat(Value: string): Double;
var
  I: Integer;
begin
  for I := Length(Value) downto 1 do
    if not (Value[I] in ['-', '0'..'9', '.']) then
      Delete(Value, I, 1);
  Result := StrToFloatDefEx(Value, 0);
end;

{ Convert float value tp money presentation '123456.78' }
function FloatToMoney(Value: Double): string;
var
  N: Integer;
begin
  Result := FloatToStrEx(Value);
  N := Pos('.', Result);
  if N > 0 then
    Result := Copy(Result, 1, N + 2);
end;

end.

