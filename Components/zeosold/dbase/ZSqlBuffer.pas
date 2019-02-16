{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                  Sql records buffer                    }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZSqlBuffer;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses Classes, {$IFNDEF LINUX}Windows,{$ENDIF} {$IFDEF VERCLX}Variants,{$ENDIF}
  SysUtils, DB, ZSqlTypes, ZSqlItems, ZList;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Buffer records class }
  TSqlBuffer = class (TZItemList)
  private
    FIsCache: Boolean;
    FBlobCount: Integer;
    FRecBufSize: Integer;
    FLastIndex: Integer;
    FSqlFields: TSqlFields;
    FSqlIndices: TSqlIndices;
    FDataset: TDataset;

    FSortFields: TFieldList;
    FSortFieldCount: Integer;
    FSortType: TSortType;
    FIsSortInverse: Boolean;

    FFilterFields: TFieldList;
    FFilterFieldCount: Integer;
    FFilterTypes: TZUpdateRecordTypes;
    FFilterBuffer: PRecordData;

    function GetItem(Index: Integer): PRecordData;
    function GetRecordSize: Integer;
    function GetRecBufSize: Integer;
    function GetFilterFields(Index: Integer): Integer;
    procedure SetFilterFields(Index: Integer; Value: Integer);
    function GetSortFields(Index: Integer): Integer;
    procedure SetSortFields(Index: Integer; Value: Integer);
  protected
    procedure DoProgress(Stage: Integer; Proc: Integer; Position, Max: Integer);
    procedure UpdateBufferSize;
    function SortRecord(Item1, Item2: Pointer): Integer;
    function FilterRecord(Item: Pointer): Boolean;
  public
    constructor Create(Dataset: TDataset);
    constructor CreateCache(SqlBuffer: TSqlBuffer);
    destructor  Destroy; override;

    procedure SetCache(SqlBuffer: TSqlBuffer);
    procedure SetSort(Fields: string; SortType: TSortType);
    procedure SortInverse;
    procedure SortRestore;
    procedure ClearBuffer(Force: Boolean);

    function Add: PRecordData;
    function Insert(Index: Integer): PRecordData;
    function Delete(Index: Integer): PRecordData;
    function IndexOfIndex(Index: Integer): Integer;
    function SafeIndexOfIndex(Index: Integer): Integer;

    function GetFieldData(FieldDesc: PFieldDesc; Buffer: Pointer;
      RecordData: PRecordData): Boolean;
    procedure SetFieldData(FieldDesc: PFieldDesc; Buffer: Pointer;
      RecordData: PRecordData);
    procedure SetFieldDataLen(FieldDesc: PFieldDesc; Buffer: Pointer;
      RecordData: PRecordData; Length: Integer);

    function GetFieldValue(FieldDesc: PFieldDesc;
      RecordData: PRecordData): Variant;
    procedure SetFieldValue(FieldDesc: PFieldDesc; Value: Variant;
      RecordData: PRecordData);
    function GetField(FieldDesc: PFieldDesc;
      RecordData: PRecordData): string;
    procedure SetField(FieldDesc: PFieldDesc; Value: string;
      RecordData: PRecordData);
    function GetFieldNull(FieldDesc: PFieldDesc;
      RecordData: PRecordData): Boolean;
    procedure SetFieldNull(FieldDesc: PFieldDesc; Value: Boolean;
      RecordData: PRecordData);

    procedure InitRecord(Value: PRecordData);
    procedure CopyRecord(Source, Dest: PRecordData; Force: Boolean);
    procedure FreeRecord(Value: PRecordData; Clear: Boolean);
    procedure BindFields(SqlFields: TSqlFields);
    procedure BindIndices(Indices: TIndexDefs; SqlIndices: TSqlIndices);
    procedure ProcessFieldList(Fields: string; var FieldList: TFieldList;
      var FieldCount: Integer);
    function CompareRecord(Item1, Item2: PRecordData; var FieldList: TFieldList;
      var FieldCount: Integer): Integer;

    property Dataset: TDataset read FDataset write FDataset;
    property Items[Index: Integer]: PRecordData read GetItem; default;
    property SqlFields: TSqlFields read FSqlFields;
    property SqlIndices: TSqlIndices read FSqlIndices;
    property BlobCount: Integer read FBlobCount;
    property RecBufSize: Integer read GetRecBufSize;
    property RecordSize: Integer read GetRecordSize;

    property SortFields[Index: Integer]: Integer read GetSortFields
      write SetSortFields;
    property SortFieldCount: Integer read FSortFieldCount write FSortFieldCount;
    property SortType: TSortType read FSortType write FSortType;
    property IsSortInverse: Boolean read FIsSortInverse write FIsSortInverse;

    property FilterFields[Index: Integer]: Integer read GetFilterFields
      write SetFilterFields;
    property FilterFieldCount: Integer read FFilterFieldCount write FFilterFieldCount;
    property FilterBuffer: PRecordData read FFilterBuffer write FFilterBuffer;
    property FilterTypes: TZUpdateRecordTypes read FFilterTypes write FFilterTypes;
  end;

const
  RecInfoSize = SizeOf(TRecordData) - SizeOf(TByteArray);

implementation

uses ZExtra, ZSqlExtra, ZToken, ZQuery;

{*************** TSqlBuffer class implementation **************}

{ Class constructor }
constructor TSqlBuffer.Create(Dataset: TDataset);
begin
  inherited Create(SizeOf(TRecordData));
  Self.Dataset := Dataset;
  FSqlFields := TSqlFields.Create;
  FSqlIndices := TSqlIndices.Create;
  FIsCache := False;
  FFilterTypes := [ztModified, ztInserted, ztUnmodified];
  OnFilter := FilterRecord;
  OnSort := SortRecord;
end;

{ Class cache constructor }
constructor TSqlBuffer.CreateCache(SqlBuffer: TSqlBuffer);
begin
  inherited Create(SizeOf(TRecordData));
  Self.Dataset := SqlBuffer.Dataset;
  FSqlFields := SqlBuffer.SqlFields;
  FSqlIndices := SqlBuffer.SqlIndices;
  FIsCache := True;
end;


{ Class destructor }
destructor TSqlBuffer.Destroy;
begin
  ClearBuffer(True);
  if not FIsCache then
  begin
    FSqlFields.Free;
    FSqlIndices.Free;
  end;
  inherited Destroy;
end;

{ Update cache data }
procedure TSqlBuffer.SetCache(SqlBuffer: TSqlBuffer);
begin
  if FIsCache then
  begin
    FBlobCount := SqlBuffer.FBlobCount;
    FRecBufSize := SqlBuffer.RecBufSize;
    ItemSize := SqlBuffer.ItemSize;
  end;
end;

{ Get record buffer }
function TSqlBuffer.GetItem(Index: Integer): PRecordData;
begin
  if (Index < 0) or (Index >= Count) then
    Error('List Index Error at %d', Index);
  Result := PRecordData(List^[Index]);
end;

{ Get filter fields value }
function TSqlBuffer.GetFilterFields(Index: Integer): Integer;
begin
  Result := FFilterFields[Index];
end;

{ Set filter fields value }
procedure TSqlBuffer.SetFilterFields(Index, Value: Integer);
begin
  FFilterFields[Index] := Value;
end;

{ Get sort fields value }
function TSqlBuffer.GetSortFields(Index: Integer): Integer;
begin
  Result := FSortFields[Index];
end;

{ Set Sort fields value }
procedure TSqlBuffer.SetSortFields(Index, Value: Integer);
begin
  FSortFields[Index] := Value;
end;

{ Init record buffer }
procedure TSqlBuffer.InitRecord(Value: PRecordData);
begin
  if Assigned(Value) then
  begin
    FillChar(Value.Bytes, RecordSize, 1);
    Value.Signature := 123;
    Value.Index := FLastIndex;
    Value.BookmarkFlag := bfCurrent;
    Value.RecordType := ztUnmodified;
    Inc(FLastIndex);
  end;
end;

{ Copy record buffer }
procedure TSqlBuffer.CopyRecord(Source, Dest: PRecordData; Force: Boolean);
var
  I: Integer;
  SourceBlob, DestBlob: PRecordBlob;
begin
  FreeRecord(Dest, False);
  if Force then
    System.Move(Source^, Dest^, RecBufSize)
  else
    System.Move(Source.Bytes, Dest.Bytes, RecordSize);
  for I := 0 to SqlFields.Count-1 do
    if (SqlFields[I].FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo])
      and (Dest.Bytes[SqlFields[I].Offset] = 0) then
    begin
      SourceBlob := PRecordBlob(@Source.Bytes[SqlFields[I].Offset+1]);
      DestBlob := PRecordBlob(@Dest.Bytes[SqlFields[I].Offset+1]);

      DestBlob.Data := AllocMem(DestBlob.Size);
      System.Move(SourceBlob.Data^, DestBlob.Data^, DestBlob.Size);
   end;
  TZDataset(Dataset).CopyRecord(Self, Source, Dest);
end;

{ Free record buffer }
procedure TSqlBuffer.FreeRecord(Value: PRecordData; Clear: Boolean);
var
  I: Integer;
begin
  TZDataset(Dataset).FreeRecord(Self, Value);
  for I := 0 to SqlFields.Count-1 do
  begin
    if (SqlFields[I].FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo])
      and (Value.Bytes[SqlFields[I].Offset] = 0) then
    begin
      FreeMem(PRecordBlob(@Value.Bytes[SqlFields[I].Offset+1]).Data);
    end;
    if Clear then
      Value.Bytes[SqlFields[I].Offset] := 1;
  end;
end;

{ Add new item }
function TSqlBuffer.Add: PRecordData;
begin
  Result := PRecordData(inherited Add);
  InitRecord(Result);
end;

{ Delete item }
function TSqlBuffer.Delete(Index: Integer): PRecordData;
begin
  if (Index < 0) or (Index >= Count) then
    Error('List Index Error at %d', Index);
  FreeRecord(PRecordData(List^[Index]), False);
  Result := PRecordData(inherited Delete(Index));
end;

{ Clear all buffer }
procedure TSqlBuffer.ClearBuffer(Force: Boolean);
var
  I: Integer;
  Ptr: Pointer;
begin
  { Clear all records }
  if BlobCount > 0 then
  begin
    { Invoke on progress event }
    DoProgress(Ord(psStarting), Ord(ppClosing), 0, FillCount);
    { Free blob buffers }
    for I := 0 to FillCount-1 do
    begin
      Ptr := FillList^[I];
      if PByte(Ptr)^ <> ITEM_DELETED then
        FreeRecord(PRecordData(LongInt(Ptr) + 1), False);
      { Invoke on progress event }
      DoProgress(Ord(psRunning), Ord(ppClosing), I+1, FillCount);
    end;
    { Invoke on progress event }
    DoProgress(Ord(psEnding), Ord(ppClosing), FillCount, FillCount);
  end;
  { Free allocated memory }
  inherited Clear;
  FLastIndex := 0;
  { Clear in full mode }
  if Force then
  begin
    { Free filter buffer }
    if Assigned(FFilterBuffer) then
      FreeRecord(FFilterBuffer, False);
    FreeMem(FFilterBuffer);
    FFilterBuffer := nil;
    { Zero variables }
    FBlobCount := 0;
    FRecBufSize := 0;
    FSortFieldCount := 0;
    FFilterFieldCount := 0;
    FIsSortInverse := False;
    { Clear fields and indices }
    SqlFields.Clear;
    SqlIndices.Clear;
  end;
end;

{ Insert new record }
function TSqlBuffer.Insert(Index: Integer): PRecordData;
begin
  Result := PRecordData(inherited Insert(Index));
  InitRecord(Result);
end;

{ Update record buffer lengths }
procedure TSqlBuffer.UpdateBufferSize;
var
  I: Integer;
begin
  FRecBufSize := 0;
  FBlobCount := 0;
  for I := 0 to SqlFields.Count - 1 do
    with SqlFields.Items[I]^ do
    begin
      if not Assigned(FieldObj) then
        DatabaseError('Fatal internal error');
      Offset := FRecBufSize;
      DataSize := FieldObj.DataSize;
      FieldType := FieldObj.DataType;
      FieldNo := FieldObj.FieldNo - 1;
      if FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo] then
      begin
        DataSize := SizeOf(TRecordBlob);
        Inc(FBlobCount);
      end;

      Inc(FRecBufSize, Max(DataSize, 4) + 1);
    end;
  Inc(FRecBufSize, RecInfoSize {!!!});
  ReallocMem(FFilterBuffer, FRecBufSize);
  InitRecord(FFilterBuffer);
end;

{ Get record size }
function TSqlBuffer.GetRecordSize: Integer;
begin
  Result := FRecBufSize - RecInfoSize {!!!};
end;

{ Get record buffer size }
function TSqlBuffer.GetRecBufSize: Integer;
begin
  Result := FRecBufSize
end;

{ Setup all fields }
procedure TSqlBuffer.BindFields(SqlFields: TSqlFields);
var
  I: Integer;
  FieldDesc: PFieldDesc;
  NewFieldDesc: PFieldDesc;
begin
  Self.SqlFields.Clear;
  Self.Clear;
  for I := 0 to Dataset.FieldCount-1 do
  begin
    FieldDesc := SqlFields.FindByAlias(Dataset.Fields[I].FieldName);
    if Assigned(FieldDesc) then
    begin
      NewFieldDesc := Self.SqlFields.AddDesc(FieldDesc);
      NewFieldDesc.FieldObj := Dataset.Fields[I];
    end else
      Self.SqlFields.AddField(Dataset.Fields[I]);
  end;

  UpdateBufferSize;
  ItemSize := RecBufSize + 1;
end;

{ Setup all indices }
procedure TSqlBuffer.BindIndices(Indices: TIndexDefs; SqlIndices: TSqlIndices);
var
  I, J: Integer;
  FieldCount: Integer;
  IndexDesc: PIndexDesc;
  NewIndexDesc: PIndexDesc;
begin
  Self.SqlIndices.Clear;
  for I := 0 to SqlIndices.Count - 1 do
  begin
    FieldCount := 0;
    IndexDesc := SqlIndices[I];
    NewIndexDesc := Self.SqlIndices.Add;
    NewIndexDesc.Table := IndexDesc.Table;
    NewIndexDesc.Name := IndexDesc.Name;
    NewIndexDesc.KeyType := IndexDesc.KeyType;
    NewIndexDesc.SortType := IndexDesc.SortType;
    for J := 0 to IndexDesc.FieldCount - 1 do
    begin
      if Assigned(SqlFields.FindByName(IndexDesc.Table, IndexDesc.Fields[J])) then
      begin
        NewIndexDesc.Fields[FieldCount] := IndexDesc.Fields[J];
        Inc(FieldCount);
      end else
        NewIndexDesc.KeyType := ktIndex;
    end;
    if FieldCount > 0 then
      NewIndexDesc.FieldCount := FieldCount
    else
      Self.SqlIndices.Remove(NewIndexDesc);
  end;
end;

{ Find field with index }
function TSqlBuffer.IndexOfIndex(Index: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (Index >= 0) and (Index < Count) then
    if Index = Items[Index].Index then
    begin
      Result := Index;
      Exit;
    end;
  for I := 0 to Count - 1 do
    if Index = Items[I].Index then
    begin
      Result := I;
      Exit;
    end;
end;

{ Find item, move it into list and return its index }
function TSqlBuffer.SafeIndexOfIndex(Index: Integer): Integer;
var
  I: Integer;
  Ptr: PChar;
begin
  { Try to find visible record }
  Result := IndexOfIndex(Index);
  if Result >= 0 then Exit;
  { Try to find hidden record }
  for I := 0 to FillCount-1 do
  begin
    Ptr := FillList^[I];
    if (PByte(Ptr)^ = ITEM_FILTERED) and (PRecordData(Ptr + 1).Index = Index) then
    begin
      Result := Count;
      Count := Count + 1;
      List^[Result] := Ptr + 1;
      Exit;
    end;
  end;
end;

{ Get field from internal buffer }
function TSqlBuffer.GetFieldData(FieldDesc: PFieldDesc; Buffer: Pointer;
  RecordData: PRecordData): Boolean;
begin
  Result := False;
  if RecordData.Bytes[FieldDesc.Offset] = 0 then
  begin
    if Buffer <> nil then
      System.Move(RecordData.Bytes[FieldDesc.Offset + 1], Buffer^, FieldDesc.DataSize);
    Result := True;
  end;
end;

{ Set field to internal buffer }
procedure TSqlBuffer.SetFieldData(FieldDesc: PFieldDesc; Buffer: Pointer;
  RecordData: PRecordData);
begin
  if Buffer = nil then
    RecordData.Bytes[FieldDesc.Offset] := 1
  else
  begin
    RecordData.Bytes[FieldDesc.Offset] := 0;
    System.Move(Buffer^, RecordData.Bytes[FieldDesc.Offset + 1], FieldDesc.DataSize);
  end;
end;

{ Set field to internal buffer with specified length }
procedure TSqlBuffer.SetFieldDataLen(FieldDesc: PFieldDesc; Buffer: Pointer;
  RecordData: PRecordData; Length: Integer);
begin
  if Buffer = nil then
    RecordData.Bytes[FieldDesc.Offset] := 1
  else begin
    RecordData.Bytes[FieldDesc.Offset] := 0;
    System.Move(Buffer^, RecordData.Bytes[FieldDesc.Offset + 1],
      Min(FieldDesc.DataSize, Length));
    if Length < FieldDesc.DataSize then
      RecordData.Bytes[FieldDesc.Offset + 1 + Length] := 0;
  end;
end;

{ Get field value as Variant }
function TSqlBuffer.GetFieldValue(FieldDesc: PFieldDesc;
  RecordData: PRecordData): Variant;
var
  Buffer: Pointer;
  TempStamp: TTimeStamp;
  Dataset: TZDataset;
begin
  Dataset := TZDataset(Self.Dataset);
  Result := Null;
  if RecordData = nil then Exit;
  Buffer := @RecordData.Bytes[FieldDesc.Offset + 1];
  if RecordData.Bytes[FieldDesc.Offset] = 0 then
    case FieldDesc.FieldType of
      ftString:
        Result := StrPas(PChar(Buffer));
{$IFNDEF VER100}
      ftLargeInt:
      {$IFDEF VERCLX}
        Result := PInt64(Buffer)^;
      {$ELSE}
        Result := IntToStr(PInt64(Buffer)^);
      {$ENDIF}
{$ENDIF}
      ftBCD:
        Result := PCurrency(Buffer)^;
      ftInteger, ftAutoInc:
        Result := PLongInt(Buffer)^;
      ftSmallInt:
        Result := PSmallInt(Buffer)^;
      ftFloat, ftCurrency:
        Result := PDouble(Buffer)^;
      ftBoolean:
        Result := PWordBool(Buffer)^;
      ftTime:
        begin
          TempStamp.Time := PLongInt(Buffer)^;
          TempStamp.Date := DateDelta;
          Result := TimeStampToDateTime(TempStamp);
        end;
      ftDateTime:
        begin
          Result := TimeStampToDateTime(MSecsToTimeStamp(PDateTime(Buffer)^));
          Result := TDateTime(Result + Sgn(Result) * 0.5E-6);
        end;
      ftDate:
        begin
          TempStamp.Time := 0;
          TempStamp.Date := PLongInt(Buffer)^;
          Result := TimeStampToDateTime(TempStamp);
        end;
{
      ftArray:
        begin
          Result := MemPas(Buffer,FieldDesc^.DataSize);
        end;
}
      ftMemo:
        Result := MemPas(PChar(PRecordBlob(Buffer).Data), PRecordBlob(Buffer).Size);
      ftBlob:
        if Dataset.DatabaseType = dtPostgreSql then
          Result := PRecordBlob(Buffer).Handle.Ptr
        else
          Result := MemPas(PChar(PRecordBlob(Buffer).Data), PRecordBlob(Buffer).Size);
    end;
end;

{ Set field value as Variant }
procedure TSqlBuffer.SetFieldValue(FieldDesc: PFieldDesc; Value: Variant;
  RecordData: PRecordData);
var
  Buffer: array[0..MAX_STRING_SIZE] of Char;
  RecordBlob: PRecordBlob;
begin
  if VarType(Value) in [varEmpty, varNull] then
    SetFieldData(FieldDesc, nil, RecordData)
  else try
    case FieldDesc.FieldType of
      ftString:
        begin
          StrPLCopy(Buffer, Value, FieldDesc.DataSize - 1);
          Buffer[FieldDesc.DataSize - 1] := #0;
        end;
{$IFNDEF VER100}
      ftLargeInt:
      {$IFDEF VERCLX}
        PInt64(@Buffer)^ := Value;
      {$ELSE}
        PInt64(@Buffer)^ := StrToInt64Def(Value, 0);
      {$ENDIF}
{$ENDIF}
      ftBCD:
        PCurrency(@Buffer)^ := Value;
      ftInteger, ftAutoInc:
        PLongInt(@Buffer)^ := Value;
      ftSmallInt:
        PSmallInt(@Buffer)^ := Value;
      ftFloat, ftCurrency:
        PDouble(@Buffer)^ := Value;
      ftBoolean:
        PWordBool(@Buffer)^ := Value;
      ftDate:
        PLongInt(@Buffer)^ := DateTimeToTimeStamp(VarAsType(Value, varDate)).Date;
      ftTime:
        PLongInt(@Buffer)^ := DateTimeToTimeStamp(VarAsType(Value, varDate)).Time;
      ftDateTime:
        PDateTime(@Buffer)^ := TimeStampToMSecs(DateTimeToTimeStamp(
          VarAsType(Value, varDate)));
      ftMemo, ftBlob:
        begin
          RecordBlob := PRecordBlob(@RecordData.Bytes[FieldDesc.Offset + 1]);
          RecordBlob.Size := Length(VarAsType(Value, varString)) + 1;
          if (RecordData.Bytes[FieldDesc.Offset] = 0) then
            ReallocMem(RecordBlob.Data, RecordBlob.Size)
          else
            RecordBlob.Data := AllocMem(RecordBlob.Size);
          StrPCopy(PChar(RecordBlob.Data), VarAsType(Value, varString));
          RecordBlob.Data[RecordBlob.Size] := 0;
        end;
      else
        Exit;
    end;

    RecordData.Bytes[FieldDesc.Offset] := 0;
    System.Move(Buffer, RecordData.Bytes[FieldDesc.Offset + 1], FieldDesc.DataSize);
  except
    RecordData.Bytes[FieldDesc.Offset] := 1;
  end;
end;

{ Get field value as String }
function TSqlBuffer.GetField(FieldDesc: PFieldDesc;
  RecordData: PRecordData): string;
begin
  if RecordData <> nil then
    Result := VariantToSqlValue(GetFieldValue(FieldDesc, RecordData),
      FieldDesc.FieldType, TZDataset(Dataset).DatabaseType)
  else
    Result := Null;
end;

{ Set field value as String }
procedure TSqlBuffer.SetField(FieldDesc: PFieldDesc; Value: string;
  RecordData: PRecordData);
begin
  SetFieldValue(FieldDesc, SqlValueToVariant(Value, FieldDesc.FieldType,
    TZDataset(Dataset).DatabaseType), RecordData);
end;

{ Get is field null }
function TSqlBuffer.GetFieldNull(FieldDesc: PFieldDesc;
  RecordData: PRecordData): Boolean;
begin
  Result := (RecordData = nil) or (RecordData.Bytes[FieldDesc.Offset] = 1);
end;

{ Set field null flag }
procedure TSqlBuffer.SetFieldNull(FieldDesc: PFieldDesc; Value: Boolean;
  RecordData: PRecordData);
begin
  if Value then
    RecordData.Bytes[FieldDesc.Offset] := 1
  else
    RecordData.Bytes[FieldDesc.Offset] := 0;
end;

{ Filter records }
function TSqlBuffer.FilterRecord(Item: Pointer): Boolean;
begin
  Result := False;
  { Check field types }
  if not (PRecordData(Item).RecordType in FilterTypes) then
    Exit;
  { Check master-detail link }
  if (FilterFieldCount > 0) and (CompareRecord(PRecordData(Item), FilterBuffer,
    FFilterFields, FFilterFieldCount) <> 0) then
    Exit;
  Result := True;
end;

{ Extract field from string and put field number into list }
procedure TSqlBuffer.ProcessFieldList(Fields: string;
  var FieldList: TFieldList; var FieldCount: Integer);
var
  I: Integer;
  Field: string;
  Found: Boolean;
begin
  FieldCount := 0;
  while (Fields <> '') and (FieldCount < MAX_FIELD_COUNT) do
  begin
    Field := UpperCase(Trim(StrTokEx(Fields,',;')));
    if (Field <> '') and IsDigit(Field[1]) then
    begin
      I := StrToIntDef(Field, -1);
      if (I >= 0) or (I < SqlFields.Count) then
      begin
        FieldList[FieldCount] := I;
        Inc(FieldCount);
      end else
        DatabaseError(Format('Unknown field %s', [Field]));
    end
    else
    begin
      DeleteQuotesEx(Field);
      Found := False;
      for I := 0 to SqlFields.Count-1 do
        if StrCaseCmp(SqlFields[I].Alias, Field) then
        begin
          Found := True;
          FieldList[FieldCount] := I;
          Inc(FieldCount);
          Break;
        end;
      if not Found then
        DatabaseError(Format('Unknown field %s', [Field]));
    end;
  end;
end;

{ Compare two records according field list }
function TSqlBuffer.CompareRecord(Item1, Item2: PRecordData;
  var FieldList: TFieldList; var FieldCount: Integer): Integer;
var
  I: Integer;
  FieldDesc: PFieldDesc;
  Value1, Value2: Pointer;
  DoubleRes: Double;
  CurrRes: System.Currency;
begin
  Result := 0;
  for I := 0 to FieldCount - 1 do
  begin
    FieldDesc := SqlFields[FieldList[I]];
    { Check null fields }
    if PRecordData(Item1).Bytes[FieldDesc.Offset] = 1 then
      Result := -1;
    if PRecordData(Item2).Bytes[FieldDesc.Offset] = 1 then
    begin
      Inc(Result);
      Break;
    end;
    if Result <> 0 then Break;
    { Obtain field buffers }
    Value1 := @PRecordData(Item1).Bytes[FieldDesc.Offset + 1];
    Value2 := @PRecordData(Item2).Bytes[FieldDesc.Offset + 1];
    { Process compation }
    case FieldDesc.FieldType of
      ftString:
        Result := AnsiStrComp(Value1, Value2);
{$IFNDEF VER100}
      ftLargeInt:
        Result := PInt64(Value1)^ - PInt64(Value2)^;
{$ENDIF}
      ftBCD:
        begin
          CurrRes := PCurrency(Value1)^ - PCurrency(Value2)^;
          if CurrRes < 0 then Result := -1
          else if CurrRes > 0 then Result := 1
          else Result := 0;
        end;
      ftInteger, ftAutoInc:
        Result := PInteger(Value1)^ - PInteger(Value2)^;
      ftSmallInt:
        Result := PSmallInt(Value1)^ - PSmallInt(Value2)^;
      ftFloat, ftCurrency:
        begin
          DoubleRes := PDouble(Value1)^ - PDouble(Value2)^;
          if DoubleRes < 0 then Result := -1
          else if DoubleRes > 0 then Result := 1
          else Result := 0;
        end;
      ftBoolean:
        Result := PByte(Value1)^ - PByte(Value2)^;
      ftTime:
        Result := PLongInt(Value1)^ - PLongInt(Value2)^;
      ftDateTime:
        begin
          DoubleRes := PComp(Value1)^ - PComp(Value2)^;
          if DoubleRes < 0 then Result := -1
          else if DoubleRes > 0 then Result := 1
          else Result := 0;
        end;
      ftDate:
        Result := PLongInt(Value1)^ - PLongInt(Value2)^;
    end;
  end;
  { Correct result according sorting type }
  if SortType = stDescending then
    Result := -Result;
end;

{ Sort records }
function TSqlBuffer.SortRecord(Item1, Item2: Pointer): Integer;
begin
  Result := CompareRecord(PRecordData(Item1), PRecordData(Item2),
    FSortFields, FSortFieldCount);
end;

{ Set sorting fields }
procedure TSqlBuffer.SetSort(Fields: string; SortType: TSortType);
begin
  FSortType := SortType;
  FIsSortInverse := False;
  ProcessFieldList(Fields, FSortFields, FSortFieldCount);
  if SortFieldCount > 0 then Sort
  else ClearSort;
end;

{ Inverse sorting fields }
procedure TSqlBuffer.SortInverse;
var
  Index1, Index2: Integer;
begin
  Index1 := 0;
  Index2 := Count - 1;
  while (Index2 - Index1) > 1 do
  begin
    Exchange(Index1, Index2);
    Inc(Index1);
    Dec(Index2);
  end;
  IsSortInverse := not IsSortInverse;
end;

{ Record sorting }
procedure TSqlBuffer.SortRestore;
begin
  if SortFieldCount > 0 then
    Sort;
  if IsSortInverse then
  begin
    SortInverse;
    IsSortInverse := True;
  end;
end;

{ Invoke a progress event }
procedure TSqlBuffer.DoProgress(Stage: Integer; Proc: Integer; Position, Max: Integer);
var
  Dataset: TZDataset;
  Cancel: Boolean;
begin
  Dataset := TZDataset(Self.Dataset);
  if Assigned(Dataset.OnProgress) then
  begin
    Cancel := False;
    Dataset.OnProgress(Dataset, TZProgressStage(Stage), TZProgressProc(Proc),
      Position, Max, Cancel);
  end;
end;

end.