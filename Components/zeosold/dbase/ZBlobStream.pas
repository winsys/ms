{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                  Blob streams classes                  }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZBlobStream;

interface

uses Classes, SysUtils, DB, DBCommon, ZSqlTypes, ZDirSql, ZSqlItems;

{$INCLUDE ../Zeos.inc}

type
  { Class for memo blobs support }
  TZMemoStream = class(TStream)
  protected
    FFieldDesc: PFieldDesc;
    FDataSet: TDataset;
    FRecordData: PRecordData;
    FRecordBlob: PRecordBlob;
    FMode: TBlobStreamMode;
    FOpened: Boolean;
    FModified: Boolean;
    FPosition: LongInt;

    function GetBlobSize: LongInt; virtual;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;

    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    procedure Truncate; virtual;
  end;

  { Class for blobs support }
  TZBlobStream = class(TZMemoStream)
  protected
    FBlob: TDirBlob;

    procedure CopyBlob;
    procedure DuplicateBlob(CopyRecordData: PRecordData);
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode; Blob: TDirBlob);
    destructor Destroy; override;
  end;

{ Read blob field }
procedure ReadBlob(Blob: TDirBlob; var Buffer: PChar; var Size: LongInt);

{ Create blob field }
procedure CreateBlob(Blob: TDirBlob; Buffer: PChar; Size: LongInt);

{ Write blob field }
procedure DeleteBlob(Blob: TDirBlob);

{ Write blob field }
procedure WriteBlob(Blob: TDirBlob; Buffer: PChar; Size: LongInt);

implementation

uses ZQuery, ZDBaseConst, ZExtra
  {$IFNDEF LINUX}, Windows, Forms {$ELSE}, QForms{$ENDIF};

{*************** TZMemoStream implementation ****************}

{ Class constructor }
constructor TZMemoStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
var
  Dataset: TZDataset;
begin
  { Set start values }
  FMode := Mode;
  FDataset := Field.Dataset;
  Dataset := TZDataset(FDataset);
  FFieldDesc := Dataset.SqlBuffer.SqlFields.FindByField(Field);
  { Check field desc }
  if FFieldDesc = nil then
    DatabaseError('Internal fatal error');
  { Get and check buffer }
  if not Dataset.GetActiveRecBuf(FRecordData) then
    Exit;
  { Get and check blob }
  FRecordBlob := PRecordBlob(@FRecordData.Bytes[FFieldDesc.Offset+1]);
  { Check field state }
  if not Field.Modified then
  begin
    if Mode <> bmRead then
      if not (Dataset.State in [dsEdit, dsInsert]) then
        DatabaseError(SNotInsertMode);
  end;
  { Set properties }
  FOpened := True;
  FModified := False;
  { Truncate if blob writing }
  if Mode = bmWrite then Truncate;
end;

{ Class destructor }
destructor TZMemoStream.Destroy;
begin
  if FModified then
  begin
    if FOpened then
      (FFieldDesc.FieldObj as TBlobField).Modified := True;
    try
      (FDataSet as TZDataset).DataEvent(deFieldChange, LongInt(FFieldDesc.FieldObj));
    except
      Application.HandleException(Self);
    end;
  end;
end;

{ Read from blob into the buffer }
function TZMemoStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := 0;
  if FOpened and (FRecordData.Bytes[FFieldDesc.Offset] = 0) then
  begin
    { Recount size }
    if Count > Size-FPosition then Result := Size-FPosition
    else Result := Count;
    { Copy data from blob to buffer }
    if Result > 0 then
    begin
      Move(FRecordBlob.Data^[FPosition], Buffer, Result);
      Inc(FPosition, Result);
    end;
  end;
end;

{ Write from buffer into the blob  }
function TZMemoStream.Write(const Buffer; Count: LongInt): LongInt;
var
  NewSize: Integer;
begin
  Result := 0;
  if FOpened then
  begin
    { Set not null flag }
    FRecordData.Bytes[FFieldDesc.Offset] := 0;
    { Recount size }
    NewSize := FPosition + Count;
    if NewSize < FRecordBlob.Size then
      NewSize := FRecordBlob.Size;
    { Realloc blob }
    if NewSize > FRecordBlob.Size then
    begin
      ReallocMem(FRecordBlob.Data, NewSize);
      FRecordBlob.Size := NewSize;
    end;
    { Move data to blob }
    Move(Buffer, FRecordBlob.Data^[FPosition], Count);
    Inc(FPosition, Count);
    { Set flags }
    FModified := True;
    Result := Count;
  end;
end;

{ Seek the position into the blob }
function TZMemoStream.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  case Origin of
    0: FPosition := Offset;
    1: Inc(FPosition, Offset);
    2: FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

{ Truncate the blob }
procedure TZMemoStream.Truncate;
begin
  if FOpened and (FRecordData.Bytes[FFieldDesc.Offset] = 0) then
  begin
    FPosition := 0;
    FRecordData.Bytes[FFieldDesc.Offset] := 1;
    FreeMem(FRecordBlob.Data);
    FRecordBlob.Data := nil;
    FRecordBlob.Size := 0;
    FModified := True;
  end;
end;

{ Get the blob size }
function TZMemoStream.GetBlobSize: LongInt;
begin
  if FOpened and (FRecordData.Bytes[FFieldDesc.Offset] = 0) then
    Result := FRecordBlob.Size
  else
    Result := 0;
end;

{********************* Blob processing functions ***************}

const
  MAX_PART_SIZE = 1024;

{ Read blob field }
procedure ReadBlob(Blob: TDirBlob; var Buffer: PChar; var Size: LongInt);
var
  N, L: LongInt;
begin
  Buffer := nil;
  Size := 0;
  if not Assigned(Blob) then Exit;
  if (Blob.Handle.Ptr = 0) and (Blob.Handle.PtrEx = 0) then Exit;

  try
    Blob.Open(fmOpenRead);
    if Blob.Status = bsOk then
    begin
      L := 0;
      repeat
        ReallocMem(Buffer, L + MAX_PART_SIZE);
        N := Blob.Read(Buffer + L, MAX_PART_SIZE);
        Inc(L, N);
      until N < MAX_PART_SIZE;
      ReallocMem(Buffer, L);
      Size := L;
    end else
      DatabaseError(SReadBlobError);
  finally
    Blob.Close;
  end;
end;

{ Create blob field }
procedure CreateBlob(Blob: TDirBlob; Buffer: PChar; Size: LongInt);
var
  L, N: Integer;
begin
  if (Buffer = nil) or (Size = 0) then Exit;
  if not Assigned(Blob) then Exit;
//  if (Blob.Handle.Ptr <> 0) or (Blob.Handle.PtrEx <> 0) then Exit;

  try
    Blob.CreateBlob;
    if Blob.Status = bsOk then
    begin
      L := 0;
      repeat
        N := Min(MAX_PART_SIZE, Size - L);
        Blob.Write(Buffer + L, N);
        Inc(L, N);
      until (L >= Size);
    end else
      DatabaseError(SCreateBlobError);
  finally
    Blob.Close;
  end;
end;

{ Write blob field }
procedure DeleteBlob(Blob: TDirBlob);
begin
  if not Assigned(Blob) then Exit;
  if (Blob.Handle.Ptr = 0) and (Blob.Handle.PtrEx = 0) then Exit;

  try
    Blob.DropBlob;
    if Blob.Status <> bsOk then
      DatabaseError(SDropBlobError);
  finally
    Blob.Close;
  end;
end;

{ Write blob field }
procedure WriteBlob(Blob: TDirBlob; Buffer: PChar; Size: LongInt);
begin
  if not Assigned(Blob) then Exit;

  if (Blob.Handle.Ptr = 0) and (Blob.Handle.PtrEx = 0) then
    CreateBlob(Blob, Buffer, Size)
  else begin
    DeleteBlob(Blob);
    if (Buffer <> nil) and (Size <> 0) then
      CreateBlob(Blob, Buffer, Size);
  end;
end;

{*************** TZBlobStream implementation ****************}

{ Class constructor }
constructor TZBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode;
  Blob: TDirBlob);
begin
  { Create with previous constructor }
  inherited Create(Field, Mode);
  { Read contents from blob }
  FBlob := Blob;
  if (Mode <> bmWrite) and (FRecordBlob.Data = nil) then
  begin
    ReadBlob(FBlob, PChar(FRecordBlob.Data), FRecordBlob.Size);
    CopyBlob;
    FRecordBlob.Handle := FBlob.Handle;
  end;
end;

{ Class destructor }
destructor TZBlobStream.Destroy;
begin
  if FModified then
  begin
    if FOpened then
    begin
      WriteBlob(FBlob, PChar(FRecordBlob.Data), FRecordBlob.Size);
      FRecordBlob.Handle := FBlob.Handle;
      (FFieldDesc.FieldObj as TBlobField).Modified := True;
    end;
    try
      (FDataSet as TZDataset).DataEvent(deFieldChange, LongInt(FFieldDesc.FieldObj));
    except
      Application.HandleException(Self);
    end;
  end;
  FBlob.Free;
end;

{ Copy read blob into other storages }
procedure TZBlobStream.CopyBlob;
var
  N, M: Integer;
begin
  with TZDataset(FDataset) do
  begin
    N := RecNo - 1;
    if (N >= 0) and (N < SqlBuffer.Count) and (FRecordBlob.Size > 0) then
    begin
      DuplicateBlob(SqlBuffer[N]);
      M := CacheBuffer.IndexOfIndex(SqlBuffer[N].Index);
      if M >= 0 then
        DuplicateBlob(CacheBuffer[M]);
    end;
  end;
end;

{ Copy read blob into other storages }
procedure TZBlobStream.DuplicateBlob(CopyRecordData: PRecordData);
var
  CopyRecordBlob: PRecordBlob;
begin
  CopyRecordBlob := PRecordBlob(@CopyRecordData.Bytes[FFieldDesc.Offset+1]);
  CopyRecordBlob.Size := FRecordBlob.Size;
  CopyRecordBlob.Data := AllocMem(CopyRecordBlob.Size);
  System.Move(FRecordBlob.Data^, CopyRecordBlob.Data^, CopyRecordBlob.Size);
end;

end.
