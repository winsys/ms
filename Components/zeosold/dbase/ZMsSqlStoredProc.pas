{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                MS SQL property editors                 }
{                                                        }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZMsSqlStoredProc;
//
// Todo: Alles moet netter
// Todo: Kijken of alle buffers goed gezet worden
// Todo: Kijken of alle fielddefs onthouden c.q. weggegooid worden
// Todo: Field editor creert de velden niet
interface

{$R *.dcr}

uses
  SysUtils, Windows, Db, Classes, ZDirSql, ZDirMsSql, DbCommon,
  ZMsSqlCon, ZMsSqlTr, ZToken, ZLibMsSql, ZSqlExtra, ZQuery,
  ZSqlTypes, ZSqlItems, ZStoredProc;

{$INCLUDE ..\Zeos.inc}

type
  TZCustomMsSqlStoredProc = class(TZStoredProc)
  private
    FMustClose: Boolean;

    function GetDatabase: TZMsSqlDatabase;
    function GetTransact: TZMsSqlTransact;
    procedure SetDatabase(const Value: TZMsSqlDatabase);
    procedure SetTransact(const Value: TZMsSqlTransact);
  protected
    procedure QueryRecord; override;
    procedure GetAllParams(const spName: String); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExecProc; override;

    procedure AddTableFields(Table: string; SqlFields: TSqlFields);override;
    procedure AddTableIndices(Table: string; SqlFields: TSqlFields;
      SqlIndices: TSqlIndices); override;
  published
    property Database: TZMsSqlDatabase read GetDatabase write SetDatabase;
    property Transaction: TZMsSqlTransact read GetTransact write SetTransact;
  end;

  TZMsSqlStoredProc = class(TZCustomMsSqlStoredProc)
  published
    property Active;
    property Database;
    property Transaction;
    property StoredProcName;
    property Params;
    property ParamBindMode;
  end;

implementation

uses ZExtra, ZDBaseConst, ZBlobStream, Math;

procedure TZCustomMsSqlStoredProc.AddTableFields(Table: string;
  SqlFields: TSqlFields);
begin
//
end;

procedure TZCustomMsSqlStoredProc.AddTableIndices(Table: string;
  SqlFields: TSqlFields; SqlIndices: TSqlIndices);
begin
//
end;

constructor TZCustomMsSqlStoredProc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  Query := TDirMsSqlQuery.Create(nil, nil);
  StoredProc := TDirMsSqlStoredProc.Create(nil,nil);
  DatabaseType := dtMsSql;
end;

destructor TZCustomMsSqlStoredProc.Destroy;
begin
  inherited;
end;

procedure TZCustomMsSqlStoredProc.ExecProc;
begin
  FMustClose := false;

  inherited ExecProc;

  if FMustClose then
    StoredProc.Close;
end;

procedure TZCustomMsSqlStoredProc.GetAllParams(const spName: String);
var
  AQuery: TDirMsSqlQuery;
  WasConnected: Boolean;
begin
  if (spName<>'') and assigned(Database) and assigned(Database.Handle)
    and assigned(Transaction) and assigned(Transaction.Handle) then
{  begin
    WasConnected := Database.Connected;
    Database.Connect;
    Transaction.Connect;
    StoredProc.ShowParams(spName);
    while not StoredProc.Eof do
    begin
        Params.CreateParam(MsSqlToDelphiTypeDesc(StoredProc.FieldByName('TYPE_NAME')),
                           StoredProc.FieldByName('COLUMN_NAME'),
                           MsSqlToDelphiParamType(StrToInt(StoredProc.FieldByName('COLUMN_TYPE'))));
        StoredProc.Next;
    end;
    StoredProc.Close;
    if not wasConnected then
      Database.Disconnect;
  end;
}
  begin
    AQuery := TDirMsSqlQuery.Create(TDirMsSqlConnect(Database.Handle),
                                    TDirMsSqlTransact(Transaction.Handle));
    try
      WasConnected := Database.Connected;
      Database.Connect;
      AQuery.Sql := 'sp_sproc_columns '''+StoredProcName+'''';
      AQuery.Open;
      Params.Clear;
      while not AQuery.Eof do
      begin
        Params.CreateParam(MsSqlToDelphiTypeDesc(AQuery.FieldByName('TYPE_NAME')),
                           AQuery.FieldByName('COLUMN_NAME'),
                           MsSqlToDelphiParamType(StrToInt(AQuery.FieldByName('COLUMN_TYPE'))));
        AQuery.Next;
      end;
      AQuery.Close;
      if not wasConnected then
        Database.Disconnect;
    finally
      AQuery.Free;
    end;
  end;
end;

function TZCustomMsSqlStoredProc.GetDatabase: TZMsSqlDatabase;
begin
  Result := TZMsSqlDatabase(DatabaseObj);
end;

function TZCustomMsSqlStoredProc.GetTransact: TZMsSqlTransact;
begin
  Result := TZMsSqlTransact(TransactObj);
end;

procedure TZCustomMsSqlStoredProc.QueryRecord;
var
  I, Count: Integer;
  RecordData: PRecordData;
  FieldDesc: PFieldDesc;
  TempSmall: SmallInt;
  TempDouble: Double;
  TempBool: WordBool;
  TempDate: DBDATETIME;
  DateRec: DBDATEREC;
  TempTime: TDateTime;
  TimeStamp: TTimeStamp;
  BlobPtr: PRecordBlob;
  Cancel: Boolean;
begin
  Count := SqlBuffer.Count;
  while not StoredProc.Eof and (Count = SqlBuffer.Count) do
  begin
    { Go to the record }
    if SqlBuffer.FillCount > 0 then
      StoredProc.Next;
    { Invoke OnProgress event }
    if Assigned(OnProgress) then
    begin
      Cancel := False;
      OnProgress(Self, psRunning, ppFetching, StoredProc.RecNo+1,
        MaxIntValue([StoredProc.RecNo+1, StoredProc.RecordCount]), Cancel);
      if Cancel then StoredProc.Close;
    end;
    if StoredProc.EOF then Break;
    { Getting record }
    RecordData := SqlBuffer.Add;
    for I := 0 to SqlBuffer.SqlFields.Count - 1 do
    begin
      FieldDesc := SqlBuffer.SqlFields[I];
      if FieldDesc.FieldNo < 0 then Continue;
      if StoredProc.FieldIsNull(FieldDesc.FieldNo) and
        not (FieldDesc.FieldType in [ftBlob, ftMemo]) then
        Continue;

      case FieldDesc.FieldType of
        ftString, ftBytes:
          begin
            SqlBuffer.SetFieldDataLen(FieldDesc, StoredProc.FieldBuffer(FieldDesc.FieldNo),
              RecordData, StoredProc.FieldSize(FieldDesc.FieldNo));
          end;
        ftSmallInt:
          begin
            case StoredProc.FieldType(FieldDesc.FieldNo) of
              SQLINT1:
                TempSmall := PByte(StoredProc.FieldBuffer(FieldDesc.FieldNo))^;
              SQLINT2:
                TempSmall := PSmallInt(StoredProc.FieldBuffer(FieldDesc.FieldNo))^;
            end;
            SqlBuffer.SetFieldData(FieldDesc, @TempSmall, RecordData);
          end;
        ftBoolean:
          begin
            TempBool := (PByte(StoredProc.FieldBuffer(FieldDesc.FieldNo))^ <> 0);
            SqlBuffer.SetFieldData(FieldDesc, @TempBool, RecordData);
          end;
        ftInteger, ftAutoInc:
          begin
            SqlBuffer.SetFieldData(FieldDesc, StoredProc.FieldBuffer(FieldDesc.FieldNo),
              RecordData);
          end;
        ftFloat, ftCurrency:
          begin
            dbconvert(TDirMsSqlTransact(StoredProc.Transact).Handle,
              StoredProc.FieldType(FieldDesc.FieldNo),
              PByte(StoredProc.FieldBuffer(FieldDesc.FieldNo)),
              StoredProc.FieldSize(FieldDesc.FieldNo),
              SQLFLT8, @TempDouble, SizeOf(TempDouble));
            SqlBuffer.SetFieldData(FieldDesc, @TempDouble, RecordData);
          end;
        ftDateTime:
          begin
            dbconvert(TDirMsSqlTransact(StoredProc.Transact).Handle,
              StoredProc.FieldType(FieldDesc.FieldNo),
              PByte(StoredProc.FieldBuffer(FieldDesc.FieldNo)),
              StoredProc.FieldSize(FieldDesc.FieldNo),
              SQLDATETIME, @TempDate, SizeOf(TempDate));
            dbdatecrack(TDirMsSqlTransact(StoredProc.Transact).Handle,
              @DateRec, @TempDate);

            TimeStamp := DateTimeToTimeStamp(EncodeDate(DateRec.year,
              DateRec.month, DateRec.day) + EncodeTime(DateRec.hour,
              DateRec.minute, DateRec.second, DateRec.millisecond));
            TempTime := TimeStampToMSecs(TimeStamp);
            SqlBuffer.SetFieldData(FieldDesc, @TempTime, RecordData);
          end;
        ftBlob, ftMemo:
          begin
            { Initialize blob and memo fields }
            BlobPtr := PRecordBlob(@RecordData.Bytes[FieldDesc.Offset+1]);
            BlobPtr.BlobType := btInternal;
            { Fill not null fields }
            if not StoredProc.FieldIsNull(FieldDesc.FieldNo) then
            begin
              RecordData.Bytes[FieldDesc.Offset] := 0;
              BlobPtr.Size := StoredProc.FieldSize(FieldDesc.FieldNo);
              BlobPtr.Data := AllocMem(BlobPtr.Size);
              System.Move(StoredProc.FieldBuffer(FieldDesc.FieldNo)^, BlobPtr.Data^,
                BlobPtr.Size);
            end
            { Fill null fields }
            else
            begin
              BlobPtr.Size := 0;
              BlobPtr.Data := nil;
            end;
          end;
        else
          DatabaseError(SUnknownType + FieldDesc.Alias);
      end;
    end;
    { Filter received record }
    SqlBuffer.FilterItem(SqlBuffer.Count-1);
  end;
  if StoredProc.Eof then  //pas na getting result
    FMustClose := true;
end;

procedure TZCustomMsSqlStoredProc.SetDatabase(
  const Value: TZMsSqlDatabase);
begin
  inherited SetDatabase(Value);
  if assigned(Value) then
    StoredProc.Connect := Value.Handle
  else
    StoredProc.Connect := nil;
end;

procedure TZCustomMsSqlStoredProc.SetTransact(
  const Value: TZMsSqlTransact);
begin
  inherited SetTransact(Value);
  if Assigned(Value) then
    StoredProc.Transact := Value.Handle
  else
    StoredProc.Transact := nil;
end;

end.
