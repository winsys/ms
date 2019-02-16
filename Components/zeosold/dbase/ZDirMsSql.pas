{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                MS SQL direct class API                 }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDirMsSql;

interface

uses SysUtils, Classes, ZDirSql, ZLibMsSql, Db, ZTransact, ZSqlTypes, ZSqlExtra,
  Windows {$IFDEF VER100}, DbTables{$ENDIF};

{$INCLUDE ..\Zeos.inc}

type
  { Direct  MS Sql connection }
  TDirMsSqlConnect = class (TDirConnect)
  protected
    function GetErrorMsg: ShortString; override;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;
    procedure CreateDatabase(Params: string); override;
    procedure DropDatabase; override;
  end;

  { Direct MS Sql transaction }
  TDirMsSqlTransact = class (TDirTransact)
  private
    FHandle: PDBPROCESS;
    FLoginRec: PLOGINREC;
  protected
    function GetErrorMsg: ShortString; override;
  public
    constructor Create(AConnect: TDirMsSqlConnect);
    destructor Destroy; override;

    procedure Open; override;
    procedure Close; override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    property Handle: PDBPROCESS read FHandle;
    property LoginRec: PLOGINREC read FLoginRec;
  end;

  { Direct MS Sql query }
  TDirMsSqlQuery = class (TDirQuery)
  protected
    function GetErrorMsg: ShortString; override;
  public
    constructor Create(AConnect: TDirMsSqlConnect; ATransact: TDirMsSqlTransact);

    function  Execute: LongInt; override;

    procedure Open; override;
    procedure Close; override;
//    function  CreateBlobObject: TDirBlob; override;

    procedure ShowDatabases(DatabaseName: ShortString); override;
    procedure ShowTables(TableName: ShortString); override;
    procedure ShowColumns(TableName, ColumnName: ShortString); override;
    procedure ShowIndexes(TableName: ShortString); override;

    procedure Next; override;

    function  FieldCount: Integer; override;
    function  RecordCount: Integer; override;

    function  FieldName(FieldNum: Integer): ShortString; override;
    function  FieldSize(FieldNum: Integer): Integer; override;
    function  FieldMaxSize(FieldNum: Integer): Integer; override;
    function  FieldType(FieldNum: Integer): Integer; override;
    function  FieldDataType(FieldNum: Integer): TFieldType; override;
    function  FieldIsNull(FieldNum: Integer): Boolean; override;
    function  Field(FieldNum: Integer): string; override;
    function  FieldBuffer(FieldNum: Integer): PChar; override;

    function StringToSql(Value: string): string; override;
  end;

  { MS SQL stored proc }
  TDirMsSqlStoredProc = class(TDirStoredProc)
  private
    procedure BindParam(Param: TParam);
  protected
    function GetErrorMsg: ShortString; override;
  public
    constructor Create(AConnect: TDirMsSqlConnect; ATransact: TDirMsSqlTransact);

    procedure Prepare(Params: TParams); override;
    procedure UnPrepare; override;
    procedure ExecProc; override;
    procedure Open; override;
    procedure Close; override;
//    function  CreateBlobObject: TDirBlob; override;
    function GetReturnValue: string; override;

    procedure ShowStoredProcs; override;
    procedure ShowParams(StoredProcedureName: ShortString); override;

    procedure Next; override;

    function FieldCount: Integer; override;
    function RecordCount: Integer; override;
    function ParamCount: Integer; override;

    function FieldName(FieldNum: Integer): ShortString; override;
    function FieldSize(FieldNum: Integer): Integer; override;
    function FieldMaxSize(FieldNum: Integer): Integer; override;
    function FieldType(FieldNum: Integer): Integer; override;
    function FieldDataType(FieldNum: Integer): TFieldType; override;
    function FieldIsNull(FieldNum: Integer): Boolean; override;
    function Field(FieldNum: Integer): string; override;
    function FieldBuffer(FieldNum: Integer): PChar; override;

    { All param functions deal with returnparameters only }
    function ParamName(ParamNum: Integer): ShortString; override;
    function ParamSize(ParamNum: Integer): Integer; override;
    function ParamMaxSize(ParamNum: Integer): Integer; override;
    function ParamType(ParamNum: Integer): Integer; override;
    function ParamDataType(ParamNum: Integer): TFieldType; override;
    function ParamIsNull(ParamNum: Integer): Boolean; override;
    function Param(ParamNum: Integer): string; override;
    function ParamBuffer(ParamNum: Integer): PChar; override;

    function StringToSql(Value: string): string; override;
  end;

const
  { MsSql ParamTypes }
  SQL_PARAM_TYPE_UNKNOWN = 0;
  SQL_PARAM_TYPE_INPUT = 1;
  SQL_PARAM_TYPE_OUTPUT = 2;
  SQL_RESULT_COL = 3;
  SQL_PARAM_OUTPUT = 4;
  SQL_RETURN_VALUE =5;


{ MsSqlToDelphiParamType translates a MSSQL ParamType to a Delphi ParamType
  pre:   Value=MsSql ParamType to translate
  post:  Result=Delphi ParamType }
function MsSqlToDelphiParamType(Value: Integer): TParamType;
function DelphiToMsSqlType(const DelphiType: TFieldType): Integer;

{ Convert MS SQL field type to delphi field type }
function MsSqlToDelphiType(Value: Integer): TFieldType;

{ Convert MS SQL field types description to delphi field types }
function MsSqlToDelphiTypeDesc(Value: string): TFieldType;

{ Monitor list }
var MonitorList: TZMonitorList;

implementation

uses ZDBaseConst, ZExtra;

{***************** TDirMsSqlConnect implementation *****************}

{ Class constructor }
constructor TDirMsSqlConnect.Create;
begin
  inherited Create;
  Port := '1433';
end;

{ Class destructor }
destructor TDirMsSqlConnect.Destroy;
begin
  inherited Destroy;
end;

{ Get an error message }
function TDirMsSqlConnect.GetErrorMsg: ShortString;
begin
  if Status <> csOk then
  begin
    Result := dbsqlerror;
    if StrCmpBegin(Result, 'General SQL Server error') then
      Result := dbmessage;
  end else
    Result := '';
end;

{ Connect to database }
procedure TDirMsSqlConnect.Connect;
begin
  inherited Connect;
  if hDll = 0 then MsSqlLoadLib;
  SetStatus(csOk);
  SetActive(True);
end;

{ Disconnect from database }
procedure TDirMsSqlConnect.Disconnect;
begin
  SetStatus(csOk);
  SetActive(False);
end;

{ Create and connect to database }
procedure TDirMsSqlConnect.CreateDatabase(Params: string);
var
  FHandle: PDBPROCESS;
  FLoginRec: PLOGINREC;
  Temp: string;
  Buffer: string;
begin
  if Active then Disconnect;
  if hDll = 0 then MsSqlLoadLib;
  SetStatus(csFail);

  { creating Login struct }
  FLoginRec := dbLogin;
  if FLoginRec = nil then
  begin
    MonitorList.InvokeEvent(Format('CONNECT %s',[Database]), Error, True);
    Exit;
  end;
  Temp := Login;
  if dbsetluser(FLoginRec,PChar(Temp)) = DBFAIL then
  begin
    dbfreelogin(FLoginRec);
    MonitorList.InvokeEvent(Format('CONNECT %s',[Database]), Error, True);
    Exit;
  end;
  Temp := Passwd;
  if dbsetlpwd(FLoginRec,PChar(Temp)) = DBFAIL then
  begin
    dbfreelogin(FLoginRec);
    MonitorList.InvokeEvent(Format('CONNECT %s',[Database]), Error, True);
    Exit;
  end;
  dbsetlapp(FLoginRec, PChar(ExtractFileName(ParamStr(0))));
  Temp := HostName;
  { connect to the server }
  FHandle := ZLibMsSql.dbopen(FLoginRec, PChar(Temp));
  if FHandle <> nil then
  begin
    { must use master-table }
    dbuse(FHandle, 'Master');
    Buffer := 'CREATE DATABASE '+Database+' '+Params;
    dbcmd(FHandle, PChar(Buffer));
    dbsqlexec(FHandle);
    if dbresults(FHandle) <> DBFAIL then
    begin
      MonitorList.InvokeEvent(Buffer, 'OK.', False);
      SetStatus(csOK);
    end
    else
      MonitorList.InvokeEvent(Buffer, Error, True);
    ZLibMsSql.dbclose(FHandle);
  end
  else
    MonitorList.InvokeEvent(Format('CONNECT %s',[Database]), Error, True);

  dbfreelogin(FLoginRec);
end;

{ Drop current database }
procedure TDirMsSqlConnect.DropDatabase;
var
  FHandle: PDBPROCESS;
  FLoginRec: PLOGINREC;
  Temp: string;
  Buffer: string;
begin
  if Active then Disconnect;
  if hDll = 0 then MsSqlLoadLib;
  SetStatus(csFail);

  { creating Login struct }
  FLoginRec := dbLogin;
  if FLoginRec = nil then
  begin
    MonitorList.InvokeEvent(Format('CONNECT %s',[Database]), Error, True);
    Exit;
  end;
  Temp := Login;
  if dbsetluser(FLoginRec,PChar(Temp)) = DBFAIL then
  begin
    dbfreelogin(FLoginRec);
    MonitorList.InvokeEvent(Format('CONNECT %s',[Database]), Error, True);
    Exit;
  end;
  Temp := Passwd;
  if dbsetlpwd(FLoginRec,PChar(Temp)) = DBFAIL then
  begin
    dbfreelogin(FLoginRec);
    MonitorList.InvokeEvent(Format('CONNECT %s',[Database]), Error, True);
    Exit;
  end;
  dbsetlapp(FLoginRec,PChar(ExtractFileName(ParamStr(0))));
  Temp := HostName;
  { connect to the server }
  FHandle := ZLibMsSql.dbopen(FLoginRec, PChar(Temp));
  if FHandle <> nil then
  begin
    { must use master-table }
    dbuse(FHandle, 'Master');
    Buffer := 'DROP DATABASE ' + Database;
    dbcmd(FHandle, PChar(Buffer));
    dbsqlexec(FHandle);
    if dbresults(FHandle) <> DBFAIL then
    begin
      MonitorList.InvokeEvent(Buffer, 'OK.', False);
      SetStatus(csOK);
    end
    else
      MonitorList.InvokeEvent(Buffer, Error, True);
    ZLibMsSql.dbclose(FHandle);
  end
  else
    MonitorList.InvokeEvent(Format('CONNECT %s', [Database]), Error, True);

  dbfreelogin(FLoginRec);
end;

{ Class constructor }
constructor TDirMsSqlTransact.Create(AConnect: TDirMsSqlConnect);
begin
  inherited Create;
  Connect := AConnect;
  FHandle := nil;
  FLoginRec := nil;
end;

{ Class destructor }
destructor TDirMsSqlTransact.Destroy;
begin
  inherited Destroy;
end;

{ Get error message }
function TDirMsSqlTransact.GetErrorMsg: ShortString;
begin
  if Status <> csOk then
  begin
    Result := dbsqlerror;
    if StrCmpBegin(Result, 'General SQL Server error') then
      Result := dbmessage;
  end else
    Result := '';
end;

{ Disconnect from database }
procedure TDirMsSqlTransact.Open;
label
  ErrorProc;
var
  Temp: string;
begin
  inherited Open;
  SetStatus(csFail);
  if not Assigned(Connect) or not Connect.Active then
    Exit;

  { Allocate login record }
  FLoginRec := dblogin;
  if FLoginRec = nil then
    goto ErrorProc;
  { Setup login record }
  Temp := Connect.Login;
  if dbsetluser(FLoginRec, PChar(Temp)) = DBFAIL then
  begin
    dbfreelogin(FLoginRec);
    goto ErrorProc;
  end;
  Temp := Connect.Passwd;
  if dbsetlpwd(FLoginRec, PChar(Temp)) = DBFAIL then
  begin
    dbfreelogin(FLoginRec);
    goto ErrorProc;
  end;
  dbsetlapp(FLoginRec, PChar(ExtractFileName(ParamStr(0))));
  { Connect to database }
  Temp := Connect.HostName;
  FHandle := ZLibMsSql.dbopen(FLoginRec, PChar(Temp));
  if FHandle <> nil then
  begin
    dbsetopt(FHandle, DBTEXTLIMIT, '2147483647');
    dbsetopt(FHandle, DBTEXTSIZE, '2147483647');
    dbsqlexec(FHandle);
    while dbresults(FHandle) = DBSUCCEED do;

    Temp := Connect.Database;
    if dbuse(FHandle, PChar(Temp)) = DBFAIL then
    begin
      ZLibMsSql.dbclose(FHandle);
      FHandle := nil;
      goto ErrorProc;
    end;
    MonitorList.InvokeEvent(Format('CONNECT %s',[Connect.Database]), 'OK.', False);
  end else
    goto ErrorProc;

  StartTransaction;
  SetActive(Status = csOk);
  Exit;

  { Process error status }
ErrorProc:
  MonitorList.InvokeEvent(Format('CONNECT %s',[Connect.Database]), Error, True);
end;

{ Connect to database }
procedure TDirMsSqlTransact.Close;
begin
  EndTransaction;
  if Active then
  begin
    { Disconnect database }
    if FHandle <> nil then
    begin
      ZLibMsSql.dbclose(FHandle);
      FHandle := nil;
    end;
    { Free login record }
    if FLoginRec <> nil then
    begin
      dbfreelogin(FLoginRec);
      FLoginRec := nil;
    end;
    { Update transact status }
    if Assigned(Connect) then
      MonitorList.InvokeEvent(Format('DISCONNECT %s',[Connect.Database]), 'OK.', False);
  end;
  SetActive(False);
end;

{ Connect to database and start transaction }
procedure TDirMsSqlTransact.StartTransaction;
begin
  { Set startup values }
  SetStatus(csFail);
  if not Assigned(Connect) then Exit;
  SetStatus(csOk);
  { Begin transaction }
  if TransactSafe then
  begin
    dbcmd(FHandle, 'BEGIN TRANSACTION');
    dbsqlexec(FHandle);
    MonitorList.InvokeEvent('BEGIN TRANSACTION', Error, Error <> '');
    dbcancel(FHandle);
  end;
end;

{ End transaction and disconnect from database }
procedure TDirMsSqlTransact.EndTransaction;
begin
  { Setup transact properties }
  SetStatus(csOk);
  if Active then
  begin
    { End transaction }
    if TransactSafe then
    begin
      dbcmd(FHandle, 'ROLLBACK');
      dbsqlexec(FHandle);
      if dbresults(FHandle) <> DBSUCCEED then
        SetStatus(csFail);
      MonitorList.InvokeEvent('ROLLBACK', Error, Error <> '');
      dbcancel(FHandle);
    end;
  end;
end;

{ Commit transaction }
procedure TDirMsSqlTransact.Commit;
begin
  { Check status }
  SetStatus(csFail);
  if not Active or not Assigned(FHandle) then Exit;
  SetStatus(csOk);
  if TransactSafe then
  begin
    { Commit execute }
    dbcmd(FHandle, 'COMMIT');
    dbsqlexec(FHandle);
    if dbresults(FHandle) <> DBSUCCEED then
      SetStatus(csFail);
    MonitorList.InvokeEvent('COMMIT', Error, Error <> '');
    dbcancel(FHandle);
    { Start new trasaction }
    dbcmd(FHandle, 'BEGIN TRANSACTION');
    dbsqlexec(FHandle);
    if dbresults(FHandle) <> DBSUCCEED then
      SetStatus(csFail);
    dbcancel(FHandle);
    MonitorList.InvokeEvent('BEGIN TRANSACTION', Error, Error <> '');
  end;
end;

{ Rollback transaction }
procedure TDirMsSqlTransact.Rollback;
begin
  { Check status }
  SetStatus(csFail);
  if not Active or not Assigned(FHandle) then Exit;
  SetStatus(csOk);
  if TransactSafe then
  begin
    { Rollback execute }
    dbcmd(FHandle, 'ROLLBACK');
    dbsqlexec(FHandle);
    if dbresults(FHandle) <> DBSUCCEED then
      SetStatus(csFail);
    MonitorList.InvokeEvent('ROLLBACK', Error, Error <> '');
    dbcancel(FHandle);
    { Start new trasaction }
    dbcmd(FHandle, 'BEGIN TRANSACTION');
    dbsqlexec(FHandle);
    if dbresults(FHandle) <> DBSUCCEED then
      SetStatus(csFail);
    MonitorList.InvokeEvent('BEGIN TRANSACTION', Error, Error <> '');
    dbcancel(FHandle);
  end;
end;

{******************* TDirMsSqlQuery implementation **********************}

{ Class constructor }
constructor TDirMsSqlQuery.Create(AConnect: TDirMsSqlConnect;
  ATransact: TDirMsSqlTransact);
begin
  inherited Create;
  Connect := AConnect;
  Transact := ATransact;
end;

{ Get an error message }
function TDirMsSqlQuery.GetErrorMsg: ShortString;
begin
  if Transact = nil then
    Result := 'Transaction object not defined'
  else if not Transact.Active then
    Result := 'Connection closed'
  else if not (Status in [qsTuplesOk, qsCommandOk]) then
  begin
    Result := dbsqlerror;
    if StrCmpBegin(Result, 'General SQL Server error') then
      Result := dbmessage;
  end else
    Result := '';
end;

{ Close open query }
procedure TDirMsSqlQuery.Close;
begin
  if Active and Assigned(Transact) then
    dbcancel(TDirMsSqlTransact(Transact).Handle);
  inherited Close;
  SetActive(False);
  SetStatus(qsCommandOk);
end;

{ Execute the query }
function TDirMsSqlQuery.Execute: LongInt;
var
  MsTransact: TDirMsSqlTransact;
begin
  Result := inherited Execute;
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  MsTransact := TDirMsSqlTransact(Transact);
  dbcmd(MsTransact.Handle, PChar(Trim(Sql)));
  dbsqlexec(MsTransact.Handle);
  if dbresults(MsTransact.Handle) = DBSUCCEED then
  begin
    SetAffectedRows(dbcount(MsTransact.Handle));
    Result := AffectedRows;
    SetStatus(qsCommandOk);
  end;
  MonitorList.InvokeEvent(Sql, Error, Error <> '');
  dbcancel(MsTransact.Handle);
end;

{ Open the query with result set }
procedure TDirMsSqlQuery.Open;
var
  MsTransact: TDirMsSqlTransact;
  Result: Integer;
begin
  inherited Open;
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  MsTransact := TDirMsSqlTransact(Transact);
  dbcmd(MsTransact.Handle, PChar(Trim(Sql)));
  dbsqlexec(MsTransact.Handle);

  Result := dbresults(MsTransact.Handle);
  while dbresults(MsTransact.Handle) = DBSUCCEED do;

  if Result = DBSUCCEED then
  begin
    SetActive(True);
    SetStatus(qsTuplesOk);
    SetBOF(False);
    SetEOF(False);
    Next;
    if Status <> qsTuplesOk then
      SetActive(False);
  end;
  MonitorList.InvokeEvent(Sql, Error, not Active);
end;

{ Go to next row }
procedure TDirMsSqlQuery.Next;
var
  FetchStat: Integer;
begin
  if not Active or EOF then Exit;
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact) then
    Exit;

  FetchStat := dbnextrow(TDirMsSqlTransact(Transact).Handle);
  if FetchStat <> DBFAIL then
  begin
    SetStatus(qsTuplesOk);
    SetEOF(FetchStat = NO_MORE_ROWS);
    if FetchStat <> NO_MORE_ROWS then
      SetRecNo(RecNo+1);
  end else
    SetEOF(True);
end;

{ Get record quantity }
function TDirMsSqlQuery.RecordCount: Integer;
begin
  if not Active then Result := 0
  else Result := dbcount(TDirMsSqlTransact(Transact).Handle);
end;

{ Get fields quantity }
function TDirMsSqlQuery.FieldCount: Integer;
begin
  Result := 0;
  if Active then
    Result := dbnumcols(TDirMsSqlTransact(Transact).Handle);
end;

{ Get field name }
function TDirMsSqlQuery.FieldName(FieldNum: Integer): ShortString;
var
  Temp: PChar;
begin
  Result := '';
  if Active then
  begin
    Temp := dbcolname(TDirMsSqlTransact(Transact).Handle, FieldNum+1);
    if (Temp <> nil) and (Temp^ <> #0) then
      Result := StrPas(Temp)
    else
      Result := 'Field' + IntToStr(FieldNum+1);
  end;
end;

{ Get field size }
function TDirMsSqlQuery.FieldSize(FieldNum: Integer): Integer;
begin
  Result := 0;
  if Active then
  begin
    Result := dbdatlen(TDirMsSqlTransact(Transact).Handle, FieldNum+1);
    if Result = -1 then Result := 0;
  end;
end;

{ Get maximum field size }
function TDirMsSqlQuery.FieldMaxSize(FieldNum: Integer): Integer;
begin
  Result := 0;
  if Active then
  begin
    Result := dbcollen(TDirMsSqlTransact(Transact).Handle, FieldNum+1);
    if Result = -1 then Result := 0;
  end;
end;

{ Get field type }
function TDirMsSqlQuery.FieldType(FieldNum: Integer): Integer;
begin
  Result := 0;
  if Active then
    Result := dbcoltype(TDirMsSqlTransact(Transact).Handle, FieldNum+1);
end;

{ Define field type }
function TDirMsSqlQuery.FieldDataType(FieldNum: Integer): TFieldType;
begin
  Result := MsSqlToDelphiType(FieldType(FieldNum));
end;

{ Get field value }
function TDirMsSqlQuery.Field(FieldNum: Integer): string;
var
  Length: LongInt;
  MsTransact: TDirMsSqlTransact;
  Ptr: PByte;
  ColType: Integer;
  DateRec: DBDATEREC;
  TempDate: TDateTime;
  TempSqlDate: DBDATETIME;
  TempFloat: Double;
begin
  Result := '';
  if not Active or Eof or Bof then Exit;

  MsTransact := TDirMsSqlTransact(Transact);
  Length := dbdatlen(MsTransact.Handle, FieldNum+1);
  if Length = -1 then Length := 0;
  Ptr := dbdata(MsTransact.Handle, FieldNum+1);
  if Ptr = nil then Exit;
  ColType := dbcoltype(MsTransact.Handle, FieldNum+1);
  case ColType of
    SQLINT1: Result := IntToStr(PByte(Ptr)^);
    SQLINT2: Result := IntToStr(PSmallInt(Ptr)^);
    SQLINT4: Result := IntToStr(PInteger(Ptr)^);
    SQLVARCHAR, SQLCHAR, SQLVARBINARY, SQLBINARY:
      Result := MemPas(PChar(Ptr), Length);
    SQLBIT:
      if PByte(Ptr)^ <> 0 then Result := '1'
      else Result := '0';
    SQLFLT4: Result := FloatToStr(PSingle(Ptr)^);
    SQLFLT8: Result := FloatToStr(PDouble(Ptr)^);
    SQLDATETIME, SQLDATETIM4:
      begin
        dbconvert(MsTransact.Handle, ColType, Ptr, Length, SQLDATETIME,
          @TempSqlDate, SizeOf(DBDATETIME));
        dbdatecrack(MsTransact.Handle, @DateRec, @TempSqlDate);
        TempDate := EncodeDate(DateRec.year, DateRec.month, DateRec.day);
        TempDate := TempDate + EncodeTime(DateRec.hour, DateRec.minute,
        DateRec.second, DateRec.millisecond);
        Result := DateTimeToSqlDate(TempDate);
      end;
    SQLMONEY, SQLMONEY4:
      begin
        dbconvert(MsTransact.Handle, ColType, Ptr, Length, SQLFLT8,
          @TempFloat, SizeOf(TempFloat));
        Result := FloatToStr(TempFloat);
      end;
    else
      Result := MemPas(PChar(Ptr), Length);
  end;
end;

{ Get field buffer }
function TDirMsSqlQuery.FieldBuffer(FieldNum: Integer): PChar;
begin
  if not Active or Eof or Bof then
    Result := nil
  else
    Result := PChar(dbdata(TDirMsSqlTransact(Transact).Handle, FieldNum+1));
end;

{ Is field null }
function TDirMsSqlQuery.FieldIsNull(FieldNum: Integer): Boolean;
begin
  if not Active or Eof or Bof then
    Result := True
  else
    Result := (dbdata(TDirMsSqlTransact(Transact).Handle, FieldNum+1) = nil);
end;

{ Showes databases }
procedure TDirMsSqlQuery.ShowDatabases(DatabaseName: ShortString);
begin
  if Active then Close;
  Sql := 'SELECT name FROM master..sysdatabases';
  if DatabaseName <> '' then
    Sql := Sql + ' WHERE name LIKE '''+DatabaseName+'''';
  Sql := Sql + ' ORDER BY name';
  Open;
end;

{ Showes tables of the database }
procedure TDirMsSqlQuery.ShowTables(TableName: ShortString);
begin
  if Active then Close;
  Sql := 'select name from sysobjects where type=''U''';
  if TableName <> '' then
    Sql := Sql + ' and name like '''+TableName+'''';
  Sql := Sql + ' order by name';
  Open;
end;

{ Showes columns of the table }
procedure TDirMsSqlQuery.ShowColumns(TableName, ColumnName: ShortString);
begin
  if Active then Close;
  Sql := 'EXEC sp_mshelpcolumns '''+TableName+'''';
  Open;
end;

{ Showes indexes of the table }
procedure TDirMsSqlQuery.ShowIndexes(TableName: ShortString);
begin
  if Active then Close;
  Sql := 'EXEC sp_helpindex '''+TableName+'''';
  Open;
end;

{ Convert string to sql format }
function TDirMsSqlQuery.StringToSql(Value: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
  begin
    Result := Result + Value[I];
    if Value[I] = '''' then
      Result := Result + Value[I];
  end;
end;

{ TDirMsSqlStoredProc }

procedure TDirMsSqlStoredProc.BindParam(Param: TParam);
var
  BindKind: Byte;
  MaxLen: LongInt;
  DataLen: LongInt;
  Ptr: PByte;
  tp: Integer;

  // temporary variables to pass a parameter to db-lib
  tmpInteger: Integer;
  tmpString: string;
  tmpBoolean: Boolean;
  tmpSmallInt: SmallInt;
  tmpWord: Word;
  tmpCurrency: Currency;
  tmpDouble: Double;
begin
  if Param.ParamType = ptUnknown then
    raise Exception.Create('Param '+Param.Name+' is of unknown type')
  else
  begin
    tp := DelphiToMsSqlType(Param.DataType);
    if Param.ParamType = ptInput then
    begin
      BindKind := 0;//DBRPCDEFAULT;
      MaxLen := -1;
      case Param.DataType of
        ftInteger, ftSmallint, ftWord, ftBoolean, ftFloat, ftCurrency, ftDateTime,
        ftDate, ftTime {$IFNDEF VER100}, ftLargeInt{$ENDIF}:
          DataLen := -1;
      else
        DataLen := Length(Param.asString)*SizeOf(Char);
      end;
    end
    else
    begin
      BindKind := DBRPCRETURN;
      case Param.DataType of
      ftInteger, ftSmallint, ftWord, ftBoolean, ftFloat, ftCurrency,
      ftDateTime, ftDate, ftTime {$IFNDEF VER100}, ftLargeInt {$ENDIF}:
          begin
             MaxLen := -1;
             if Param.Isnull then
               DataLen := 0
             else
               DataLen := -1;
          end;
      else
        begin
          if Param.Isnull then
          begin
            MaxLen := 0;
            DataLen := 0;
          end
          else
          begin
            //MaxLen := High(string);
            MaxLen := 2147483647;
            DataLen := Length(Param.asString)*SizeOf(Char);
          end;
        end;
      end;
    end;

    // Getting a pointer to a variable containing the value of the parameter
    case Param.DataType of
      ftInteger {$IFNDEF VER100}, ftLargeInt{$ENDIF}:
        begin
          tmpInteger := Param.AsInteger;
          Ptr := PByte(@tmpInteger);
        end;
      ftString:
        begin
          tmpString := Param.AsString;
          Ptr := PByte(PChar(tmpString));
        end;
      ftBoolean:
        begin
          tmpBoolean := Param.asBoolean;
          Ptr := PByte(@tmpBoolean);
        end;
      ftBlob, ftGraphic, ftFmtMemo:
        begin
          tmpString := Param.AsBlob;
          Ptr := PByte(PChar(tmpString));
        end;
      ftCurrency:
        begin
          tmpCurrency := Param.AsCurrency;
          Ptr := PByte(@tmpCurrency);
        end;
      ftDate:
        begin
          tmpDouble := Param.asDate;
          Ptr := PByte(@tmpDouble);
        end;
      ftDateTime:
        begin
          tmpDouble := Param.asDateTime;
          Ptr := PByte(@tmpDouble);
        end;
      ftTime:
        begin
          tmpDouble := Param.asTime;
          Ptr := PByte(@tmpDouble);
        end;
      ftFloat:
        begin
          tmpDouble := Param.asFloat;
          Ptr := PByte(@tmpDouble);
        end;
      ftSmallInt:
        begin
          tmpSmallInt := Param.AsSmallInt;
          Ptr := PByte(@tmpSmallInt);
        end;
      ftWord:
        begin
          tmpWord := Param.asWord;
          Ptr := PByte(@tmpWord);
        end;
      ftMemo:
        begin
          tmpString := Param.AsString;
          Ptr := PByte(PChar(tmpString));
        end
    else
      begin
        tmpString := Param.AsString;
        Ptr := PByte(PChar(tmpString));
      end;
    end;

    // add the parameter
    dbrpcparam(TDirMsSqlTransact(Transact).Handle, PChar(Param.Name), BindKind,
      tp, MaxLen, DataLen, Ptr);
  end;
end;

procedure TDirMsSqlStoredProc.Close;
begin
  if Active and Assigned(Transact) then
    dbcancel(TDirMsSqlTransact(Transact).Handle);

  inherited Close;
  SetActive(False);
  SetStatus(qsCommandOk);
end;

constructor TDirMsSqlStoredProc.Create(AConnect: TDirMsSqlConnect;
  ATransact: TDirMsSqlTransact);
begin
  inherited Create;
  Connect := AConnect;
  Transact := ATransact;
end;

procedure TDirMsSqlStoredProc.ExecProc;
var
  MsTransact: TDirMsSqlTransact;
  dbResult: Integer;
begin
  if Prepared then
  begin
    inherited ExecProc;
    SetStatus(qsFail);
    if not Assigned(Connect) or not Assigned(Transact)
      or not (Connect.Active and Transact.Active) then
        Exit;

    MsTransact := TDirMsSqlTransact(Transact);
    dbrpcexec(MsTransact.Handle);

    dbResult := dbresults(MsTransact.Handle);

    while dbresults(MsTransact.Handle) = DBSUCCEED do;

    if dbResult = DBSUCCEED then
    begin
      SetActive(True);
      SetStatus(qsTuplesOk);
      SetBOF(False);
      SetEOF(False);
      Next;
      if Status <> qsTuplesOk then
        SetActive(False);
    end;

    MonitorList.InvokeEvent(StoredProcName, Error, not Active);
  end;
end;

function TDirMsSqlStoredProc.Field(FieldNum: Integer): string;
var
  Length: LongInt;
  MsTransact: TDirMsSqlTransact;
  Ptr: PByte;
  ColType: Integer;
  DateRec: DBDATEREC;
  TempDate: TDateTime;
  TempSqlDate: DBDATETIME;
  TempFloat: Double;
begin
  Result := '';
  if not Active or Eof or Bof then Exit;

  MsTransact := TDirMsSqlTransact(Transact);
  Length := dbdatlen(MsTransact.Handle, FieldNum+1);
  if Length = -1 then Length := 0;
  Ptr := dbdata(MsTransact.Handle, FieldNum+1);
  if Ptr = nil then Exit;
  ColType := dbcoltype(MsTransact.Handle, FieldNum+1);
  case ColType of
    SQLINT1: Result := IntToStr(PByte(Ptr)^);
    SQLINT2: Result := IntToStr(PSmallInt(Ptr)^);
    SQLINT4: Result := IntToStr(PInteger(Ptr)^);
    SQLVARCHAR, SQLCHAR, SQLVARBINARY, SQLBINARY:
      Result := MemPas(PChar(Ptr), Length);
    SQLBIT:
      if PByte(Ptr)^ <> 0 then Result := '1'
      else Result := '0';
    SQLFLT4: Result := FloatToStr(PSingle(Ptr)^);
    SQLFLT8: Result := FloatToStr(PDouble(Ptr)^);
    SQLDATETIME, SQLDATETIM4:
      begin
        dbconvert(MsTransact.Handle, ColType, Ptr, Length, SQLDATETIME,
          @TempSqlDate, SizeOf(DBDATETIME));
        dbdatecrack(MsTransact.Handle, @DateRec, @TempSqlDate);
        TempDate := EncodeDate(DateRec.year, DateRec.month, DateRec.day);
        TempDate := TempDate + EncodeTime(DateRec.hour, DateRec.minute,
        DateRec.second, DateRec.millisecond);
        Result := DateTimeToSqlDate(TempDate);
      end;
    SQLMONEY, SQLMONEY4:
      begin
        dbconvert(MsTransact.Handle, ColType, Ptr, Length, SQLFLT8,
          @TempFloat, SizeOf(TempFloat));
        Result := FloatToStr(TempFloat);
      end;
    else
      Result := MemPas(PChar(Ptr), Length);
  end;
end;

function TDirMsSqlStoredProc.FieldBuffer(FieldNum: Integer): PChar;
begin
  if not Active or Eof or Bof then
    Result := nil
  else
    Result := PChar(dbdata(TDirMsSqlTransact(Transact).Handle, FieldNum+1));
end;

function TDirMsSqlStoredProc.FieldCount: Integer;
begin
  Result := 0;
  if Active then
    Result := dbnumcols(TDirMsSqlTransact(Transact).Handle);
end;

function TDirMsSqlStoredProc.FieldDataType(FieldNum: Integer): TFieldType;
begin
  Result := MsSqlToDelphiType(FieldType(FieldNum));
end;

function TDirMsSqlStoredProc.FieldIsNull(FieldNum: Integer): Boolean;
begin
  if not Active or Eof or Bof then
    Result := True
  else
    Result := (dbdata(TDirMsSqlTransact(Transact).Handle, FieldNum+1) = nil);
end;

function TDirMsSqlStoredProc.FieldMaxSize(FieldNum: Integer): Integer;
begin
  Result := 0;
  if Active then
  begin
    Result := dbcollen(TDirMsSqlTransact(Transact).Handle, FieldNum+1);
    if Result = -1 then Result := 0;
  end;
end;

function TDirMsSqlStoredProc.FieldName(FieldNum: Integer): ShortString;
var
  Temp: PChar;
begin
  Result := '';
  if Active then
  begin
    Temp := dbcolname(TDirMsSqlTransact(Transact).Handle, FieldNum+1);
    if (Temp <> nil) and (Temp^ <> #0) then
      Result := StrPas(Temp)
    else
      Result := 'Field' + IntToStr(FieldNum+1);
  end;
end;

function TDirMsSqlStoredProc.FieldSize(FieldNum: Integer): Integer;
begin
  Result := 0;
  if Active then
  begin
    Result := dbdatlen(TDirMsSqlTransact(Transact).Handle, FieldNum+1);
    if Result = -1 then Result := 0;
  end;
end;

function TDirMsSqlStoredProc.FieldType(FieldNum: Integer): Integer;
begin
  Result := 0;
  if Active then
    Result := dbcoltype(TDirMsSqlTransact(Transact).Handle, FieldNum+1);
end;

function TDirMsSqlStoredProc.GetErrorMsg: ShortString;
begin
  if Transact = nil then
    Result := 'Transaction object not defined'
  else if not Transact.Active then
    Result := 'Connection closed'
  else if not (Status in [qsTuplesOk, qsCommandOk]) then
  begin
    Result := dbsqlerror;
    if StrCmpBegin(Result, 'General SQL Server error') then
      Result := dbmessage;
  end else
    Result := '';
end;

procedure TDirMsSqlStoredProc.Next;
var
  FetchStat: Integer;
begin
  if not Active or EOF then Exit;
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact) then
    Exit;

  FetchStat := dbnextrow(TDirMsSqlTransact(Transact).Handle);
  if FetchStat <> DBFAIL then
  begin
    SetStatus(qsTuplesOk);
    SetEOF(FetchStat = NO_MORE_ROWS);
    if FetchStat <> NO_MORE_ROWS then
      SetRecNo(RecNo+1);
  end else
    SetEOF(True);
end;

procedure TDirMsSqlStoredProc.Open;
begin
  ExecProc;
end;

function TDirMsSqlStoredProc.Param(ParamNum: Integer): string;
var
  MsTransact: TDirMsSqlTransact;
  Length: LongInt;
  Ptr: PByte;
  ParType: Integer;
  tmpSqlDate: DBDATETIME;
  DateRec: DBDATEREC;
  TempDate: TDateTime;
  TempFloat: Double;
begin
  Result := '';
  if not Active then
    Exit;

  MsTransact := TDirMsSqlTransact(Transact);
  Length := dbretlen(MsTransact.Handle,ParamNum+1);
  if Length = -1 then
    Length := 0;
  Ptr := dbretdata(MsTransact.Handle,ParamNum+1);
  if Ptr = nil then
    Exit;
  ParType := dbrettype(MsTransact.Handle,ParamNum+1);
  case ParType of
    SQLINT1: Result := IntToStr(PByte(Ptr)^);
    SQLINT2: Result := IntToStr(PSmallInt(Ptr)^);
    SQLINT4: Result := IntToStr(PInteger(Ptr)^);
    SQLVARCHAR, SQLCHAR, SQLVARBINARY, SQLBINARY:
      Result := MemPas(PChar(Ptr),Length);
    SQLBIT: if PByte(Ptr)^ <> 0 then
              Result := '1'
            else
              Result := '0';
    SQLFLT4: Result := FloatToStr(PSingle(Ptr)^);
    SQLFLT8: Result := FloatToStr(PDouble(Ptr)^);
    SQLDATETIME, SQLDATETIM4:
      begin
        dbconvert(MsTransact.Handle,ParType,Ptr,Length,SQLDATETIME,
                  @tmpSqlDate, sizeof(DBDATETIME));
        dbdatecrack(MsTransact.Handle,@DateRec,@tmpSqlDate);
        TempDate := EncodeDate(DateRec.year, DateRec.Month, DateRec.Day);
        TempDate := TempDate+EncodeTime(DateRec.Hour,DateRec.Minute,
                                        DateRec.Second,DateRec.MilliSecond);
        Result := DateTimeToSqlDate(TempDate);
      end;
    SQLMONEY, SQLMONEY4:
      begin
        dbconvert(MsTransact.Handle,ParType,Ptr,Length,SQLFLT8,@TempFloat,
                  sizeOf(TempFloat));
        Result := FloatToStr(TempFloat);
      end;
    else
      Result := MemPas(PChar(Ptr),Length);
  end;
end;

function TDirMsSqlStoredProc.ParamBuffer(ParamNum: Integer): PChar;
begin
  if not Transact.Active then
    Result := nil
  else
    Result := PChar(dbretdata(TDirMsSqlTransact(Transact).Handle, ParamNum+1));
end;

function TDirMsSqlStoredProc.ParamCount: Integer;
begin
  Result := 0;
  if Transact.Active then
    Result := dbnumrets(TDirMsSqlTransact(Transact).Handle);
end;

function TDirMsSqlStoredProc.ParamDataType(ParamNum: Integer): TFieldType;
begin
  Result := MsSqlToDelphiType(ParamType(ParamNum));
end;

function TDirMsSqlStoredProc.ParamIsNull(ParamNum: Integer): Boolean;
begin
  if not Transact.Active then
    Result := True
  else
    Result := (dbretdata(TDirMsSqlTransact(Transact).Handle, ParamNum+1) = nil);
end;

function TDirMsSqlStoredProc.ParamMaxSize(ParamNum: Integer): Integer;
begin
  Result := 0;
  if Transact.Active then
  begin
    Result := dbretlen(TDirMsSqlTransact(Transact).Handle, ParamNum+1);
    if Result = -1 then Result := 0;
  end;
end;

function TDirMsSqlStoredProc.ParamName(ParamNum: Integer): ShortString;
var
  Temp: PChar;
begin
  Result := '';
  if Transact.Active then
  begin
    Temp := dbretname(TDirMsSqlTransact(Transact).Handle, ParamNum+1);
    if (Temp <> nil) and (Temp^ <> #0) then
      Result := StrPas(Temp)
    else
      Result := 'Field' + IntToStr(ParamNum+1);
  end;
end;

function TDirMsSqlStoredProc.ParamSize(ParamNum: Integer): Integer;
begin
  Result := 0;
  if Transact.Active then
  begin
    Result := dbretlen(TDirMsSqlTransact(Transact).Handle, ParamNum+1);
    if Result = -1 then Result := 0;
  end;
end;

function TDirMsSqlStoredProc.ParamType(ParamNum: Integer): Integer;
begin
  Result := 0;
  if Transact.Active then
    Result := dbrettype(TDirMsSqlTransact(Transact).Handle, ParamNum+1);
end;

procedure TDirMsSqlStoredProc.Prepare(Params: TParams);
var
  I: Integer;
begin
  if not Prepared then
  begin
    dbrpcinit(TDirMsSqlTransact(Transact).Handle,PChar(StoredProcName),0);
    for I := 0 to Params.Count-1 do
//      if (Params[I].ParamType=ptInput) or (Params[I].ParamType=ptInputOutput) then
      BindParam(Params[i]);
    Prepared := true;
  end;
end;

function TDirMsSqlStoredProc.RecordCount: Integer;
begin
  if not Active then
    Result := 0
  else
    Result := dbcount(TDirMsSqlTransact(Transact).Handle);
end;

procedure TDirMsSqlStoredProc.ShowParams(StoredProcedureName: ShortString);
var
  tmpString: String;
  tmpprocname: string;
  Ptr: PByte;
begin
  if Active then
    Close;

//  StoredprocName := 'sp_sproc_columns';
  tmpprocname := 'sp_sproc_columns';
  dbrpcinit(TDirMsSqlTransact(Transact).Handle,PChar(tmpprocname),0);
    if dbresults(TDirMsSqlTransact(Transact).Handle)<>SUCCEED then
      raise Exception.Create('no succes init');

  if StoredProcedureName<>'' then
  begin
//    if StoredProcedureName[Length(StoredProcedureName)] = '.' then
//    raise Exception.Create(storedprocedurename[Length(StoredProcedureName)]);
    tmpString := Copy(StoredProcedureName,1,pos(';',StoredProcedureName)-1);
    Ptr := PByte(PChar(tmpString));
    dbrpcparam(TDirMsSqlTransact(Transact).Handle,PChar('@procedure_name'),0,
               SQLVARCHAR,-1,Length(tmpString)*sizeof(Char),Ptr);
    if dbresults(TDirMsSqlTransact(Transact).Handle)<>SUCCEED then
      raise Exception.Create('no succes');
  end
  else
    raise exception.Create('no spName');
  Prepared := true;
  ExecProc;
end;

procedure TDirMsSqlStoredProc.ShowStoredProcs;
begin
  if Active then
    Close;

  StoredProcName := 'sp_stored_procedures';
  dbrpcinit(TDirMsSqlTransact(Transact).Handle,PChar(StoredProcName),0);
  Prepared := true;
  ExecProc;
end;

function TDirMsSqlStoredProc.StringToSql(Value: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
  begin
    Result := Result + Value[I];
    if Value[I] = '''' then
      Result := Result + Value[I];
  end;
end;

procedure TDirMsSqlStoredProc.UnPrepare;
begin
  inherited Unprepare;
  dbcancel(TDirMsSqlTransact(Transact).Handle);
end;

function DelphiToMsSqlType(const DelphiType: TFieldType): Integer;
begin
  case DelphiType of
    ftUnknown: Result := -1;
    ftString: Result := SQLVARCHAR;
    ftSmallInt: Result := SQLINT2;
    ftInteger: Result := SQLINT4;
    ftWord: Result := SQLINT2;
    ftBoolean: Result := SQLBIT;
    ftFloat: Result := SQLFLT8;
    ftCurrency: Result := SQLMONEY;
    ftDate: Result := SQLDATETIME;
    ftTime: Result := SQLDATETIME;
    ftDateTime: Result := SQLDATETIME;
    ftBytes: Result := SQLBINARY;
    ftVarBytes: Result := SQLVARBINARY;
    ftBlob: Result := SQLIMAGE;
    ftMemo: Result := SQLTEXT;
    ftGraphic: Result := SQLIMAGE;
    ftFmtMemo: Result := SQLIMAGE;
//    ftFixedChar: Result := SQLCHAR;
//    ftWideString: Result := SQLVARCHAR;
//    ftLargeInt: Result := SQLINT4;
  else
    Result := -1;
  end;
end;

function TDirMsSqlStoredProc.GetReturnValue: String;
begin
  if Transact.Active and dbhasretstat(TDirMsSqlTransact(Transact).Handle) then
    Result := IntToStr(dbretstatus(TDirMsSqlTransact(Transact).Handle));
end;

function MsSqlToDelphiParamType(Value: Integer): TParamType;
begin
  case Value of
    SQL_PARAM_TYPE_UNKNOWN: Result := ptUnknown;
    SQL_PARAM_TYPE_INPUT: Result := ptInput;
    SQL_PARAM_TYPE_OUTPUT: Result := ptInputOutput;
    SQL_RESULT_COL: Result := ptUnknown;
    SQL_PARAM_OUTPUT: Result := ptUnknown;
    SQL_RETURN_VALUE: Result := ptResult;
  else
    Result := ptUnknown;
  end;
end;

{*************** Extra functions implementation ****************}

{ Convert MS SQL field type to delphi field type }
function MsSqlToDelphiType(Value: Integer): TFieldType;
begin
  case Value of
    SQLINT1, SQLINT2:
      Result := ftSmallInt;
    SQLINT4:
      Result := ftInteger;
    SQLFLT4, SQLFLT8, SQLFLTN:
      Result := ftFloat;
    SQLDECIMAL, SQLNUMERIC:
      Result := ftFloat;
    SQLMONEY, SQLMONEYN:
      Result := ftCurrency;
    SQLBINARY, SQLVARBINARY:
      Result := ftBytes;
    SQLDATETIME:
      Result := ftDateTime;
    SQLBIT:
      Result := ftBoolean;
    SQLTEXT:
      Result := ftMemo;
    SQLIMAGE:
      Result := ftBlob;
    else
      Result := ftString;
  end;
end;

{ Convert MS SQL field types description to delphi field types }
function MsSqlToDelphiTypeDesc(Value: string): TFieldType;
begin
  Value := LowerCase(Value);
  if Value = 'int' then
    Result := ftInteger
  else if (Value = 'char') or (Value = 'varchar') then
    Result := ftString
  else if (Value = 'nchar') or (Value = 'nvarchar') then
    Result := ftString
  else if (Value = 'varbinary') or (Value = 'binary')
    or (Value = 'timestamp') then
    Result := ftBytes
  else if (Value = 'float') or (Value = 'real') then
    Result := ftFloat
  else if (Value = 'decimal') or (Value = 'numeric') then
    Result := ftFloat
  else if (Value = 'datetime') or (Value = 'smalldatetime') then
    Result := ftDateTime
  else if (Value = 'money') or (Value = 'smallmoney') then
    Result := ftCurrency
  else if (Value = 'tinyint') or (Value = 'smallint') then
    Result := ftSmallInt
  else if Value = 'text' then
    Result := ftMemo
  else if Value = 'image' then
    Result := ftBlob
  else if Value = 'bit' then
    Result := ftBoolean
  else
    Result := ftUnknown;
end;

initialization
  MonitorList := TZMonitorList.Create;
finalization
  MonitorList.Free;
end.
