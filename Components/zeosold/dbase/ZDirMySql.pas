{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                 MySql direct class API                 }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDirMySql;

interface

uses {$IFNDEF LINUX}Windows,{$ENDIF} SysUtils, ZDirSql, ZLibMySql, DB, Math,
  ZTransact, ZSqlTypes;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Direct connection to MySql database }
  TDirMySqlConnect = class (TDirConnect)
  private
    FError: string;
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

  { Direct Mysql transaction stub }
  TDirMySqlTransact = class (TDirTransact)
  private
    FHandle: MYSQL;
    FError: string;
  protected
    function GetErrorMsg: ShortString; override;
  public
    constructor Create(AConnect: TDirMySqlConnect);

    procedure Open; override;
    procedure Close; override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    property Handle: MYSQL read FHandle;
  end;

  { Direct query to MySql }
  TDirMySqlQuery = class (TDirQuery)
  private
    FHandle:   PMYSQL_RES;
    FStoreResult: Boolean;
    FRow:      PMYSQL_ROW;
  protected
    function GetErrorMsg: ShortString; override;
    function GetEof: Boolean; override;
  public
    constructor Create(AConnect: TDirMySqlConnect; ATransact: TDirMySqlTransact);
    destructor  Destroy; override;

    function  Execute: LongInt; override;
    procedure Open; override;
    procedure Close; override;

    procedure ShowDatabases(DatabaseName: ShortString); override;
    procedure ShowTables(TableName: ShortString); override;
    procedure ShowColumns(TableName, ColumnName: ShortString); override;
    procedure ShowIndexes(TableName: ShortString); override;

    procedure First; override;
    procedure Last; override;
    procedure Prev; override;
    procedure Next; override;
    procedure Go(N: Integer); override;

    function FieldCount:  Integer; override;
    function RecordCount: Integer; override;

    function FieldName(FieldNum: Integer): ShortString; override;
    function FieldSize(FieldNum: Integer): Integer; override;
    function FieldMaxSize(FieldNum: Integer): Integer; override;
    function FieldType(FieldNum: Integer): Integer; override;
    function FieldDataType(FieldNum: Integer): TFieldType; override;
    function FieldIsNull(FieldNum: Integer): Boolean; override;
    function Field(FieldNum: Integer): string; override;
    function FieldBuffer(FieldNum: Integer): PChar; override;
    function FieldDecimals(FieldNum: Integer): Integer; override;

    function FieldFlags(FieldNum: Integer): Integer;
    function StringToSql(Value: string): string; override;

    property Handle: PMYSQL_RES read FHandle;
    property StoreResult: Boolean read FStoreResult write FStoreResult;
  end;

{ Convert mysql field types to delphi field types }
function MySqlToDelphiType(Value: Byte; Flags: Integer): TFieldType;

{ Convert MySql field types to delphi field types }
function MySqlToDelphiTypeDesc(Value: string; var Size, Precision: Integer): TFieldType;

{ Monitor list }
var MonitorList: TZMonitorList;

implementation

uses ZDBaseConst, ZExtra, ZToken, ZSqlExtra;

{*************** TDirMySqlConnect implementation *****************}

{ Class constructor }
constructor TDirMySqlConnect.Create;
begin
  inherited Create;
  Port := '3306';
end;

{ Class destructor }
destructor TDirMySqlConnect.Destroy;
begin
  inherited Destroy;
end;

{ Get an error message }
function TDirMySqlConnect.GetErrorMsg: ShortString;
begin
  if Status <> csOk then
    Result := FError
  else
    Result := '';
end;

{ Connect to database server }
procedure TDirMySqlConnect.Connect;
begin
  inherited Connect;
  if hDll = 0 then MySqlLoadLib;
  SetStatus(csOk);
  SetActive(True);
end;

{ Break connection to server }
procedure TDirMySqlConnect.Disconnect;
begin
  SetStatus(csOk);
  SetActive(False);
end;

{ Create a new database }
procedure TDirMySqlConnect.CreateDatabase(Params: string);
var
  Handle: MYSQL;
begin
  if Active then Disconnect;
  if hDll = 0 then MySqlLoadLib;
  SetStatus(csFail);

  mysql_init(@Handle);
{$IFDEF NEW_LIBMYSQL_DLL}
  mysql_options(@Handle, MYSQL_OPT_COMPRESS, nil);
{$ENDIF}
  if mysql_connect(@Handle,PChar(string(HostName)),PChar(string(Login)),
    PChar(string(Passwd))) = nil then
    FError := SConnectError
  else if mysql_create_db(@Handle, PChar(string(Database))) <> 0 then
  begin
    mysql_close(@Handle);
    FError := SDbCreateError;
  end
  else
  begin
    mysql_close(@Handle);
    SetStatus(csOk);
  end;
  MonitorList.InvokeEvent(Format('CREATE DATABASE %s',[Database]), FError, Status <> csOk);
end;

{ Drop the database }
procedure TDirMySqlConnect.DropDatabase;
var
  Handle: MYSQL;
begin
  if Active then Disconnect;
  SetStatus(csFail);
  FError := SConnectError;

  mysql_init(@Handle);
{$IFDEF NEW_LIBMYSQL_DLL}
  mysql_options(@Handle, MYSQL_OPT_COMPRESS, nil);
{$ENDIF}
  if mysql_real_connect(@Handle, PChar(string(HostName)),
    PChar(string(Login)), PChar(string(Passwd)), PChar(string(Database)),
    StrToInt(Port), nil, _CLIENT_CONNECT_WITH_DB) <> nil then
  begin
    if mysql_drop_db(@Handle, PChar(string(Database))) = 0 then
      SetStatus(csOk);
    mysql_close(@Handle);
  end;

  MonitorList.InvokeEvent(Format('DROP DATABASE %s',[Database]), 'Fail.', Status <> csOk);
end;

{******************** TDirMySqlTransact implementation **************}

{ Class constructor }
constructor TDirMySqlTransact.Create(AConnect: TDirMySqlConnect);
begin
  inherited Create;
  Connect := AConnect;
  TransactSafe := False;
end;

{ Get an error message }
function TDirMySqlTransact.GetErrorMsg: ShortString;
begin
  if Assigned(Connect) then
    FError := Trim(StrPas(@Handle._net.last_error));
  Result := FError;
end;

{ Connect transaction }
procedure TDirMySqlTransact.Open;
begin
  inherited Open;
  SetStatus(csFail);
  if not Assigned(Connect) or not Connect.Active then
    Exit;

  mysql_init(@Handle);
{$IFDEF NEW_LIBMYSQL_DLL}
//!!  mysql_options(@Handle, MYSQL_OPT_COMPRESS, nil);
{$ENDIF}
  if mysql_real_connect(@Handle, PChar(string(Connect.HostName)),
     PChar(string(Connect.Login)), PChar(string(Connect.Passwd)),
     PChar(string(Connect.Database)), StrToInt(Connect.Port), nil,
     _CLIENT_CONNECT_WITH_DB) = nil then
  begin
    FError := SConnectError;
    SetStatus(csFail);
    MonitorList.InvokeEvent(Format('CONNECT %s',[Connect.Database]), FError, True);
    Exit;
  end;
  MonitorList.InvokeEvent(Format('CONNECT %s',[Connect.Database]), 'OK.', False);

  StartTransaction;
  SetActive(Status = csOk);
end;

{ Disconnect transaction }
procedure TDirMySqlTransact.Close;
begin
  EndTransaction;
  if Active then
  begin
    mysql_close(@Handle);
    if Assigned(Connect) then
      MonitorList.InvokeEvent(Format('DISCONNECT %s',
        [Connect.Database]), 'OK.', False);
  end;
  SetActive(False);
end;

{ Start transaction stub }
procedure TDirMySqlTransact.StartTransaction;
begin
  SetStatus(csFail);
  if not Assigned(Connect) then Exit;

  if TransactSafe then
  begin
    mysql_query(@Handle, 'BEGIN');
    MonitorList.InvokeEvent('BEGIN', 'OK.', False);
  end;
  SetActive(True);
  SetStatus(csOk);
end;

{ End transaction stub }
procedure TDirMySqlTransact.EndTransaction;
begin
  if Active and TransactSafe then
  begin
    mysql_query(@Handle, 'ROLLBACK');
    MonitorList.InvokeEvent('ROLLBACK', 'OK.', False);
  end;
  SetStatus(csOk);
end;

{ Commit transaction stub }
procedure TDirMySqlTransact.Commit;
begin
  SetStatus(csFail);
  if not Active then Exit;
  SetStatus(csOk);
  if TransactSafe then
  begin
    mysql_query(@Handle, 'COMMIT');
    MonitorList.InvokeEvent('COMMIT', Error, Error <> '');
    mysql_query(@Handle, 'BEGIN');
    MonitorList.InvokeEvent('BEGIN', Error, Error <> '');
    if Error <> ''  then
      SetStatus(csFail);
  end;
end;

{ Rollback transaction stub }
procedure TDirMySqlTransact.Rollback;
begin
  SetStatus(csFail);
  if not Active then Exit;
  SetStatus(csOk);
  if TransactSafe then
  begin
    mysql_query(@Handle, 'ROLLBACK');
    MonitorList.InvokeEvent('ROLLBACK', Error, Error <> '');
    mysql_query(@Handle, 'BEGIN');
    MonitorList.InvokeEvent('BEGIN', Error, Error <> '');
    if Error <> ''  then
      SetStatus(csFail);
  end;
end;

{************************************************************}

{ Making capability with old libmysql.dll }
procedure mysql_seek(Result: PMYSQL_RES; Offset: Cardinal);
{$IFNDEF OLD_LIBMYSQL_DLL}
var
  Temp: TInt64;
begin
  Temp.Data := Offset;
  Temp.Pad := 0;
  mysql_data_seek(Result, Temp);
end;
{$ELSE}
begin
  mysql_data_seek(Result, Offset);
end;
{$ENDIF}

{****************** TDirMySqlQuery implementation ******************}

{ Class constructor }
constructor TDirMySqlQuery.Create(AConnect: TDirMySqlConnect;
  ATransact: TDirMySqlTransact);
begin
  inherited Create;
  Connect   := AConnect;
  Transact  := ATransact;
  FHandle   := nil;
  FStoreResult := True;
  FRow      := nil;
end;

{ Class destructor }
destructor TDirMySqlQuery.Destroy;
begin
  inherited Destroy;
end;

{ Get an error message }
function TDirMySqlQuery.GetErrorMsg: ShortString;
begin
  Result := '';
  if not (Status in [qsTuplesOk, qsCommandOk]) and Assigned(Transact)
    and Assigned(Connect) then
    Result := Trim(StrPas(@TDirMySqlTransact(Transact).Handle._net.last_error));
end;

{ Close an open query }
procedure TDirMySqlQuery.Close;
begin
  inherited Close;
  if FHandle <> nil then
    mysql_free_result(FHandle);
  FHandle := nil;
  SetActive(False);
  FRow := nil;
end;

{ Execute a query without rows returning }
function TDirMySqlQuery.Execute: LongInt;
var
  Temp: string;
begin
  Result := inherited Execute;
  FHandle := nil;
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  if mysql_query(@TDirMySqlTransact(Transact).Handle,
{$IFDEF DELETE_QUERY_SPACES}
    PChar(ClearSpaces(Trim(Sql)))) = 0 then
{$ELSE}
    PChar(Trim(Sql))) = 0 then
{$ENDIF}
  begin
    SetStatus(qsCommandOk);

    Temp := Sql;
    Temp := UpperCase(StrTok(Temp, ' '#9#10#13));
    if (Temp = 'SELECT') or (Temp = 'SHOW') or (Temp = 'DESCRIBE') then
    begin
      FHandle := mysql_store_result(@TDirMySqlTransact(Transact).Handle);
      if FHandle <> nil then
      begin
        SetAffectedRows(FHandle^.row_count.Data);
        mysql_free_result(FHandle);
        FHandle := nil;
      end else
        SetAffectedRows(TDirMySqlTransact(Transact).Handle.affected_rows.Data);
    end else
      SetAffectedRows(mysql_affected_rows(@TDirMySqlTransact(Transact).Handle));
  end;
  MonitorList.InvokeEvent(Sql, Transact.Error, Status <> qsCommandOk);

  Result := AffectedRows;
end;

{ Open a query }
procedure TDirMySqlQuery.Open;
begin
  if not Assigned(Transact) or not Transact.Active then Exit;
  inherited Open;

  SetActive(False);
  SetAffectedRows(0);
  FRow := nil;

  if mysql_query(@TDirMySqlTransact(Transact).Handle,
{$IFDEF DELETE_QUERY_SPACES}
    PChar(ClearSpaces(Trim(Sql)))) = 0 then
{$ELSE}
    PChar(Trim(Sql))) = 0 then
{$ENDIF}
  begin
    if StoreResult then
      FHandle := mysql_store_result(@TDirMySqlTransact(Transact).Handle)
    else
      FHandle := mysql_use_result(@TDirMySqlTransact(Transact).Handle);

    if Assigned(FHandle) then
      if FHandle^.field_count <> 0 then
      begin
        FRow := mysql_fetch_row(FHandle);
        SetActive(True);
        SetStatus(qsTuplesOk);
      end
  end;
  First;
  MonitorList.InvokeEvent(Sql, Transact.Error, Status <> qsTuplesOk);
end;

{ Get a rows quantity }
function TDirMySqlQuery.RecordCount: LongInt;
begin
  if FHandle = nil then Result := 0
  else Result := FHandle^.row_count.Data;
end;

{ Get a fields quantity }
function TDirMySqlQuery.FieldCount: Integer;
begin
  if FHandle = nil then Result := 0
  else Result := FHandle^.field_count;
end;

{ Get a field name by it number }
function TDirMySqlQuery.FieldName(FieldNum: Integer): ShortString;
var
  Field: PMYSQL_FIELD;
begin
  Result := '';
  if FHandle <> nil then
  begin
    mysql_field_seek(FHandle, FieldNum);
    Field := mysql_fetch_field(FHandle);
    if Field <> nil then
      Result := Field.name;
  end;
end;

{ Get a field size }
function TDirMySqlQuery.FieldSize(FieldNum: Integer): Integer;
var
  Lengths: PLongInt;
begin
  if (FHandle = nil) or (FieldNum >= FieldCount) then
    Result := 0
  else begin
    Lengths := mysql_fetch_lengths(FHandle);
    if Lengths <> nil then
      Result  := PLongInt(LongInt(Lengths) + FieldNum * SizeOf(LongInt))^
    else
      Result := 0;
  end;
end;

{ Get a maxumum field size }
function TDirMySqlQuery.FieldMaxSize(FieldNum: Integer): Integer;
var
  Field: PMYSQL_FIELD;
begin
  Result := 0;
  if FHandle <> nil then
  begin
    mysql_field_seek(FHandle, FieldNum);
    Field := mysql_fetch_field(FHandle);
    if Field <> nil then
      Result := Max(Field.max_length, Field.length);
  end;
end;

{ Get a field type }
function TDirMySqlQuery.FieldType(FieldNum: Integer): Integer;
var
  Field: PMYSQL_FIELD;
begin
  Result := 0;
  if FHandle <> nil then
  begin
    mysql_field_seek(FHandle, FieldNum);
    Field := mysql_fetch_field(FHandle);
    if Field <> nil then
      Result := Field._type;
  end;
end;

{ Get a field data type }
function TDirMySqlQuery.FieldDataType(FieldNum: Integer): TFieldType;
var
  Field: PMYSQL_FIELD;
begin
  Result := ftUnknown;
  if FHandle <> nil then
  begin
    mysql_field_seek(FHandle, FieldNum);
    Field := mysql_fetch_field(FHandle);
    if Field <> nil then
      Result := MySqlToDelphiType(Field._type, Field.flags);
  end;
end;

{ Get a field flags }
function TDirMySqlQuery.FieldFlags(FieldNum: Integer): Integer;
var
  Field: PMYSQL_FIELD;
begin
  Result := 0;
  if FHandle <> nil then
  begin
    mysql_field_seek(FHandle, FieldNum);
    Field := mysql_fetch_field(FHandle);
    if Field <> nil then
      Result := Field.flags;
  end;
end;

{ Get a field decimals }
function TDirMySqlQuery.FieldDecimals(FieldNum: Integer): Integer;
var
  Field: PMYSQL_FIELD;
begin
  Result := 0;
  if FHandle <> nil then
  begin
    mysql_field_seek(FHandle, FieldNum);
    Field := mysql_fetch_field(FHandle);
    if Field <> nil then
      Result := Field.decimals;
  end;
end;

{ Get a field value }
function TDirMySqlQuery.Field(FieldNum: Integer): string;
var
  Lengths: PLongInt;
  Length: LongInt;
begin
  Result := '';
  if (FHandle = nil) or (FRow = nil) then
    Exit;
  Lengths := mysql_fetch_lengths(FHandle);
  if Lengths <> nil then
    Length  := PLongInt(LongInt(Lengths)+FieldNum*SizeOf(LongInt))^
  else
    Length := 0;
  Result  := MemPas(FRow[FieldNum], Length);
end;

{ Get a field value's buffer }
function TDirMySqlQuery.FieldBuffer(FieldNum: Integer): PChar;
begin
  Result := nil;
  if (FHandle = nil) or EOF or BOF then Exit;
  Result := FRow[FieldNum];
end;

{ Define if field empty }
function TDirMySqlQuery.FieldIsNull(FieldNum: Integer): Boolean;
begin
  Result := FieldBuffer(FieldNum) = nil;
end;

{ Is end of rows? }
function TDirMySqlQuery.GetEOF: Boolean;
begin
  if StoreResult then
    Result := inherited GetEOF
  else
    Result := (FRow = nil);
end;

{ Go to first row }
procedure TDirMySqlQuery.First;
begin
  inherited First;
  if StoreResult and Assigned(FHandle) then
  begin
    mysql_seek(FHandle, RecNo);
    FRow := mysql_fetch_row(FHandle);
  end;
end;

{ Go to last row }
procedure TDirMySqlQuery.Last;
begin
  inherited Last;
  if StoreResult and Assigned(FHandle) then
  begin
    mysql_seek(FHandle, RecNo);
    FRow := mysql_fetch_row(FHandle);
  end;
end;

{ Go to prior row }
procedure TDirMySqlQuery.Prev;
begin
  inherited Prev;
  if StoreResult and Assigned(FHandle) then
  begin
    mysql_seek(FHandle, RecNo);
    FRow := mysql_fetch_row(FHandle);
  end;
end;

{ Go to next row }
procedure TDirMySqlQuery.Next;
begin
  inherited Next;
  FRow := mysql_fetch_row(FHandle);
end;

{ Go to row with N number }
procedure TDirMySqlQuery.Go(N: Integer);
begin
  inherited Go(N);
  if StoreResult and Assigned(FHandle) then
  begin
    mysql_seek(FHandle, RecNo);
    FRow := mysql_fetch_row(FHandle);
  end;
end;

{ Showes databases }
procedure TDirMySqlQuery.ShowDatabases(DatabaseName: ShortString);
begin
  if Active then Close;
  Sql := 'SHOW DATABASES';
  if DatabaseName <> '' then
    Sql := Sql + ' LIKE "' + DatabaseName + '"';
  Open;
end;

{ Showes tables of the database }
procedure TDirMySqlQuery.ShowTables(TableName: ShortString);
begin
  if Active then Close;
  Sql := 'SHOW TABLES';
  if TableName <> '' then
    Sql := Sql + ' LIKE "' + TableName + '"';
  Open;
end;

{ Showes columns of the table }
procedure TDirMySqlQuery.ShowColumns(TableName, ColumnName: ShortString);
begin
  if Active then Close;
  Sql := 'SHOW COLUMNS';
  if ColumnName <> '' then
    Sql := Sql + ' LIKE "' + ColumnName + '"';
  Sql := Sql + ' FROM ' + TableName;
  Open;
end;

{ Showes indexes of the table }
procedure TDirMySqlQuery.ShowIndexes(TableName: ShortString);
begin
  if Active then Close;
  Sql := 'SHOW INDEX FROM ' + TableName;
  Open;
end;

{ Convert string to sql format }
function TDirMySqlQuery.StringToSql(Value: string): string;
begin
  Result := ZSqlTypes.StringToSql(Value);
end;

{*********** Extra functions implementation ************}

{ Convert mysql field types to delphi field types }
function MySqlToDelphiType(Value: Byte; Flags: Integer): TFieldType;
begin
  case Value of
    FIELD_TYPE_LONGLONG:
{$IFNDEF VER100}
      Result := ftLargeInt;
{$ELSE}
      Result := ftInteger;
{$ENDIF}
    FIELD_TYPE_TINY, FIELD_TYPE_YEAR, FIELD_TYPE_LONG, FIELD_TYPE_SHORT,
    FIELD_TYPE_INT24:
      Result := ftInteger;
    FIELD_TYPE_DECIMAL, FIELD_TYPE_FLOAT, FIELD_TYPE_DOUBLE:
      Result := ftFloat;
    FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
      Result := ftDate;
    FIELD_TYPE_TIME:
      Result := ftTime;
    FIELD_TYPE_DATETIME:
      Result := ftDateTime;
    FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
    FIELD_TYPE_BLOB:
      if (Flags and BINARY_FLAG) = 0 then
        Result := ftMemo
      else Result := ftBlob;
    else
      Result := ftString;
  end;
end;

{ Calc maximum value len of enum type }
{ Value - type definition as 'enum(val1,val2...valN)' }
function EnumMaxLength(Value: string): Integer;
var
  Buffer, Token: string;
begin
  Buffer := Copy(Value, 6, Length(Value)-6);
  Result := 0;
  while Buffer <> '' do
  begin
    Token := StrTok(Buffer, ',');
    DeleteQuotes(Token);
    if Length(Token) > Result then
      Result := Length(Token);
  end;
end;

{ Convert MySql field types to delphi field types }
function MySqlToDelphiTypeDesc(Value: string; var Size, Precision: Integer): TFieldType;
begin
  Size := 0;
  Precision := 0;

{$IFNDEF VER100}
  if StrCmpBegin(Value,'bigint') then
    Result := ftLargeInt
  else
{$ENDIF}
  if (LowerCase(Value) = 'enum(''y'',''n'')')
      or (LowerCase(Value) = 'enum(''n'',''y'')') then
    Result := ftBoolean
  else if (LowerCase(Value) = 'enum(''t'',''f'')')
      or (LowerCase(Value) = 'enum(''f'',''t'')') then
    Result := ftBoolean
  else if StrCmpBegin(Value,'enum') then
  begin
    Result := ftString;
    Size   := EnumMaxLength(Value);
  end else if (Pos('int',Value) > 0) or StrCmpBegin(Value,'year') then
    Result := ftInteger
  else if StrCmpBegin(Value,'double') or StrCmpBegin(Value,'float')
    or StrCmpBegin(Value,'real') or StrCmpBegin(Value,'decimal')
    or StrCmpBegin(Value,'numeric') then
  begin
    Result    := ftFloat;
    Precision := ExtractPrecision(Value);
  end else if StrCmpBegin(Value,'char') then
  begin
    Result := ftString;
    Size   := StrToIntDef(Copy(Value,6,Pos(')',Value)-6),50);
  end else if StrCmpBegin(Value,'varchar') then
  begin
    Result := ftString;
    Size   := StrToIntDef(Copy(Value,9,Pos(')',Value)-9),50);
  end else if Value = 'date' then
    Result := ftDate
  else if Value = 'datetime' then
    Result := ftDateTime
  else if Value = 'time' then
    Result := ftTime
  else if StrCmpBegin(Value,'timestamp') then
    Result := ftDateTime
  else if Pos('blob',Value) > 0 then
    Result := ftBlob
  else if Pos('text',Value) > 0 then
    Result := ftMemo
  else if StrCmpBegin(Value,'set') then
  begin
    Result := ftString;
    Size   := Length(Value) - 4;
  end else
    Result := ftUnknown;
end;

initialization
  MonitorList := TZMonitorList.Create;
finalization
  MonitorList.Free;
end.
