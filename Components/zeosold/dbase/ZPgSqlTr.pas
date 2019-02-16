{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{            PostgreSql Transaction component            }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZPgSqlTr;

interface

{$R *.dcr}

uses
  Classes, ZDirPgSql, ZPgSqlCon, ZTransact, ZLibPgSql, ZSqlTypes, DB, ZDBaseConst;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { PostgreSql transaction }
  TZPgSqlTransact = class(TZTransact)
  private
    FNotice: AnsiString;
    function  GetDatabase: TZPgSqlDatabase;
    function GetPID: Integer;
    procedure SetDatabase(Value: TZPgSqlDatabase);
    function GetLastInsertOid: Oid;
    function  GetTransIsolation: TZPgSqlTransIsolation;
    procedure SetTransIsolation(const Value: TZPgSqlTransIsolation);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Connect; override;
    procedure Recovery(Force: Boolean); override;
    procedure Reset;
    function ExecSql(Sql: WideString): LongInt; override;

    procedure AddMonitor(Monitor: TZMonitor); override;
    procedure DeleteMonitor(Monitor: TZMonitor); override;

    property PID: Integer read GetPID;
    property LastInsertOid: Oid read GetLastInsertOid;
    property Notice: AnsiString read FNotice;
  published
    property Database: TZPgSqlDatabase read GetDatabase write SetDatabase;
    property AutoRecovery;
    property TransactSafe;
    property TransIsolation: TZPgSqlTransIsolation read GetTransIsolation
      write SetTransIsolation;
  end;

  { PostgreSQL class for asynchronous notifying}
  TZPgSqlNotify = class(TZNotify)
  private
    FDatabase: TZPgSqlDatabase;
  protected
    procedure Disconnect(Sender: TObject); virtual;
    procedure SetDatabase(Value: TZPgSqlDatabase); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Database: TZPgSqlDatabase read FDatabase write SetDatabase;
  end;

implementation

{***************** TZPgSqlTransact implementation *****************}

{ Class constructor }
constructor TZPgSqlTransact.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirPgSqlTransact.Create(nil);
  FQuery  := TDirPgSqlQuery.Create(nil, TDirPgSqlTransact(FHandle));
  AutoRecovery := True;
  FDatabaseType := dtPostgreSql;
end;

{ Get database component }
function TZPgSqlTransact.GetDatabase: TZPgSqlDatabase;
begin
  Result := TZPgSqlDatabase(FDatabase);
end;

{ Set database component }
procedure TZPgSqlTransact.SetDatabase(Value: TZPgSqlDatabase);
begin
  inherited SetDatabase(Value);
end;

{ Retrieve backend server's process id (PID) }
function TZPgSqlTransact.GetPID: Integer;
begin
  Result := TDirPgSqlTransact(Handle).PID;
end;

{ Get last inserted oid value }
function TZPgSqlTransact.GetLastInsertOid: Oid;
begin
  Result := TDirPgSqlQuery(FQuery).LastInsertOid;
end;

{ Get transaction type }
function TZPgSqlTransact.GetTransIsolation: TZPgSqlTransIsolation;
begin
  Result := TDirPgSqlTransact(FHandle).TransIsolation;
end;

{ Set transaction type }
procedure TZPgSqlTransact.SetTransIsolation(const Value: TZPgSqlTransIsolation);
begin
  if Value <> TDirPgSqlTransact(FHandle).TransIsolation then
  begin
    Disconnect;
    TDirPgSqlTransact(FHandle).TransIsolation := Value;
  end;
end;

{ Execute Sql. Overrided to provide NoticeProcessor support}
function TZPgSqlTransact.ExecSql(Sql: WideString): LongInt;
begin
  TDirPgSqlTransact(Handle).Notice := '';
  Result := inherited ExecSql(Sql);
  { Setting possible notices from the database backend }
  fNotice := TDirPgSqlTransact(Handle).Notice;
end;

{ Recovery after errors }
procedure TZPgSqlTransact.Recovery(Force: Boolean);
begin
  if AutoRecovery or Force then
    FHandle.Rollback;
end;

{ Connect to Sql-database }
procedure TZPgSqlTransact.Connect;
begin
  inherited Connect;
  ExecSql('SET DateStyle TO ''ISO''');
end;

{ Add monitor into monitor list }
procedure TZPgSqlTransact.AddMonitor(Monitor: TZMonitor);
begin
  ZDirPgSql.MonitorList.AddMonitor(Monitor);
end;

{ Delete monitor from monitor list }
procedure TZPgSqlTransact.DeleteMonitor(Monitor: TZMonitor);
begin
  ZDirPgSql.MonitorList.DeleteMonitor(Monitor);
end;

{ Reconnect to the database server}
procedure TZPgSqlTransact.Reset;
begin
  TDirPgSqlTransact(Handle).Reset;
end;

{***************** TZPgSqlNotify implementation *****************}

{ Class constructor }
constructor TZPgSqlNotify.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirPgSqlNotify.Create(nil, nil);
  SetTransact(TZPgSqlTransact.Create(Self));
  TZPgSqlTransact(FTransact).TransactSafe:=False;
  TZPgSqlTransact(FTransact).OnBeforeDisconnect:=Disconnect;
end;

{ Handles external Transaction's disable }
procedure TZPgSqlNotify.Disconnect(Sender: TObject);
begin
  TZPgSqlTransact(FTransact).OnBeforeDisconnect:=nil;
  Active:=False;
  TZPgSqlTransact(FTransact).OnBeforeDisconnect:=Disconnect;
end;

destructor TZPgSqlNotify.Destroy;
begin
  FTransact.Free;
  inherited Destroy;
end;

procedure TZPgSqlNotify.SetDatabase(Value: TZPgSqlDatabase);
begin
  if FDatabase<>Value then
  begin
    if Active then Close;
    FDatabase:=Value;
    FTransact.Database:=FDatabase;
  end;
end;

procedure TZPgSqlNotify.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDatabase) and (Operation = opRemove) then
    Database:=nil;
end;

{ Open autoactivated datasets }
procedure TZPgSqlNotify.Loaded;
begin
  inherited Loaded;
  if Active and Assigned(Database) then
    Open;
end;

end.
