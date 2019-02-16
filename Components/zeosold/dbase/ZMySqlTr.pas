{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              MySql Transaction component               }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZMySqlTr;

interface

{$R *.dcr}

uses
  {$IFNDEF LINUX}Windows, {$ENDIF} SysUtils, DB, Classes, ZDirSql, ZDirMySql,
  ZLibMySql, ZToken, ZMySqlCon, ZTransact, ZSqlTypes;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { MySql transaction component (stub) }
  TZMySqlTransact = class(TZTransact)
  private
    function  GetDatabase: TZMySqlDatabase;
    procedure SetDatabase(Value: TZMySqlDatabase);
  public
    constructor Create(AOwner: TComponent); override;

    {$IFDEF USE_GENERATORS}
    function GetGen(GenName: ShortString): LongInt;
    {$ENDIF}

    function GetHostInfo: ShortString;
    function GetProtoInfo: Cardinal;
    function GetClientInfo: ShortString;
    function GetServerInfo: ShortString;
    function GetServerStat: ShortString;

    procedure Kill(PID: Integer);
    procedure Ping;
    procedure Shutdown;

    procedure AddMonitor(Monitor: TZMonitor); override;
    procedure DeleteMonitor(Monitor: TZMonitor); override;
  published
    property Database: TZMySqlDatabase read GetDatabase write SetDatabase;
    property TransactSafe;
  end;

implementation

uses ZDbaseConst;

{***************** TZMySqlTransact implementation *****************}

{ Class constructor }
constructor TZMySqlTransact.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirMySqlTransact.Create(nil);
  FQuery  := TDirMySqlQuery.Create(nil, TDirMySqlTransact(FHandle));
  FDatabaseType := dtMySql;
end;

{ Get database component }
function TZMySqlTransact.GetDatabase: TZMySqlDatabase;
begin
  Result := TZMySqlDatabase(FDatabase);
end;

{ Set database component }
procedure TZMySqlTransact.SetDatabase(Value: TZMySqlDatabase);
begin
  inherited SetDatabase(Value);
end;

{$IFDEF USE_GENERATORS}
{ Get a generator value }
function TZMySqlTransact.GetGen(GenName: ShortString): LongInt;
begin
  if not Connected then
    DatabaseError(LoadStr(SNotConnected));
  Result := StrToIntDef(ExecFunc('GETGEN("'+GenName+'")'),0);
end;
{$ENDIF}

{ Get host information }
function TZMySqlTransact.GetHostInfo: ShortString;
begin
  if Connected then
    Result := mysql_get_host_info(@TDirMySqlTransact(Handle).Handle)
  else
    Result := '';
end;

{ Get protocol information }
function TZMySqlTransact.GetProtoInfo: Cardinal;
begin
  if Connected then
    Result := mysql_get_proto_info(@TDirMySqlTransact(Handle).Handle)
  else
    Result := 0;
end;

{ Get client information }
function TZMySqlTransact.GetClientInfo: ShortString;
begin
  Result := mysql_get_client_info;
end;

{ Get server information }
function TZMySqlTransact.GetServerInfo: ShortString;
begin
  if Connected then
    Result := mysql_get_server_info(@TDirMySqlTransact(Handle).Handle)
  else
    Result := '';
end;

{ Get serve status }
function TZMySqlTransact.GetServerStat: ShortString;
begin
  if Connected then
    Result := mysql_stat(@TDirMySqlTransact(Handle).Handle)
  else
    Result := '';
end;

{ Kill server process }
procedure TZMySqlTransact.Kill(PID: Integer);
begin
  if mysql_kill(@TDirMySqlTransact(Handle).Handle, PID) <> 0 then
    DatabaseError(Handle.Error);
end;

{ Check connection }
procedure TZMySqlTransact.Ping;
begin
  if mysql_ping(@TDirMySqlTransact(Handle).Handle) <> 0 then
    DatabaseError(Handle.Error);
end;

{ Shutdown server }
procedure TZMySqlTransact.Shutdown;
begin
  if mysql_shutdown(@TDirMySqlTransact(Handle).Handle) <> 0 then
    DatabaseError(Handle.Error);
end;

{ Add monitor into monitor list }
procedure TZMySqlTransact.AddMonitor(Monitor: TZMonitor);
begin
  ZDirMySql.MonitorList.AddMonitor(Monitor);
end;

{ Delete monitor from monitor list }
procedure TZMySqlTransact.DeleteMonitor(Monitor: TZMonitor);
begin
  ZDirMySql.MonitorList.DeleteMonitor(Monitor);
end;

end.
