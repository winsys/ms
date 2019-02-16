{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              MS SQL Transaction component              }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZMsSqlTr;

interface

{$R *.dcr}

uses
  Classes, ZDirMsSql, ZMsSqlCon, ZTransact, ZSqlTypes;

{$INCLUDE ..\Zeos.inc}

type
  { MS SQL transaction }
  TZMsSqlTransact = class(TZTransact)
  private
    function  GetDatabase: TZMsSqlDatabase;
    procedure SetDatabase(Value: TZMsSqlDatabase);
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddMonitor(Monitor: TZMonitor); override;
    procedure DeleteMonitor(Monitor: TZMonitor); override;
  published
    property Database: TZMsSqlDatabase read GetDatabase write SetDatabase;
    property TransactSafe;
  end;

implementation

{***************** TZMsSqlTransact implementation *****************}

{ Class constructor }
constructor TZMsSqlTransact.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirMsSqlTransact.Create(nil);
  FQuery  := TDirMsSqlQuery.Create(nil, TDirMsSqlTransact(FHandle));
  FDatabaseType := dtMsSql;
end;

{ Get database component }
function TZMsSqlTransact.GetDatabase: TZMsSqlDatabase;
begin
  Result := TZMsSqlDatabase(FDatabase);
end;

{ Set database component }
procedure TZMsSqlTransact.SetDatabase(Value: TZMsSqlDatabase);
begin
  inherited SetDatabase(Value);
end;

{ Add monitor into monitor list }
procedure TZMsSqlTransact.AddMonitor(Monitor: TZMonitor);
begin
  ZDirMsSql.MonitorList.AddMonitor(Monitor);
end;

{ Delete monitor from monitor list }
procedure TZMsSqlTransact.DeleteMonitor(Monitor: TZMonitor);
begin
  ZDirMsSql.MonitorList.DeleteMonitor(Monitor);
end;

end.
