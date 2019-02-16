{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{               DB2 Transaction component                }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDb2SqlTr;

interface

{$R *.dcr}

uses
  Windows, SysUtils, Classes, Db, ZDirDb2Sql, ZDb2SqlCon, ZTransact, ZSqlExtra,
  ZLibDb2Sql, ZToken, ZSqlTypes;

{$INCLUDE ..\Zeos.inc}

type
  { Transaction Interbase component }
  TZDb2SqlTransact = class(TZTransact)
  private
    function GetDatabase: TZDb2SqlDatabase;
    procedure SetDatabase(Value: TZDb2SqlDatabase);
    function  GetTransIsolation: TZDb2SqlTransIsolation;
    procedure SetTransIsolation(const Value: TZDb2SqlTransIsolation);
  public
    constructor Create(AOwner: TComponent); override;

    function ExecFunc(Func: WideString): WideString; override;

    procedure AddMonitor(Monitor: TZMonitor); override;
    procedure DeleteMonitor(Monitor: TZMonitor); override;
  published
    property Database: TZDb2SqlDatabase read GetDatabase write SetDatabase;
    property TransIsolation: TZDb2SqlTransIsolation read GetTransIsolation
      write SetTransIsolation;
    property TransactSafe;
  end;

implementation

uses ZDbaseConst, ZDirSql;

{***************** TZDb2SqlTransact implementation *****************}

{ Class constructor }
constructor TZDb2SqlTransact.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirDb2SqlTransact.Create(nil);
  FQuery  := TDirDb2SqlQuery.Create(nil, TDirDb2SqlTransact(FHandle));
  FDatabaseType := dtDb2;
end;

{ Get transaction type }
function TZDb2SqlTransact.GetTransIsolation: TZDb2SqlTransIsolation;
begin
  Result := TDirDb2SqlTransact(FHandle).TransIsolation;
end;

{ Set transaction type }
procedure TZDb2SqlTransact.SetTransIsolation(const Value: TZDb2SqlTransIsolation);
begin
  if Value <> TDirDb2SqlTransact(FHandle).TransIsolation then
  begin
    Disconnect;
    TDirDb2SqlTransact(FHandle).TransIsolation := Value;
  end;
end;

{ Add monitor into monitor list }
procedure TZDb2SqlTransact.AddMonitor(Monitor: TZMonitor);
begin
  ZDirDb2Sql.MonitorList.AddMonitor(Monitor);
end;

{ Delete monitor from monitor list }
procedure TZDb2SqlTransact.DeleteMonitor(Monitor: TZMonitor);
begin
  ZDirDb2Sql.MonitorList.DeleteMonitor(Monitor);
end;

{ Get database component }
function TZDb2SqlTransact.ExecFunc(Func: WideString): WideString;
begin
  if Pos('FROM', UpperCase(Func)) <= 0 then
    Func := Func + ' FROM SYSIBM.SYSDUMMY1';
  Result := inherited ExecFunc(Func);
end;

{ Get database component }
function TZDb2SqlTransact.GetDatabase: TZDb2SqlDatabase;
begin
  Result := TZDb2SqlDatabase(FDatabase);
end;

{ Set database component }
procedure TZDb2SqlTransact.SetDatabase(Value: TZDb2SqlDatabase);
begin
  inherited SetDatabase(Value);
end;

end.
