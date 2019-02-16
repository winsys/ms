{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{             Oracle8 Transaction component              }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZOraSqlTr;

interface

{$R *.dcr}

uses
  Windows, SysUtils, Classes, Db, ZDirOraSql, ZOraSqlCon, ZTransact, ZSqlExtra,
  ZLibOraSql, ZToken, ZSqlTypes;

{$INCLUDE ..\Zeos.inc}

type
  { Transaction Interbase component }
  TZOraSqlTransact = class(TZTransact)
  private
    function GetDatabase: TZOraSqlDatabase;
    procedure SetDatabase(Value: TZOraSqlDatabase);
    function  GetTransIsolation: TZOraSqlTransIsolation;
    procedure SetTransIsolation(const Value: TZOraSqlTransIsolation);
  public
    constructor Create(AOwner: TComponent); override;

//    function ExecSqlParams(Sql: WideString; Params: TVarRecArray;
//      ParamCount: Integer): LongInt; override;
    function ExecFunc(Func: WideString): WideString; override;

    procedure AddMonitor(Monitor: TZMonitor); override;
    procedure DeleteMonitor(Monitor: TZMonitor); override;
  published
    property Database: TZOraSqlDatabase read GetDatabase write SetDatabase;
    property TransIsolation: TZOraSqlTransIsolation read GetTransIsolation
      write SetTransIsolation;
  end;

implementation

uses ZDbaseConst, ZDirSql;

{***************** TZOraSqlTransact implementation *****************}

{ Class constructor }
constructor TZOraSqlTransact.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirOraSqlTransact.Create(nil);
  FQuery  := TDirOraSqlQuery.Create(nil, TDirOraSqlTransact(FHandle));
  FDatabaseType := dtOracle;
end;

{ Get transaction type }
function TZOraSqlTransact.GetTransIsolation: TZOraSqlTransIsolation;
begin
  Result := TDirOraSqlTransact(FHandle).TransIsolation;
end;

{ Set transaction type }
procedure TZOraSqlTransact.SetTransIsolation(const Value: TZOraSqlTransIsolation);
begin
  if Value <> TDirOraSqlTransact(FHandle).TransIsolation then
  begin
    Disconnect;
    TDirOraSqlTransact(FHandle).TransIsolation := Value;
  end;
end;

{ Add monitor into monitor list }
procedure TZOraSqlTransact.AddMonitor(Monitor: TZMonitor);
begin
  ZDirOraSql.MonitorList.AddMonitor(Monitor);
end;

{ Delete monitor from monitor list }
procedure TZOraSqlTransact.DeleteMonitor(Monitor: TZMonitor);
begin
  ZDirOraSql.MonitorList.DeleteMonitor(Monitor);
end;

{ Get database component }
function TZOraSqlTransact.ExecFunc(Func: WideString): WideString;
begin
  if Pos('FROM', UpperCase(Func)) <= 0 then
    Func := Func + ' FROM DUAL';
  Result := inherited ExecFunc(Func);
end;

{ Get database component }
function TZOraSqlTransact.GetDatabase: TZOraSqlDatabase;
begin
  Result := TZOraSqlDatabase(FDatabase);
end;

{ Set database component }
procedure TZOraSqlTransact.SetDatabase(Value: TZOraSqlDatabase);
begin
  inherited SetDatabase(Value);
end;
(*
{ Execute a query }
function TZOraSqlTransact.ExecSqlParams(Sql: WideString;
  Params: TVarRecArray; ParamCount: Integer): LongInt;
var
  OldCursor: TCursor;
  Error: string;
begin
  if not FConnected then
    DatabaseError(LoadStr(SNotConnected));

  OldCursor := Screen.Cursor;
  try
    if toHourGlass in FOptions then
      Screen.Cursor := crSqlWait;

    FQuery.Sql := Sql;
    Result := TDirOraSqlQuery(FQuery).ExecuteParams(Params, ParamCount);
    if FQuery.Status <> qsCommandOk then
    begin
      Error := FQuery.Error;
      Recovery(False);
      DatabaseError(Error);
    end else
      DoDataChange(Sql);

    if FAutoCommit then Commit;
  finally
    Screen.Cursor := OldCursor;
  end;
end;
*)

end.
