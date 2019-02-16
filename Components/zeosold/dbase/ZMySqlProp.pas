{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                 MySql property editors                 }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZMySqlProp;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses Classes, ZProperty, {$IFNDEF VERCLX}DsgnIntf{$ELSE}DesignIntf{$ENDIF};

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Property editor for TMySqlTable }
  TZMySqlTableNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

  { Property editor for TMySqlDatabase }
  TZMySqlDatabaseNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

implementation

uses SysUtils, ZDirMySql, ZMySqlCon, ZMySqlQuery;

{*********** TMySqlTableNameProperty implementation ***********}

procedure TZMySqlTableNameProperty.GetValueList(Values: TStringList);
var
  Connect: TDirMySqlConnect;
  Transact: TDirMySqlTransact;
  Query: TDirMySqlQuery;
begin
  Connect := TDirMySqlConnect.Create;
  Transact := TDirMySqlTransact.Create(Connect);
  Query := TDirMySqlQuery.Create(Connect, Transact);
  try
    with GetComponent(0) as TZMySqlTable do
    begin
      if Assigned(Database) then
      begin
        Connect.HostName := Database.Host;
        Connect.Port := Database.Port;
        Connect.Database := Database.Database;
        Connect.Login := Database.Login;
        Connect.Passwd := Database.Password;
      end;
      Connect.Connect;
      Transact.Open;
    end;
    if Transact.Active then
    begin
      Query.ShowTables('');
      if Query.Active then
        while not Query.Eof do
        begin
          Values.Add(Query.Field(0));
          Query.Next;
        end;
    end;
  finally
    Query.Free;
    Transact.Free;
    Connect.Free;
  end;
end;

{*********** TMySqlDatabaseNameProperty implementation ***********}

procedure TZMySqlDatabaseNameProperty.GetValueList(Values: TStringList);
var
  _Connect: TDirMySqlConnect;
  Transact: TDirMySqlTransact;
  Query: TDirMySqlQuery;
begin
  _Connect := TDirMySqlConnect.Create;
  Transact := TDirMySqlTransact.Create(_Connect);
  Query := TDirMySqlQuery.Create(_Connect, Transact);
  try
    with GetComponent(0) as TZMySqlDatabase do
    begin
      _Connect.HostName := Host;
      _Connect.Port := Port;
      _Connect.Database := 'mysql';
      _Connect.Login := Login;
      _Connect.Passwd := Password;

      _Connect.Connect;
      Transact.Open;
    end;
    if Transact.Active then
    begin
      Query.ShowDatabases('');
      if Query.Active then
        while not Query.Eof do
        begin
          Values.Add(Query.Field(0));
          Query.Next;
        end;
    end;
  finally
    Query.Free;
    Transact.Free;
    _Connect.Free;
  end;
end;

end.
