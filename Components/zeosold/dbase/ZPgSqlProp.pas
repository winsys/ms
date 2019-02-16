{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              PostgreSql property editors               }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZPgSqlProp;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses Classes, ZProperty, {$IFNDEF VERCLX}DsgnIntf{$ELSE}DesignEditors{$ENDIF};

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Property editor for TPgSqlTable }
  TZPgSqlTableNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

  { Property editor for TPgSqlDatabase }
  TZPgSqlDatabaseNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

implementation

uses SysUtils, ZDirPgSql, ZPgSqlCon, ZPgSqlQuery;

{*********** TZPgSqlTableNameProperty implementation ***********}

procedure TZPgSqlTableNameProperty.GetValueList(Values: TStringList);
var
  Connect: TDirPgSqlConnect;
  Transact: TDirPgSqlTransact;
  Query: TDirPgSqlQuery;
begin
  Connect := TDirPgSqlConnect.Create;
  Transact := TDirPgSqlTransact.Create(Connect);
  Query := TDirPgSqlQuery.Create(Connect, Transact);
  try
    with GetComponent(0) as TZPgSqlTable do
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
          Values.Add(Query.Field(1));
          Query.Next;
        end;
    end;
  finally
    Query.Free;
    Transact.Free;
    Connect.Free;
  end;
end;

{*********** TZPgSqlDatabaseNameProperty implementation ***********}

procedure TZPgSqlDatabaseNameProperty.GetValueList(Values: TStringList);
var
  _Connect: TDirPgSqlConnect;
  Transact: TDirPgSqlTransact;
  Query: TDirPgSqlQuery;
begin
  _Connect := TDirPgSqlConnect.Create;
  Transact := TDirPgSqlTransact.Create(_Connect);
  Query := TDirPgSqlQuery.Create(_Connect, Transact);
  try
    with GetComponent(0) as TZPgSqlDatabase do
    begin
      _Connect.HostName := Host;
      _Connect.Port := Port;
      _Connect.Database := 'template1';
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
