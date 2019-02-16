{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                MS SQL property editors                 }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZMsSqlProp;

interface

{$INCLUDE ..\ZeosDef.inc}

uses Classes, {$IFNDEF VERCLX}DsgnIntf,{$ELSE}DesignIntf,{$ENDIF} ZProperty,
  ZMsSqlStoredProc;

{$INCLUDE ..\Zeos.inc}

type
  { Property editor for TMsSqlTable }
  TZMsSqlTableNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

  { Property editor for TMsSqlDatabase }
  TZMsSqlDatabaseNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

  { Property editor for tZMsSqlStoredProc }
  TZMsSqlStoredProcNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

implementation

uses SysUtils, ZDirMsSql, ZMsSqlCon, ZMsSqlQuery;

{*********** TZMsSqlTableNameProperty implementation ***********}

procedure TZMsSqlTableNameProperty.GetValueList(Values: TStringList);
var
  Connect: TDirMsSqlConnect;
  Transact: TDirMsSqlTransact;
  Query: TDirMsSqlQuery;
begin
  Connect := TDirMsSqlConnect.Create;
  Transact := TDirMsSqlTransact.Create(Connect);
  Query := TDirMsSqlQuery.Create(Connect, Transact);
  try
    with GetComponent(0) as TZMsSqlTable do
    begin
      if Assigned(Database) then
      begin
        Connect.HostName := Database.Host;
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

{*********** TZMsSqlDatabaseNameProperty implementation ***********}

procedure TZMsSqlDatabaseNameProperty.GetValueList(Values: TStringList);
var
  _Connect: TDirMsSqlConnect;
  Transact: TDirMsSqlTransact;
  Query: TDirMsSqlQuery;
begin
  _Connect := TDirMsSqlConnect.Create;
  Transact := TDirMsSqlTransact.Create(_Connect);
  Query := TDirMsSqlQuery.Create(_Connect, Transact);
  try
    with GetComponent(0) as TZMsSqlDatabase do
    begin
      _Connect.HostName := Host;
      _Connect.Database := Database;
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

{ TZMsSqlStoredProcNameProperty }

procedure TZMsSqlStoredProcNameProperty.GetValueList(Values: TStringList);
var
  Connect: TDirMsSqlConnect;
  Transact: TDirMsSqlTransact;
//  Query: TDirMsSqlQuery;
  StoredProc: TDirMsSqlStoredProc;
begin
  Connect := TDirMsSqlConnect.Create;
  Transact := TDirMsSqlTransact.Create(Connect);
//  Query := TDirMsSqlQuery.Create(Connect, Transact);
  StoredProc := TDirMsSqlStoredProc.Create(Connect, Transact);
  try
    with GetComponent(0) as TZMsSqlStoredProc do
    begin
      if Assigned(Database) then
      begin
        Connect.HostName := Database.Host;
        Connect.Database := Database.Database;
        Connect.Login := Database.Login;
        Connect.Passwd := Database.Password;
      end;
      Connect.Connect;
      Transact.Open;
    end;
    if Transact.Active then
    begin
//      Query.Sql := 'select name from sysobjects where type=''P''';
//      Query.Open;
      StoredProc.ShowStoredProcs;
//      if Query.Active then
      if StoredProc.Active then
      begin
        while not StoredProc.Eof do
        begin
          Values.Add(StoredProc.Field(2));
          StoredProc.Next;
        end;
        StoredProc.Close;
      end;
//        while not Query.Eof do
//        begin
//          Values.Add(Query.Field(0));
//          Query.Next;
//        end;
    end;
  finally
//    Query.Free;
    StoredProc.Free;
    Transact.Free;
    Connect.Free;
  end;
end;

end.
