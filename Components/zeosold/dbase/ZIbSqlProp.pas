{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{               Interbase property editors               }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZIbSqlProp;

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
  { Property editor for TIbSqlTable }
  TZIbSqlTableNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

  { Property editor for tZIbSqlStoredProc }
  TZIbSqlStoredProcNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

implementation

uses SysUtils, ZDirIbSql, ZIbSqlCon, ZIbSqlQuery;

{*********** TZIbSqlTableNameProperty implementation ***********}

procedure TZIbSqlTableNameProperty.GetValueList(Values: TStringList);
var
  Connect: TDirIbSqlConnect;
  Transact: TDirIbSqlTransact;
  Query: TDirIbSqlQuery;
begin
  with GetComponent(0) as TZIbSqlTable do
  begin
    if not Assigned(Database) then Exit;

    if (FHost = Database.Host) and (FDatabase = Database.Database)
      and FRead and Assigned(FList) then
    begin
      Values.Assign(FList);
      Exit;
    end;
  end;

  Connect := TDirIbSqlConnect.Create;
  Transact := TDirIbSqlTransact.Create(Connect);
  Query := TDirIbSqlQuery.Create(Connect, Transact);
  try
    with GetComponent(0) as TZIbSqlTable do
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
      begin
        if not Assigned(FList) then
          FList := TStringList.Create;
        FList.Clear;
        while not Query.Eof do
        begin
          FList.Add(Trim(Query.Field(1)));
          Query.Next;
        end;
        Values.Assign(FList);
        FRead := True;
        with GetComponent(0) as TZIbSqlTable do
        begin
          FHost := Database.Host;
          FDatabase := Database.Database;
        end;
      end;
    end;
  finally
    Query.Free;
    Transact.Free;
    Connect.Free;
  end;
end;

{ TZMsSqlStoredProcNameProperty }
procedure TZIbSqlStoredProcNameProperty.GetValueList(Values: TStringList);
var
  AQuery: TDirIbSqlQuery;
begin
  with GetComponent(0) as TZIbSqlStoredProc do
    if Assigned(Database) and Assigned(Transaction) then
  try
    Database.Connect;
    Transaction.Connect;

    AQuery := TDirIbSqlQuery(Transaction.QueryHandle);

    AQuery.ShowProcs('');
    while not AQuery.EOF do
    begin
      Values.Add(AQuery.Field(1));
      AQuery.Next;
    end;
    AQuery.Close;
  except
  end;
end;

end.
