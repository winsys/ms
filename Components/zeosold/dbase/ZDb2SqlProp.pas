{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                 DB2 property editors                   }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDb2SqlProp;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses Classes, {$IFNDEF VERCLX}DsgnIntf,{$ELSE}DesignIntf,{$ENDIF} ZProperty;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Property editor for TOraSqlTable }
  TZDb2SqlTableNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

implementation

uses SysUtils, ZDirDb2Sql, ZDb2SqlCon, ZDb2SqlQuery;

{*********** TZDb2SqlTableNameProperty implementation ***********}

procedure TZDb2SqlTableNameProperty.GetValueList(Values: TStringList);
var
  Connect: TDirDb2SqlConnect;
  Transact: TDirDb2SqlTransact;
  Query: TDirDb2SqlQuery;
begin
  with GetComponent(0) as TZDb2SqlTable do
  begin
    if not Assigned(Database) then Exit;

    if (FHost = Database.Host) and (FDatabase = Database.Database)
      and FRead and Assigned(FList) then
    begin
      Values.Assign(FList);
      Exit;
    end;
  end;

  Connect  := TDirDb2SqlConnect.Create;
  Transact := TDirDb2SqlTransact.Create(Connect);
  Query    := TDirDb2SqlQuery.Create(Connect, Transact);
  try
    with GetComponent(0) as TZDb2SqlTable do
    begin
      if Assigned(Database) then
      begin
        Connect.Database := Database.Database;
        Connect.Login    := Database.Login;
        Connect.Passwd   := Database.Password;
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
          FList.Add(Trim(Query.Field(0)));
          Query.Next;
        end;
        Query.Close;
        Values.Assign(FList);
        FRead := True;
        with GetComponent(0) as TZDb2SqlTable do
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

end.
