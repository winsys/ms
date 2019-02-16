{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                Oracle8 property editors                }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZOraSqlProp;

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
  TZOraSqlTableNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

implementation

uses SysUtils, ZDirOraSql, ZOraSqlCon, ZOraSqlQuery;

{*********** TZOraSqlTableNameProperty implementation ***********}

procedure TZOraSqlTableNameProperty.GetValueList(Values: TStringList);
var
  Connect: TDirOraSqlConnect;
  Transact: TDirOraSqlTransact;
  Query: TDirOraSqlQuery;
begin
  with GetComponent(0) as TZOraSqlTable do
  begin
    if not Assigned(Database) then Exit;

    if (FHost = Database.Host) and (FDatabase = Database.Database)
      and FRead and Assigned(FList) then
    begin
      Values.Assign(FList);
      Exit;
    end;
  end;

  Connect  := TDirOraSqlConnect.Create;
  Transact := TDirOraSqlTransact.Create(Connect);
  Query    := TDirOraSqlQuery.Create(Connect, Transact);
  try
    with GetComponent(0) as TZOraSqlTable do
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
        with GetComponent(0) as TZOraSqlTable do
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
