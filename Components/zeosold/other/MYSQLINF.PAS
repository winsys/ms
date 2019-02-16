{*************************************************************}
{                      Simple Query pack                      }
{                Copyright © 1998,99 Korzh company            }
{            http://www.korzh.com/simplequery.htm             }
{                   mailto:info@korzh.com                     }
{-------------------------------------------------------------}
{                    TMySQLInfo component                     }
{                Created by Fredrick Bartlett                 }
{                  palmtreefrb@earthlink.net                  }
{               for Zeos mySQL Database Objects               }
{                  last updated: Jul-12-2000                  }
{*************************************************************}


unit mysqlinf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, ZQuery, ZMySqlQuery, ZConnect, ZMySqlCon, ZTransact, ZMySqlTr, KDbInfo;

type
  TMySQLInfo = class(TKDBInfo)
  private
   FHost:   string;
   FPort:     string;
   FLogin:    string;
   FPassword: string;
  protected
    procedure TablesByDatabase(TablesList : TStrings);override;
    procedure FieldsByTable(const TableName : string; FieldsList : TDBFieldList);override;
  public
    procedure GetDatabaseNames(AList : TStrings);override;
    procedure GetSQLValues(SQL : String;AValues,AItems : TStrings);override;
  published
    property Host: string read FHost write FHost;
    property Port: string read FPort write FPort;
    property Login: string read FLogin write FLogin;
    property Password: string read FPassword write FPassword;
  end;

procedure Register;

implementation

procedure TMYSQLInfo.GetDatabaseNames(AList : TStrings);
var
 ZQuery:    TZMySqlQuery;
 ZTrans:    TZMySqlTransact;
 ZDatabase: TZMySqlDatabase;
 i:         integer;
begin
 ZQuery:=TZMySqlQuery.Create(nil);
 ZTrans:=TZMySqlTransact.create(nil);
 ZDatabase:=TZMySqlDatabase.create(nil);

 ZDatabase.Database:='mysql';
 ZDatabase.host:=FHost;
 ZDatabase.Port:=FPort;
 ZDatabase.Login:=FLogin;
 ZDatabase.Password:=FPassword;

 ZTrans.Database:=zDatabase;

 ZQuery.Database:=ZDatabase;
 ZQuery.Transaction:=ZTrans;
 ZQuery.close;
 ZQuery.sql.clear;
 ZQuery.sql.Add('show databases');
 ZQuery.open;

 AList.Clear;
 ZQuery.First;
 for i:=0 to ZQuery.recordcount -1 do begin
  AList.Add(ZQuery.Fields[0].asString);
  ZQuery.Next;
 end;
 ZQuery.close;
 ZDatabase.Disconnect;
end;

procedure TMYSQLInfo.TablesByDatabase(TablesList : TStrings);
var
 ZQuery:    TZMySqlQuery;
 ZTrans:    TZMySqlTransact;
 ZDatabase: TZMySqlDatabase;
 i:         integer;
begin
 ZQuery:=TZMySqlQuery.Create(nil);
 ZTrans:=TZMySqlTransact.create(nil);
 ZDatabase:=TZMySqlDatabase.create(nil);

 ZDatabase.Database:=databaseName;
 ZDatabase.host:=FHost;
 ZDatabase.Port:=FPort;
 ZDatabase.Login:=FLogin;
 ZDatabase.Password:=FPassword;

 ZTrans.Database:=zDatabase;

 ZQuery.Database:=ZDatabase;
 ZQuery.Transaction:=ZTrans;
 ZQuery.close;
 ZQuery.sql.clear;
 ZQuery.sql.Add('show tables');
 ZQuery.open;

 TablesList.Clear;
 ZQuery.First;
 for i:=0 to ZQuery.recordcount -1 do begin
  TablesList.Add(ZQuery.Fields[0].asString);
  ZQuery.Next;
 end;
 ZQuery.close;
 ZDatabase.Disconnect;
end;

procedure TMYSQLInfo.FieldsByTable(const TableName : string; FieldsList : TDBFieldList);
var
 ZQuery:    TZMySqlQuery;
 ZTrans:    TZMySqlTransact;
 ZDatabase: TZMySqlDatabase;
 i : integer;
 FI : TDBFieldInfo;
begin
 ZQuery:=TZMySqlQuery.Create(nil);
 ZTrans:=TZMySqlTransact.create(nil);
 ZDatabase:=TZMySqlDatabase.create(nil);

 ZDatabase.Database:=databaseName;
 ZDatabase.host:=FHost;
 ZDatabase.Port:=FPort;
 ZDatabase.Login:=FLogin;
 ZDatabase.Password:=FPassword;

 ZTrans.Database:=zDatabase;

 ZQuery.Database:=ZDatabase;
 ZQuery.Transaction:=ZTrans;
 ZQuery.close;
 ZQuery.sql.clear;
 ZQuery.sql.Add('select * from '+tableName+' limit 1');
 ZQuery.open;

 for i := 0 to ZQuery.FieldDefs.Count - 1 do  begin
  FI := TDBFieldInfo.Create;
  FI.FieldName := ZQuery.FieldDefs[i].Name;
  FI.FieldType := ZQuery.FieldDefs[i].DataType;
  FI.FieldSize := ZQuery.FieldDefs[i].Size;
  FieldsList.Add(FI);
 end;
 ZQuery.close;
 ZDatabase.Disconnect;
end;

procedure TMYSQLInfo.GetSQLValues(SQL : string; AValues,AItems : TStrings);
var
 ZQuery:    TZMySqlQuery;
 ZTrans:    TZMySqlTransact;
 ZDatabase: TZMySqlDatabase;
 i : integer;
begin
 AValues.Clear;
 AItems.Clear;
 ZQuery:=TZMySqlQuery.Create(nil);
 ZTrans:=TZMySqlTransact.create(nil);
 ZDatabase:=TZMySqlDatabase.create(nil);

 ZDatabase.Database:=databaseName;
 ZDatabase.host:=FHost;
 ZDatabase.Port:=FPort;
 ZDatabase.Login:=FLogin;
 ZDatabase.Password:=FPassword;

 ZTrans.Database:=zDatabase;

 ZQuery.Database:=ZDatabase;
 ZQuery.Transaction:=ZTrans;

 Screen.Cursor := crHourGlass;
 try
   ZQuery.SQL.add(SQL);
   try
     ZQuery.Open;
     if ZQuery.FieldCount < 2 then raise EKDBInfoError.Create('Field count must be >= 2');
     while not ZQuery.EOF do
     begin
       AValues.Add(ZQuery.Fields[0].AsString);
       AItems.Add(ZQuery.Fields[1].AsString);
       ZQuery.Next;
     end;
   except
     // You can raise exception here
   end;
 finally
   Screen.Cursor := crDefault;
   ZQuery.close;
   ZDatabase.disconnect;
 end;
end;

procedure Register;
begin
  RegisterComponents('SimpleQuery', [TMySQLInfo]);
end;

end.
