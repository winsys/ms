{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{           PostgreSql Transaction component             }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZPgSqlCon;

interface

{$R *.dcr}

uses
  Classes, ZConnect, ZDirPgSql;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Postgresql database }
  TZPgSqlDatabase = class(TZDatabase)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property  Host;
    property  Port;
    property  Database;
    property  Encoding;
    property  Login;
    property  Password;
    property  LoginPrompt;
    property  Connected;
  end;

implementation

{***************** TPgSqlDatabase implementation *****************}

{ Class constructor }
constructor TZPgSqlDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirPgSqlConnect.Create;
  Port := '5432';
end;

end.
