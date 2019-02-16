{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{               MySql Database component                 }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZMySqlCon;

interface

{$R *.dcr}

uses
  {$IFNDEF LINUX}Windows,{$ENDIF} SysUtils, DB, Classes, ZConnect, ZDirSql,
  ZDirMySql, ZLibMySql, ZToken;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { MySql direct database class }
  TZMySqlDatabase = class(TZDatabase)
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

{***************** TZMySqlDatabase implementation *****************}

{ Class constructor }
constructor TZMySqlDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirMySqlConnect.Create;
  FPort := '3306';
end;

end.
