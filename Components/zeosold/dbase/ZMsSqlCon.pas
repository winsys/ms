{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              MS SQL Transaction component              }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZMsSqlCon;

interface

{$R *.dcr}

uses
  Classes, ZConnect, ZDirMsSql;

{$INCLUDE ..\Zeos.inc}

type
  { MS SQL database }
  TZMsSqlDatabase = class(TZDatabase)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property  Host;
    property  Database;
    property  Encoding;
    property  Login;
    property  Password;
    property  LoginPrompt;
    property  Connected;
  end;

implementation

{***************** TMsSqlDatabase implementation *****************}

{ Class constructor }
constructor TZMsSqlDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirMsSqlConnect.Create;
end;

end.
