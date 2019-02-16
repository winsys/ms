{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{               Oracle8 Database component               }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZOraSqlCon;

interface

{$R *.dcr}

uses
  Windows, Classes, SysUtils, ZConnect, ZDirOraSql, ZLibOraSql, ZToken;

{$INCLUDE ..\Zeos.inc}

type
  { Oracle database component }
  TZOraSqlDatabase = class(TZDatabase)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property  Database;
    property  Encoding;
    property  Login;
    property  Password;
    property  LoginPrompt;
    property  Connected;
  end;

implementation

{***************** TZOraSqlDatabase implementation *****************}

{ Class constructor }
constructor TZOraSqlDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirOraSqlConnect.Create;
end;

end.
