{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                 DB2 Database component                 }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDb2SqlCon;

interface

{$R *.dcr}

uses
  Classes, ZConnect, ZDirDb2Sql;

{$INCLUDE ..\Zeos.inc}

type
  { Oracle database component }
  TZDb2SqlDatabase = class(TZDatabase)
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

{***************** TZDb2SqlDatabase implementation *****************}

{ Class constructor }
constructor TZDb2SqlDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirDb2SqlConnect.Create;
end;

end.
