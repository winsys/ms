{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Interbase Database component              }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZIbSqlCon;

interface

{$R *.dcr}

uses
  Classes, SysUtils, ZConnect, ZDirIbSql, ZLibIbSql, ZToken;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Interbase database component }
  TZIbSqlDatabase = class(TZDatabase)
  private
    FParams: TStrings;

    function  GetSqlDialect: Integer;
    procedure SetSqlDialect(Value: Integer);
    function GetCharSet: string;
    procedure SetCharSet(Value: string);
    function  GetSqlRole: string;
    procedure SetSqlRole(Value: string);
    procedure SetParams(Value: TStrings);
    procedure ProcessParams;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Connect; override;
  published
    property  Host;
    property  Database;
    property  Encoding;
    property  Login;
    property  Password;
    property  LoginPrompt;
    property  Params: TStrings read FParams write SetParams;
    property  SqlDialect: Integer read GetSqlDialect write SetSqlDialect;
    property  SqlRole: string read GetSQLRole write SetSQLRole;
    property  CharSet: string  read GetCharSet write SetCharSet;
    property  Connected;
  end;

implementation

uses ZDbaseConst;

{***************** TZIbSqlDatabase implementation *****************}

{ Class constructor }
constructor TZIbSqlDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirIbSqlConnect.Create;
  FParams := TStringList.Create;
end;

{ Class destructor }
destructor TZIbSqlDatabase.Destroy;
begin
  inherited Destroy;
  FParams.Free;
end;

{ Get interbase sql dialect value }
function TZIbSqlDatabase.GetSqlDialect: Integer;
begin
  Result := TDirIbSqlConnect(FHandle).Dialect;
end;

{ Set interbase sql dialect value }
procedure TZIbSqlDatabase.SetSqlDialect(Value: Integer);
begin
  TDirIbSqlConnect(FHandle).Dialect := Value;
end;

{Get Database CharSet}
function TZIbSqlDatabase.GetCharSet: string;
begin
  Result := TDirIbSqlConnect(FHandle).CharSet;
end;

{Set Database CharSet}
procedure TZIbSqlDatabase.SetCharSet(Value: string);
begin
  TDirIbSqlConnect(FHandle).CharSet := Value;
end;

{Get Sql Role}
function  TZIbSqlDatabase.GetSqlRole: string;
begin
  Result := TDirIbSqlConnect(FHandle).SqlRole
end;

{Set Sql Role}
procedure TZIbSqlDatabase.SetSqlRole(Value: string);
begin
  TDirIbSqlConnect(FHandle).SqlRole := Value;
end;

{ Assign new database parameters }
procedure TZIbSqlDatabase.SetParams(Value: TStrings);
begin
  FParams.Assign(Value);
end;

{ Process database parameter block }
procedure TZIbSqlDatabase.ProcessParams;
const
  MAX_DPB_PARAMS = 21;
  ParamNames: array[1..MAX_DPB_PARAMS] of string = (
      'user_name', 'password', 'password_enc',
      'sys_user_name', 'license', 'encrypt_key',
      'lc_messages', 'lc_ctype', 'sql_role_name',
      'num_buffers', 'dbkey_scope', 'force_write',
      'no_reserve', 'damaged', 'verify', 'sweep',
      'sweep_interval', 'activate_shadow',
      'delete_shadow', 'begin_log', 'quit_log'
    );
  ParamIndexes: array[1..MAX_DPB_PARAMS] of SmallInt = (
      isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc,
      isc_dpb_sys_user_name, isc_dpb_license, isc_dpb_encrypt_key,
      isc_dpb_lc_messages, isc_dpb_lc_ctype, isc_dpb_sql_role_name,
      isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
      isc_dpb_no_reserve, isc_dpb_damaged, isc_dpb_verify, isc_dpb_sweep,
      isc_dpb_sweep_interval, isc_dpb_activate_shadow,
      isc_dpb_delete_shadow, isc_dpb_begin_log, isc_dpb_quit_log
    );
var
  I, J: Integer;
  Buffer, ParamName, ParamValue: string;
  ParamList: TIbParamList;
const
  BPBPrefix = 'isc_dpb_';
begin
  ParamList := TDirIbSqlConnect(Handle).Params;
  ParamList.Clear;
  for I := 0 to Params.Count - 1 do
  begin
    Buffer := Params[I];
    if Trim(Buffer) = '' then
      Continue;

    ParamName := LowerCase(StrTok(Buffer, ' ='#9#10#13));
    ParamValue := StrTok(Buffer, ' ='#9#10#13);

    if Pos(BPBPrefix, ParamName) = 1 then
      Delete(ParamName, 1, Length(BPBPrefix));

    for J := 1 to MAX_DPB_PARAMS do
    begin
      if ParamName = ParamNames[J] then
      begin
        ParamList.Add(ParamIndexes[J], ParamValue);
        Break;
      end;
    end;
  end;
end;

{ Connect to database }
procedure TZIbSqlDatabase.Connect;
begin
  if Connected then Exit;
  ProcessParams;
  inherited Connect;
end;

end.
