{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{            Interbase Transaction component             }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZIbSqlTr;

interface

{$R *.dcr}

uses
  SysUtils, Classes, DB, ZDirIbSql, ZIbSqlCon, ZTransact, ZSqlExtra, ZLibIbSql,
  ZToken, ZSqlTypes;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Transaction Interbase component }
  TZIbSqlTransact = class(TZTransact)
  private
    FParams: TStrings;

    function GetDatabase: TZIbSqlDatabase;
    procedure SetDatabase(Value: TZIbSqlDatabase);
    procedure SetParams(Value: TStrings);
    procedure ProcessParams;
    function  GetTransIsolation: TZIbSqlTransIsolation;
    procedure SetTransIsolation(const Value: TZIbSqlTransIsolation);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect; override;
    function ExecFunc(Func: WideString): WideString; override;

    procedure AddMonitor(Monitor: TZMonitor); override;
    procedure DeleteMonitor(Monitor: TZMonitor); override;
  published
    property Params: TStrings read FParams write SetParams;
    property Database: TZIbSqlDatabase read GetDatabase write SetDatabase;
    property TransIsolation: TZIbSqlTransIsolation read GetTransIsolation
      write SetTransIsolation;
  end;

implementation

uses ZDbaseConst, ZDirSql;

{***************** TZIbSqlTransact implementation *****************}

{ Class constructor }
constructor TZIbSqlTransact.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := TDirIbSqlTransact.Create(nil);
  FQuery := TDirIbSqlQuery.Create(nil, TDirIbSqlTransact(FHandle));
  FParams := TStringList.Create;
  FDatabaseType := dtInterbase;
end;

{ Class destructor }
destructor TZIbSqlTransact.Destroy;
begin
  inherited Destroy;
  FParams.Free;
end;

{ Get database component }
function TZIbSqlTransact.ExecFunc(Func: WideString): WideString;
begin
  if Pos('FROM', UpperCase(Func)) <= 0 then
    Func := Func + ' FROM RDB$DATABASE';
  Result := inherited ExecFunc(Func);
end;

{ Get database component }
function TZIbSqlTransact.GetDatabase: TZIbSqlDatabase;
begin
  Result := TZIbSqlDatabase(FDatabase);
end;

{ Set database component }
procedure TZIbSqlTransact.SetDatabase(Value: TZIbSqlDatabase);
begin
  inherited SetDatabase(Value);
end;

{ Assign new transaction parameters }
procedure TZIbSqlTransact.SetParams(Value: TStrings);
begin
  FParams.Assign(Value);
end;

{
  [
  isc_tpb_consistency,
  isc_tpb_concurrency,
  isc_tpb_shared,
  isc_tpb_protected,
  isc_tpb_exclusive,
  isc_tpb_wait,
  isc_tpb_nowait,
  isc_tpb_read,
  isc_tpb_write,
  isc_tpb_lock_read,
  isc_tpb_lock_write,
  isc_tpb_verb_time,
  isc_tpb_commit_time,
  isc_tpb_ignore_limbo,
  isc_tpb_read_committed,
  isc_tpb_autocommit,
  isc_tpb_rec_version,
  isc_tpb_no_rec_version,
  isc_tpb_restart_requests,
  isc_tpb_no_auto_undo
  ]

  [
  'consistency',
  'concurrency',
  'shared',
  'protected',
  'exclusive',
  'wait',
  'nowait',
  'read',
  'write',
  'lock_read',
  'lock_write',
  'verb_time',
  'commit_time',
  'ignore_limbo',
  'read_committed',
  'autocommit',
  'rec_version',
  'no_rec_version',
  'restart_requests',
  'no_auto_undo'
  ]
}

{ Process transaction parameter block }
procedure TZIbSqlTransact.ProcessParams;
const
  MAX_TPB_PARAMS = 14;
  ParamNames: array[1..MAX_TPB_PARAMS] of string = (
      'consistency', 'exclusive', 'concurrency',
      'shared', 'wait', 'nowait', 'read',
      'write', 'ignore_limbo', 'read_committed',
      'rec_version', 'no_rec_version', 'lock_read',
      'lock_write'
    );
  ParamIndexes: array[1..MAX_TPB_PARAMS] of SmallInt = (
      isc_tpb_consistency, isc_tpb_exclusive, isc_tpb_concurrency,
      isc_tpb_shared, isc_tpb_wait, isc_tpb_nowait, isc_tpb_read,
      isc_tpb_write, isc_tpb_ignore_limbo, isc_tpb_read_committed,
      isc_tpb_rec_version, isc_tpb_no_rec_version, isc_tpb_lock_read,
      isc_tpb_lock_write
    );
var
  I, J: Integer;
  Buffer, ParamName, ParamValue: string;
  ParamList: TIbParamList;
  Found: boolean;
const
  TPBPrefix = 'isc_tpb_';
begin
  ParamList := TDirIbSqlTransact(Handle).Params;
  ParamList.Clear;
  for I := 0 to Params.Count - 1 do
  begin
    Buffer := Params[I];
    if Trim(Buffer) = '' then
      Continue;

    ParamName := LowerCase(StrTok(Buffer, ' ='#9#10#13));
    ParamValue := StrTok(Buffer, ' ='#9#10#13);

    if Pos(TPBPrefix, ParamName) = 1 then
      Delete(ParamName, 1, Length(TPBPrefix));

    Found := False;
    for J := 1 to MAX_TPB_PARAMS do
    begin
      if ParamName = ParamNames[J] then
      begin
        ParamList.Add(ParamIndexes[J], ParamValue);
        Found := True;
        Break;
      end;
    end;

    if not Found then
      raise Exception.CreateFmt(SIncorrectField, [ParamName]);
  end;
end;

{ Connect to database }
procedure TZIbSqlTransact.Connect;
begin
  if Connected then Exit;
  ProcessParams;

  inherited Connect;
end;

{ Get transaction type }
function TZIbSqlTransact.GetTransIsolation: TZIbSqlTransIsolation;
begin
  Result := TDirIbSqlTransact(FHandle).TransIsolation;
end;

{ Set transaction type }
procedure TZIbSqlTransact.SetTransIsolation(const Value: TZIbSqlTransIsolation);
begin
  if Value <> TDirIbSqlTransact(FHandle).TransIsolation then
  begin
    Disconnect;
    TDirIbSqlTransact(FHandle).TransIsolation := Value;
  end;
end;

{ Add monitor into monitor list }
procedure TZIbSqlTransact.AddMonitor(Monitor: TZMonitor);
begin
  ZDirIbSql.MonitorList.AddMonitor(Monitor);
end;

{ Delete monitor from monitor list }
procedure TZIbSqlTransact.DeleteMonitor(Monitor: TZMonitor);
begin
  ZDirIbSql.MonitorList.DeleteMonitor(Monitor);
end;

end.
