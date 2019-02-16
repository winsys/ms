{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{          Interbase components registration             }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZIbSqlReg;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

procedure Register;

implementation

uses Classes, ZIbSqlCon, ZIbSqlTr, ZIbSqlQuery, DB, ZQuery, ZIbSqlNotify,
  {$IFDEF WITH_PROPEDIT} ZIbSqlProp,
  {$IFNDEF VERCLX}DsgnIntf{$ELSE}DesignIntf{$ENDIF} {$ENDIF};

{**********************************************************}

{ Register component in a component palette }
procedure Register;
begin
  RegisterComponents(ZEOS_DB_PALETTE, [TZIbSqlDatabase]);
  RegisterComponents(ZEOS_DB_PALETTE, [TZIbSqlTransact]);

  RegisterComponents(ZEOS_DB_PALETTE, [TZIbSqlTable]);
  RegisterComponents(ZEOS_DB_PALETTE, [TZIbSqlQuery]);

  RegisterComponents(ZEOS_DB_PALETTE, [TZIbSqlStoredProc]);
  RegisterComponents(ZEOS_DB_PALETTE, [TZIbSqlNotify]);

  RegisterFields([TZBCDField]);

{$IFDEF WITH_PROPEDIT}
(*
{$IFDEF WITH_PROPEDIT}
  RegisterPropertyEditor(TypeInfo(string), TZIbSqlDatabase, 'Database',
    TZIbSqlDatabaseNameProperty);
{$ENDIF}
*)
  RegisterPropertyEditor(TypeInfo(string), TZIbSqlTable, 'TableName',
    TZIbSqlTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TZIbSqlStoredProc, 'StoredProcName',
    TZIbSqlStoredProcNameProperty);
{$ENDIF}
end;

end.
