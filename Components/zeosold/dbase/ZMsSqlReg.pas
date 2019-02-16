{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              MS SQL components registration            }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZMsSqlReg;

interface

{$INCLUDE ..\ZeosDef.inc}
{$INCLUDE ..\Zeos.inc}

procedure Register;

implementation

uses Classes, ZMsSqlCon, ZMsSqlTr, ZMsSqlQuery, ZMsSqlStoredProc
  {$IFDEF WITH_PROPEDIT}, ZMsSqlProp, {$IFNDEF VERCLX}DsgnIntf
  {$ELSE}DesignIntf{$ENDIF}{$ENDIF};

{ Register components in a component palette }
procedure Register;
begin
  RegisterComponents(ZEOS_DB_PALETTE, [TZMsSqlDatabase]);
{$IFDEF WITH_PROPEDIT}
  RegisterPropertyEditor(TypeInfo(string), TZMsSqlDatabase, 'Database',
    TZMsSqlDatabaseNameProperty);
{$ENDIF}

  RegisterComponents(ZEOS_DB_PALETTE, [TZMsSqlTransact]);

  RegisterComponents(ZEOS_DB_PALETTE, [TZMsSqlTable]);
  RegisterComponents(ZEOS_DB_PALETTE, [TZMsSqlQuery]);
  RegisterComponents(ZEOS_DB_PALETTE, [TZMsSqlStoredProc]);
{$IFDEF WITH_PROPEDIT}
  RegisterPropertyEditor(TypeInfo(string), TZMsSqlTable, 'TableName',
    TZMsSqlTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TZMsSqlStoredProc, 'StoredProcName',
    TZMsSqlStoredProcNameProperty);
{$ENDIF}
end;

end.
