{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{          PostgreSql components registration            }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZPgSqlReg;

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

uses Classes, ZPgSqlCon, ZPgSqlTr, ZPgSqlQuery
{$IFDEF WITH_PROPEDIT}, ZPgSqlProp,{$IFNDEF VERCLX}DsgnIntf
{$ELSE}DesignIntf{$ENDIF}{$ENDIF};

{ Register components in a component palette }
procedure Register;
begin
  RegisterComponents(ZEOS_DB_PALETTE, [TZPgSqlDatabase]);
{$IFDEF WITH_PROPEDIT}
  RegisterPropertyEditor(TypeInfo(string), TZPgSqlDatabase, 'Database',
    TZPgSqlDatabaseNameProperty);
{$ENDIF}

  RegisterComponents(ZEOS_DB_PALETTE, [TZPgSqlTransact]);

  RegisterComponents(ZEOS_DB_PALETTE, [TZPgSqlTable]);
  RegisterComponents(ZEOS_DB_PALETTE, [TZPgSqlQuery]);
  RegisterComponents(ZEOS_DB_PALETTE, [TZPgSqlNotify]);
{$IFDEF WITH_PROPEDIT}
  RegisterPropertyEditor(TypeInfo(string), TZPgSqlTable, 'TableName',
    TZPgSqlTableNameProperty);
{$ENDIF}
end;

end.
