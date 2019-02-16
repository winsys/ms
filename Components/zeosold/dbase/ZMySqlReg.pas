{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{            MySql components registration               }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZMySqlReg;

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

uses Classes, ZMySqlCon, ZMySqlTr, ZMySqlQuery
{$IFDEF WITH_PROPEDIT}, ZMySqlProp,
{$IFNDEF VERCLX}DsgnIntf{$ELSE}DesignIntf{$ENDIF} {$ENDIF};

{ Register components in a component palette }
procedure Register;
begin
  RegisterComponents(ZEOS_DB_PALETTE, [TZMySqlDatabase]);
{$IFDEF WITH_PROPEDIT}
  RegisterPropertyEditor(TypeInfo(string), TZMySqlDatabase, 'Database',
    TZMySqlDatabaseNameProperty);
{$ENDIF}

  RegisterComponents(ZEOS_DB_PALETTE, [TZMySqlTransact]);

  RegisterComponents(ZEOS_DB_PALETTE, [TZMySqlTable]);
  RegisterComponents(ZEOS_DB_PALETTE, [TZMySqlQuery]);
{$IFDEF WITH_PROPEDIT}
  RegisterPropertyEditor(TypeInfo(string), TZMySqlTable, 'TableName',
    TZMySqlTableNameProperty);
{$ENDIF}
end;

end.
