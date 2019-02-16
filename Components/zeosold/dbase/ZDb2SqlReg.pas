{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              DB2 components registration               }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDb2SqlReg;

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

uses Classes, ZDb2SqlCon, ZDb2SqlTr, ZDb2SqlQuery
  {$IFDEF WITH_PROPEDIT}, ZDb2SqlProp, {$IFNDEF VERCLX}DsgnIntf
  {$ELSE}DesignIntf{$ENDIF} {$ENDIF};

{**********************************************************}

{ Register component in a component palette }
procedure Register;
begin
  RegisterComponents(ZEOS_DB_PALETTE, [TZDb2SqlDatabase]);
(*
{$IFDEF WITH_PROPEDIT}
  RegisterPropertyEditor(TypeInfo(string), TZDb2SqlDatabase, 'Database',
    TZDb2SqlDatabaseNameProperty);
{$ENDIF}
*)

  RegisterComponents(ZEOS_DB_PALETTE, [TZDb2SqlTransact]);

  RegisterComponents(ZEOS_DB_PALETTE, [TZDb2SqlTable]);
  RegisterComponents(ZEOS_DB_PALETTE, [TZDb2SqlQuery]);
{$IFDEF WITH_PROPEDIT}
  RegisterPropertyEditor(TypeInfo(string), TZDb2SqlTable, 'TableName',
    TZDb2SqlTableNameProperty);
{$ENDIF}
end;

end.
