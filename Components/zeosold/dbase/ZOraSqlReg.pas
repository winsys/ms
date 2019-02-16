{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{           Oracle8 components registration              }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZOraSqlReg;

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

uses Classes, ZOraSqlCon, ZOraSqlTr, ZOraSqlQuery
  {$IFDEF WITH_PROPEDIT}, ZOraSqlProp, {$IFNDEF VERCLX}DsgnIntf
  {$ELSE}DesignIntf{$ENDIF} {$ENDIF};

{**********************************************************}

{ Register component in a component palette }
procedure Register;
begin
  RegisterComponents(ZEOS_DB_PALETTE, [TZOraSqlDatabase]);
(*
{$IFDEF WITH_PROPEDIT}
  RegisterPropertyEditor(TypeInfo(string), TZOraSqlDatabase, 'Database',
    TZOraSqlDatabaseNameProperty);
{$ENDIF}
*)

  RegisterComponents(ZEOS_DB_PALETTE, [TZOraSqlTransact]);

  RegisterComponents(ZEOS_DB_PALETTE, [TZOraSqlTable]);
  RegisterComponents(ZEOS_DB_PALETTE, [TZOraSqlQuery]);
{$IFDEF WITH_PROPEDIT}
  RegisterPropertyEditor(TypeInfo(string), TZOraSqlTable, 'TableName',
    TZOraSqlTableNameProperty);
{$ENDIF}
end;

end.
