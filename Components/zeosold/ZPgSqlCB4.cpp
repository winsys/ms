//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("ZDbwareCB4.bpi");
USEPACKAGE("ZCommonCB4.bpi");
USEUNIT("dbase\ZPgSqlTr.pas");
USERES("dbase\ZPgSqlTr.dcr");
USEUNIT("dbase\ZDirPgSql.pas");
USEUNIT("dbase\ZPgSqlCon.pas");
USERES("dbase\ZPgSqlCon.dcr");
USEUNIT("dbase\ZPgSqlProp.pas");
USEUNIT("dbase\ZPgSqlQuery.pas");
USERES("dbase\ZPgSqlQuery.dcr");
USEUNIT("dbase\ZLibPgSql.pas");
USEUNIT("dbase\ZPgSqlReg.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
