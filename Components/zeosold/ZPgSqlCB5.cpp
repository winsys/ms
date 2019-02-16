//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("ZDbwareCB5.bpi");
USEPACKAGE("ZCommonCB5.bpi");
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
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
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
