//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("ZDbwareCB5.bpi");
USEPACKAGE("ZCommonCB5.bpi");
USEUNIT("dbase\ZIbSqlTr.pas");
USERES("dbase\ZIbSqlTr.dcr");
USEUNIT("dbase\ZDirIbSql.pas");
USEUNIT("dbase\ZIbSqlProp.pas");
USEUNIT("dbase\ZIbSqlCon.pas");
USERES("dbase\ZIbSqlCon.dcr");
USEUNIT("dbase\ZIbSqlQuery.pas");
USERES("dbase\ZIbSqlQuery.dcr");
USEUNIT("dbase\ZLibIbSql.pas");
USEUNIT("dbase\ZIbSqlReg.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
USEUNIT("dbase\ZIbSqlNotify.pas");
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
