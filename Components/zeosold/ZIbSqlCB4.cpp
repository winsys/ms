//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("ZDbwareCB4.bpi");
USEPACKAGE("ZCommonCB4.bpi");
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
