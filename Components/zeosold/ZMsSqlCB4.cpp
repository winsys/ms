//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("ZMsSqlCB4.res");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEUNIT("dbase\ZMsSqlTr.pas");
USERES("dbase\ZMsSqlTr.dcr");
USEUNIT("dbase\ZMsSqlCon.pas");
USERES("dbase\ZMsSqlCon.dcr");
USEUNIT("dbase\ZMsSqlProp.pas");
USEUNIT("dbase\ZMsSqlQuery.pas");
USERES("dbase\ZMsSqlQuery.dcr");
USEUNIT("dbase\ZMsSqlReg.pas");
USEUNIT("dbase\ZDirMsSql.pas");
USEUNIT("dbase\ZMsSqlStoredProc.pas");
USEPACKAGE("ZDbwareCB4.bpi");
USEPACKAGE("ZCommonCB4.bpi");
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
