//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ZMsSqlCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("dbase\ZMsSqlTr.pas");
USERES("dbase\ZMsSqlTr.dcr");
USEUNIT("dbase\ZLibMsSql.pas");
USEUNIT("dbase\ZMsSqlCon.pas");
USERES("dbase\ZMsSqlCon.dcr");
USEUNIT("dbase\ZMsSqlProp.pas");
USEUNIT("dbase\ZMsSqlQuery.pas");
USERES("dbase\ZMsSqlQuery.dcr");
USEUNIT("dbase\ZMsSqlReg.pas");
USEUNIT("dbase\ZDirMsSql.pas");
USEUNIT("dbase\ZMsSqlStoredProc.pas");
USEPACKAGE("ZDbwareCB5.bpi");
USEPACKAGE("ZCommonCB5.bpi");
USEPACKAGE("Vcldb50.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
