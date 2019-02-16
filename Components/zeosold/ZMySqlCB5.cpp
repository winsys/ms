//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("ZDbwareCB5.bpi");
USEPACKAGE("ZCommonCB5.bpi");
USEUNIT("dbase\ZMySQLTr.pas");
USERES("dbase\ZMySQLTr.dcr");
USEUNIT("dbase\ZDirMySql.pas");
USEUNIT("dbase\ZMySQLCon.pas");
USERES("dbase\ZMySQLCon.dcr");
USEUNIT("dbase\ZMySQLProp.pas");
USEUNIT("dbase\ZMySQLQuery.pas");
USERES("dbase\ZMySQLQuery.dcr");
USEUNIT("dbase\ZLibMySQL.pas");
USEUNIT("dbase\ZMySqlReg.pas");
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
