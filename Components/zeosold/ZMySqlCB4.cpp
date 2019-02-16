//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("ZDbwareCB4.bpi");
USEPACKAGE("ZCommonCB4.bpi");
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
