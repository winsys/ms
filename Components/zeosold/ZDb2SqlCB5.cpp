//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ZDb2SqlCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("dbase\ZDirDb2Sql.pas");
USEUNIT("dbase\ZDb2SqlProp.pas");
USEUNIT("dbase\ZDb2SqlQuery.pas");
USEUNIT("dbase\ZDb2SqlReg.pas");
USEUNIT("dbase\ZDb2SqlTr.pas");
USEUNIT("dbase\ZDb2SqlCon.pas");
USEUNIT("dbase\ZLibDb2Sql.pas");
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
