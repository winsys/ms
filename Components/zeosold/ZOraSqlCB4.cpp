//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("ZDbwareCB4.bpi");
USEPACKAGE("ZCommonCB4.bpi");
USEUNIT("dbase\ZOraSqlTr.pas");
USERES("dbase\ZOraSqlTr.dcr");
USEUNIT("dbase\ZLibOraSql.pas");
USEUNIT("dbase\ZOraSqlCon.pas");
USERES("dbase\ZOraSqlCon.dcr");
USEUNIT("dbase\ZOraSqlProp.pas");
USEUNIT("dbase\ZOraSqlQuery.pas");
USERES("dbase\ZOraSqlQuery.dcr");
USEUNIT("dbase\ZOraSqlReg.pas");
USEUNIT("dbase\ZDirOraSql.pas");
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
