//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("ZDbwareCB5.bpi");
USEPACKAGE("ZCommonCB5.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
USEUNIT("dbase\ZOraSqlReg.pas");
USEUNIT("dbase\ZLibOraSql.pas");
USEUNIT("dbase\ZOraSqlCon.pas");
USERES("dbase\ZOraSqlCon.dcr");
USEUNIT("dbase\ZOraSqlProp.pas");
USEUNIT("dbase\ZOraSqlQuery.pas");
USERES("dbase\ZOraSqlQuery.dcr");
USEUNIT("dbase\ZDirOraSql.pas");
USEUNIT("dbase\ZOraSqlTr.pas");
USERES("dbase\ZOraSqlTr.dcr");
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
