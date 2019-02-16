//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEUNIT("common\ZExtra.pas");
USEUNIT("common\ZConvert.pas");
USEUNIT("common\ZToken.pas");
USEUNIT("common\ZMatch.pas");
USEUNIT("common\ZParser.pas");
USEUNIT("common\ZCommonConst.pas");
USEUNIT("common\ZList.pas");
USEUNIT("common\ZScanner.pas");
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
