//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl40.bpi");
USEUNIT("common\ZExtra.pas");
USEUNIT("common\ZConvert.pas");
USEUNIT("common\ZToken.pas");
USEUNIT("common\ZMatch.pas");
USEUNIT("common\ZParser.pas");
USEUNIT("common\ZCommonConst.pas");
USEUNIT("common\ZScanner.pas");
USEPACKAGE("vcldb40.bpi");
USEUNIT("common\ZList.pas");
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
