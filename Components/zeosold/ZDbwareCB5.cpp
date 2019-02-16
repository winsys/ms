//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ZDbwareCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("dbase\ZUpdateSql.pas");
USERES("dbase\ZUpdateSql.dcr");
USEUNIT("dbase\ZConnect.pas");
USEUNIT("dbase\ZDBaseConst.pas");
USEUNIT("dbase\ZDBaseReg.pas");
USEUNIT("dbase\ZDirSql.pas");
USEFORMNS("dbase\ZLinkProp.pas", Zlinkprop, frmLinkFields);
USEUNIT("dbase\ZProperty.pas");
USEUNIT("dbase\ZQuery.pas");
USEUNIT("dbase\ZSqlBuffer.pas");
USEUNIT("dbase\ZSqlExtra.pas");
USEUNIT("dbase\ZSqlItems.pas");
USEUNIT("dbase\ZSqlParser.pas");
USEUNIT("dbase\ZSqlScript.pas");
USEUNIT("dbase\ZSqlTypes.pas");
USEUNIT("dbase\ZTransact.pas");
USERES("dbase\ZTransact.dcr");
USEUNIT("dbase\ZBlobStream.pas");
USEUNIT("dbase\ZSqlScanner.pas");
USEUNIT("dbase\ZStoredProc.pas");
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
