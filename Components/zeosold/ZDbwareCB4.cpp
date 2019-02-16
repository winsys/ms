//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEUNIT("dbase\ZUpdateSQL.pas");
USEUNIT("dbase\ZConnect.pas");
USEUNIT("dbase\ZDBaseConst.pas");
USEUNIT("dbase\ZDirSql.pas");
USEUNIT("dbase\ZProperty.pas");
USEUNIT("dbase\ZQuery.pas");
USEUNIT("dbase\ZSqlExtra.pas");
USEUNIT("dbase\ZTransact.pas");
USEUNIT("dbase\ZBlobStream.pas");
USEUNIT("dbase\ZDBaseReg.pas");
USEPACKAGE("ZCommonCB4.bpi");
USEUNIT("dbase\ZSqlTypes.pas");
USEUNIT("dbase\ZSqlItems.pas");
USEUNIT("dbase\ZSqlParser.pas");
USEUNIT("dbase\ZSqlScript.pas");
USEUNIT("dbase\ZSqlBuffer.pas");
USEUNIT("dbase\ZSqlScanner.pas");
USEUNIT("dbase\ZStoredProc.pas");
USEFORMNS("dbase\ZLinkProp.pas", Zlinkprop, frmLinkFields);
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
