{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                Simple MySql Query Test                 }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit simple_mysql1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, DBCtrls, Db, ComCtrls, ToolWin,
  Grids, DBGrids, ZMySQLQuery, ZMySQLCon, ZDirMySQL, StdCtrls, ZMySQLTr,
  ZSQLExtra, DBTables, ZQuery, ZTransact, ZConnect, ImgList;

type
  TfrmMain = class(TForm)
    dsMain: TDataSource;
    ilMain: TImageList;
    pnMain: TPanel;
    lbHost: TLabel;
    lbDb: TLabel;
    lbLogin: TLabel;
    lbPswd: TLabel;
    edHost: TEdit;
    edDb: TEdit;
    edLogin: TEdit;
    edPswd: TEdit;
    cbxRequest: TCheckBox;
    cbxStore: TCheckBox;
    Panel1: TPanel;
    mmSql: TMemo;
    Splitter2: TSplitter;
    dgGrid: TDBGrid;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnExit: TButton;
    btnSort: TButton;
    btnClearSort: TButton;
    btnLocate: TButton;
    btnFilter: TButton;
    dbMain: TZMySqlDatabase;
    trMain: TZMySqlTransact;
    qrMain: TZMySqlQuery;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnSortClick(Sender: TObject);
    procedure btnClearSortClick(Sender: TObject);
    procedure btnLocateClick(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

{ Connect to database }
procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  dbMain.Host := edHost.Text;
  dbMain.Database := edDb.Text;
  dbMain.Login := edLogin.Text;
  dbMain.Password := edPswd.Text;
  qrMain.RequestLive := cbxRequest.Checked;
  if cbxStore.Checked then
    qrMain.ExtraOptions := qrMain.ExtraOptions + [moStoreResult]
  else
    qrMain.ExtraOptions := qrMain.ExtraOptions - [moStoreResult];

  qrMain.SQL.Assign(mmSql.Lines);
  qrMain.Open;
end;

{ Disconnect from database }
procedure TfrmMain.btnDisconnectClick(Sender: TObject);
begin
  qrMain.Close;
  trMain.Disconnect;
  dbMain.Disconnect;
end;

{ Exit program }
procedure TfrmMain.btnExitClick(Sender: TObject);
begin
  Close;
end;

{ Sort by selected field }
procedure TfrmMain.btnSortClick(Sender: TObject);
begin
  qrMain.SortByField(dgGrid.SelectedField.FieldName);
end;

{ Clear sort }
procedure TfrmMain.btnClearSortClick(Sender: TObject);
begin
  qrMain.SortClear;
end;

{ Locate on selected field }
procedure TfrmMain.btnLocateClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := '';
  if InputQuery('Locate Query', 'Field Value "'+dgGrid.SelectedField.FieldName+'" :', Temp) then
    if qrMain.Locate('"'+dgGrid.SelectedField.FieldName+'"',Temp,
      [loCaseInsensitive{, loPartialKey}]) then
      MessageDlg('Record Found!',mtInformation,[mbOk],0);
end;

{ Filter records }
procedure TfrmMain.btnFilterClick(Sender: TObject);
var
  Value: string;
begin
  Value := qrMain.Filter;
  if InputQuery('Filtering Query', 'Filter:', Value) then
  begin
    qrMain.Filter := Value;
    qrMain.Filtered := true;
  end else
    qrMain.Filtered := false;
end;

end.
