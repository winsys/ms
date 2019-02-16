unit QuotesFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ChildWin, Main;

type
  TQuotesForm = class(TMDIChild)
    procedure FormShow(Sender: TObject);
    procedure tb_AddParagraphNotesClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormHide(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Changed: boolean;

    constructor Create(AOwner: TComponent; AFileName: string); reintroduce;
    destructor  Destroy; override;

    procedure AddChapter(AChapter: TDATARec);
    procedure NewFile;
  end;

var QuotesForm: TQuotesForm;

implementation

{$R *.DFM}

constructor TQuotesForm.Create(AOwner: TComponent; AFileName: string);
begin
  inherited Create(AOwner, 'QUOTES');
  FFileName:=AFileName;
  SetLength(FDATA, 0);
  SetLength(ParamArray, 0);

  Width:=288;
  Left:=MainForm.ClientRect.Right-MainForm.Panel1.Width-Width-9;
  Top:=MainForm.ClientRect.Top+2;
  Height:=MainForm.ClientHeight-MainForm.Toolbar2.Height-MainForm.StatusBar1.Height-8;
  Caption := msMsg19+ExtractFileName(FFileName);
  ToolButton4.Visible:=false;
  TXT.Color:=$00D8D8D8;//clBtnFace;

  tb_AddParagraphNotes.OnClick:=tb_AddParagraphNotesClick;
  DelChapter.Enabled:=true;
  DelChapter.Visible:=true;

  Changed:=false;
end;


destructor TQuotesForm.Destroy;
begin
  SetLength(FDATA, 0);
  SetLength(ParamArray, 0);
  QuotesForm:=nil;
  FFileName:='';
  inherited;
end;

procedure TQuotesForm.FormShow(Sender: TObject);
begin
  inherited;
  MoveToQuotes.Enabled:=false;
  Panel9.Visible:=false;
//  Label1.Visible:=false;
//  Label2.Visible:=False;
//  ToolButton10.Visible:=false;
//  ToolButton11.Visible:=false;
//  ToolButton12.Visible:=false;

  ToolButton9.Enabled:=true;
  ToolButton4.Enabled:=false;
//  ToolButton8.Visible:=false;
  sb_ShowSelectedText.Visible:=false;
//  tb_NextRecs.Visible:=False;
//  tb_PreviousRecs.Visible:=False;
//  SearchCount.Visible:=False;
//  DeleteChapterBtn.Action:=DelChapter;
end;

procedure TQuotesForm.AddChapter(AChapter: TDATARec);
var i: integer;
begin
  for i:=low(FDATA) to high(FDATA) do
    if FDATA[i].ID=AChapter.ID
      then begin
        ShowMessage(msMsg20);
        exit;
      end;
  SetLength(FDATA, High(FDATA)+2);
  FDATA[High(FDATA)].ID:=AChapter.ID;
  FDATA[High(FDATA)].CNUM:=AChapter.CNUM;
  FDATA[High(FDATA)].TITLEID:=AChapter.TITLEID;
  FDATA[High(FDATA)].ETITLE:=AChapter.ETITLE;
  FDATA[High(FDATA)].ISBIBLE:=AChapter.ISBIBLE;
  FDATA[High(FDATA)].Abzac:=AChapter.Abzac;
  FDATA[High(FDATA)].Notes:=AChapter.Notes;
  FDATA[High(FDATA)].Title:=AChapter.Title;
  Changed:=true;
end;

procedure TQuotesForm.tb_AddParagraphNotesClick(Sender: TObject);
var ANote: string;
    tmpCurChapterInd: smallint;
begin
  tmpCurChapterInd:=CurrentChapterIndex;
  if tmpCurChapterInd>=0
    then begin
      ANote:=FDATA[tmpCurChapterInd].Notes;
      if InputQuery(msMsg15, msMsg16, ANote)
        then begin
          FDATA[tmpCurChapterInd].Notes:=ANote;
          Changed:=true;
          if sb_ShowParagraphNotes.Down
            then Redraw(DATAToStr);
        end;
    end;
end;

procedure TQuotesForm.NewFile;
begin
  SetLength(FDATA, 0);
  FFileName:='NONAME';
  Caption := msMsg19+ExtractFileName(FFileName);
  Redraw(DATAToStr);
end;

procedure TQuotesForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose:=true;
  if Changed
    then case MessageDlg(msMsg1, mtConfirmation,
                         [mbYes, mbNo, mbCancel], 0) of
           mrYes    : begin
                        SaveExecute(Self);
                        Changed:=false;
                      end;
           mrNo     : ;
           mrCancel : begin
                        CanClose:=false;
                        Exit;
                      end;  
         end;
  QuotesForm:=nil;
  SetLength(FDATA, 0);
end;


procedure TQuotesForm.FormHide(Sender: TObject);
begin
  inherited;
  CurMDIForm:=nil;
  MainForm.GoToQuotes.Visible:=true;
  MainForm.GoBackFromQuotes.Visible:=false;
end;

end.
