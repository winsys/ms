unit Main;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, StdActns,
  ActnList, ToolWin, ImgList, TreeNT, Registry, Compress;

{$I CONSTANTS.PAS}

type
  TTreeMode = (byYears, byTitles, byBibleChapters);

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileOpenItem: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    WindowCascadeItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    Edit1: TMenuItem;
    CopyItem: TMenuItem;
    StatusBar1: TStatusBar;
    ActionList1: TActionList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    FileNew1: TAction;
    FileExit1: TAction;
    FileOpen1: TAction;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowArrangeAll1: TWindowArrange;
    WindowMinimizeAll1: TWindowMinimizeAll;
    HelpAbout1: TAction;
    WindowTileVertical1: TWindowTileVertical;
    ToolBar2: TToolBar;
    ToolButton7: TToolButton;
    ImageList1: TImageList;
    SearchText: TEdit;
    ToolButton14: TToolButton;
    Panel1: TPanel;
    Tree: TTreeNT;
    Splitter1: TSplitter;
    MSearch: TSpeedButton;
    TSearch: TSpeedButton;
    BSearch: TSpeedButton;
    DoSearch: TSpeedButton;
    IsTree: TSpeedButton;
    ShowTree: TAction;
    Addmessages1: TMenuItem;
    N3: TMenuItem;
    CloseAllWindows: TAction;
    ImportFile: TAction;
    ImportDialog: TOpenDialog;
    ReceiveFile: TAction;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    PopupMenu1: TPopupMenu;
    SelectAll: TMenuItem;
    UnselectAll: TMenuItem;
    Search: TAction;
    sb_CaseSensitive: TSpeedButton;
    Compress: TCompress;
    N7: TMenuItem;
    ToolButton1: TToolButton;
    GoToQuotes: TToolButton;
    ToolButton3: TToolButton;
    Splitter2: TSplitter;
    sbWhole: TSpeedButton;
    GoBackFromQuotes: TToolButton;
    kjhgfkj1: TMenuItem;
    ImageList2: TImageList;
    N2: TMenuItem;
    procedure FileOpen1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure ShowTreeExecute(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeDblClick(Sender: TObject);
    procedure CloseAllWindowsExecute(Sender: TObject);
    procedure SearchTextKeyPress(Sender: TObject; var Key: Char);
    procedure DoSearchClick(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNTNode);
    procedure ImportFileExecute(Sender: TObject);
    procedure IsTreeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TSearchClick(Sender: TObject);
    procedure BSearchClick(Sender: TObject);
    procedure MSearchClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure UnselectAllClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure N5Click(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure FormResize(Sender: TObject);
//    procedure N11Click(Sender: TObject);
    procedure ReceiveFileExecute(Sender: TObject);
    procedure SelectFormClick(Sender: TObject);
    procedure RefreshChildList;
    procedure Window1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GoToQuotesClick(Sender: TObject);
    procedure File1MeasureItem(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);
    procedure DoDrawText(AMenuItem: TMenuItem; ACanvas: TCanvas; const ACaption: string;
      var Rect: TRect; Selected: Boolean; Flags: Longint);
    procedure File1AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; State: TOwnerDrawState);
    procedure GoBackFromQuotesClick(Sender: TObject);
    procedure N2Click(Sender: TObject);
  private
    procedure CreateMDIChild(const ACaption: string);
  public
    TreeMode: TTreeMode;
    procedure ReloadTitles(AMode: TTreeMode);
    function  StrQuery(AQuery:string): string;
  end;

var
  MainForm: TMainForm;
  CurMDIForm: TForm;

const
  ReplaceAll: boolean = false;


implementation

{$R *.DFM}

uses ChildWin, About, DataUnit, QuotesFormUnit, ReplaceConfirmUnit,
  UpdateProgressUnit, HelpUnit;



function StyleString(AStyles: TFontStyles): string;
begin
  if fsBold in AStyles then Result:='1' else Result:='0';
  if fsItalic in AStyles then Result:=Result+'1' else Result:=Result+'0';
  if fsUnderline in AStyles then Result:=Result+'1' else Result:=Result+'0';
  if fsStrikeOut in AStyles then Result:=Result+'1' else Result:=Result+'0';
end;

function StyleArray(AStyleStr: string): TFontStyles;
begin
  Result:=[];
  if AStyleStr[1]='1' then Result:=Result+[fsBold];
  if AStyleStr[2]='1' then Result:=Result+[fsItalic];
  if AStyleStr[3]='1' then Result:=Result+[fsUnderline];
  if AStyleStr[4]='1' then Result:=Result+[fsStrikeOut];
end;

function  TMainForm.StrQuery(AQuery:string): string;
begin
  DATA.Query1.SQL.Text:=AQuery;
  try
    DATA.Query1.Open;
  except
    Result:='';
    exit;
  end;
  if not (DATA.Query1.EOF and DATA.Query1.BOF)
    then Result:=DATA.Query1.Fields[0].AsString
    else Result:='';
  if DATA.Query1.Active then DATA.Query1.Close;
end;

procedure TMainForm.CreateMDIChild(const ACaption: string);
var Child: TMDIChild;
begin
  if ChildOperation='DblClick'
    then SearchResult.OpenContext(Tree.Selected.iCatID);
  if ChildOperation='DblClick'
    then Child:=TMDIChild.Create(Application, 'TEXT')
    else
  if ChildOperation='Find'
    then Child:=TMDIChild.Create(Application, 'SEARCH')
    else
  if (ChildOperation='Open') or (ChildOperation='Citation')
    then Child:=TMDIChild.Create(Application, 'QUOTES');
  Child.Caption:=ACaption;
  if (ChildOperation='Find') and (LastFoundChaptersCount=0)
    then begin
      ShowMessage(msMsg32);
      Child.Free;
    end;
end;

procedure TMainForm.FileOpen1Execute(Sender: TObject);
begin
  with TRegistry.Create do
  begin
    OpenKey(SystemKey, false);
    CreateDir('C:\MS\'+msMsg33);
    if ValueExists('Open dialog')
      then OpenDialog.InitialDir:=ReadString('Open dialog')
      else OpenDialog.InitialDir:='C:\MS\'+msMsg33;
    if (OpenDialog.Execute) and (OpenDialog.FileName<>'')
      then begin
        WriteString('Open dialog', ExtractFilePath(OpenDialog.FileName));
        ChildOperation:='Citation';
        //TempFileName:=OpenDialog.FileName;
        if assigned(QuotesForm)
          then begin
            if QuotesForm.Changed
              then case MessageDlg(msMsg1, mtConfirmation,
                                   [mbYes, mbNo, mbCancel], 0) of
                     mrYes    : begin
                                  QuotesForm.SaveExecute(QuotesForm);
                                  QuotesForm.Changed:=false;
                                end;
                     mrNo     : ;
                     mrCancel : begin
                                  CloseKey;
                                  Free;
                                  exit;
                                end;
                   end;
            QuotesForm.OpenText(OpenDialog.FileName);
            QuotesForm.Redraw(QuotesForm.DATAToStr);
          end
          else QuotesForm:=TQuotesForm.Create(Application, OpenDialog.FileName);
        QuotesForm.Changed:=false;
      end;
    CloseKey;
    Free;
  end;
end;

procedure TMainForm.HelpAbout1Execute(Sender: TObject);
begin
  AboutBox:=TAboutBox.Create(Self);
  AboutBox.ShowModal;
  AboutBox.Free;
end;

procedure TMainForm.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ShowTreeExecute(Sender: TObject);
begin
  with TRegistry.Create do
  begin
    OpenKey('SOFTWARE\BIGIT Software\MessageSearch', true);
    if IsTree.Down
      then WriteInteger('Tree visible', 1)
      else WriteInteger('Tree visible', 0);
    if IsTree.Down
      then if ValueExists('Tree width') and (ReadInteger('Tree width')>10)
             then Panel1.Width:=ReadInteger('Tree width')
             else Panel1.Width:=150
      else begin
        WriteInteger('Tree width', Panel1.Width);
        Panel1.Width:=0;
      end;
    CloseKey;
    Free;
  end;
end;

procedure TMainForm.Panel1Resize(Sender: TObject);
begin
  IsTree.Down:=Panel1.Width>10;
end;

procedure TMainForm.ReloadTitles(AMode: TTreeMode);
var Node: TTreeNTNode;
    TtlYear: integer;
begin
  TreeMode:=AMode;
  Tree.Items.Clear;
  case TreeMode of
    byYears : begin
//                Tree.SortType:=TreeNT.stNone;
                DATA.QTemp.SQL.Text:='select distinct PYEAR from TITLES where ISBIBLE=0 order by PYEAR desc';
                DATA.QTemp.Open;
                while not DATA.QTemp.EOF do
                begin
                  if DATA.QTemp.FieldByName('PYEAR').AsInteger>5000
                    then Node:=Tree.Items.Add(nil, StrQuery('select TITLE from AddTitles where PYEAR='+DATA.QTemp.FieldByName('PYEAR').AsString))
                    else Node:=Tree.Items.Add(nil, DATA.QTemp.FieldByName('PYEAR').AsString);
                  Node.CheckType:=ctCheckBoxGrayed;
                  Node.Font.Style:=[fsBold];
                  Node.sDATA:='00';
                  if DATA.QTemp.FieldByName('PYEAR').AsInteger>5000
                    then DATA.TITLES.SQL.Text:='select ID, PYEAR, PMONTH, PDAY, PTIME, PLENGTH, ISSPECIAL, TITLE, ETITLE, HASBOOKMARK from TITLES '+
                            'where PYEAR='+DATA.QTemp.FieldByName('PYEAR').AsString+' and ISSPECIAL = 1 '+
                            'order by PMONTH desc, PDAY desc, PTIME, TITLE'
                    else DATA.TITLES.SQL.Text:='select ID, PYEAR, PMONTH, PDAY, PTIME, PLENGTH, ISSPECIAL+ISBIBLE as ADDKEY, TITLE, ETITLE, HASBOOKMARK from TITLES '+
                            'where PYEAR='+DATA.Qtemp.FieldByName('PYEAR').AsString+' and (ISSPECIAL+ISBIBLE) = 0 '+
                            'order by PMONTH desc, PDAY desc, PTIME, ADDKEY, TITLE';
                  DATA.TITLES.Open;
                  while not DATA.TITLES.EOF do
                  begin
                    with DATA, Tree.Items.AddChild(Node, Format('%s - %s%s.%s',
                          [TITLES.FieldByName('TITLE').AsString,
                           Times[TITLES.FieldByName('PTIME').AsInteger],
                           LZero(TITLES.FieldByName('PDAY').AsInteger),
                           LZero(TITLES.FieldByName('PMONTH').AsInteger)])) do
                      begin
                        CheckType:=ctCheckBox;
                        iCatID:=TITLES.FieldByName('ID').AsInteger;
                        iDATA:=TITLES.FieldByName('PYEAR').AsInteger;
                        bCatID:=TITLES.FieldByName('PMONTH').AsInteger;
                        bDATA:=TITLES.FieldByName('PDAY').AsInteger;
                        sDATA:=Times[TITLES.FieldByName('PTIME').AsInteger]+'1'+
                                  TITLES.FieldByName('ETITLE').AsString+' ('+TITLES.FieldByName('PLENGTH').AsString+' min.)';
                        fDATA:=TITLES.FieldByName('HASBOOKMARK').AsInteger;
                      end;
                    DATA.TITLES.Next;
                  end;
                  DATA.TITLES.Close;
                  DATA.QTemp.Next; // BM
                end;
                DATA.QTemp.Close;
              end;
    byTitles: begin
         // Проповеди
                DATA.TITLES.SQL.Text:='select ID, PYEAR, PMONTH, PDAY, PTIME, PLENGTH, ISSPECIAL, ISBIBLE, TITLE, ETITLE, HASBOOKMARK from TITLES '+
                          'where ISSPECIAL+ISBIBLE = 0 '+
                          'order by TITLE';
                DATA.TITLES.Open;
                while not DATA.TITLES.EOF do
                begin
                  if DATA.TITLES.FieldByName('PYEAR').AsInteger>1000
                    then TtlYear:=DATA.TITLES.FieldByName('PYEAR').AsInteger-1900
                    else TtlYear:=DATA.TITLES.FieldByName('PYEAR').AsInteger;
                  with DATA, Tree.Items.Add(nil, Format('%s - %s%s.%s.%s',
                        [TITLES.FieldByName('TITLE').AsString,
                         Times[TITLES.FieldByName('PTIME').AsInteger],
                         LZero(TITLES.FieldByName('PDAY').AsInteger),
                         LZero(TITLES.FieldByName('PMONTH').AsInteger),
                         LZero(TtlYear)
                          ])) do
                    begin
                      CheckType:=ctCheckBox;
                      iCatID:=TITLES.FieldByName('ID').AsInteger;
                      iDATA:=TITLES.FieldByName('PYEAR').AsInteger;
                      bCatID:=TITLES.FieldByName('PMONTH').AsInteger;
                      bDATA:=TITLES.FieldByName('PDAY').AsInteger;
                      sDATA:=Times[TITLES.FieldByName('PTIME').AsInteger]+'1'+
                                TITLES.FieldByName('ETITLE').AsString+' ('+TITLES.FieldByName('PLENGTH').AsString+' min.)';
                      fDATA:=TITLES.FieldByName('HASBOOKMARK').AsInteger;
                    end;
                  DATA.TITLES.Next;
                end;
                DATA.TITLES.Close;
//                Tree.SortType:=TreeNT.stText;
              end;
    byBibleChapters:
              begin
                //                                    Назв.книги                                 Номер книги
                DATA.QTemp.SQL.Text:='select distinct PLENGTH, PLACE from TITLES where ISBIBLE=1 order by PLENGTH';
                DATA.QTemp.Open;
                while not DATA.QTemp.EOF do
                begin
                  Node:=Tree.Items.Add(nil, DATA.QTemp.FieldByName('PLACE').AsString);
                  Node.CheckType:=ctCheckBoxGrayed;
                  Node.Font.Style:=[fsBold];
                  Node.sDATA:='00';
                  DATA.TITLES.SQL.Text:='select ID, TITLE, ETITLE, PLACE, PLENGTH, (substring(right(TITLE,3), locate(" ",right(TITLE,3))+1, 3)+0) as AAA from TITLES '+
                            'where PLENGTH='+DATA.Qtemp.FieldByName('PLENGTH').AsString+' and ISBIBLE = 1 '+
                            'order by AAA';
                  DATA.TITLES.Open;
                  while not DATA.TITLES.EOF do
                  begin
                    with DATA, Tree.Items.AddChild(Node, Format('%s',
                          [TITLES.FieldByName('TITLE').AsString])) do
                      begin
                        CheckType:=ctCheckBox;
                        iCatID:=TITLES.FieldByName('ID').AsInteger;
                        sDATA:='01'+TITLES.FieldByName('ETITLE').AsString;
                      end;
                    DATA.TITLES.Next;
                  end;
                  DATA.TITLES.Close;
                  DATA.QTemp.Next; // BM
                end;
                DATA.QTemp.Close;
              end;
  end;
end;

//------------------------------------------------------------------------------
//
procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var Reg: TRegistry;
begin
{  if UserParams.Changed
    then UserParams.SaveToRegistry;
  UserParams.Free;}
  SaveWindowPosition(Self, true);
  Reg:=TRegistry.Create;
  Reg.OpenKey('SOFTWARE\BIGIT Software\MessageSearch', true);
  if SearchText.Width > MainForm.ClientWidth div 2
    then SearchText.Width:=315;
  Reg.WriteInteger('Search window width', SearchText.Width);
  if Panel1.Width>10
    then Reg.WriteInteger('Tree width', Panel1.Width);
  if IsTree.Down
    then Reg.WriteInteger('Tree visible', 1)
    else Reg.WriteInteger('Tree visible', 0);
  Reg.CloseKey;
  Reg.Free;
//  DB.Disconnect;
end;

procedure TMainForm.TreeDblClick(Sender: TObject);
var i: integer;
begin
  if (Tree.Selected<>nil) and ((Tree.Selected.Parent<>nil) or (TSearch.Down))
    then begin
      for i := Tree.Items.Count-1 downto 0 do
        Tree.Items[i].CheckState:=TreeNT.csUnChecked;
      Tree.Selected.CheckState:=TreeNT.csChecked;
      ChildOperation:='DblClick';
      if BSearch.Down
        then CreateMDIChild(msMsg2+msMsg25+': '+Tree.Selected.Text+']')
        else CreateMDIChild(msMsg2+copy(Tree.Selected.Text,1,pos('-',Tree.Selected.Text)-2)+']');
    end;
end;

procedure TMainForm.CloseAllWindowsExecute(Sender: TObject);
var I: Integer;
begin
  for I := MDIChildCount-1 downto 0 do
    MDIChildren[I].Close;
end;

procedure TMainForm.SearchTextKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then DoSearchClick(DoSearch);
end;

procedure TMainForm.TreeChange(Sender: TObject; Node: TTreeNTNode);
begin
  if Node=nil then exit;
  if (length(Node.sDATA)>2) and (Node.sDATA[2]<>'0')
     then Node.TreeView.Hint:=copy(Node.sDATA, 3, 100)
     else Node.TreeView.Hint:='';
end;

procedure TMainForm.ImportFileExecute(Sender: TObject);

label BibleCycle;

var T: TMemoryStream;
    AList: TStringList;
    i, j, k, CurTtl: integer;
    c: char;
    LoadingBible: boolean;
    NewTS, Ts, CNUM_str: string;

    SR: record
         ID: integer;
         PYEAR: smallint;
         PMONTH: byte;
         PDAY: byte;
         PTIME: byte;
         PLENGTH: smallint;
         ISBIBLE: byte;
         ISSPECIAL: byte;
         HASBOOKMARK: byte;
         TITLE: string;
         ETITLE:string;
         PLACE:string;
    end;
    Reg: TRegistry;

    procedure ReadStreamLine(var S: string);
    begin
      S:=''; c:=#0;
      while (c<>#13) and (T.Position < T.Size) do
      begin
        T.Read(c, 1);
        if c<>#13
          then S:=S+c
          else if (length(S)>0) and (S[length(S)]=#10) and (T.Position < T.Size)
                 then begin
                   S:=S+c;
                 end;
      end;
      if (T.Position < T.Size)
        then T.Read(c, 1);
    end;

    function varyof(AYear: integer): integer;
    begin
      if AYear>1000
        then Result:=AYear-1900
        else Result:=AYear+1900;
    end;

begin
  Reg:=TRegistry.Create;
  Reg.OpenKey(SystemKey, false);
  if Reg.ValueExists('Import dialog')
    then ImportDialog.InitialDir:=Reg.ReadString('Import dialog')
    else ImportDialog.InitialDir:='C:\MS\Updates';
  if ImportDialog.Execute
    then if ImportDialog.Files.Count>0
    then begin
      LoadingBible:=false;
      Reg.WriteString('Import dialog', ExtractFilePath(ImportDialog.Files[0]));
      Reg.CloseKey;
      Reg.Free;
      UpdateProgress:=TUpdateProgress.Create(Self);
      UpdateProgress.Show;
      for j:=0 to ImportDialog.Files.Count-1 do
        if AnsiUpperCase(ExtractFileExt(ImportDialog.Files[j]))='.MSA'
        then begin
          AList:=TStringList.Create;
          Compress.ScanCompressedFile(ImportDialog.Files[j], AList);
          Application.ProcessMessages;
          if AList.Count>0 then
            for k:=0 to AList.Count-1 do
            begin
              UpdateProgress.P.Max:=AList.Count;
              T:=TMemoryStream.Create;
              try
                Compress.ExpandStreamFromArchive(ImportDialog.Files[j], T, AList[k]);
              except
                raise Exception.Create(msMsg4+ImportDialog.Files[j]);
                Compress.FreeFileList(AList);
                AList.Free;
                T.Free;
                exit;
              end;
              T.Position:=0;

              ReadStreamLine(Ts);
BibleCycle:
              if Ts='@'
                then begin
                  LoadingBible:=true;
                  ReadStreamLine(SR.TITLE);
                end
                else SR.TITLE:=Ts;
              ReadStreamLine(SR.ETITLE);
              ReadStreamLine(Ts);  // date
              if TS[1] in ['0'..'9']
                then i:=0
                else for i:=0 to 5 do
                       if Times[i]=Ts[1] then break;
              SR.PTIME:=i;
              SR.PDAY:=StrToInt(copy(Ts, 2, 2));
              SR.PMONTH:=StrToInt(copy(Ts, 5, 2));
              SR.PYEAR:=StrToInt(copy(Ts, 8, 4));
              SR.ISBIBLE:=byte(LoadingBible);
              SR.ISSPECIAL:=0;
              SR.HASBOOKMARK:=0;
              ReadStreamLine(SR.PLACE);
              // Sermon length / bible chapter order
              ReadStreamLine(Ts);
              SR.PLENGTH:=StrToInt(Trim(Ts));

              UpdateProgress.Label2.Caption:=SR.TITLE;
              UpdateProgress.Label2.Update;
              Screen.Cursor:=crSQLWait;
              if DATA.qTEMP.Active then DATA.qTEMP.Close;

              if not LoadingBible
                then begin
                  with SR do
                    DATA.qTEMP.SQL.Text:=Format('select * from TITLES where (PYEAR in (%d,%d)) and PMONTH=%d and PDAY=%d and PTIME=%d',
                                              [PYEAR,varyof(PYEAR),PMONTH,PDAY,PTIME]);
                  DATA.qTEMP.Open;
                  if not (DATA.qTEMP.EOF and DATA.qTEMP.BOF)
                    then begin
                      ReplaceConfirmForm:=TReplaceConfirmForm.Create(Self);
                      ReplaceConfirmForm.Top:=UpdateProgress.Top-ReplaceConfirmForm.Height-10;
                      ReplaceConfirmForm.Label3.Caption:=DATA.qTEMP.FieldByName('TITLE').AsString;
                      Application.ProcessMessages;
                      UpdateProgress.Label2.Caption:=DATA.qTEMP.FieldByName('TITLE').AsString;
                      UpdateProgress.Label2.Update;
                      UpdateProgress.Update;
                      Screen.Cursor:=crArrow;
                      Application.ProcessMessages;
                      if ReplaceALL or (ReplaceConfirmForm.ShowModal=mrYes)
                        then begin
                          Screen.Cursor:=crSQLWait;
                          DATA.qTEMP.Close;
                          with SR do
                            DATA.ExecSQL(Format('delete from TITLES where (PYEAR in (%d,%d)) and PMONTH=%d and PDAY=%d and PTIME=%d',
                                                      [PYEAR,varyof(PYEAR),PMONTH,PDAY,PTIME]));
                          Application.ProcessMessages;
                          with SR do
                            DATA.ExecSQL(Format('delete from DATA where (PYEAR in (%d,%d)) and PMONTH=%d and PDAY=%d and PTIME=%d',
                                                      [PYEAR,varyof(PYEAR),PMONTH,PDAY,PTIME]));
                          Application.ProcessMessages;
                        end
                        else begin
                          UpdateProgress.Free;
                          ReplaceConfirmForm.Free;
                          DATA.qTEMP.Close;
                          T.Free;
                          exit;
                        end;
                      ReplaceConfirmForm.Free;
                      UpdateProgress.Update;
                      Application.ProcessMessages;
                    end;
                  if DATA.qTEMP.Active then DATA.qTEMP.Close;
                end
                else begin
                  CurTtl:=DATA.IntQuery('select ID from TITLES where ISBIBLE=1 and TITLE="'+SqlQuotes(SR.TITLE)+'"');
                  Application.ProcessMessages;
                  DATA.ExecSQL('delete from TITLES where ID='+IntToStr(CurTtl));
                  Application.ProcessMessages;
                  DATA.ExecSQL('delete from DATA where TITLEID='+IntToStr(CurTtl));
                  Application.ProcessMessages;
                end;

              Screen.Cursor:=crSQLWait;
              with SR do
                DATA.ExecSQL(Format('insert into TITLES (PYEAR,PMONTH,PDAY,PTIME,PLENGTH,ISBIBLE,ISSPECIAL,HASBOOKMARK,TITLE,PLACE,ETITLE)'#13#10+
                          'values (%d, %d, %d, %d, %d, %d, %d, %d, "%s", "%s", "%s")',
                        [PYEAR,PMONTH,PDAY,PTIME,PLENGTH,ISBIBLE,ISSPECIAL,HASBOOKMARK,
                         SQLQuotes(TITLE),SQLQuotes(PLACE),SQLQuotes(ETITLE)]));
              Application.ProcessMessages;
              DATA.QTemp.SQL.Text:='select max(ID) from TITLES';
              DATA.QTemp.Open;
              Application.ProcessMessages;
              SR.ID:=DATA.QTemp.Fields[0].AsInteger;
              DATA.QTemp.Close;

              ReadStreamLine(Ts); // First empty line

              // Chapters
              UpdateProgress.P2.Max:=T.Size;
              repeat
                ReadStreamLine(NewTs);        // Chapter first paragraph
                Ts:=NewTs;
                Application.ProcessMessages;
                while (trim(NewTs)<>'') and (trim(NewTs)<>'@') do
                begin
                  ReadStreamLine(NewTs);
                  if trim(NewTs)<>''
                    then Ts:=Ts+#13#10+NewTs; // Chapter next paragraph
                end;
                if (TS<>'') and (trim(Ts)<>'@')
                then begin
                  i:=1;
                  CNUM_str:='';
                  while TS[i] in ['0'..'9'] do
                    begin
                      CNUM_str:=CNUM_str+TS[i];
                      inc(i);
                    end;
                  if CNUM_str=''
                    then i:=0
                    else i:=StrToInt(Trim(CNUM_str));
                  with SR do
                    DATA.ExecSQL(Format('insert into DATA (PYEAR,PMONTH,PDAY,PTIME,ISBIBLE,ISSPECIAL,HASBOOKMARK,TITLEID,CNUM,CHAPTER)'#13#10+
                              'values (%d, %d, %d, %d, %d, %d, %d, %d, %d, "%s")',
                            [PYEAR,PMONTH,PDAY,PTIME,ISBIBLE,ISSPECIAL,HASBOOKMARK,
                             ID, I, SQLQuotes(Ts)]));
                  Application.ProcessMessages;
                  UpdateProgress.P2.Position:=T.Position;
                  UpdateProgress.P2.Update;
                  Application.ProcessMessages;
                end;
              until (trim(Ts)='') or (trim(Ts)='@');

              if trim(Ts)='@'
                then goto BibleCycle;

              T.Free;
              UpdateProgress.P.Position:=k+1;
              UpdateProgress.P.Update;
              Application.ProcessMessages;

              DATA.ExecSQL('flush tables');
              Application.ProcessMessages;
            end;
          Compress.FreeFileList(AList);
          AList.Free;
          Screen.Cursor:=crArrow;

          if TSearch.Down
            then ReloadTitles(byTitles);
          if BSearch.Down
            then ReloadTitles(byBibleChapters);
          if MSearch.Down
            then ReloadTitles(byYears);

        end  // File name OK
        else ShowMessage(msMsg3); // Incorrect Source file
      UpdateProgress.Free;
    end
    else begin
      Reg.CloseKey;
      Reg.Free;
    end
  else begin
    Reg.CloseKey;
    Reg.Free;
  end;
end;

procedure TMainForm.IsTreeMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if IsTree.Down
    then ShowTree.Hint:=msMsg5
    else ShowTree.Hint:=msMsg6;
end;

procedure TMainForm.TSearchClick(Sender: TObject);
begin
  ReloadTitles(byTitles);
end;

procedure TMainForm.BSearchClick(Sender: TObject);
begin
  ReloadTitles(byBibleChapters);
end;

procedure TMainForm.MSearchClick(Sender: TObject);
begin
  ReloadTitles(byYears);
end;

procedure TMainForm.SelectFormClick(Sender: TObject);
var i: smallint;
begin
  i:=(Sender as TMenuItem).Tag;
  MDIChildren[i].Show;
end;


procedure TMainForm.RefreshChildList;
var M: TMenuItem;
    i: smallint;
begin
  while Window1.Count>3 do
    Window1.Delete(3);
  for i:=MDIChildCount-1 downto 0 do
  begin
    M:=TMenuItem.Create(Self);
//    M.Name:='Child_'+IntToStr(i+1);
    M.Caption:=MDIChildren[i].Caption;
    M.Tag:=i;
    M.OnClick:=SelectFormClick;
    M.OnAdvancedDrawItem:=File1AdvancedDrawItem;
    M.OnMeasureItem:=File1MeasureItem;
    if copy(M.Caption, 1, length(msMsg2))=msMsg2  // text
      then M.ImageIndex:=32
      else
    if copy(M.Caption, 1, length(msMsg7))=msMsg7  // search
      then M.ImageIndex:=33
      else
    if copy(M.Caption, 1, length(msMsg14))=msMsg14  // context
      then M.ImageIndex:=35
      else
    if copy(M.Caption, 1, length(msMsg19))=msMsg19  // quotes
      then M.ImageIndex:=34;
    Window1.Add(m);
  end;
end;


procedure TMainForm.DoSearchClick(Sender: TObject);
begin
  if SearchText.Text<>''
    then begin
      ChildOperation:='Find';
      isWholeChecked:=not sbWhole.Down;
      if BSearch.Down
        then CreateMDIChild(msMsg7+msMsg25+': '+SearchText.Text+']')
        else CreateMDIChild(msMsg7+SearchText.Text+']');
    end;
end;

procedure TMainForm.SelectAllClick(Sender: TObject);
var i: Integer;
begin
  for i := Tree.Items.Count-1 downto 0 do
    Tree.Items[i].CheckState:=TreeNT.csChecked;
end;

procedure TMainForm.UnselectAllClick(Sender: TObject);
var i: Integer;
begin
  for i := Tree.Items.Count-1 downto 0 do
    Tree.Items[i].CheckState:=TreeNT.csUnChecked;
end;

procedure TMainForm.FormShow(Sender: TObject);
var i,j,n: byte;
begin
  // Set Additional Hints
  for i:=0 to 16 do
    (ActionList1.Actions[i] as TAction).Hint:=msAct0[i];
  // Set Menu captions
  n:=0;
  for i:=0 to 3 do
    begin
      MainMenu1.Items[i].Caption:=msMenu[n];
      inc(n);
      for j:=0 to MainMenu1.Items[i].Count-1 do
      if not MainMenu1.Items[i].Items[j].IsLine
        then begin
          MainMenu1.Items[i].Items[j].Caption:=msMenu[n];
          inc(n);
        end;
    end;
  SelectAll.Caption:=msMsg21;
  UnselectAll.Caption:=msMsg22;
  SelectAll.Hint:=msMsg21;
  UnselectAll.Hint:=msMsg22;
  MSearch.Hint:=msMsg23;
  TSearch.Hint:=msMsg24;
  BSearch.Hint:=msMsg25;

  StatusBar1.Font.Charset:=msFontCharset;
  StatusBar1.Font.Name:=msFontName;

  SearchText.Hint:=msMsg26;
  DoSearch.Hint:=msMsg27;
  GoToQuotes.Hint:=msMsg28;
  sb_CaseSensitive.Hint:=msMsg30;
  sbWhole.Hint:=msMsg31;

  SetLanguage(SearchText);

  SearchResult:=TSearchResult.Create;
  with TRegistry.Create do
  begin
    OpenKey('SOFTWARE\BIGIT Software\MessageSearch', false);
    if ValueExists('Search window width')
      then SearchText.Width:=ReadInteger('Search window width')
      else SearchText.Width:=Width-292;
    if ValueExists('Tree visible')
      then if ReadInteger('Tree visible')=1
        then if ValueExists('Tree width')
               then Panel1.Width:=ReadInteger('Tree width')
               else Panel1.Width:=150
        else Panel1.Width:=0
      else begin
        WriteInteger('Tree visible', 0);
        Panel1.Width:=0;
      end;

    try
      if StrToDate(ReadString('Last reminded')) < Date-30
        then begin
          ShowMessage(msMsg8);
          WriteString('Last reminded', DateToStr(Date));
        end;
    except
      WriteString('Last reminded', DateToStr(Date));
    end;

    CloseKey;
    Free;
  end;
  // moved from DATAUnit
  rtfHeader:= '{\rtf1\ansicpg'+msANSICPG+'{\fonttbl{\f0\fnil\fcharset'+msCharSet+' '+msFontName+';\f1\fcharset'+msCharSet+' Times New Roman;}}'+
                      '{\colortbl ;\red0\green0\blue0;'+selTextColor+selTextBack+'\red255\green248\blue157;\red0\green128\blue0;\red128\green0\blue0;'+
                        RGBToString(clBtnFace)+';'+RGBToString(clWindow)+';\red51\green102\blue255;}';

  ReloadTitles(byYears);
end;

procedure TMainForm.N5Click(Sender: TObject);
begin
//  Application.HelpCommand(HELP_CONTENTS, 0);
  frmHelp:=TfrmHelp.Create(Self, 'N');
  frmHelp.ShowModal;
  frmHelp.Free;
end;

procedure TMainForm.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if assigned(QuotesForm)
  then begin
    QuotesForm.Left:=ClientRect.Right-Panel1.Width-QuotesForm.Width-9;
    QuotesForm.Top:=ClientRect.Top+2;
    QuotesForm.Height:=ClientHeight-Toolbar2.Height-StatusBar1.Height-8;
  end;
end;

//------------------------------------------------------------------------------
// Создание нового файла для цитат.
{procedure TMainForm.N11Click(Sender: TObject);
begin
  if assigned(QuotesForm)
    then QuotesForm.NewFile
    else begin
      ChildOperation:='Citation';
      QuotesForm:=TQuotesForm.Create(Application, 'NONAME');
    end;
end;
}
//==============================================================================
//==============================================================================

//------------------------------------------------------------------------------
// Проверка проповедей в интернете.
procedure TMainForm.ReceiveFileExecute(Sender: TObject);
//var ReceivedList: TStringList;
begin
  ShowMessage(msMsg9);
{  ReceivedList:=TStringList.Create;
  inf_InternetSearch:=Tinf_InternetSearch.Create(self);
  inf_InternetSearch.ShowModal;
  inf_InternetSearch.Free;
  ReceivedList.Free;      }
end;

procedure TMainForm.Window1Click(Sender: TObject);
begin
  RefreshChildList;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RestoreWindowPosition(Self, true);
  SetLanguage(Tree);
  SetLanguage(SearchText);
end;

procedure TMainForm.GoToQuotesClick(Sender: TObject);
var i: smallint;
begin
  if ActiveMDIChild=nil then exit;

  CurMDIForm:=ActiveMDIChild;

  for i:=MDIChildCount-1 downto 0 do
    if MDIChildren[i]=QuotesForm
      then MDIChildren[i].Show;

  GoToQuotes.Visible:=false;
  GoBackFromQuotes.Visible:=true;
end;

procedure TMainForm.DoDrawText(AMenuItem: TMenuItem; ACanvas: TCanvas; const ACaption: string;
  var Rect: TRect; Selected: Boolean; Flags: Longint);
var
  Text: string;
  R: TRect;
  ParentMenu: TMenu;
begin
  with AMenuItem do
  begin
    ParentMenu := GetParentMenu;
    if (ParentMenu <> nil) and (ParentMenu.IsRightToLeft) then
    begin
      if Flags and DT_LEFT = DT_LEFT then
        Flags := Flags and (not DT_LEFT) or DT_RIGHT
      else if Flags and DT_RIGHT = DT_RIGHT then
        Flags := Flags and (not DT_RIGHT) or DT_LEFT;
      Flags := Flags or DT_RTLREADING;
    end;
    Text := ACaption;
    if (Flags and DT_CALCRECT <> 0) and ((Text = '') or
      (Text[1] = cHotkeyPrefix) and (Text[2] = #0)) then Text := Text + ' ';

    if ParentMenu is TPopupMenu then
    begin
      Rect.Left:=Rect.Left+2;
      Rect.Right:=Rect.Right+2;
    end;

    with ACanvas do
    begin
      if Text = cLineCaption then
      begin
        if Flags and DT_CALCRECT = 0 then
        begin
          R := Rect;
          Inc(R.Top, 4);
          DrawEdge(Handle, R, EDGE_ETCHED, BF_TOP);
        end;
      end
      else
      begin
        Brush.Style := bsClear;
        if Default then
          Font.Style := Font.Style + [fsBold];
        if not Enabled then
        begin
          if not Selected then
          begin
            OffsetRect(Rect, 1, 1);
            Font.Color := clBtnHighlight;
            ACanvas.Font.Name:=msFontName;
            ACanvas.Font.CHARSET:=msFontCharset;
            DrawText(ACanvas.Handle, PChar(Text), Length(Text), Rect, Flags);
            OffsetRect(Rect, -1, -1);
          end;
          if Selected and (ColorToRGB(clHighlight) = ColorToRGB(clBtnShadow)) then
            Font.Color := clBtnHighlight else
            Font.Color := clBtnShadow;
        end;
        ACanvas.Font.Name:=msFontName;
        ACanvas.Font.CHARSET:=msFontCharset;
        DrawText(ACanvas.Handle, PChar(Text), Length(Text), Rect, Flags);
      end;
    end;
  end;
end;


procedure TMainForm.File1MeasureItem(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
const
  Alignments: array[TPopupAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Alignment: TPopupAlignment;
  ImageList: TCustomImageList;
  ParentMenu: TMenu;
  DrawGlyph: Boolean;
  TopLevel: Boolean;
  DrawStyle: Integer;
  Text: string;
  R: TRect;

  procedure GetMenuSize;
  var
    NonClientMetrics: TNonClientMetrics;
  begin
    NonClientMetrics.cbSize := sizeof(NonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    begin
      Width := NonClientMetrics.iMenuWidth;
      Height := NonClientMetrics.iMenuHeight;
    end;
  end;

begin
  with Sender as TMenuItem do
  begin
    if GetParentComponent is TMainMenu then
    begin
      TopLevel := True;
      GetMenuSize;
    end
    else TopLevel := False;
    ParentMenu := GetParentMenu;
    ImageList := GetImageList;

    if Caption = cLineCaption then
    begin
      Height := 5;
      Width := -2;
      DrawGlyph := False;
    end
    else if Assigned(ImageList) and ((ImageIndex > -1) or not TopLevel) then
    begin
      Width := ImageList.Width;
      if not TopLevel then
        Height := ImageList.Height;
      DrawGlyph := True;
    end
    else if Assigned(Bitmap) and not Bitmap.Empty then
    begin
      Width := 16;
      if not TopLevel then
        Height := 16;
      DrawGlyph := True;
    end
    else
    begin
      Width := -7;
      DrawGlyph := False;
    end;

    if ParentMenu is TPopupMenu then
    begin
      Inc(Width, 10);
    end;

    if DrawGlyph and not TopLevel then
      Inc(Width, 15);
    if not TopLevel then
      Inc(Height, 3);
    FillChar(R, SizeOf(R), 0);
    if ParentMenu is TPopupMenu then
    begin
      Alignment := TPopupMenu(ParentMenu).Alignment;
      // Inc(Width, 50);
    end
    else if ParentMenu is TMenu then
      Alignment := paLeft
    else
      Alignment := paLeft;
    if ShortCut <> 0 then
      Text := Concat(Caption, ShortCutToText(ShortCut)) else
      Text := Caption;
    DrawStyle := Alignments[Alignment] or DT_EXPANDTABS or DT_SINGLELINE or
      DT_NOCLIP or DT_CALCRECT;
    DoDrawText(Sender as TMenuItem, ACanvas, Text, R, False, DrawStyle);
    Inc(Width, R.Right - R.Left + 7);
    Inc(Height, 2);
  end;
end;

procedure TMainForm.File1AdvancedDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
const
  Alignments: array[TPopupAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  EdgeStyle: array[Boolean] of Longint = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
var
  ImageList: TCustomImageList;
  ParentMenu: TMenu;
  Alignment: TPopupAlignment;
  DrawImage, DrawGlyph: Boolean;
  GlyphRect, SaveRect: TRect;
  DrawStyle: Longint;
  Glyph: TBitmap;
  OldBrushColor: TColor;
  Selected: Boolean;
  Win98Plus: Boolean;
  Win2K: Boolean;

  function TopLevel: boolean;
  begin
    with Sender as TMenuItem do
      Result := GetParentComponent is TMainMenu;
  end;

  procedure NormalDraw;
  begin
    with ACanvas, Sender as TMenuItem do
    begin
      if not TopLevel
        then Brush.Color := clMenu
        else Brush.Color := clBtnFace;
      //ImageList := GetImageList;
      {if not Selected then }
      FillRect(ARect);
      if ParentMenu is TMenu then
        Alignment := paLeft
      else if ParentMenu is TPopupMenu then
        Alignment := TPopupMenu(ParentMenu).Alignment
      else
        Alignment := paLeft;
      GlyphRect.Left := ARect.Left + 1;
      GlyphRect.Top := ARect.Top + 1;
      if Caption = cLineCaption then
      begin
        FillRect(ARect);
        GlyphRect.Left := 0;
        GlyphRect.Right := -4;
        DrawGlyph := False;
      end
      else
      begin
        DrawImage := (ImageList <> nil) and ((ImageIndex > -1) and
          (ImageIndex < ImageList.Count) or Checked and ((Bitmap = nil) or
          Bitmap.Empty));
        if DrawImage or Assigned(Bitmap) and not Bitmap.Empty then
        begin
          DrawGlyph := True;

          if DrawImage then
          begin
            GlyphRect.Right := GlyphRect.Left + ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end
          else
          begin
            { Need to add BitmapWidth/Height properties for TMenuItem if we're to
              support them.  Right now let's hardcode them to 16x16. }
            GlyphRect.Right := GlyphRect.Left + 16;
            GlyphRect.Bottom := GlyphRect.Top + 16;
          end;

          { Draw background pattern brush if selected }
          if Checked then
          begin
            Inc(GlyphRect.Right);
            Inc(GlyphRect.Bottom);
            OldBrushColor := Brush.Color;
            if not (odSelected in State) then
            begin
              OldBrushColor := Brush.Color;
              Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
              FillRect(GlyphRect);
            end
            else
            begin
              Brush.Color := clBtnFace;
              FillRect(GlyphRect);
            end;
            Brush.Color := OldBrushColor;
            Inc(GlyphRect.Left);
            Inc(GlyphRect.Top);
          end;

          if DrawImage then
          begin
            if (ImageIndex > -1) and (ImageIndex < ImageList.Count) then
              ImageList.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top, ImageIndex,
                Enabled)
            else
            begin
              { Draw a menu check }
              Glyph := TBitmap.Create;
              try
                Glyph.Transparent := True;
                Glyph.Handle := LoadBitmap(0, PChar(OBM_CHECK));
                OldBrushColor := Font.Color;
                Font.Color := clBtnText;
                Draw(GlyphRect.Left + (GlyphRect.Right - GlyphRect.Left - Glyph.Width) div 2 + 1,
                  GlyphRect.Top + (GlyphRect.Bottom - GlyphRect.Top - Glyph.Height) div 2 + 1, Glyph);
                Font.Color := OldBrushColor;
              finally
                Glyph.Free;
              end;
            end;
          end
          else
          begin
            SaveRect := GlyphRect;
            { Make sure image is within glyph bounds }
            if Bitmap.Width < GlyphRect.Right - GlyphRect.Left then
              with GlyphRect do
              begin
                Left := Left + ((Right - Left) - Bitmap.Width) div 2 + 1;
                Right := Left + Bitmap.Width;
              end;
            if Bitmap.Height < GlyphRect.Bottom - GlyphRect.Top then
              with GlyphRect do
              begin
                Top := Top + ((Bottom - Top) - Bitmap.Height) div 2 + 1;
                Bottom := Top + Bitmap.Height;
              end;
            StretchDraw(GlyphRect, Bitmap);
            GlyphRect := SaveRect;
          end;

          if Checked then
          begin
            Dec(GlyphRect.Right);
            Dec(GlyphRect.Bottom);
          end;
        end
        else
        begin
          if (ImageList <> nil) and not TopLevel then
          begin
            GlyphRect.Right := GlyphRect.Left + ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end
          else
          begin
            GlyphRect.Right := GlyphRect.Left;
            GlyphRect.Bottom := GlyphRect.Top;
          end;
          DrawGlyph := False;
        end;
      end;
      with GlyphRect do
      begin
        Dec(Left);
        Dec(Top);
        Inc(Right, 2);
        Inc(Bottom, 2);
      end;

      if Checked or Selected and DrawGlyph then
        DrawEdge(Handle, GlyphRect, EdgeStyle[Checked], BF_RECT);

      if Selected then
      begin
        if DrawGlyph then ARect.Left := GlyphRect.Right + 1;
        if not (Win98Plus and TopLevel) then
          Brush.Color := clHighlight;
        FillRect(ARect);
      end;
      if TopLevel and Win98Plus then
      begin
        if Selected or (odHotLight in State)
          then begin
             Brush.Color := clHighlight;
             FillRect(ARect);
             Font.Color := clHighlightText;
          end;
        if not Selected then
          OffsetRect(ARect, 0, -1);
      end;

      if not (Selected and DrawGlyph) then
        ARect.Left := GlyphRect.Right + 1;
      Inc(ARect.Left, 2);
      Dec(ARect.Right, 1);

      DrawStyle := DT_EXPANDTABS or DT_SINGLELINE or Alignments[Alignment];
      if Win2K and (odNoAccel in State) then
        DrawStyle := DrawStyle or DT_HIDEPREFIX;
      { Calculate vertical layout }
      SaveRect := ARect;
      if odDefault in State then
        Font.Style := [fsBold];
      DoDrawText(Sender as TMenuItem, ACanvas, Caption, ARect, Selected, DrawStyle or DT_CALCRECT or DT_NOCLIP);
      OffsetRect(ARect, 0, ((SaveRect.Bottom - SaveRect.Top) - (ARect.Bottom - ARect.Top)) div 2);
      if TopLevel and Selected and Win98Plus then
        OffsetRect(ARect, 1, 0);
      DoDrawText(Sender as TMenuItem, ACanvas, Caption, ARect, Selected, DrawStyle);
      if (ShortCut <> 0) and not TopLevel then
      begin
        ARect.Left := ARect.Right;
        ARect.Right := SaveRect.Right - 10;
        DoDrawText(Sender as TMenuItem, ACanvas, ShortCutToText(ShortCut), ARect, Selected, DT_RIGHT);
      end;
    end;
  end;

  procedure BiDiDraw;
  var
    S: string;
  begin
    with ACanvas, Sender as TMenuItem do
    begin
      //ImageList := GetImageList;
      if not Selected then FillRect(ARect);
      if ParentMenu is TMenu then
        Alignment := paLeft
      else if ParentMenu is TPopupMenu then
        Alignment := TPopupMenu(ParentMenu).    Alignment
      else
        Alignment := paLeft;
      GlyphRect.Right := ARect.Right - 1;
      GlyphRect.Top := ARect.Top + 1;
      if Caption = cLineCaption then
      begin
        FillRect(ARect);
        GlyphRect.Left := GlyphRect.Right + 2;
        GlyphRect.Right := 0;
        DrawGlyph := False;
      end
      else
      begin
        DrawImage := (ImageList <> nil) and ((ImageIndex > -1) and
          (ImageIndex < ImageList.    Count) or Checked and ((Bitmap = nil) or
          Bitmap.Empty));
        if DrawImage or Assigned(Bitmap) and not Bitmap.Empty then
        begin
          DrawGlyph := True;    
    
          if DrawImage then
          begin
            GlyphRect.Left := GlyphRect.Right - ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end
          else
          begin
            { Need to add BitmapWidth/Height properties for TMenuItem if we're to
              support them.  Right now let's hardcode them to 16x16. }
            GlyphRect.Left := GlyphRect.Right - 16;
            GlyphRect.Bottom := GlyphRect.Top + 16;
          end;
    
          { Draw background pattern brush if selected }
          if Checked then
          begin
            Dec(GlyphRect.Left);
            Inc(GlyphRect.Bottom);
            OldBrushColor := Brush.Color;
            if not Selected then
            begin
              OldBrushColor := Brush.Color;
              Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
              FillRect(GlyphRect);
            end
            else
            begin
              Brush.Color := clBtnFace;
              FillRect(GlyphRect);
            end;
            Brush.Color := OldBrushColor;
            Dec(GlyphRect.Right);
            Inc(GlyphRect.Top);
          end;
    
          if DrawImage then
          begin
            if (ImageIndex > -1) and (ImageIndex < ImageList.Count) then
              ImageList.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top, ImageIndex,
                Enabled)
            else
            begin
              { Draw a menu check }
              Glyph := TBitmap.Create;
              try
                Glyph.Transparent := True;
                Glyph.Handle := LoadBitmap(0, PChar(OBM_CHECK));
                OldBrushColor := Font.Color;
                Font.Color := clBtnText;
                Draw(GlyphRect.Left + (GlyphRect.Right - GlyphRect.Left - Glyph.Width) div 2 + 1,
                  GlyphRect.Top + (GlyphRect.Bottom - GlyphRect.Top - Glyph.Height) div 2 + 1, Glyph);
                Font.Color := OldBrushColor;
              finally
                Glyph.Free;
              end;
            end;
          end
          else
          begin
            SaveRect := GlyphRect;
            { Make sure image is within glyph bounds }
            if Bitmap.Width < GlyphRect.Right - GlyphRect.Left then
              with GlyphRect do
              begin
                Right := Right - ((Right - Left) - Bitmap.Width) div 2 + 1;
                Left := Right - Bitmap.Width;
              end;
            if Bitmap.Height < GlyphRect.Bottom - GlyphRect.Top then
              with GlyphRect do
              begin
                Top := Top + ((Bottom - Top) - Bitmap.Height) div 2 + 1;
                Bottom := Top + Bitmap.Height;
              end;
            StretchDraw(GlyphRect, Bitmap);
            GlyphRect := SaveRect;
          end;
    
          if Checked then
          begin
            Dec(GlyphRect.Right);    
            Dec(GlyphRect.Bottom);    
          end;
        end
        else
        begin
          if (ImageList <> nil) and not TopLevel then
          begin
            GlyphRect.Left := GlyphRect.Right - ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end
          else
          begin
            GlyphRect.Left := GlyphRect.Right;
            GlyphRect.Bottom := GlyphRect.Top;
          end;
          DrawGlyph := False;
        end;    
      end;
      with GlyphRect do
      begin
        Dec(Left);
        Dec(Top);
        Inc(Right, 2);
        Inc(Bottom, 2);    
      end;    

      if Checked or Selected and DrawGlyph then
        DrawEdge(Handle, GlyphRect, EdgeStyle[Checked], BF_RECT);    

      if Selected then
      begin
        if DrawGlyph then ARect.Right := GlyphRect.Left - 1;
        if not (Win98Plus and TopLevel) then
          Brush.Color := clHighlight;
        FillRect(ARect);    
      end;    
      if TopLevel and Win98Plus then
      begin
        if Selected then
          DrawEdge(Handle, ARect, BDR_SUNKENOUTER, BF_RECT)
        else if odHotLight in State then
          DrawEdge(Handle, ARect, BDR_RAISEDINNER, BF_RECT);
        if not Selected then
          OffsetRect(ARect, 0, -1);
      end;
      if not (Selected and DrawGlyph) then
        ARect.Right := GlyphRect.Left - 1;
      Inc(ARect.Left, 2);
      Dec(ARect.Right, 1);    

      DrawStyle := DT_EXPANDTABS or DT_SINGLELINE or Alignments[Alignment];    
      if Win2K and (odNoAccel in State) then
        DrawStyle := DrawStyle or DT_HIDEPREFIX;
      { Calculate vertical layout }
      SaveRect := ARect;    
      if odDefault in State then
        Font.Style := [fsBold];
      DoDrawText(Sender as TMenuItem, ACanvas, Caption, ARect, Selected, DrawStyle or DT_CALCRECT or DT_NOCLIP);
      { the DT_CALCRECT does not take into account alignment }
      ARect.Left := SaveRect.Left;
      ARect.Right := SaveRect.Right;
      OffsetRect(ARect, 0, ((SaveRect.Bottom - SaveRect.Top) - (ARect.Bottom - ARect.Top)) div 2);
      if TopLevel and Selected and Win98Plus then
        OffsetRect(ARect, 1, 0);
      DoDrawText(Sender as TMenuItem, ACanvas, Caption, ARect, Selected, DrawStyle);
      if (ShortCut <> 0) and not TopLevel then
      begin
        S := ShortCutToText(ShortCut);
        ARect.Left := 10;
        ARect.Right := ARect.Left + ACanvas.TextWidth(S);
        DoDrawText(Sender as TMenuItem, ACanvas, S, ARect, Selected, DT_RIGHT);
      end;
    end;
  end;

begin
  with Sender as TMenuItem do
  begin
    ParentMenu := GetParentMenu;
    ImageList := GetImageList;
    Selected := odSelected in State;
    Win98Plus := (Win32MajorVersion > 4) or
      ((Win32MajorVersion = 4) and (Win32MinorVersion > 0));
    Win2K := (Win32MajorVersion > 4) and (Win32Platform = VER_PLATFORM_WIN32_NT);
    if (ParentMenu <> nil) and (not ParentMenu.IsRightToLeft) then
      NormalDraw
    else
      BiDiDraw;
  end;
end;

procedure TMainForm.GoBackFromQuotesClick(Sender: TObject);
begin
  CurMDIForm.Show;
  CurMDIForm:=nil;
  GoToQuotes.Visible:=true;
  GoBackFromQuotes.Visible:=false;
end;

procedure TMainForm.N2Click(Sender: TObject);
begin
  frmHelp:=TfrmHelp.Create(Self, 'E');
  frmHelp.ShowModal;
  frmHelp.Free;
end;

end.
