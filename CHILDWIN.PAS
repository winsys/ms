unit Childwin;

interface

uses Windows, Classes, Graphics, Forms, Controls, StdCtrls, ComCtrls,
  ImgList, ToolWin, Menus, Registry, ActnList, ExtCtrls, Dialogs, SysUtils,
  RichEdit2, DataUnit, Buttons, TreeNT, RichPrint, Mask, ToolEdit, CurrEdit,
  Messages;//, clipbrd;

type
  TDATARec = record
    ID         : longint;
    CNUM       : integer;
    ETITLE     : string;
    TITLEID    : integer;
    Abzac      : string;
    Notes      : string;
    Title      : string;
    NoteId     : integer;
    ISBIBLE: boolean;
  end;

  TMDIChild = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ImageList2: TImageList;
    ToolButton9: TToolButton;
    ActionList1: TActionList;
    Print: TAction;
    Save: TAction;
    SaveAs: TAction;
    FindContext: TAction;
    Send: TAction;
    MoveToQuotes: TAction;
    SaveDialog: TSaveDialog;
    PrintDialog: TPrintDialog;
    TXT: TRichEdit98;
    tb_AddParagraphNotes: TToolButton;
    sb_ShowParagraphNotes: TSpeedButton;
    sb_ShowSelectedText: TSpeedButton;
    RichPrinter1: TRichPrinter;
    DelChapter: TAction;
    sb_ShowParagraphTitles: TSpeedButton;
    AddParNote: TAction;
    ShowNotes: TAction;
    ShowSearchRes: TAction;
    ShowHeaders: TAction;
    PopupMenu1: TPopupMenu;
    c1: TMenuItem;
    d1: TMenuItem;
    gh1: TMenuItem;
    j1: TMenuItem;
    bb1: TMenuItem;
    N3: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    DelChaptersBtn: TToolButton;
    Panel9: TPanel;
    Label2: TLabel;
    SearchCount: TRxCalcEdit;
    tb_PreviousRecs: TSpeedButton;
    Label3: TLabel;
    tb_NextRecs: TSpeedButton;
    Label1: TLabel;
    ToolButton2: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton10: TToolButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure TXTChange(Sender: TObject);
    procedure PrintExecute(Sender: TObject);
    procedure SaveExecute(Sender: TObject);
    procedure FindContextExecute(Sender: TObject);
    procedure SendExecute(Sender: TObject);
    procedure MoveToQuotesExecute(Sender: TObject);
    procedure AddParNoteExecute(Sender: TObject);
    procedure ShowExtendedText(Sender: TObject);
    procedure tb_NextRecClick(Sender: TObject);
    procedure tb_PreviousRecClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure DelChapterExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TXTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure File1MeasureItem(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);
    procedure DoDrawText(AMenuItem: TMenuItem; ACanvas: TCanvas; const ACaption: string;
      var Rect: TRect; Selected: Boolean; Flags: Longint);
    procedure File1AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; State: TOwnerDrawState);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    function  GetCurrentChapter: TDATARec;
    function  GetSelectedChapText: string;
  public
    { Private declarations }
    FFileName: string;                     // - имя файла (если было сохранено содержимое, или он был открыт), иначе "".
    CurrentPage: integer;
    FFirstRecordPos: integer;              // - позиция первой выбранной записи.
    FLastRecordPos: integer;               // - позиция последней выбранной записи.
    SearchTextString: string;              // - строка для поиска.
    TitlesSearchList: string;              // - список абзацев для поиска.
    CaseSensitive: boolean;
    { Public declarations }
    ChaptersCount: integer;
    FDATA: array of TDATARec;
    ParamArray: array of string;
    scfNormal,
    scfSong,
    scfBible,
    scfTree,
    prfNormal,
    prfSong,
    prfBible,
    scfBibleText,
    scf1,
    scf2,
    scf3: string;
    function DATAToStr: string;
    procedure ReloadRecords(StartPos: integer; var RecCnt: integer);
    function  TitlesIdList: string;
    procedure DeleteChapter;
    procedure Redraw(sDATA: string);
    procedure SaveText(AFName: string);
    procedure OpenText(AFName: string);
    function  CurrentChapterIndex: integer;   // - ф-ция возвращает порядковый номер абзаца в массиве FDATA.

    constructor Create(AOwner: TComponent; const ACaption: string);

    property CurrentChapter: TDATARec read GetCurrentChapter;
    property SelectedChapText: string read GetSelectedChapText;
  end;

const
  Names:array[1..7] of string = ('Sermon','Song','Bible','Tree','prnSermon','prnSong','prnBible');

var
   f: Text;

implementation

uses Main, QuotesFormUnit, inf_SendByEMailUnit;

{$R *.DFM}

constructor TMDIChild.Create(AOwner: TComponent; const ACaption: string);
begin
  inherited Create(AOwner);
  if ACaption='TEXT'
    then MainForm.ImageList2.GetIcon(0, Icon)
    else
  if ACaption='SEARCH'
    then MainForm.ImageList2.GetIcon(1, Icon)
    else
  if ACaption='QUOTES'
    then MainForm.ImageList2.GetIcon(2, Icon)
    else
  if ACaption='CONTEXT'
    then MainForm.ImageList2.GetIcon(3, Icon);
end;

function TMDIChild.DATAToStr: string;
var i, k, InsPos, CurPos, cabz: integer;
    a,b,c,CS: string;

  function SixStr: string;
  begin
    Result:=IntToStr(FDATA[cabz].ID);
    while length(Result)<6 do
      Result:='0'+Result;
  end;

  function GetCurInsStr(AValue: string): string;
  begin
    if AValue='@'
      then Result:=songIndent+scfSong
      else if AValue='*'
             then Result:=bibleIndent+scfBible
             else if AValue='^'
                    then Result:='\b '
                    else if AValue='~1'
                           then Result:=scf1
                           else if AValue='~2'
                                  then Result:=scf2
                                  else if AValue='~3'
                                         then Result:=scf3
                                         else if FDATA[cabz].ISBIBLE
                                                then Result:=scfBibleText
                                                else Result:=scfNormal;
  end;

  function ConvertAbzac: string;
  var Stack : array of string;
      i: integer;
      isSong: boolean;
      isBible, NumSelected: boolean;
      s: string;
      CurSymb, InsStr: string;

      procedure WriteToStack(AValue: string);
      begin
        SetLength(Stack, High(Stack)+2);
        Stack[High(Stack)]:=AValue;
      end;

      function DeleteFromStack: string;
      begin
        if High(Stack)>=0
          then begin
            Result:=Stack[High(Stack)];
            SetLength(Stack, High(Stack));
          end
          else Result:='';
      end;

      function StackCount: integer;
      begin
        Result:=High(Stack)+1;
      end;

  begin
    isSong:=false;
    isBible:=false;
    SetLength(Stack, 0);
    if FDATA[cabz].ISBIBLE
      then begin
        s:=GetCurInsStr('x')+'\cf5 '+FDATA[cabz].Abzac;
        i:=length(GetCurInsStr('x')+'\cf5 ')+1;
      end
      else begin
        s:=GetCurInsStr('x')+FDATA[cabz].Abzac;
        i:=1;
      end;
    NumSelected:=false;
    repeat
      if FDATA[cabz].ISBIBLE then
        if not NumSelected
          then begin
            while s[i]<>#9 do
              Inc(i);
            insert('\cf1 ', s, i);
            NumSelected:=true;
            inc(i,5);
            s[i+1]:=AnsiUpperCase(s[i+1])[1];
          end;
      inc(i);
      if s[i] in ['@','*','~','^',#13,#10]
        then begin
          InsStr:='';
          InsPos:=i;
          CurSymb:=s[i];
          if ((s[i]='~') and (i<Length(s)-1) and (s[i+1] in ['1','2','3'])) or
             ((s[i]=#13) and (i<Length(s)-1) and (s[i+1]=#10))
            then CurSymb:=CurSymb+s[i+1];
          if (CurSymb=#13#10) or (CurSymb=#13)
            then begin
              if (isSong or isBible)
                then if isBible
                        then InsStr:='\par'+bibleIndent+scfBible
                        else InsStr:='\par'+songIndent+scfSong
                else InsStr:='\par\li50\ri50'+scfNormal;
              if isBible
                then if s[i+1]=#10
                       then if s[i+2]<>#9
                              then InsStr:=InsStr+#9
                              else
                       else if s[i+1]<>#9
                              then InsStr:=InsStr+#9;
            end
            else begin
              if StackCount=0
                then begin
                  if CurSymb='@'
                    then isSong:=true;
                  if CurSymb='*'
                    then isBible:=true;
                  WriteToStack(CurSymb);
                  if isBible
                    then InsStr:=GetCurInsStr(CurSymb)+#9
                    else InsStr:=GetCurInsStr(CurSymb);
                end
                else begin
                  if ((CurSymb='@') or (CurSymb='*') or (CurSymb='^') or (CurSymb='~')) and (Stack[High(Stack)][1]=CurSymb)
                    then begin
                      if CurSymb='@'
                        then isSong:=false;
                      if CurSymb='*'
                        then isBible:=false;
                      DeleteFromStack;
                      if CurSymb='^'
                        then InsStr:='\b0 '
                        else
                      if CurSymb='@'
                        then InsStr:=''//songIndent
                        else
                      if CurSymb='*'
                        then InsStr:=''//bibleIndent
                        else if StackCount>0
                                then InsStr:=GetCurInsStr(Stack[High(Stack)])
                                else InsStr:=GetCurInsStr('');
                    end
                    else begin
                      WriteToStack(CurSymb);
                      InsStr:=GetCurInsStr(CurSymb);
                    end;
                end;
            end;
          Delete(s,InsPos,Length(CurSymb));
          if InsStr<>''
            then Insert(InsStr,s,InsPos);
          inc(i, Length(InsStr)-Length(CurSymb));
        end;
    until i=Length(s);
    SetLength(Stack, 0);
    Result:=s+'\par ';
//    ClipBoard.AsText:=Result;
  end;

  function LastCF(Src: string): string;
  begin
    Result:='\cf1 ';
    while Pos('\cf', src)>0 do
     begin
       Result:=Copy(src, Pos('\cf', src), 4)+' ';
       src:=Copy(src, Pos('\cf', src)+3, Length(src));
     end;
  end;

begin
  Result:='';
  for cabz:=0 to High(FDATA) do
    begin
      {if FDATA[cabz].ISBIBLE and sb_ShowParagraphNotes.Down
        then CS:=scfBibleText
        else }CS:=scfNormal;
      if sb_ShowParagraphTitles.Down
        then if ChildOperation='Citation'
                then CS:=CS+{\b}'\ul\fs18\cf9 '+AnsiUpperCase(FDATA[cabz].Title)+'\fs1\ul0\cf7  bb'+SixStr+'\par '+scfNormal
                else CS:=CS+{\b}'\ul\fs18\cf9 '+AnsiUpperCase(FDATA[cabz].Title)+'\fs1\ul0\cf8  bb'+SixStr+'\par '+scfNormal;
      if sb_ShowParagraphNotes.Down and (trim(FDATA[cabz].Notes)<>'')
        then if FDATA[cabz].ISBIBLE
               then CS:=CS+'\tab\highlight4 '+msMsg10+FDATA[cabz].Notes+'\highlight0\par '
               else CS:=CS+'\highlight4 '+msMsg10+FDATA[cabz].Notes+'\highlight0\par ';

      if FDATA[cabz].ISBIBLE
        then CS:=CS + '\tab '+ConvertAbzac+'\par '#13#10
        else CS:=CS + ConvertAbzac+'\par '#13#10;

      // поиск слов в абзаце, и их выделение(если это необходимо)
      if (sb_ShowSelectedText<>nil) and (sb_ShowSelectedText.Down)
        then for i:=0 to High(ParamArray) do
          if trim(ParamArray[i])<>''
            then begin
              CurPos:=1;
              repeat
                if CaseSensitive
                  then begin
                    a:=ParamArray[i];
                    b:=CS;
                    c:=copy(b, CurPos, length(b));
                  end
                  else begin
                    a:=AnsiUpperCase(ParamArray[i]);
                    b:=AnsiUpperCase(CS);
                    c:=copy(b, CurPos, length(b));
                  end;
                // замена "_" на пробел.
                for k:=1 to Length(a) do
                 if a[k]='_'
                   then begin
                     Insert(' ',a,k);
                     Delete(a,k+1,1);
                   end;

                InsPos:=Pos(a, c);
                if (InsPos>0) and (CurPos+InsPos<Length(CS))
                  then begin
                    insert('\cf2\highlight3 ', CS, InsPos+CurPos-1);
                    CurPos:=CurPos+InsPos-1+Length(ParamArray[i])+16;
                    insert('\highlight0'+LastCF(Copy(CS, 1,
                           CurPos-Length(ParamArray[i])-16)), CS, CurPos);
                    CurPos:=CurPos+15;
                  end;
              until InsPos=0;
            end;

      Result:=Result+CS;

    end;
end;

//------------------------------------------------------------------------------
// - ф-ция возвращает порядковый номер абзаца в массиве FDATA.
function TMDIChild.CurrentChapterIndex: integer;
var i,delta: integer;
    ANum: string;
begin
  Result:=-1;
  if trim(TXT.Lines.Text)='' then exit;
  if not sb_ShowParagraphTitles.Down
    then begin
      ShowMessage(msMsg11);
      exit;
    end;
  i:=TXT.CaretPos.y;// + 1 + byte(sb_ShowParagraphNotes.Down) + byte(sb_ShowParagraphTitles.Down);
  delta:=-1;
  while (TXT.Lines.Strings[i]='') or (pos('bb',TXT.Lines.Strings[i])=0) do
    if (TXT.Lines.Strings[i]='') and (i < TXT.Lines.Count-1)
      then begin
        delta:=1;
        i:=i+delta;
      end
      else i:=i+delta;
  ANum:=copy(TXT.Lines.Strings[i], pos('bb',TXT.Lines.Strings[i])+2, 6);
  for i:=low(FDATA) to high(FDATA) do
    if FDATA[i].ID=StrToInt(ANum)
      then Result:=i;
  if Result < 0
    then ShowMessage(msMsg12);
end;

//------------------------------------------------------------------------------
function TMDIChild.GetCurrentChapter: TDATARec;
var i: integer;
begin
  i:=CurrentChapterIndex;
  if i<0
    then Result:=FDATA[0]
    else Result:=FDATA[i];
end;

function TMDIChild.GetSelectedChapText: string;
begin
  GetSelectedChapText:=TXT.SelText;
end;

procedure TMDIChild.DeleteChapter;
var i, j: integer;
begin
  i:=CurrentChapterIndex;
  if i<0 then exit;
  if i<>High(FDATA)
    then for j:=i to High(FDATA)-1 do
           FDATA[j]:=FDATA[j+1];
  SetLength(FDATA, High(FDATA));
  Redraw(DATAToStr);
end;

procedure TMDIChild.Redraw(sDATA: string);
var ST: TStringStream;
    CarX, CarY: integer;
begin
{  ToolButton9.Enabled:=sb_ShowParagraphTitles.Down;
  ToolButton4.Enabled:=sb_ShowParagraphTitles.Down;
  AddParNote.Enabled:=sb_ShowParagraphTitles.Down;  }
  CarX:=TXT.CaretPos.X;
  CarY:=TXT.CaretPos.Y;
  if high(FDATA)>=0
    then if FDATA[0].ISBIBLE
           then ST:=TStringStream.Create(RtfHeader + scfBibleText + sDATA + '}')
           else ST:=TStringStream.Create(RtfHeader + scfNormal + sDATA + '}')
    else ST:=TStringStream.Create(RtfHeader + scfNormal + sDATA + '}');
  TXT.Lines.LoadFromStream(ST);
  ST.Free;
  TXT.SetCaret(CarY, CarX);
end;

procedure TMDIChild.SaveText(AFName: string);
var i: integer;
    MS: TMemoryStream;

  procedure WriteToMemoryStream(const AStr: string);
  var tmp: PChar;
      Cnt: longint;
  begin
    Cnt:=length(AStr);
    GetMem(tmp, Cnt+1);
    StrPCopy(tmp, AStr);
    MS.Write(Cnt, 4);
    MS.Write(tmp^, Cnt);
    FreeMem(tmp);
  end;

begin
  if ExtractFileExt(AFName)='.rtf'
    then TXT.Lines.SaveToFile(AFName)
    else begin
      MS:=TMemoryStream.Create;
      MS.Clear;
      for i:=0 to High(FDATA) do
       begin
         WriteToMemoryStream(IntToStr(FDATA[i].ID));
         WriteToMemoryStream(IntToStr(FDATA[i].CNUM));
         WriteToMemoryStream(IntToStr(FDATA[i].TITLEID));
         WriteToMemoryStream(IntToStr(FDATA[i].NoteId));
         WriteToMemoryStream(IntToStr(byte(FDATA[i].ISBIBLE)));
         WriteToMemoryStream(FDATA[i].Title);
         WriteToMemoryStream(FDATA[i].Notes);
         WriteToMemoryStream(FDATA[i].Abzac);
       end;
      MS.SaveToFile(AFName);
      MS.Free;
    end;
end;

procedure TMDIChild.OpenText(AFName: string);
var MS: TMemoryStream;
    s: string;

  function GetNextStringFromStream: string;
  var tmp: PChar;
      Cnt: longint;
  begin
    if MS.Position<MS.Size
      then begin
        MS.Read(Cnt, 4);
        GetMem(tmp, Cnt+1);
        FillChar(tmp^, Cnt+1, 0);
        MS.Read(tmp^, Cnt);
        Result:=StrPas(tmp);
        FreeMem(tmp);
      end
      else Result:='#';
  end;

begin
  if AFName<>''
    then begin
      MS:=TMemoryStream.Create;
      MS.LoadFromFile(AFName);
      SetLength(FDATA, 0);
      s:=GetNextStringFromStream;
      while s<>'#' do
       begin
         SetLength(FDATA, High(FDATA)+2);
         FDATA[High(FDATA)].ID:=StrToInt(s);
         s:=GetNextStringFromStream;
         if s<>'#'
           then FDATA[High(FDATA)].CNUM:=StrToInt(s);
         s:=GetNextStringFromStream;
         if s<>'#'
           then FDATA[High(FDATA)].TITLEID:=StrToInt(s);
         s:=GetNextStringFromStream;
         if s<>'#'
           then FDATA[High(FDATA)].NoteId:=StrToInt(s);
         s:=GetNextStringFromStream;
         FDATA[High(FDATA)].ISBIBLE:=boolean(StrToInt(s));
         s:=GetNextStringFromStream;
         if s<>'#'
           then begin
             FDATA[High(FDATA)].Title:=s;
           end;
         s:=GetNextStringFromStream;
         if s<>'#'
           then FDATA[High(FDATA)].Notes:=s;
         s:=GetNextStringFromStream;
         if s<>'#'
           then FDATA[High(FDATA)].Abzac:=s;
         s:=GetNextStringFromStream;
       end;
      MS.Free;
    end;
end;

procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure TMDIChild.FormShow(Sender: TObject);
var CurLine,i,j,k: integer;
    ParamString: string;
    fnd: boolean;
begin
  FFirstRecordPos:=0;
  FLastRecordPos:=0;
  SearchTextString:=MainForm.SearchText.Text;
  CaseSensitive:=MainForm.sb_CaseSensitive.Down;
  DelChapter.Enabled:=false;
  TitlesSearchList:=TitlesIdList;
  SetLength(ParamArray, 0);

  // Language-dependent settings
  TXT.Font.Name:=msFontName;
  TXT.Font.Charset:=msFontCharset;

  Label2.Caption:=msMsg18;
  SetLanguage(Label1, msMsg13);  // total
  SetLanguage(Label2, msMsg18); // par count
  SetLanguage(Label3);

  SearchCount.Left:=Label2.Left + Label2.Width;
  tb_PreviousRecs.Left:=SearchCount.Left + 31;
  Label3.Left:=SearchCount.Left + 51;

  tb_NextRecs.Left:=Label3.Left + Label3.Width+ 2;
  Label1.Left:=tb_NextRecs.Left + 23;

  // Set action hints
  for i:=0 to 10 do
    (ActionList1.Actions[i] as TAction).Hint:=msAct1[i];
  Print.Caption:=msAct1[0];
  Save.Caption:=msAct1[1];
  FindContext.Caption:=msAct1[3];
  Send.Caption:=msAct1[4];
  MoveToQuotes.Caption:=msAct1[5];
  DelChaptersBtn.Caption:=msAct1[6];
  AddParNote.Caption:=msAct1[7];

  // default styles
  scfNormal:='\plain\f0\fs24\cf1\li50\ri50 ';
  scfSong:='\plain\f0\fs22\cf1 ';
  scfBible:='\plain\f1\fs25\cf6\i ';
  scfTree:='\plain\f0\fs24\cf1 ';

  scf1:=scfNormal;                // Добавочные слова в цитатах.
  scf2:='\i ';  // Выделение в основном тексте.
  scf3:='\plain\f0\fs20\cf5 ';    // Примечания переводчика.
  scfBibleText:='\li50\ri50\plain\f1\fs25\cf1 ';

  if ChildOperation='DblClick'
    then begin
      Setlength(ParamArray, High(tmpParamArray)+1);
      for i:=0 to High(ParamArray) do
        ParamArray[i]:=tmpParamArray[i];

      for i:=0 to SearchResult.HighResultTXT do
       begin
        SetLength(FDATA,i+1);
        FDATA[i].ID:=SearchResult.ReadInf(i).ID;
        FDATA[i].CNUM:=SearchResult.ReadInf(i).CNUM;
        FDATA[i].ETITLE:=SearchResult.ReadInf(i).ETITLE;
        FDATA[i].TITLEID:=SearchResult.ReadInf(i).TITLEID;
        FDATA[i].Abzac:=SearchResult.ReadInf(i).Abzac;
        FDATA[i].Title:=SearchResult.ReadInf(i).Title;
        FDATA[i].Notes:=SearchResult.ReadInf(i).Notes;
        FDATA[i].NoteId:=SearchResult.ReadInf(i).NoteId;
        FDATA[i].ISBIBLE:=SearchResult.ReadInf(i).ISBIBLE;
      end;
      Panel9.Visible:=false;
      sb_ShowSelectedText.Down:=true;
      Redraw(DATAToStr);
    end;

  if ChildOperation='Context'
    then begin
      Setlength(ParamArray, High(tmpParamArray)+1);
      for i:=0 to High(ParamArray) do
        ParamArray[i]:=tmpParamArray[i];

      for i:=0 to SearchResult.HighResultTXT do
       begin
        SetLength(FDATA,i+1);
        FDATA[i].ID:=SearchResult.ReadInf(i).ID;
        FDATA[i].CNUM:=SearchResult.ReadInf(i).CNUM;
        FDATA[i].ETITLE:=SearchResult.ReadInf(i).ETITLE;
        FDATA[i].TITLEID:=SearchResult.ReadInf(i).TITLEID;
        FDATA[i].Abzac:=SearchResult.ReadInf(i).Abzac;
        FDATA[i].Title:=SearchResult.ReadInf(i).Title;
        FDATA[i].Notes:=SearchResult.ReadInf(i).Notes;
        FDATA[i].NoteId:=SearchResult.ReadInf(i).NoteId;
        FDATA[i].ISBIBLE:=SearchResult.ReadInf(i).ISBIBLE;
      end;
      Panel9.Visible:=false;
      sb_ShowSelectedText.Down:=true;
      Redraw(DATAToStr);

      // поиск параграфа в контексте.
      CurLine:=1; fnd:=false;
      while (not fnd) and (CurLine<=TXT.Lines.Count) do
       if pos('bb',TXT.Lines.Strings[CurLine])=0
         then Inc(CurLine)
         else if TXT.Lines.Strings[CurLine][pos('bb',TXT.Lines.Strings[CurLine])+2] in ['0'..'9']
                then if tmpContextNum = StrToInt(copy(TXT.Lines.Strings[CurLine], pos('bb',TXT.Lines.Strings[CurLine])+2, 6))
                       then Fnd:=true
                       else Inc(CurLine)
                else Inc(CurLine);
      if fnd
        then TXT.SetCaret(CurLine, 0);
    end;

  if ChildOperation='Find'
    then begin
      ParamString:=PrepareParams(SearchTextString);
      // выделение слов из строки поиска
      j:=1;
      SetLength(ParamArray, j);
      ParamArray[j-1]:='';
      for k:=1 to Length(ParamString) do
        begin
          if not (ParamString[k] in [TOpKindSymbols[opAnd], TOpKindSymbols[opOr], TOpKindSymbols[opWhole], TOpKindSymbols[opNot], ')', '('])
            then ParamArray[j-1]:=ParamArray[j-1]+ParamString[k];
          if (ParamString[k] in [TOpKindSymbols[opAnd], TOpKindSymbols[opOr], TOpKindSymbols[opWhole], TOpKindSymbols[opNot], ')', '(']) and (ParamArray[j-1]<>'')
            then begin
              Inc(j);
              SetLength(ParamArray, j);
              ParamArray[j-1]:='';
            end;
        end;
      if ParamArray[0]='' then ParamArray[0]:=ParamString;

      sb_ShowSelectedText.Down:=true;
      tb_NextRecClick(self);  // Searches database
    end;
  if ChildOperation='Open'
    then begin
      Panel9.Visible:=false;
//      tb_NextRecs.Visible:=False;
//      tb_PreviousRecs.Visible:=False;
//      SearchCount.Visible:=False;
//      Label2.Visible:=False;
//      Label1.Visible:=false;
//      Label3.Visible:=false;

//      Panel8.Visible:=false;
//      Panel9.Visible:=false;

      DelChapter.Enabled:=false;
      OpenText(FFileName);
      Redraw(DATAToStr);
    end;
  if ChildOperation='Citation'
    then begin
      Panel9.Visible:=false;

      DelChapter.Enabled:=true;
      if FFileName<>'NONAME'
        then OpenText(FFileName);
      Redraw(DATAToStr);
    end
    else Label1.Caption:=msMsg13+IntToStr(LastFoundChaptersCount);
end;


procedure TMDIChild.TXTChange(Sender: TObject);
begin
  if (TXT.Lines.Count>0) or (TXT.Lines.Strings[0]<>'') then
  begin
    Save.Enabled:=true;
    SaveAs.Enabled:=true;
    Send.Enabled:=true;
    Print.Enabled:=true;
    AddParNote.Enabled:=true;
    DelChapter.Enabled:=true;
    if ToolButton9.Visible
      then FindContext.Enabled:=true;
    if ToolButton4.Visible
      then MoveToQuotes.Enabled:=true;
    if sb_ShowSelectedText.Visible
      then sb_ShowSelectedText.Enabled:=true;
    if sb_ShowParagraphNotes.Visible
      then sb_ShowParagraphNotes.Enabled:=true;
    if sb_ShowParagraphTitles.Visible
      then sb_ShowParagraphTitles.Enabled:=true;
  end
  else
  begin
    Save.Enabled:=false;
    SaveAs.Enabled:=false;
    Send.Enabled:=false;
    Print.Enabled:=false;
    DelChapter.Enabled:=false;
    AddParNote.Enabled:=false;
    if ToolButton9.Visible
      then FindContext.Enabled:=false;
    if ToolButton4.Visible
      then MoveToQuotes.Enabled:=false;
    if sb_ShowSelectedText.Visible
      then sb_ShowSelectedText.Enabled:=false;
    if sb_ShowParagraphNotes.Visible
      then sb_ShowParagraphNotes.Enabled:=false;
    if sb_ShowParagraphTitles.Visible
      then sb_ShowParagraphTitles.Enabled:=false;
  end;
  if sb_ShowParagraphTitles.Visible and sb_ShowParagraphNotes.Enabled
    then begin
      ToolButton9.Enabled:=sb_ShowParagraphTitles.Down;
      ToolButton4.Enabled:=sb_ShowParagraphTitles.Down;
      AddParNote.Enabled:=sb_ShowParagraphTitles.Down;
    end;
end;

procedure TMDIChild.PrintExecute(Sender: TObject);
begin
  RichPrinter1.Title:=Caption;
  if RichPrinter1.PageDialog
    then RichPrinter1.PrintRichEdit(TXT, 1);
end;

procedure TMDIChild.SaveExecute(Sender: TObject);
begin
  with TRegistry.Create do
  begin
    SaveDialog.FileName:=FFileName;
    OpenKey(SystemKey, false);
    CreateDir('C:\MS\'+msMsg33);
    if ValueExists('Save dialog')
      then SaveDialog.InitialDir:=ReadString('Save dialog')
      else SaveDialog.InitialDir:='C:\MS\'+msMsg33;
    if SaveDialog.Execute
      then if SaveDialog.FileName<>''
             then begin
               SaveText(SaveDialog.FileName);
               WriteString('Save dialog', ExtractFilePath(SaveDialog.FileName));
               if Self is TQuotesForm
                 then (Self as TQuotesForm).Changed:=false;
             end;
    CloseKey;
    Free;
  end;
end;

procedure TMDIChild.FindContextExecute(Sender: TObject);
var Child: TMDIChild;
    i: smallint;
begin
//  ShowMessage('Функция временно отключена... Идет доработка...');
  ChildOperation:='Context';
  SetLength(tmpParamArray, High(ParamArray)+1);
  for i:=0 to High(tmpParamArray) do
   tmpParamArray[i]:=ParamArray[i];
  tmpContextNum:=CurrentChapter.ID;
  SearchResult.OpenContext(CurrentChapter.TITLEID);
  Child:=TMDIChild.Create(Application, 'CONTEXT');
  Child.Caption:=msMsg14+IntToStr(CurrentChapter.CNUM)+'] - '+copy(CurrentChapter.Title,1,pos('-',CurrentChapter.Title)-2);
end;

procedure TMDIChild.SendExecute(Sender: TObject);
var tmpFName: string;
begin
  tmpFName:=NormalizeFName(ExtractFilePath(Application.ExeName), 'ms_quotes.rtf');
  TXT.Lines.SaveToFile(tmpFName);
  inf_SendByEMail:=Tinf_SendByEMail.Create(self, tmpFName);
  inf_SendByEMail.ShowModal;
  inf_SendByEMail.Free;
  DeleteFile(tmpFName);
end;

procedure TMDIChild.MoveToQuotesExecute(Sender: TObject);
begin
  if CurrentChapter.Abzac<>''
    then begin
      if assigned(QuotesForm)
        then
        else begin
          CurMDIForm:=Self;
          ChildOperation:='Citation';
          QuotesForm:=TQuotesForm.Create(Application, 'NONAME');
          MainForm.GoToQuotes.Visible:=false;
          MainForm.GoBackFromQuotes.Visible:=true;
        end;
      QuotesForm.AddChapter(CurrentChapter);
      QuotesForm.Redraw(QuotesForm.DATAToStr);
    end;
end;

procedure TMDIChild.AddParNoteExecute(Sender: TObject);
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
          if FDATA[tmpCurChapterInd].NoteId>0
            then Data.ExecSQL('update notes set COMMENT="'+SQLQuotes(ANote)+'" where ID='+IntToStr(FDATA[tmpCurChapterInd].NoteId))
            else begin
              Data.ExecSQL('insert into notes (TITLEID, CNUM, COMMENT)'+
                           ' select TITLEID, CNUM, "'+SQLQuotes(ANote)+'" from data where ID='+IntToStr(FDATA[tmpCurChapterInd].ID));
              FDATA[tmpCurChapterInd].NoteId:=Data.IntQuery('select max(ID) from notes');
            end;
          if sb_ShowParagraphNotes.Down
            then Redraw(DATAToStr);
        end;
    end;
end;

procedure TMDIChild.ShowExtendedText(Sender: TObject);
begin
  Redraw(DATAToStr);
end;

function TMDIChild.TitlesIdList: string;
var i: integer;
begin
  Result:='';
  for i:=0 to MainForm.Tree.Items.Count-1 do
   if (MainForm.Tree.Items[i].CheckState in [csChecked, csGrayed, csCheckedGrayed]) and (MainForm.Tree.Items[i].iCatID>0)
     then if trim(Result)=''
            then Result:=IntToStr(MainForm.Tree.Items[i].iCatID)
            else Result:=Result+','+IntToStr(MainForm.Tree.Items[i].iCatID);
  if trim(Result)<>''
    then Result:=' and d.TITLEID in ('+Result+')';
end;


procedure TMDIChild.ReloadRecords(StartPos: integer; var RecCnt: integer);
var i: integer;
begin
  SearchResult.FindIt(SearchTextString, StartPos, RecCnt, TitlesSearchList, CaseSensitive);
  tb_PreviousRecs.Enabled:=(StartPos>0);
  if SearchResult.RecCount<RecCnt
    then tb_NextRecs.Enabled:=false
    else tb_NextRecs.Enabled:=true;
  RecCnt:=SearchResult.RecCount;
  Label3.Caption:=' '+IntToStr(StartPos+1)+'-'+IntToStr(StartPos+RecCnt)+' ';
  tb_NextRecs.Left:=Label3.Left + Label3.Width+ 2;
  Label1.Left:=tb_NextRecs.Left + 23;
  for i:=0 to SearchResult.HighResultTXT do
    begin
      SetLength(FDATA,i+1);
      FDATA[i].ID:=SearchResult.ReadInf(i).ID;
      FDATA[i].CNUM:=SearchResult.ReadInf(i).CNUM;
      FDATA[i].ETITLE:=SearchResult.ReadInf(i).ETITLE;
      FDATA[i].TITLEID:=SearchResult.ReadInf(i).TITLEID;
      FDATA[i].Abzac:=SearchResult.ReadInf(i).Abzac;
      FDATA[i].Title:=SearchResult.ReadInf(i).Title;
      FDATA[i].ISBIBLE:=SearchResult.ReadInf(i).ISBIBLE;
      FDATA[i].Notes:=SearchResult.ReadInf(i).Notes;
      FDATA[i].NoteId:=SearchResult.ReadInf(i).NoteId;
    end;
  Redraw(DATAToStr);
end;

procedure TMDIChild.tb_NextRecClick(Sender: TObject);
var SearchCnt: integer;
begin
  try
    SearchCnt:=StrToInt(SearchCount.Text);
   except
    raise Exception.Create(msMsg17);
  end;
  ReloadRecords(FLastRecordPos, SearchCnt);
  FFirstRecordPos:=FLastRecordPos;
  FLastRecordPos:=FLastRecordPos + SearchCnt;
end;

procedure TMDIChild.tb_PreviousRecClick(Sender: TObject);
var SearchCnt: integer;
begin
  try
    SearchCnt:=StrToInt(SearchCount.Text);
   except
    raise Exception.Create(msMsg17);
  end;
  if (FFirstRecordPos - SearchCnt)>=0
    then FFirstRecordPos:=FFirstRecordPos-SearchCnt
    else FFirstRecordPos:=0;
  ReloadRecords(FFirstRecordPos, SearchCnt);
  if FFirstRecordPos=0
    then FLastRecordPos:=SearchCnt
    else FLastRecordPos:=FLastRecordPos - SearchCnt;
  if CurrentPage<=1
    then CurrentPage:=1
    else dec(CurrentPage);
end;

procedure TMDIChild.FormActivate(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TMDIChild.DelChapterExecute(Sender: TObject);
begin
  DeleteChapter;
end;

procedure TMDIChild.FormDestroy(Sender: TObject);
begin
  SetLength(FDATA, 0);
  SetLength(ParamArray, 0);
  FFileName:='';
  FFirstRecordPos:=0;
  FLastRecordPos:=0;
  CurrentPage:=0;
  SearchTextString:='';
  TitlesSearchList:='';
  ChaptersCount:=0;
end;

procedure TMDIChild.TXTMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight
    then SendMessage(TXT.Handle, WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(X, Y));
end;



procedure TMDIChild.DoDrawText(AMenuItem: TMenuItem; ACanvas: TCanvas; const ACaption: string;
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


procedure TMDIChild.File1MeasureItem(Sender: TObject; ACanvas: TCanvas;
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

procedure TMDIChild.File1AdvancedDrawItem(Sender: TObject;
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



procedure TMDIChild.PopupMenu1Popup(Sender: TObject);
begin
  MoveToQuotes.Enabled:=(high(FDATA)>=0) and (sb_ShowParagraphTitles.Down);
  FindContext.Enabled:=(high(FDATA)>=0) and (sb_ShowParagraphTitles.Down);
end;

end.


