unit DataUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  comctrls, Db, ZQuery, ZMySqlQuery, ZTransact, ZMySqlTr, ZConnect, ZMySqlCon;

{$I CONSTANTS.PAS}

type
  TOpKind = (opAnd, opOr, opNot, opWhole);

const
  SystemKey = 'SOFTWARE\BIGIT Software\MessageSearch';
  // операции
  TOpKindSymbols: array[TOpKind] of char = ('+', '=', '-', '.');
  TOpSet: set of char = ['+', '=', '-', '.'];
  DefOperation: TOpKind = opAnd;
// количество окон с найденными абзацами
  RTXTN: integer = 0;
// после какой операции открываеться форма Childwin
  ChildOperation: string = 'Find';


  selTextColor: string = '\red0\green0\blue0;';
  selTextBack: string = '\red96\green199\blue249;';
  rtfHeader: string = ''; // see on data module create

  songIndent = '\li2000\ri1000';
  bibleIndent = '\li1300\ri1200';

type
  TData = class(TDataModule)
    DB: TZMySqlDatabase;
    Transact1: TZMySqlTransact;
    Query1: TZMySqlQuery;
    DataSource1: TDataSource;
    QTemp: TZMySqlQuery;
    TITLES: TZMySqlQuery;
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ExecSQL(AQuery: string);
    function  IntQuery(SqlText: string): integer;
  end;

// структура исходных данных для поиска
  TSearchParams = record
    Query: string;
  end;

// результат поиска - один обзац
  TResultRec = record
    ID      : longint;
    CNUM    : integer;
    TITLEID : integer;
    ISBIBLE : boolean;
    ETITLE  : string;
    Abzac   : string;
    Notes   : string;
    Title   : string;
    NoteId  : integer;
  end;

// параметры обзаца в контексте
  TSearchResult = class{(TList)}
  private
    ResultTXT: array of TResultRec;
    Function UpCaseStr(UpCaseString: String): String;
    Function BuildParam(Param: String; DefOp: TOpKind): String;
  public
    RecCount:  integer;
    function  HighResultTXT: Integer;
    procedure FindIt(Query: string; ALastPos, ACount: integer; ATitlesIdList: string; ACaseSensitive: boolean);
    procedure OpenContext(ATITLEID: integer);
    function  ReadInf(NumInf: integer): TResultRec;
  end;


//------------------------------------------------------------------------------
//
//const
//  Months: array[1..12] of string[3] = ('Янв', 'Фев', 'Мар', 'Апр', 'Май', 'Июн',
//                                  'Июл', 'Авг', 'Сен', 'Окт', 'Ноя', 'Дек');
//------------------------------------------------------------------------------


var
  Data: TData;
  SearchResult: TSearchResult;
  DefaultDir: string;
  tmpParamArray: array of string;
  tmpContextNum: integer;
  LastFoundChaptersCount: integer;
  isWholeChecked: boolean;


// Заменяет символы " в строке на "" для формирования запроса
function SQLQuotes(ASrc: string):string;
// Сохраняет/восстанавливает положение текущего окна
procedure SaveWindowPosition(AWin: TCustomForm; SaveSize: boolean = false);
procedure RestoreWindowPosition(AWin: TCustomForm; RestoreSize: boolean = false);
function  PrepareParams(ASrc: string):string;
function  NormalizeFName(APath, AName: string): string;
function  LZero(Ab: byte): string;
function  RGBToString(AColor: TColor): string;
procedure SetLanguage(ATarget: TControl; ANewCaption: string = '');


implementation

uses Registry, StdCtrls, TreeNT, Buttons, Main;

{$R *.DFM}

//------------------------------------------------------------------------------
// Функция возвращает цвет в виде: \redXXX\greenXXX\blueXXX
function RGBToString(AColor: TColor): string;
begin
  Result:='\red'+IntToStr(GetRValue(ColorToRGB(AColor)))+
          '\green'+IntToStr(GetGValue(ColorToRGB(AColor)))+
          '\blue'+IntToStr(GetBValue(ColorToRGB(AColor)));
end;

//------------------------------------------------------------------------------
// Функция возвращает полный путь к файлу.
function NormalizeFName(APath, AName: string): string;
begin
  APath:=trim(APath);
  AName:=trim(AName);
  if APath=''
    then Result:=AName
    else if APath[Length(APath)]<>'\'
           then Result:=APath+'\'+AName
           else Result:=APath+AName;
end;

//------------------------------------------------------------------------------
// Функция добавляет 0 впереди числа(если необходимо).
function LZero(Ab: byte): string;
begin
  if Ab>9
    then Result:=IntToStr(Ab)
    else Result:='0'+IntToStr(Ab);
end;

//------------------------------------------------------------------------------
//
function PrepareParams(ASrc: string):string;
var i: byte;
begin
  Result:=ASrc;
  // удаление двойных пробелов
  while pos('  ', Result)>0 do
    delete(Result, pos('  ', Result), 1);
  // замена пробелов на операцию по умолчанию
  for i:=1 to length(Result) do
    if Result[i]=' ' then Result[i]:=TOpKindSymbols[DefOperation];
end;

//------------------------------------------------------------------------------
//
function SQLQuotes(ASrc: string):string;
var i: integer;
begin
  Result:=ASrc;
  i:=1;
  while i<=length(Result) do
  begin
    if Result[i]='"'
      then begin
        insert('"', Result, i);
        inc(i);
      end;
    inc(i);
  end;
end;

//------------------------------------------------------------------------------

procedure SaveWindowPosition(AWin: TCustomForm; SaveSize: boolean = false);
begin
  with TRegistry.Create do
  if OpenKey(SystemKey+'\Forms\'+AWin.Name, true)
    then begin
      WriteInteger('Left', AWin.Left);
      WriteInteger('Top', AWin.Top);
      if SaveSize
        then begin
          WriteInteger('Width', AWin.Width);
          WriteInteger('Height', AWin.Height);
          WriteInteger('State', integer(AWin.WindowState));
        end;
      CloseKey;
      Free;
    end
    else Free;
end;

//------------------------------------------------------------------------------

procedure RestoreWindowPosition(AWin: TCustomForm; RestoreSize: boolean = false);
begin
  with TRegistry.Create do
  if OpenKey(SystemKey+'\Forms\'+AWin.Name, true)
    then begin
      if ValueExists('Left')
        then AWin.Left:=ReadInteger('Left')
        else WriteInteger('Left', AWin.Left);
      if ValueExists('Top')
        then AWin.Top:=ReadInteger('Top')
        else WriteInteger('Top', AWin.Top);
      if RestoreSize
        then begin
          if ValueExists('Width')
            then AWin.Width:=ReadInteger('Width')
            else WriteInteger('Width', AWin.Width);
          if ValueExists('Height')
            then AWin.Height:=ReadInteger('Height')
            else WriteInteger('Height', AWin.Height);
          if ValueExists('State')
            then AWin.WindowState:=TWindowState(ReadInteger('State'))
            else WriteInteger('State', integer(AWin.WindowState));
        end;
      CloseKey;
      Free;
    end
    else Free;
end;

//------------------------------------------------------------------------------

procedure SetLanguage(ATarget: TControl; ANewCaption: string = '');
begin
  if ATarget is TEdit
    then begin
      (ATarget as TEdit).Font.Name:=msFontName;
      (ATarget as TEdit).Font.Charset:=msFontCharset;
      if ANewCaption<>''
        then (ATarget as TEdit).Text:=ANewCaption;
    end
    else
  if ATarget is TLabel
    then begin
      (ATarget as TLabel).Font.Name:=msFontName;
      (ATarget as TLabel).Font.Charset:=msFontCharset;
      if ANewCaption<>''
        then (ATarget as TLabel).Caption:=ANewCaption;
    end
    else
  if ATarget is TTreeNT
    then begin
      (ATarget as TTreeNT).Font.Name:=msFontName;
      (ATarget as TTreeNT).Font.Charset:=msFontCharset;
    end
    else
  if ATarget is TComboBox
    then begin
      (ATarget as TComboBox).Font.Name:=msFontName;
      (ATarget as TComboBox).Font.Charset:=msFontCharset;
    end
    else
  if ATarget is TCheckBox
    then begin
      (ATarget as TCheckBox).Font.Name:=msFontName;
      (ATarget as TCheckBox).Font.Charset:=msFontCharset;
      if ANewCaption<>''
        then (ATarget as TCheckBox).Caption:=ANewCaption;
    end
    else
  if ATarget is TButton
    then begin
      (ATarget as TButton).Font.Name:=msFontName;
      (ATarget as TButton).Font.Charset:=msFontCharset;
      if ANewCaption<>''
        then (ATarget as TButton).Caption:=ANewCaption;
    end
    else
  if ATarget is TBitBtn
    then begin
      (ATarget as TBitBtn).Font.Name:=msFontName;
      (ATarget as TBitBtn).Font.Charset:=msFontCharset;
      if ANewCaption<>''
        then (ATarget as TBitBtn).Caption:=ANewCaption;
    end;
end;

//------------------------------------------------------------------------------

procedure TDATA.ExecSQL(AQuery: string);
begin
  if QTemp.Active then QTemp.Close;
  QTemp.SQL.Text:=AQuery;
  QTemp.ExecSQL;
end;

//------------------------------------------------------------------------------
//
function TDATA.IntQuery(SqlText: string): integer;
begin
  try
    if QTemp.Active
      then QTemp.Close;
    QTemp.Sql.Text:=SqlText;
    QTemp.Open;
    Result:=QTemp.Fields[0].AsInteger;
   except
    Result:=0;
  end;
end;



Function TSearchResult.UpCaseStr(UpCaseString : String): String;
Var
   s:String;
   i:Integer;
Begin
 s:='';
 For i:=1 To Length(UpCaseString) Do
  If Ord(UpCaseString[i]) in [224..255] Then
   s:=s+Chr(Ord(UpCaseString[i])-32)
  Else
   s:=s+UpCaseString[i];
 UpCaseStr:=s;
End;

function TSearchResult.BuildParam(Param: string; DefOp: TOpKind): string;
var
  i,j,sks: integer;
  s: string;
  IzSKS,Minus: boolean;
  IsNOT,bNOT,IsWhole: boolean;

begin
  // Заменяем большое количество пробелов на один}
  i:=1; s:=Param;
  repeat
    Inc(i);
    if (s[i]=' ') and (s[i-1]=' ')
      then begin
        Dec(i);
        Delete(s,i,1);
      end;
  until i=Length(s);

  s:=trim(s);
  i:=0;
  repeat
    Inc(i);
    if ((s[i]=TOpKindSymbols[opAnd]) or (s[i]=TOpKindSymbols[opOr])) and (s[i-1]=' ')
      then begin
        Dec(i);
        Delete(s,i,1);
      end;
    if ((s[i]=TOpKindSymbols[opAnd]) or (s[i]=TOpKindSymbols[opOr])) and (s[i+1]=' ')
      then begin
        Inc(i);
        Delete(s,i,1);
        Dec(i);
      end;
  until i=Length(s);

  // Проверяем оператор поиска целого слова
  i:=0;
  sks:=0;
  IsWhole:=false;
  repeat
    Inc(i);
    if (s[i]=TOpKindSymbols[opWhole]) or
       (isWholeChecked and
        not IsWhole and
        not (s[i] in ['(',')',' ',TOpKindSymbols[opAnd],
                      TOpKindSymbols[opOr],TOpKindSymbols[opNot]]))
      then begin
        isWhole:=true;
        if s[i]=TOpKindSymbols[opWhole]
          then Delete(s,i,1);
        Insert('[[:<:]]',s,i); // start of whole word
        inc(i, 6);
      end;
    if isWhole and ((s[i] in ['(',')',' ',TOpKindSymbols[opAnd],
                             TOpKindSymbols[opOr],TOpKindSymbols[opNot]])
       or (i=length(s)))
      then begin
        isWhole:=false;
        if i=length(s)
          then begin
            s:=s+'[[:>:]]';
            inc(i);
          end
          else Insert('[[:>:]]',s,i); // end of whole word
        inc(i, 6);
      end;
  until i>=Length(s);

  // Добавляем скобки
  i:=0;
  repeat
    Inc(i);
    if (s[i]=TOpKindSymbols[opOr]) or (s[i]=TOpKindSymbols[opAnd]) or (s[i]=' ')
      then begin
        j:=i;
        sks:=0;
        IzSKS:=False;
        Minus:=False;
        repeat
          Dec(j);
          if s[j]=')'
            then Inc(sks);
          if s[j]='('
            then Dec(sks);
          if sks<>0
            then IzSKS:=True;
          if sks<0
            then Minus:=True;
        until ((sks=0) and (IzSKS=True)) or (j=1) or
              ((s[j]=TOpKindSymbols[opOr]) and (sks=0)) or ((s[j]=TOpKindSymbols[opAnd]) and (sks=0)) or
              ((s[j]=' ') and (sks=0)) or (Minus=True);
        if (((s[j-1]<>'(') and (j>1)) or ((s[j]<>'(') and (j=1)) or
           ((j=1) and (sks=0) and (IzSKS=True))) and (Minus=False)
          then begin
            if s[j] in [' ',TOpKindSymbols[opAnd],TOpKindSymbols[opOr]]
              then Insert('(',s,j+1)
              else Insert('(',s,j);
            Inc(i);
          end;
        j:=i;
        sks:=0;
        IzSKS:=False;
        Minus:=False;
        repeat
          Inc(j);
          if s[j]='('
            then Inc(sks);
          if s[j]=')'
            then Dec(sks);
          if sks<>0
            then IzSKS:=True;
          if sks<0
            then Minus:=True;
        until ((sks=0) and (IzSKS=True)) or (j=Length(s)) or
              ((s[j]=TOpKindSymbols[opOr]) and (sks=0)) or
              ((s[j]=TOpKindSymbols[opAnd]) and (sks=0)) or
              ((s[j]=' ') and (sks=0)) or (Minus=True);
        if (((s[j+1]<>')') and (j<Length(s))) or ((s[j]<>')') and (j=Length(s))) or
           ((j=Length(s)) and (sks=0) and (IzSKS=True))) and (Minus=False)
          then
            if s[j] in [' ',TOpKindSymbols[opAnd],TOpKindSymbols[opOr]]
              then Insert(')',s,j)
              else Insert(')',s,j+1);
      End;
  until i=Length(s);

  // Вносим отрицание под скобки
  i:=0;
  sks:=0;
  IsNOT:=false;
  repeat
    Inc(i);
    if (s[i]=TOpKindSymbols[opNot]) and (s[i+1]='(') and (IsNOT=false)
      then begin
        IsNOT:=true;
        Delete(s,i,1);
        Inc(sks);
      end;
    if (s[i]='(') and (IsNOT=true)
      then Inc(sks);
    if (s[i]=')') and (IsNOT=true)
      then Dec(sks);
    if (s[i] in ['(',')',' ',TOpKindSymbols[opAnd],TOpKindSymbols[opOr]]) and (IsNOT=true)
      then bNOT:=true;
    if (s[i]<>'(') and (s[i]<>')') and (s[i]<>' ') and
       (s[i]<>TOpKindSymbols[opAnd]) and (s[i]<>TOpKindSymbols[opOr]) and (bNOT=true) and (IsNOT=true)
      then begin
        bNOT:=false;
        Insert(TOpKindSymbols[opNot],s,i);
      end;
    if (IsNOT=true) and (sks=0)
      then IsNOT:=false;
  until i=Length(s);

{Пробелы заменяються на оператор по умолчанию}
  for i:=1 to Length(s)
    do begin
      if (s[i]=' ') and (DefOp=opAnd)
        then begin
          Insert(TOpKindSymbols[opAnd],s,i);
          Delete(s,i+1,1);
        end;
      if (s[i]=' ') and (DefOp=opOr)
        then begin
          Insert(TOpKindSymbols[opOr],s,i);
          Delete(s,i+1,1);
        end;
    end;
{Подчёркивание заменяем на пробелы}
  for i:=1 to Length(s)
    do
      if s[i]='_'
        then begin
          Insert(' ',s,i);
          Delete(s,i+1,1);
        end;
  BuildParam:=s;
end;


procedure TSearchResult.FindIt(Query: string; ALastPos, ACount: integer; ATitlesIdList: string; ACaseSensitive: boolean);
var
   st, SQLs: string;
   i, ResultTXTNum: integer;

  function RemovePlace(ASrc:string):string;
  var CommaFlag: boolean;
      j: integer;
  begin
    Result:='';
    CommaFlag:=false;
    for j:=1 to length(ASrc) do
      if CommaFlag
        then if Result=' '
               then Result:=ASrc[j]
               else Result:=Result+ASrc[j]
        else if (not CommaFlag) and (ASrc[j]=',')
               then CommaFlag:=true;
  end;

begin
  st:='';
  SQLs:='';
  st:=BuildParam(Query, DefOperation);
//  ShowMessage(st);
  i:=0;
  repeat
    Inc(i);
    if (st[i]<>')') and (st[i]<>'(') and (st[i]<>TOpKindSymbols[opAnd]) and
       (st[i]<>TOpKindSymbols[opOr])
      then begin
        if (i>1) and (st[i] in TOpSet) and (st[i-1]=')')
          then { case }
               if st[i] in [TOpKindSymbols[opAnd],
                            TOpKindSymbols[opNot],
                            TOpKindSymbols[opWhole]]
                  then SQLs:=SQLs+' and '
                  else
               if  st[i] in [TOpKindSymbols[opOr]]
                  then SQLs:=SQLs+' or '
                  else SQLs:=SQLs+st[i];
        if ACaseSensitive
          then begin
            if st[i]=TOpKindSymbols[opNot]
              then SQLs:=SQLs+'CHAPTER NOT REGEXP BINARY "'
              else SQLs:=SQLs+'CHAPTER REGEXP BINARY "';
          end
          else begin
            if st[i]=TOpKindSymbols[opNot]
              then SQLs:=SQLs+'CHAPTER NOT REGEXP "'
              else SQLs:=SQLs+'CHAPTER REGEXP "';
          end;
        while(st[i]<>')') and (st[i]<>'(') and (st[i]<>TOpKindSymbols[opAnd]) and
             (st[i]<>TOpKindSymbols[opOr]) and (i<=Length(st))
          do begin
            if st[i]<>TOpKindSymbols[opNot]
              then SQLs:=SQLs+st[i];
            Inc(i);
          end;
        Dec(i);
        SQLs:=SQLs+'"';
      end
      else begin
         if st[i] in [TOpKindSymbols[opAnd]]
            then SQLs:=SQLs+' and '
            else
         if  st[i] in [TOpKindSymbols[opOr]]
            then SQLs:=SQLs+' or '
            else SQLs:=SQLs+st[i];
      end;
  until i=Length(st);

  if ALastPos=0
    then begin
      Data.Query1.Sql.Clear;
      Data.Query1.Sql.Text:='select count(d.ID) as QTYFOUND';
      Data.Query1.Sql.Add(' from data d');
      Data.Query1.Sql.Add(' where ');
      if MainForm.BSearch.Down
        then Data.Query1.Sql.Add(' d.ISBIBLE=1 and')
        else Data.Query1.Sql.Add(' d.ISBIBLE=0 and');
      Data.Query1.Sql.Add(' ('+SQLs + ') ' + ATitlesIdList);
      Data.Query1.Sql.Add(' limit '+IntToStr(ALastPos)+','+IntToStr(ACount));
      Data.Query1.Open;
      LastFoundChaptersCount:=Data.Query1.FieldByName('QTYFOUND').AsInteger;
      Data.Query1.Close;
    end;

  Data.Query1.Sql.Clear;
  Data.Query1.Sql.Text:='select d.ID, d.ISBIBLE, d.CNUM, d.CHAPTER, t.PYEAR, t.PMONTH, t.PDAY, t.PLENGTH, t.PTIME, t.ETITLE, t.TITLE, d.TITLEID, t.PLACE, n.COMMENT as NOTES, n.ID as NOTEID';
  Data.Query1.Sql.Add(' from data d');
  Data.Query1.Sql.Add('  left join titles t on d.TITLEID=t.ID');
  Data.Query1.Sql.Add('  left join notes n on (d.TITLEID=n.TITLEID and d.CNUM=n.CNUM)');
  Data.Query1.Sql.Add(' where ');
  if MainForm.BSearch.Down
    then Data.Query1.Sql.Add(' d.ISBIBLE=1 and')
    else Data.Query1.Sql.Add(' d.ISBIBLE=0 and');
  Data.Query1.Sql.Add(' ('+SQLs + ') ' + ATitlesIdList);
  if MainForm.BSearch.Down
    then Data.Query1.Sql.Add(' order by t.PLENGTH, t.ID, d.CNUM')
    else Data.Query1.Sql.Add(' order by d.PYEAR desc, d.PMONTH desc, d.PDAY desc, d.PTIME desc, d.CNUM asc');
  Data.Query1.Sql.Add(' limit '+IntToStr(ALastPos)+','+IntToStr(ACount));
  Data.Query1.Open;
  RecCount:=Data.Query1.RecordCount;
  Data.Query1.First;
  ResultTXTNum:=0;
  SetLength(ResultTXT,ResultTXTNum);
  while not Data.Query1.Eof
    do begin
      Inc(ResultTXTNum);
      SetLength(ResultTXT,ResultTXTNum);
      ResultTXT[ResultTXTNum-1].ID:=Data.Query1.FieldByName('ID').AsInteger;
      ResultTXT[ResultTXTNum-1].ISBIBLE:=Data.Query1.FieldByName('ISBIBLE').AsInteger>0;
      ResultTXT[ResultTXTNum-1].CNUM:=Data.Query1.FieldByName('CNUM').AsInteger;
      ResultTXT[ResultTXTNum-1].ETITLE:=Data.Query1.FieldByName('ETITLE').AsString;
      ResultTXT[ResultTXTNum-1].TITLEID:=Data.Query1.FieldByName('TITLEID').AsInteger;
      ResultTXT[ResultTXTNum-1].Abzac:=Data.Query1.FieldByName('CHAPTER').AsString;
      ResultTXT[ResultTXTNum-1].Notes:=Data.Query1.FieldByName('NOTES').AsString;
      if ResultTXT[ResultTXTNum-1].ISBIBLE
        then ResultTXT[ResultTXTNum-1].Title:=Format('%s:%d',
                                              [Data.Query1.FieldByName('TITLE').AsString,
                                               ResultTXT[ResultTXTNum-1].CNUM])
        else ResultTXT[ResultTXTNum-1].Title:=Format('%s - %s%s.%s.%s %s',
                                              [Data.Query1.FieldByName('TITLE').AsString,
                                               Times[Data.Query1.FieldByName('PTIME').AsInteger],
                                               LZero(Data.Query1.FieldByName('PDAY').AsInteger),
                                               LZero(Data.Query1.FieldByName('PMONTH').AsInteger),
                                               Data.Query1.FieldByName('PYEAR').AsString,
                                               RemovePlace(Data.Query1.FieldByName('PLACE').AsString) ]);
      ResultTXT[ResultTXTNum-1].NoteId:=Data.Query1.FieldByName('NOTEID').AsInteger;
      Data.Query1.Next;
    end;
  Data.Query1.Close;
end;

procedure TSearchResult.OpenContext(ATITLEID: integer);
var
   SQLs: string;
   ResultTXTNum: integer;

  function RemovePlace(ASrc:string):string;
  var CommaFlag: boolean;
      j: integer;
  begin
    Result:='';
    CommaFlag:=false;
    for j:=1 to length(ASrc) do
      if CommaFlag
        then if Result=' '
               then Result:=ASrc[j]
               else Result:=Result+ASrc[j]
        else if (not CommaFlag) and (ASrc[j]=',')
               then CommaFlag:=true;
  end;

begin
  SQLs:='';
  if Data.Query1.Active then Data.Query1.Close;
  Data.Query1.Sql.Clear;
  Data.Query1.Sql.Text:='select d.ID, d.ISBIBLE, d.CNUM, d.CHAPTER, t.PYEAR, t.PMONTH, t.PDAY, t.PTIME, t.ETITLE, t.TITLE, d.TITLEID, t.PLACE, n.COMMENT as NOTES, n.ID as NOTEID';
  Data.Query1.Sql.Add(' from data d');
  Data.Query1.Sql.Add('  left join titles t on t.ID=d.TITLEID');
  Data.Query1.Sql.Add('  left join notes n on n.TITLEID=d.TITLEID and n.CNUM=d.CNUM');
  Data.Query1.Sql.Add(' where d.TITLEID='+IntToStr(ATITLEID));
  Data.Query1.Sql.Add(' order by d.CNUM');

  Data.Query1.Open;
  ResultTXTNum:=0;
  while not Data.Query1.Eof do
    begin
      Inc(ResultTXTNum);
      SetLength(ResultTXT,ResultTXTNum);
      ResultTXT[ResultTXTNum-1].ID:=Data.Query1.FieldByName('ID').AsInteger;
      ResultTXT[ResultTXTNum-1].ISBIBLE:=Data.Query1.FieldByName('ISBIBLE').AsInteger>0;
      ResultTXT[ResultTXTNum-1].CNUM:=Data.Query1.FieldByName('CNUM').AsInteger;
      ResultTXT[ResultTXTNum-1].ETITLE:=Data.Query1.FieldByName('ETITLE').AsString;
      ResultTXT[ResultTXTNum-1].TITLEID:=Data.Query1.FieldByName('TITLEID').AsInteger;
      ResultTXT[ResultTXTNum-1].Abzac:=Data.Query1.FieldByName('CHAPTER').AsString;
      ResultTXT[ResultTXTNum-1].Notes:=Data.Query1.FieldByName('NOTES').AsString;
      if ResultTXT[ResultTXTNum-1].ISBIBLE
        then ResultTXT[ResultTXTNum-1].Title:=Format('%s:%d',
                                              [Data.Query1.FieldByName('TITLE').AsString,
                                               ResultTXT[ResultTXTNum-1].CNUM])
        else ResultTXT[ResultTXTNum-1].Title:=Format('%s - %s%s.%s.%s %s',
                                              [Data.Query1.FieldByName('TITLE').AsString,
                                               Times[Data.Query1.FieldByName('PTIME').AsInteger],
                                               LZero(Data.Query1.FieldByName('PDAY').AsInteger),
                                               LZero(Data.Query1.FieldByName('PMONTH').AsInteger),
                                               Data.Query1.FieldByName('PYEAR').AsString,
                                               RemovePlace(Data.Query1.FieldByName('PLACE').AsString) ]);
      ResultTXT[ResultTXTNum-1].NoteId:=Data.Query1.FieldByName('NOTEID').AsInteger;
      Data.Query1.Next;
    end;
  Data.Query1.Close;
end;

function TSearchResult.HighResultTXT: Integer;
begin
  HighResultTXT:=High(ResultTXT);
end;

function TSearchResult.ReadInf(NumInf: integer): TResultRec;
begin
  ReadInf:=ResultTXT[NumInf];
end;

procedure TData.DataModuleDestroy(Sender: TObject);
begin
  SetLength(tmpParamArray, 0);
end;

end.
