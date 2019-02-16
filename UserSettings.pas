unit UserSettings;

interface

uses Graphics, Classes, SysUtils, IniFiles;

type

  TUserFontStyles = (fsSermon,fsSong,fsBible,fsTree,fsprnSermon,fsprnSong,fsprnBible);

  TItemSettings = record
    FntName  : TFontName;
    FntSize  : smallint;
    FntColor : TColor;
    FntStyle : TFontStyles;
  end;

//  PItemSettings = ^TItemSettings;

  TUserSettings = class(TObject)
  private
    AList: array[TUserFontStyles] of TItemSettings;
//    FChanged: boolean;

    function  GetItem(Ind: TUserFontStyles): TItemSettings;
//    function  GetChangedState: boolean;
//    procedure SetChangedState(NewState: boolean);
//    function  GetCount: smallint;
//    procedure SetCount(NewValue: smallint);

{    procedure SetFntName(Ind: smallint; NewValue: TFontName);
    procedure SetFntSize(Ind: smallint; NewValue: smallint);
    procedure SetFntColor(Ind: smallint; NewValue: TColor);
    procedure SetFntStyle(Ind: smallint; NewValue: TFontStyles);
    procedure SetBgColor(Ind: smallint; NewValue: TColor);
    procedure SetBandHeight(Ind: smallint; NewValue: smallint);
    procedure SetFrameColor(Ind: smallint; NewValue: TColor);
    procedure SetFrameWidth(Ind: smallint; NewValue: smallint);
    procedure SetFrameTop(Ind: smallint; NewValue: boolean);
    procedure SetFrameRight(Ind: smallint; NewValue: boolean);
    procedure SetFrameLeft(Ind: smallint; NewValue: boolean);
    procedure SetFrameBottom(Ind: smallint; NewValue: boolean); }
  public
//    ColumnSpace: single;   // - расстояние между колонками в распечатке.

    constructor Create;
//    destructor  Destroy; override;
    procedure   Clear;
//    procedure   SaveToStream(var AStream: TMemoryStream);
//    procedure   LoadFromStream(AStream: TMemoryStream);
//    procedure   SaveToFile(FName: string);
//    procedure   LoadFromFile(FName: string);
{    procedure   SaveToIniFile(FName: string);
    procedure   LoadFromIniFile(FName: string);
    function    StylesToString(Ind: smallint): string;
    function    StringToStyles(AStr: string): TFontStyles; }

    property Item[Ind: TUserFontStyles]: TItemSettings read GetItem; default;
//    property Changed: boolean read GetChangedState write SetChangedState;
//    property Count: smallint read GetCount write SetCount;

{    property FntName[Ind: smallint]: TFontName write SetFntName;
    property FntSize[Ind: smallint]: smallint write SetFntSize;
    property FntColor[Ind: smallint]: TColor write SetFntColor;
    property FntStyle[Ind: smallint]: TFontStyles write SetFntStyle;
    property BgColor[Ind: smallint]: TColor write SetBgColor;
    property BandHeight[Ind: smallint]: smallint write SetBandHeight;
    property FrameColor[Ind: smallint]: TColor write SetFrameColor;
    property FrameWidth[Ind: smallint]: smallint write SetFrameWidth;
    property FrameTop[Ind: smallint]: boolean write SetFrameTop;
    property FrameRight[Ind: smallint]: boolean write SetFrameRight;
    property FrameLeft[Ind: smallint]: boolean write SetFrameLeft;
    property FrameBottom[Ind: smallint]: boolean write SetFrameBottom; }

{    property FntName[Ind: smallint]: TFontName read GetFntName write SetFntName;
    property FntSize[Ind: smallint]: smallint read GetFntSize write SetFntSize;
    property FntColor[Ind: smallint]: TColor read GetFntColor write SetFntColor;
    property FntStyle[Ind: smallint]: TFontStyles read GetFntStyle write SetFntStyle;
    property BgColor[Ind: smallint]: TColor read GetBgColor write SetBgColor;
    property BandHeight[Ind: smallint]: smallint read GetBandHeight write SetBandHeight;
    property FrameColor[Ind: smallint]: TColor read GetFrameColor write SetFrameColor;
    property FrameWidth[Ind: smallint]: smallint read GetFrameWidth write SetFrameWidth;
    property FrameTop[Ind: smallint]: boolean read GetFrameTop write SetFrameTop;
    property FrameRight[Ind: smallint]: boolean read GetFrameRight write SetFrameRight;
    property FrameLeft[Ind: smallint]: boolean read GetFrameLeft write SetFrameLeft;
    property FrameBottom[Ind: smallint]: boolean read GetFrameBottom write SetFrameBottom;}
  end;

const
  DefItem: TItemSettings =
    ( FntName     : 'Arial';
      FntSize     : 9;
      FntColor    : clBlack;
      FntStyle    : [] );

implementation

//------------------------------------------------------------------------------
// Конструктор объекта
constructor TUserSettings.Create;
begin
  inherited Create;
  Clear;
//  ColumnSpace:=2;
end;
{
//------------------------------------------------------------------------------
// Деструктор объекта
destructor TUserSettings.Destroy;
begin
  Clear;
  inherited Destroy;
end;
}
//------------------------------------------------------------------------------
// Устанавливает все настройки в Default.
procedure TUserSettings.Clear;
var i: TUserFontStyles;
begin
  for i:=Low(TUserFontStyles) to High(TUserFontStyles) do
    AList[i]:=DefItem;
end;

//------------------------------------------------------------------------------
//
function TUserSettings.GetItem(Ind: TUserFontStyles): TItemSettings;
begin Result:=AList[Ind] end;
{
//------------------------------------------------------------------------------
// Все процедуры для того, что бы можно было менять значения.
procedure TUserSettings.SetFntName(Ind: smallint; NewValue: TFontName);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].FntName:=NewValue;
      FChanged:=true;
    end;
end;
//------------------------------------------------------------------------------
procedure TUserSettings.SetFntSize(Ind: smallint; NewValue: smallint);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].FntSize:=NewValue;
      FChanged:=true;
    end;
end;
//------------------------------------------------------------------------------
procedure TUserSettings.SetFntColor(Ind: smallint; NewValue: TColor);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].FntColor:=NewValue;
      FChanged:=true;
    end;
end;
//------------------------------------------------------------------------------
procedure TUserSettings.SetFntStyle(Ind: smallint; NewValue: TFontStyles);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].FntStyle:=NewValue;
      FChanged:=true;
    end;
end;
//------------------------------------------------------------------------------
procedure TUserSettings.SetBgColor(Ind: smallint; NewValue: TColor);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].BgColor:=NewValue;
      FChanged:=true;
    end;
end;
//------------------------------------------------------------------------------
procedure TUserSettings.SetBandHeight(Ind: smallint; NewValue: smallint);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].BandHeight:=NewValue;
      FChanged:=true;
    end;
end;
//------------------------------------------------------------------------------
procedure TUserSettings.SetFrameColor(Ind: smallint; NewValue: TColor);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].FrameColor:=NewValue;
      FChanged:=true;
    end;
end;
//------------------------------------------------------------------------------
procedure TUserSettings.SetFrameWidth(Ind: smallint; NewValue: smallint);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].FrameWidth:=NewValue;
      FChanged:=true;
    end;
end;
//------------------------------------------------------------------------------
procedure TUserSettings.SetFrameTop(Ind: smallint; NewValue: boolean);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].FrameTop:=NewValue;
      FChanged:=true;
    end;
end;
//------------------------------------------------------------------------------
procedure TUserSettings.SetFrameRight(Ind: smallint; NewValue: boolean);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].FrameRight:=NewValue;
      FChanged:=true;
    end;
end;
//------------------------------------------------------------------------------
procedure TUserSettings.SetFrameLeft(Ind: smallint; NewValue: boolean);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].FrameLeft:=NewValue;
      FChanged:=true;
    end;
end;
//------------------------------------------------------------------------------
procedure TUserSettings.SetFrameBottom(Ind: smallint; NewValue: boolean);
begin
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      AList[Ind].FrameBottom:=NewValue;
      FChanged:=true;
    end;
end;

//------------------------------------------------------------------------------
//
function TUserSettings.GetChangedState: boolean;
begin Result:=FChanged; end;

//------------------------------------------------------------------------------
//
procedure TUserSettings.SetChangedState(NewState: boolean);
begin FChanged:=NewState; end;

//------------------------------------------------------------------------------
//
function TUserSettings.GetCount: smallint;
begin Result:=High(AList)+1; end;

//------------------------------------------------------------------------------
//
procedure TUserSettings.SetCount(NewValue: smallint);
var i, LastCount: smallint;
begin
  if NewValue<>High(AList)+1
    then begin
      FChanged:=True;
      if NewValue<High(AList)+1
        then SetLength(AList, NewValue)
        else begin
          LastCount:=High(AList)+1;
          SetLength(AList, NewValue);
          for i:=LastCount to NewValue-1 do
           AList[i]:=DefItem;
        end;
    end;
end;

//------------------------------------------------------------------------------
//
function TUserSettings.StylesToString(Ind: smallint): string;
begin
  Result:='';
  if (Ind>=0) and (Ind<=High(AList))
    then begin
      if fsBold in AList[Ind].FntStyle
       then Result:='1'
       else Result:='0';
     if fsItalic in AList[Ind].FntStyle
       then Result:=Result + '1'
       else Result:=Result + '0';
     if fsUnderline in AList[Ind].FntStyle
       then Result:=Result + '1'
       else Result:=Result + '0';
     if fsStrikeOut in AList[Ind].FntStyle
       then Result:=Result + '1'
       else Result:=Result + '0';
    end;
end;

//------------------------------------------------------------------------------
//
function TUserSettings.StringToStyles(AStr: string): TFontStyles;
begin
  Result:=[];
  if (Length(AStr)>0) and (AStr[1]='1')
    then Result:=Result + [fsBold];
  if (Length(AStr)>1) and (AStr[2]='1')
    then Result:=Result + [fsItalic];
  if (Length(AStr)>2) and (AStr[3]='1')
    then Result:=Result + [fsUnderline];
  if (Length(AStr)>3) and (AStr[4]='1')
    then Result:=Result + [fsStrikeOut];
end;

//------------------------------------------------------------------------------
//
(*procedure TUserSettings.SaveToStream(var AStream: TMemoryStream);
var i: smallint;

    procedure WriteToStream(const AStr: string);
    var tmp: PChar;
        Cnt: integer;
    begin
      Cnt:=length(AStr);
      GetMem(tmp, Cnt+1);
      StrPCopy(tmp, AStr);
      AStream.Write(Cnt, sizeof(integer));
      AStream.Write(tmp^, Cnt);
      FreeMem(tmp);
    end;

begin
//  AStream.Write(ColumnSpace, sizeof(single));
  i:=Count;
  AStream.Write(i, sizeof(smallint));
  for i:=0 to High(AList) do
   begin
     WriteToStream(AList[i].FntName);
     AStream.Write(AList[i].FntSize, sizeof(smallint));
     AStream.Write(AList[i].FntColor, sizeof(TColor));
     WriteToStream(StylesToString(i));
     AStream.Write(AList[i].BgColor, sizeof(TColor));
     AStream.Write(AList[i].BandHeight, sizeof(smallint));
     AStream.Write(AList[i].FrameColor, sizeof(TColor));
     AStream.Write(AList[i].FrameWidth, sizeof(smallint));
     AStream.Write(AList[i].FrameTop, sizeof(boolean));
     AStream.Write(AList[i].FrameRight, sizeof(boolean));
     AStream.Write(AList[i].FrameLeft, sizeof(boolean));
     AStream.Write(AList[i].FrameBottom, sizeof(boolean));
   end;
end;

//------------------------------------------------------------------------------
//
procedure TUserSettings.LoadFromStream(AStream: TMemoryStream);
var i: smallint;
    tmpStr: string;

  function ReadFromStream: string;
  var Cnt: smallint;
      tmp: PChar;
  begin
    if AStream.Position<AStream.Size
      then begin
        AStream.Read(Cnt, sizeof(smallint));
        GetMem(tmp, Cnt+1);
        FillChar(tmp^, Cnt+1, 0);
        AStream.Read(tmp^, Cnt);
        Result:=StrPas(tmp);
        FreeMem(tmp);
      end
      else Result:=DefItem.FntName;
  end;

begin
//  AStream.Read(ColumnSpace, sizeof(single));
  AStream.Read(i, sizeof(smallint));
  Count:=i;
  for i:=0 to High(AList) do
   begin
     AList[i].FntName:=ReadFromStream;
     AStream.Read(AList[i].FntSize, sizeof(smallint));
     AStream.Read(AList[i].FntColor, sizeof(TColor));
     tmpStr:=ReadFromStream;
     AList[i].FntStyle:=[];
     if (Length(tmpStr)>0) and (tmpStr[1]='1')
       then AList[i].FntStyle:=AList[i].FntStyle + [fsBold];
     if (Length(tmpStr)>1) and (tmpStr[2]='1')
       then AList[i].FntStyle:=AList[i].FntStyle + [fsItalic];
     if (Length(tmpStr)>2) and (tmpStr[3]='1')
       then AList[i].FntStyle:=AList[i].FntStyle + [fsUnderline];
     if (Length(tmpStr)>3) and (tmpStr[4]='1')
       then AList[i].FntStyle:=AList[i].FntStyle + [fsStrikeOut];
     AStream.Read(AList[i].BgColor, sizeof(TColor));
     AStream.Read(AList[i].BandHeight, sizeof(smallint));
     AStream.Read(AList[i].FrameColor, sizeof(TColor));
     AStream.Read(AList[i].FrameWidth, sizeof(smallint));
     AStream.Read(AList[i].FrameTop, sizeof(boolean));
     AStream.Read(AList[i].FrameRight, sizeof(boolean));
     AStream.Read(AList[i].FrameLeft, sizeof(boolean));
     AStream.Read(AList[i].FrameBottom, sizeof(boolean));
   end;
end;

//------------------------------------------------------------------------------
//
procedure TUserSettings.SaveToFile(FName: string);
var S: TMemoryStream;
begin
  S:=TMemoryStream.Create;
  S.Clear;
  SaveToStream(S);
  S.Position:=0;
  S.SaveToFile(FName);
  S.Free;
end;

//------------------------------------------------------------------------------
//
procedure TUserSettings.LoadFromFile(FName: string);
var S: TMemoryStream;
begin
  Clear;
  S:=TMemoryStream.Create;
  S.Clear;
  S.LoadFromFile(FName);
  S.Position:=0;
  LoadFromStream(S);
  S.Free;
end; *)

//------------------------------------------------------------------------------
//
procedure TUserSettings.SaveToIniFile(FName: string);
var AppIni: TIniFile;
    i: smallint;

    procedure SaveSection(SectionName: string; Ind: smallint);
    begin
      AppIni.WriteString(SectionName, 'FONTNAME', AList[Ind].FntName);
      AppIni.WriteInteger(SectionName, 'FONTSIZE', AList[Ind].FntSize);
      AppIni.WriteInteger(SectionName, 'FONTCOLOR', AList[Ind].FntColor);
      AppIni.WriteString(SectionName, 'FONTSTYLE', StylesToString(Ind));
      AppIni.WriteInteger(SectionName, 'BGCOLOR', AList[Ind].BgColor);
      AppIni.WriteInteger(SectionName, 'BANDHEIGHT', AList[Ind].BandHeight);
      AppIni.WriteInteger(SectionName, 'FRAMECOLOR', AList[Ind].FrameColor);
      AppIni.WriteInteger(SectionName, 'FRAMEWIDTH', AList[Ind].FrameWidth);
      AppIni.WriteBool(SectionName, 'FRAMETOP', AList[Ind].FrameTop);
      AppIni.WriteBool(SectionName, 'FRAMERIGHT', AList[Ind].FrameRight);
      AppIni.WriteBool(SectionName, 'FRAMELEFT', AList[Ind].FrameLeft);
      AppIni.WriteBool(SectionName, 'FRAMEBOTTOM', AList[Ind].FrameBottom);
    end;

begin
  // Сохранение информации в ini файле.
  AppIni:=TIniFile.Create(FName);
  try
    AppIni.WriteString('COMMON', 'HEADERSCOUNT', IntToStr(Count));
    for i:=0 to High(AList)-1 do
      SaveSection('HEADER'+IntToStr(i+1), i);
    SaveSection('TEXT', High(AList));

    // Удаление лишних разделов из TIniFile (если они там есть).
    i:=High(AList) + 1;
    while AppIni.SectionExists('HEADER'+IntToStr(i)) do
     begin
       AppIni.EraseSection('HEADER'+IntToStr(i));
       Inc(i);
     end;

    AppIni.UpdateFile;
   finally
    AppIni.Free;
  end;
end;

//------------------------------------------------------------------------------
//
procedure TUserSettings.LoadFromIniFile(FName: string);
var AppIni: TIniFile;
    i: smallint;

    procedure LoadSection(SectionName: string; Ind: smallint);
    begin
      AList[Ind].FntName:=AppIni.ReadString(SectionName, 'FONTNAME', DefItem.FntName);
      AList[Ind].FntSize:=AppIni.ReadInteger(SectionName, 'FONTSIZE', DefItem.FntSize);
      AList[Ind].FntColor:=AppIni.ReadInteger(SectionName, 'FONTCOLOR', DefItem.FntColor);
      AList[Ind].FntStyle:=StringToStyles(AppIni.ReadString(SectionName, 'FONTSTYLE', '0000'));
      AList[Ind].BgColor:=AppIni.ReadInteger(SectionName, 'BGCOLOR', DefItem.BgColor);
      AList[Ind].BandHeight:=AppIni.ReadInteger(SectionName, 'BANDHEIGHT', DefItem.BandHeight);
      AList[Ind].FrameColor:=AppIni.ReadInteger(SectionName, 'FRAMECOLOR', DefItem.FrameColor);
      AList[Ind].FrameWidth:=AppIni.ReadInteger(SectionName, 'FRAMEWIDTH', DefItem.FrameWidth);
      AList[Ind].FrameTop:=AppIni.ReadBool(SectionName, 'FRAMETOP', DefItem.FrameTop);
      AList[Ind].FrameRight:=AppIni.ReadBool(SectionName, 'FRAMERIGHT', DefItem.FrameRight);
      AList[Ind].FrameLeft:=AppIni.ReadBool(SectionName, 'FRAMELEFT', DefItem.FrameLeft);
      AList[Ind].FrameBottom:=AppIni.ReadBool(SectionName, 'FRAMEBOTTOM', DefItem.FrameBottom);
    end;

begin
  // Загрузка информации (если она была в ini файле).
  AppIni:=TIniFile.Create(FName);
  try
    if AppIni.SectionExists('COMMON')
      then begin
        Count:=AppIni.ReadInteger('COMMON', 'HEADERSCOUNT', 0);
        for i:=0 to High(AList)-1 do
          if AppIni.SectionExists('HEADER'+IntToStr(i+1))
            then LoadSection('HEADER'+IntToStr(i+1), i);
        if (High(AList)>0) and (AppIni.SectionExists('TEXT'))
          then LoadSection('TEXT', High(AList));
      end;
    finally
   AppIni.Free;
   FChanged:=false;
  end;
end;           }

end.

