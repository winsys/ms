{$A+,B-,C+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$D-}   { Make it D+ for debug information }
(************************************************************************
 * Compctrl.PAS V3.52     This source is for Delphi 3.0 ONLY            *
 *                                                                      *
 * Legal stuff:                                                         *
 *  Copyright (c) 1995-97 South Pacific Information Services Ltd        *
 *                                                                      *
 * This source code is provided for your inspection and adaptation.     *
 * While you are free to on-sell applications which make use            *
 * of the source or components based on it, you may NOT:                *
 * a) Use this source to make general data compression components       *
 *   (DCUs, DLLs, OBJS etc) for resale to other developers              *
 * b) On-sell this source code as part of any product                   *
 *                                                                      *
 * If you do not accept these restrictions, please contact              *
 * South Pacific Information Services Ltd for a full and immediate      *
 * refund.                                                              *
 *                                                                      *
 * Other than that -- have fun!                                         *
 ************************************************************************
 V3.x amendment history:
    October 14, '97 -- 3.52 released (single mod to reflect Delphi 3.01 change }
    June 6, '97 -- 3.04 released (missing CField property restored for Delphi 3)
    May 27 '97 -- 3.03 released ("too many blobs open" fixed }
    May '97 -- 3.01 released
*)
unit Compctrl;

interface

uses Windows, SysUtils, Messages, Classes, Controls, Forms,
     Graphics, Menus, StdCtrls, ExtCtrls, Mask, Buttons, ComCtrls, Db, Compress,
     Dbctrls, DbTables;

type

  TCBlobField = class(TBlobField)
  private
    Ftransliterate : Boolean;
    FCompressSource: TCompress;
    FCompressionMethod: TCompressionMethod;
    function GetBlobType: TBlobType;
    procedure LoadFromBlob(Blob: TBlobField);
    procedure LoadFromBitmap(Bitmap: TBitmap);
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToBitmap(Bitmap: TBitmap);
    procedure SaveToStrings(Strings: TStrings);
    procedure SetBlobType(Value: TBlobType);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetBlobSize: Integer; override;  { new 3.01 }
    function GetIsNull: Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
    procedure SetText(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    class function IsBlob: Boolean; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SetFieldType(Value: TFieldType); override;
    property BlobSize: Integer read GetBlobSize;
    property Value: string read GetAsString write SetAsString;
    property Transliterate: Boolean read FTransliterate write FTransliterate;
  published
    property BlobType: TBlobType read GetBlobType write SetBlobType;
    property Size default 0;
    property CompressSource: TCompress Read FCompressSource Write FCompressSource;
    property CompressionMethod: TCompressionMethod Read FCompressionmethod Write FCompressionMethod;
  end;

  { TCMemoField }

  TCMemoField = class(TCBlobField)
  public
    constructor Create(AOwner: TComponent); override;
  public
    property Transliterate default True;
  end;

  { TCGraphicField }

  TCGraphicField = class(TCBlobField)
  public
    constructor Create(AOwner: TComponent); override;
  end;


{ TCDBMemo }

  TCDBMemo = class(TCustomMemo)
  private
{**} FCField: TCMemoField;
{**} FCompressSource: TCompress;
{**} FCompressionMethod: TCompressionMethod;
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FFocused: Boolean;
    FMemoLoaded: Boolean;
    FPaintControl: TPaintControl;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
{**}procedure CheckSetCompressedField;
{**}procedure SetCompressionMethod(value: TCompressionMethod);
{**}procedure SetCompressSource(value: TCompress);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;


  protected
    procedure Change; override;
{**}property CompressedField: TCMemoField read FCField Write FCField;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
{**}procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadMemo;
    property Field: TField read GetField;
    property CField: TCMemoField read FCField; { v3.04 }
  published
    property Align;
    property Alignment;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BorderStyle;
    property Color;

{**}property CompressionMethod: TCompressionMethod Read FCompressionMethod Write SetCompressionMethod;
{**}property CompressSource: TCompress Read FCompressSource Write SetCompressSource;

    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantTabs;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{ TCDBImage }

  TCDBImage = class(TCustomControl)
  private
{**} FCField: TCGraphicField;
{**} FCompressSource: TCompress;
{**} FCompressionMethod: TCompressionMethod;
    FDataLink: TFieldDataLink;
    FPicture: TPicture;
    FBorderStyle: TBorderStyle;
    FAutoDisplay: Boolean;
    FStretch: Boolean;
    FCenter: Boolean;
    FPictureLoaded: Boolean;
    FQuickDraw: Boolean;

{**}procedure CheckSetCompressedField;

    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure PictureChanged(Sender: TObject);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCenter(Value: Boolean);

{**}procedure SetCompressionMethod(value: TCompressionMethod);
{**}procedure SetCompressSource(value: TCompress);

    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetPicture(Value: TPicture);
    procedure SetReadOnly(Value: Boolean);
    procedure SetStretch(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
{**}property CompressedField: TCGraphicField read FCField Write FCField;
    function GetPalette: HPALETTE; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
{**}procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure LoadPicture;
    procedure PasteFromClipboard;
    property Field: TField read GetField;
    property CField: TCGraphicField read FCField; { v3.04 }
    property Picture: TPicture read FPicture write SetPicture;
  published
    property Align;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Center: Boolean read FCenter write SetCenter default True;
    property Color;

{**}property CompressionMethod: TCompressionMethod Read FCompressionMethod Write SetCompressionMethod;
{**}property CompressSource: TCompress Read FCompressSource Write SetCompressSource;

    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property QuickDraw: Boolean read FQuickDraw write FQuickDraw default True;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TCBlobStream = class(TStream)
  private
     FsaveTransliterateMode: Boolean;
     FOpened: Boolean;
     FField: TCBlobField;
     FModified: Boolean;
     FBlobStream: TBlobStream;
     FMemoryStream: TMemoryStream;
  protected
  public
     property BlobStream: TBlobStream read FBlobStream;
     constructor Create(Field: TCBlobField; Mode: TBlobstreamMode);
     destructor Destroy; Override;
     function Read(var Buffer; Count: Longint): Longint; Override;
     function Write(const Buffer; Count: Longint): Longint; Override;
     function Seek(Offset: Longint; Origin: Word): Longint; Override;
     procedure Truncate;
  end;

{ TCDBRichEdit }

  TCDBRichEdit = class(TCustomRichEdit)
  private
{**} FCField: TCMemoField;
{**} FCompressSource: TCompress;
{**} FCompressionMethod: TCompressionMethod;
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FFocused: Boolean;
    FMemoLoaded: Boolean;
    FDataSave: string;
    procedure BeginEditing;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
{**}procedure CheckSetCompressedField;
{**}procedure SetCompressionMethod(value: TCompressionMethod);
{**}procedure SetCompressSource(value: TCompress);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; override;
{**}property CompressedField: TCMemoField read FCField Write FCField;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
{**}procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadMemo;
    property Field: TField read GetField;
    property CField: TCMemoField read FCField; { v3.04 }
  published
    property Align;
    property Alignment;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BorderStyle;
    property Color;

{**}property CompressionMethod: TCompressionMethod Read FCompressionMethod Write SetCompressionMethod;
{**}property CompressSource: TCompress Read FCompressSource Write SetCompressSource;

    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResizeRequest;
    property OnSelectionChange;
    property OnProtectChange;
    property OnSaveClipboard;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses Clipbrd, DBConsts, Dialogs;

{ TCBlobStream }

constructor TCBlobStream.Create(Field: TCBlobField; Mode: TBlobstreamMode);
begin
   FField := Field;
   FSaveTransliterateMode := False; { default no action }
   if FField.FTransliterate then
   begin
     FSaveTransliterateMode := True;
     FField.FTransliterate := False; { for now }
   end;

  FMemoryStream := TMemoryStream.create;
  FBlobStream := TBlobStream.create(Field,Mode);

  if Mode=bmRead then { fill the buffer initially }
  begin
      if assigned(FField.FCompressSource) then
         FField.FCompressSource.Expand(FMemoryStream,FBlobStream)
      else
         FMemoryStream.CopyFrom(FBlobStream,0); { got it? }
  end else
    FMemoryStream.SetSize(FField.DataSize); { preset wot they will write to us... }

  FOpened := True;
  FMemoryStream.Position:=0;
  FBlobStream.Position := 0;
{ At this moment we have either a full (expanded) memory stream, or an empty
  (ready to be writ) Memorystream allegedly the same size as the original object }
end;

destructor TCBlobStream.Destroy;
begin
  if FModified then { we must Compress the result! }
  begin
    FMemoryStream.Position := 0;
    FBlobStream.Position := 0;
    if assigned(FField.FCompressSource) then
       FField.FCompressSource.Compress(FBlobStream,FMemoryStream,
            FField.CompressionMethod)
    else
       FBlobStream.CopyFrom(FMemoryStream,0); { retrieve it... }
  end; { FModified }
  if FSaveTransliterateMode then { did some fudging }
    FField.FTransliterate := True; { original setting }

  FMemoryStream.destroy;
  FField.Modified := False; { V3.03 moved from AFTER below to avoid too many blobs open!
                             (was originally added avoid 'Blob not open' error) }
  FBlobStream.destroy;
end;

function TCBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Fopened then
    Result := FMemoryStream.Read(Buffer,Count)
  else
    Result := 0;
end;

function TCBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Fopened then
  begin
    Result := FMemoryStream.Write(Buffer,Count);
    FModified := True
  end else
    Result := 0;
end;

function TCBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if Fopened then
    Result := FMemoryStream.Seek(Offset,Origin)
  else
    Result := 0;
end;

procedure TCBlobStream.Truncate;
begin
  if FOpened then
  begin
    FMemorystream.setSize(Fmemorystream.Position);
    FModified := True;
  end;
end;


{ TCBlobField }

constructor TCBlobField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftBlob);
end;

procedure TCBlobField.Assign(Source: TPersistent);
begin
  if Source is TCBlobField then
  begin
    LoadFromBlob(TCBlobField(Source));
    Exit;
  end;
  if Source is TStrings then
  begin
    LoadFromStrings(TStrings(Source));
    Exit;
  end;
  if Source is TBitmap then
  begin
    LoadFromBitmap(TBitmap(Source));
    Exit;
  end;
  if (Source is TPicture) and (TPicture(Source).Graphic is TBitmap) then
  begin
    LoadFromBitmap(TBitmap(TPicture(Source).Graphic));
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TCBlobField.AssignTo(Dest: TPersistent);
begin
  if Dest is TStrings then
  begin
    SaveToStrings(TStrings(Dest));
    Exit;
  end;
  if Dest is TBitmap then
  begin
    SaveToBitmap(TBitmap(Dest));
    Exit;
  end;
  if Dest is TPicture then
  begin
    SaveToBitmap(TPicture(Dest).Bitmap);
    Exit;
  end;
  inherited AssignTo(Dest);
end;

procedure TCBlobField.Clear; { 3.0 ok as-is -- regular Blobstream ok to clear it }
begin
  DataSet.CreateBlobStream(Self, bmWrite).Free;
end;

function TCBlobField.GetAsString: string;
var
  Len: Integer;
begin
  with TCBlobStream.Create(Self, bmRead) do
    try
      Len := Size;
      SetString(Result, nil, Len);
      ReadBuffer(Pointer(Result)^, Len);
    finally
      Free;
    end;
end;

function TCBlobField.GetAsVariant: Variant;    { altered 3.52 }
begin
  result := GetAsString;
end;

function TCBlobField.GetBlobSize: Integer;   { new 3.01 }
begin
  with TCBlobStream.Create(Self, bmRead) do
    try
      Result := Size;
    finally
      Free;
    end;
end;

function TCBlobField.GetBlobType: TBlobType;
begin
  Result := TBlobType(DataType);
end;

function TCBlobField.GetIsNull: Boolean; { 3.0 -- should be ok as is }
begin
  if Modified then
  begin
    with DataSet.CreateBlobStream(Self, bmRead) do { regular Blobstream since just size 0 check }
    try
      Result := (Size = 0);
    finally
      Free;
    end;
  end else
    Result := inherited GetIsNull;
end;

procedure TCBlobField.GetText(var Text: string; DisplayText: Boolean);
begin
  Text := inherited GetAsString;
end;

class function TCBlobField.IsBlob: Boolean;
begin
  Result := True;
end;

{ Paradox graphic BLOB header  -- excerpted from DB.PAS -- beware of changes! }

type
  TGraphicHeader = record
    Count: Word;                { Fixed at 1 }
    HType: Word;                { Fixed at $0100 }
    Size: Longint;              { Size not including header }
  end;

procedure TCBlobField.LoadFromBitmap(Bitmap: TBitmap);
var
  BlobStream: TStream;
  Header: TGraphicHeader;
begin  { wants to be DataSet.CreateBlobstream 3.0 }
  BlobStream := TCBlobStream.Create(Self, bmWrite);
  try
    if (DataType = ftGraphic) or (Datatype = ftTypedBinary) then
    begin
      Header.Count := 1;
      Header.HType := $0100;
      Header.Size := 0;
      BlobStream.Write(Header, SizeOf(Header));
      Bitmap.SaveToStream(BlobStream);
      Header.Size := BlobStream.position - SizeOf(Header);
      BlobStream.position := 0;
      BlobStream.Write(Header, SizeOf(Header));
    end else
      Bitmap.SaveToStream(BlobStream);
  finally
    BlobStream.Free;
  end;
end;

procedure TCBlobField.LoadFromBlob(Blob: TBlobField);
var
  BlobStream: TStream;
begin
  BlobStream := TCBlobStream.Create(Self, bmWrite);
  try
    Blob.SaveToStream(BlobStream);
  finally
    BlobStream.Free;
  end;
end;

procedure TCBlobField.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCBlobField.LoadFromStream(Stream: TStream);
begin
  with TCBlobStream.Create(Self, bmWrite) do
  try
    CopyFrom(Stream, 0);
  finally
    Free;
  end;
end;

procedure TCBlobField.LoadFromStrings(Strings: TStrings);
var
  BlobStream: TStream;
begin
  BlobStream := TCBlobStream.Create(Self, bmWrite);
  try
    Strings.SaveToStream(BlobStream);
  finally
    BlobStream.Free;
  end;
end;

procedure TCBlobField.SaveToBitmap(Bitmap: TBitmap);
var
  BlobStream: TStream;
  Size: Longint;
  Header: TGraphicHeader;
begin
  BlobStream := TCBlobStream.Create(Self, bmRead);
  try
    Size := BlobStream.Size;
    if Size >= SizeOf(TGraphicHeader) then
    begin
      BlobStream.Read(Header, SizeOf(Header));
      if (Header.Count <> 1) or (Header.HType <> $0100) or
        (Header.Size <> Size - SizeOf(Header)) then
        BlobStream.Position := 0;
    end;
     Bitmap.LoadFromStream(BlobStream)
  finally
    BlobStream.Free;
  end;
end;

procedure TCBlobField.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCBlobField.SaveToStream(Stream: TStream);
var
  BlobStream: TStream;
begin
  BlobStream := TCBlobStream.Create(Self, bmRead);
  try
    Stream.CopyFrom(BlobStream, 0);
  finally
    BlobStream.Free;
  end;
end;

procedure TCBlobField.SaveToStrings(Strings: TStrings);
var
  BlobStream: TStream;
begin
  BlobStream := TCBlobStream.Create(Self, bmRead);
  try
    Strings.LoadFromStream(BlobStream);
  finally
    BlobStream.Free;
  end;
end;

procedure TCBlobField.SetAsString(const Value: string);
begin
  with TCBlobStream.Create(Self, bmWrite) do
    try
      WriteBuffer(Pointer(Value)^, Length(Value));
    finally
      Free;
    end;
end;

procedure TCBlobField.SetBlobType(Value: TBlobType);
begin
  SetFieldType(Value);
end;

procedure TCBlobField.SetFieldType(Value: TFieldType);
begin
  if Value in [Low(TBlobType)..High(TBlobType)] then SetDataType(Value);
end;

procedure TCBlobField.SetText(const Value: string);
begin
  raise AccessError('Text'); { raise added 3.01 }
end;

procedure TCBlobField.SetVarValue(const Value: Variant);
begin
  SetAsString(Value);
end;

{ TMemoField }

constructor TCMemoField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftMemo);
  Transliterate := True;
end;


{ TGraphicField }

constructor TCGraphicField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftGraphic);
end;

{ Common routines used by the three visual components below... }

{ Free our field ONLY if we are sure it is not linked in a dataset somewhere }
procedure FreeIfOk(field:TCBlobField; Datasource: TDatasource);
begin
  if Field=nil then exit; { no need }
  if Field.Dataset=nil then
  begin
    if Datasource=nil then exit;
    if Datasource.dataset = nil then exit;
    if Field.name<>'' then exit;
   { above whoa -- it got assigned a dataset (hence the name), which must now
                                              have deleted the original field??? }
  end;
  if (DataSource<>nil) and (Datasource.Dataset<>nil) and
     (DataSource.DataSet.FindField(Field.Fieldname) is TCBlobfield) then exit;
  Field.free; { can only free if table field list doesn't contain IT }
end;

{ Make a VALID component name from the field name passed to us }
function fixName(s: string): string;
var count: Integer;
begin
  Result:='';
  for count :=1 to Length(s) do
     if (s[count] in ['A'..'Z','a'..'z','_']) or
        ((count>1) and (s[count] in ['0'..'9'] ) ) then
        Result := Result + s[count];

  if (Result ='') or (Result[1] in ['0'..'9']) then
    Result :='_'+Result;
end;


{ TCDBMemo }

constructor TCDBMemo.Create(AOwner: TComponent);
const registered: Boolean = False;
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FAutoDisplay := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FPaintControl := TPaintControl.Create(Self,'EDIT');
  if not registered then
  begin
    registered := True;
    RegisterClass(TCMemoField);
  end;
end;

destructor TCDBMemo.Destroy;
begin

  FreeIfOk(FCField,DataSource); { if not nil  }
  FCField := nil;

  FPaintControl.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TCDBMemo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then DataSource := nil
    else if (AComponent=CompressSource) then CompressSource:=nil; { which forces FCField check }
  end;
end;

procedure TCDBMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FMemoLoaded then
  begin
    if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
      FDataLink.Edit;
  end;
end;

procedure TCDBMemo.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FMemoLoaded then
  begin
    if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
      not FDataLink.Field.IsValidChar(Key) then
    begin
      MessageBeep(0);
      Key := #0;
    end;
    case Key of
      ^H, ^I, ^J, ^M, ^V, ^X, #32..#255:
        FDataLink.Edit;
      #27:
        FDataLink.Reset;
    end;
  end else
  begin
    if Key = #13 then LoadMemo;
    Key := #0;
  end;
end;

procedure TCDBMemo.Change;
begin
  if FMemoLoaded then FDataLink.Modified;
  FMemoLoaded := True;
  inherited Change;
end;

function TCDBMemo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCDBMemo.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  CheckSetCompressedField;
end;

function TCDBMemo.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TCDBMemo.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
  CheckSetCompressedField;
end;

function TCDBMemo.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCDBMemo.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TCDBMemo.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCDBMemo.LoadMemo;
begin
  if not FMemoLoaded and Assigned(FDataLink.Field) and (FDataLink.Field.isBlob) then { safe for CDB too }
  begin
    try
      if CompressedField<>nil then
         Lines.Text := CompressedField.asString
      else
        Lines.Text := FDataLink.Field.AsString;
      FMemoLoaded := True;
    except
      { Memo too large }
      on E:EInvalidOperation do
        Lines.Text := Format('(%s)', [E.Message]);
    end;
    EditingChange(Self);
  end;
end;

procedure TCDBMemo.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    if FDataLink.Field.isBlob then
    begin
      if FAutoDisplay or (FDataLink.Editing and FMemoLoaded) then
      begin
        FMemoLoaded := False;
        LoadMemo;
      end else
      begin
        Text := Format('(%s)', [FDataLink.Field.DisplayLabel]);
        FMemoLoaded := False;
      end;
    end else
    begin
      if FFocused and FDataLink.CanModify then
        Text := FDataLink.Field.Text
      else
        Text := FDataLink.Field.DisplayText;
      FMemoLoaded := True;
    end
  else
  begin
    if csDesigning in ComponentState then Text := Name else Text := '';
    FMemoLoaded := False;
  end;
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
end;

procedure TCDBMemo.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not (FDataLink.Editing and FMemoLoaded);
end;

procedure TCDBMemo.UpdateData(Sender: TObject);
begin
   if CompressedField<>nil then
      CompressedField.AsString := Text
   else
      FDataLink.Field.AsString := Text;
end;

procedure TCDBMemo.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if not Assigned(FDataLink.Field) or not FDataLink.Field.IsBlob then
      FDataLink.Reset;
  end;
end;

procedure TCDBMemo.WndProc(var Message: TMessage);
begin
  with Message do
    if (Msg = WM_CREATE) or (Msg = WM_WINDOWPOSCHANGED) or
      (Msg = CM_FONTCHANGED) then FPaintControl.DestroyHandle;
  inherited;
end;

procedure TCDBMemo.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then { V3.01 }
    inherited ReadOnly := False;
end;

procedure TCDBMemo.CMExit(var Message: TCMExit);
begin
  if not (FDataLink.Field is TBlobField) then
    try
      FDataLink.UpdateRecord;
    except
      SetFocus;
      raise;
    end;
  SetFocused(False);
  inherited;
end;

procedure TCDBMemo.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadMemo;
  end;
end;

procedure TCDBMemo.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FMemoLoaded then LoadMemo else inherited;
end;

procedure TCDBMemo.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TCDBMemo.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TCDBMemo.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TCDBMemo.WMPaint(var Message: TWMPaint);
var
  S: string;
begin
  if not (csPaintCopy in ControlState) then inherited else
  begin
    if FDataLink.Field <> nil then
      if FDataLink.Field.isBlob then
      begin
        if FAutoDisplay then { V3.01 }
        begin
          if compressedField<> nil then
            S := AdjustLineBreaks(CompressedField.AsString)
          else
            S := AdjustLineBreaks(FDataLink.Field.AsString)
        end else
          S := Format('(%s)', [FDataLink.Field.DisplayLabel]);
      end else
        S := FDataLink.Field.DisplayText;
    SendMessage(FPaintControl.Handle, WM_SETTEXT, 0, Integer(PChar(S)));
    SendMessage(FPaintControl.Handle, WM_ERASEBKGND, Message.DC, 0);
    SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
  end;
end;

{ added routines: }

procedure TCDBMemo.SetCompressSource(value: TCompress);
begin
  FCompressSource:= Value;
  if FCField<>nil then
     FCField.CompressSource:=Value;
end;

procedure TCDBMemo.SetCompressionMethod(value: TCompressionMethod);
begin
  FCompressionMethod:= Value;
  if FCField<>nil then
     FCField.CompressionMethod := Value;
end;

procedure TCDBMemo.CheckSetCompressedField;
var tmpfield: TField;
    where: Smallint;
begin
  if (DataField<>'') and (Datasource<>nil) and (Datasource.Dataset<>nil) and (CompressSource<>nil) then
  begin
    tmpField:=DataSource.Dataset.FindField(DataField);
    if tmpField is TCMemoField then
    begin
      FCField :=TCMemoField(tmpfield);
      exit
    end;        { else nil or regular blob field... }

    if FCField = nil then
      FCField:= TCMemoField.Create(Self.Owner);
    FCField.CompressSource:=CompressSource;
    FCField.CompressionMethod:=CompressionMethod;
    if (AnsiCompareText(FCField.FieldName,DataField)<>0) or (FCField.DataSet<>DataSource.DataSet) then
    try
     DataSource.Dataset.fielddefs.update;
     where :=DataSource.Dataset.fielddefs.IndexOf(DataField);
     if where < 0 then { oops }
     begin
       showmessage('Can''t find field: '+DataField);
       DataField:=''; { which forces another call here and FREEs FCField... }
       exit;
     end;
     if tmpfield<>nil then
     begin
        showmessage('TCompress warning: '+DataSource.DataSet.name+' already has a non-compressed'+#13+
         'field called '''+DataField+'''.'+#13+#13+
         'Double-click on '+DataSource.DataSet.name+', remove the existing field, and try again.');
         DataField:=''; { which forces another call here and FREEs FCField... }
          exit;
     end;
     FCField.Size := DataSource.Dataset.fielddefs[where].Size;
     FCField.FieldName:=DataField; { must be before DataSet assignment }
     FCField.DataSet :=Datasource.Dataset;
     FCField.Name:=Self.Name+fixName(DataField);
    except on EDatabaseError do
     begin
       FreeIfOk(FCField,DataSource);
       FCField := nil;
       raise;
     end;
    end;
  end else { sorry boys... }
  begin
    FreeIfOk(FCField,DataSource);
    FCField := nil;
  end;
end;

procedure TCDBMemo.Loaded;
begin
  inherited Loaded;
  CheckSetCompressedField;
  if (csDesigning in ComponentState) then DataChange(Self); { V3.01 }
end;

{ TCDBImage }

constructor TCDBImage.Create(AOwner: TComponent);
const registered: Boolean = False;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  Width := 105;
  Height := 105;
  TabStop := True;
  ParentColor := False;
  FPicture := TPicture.Create;
  FBorderStyle := bsSingle;
  FCenter := True;

  FPicture.OnChange := PictureChanged;
  FAutoDisplay := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FQuickDraw := True;
  if not registered then
  begin
    registered := True;
    RegisterClass(TCGraphicField);
  end;
end;

destructor TCDBImage.Destroy;
begin

  FreeIfOk(FCField,DataSource);
  FCField := nil;

  FPicture.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TCDBImage.Loaded;
begin
  inherited Loaded;
  CheckSetCompressedField;{}

end;

procedure TCDBImage.SetCompressSource(value: TCompress);
begin
  FCompressSource:= Value;
  if FCField<>nil then
     FCField.CompressSource := Value;
end;

procedure TCDBImage.SetCompressionMethod(value: TCompressionMethod);
begin
  FCompressionMethod:= Value;
  if FCField<>nil then
     FCField.CompressionMethod := Value;
end;

procedure TCDBImage.CheckSetCompressedField;
var tmpfield: TField;
    where: Smallint;
begin
  if (DataField<>'') and (Datasource<>nil) and (Datasource.Dataset<>nil) and (CompressSource<>nil) then
  begin
    tmpField:=DataSource.Dataset.FindField(DataField);
    if tmpField is TCGraphicField then
    begin
      FCField :=TCGraphicField(tmpfield);
      exit
    end;        { else nil or regular blob field... }

    if FCField = nil then
      FCField:= TCGraphicField.Create(Self.Owner);
    FCField.CompressSource:=CompressSource;
    FCField.CompressionMethod:=CompressionMethod;
    if (AnsiCompareText(FCField.FieldName,DataField)<>0) or (FCField.DataSet<>DataSource.DataSet) then
    try
     DataSource.Dataset.fielddefs.update;
     where :=DataSource.Dataset.fielddefs.IndexOf(DataField);
     if where < 0 then { oops }
     begin
       showmessage('Can''t find field: '+DataField);
       DataField:=''; { which forces another call here and FREEs FCField... }
       exit;
     end;
     if tmpfield<>nil then
     begin
        showmessage('TCompress warning: '+DataSource.DataSet.name+' already has a non-compressed'+#13+
         'field called '''+DataField+'''.'+#13+#13+
         'Double-click on '+DataSource.DataSet.name+', remove the existing field, and try again.');
         DataField:=''; { as above }
         exit;
     end;
     FCField.Size := DataSource.Dataset.fielddefs[where].Size;
     FCField.FieldName:=DataField; { must be before DataSet assignment }
     FCField.DataSet :=Datasource.Dataset;
     FCField.Name:=Self.Name+fixName(DataField);
    except on EDatabaseError do
     begin
       FreeIfOk(FCField,DataSource);
       FCField := nil;
       raise;
     end;
    end;
  end else { sorry boys... }
  begin
    FreeIfOk(FCField,DataSource);
    FCField := nil;
  end;
end;

procedure TCDBImage.LoadPicture;
begin
  if not FPictureLoaded then
  begin
    if CompressedField<>nil then
      CompressedField.AssignTo(Picture.BitMap)
    else if (not Assigned(FDataLink.Field) or
         FDataLink.Field.IsBlob) then
      Picture.Assign(FDataLink.Field);
  end;
end;

procedure TCDBImage.DataChange(Sender: TObject);
begin
  Picture.Graphic := nil;
  FPictureLoaded := False;
  if FAutodisplay then LoadPicture;
end;

procedure TCDBImage.UpdateData(Sender: TObject);
begin
  if CompressedField<>nil then
  begin
    with CompressedField do
      if Picture.Graphic is TBitmap then
        Assign(Picture.Graphic)
      else
        Clear;
  end else with TBlobField(FDataLink.Field) do
      if Picture.Graphic is TBitmap then
        Assign(Picture.Graphic)
      else
        Clear;
end;

function TCDBImage.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCDBImage.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  CheckSetCompressedField;
end;

function TCDBImage.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TCDBImage.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
  CheckSetCompressedField;
end;

function TCDBImage.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCDBImage.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TCDBImage.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCDBImage.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadPicture;
  end;
end;

procedure TCDBImage.PictureChanged(Sender: TObject);
begin
  if FPictureLoaded then FDataLink.Modified; { add test V3.01 }
  FPictureLoaded := True;
  Invalidate;
end;

procedure TCDBImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then DataSource := nil
    else if (AComponent=CompressSource) then CompressSource:=nil; { which forces FCField check }
  end;
end;

procedure TCDBImage.CopyToClipboard;
begin
  if Picture.Graphic <> nil then Clipboard.Assign(Picture);
end;

procedure TCDBImage.CutToClipboard;
begin
  if Picture.Graphic <> nil then
    if FDataLink.Edit then
    begin
      CopyToClipboard;
      Picture.Graphic := nil;
    end;
end;

procedure TCDBImage.PasteFromClipboard;
begin
  if Clipboard.HasFormat(CF_BITMAP) and FDataLink.Edit then
    Picture.BitMap.Assign(Clipboard);
end;

procedure TCDBImage.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    ^X: CutToClipBoard;
    ^C: CopyToClipBoard;
    ^V: PasteFromClipBoard;
    #13: LoadPicture;
    #27: FDataLink.Reset;
  end;
end;

procedure TCDBImage.Paint; { Rewritten V3.01 }
var
  Size: TSize;
  R: TRect;
  S: string;
  DrawPict: TPicture;
  Form: TCustomForm;
  Pal: HPalette;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    if FPictureLoaded or (csPaintCopy in ControlState) then
    begin
      DrawPict := TPicture.Create;
      Pal := 0;
      try
        if (csPaintCopy in ControlState) and
          Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
        begin
           if CompressedField<>nil then
             DrawPict.Assign(CompressedField)
           else
             DrawPict.Assign(FDataLink.Field);
          if DrawPict.Graphic is TBitmap then
            DrawPict.Bitmap.IgnorePalette := QuickDraw;
        end
        else
        begin
          DrawPict.Assign(Picture);
          if Focused and (DrawPict.Graphic <> nil) and (DrawPict.Graphic.Palette <> 0) then
          begin { Control has focus, so realize the bitmap palette in foreground }
            Pal := SelectPalette(Handle, DrawPict.Graphic.Palette, False);
            RealizePalette(Handle);
          end;
        end;
        if Stretch then
          if (DrawPict.Graphic = nil) or DrawPict.Graphic.Empty then
            FillRect(ClientRect)
          else
            StretchDraw(ClientRect, DrawPict.Graphic)
        else
        begin
          SetRect(R, 0, 0, DrawPict.Width, DrawPict.Height);
          if Center then OffsetRect(R, (ClientWidth - DrawPict.Width) div 2,
            (ClientHeight - DrawPict.Height) div 2);
          StretchDraw(R, DrawPict.Graphic);
          ExcludeClipRect(Handle, R.Left, R.Top, R.Right, R.Bottom);
          FillRect(ClientRect);
          SelectClipRgn(Handle, 0);
        end;
      finally
        if Pal <> 0 then SelectPalette(Handle, Pal, True);
        DrawPict.Free;
      end;
    end
    else begin
      Font := Self.Font;
      if FDataLink.Field <> nil then
        S := FDataLink.Field.DisplayLabel
      else S := Name;
      S := '(' + S + ')';
      Size := TextExtent(S);
      R := ClientRect;
      TextRect(R, (R.Right - Size.cx) div 2, (R.Bottom - Size.cy) div 2, S);
    end;
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.ActiveControl = Self) and
      not (csDesigning in ComponentState) and
      not (csPaintCopy in ControlState) then
    begin
      Brush.Color := clWindowFrame;
      FrameRect(ClientRect);
    end;
  end;
end;

procedure TCDBImage.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if not FPictureLoaded then Invalidate;
end;

procedure TCDBImage.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  LoadPicture;
  inherited;
end;

procedure TCDBImage.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TCDBImage.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCDBImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    Invalidate;
  end;
end;

procedure TCDBImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TCDBImage.SetStretch(Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Invalidate;
  end;
end;

procedure TCDBImage.CMEnter(var Message: TCMEnter);
begin
  Invalidate; { Draw the focus marker }
  inherited;
end;

procedure TCDBImage.CMExit(var Message: TCMExit);
begin
  try { added V3.01 }
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  Invalidate; { Erase the focus marker }
  inherited;
end;

procedure TCDBImage.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if TabStop and CanFocus then SetFocus;
  inherited;
end;

procedure TCDBImage.WMCut(var Message: TMessage);
begin
  CutToClipboard;
end;

procedure TCDBImage.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
end;

procedure TCDBImage.WMPaste(var Message: TMessage);
begin
  PasteFromClipboard;
end;

procedure TCDBImage.WMSize(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

function TCDBImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic is TBitmap then
    Result := TBitmap(FPicture.Graphic).Palette;
end;


procedure TCDBImage.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_INSERT:
      if ssShift in Shift then PasteFromClipBoard else
        if ssCtrl in Shift then CopyToClipBoard;
    VK_DELETE:
      if ssShift in Shift then CutToClipBoard;
  end;
end;


{ TCDBRichEdit }

constructor TCDBRichEdit.Create(AOwner: TComponent);
const registered: Boolean = False;
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  FAutoDisplay := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  if not registered then
  begin
    registered := True;
    RegisterClass(TCMemoField);
  end;
end;

destructor TCDBRichEdit.Destroy;
begin
  FreeIfOk(FCField,DataSource); { if not nil }
  FCField := nil;

  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TCDBRichEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then DataSource := nil
    else if (AComponent=CompressSource) then CompressSource:=nil; { which forces FCField check }
  end;
end;

procedure TCDBRichEdit.BeginEditing;
begin
  if not FDataLink.Editing then
  try
    if FDataLink.Field.IsBlob then
      FDataSave := FDataLink.Field.AsString;
    FDataLink.Edit;
  finally
    FDataSave := '';
  end;
end;

procedure TCDBRichEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FMemoLoaded then
  begin
    if (Key = VK_DELETE) or (Key = VK_BACK) or
      ((Key = VK_INSERT) and (ssShift in Shift)) or
      (((Key = Ord('V')) or (Key = Ord('X'))) and (ssCtrl in Shift)) then
      BeginEditing;
  end;
end;

procedure TCDBRichEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FMemoLoaded then
  begin
    if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
      not FDataLink.Field.IsValidChar(Key) then
    begin
      MessageBeep(0);
      Key := #0;
    end;
    case Key of
      ^H, ^I, ^J, ^M, ^V, ^X, #32..#255:
        BeginEditing;
      #27:
        FDataLink.Reset;
    end;
  end else
  begin
    if Key = #13 then LoadMemo;
    Key := #0;
  end;
end;

procedure TCDBRichEdit.Change;
begin
  if FMemoLoaded then FDataLink.Modified;
  FMemoLoaded := True;
  inherited Change;
end;

function TCDBRichEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCDBRichEdit.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  CheckSetCompressedField;
end;

function TCDBRichEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TCDBRichEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
  CheckSetCompressedField;
end;

function TCDBRichEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCDBRichEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TCDBRichEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCDBRichEdit.LoadMemo;
begin
  if not FMemoLoaded and Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
  begin
    try
      if CompressedField<>nil then
        Lines.Assign(CompressedField)
      else
        Lines.Assign(FDataLink.Field);
      FMemoLoaded := True;
    except
      { Rich Edit Load failure }
      on E:EOutOfResources do
        Lines.Text := Format('(%s)', [E.Message]);
    end;
    EditingChange(Self);
  end;
end;

procedure TCDBRichEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    if FDataLink.Field.IsBlob then
    begin
      if FAutoDisplay or (FDataLink.Editing and FMemoLoaded) then
      begin
        { Check if the data has changed since we read it the first time }
        if (FDataSave <> '') and (FDataSave = FDataLink.Field.AsString) then Exit;
        FMemoLoaded := False;
        LoadMemo;
      end else
      begin
        Text := Format('(%s)', [FDataLink.Field.DisplayLabel]);
        FMemoLoaded := False;
      end;
    end else
    begin
      if FFocused and FDataLink.CanModify then
        Text := FDataLink.Field.Text
      else
        Text := FDataLink.Field.DisplayText;
      FMemoLoaded := True;
    end
  else
  begin
    if csDesigning in ComponentState then Text := Name else Text := '';
    FMemoLoaded := False;
  end;
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
end;

procedure TCDBRichEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not (FDataLink.Editing and FMemoLoaded);
end;

procedure TCDBRichEdit.UpdateData(Sender: TObject);
begin
  if FDataLink.Field.IsBlob then
  begin
    if CompressedField<>nil then
       CompressedField.assign(Lines)
    else
       FDataLink.Field.Assign(Lines)
  end else
    FDataLink.Field.AsString := Text;

end;

procedure TCDBRichEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if not Assigned(FDataLink.Field) or not FDataLink.Field.IsBlob then
      FDataLink.Reset;
  end;
end;

procedure TCDBRichEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then { V3.01 }
    inherited ReadOnly := False;
end;

procedure TCDBRichEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetFocused(False);
  inherited;
end;

procedure TCDBRichEdit.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadMemo;
  end;
end;

procedure TCDBRichEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FMemoLoaded then LoadMemo else inherited;
end;

procedure TCDBRichEdit.WMCut(var Message: TMessage);
begin
  BeginEditing;
  inherited;
end;

procedure TCDBRichEdit.WMPaste(var Message: TMessage);
begin
  BeginEditing;
  inherited;
end;

procedure TCDBRichEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

{ added routines: }

procedure TCDBRichEdit.SetCompressSource(value: TCompress);
begin
  FCompressSource:= Value;
  if FCField<>nil then
     FCField.CompressSource:=Value;
end;

procedure TCDBRichEdit.SetCompressionMethod(value: TCompressionMethod);
begin
  FCompressionMethod:= Value;
  if FCField<>nil then
     FCField.CompressionMethod := Value;
end;

procedure TCDBRichEdit.CheckSetCompressedField;
var tmpfield: TField;
    where: Smallint;
begin
  if (DataField<>'') and (Datasource<>nil) and (Datasource.Dataset<>nil) and (CompressSource<>nil) then
  begin
    tmpField:=DataSource.Dataset.FindField(DataField);
    if tmpField is TCMemoField then
    begin
      FCField :=TCMemoField(tmpfield);
      exit
    end;        { else nil or regular blob field... }

    if FCField = nil then
      FCField:= TCMemoField.Create(Self.Owner);
    FCField.CompressSource:=CompressSource;
    FCField.CompressionMethod:=CompressionMethod;
    if (AnsiCompareText(FCField.FieldName,DataField)<>0) or (FCField.DataSet<>DataSource.DataSet) then
    try
     DataSource.Dataset.fielddefs.update;
     where :=DataSource.Dataset.fielddefs.IndexOf(DataField);
     if where < 0 then { oops }
     begin
       showmessage('Can''t find field: '+DataField);
       DataField:=''; { which forces another call here and FREEs FCField... }
       exit;
     end;
     if tmpfield<>nil then
     begin
        showmessage('TCompress warning: '+DataSource.DataSet.name+' already has a non-compressed'+#13+
         'field called '''+DataField+'''.'+#13+#13+
         'Double-click on '+DataSource.DataSet.name+', remove the existing field, and try again.');
         DataField:=''; { which forces another call here and FREEs FCField... }
          exit;
     end;
     FCField.Size := DataSource.Dataset.fielddefs[where].Size;
     FCField.FieldName:=DataField; { must be before DataSet assignment }
     FCField.DataSet :=Datasource.Dataset;
     FCField.Name:=Self.Name+fixName(DataField);
     if Datasource.DataSet is TBDEDataset then { V3.0 }
        TBDEDataset(Datasource.DataSet).CacheBlobs := False;

    except on EDatabaseError do
     begin
       FreeIfOk(FCField,DataSource);
       FCField := nil;
       raise;
     end;
    end;
  end else { sorry boys... }
  begin
    FreeIfOk(FCField,DataSource);
    FCField := nil;
  end;
end;

procedure TCDBRichEdit.Loaded;
begin
  inherited Loaded;
  CheckSetCompressedField;
  if (csDesigning in ComponentState) then DataChange(Self); { V3.01 }
end;

procedure Register;
begin
  RegisterComponents('Compress', [TCDBImage, TCDBMemo, TCDBRichEdit]);
end;

end.
