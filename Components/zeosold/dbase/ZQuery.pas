{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{               Abstract Dataset component               }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZQuery;

interface
                         
{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses
{$IFNDEF LINUX} Windows, {$IFDEF VER100} DbTables, {$ENDIF} {$ENDIF}
{$IFDEF VERCLX}Variants, FmtBcd,{$ENDIF}
  SysUtils, DB, Classes, ZDirSql, DBCommon, ZToken, ZSqlExtra,
  ZBlobStream, ZConnect, ZTransact,
  ZUpdateSql, ZParser, ZSqlTypes, ZSqlParser, ZSqlBuffer, ZSqlItems;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type

{$IFNDEF WINDOWS}
  TUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApplied);
{$ENDIF}

  { Supported databases }
  TZDatabaseType = TDatabaseType;

  { Link options }
  TZLinkOption = (loLinkRequery, loCascadeUpdate, loCascadeDelete, loAlwaysResync);
  TZLinkOptions = set of TZLinkOption;

  { General dataset options }
  TZDatasetOption = (doParamsAsIs, doHourGlass, doQueryAllRecords,
    doAutoFillDefs, doCalcDefault, doQuickOpen, doEnableAutoInc,
    doUseRowId, doCursorFetch, doSqlFilter, doRefreshAfterPost, doRefreshBeforeEdit);
  TZDatasetOptions = set of TZDatasetOption;

  { Update types }
  TUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind;
    var UpdateAction: TUpdateAction) of object;

  TZUpdateRecordTypes = ZSqlTypes.TZUpdateRecordTypes;

  { Progress event types }
  TZProgressStage = (psStarting, psRunning, psEnding);
  TZProgressProc = (ppFetching, ppClosing);
  TZProgressEvent = procedure (Sender: TObject; Stage: TZProgressStage;
    Proc: TZProgressProc; Position, Max: Integer; var Cancel: Boolean) of object;

  TZDataset = class;

  { Query datalink class }
  TZQueryDataLink = class(TDataLink)
  private
    FQuery: TZDataset;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(AQuery: TZDataset);
  end;

  { Abstract query with descendant of TDataSet }
  TZDataset = class(TDataSet)
  private
    { Internal fields }
    FAutoOpen: Boolean;
    FAutoStart: Boolean;
    FNewValueState: Boolean;
    FDatabaseType: TZDatabaseType;
    FVersion: Integer;
    { Query fields }
    FSqlParser: TSqlParser;
    FSqlBuffer: TSqlBuffer;
    FCacheBuffer: TSqlBuffer;
    FTableName: string;
    FDefaultIndex : Boolean;
    FRowsAffected: Integer;
    FRequestLive: Boolean;
    { Params fields }
    FParams: TParams;
    FParamCheck: Boolean;
    FMacroChar: Char;
    FMacros: TParams;
    FMacroCheck: Boolean;
    { Internal connections }
    FDatabase: TZDatabase;
    FTransact: TZTransact;
    FQuery: TDirQuery;
    { IndexDefs support }
    FIndexDefs: TIndexDefs;
    FIndexName: string;
    FFieldsIndex: Boolean;
    { Other fields }
    FCurRec: Integer;
    FOptions: TZDatasetOptions;
    FParser: TZParser;
    FCCParser: TZParser;
    FFiltered: Boolean;
    FFetchAll: Boolean;
    { Updates properties }
    FCachedUpdates: Boolean;
    FOnApplyUpdateError: TDataSetErrorEvent;
    FOnUpdateRecord: TUpdateRecordEvent;
    FUpdateObject: TZUpdateSql;
    { Master-detail properties }
    FLinkCheck:   Boolean;
    FLinkFields:  string;
    FLinkOptions: TZLinkOptions;
    FMasterIndex: Integer;
    FMasterFields: TFieldList;
    FMasterFieldCount: Integer;
    FMasterLink: TMasterDataLink;
    FDetailFields: array[0..MAX_FIELD_COUNT] of string;
    { Internal data-link support }
    FDataLink: TZQueryDataLink;
    { Other fields }
    FOnProgress: TZProgressEvent;

    { IndexDefs support methods }
    procedure SetIndexDefs(Value: TIndexDefs);
    function GetIndexName: string;
    procedure SetIndexName(const Value: string);
    function GetIndexFieldNames: string;
    procedure SetIndexFieldNames(const Value: string);
    function GetIndexFieldCount: Integer;
    function GetIndexField(Index: Integer): TField;
    procedure SetIndexField(Index: Integer; Value: TField);
    procedure SetIndex(const Value: string; FieldsIndex: Boolean);
    { Private property processing methods }
    function GetParamsCount: Word;
    function GetMacroCount: Word;
    function GetUpdatesPending: Boolean;
    function GetSql: TStrings;
    function GetReadOnly: Boolean;
    procedure SetParamsList(Value: TParams);
    procedure SetMacroChar(Value: Char);
    procedure SetMacros(Value: TParams);
    procedure SetSql(Value: TStrings);
    procedure SetTableName(Value: string);
    procedure SetReadOnly(Value: Boolean);
    procedure SetUpdateObject(Value: TZUpdateSql);
    function GetUpdateRecord: TZUpdateRecordTypes;
    procedure SetUpdateRecord(Value: TZUpdateRecordTypes);
    procedure SetOptions(Value: TZDatasetOptions);
  protected
    { Private methods for master-detail support }
    function GetLinkFields: string;
    function GetMasterDataSource: TDataSource;
    procedure SetLinkFields(Value: string);
    procedure SetMasterDataSource(Value: TDataSource);
    procedure UpdateLinkFields;
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure MasterCascade;
    procedure MasterRequery;
    function MasterStateCheck(Dataset: TDataset): Boolean;
    procedure MasterDefine;
    { Private methods for data-link support }
    function GetDataLinkSource: TDataSource;
    procedure SetDataLinkSource(Value: TDataSource);
    procedure ParamsRequery;
    procedure RefreshParams;
    { Private methods for getting fields }
    function CheckRecordByFilter(RecNo: LongInt): Boolean;
    procedure QueryRecords(Force: Boolean);
    procedure QueryOneRecord;
    { Other private methods }
    procedure ShortRefresh;
    function RefreshCurrentRow(RecordData: PRecordData): Boolean;
{$IFNDEF VER100}
    procedure ResetAggField(Field: TField); override;
    function GetAggregateValue(Field: TField): Variant; override;
{$ENDIF}
    procedure SqlFilterRefresh;
  protected
    { Wrapping main fields }
    property DatabaseObj: TZDatabase read FDatabase write FDatabase;
    property TransactObj: TZTransact read FTransact write FTransact;
    property Query: TDirQuery read FQuery write FQuery;
    property FetchAll: Boolean read FFetchAll write FFetchAll;
    property FilterMark: Boolean read FFiltered write FFiltered;
  protected
    { Private property processing methods }
    procedure SetDatabase(Value: TZDatabase);
    procedure SetTransact(Value: TZTransact);

    procedure ChangeAddBuffer(AddRecord: PRecordData); virtual;
    procedure CreateConnections; virtual;
    procedure FormSqlQuery(OldData, NewData: PRecordData); virtual;
    function FormatFieldsList(Value: string): string;
    function FormTableSqlOrder: string;
    procedure QueryRecord; virtual; abstract;
    { Methods for query fieds description }
    procedure DefineTableKeys(Table: string; Unique: Boolean;
      var FieldList: TFieldList; var FieldCount: Integer);
    function FormSqlWhere(Table: string; RecordData: PRecordData): string;
    { Methods for internal fields processing }
    function EvaluteDef(Value: string): string;
    { Overrided methods for fields processing }
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean):
      TGetResult; override;
    function GetRecordSize: Word; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;

    function AllocRecordBuffer: PChar; override;
    procedure CloseBlob(Field: TField); override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    { Overrided methods for implement general dataset operation }
    procedure Loaded; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalEdit; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalPost; override;
    procedure InternalDelete; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalUpdate;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalRefresh; override;
    procedure InternalHandleException; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    { Other internal methods }
    function InternalLocate(KeyFields: string; KeyValues: Variant;
      Options: TLocateOptions): LongInt;
    procedure InternalFormKeyValues(RecordData: PRecordData; Unique: Boolean;
      var KeyFields: string; var KeyValues: Variant);
    procedure InternalSort(Fields: string; SortType: TSortType);
    { Overrided methods for bookmarks processing }
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    { Overrided methods for searching and filtering }
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;
    { Overrided methods for navigation }
    function GetCanModify: Boolean; override;
    function GetRecNo: Integer; override;
    function GetRecordCount: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function IsCursorOpen: Boolean; override;
    { Methods for caching updates support }
    procedure ClearBuffer;
    procedure FlushBuffer; virtual;
    procedure CheckContraints;
    procedure UpdateAfterPost(OldData, NewData: PRecordData); virtual;
    procedure UpdateAfterInit(RecordData: PRecordData); virtual;
    procedure Flush;
    { Other protected methods }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoProgress(Stage: TZProgressStage; Proc: TZProgressProc;
      Position: Integer);
    {$IFNDEF VER100}
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    {$ENDIF}
    procedure AutoFillObjects;
    procedure InternalInitFieldDefs; override;
    procedure UpdateIndexDefs; override;
    procedure UpdateFieldDef(FieldDesc: PFieldDesc; var FieldType: TFieldType;
      var FieldSize: Integer); virtual;
    function ConvertToSqlEnc(Value: string): string;
    function ConvertFromSqlEnc(Value: string): string;
    function ValueToRowId(Value: string): TRowId; virtual;
    function RowIdToValue(Value: TRowId): string; virtual;
  protected
    {$IFDEF WITH_IPROVIDER}
    { Provider support }
    procedure PSEndTransaction(Commit: Boolean); override;
    function PSGetTableName: string; override;
    function PSGetQuoteChar: string; override;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
    function PSIsSqlBased: Boolean; override;
    function PSIsSqlSupported: Boolean; override;
    procedure PSStartTransaction; override;
    procedure PSReset; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
    { ADO support }
    procedure PSExecute; override;
    procedure PSGetAttributes(List: TList); override;
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetKeyFields: string; override;
    function PSGetParams: TParams; override;
    function PSGetIndexDefs(IndexTypes: TIndexOptions = [ixPrimary..ixNonMaintained]): TIndexDefs; override;
    procedure PSSetParams(AParams: TParams); override;
    {$ENDIF}

    { Hided properties }
    property TableName: string read FTableName write SetTableName;
    property RequestLive: Boolean read FRequestLive write FRequestLive;
    property DefaultIndex: Boolean read FDefaultIndex write FDefaultIndex;
    { Data-link support }
    property DataSource: TDataSource read GetDataLinkSource write SetDataLinkSource;
  public
    { Class constructors and destructors }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Special properties }
    property SqlParser: TSqlParser read FSqlParser;
    property SqlBuffer: TSqlBuffer read FSqlBuffer;
    property CacheBuffer: TSqlBuffer read FCacheBuffer;
    property CurRec: Integer read FCurRec write FCurRec;

    { Abstract method for implementing in descendant classes }
    procedure AddTableFields(Table: string; SqlFields: TSqlFields);
      virtual; abstract;
    procedure AddTableIndices(Table: string; SqlFields: TSqlFields;
      SqlIndices: TSqlIndices); virtual; abstract;
    function CheckTableExistence(Table: string): Boolean; virtual;
    { Extra functions }
    function StringToSql(Value: string): string;
    function ParamToSql(Value: Variant): string;
    function ValueToSql(Value: Variant): string;
    function ProcessIdent(Value: string): string; virtual;
    function FieldValueToSql(Value: string; FieldDesc: PFieldDesc): string; virtual;
    { Buffer support methods }
    procedure CopyRecord(SqlBuffer: TSqlBuffer; Source, Dest: PRecordData); virtual;
    procedure FreeRecord(SqlBuffer: TSqlBuffer; Value: PRecordData); virtual;

    { Public executing methods }
    procedure ExecSql; virtual;
    function  RowsAffected: LongInt;
    { Public methods for record sorting and searching }
    procedure SortInverse;
    procedure SortClear;
    procedure SortByField(Fields: string);
    procedure SortDescByField(Fields: string);
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    function IsSequenced: Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    { Public methods for caching updates support }
    procedure ApplyUpdates;
    procedure CommitUpdates;
    procedure CancelUpdates;
    procedure RevertRecord;
    function UpdateStatus: TUpdateStatus; {$IFNDEF VER100} override; {$ENDIF}
    { Public methods for params processing }
    function ParamByName(const Value: string): TParam;
    function MacroByName(const Value: string): TParam;
    { Public methods for blob processing }
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    { Other public methods }
    function GetActiveRecBuf(var Value: PRecordData): Boolean;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    { File storing data methods }
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    { IndexDefs support methods }
    procedure GetIndexNames(List: TStrings);
    procedure FormKeyValues(var KeyFields: string; var KeyValues: Variant); virtual;

    { Public properties }
    property UpdatesPending: Boolean read GetUpdatesPending;
    property Active;
    property Sql: TStrings read GetSql write SetSql;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property DatabaseType: TZDatabaseType read FDatabaseType write FDatabaseType;
    property AutoOpen: Boolean read FAutoOpen;
    { Sql parameter properties }
    property Params: TParams read FParams write SetParamsList stored False;
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    property ParamCount: Word read GetParamsCount;
    { Sql Macros properties }
    property Macros: TParams read FMacros write SetMacros;
    property MacroCheck: Boolean read FMacroCheck write FMacroCheck default True;
    property MacroCount: Word read GetMacroCount;
    property MacroChar: Char read FMacroChar write SetMacroChar default DEFAULT_MACRO_CHAR;
    { IndexDefs support properties }
    property IndexFieldCount: Integer read GetIndexFieldCount;
    property IndexFields[Index: Integer]: TField read GetIndexField write SetIndexField;
  published
    { General Sql properties }
    property Database: TZDatabase read FDatabase write SetDatabase;
    property Transaction: TZTransact read FTransact write SetTransact;
    property UpdateObject: TZUpdateSql read FUpdateObject write SetUpdateObject;
    property Version: Integer read FVersion;

    property CachedUpdates: Boolean read FCachedUpdates write FCachedUpdates;
    property ShowRecordTypes: TZUpdateRecordTypes read GetUpdateRecord
      write SetUpdateRecord;
    property Options: TZDatasetOptions read FOptions write SetOptions;
    { Master-detail support properties }
    property LinkFields: string read GetLinkFields write SetLinkFields;
    property LinkOptions: TZLinkOptions read FLinkOptions write FLinkOptions;
    property MasterSource: TDataSource read GetMasterDataSource
      write SetMasterDataSource;

    property FieldDefs stored False;
    property Constraints;

    { IndexDefs support properties }
    property IndexDefs: TIndexDefs read FIndexDefs write SetIndexDefs stored False;
    property IndexName: string read GetIndexName write SetIndexName;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
    { New data update events }
    property OnApplyUpdateError: TDataSetErrorEvent read FOnApplyUpdateError
      write FOnApplyUpdateError;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord
      write FOnUpdateRecord;
    { Progress event }
    property OnProgress: TZProgressEvent read FOnProgress write FOnProgress;
    { Inherited DataSet properties }
    property AutoCalcFields;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnPostError;
    property OnFilterRecord;
    property OnNewRecord;
    property Filter;
    property Filtered;
    property FilterOptions;
  end;

  TZBCDField = class(TBCDField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsCurrency: Currency; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
{$IFDEF VER130ABOVE}
    function GetDataSize: Integer; override;
{$ELSE}
    function GetDataSize: Word; override;
{$ENDIF}
   public
     constructor Create(AOwner: TComponent); override;
   published
     property Size default 4;
   end;

implementation

uses ZExtra, ZDBaseConst, ZList, ZConvert
{$IFNDEF LINUX}
  ,Forms{$IFNDEF NO_GUI}, Controls{$ENDIF}
{$ELSE}
  ,QForms{$IFNDEF NO_GUI}, QControls{$ENDIF}
{$ENDIF};

{*************** TZDataset class implemantation ****************}

{*** Class constructors and destructors ***}

{ Class constructor }
constructor TZDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Create internal objects }
  FParams := TParams.Create{$IFNDEF VER100}(Self){$ENDIF};
  FMacros := TParams.Create{$IFNDEF VER100}(Self){$ENDIF};
  MacroChar := DEFAULT_MACRO_CHAR;
  FSqlParser := TSqlParser.Create(Self);
  FSqlBuffer := TSqlBuffer.Create(Self);
  FCacheBuffer := TSqlBuffer.CreateCache(FSqlBuffer);
  FIndexDefs := TIndexDefs.Create(Self);
  FParser := TZParser.Create(nil);
  FCCParser := TZParser.Create(nil);
  FDataLink := TZQueryDataLink.Create(Self);

  { Initialize properties }
  FParamCheck := True;
  FMacroCheck := True;
  FOptions := [doHourGlass, doAutoFillDefs, doUseRowId];
  FDatabaseType := dtUnknown;
  BookmarkSize := SizeOf(Integer);
  FLinkOptions := [loAlwaysResync];

  { Master-detail links }
  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange  := MasterChanged;
  FMasterLink.OnMasterDisable := MasterDisabled;
  FMasterIndex := -1;
  FVersion := ZDBO_VERSION;
end;

{ Class destructor }
destructor TZDataset.Destroy;
begin
  FOnProgress := nil;
  if Assigned(FDatabase) then
    FDatabase.RemoveDataset(Self);
  inherited Destroy;

  FMacros.Free;
  FParams.Free;
  FSqlParser.Free;
  FCacheBuffer.Free;
  FSqlBuffer.Free;
  FIndexDefs.Free;
  FParser.Free;
  FCCParser.Free;
  FDataLink.Free;
  FMasterLink.Free;
  FQuery.Free;
end;

{*** Private property processing methods ***}

{ Set connect to database component }
procedure TZDataset.SetDatabase(Value: TZDatabase);
begin
  if FDatabase = Value then Exit;

  if Active then Close;
  try
    if Assigned(FDatabase) then
      FDatabase.RemoveDataset(Self);
    if Assigned(Value) then
    begin
      FQuery.Connect := Value.Handle;
      Value.AddDataset(Self);
      if (not Assigned(FTransact) or (FTransact.Database <> Value)) and
        not (csLoading in ComponentState) then
        SetTransact(TZTransact(Value.DefaultTransaction));
    end
    else
    begin
      FQuery.Connect := nil;
      if Assigned(FTransact) and Assigned(FTransact.Database) then
        SetTransact(nil);
    end;
  finally
    FDatabase := Value;
  end;
end;

{ Set connect to transaction component }
procedure TZDataset.SetTransact(Value: TZTransact);
begin
  if FTransact = Value then Exit;

  if Active then Close;
  FTransact := Value;
  if Assigned(FTransact) then
  begin
    FQuery.Transact := Value.Handle;
    if (FDatabase <> FTransact.Database)
      and not (csLoading in ComponentState) then
      FDatabase := FTransact.Database;
  end else
    FQuery.Transact := nil;
end;

{ Set update Sql object }
procedure TZDataset.SetUpdateObject(Value: TZUpdateSql);
begin
  if FUpdateObject <> Value then
  begin
    if Assigned(FUpdateObject) then
      FUpdateObject.Dataset := nil;
    FUpdateObject := Value;
    if Assigned(FUpdateObject) then
      FUpdateObject.Dataset := Self;
  end;
end;

{ Auto fill database and transaction objects }
procedure TZDataset.AutoFillObjects;
begin
  if Assigned(TransactObj) and not Assigned(TransactObj.Database) then
    TransactObj.Database := Database;
  if not Assigned(DatabaseObj) and Assigned(TransactObj) then
    DatabaseObj := TransactObj.Database;
  if Assigned(DatabaseObj) and not Assigned(TransactObj) then
    TransactObj := TZTransact(DatabaseObj.DefaultTransaction);
end;

{ Assign new table name value }
procedure TZDataset.SetTableName(Value: string);
begin
  if FTableName <> Value then
  begin
    Close;
    FTableName := Value;
    SqlParser.Sql.Clear;
    SqlParser.Sql.Add('SELECT * FROM ' + ProcessIdent(Value));
    SqlParser.ExtraOrderBy := FormTableSqlOrder;
  end;
end;

{ Get current sql query }
function TZDataset.GetSql: TStrings;
begin
  Result := SqlParser.Sql;
end;

{ Set new Sql query }
procedure TZDataset.SetSql(Value: TStrings);
begin
  if SqlParser.Sql.Text <> Value.Text then
  begin
    Close;
    SqlParser.Sql := Value;
  end;
end;

{ Get ReadOnly property }
function TZDataset.GetReadOnly: Boolean;
begin
  Result := not RequestLive;
end;

{ Set ReadOnly property }
procedure TZDataset.SetReadOnly(Value: Boolean);
begin
  RequestLive := not Value;
end;

procedure TZDataset.SetOptions(Value: TZDatasetOptions);
var
  SqlFilterChange: Boolean;
begin
  if FOptions <> Value then
  begin
    SqlFilterChange := (doSqlFilter in FOptions) <> (doSqlFilter in Value);
    FOptions := Value;
    FFiltered := Filtered and not (doSqlFilter in Value);
    if SqlFilterChange and Filtered and (Filter <> '') then
      SqlFilterRefresh;
  end;
end;

{*** Method for processing Sql parameters ***}

{ Get parameters count }
function TZDataset.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

{ Set Sql query parameters list }
procedure TZDataset.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

{ Get query parameter by name }
function TZDataset.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

{Macro Char}
procedure TZDataset.SetMacroChar(Value: Char);
begin
  if Value <> FMacroChar then
  begin
    FMacroChar := Value;
    //RecreateMacros;
  end;
end;

{ Get Macros count }
function TZDataset.GetMacroCount: Word;
begin
  Result := FMacros.Count;
end;

{Set Sql query Macros list}
procedure TZDataset.SetMacros(Value: TParams);
begin
  FMacros.AssignValues(Value);
end;

{ Get query Macro by name }
function TZDataset.MacroByName(const Value: string): TParam;
begin
  Result := FMacros.ParamByName(Value);
end;

{$IFNDEF VER100}
{ Define properties of Sql params }
procedure TZDataset.DefineProperties(Filer: TFiler);
  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TZDataset(Filer.Ancestor).FParams)
    else
      Result := (FParams.Count > 0);
  end;
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

{ Read Sql params from DFM file }
procedure TZDataset.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

{ Write Sql params into DFM file }
procedure TZDataset.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;
{$ENDIF}

{*** Internal getting field and navigation methods ***}

{ Get visible updated records types }
function TZDataset.GetUpdateRecord: TZUpdateRecordTypes;
begin
  Result := SqlBuffer.FilterTypes;
end;

{ Set visible updated records types }
procedure TZDataset.SetUpdateRecord(Value: TZUpdateRecordTypes);
var
  Index: Integer;
begin
  SqlBuffer.FilterTypes := Value;
  if (SqlBuffer.Count > 0) and (CurRec >= 0) then
  begin
    Index := SqlBuffer[CurRec].Index;
    SqlBuffer.Filter;
    CurRec := SqlBuffer.IndexOfIndex(Index);
  end else
    SqlBuffer.Filter;
  if (CurRec < 0) and (SqlBuffer.Count > 0) then
    CurRec := 0;
  if not (State in [dsInactive]) then Resync([]);
end;

{ Read all records from server }
procedure TZDataset.QueryRecords(Force: Boolean);
{$IFNDEF NO_GUI}
var
  OldCursor: TCursor;
{$ENDIF}
begin
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}
    { Check current fetch state }
    if not ((not Query.EOF) and ((doQueryAllRecords in Options)
      or (Query.RecNo < MIN_FETCH_ROWS) or Force or FetchAll)) then
      Exit;
    { Invoke on progress event }
    DoProgress(psStarting, ppFetching, SqlBuffer.Count);
    { Query records }
    while (not Query.EOF) and ((doQueryAllRecords in Options)
      or (Query.RecNo < MIN_FETCH_ROWS) or Force) do
      QueryRecord;
    { Invoke on progress event }
    DoProgress(psEnding, ppFetching, SqlBuffer.Count);
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Read one record from server }
procedure TZDataset.QueryOneRecord;
begin
  { Invoke on progress event }
  DoProgress(psStarting, ppFetching, SqlBuffer.Count);
  { Query records }
  QueryRecord;
  { Invoke on progress event }
  DoProgress(psEnding, ppFetching, SqlBuffer.Count);
end;

{ Send events to inherited dataset }
procedure TZDataset.DataEvent(Event: TDataEvent; Info: Longint);
begin
  inherited DataEvent(Event, Info);
end;

{ Fetching query records }
function TZDataset.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  TempRec: LongInt;
  CanFetch: Boolean;
begin
  CanFetch := True;
  Result := grOK;
  case GetMode of
    gmNext:
      begin
        TempRec := CurRec;
        while Result <> grEOF do
        begin
          if TempRec < SqlBuffer.Count - 1 then
            Inc(TempRec)
          else
            if FQuery.EOF or (not CanFetch) then
              Result := grEOF
            else begin
              QueryOneRecord;
              if FQuery.EOF then
                Result := grEOF
              else if SqlBuffer.Count = 0 then
                Continue
              else
                Inc(TempRec);
            end;
          if Result = grEOF then
            Break;
          if CheckRecordByFilter(TempRec) then
            Break;
        end;
        if Result = grOk then
          CurRec := TempRec;
      end;
    gmPrior:
      begin
        TempRec := CurRec;
        while Result <> grBOF do
        begin
          if TempRec <= 0 then
            Result := grBOF
          else
            Dec(TempRec);
          if Result = grBOF then
            Break;
          if CheckRecordByFilter(TempRec) then
            Break;
        end;
        if Result = grOk then
          CurRec := TempRec;
      end;
    gmCurrent:
      begin
        TempRec := CurRec;
        while Result <> grError do
        begin
          if (TempRec < 0) or (TempRec >= SqlBuffer.Count) then
          begin
            if FQuery.EOF or (not CanFetch) then
              Result := grError
            else begin
              QueryOneRecord;
              if FQuery.EOF then
                Result := grError;
            end;
          end;
          if Result = grError then
            Break;
          if CheckRecordByFilter(TempRec) then
            Break;
          Inc(TempRec);
        end;
        if Result = grOk then
          CurRec := TempRec;
      end;
  end;

  if Result = grOK then
  begin
    SqlBuffer.CopyRecord(SqlBuffer[CurRec], PRecordData(Buffer), True);
    with PRecordData(Buffer)^ do
      BookmarkFlag := bfCurrent;
    GetCalcFields(Buffer);
  end
  else if (Result = grError) and DoCheck then
    DatabaseError(SNoMoreRec);
end;

{*** Internal updating fields methods *** }

{ Store record buffer into TField }
function TZDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  Index: Integer;
  RecBuffer: PRecordData;
  FieldDesc: PFieldDesc;
  TempCurr: System.Currency;
begin
  Result := False;

  if not GetActiveRecBuf(RecBuffer) then Exit;

  if State = dsOldValue then
  begin
     if (CurRec >= 0) and (CurRec < SqlBuffer.Count) then
     begin
       Index := CacheBuffer.IndexOfIndex(SqlBuffer[CurRec].Index);
       if Index >= 0 then
         RecBuffer := CacheBuffer[Index]
     end
  end;

  FieldDesc := SqlBuffer.SqlFields.FindByField(Field);
  if not Assigned(FieldDesc) then
    DatabaseError('Fatal internal error');

  if (Field.DataType = ftBCD) and (Buffer <> nil) then
  begin
    Result := SqlBuffer.GetFieldData(FieldDesc, @TempCurr, RecBuffer);
    if Result then
{$IFDEF VER130ABOVE}
      CurrToBCD(TempCurr, TBCD(Buffer^), 32, Field.Size);
{$ELSE}
      CurrToBCD(TempCurr, Buffer, 32, Field.Size);
{$ENDIF}
  end
  else
    Result := SqlBuffer.GetFieldData(FieldDesc, Buffer, RecBuffer);
end;

{ Retrive data from TField into record buffer }
procedure TZDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuffer: PRecordData;
  FieldDesc: PFieldDesc;
  TempCurr: System.Currency;
begin
  if not GetActiveRecBuf(RecBuffer) then Exit;

  if State in [dsEdit, dsInsert] then
    Field.Validate(Buffer);
  FieldDesc := SqlBuffer.SqlFields.FindByField(Field);
  if not Assigned(FieldDesc) then
    DatabaseError('Fatal internal error');

  if (Field.DataType = ftBCD) and (Buffer <> nil) then
  begin
{$IFDEF VER130ABOVE}
    BCDToCurr(TBCD(Buffer^), TempCurr);
{$ELSE}
    BCDToCurr(Buffer, TempCurr);
{$ENDIF}
    SqlBuffer.SetFieldData(FieldDesc, @TempCurr, RecBuffer);
  end
  else
    SqlBuffer.SetFieldData(FieldDesc, Buffer, RecBuffer);

  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, LongInt(Field));
end;

function TZDataset.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftBCD then
    Result := TZBCDField
  else
    Result := inherited GetFieldClass(FieldType);
end;

{ Check if cursor open (??) }
function TZDataset.IsCursorOpen: Boolean;
begin
  Result := Query.Active or Active or (SqlBuffer.Count > 0);
end;

{ Get current affected rows }
function TZDataset.RowsAffected: LongInt;
begin
  Result := FRowsAffected;
end;

{ Internal methods for buffers processing }

{ Get record buffer size }
function TZDataset.GetRecordSize: Word;
begin
  Result := SqlBuffer.RecBufSize;
end;

{ Allocate record buffer in memory }
function TZDataset.AllocRecordBuffer: PChar;
begin
  Result := AllocMem(SqlBuffer.RecBufSize);
  SqlBuffer.InitRecord(PRecordData(Result));
end;

{ Free allocated buffer }
procedure TZDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  SqlBuffer.FreeRecord(PRecordData(Buffer), False);
  FreeMem(Buffer);
end;

{ Get current record buffer }
function TZDataset.GetActiveRecBuf(var Value: PRecordData): Boolean;
var
  N: Integer;
  CheckState: TDatasetState;
begin
  Value := nil;
  CheckState := State;
  if FNewValueState then
    CheckState := dsNewValue;
  case CheckState of
    dsBrowse:
      if not IsEmpty then
        Value := PRecordData(ActiveBuffer);
    dsEdit, dsInsert:
      Value := PRecordData(ActiveBuffer);
    dsCalcFields:
      Value := PRecordData(CalcBuffer);
    dsNewValue, dsCurValue:
      if (CurRec >= 0) and (CurRec < SqlBuffer.Count) then
        Value := SqlBuffer[CurRec];
    dsOldValue:
      begin
        if (CurRec >= 0) and (CurRec < SqlBuffer.Count) then
        begin
          N := CacheBuffer.IndexOfIndex(SqlBuffer[CurRec].Index);
          if N >= 0 then
            Value := CacheBuffer[N]
          else
            Value := SqlBuffer[CurRec];
        end
      end;
  end;
  Result := Value <> nil;
end;

{ Exec a non SELECT Sql query }
procedure TZDataset.ExecSql;
begin
  AutoFillObjects;

  if Assigned(TransactObj) then
  begin
    TransactObj.Connected := True;
    SqlParser.UpdateText;
    FRowsAffected := TransactObj.ExecSql(ConvertToSqlEnc(SqlParser.Text));
  end else
    DatabaseError(STransactNotDefined);
end;

{ After Load properties method }
procedure TZDataset.Loaded;
begin
  FAutoStart := True;
  inherited Loaded;
  FAutoStart := False;
end;

{ Update field parameters }
procedure TZDataset.UpdateFieldDef(FieldDesc: PFieldDesc;
  var FieldType: TFieldType; var FieldSize: Integer);
begin
  { Fix unknown blobs }
  if (FieldDesc = nil) and (FieldType = ftBlob) and (DatabaseType = dtPostgreSql) then
    FieldType := ftInteger;

  if FieldType <> ftBCD then
  begin
    { Fix string length }
    if FieldType <> ftString then FieldSize := 0
    else FieldSize := Min(MAX_STRING_SIZE, FieldSize);
    if (FieldType = ftString) and (FieldSize = 0) then
      FieldSize := DEFAULT_STRING_SIZE;
    if (FieldType = ftBytes) and (FieldSize = 0) then
      FieldSize := DEFAULT_STRING_SIZE;
  end;
  { Autoinc fields }
  if (doEnableAutoInc in Options) and (FieldDesc <> nil)
    and (FieldType = ftInteger) and (FieldDesc.AutoType in [atAutoInc, atIdentity]) then
    FieldType := ftAutoInc;
end;

{ Define all fields in a query }
procedure TZDataset.InternalInitFieldDefs;
var
  SaveActive: Boolean;
  I: Integer;
  FieldName: string;
  FieldRequired: Boolean;
  FieldSize: Integer;
  FieldType: TFieldType;
  FieldNo: Integer;
  FieldDesc: PFieldDesc;
  FieldReadOnly: Boolean;
  FieldPrecision: Integer;
begin
  { Set start values }
  FieldDefs.Clear;
  FieldNo := 1;
  { Open connections for separate func call }
  SaveActive := Query.Active;
  if not Query.Active then
  begin
    CreateConnections;
    { Define all tables fields }
    if RequestLive or not (doQuickOpen in Options)then
      SqlParser.DefineTableDefs;
    { Open a query }
    Query.Sql := ConvertToSqlEnc(SqlParser.Text);
    Query.Open;
  end;
  { Create TField for every query field }
  for I := 0 to Query.FieldCount - 1 do
  begin
    if (I = 0) and SqlParser.UsedRowId then
      Continue;

    FieldRequired := False;
    FieldDesc := SqlParser.SqlFields.FindByAlias(Query.FieldAlias(I));
    if Assigned(FieldDesc) then
    begin
      { Process table fields }
      FieldName := FieldDesc.Alias;
      FieldType := FieldDesc.FieldType;
      if FieldType = ftBCD then
        FieldSize := FieldDesc.Decimals
      else
        FieldSize := FieldDesc.Length;
      FieldRequired := not FieldDesc.IsNull and (FieldDesc.AutoType = atNone);
      FieldReadOnly := FieldDesc.ReadOnly;
      FieldPrecision := Query.FieldPrecision(I);
    end
    else
    begin
      { Process calc and unknown fields }
      FieldName := Query.FieldAlias(I);
      FieldType := Query.FieldDataType(I);
      if FieldType = ftBCD then
        FieldSize := Query.FieldDecimals(I)
      else
        FieldSize := Max(Query.FieldSize(I), Query.FieldMaxSize(I));
      FieldReadOnly := Query.FieldReadOnly(I);
      FieldPrecision := Query.FieldPrecision(I);
    end;
    { Correct field size }
    UpdateFieldDef(FieldDesc, FieldType, FieldSize);
    { Add new field def }
    with TFieldDef.Create(FieldDefs, FieldName, FieldType, FieldSize,
        FieldRequired, FieldNo) do
    begin
{$IFNDEF VER100}
      if FieldReadOnly then
        Attributes := Attributes + [faReadonly];
{$ENDIF}
      Precision := FieldPrecision;
    end;
    Inc(FieldNo);
  end;
  { Restore dataset state }
  if not SaveActive then
    Query.Close;
end;

{ Update index defs }
procedure TZDataset.UpdateIndexDefs;
begin
  FieldDefs.Update;
  SqlParser.UpdateIndexDefs(IndexDefs);
end;

{ Create demanded connections }
procedure TZDataset.CreateConnections;
begin
  { Check database and transact components }
  if not Assigned(DatabaseObj) then
    DatabaseError(SConnectNotDefined);
  if not Assigned(TransactObj) then
    DatabaseError(STransactNotDefined);
  { Check connect }
  TransactObj.Connect;
  if not TransactObj.Connected then
    DatabaseError(SConnectTransactError);
end;

{ Internal open query }
procedure TZDataset.InternalOpen;
var
  Error: string;
{$IFNDEF NO_GUI}
  OldCursor: TCursor;
{$ENDIF}
begin
  { Make auto-open }
  if FAutoStart and (not Assigned(DatabaseObj) or not Assigned(TransactObj)) then
  begin
    FAutoOpen := True;
    Exit;
  end;
  { Autofill objects }
  AutoFillObjects;
  { Change cursor }
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}
    { Set mail query params }
    FRowsAffected := 0;
    CurRec := -1;
    FMasterIndex := -1;
    FAutoOpen := False;
    FLinkCheck := False;
    { Create necessary connections }
    CreateConnections;
    { Set sql statement }
    SqlParser.ExtraOrderBy := FormTableSqlOrder;
    Query.Sql := ConvertToSqlEnc(SqlParser.Text);
    { Define all tables fields }
    if RequestLive or not (doQuickOpen in Options) then
      SqlParser.DefineTableDefs;
    { Update master-detail links }
    if Assigned(MasterSource) and (loLinkRequery in LinkOptions) then
      MasterRequery
    else if Assigned(DataSource) then
      ParamsRequery;
    { Set cursor fetch mode }
    Query.UseCursor := (doCursorFetch in Options);
    { Open the query }
    if Trim(Query.Sql) = '' then
      DatabaseError('Empty Query');
    if not Query.Active then
      Query.Open;
    if not Query.Active then
    begin
      Error := ConvertFromSqlEnc(Query.Error);
      if Assigned(TransactObj) then
        TransactObj.Recovery(True);
      if Error <> '' then
        DatabaseError(Error);
    end;
    { Initialize field and index defs }

    InternalInitFieldDefs;
    if DefaultFields then
      CreateFields;
    BindFields(True);
    { Update field and index defs }
    SqlParser.UpdateIndexDefs(IndexDefs);
    SqlBuffer.BindFields(SqlParser.SqlFields);
    SqlBuffer.BindIndices(IndexDefs, SqlParser.SqlIndices);
    CacheBuffer.SetCache(SqlBuffer);
    { Fetch records }
    QueryRecords(False);
    if Assigned(MasterSource) then
      MasterRequery;
    { Set index sorting }
    if FIndexName <> '' then
      SetIndex(FIndexName, FFieldsIndex);
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Internal close qiery }
procedure TZDataset.InternalClose;
{$IFNDEF NO_GUI}
var
  OldCursor: TCursor;
{$ENDIF}
begin
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}
    { Clear all collections }
    Query.Close;
    SqlBuffer.ClearBuffer(True);
    CacheBuffer.ClearBuffer(True);
    SqlParser.Clear;
    { Destroy default fields }
    if DefaultFields then
      DestroyFields;
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Internal go to first record }
procedure TZDataset.InternalFirst;
begin
  CurRec := -1;
end;

{ Internal go to last record }
procedure TZDataset.InternalLast;
begin
  QueryRecords(True);
  CurRec := SqlBuffer.Count;
end;

{ Internal exception processing }
procedure TZDataset.InternalHandleException;
begin
  Application.HandleException(Self);
end;

{ Get records quantity }
function TZDataset.GetRecordCount: Longint;
var
  I: LongInt;
begin
  QueryRecords(True);
  if Filtered and not (doSqlFilter in Options) then
  begin
    Result := 0;
    for I := 0 to SqlBuffer.Count-1 do
      if CheckRecordByFilter(I) then
        Inc(Result);
  end else
    Result := SqlBuffer.Count;
end;

{ Get current record number }
function TZDataset.GetRecNo: Longint;
begin
  UpdateCursorPos;
  if (CurRec = -1) and (RecordCount > 0) then
    Result := 1
  else
    Result := CurRec + 1;
end;

{ Set currenct record number }
procedure TZDataset.SetRecNo(Value: Integer);
{$IFNDEF NO_GUI}
var
  OldCursor: TCursor;
{$ENDIF}
begin
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    Value := Max(1, Value);

    { Invoke on progress event }
    DoProgress(psStarting, ppFetching, SqlBuffer.Count);
    { Fetch one record from server }
    while not Query.EOF and (Value > SqlBuffer.Count) do
      QueryRecord;
    { Invoke on progress event }
    DoProgress(psEnding, ppFetching, SqlBuffer.Count);

    if Value <= SqlBuffer.Count then
      CurRec := Value - 1
    else
      CurRec := SqlBuffer.Count - 1;
    if not (State in [dsInactive]) then Resync([]);
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Define is query editable? }
function TZDataset.GetCanModify: Boolean;
begin
  Result := FRequestLive;
end;

{*** Bookmarks processing methods ***}

{ Internal go to bookmark }
procedure TZDataset.InternalGotoBookmark(Bookmark: Pointer);
var
  Index: Integer;
begin
  Index := PInteger(Bookmark)^;
  CurRec := SqlBuffer.IndexOfIndex(Index);
  if CurRec < 0 then
    DatabaseError(SBookmarkNotFound);
end;

{ Internal go to defined record }
procedure TZDataset.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@PRecordData(Buffer).Index);
end;

{ Get bookmark flag }
function TZDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecordData(Buffer).BookmarkFlag;
end;

{ Set bookmark flag }
procedure TZDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecordData(Buffer).BookmarkFlag := Value;
end;

{ Get bookmark data }
procedure TZDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PRecordData(Buffer).Index;
end;

{ Set boomark data }
procedure TZDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PRecordData(Buffer).Index := PInteger(Data)^;
end;

{ Compare two bookmarks }
function TZDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
var
  Index1, Index2: Integer;
begin
  Result := 0;
  if not Assigned(Bookmark1) or not Assigned(Bookmark2) then
    Exit;
  Index1 := SqlBuffer.IndexOfIndex(PInteger(Bookmark1)^);
  Index2 := SqlBuffer.IndexOfIndex(PInteger(Bookmark2)^);
  if Index1 < Index2 then Result := -1
  else if Index1 > Index2 then Result := 1;
end;

{ Validate book }
function TZDataset.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := False;
  if Active and Assigned(Bookmark) then
    try
      Result := (SqlBuffer.IndexOfIndex(PInteger(Bookmark)^) >= 0)
    except
      Result := False;
    end;
end;

{*************** Updating methods **************}

{ Update record after initialization }
procedure TZDataset.UpdateAfterInit(RecordData: PRecordData);
var
  I: Integer;
  FieldDesc: PFieldDesc;
  RecordBlob: PRecordBlob;
begin
  { Correct blobs description }
  for I := 0 to SqlBuffer.SqlFields.Count-1 do
  begin
    FieldDesc := SqlBuffer.SqlFields[I];
    if FieldDesc.FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo] then
    begin
      RecordBlob := PRecordBlob(@RecordData.Bytes[FieldDesc.Offset+1]);
      RecordBlob.BlobType := FieldDesc.BlobType;
      RecordBlob.Data := nil;
      RecordBlob.Size := 0;
      RecordBlob.Handle.Ptr := 0;
      RecordBlob.Handle.PtrEx := 0;
    end;
  end;
end;

{ Clear and initialize new record buffer }
procedure TZDataset.InternalInitRecord(Buffer: PChar);
var
  I: Integer;
  FieldDesc: PFieldDesc;
begin
  { Initiate buffer }
  SqlBuffer.FreeRecord(PRecordData(Buffer), True);
  { Put default expressions }
  for I := 0 to SqlBuffer.SqlFields.Count-1 do
  begin
    FieldDesc := SqlBuffer.SqlFields[I];
    if FieldDesc.FieldObj.DefaultExpression <> '' then
    begin
      if doCalcDefault in Options then
        { Calculate default expression }
        SqlBuffer.SetField(FieldDesc,
          EvaluteDef(FieldDesc.FieldObj.DefaultExpression), PRecordData(Buffer))
      else
        { Copy default expression }
        SqlBuffer.SetField(FieldDesc, FieldDesc.FieldObj.DefaultExpression,
          PRecordData(Buffer));
    end else
      if ((FieldDesc.Default <> '') or (not FieldDesc.IsNull
        and (doAutoFillDefs in Options))) and (FieldDesc.AutoType = atNone) then
        { Calculate sql field default value }
        SqlBuffer.SetField(FieldDesc, EvaluteDef(FieldDesc.Default),
          PRecordData(Buffer));
  end;
  { Put link values from master dataset }
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
  begin
    if FMasterIndex = -1 then
      MasterDefine;
    if FMasterIndex <> -1 then
      for I := 0 to SqlBuffer.FilterFieldCount-1 do
      begin
        FieldDesc := SqlBuffer.SqlFields[SqlBuffer.FilterFields[I]];
        SqlBuffer.SetFieldValue(FieldDesc,
          SqlBuffer.GetFieldValue(FieldDesc, SqlBuffer.FilterBuffer),
          PRecordData(Buffer));
      end;
  end;
  { Update record by local init }
  UpdateAfterInit(PRecordData(Buffer));
end;

{ Evalute default value }
function TZDataset.EvaluteDef(Value: string): string;
  { Try to count equation }
  function Evalute(Buffer: string; var Value: string): Boolean;
  var
    CmdNo: Integer;
    TokenType: TTokenType;
  begin
    TokenType := ExtractHighToken(Buffer, nil, Value, CmdNo);
    DeleteQuotes(Value);
    if (TokenType in [ttString, ttDigit]) and (Trim(Buffer) = '') then
      Result := True
    else
      Result := False;
  end;
begin
  Result := '';
  if Value = '' then Exit;
  try
    if not Evalute(Value, Result) then
      Result := FTransact.ExecFunc(Value)
  except
  end;
end;

{ Internal edit mode setting }
procedure TZDataset.InternalEdit;
var
  ActiveData: PRecordData;
begin
  if not CachedUpdates and (doRefreshBeforeEdit in Options) then
  begin
    if not GetActiveRecBuf(ActiveData) then Exit;
    if RefreshCurrentRow(ActiveData) then
    begin
      SqlBuffer.CopyRecord(ActiveData, SqlBuffer[CurRec], False);
      Resync([]);
    end;
  end;
end;

{ Internal updates store }
procedure TZDataset.InternalUpdate;
var
  ActiveData: PRecordData;
  SaveRecord: PRecordData;
  CacheRecord: PRecordData;
  Index: Integer;
begin
  { Get current buffer }
  if not GetActiveRecBuf(ActiveData) then Exit;
  { Change record tyoe }
  SaveRecord := PRecordData(AllocRecordBuffer);
  SqlBuffer.CopyRecord(SqlBuffer[CurRec], SaveRecord, False);
  if SqlBuffer[CurRec].RecordType = ztUnmodified then
    SqlBuffer[CurRec].RecordType := ztModified;
  { Copy old record content into cache }
  if CacheBuffer.IndexOfIndex(SqlBuffer[CurRec].Index) < 0 then
  begin
    CacheRecord := CacheBuffer.Add;
    SqlBuffer.CopyRecord(SqlBuffer[CurRec], CacheRecord, True);
  end else
    CacheRecord := nil;
  { Save current data and post updates }
  SqlBuffer.CopyRecord(ActiveData, SqlBuffer[CurRec], False);
  { Filter updated record }
  if SqlBuffer.FilterItem(CurRec) then
    CurRec := Min(SqlBuffer.Count-1, CurRec);
  { Post updates }
  try
    if not CachedUpdates then Flush;
  except
    SqlBuffer.CopyRecord(SaveRecord, SqlBuffer[CurRec], False);
    if Assigned(CacheRecord) then
      CacheBuffer.Remove(CacheRecord);
    FreeRecordBuffer(PChar(SaveRecord));
    raise;
  end;
  FreeRecordBuffer(PChar(SaveRecord));
  { Resort query }
  if (SqlBuffer.SortFieldCount > 0) or (SqlBuffer.IsSortInverse) then
  begin
    Index := SqlBuffer[CurRec].Index;
    SqlBuffer.SortRestore;
    CurRec := SqlBuffer.IndexOfIndex(Index);
  end;
end;

{ Internal add new record }
procedure TZDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  ActiveData: PRecordData;
  AddRecord: PRecordData;
  CacheRecord: PRecordData;
  Index: Integer;
begin
  { Get and check current buffer }
  if not GetActiveRecBuf(ActiveData) then Exit;
  if ActiveData <> Buffer then
    DatabaseError(Format(SIntFuncError,['InternalAddRecord']));
  { Append or insert a new record }
  if Append or (CurRec < 0) or (CurRec >= SqlBuffer.Count) then
  begin
    InternalLast;
    AddRecord := SqlBuffer.Add;
    CurRec := SqlBuffer.Count-1;
  end else
    AddRecord := SqlBuffer.Insert(CurRec);
  { Fill inserted record with current values }
  SqlBuffer.CopyRecord(ActiveData, AddRecord, False);
  AddRecord.RecordType := ztInserted;
  ChangeAddBuffer(AddRecord);
  { Add a record into the cache and post updates }
  CacheRecord := CacheBuffer.Add;
  SqlBuffer.CopyRecord(AddRecord, CacheRecord, True);
  { Filter inserted record }
  if SqlBuffer.FilterItem(CurRec) then
    CurRec := Min(SqlBuffer.Count-1, CurRec);
  { Post changes }
  try
    if not CachedUpdates then Flush;
  except
    SqlBuffer.Remove(AddRecord);
    CacheBuffer.Remove(CacheRecord);
    raise;
  end;
  { Resort query }
  if (SqlBuffer.SortFieldCount > 0) or (SqlBuffer.IsSortInverse) then
  begin
    Index := SqlBuffer[CurRec].Index;
    SqlBuffer.SortRestore;
    CurRec := SqlBuffer.IndexOfIndex(Index);
  end;
end;

{ Internal procedure for change inserting data }
procedure TZDataset.ChangeAddBuffer(AddRecord: PRecordData);
begin
end;

{ Internal post updates }
procedure TZDataset.InternalPost;
var
  ActiveData: PRecordData;
begin
  { Get current buffer }
  GetActiveRecBuf(ActiveData);

  CheckContraints;

  { Update or insert record according dataset state }
  if State = dsEdit then
    InternalUpdate
  else
    InternalAddRecord(ActiveData, False);
end;

{ Internal delete record }
procedure TZDataset.InternalDelete;
var
  ActiveData: PRecordData;
  Index: Integer;
begin
  CheckBrowseMode;
  if not CanModify then
    DatabaseError('Cannot modify a read-only dataset');
  { Get and check current buffer }
  if not GetActiveRecBuf(ActiveData) then Exit;
  { Check record type and delete (if inserted) or mark the record }
  if SqlBuffer[CurRec].RecordType = ztInserted then
  begin
    CacheBuffer.Delete(CacheBuffer.IndexOfIndex(SqlBuffer[CurRec].Index));
    SqlBuffer.Delete(CurRec);
  end
  else
  begin
    SqlBuffer[CurRec].RecordType := ztDeleted;
    Index := CacheBuffer.IndexOfIndex(SqlBuffer[CurRec].Index);
    { If record already in buffer - mark, else add }
    if Index < 0 then
      SqlBuffer.CopyRecord(SqlBuffer[CurRec], CacheBuffer.Add, True)
    else
      CacheBuffer[Index].RecordType := ztDeleted;
  end;
  { Filter updated record }
  if CurRec < SqlBuffer.Count then
    SqlBuffer.FilterItem(CurRec);
  CurRec := Min(SqlBuffer.Count-1, CurRec);
  { Post changes }
  if not CachedUpdates then Flush;
end;

{ Internal refresh query }
procedure TZDataset.InternalRefresh;
var
  Error: string;
  KeyFields: string;
  KeyValues: Variant;
  RecordCount: Integer;
{$IFNDEF NO_GUI}
  OldCursor: TCursor;
{$ENDIF}
begin
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
       Screen.Cursor := crSqlWait;
{$ENDIF}
    { Store record params }
    RecordCount := Self.RecordCount;
    FormKeyValues(KeyFields, KeyValues);
    { Clear all collections }
    Query.Close;
    SqlBuffer.ClearBuffer(False);
    CacheBuffer.ClearBuffer(False);
    { Open the query }
    if Trim(Query.Sql) = '' then
      DatabaseError('Empty Query');
    Query.Open;
    if not Query.Active then
    begin
      Error := ConvertFromSqlEnc(Query.Error);
      if Assigned(TransactObj) then
        TransactObj.Recovery(True);
      DatabaseError(Error);
    end;
    { Set mail query params }
    FRowsAffected := 0;
    CurRec := -1;

    { Invoke on progress event }
    DoProgress(psStarting, ppFetching, SqlBuffer.Count);
    { Fetch fields }
    while (not Query.EOF) and ((RecordCount > 0) or FetchAll) do
    begin
      QueryRecord;
      Dec(RecordCount);
    end;
    { Invoke on progress event }
    DoProgress(psEnding, ppFetching, SqlBuffer.Count);

    { Sort with old method }
    SqlBuffer.SortRestore;
    { Locate to old position }
    if KeyFields <> '' then
      Locate(KeyFields, KeyValues, []);
    { Resync records }
    if not (State in [dsInactive]) then Resync([]);
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Short refresh dataset }
procedure TZDataset.ShortRefresh;
{$IFNDEF NO_GUI}
var
  OldCursor: TCursor;
{$ENDIF}
begin
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}
    { Clear all collections }
    Query.Close;
    SqlBuffer.ClearBuffer(False);
    CacheBuffer.ClearBuffer(False);
    { Open the query }
    Query.Open;
    if not Query.Active then
    begin
      if Assigned(TransactObj) then
        TransactObj.Recovery(True);
      DatabaseError(SDetailQueryError);
    end;
    { Initialize field and index defs }
    CurRec := -1;
    { Fetch records }
    if Active then
      QueryRecords(False);
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Refresh computed fields }
function TZDataset.RefreshCurrentRow(RecordData: PRecordData): Boolean;
var
  I: Integer;
  Sql, Where: string;
  Query: TDirQuery;
  FieldDesc: PFieldDesc;
  IsNull: Boolean;
  FieldValue: string;
begin
  Result := False;

  { Check tables }
  if SqlParser.Tables.Count = 0 then Exit;
  { Form Sql command }
  Where := Trim(FormSqlWhere(SqlParser.Tables[0], RecordData));
  if Where = '' then Exit;
  Sql := 'SELECT * FROM ' + ProcessIdent(SqlParser.Tables[0]) + ' ' + Where;

  Query := TransactObj.QueryHandle;
  Query.Close;
  Query.Sql := Sql;
  Query.Open;
  if not Query.Active or Query.EOF then Exit;

  for I := 0 to SqlBuffer.SqlFields.Count-1 do
  begin
    FieldDesc := SqlBuffer.SqlFields[I];
    if FieldDesc.FieldNo < 0 then Continue;
    IsNull := Query.FieldIsNull(FieldDesc.FieldNo);

    if IsNull and not SqlBuffer.GetFieldNull(FieldDesc, RecordData) then
    begin
      SqlBuffer.SetFieldNull(FieldDesc, IsNull, RecordData);
      Result := True;
    end
    else   //refresh only for no blob field
    if FieldDesc.BlobType = btInternal then
    begin
      FieldValue := Query.Field(FieldDesc.FieldNo);
      if FieldValue <> SqlBuffer.GetField(FieldDesc, RecordData) then
      try
        SqlBuffer.SetField(FieldDesc,FieldValue,RecordData);
        Result := True;
      except
      end;
    end;
  end;
  Query.Close;
end;

{$IFNDEF VER100}

procedure TZDataset.ResetAggField(Field: TField);
var i :integer;
begin
 {
 if (Field<>nil) and (Field is TAggregateField) then
 (Field as TAggregateField).Active := False;
 }

  for I := 0 to AggFields.Count - 1 do
  if AggFields[I] = Field then
  begin
    (AggFields[I] as TAggregateField).Active := False;
    break;
  end;
end;


function TZDataset.GetAggregateValue(Field: TField): Variant;
begin
 Result := Field.Value;
end;

{$ENDIF}

{***************** Filter methods **************}

{ Find record in a filtered query }
function TZDataset.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  Index: Integer;
  SaveFiltered: Boolean;
begin
  { Check state }
  CheckBrowseMode;
  DoBeforeScroll;
  Result := False;
  { Set position }
  if Restart then
  begin
    if GoForward then
      Index := 0
    else
    begin
      QueryRecords(True);
      Index := SqlBuffer.Count-1;
    end
  end
  else
  begin
    Index := CurRec;
    if GoForward then
      Inc(Index)
    else
      Dec(Index);
  end;
  { Find a record }
  SaveFiltered := FFiltered;
  try
    FFiltered := True;
    while (Index >= 0) and (Index < SqlBuffer.Count) do
    begin
      if CheckRecordByFilter(Index) then
      begin
        Result := True;
        Break;
      end;
      if not GoForward then
        Dec(Index)
      else begin
        Inc(Index);
        if (Index >= SqlBuffer.Count) and not Query.EOF then
          QueryOneRecord;
      end;
    end
  finally
    FFiltered := SaveFiltered;
  end;

  SetFound(Result);
  if Result then
  begin
    RecNo := Index + 1;
    DoAfterScroll;
  end;
end;

{ Turn off/on filtering }
procedure TZDataset.SetFiltered(Value: Boolean);
var
  Bookmark: TBookmark;
begin
  if Value <> Filtered then
  begin
    { Turn off controls and save current position }
    DisableControls;
    Bookmark := GetBookmark;
    try
      { Process filtered }
      FFiltered := Value and not (doSqlFilter in Options);
      inherited SetFiltered(Value);
      { Resync recordset }
      if not (State in [dsInactive]) and FFiltered  then
      begin
        Resync([]);
        First;
      end else
        if (doSqlFilter in Options) and (Filter <> '') then
          SqlFilterRefresh;

      { Restore position }
      if not Value then
        GotoBookmark(Bookmark);
    finally
      FreeBookmark(Bookmark);
      EnableControls;
    end;
  end;
end;

{ Set filter equation }
procedure TZDataset.SetFilterText(const Value: string);
var
  Bookmark: TBookmark;
begin
  inherited SetFilterText(Trim(Value));
  if Trim(Value) <> FParser.Equation then
  begin
    { Turn off controls and save current position }
    DisableControls;
    Bookmark := GetBookmark;
    try
      { Set new equation }
      FParser.Equation := Trim(Value);
      { Adjust position }
      if FFiltered and not (State in [dsInactive]) then
      begin
        Resync([]);
        if Trim(Value) <> '' then
          First
        else
          GotoBookmark(Bookmark);
      end else
      if Filtered and (doSqlFilter in Options) then
         SqlFilterRefresh;
    finally
      FreeBookmark(Bookmark);
      EnableControls;
    end;
  end;
end;

procedure TZDataset.SqlFilterRefresh;
begin
  if not (State in [dsInactive]) and
    (ConvertToSqlEnc(SqlParser.Text) <> Query.Sql) then
  begin
    Query.Sql := ConvertToSqlEnc(SqlParser.Text);
    ShortRefresh;
    First;
  end;
end;

{ Check is query sequensed? }
function TZDataset.IsSequenced: Boolean;
begin
  Result := (not Filtered);
end;

{ Check is record hided by filter? }
function TZDataset.CheckRecordByFilter(RecNo: LongInt): Boolean;
var
  I: Integer;
  OldCurRec: Integer;
  OldState: TDatasetState;
  FieldDesc: PFieldDesc;
  Value: Variant;
begin
  Result := True;
  { Check record index }
  if (RecNo < 0) or (RecNo >= SqlBuffer.Count) then
  begin
    Result := False;
    Exit;
  end;
  { Check record by OnFilterRecord event }
  if Filtered and Assigned(OnFilterRecord) then
  begin
    OldState  := State;
    OldCurRec := CurRec;
    try
      SetState(dsNewValue);
      CurRec := RecNo;
      OnFilterRecord(Self, Result);
    finally
      SetState(OldState);
      CurRec := OldCurRec;
    end;
  end;
  if not Result then Exit;
  { Check record by equation }
  if FFiltered and (FParser.Equation <> '') then
  begin
    { Fill field variables }
    for I := 0 to FParser.VarCount-1 do
    begin
      FieldDesc := SqlBuffer.SqlFields.FindByAlias(FParser.VarNames[I]);
      if not Assigned(FieldDesc) then Continue;
      FParser.Variables[FParser.VarNames[I]] :=
        SqlBuffer.GetFieldValue(FieldDesc, SqlBuffer[RecNo]);
    end;
    { Evalute the result }
    Value := FParser.Evalute;
    Result := (Value <> Null) and
      (StrToFloatDefEx(VarAsType(Value, varString),-1) <> 0);
  end;
end;

{***************** Extra methods ***************}

{ Process notification method }
procedure TZDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then Exit;
  { Check if removing database object }
  if AComponent = FDatabase then
  begin
    Close;
    try
      FDatabase.RemoveDataset(Self);
    finally
      FDatabase := nil;
    end;
  end;
  { Check if removing transact object }
  if AComponent = FTransact then
  begin
    Close;
    RequestLive := False;
    FTransact   := nil;
  end;
  { Clear other external components }
  if AComponent = FUpdateObject then
    FUpdateObject := nil;
  if AComponent = FMasterLink.Datasource then
    FMasterLink.DataSource := nil;
  if AComponent = FDataLink.Datasource then
    FDataLink.DataSource := nil;
end;

{ Invoke OnProgress event }
procedure TZDataset.DoProgress(Stage: TZProgressStage;
  Proc: TZProgressProc; Position: Integer);
var
  Cancel: Boolean;
begin
  if Assigned(OnProgress) then
  begin
    Cancel := False;
    OnProgress(Self, Stage, Proc, Position,
      Max(SqlBuffer.Count, Query.RecordCount), Cancel);
  end;
end;

{******************* IndexDefs support routines *****************}

{ Set new IndexDefs }
procedure TZDataset.SetIndexDefs(Value: TIndexDefs);
begin
  IndexDefs.Assign(Value);
end;

{ Get current index name }
function TZDataset.GetIndexName: string;
begin
  if FFieldsIndex then Result := ''
  else Result := FIndexName;
end;

{ Set new index name }
procedure TZDataset.SetIndexName(const Value: string);
begin
  if (FIndexName <> Value) or (FFieldsIndex <> False) then
    SetIndex(Value, False);
end;

{ Get fields index }
function TZDataset.GetIndexFieldNames: string;
begin
  if FFieldsIndex then Result := FIndexName
  else Result := '';
end;

{ set fields index }
procedure TZDataset.SetIndexFieldNames(const Value: string);
begin
  if (FIndexName <> Value) or (FFieldsIndex <> True) then
    SetIndex(Value, True);
end;

{ Get field index count }
function TZDataset.GetIndexFieldCount: Integer;
begin
  Result := FieldCount;
end;

{ Get index field }
function TZDataset.GetIndexField(Index: Integer): TField;
begin
  Result := Fields[Index];
end;

{ Set index field }
procedure TZDataset.SetIndexField(Index: Integer; Value: TField);
begin
  GetIndexField(Index).Assign(Value);
end;

{ Set new index }
procedure TZDataset.SetIndex(const Value: string; FieldsIndex: Boolean);
var
  IndexDef: TIndexDef;
  Fields: string;
  SortType: TSortType;
begin
  { Set startup values }
  FIndexName := Value;
  FFieldsIndex := FieldsIndex;
  SortType := stAscending;
  { Get sorting fields }
  if FieldsIndex then
    Fields := Value
  else
    try
      if IndexDefs.IndexOf(Value) >= 0 then
      begin
        IndexDef := IndexDefs[IndexDefs.IndexOf(Value)];
        Fields := IndexDef.Fields;
        if ixDescending in IndexDef.Options then
          SortType := stDescending;
      end else
        Fields := '';
    except
      Fields := '';
    end;
  { Check state }
  InternalSort(Fields, SortType);
end;

{delphi str fields to sql str fields}
function TZDataset.FormatFieldsList(Value: string): string;
var
  FieldName: string;
  i: Integer;
begin
  i := 1;
  Result := '';
  while i <= Length(Value) do
  begin
   FieldName := ExtractFieldName(Value, i);
   if Result = '' then
      Result := ProcessIdent(FieldName)
    else
      Result := Result + ', ' + ProcessIdent(FieldName);
  end;
end;

{generate sql order for table}
function TZDataset.FormTableSqlOrder: string;
var
  IndexDef : TIndexDef;
  I: Integer;

{$IFDEF VER100}
  function FindIndex(IndexDefs: TIndexDefs; Name: string): TIndexDef;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to IndexDefs.Count-1 do
      if IndexDefs.Items[I].Name = Name then
      begin
        Result := IndexDefs.Items[I];
        Exit;
      end;
  end;
{$ENDIF}

begin
  Result := '';
  if Self.TableName = '' then Exit;
  if (IndexFieldNames <> '') then
    Result := FormatFieldsList(IndexFieldNames)
  else
  if (IndexName <> '') then
  begin
    IndexDefs.Update;
{$IFNDEF VER100}
    IndexDef := IndexDefs.Find(IndexName);
{$ELSE}
    IndexDef := FindIndex(IndexDefs, IndexName);
{$ENDIF}
    if IndexDef <> nil then
    begin
      Result := FormatFieldsList(IndexDef.Fields);

      if ixDescending in IndexDef.Options then
      begin
{$IFNDEF VER100}
        StringReplace(Result, ',', ' DESC,', [rfReplaceAll]);
{$ENDIF}
        Result := Result + ' DESC';
      end;
    end;
  end else
  if DefaultIndex then
  begin
    //IndexDefs.Update;
    for I := 0 to IndexDefs.Count-1 do
    if ixPrimary in IndexDefs[I].Options then
    begin
     Result := FormatFieldsList(IndexDefs[I].Fields);
     Break;
    end;
  end;
end;

{ Get index names }
procedure TZDataset.GetIndexNames(List: TStrings);
var
  I: Integer;
begin
  IndexDefs.Update;
  List.Clear;
  for I := 0 to IndexDefs.Count-1 do
    List.Add(IndexDefs[I].Name);
end;

{*** Sorting records in a query ***}

procedure TZDataset.InternalSort(Fields: string; SortType: TSortType);
var
  Index: Integer;
{$IFNDEF NO_GUI}
  OldCursor: TCursor;
{$ENDIF}
begin
  { Get all records and check buffer }
  QueryRecords(True);
  if SqlBuffer.Count = 0 then Exit;
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}
    { Save current position }
    if CurRec >= 0 then
      Index := SqlBuffer[CurRec].Index
    else
     Index := -1;
    { Sorting fields }
    if Fields <> '' then
      SqlBuffer.SetSort(Fields, SortType)
    else begin
      if SortType = stAscending then
        SqlBuffer.ClearSort
      else
        SqlBuffer.SortInverse;
    end;
    { Restore position }
    if Index >= 0 then
      CurRec := SqlBuffer.IndexOfIndex(Index);
    { Resync recordset }
    if not (State in [dsInactive]) then
      Resync([]);
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Inverse records order }
procedure TZDataset.SortInverse;
begin
  CheckBrowseMode;
  InternalSort('', stDescending);
end;

{ Inc records sort by a field }
procedure TZDataset.SortByField(Fields: string);
begin
  CheckBrowseMode;
  InternalSort(Fields, stAscending);
end;

{ Descending records sort by field }
procedure TZDataset.SortDescByField(Fields: string);
begin
  CheckBrowseMode;
  InternalSort(Fields, stDescending);
end;

{ Clear records sort order }
procedure TZDataset.SortClear;
begin
  CheckBrowseMode;
  InternalSort('', stAscending);
end;

{*** Searching methods ***}

{ Define table keys }
procedure TZDataset.DefineTableKeys(Table: string; Unique: Boolean;
  var FieldList: TFieldList; var FieldCount: Integer);
var
  I, N: Integer;
  FieldDesc: PFieldDesc;
  IndexDesc: PIndexDesc;
  MaxKey: TKeyType;
begin
  { Set start values }
  N := -1;
  MaxKey := ktIndex;
  FieldCount := 0;
  { Search primary or unique key }
  for I := 0 to SqlBuffer.SqlIndices.Count-1 do
  begin
    IndexDesc := SqlBuffer.SqlIndices[I];
    if (MaxKey < IndexDesc.KeyType) and StrCaseCmp(Table, IndexDesc.Table) then
    begin
      MaxKey := IndexDesc.KeyType;
      N := I;
    end;
  end;
  { Define fields of primary or unique key }
  if N >= 0 then
  begin
    IndexDesc := SqlBuffer.SqlIndices[N];
    for I := 0 to IndexDesc.FieldCount-1 do
    begin
      FieldDesc := SqlBuffer.SqlFields.FindByName(IndexDesc.Table,
        IndexDesc.Fields[I]);
      if Assigned(FieldDesc) then
      begin
        FieldList[FieldCount] := SqlBuffer.SqlFields.IndexOf(FieldDesc);
        Inc(FieldCount);
      end;
    end;
  end
  else
  if not Unique then
  begin
  { Define all fields }
    for I := 0 to SqlBuffer.SqlFields.Count-1 do
    begin
      FieldDesc := SqlBuffer.SqlFields[I];
      if StrCaseCmp(FieldDesc.Table, Table) and not
        (FieldDesc.FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo]) then
      begin
        FieldList[FieldCount] := I;
        Inc(FieldCount);
      end;
    end;
  end;

end;

{ Internal locate record }
function TZDataset.InternalLocate(KeyFields: string; KeyValues: Variant;
  Options: TLocateOptions): LongInt;
var
  I, J: LongInt;
  Value: string;
  IsFound: Boolean;
  FieldList: TFieldList;
  FieldCount: Integer;
{$IFNDEF NO_GUI}
  OldCursor: TCursor;
{$ENDIF}
  Values: array[0..MAX_FIELD_COUNT] of string;
  RecordData: PRecordData;
begin
  CheckBrowseMode;
  { Set start values }
  Result := -1;
  RecordData := AllocMem(SqlBuffer.RecBufSize);
  FillChar(RecordData^, SqlBuffer.RecBufSize, 1);
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}
    { Process fields }
    SqlBuffer.ProcessFieldList(KeyFields, FieldList, FieldCount);
    { Correct field }
    if FieldCount = 0 then Exit;
    if FieldCount = 1 then
      KeyValues := VarArrayOf([KeyValues]);
    { Check arguments count }
    if VarArrayHighBound(KeyValues,1)-VarArrayLowBound(KeyValues,1) <> FieldCount-1 then
      DatabaseError(SIncorrectArgs);
    { Prepare locate buffer }
    for I := 0 to FieldCount-1 do
    begin
      SqlBuffer.SetFieldValue(SqlBuffer.SqlFields[FieldList[I]], KeyValues[I],
        RecordData);
      Values[I] := SqlBuffer.GetField(SqlBuffer.SqlFields[FieldList[I]],
        RecordData);
      if loCaseInsensitive in Options then
        Values[I] := AnsiUpperCase(Values[I]);
    end;
    { Start search }
    I := 0;
    while True do
    begin
      if I >= SqlBuffer.Count then
      begin
        DoProgress(psStarting, ppFetching, SqlBuffer.Count);
        while (I >= SqlBuffer.Count) and (not Query.EOF) do
          QueryOneRecord;
        DoProgress(psEnding, ppFetching, SqlBuffer.Count);
      end;
      { Check a position }
      if I >= SqlBuffer.Count then Exit;
      { Check field by filter }
      if Filtered and not CheckRecordByFilter(I) then
      begin
        Inc(I);
        Continue;
      end;
      { Ordinary compare }
      if Options = [] then
        IsFound := (SqlBuffer.CompareRecord(SqlBuffer[I], RecordData,
          FieldList, FieldCount) = 0)
      else begin
        { Special compation }
        IsFound := True;
        for J := 0 to FieldCount-1 do
        begin
          { Get field value }
          Value := SqlBuffer.GetField(SqlBuffer.SqlFields[FieldList[J]], SqlBuffer[I]);
          { Correct value by options }
          if loCaseInsensitive in Options then
            Value := AnsiUpperCase(Value);
          { Compation }
          if loPartialKey in Options then
            IsFound := (StrLComp(PChar(Value), PChar(Values[J]),
              Length(Values[J])) = 0)
          else
            IsFound := (Value = Values[J]);
          if not IsFound then Break;
        end;
      end;
      if IsFound then
      begin
        Result := I;
        Break;
      end;
      Inc(I);
    end;
  finally
    FreeMem(RecordData);
{$IFNDEF NO_GUI}
    if doHourGlass in FOptions then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Locate a record }
function TZDataset.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
var
  Index: Integer;
begin
  DoBeforeScroll;
  Index := InternalLocate(KeyFields, KeyValues, Options);
  if Index >= 0 then
  begin
    RecNo := Index + 1;
    Result := True;
    DoAfterScroll;
  end else
    Result := False;
  SetFound(Result);
end;

{ For lookup fields... }
function TZDataset.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
var
  I, J: Integer;
  FieldList: TFieldList;
  FieldCount: Integer;
  ResValues: Variant;
begin
  Result := False;
  { Process fields }
  SqlBuffer.ProcessFieldList(ResultFields, FieldList, FieldCount);
  { Locate to record }
  I := InternalLocate(KeyFields, KeyValues, []);
  if I < 0 then Exit;
  { Fill result array }
  if FieldCount < 2 then
    ResValues := SqlBuffer.GetFieldValue(SqlBuffer.SqlFields[FieldList[0]],
      SqlBuffer[I])
  else begin
    ResValues := VarArrayCreate([0, FieldCount-1], varVariant);
    for J := 0 to FieldCount-1 do
      ResValues[J] := SqlBuffer.GetFieldValue(SqlBuffer.SqlFields[FieldList[J]],
        SqlBuffer[I]);
  end;
  Result := ResValues;
end;

{ Internal keys form }
procedure TZDataset.InternalFormKeyValues(RecordData: PRecordData; Unique: Boolean;
  var KeyFields: string; var KeyValues: Variant);
var
  I, J: Integer;
  AllCount, FieldCount: Integer;
  AllList, FieldList: TFieldList;
  FieldDesc: PFieldDesc;
begin
  { Set startup values }
  KeyFields := '';
  AllCount := 0;
  { Count fields and form field list }
  for I := 0 to SqlParser.Tables.Count-1 do
  begin
    DefineTableKeys(SqlParser.Tables[I], Unique, FieldList, FieldCount);
    for J := 0 to FieldCount-1 do
    begin
      AllList[AllCount] := FieldList[J];
      Inc(AllCount);
    end;
  end;
  { Create array for key values }
  if AllCount > 1 then
    KeyValues := VarArrayCreate([0, AllCount-1], varVariant);
  { Form field values }
  for I := 0 to AllCount-1 do
  begin
    FieldDesc := SqlBuffer.SqlFields[AllList[I]];
    if KeyFields <> '' then KeyFields := KeyFields + ',';
    if Pos(' ', FieldDesc.Alias) > 0 then
      KeyFields := KeyFields + '"' + FieldDesc.Alias + '"'
    else
      KeyFields := KeyFields + FieldDesc.Alias;
    if AllCount > 1 then
      KeyValues[I] := SqlBuffer.GetFieldValue(FieldDesc, RecordData)
    else
      KeyValues := SqlBuffer.GetFieldValue(FieldDesc, RecordData);
  end;
end;

{ Auto keys form }
procedure TZDataset.FormKeyValues(var KeyFields: string; var KeyValues: Variant);
begin
  if (CurRec >= 0) and (CurRec < SqlBuffer.Count) then
    InternalFormKeyValues(SqlBuffer[CurRec], False, KeyFields, KeyValues);
end;

{**** Data-link support ****}

{ Get datasource }
function TZDataset.GetDataLinkSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

{ Set datasource }
procedure TZDataset.SetDataLinkSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCyclicLinkError);
  FDataLink.DataSource := Value;
end;

{ Data-link refresh params }
procedure TZDataset.ParamsRequery;
var
  I, N: Integer;
  MasterField: TField;
begin
  if FMasterLink.Active or not FDataLink.Active then Exit;
  if not MasterStateCheck(FDataLink.Dataset) then Exit;

{ Changing parameter values }
  N := 0;
  for I := 0 to ParamCount-1 do
  begin
    MasterField := FDataLink.Dataset.FindField(Params[I].Name);
    if Assigned(MasterField) then
    begin
      Params[I].Value := MasterField.AsVariant;
      Inc(N);
    end;
  end;

  if (N = 0) then Exit;
  if ConvertToSqlEnc(SqlParser.Text) = Query.Sql then Exit;
  Query.Sql := ConvertToSqlEnc(SqlParser.Text);
end;

{ Data-link change event }
procedure TZDataset.RefreshParams;
begin
  if {FMasterLink.Active or} not FDataLink.Active then Exit;
  CheckBrowseMode;
  ParamsRequery;
  ShortRefresh;
  if not (State in [dsInactive]) then
    Resync([]);
end;

{*** Cached updates support ***}

{ Clear updates from cache }
procedure TZDataset.ClearBuffer;
var
  I: Integer;
begin
  { Clear old records and erase marks }
  CacheBuffer.ClearBuffer(False);
  for I := SqlBuffer.Count-1 downto 0 do
  begin
    if SqlBuffer[I].RecordType <> ztDeleted then
      SqlBuffer[I].RecordType := ztUnmodified
    else
      SqlBuffer.Delete(I);
  end;
  { Correct position }
  if CurRec >= SqlBuffer.Count then
    CurRec := SqlBuffer.Count-1;
end;

{ Post updates from buffer }
procedure TZDataset.FlushBuffer;
var
  I, N: Integer;
  OldRec: Integer;
  Error: string;
  Retry: Boolean;
  RecordData: PRecordData;
  CacheData: PRecordData;
  DataAction: TDataAction;
  UpdateAction: TUpdateAction;
begin
  { Check state }
  if not RequestLive then Exit;
  if CacheBuffer.Count = 0 then Exit;
  { Set start values }
  OldRec := CurRec;
  FNewValueState := True;
  Error := '';
  I := 0;
  { Processig of all changed records }
  while I < CacheBuffer.Count do
  begin
    { Define indexes of field buffers }
    Retry := False;
    CacheData := CacheBuffer[I];
    if CacheData.RecordType = ztUnmodified then
    begin
      Inc(I);
      Continue;
    end;
    N := SqlBuffer.SafeIndexOfIndex(CacheData.Index);
    if N < 0 then Continue;
    CurRec := N;
    RecordData := SqlBuffer[N];
    { Process postings }
    try
      { Call UpdateRecordEvent event }
      UpdateAction := uaApplied;
      if Assigned(OnUpdateRecord) then
      begin
        case CacheData.RecordType of
          ztInserted:
            OnUpdateRecord(Self, ukInsert, UpdateAction);
          ztDeleted:
            OnUpdateRecord(Self, ukDelete, UpdateAction);
          ztModified:
            OnUpdateRecord(Self, ukModify, UpdateAction);
        end;
        case UpdateAction of
          uaFail:  DatabaseError(SPostError);
          uaAbort: Abort;
          uaRetry: Retry := True;
        end;
      end;
      { Post changes to the database using UpdateObject }
      if UpdateAction = uaApplied then
      begin
        if Assigned(UpdateObject) then
        begin
          { Update directly thourgh UpdateObject }
          case CacheData.RecordType of
            ztInserted:
              UpdateObject.Apply(ukInsert);
            ztDeleted:
              UpdateObject.Apply(ukDelete);
            ztModified:
              UpdateObject.Apply(ukModify);
          end;
        end else
          { Otherwise put changes using autoform queries }
          FormSqlQuery(CacheData, RecordData);
      end;
    except on E: Exception do
      begin
      { Exception processing }
        CurRec := N;
        if Assigned(OnApplyUpdateError) then
        begin
          { Call OnApplyUpdateError event }
          DataAction := daFail;
          if E is EDatabaseError then
            OnApplyUpdateError(Self, E as EDatabaseError, DataAction);
          CurRec := OldRec;
          FNewValueState := False;
          case DataAction of
            daFail:
              begin
                SqlBuffer.FilterItem(N);
                DatabaseError(E.Message);
              end;
            daAbort: Error := E.Message;
            daRetry: Retry := True;
          end;
        end
        else
        begin
          Error := E.Message;
          Break;
        end;
      end;
    end;
    if Retry then
    begin
      Error := '';
      Continue;
    end;

    { Update record after post }
    if CacheData.RecordType in [ztInserted, ztModified] then
      UpdateAfterPost(CacheData, RecordData);
    { Cancel update record }
    CacheData.RecordType := ztUnmodified;
    if RecordData.RecordType <> ztDeleted then
      RecordData.RecordType := ztUnmodified
    else
      SqlBuffer.Delete(N);

    Inc(I);
  end;
  { Restore old values }
  CurRec := OldRec;
  FNewValueState:= False;
  { Raise occupied database error }
  if Error <> '' then
    DatabaseError(Error)
  else if Assigned(Transaction.OnApplyUpdates) then
    Transaction.OnApplyUpdates(Self);
end;

{CheckContraints}
procedure TZDataset.CheckContraints;
var 
  I, J: Integer;
  FieldDesc: PFieldDesc;
  RValue: Variant;
begin
  {Fields Contraints}
  for I := 0 to FieldCount-1 do
  if not Fields[i].ReadOnly and (Fields[i].CustomConstraint<>'') then
  begin
    FieldDesc := SqlBuffer.SqlFields.FindByField(Fields[i]);
    if not Assigned(FieldDesc) then Continue;
    if FieldDesc.ReadOnly then Continue;
    FCCParser.Equation :=Fields[i].CustomConstraint;
    for J := 0 to FCCParser.VarCount-1 do
      FCCParser.Variables[FCCParser.VarNames[J]] := Fields[i].Value;
    RValue := FCCParser.Evalute;
    if RValue <> True then
       DataBaseErrorFmt(SConstraintFailed,[Fields[i].CustomConstraint]);
  end;

  {Record Contraints}
  for I := 0 to Constraints.Count-1 do
  begin
    if Trim(Constraints[I].ImportedConstraint)<>'' then
    begin
      FCCParser.Equation := Constraints[I].ImportedConstraint;
      for J := 0 to FCCParser.VarCount-1 do
        FCCParser.Variables[FCCParser.VarNames[J]] :=
                  FieldByName(FCCParser.VarNames[J]).Value;
      RValue := FCCParser.Evalute;
      if RValue <> True then
         DataBaseErrorFmt(SConstraintfailed,[Constraints[I].ImportedConstraint]);
    end;

    if Trim(Constraints[I].CustomConstraint)<>'' then
    begin
     FCCParser.Equation := Constraints[I].CustomConstraint;
      for J := 0 to FCCParser.VarCount-1 do
        FCCParser.Variables[FCCParser.VarNames[J]] :=
                  FieldByName(FCCParser.VarNames[J]).Value;
      RValue := FCCParser.Evalute;
      if RValue <> True then
         DataBaseErrorFmt(SConstraintfailed,[Constraints[I].CustomConstraint]);
    end;
  end;
end;

{ Update record after post updates }
procedure TZDataset.UpdateAfterPost(OldData, NewData: PRecordData);
begin
  (*
  {CalcComputedFields}   to do
  {CalcAggrFileds;}      to do
  *)

  if doRefreshAfterPost in Options then
  begin
    if RefreshCurrentRow(NewData) then
      Resync([]);
  end;
end;

{ Commit updates }
procedure TZDataset.Flush;
begin
  FlushBuffer;
  ClearBuffer;
end;

{ Post cached updates }
procedure TZDataset.ApplyUpdates;
begin
  if not Active then Exit;
  if State in [dsEdit, dsInsert] then Post;
  FlushBuffer;
  if not (State in [dsInactive]) then Resync([]);
end;

{ Clear updates buffer }
procedure TZDataset.CommitUpdates;
begin
  CheckBrowseMode;
  ClearBuffer;
end;

{ Rollback all cached updates }
procedure TZDataset.CancelUpdates;
var
  I, N: Integer;
  RecordData: PRecordData;
begin
  if State in [dsEdit, dsInsert] then
    Cancel;
  { Rollback updates }
  for I := SqlBuffer.Count-1 downto 0 do
  begin
    RecordData := SqlBuffer[I];
    if RecordData.RecordType = ztUnmodified then
      Continue;
    if RecordData.RecordType = ztInserted then
      SqlBuffer.Delete(I)
    else begin
      N := CacheBuffer.IndexOfIndex(RecordData.Index);
      if N < 0 then
        DatabaseError(SIntBufferError);
      SqlBuffer.CopyRecord(CacheBuffer[N], RecordData, False);
      RecordData.RecordType := ztUnmodified;
    end;
  end;
  { Clear cache buffer }
  CacheBuffer.ClearBuffer(False);
  { Resync records }
  if not (State in [dsInactive]) then
    Resync([]);
  { Check dataset state }
  if not RequestLive then
    DatabaseError(SROCmdError);
end;

{ Record update type }
function TZDataset.UpdateStatus: TUpdateStatus;
begin
  case SqlBuffer[CurRec].RecordType of
    ztInserted:
      Result := usInserted;
    ztDeleted:
      Result := usDeleted;
    ztModified:
      Result := usModified;
    else
      Result := usUnmodified;
  end;
end;

{ Erase updates for a field }
procedure TZDataset.RevertRecord;
var
  Index: Integer;
  RecordData: PRecordData;
  CacheData: PRecordData;
begin
  { Check state }
  if not RequestLive or (CurRec < 0) or (CurRec >= SqlBuffer.Count) then
    Exit;
  if State in [dsInsert] then
  begin
    Cancel;
    Exit;
  end;
  if State in [dsEdit] then
    Cancel;
  { Rollback changed }
  RecordData := SqlBuffer[CurRec];
  if RecordData.RecordType = ztUnmodified then
    Exit;
  { Find a cached record }
  Index := CacheBuffer.IndexOfIndex(RecordData.Index);
  if Index < 0 then Exit;
  CacheData := CacheBuffer[Index];
  { Copy data back }
  if RecordData.RecordType in [ztInserted] then
    SqlBuffer.Delete(CurRec)
  else begin
    SqlBuffer.CopyRecord(CacheData, RecordData, False);
    RecordData.RecordType := ztUnmodified;
  end;
  CacheBuffer.Delete(Index);
  { Resync records }
  if not (State in [dsInactive]) then
    Resync([]);
end;

{ Check if any updates in a cache buffer }
function TZDataset.GetUpdatesPending: Boolean;
begin
  if State = dsInactive then
    Result := False
  else if State in [dsInsert, dsEdit] then
    Result := True
  else
    Result := (CacheBuffer.Count > 0);
end;

{ Process sql identificators }
function TZDataset.ProcessIdent(Value: string): string;
begin
  Result := Value;
  if Value = '' then Exit;
  case DatabaseType of
    dtPostgreSql:
      if (Value[1] <> '"') and (Pos(' ', Result) > 0) then
        Result := '"' + Value + '"';
    dtMsSql:
      if (Value[1] <> '[') and (Pos(' ', Result) > 0) then
        Result := '[' + Value + ']';
  end;
end;

{ Form where sql statement part }
function TZDataset.FormSqlWhere(Table: string; RecordData: PRecordData): string;
var
  I: Integer;
  FieldList: TFieldList;
  FieldCount: Integer;
  FieldDesc: PFieldDesc;
begin
  { Set start values }
  Result := '';
  { Try to use rowid value }
  if SqlParser.UsedRowId and (SqlParser.Tables.Count > 0)
    and StrCaseCmp(SqlParser.Tables[0], Table) then
  begin
    if (DatabaseType = dtPostgreSql) and (PInteger(@RecordData^.RowId)^ <> 0) then
      Result := 'oid='+RowIdToValue(RecordData^.RowId);
  end;
  if Result = '' then
  begin
    { Get table field keys }
    DefineTableKeys(Table, False, FieldList, FieldCount);
    { Process keys and form where part }
    for I := 0 to FieldCount-1 do
    begin
      FieldDesc := SqlBuffer.SqlFields[FieldList[I]];
      if Result <> '' then
        Result := Result + ' AND ';
      if SqlBuffer.GetFieldNull(FieldDesc, RecordData) then
        Result := Result + ProcessIdent(FieldDesc.Field) + ' IS NULL'
      else
        Result := Result + ProcessIdent(FieldDesc.Field) + '=' +
          FieldValueToSql(SqlBuffer.GetField(FieldDesc, RecordData),
          FieldDesc);
    end;
  end;
  { Finaly form where }
  if Result <> '' then
    Result := ' WHERE ' + Result;
end;

{*** Master-detail links support ***}

{ Get link field names }
function TZDataset.GetLinkFields: string;
begin
  Result := FLinkFields;
end;

{ Update master-detail links }
procedure TZDataset.UpdateLinkFields;
  { Get field count }
  function GetFieldNo(Dataset: TZDataset; Field: string): Integer;
  begin
    DeleteQuotesEx(Field);
    Result := StrToIntDef(Field, -1);
    if (Result >= 0) and (Result < Dataset.SqlBuffer.SqlFields.Count) then
      Exit;
    Result := Dataset.SqlBuffer.SqlFields.IndexOf(
      Dataset.SqlBuffer.SqlFields.FindByAlias(Field));
    if (Result >= 0) and (Result < Dataset.SqlBuffer.SqlFields.Count) then
      Exit;
    Result := -1;
  end;
var
  MasterDataset: TZDataset;
  Buffer, Field, Sign: string;
  FieldNo: Integer;
  FieldList: string;
begin
  { Check state }
  MasterDataset := FMasterLink.Dataset as TZDataset;
  if not Assigned(MasterDataset) or (Trim(FLinkFields) = '') then
    Exit;
  { Set start values }
  SqlBuffer.FilterFieldCount := 0;
  FMasterFieldCount := 0;
  Buffer := FLinkFields;
  { Parse linked fields }
  while Buffer <> '' do
  begin
    { Obtain master field }
    ExtractTokenEx(Buffer, Field);
    FieldNo := GetFieldNo(MasterDataset, Field);
    if FieldNo < 0 then
      DatabaseError(SIncorrectLinks);
    FMasterFields[FMasterFieldCount] := FieldNo;
    { Fill masterlink fields }
    if FieldList <> '' then
      FieldList := FieldList + '; ';
    FieldList := FieldList + Field;
    { Check sign }
    ExtractTokenEx(Buffer, Sign);
    if Sign <> '=' then
      PutbackToken(Buffer, Sign)
    else ExtractTokenEx(Buffer, Field);
    FDetailFields[FMasterFieldCount] := Field;
    if SqlBuffer.SqlFields.Count > 0 then
    begin
      FieldNo := GetFieldNo(Self, Field);
      if FieldNo < 0 then
        DatabaseError(SIncorrectLinks);
      SqlBuffer.FilterFields[FMasterFieldCount] := FieldNo;
    end;
    Inc(FMasterFieldCount);
    { Check delimiter }
    ExtractToken(Buffer, Field);
    if (Field <> ';') and (Field <> ',') then
      PutbackToken(Buffer, Field);
  end;
  { Store values }
  FMasterLink.FieldNames := FieldList;
  if SqlBuffer.SqlFields.Count > 0 then
    SqlBuffer.FilterFieldCount := FMasterFieldCount;
end;

{ Set link field values }
procedure TZDataset.SetLinkFields(Value: string);
begin
  if FLinkFields <> Trim(Value) then
    FLinkFields := Trim(Value);
  SqlBuffer.FilterFieldCount := 0;
end;

{ Get master datasource }
function TZDataset.GetMasterDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;

{ Set master source }
procedure TZDataset.SetMasterDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCyclicLinkError);
  FMasterLink.DataSource := Value;
  SqlBuffer.FilterFieldCount := 0;
end;

{ Master cascade deletes/updates }
procedure TZDataset.MasterCascade;
var
  I: Integer;
  MasterDataset: TZDataset;
  MasterBuffer: PRecordData;
  FieldDesc: PFieldDesc;
  MasterFieldDesc: PFieldDesc;
  OldFilterTypes: TZUpdateRecordTypes;
  OldRecNo: Integer;
  OldRecCount: Integer;
  RecordPtr: PChar;
  Deleted: Boolean;
  Updated: Boolean;
  Values: array[0..MAX_FIELD_COUNT] of Variant;
begin
  { Get old master buffer }
  MasterDataset := FMasterLink.Dataset as TZDataset;
  RecordPtr := MasterDataset.SqlBuffer.GetRealItem(FMasterIndex);
  MasterBuffer := PRecordData(RecordPtr);
  { Check if master record was deleted }
  if MasterBuffer.RecordType in [ztUnmodified{, ztInserted}] then Exit;
  Deleted := MasterBuffer.RecordType = ztDeleted;
  Dec(RecordPtr);
  Deleted := Deleted or (PByte(RecordPtr)^ = ITEM_DELETED);
  { Check if record was updated }
  Updated := False;
  if not Deleted then
  begin
    for I := 0 to FMasterFieldCount-1 do
    begin
      { Get field descriptions }
      MasterFieldDesc := MasterDataset.SqlBuffer.SqlFields[FMasterFields[I]];
      FieldDesc := SqlBuffer.SqlFields[SqlBuffer.FilterFields[I]];
      Values[I] := MasterDataset.SqlBuffer.GetFieldValue(MasterFieldDesc, MasterBuffer);
      Updated := Updated or (Values[I] <> SqlBuffer.GetFieldValue(FieldDesc,
        SqlBuffer.FilterBuffer));
    end;
  end;
  { Exit if nothing change }
  if not Deleted and not Updated then Exit;
  { Store current values }
  DisableControls;
  OldFilterTypes := SqlBuffer.FilterTypes;
  OldRecNo := RecNo;
  try
    { Make normal filter }
    SqlBuffer.FilterTypes := [ztUnmodified, ztModified, ztInserted];
    SqlBuffer.Filter;
    First;
    { Make cascade updates }
    if not Deleted then
      while not Eof do
      begin
        OldRecCount := RecordCount;
        for I := 0 to FMasterFieldCount - 1 do
        begin
          { Get field descriptions }
          FieldDesc := SqlBuffer.SqlFields[SqlBuffer.FilterFields[I]];
          { If field not equal - update them }
          if Values[I] <> FieldDesc.FieldObj.AsVariant then
          begin
            Edit;
            FieldDesc.FieldObj.AsVariant := Values[I];
            Post;
          end;
        end;
        if OldRecCount = RecordCount then
          Next;
      end
    else
     { Make cascade deletes }
      while not Eof do
        Delete;
  finally
    { Restore filtering and sorting }
    if (SqlBuffer.FilterTypes <> OldFilterTypes) or Updated then
    begin
      SqlBuffer.FilterTypes := OldFilterTypes;
      SqlBuffer.Filter;
      SqlBuffer.SortRestore;
    end;
    { Restore old position }
    RecNo := OldRecNo;
    EnableControls;
  end;
end;

{ Define master properties }
procedure TZDataset.MasterDefine;
var
  I: Integer;
  MasterDataset: TZDataset;
  MasterBuffer: PRecordData;
  MasterFieldDesc: PFieldDesc;
  FieldDesc: PFieldDesc;
  FieldValue: Variant;
begin
  if not FMasterLink.Active then
  begin
    FMasterIndex := -1;
    SqlBuffer.FilterFieldCount := 0;
    Exit;
  end;

  if (loCascadeDelete in FLinkOptions) or (loCascadeUpdate in FLinkOptions) then
    if FMasterLink.Dataset.State = dsEdit then Exit;
  if SqlBuffer.FilterFieldCount = 0 then
    UpdateLinkFields;
  if SqlBuffer.SqlFields.Count = 0 then Exit;

  MasterDataset := FMasterLink.Dataset as TZDataset;
  MasterDataset.GetActiveRecBuf(MasterBuffer);
  SqlBuffer.FreeRecord(SqlBuffer.FilterBuffer, True);
  if (MasterDataset.RecordCount > 0) and Assigned(MasterBuffer) then
  begin
    if MasterDataset.RecNo <= 0 then
      FMasterIndex := -2
    else begin
      FMasterIndex := 0;
      if MasterDataset.RecNo < MasterDataset.SqlBuffer.Count then
        FMasterIndex := MasterDataset.SqlBuffer.GetRealIndex(
          MasterDataset.SqlBuffer[MasterDataset.RecNo-1]);
      for I := 0 to SqlBuffer.FilterFieldCount-1 do
      begin
        MasterFieldDesc := MasterDataset.SqlBuffer.SqlFields[FMasterFields[I]];
        FieldValue := MasterDataset.SqlBuffer.GetFieldValue(MasterFieldDesc,
          MasterBuffer);
        FieldDesc := SqlBuffer.SqlFields[SqlBuffer.FilterFields[I]];
        SqlBuffer.SetFieldValue(FieldDesc, FieldValue, SqlBuffer.FilterBuffer);
      end;
    end;
  end else
    FMasterIndex := -1;
end;

{ Check Master Link Mode }
function TZDataset.MasterStateCheck(Dataset: TDataset): Boolean;
begin
  if not (loAlwaysResync in LinkOptions) or (loCascadeUpdate in LinkOptions)
    or (loCascadeDelete in LinkOptions) then
    Result := Dataset.State = dsBrowse
  else
  begin
    Result := True;

    if not (Dataset.State in [dsBrowse, dsEdit, dsInsert]) then
    begin
      Result := False;
      Exit;
    end;
    if Dataset.State <> dsEdit then
      FLinkCheck := False
    else
      if not FLinkCheck then
      begin
        FLinkCheck := True;
        Result := False;
      end;
  end;
end;

{ Master dataset change event }
procedure TZDataset.MasterChanged(Sender: TObject);
begin
  if not FMasterLink.Active or not Active then Exit;
  CheckBrowseMode;
  if FMasterLink.DataSet.State = dsInsert then
    FMasterIndex := -1;
  if not MasterStateCheck(FMasterLink.Dataset) then Exit;

{ Cascade deletes/updates processing }
  if ((loCascadeDelete in FLinkOptions) or (loCascadeUpdate in FLinkOptions))
    and (FMasterIndex >= 0) then
    MasterCascade;

  MasterDefine;
  MasterRequery;
end;

{ Master dataset deactivate event }
procedure TZDataset.MasterDisabled(Sender: TObject);
begin
  First;
  FLinkCheck := False;
  MasterDefine;
  MasterRequery;
  if not (State in [dsInactive]) then Resync([]);
end;

{ Detail query restructure }
procedure TZDataset.MasterRequery;
var
  I: Integer;
  Where: string;
  FieldDesc: PFieldDesc;
  MasterDataset: TZDataset;
  MasterBuffer: PRecordData;
begin
  { Check link state }
  MasterDataset := FMasterLink.Dataset as TZDataset;
  { Define a linked fields values }
  if FMasterIndex = -1 then
    MasterDefine;
  { Refilter detail records }
  if not (loLinkRequery in FLinkOptions) then
  begin
    if SqlParser.ExtraWhere <> '' then
    begin
      SqlParser.ExtraWhere := '';
      Query.Sql := ConvertToSqlEnc(SqlParser.Text);
      ShortRefresh;
    end;
    SqlBuffer.Filter;
  end
  else
  begin
    { Reread detail records }
    Where := '';
    if FMasterLink.Active and Assigned(MasterDataset) then
    begin
      MasterDataset.GetActiveRecBuf(MasterBuffer);
      { Form extra where condition }
      for I := 0 to FMasterFieldCount-1 do
      begin
        FieldDesc := MasterDataset.SqlBuffer.SqlFields[FMasterFields[I]];
        if Where <> '' then Where := Where + ' AND ';
        if not MasterDataset.SqlBuffer.GetFieldNull(FieldDesc, MasterBuffer) then
          Where := Where + FDetailFields[I] + '=' +
            FieldValueToSql(MasterDataset.SqlBuffer.GetField(FieldDesc, MasterBuffer),
              FieldDesc)
        else
          Where := Where + FDetailFields[I] + ' IS NULL';
      end;
    end;
    { Reopen detail query }
    if (SqlParser.ExtraWhere <> Where) then
    begin
      SqlParser.ExtraWhere := Where;
      Query.Sql := ConvertToSqlEnc(SqlParser.Text);
      ShortRefresh;
    end else
      Exit;
  end;
  { Resync records }
  if not (State in [dsInactive]) then
  begin
    First;
    Resync([]);
  end;
end;

{*** Internal fields processing ***}

{ Auto form update sql query }
procedure TZDataset.FormSqlQuery(OldData, NewData: PRecordData);
var
  I: Integer;
  Sql, Where: WideString;
  Fields, Values: string;
  FieldDesc: PFieldDesc;
  FieldValue: string;
  Params: TVarRecArray;
  ParamCount: Integer;
  AffectedRowCount: LongInt;
begin
  { Check tables }
  if SqlParser.Tables.Count = 0 then Exit;
  { Form Sql command }
  Sql := '';
  ParamCount := 0;
  case OldData.RecordType of
    { Form UPDATE statement }
    ztModified:
      begin
        Where := FormSqlWhere(SqlParser.Tables[0], OldData);
        for I := 0 to SqlBuffer.SqlFields.Count-1 do
        begin
          FieldDesc := SqlBuffer.SqlFields[I];
          if FieldDesc.ReadOnly then Continue;
          if not StrCaseCmp(FieldDesc.Table, SqlParser.Tables[0]) then
            Continue;
          FieldValue := SqlBuffer.GetField(FieldDesc, NewData);
          if FieldValue = SqlBuffer.GetField(FieldDesc, OldData) then
            Continue;
          if Sql <> '' then Sql := Sql + ', ';
          if (DatabaseType in [dtInterbase, dtDb2]) and
            (FieldDesc.FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo])
            and not SqlBuffer.GetFieldNull(FieldDesc, NewData) then
          begin
            Sql := Sql + ProcessIdent(FieldDesc.Field) + '=?';
            Inc(ParamCount);
            if DatabaseType = dtInterbase then
              Params[ParamCount-1] :=
                TVarRec(PRecordBlob(@NewData.Bytes[FieldDesc.Offset+1]).Handle)
            else
              Params[ParamCount-1].VPointer :=
                PRecordBlob(@NewData.Bytes[FieldDesc.Offset+1]);
          end
          else if SqlBuffer.GetFieldNull(FieldDesc, NewData) then
            Sql := Sql + ProcessIdent(FieldDesc.Field) + '= NULL'
          else if (DatabaseType = dtOracle)
            and (FieldDesc.FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo])
            and (FieldDesc.BlobType = btExternal) then
          begin
            Sql := Sql + ProcessIdent(FieldDesc.Field)
              + '= EMPTY_' + FieldDesc.TypeName + '()';
          end
          else
            Sql := Sql + ProcessIdent(FieldDesc.Field) + '= '
              + FieldValueToSql(FieldValue, FieldDesc)
        end;
        if Sql <> '' then
          Sql := 'UPDATE ' + ProcessIdent(SqlParser.Tables[0]) + ' SET ' + Sql + Where;
      end;
    { Form INSERT statement }
    ztInserted:
      begin
        Fields := '';
        Values := '';
        for I := 0 to SqlBuffer.SqlFields.Count-1 do
        begin
          FieldDesc := SqlBuffer.SqlFields[I];
          if FieldDesc.ReadOnly then Continue;
          if not StrCaseCmp(FieldDesc.Table, SqlParser.Tables[0]) then
            Continue;
          FieldValue := SqlBuffer.GetField(FieldDesc, NewData);
          if (DatabaseType = dtMsSql) and (FieldDesc.AutoType = atIdentity) and
            (FieldValue = 'NULL') then
            Continue;
          if (DatabaseType = dtDb2) and (FieldDesc.AutoType = atIdentity) and
            (FieldValue = 'NULL') then
            Continue;
          if Fields <> '' then Fields := Fields + ', ';
          Fields := Fields + ProcessIdent(FieldDesc.Field);
          if Values <> '' then Values := Values + ', ';
          if (DatabaseType in [dtInterbase, dtDb2]) and
            (FieldDesc.FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo])
            and not SqlBuffer.GetFieldNull(FieldDesc, NewData) then
          begin
            Values := Values + '?';
            Inc(ParamCount);
            if DatabaseType = dtInterbase then
              Params[ParamCount-1] :=
                TVarRec(PRecordBlob(@NewData.Bytes[FieldDesc.Offset+1]).Handle)
            else
              Params[ParamCount-1].VPointer :=
                PRecordBlob(@NewData.Bytes[FieldDesc.Offset+1]);
          end
          else if SqlBuffer.GetFieldNull(FieldDesc, NewData) then
            Values := Values + 'NULL'
          else if (DatabaseType = dtOracle)
            and (FieldDesc.FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo])
            and (FieldDesc.BlobType = btExternal) then
            Values := Values + 'EMPTY_' + FieldDesc.TypeName + '()'
          else
            Values := Values + FieldValueToSql(FieldValue, FieldDesc)
        end;
        if (Fields <> '') and (Values <> '') then
          Sql := 'INSERT INTO ' + ProcessIdent(SqlParser.Tables[0])
            +' (' + Fields + ') VALUES (' + Values + ')';
      end;
    { Form Delete statement }
    ztDeleted:
      begin
        Where := FormSqlWhere(SqlParser.Tables[0], OldData);
        if Where <> '' then
          Sql := 'DELETE FROM ' + ProcessIdent(SqlParser.Tables[0]) + Where;
      end;
  end;
  { Execute sql statement }
  if Sql <> '' then
  begin
    if ParamCount = 0 then
      AffectedRowCount := TransactObj.ExecSql(Sql)
    else
      AffectedRowCount := TransactObj.ExecSqlParams(Sql, Params, ParamCount);

    { Key deleted mast refresh dataset }
    if (AffectedRowCount = 0) and  (OldData.RecordType = ztModified) then
      DatabaseError(SPostError);
  end;
end;

{*** Blobs fields support ***}

{ Close blobs }
procedure TZDataset.CloseBlob(Field: TField);
begin
end;

{ Make different assignents for record }
procedure TZDataset.CopyRecord(SqlBuffer: TSqlBuffer; Source,
  Dest: PRecordData);
begin
end;

{ Make different disallocations for record }
procedure TZDataset.FreeRecord(SqlBuffer: TSqlBuffer; Value: PRecordData);
begin
end;

{ Create a blob stream }
function TZDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  BlobObj: TDirBlob;
  RecordData: PRecordData;
  RecordBlob: PRecordBlob;
  FieldDesc: PFieldDesc;
begin
  { Set start values }
  Result := nil;
  FieldDesc := SqlBuffer.SqlFields.FindByField(Field);
  { Check field desc }
  if FieldDesc = nil then Exit;
  { If empty buffer create a free stream end exit }
  if not GetActiveRecBuf(RecordData) then
  begin
    Result := TMemoryStream.Create;
    Exit;
  end;
  RecordBlob := PRecordBlob(@RecordData.Bytes[FieldDesc.Offset+1]);
  { Process internal blob }
  if RecordBlob.BlobType = btInternal then
    Result := TZMemoStream.Create(Field as TBlobField, Mode)
  else
  { Process external blob }
  begin
    BlobObj := Query.CreateBlobObject;
    if Assigned(BlobObj) then
    begin
      BlobObj.Handle := RecordBlob.Handle;
      Result := TZBlobStream.Create(Field as TBlobField, Mode, BlobObj);
    end;
  end;
end;

{*** Other methods ***}

{ File storing data methods }

{ Save data to stream }
procedure TZDataset.SaveToStream(Stream: TStream);
var
  I, J: Integer;
  Buffer: string;
  RecordData: PRecordData;
  FieldDesc: PFieldDesc;
begin
  QueryRecords(True);
  for I := 0 to SqlBuffer.Count-1 do
  begin
    if not CheckRecordByFilter(I) then Continue;
    RecordData := SqlBuffer[I];
    Buffer := '';
    if I > 0 then Buffer := Buffer + #13#10;
    for J := 0 to SqlBuffer.SqlFields.Count-1 do
    begin
      FieldDesc := SqlBuffer.SqlFields[J];
      if J > 0 then Buffer := Buffer + ',';
      if not (FieldDesc.FieldType in [ftInteger, ftSmallInt, ftFloat,
        ftAutoInc, ftCurrency{$IFNDEF VER100}, ftLargeInt{$ENDIF}, ftBCD])
        and not SqlBuffer.GetFieldNull(FieldDesc, RecordData) then
        Buffer := Buffer + '''' +
          ZSqlTypes.StringToSql(SqlBuffer.GetField(FieldDesc, RecordData)) + ''''
      else
        Buffer := Buffer + SqlBuffer.GetField(FieldDesc, RecordData);
    end;
    Stream.Write(PChar(Buffer)^, Length(Buffer));
  end;
end;

{ Load data from stream }
procedure TZDataset.LoadFromStream(Stream: TStream);
  { Read string from a stream }
  function ReadStr(Stream: TStream): string;
  var
    Length, Capacity: Integer;
    Buffer: PChar;
    Chr: Char;
  begin
    Result := '';
    Buffer := PChar(Result);
    Length := 0;
    Capacity := 0;
    while Stream.Position < Stream.Size do
    begin
      Stream.Read(Chr, 1);
      case Chr of
        #13: Continue;
        #10: Break;
        else
          begin
            if Length >= Capacity then
            begin
              Inc(Capacity, 512);
              SetLength(Result, Capacity);
              Buffer := PChar(Result);
              Inc(Buffer, Length);
            end;
            Buffer^ := Chr;
            Inc(Buffer);
            Inc(Length);
          end;
      end;
    end;
    SetLength(Result, Length);
  end;

var
  I, Index: Integer;
  Value, Buffer: string;
  FieldDesc: PFieldDesc;
  RecordData: PRecordData;
  ActiveData: PRecordData;
  Bookmark: TBookmark;
  KeyFields: string;
  KeyValues: Variant;
begin
  DisableControls;
  Bookmark := GetBookmark;
  RecordData := PRecordData(AllocRecordBuffer);
  try
    while Stream.Position < Stream.Size do
    begin
      { Read string }
      Buffer := Trim(ReadStr(Stream));
      if Buffer = '' then Continue;
      SqlBuffer.FreeRecord(RecordData, True);
      { Fill record buffer }
      for I := 0 to SqlBuffer.SqlFields.Count-1 do
      begin
        FieldDesc := SqlBuffer.SqlFields[I];
        Value := Trim(StrTokEx(Buffer, ',;'));
        if (Value <> '') and not StrCaseCmp(Value, 'NULL') then
        begin
          if Value[1] = '''' then
          begin
            DeleteQuotes(Value);
            Value := ZSqlTypes.SqlToString(Value);
          end;
          SqlBuffer.SetField(FieldDesc, Value, RecordData);
        end;
      end;
      { Find exist field }
      InternalFormKeyValues(RecordData, True, KeyFields, KeyValues);
      Index := -1;
      if KeyFields <> '' then
        Index := InternalLocate(KeyFields, KeyValues, []);
      { Insert or update record }
      if Index >= 0 then
      begin
        RecNo := Index + 1;
        Edit;
      end else
        Append;
      GetActiveRecBuf(ActiveData);
      { Fill fields' values }
      for I := 0 to SqlBuffer.SqlFields.Count-1 do
      begin
        FieldDesc := SqlBuffer.SqlFields[I];
        Value := SqlBuffer.GetField(FieldDesc, RecordData);
        if not StrCaseCmp(Value, 'NULL') then
          SqlBuffer.SetField(FieldDesc, Value, ActiveData);
      end;
      { Post updates }
      if State in [dsInsert, dsEdit] then Post;
    end;
  finally
    FreeRecordBuffer(PChar(RecordData));
    GotoBookmark(Bookmark);
    FreeBookmark(Bookmark);
    EnableControls;
  end;
end;

{ Save data to file }
procedure TZDataset.SaveToFile(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ Load data from file }
procedure TZDataset.LoadFromFile(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ Convert string to sql value }
function TZDataset.StringToSql(Value: string): string;
begin
  Result := Query.StringToSql(Value);
end;

{ Convert parameter to sql value }
function TZDataset.ParamToSql(Value: Variant): string;
begin
  case VarType(Value) of
    varEmpty, varNull:
       Result := 'NULL';
    varSmallint, varInteger, varByte:
       Result := IntToStr(Value);
    varSingle, varDouble, varCurrency:
       Result := FloatToStrEx(VarAsType(Value, varDouble));
    varDate:
      begin
        if DatabaseType = dtInterbase then
          Result := DateTimeToIbDate(Value)
        else Result := DateTimeToSqlDateEx(Value);
        Result := '''' + Result + '''';
      end;
    varBoolean:
      if DatabaseType = dtMySql then
      begin
        if Value then Result := '''Y'''
        else Result := '''N''';
      end
      else
      if DatabaseType = dtMsSql then
      begin
        if Value then Result := '1'
        else Result := '0';
      end
      else
      begin
        if Value then Result := '''t'''
        else Result := '''f''';
      end;
    else
      Result := '''' + StringToSql(VarAsType(Value, varString)) + '''';
  end;
end;

{ Convert variant to sql value }
function TZDataset.ValueToSql(Value: Variant): string;
begin
  Result := ConvertToSqlEnc(ParamToSql(Value));
end;

{ Convert field value to sql value }
function TZDataset.FieldValueToSql(Value: string; FieldDesc: PFieldDesc): string;
var
  DecPos: Integer;
begin
  if FieldDesc.FieldType in [ftInteger, ftSmallInt, ftCurrency, ftFloat, ftAutoInc
    {$IFNDEF VER100}, ftLargeInt{$ENDIF}, ftBCD] then
  begin
    DecPos := Pos(DecimalSeparator, Value);
    if DecPos > 0 then
      Value[DecPos] := '.';
    Result := Value;
  end 
  else if (DatabaseType = dtMsSql) and (FieldDesc.FieldType in [ftBytes, ftBlob]) then
    Result := BytesToSql(Value)
  else if (DatabaseType = dtMySql) and (FieldDesc.FieldType = ftBlob) then
    Result := '''' + StringToSql(Value) + ''''
  else if FieldDesc.FieldType in [ftString, ftMemo] then
    Result := '''' + ConvertToSqlEnc(StringToSql(Value)) + ''''
  else
    Result := '''' + Value + '''';
end;

{ Convert from windows to server encoding }
function TZDataset.ConvertToSqlEnc(Value: string): string;
begin
  Result := Convert(Value, etNone, DatabaseObj.Encoding);
end;

{ Convert from server to windows encoding }
function TZDataset.ConvertFromSqlEnc(Value: string): string;
begin
  Result := Convert(Value, DatabaseObj.Encoding, etNone);
end;

{ Convert RowId value to string }
function TZDataset.RowIdToValue(Value: TRowId): string;
begin
  Result := '0';
end;

{ Convert string value to RowId }
function TZDataset.ValueToRowId(Value: string): TRowId;
begin
  FillChar(Result, SizeOf(TRowId), 0);
end;

{ Check is table exist }
function TZDataset.CheckTableExistence(Table: string): Boolean;
begin
  Result := True;
end;

{$IFDEF WITH_IPROVIDER}

{ TZDataset IProviderSupport }

{ Start new transaction }
procedure TZDataset.PSStartTransaction;
begin
  if Assigned(FTransact) then
    FTransact.Connect;
end;

{ End current transaction }
procedure TZDataset.PSEndTransaction(Commit: Boolean);
begin
  if Assigned(FTransact) then
  begin
    if Commit then FTransact.Commit
    else FTransact.Rollback;
  end;
end;

{ Return an quote symbol }
function TZDataset.PSGetQuoteChar: string;
begin
  Result := '''';
end;

{ Is datasets query based on Sql? }
function TZDataset.PSIsSqlBased: Boolean;
begin
  Result := True;
end;

{ Is dataset supports Sql? }
function TZDataset.PSIsSqlSupported: Boolean;
begin
  Result := True;
end;

{ Reset dataset }
procedure TZDataset.PSReset;
begin
  inherited PSReset;
  if Active then
  begin
    Close;
    Open;
  end;
end;

{ Execute statement as Sql command }
procedure TZDataset.PSExecute;
begin
  ExecSql;
end;

{ Update database exception }
function TZDataset.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
begin
  Result := inherited PSGetUpdateException(E, Prev);
end;

{ Get table name }
function TZDataset.PSGetTableName: string;
begin
  Result := GetTableNameFromSql(SqlParser.Text)
end;

function TZDataset.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
var
  UpdateAction: TUpdateAction;
  Sql: string;
  Params: TParams;

  procedure AssignParams(DataSet: TDataSet; Params: TParams);
  var
    I: Integer;
    Old: Boolean;
    Param: TParam;
    PName: string;
    Field: TField;
    Value: Variant;
  begin
    for I := 0 to Params.Count - 1 do
    begin
      Param := Params[I];
      PName := Param.Name;
      Old := StrCmpBegin(UpperCase(PName), 'OLD_'); {do not localize}
      if Old then System.Delete(PName, 1, 4);
      Field := DataSet.FindField(PName);
      if not Assigned(Field) then Continue;
      if Old then Param.AssignFieldValue(Field, Field.OldValue) else
      begin
        Value := Field.NewValue;
        if VarIsEmpty(Value) then Value := Field.OldValue;
        Param.AssignFieldValue(Field, Value);
      end;
    end;
  end;

begin
  Result := False;
  if Assigned(FOnUpdateRecord) then
  begin
    UpdateAction := uaFail;
    if Assigned(FOnUpdateRecord) then
    begin
      FOnUpdateRecord(Delta, UpdateKind, UpdateAction);
      Result := UpdateAction = uaApplied;
    end;
  end
  else
  if Assigned(UpdateObject) then
  begin
    case UpdateKind of
      ukModify: Sql := UpdateObject.ModifySql.Text;
      ukDelete: Sql := UpdateObject.DeleteSql.Text;
      ukInsert: Sql := UpdateObject.InsertSql.Text;
    end;
    Sql := TZUpdateSql(UpdateObject).GetSql(UpdateKind).Text;
    if Sql <> '' then
    begin
      Params := TParams.Create;
      try
        Params.ParseSql(Sql, True);
        AssignParams(Delta, Params);
        if PSExecuteStatement(Sql, Params) = 0 then
          DatabaseError(LoadStr(SConnectError));
        Result := True;
      finally
        Params.Free;
      end;
    end;
  end;
end;

{ ADO Support }

{ Get query parameters }
function TZDataset.PSGetParams: TParams;
begin
  Result := Params;
end;

{ Set query parameters }
procedure TZDataset.PSSetParams(AParams: TParams);
begin
  if AParams.Count > 0 then
    Params.Assign(AParams);
  Close;
end;

function TZDataset.PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs;
begin
  Result := GetIndexDefs(IndexDefs, IndexTypes);
end;

procedure TZDataset.PSGetAttributes(List: TList);
begin
  inherited PSGetAttributes(List);
end;

{ Get default query order }
function TZDataset.PSGetDefaultOrder: TIndexDef;
  function GetIdx(IdxType: TIndexOption): TIndexDef;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to IndexDefs.Count - 1 do
      if IdxType in IndexDefs[I].Options then
      try
        Result := IndexDefs[I];
        GetFieldList(nil, Result.Fields);
        Break;
      except
        Result := nil;
      end;
  end;
var
  DefIdx: TIndexDef;
begin
  DefIdx := nil;
  IndexDefs.Update;
  try
    if IndexName <> '' then
      DefIdx := IndexDefs.Find(IndexName)
    else if IndexFieldNames <> '' then
      DefIdx := IndexDefs.FindIndexForFields(IndexFieldNames);
    if Assigned(DefIdx) then
      GetFieldList(nil, DefIdx.Fields);
  except
    DefIdx := nil;
  end;
  if not Assigned(DefIdx) then
    DefIdx := GetIdx(ixPrimary);
  if not Assigned(DefIdx) then
    DefIdx := GetIdx(ixUnique);
  if Assigned(DefIdx) then
  begin
    Result := TIndexDef.Create(nil);
    Result.Assign(DefIdx);
  end else
    Result := nil;
end;

{ Define a table keys }
function TZDataset.PSGetKeyFields: string;
var
  I, Pos: Integer;
  IndexFound: Boolean;
begin
  Result := inherited PSGetKeyFields;
  if Result = '' then
  begin
    IndexFound := False;
    IndexDefs.Update;
    for i := 0 to IndexDefs.Count - 1 do
      if ixUnique in IndexDefs[I].Options then
      begin
        Result := IndexDefs[I].Fields;
        IndexFound := (FieldCount = 0);
        if not IndexFound then
        begin
          Pos := 1;
          while Pos <= Length(Result) do
          begin
            IndexFound := FindField(ExtractFieldName(Result, Pos)) <> nil;
            if not IndexFound then Break;
          end;
        end;
        if IndexFound then Break;
      end;
    if not IndexFound then
      Result := '';
  end;
end;

{$ENDIF}

{**************** TZQueryDataLink implementation **********}

{ Class constructor }
constructor TZQueryDataLink.Create(AQuery: TZDataset);
begin
  inherited Create;
  FQuery := AQuery;
end;

{ On activate method }
procedure TZQueryDataLink.ActiveChanged;
begin
  if FQuery.Active then FQuery.RefreshParams;
end;

{ On record change method }
procedure TZQueryDataLink.RecordChanged(Field: TField);
begin
  if ((Field = nil) or (loAlwaysResync in FQuery.LinkOptions))
    and FQuery.Active then
    FQuery.RefreshParams;
end;

{ TZBCDField }

constructor TZBCDField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftBCD);
  size := 4;
end;

class procedure TZBCDField.CheckTypeSize(Value: Integer);
begin
  { No need to check as the base type is currency, not BCD }
end;


function TZBCDField.GetAsCurrency: Currency;
begin
  if not GetValue(Result) then
    Result := 0;
end;

function TZBCDField.GetAsString: string;
var
  C: System.Currency;
begin
  if GetValue(C) then
    Result := CurrToStr(C)
  else
    Result := '';
end;

function TZBCDField.GetAsVariant: Variant;
var
  C: System.Currency;
begin
  if GetValue(C) then
    Result := C
  else
    Result := Null;
end;

{$IFDEF VER130ABOVE}
function TZBCDField.GetDataSize: Integer;
{$ELSE}
function TZBCDField.GetDataSize: Word;
{$ENDIF}
begin
  Result := SizeOf(System.Currency);
end;

end.