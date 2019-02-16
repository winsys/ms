{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{            SQL Fields and Indices Collections          }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZSqlItems;

interface

uses DB, ZList, ZSqlTypes;

{$INCLUDE ../Zeos.inc}

type
  { Index key's types }
  TKeyType = (ktNone, ktIndex, ktUnique, ktPrimary);

  { Sorting type }
  TSortType = (stNone, stAscending, stDescending);

  { Autoupdated field's types }
  TAutoType = (atNone, atAutoInc, atTimestamp, atIdentity, atGenerated);

  { Field name string }
  TFieldName = string[MAX_NAME_LENGTH];

  { Sql field description structure }
  TFieldDesc = packed record
    { General info }
    Table: TFieldName;          // Table name
    Field: TFieldName;          // Field name
    Alias: TFieldName;          // Query column name
    TypeName: ShortString;      // Column type name
    Index: Integer;             // Index in the query (reserved)
    Length: Integer;            // Field length
    Decimals: Integer;          // Decimal scale
    AutoType: TAutoType;        // Auto update type
    IsNull: Boolean;            // Is field null
    Default: ShortString;       // Field default value
    BlobType: TBlobType;        // Blob type
    ReadOnly: Boolean;          // Is field ReadOnly
    { Local Info }
    FieldObj: TField;           // Linked field
    DataSize: Integer;          // Field content size
    Offset: Integer;            // Field content offset
    BlobNo: Integer;            // Blob content index
    { Optimize Info }
    FieldType: TFieldType;      // Field type
    FieldNo: Integer;           // Field index in query
  end;
  PFieldDesc = ^TFieldDesc;

  { Fields collection of SQL query }
  TSqlFields = class(TZItemList)
  private
    function GetItem(Index: Integer): PFieldDesc;
  public
    constructor Create;

    function Add(Table, Field, Alias, TypeName: string;
      FieldType: TFieldType; Length, Decimals: Integer; AutoType: TAutoType;
      IsNull, ReadOnly: Boolean; Default: string; BlobType: TBlobType): PFieldDesc;
    function AddDesc(FieldDesc: PFieldDesc): PFieldDesc;
    function AddField(Field: TField): PFieldDesc;

    function FindByName(Table, Field: string): PFieldDesc;
    function FindByAlias(Alias: string): PFieldDesc;
    function FindByField(Field: TField): PFieldDesc;

    property Items[Index: Integer]: PFieldDesc read GetItem; default;
  end;

  { Internal fields processing class }
  TIndexDesc = packed record
    Table: TFieldName;          // Table name
    Name: TFieldName;           // Index name
    Fields: array[0..MAX_INDEX_FIELDS-1] of TFieldName;
                                // Fields list
    FieldCount: Integer;        // Fields count
    KeyType: TKeyType;          // Key mode
    SortType: TSortType;        // Sorting mode
  end;
  PIndexDesc = ^TIndexDesc;

  { Buffer records class }
  TSqlIndices = class(TZItemList)
  private
    function GetItem(Index: Integer): PIndexDesc;
  public
    constructor Create;
    procedure AddIndex(Name, Table, Fields: string; KeyType: TKeyType;
      SortType: TSortType);
    function FindByField(Table, Field: string): PIndexDesc;
    function FindByName(Name: string): PIndexDesc;

    property Items[Index: Integer]: PIndexDesc read GetItem; default;
  end;

implementation

uses SysUtils, ZToken, ZExtra;

{*************** TSqlFields class implementation **************}

{ Class constructor }
constructor TSqlFields.Create;
begin
  inherited Create(SizeOf(TFieldDesc));
end;

{ Get field item }
function TSqlFields.GetItem(Index: Integer): PFieldDesc;
begin
  if (Index < 0) or (Index >= Count) then
    Error('List Index Error at %d', Index);
  Result := PFieldDesc(List^[Index]);
end;

{ Add new field description }
function TSqlFields.Add(Table, Field, Alias, TypeName: string;
  FieldType: TFieldType; Length, Decimals: Integer; AutoType: TAutoType;
  IsNull, ReadOnly: Boolean; Default: string; BlobType: TBlobType): PFieldDesc;
begin
  Result := inherited Add;
  Result.Table := Table;
  Result.Field := Field;
  Result.Alias := Alias;
  Result.TypeName := TypeName;
  Result.FieldType := FieldType;
  Result.Length := Length;
  Result.Decimals := Decimals;
  Result.AutoType := AutoType;
  Result.IsNull := IsNull;
  Result.ReadOnly := ReadOnly;
  if Result.AutoType = atNone then
    Result.Default := Default
  else
    Result.Default := '';
  Result.BlobType := BlobType;
  Result.FieldObj := nil;
end;

{ Add new field description with another description }
function TSqlFields.AddDesc(FieldDesc: PFieldDesc): PFieldDesc;
begin
  with FieldDesc^ do
    Result := Add(Table, Field, Alias, TypeName, FieldType, Length, Decimals,
      AutoType, IsNull, ReadOnly, Default, BlobType);
end;

{ Add new field description with Field Object }
function TSqlFields.AddField(Field: TField): PFieldDesc;
begin
  Result := inherited Add;
  Result.Table := '';
  Result.Field := '';
  Result.Alias := Field.FieldName;
  Result.TypeName := '';
  Result.FieldType := Field.DataType;
  Result.Length := Field.Size;
  Result.Decimals := 0;
  Result.AutoType := atNone;
  Result.IsNull := not Field.Required;
  Result.ReadOnly := Field.ReadOnly;
  Result.Default := '';
  Result.BlobType := btInternal;
  Result.FieldObj := Field;
end;

{ Find field desc by table and field name }
function TSqlFields.FindByName(Table, Field: string): PFieldDesc;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count-1 do
    if ((Table = '') or StrCaseCmp(Items[I].Table, Table))
      and StrCaseCmp(Items[I].Field, Field) then
    begin
      Result := Items[I];
      Exit;
    end;
end;

{ Find field desc by field alias }
function TSqlFields.FindByAlias(Alias: string): PFieldDesc;
var
  I: Integer;
begin
  Result := nil;
  Alias := UpperCase(Alias);
  for I := 0 to Count-1 do
    if StrCaseCmp(Items[I].Alias, Alias) then
    begin
      Result := Items[I];
      Exit;
    end;
end;

{ Find field desc by TField }
function TSqlFields.FindByField(Field: TField): PFieldDesc;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count-1 do
    if Items[I].FieldObj = Field then
    begin
      Result := Items[I];
      Exit;
    end;
end;

{************* TSqlIndices class implementation ***********}

{ Class constructor }
constructor TSqlIndices.Create;
begin
  inherited Create(SizeOf(TIndexDesc));
end;

{ Get index item }
function TSqlIndices.GetItem(Index: Integer): PIndexDesc;
begin
  if (Index < 0) or (Index >= Count) then
    Error('List Index Error at %d', Index);
  Result := PIndexDesc(List^[Index]);
end;

{ Add new index }
procedure TSqlIndices.AddIndex(Name, Table, Fields: string;
  KeyType: TKeyType; SortType: TSortType);
var
  Item: PIndexDesc;
  Token: string;
begin
  Item := FindByName(Name);
  if not Assigned(Item) then
    Item := PIndexDesc(Add);
  Item.Name  := Name;
  Item.Table := Table;
  Item.KeyType  := KeyType;
  Item.SortType := SortType;
  while Fields <> '' do
  begin
    Token := Trim(StrTokEx(Fields,';,'));
    DeleteQuotes(Token);
    if (Token <> '') and (Item.FieldCount < MAX_INDEX_FIELDS) then
    begin
      Item.Fields[Item.FieldCount] := Token;
      Item.FieldCount := Item.FieldCount + 1;
    end;
  end;
end;

{ Find index by field }
function TSqlIndices.FindByField(Table, Field: string): PIndexDesc;
var
  I, J: Integer;
  MaxKey: TKeyType;
begin
  MaxKey := ktNone;
  Result := nil;
  for I := 0 to Count-1 do
  begin
    if StrCaseCmp(Items[I].Table, Table) then
      with Items[I]^ do
        for J := 0 to FieldCount - 1 do
          if StrCaseCmp(Fields[J], Field) and (MaxKey < KeyType) then
          begin
            Result := Items[I];
            MaxKey := KeyType;
          end;
    if MaxKey = ktPrimary then Exit;
  end;
end;

{ Find index by name }
function TSqlIndices.FindByName(Name: string): PIndexDesc;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count-1 do
  begin
    if StrCaseCmp(Items[I].Name, Name) then
    begin
      Result := Items[I];
      Exit;
    end;
  end;
end;

end.
