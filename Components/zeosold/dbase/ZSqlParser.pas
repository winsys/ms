{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                 SQL Statements Parser                  }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZSqlParser;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses Classes, DB, ZSqlTypes, ZSqlItems {$IFDEF VER100}, DbTables {$ENDIF}
  {$IFDEF VERCLX}, Variants{$ENDIF};

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { SQL statements parser class }
  TSqlParser = class (TObject)
  private
    FDataset: TDataset;
    FSql: TStrings;
    FTables: TStrings;
    FAliases: TStrings;
    FSqlFields: TSqlFields;
    FSqlIndices: TSqlIndices;
    FIsSelect: Boolean;
    FText: string;
    FExtraWhere: string;
    FSelectStartPos: Integer;
    FWhereStartPos: Integer;
    FWherePos: Integer;
    FExtraOrderBy: string;
    FOrderPos: Integer;
    FUsedRowId: Boolean;

    procedure SetSql(Value: TStrings);
    procedure SetDataset(Value: TDataset);
    function GetText: string;

    procedure QueryChanged(Sender: TObject);
    function  ProcessAttribute(Value: string): string;
    procedure DefineField(Table, Field, Alias: string);
    procedure DefineTableFields(Table: string);
  protected
    procedure ProcessParams(Params: TParams; EscapeChar: string;
      ProcessAsIs: Boolean);
  public
    constructor Create(Dataset: TDataset);
    destructor Destroy; override;

    function  ExtraFilter: string;
    procedure UpdateText;
    procedure DefineTableDefs;
    procedure UpdateIndexDefs(IndexDefs: TIndexDefs);
    procedure Clear;

    property Dataset: TDataset read FDataset write SetDataset;
    property SqlFields: TSqlFields read FSqlFields;
    property SqlIndices: TSqlIndices read FSqlIndices;
    property Tables: TStrings read FTables;
    property Aliases: TStrings read FAliases;
    property Sql: TStrings read FSql write SetSql;

    property ExtraWhere: string read FExtraWhere write FExtraWhere;
    property ExtraOrderBy: string read FExtraOrderBy write FExtraOrderBy;
    property UsedRowId: Boolean read FUsedRowId;

    property Text: string read GetText;
    property IsSelect: Boolean read FIsSelect;
  end;

{ Extra functions }

{ Create params by Sql statement }
procedure CreateParams(List: TParams; Value: string; EscapeChar: Char);

implementation

uses SysUtils, ZExtra, ZSqlExtra, ZToken, ZQuery, ZSqlScript;

{****************** TSqlParser class implementatoin *************}

{ Class constructor }
constructor TSqlParser.Create(Dataset: TDataset);
begin
  FDataset := Dataset;
  FSql := TStringList.Create;
  TStringList(FSql).OnChange := QueryChanged;
  FSqlFields := TSqlFields.Create;
  FSqlIndices := TSqlIndices.Create;
  FTables := TStringList.Create;
  FAliases := TStringList.Create;
end;

{ Class destructor }
destructor TSqlParser.Destroy;
begin
  FSql.Free;
  FSqlIndices.Free;
  FSqlFields.Free;
  FTables.Free;
  FAliases.Free;
end;

{ Set new dataset }
procedure TSqlParser.SetDataset(Value: TDataset);
begin
  FDataset := Value;
end;

{ Set new Sql statement }
procedure TSqlParser.SetSql(Value: TStrings);
begin
  FSql.BeginUpdate;
  try
    FSql.Assign(Value);
  finally
    FSql.EndUpdate;
  end;
end;

{ Get sql text statement }
function TSqlParser.GetText: string;
begin
  UpdateText;
  Result := FText;
end;


{ OnChange event for Sql query list }
procedure TSqlParser.QueryChanged(Sender: TObject);
var
  Dataset: TZDataset;
  AParams: TParams;
  AMacros: TParams;
begin
  Dataset := TZDataset(Self.Dataset);
  Dataset.Close;
  if Assigned(Dataset) and Dataset.ParamCheck then
  begin
    { Refresh dataset params }
    AParams := TParams.Create;
    AMacros := TParams.Create;
    try
      AMacros.Assign(Dataset.Macros);
      AParams.Assign(Dataset.Params);

      Dataset.Macros.Clear;
      Dataset.Params.Clear;

      CreateParams(Dataset.Params, Sql.Text, ':');
      CreateParams(Dataset.Macros, Sql.Text, Dataset.MacroChar);

      { Update field defs }
      if not Dataset.DefaultFields then
      begin
        Dataset.FieldDefs.Clear;
      {$IFNDEF VER100}
        Dataset.FieldDefs.Updated := False;
      {$ENDIF}
      end;
      { Clear index defs }
      Dataset.IndexDefs.Clear;
      Dataset.IndexDefs.Updated := False;

      if Dataset.Macros.Count > 0 then
        Dataset.Macros.AssignValues(AMacros);

      if Dataset.Params.Count > 0 then
        Dataset.Params.AssignValues(AParams);
    finally
      AMacros.Free;
      AParams.Free;
    end;
  end;
  { Clear internal defs }
  SqlFields.Clear;
  SqlIndices.Clear;
end;

{ Update sql text with dataset params and macros }
procedure TSqlParser.ProcessParams(Params: TParams; EscapeChar: string;
  ProcessAsIs: Boolean);
var
  Token, Temp, Value: string;
  ParamValue: Variant;
  Dataset: TZDataset;
begin
  Dataset := TZDataset(Self.Dataset);

  Temp := '';
  while FText <> '' do
  begin
    if (Temp <> '') and (FText[1] in [' ', #9]) then
      Temp := Temp + ' ';
    ExtractLowToken(FText, Token);

    if Token = EscapeChar then
    begin
      ExtractLowToken(FText, Token);
      if Token <> EscapeChar then
      begin
        DeleteQuotesEx(Token);
        ParamValue := Params.ParamValues[Token];
        if ProcessAsIs then
          case VarType(Value) of
            varEmpty, varNull:
              Value := '';
            varString, varOleStr:
              Value := ParamValue;
          end
        else
          Value := Dataset.ParamToSql(ParamValue);
      end else
        Value := EscapeChar;
      Temp := Temp + Value;
    end else
      Temp := Temp + Token;
  end;
  FText := Temp;
end;

function TSqlParser.ExtraFilter: string;
begin
  with DataSet as TZDataset do
    if (doSqlFilter in Options) and Filtered and (Trim(Filter) <> '') then
      Result := Filter
  else Result := '';
end;

{ Update sql text if changes occured }
procedure TSqlParser.UpdateText;
var
  Dataset: TZDataset;
  Select, From, StrWhere: string;
begin
  Dataset := TZDataset(Self.Dataset);
  FText := FSql.Text;

  FIsSelect := DefineSqlPos(FText, Dataset.DatabaseType, FSelectStartPos,
    FWhereStartPos, FWherePos, FOrderPos);

  if (Dataset.DatabaseType = dtPostgreSql) and (doUseRowId in Dataset.Options)
    and (FSelectStartPos > 0) and not Dataset.ReadOnly then
  begin
    SplitSelect(FText, Dataset.DatabaseType, Select, From);
    Tables.Clear;
    Aliases.Clear;
    ExtractTables(From, Tables, Aliases);
    if (Tables.Count > 0) and Dataset.CheckTableExistence(Tables[0]) then
    begin
      Insert(' '+Aliases[0]+'.oid,', FText, FSelectStartPos);
      FUsedRowid := True;
    end;
  end
  else FUsedRowId := False;

  if FIsSelect then
  begin
    StrWhere := FExtraWhere;
    if (StrWhere <> '') and (ExtraFilter <> '') then
      StrWhere := StrWhere + ' AND ' + ExtraFilter
    else if StrWhere = '' then
      StrWhere := ExtraFilter;

    FText := ComposeSelect(FText, StrWhere, FExtraOrderBy, FWhereStartPos,
      FWherePos, FOrderPos);
  end;

  if Dataset.MacroCheck then
    ProcessParams(Dataset.Macros, Dataset.MacroChar, True);
  if Dataset.ParamCheck then
    ProcessParams(Dataset.Params, ':', doParamsAsIs in Dataset.Options);
end;

{ Process attributes according databaset type }
function TSqlParser.ProcessAttribute(Value: string): string;
var
  Dataset: TZDataset;
begin
  Dataset := TZDataset(Self.Dataset);
  Result := Value;
  if Value = '' then Exit;
  case Dataset.DatabaseType of
    dtPostgreSql, dtMsSql:
      if Value[1] = '"' then DeleteQuotes(Result)
      else Result := LowerCase(Result);
  end;
end;

{ Define and normalize description of the field }
procedure TSqlParser.DefineField(Table, Field, Alias: string);
var
  I: Integer;
  FieldDesc: PFieldDesc;
  Temp: string;
begin
{ Correct table name }
  I := CaseIndexOf(Aliases, Table);
  if I >= 0 then Table := Tables[I];
{ Correct alias and name }
  if Alias = '' then Alias := Field;
{ Search in exists fields }
  FieldDesc := SqlFields.FindByName(Table, Field);
  if (FieldDesc = nil) or (FieldDesc.Alias <> '') then Exit;
{ Count all duplicates }
  if SqlFields.FindByAlias(Alias) <> nil then
  begin
    I := 1;
    repeat
      Temp := Alias + '_' + IntToStr(I);
      Inc(I);
    until SqlFields.FindByAlias(Temp) = nil;
    Alias := Temp;
  end;
  FieldDesc.Alias := Alias;
end;

{ Define and normalize description of the field }
procedure TSqlParser.DefineTableFields(Table: string);
var
  I: Integer;
  TempTable: string;
begin
{ Correct table name }
  I := CaseIndexOf(Aliases, Table);
  if I >= 0 then TempTable := Tables[I]
  else TempTable := Table;
{ Fill fields descriptions }
  for I := 0 to SqlFields.Count-1 do
  begin
    if (Table = '') or StrCaseCmp(SqlFields[I].Table, TempTable) then
      DefineField(Table, SqlFields[I].Field, SqlFields[I].Alias);
  end;
end;

{ Define field names in a query }
procedure TSqlParser.DefineTableDefs;
label NextLabel;
var
  I: Integer;
  Query, Token, Table, Field, Alias: string;
  Select, Temp, From: string;
  Dataset: TZDataset;
begin
  Dataset := TZDataset(Self.Dataset);
  Tables.Clear;
  Aliases.Clear;
  SqlFields.Clear;
  SqlIndices.Clear;
  if not IsSelect then Exit;

  Query := FText;
  SplitSelect(Query, Dataset.DatabaseType, Select, From);

{ Fill all fields of the query tables }
  ExtractTables(From, Tables, Aliases);
  for I := 0 to Tables.Count-1 do
  begin
    Dataset.AddTableFields(Tables[I], SqlFields);
    Dataset.AddTableIndices(Tables[I], SqlFields, SqlIndices);
  end;

  { Escape select keywords }
  while True do
  begin
    Token := StrTok(Select, ' '#13#10);
    Temp := UpperCase(Token);
    if (Temp = 'DISTINCT') or (Temp = 'ALL') or (Temp = 'DISTINCTROW') then
      Continue;
    if (Dataset.DatabaseType = dtMySql) and ((Temp = 'STRAIGNT_JOIN')
      or (Temp = 'SQL_SMALL_RESULT') or (Temp = 'SQL_BIG_RESULT')
      or (Temp = 'SQL_BUFFER_RESULT') or (Temp = 'HIGH_PRIORITY')) then
      Continue;
    Select := Token + Select;
    Break;
  end;
{ Field process cycle }
  while Select <> '' do
  begin
    Token := StrTokEx(Select, ' ,'#9#13#10);

    if Token = '*' then
    begin
{ All fields of tables }
      for I := 0 to Tables.Count-1 do
        DefineTableFields(Tables[I]);
      Break;
    end
    else
    begin
      if Pos('.', Token) > 0 then
      begin
{ If contain complex name }
        Table := StrTokEx(Token, '.');
        Token := StrTokEx(Token, '.');
      end
      else
        Table := '';
      Field := ProcessAttribute(Token);

      if Field = '*' then
      begin
        DefineTableFields(Table);
      end
      else
      begin
        ExtractToken(Select, Token);
        if StrCaseCmp(Token, 'AS') or (Token = '=') then
          Alias := ProcessAttribute(StrTokEx(Select, ' ,'#9#13#10))
        else begin
          PutbackToken(Select, Token);
          Alias := Field;
        end;
      end;
      DefineField(Table, Field, Alias);
    end;

NextLabel:
    repeat
      ExtractToken(Select, Token);
    until (Select = '') or (Token = ',');
  end;
end;

{ Clear parser def contents }
procedure TSqlParser.Clear;
begin
  ExtraWhere := '';
  ExtraOrderBy := '';
  Tables.Clear;
  Aliases.Clear;
end;

{ Update IndexDefs }
procedure TSqlParser.UpdateIndexDefs(IndexDefs: TIndexDefs);
var
  I, J: Integer;
  FieldDesc: PFieldDesc;
  IndexDesc: PIndexDesc;
  KeyType: TKeyType;
  FieldList: string;
  Options: TIndexOptions;
begin
  IndexDefs.Clear;

  for I := 0 to SqlIndices.Count - 1 do
  begin
    IndexDesc := SqlIndices[I];
    KeyType := IndexDesc.KeyType;
    FieldList := '';
    for J := 0 to IndexDesc.FieldCount-1 do
    begin
      FieldDesc := SqlFields.FindByName(IndexDesc.Table, IndexDesc.Fields[J]);
      if (FieldDesc <> nil) and (FieldDesc.Alias <> '') then
      begin
        if FieldList <> '' then
          FieldList := FieldList + ',';
        FieldList := FieldList + FieldDesc.Alias;
      end else
        KeyType := ktIndex;
    end;

    if FieldList <> '' then
    begin
      case KeyType of
        ktPrimary: Options := [ixPrimary, ixUnique];
        ktUnique:  Options := [ixUnique];
        else       Options := [];
      end;
      if IndexDesc.SortType = stDescending then
        Options := Options + [ixDescending];
      IndexDefs.Add(IndexDesc.Name, FieldList, Options);
    end;
  end;
end;

{***************** Extra functions implementation ***************}

{ Create params by Sql statement }
procedure CreateParams(List: TParams; Value: string; EscapeChar: Char);
var
  Token: string;
begin
  if not Assigned(List) then Exit;
  while Value <> '' do
  begin
    ExtractLowToken(Value, Token);
    if Token = EscapeChar then
    begin
      ExtractLowToken(Value, Token);
      DeleteQuotes(Token);
      {$IFNDEF VER100}
      if List.FindParam(Token) = nil then
      {$ELSE}
      if List.ParamByName(Token) = nil then
      {$ENDIF}
        List.CreateParam(ftUnknown, Token, ptUnknown);
    end;
  end;
end;

end.
