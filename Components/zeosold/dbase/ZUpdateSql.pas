{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{             Unidatabase UpdateSql component            }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZUpdateSql;

interface

{$R *.dcr}

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses
  {$IFNDEF LINUX} Windows, {$IFDEF VER100} DbTables, {$ENDIF} {$ENDIF}
  SysUtils, {$IFDEF VERCLX}Variants,{$ENDIF} Classes, DB, ZExtra, ZToken,
  ZSqlTypes, ZSqlItems, ZSqlBuffer;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { TZUpdateSql }
  TZUpdateSql = class(TComponent)
  private
    FDeleteSql, FInsertSql, FModifySql: TStrings;
    FDeleteQuery, FInsertQuery, FModifyQuery: string;
    FDataset: TDataset;
    FParamCheck: Boolean;
    FParams: TParams;

    function GetSql(UpdateKind: TUpdateKind): TStrings;
    procedure SetSql(UpdateKind: TUpdateKind; Value: TStrings);
    function GetParamsCount: Word;
    function GetParamValue(Name: string): string;
    procedure SetParamsList(Value: TParams);
    procedure SetParams(UpdateKind: TUpdateKind);
    procedure SetParamCheck(Value: Boolean);

    procedure SetDeleteSql(Value: TStrings);
    procedure SetInsertSql(Value: TStrings);
    procedure SetModifySql(Value: TStrings);

    procedure UpdateParams;
  protected
    {$IFNDEF VER100}
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Apply(UpdateKind: TUpdateKind);
    procedure ExecSql(UpdateKind: TUpdateKind);

    property DataSet: TDataset read FDataset write FDataset;
    property Sql[UpdateKind: TUpdateKind]: TStrings read GetSql write SetSql;
    property ParamCount: Word read GetParamsCount;
  published
    property DeleteSql: TStrings read FDeleteSql write SetDeleteSql;
    property InsertSql: TStrings read FInsertSql write SetInsertSql;
    property ModifySql: TStrings read FModifySql write SetModifySql;
    property Params: TParams read FParams write SetParamsList stored False;
    property ParamCheck: Boolean read FParamCheck write SetParamCheck default False;
  end;

implementation

uses ZQuery, ZDBaseConst, ZSqlParser;

{****************** TZUpdateSql implementation *************}

{ Class constructor }
constructor TZUpdateSql.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDeleteSql := TStringList.Create;
  FInsertSql := TStringList.Create;
  FModifySql := TStringList.Create;
  FParams := TParams.Create{$IFNDEF VER100}(Self){$ENDIF};
  FParamCheck := False;
end;

{ Class destructor }
destructor TZUpdateSql.Destroy;
begin
  FParams.Free;
  FDeleteSql.Free;
  FInsertSql.Free;
  FModifySql.Free;
  inherited Destroy;
end;

{ Internal method to set XXXSql property }
procedure TZUpdateSql.SetSql(UpdateKind: TUpdateKind; Value: TStrings);
begin
  case UpdateKind of
    ukModify: FModifySql.Assign(Value);
    ukInsert: FInsertSql.Assign(Value);
    ukDelete: FDeleteSql.Assign(Value);
  end;
  UpdateParams;
end;

{ Internal method to get XXXSql property }
function TZUpdateSql.GetSql(UpdateKind: TUpdateKind): TStrings;
begin
  case UpdateKind of
    ukModify: Result := FModifySql;
    ukInsert: Result := FInsertSql;
    else Result := FDeleteSql;
  end;
end;

{ Get parameters count }
function TZUpdateSql.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

{ Set param checking and update parameters }
procedure TZUpdateSql.SetParamCheck(Value: Boolean);
begin
  if FParamCheck <> Value then
  begin
    FParamCheck := Value;
    if FParamCheck and (FParams.Count = 0) then
      UpdateParams;
  end;
end;

{ Set Sql query parameters list }
procedure TZUpdateSql.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

{$IFNDEF VER100}
{ Define properties of Sql params }
procedure TZUpdateSql.DefineProperties(Filer: TFiler);
  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TZUpdateSql(Filer.Ancestor).FParams)
    else
      Result := (FParams.Count > 0);
  end;
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

{ Read Sql params from DFM file }
procedure TZUpdateSql.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

{ Write Sql params into DFM file }
procedure TZUpdateSql.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;
{$ENDIF}

{ Get field value by parameter name }
function TZUpdateSql.GetParamValue(Name: string): string;
var
  Dataset: TZDataset;
  TempName: string;
  IsNew: Boolean;
  Field: TField;
  FieldValue: Variant;
begin
  if Name = ':' then
  begin
    Result := ':';
    Exit;
  end;

  TempName := Name;
  if StrCmpBegin('OLD_', UpperCase(Name)) then
  begin
    IsNew := False;
    Name := Copy(Name, 5, Length(Name)-4);
  end
  else
  begin
    IsNew := True;
    if StrCmpBegin('NEW_', UpperCase(Name)) then
      Name := Copy(Name, 5, Length(Name) - 4);
  end;

  if not Assigned(Self.Dataset) then
    DatabaseError(SDatasetNotDefined);

  Dataset := TZDataset(Self.Dataset);
  if DataSet.FindField(Name) <> nil then
  begin
    Field := Dataset.FieldByName(Name);

    if not Assigned(Field) then
      DatabaseError('Parameters can not be defined');

    if IsNew then
      FieldValue := Field.NewValue
    else FieldValue := Field.OldValue;

    Result := Dataset.ValueToSql(FieldValue);
  end
  else
{$IFNDEF VER100}
  if FParamCheck and (FParams.FindParam(TempName) <> nil) then
    Result := Dataset.ValueToSql(FParams.FindParam(TempName).Value);
{$ELSE}
  if FParamCheck and (FParams.ParamByName(TempName) <> nil) then
    Result := Dataset.ValueToSql(FParams.ParamByName(TempName).Value);
{$ENDIF}
end;

{ Set value of Delete Sql statement }
procedure TZUpdateSql.SetDeleteSql(Value: TStrings);
begin
  FDeleteSql.Assign(Value);
  UpdateParams;
end;

{ Set value of Insert Sql statement }
procedure TZUpdateSql.SetInsertSql(Value: TStrings);
begin
  FInsertSql.Assign(Value);
  UpdateParams;
end;

{ Set value of Modify Sql statement }
procedure TZUpdateSql.SetModifySql(Value: TStrings);
begin
  FModifySql.Assign(Value);
  UpdateParams;
end;

{ Replace parameters and execute a query }
procedure TZUpdateSql.Apply(UpdateKind: TUpdateKind);
begin
  SetParams(UpdateKind);
  ExecSql(UpdateKind);
end;

{ Execute a query }
procedure TZUpdateSql.ExecSql(UpdateKind: TUpdateKind);
var
  Text: string;
begin
  if not Assigned(Dataset) then
    DatabaseError(SDatasetNotDefined);
  if not Assigned((Dataset as TZDataset).Transaction) then
    DatabaseError(STransactNotDefined);

  case UpdateKind of
    ukModify: Text := FModifyQuery;
    ukInsert: Text := FInsertQuery;
    ukDelete: Text := FDeleteQuery;
  end;

  if Text = '' then
    DatabaseError(SUpdateSqlIsEmpty);

  (Dataset as TZDataset).Transaction.BatchExecSql(Text);
end;

{ Replace parameters }
procedure TZUpdateSql.SetParams(UpdateKind: TUpdateKind);
var
  IsWhere: Boolean;
  EqualPos: Integer;
  Buffer, Token, Text: string;
begin
  Buffer := Sql[UpdateKind].Text;
  Text := '';
  EqualPos := 0;
  IsWhere := False;

  while Buffer <> '' do
  begin
    if Buffer[1] in [' ', #9, #10, #13] then
      Text := Text + ' ';
    ExtractToken(Buffer, Token);
    if (Token = ':') and (Buffer[1] <> ':') then
    begin
      ExtractToken(Buffer, Token);
      DeleteQuotes(Token);
      Token := GetParamValue(Token);
      if StrCaseCmp(Token, 'NULL') and IsWhere and (EqualPos > 0) then
      begin
        Delete(Text, EqualPos, Length(Text) - EqualPos + 1);
        Text := Text + ' IS ' + Token;
      end else
        Text := Text + Token;
    end
    else
    begin
      if Token = '=' then EqualPos := Length(Text) + 1
      else EqualPos := 0;

      if StrCaseCmp(Token, 'WHERE') then
        IsWhere := True
      else if StrCaseCmp(Token, 'ORDER') or StrCaseCmp(Token, 'HAVING')
        or StrCaseCmp(Token, 'GROUP') then
        IsWhere := False;

      Text := Text + Token;
    end;
  end;

  FDeleteQuery := '';
  FInsertQuery := '';
  FModifyQuery := '';
  case UpdateKind of
    ukModify: FModifyQuery := Text;
    ukInsert: FInsertQuery := Text;
    ukDelete: FDeleteQuery := Text;
  end;
end;

{ Update params from sql statements }
procedure TZUpdateSql.UpdateParams;
begin
  if FParamCheck then
  begin
    FParams.Clear;
    CreateParams(FParams, FDeleteSql.Text, ':');
    CreateParams(FParams, FInsertSql.Text, ':');
    CreateParams(FParams, FModifySql.Text, ':');
  end;
end;

end.
