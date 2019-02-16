{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{          Field Link Designer Property Editor           }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZLinkProp;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses
{$IFNDEF LINUX}
  Windows, Messages, StdCtrls, Controls, ExtCtrls,
{$ELSE}
  QStdCtrls, QControls, QExtCtrls,
{$ENDIF}
{$IFNDEF VERCLX} DsgnIntf, {$ELSE} DesignIntf, DesignEditors, {$ENDIF}
  SysUtils, Classes, Graphics, Forms, Dialogs, DB, ZQuery;

type
  { Link fields editor dialog }
  TfrmLinkFields = class(TForm)
    pnMain: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    lblDetailFields: TLabel;
    lblMasterFields: TLabel;
    lbxDetail: TListBox;
    lbxMaster: TListBox;
    btnAdd: TButton;
    btnAuto: TButton;
    lblJoinedFields: TLabel;
    lbxJoined: TListBox;
    btnDelete: TButton;
    btnClear: TButton;
    cbxCascadeUpdates: TCheckBox;
    cbxCascadeDeletes: TCheckBox;
    cbxLinkRequery: TCheckBox;
    cbxAlwaysResync: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbxDetailClick(Sender: TObject);
    procedure lbxDetailKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbxJoinedClick(Sender: TObject);
    procedure lbxJoinedKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnAutoClick(Sender: TObject);
  private
    FDetail, FMaster: TStrings;
    FJDetail, FJMaster: TStrings;

    procedure FillFieldList(Dataset: TDataset; List: TStrings);
    procedure FillListBox(List: TStrings; ListBox: TListBox);
    procedure FillJoinListBox(MasterList, DetailList: TStrings; ListBox: TListBox);
    function  GetLinkFields: string;
    procedure SetLinkFields(Value: string);
    procedure RefreshListBoxes;
    function  GetLinkOptions: TZLinkOptions;
    procedure SetLinkOptions(Value: TZLinkOptions);
  public
    function Execute(Dataset: TZDataset): Boolean;
  end;

  { Link fields property editor }
  TZLinkFieldsProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

uses ZToken, ZSqlExtra, ZDbaseConst, ZExtra;

{$R *.dfm}

{************* TfrmLinkFields implementation *************}

{ On create form event }
procedure TfrmLinkFields.FormCreate(Sender: TObject);
begin
  FDetail := TStringList.Create;
  FMaster := TStringList.Create;
  FJDetail := TStringList.Create;
  FJMaster:= TStringList.Create;
end;

{ On destroy form event }
procedure TfrmLinkFields.FormDestroy(Sender: TObject);
begin
  FJMaster.Free;
  FJDetail.Free;
  FMaster.Free;
  FDetail.Free;
end;

{ Show dialog box and make selection }
function TfrmLinkFields.Execute(Dataset: TZDataset): Boolean;
var
  MDataset: TZDataset;
begin
  { Check conditions }
  if not Assigned(Dataset.MasterSource) then
    DatabaseError('Isn''t master datasource defined');
  if not (Dataset.MasterSource.Dataset is TZDataset) then
    DatabaseError('Master dataset isn''t TZDataset type');

  { Generate lists }
  MDataset := TZDataset(Dataset.MasterSource.Dataset);
  if not Dataset.Active and (Dataset.FieldCount = 0) then
  begin
    Dataset.FieldDefs.Clear;
    Dataset.FieldDefs.Update;
  end;
  if not MDataset.Active and (MDataset.FieldCount = 0) then
  begin
    MDataset.FieldDefs.Clear;
    MDataset.FieldDefs.Update;
  end;

  FillFieldList(Dataset, FDetail);
  FillFieldList(MDataset, FMaster);
  SetLinkFields(Dataset.LinkFields);
  SetLinkOptions(Dataset.LinkOptions);
  { Fill controls }
  RefreshListBoxes;
  btnAdd.Enabled := False;
  btnDelete.Enabled := False;

  Result := False;
  if ShowModal = mrOk then
  begin
    Dataset.LinkOptions := GetLinkOptions;
    Dataset.LinkFields := GetLinkFields;
  end;
end;

{ Refresh all listboxes }
procedure TfrmLinkFields.RefreshListBoxes;
begin
  FillListBox(FDetail, lbxDetail);
  FillListBox(FMaster, lbxMaster);
  FillJoinListBox(FJMaster, FJDetail, lbxJoined);
  lbxJoinedClick(Self);
  lbxDetailClick(Self);
end;

{ Fill list with Dataset Fields }
procedure TfrmLinkFields.FillFieldList(Dataset: TDataset; List: TStrings);
var
  I: Integer;
begin
  List.Clear;
  if Dataset.FieldCount = 0 then
  begin
    for I := 0 to Dataset.FieldDefs.Count-1 do
      List.Add(Dataset.FieldDefs[I].Name);
  end
  else
  begin
    for I := 0 to Dataset.FieldCount-1 do
      List.Add(Dataset.Fields[I].FieldName);
  end;
end;

{ Fill listbox with joined fields from lists }
procedure TfrmLinkFields.FillJoinListBox(MasterList, DetailList: TStrings;
  ListBox: TListBox);
var
  I: Integer;
begin
  ListBox.Items.BeginUpdate;
  ListBox.Items.Clear;
  for I := 0 to MasterList.Count-1 do
    if I < DetailList.Count then
      ListBox.Items.Add(DetailList[I] + ' -> ' + MasterList[I]);
  ListBox.Items.EndUpdate;
end;

{ Fill listbox with fields from list }
procedure TfrmLinkFields.FillListBox(List: TStrings; ListBox: TListBox);
var
  I: Integer;
begin
  ListBox.Items.BeginUpdate;
  ListBox.Items.Clear;
  for I := 0 to List.Count-1 do
    if Integer(List.Objects[I]) = 0 then
      ListBox.Items.Add(List[I]);
  ListBox.Items.EndUpdate;
end;

{ Create result link fields list }
function TfrmLinkFields.GetLinkFields: string;

  function GetFieldName(Value: string): string;
  begin
    Result := Trim(Value);
    if Pos(' ', Result) > 0 then
      Result := '"' + Result + '"';
  end;

var
  I: Integer;
begin
  Result := '';
  for I := 0 to FJMaster.Count-1 do
    if I < FJDetail.Count then
    begin
      if Result <> '' then
        Result := Result + ';';
      Result := Result + GetFieldName(FJMaster[I]) + '='
        + GetFieldName(FJDetail[I]);
    end;
end;

{ Set defined link fields }
procedure TfrmLinkFields.SetLinkFields(Value: string);
var
  Token, MasterField, DetailField: string;
  TokenType: TTokenType;
  N: Integer;
begin
  FJMaster.Clear;
  FJDetail.Clear;

  while Value <> '' do
  begin
    MasterField := '';
    TokenType := ExtractTokenEx(Value, Token);
    case TokenType of
      ttAlpha: MasterField := Token;
      ttString:
        begin
          DeleteQuotes(Token);
          MasterField := Token;
        end;
      else
        DatabaseError(SIncorrectLinks);
    end;

    ExtractTokenEx(Value, Token);
    if Token <> '=' then
      DatabaseError(SIncorrectLinks);

    DetailField := '';
    TokenType := ExtractTokenEx(Value, Token);
    case TokenType of
      ttAlpha: DetailField := Token;
      ttString:
        begin
          DeleteQuotes(Token);
          DetailField := Token;
        end;
      else
        DatabaseError(SIncorrectLinks);
    end;
    ExtractToken(Value, Token);
    if (Token <> ';') and (Token <> ',') then
      PutbackToken(Value, Token);

    N := CaseIndexOf(FMaster, MasterField);
    if N < 0 then
      DatabaseError(SIncorrectLinks);
    FMaster.Objects[N] := TObject($ffff);
    FJMaster.Add(FMaster[N]);

    N := CaseIndexOf(FDetail, DetailField);
    if N < 0 then
      DatabaseError(SIncorrectLinks);
    FDetail.Objects[N] := TObject($ffff);
    FJDetail.Add(FDetail[N]);
  end;
end;

{ Get master-detail link options }
function TfrmLinkFields.GetLinkOptions: TZLinkOptions;
begin
  Result := [];
  if cbxCascadeUpdates.Checked then
    Result := Result + [loCascadeUpdate];
  if cbxCascadeDeletes.Checked then
    Result := Result + [loCascadeDelete];
  if cbxLinkRequery.Checked then
    Result := Result + [loLinkRequery];
  if cbxAlwaysResync.Checked then
    Result := Result + [loAlwaysResync];
end;

{ Set master-detail link options }
procedure TfrmLinkFields.SetLinkOptions(Value: TZLinkOptions);
begin
  cbxCascadeUpdates.Checked := (loCascadeUpdate in Value);
  cbxCascadeDeletes.Checked := (loCascadeDelete in Value);
  cbxLinkRequery.Checked    := (loLinkRequery in Value);
  cbxAlwaysResync.Checked   := (loAlwaysResync in Value);
end;

{ If selected item enable delete button }
procedure TfrmLinkFields.lbxJoinedClick(Sender: TObject);
begin
  btnDelete.Enabled := (lbxJoined.ItemIndex >= 0);
end;

{ The same }
procedure TfrmLinkFields.lbxJoinedKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  lbxJoinedClick(Self);
end;

{ If selected item enable add button }
procedure TfrmLinkFields.lbxDetailKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  lbxDetailClick(Self);
end;

{ The same }
procedure TfrmLinkFields.lbxDetailClick(Sender: TObject);
begin
  btnAdd.Enabled := (lbxMaster.ItemIndex >= 0) and
   (lbxDetail.ItemIndex >= 0);
end;

{ Clear all linked fields }
procedure TfrmLinkFields.btnClearClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FDetail.Count-1 do
    FDetail.Objects[I] := nil;
  for I := 0 to FMaster.Count-1 do
    FMaster.Objects[I] := nil;
  FJDetail.Clear;
  FJMaster.Clear;
  RefreshListBoxes;
end;

{ Delete selected link }
procedure TfrmLinkFields.btnDeleteClick(Sender: TObject);
var
  N, M: Integer;
begin
  N := lbxJoined.ItemIndex;
  if N < 0 then Exit;

  if N < FJDetail.Count then
  begin
    M := CaseIndexOf(FDetail, FJDetail[N]);
    FJDetail.Delete(N);
    if M < FDetail.Count then
      FDetail.Objects[M] := nil;
  end;

  if N < FJMaster.Count then
  begin
    M := CaseIndexOf(FMaster, FJMaster[N]);
    FJMaster.Delete(N);
    if M < FMaster.Count then
      FMaster.Objects[M] := nil;
  end;

  RefreshListBoxes;
end;

{ Add new link }
procedure TfrmLinkFields.btnAddClick(Sender: TObject);
var
  N: Integer;
begin
  if (lbxDetail.ItemIndex < 0) or (lbxMaster.ItemIndex < 0) then
    Exit;

  N := CaseIndexOf(FDetail, lbxDetail.Items[lbxDetail.ItemIndex]);
  FJDetail.Add(FDetail[N]);
  FDetail.Objects[N] := TObject($ffff);

  N := CaseIndexOf(FMaster, lbxMaster.Items[lbxMaster.ItemIndex]);
  FJMaster.Add(FMaster[N]);
  FMaster.Objects[N] := TObject($ffff);

  RefreshListBoxes;
end;

{ Auto add field links }
procedure TfrmLinkFields.btnAutoClick(Sender: TObject);
var
  I, J: Integer;
begin
  btnClearClick(Self);

  for I := 0 to FDetail.Count-1 do
    for J := 0 to FMaster.Count-1 do
    begin
      if FMaster.Objects[J] <> nil then
        Continue;
      if StrCaseCmp(FDetail[I], FMaster[J]) then
      begin
        FJDetail.Add(FDetail[I]);
        FDetail.Objects[I] := TObject($ffff);
        FJMaster.Add(FMaster[J]);
        FMaster.Objects[J] := TObject($ffff);
      end;
    end;

  RefreshListBoxes;
end;

{******************** TDbPropertyEditor implementation ************* }

procedure TZLinkFieldsProperty.Edit;
begin
  with TfrmLinkFields.Create(Application) do
    try
      Execute(TZDataset(GetComponent(0)));
//      SetValue(TZDataset(GetComponent(0)).LinkFields);
    finally
      Free;
    end;
end;

function TZLinkFieldsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.
