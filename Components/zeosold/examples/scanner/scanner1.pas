unit Scanner1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ZScanner, ZToken, ZSqlScanner, ZSqlTypes;

type
  TfrmMain = class(TForm)
    memSql: TMemo;
    btnStart: TButton;
    btnGetToken: TButton;
    btnGetTokenEx: TButton;
    memResult: TMemo;
    cbxShowComment: TCheckBox;
    cbxShowEol: TCheckBox;
    cbxShowString: TCheckBox;
    cbxShowType: TCheckBox;
    cbxShowKeyword: TCheckBox;
    btnClearResult: TButton;
    cbDatabase: TComboBox;
    Label1: TLabel;
    btnStatement: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGetTokenClick(Sender: TObject);
    procedure cbxShowCommentClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnGetTokenExClick(Sender: TObject);
    procedure btnClearResultClick(Sender: TObject);
    procedure btnStatementClick(Sender: TObject);
  private
    FScanner: TZScanner;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cbDatabase.ItemIndex := 0;
  btnStartClick(Self);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FScanner) then FScanner.Free;
end;

function TokenToString(TokenType: Integer): string;
begin
  case TokenType of
    tokComment:  Result := 'Comment';
    tokKeyword:  Result := 'Keyword';
    tokType:     Result := 'Type';
    tokIdent:	 Result := 'Ident';

    tokOperator: Result := 'Operator';
    tokBrace:    Result := 'Brace';
    tokSeparator: Result := 'Separator';
    tokEol:      Result := 'EOL';

    tokInt:      Result := 'Integer';
    tokFloat:    Result := 'Float';
    tokString:   Result := 'String';
    tokBool:     Result := 'Boolean';

    tokEof:      Result := 'EOF';
    else         Result := 'Unknown';
  end;
end;

procedure TfrmMain.btnGetTokenClick(Sender: TObject);
begin
  FScanner.Lex;
  memResult.Lines.Add(Format('Type: %d - %s, Token: %s, Pos: %d, Line: %d',
    [FScanner.TokenType, TokenToString(FScanner.TokenType), FScanner.Token,
    FScanner.Position, FScanner.LineNo]));
end;

procedure TfrmMain.cbxShowCommentClick(Sender: TObject);
begin
  if Visible then
  begin
    FScanner.ShowComment := cbxShowComment.Checked;
    FScanner.ShowEol := cbxShowEol.Checked;
    FScanner.ShowString := cbxShowString.Checked;
    FScanner.ShowType := cbxShowType.Checked;
    FScanner.ShowKeyword := cbxShowKeyword.Checked;
  end;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  { Sets new scanner depending on value chosen on checkbox }
  if Assigned(FScanner) then
    FScanner.Free;

  if cbDatabase.Text = 'PostgreSQL' then
    FScanner := CreateSqlScanner(dtPostgreSql)
  else if cbDatabase.Text = 'Interbase' then
    FScanner := CreateSqlScanner(dtInterbase)
  else if cbDatabase.Text = 'MSSQL' then
    FScanner := CreateSqlScanner(dtMsSql)
  else if cbDatabase.Text = 'MySQL' then
    FScanner := CreateSqlScanner(dtMySql)
  else if cbDatabase.Text = 'Oracle' then
    FScanner := CreateSqlScanner(dtOracle)
  else if cbDatabase.Text = 'C' then
    FScanner := TZCScanner.Create
  else if cbDatabase.Text = 'Pascal' then
    FScanner := TZPasScanner.Create
  else
    FScanner := TZScanner.Create;

  FScanner.Buffer := memSql.Lines.Text;
  cbxShowComment.Checked := FScanner.ShowComment;
  cbxShowEol.Checked := FScanner.ShowEol;
  cbxShowString.Checked := FScanner.ShowString;
  cbxShowType.Checked := FScanner.ShowType;
  cbxShowKeyword.Checked := FScanner.ShowKeyword;
  FScanner.Buffer := memSql.Lines.Text;
end;

procedure TfrmMain.btnGetTokenExClick(Sender: TObject);
begin
  memResult.Lines.Add(Format('Next Token... Type: %d - %s, Token: %s, Pos: %d, Line: %d',
    [FScanner.NextTokenType, TokenToString(FScanner.NextTokenType),
    FScanner.NextToken, FScanner.NextPosition, FScanner.NextLineNo]));
end;

procedure TfrmMain.btnClearResultClick(Sender: TObject);
begin
  memResult.Lines.Clear;
end;

procedure TfrmMain.btnStatementClick(Sender: TObject);
var
  Query: string;
  Pos, Len, Line: Integer;
begin
  if FScanner is TZSqlScanner then
  begin
    Query := (FScanner as TZSqlScanner).ExtractStatement(Pos, Len, Line);
    memResult.Lines.Add(Format('Extracted statement... Pos: %d, Len: %d, Line: %d',
      [Pos, Len, Line]));
    memResult.Lines.Add(Query);
  end;
end;

end.
