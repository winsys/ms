{*******************************************************}
{                                                       }
{ TSBatchSQL - the component is given "as is".          }
{ As a basis for creation of a component has served     }
{ TSQLScrip from RXLib (ver. 2.75) - www.rxlib.com.     }
{ Copyright (c) 2001 Sergey Merkuriev			}
{ Contact E-Mail: abbat@rambler.ru			}
{                                                       }
{*******************************************************}

unit SVBatchSQL;

interface

uses Bde, Windows, forms, Classes, SysUtils, DB, DBTables, StrUtils, BdeUtils, ZMySqlQuery,
     ZMySqlCon, ZMySqlTr;

  const
   DefaultTermChar  = '/';
   TrueExpr = '0=0';

  type
  TSBatchAction = (saFail, saAbort, saRetry, saIgnore, saContinue);

  TSBatchErrorEvent = procedure(Sender: TObject; E: EDatabaseError;
    LineNo, StatementNo: Integer; var Action: TSBatchAction) of object;

  TSBatchSQL = class(TComponent)
  private
    FSQL: TStrings;
    FParams: TParams;
    FQuery: TZMySqlQuery;
    FSemicolonTerm: Boolean;
    FIgnoreParams: Boolean;
    FTerm: Char;
    FBeforeExec: TNotifyEvent;
    FAfterExec: TNotifyEvent;
    FOnScriptError: TSBatchErrorEvent;
    function GetText: string;
    procedure CreateParams(List: TParams; const Value: PChar);
    procedure QueryChanged(Sender: TObject);
    procedure SetQuery(Value: TStrings);
    procedure SetParamsList(Value: TParams);
    function GetParamsCount: Cardinal;
    function GetZDatabase: TZMySqlDataBase;
    procedure SetZDatabase(const Value: TZMySqlDataBase);
    function GetZTransact: TZMySqlTransact;
    procedure SetZTransact(const Value: TZMySqlTransact);
  protected
    procedure CheckExecQuery(LineNo, StatementNo: Integer);
    procedure ExecuteScript(StatementNo: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecSQL;
    procedure ExecStatement(StatementNo: Integer);
    function ParamByName(const Value: string): TParam;
    property Text: string read GetText;
    property ParamCount: Cardinal read GetParamsCount;
  published
    property Database: TZMySqlDataBase read GetZDatabase write SetZDatabase;
    property Transaction: TZMySqlTransact read GetZTransact write SetZTransact;
    property SemicolonTerm: Boolean read FSemicolonTerm write FSemicolonTerm default True;
    property Term: Char read FTerm write FTerm default DefaultTermChar;
    property SQL: TStrings read FSQL write SetQuery;
    property Params: TParams read FParams write SetParamsList;
    property BeforeExec: TNotifyEvent read FBeforeExec write FBeforeExec;
    property AfterExec: TNotifyEvent read FAfterExec write FAfterExec;
    property OnScriptError: TSBatchErrorEvent read FOnScriptError write FOnScriptError;
  end;

  procedure CreateQueryParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);

  procedure Register;

implementation

function NameDelimiter(C: Char; Delims: TCharSet): Boolean;
begin
  Result := (C in [' ', ',', ';', ')', #13, #10]) or (C in Delims);
end;

function IsLiteral(C: Char): Boolean;
begin
  Result := C in ['''', '"'];
end;

procedure CreateQueryParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);
var
  CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: Word;
    TempBuf: PChar;

    procedure StripChar(Value: Char);
    begin
      if TempBuf^ = Value then
        StrMove(TempBuf, TempBuf + 1, Len - 1);
      if TempBuf[StrLen(TempBuf) - 1] = Value then
        TempBuf[StrLen(TempBuf) - 1] := #0;
    end;

  begin
    Len := StrLen(Buffer) + 1;
    TempBuf := AllocMem(Len);
    Result := '';
    try
      StrCopy(TempBuf, Buffer);
      StripChar('''');
      StripChar('"');
      Result := StrPas(TempBuf);
    finally
      FreeMem(TempBuf, Len);
    end;
  end;

begin
  if SpecialChar = #0 then Exit;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ <> SpecialChar) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar, Delims)) do begin
        Inc(CurPos);
        CurChar := CurPos^;
        if IsLiteral(CurChar) then begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else Name := StrPas(StartPos + 1);
      if Assigned(List) then begin
       if Macro then
         List.CreateParam(ftString, Name, ptInput).AsString := TrueExpr
       else List.CreateParam(ftUnknown, Name, ptUnknown);
      end;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ = SpecialChar) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral(CurChar) then Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

constructor TSBatchSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(SQL).OnChange := QueryChanged;
  FParams := TParams.Create;
  FQuery := TZMySqlQuery.Create(Self);
  FSemicolonTerm := True;
  FTerm := DefaultTermChar;
end;

destructor TSBatchSQL.Destroy;
begin
  FQuery.Free;
  FSQL.Free;
  FParams.Free;
  inherited Destroy;
end;

procedure TSBatchSQL.CheckExecQuery(LineNo, StatementNo: Integer);
var
  Done: Boolean;
  Action: TSBatchAction;
  I: Integer;
  Param: TParam;
  Msg: array[0..255] of Char;
  S: string;
begin
  Done := False;
  repeat
    try
     for I := 0 to FQuery.Params.Count - 1 do begin
         Param := FQuery.Params[I];
         Param.Assign(Params.ParamByName(Param.Name));
      end;
      FQuery.ExecSQL;
      Done := True;
    except
      on E: EDatabaseError do begin
        Action := saFail;
        S := Format('Error %s %d', ['sql statement in line', LineNo]);
        if E is EDBEngineError then
          TDBError.Create(EDBEngineError(E), 0, LineNo, PChar(S))
        else begin
          if E.Message <> '' then E.Message := E.Message + '. ';
          E.Message := E.Message + S;
        end;
        if Assigned(FOnScriptError) then
          FOnScriptError(Self, E, LineNo, StatementNo, Action);
        if Action = saFail then raise;
        if Action = saAbort then SysUtils.Abort;
        if Action = saContinue then begin
//          Application.HandleException(Self);
          Done := True;
        end
        else if Action = saIgnore then Done := True;
      end;
    end;
  until Done;
end;

procedure TSBatchSQL.ExecuteScript(StatementNo: Integer);
var
  S, LastStr: string;
  IsTrans, SQLFilled, StmtFound: Boolean;
  I, P, CurrStatement: Integer;
begin
  LastStr := '';
  if assigned(FQuery.Transaction) then IsTrans := true else IsTrans := false;
   try
    I := 0;
    CurrStatement := 0;
    StmtFound := False;
    while I < SQL.Count do begin
      FQuery.SQL.BeginUpdate;
      try
        FQuery.SQL.Clear;
        SQLFilled := False;
        repeat
          if LastStr <> '' then begin
            FQuery.SQL.Add(LastStr);
            LastStr := '';
          end;
          if I < SQL.Count then begin
            S := Trim(SQL[I]);
            Inc(I);
            P := Pos(';', S);
            if (P > 0) and FSemicolonTerm then begin
              LastStr := Trim(Copy(S, P + 1, MaxInt));
              S := Copy(S, 1, P - 1);
              if S <> '' then FQuery.SQL.Add(S);
              SQLFilled := True;
            end
            else begin
              if (S = Term) then SQLFilled := True
              else if S <> '' then FQuery.SQL.Add(S);
            end;
          end
          else SQLFilled := True;
        until SQLFilled;
      finally
        FQuery.SQL.EndUpdate;
      end;
      if FQuery.SQL.Count > 0 then begin
        if (StatementNo < 0) or (StatementNo = CurrStatement) then begin
          StmtFound := True;
          CheckExecQuery(I - 1, CurrStatement);
          if StatementNo = CurrStatement then Break;
        end;
        Inc(CurrStatement);
      end;
    end;
    if not StmtFound then begin
      DatabaseError(Format('%s: %d', ['Error sql number ', StatementNo]));
    end;
    if IsTrans then FQuery.Transaction.Commit;
  except
    if IsTrans then FQuery.Transaction.Commit;
    raise;
  end;
end;

procedure TSBatchSQL.ExecStatement(StatementNo: Integer);
begin
  if FSQL.Count = 0 then DatabaseError('Database error');
  if not Database.Connected then DatabaseError('Database closed');
  if Assigned(FBeforeExec) then FBeforeExec(Self);
   ExecuteScript(StatementNo);
  if Assigned(FAfterExec) then FAfterExec(Self);
end;

procedure TSBatchSQL.ExecSQL;
begin
  ExecStatement(-1);
end;

procedure TSBatchSQL.CreateParams(List: TParams; const Value: PChar);
begin
  CreateQueryParams(List, Value, False, ':', []);
end;

procedure TSBatchSQL.SetQuery(Value: TStrings);
begin
  TStringList(SQL).OnChange := nil;
  FSQL.Assign(Value);
  TStringList(SQL).OnChange := QueryChanged;
  QueryChanged(nil);
end;

function TSBatchSQL.GetText: string;
begin
  Result := SQL.Text;
end;

procedure TSBatchSQL.QueryChanged(Sender: TObject);
var
  List: TParams;
  P: PChar;
begin
    List := TParams.Create(Self);
    try
      CreateParams(List, PChar(Text));
      List.AssignValues(FParams);
      FParams.Free;
      FParams := List;
    except
      List.Free;
    end;
end;

function TSBatchSQL.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

procedure TSBatchSQL.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

function TSBatchSQL.GetParamsCount: Cardinal;
begin
  Result := FParams.Count;
end;

function TSBatchSQL.GetZDatabase: TZMySqlDataBase;
begin
 result := FQuery.Database;
end;

procedure TSBatchSQL.SetZDatabase(const Value: TZMySqlDataBase);
begin
 FQuery.Database := Value;
end;

function TSBatchSQL.GetZTransact: TZMySqlTransact;
begin
 result := FQuery.Transaction;
end;

procedure TSBatchSQL.SetZTransact(const Value: TZMySqlTransact);
begin
 FQuery.Transaction := Value;
end;

procedure Register;
begin
  RegisterComponents('Zeos Access', [TSBatchSQL]);
end;

end.

