{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Sql-script forming routines               }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZSqlScript;

interface

uses Classes, ZSqlTypes;

{***************** Sql Forming Routines **************}

{ Show all tables }
function ShowTables(System: Boolean; DatabaseType: TDatabaseType): string;

{ Show table columns }
function ShowColumns(TableName: string; DatabaseType: TDatabaseType): string;

{ Show table indices }
function ShowIndex(TableName: string; DatabaseType: TDatabaseType): string;

{**************** Sql Parsing Routines ***************}

{ Skip spaces }
function SkipSpaces(var Buffer: string): Boolean;

{ Skip all white spaces }
function SkipWhite(var Buffer: string): Boolean;

{ Skip rest line }
function SkipLine(var Buffer: string): Boolean;

{ Skip rest chars till comment delimiter }
function SkipRest(var Buffer: string; Delim: string): Boolean;

{ Skip comments }
function SkipComment(var Buffer: string; DatabaseType: TDatabaseType): Boolean;

{ Skip term char }
function SkipTerm(var Buffer: string; Term: string;
  DatabaseType: TDatabaseType): Boolean;

{ Extract Sql Token }
function SqlToken(var Buffer: string; Term: string;
  DatabaseType: TDatabaseType): string;

{ Extract Sql Token which skips NL CR symbols }
function SqlTokenEx(var Buffer: string; Term: string;
  DatabaseType: TDatabaseType): string;

{ Check if sql start with command }
function SqlStartWith(Buffer, Value, Term: string;
  DatabaseType: TDatabaseType): Boolean;

{ Check if command at begin }
function CmdStartWith(Buffer, Value: string): Boolean;

{************* Sql form routines ***************}

{ Extract sql query }
function ExtractSqlQuery(var Buffer: string; Term: string;
  DatabaseType: TDatabaseType): string;

{ Check a reserved sql word }
function CheckKeyword(DatabaseType: TDatabaseType; Value: string): Boolean;

{ Extract from sql query select and from parts }
function SplitSelect(Sql: string; DatabaseType: TDatabaseType;
  var Select, From: string): Boolean;

{ Define position of sql parts }
function DefineSqlPos(Sql: string; DatabaseType: TDatabaseType;
  var SelectStartPos, WhereStartPos, WherePos, OrderPos: Integer): Boolean;

{ Compose sql statement }
function ComposeSelect(Sql, WhereAdd, OrderAdd: string;
  WhereStartPos, WherePos, OrderPos: Integer): string;

{ Define table names in SELECT query }
procedure ExtractTables(From: string; Tables, Aliases: TStrings);

implementation

uses ZToken, ZExtra, ZSqlExtra, SysUtils;

{***************** Additional Routines **************************}

function DeleteSqlQuotes(Value: string): string;
begin
  DeleteQuotes(Value);
  if (Value <> '') and (Value[1] = '[') then
    Delete(Value, 1, 1);
  if (Value <> '') and (Value[Length(Value)] = ']') then
    Delete(Value, Length(Value), 1);
  Result := Value;
end;

{******************** Sql Forming Routines ******************}

{ Show all tables }
function ShowTables(System: Boolean; DatabaseType: TDatabaseType): string;
begin
  Result := '';
  case DatabaseType of
    dtMySql:
      if System then
        Result := 'SHOW TABLES FROM mysql'
      else
        Result := 'SHOW TABLES';
    dtPostgreSql:
      if System then
        Result := 'SELECT relname AS TableName FROM pg_class WHERE'
          +' relkind = ''r'' AND relname ~''^pg_'''
      else
        Result := 'SELECT relname AS TableName FROM pg_class WHERE'
          +' relkind = ''r'' AND relname !~''^pg_''';
    dtInterbase:
      if System then
        Result := 'SELECT RDB$RELATION_NAME AS TableName'
          +' FROM RDB$RELATIONS WHERE RDB$RELATION_NAME LIKE ''RDB$%'''
          +' ORDER BY RDB$RELATION_NAME'
      else
        Result := 'SELECT RDB$RELATION_NAME AS TableName'
          +' FROM RDB$RELATIONS WHERE RDB$RELATION_NAME NOT LIKE ''RDB$%'''
          +' ORDER BY RDB$RELATION_NAME';
    dtMsSql:
      if System then
        Result := 'SELECT o.name FROM sysobjects o, sysindexes i'
          + ' WHERE o.sysstat & 0xf <> 3 AND i.id = o.id AND i.indid < 2'
          + ' AND o.name NOT LIKE ''#%'' ORDER BY o.name'
      else
        Result := 'SELECT o.name FROM sysobjects o, sysindexes i'
          + ' WHERE o.sysstat & 0xf = 3 AND i.id = o.id AND i.indid < 2'
          + ' AND o.name NOT LIKE ''#%'' ORDER BY o.name';
  end;
end;

{ Show table columns }
function ShowColumns(TableName: string; DatabaseType: TDatabaseType): string;
begin
  Result := '';
  case DatabaseType of
    dtMySql: Result := 'SHOW COLUMNS FROM '+TableName;
    dtPostgreSql: Result := 'SELECT attname AS field,'
      +' typname AS type, atttypmod-4 as length, NOT attnotnull AS "null",'
      +' adsrc AS def FROM pg_attribute, pg_class, pg_type, pg_attrdef WHERE'
      +' pg_class.oid=attrelid AND pg_type.oid=atttypid AND attnum>0'
      +' AND pg_class.oid=adrelid AND adnum=attnum AND atthasdef=''t'''
      +' AND relname='''+LowerCase(TableName)+''''
      +' UNION SELECT attname AS field,'
      +' typname AS type, atttypmod-4 as length, NOT attnotnull AS "null",'
      +' '''' AS def FROM pg_attribute, pg_class, pg_type WHERE'
      +' pg_class.oid=attrelid AND pg_type.oid=atttypid AND attnum>0'
      +' AND atthasdef=''f'' AND relname='''+TableName+'''';
    dtInterbase: Result := 'SELECT A.RDB$FIELD_NAME AS Fld,'
      +' C.RDB$TYPE_NAME AS Typ, B.RDB$FIELD_LENGTH AS Len,'
      +' A.RDB$NULL_FLAG AS N_Nul, A.RDB$DEFAULT_SOURCE AS Def,'
      +' -B.RDB$FIELD_SCALE AS Scale'
      +' FROM RDB$RELATION_FIELDS A LEFT JOIN RDB$FIELDS B'
      +' ON A.RDB$FIELD_SOURCE=B.RDB$FIELD_NAME LEFT JOIN RDB$TYPES C'
      +' ON B.RDB$FIELD_TYPE=C.RDB$TYPE WHERE'
      +' A.RDB$RELATION_NAME='''+TableName+''''
      +' AND C.RDB$FIELD_NAME=''RDB$FIELD_TYPE'''
      +' ORDER BY RDB$FIELD_POSITION';
    dtMsSql: Result := 'EXEC sp_mshelpcolumns '''+TableName+'''';
  end;
end;

{ Show table indices }
function ShowIndex(TableName: string; DatabaseType: TDatabaseType): string;
begin
  Result := '';
  case DatabaseType of
    dtMySql: Result := 'SHOW INDEX FROM '+TableName;
    dtPostgreSql: Result := 'SELECT t1.relname AS name, t2.relname AS table,'
      +' indisunique AS "unique", indkey AS fields'
      +' FROM pg_index AS i, pg_class AS t1, pg_class AS t2 WHERE'
      +' i.indexrelid=t1.oid AND i.indrelid=t2.oid'
      +' AND t2.relname='''+TableName+'''';
    dtInterbase: Result := 'SELECT A.RDB$INDEX_NAME AS Name, RDB$RELATION_NAME'
      +' AS Tbl, RDB$UNIQUE_FLAG AS Uni, RDB$INDEX_TYPE AS Srt, RDB$FIELD_NAME AS Fld'
      +' FROM RDB$INDICES A LEFT JOIN RDB$INDEX_SEGMENTS B'
      +' ON A.RDB$INDEX_NAME=B.RDB$INDEX_NAME WHERE'
      +' RDB$RELATION_NAME LIKE '''+TableName+' %''';
    dtMsSql: Result := 'EXEC sp_helpindex '''+TableName+'''';
  end;
end;

{******************** Sql Parsing Routines *******************}

{ Skip spaces }
function SkipSpaces(var Buffer: string): Boolean;
var
  Ptr: PChar;
begin
  Ptr := PChar(Buffer);
  Result := False;
  while Ptr^ <> #0 do
  begin
    if not (Ptr^ in [' ',#9]) then
      Break;
    Inc(Ptr);
    Result := True;
  end;
  Buffer := StrPas(Ptr);
end;

{ Skip white }
function SkipWhite(var Buffer: string): Boolean;
var
  Ptr: PChar;
begin
  Ptr := PChar(Buffer);
  Result := False;
  while Ptr^ <> #0 do
  begin
    if not (Ptr^ in [' ',#9,#10,#13]) then
      Break;
    Inc(Ptr);
    Result := True;
  end;
  Buffer := StrPas(Ptr);
end;

{ Skip rest line }
function SkipLine(var Buffer: string): Boolean;
var
  N: Integer;
begin
  N := Pos(#10, Buffer);
  if N > 0 then
    Buffer := Copy(Buffer, N+1, Length(Buffer)-N)
  else begin
    N := Pos(#13, Buffer);
    if N > 0 then
      Buffer := Copy(Buffer, N+1, Length(Buffer)-N)
    else
      Buffer := '';
  end;
  Result := True;
end;

{ Skip rest chars till comment delimiter }
function SkipRest(var Buffer: string; Delim: string): Boolean;
var
  N: Integer;
begin
  N := Pos(Delim, Buffer) + Length(Delim) - 1;
  if N > 0 then
    Buffer := Copy(Buffer, N+1, Length(Buffer)-N)
  else
    Buffer := '';
  Result := True;
end;

{ Skip comments }
function SkipComment(var Buffer: string; DatabaseType: TDatabaseType): Boolean;
var
  Skip: Boolean;
begin
  Result := False;
  repeat
    Result := SkipSpaces(Buffer) or Result;
    Skip := False;
    if DatabaseType = dtMySql then
    begin
      if StrCmpBegin(Buffer, '#') then
        Skip := SkipLine(Buffer);
    end;
    if DatabaseType = dtInterbase then
    begin
      if StrCmpBegin(Buffer, '//') then
        Skip := SkipLine(Buffer);
      if StrCmpBegin(Buffer, '/*') then
        Skip := SkipRest(Buffer, '*/');
    end;
    if DatabaseType = dtPostgreSql then
    begin
      if StrCmpBegin(Buffer, '--') then
        Skip := SkipLine(Buffer);
      if StrCmpBegin(Buffer, '/*') then
        Skip := SkipRest(Buffer, '*/');
    end;
    Result := Result or Skip;
  until not Skip;
end;

{ Skip term char }
function SkipTerm(var Buffer: string; Term: string;
  DatabaseType: TDatabaseType): Boolean;
begin
  SkipComment(Buffer, DatabaseType);
  SkipSpaces(Buffer);
  Result := StrCmpBegin(Buffer, Term);
  if Result then
    Buffer := Copy(Buffer, Length(Term)+1, Length(Buffer) - Length(Term));
  Result := Result or (Buffer = '');
end;

{ Extract Sql Token }
function SqlToken(var Buffer: string; Term: string;
  DatabaseType: TDatabaseType): string;
var
  Temp: string;
  N, M: Integer;
begin
  Result := '';
  SkipComment(Buffer, DatabaseType);
  if not StrCmpBegin(Buffer, Term) and (Buffer <> '') then
  begin
    N := 0;
    M := 0;
    repeat
      if (Buffer <> '') and (Buffer[1] in [' ', #9]) then
        Result := Result + ' ';
      ExtractLowToken(Buffer, Temp);
      Result := Result + Temp;
      if Temp = '(' then Inc(N);
      if Temp = ')' then Dec(N);
      if Temp = '[' then Inc(M);
      if Temp = ']' then Dec(M);
    until ((N <= 0) and (M <= 0)) or (Buffer = '');
  end;
end;

{ Extract Sql Token which skips NL CR symbols }
function SqlTokenEx(var Buffer: string; Term: string;
  DatabaseType: TDatabaseType): string;
begin
  repeat
    Result := SqlToken(Buffer, Term, DatabaseType);
  until (Result <> #9) and (Result <> #10) and (Result <> #13);
end;

{ Check if start with command }
function SqlStartWith(Buffer, Value, Term: string;
  DatabaseType: TDatabaseType): Boolean;
var
  Token1, Token2: string;
begin
  Result := True;
  while Value <> '' do
  begin
    Token1 := SqlToken(Buffer, Term, DatabaseType);
    Token2 := StrTok(Value, ' '#9#10#13);
    if Token2 = '' then Exit;
    Result := Result and StrCaseCmp(Token1, Token2);
    if not Result then Exit;
  end;
end;

{ Check if command start with command }
function CmdStartWith(Buffer, Value: string): Boolean;
var
  Token1, Token2: string;
begin
  Result := True;
  while Value <> '' do
  begin
    Token1 := StrTok(Buffer, ' '#9#10#13);
    Token2 := StrTok(Value, ' '#9#10#13);
    if Token2 = '' then Exit;
    Result := Result and StrCaseCmp(Token1, Token2);
    if not Result then Exit;
  end;
end;

{ Extract sql query }
function ExtractSqlQuery(var Buffer: string; Term: string;
  DatabaseType: TDatabaseType): string;
var
  Token: string;
  NewLine: Boolean;
begin
  Result := '';
  NewLine := True;
  while Buffer <> '' do
  begin
    if SkipComment(Buffer, DatabaseType) and (Result <> '') then
      Result := Result + ' ';
    if SkipTerm(Buffer, Term, DatabaseType) then
      Break;
    ExtractLowToken(Buffer, Token);
    if Token = #10 then Token := #13;
    if NewLine and (Token = #13) then
      Continue;
    NewLine := (Token = #13);
    Result := Result + Token;
  end;
end;

{ Check a reserved sql word }
function CheckKeyword(DatabaseType: TDatabaseType; Value: string): Boolean;
begin
  Value := UpperCase(Value);
  Result := True;
  { Check universal keywords }
  if (Value = 'WHERE') or (Value = 'INTO') or (Value = 'GROUP')
    or (Value = 'ORDER') or (Value = 'HAVING') or (Value = 'FROM')
    or (Value = 'FOR') then Exit;
  { Check MySql keywords }
  if (DatabaseType = dtMySql) and ((Value = 'PROCEDURE')
    or (Value = 'LIMIT')) then Exit;
  { Check PostgreSql keywords }
  if (DatabaseType = dtPostgreSql) and ((Value = 'LIMIT')) then Exit;
  { Check Oracle keywords }
  if (DatabaseType = dtOracle) and ((Value = 'START')) then Exit;
  { Check MS SQL keywords }
  if (DatabaseType = dtMsSql) and ((Value = 'COMPUTE')
    or (Value = 'OPTION')) then Exit;
  { Check Interbase, MS SQL, PostgreSql and Oracle keywords }
  if (DatabaseType in [dtInterbase, dtMsSql, dtPostgreSql, dtOracle])
    and ((Value = 'UNION')) then Exit;
  { Check PostgreSql and Oracle keywords }
  if (DatabaseType in [dtPostgreSql, dtOracle])
    and ((Value = 'INTERSECT') or (Value = 'EXCEPT')) then Exit;
  Result := False;
end;

{ Extract from sql query select and from parts }
function SplitSelect(Sql: string; DatabaseType: TDatabaseType;
  var Select, From: string): Boolean;
var
  Token: string;
begin
  Select := '';
  From := '';
  Result := False;

  Token := SqlTokenEx(Sql, ';', DatabaseType);
  if not StrCaseCmp(Token, 'SELECT') then Exit;
  Result := True;

  while Sql <> '' do
  begin
    Token := SqlTokenEx(Sql, ';', DatabaseType);
    if StrCaseCmp(Token, 'FROM') then
      Break;
    if StrCaseCmp(Token, 'INTO') then
    begin
      while (SqlTokenEx(Sql, ';', DatabaseType) <> 'FROM') and (Sql <> '') do;
      Break;
    end;
    if CheckKeyword(DatabaseType, Token) then
      Exit;
    Select := Select + Token;
    if (Sql <> '') and (Sql[1] in [' ', #9, #10, #13]) then
      Select := Select + ' ';
  end;

  while (Sql <> '') and (Sql[1] <> ';') do
  begin
    Token := SqlTokenEx(Sql, ';', DatabaseType);
    if CheckKeyword(DatabaseType, Token) then
      Exit;
    From := From + Token;
    if (Sql <> '') and (Sql[1] in [' ', #9, #10, #13]) then
      From := From + ' ';
  end;
end;

{ Define position of sql parts }
function DefineSqlPos(Sql: string; DatabaseType: TDatabaseType;
  var SelectStartPos, WhereStartPos, WherePos, OrderPos: Integer): Boolean;
type
  TSeekState = (ssNone, ssFrom, ssWhere, ssOrder);
var
  Temp, Token: string;
  SaveLen: Integer;
  SavePos: Integer;
  State: TSeekState;
begin
  SelectStartPos := 0;
  WhereStartPos := 0;
  WherePos := 0;
  OrderPos := 0;
  SaveLen := Length(Sql) + 1;
  State := ssNone;

  Result := (UpperCase(SqlTokenEx(Sql, ';', DatabaseType)) = 'SELECT');
  if not Result then Exit;

  { Skip select list keywords }
  while True do
  begin
    Temp := Sql;
    Token := UpperCase(SqlTokenEx(Sql, ';', DatabaseType));
    if (Token = 'DISTINCT') or (Token = 'ALL') or (Token = 'DISTINCTROW') then
      Continue;
    if (DatabaseType = dtMySql) and ((Token = 'STRAIGNT_JOIN')
      or (Token = 'SQL_SMALL_RESULT') or (Token = 'SQL_BIG_RESULT')
      or (Token = 'SQL_BUFFER_RESULT') or (Token = 'HIGH_PRIORITY')) then
      Continue;
    Sql := Temp;
    SelectStartPos := SaveLen - Length(Sql);
    Break;
  end;

  while (Sql <> '') and (Sql[1] <> ';')
    and ((WherePos = 0) or (OrderPos = 0)) do
  begin
    SavePos := SaveLen - Length(Sql);
    Token := UpperCase(SqlTokenEx(Sql, ';', DatabaseType));
    { Set where start pos }
    if (State = ssWhere) and (WhereStartPos = 0) then
      WhereStartPos := SaveLen - Length(Sql) - Length(Token);
    if not CheckKeyword(DatabaseType, Token) then
      Continue;
    { Check for Where position }
    if (Token = 'FROM') and (WherePos = 0) then
    begin
      State := ssFrom;
      Continue;
    end;
    if (Token = 'WHERE') and (WherePos = 0) then
    begin
      State := ssWhere;
      Continue;
    end;
    if (State = ssFrom) and (WherePos = 0) then
    begin
      WherePos := -SavePos;
      State := ssNone;
    end;
    if (State = ssWhere) and (WherePos = 0) then
    begin
      WherePos := SavePos;
      State := ssNone;
    end;
    { Check for Order position }
    if (Token = 'ORDER') and (OrderPos = 0) then
    begin
      State := ssOrder;
      Continue;
    end;
    if (State = ssOrder) and (OrderPos = 0) then
    begin
      OrderPos := SavePos;
      State := ssNone;
    end;
    if (Token = 'COMPUTE') or (Token = 'FOR') then
    begin
      if WherePos = 0 then
        WherePos := -SavePos;
      if OrderPos = 0 then
        OrderPos := -SavePos;
    end;
  end;

  if (State = ssWhere) and (WherePos = 0) then
    WherePos := SaveLen;
  if (State = ssOrder) and (OrderPos = 0) then
    OrderPos := SaveLen;
end;

{ Compose sql statement }
function ComposeSelect(Sql, WhereAdd, OrderAdd: string;
  WhereStartPos, WherePos, OrderPos: Integer): string;
var
  Temp: string;
begin
  Temp := '';
  Result := Sql;
  { Update positions }
  if WherePos = 0 then WherePos := - Length(Sql) - 1;
  if OrderPos = 0 then OrderPos := - Length(Sql) - 1;
  if WhereAdd = '' then WherePos := 0;
  if OrderAdd = '' then OrderPos := 0;
  { Insert where statement}
  if WhereAdd <> '' then
  begin
    if (WhereStartPos > 0) and (WherePos > 0) then
    begin
      Insert('(', Result, WhereStartPos);
      if WherePos < 0 then Dec(WherePos)
      else Inc(WherePos);
      if OrderPos < 0 then Dec(OrderPos)
      else Inc(OrderPos);
    end;
    if WherePos < 0 then
      Temp := ' WHERE ' + WhereAdd
    else Temp := ') AND ' + WhereAdd;
    WherePos := Abs(WherePos);
    Insert(Temp, Result, WherePos);
    if OrderPos < 0 then
      OrderPos := OrderPos - Length(Temp)
    else OrderPos := OrderPos + Length(Temp);
  end;
  { Insert order by statement }
  if OrderAdd <> '' then
  begin
    if OrderPos < 0 then
      Temp := ' ORDER BY ' + OrderAdd
    else Temp := ', ' + OrderAdd;
    OrderPos := Abs(OrderPos);
    Insert(Temp, Result, OrderPos);
  end;
end;

{ Define table names in SELECT query }
procedure ExtractTables(From: string; Tables, Aliases: TStrings);
var
  Token, Table, Alias: string;
  NextTable, Find: Boolean;
  I: Integer;
begin
  Tables.Clear;
  Aliases.Clear;

{ Extract table names }
  NextTable := True;
  while From <> '' do
  begin
    Token := SqlTokenEx(From, ';', dtUnknown);
    if NextTable then
    begin
      NextTable := False;
      Table := Token;

      { Define table alias }
      Token := SqlTokenEx(From, ';', dtUnknown);
      if Token = '.' then
      begin
        Token := SqlTokenEx(From, ';', dtUnknown);
        Table := Table + '.' + Token;
        Token := SqlTokenEx(From, ';', dtUnknown);
      end;

      if StrCaseCmp(Token, 'AS') or (Token = '=') then
        Alias := SqlTokenEx(From, ';', dtUnknown)
      else if ((Token <> '') and (Token[1] in ['a'..'z','A'..'Z','''','"','[']))
        and not StrCaseCmp(Token, 'LEFT')
        and not StrCaseCmp(Token, 'NATURAL') and not StrCaseCmp(Token, 'RIGHT')
        and not StrCaseCmp(Token, 'ON') and not StrCaseCmp(Token, 'USING')
      then
        Alias := Token
      else
        Alias := Table;

      Find := False;
      Table := DeleteSqlQuotes(Table);
      Alias := DeleteSqlQuotes(Alias);
      for I := 0 to Tables.Count-1 do
        if StrCaseCmp(Tables[I], Table) and StrCaseCmp(Aliases[I], Alias) then
        begin
          Find := True;
          Break;
        end;

      if not Find then
      begin
        Tables.Add(Table);
        Aliases.Add(Alias);
      end;
    end;
    if StrCaseCmp(Token, 'JOIN') or (Token = ',') then
      NextTable := True;
  end;
end;

end.
