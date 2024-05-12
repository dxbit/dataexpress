unit DxSQLQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, myfpsqlparser, myfpsqltree, strconsts, sqldb, db,
  Dialogs, dxctrls;

type
  { TMySQLQuery }

  TMySQLQuery = class(TSQLQuery)
  private
    FUseExecuteBlock: Boolean;
  protected
    procedure ApplyRecUpdate(UpdateKind: TUpdateKind); override;
  public
    property UseExecuteBlock: Boolean read FUseExecuteBlock write FUseExecuteBlock;
  end;

  TUseGeneratorOption = (ugNotUse, ugAppend, ugApplyUpdates);

  { TdxSQLQuery }

  TdxSQLQuery = class
  private
    FDeletedRecs: array of Integer;
    FKeyField: TField;
    FSQL, FGenName: String;
    FDataSet: TMySQLQuery;
    FUseGenerator: TUseGeneratorOption;
    function GetUseExecuteBlock: Boolean;
    function ProcessStatement(const S: String): String;
    procedure SetUseExecuteBlock(AValue: Boolean);
    function ToSQL: String;
    procedure ApplyKeyValues;
    function GetAsDT(Index: String): TDateTime;
    function GetAsF(Index: String): Extended;
    function GetAsI(Index: String): Integer;
    function GetAsS(Index: String): String;
    function GetField(Index: Integer): TField;
    function GetFields(Name: String): Variant;
    function GetState: TDataSetState;
    procedure SetAsDT(Index: String; AValue: TDateTime);
    procedure SetAsF(Index: String; AValue: Extended);
    procedure SetAsI(Index: String; AValue: Integer);
    procedure SetAsS(Index: String; AValue: String);
    procedure SetFields(Name: String; AValue: Variant);
  public
    constructor Create(const SQL: String; ACurrentForm: TdxForm = nil; ADisableCalcs: Boolean = False);
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function Opened: Boolean;
    procedure Append;
    procedure Edit;
    procedure Delete;
    procedure Cancel;
    procedure Post;
    procedure ApplyUpdates;
    procedure CancelUpdates;
    procedure MoveBy(Distance: Integer);
    procedure MoveFirst;
    procedure MovePrior;
    procedure MoveNext;
    procedure MoveLast;
    procedure MoveTo(aRecNo: Integer);
    function BOF: Boolean;
    function EOF: Boolean;
    function RecNo: Integer;
    function RecordCount: Integer;
    function FieldCount: Integer;
    //function GenId(IncValue: Integer): Integer;
    function Locate(const FieldNames: String; FieldValues: array of Variant; Options: TLocateOptions): Boolean;
    procedure LoadFromStream(const FieldName: String; Stream: TStream);
    procedure SaveToStream(const FieldName: String; Stream: TStream);
    property Fields[Name: String]: Variant read GetFields write SetFields; default;
    property Field[Index: Integer]: TField read GetField;
    property AsI[Index: String]: Integer read GetAsI write SetAsI;
    property AsF[Index: String]: Extended read GetAsF write SetAsF;
    property AsDT[Index: String]: TDateTime read GetAsDT write SetAsDT;
    property AsS[Index: String]: String read GetAsS write SetAsS;
    property DataSet: TMySQLQuery read FDataSet;
    property State: TDataSetState read GetState;
    property UseGenerator: TUseGeneratorOption read FUseGenerator write FUseGenerator;
    property UseExecuteBlock: Boolean read GetUseExecuteBlock write SetUseExecuteBlock;
  end;

  { TdxSQLParser }

  TdxSQLParser = class
  private
    FCurrentForm: TdxForm;
    FDisableCalcs: Boolean;
    FExtractForms: Boolean;
    FTableName: String;
    FFields, FAliases: TStringList;
    FSourceForms: TStrings;
    function PrepareSQLExpr(const S: String): String;
    function ReplaceFieldName(S: String; Fm: TdxForm; const AliasName: String): String;
    procedure ProcessElement(El: TSQLElement; Fm: TdxForm; const AliasName: String; out DetectNull: Boolean);
    procedure ProcessJoinTableRef(Join: TSQLJoinTableReference; Fm: TdxForm; const AliasName: String);
    procedure ReplaceFieldNames(Stat: TSQLSelectStatement; Fm: TdxForm; const AliasName: String);
    procedure ProcessSQLExpression(Expr: TSQLExpression; Fm: TdxForm; const AliasName: String; out DetectNull: Boolean);
    procedure ParseSimpleTableRef(Stat: TSQLSelectStatement; T: TSQLSimpleTableReference);
    procedure ParseSelectTableRef(T: TSQLSelectTableReference);
    procedure ParseJoinTableRef(Stat: TSQLSelectStatement; T: TSQLJoinTableReference);
    procedure ParseSelectStatement(Stat: TSQLSelectStatement);
    procedure FindTableAndFields(Stat: TSQLSelectStatement);
  public
    constructor Create;
    destructor Destroy; override;
    function Parse(const SQL: String): String;
    function GetInsertSQL: String;
    function GetUpdateSQL: String;
    function GetDeleteSQL: String;
    function GetGeneratorName: String;
    property CurrentForm: TdxForm read FCurrentForm write FCurrentForm;
    property SourceForms: TStrings read FSourceForms;
    property ExtractForms: Boolean read FExtractForms write FExtractForms;
    property DisableCalcs: Boolean read FDisableCalcs write FDisableCalcs;
  end;

function SQLSelect(const SQL: String): TdxSQLQuery;
procedure SQLExecute(const SQL: String);
function ParseSQL(const SQL: String; Fm: TdxForm): String;

implementation

uses
  LazUtf8, formmanager, sqlgen, dbengine, apputils, Variants, StrUtils,
  expressions;

{ TMySQLQuery }

procedure TMySQLQuery.ApplyRecUpdate(UpdateKind: TUpdateKind);
begin
  // Ничего не делаем, если используется Execute Block
  if not FUseExecuteBlock then
    inherited ApplyRecUpdate(UpdateKind);
end;

function TdxSQLParser.PrepareSQLExpr(const S: String): String;
var
  i, L, p0: Integer;
  Ch, Prefix: Char;
  Ident, Value, Expr: String;
  Fm: TdxForm;
  C: TComponent;
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
begin
  Result := '';
  EB := nil; E := nil;
  L := Length(S);
  i := 1;
  while i <= L do
  begin
    p0 := i;
    case S[i] of
      '''', '"':
        begin
          Ch := S[i];
          while (i <= L) and (S[i] <> Ch) do
            Inc(i);
          Result := Result + Copy(S, p0, i - p0 + 1);
        end;
      '[':
        begin
          while (i <= L) and (S[i] <> ']') do
            Inc(i);
          Ident := Copy(S, p0 + 1, i - p0 - 1);
          if Ident = '' then
            raise Exception.Create(rsEmptySquareBracketsDetected);
          if Ident[1] in [':', '!'] then
          begin
            Prefix := Ident[1];
            Delete(Ident, 1, 1);
            if Ident = '' then raise Exception.Create(rsFieldNameEmpty);

            if FCurrentForm <> nil then
            begin
              if (Prefix = ':') or (FCurrentForm.PId = 0) then Fm := FCurrentForm
              else if FCurrentForm.IsBinded then Fm := FCurrentForm.ParentForm
              else Fm := FormMan.FindForm(FCurrentForm.PId);
              C := Fm.FindComponentByFldName(Ident);
              if C = nil then
                raise Exception.CreateFmt(rsFieldNotFound, [Fm.FormCaption + ' -> ' + Ident]);
              if Fm.IsBinded then
              begin
                V := GetComponentFieldValue(Fm.DataSet, C);
                if VarIsNull(V) then
                  Value := 'null'
                else if C is TdxDateEdit then
                  Value := '''' + FormatDateTime('yyyy-mm-dd', V) + ''''
                else
                begin
                  Value := VarToStr(V);
                  if not IsNumericComponent(C) then
                    Value := '''' + Value + '''';
                end;
              end
              else
                Value := 'null';
            end
            else if FExtractForms then
              Value := 'null'
            else
              raise Exception.Create(rsFormNotAvail);

            Result := Result + Value;
          end
          else
          begin
            Ident := StringReplace(Ident, '.', #1, [rfReplaceAll]);
            Result := Result + '"___' + Ident + '"';
          end;
        end;
      '{':
        begin
          while (i <= L) and (S[i] <> '}') do
            Inc(i);
          Expr := Copy(S, p0 + 1, i - p0 - 1);
          if Trim(Expr) = '' then
            raise Exception.Create(rsExprEmpty);

          if not FExtractForms then
          begin

            if EB = nil then
            begin
              EB := TExpressionBuilder.Create;
              if FCurrentForm <> nil then
              begin
                EB.Form := FCurrentForm;
                if FCurrentForm.PId > 0 then
                  EB.ParentForm := FormMan.FindForm(FCurrentForm.PId);
                EB.DataSet := FCurrentForm.DataSet;
              end;
              EB.SkipLabels := True;
            end;

            try
              FreeAndNil(E);
              E := EB.Build(Expr);
            except
              on E: Exception do
                raise;
            end;

            //if (FCurrentForm = nil) or FCurrentForm.IsBinded then
            if not FDisableCalcs then
            begin
              V := E.Calc;
              if VarIsNull(V) then
                Value := 'null'
              else if VarType(V) = varDate then
              begin
                if Frac(V) > 0 then
                  Value := '''' + FormatDateTime('hh:nn:ss', V) + ''''
                else
                  Value := '''' + FormatDateTime('yyyy-mm-dd', V) + '''';
              end
              else if VarIsNumeric(V) then
                Value := VarToStr(V)
              else
                Value := '''' + VarToStr(V) + '''';
            end
            else
              Value := 'null';

          end
          else
            Value := 'null';

          Result := Result + Value;
        end;
      '/':
        begin
          if Copy(S, i + 1, 1) = '*' then
          begin
            Inc(i, 2);
            while i <= L do
            begin
              if (S[i] = '*') and (Copy(S, i + 1, 1) = '/') then
              begin
                Inc(i);
                Break;
              end;
              Inc(i);
            end;
            Result := Result + Copy(S, p0, i - p0 + 1);
          end
          else
            Result := Result + S[i];
        end
      else
        Result := Result + S[i];
    end;
    Inc(i);
  end;
  FreeAndNil(EB);
  FreeAndNil(E);
  //Debug(Result);
end;

function DetectOptionalField(var S: String): Boolean;
begin
  Result := (Pos('?', S) = 1) or (Pos('___?', S) > 0);
  if Result then Delete(S, Pos('?', S), 1);
end;

// Необязательные поля обозначаются вопросительным знаком. Знак может стоят
// перед именем алиаса/таблицы, перед именем формы, перед именем поля формы.
// Допустимые варианты: "?pt.f1", "?pt".[дата], pt.[?дата], [?поступление товара].[дата]
// А так нельзя: "pt.?f1"
function TdxSQLParser.ReplaceFieldName(S: String; Fm: TdxForm; const AliasName: String): String;
var
  p: SizeInt;
  FmNm, FlNm: String;
  C: TComponent;
  IsOptionalField: Boolean;
begin
  IsOptionalField := DetectOptionalField(S);
  Result := S;
  p := Pos('.', S);
  if p > 0 then
  begin
    if Copy(S, 1, 3) = '___' then
    begin
    	Delete(S, 1, 3);
      p := p - 3;
    end;
    FmNm := Copy(S, 1, p - 1);
    FmNm := StringReplace(FmNm, #1, '.', [rfReplaceAll]); // Возможно в имени формы есть точка...
    if (MyUtf8CompareText(FmNm, Fm.FormCaption) = 0) or
  	  (MyUtf8CompareText(FmNm, AliasName) = 0) then
    begin
      if AliasName <> '' then Result := AliasName
      else Result := TableStr(Fm.Id);

      FlNm := Copy(S, p + 1, 1024);
      if Copy(FlNm, 1, 3) = '___' then
      begin
        Delete(FlNm, 1, 3);
        FlNm := StringReplace(FlNm, #1, '.', [rfReplaceAll]); // Возможно в имени поля есть точка...
	      C := FindComponentByFieldName(Fm, FlNm);
        if C = nil then raise Exception.CreateFmt(rsFieldNotFound,
        	[Fm.FormCaption + ' -> ' + FlNm]);
        Result := Result + '.' + FieldStr(C);
      end
      else Result := Result + '.' + FlNm;
    end
    //else
    //  raise Exception.CreateFmt(rsUndefinedAliasOrFm, [FmNm]);
  end
  // Если в имени поля форма не указана, то пытаемся определить автоматически
  else
  begin
    FlNm := S;
    if Copy(FlNm, 1, 3) = '___' then
    begin
      Delete(FlNm, 1, 3);
      FlNm := StringReplace(FlNm, #1, '.', [rfReplaceAll]); // Возможно в имени поля есть точка...
      C := FindComponentByFieldName(Fm, FlNm);
      if C = nil then raise Exception.CreateFmt(rsFieldNotFound,
        [Fm.FormCaption + ' -> ' + FlNm]);
      FlNm := FieldStr(C);
      if AliasName <> '' then
        Result := AliasName + '.' + FlNm
      else
        Result := TableStr(Fm.Id) + '.' + FlNm;
    end;
  end;

  if IsOptionalField then
    Result := '?' + Result;
end;

procedure TdxSQLParser.ProcessElement(El: TSQLElement; Fm: TdxForm;
  const AliasName: String; out DetectNull: Boolean);
var
  Dummy: Boolean;
begin
  if El is TSQLSelectField then
  	ProcessSQLExpression(TSQLSelectField(El).Expression, Fm, AliasName, Dummy)
  else if El is TSQLOrderByElement then
  	ProcessElement(TSQLOrderByElement(El).Field, Fm, AliasName, Dummy)
  else if El is TSQLIdentifierName then
  	with TSQLIdentifierName(El) do
    	Name := ReplaceFieldName(Name, Fm, AliasName)
  else if El is TSQLExpression then
  	ProcessSQLExpression(TSQLExpression(El), Fm, AliasName, DetectNull);
end;

procedure TdxSQLParser.ProcessJoinTableRef(Join: TSQLJoinTableReference;
  Fm: TdxForm; const AliasName: String);
var
  Dummy: Boolean;
begin
  ProcessSQLExpression(Join.JoinClause, Fm, AliasName, Dummy);
  if Join.Left is TSQLJoinTableReference then
  	ProcessJoinTableRef(TSQLJoinTableReference(Join.Left), Fm, AliasName);
  if Join.Right is TSQLJoinTableReference then
  	ProcessJoinTableRef(TSQLJoinTableReference(Join.Right), Fm, AliasName);
end;

procedure TdxSQLParser.ReplaceFieldNames(Stat: TSQLSelectStatement;
  Fm: TdxForm; const AliasName: String);
var
  i: Integer;
  F, T: TSQLElement;
  Dummy: Boolean;
begin
  for i := 0 to Stat.Fields.Count - 1 do
  begin
    F := Stat.Fields[i];
    ProcessElement(F, Fm, AliasName, Dummy);
  end;
  for i := 0 to Stat.Tables.Count - 1 do
  begin
    T := Stat.Tables[i];
    if T is TSQLJoinTableReference then
    	ProcessJoinTableRef(TSQLJoinTableReference(T), Fm, AliasName);
  end;
  ProcessSQLExpression(Stat.Where, Fm, AliasName, Dummy);
  ProcessSQLExpression(Stat.Having, Fm, AliasName, Dummy);
  for i := 0 to Stat.GroupBy.Count - 1 do
    ProcessElement(Stat.GroupBy[i], Fm, AliasName, Dummy);
  for i := 0 to Stat.OrderBy.Count - 1 do
  	ProcessElement(Stat.OrderBy[i], Fm, AliasName, Dummy);
end;

function CreateIntLiteral(Parent: TSQLElement): TSQLLiteralExpression;
begin
  Result := TSQLLiteralExpression.Create(Parent);
  Result.Literal := TSQLIntegerLiteral.Create(Result);
  TSQLIntegerLiteral(Result.Literal).Value:=1;
end;

procedure TdxSQLParser.ProcessSQLExpression(Expr: TSQLExpression; Fm: TdxForm;
  const AliasName: String; out DetectNull: Boolean);
var
  i: Integer;
  Dummy: Boolean;
begin
  DetectNull := False;
  if Expr = nil then Exit;
  if Expr is TSQLIdentifierExpression then
    with TSQLIdentifierExpression(Expr) do
	    Identifier.Name := ReplaceFieldName(Identifier.Name, Fm, AliasName)
  else if Expr is TSQLUnaryExpression then
  	ProcessSQLExpression(TSQLUnaryExpression(Expr).Operand, Fm, AliasName, Dummy)
  else if Expr is TSQLBinaryExpression then
    with TSQLBinaryExpression(Expr) do
    begin
      ProcessSQLExpression(Left, Fm, AliasName, Dummy);
      ProcessSQLExpression(Right, Fm, AliasName, DetectNull);

      if DetectNull then
      begin
        if Left is TSQLIdentifierExpression then
        begin
          if TSQLIdentifierExpression(Left).Identifier.Name[1] = '?' then
          begin
            Left.Free;
            Right.Free;
            Left := CreateIntLiteral(Expr);
            Right := CreateIntLiteral(Expr);
            Operation := boEQ;
          end
          else if Operation in [boEQ, boLT, boGT, boLE, boGE, boLike,
            boContaining, boStarting] then
            Operation := boIS
          else if Operation = boNE then
            Operation := boIsNot;
        end;
      end
      else if Left is TSQLIdentifierExpression then
      begin
        if TSQLIdentifierExpression(Left).Identifier.Name[1] = '?' then
          TSQLIdentifierExpression(Left).Identifier.Name :=
            Copy(TSQLIdentifierExpression(Left).Identifier.Name, 2, 1024);
      end;
    end
  else if Expr is TSQLTernaryExpression then
  begin
    ProcessSQLExpression(TSQLTernaryExpression(Expr).Left, Fm, AliasName, Dummy);
    ProcessSQLExpression(TSQLTernaryExpression(Expr).Middle, Fm, AliasName, Dummy);
    ProcessSQLExpression(TSQLTernaryExpression(Expr).Right, Fm, AliasName, Dummy);
  end
  else if Expr is TSQLSelectionExpression then
  begin
  	ReplaceFieldNames(TSQLSelectionExpression(Expr).Select, Fm, AliasName);
    ParseSelectStatement(TSQLSelectionExpression(Expr).Select);
  end
  else if Expr is TSQLAggregateFunctionExpression then
  	ProcessSQLExpression(TSQLAggregateFunctionExpression(Expr).Expression, Fm, AliasName, Dummy)
	else if Expr is TSQLListExpression then
  	with TSQLListExpression(Expr) do
      for i := 0 to List.Count - 1 do
        ProcessElement(List[i], Fm, AliasName, Dummy)
  else if Expr is TSQLFunctionCallExpression then
  	with TSQLFunctionCallExpression(Expr) do
      for i := 0 to Arguments.Count - 1 do
      	ProcessElement(Arguments[i], Fm, AliasName, Dummy)
  else if Expr is TSQLCastExpression then
    with TSQLCastExpression(Expr) do
      ProcessElement(Value, Fm, AliasName, Dummy)
  else if Expr is TSQLExtractExpression then
    with TSQLExtractExpression(Expr) do
      ProcessElement(Value, Fm, AliasName, Dummy)
  else if Expr is TSQLLiteralExpression then
    DetectNull := TSQLLiteralExpression(Expr).Literal is TSQLNullLiteral;
end;

procedure TdxSQLParser.ParseSimpleTableRef(Stat: TSQLSelectStatement; T: TSQLSimpleTableReference);
var
  S: String;
  Fm: TdxForm;
begin
  S := T.ObjectName.Name;
  if Copy(S, 1, 3) <> '___' then Exit;
  Delete(S, 1, 3);
  S := StringReplace(S, #1, '.', [rfReplaceAll]);
  Fm := FormMan.FindFormByName(S);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [S]);
  if FSourceForms.IndexOf(Fm.FormCaption) < 0 then
    FSourceForms.Add(Fm.FormCaption);
  S := '';
  if T.AliasName <> nil then
  	S := T.AliasName.Name;
  ReplaceFieldNames(Stat, Fm, S);
  T.ObjectName.Name := TableStr(Fm.Id);
end;

procedure TdxSQLParser.ParseSelectTableRef(T: TSQLSelectTableReference);
begin
  ParseSelectStatement(T.Select);
end;

procedure TdxSQLParser.ParseJoinTableRef(Stat: TSQLSelectStatement; T: TSQLJoinTableReference);
begin
  if T.Left is TSQLJoinTableReference then
		ParseJoinTableRef(Stat, TSQLJoinTableReference(T.Left))
  else if T.Left is TSQLSelectTableReference then
  	ParseSelectTableRef(TSQLSelectTableReference(T.Left))
  else
    ParseSimpleTableRef(Stat, TSQLSimpleTableReference(T.Left));
  if T.Right is TSQLJoinTableReference then
		ParseJoinTableRef(Stat, TSQLJoinTableReference(T.Right))
  else if T.Right is TSQLSelectTableReference then
  	ParseSelectTableRef(TSQLSelectTableReference(T.Right))
  else
    ParseSimpleTableRef(Stat, TSQLSimpleTableReference(T.Right));
end;

procedure TdxSQLParser.ParseSelectStatement(Stat: TSQLSelectStatement);
var
  i: Integer;
  T: TSQLElement;
begin
  if Stat.WithSelect <> nil then
  	for i := 0 to Stat.WithSelect.SelectList.Count - 1 do
    begin
      with TSQLWithSelectElement(Stat.WithSelect.SelectList[i]) do
      	ParseSelectStatement(Select);
    end;
  for i := 0 to Stat.Tables.Count - 1 do
  begin
    T := Stat.Tables[i];
    if T is TSQLSimpleTableReference then
    	ParseSimpleTableRef(Stat, TSQLSimpleTableReference(T))
    else if T is TSQLJoinTableReference then
      ParseJoinTableRef(Stat, TSQLJoinTableReference(T))
    else if T is TSQLSelectTableReference then
    	ParseSelectTableRef(TSQLSelectTableReference(T));
  end;
  if Stat.Union <> nil then
  	ParseSelectStatement(Stat.Union);
end;

procedure TdxSQLParser.FindTableAndFields(Stat: TSQLSelectStatement);
var
  El: TSQLElement;
  T: TSQLSimpleTableReference;
  i: Integer;
  p: SizeInt;
  Als, FlNm, S, TblAls: String;
  F: TSQLSelectField;
  Ident: TSQLIdentifierExpression;
begin
  FTableName := '';
  FFields.Clear;
  if Stat.Tables.Count = 0 then Exit;
  El := Stat.Tables[0];
  while El is TSQLJoinTableReference do
    El := TSQLJoinTableReference(El).Left;
  if El is TSQLSimpleTableReference then T := TSQLSimpleTableReference(El)
  else Exit;

  FTableName := T.ObjectName.Name;
  TblAls := FTableName;
  if T.AliasName <> nil then TblAls := T.AliasName.Name;

  for i := 0 to Stat.Fields.Count - 1 do
  begin
    El := Stat.Fields[i];
    if El is TSQLSelectField then
    begin
      F := TSQLSelectField(El);
      if not (F.Expression is TSQLIdentifierExpression) then Continue;
      Ident := TSQLIdentifierExpression(F.Expression);
      if Ident.ElementIndex <> -1 then Continue;
      S := Ident.Identifier.Name;

      p := Pos('.', S);
      if p > 0 then
      begin
        Als := Copy(S, 1, p - 1);
        FlNm := Copy(S, p + 1, 255);
      end
      else
      begin
        Als := '';
        FlNm := S;
      end;
      if (Als = '') or (CompareText(Als, TblAls) = 0) then
      begin
        FFields.Add(FlNm);
        if F.AliasName <> nil then
	        FAliases.Add(F.AliasName.Name)
        else
          FAliases.Add('');
      end;
    end;
  end;
end;

constructor TdxSQLParser.Create;
begin
  FFields := TStringList.Create;
  FAliases := TStringList.Create;
  FSourceForms := TStringList.Create;
end;

destructor TdxSQLParser.Destroy;
begin
  FSourceForms.Free;
  FFields.Free;
  FAliases.Free;
  inherited Destroy;
end;

function TdxSQLParser.Parse(const SQL: String): String;
var
  El: TSQLElement;
  St: TStringStream;
  Parser: TSQLParser;
begin
  Result := '';

  St := TStringStream.Create(PrepareSQLExpr(SQL));
  Parser := TSQLParser.Create(St);
  El := nil;

  try
    El := Parser.Parse;
  	if El is TSQLSelectStatement then
    begin
	  	ParseSelectStatement(TSQLSelectStatement(El));
      Result := El.GetAsSQL([]);
      //Debug(Result);
      FindTableAndFields(TSQLSelectStatement(El));
    end;
  finally
    FreeAndNil(El);
    Parser.Free;
    St.Free;
  end;
end;

function TdxSQLParser.GetInsertSQL: String;
var
  S, F, V: String;
  i: Integer;
begin
  if FFields.Count = 0 then Exit('');
  F := ''; V := '';
	for i := 0 to FFields.Count - 1 do
  begin
    S := FFields[i];
    if FAliases[i] <> '' then S := FAliases[i];
    F := F + FFields[i] + ',';
    V := V + ':' + S + ',';
  end;
  Result := Format('insert into %s (%s) values (%s)', [FTableName,
  	Copy(F, 1, Length(F) - 1), Copy(V, 1, Length(V) - 1)]);
end;

function TdxSQLParser.GetUpdateSQL: String;
var
  i: Integer;
  S, V: String;
begin
  if FFields.Count = 0 then Exit('');
  S := '';
	for i := 0 to FFields.Count - 1 do
  begin
    V := FFields[i];
    if FAliases[i] <> '' then V := FAliases[i];
    S := S + FFields[i] + '=:' + V + ',';
  end;
  S := Copy(S, 1, Length(S) - 1);
  Result := Format('update %s set %s where id=:id', [FTableName, S]);
end;

function TdxSQLParser.GetDeleteSQL: String;
begin
  //if FFields.Count = 0 then Exit('');
	Result := Format('delete from %s where id=:id', [FTableName]);
end;

function TdxSQLParser.GetGeneratorName: String;
begin
  Result := 'gen_' + FTableName;
end;

////////////////////////////////////////////////////////////////////////////////

function SQLSelect(const SQL: String): TdxSQLQuery;
begin
  Result := TdxSQLQuery.Create(SQL);
  try
    Result.Open
  except
    on E: Exception do
    begin
    	Result.Free;
      raise;
    end;
  end;
end;

procedure SQLExecute(const SQL: String);
begin
  DBase.Execute(SQL);
end;

function ParseSQL(const SQL: String; Fm: TdxForm): String;
begin
  with TdxSQLParser.Create do
  try
    CurrentForm := Fm;
    Result := Parse(SQL);
  finally
    Free;
  end;
end;

{ TdxSQLQuery }

function ConvertValue(F: TField): String;
begin
  if F.IsNull then Result := 'null'
  else
    case F.DataType of
      ftFloat: Result := StringReplace(F.AsString, DefaultFormatSettings.DecimalSeparator, '.', []);
      ftDate: Result := '''' + Date2Str(F.AsDateTime) + '''';
      ftTime: Result := '''' + F.AsString + '''';
      ftString, ftMemo, ftBlob: Result := '''' + StringReplace(F.AsString, '''', '''' + '''', [rfReplaceAll]) + '''';
      else Result := F.AsString;
    end;
end;

function TdxSQLQuery.ProcessStatement(const S: String): String;
var
  IsParam: Boolean;
  Param: String;
  i: Integer;
  Ch: Char;
begin
  Result := '';
  IsParam := False;
  for i := 1 to Length(S) do
  begin
    Ch := S[i];
    if Ch = ':'  then
    begin
      IsParam := True;
      Param := '';
    end
    else if (Ch in [#0..#32, ',', ')']) and IsParam then
    begin
      Result := Result + ConvertValue(FDataSet.FieldByName(Param)) + Ch;
      IsParam := False;
    end
    else if IsParam then
      Param := Param + Ch
    else
      Result := Result + Ch;
  end;
end;

function TdxSQLQuery.GetUseExecuteBlock: Boolean;
begin
  Result := FDataSet.UseExecuteBlock;
end;

procedure TdxSQLQuery.SetUseExecuteBlock(AValue: Boolean);
begin
  FDataSet.UseExecuteBlock := AValue;
end;

function TdxSQLQuery.ToSQL: String;
var
  B: TBookMark;
  S, W: String;
  n, i, len: Integer;
  ISQL, USQL, DSQL: String;
begin
  S := '';
  ISQL := FDataSet.InsertSQL.Text;
  USQL := FDataSet.UpdateSQL.Text;
  DSQL := FDataSet.DeleteSQL.Text;
  B := FDataSet.GetBookmark;
  FDataSet.First;

  n := 1;  len := 0;
  S := 'SET TERM ~;' +
    'EXECUTE BLOCK AS BEGIN ';

  for i := 0 to High(FDeletedRecs) do
  begin
    if n = 255 then
    begin
      S := S + ' END~ EXECUTE BLOCK AS BEGIN ';
      n := 1;
    end;
    W := StringReplace(DSQL, ':id', IntToStr(FDeletedRecs[i]), []) + ';';
    S := S + W;
    Inc(n);
    len := len + Length(W);
  end;

  Dec(n);
  while not FDataSet.EOF do
  begin
    if FDataSet.UpdateStatus = usInserted then
    begin
      W := ProcessStatement(ISQL) + ';';
      Inc(n);
    end
    // Опытным путем выяснил, что инструкций update может быть не более 127 в
    // execute block. Поэтому update идет за 2 операции.
    else if FDataSet.UpdateStatus = usModified then
    begin
      W := ProcessStatement(USQL) + ';';
      Inc(n, 2);
    end
    else
    begin
      FDataSet.Next;
      Continue;
    end;

    len := len + Length(W);
    // Длина запроса не должна превышать 65535 байт, но почему то и на 65234
    // выдает ошибку: unexcpected end of command
    if (n >= 254) or (len > 60000) then
    begin
      S := S + ' END~ EXECUTE BLOCK AS BEGIN ';
      n := 1; len := 0;
    end;
    S := S + W;
    FDataSet.Next;
  end;

  S := S + ' END~';
  FDataSet.GotoBookmark(B);
  FDataSet.FreeBookmark(B);
  Result := S;
end;

procedure TdxSQLQuery.ApplyKeyValues;
var
  n, maxid: Integer;
  B: TBookMark;
begin
  n := 0;
  B := FDataSet.GetBookmark;
  FDataSet.First;
  while not FDataSet.EOF do
  begin
    if (FDataSet.UpdateStatus = usInserted) and FKeyField.IsNull then
      Inc(n);
    FDataSet.Next;
  end;

  maxid := DBase.GenId(FGenName, n);
  while not FDataSet.BOF do
  begin
    if (FDataSet.UpdateStatus = usInserted) and FKeyField.IsNull then
    begin
      FDataSet.Edit;
      FKeyField.Value := maxid;
      FDataSet.Post;
      Dec(maxid);
    end;
    FDataSet.Prior;
  end;
  FDataSet.GotoBookmark(B);
  FDataSet.FreeBookmark(B);
end;

function TdxSQLQuery.GetAsDT(Index: String): TDateTime;
begin
  Result := FDataSet.FieldByName(Index).AsDateTime;
end;

function TdxSQLQuery.GetAsF(Index: String): Extended;
begin
  Result := FDataSet.FieldByName(Index).AsFloat;
end;

function TdxSQLQuery.GetAsI(Index: String): Integer;
begin
  Result := FDataSet.FieldByName(Index).AsInteger;
end;

function TdxSQLQuery.GetAsS(Index: String): String;
begin
  Result := FDataSet.FieldByName(Index).AsString;
end;

function TdxSQLQuery.GetField(Index: Integer): TField;
begin
  Result := FDataSet.Fields[Index];
end;

function TdxSQLQuery.FieldCount: Integer;
begin
  Result := FDataSet.Fields.Count;
end;

{function TdxSQLQuery.GenId(IncValue: Integer): Integer;
begin
  if FKeyField = nil then Exception.Create('Key field [id] not exists');
  if FGenName = '' then Exception.Create('Generator undefined');
  if IncValue < 0 then Exception.Create('Increment value of generator must be positive');
  Result := DBase.GenId(FGenName, IncValue);
end;    }

function TdxSQLQuery.Locate(const FieldNames: String;
  FieldValues: array of Variant; Options: TLocateOptions): Boolean;
var
  VArr: Variant;
begin
  VArr := VarArrayOf(FieldValues);
  Result := FDataSet.Locate(FieldNames, VArr, Options);
  VarClear(VArr);
end;

procedure TdxSQLQuery.LoadFromStream(const FieldName: String; Stream: TStream);
var
  F: TField;
begin
  F := FDataSet.FieldByName(FieldName);
  if not F.IsBlob then raise Exception.CreateFmt('Field "%s" is not a blob.', [FieldName]);
  (F as TBlobField).LoadFromStream(Stream);
end;

procedure TdxSQLQuery.SaveToStream(const FieldName: String; Stream: TStream);
var
  F: TField;
begin
  F := FDataSet.FieldByName(FieldName);
  if not F.IsBlob then raise Exception.CreateFmt('Field "%s" is not a blob.', [FieldName]);
  (F as TBlobField).SaveToStream(Stream);
end;

function TdxSQLQuery.GetFields(Name: String): Variant;
begin
  Result := FDataSet.FieldByName(Name).Value;
end;

function TdxSQLQuery.GetState: TDataSetState;
begin
  Result := FDataSet.State;
end;

procedure TdxSQLQuery.SetAsDT(Index: String; AValue: TDateTime);
begin
	FDataSet.FieldByName(Index).AsDateTime := AValue;
end;

procedure TdxSQLQuery.SetAsF(Index: String; AValue: Extended);
begin
  FDataSet.FieldByName(Index).AsFloat := AValue;
end;

procedure TdxSQLQuery.SetAsI(Index: String; AValue: Integer);
begin
  FDataSet.FieldByName(Index).AsInteger := AValue;
end;

procedure TdxSQLQuery.SetAsS(Index: String; AValue: String);
begin
  FDataSet.FieldByName(Index).AsString := AValue;
end;

procedure TdxSQLQuery.SetFields(Name: String; AValue: Variant);
begin
  FDataSet.FieldByName(Name).Value := AValue;
end;

constructor TdxSQLQuery.Create(const SQL: String; ACurrentForm: TdxForm;
  ADisableCalcs: Boolean);
var
  P: TdxSQLParser;
begin
  P := TdxSQLParser.Create;
  P.CurrentForm := ACurrentForm;
  P.DisableCalcs := ADisableCalcs;
  try
    FSQL := P.Parse(SQL);
    FDataSet := TMySQLQuery.Create(nil);
    FDataSet.UseExecuteBlock := True;
    DBase.AttachDataSet(FDataSet);
    FDataSet.SQL.Text := FSQL;
    FDataSet.InsertSQL.Text := P.GetInsertSQL;
    FDataSet.UpdateSQL.Text := P.GetUpdateSQL;
    FDataSet.DeleteSQL.Text := P.GetDeleteSQL;
    FGenName := P.GetGeneratorName;
    FUseGenerator := ugAppend;
  finally
    P.Free;
  end;
end;

destructor TdxSQLQuery.Destroy;
begin
  if FDataSet <> nil then
  begin
	  FDataSet.Close;
  	FDataSet.Free;
  end;
  inherited Destroy;
end;

procedure TdxSQLQuery.Open;
var
  i: Integer;
begin
  FDataSet.Open;
  for i := 0 to FDataSet.Fields.Count - 1 do
  	with FDataSet.Fields[i] do
    begin
      Required := False;
      ReadOnly := False;
    end;
  FKeyField := FDataSet.FindField('id');
end;

procedure TdxSQLQuery.Close;
begin
	FDataSet.Close;
end;

function TdxSQLQuery.Opened: Boolean;
begin
  Result := FDataSet.Active;
end;

procedure TdxSQLQuery.Append;
begin
  FDataSet.Append;
  if (FUseGenerator = ugAppend) and (FKeyField <> nil) then
		FKeyField.Value := DBase.GenId(FGenName);
end;

procedure TdxSQLQuery.Edit;
begin
	FDataSet.Edit;
end;

procedure TdxSQLQuery.Delete;
var
  n: Integer;
begin
  if FKeyField <> nil then
  begin
    n := Length(FDeletedRecs);
    SetLength(FDeletedRecs, n + 1);
    FDeletedRecs[n] := FKeyField.AsInteger;
  end;

	FDataSet.Delete;
end;

procedure TdxSQLQuery.Cancel;
begin
  FDataSet.Cancel;
end;

procedure TdxSQLQuery.Post;
begin
	FDataSet.Post;
end;

procedure TdxSQLQuery.ApplyUpdates;
var
  S: String;
begin
  // UpdateSQL тоже будет пустым
  if FDataSet.InsertSQL.Count = 0 then
    raise Exception.Create('Can not applying updates. The SQL expressions for insertion and update is undefined.');
  if FUseGenerator = ugApplyUpdates then
    ApplyKeyValues;
  if UseExecuteBlock then
    S := ToSQL;
  SetLength(FDeletedRecs, 0);
  DBase.ApplyDataSet(FDataSet);
  if UseExecuteBlock then
    DBase.Execute(S)
  else
    DBase.Commit;
end;

procedure TdxSQLQuery.CancelUpdates;
begin
  SetLength(FDeletedRecs, 0);
  FDataSet.CancelUpdates;
end;

procedure TdxSQLQuery.MoveBy(Distance: Integer);
begin
	FDataSet.MoveBy(Distance);
end;

procedure TdxSQLQuery.MoveFirst;
begin
	FDataSet.First;
end;

procedure TdxSQLQuery.MovePrior;
begin
	FDataSet.Prior;
end;

procedure TdxSQLQuery.MoveNext;
begin
	FDataSet.Next;
end;

procedure TdxSQLQuery.MoveLast;
begin
	FDataSet.Last;
end;

procedure TdxSQLQuery.MoveTo(aRecNo: Integer);
begin
	MoveBy(aRecNo - RecNo);
end;

function TdxSQLQuery.BOF: Boolean;
begin
  Result := FDataSet.BOF;
end;

function TdxSQLQuery.EOF: Boolean;
begin
	Result := FDataSet.EOF;
end;

function TdxSQLQuery.RecNo: Integer;
begin
	Result := FDataSet.RecNo;
end;

function TdxSQLQuery.RecordCount: Integer;
begin
	Result := FDataSet.RecordCount;
end;

end.

