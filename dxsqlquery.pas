{
Copyright © 2015-2017 Pavel Duborkin
Author: Pavel Duborkin
E-Mail: 7bit@list.ru, mydataexpress@mail.ru

This file is part of DataExpress.

DataExpress is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

DataExpress is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with DataExpress.  If not, see <http://www.gnu.org/licenses/>.
}
unit DxSQLQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, myfpsqlparser, myfpsqltree, strconsts, sqldb, db,
  Dialogs;

type

  { TdxSQLQuery }

  TdxSQLQuery = class
  private
    FSQL: String;
    FDataSet: TSQLQuery;
    function GetAsDT(Index: String): TDateTime;
    function GetAsF(Index: String): Extended;
    function GetAsI(Index: String): Integer;
    function GetAsS(Index: String): String;
    function GetField(Index: Integer): TField;
    function GetFields(Name: String): Variant;
  public
    constructor Create(const SQL: String);
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function Opened: Boolean;
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
    property Fields[Name: String]: Variant read GetFields; default;
    property Field[Index: Integer]: TField read GetField;
    property AsI[Index: String]: Integer read GetAsI;
    property AsF[Index: String]: Extended read GetAsF;
    property AsDT[Index: String]: TDateTime read GetAsDT;
    property AsS[Index: String]: String read GetAsS;
    property DataSet: TSQLQuery read FDataSet;
  end;

function SQLSelect(const SQL: String): TdxSQLQuery;
function DxSQLToNativeSQL(const SQL: String): String;

implementation

uses
  LazUtf8, formmanager, dxctrls, sqlgen, dbengine;

function ReplaceBrackets(const S: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    if S[i] = '[' then Result := Result + '"___'
    else if S[i] = ']' then Result := Result + '"'
    else Result := Result + S[i];
  end;
end;

function ReplaceFieldName(S: String; Fm: TdxForm; const AliasName: String): String;
var
  p: SizeInt;
  FmNm, FlNm: String;
  C: TComponent;
begin
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
    if (Utf8CompareText(FmNm, Fm.FormCaption) = 0) or
  	  (Utf8CompareText(FmNm, AliasName) = 0) then
    begin
      if AliasName <> '' then Result := AliasName
      else Result := TableStr(Fm.Id);

      FlNm := Copy(S, p + 1, 1024);
      if Copy(FlNm, 1, 3) = '___' then
      begin
        Delete(FlNm, 1, 3);
	      C := FindComponentByFieldName(Fm, FlNm);
        if C = nil then raise Exception.CreateFmt(rsFieldNotFound,
        	[Fm.FormCaption + ' -> ' + FlNm]);
        Result := Result + '.' + FieldStr(C);
      end
      else Result := Result + '.' + FlNm;
    end
  end
  // Если в имени поля форма не указана, то пытаемся определить автоматически
  else
  begin
    FlNm := S;
    if Copy(FlNm, 1, 3) = '___' then
    begin
      Delete(FlNm, 1, 3);
      C := FindComponentByFieldName(Fm, FlNm);
      if C = nil then Exit;
      if AliasName <> '' then
        Result := AliasName + '.' + FieldStr(C)
      else
        Result := TableStr(Fm.Id) + '.' + FieldStr(C);
    end
  end;
end;

procedure ProcessSQLExpression(Expr: TSQLExpression; Fm: TdxForm; const AliasName: String); forward;
procedure ParseSelectStatement(Stat: TSQLSelectStatement); forward;

procedure ProcessElement(El: TSQLElement; Fm: TdxForm; const AliasName: String);
begin
  if El is TSQLSelectField then
  	ProcessSQLExpression(TSQLSelectField(El).Expression, Fm, AliasName)
  else if El is TSQLOrderByElement then
  	ProcessElement(TSQLOrderByElement(El).Field, Fm, AliasName)
  else if El is TSQLIdentifierName then
  	with TSQLIdentifierName(El) do
    	Name := ReplaceFieldName(Name, Fm, AliasName)
  else if El is TSQLExpression then
  	ProcessSQLExpression(TSQLExpression(El), Fm, AliasName);
end;

procedure ProcessJoinTableRef(Join: TSQLJoinTableReference; Fm: TdxForm; const AliasName: String);
begin
  ProcessSQLExpression(Join.JoinClause, Fm, AliasName);
  if Join.Left is TSQLJoinTableReference then
  	ProcessJoinTableRef(TSQLJoinTableReference(Join.Left), Fm, AliasName);
  if Join.Right is TSQLJoinTableReference then
  	ProcessJoinTableRef(TSQLJoinTableReference(Join.Right), Fm, AliasName);
end;

procedure ReplaceFieldNames(Stat: TSQLSelectStatement; Fm: TdxForm; const AliasName: String);
var
  i: Integer;
  F, T: TSQLElement;
begin
  for i := 0 to Stat.Fields.Count - 1 do
  begin
    F := Stat.Fields[i];
    ProcessElement(F, Fm, AliasName);
  end;
  for i := 0 to Stat.Tables.Count - 1 do
  begin
    T := Stat.Tables[i];
    if T is TSQLJoinTableReference then
    	ProcessJoinTableRef(TSQLJoinTableReference(T), Fm, AliasName);
  end;
  ProcessSQLExpression(Stat.Where, Fm, AliasName);
  ProcessSQLExpression(Stat.Having, Fm, AliasName);
  for i := 0 to Stat.GroupBy.Count - 1 do
    ProcessElement(Stat.GroupBy[i], Fm, AliasName);
  for i := 0 to Stat.OrderBy.Count - 1 do
  	ProcessElement(Stat.OrderBy[i], Fm, AliasName);
end;

procedure ProcessSQLExpression(Expr: TSQLExpression; Fm: TdxForm; const AliasName: String);
var
  i: Integer;
begin
  if Expr = nil then Exit;
  if Expr is TSQLIdentifierExpression then
    with TSQLIdentifierExpression(Expr) do
	    Identifier.Name := ReplaceFieldName(Identifier.Name, Fm, AliasName)
  else if Expr is TSQLUnaryExpression then
  	ProcessSQLExpression(TSQLUnaryExpression(Expr).Operand, Fm, AliasName)
  else if Expr is TSQLBinaryExpression then
  begin
    ProcessSQLExpression(TSQLBinaryExpression(Expr).Left, Fm, AliasName);
    ProcessSQLExpression(TSQLBinaryExpression(Expr).Right, Fm, AliasName);
  end
  else if Expr is TSQLTernaryExpression then
  begin
    ProcessSQLExpression(TSQLTernaryExpression(Expr).Left, Fm, AliasName);
    ProcessSQLExpression(TSQLTernaryExpression(Expr).Middle, Fm, AliasName);
    ProcessSQLExpression(TSQLTernaryExpression(Expr).Right, Fm, AliasName);
  end
  else if Expr is TSQLSelectionExpression then
  begin
  	ReplaceFieldNames(TSQLSelectionExpression(Expr).Select, Fm, AliasName);
    ParseSelectStatement(TSQLSelectionExpression(Expr).Select);
  end
  else if Expr is TSQLAggregateFunctionExpression then
  	ProcessSQLExpression(TSQLAggregateFunctionExpression(Expr).Expression, Fm, AliasName)
	else if Expr is TSQLListExpression then
  	with TSQLListExpression(Expr) do
      for i := 0 to List.Count - 1 do
        ProcessElement(List[i], Fm, AliasName)
  else if Expr is TSQLFunctionCallExpression then
  	with TSQLFunctionCallExpression(Expr) do
      for i := 0 to Arguments.Count - 1 do
      	ProcessElement(Arguments[i], Fm, AliasName);
end;

procedure ParseSimpleTableRef(Stat: TSQLSelectStatement; T: TSQLSimpleTableReference);
var
  S: String;
  Fm: TdxForm;
begin
  S := T.ObjectName.Name;
  if Copy(S, 1, 3) <> '___' then Exit;
  Delete(S, 1, 3);
  Fm := FormMan.FindFormByName(S);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [S]);
  S := '';
  if T.AliasName <> nil then
  	S := T.AliasName.Name;
  ReplaceFieldNames(Stat, Fm, S);
  T.ObjectName.Name := TableStr(Fm.Id);
end;

procedure ParseSelectTableRef(T: TSQLSelectTableReference);
begin
  ParseSelectStatement(T.Select);
end;

procedure ParseJoinTableRef(Stat: TSQLSelectStatement; T: TSQLJoinTableReference);
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

procedure ParseSelectStatement(Stat: TSQLSelectStatement);
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

////////////////////////////////////////////////////////////////////////////////

function SQLSelect(const SQL: String): TdxSQLQuery;
begin
  Result := TdxSQLQuery.Create(SQL);
  try
    Result.Open
  except
  	Result.Free;
    raise;
  end;
end;

function DxSQLToNativeSQL(const SQL: String): String;
var
  St: TStringStream;
  Parser: TSQLParser;
  El: TSQLElement;
begin
  Result := '';
	St := TStringStream.Create(ReplaceBrackets(SQL));
  Parser := TSQLParser.Create(St);
  try
	  El := Parser.Parse;
    //ShowMessage(El.ClassName);
  	if El is TSQLSelectStatement then
    begin
	  	ParseSelectStatement(TSQLSelectStatement(El));
      Result := El.GetAsSQL([]);
    end;
  finally
	  Parser.Free;
  	St.Free;
  end;
end;

{ TdxSQLQuery }

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

function TdxSQLQuery.GetFields(Name: String): Variant;
begin
  Result := FDataSet.FieldByName(Name).Value;
end;

constructor TdxSQLQuery.Create(const SQL: String);
begin
  FSQL := DxSQLToNativeSQL(ReplaceBrackets(SQL));
  FDataSet := TSQLQuery.Create(nil);
  DBase.AttachDataSet(FDataSet);
  FDataSet.SQL.Text := FSQL;
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
begin
  FDataSet.Open;
end;

procedure TdxSQLQuery.Close;
begin
	FDataSet.Close;
end;

function TdxSQLQuery.Opened: Boolean;
begin
  Result := FDataSet.Active;
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

