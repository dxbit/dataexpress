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
unit SqlGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, dxctrls, Db, strconsts, expressions, myclasses;

type

  { EFilterParserError }

  EFilterParserError = class(Exception)
  private
    FPos: Integer;
  public
    constructor Create(const msg: string; P: Integer);
    property Position: Integer read FPos write FPos;
  end;

  { TSQLFilterParser }

  TSQLFilterParser = class
  private
    FDisableCalcExpr: Boolean;
    FExprBuilder: TExpressionBuilder;
    FPos: Integer;
  protected
    function FieldNameParse(const FieldName: String): String; virtual;
    function CheckValue(var Value: String): Boolean; virtual;
    function CheckOp(const Op: String): Boolean; virtual;
    function GetAnotherStr: String; virtual;
  public
    function Parse(const Flt: String): String;
    property ExprBuilder: TExpressionBuilder read FExprBuilder write FExprBuilder;
    property DisableCalcExpr: Boolean read FDisableCalcExpr write FDisableCalcExpr;
    property Position: Integer read FPos;
  end;


function SqlCreateTable(Fm: TdxForm): String;
function SqlCreateField(aControl: TComponent): String;
function SqlDeleteTable(Fm: TdxForm): String;
function SqlDeleteField(aControl: TComponent): String;
function SqlCompareTables(Fm, SFm: TdxForm): String;
function SqlSelectGroups(SourceTId, SourceFId: Integer; AllFields: Boolean): String;
function SqlSelectGroupsForTree(SourceTId, SourceFId: Integer): String;
function SqlSelectStatement(Fm: TdxForm): String;
function SqlSimpleSelectStatement(Fm: TdxForm; RecId: Integer): String;
function SqlInsertStatement(Fm: TdxForm): String;
function SqlUpdateStatement(Fm: TdxForm): String;
function SqlDeleteStatement(Fm: TdxForm): String;
function SqlLookupSelect(C: TComponent; const Wh: String): String;
function SqlLookupFilter(Fm, PFm: TdxForm; DS: TDataSet; Cbx: TComponent): String;
function SqlSelectIDs(SourceTId: Integer; SId: String): String;
//function SqlLookupParamSelect(C: TComponent): String;
function SqlSelCondFilter(Fm:TdxForm; const Cond: String): String;
function SqlSelCondFilter2(SrcFm: TdxForm; const Cond: String; Fm, PFm: TdxForm;
  DS: TDataSet): String;
function SqlFormFilter(Fm: TdxForm; Flt: TFilterObject): String;
function SqlUpdateField(Fm: TdxForm; C: TComponent): String;
function SqlInitField(Fm:TdxForm; C: TComponent): String;

function TableStr(Id: Integer): String;
function FieldStr(aComponent: TComponent): String;
function FieldStr(Id: Integer): String; overload;
function GetJoinType(C: TComponent): String;

procedure DoFilterParserError(const Msg: String; P: Integer);

procedure CheckTime(fmt: TdxTimeFormat; var Bg, Ed: String);

implementation

uses
  dximages, dxfiles, apputils, formmanager, Dialogs, Variants, dxusers, dbengine,
  myctrls, dateutils;

type

  { TSQLLookupFilterParser }

  TSQLLookupFilterParser = class(TSQLFilterParser)
  private
    FCmp: TComponent;
    FSrcForm: TdxForm;
    FFieldName, FValue, FOp: String;
  protected
    function FieldNameParse(const FieldName: String): String; override;
    function CheckValue(var Value: String): Boolean; override;
    function CheckOp(const Op: String): Boolean; override;
    function GetAnotherStr: String; override;
  public
    property SrcForm: TdxForm read FSrcForm write FSrcForm;
  end;

procedure DoFilterParserError(const Msg: String; P: Integer);
begin
  raise EFilterParserError.Create(Msg, P);
end;

function TableStr(Id: Integer): String;
begin
  Result := 't' + IntToStr(Id);
end;

function FieldStr(aComponent: TComponent): String;
begin
  Result := FieldStr(GetId(aComponent));
end;

function FieldStr(Id: Integer): String;
begin
  Result := 'f' + IntToStr(Id);
end;

function GetJoinType(C: TComponent): String;
begin
  if GetRequired(C) then Result := ' inner join '
  else Result := ' left join ';
end;

function SqlCreateTable(Fm: TdxForm): String;
var
  T: String;
  C: TComponent;
  i: Integer;
begin
  T := TableStr(Fm.Id);
  //Result := 'create table ' + T + ' (id integer primary key);' +
  Result := 'create table ' + T + ' (id integer not null);' +
    'alter table ' + T + ' add primary key (id);' +
    'create sequence gen_' + T + ';';
  if Fm.PId > 0 then
    Result := Result + 'alter table ' + T + ' add pid integer;' +
      'create index i' + T + 'pid on ' + T + '(pid);';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if IsField(C) then
      Result := Result + SqlCreateField(C);
  end;
end;

function SqlCreateField(aControl: TComponent): String;
var
  ft, tn, fn: String;
  N: Integer;
begin
  Result := '';
  if aControl is TdxEdit then
    ft := 'varchar(' + IntToStr(GetFieldSize(aControl)) + ')'
  else if aControl is TdxCalcEdit then
    ft := 'double precision'
  else if aControl is TdxDateEdit then
    ft := 'date'
  else if aControl is TdxMemo then
    ft := 'varchar(' + IntToStr(GetFieldSize(aControl)) + ')'
  else if aControl is TdxCheckBox then
    ft := 'smallint'
  else if aControl is TdxComboBox then
    ft := 'varchar(' + IntToStr(GetFieldSize(aControl)) + ')'
  else if aControl is TdxLookupComboBox then
    ft := 'integer'
  else if aControl is TdxDBImage then
    ft := 'BLOB SUB_TYPE 0 SEGMENT SIZE 512'
  else if aControl is TdxFile then
    ft := 'BLOB SUB_TYPE 0 SEGMENT SIZE 512'
  else if aControl is TdxTimeEdit then
    ft := 'time'
  else if aControl is TdxCounter then
    ft := 'integer'
  else
    Exit;
  tn := TableStr(TdxForm(aControl.Owner).Id);
  fn := FieldStr(aControl);
  if aControl is TdxDBImage then
    Result := Result + 'alter table ' + tn + ' add ' + fn + 'src varchar(255);' +
      'alter table ' + tn + ' add ' + fn + 'dest varchar(150);' +
      'alter table ' + tn + ' add ' + fn + 'thumb ' + ft + ';' +
      'alter table ' + tn + ' add ' + fn + ' ' + ft + ';'
  else if aControl is TdxFile then
    Result := Result + 'alter table ' + tn + ' add ' + fn + 'src varchar(255);' +
      'alter table ' + tn + ' add ' + fn + 'dest varchar(150);' +
      'alter table ' + tn + ' add ' + fn + 'd varchar(' + IntToStr(GetFieldSize(aControl)) + ');' +
      'alter table ' + tn + ' add ' + fn + ' ' + ft + ';'
  else
    Result := Result + 'alter table ' + tn + ' add ' + fn + ' ' + ft + ';';
  if (aControl is TdxDateEdit) or (aControl is TdxLookupComboBox) or
    (aControl is TdxCounter) then
    Result := Result + 'create index i' + fn + ' on ' + tn + '(' + fn + ');';
  if aControl is TdxCounter then
  begin
    Result := Result + 'create sequence gen_' + fn + ';';
    N := TdxCounter(aControl).StartWith;
    if N <> 0 then
      Result := Result + 'alter sequence gen_' + fn + ' restart with ' +
        IntToStr(N) + ';';
  end;
end;

function SqlDeleteTable(Fm: TdxForm): String;
var
  T: String;
  i: Integer;
  C: TComponent;
begin
  T := TableStr(Fm.Id);
  Result := '';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if IsField(C) then
      Result := Result + SqlDeleteField(C);
  end;
  if Fm.PId > 0 then
    Result := Result + 'drop index i' + T + 'pid;';
  Result := Result + 'drop table ' + T + ';drop sequence gen_' + T + ';';
end;

function SqlDeleteField(aControl: TComponent): String;
var
  tn, fn: String;
begin
  Result := '';
  tn := TableStr(TdxForm(aControl.Owner).Id);
  fn := FieldStr(aControl);
  if (aControl is TdxDateEdit) or (aControl is TdxLookupComboBox) or
    (aControl is TdxCounter) then
    Result := Result + 'drop index i' + fn + ';';
  if aControl is TdxCounter then
      Result := Result + 'drop sequence gen_' + fn + ';';
  if aControl is TdxDBImage then
    Result := Result + 'alter table ' + tn + ' drop ' + fn + 'src;' +
      'alter table ' + tn + ' drop ' + fn + 'dest;' +
      'alter table ' + tn + ' drop ' + fn + 'thumb;' +
      'alter table ' + tn + ' drop ' + fn + ';'
  else if aControl is TdxFile then
    Result := Result + 'alter table ' + tn + ' drop ' + fn + 'src;' +
      'alter table ' + tn + ' drop ' + fn + 'dest;' +
      'alter table ' + tn + ' drop ' + fn + 'd;' +
      'alter table ' + tn + ' drop ' + fn + ';'
  else
    Result := Result + 'alter table ' + tn + ' drop ' + fn + ';';
end;

function IsTextField(C: TComponent): Boolean;
begin
  Result := (C is TdxMemo) or (C is TdxEdit) or (C is TdxComboBox) or
    (C is TdxFile);
end;

function SqlCompareTables(Fm, SFm: TdxForm): String;
var
  i, id: Integer;
  C, CC: TComponent;
begin
  Result := '';
  for i := 0 to SFm.ComponentCount - 1 do
  begin
    C := SFm.Components[i];
    if not IsField(C) then Continue;
    Id := GetId(C);
    CC := FindById(Fm, Id);
    if CC = nil then
      Result := Result + SQLDeleteField(C)
    else if IsTextField(CC) and IsTextField(C) then
      // пропускаем
    else if (CC.ClassName <> C.ClassName) then
      Result := Result + SQLDeleteField(C)
  end;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not IsField(C) then Continue;
    Id := GetId(C);
    CC := FindById(SFm, Id);
    if CC = nil then
      Result := Result + SQLCreateField(C) + SQLInitField(Fm, C)
    else if (IsTextField(CC) and IsTextField(C)) or ((CC is TdxCounter) and
      (C is TdxCounter)) then
      Result := Result + SQLUpdateField(Fm, C)
    else if (C.ClassName <> CC.ClassName) then
      Result := Result + SQLCreateField(C) + SQLInitField(Fm, C)
  end;
end;

function SqlSelectGroups(SourceTId, SourceFId: Integer; AllFields: Boolean
  ): String;
var
  Fm: TdxForm;
  TNm, FNm, PFNm, S: String;
  i: Integer;
  C: TComponent;
begin
  Result := '';
  if (SourceTId = 0) or (SourceFId = 0) then Exit;
  Fm := FormMan.FindForm(SourceTId);
  if Fm.ParentField = 0 then Exit;
  TNm := TableStr(SourceTId);
  FNm := FieldStr(SourceFId);
  PFNm := FieldStr(Fm.ParentField);
  Result := 'with recursive pathes as ' +
    '(select id, ' + FNm + ' from ' + TNm +
    ' where ' + PFNm + ' is null ' +
    'union all ' +
    'select t.id,p.' + FNm + ' || ''\'' || t.' + FNm + ' from ' + TNm +
    ' t join pathes p on t.' + PFNm + '=p.id) ';

  if AllFields then
  begin
    S := '';
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if IsField(C) and (GetId(C) <> SourceFId) then
      	S := S + ',' + TableStr(Fm.Id) + '.' + FieldStr(C);
        //S := S + ',t.' + FieldStr(C);
    end;
    Result := Result + 'select p.id,p.' + FNm + S + ' from pathes p inner join ' +
    	TableStr(Fm.Id) + ' on p.id=' + TableStr(Fm.Id) + '.id';
    //  TableStr(Fm.Id) + ' t on p.id=t.id';
  end
  else
    Result := Result + 'select id,' + FNm + ' from pathes';
end;

function SqlSelectGroupsForTree(SourceTId, SourceFId: Integer): String;
var
  Fm: TdxForm;
  TNm, FNm, PFNm, S: String;
  i: Integer;
  C: TComponent;
begin
  Result := '';
  if (SourceTId = 0) or (SourceFId = 0) then Exit;
  Fm := FormMan.FindForm(SourceTId);
  if Fm.ParentField = 0 then Exit;
  TNm := TableStr(SourceTId);
  FNm := FieldStr(SourceFId);
  PFNm := FieldStr(Fm.ParentField);
  Result := 'with recursive pathes as ' +
    '(select id, ' + FNm + ',' + PFNm + ' from ' + TNm +
    ' where ' + PFNm + ' is null ' +
    'union all ' +
    'select t.id,p.' + FNm + ' || ''\'' || t.' + FNm + ',' + PFNm + ' from ' + TNm +
    ' t join pathes p on t.' + PFNm + '=p.id) ';

 Result := Result + 'select id,' + FNm + ',' + PFNm + ' from pathes';
end;

function ProcessObjectField(Obj: TdxLookupComboBox; ObjF: TdxObjectField;
  var Fl, Jn: String): Boolean;
var
  Fm: TdxForm;
  C: TComponent;
  F, Ta, ST, STa: String;
begin
  Result := False;
  Fm := FormMan.FindForm(Obj.SourceTId);
  if Fm = nil then Exit;
  C := FindById(Fm, ObjF.FieldId);
  if C = nil then Exit;
  Ta := TableStr(Fm.Id) + '_' + FieldStr(Obj.Id);
  F := FieldStr(C);
  if C is TdxLookupComboBox then
    with TdxLookupComboBox(C) do
    begin
      if (SourceTId > 0) and (SourceFId > 0) then
      begin
        ST := SqlSelectGroups(SourceTId, SourceFId, True);
        if ST = '' then ST := TableStr(SourceTId)
        else ST := '(' + ST + ')';

        STa := TableStr(SourceTId) + '_' + FieldStr(Id);
        Jn := Jn + GetJoinType(C) + ST + ' ' + STa + ' on ' + Ta + '.' + F + '=' +
          STa + '.id';
        Fl := Fl + STa + '.' + FieldStr(SourceFId) + ' as ' + FieldStr(ObjF.Id) + ',';
      end
      else Exit;
    end
    else
    begin
      Fl := Fl + Ta + '.' + F + ' as ' + FieldStr(ObjF.Id) + ',';
    end;
  Result := True;
end;

function SqlSelectStatement(Fm: TdxForm): String;
var
  i, j: Integer;
  C, CC: TComponent;
  CF: TdxObjectField;
  Fl, Jn,
  T,    // главная таблица
  F,    // поле главной таблицы
  ST,   // таблица-источник (для лукап полей)
  SF,   // поле таблицы-источника
  STa   // алиас таблицы-источника
  : String;
begin
  Result := '';
  T := TableStr(Fm.Id);
  Fl := T + '.id,'; Jn := T;
  if Fm.PId > 0 then
    Fl := Fl + T + '.pid,';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    // Вставляем заглушки для неопределенных полей объектов
    if C is TdxObjectField then
    begin
      with TdxObjectField(C) do
        if (ObjId = 0) or (FieldId = 0) then
          Fl := Fl + 'null as ' + FieldStr(C) + ',';
      Continue;
    end
    else if not IsField(C) then Continue;
    F := FieldStr(C);
    Fl := Fl + T + '.' + F + ',';
    if C is TdxLookupComboBox then
    begin
      with TdxLookupComboBox(C) do
      begin
        if (SourceTId > 0) and (SourceFId > 0) then
        begin
          ST := SqlSelectGroups(SourceTId, SourceFId, True);
          if ST = '' then ST := TableStr(SourceTId)
          else ST := '(' + ST + ')';
          STa := TableStr(SourceTId) + '_' + F; //IntToStr(i);
          SF := FieldStr(SourceFId);
          Fl := Fl + STa + '.' + SF + ' as ' + F + 'l,';
          Jn := Jn + GetJoinType(C) + ST + ' ' + STa + ' on ' + T + '.' + F + '=' +
            STa + '.id';
        end
        else
          Fl := Fl + 'null as ' + F + 'l,'; // заглушка
        // поля объекта
        for j := 0 to Fm.ComponentCount - 1 do
        begin
          CC := Fm.Components[j];
          if CC is TdxObjectField then
          begin
            CF := TdxObjectField(CC);
            if (CF.ObjId = Id) and (CF.FieldId > 0) then
            begin
              if (SourceTId > 0) and (SourceFId > 0) and
                ProcessObjectField(TdxLookupComboBox(C), CF, Fl, Jn) then
                //Fl := Fl + STa + '.' + FieldStr(CF.FieldId) + ' as ' +
                  //FieldStr(CF.Id) + ','
              else
                Fl := Fl + 'null as ' + FieldStr(CF.Id) + ',';
            end;
          end;
        end;
      end;
    end
    else if C is TdxDBImage then
      Fl := Fl + T + '.' + F + 'src,' + T + '.' + F + 'dest,' + T + '.' +
        F + 'thumb,'
    else if C is TdxFile then
      Fl := Fl + T + '.' + F + 'src,' + T + '.' + F + 'dest,' + T + '.' +
        F + 'd,';
  end;
  Fl := Copy(Fl, 1, Length(Fl) - 1);
  Result := 'select ' + Fl + ' from ' + Jn;
end;

function SqlSimpleSelectStatement(Fm: TdxForm; RecId: Integer): String;
var
  i: Integer;
  C: TComponent;
  S, FNm: String;
begin
  S := 'select ';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not IsField(C) then Continue;
    FNm := FieldStr(C);
    if C is TdxFile then
      S := S + FNm + 'src,' + FNm + 'dest,' + FNm + 'd,'
    else if C is TdxDBImage then
      S := S + FNm + 'src,' + FNm + 'dest,'
    else
      S := S + FNm + ',';
  end;
  S := Copy(S, 1, Length(S) - 1);
  Result := S + ' from ' + TableStr(Fm.Id) + ' where id=' + IntToStr(RecId);
end;

function SqlInsertStatement(Fm: TdxForm): String;
var
  i: Integer;
  C: TComponent;
  Fl, Vl, FNm: String;
begin
  Fl := 'id,'; Vl := ':id,';
  if Fm.PId > 0 then begin Fl := Fl + 'pid,'; Vl := Vl + ':pid,'; end;
  Result := 'insert into ' + TableStr(Fm.Id);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not IsField(C) then Continue;
    FNm := FieldStr(C);
    //if C is TdxDBImage then FNm := FNm + 'thumb';
    Fl := Fl + FNm + ',';
    Vl := Vl + ':' + FNm + ',';
    if C is TdxDBImage then
    begin
      Fl := Fl + FNm + 'src,' + FNm + 'dest,' + FNm + 'thumb,';
      Vl := Vl + ':' + FNm + 'src,:' + FNm + 'dest,:' + FNm + 'thumb,';
    end
    else if C is TdxFile then
    begin
      Fl := Fl + FNm + 'src,' + FNm + 'dest,' + FNm + 'd,';
      Vl := Vl + ':' + FNm + 'src,:' + FNm + 'dest,:' + FNm + 'd,';
    end
  end;
  Fl := Copy(Fl, 1, Length(Fl) - 1);
  Vl := Copy(Vl, 1, Length(Vl) - 1);
  Result := Result + ' (' + Fl + ') values (' + Vl + ')';
end;

function SqlUpdateStatement(Fm: TdxForm): String;
var
  i: Integer;
  C: TComponent;
  FNm: String;
begin
  Result := 'update ' + TableStr(Fm.Id) + ' set ';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not IsField(C) then Continue;
    FNm := FieldStr(C);
    //if C is TdxDBImage then FNm := FNm + 'thumb';
    Result := Result + FNm + '=:' + FNm + ',';
    if C is TdxDBImage then
      Result := Result + FNm + 'src=:' + FNm + 'src,' + FNm + 'dest=:' +
        FNm + 'dest,' + FNm + 'thumb=:' + FNm + 'thumb,'
    else if C is TdxFile then
      Result := Result + FNm + 'src=:' + FNm + 'src,' + FNm + 'dest=:' +
        FNm + 'dest,' + FNm + 'd=:' + FNm + 'd,';
  end;
  Result := Copy(Result, 1, Length(Result) - 1) + ' where id=:id';
end;

function SqlDeleteStatement(Fm: TdxForm): String;
begin
  Result := 'delete from ' + TableStr(Fm.Id) + ' where id=:id';
end;

function SqlLookupSelect(C: TComponent; const Wh: String): String;
var
  TId, FId: Integer;
begin
  TId := GetSourceTId(C);
  FId := GetSourceFId(C);
  if (TId = 0) or (FId = 0) then Exit;
  Result := SqlSelectGroups(TId, FId, Wh <> '');
  if Result = '' then
    Result := 'select id,' + FieldStr(FId) + ' from ' + TableStr(TId);
  if Wh <> '' then Result := Result + ' where ' + Wh;

  Result := Result + ' order by ' + FieldStr(FId);
end;


{function CheckOp(const S: String): Boolean;
begin
  Result := Pos(S, '= <> <= >= == #') > 0;
end;   }

function SqlLookupFilter(Fm, PFm: TdxForm; DS: TDataSet; Cbx: TComponent
  ): String;
var
  EB: TExpressionBuilder;
  P: TSQLLookupFilterParser;
  SrcFm: TdxForm;
  Flt, S: String;
begin
  Result := '';
  SrcFm := FormMan.FindForm(GetSourceTId(Cbx));
  if SrcFm = nil then Exit;
  Flt := GetComboFilter(Cbx);

  if Flt <> '' then
  begin
    EB := TEXpressionBuilder.Create;
    EB.DataSet := DS;
    EB.Form := Fm;
    EB.ParentForm := PFm;
    EB.SkipLabels:=True;
    P := TSQLLookupFilterParser.Create;
    P.ExprBuilder := EB;
    P.SrcForm := SrcFm;
    try
      Result := P.Parse(Flt);
    finally
      P.Free;
      EB.Free;
    end;
  end;

  // !!! Доступ
  if UserMan.GetApplySelCondToObj(SrcFm.Id) then
  begin
    S := UserMan.GetSelCond(SrcFm.Id);
    if S <> '' then
    begin
      S := SqlSelCondFilter(SrcFm, S);
      if Result <> '' then Result := Result + ' and ';
      Result := Result + '(' + S + ')';
    end;
  end;
  //
end;

function CheckNumber(const S: String): String;
begin
  Result := StringReplace(S, DefaultFormatSettings.DecimalSeparator, '.', []);
end;

procedure CheckTime(fmt: TdxTimeFormat; var Bg, Ed: String);
begin
  case fmt of
    ttHH:
      begin
        if Bg <> '' then
	        Bg := FormatDateTime('hh:00:00:0000', StrToTime(Bg));
        if Ed <> '' then
	        Ed := FormatDateTime('hh:59:59:9999', StrToTime(Ed));
      end;
    ttHHMM:
      begin
        if Bg <> '' then
	        Bg := FormatDateTime('hh:nn:00:0000', StrToTime(Bg));
        if Ed <> '' then
	        Ed := FormatDateTime('hh:nn:59:9999', StrToTime(Ed));
      end;
    ttHHMMSS:
      begin
        if Bg <> '' then
	        Bg := FormatDateTime('hh:nn:ss:0000', StrToTime(Bg));
        if Ed <> '' then
	        Ed := FormatDateTime('hh:nn:ss:9999', StrToTime(Ed));
      end;
  end;
end;

{function SqlSelectTree(SourceTId, SourceFId: Integer): String;
var
  Fm: TdxForm;
  TNm, FNm, PFNm: String;
begin
  Result := '';
  Fm := FormMan.FindForm(SourceTId);
  if Fm.ParentField = 0 then Exit;
  TNm := TableStr(SourceTId);
  FNm := FieldStr(SourceFId);
  PFNm := FieldStr(Fm.ParentField);
  Result := 'with recursive pathes as ' +
    '(select id, ' + FNm + ' from ' + TNm +
    ' where ' + PFNm + ' is null ' +
    'union all ' +
    'select t.id,' + '''  '' || t.' + FNm + ' from ' + TNm +
    ' t join pathes p on t.' + PFNm + '=p.id) ' +
    'select id,' + FNm + ' from pathes order by 2';
end;

function SqlLookupParamSelect(C: TComponent): String;
var
  TId, FId: Integer;
begin
  TId := GetSourceTId(C);
  FId := GetSourceFId(C);
  if (TId = 0) or (FId = 0) then Exit;
  Result := SqlSelectTree(TId, FId);
  if Result = '' then
    Result := 'select id,' + FieldStr(FId) + ' from ' + TableStr(TId);

  Result := Result;// + ' order by ' + FieldStr(FId);
end;    }

function SqlSelCondFilter(Fm: TdxForm; const Cond: String): String;
var
  EB: TExpressionBuilder;
  P: TSQLLookupFilterParser;
begin
  EB := TExpressionBuilder.Create;
  EB.SkipLabels:=True;
  P := TSQLLookupFilterParser.Create;
  P.ExprBuilder := EB;
  P.SrcForm := Fm;
  try
    Result := P.Parse(Cond);
  finally
    P.Free;
    EB.Free;
  end;
end;

function SqlSelectIDs(SourceTId: Integer; SId: String): String;
var
  Fm: TdxForm;
  S, PFNm, TNm, Tmp: String;
  p: Integer;
begin
  Result := '';
  Fm := FormMan.FindForm(SourceTId);
  if Fm.ParentField = 0 then Exit;
  TNm := TableStr(SourceTId);
  PFNm := FieldStr(Fm.ParentField);
  S := 'with recursive pathes as ' +
    '(select id, cast(id as varchar(20)) as pid from ' + TNm +
    ' where ' + PFNm + ' is null ' +
    'union all ' +
    'select t.id, t.id || ''\'' || p.pid from ' + TNm +
    ' t join pathes p on t.' + PFNm + '=p.id) ' +
    'select pid from pathes';
  with DBase.OpenDataSet(S) do
  try
    S := '\';
    SId := '\' + SId + '\';
    while not Eof do
    begin
      Tmp := Fields[0].AsString;
      if Pos(SId, '\' + Tmp + '\') > 0 then
      begin
        p := Pos('\', Tmp);
        if p > 0 then S := S + Copy(Tmp, 1, p - 1) + '\'
        else S := S + Tmp + '\';
      end;
      Next;
    end;
    Result := S;
  finally
    Free;
  end;
end;

function SqlSelCondFilter2(SrcFm: TdxForm; const Cond: String; Fm,
  PFm: TdxForm; DS: TDataSet): String;
var
  EB: TExpressionBuilder;
  P: TSQLLookupFilterParser;
begin
  EB := TExpressionBuilder.Create;
  EB.SkipLabels:=True;
  EB.Form := Fm;
  EB.ParentForm := PFm;
  EB.DataSet := DS;
  P := TSQLLookupFilterParser.Create;
  P.ExprBuilder := EB;
  P.SrcForm := SrcFm;
  try
    Result := P.Parse(Cond);
  finally
    P.Free;
    EB.Free;
  end;
end;

procedure GetObjectFieldComponent(var C: TComponent; var FlNm: String);
var
  ObjF: TdxObjectField;
  Obj: TComponent;
  Fm: TdxForm;
begin
  ObjF := TdxObjectField(C);
  C := nil;
  Fm := TdxForm(ObjF.Owner);
  Obj := FindById(Fm, ObjF.ObjId);
  if Obj <> nil then
  begin
    Fm := FormMan.FindForm(GetSourceTId(Obj));
    if Fm <> nil then
    begin
      C := FindById(Fm, ObjF.FieldId);
      FlNm := TableStr(Fm.Id) + '_' + FieldStr(Obj) + '.' + FieldStr(ObjF.FieldId);
    end;
  end;
end;

function SqlFormFilter(Fm: TdxForm; Flt: TFilterObject): String;
var
  S, W, V, FlNm, Bg, Ed, Tmp: String;
  i, j: Integer;
  C: TComponent;
  F: TFilterField;
begin
  Result := '';
  S := '';
  for i := 0 to Flt.Count - 1 do
  begin
    F := Flt.Fields[i];
    C := FindById(Fm, F.FId);
    if C = nil then Continue;
    FlNm := TableStr(Fm.Id) + '.' + FieldStr(C);
    // Поле объекта
    if C is TdxObjectField then
    begin
      GetObjectFieldComponent(C, FlNm);
      if C = nil then Continue;
    end;
    //
    V := '';
    if F.IsNull then
      V := V + FlNm + ' is null or ';
    for j := 0 to F.Values.Count - 1 do
    begin
      W := F.Values[j];
      if W = '' then Continue;

      if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) then
        V := V + FlNm + ' containing ''' + W + ''' or '
      else if C is TdxCalcEdit then
      begin
        V := V + '(';
        Bg := F.Value[j];
        if Bg <> '' then
        begin
          Bg := CheckNumber(F.Value[j]);
          Bg := Bg + '-' + Bg + '*2e-12';
          V := V + FlNm + '>=' + Bg + ' and ';
        end;
        Ed := F.EndValue[j];
        if Ed <> '' then
        begin
          Ed := CheckNumber(Ed);
          Ed := Ed + '+' + Ed + '*2e-12';
          V := V + FlNm + '<=' + Ed + ' and ';
        end;
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if C is TdxCounter then
      begin
        Bg := CheckNumber(F.Value[j]);
        Ed := CheckNumber(F.EndValue[j]);
        V := V + '(';
        if Bg <> '' then
          V := V + FlNm + '>=' + Bg + ' and ';
        if Ed <> '' then
          V := V + FlNm + '<=' + Ed + ' and ';
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if C is TdxDateEdit then
      begin
        if Copy(W, 1, 1) = '$' then
        begin
          Ed := Date2Str(Date);
          case TPeriodType(StrToInt(Copy(W, 2, 10))) of
            ptToday: Bg := Date2Str(Date);
            ptThisWeek: Bg := Date2Str( IncDay(Date, -DayOfTheWeek(Date)+1) );
            ptThisMonth: Bg := Date2Str( IncDay(Date, -DayOf(Date)+1) );
            ptThisYear: Bg := Date2Str( EncodeDate(YearOf(Date), 1, 1) );
          end;
        end
        else
        begin
          Bg := F.Value[j];
          Ed := F.EndValue[j];
        end;
        V := V + '(';
        if Bg <> '' then
          V := V + FlNm + '>=''' + Bg + ''' and ';
        if Ed <> '' then
          V := V + FlNm + '<=''' + Ed + ''' and ';
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if C is TdxTimeEdit then
      begin
        Bg := F.Value[j];
        Ed := F.EndValue[j];
        CheckTime(TdxTimeEdit(C).TimeFormat, Bg, Ed);
        V := V + '(';
        if Bg <> '' then
          V := V + FlNm + '>=''' + Bg  + ''' and ';
        if Ed <> '' then
          V := V + FlNm + '<=''' + Ed + ''' and ';
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if C is TdxCheckBox then
        V := V + FlNm + '=' + W + ' or '
      else if C is TdxLookupComboBox then
      begin
        Tmp := SqlSelectIds(GetSourceTId(C), W);
        if Tmp <> '' then
          V := V + '''' + Tmp + ''' containing ''\'' || ' + FlNm + ' || ''\'' or '
        else
          V := V + FlNm + '=' + W + ' or ';
      end;
    end;
    V := Copy(V, 1, Length(V) - 4);
    if V <> '' then
    begin
      if F.IsNot then V := 'not (' + V + ')';
      S := S + '(' + V + ') and ';
    end;
  end;
  S := Copy(S, 1, Length(S) - 5);
  Result := S;
end;

function SqlUpdateField(Fm: TdxForm; C: TComponent): String;
var
  FS: Integer;
  S, TNm, FNm: String;
begin
  S := '';
  if (C is TdxEdit) or (C is TdxMemo) or (C is TdxComboBox) or (C is TdxFile) then
  begin
    FS := GetFieldSize(C);

    if FS <> GetOldSize(C) then
    begin
      TNm := TableStr(Fm.Id);
      FNm := FieldStr(C);
      if C is TdxFile then FNm := FNm + 'd';
      S := S + 'alter table ' + TNm + ' alter column ' + FNm + ' to tmp;' +
        'alter table ' + TNm + ' add ' + FNm + ' varchar(' + IntToStr(FS) + ');' +
        'commit;' +
        'update ' + TNm + ' set ' + FNm + '=LEFT(tmp,' + IntToStr(FS) + ');' +
        'commit;' +
        'alter table ' + TNm + ' drop tmp;' +
        'commit;';
      SetOldSize(C, FS);
    end
  end
  else if C is TdxCounter then
  begin
    with TdxCounter(C) do
      if Restart then
      begin
        S := S + 'alter sequence gen_' + FieldStr(C) + ' restart with ' +
          IntToStr(StartWith) + ';';
        Restart := False;
      end;
  end;
  Result := S;
end;

function SqlInitField(Fm: TdxForm; C: TComponent): String;
begin
  Result := '';
  if (C is TdxCalcEdit) or (C is TdxCheckBox) then
    Result := 'commit;update ' + TableStr(Fm.Id) + ' set ' + FieldStr(C) +
      '=0;commit;'
end;

{ EFilterParserError }

constructor EFilterParserError.Create(const msg: string; P: Integer);
begin
  FPos := P;
  inherited Create(msg);
end;

{ TSQLLookupFilterParser }

function TSQLLookupFilterParser.FieldNameParse(const FieldName: String): String;
begin
  Result := '';
  FCmp := FindComponentByFieldName(FSrcForm, FieldName);
  if (FCmp = nil) or (FCmp is TdxFile) or (FCmp is TdxDBImage) then Exit;
  Result := TableStr(FSrcForm.Id) + '.' + FieldStr(FCmp);
  FFieldName := Result;
end;

function TSQLLookupFilterParser.CheckValue(var Value: String): Boolean;
var
  Tmp: String;
begin
  Result := CheckType(FCmp, Value);
  // экранируем апострофы
  if (FCmp is TdxEdit) or (FCmp is TdxMemo) or (FCmp is TdxComboBox) or
    (FCmp is TdxDateEdit) then
    Value := '''' + StringReplace(Value, #39, #39#39, [rfReplaceAll]) + ''''
	else if FCmp is TdxCalcEdit then
  begin
    if FOp = '>=' then
    	Value := Value + '-' + Value + '*2e-12'
    else if FOp = '<=' then
    	Value := Value + '+' + Value + '*2e-12';
  end
  else if FCmp is TdxTimeEdit then
  begin
    if FOp = '>=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Value, Tmp);
      Value := '''' + Value + '''';
    end
    else if FOp = '<=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Tmp, Value);
      Value := '''' + Value + '''';
    end
    else
    	Value := '''' + Value + '''';
  end;
  FValue := Value;
end;

function TSQLLookupFilterParser.CheckOp(const Op: String): Boolean;
begin
  FOp := Op;
  Result:=inherited CheckOp(Op);
  if (FCmp is TdxLookupComboBox) and (Op <> '=') and (Op <> '<>') then Result := False;
end;

function TSQLLookupFilterParser.GetAnotherStr: String;
var
  Fm: TdxForm;
  TId: Integer;
  Ed: String;
begin
  Result := '';
  if FCmp is TdxLookupComboBox then
  begin
    TId := GetSourceTId(FCmp);
    Fm := FormMan.FindForm(TId);
    if (Fm <> nil) and (Fm.ParentField > 0) then
    begin
      FValue := SqlSelectIDs(TId, FValue);
      Result := '''' + FValue + ''' containing ''\'' || ' + FFieldName + ' || ''\''';
      if FOp = '<>' then Result := 'not (' + Result + ')';
    end;
  end
  else if FCmp is TdxCalcEdit then
  begin
    if FOp = '=' then
    begin
      Result := FFieldName + '>=' + FValue + '-' + FValue + '*2e-12 and ' +
      	FFieldName + '<=' + FValue + '+' + FValue + '*2e-12';
    end;
  end
  else if FCmp is TdxTimeEdit then
  begin
    if FOp = '=' then
    begin
      Ed := FValue;
      CheckTime(TdxTimeEdit(FCmp).TimeFormat, FValue, Ed);
      Result := FFieldName + '>=''' + FValue + ''' and ' +
      	FFieldName + '<=''' + Ed + '''';
    end;
  end;
end;


////////////////////////////////////////////////////////////////////////////////

{ TSQLFilterParser }

function TSQLFilterParser.FieldNameParse(const FieldName: String): String;
begin
  Result := '';
end;

function TSQLFilterParser.CheckValue(var Value: String): Boolean;
begin
  Result := True;
end;

function TSQLFilterParser.CheckOp(const Op: String): Boolean;
begin
  Result := Pos(Op, '= <> <= >= == #') > 0;
end;

// Эта функция введена для изменения строки условия, если полем является
// иерархическая группа
function TSQLFilterParser.GetAnotherStr: String;
begin
  Result := '';
end;

// Если необязательные поля идут после, то возможна ситуация, когда в
// конце оказывается and или or. В этом случае добавляем фиктивное условие.
function TrailLastOp(const S: String): String;
var
  Tmp: String;
  L: Integer;
begin
  Result := S;
  L := Length(S);
  Tmp := Trim(Copy(S, L - 4, 4));
  if Tmp = 'and'  then Result := S + ' 1=1 '
  else if Tmp = 'or' then Result := S + ' 1=0 ';
end;

function TSQLFilterParser.Parse(const Flt: String): String;
type
  TState = (stField, stOp, stExpr, stBoolOp, stBrace);
var
  S, Wh, Expr, Op, BlOp, FlNm, Tmp: String;
  l, BrCnt, BrN, ExprPos: Integer;
  Tk: Char;
  St: TState;
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
  Optional, NeedBrace: Boolean;
begin
  Result := '';
  Wh := '';
  FPos := 1;
  St := stField;
  EB := FExprBuilder;
  E := nil;
  BrN := 0;

  try

  while True do
  begin
    S := ReadToken(Flt, FPos, Tk);
    if (S = '{') and (St = stField) then
    begin
      Inc(BrN);
      Wh := Wh + '(';
      Continue;
    end;
    case St of
      stField:
        begin
          if Tk = '[' then
          begin
            BlOp := '';
            Optional := False;
            if (Length(S) > 0) and (S[1] = '?') then
            begin
              Optional := True;
              Delete(S, 1, 1);
            end;
            FlNm := FieldNameParse(S);
            if FlNm = '' then DoFilterParserError(Format(rsFPSrcFldNotFound, [S]), FPos);
            St := stOp;
          end
          else if Tk = #0 then Break
          else DoFilterParserError(rsFPSrcFldExcept, FPos);
        end;
      stOp:
        begin
          if Tk = '=' then
          begin
            if not CheckOp(S) then DoFilterParserError(rsInvalidCmpOp, FPos);
            St := stExpr;
            BrCnt := 0;
            Expr := '';
            if S = '==' then S := ' containing '
            else if S = '#' then S := ' not containing ';
            Op := S;
            ExprPos := FPos;
          end
          else DoFilterParserError(rsFPCmpOpExpect, FPos);
        end;
      stExpr:
        begin
          NeedBrace := S = '}';
          if (Tk = #0) or (((S = '&') or (S = '|')) and (BrCnt = 0))
            or (NeedBrace) then
          begin
            try
              FreeAndNil(E);
              E := EB.Build(Expr);
              if E = nil then DoFilterParserError(rsFPExprNotParsed, FPos);
              if not FDisableCalcExpr then V := E.Calc
            except
              on Err: ECalcError do
                DoFilterParserError(Format(rsFPErrInExpr, [Expr, Err.Message]), ExprPos + Err.Position)
            end;
            if (V = Null) and Optional then
            else if V = Null then
            begin
              if Op = '<>' then
                S := ' is not null '
              else
                S := ' is null ';
              Wh := Wh + FlNm + S;
            end
            else
            begin
              S := VarToStr(V);
              //if not CheckValue(S) then DoFilterParserError(rsIncompatibleTypes, FPos);
              if not CheckValue(S) then DoFilterParserError(rsIncompatibleTypesSrcFieldAndExpr, FPos);
              Tmp := GetAnotherStr;
              if Tmp = '' then
                Wh := Wh + FlNm + Op + S
              else
                Wh := Wh + Tmp;
            end;
            if NeedBrace then
            begin
              Dec(FPos);
              St := stBrace;
            end
            else if Tk <> #0 then
            begin
              St := stBoolOp;
              Dec(FPos);
            end
            else Break;
          end
          else
          begin
            if S = '(' then
            begin
              Inc(BrCnt);
              Expr := Expr + S;
            end
            else if S = ')' then
            begin
              Dec(BrCnt);
              Expr := Expr + S;
            end
            else if Tk = '[' then Expr := Expr + '[' + S + ']'
            else if Tk = '''' then
            begin
              if Pos('"', S) > 0 then
                Expr := Expr + '''' + S + ''''
              else
                Expr := Expr + '"' + S + '"';
            end
            else Expr := Expr + S;
          end;
        end;
      stBoolOp:
        begin
          if Optional and (V = Null) then
          else
          begin
            if S = '&' then Wh := Wh + ' and '
            else if S = '|' then Wh := Wh + '  or '
            else DoFilterParserError(rsFPBoolOpExpect, FPos);
            BlOp := S;
          end;
          St := stField;
        end;
      stBrace:
        begin
          if S = '}' then
          begin
            Wh := TrailLastOp(Wh);
            l := Length(Wh);
            if Copy(Wh, l, 1) = '(' then
              Delete(Wh, l, 1)
            else
            begin
              Wh := Wh + ')';
              Optional := False;		// Сбрасываем флаг, иначе идущая после
              											// логическая операция проигнорируется (баг от 28.03.2017).
            end;
            Dec(BrN);
          end
          else if (S = '|') or (S = '&') then
          begin
	          BlOp := S;
	 	        Dec(FPos);
  	  	    St := stBoolOp;
          end
          else if Tk = #0 then Break
          else DoFilterParserError(rsFPBoolOpExpect, FPos);
        end;
    end;
  end;
  if (BlOp = '|') or (BlOp = '&') then DoFilterParserError(rsFPSrcFldExcept, FPos)
  else if BrN < 0 then DoFilterParserError(rsCharLBrExpext, FPos)
  else if BrN > 0 then DoFilterParserError(rsCharRBrExpect, FPos);
  // Если необязательные поля идут после, то возможна ситуация, когда в
  // конце оказывается and или or
  //S := Trim(Copy(Wh, Length(Wh) - 4, 4));
  //if (S = 'and') or (S = 'or') then Wh := Copy(Wh, 1, Length(Wh) - 4);
  Result := TrailLastOp(Wh);

  finally
    FreeAndNil(E);
  end;
end;

end.

