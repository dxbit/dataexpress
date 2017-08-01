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
unit ExprFuncs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dxctrls, Db, strconsts, dxreports, Controls;

type
  PVrData = ^TVrData;
  TVrData = record
    Name: String;
    Value: Variant;
  end;

  { TVarList }

  TVarList = class(TList)
  private
    function GetVars(Index: Integer): PVrData;
  public
    function AddVar(const aName: String; aValue: Variant): PVrData;
    function FindVar(const aName: String): PVrData;
    procedure Clear; override;
    property Vars[Index: Integer]: PVrData read GetVars; default;
  end;

  EGetQueryDataError = class(Exception);

procedure GetFormData(aForm: TdxForm; const FormName, FieldName: String; var Fm: TdxForm;
  var DS: TDataSet; var F: TField);
procedure GetQueryData(aForm: TdxForm; const QueryName, FieldName: String;
  var RD: TReportData; var DS: TDataSet; var F: TField);
function CalcAggFunc(aForm, aParForm: TdxForm; aDataSet: TDataSet; const FormName,
  FieldName, Condition: String; Func: TRpTotalFunc): Variant;
function FirstLetterUpper(const S: String): String;
function MyFrac(E: Extended; Digits: Byte): LongInt;
function GetWeekName(D: TDateTime; Brief: Boolean): String;
function GetMonthName(D: TDateTime; Brief: Boolean): String;
function IIF(Condition: Boolean; Value1, Value2: Variant): Variant;
function MyPower(V1, V2: Extended): Extended;
function GetRecNo(aForm: TdxForm; const FormName: String): Variant;
function MyRoundToStr(V: Extended; Digits: Integer): String;
function MyHoursBetween(D1, T1, D2, T2: TDateTime): Int64;
function MyMinutesBetween(D1, T1, D2, T2: TDateTime): Int64;
function MySecondsBetween(D1, T1, D2, T2: TDateTime): Int64;
function AddHour(Fm: TdxForm; DS: TDataSet; D, T: TDateTime; N: Integer;
  const DateField, Func: String): TDateTime;
function MyIndexOf(i: Integer; const S: String): String;
function FmtDate(DT: TDateTime): String;
function MergeRows(aForm: TdxForm; const FormName, FieldName, Delim: String): Variant;
function GetRecId(aForm, aParForm: TdxForm; const FormName: String): Variant;
function GetObjId(const FormName, FieldName, FieldValue: String): Variant;
function GetUser: String;
function GetRole: String;
function DBGet(aForm, aParForm: TdxForm; aDS: TDataSet;
  const FormName, FieldName, Filter: String; Func: TRpTotalFunc): Variant;
function DBGetById(const FormName, FieldName: String; aId: Integer): Variant;
function GetMaxV(Vals: array of Variant): Variant;
function GetMinV(Vals: array of Variant): Variant;
function SetVar(const aName: String; aValue: Variant): Variant;
function GetVar(const aName: String): Variant;
function CalcPeriod(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String;
function CalcPeriodRu(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String;
function SumPeriod(aForm: TdxForm; const FormName, Date1, Date2: String; Detail: Integer): String;
function Block(Vals: array of Variant): Variant;
function VarExists(const aName: String): Integer;
function PathLength(const Path: String): Integer;
function ExtractPath(const Path: String; aStart, aLen: Integer): String;
function IsNewRec(DS: TDataSet): Integer;
function GetFieldValue(aForm: TdxForm; const FormName, FieldName: String): Variant;
function GetOldValue(aForm: TdxForm; DS: TDataSet; const FieldName: String): Variant;
function BeginYear(D: TDateTime): TDateTime;
function BeginMonth(D: TDateTime): TDateTime;
function BeginWeek(D: TDateTime): TDateTime;
function EndYear(D: TDateTime): TDateTime;
function EndMonth(D: TDateTime): TDateTime;
function EndWeek(D: TDateTime): TDateTime;
function BeginQuarter(D: TDateTime): TDateTime;
function EndQuarter(D: TDateTime): TDateTime;
function QuarterOf(D: TDateTime): Word;
function DBUnique(aForm: TdxForm; aDS: TDataSet; const Fields: String): Integer;
function IsUniqueRecords(aForm: TdxForm; const FormName, Fields: String): Integer;
function MsgBox(const Caption, Msg: String): Variant;
function YesNoBox(const Caption, Msg: String): Integer;
function YesNoCancelBox(const Caption, Msg: String): Integer;
function SetField(aForm: TdxForm; aDS: TDataSet; const aFieldName: String; Value: Variant): Variant;
function SetLabel(aForm: TdxForm; const aFieldName: String; Value: Variant): Variant;
function IsEditRec(aDS: TDataSet): Integer;
function IsModifiedRec(aDS: TDataSet): Integer;

var
  VarList: TVarList;

implementation

uses
  formmanager, LazUtf8, sqlgen, reportmanager, dbengine,
  apputils, dateutils, dialogs, Math, expressions, Variants, StrUtils, appsettings,
  dxusers;

procedure GetFormData(aForm: TdxForm; const FormName, FieldName: String; var Fm: TdxForm;
  var DS: TDataSet; var F: TField);
var
  Grid: TdxGrid;
  C: TComponent;
begin
  if aForm = nil then raise Exception.Create(rsFormNotAvail);
  DS := nil; F := nil;
  Fm := FormMan.FindFormByName(FormName);
  if (Fm <> nil) and (Fm.PId = aForm.Id) then
  begin
    Grid := FindGridById(aForm, Fm.Id);
    DS := Grid.DataSource.DataSet;
    if FieldName <> '' then
    begin
      C := FindComponentByFieldName(Fm, FieldName);
      if C = nil then raise Exception.Create(Format(rsFieldNotFound, [FieldName]));
      F := DS.FieldByName(FieldStr(C));
    end;
  end
  else Fm := nil;
end;

function FindQueryGrid(aForm: TdxForm; Id: Integer): TdxQueryGrid;
var
  j: Integer;
  C: TComponent;
begin
  Result := nil;
  for j := 0 to aForm.ComponentCount - 1 do
  begin
    C := aForm.Components[j];
    if (C is TdxQueryGrid) and (TdxQueryGrid(C).Id = Id) then
      Exit(TdxQueryGrid(C));
  end;
end;

procedure GetQueryData(aForm: TdxForm; const QueryName, FieldName: String;
  var RD: TReportData; var DS: TDataSet; var F: TField);
var
  QG: TdxQueryGrid;
  Col: TRpGridColumn;
begin
  if aForm = nil then raise Exception.Create(rsFormNotAvail);
  DS := nil; F := nil;
  RD := ReportMan.FindQueryByName(QueryName);
  if RD = nil then raise Exception.Create(Format(rsQueryNotFound, [QueryName]));
  if RD.Sources.Count > 0 then
  begin
    QG := FindQueryGrid(aForm, RD.Id);
    if QG <> nil then
    begin
      DS := QG.DataSource.DataSet;
      if not DS.Active then raise EGetQueryDataError.Create('Dataset inactive');
      if FieldName <> '' then
      begin
        Col := RD.Grid.FindColumnByTitle(FieldName);
        if Col = nil then raise Exception.Create(Format(rsFieldNotFound, [FieldName]));
        F := DS.FieldByName(Col.FieldName);
      end;
    end;
  end;
end;

function CalcAggFunc(aForm, aParForm: TdxForm; aDataSet: TDataSet;
  const FormName, FieldName, Condition: String; Func: TRpTotalFunc): Variant;
var
  Fm: TdxForm;
  DS: TDataSet;
  F: TField;
  E: TExpression;
  N: Integer;
  Sum: Extended;
  Mn, Mx, V: Variant;
  AfterScroll, BeforeScroll: TDataSetNotifyEvent;
  B: TBookmark;
  FirstRec: Boolean;
  Rp: TReportData;
  OldState: TDataSetState;
begin
  if (FieldName = '') and (Func <> tfCount) then raise Exception.Create(rsFieldNameEmpty);
  Result := Null;
  E := nil;
  GetFormData(aForm, FormName, FieldName, Fm, DS, F);
  if Fm <> nil then
  begin
    if Condition <> '' then
      with TExpressionBuilder.Create do
      try
        Form := Fm;
        ParentForm := aForm;
        DataSet := DS;
        SkipLabels:=True;
        E := Build(Condition);
      finally
        Free;
      end;
  end
  else
  begin
    GetQueryData(aForm, FormName, FieldName, Rp, DS, F);
    if Condition <> '' then
      with TExpressionBuilder.Create do
      try
        Form := aForm;
        ParentForm := aParForm;
        DataSet := aDataSet;
        RD := Rp;
        RDSet := DS;
        SkipLabels:=True;
        E := Build(Condition);
      finally
        Free;
      end;
  end;
  if DS = nil then
    raise Exception.Create(Format(rsFormQryNotFound, [FormName]));

  FirstRec := True;
  AfterScroll := DS.AfterScroll;
  BeforeScroll := DS.BeforeScroll;
  DS.AfterScroll := nil;
  DS.BeforeScroll := nil;
  B := DS.GetBookmark;
  DS.DisableControls;
  N := 0; Sum := 0;
  Mx := Null; Mn := Null;
  OldState := DS.State;
  try
    DS.First;
    while not DS.EOF do
    begin
      if E <> nil then
      begin
        V := E.Calc;
        if VarIsBool(V) and (V = False) then
        begin
          DS.Next;
          Continue;
        end;
      end;
      if F <> nil then
      begin
        if Func in [tfMax, tfMin] then
        begin
          if FirstRec then
          begin
            Mx := F.Value;
            Mn := Mx;
            FirstRec := False;
          end
          else
          begin
            if F.Value > Mx then Mx := F.Value;
            if F.Value < Mn then Mn := F.Value;
          end;
        end
        else if Func in [tfSum, tfAvg] then
          Sum := Sum + F.AsFloat;
      end;
      Inc(N);
      DS.Next;
    end;
  finally
    FreeAndNil(E);
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    DS.EnableControls;
    DS.AfterScroll:=AfterScroll;
    DS.BeforeScroll:=BeforeScroll;
    if OldState in [dsInsert, dsEdit] then
      DS.Edit;
  end;

  case Func of
    tfSum: Result := Sum;
    tfAvg: if N > 0 then Result := Sum / N;
    tfMax: Result := Mx;
    tfMin: Result := Mn;
    tfCount: Result := N;
  end;
end;

function FirstLetterUpper(const S: String): String;
var
  L: String;
begin
  Result := Utf8LowerCase(S);
  if Result = '' then Exit;
  L := Utf8UpperCase(Utf8Copy(S, 1, 1));
  Utf8Delete(Result, 1, 1);
  Result := L + Result;
end;

function MyFrac(E: Extended; Digits: Byte): LongInt;
var
  F: Extended;
  i: Integer;
begin
  F := Frac(E);
  for i := 1 to Digits do
    F := F * 10;
  Result := Round(F);
end;

function GetWeekName(D: TDateTime; Brief: Boolean): String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    if Brief then
      SplitStr(rsWeekNamesBrief, ' ', SL)
    else
      SplitStr(rsWeekNames, ' ', SL);
    Result := SL[DayOfTheWeek(D) - 1];
  finally
    SL.Free;
  end;
end;

function GetMonthName(D: TDateTime; Brief: Boolean): String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    if Brief then
      SplitStr(rsMonthNamesBrief, ' ', SL)
    else
      SplitStr(rsMonthNames, ' ', SL);
    Result := SL[MonthOf(D) - 1];
  finally
    SL.Free;
  end;
end;

function IIF(Condition: Boolean; Value1, Value2: Variant): Variant;
begin
  if Condition then
    Result := Value1
  else
    Result := Value2;
end;

function MyPower(V1, V2: Extended): Extended;
begin
  Result := Power(V1, V2);
end;

function GetRecNo(aForm: TdxForm; const FormName: String): Variant;
var
  Fm: TdxForm;
  DS: TDataSet;
  F: TField;
  RD: TReportData;
begin
  if aForm = nil then raise Exception.Create(rsFormNotAvail);
  if (aForm.PId = 0) and (UTF8CompareText(aForm.FormCaption, FormName) = 0) then
    DS := aForm.Grid.DataSource.DataSet
  else
    GetFormData(aForm, FormName, '', Fm, DS, F);
  if Fm = nil then
    GetQueryData(aForm, FormName, '', RD, DS, F);
  if DS = nil then raise Exception.CreateFmt(rsFormQryNotFound, [FormName]);
  Result := DS.RecNo;
end;

function MyRoundToStr(V: Extended; Digits: Integer): String;
var
  S: String;
  d: Double;
begin
  d := MathRound(V, Digits);
  S := '0';
  if Digits > 0 then
    S := '0.' + DupeString('0', Digits);
  Result := FormatFloat(S, d);
end;

function MergeDateTime(D1, T1: TDateTime): TDateTime;
begin
  Result := EncodeDateTime(YearOf(D1), MonthOf(D1), DayOf(D1), HourOf(T1), MinuteOf(T1),
    SecondOf(T1), 0);
end;

procedure SplitDateTime(DT: TDateTime; var D1, T1: TDateTime);
var
  Y, M, D, H, Mn, S, Ms: Word;
begin
  DecodeDateTime(DT, Y, M, D, H, Mn, S, Ms);
  D1 := EncodeDate(Y, M, D);
  T1 := EncodeTime(H, Mn, S, Ms);
end;

// Оказывается HoursBetween некорректно вычисляет результат.
// Разница между 01.03.2017 12:00:00 и 01.03.2017 13:00:00 будет 0,
// хотя должно быть 1. Все дело в миллисекундах. Чтобы избежать
// ошибки, надо большее значение увеличить на 1 миллисекунду.
procedure ShiftDT(D1, T1, D2, T2: TDateTime; var DT1, DT2: TDateTime);
begin
  DT1 := MergeDateTime(D1, T1);
  DT2 := MergeDateTime(D2, T2);
  if MilliSecondOf(T1) = MilliSecondOf(T2) then
  begin
	  if DT1 > DT2 then DT1 := IncMilliSecond(DT1)
  	else if DT2 > DT1 then DT2 := IncMilliSecond(DT2);
  end;
end;

function MyHoursBetween(D1, T1, D2, T2: TDateTime): Int64;
var
  DT1, DT2: TDateTime;
begin
  ShiftDT(D1, T1, D2, T2, DT1, DT2);
  Result := HoursBetween(DT1, DT2);
end;

function MyMinutesBetween(D1, T1, D2, T2: TDateTime): Int64;
var
  DT1, DT2: TDateTime;
begin
  ShiftDT(D1, T1, D2, T2, DT1, DT2);
  Result := MinutesBetween(DT1, DT2);
end;

function MySecondsBetween(D1, T1, D2, T2: TDateTime): Int64;
var
  DT1, DT2: TDateTime;
begin
  ShiftDT(D1, T1, D2, T2, DT1, DT2);
  Result := SecondsBetween(DT1, DT2);
end;

function AddHour(Fm: TdxForm; DS: TDataSet; D, T: TDateTime; N: Integer;
  const DateField, Func: String): TDateTime;
var
  DE, DT: TDateTime;
  F: TField;
  C: TComponent;
begin
  if Fm = nil then raise Exception.Create(rsFormNotAvail);
  F := nil;
  if DateField <> '' then
  begin
    C := FindComponentByFieldName(Fm, DateField);
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [DateField]);
    if not (C is TdxDateEdit) then raise Exception.CreateFmt(rsFieldNotDate, [DateField]);
    F := DS.FieldByName(FieldStr(C));
  end;
  if Func = 'h' then
    DT := IncHour(MergeDateTime(D, T), N)
  else if Func = 'm' then
    DT := IncMinute(MergeDateTime(D, T), N)
  else if Func = 's' then
    DT := IncSecond(MergeDateTime(D, T), N);
  SplitDateTime(DT, DE, Result);
  if (F <> nil) and (DS.State in [dsInsert, dsEdit]) then
    F.AsDateTime:=DE;
end;

function MyIndexOf(i: Integer; const S: String): String;
var
  SL: TStringList;
begin
  Result := '';
  SL := TStringList.Create;
  SplitStr(S, ';', SL);
  if (i >= 0) and (i < SL.Count) then
    Result := SL[i];
  SL.Free;
end;

function FmtDateRu(DT: TDateTime): String;
const
  MStr: array[1..12] of String = ('января', 'февраля', 'марта', 'апреля', 'мая',
    'июня', 'июля', 'августа', 'сентября', 'октября', 'ноября', 'декабря');
var
  Y, M, D: word;
begin
  DecodeDate(DT, Y, M, D);
  Result := IntToStr(D) + ' ' + MStr[M] + ' ' + IntToStr(Y);
end;

function FmtDate(DT: TDateTime): String;
var
  L: String;
begin
  L := AppConfig.Language;
  if L = 'ru' then Result := FmtDateRu(DT)
  else Result := 'Unsupported';
end;

function MergeRows(aForm: TdxForm; const FormName, FieldName, Delim: String
  ): Variant;
var
  Fm: TdxForm;
  DS: TDataSet;
  F: TField;
  C: TComponent;
  B: TBookmark;
  S: String;
  AftScroll, BefScroll: TDataSetNotifyEvent;
  RD: TReportData;
begin
  if FieldName = '' then raise Exception.Create(rsFieldNameEmpty);
  Result := Null;
  GetFormData(aForm, FormName, FieldName, Fm, DS, F);
  if Fm <> nil then
  begin
    C := FindComponentByFieldName(Fm, FieldName);
    if C is TdxLookupComboBox then
      F := DS.FieldByName(FieldStr(C) + 'l');
  end
  else
    GetQueryData(aForm, FormName, FieldName, RD, DS, F);
  if DS = nil then raise Exception.CreateFmt(rsFormQryNotFound, [FormName]);

  DS.DisableControls;
  B := DS.GetBookmark;
  AftScroll := DS.AfterScroll;
  BefScroll := DS.BeforeScroll;
  try
    DS.First;
    S := '';
    while not DS.Eof do
    begin
      S := S + F.AsString + Delim;
      DS.Next;
    end;
    Result := Copy(S, 1, Length(S) - Length(Delim));
  finally
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    DS.AfterScroll:=AftScroll;
    DS.BeforeScroll := BefScroll;
    DS.EnableControls;
  end;
end;

function GetRecId(aForm, aParForm: TdxForm; const FormName: String): Variant;
var
  Fm: TdxForm;
  F: TField;
  DS: TDataSet;
  RD: TReportData;
begin
  if aForm = nil then raise Exception.Create(rsFormNotAvail);
  Fm := aForm;
  if Fm.PId > 0 then Fm := aParForm;
  if UTF8CompareText(FormName, Fm.FormCaption) = 0 then
    DS := Fm.Grid.DataSource.DataSet
  else
    GetQueryData(aForm, FormName, '', RD, DS, F);
  if DS = nil then raise Exception.CreateFmt(rsFormQryNotFound, [FormName]);
  Result := DS.FieldByName('id').AsInteger;
end;

function GetObjId(const FormName, FieldName, FieldValue: String): Variant;
var
  Fm: TdxForm;
  C, CC: TComponent;
  SQL: String;
begin
  Fm := FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);
  C := FindComponentByFieldName(Fm, FieldName);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);

  SQL := 'select id from ' + TableStr(Fm.Id) + ' where ' + FieldStr(C) +
    '=''' + FieldValue + '''';
  if Fm.ParentField > 0 then
  begin
    CC := FindById(Fm, Fm.ParentField);
    if GetSourceFId(CC) = GetId(C) then
    begin
      SQL := SqlSelectGroups(Fm.Id, GetId(C), False);
      SQL := SQL + ' where ' + FieldStr(C) + '=''' + FieldValue + '''';
    end;
  end;

  with DBase.OpenDataSet(SQL) do
  begin
    Result := Fields[0].AsInteger;
    Free;
  end;
end;

function GetUser: String;
var
  U: TdxUser;
begin
  Result := '';
  U := UserMan.CurrentUser;
  if U <> nil then Result := U.Name;
end;

function GetRole: String;
var
  U: TdxUser;
  R: TdxRole;
begin
  Result := '';
  U := UserMan.CurrentUser;
  if U = nil then Exit;
  R := UserMan.Roles.FindRole(U.RoleId);
  if R = nil then Exit;
  Result := R.Name;
end;

function ExtractFieldName(const S: String): String;
var
  p: SizeInt;
begin
  p := Pos('|', S);
  if p > 0 then
    Result := Copy(S, 1, p - 1)
  else
    Result := S;
end;

function DBGet(aForm, aParForm: TdxForm; aDS: TDataSet; const FormName,
  FieldName, Filter: String; Func: TRpTotalFunc): Variant;
var
  Fm, PFm: TdxForm;
  C: TComponent;
  RD: TReportData;
  pSr: PRpSource;
  pF: PRpField;
  Sql: String;
begin
  //if aForm = nil then raise Exception.Create(rsFormNotAvail);
  Result := Null;
  Fm := FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);
  if FieldName <> '' then
  begin
    if LookupComponent(Fm, FieldName) = nil then
      raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    C := FindComponentByFieldName(Fm, ExtractFieldName(FieldName));
    //if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
  end;
  PFm := nil;
  if Fm.PId > 0 then
    PFm := FormMan.FindForm(Fm.PId);
  RD := TReportData.Create;
  RD.Sources.AddSource(pSr);
  if PFm <> nil then
  begin
    pSr^.Id:=IntToStr(PFm.Id);
    pSr^.TId := IntToStr(Fm.Id);
  end
  else
    pSr^.Id := IntToStr(Fm.Id);
  pSr^.Filter:=Filter;
  if FieldName <> '' then
  begin
    pSr^.Fields.AddField(pF);
    SetupRpField(C, FieldName, pF);
    {pF^.Tp:=GetTypeByComponent(C);
    pF^.TId:=IntToStr(Fm.Id);
    pF^.FId := IntToStr(GetId(C));}
    pF^.Func:=Func;
    pF^.Visible := True;
  end
  else if Func = tfCount then
  begin
    pSr^.Fields.AddField(pF);
    pF^.Zero:=True;
    pF^.Tp := flNumber;
    pF^.Func:=Func;
    pF^.Visible := True;
  end;

  try
    Sql := SqlReportSelect(RD, aForm, aParForm, aDS);
    with DBase.OpenDataSet(Sql) do
    try
      if (FieldName <> '') or (Func = tfCount) then
        Result := FieldByName('f0').Value
      else if Fields.Count > 0 then
      begin
        if PFm <> nil then
          Result := Fields[1].Value
        else
          Result := Fields[0].Value;
      end;
      if (Func in [tfSum, tfAvg, tfCount, tfDistCount]) and (Result = Null) then Result := 0;
    finally
      Free;
    end;
  finally
    RD.Free;
  end;
end;

function DBGetById(const FormName, FieldName: String; aId: Integer): Variant;
var
  Fm: TdxForm;
  C: TComponent;
  SQL: String;
begin
  Result := Null;
  Fm := FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);
  C := FindComponentByFieldName(Fm, FieldName);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);

  SQL := 'select ' + FieldStr(C) + ' from ' + TableStr(Fm.Id) + ' where id=' +
    IntToStr(aId);
  with DBase.OpenDataSet(SQL) do
  begin
    Result := Fields[0].Value;
    Free;
  end;
end;

function GetMaxV(Vals: array of Variant): Variant;
var
  i: Integer;
begin
  Result := Null;
  if Length(Vals) = 0 then Exit;
  Result := Vals[0];
  for i := 1 to Length(Vals) - 1 do
    if Vals[i] > Result then Result := Vals[i];
end;

function GetMinV(Vals: array of Variant): Variant;
var
  i: Integer;
begin
  Result := Null;
  if Length(Vals) = 0 then Exit;
  Result := Vals[0];
  for i := 1 to Length(Vals) - 1 do
    if Vals[i] < Result then Result := Vals[i];
end;

function SetVar(const aName: String; aValue: Variant): Variant;
var
  pV: PVrData;
begin
  pV := VarList.FindVar(aName);
  if pV = nil then
    pV := VarList.AddVar(aName, aValue)
  else
    pV^.Value := aValue;
  Result := aValue;
end;

function GetVar(const aName: String): Variant;
var
  pV: PVrData;
begin
  Result := Null;
  pV := VarList.FindVar(aName);
  if pV <> nil then Result := pV^.Value
  else raise Exception.Create(Format(rsVarNotFound, [aName]));
end;

procedure Period(Date1, Date2: TDateTime; var D, M, Y: Integer);
var
  D1, D2, M1, M2, Mdx, Y1, Y2, Ydx: Integer;
begin
  D1 := DayOf(Date1);
  D2 := DayOf(Date2);
  M1 := MonthOf(Date1);
  M2 := MonthOf(Date2);
  Y1 := YearOf(Date1);
  Y2 := YearOf(Date2);

  Mdx := 0;
  Ydx := 0;

  D := D2 - D1 + 1;
  if D < 0 then
  begin
    D := 30 + D;
    Mdx := 1;
  end;

  M := M2 - M1 - Mdx;
  if M < 0 then
  begin
    M := 12 + M;
    Ydx := 1;
  end;

  Y := Y2 - Y1 - Ydx;
end;

function FormatPeriodRu(Y, M, D, Detail: Integer): String;
const
  YStr: array [0..9] of String = ('лет', 'год', 'года', 'года', 'года', 'лет',
    'лет', 'лет', 'лет', 'лет');
  MStr: array [0..9] of String = ('месяцев', 'месяц', 'месяца', 'месяца', 'месяца',
    'месяцев', 'месяцев', 'месяцев', 'месяцев', 'месяцев');
  DStr: array [0..9] of String = ('дней', 'день', 'дня', 'дня', 'дня', 'дней',
    'дней', 'дней', 'дней', 'дней');

  function Idx(n: Integer): Integer;
  begin
    if n in [5..20] then Result := 0
    else Result := n mod 10;
  end;

begin
  Result := '';
  if Y > 0 then
    Result := IntToStr(Y) + ' ' + YStr[Idx(Y)] + ' ';
  if (M > 0) and (Detail > 1) then
    Result := Result + IntToStr(M) + ' ' + MStr[Idx(M)] + ' ';
  if (D > 0) and (Detail > 2) then
    Result := Result + IntToStr(D) + ' ' + DStr[Idx(D)];
  Result := Trim(Result);
end;

function CalcPeriodRu(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String;
var
  Y, M, D: Integer;
begin
  Period(D1, D2, D, M, Y);
  if Age then
  begin
    Dec(D);
    if D < 0 then
    begin
      Dec(M); D := 29;
      if M < 0 then
      begin
        Dec(Y); M := 11;
      end;
    end;
  end;
  Result := FormatPeriodRu(Y, M, D, Detail);
end;

function CalcPeriod(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String;
var
  L: String;
begin
  L := AppConfig.Language;
  if L = 'ru' then Result := CalcPeriodRu(D1, D2, Detail, Age)
  else Result := 'Unsupported';
end;

procedure PeriodAdd(D1, M1, Y1, D2, M2, Y2: Integer; var D, M, Y: Integer);
var
  Mdx, Ydx: Integer;
begin
  Mdx := 0; Ydx := 0;
  D := D1 + D2;
  if D > 30 then
  begin
    D := D - 30;
    Mdx := 1;
  end;
  M := M1 + M2 + Mdx;
  if M > 12 then
  begin
    M := M - 12;
    Ydx := 1;
  end;
  Y := Y1 + Y2 + Ydx;
end;

function SumPeriodRu(aForm: TdxForm; const FormName, Date1, Date2: String;
  Detail: Integer): String;
var
  Fm: TdxForm;
  DS: TDataSet;
  DF1, DF2: TField;
  AfterScroll, BeforeScroll: TDataSetNotifyEvent;
  B: TBookmark;
  D1, D2: TDateTime;
  Y, M, D, Yr, Mr, Dr: Integer;
  RD: TReportData;
begin
  if (Date1 = '') or (Date2 = '') then raise Exception.Create(rsFieldNameEmpty);
  Result := '';
  GetFormData(aForm, FormName, Date1, Fm, DS, DF1);
  if Fm <> nil then
    GetFormData(aForm, FormName, Date2, Fm, DS, DF2)
  else
  begin
    GetQueryData(aForm, FormName, Date1, RD, DS, DF1);
    if DS <> nil then
      GetQueryData(aForm, FormName, Date2, RD, DS, DF2);
  end;
  if DS = nil then raise Exception.CreateFmt(rsFormQryNotFound, [FormName]);

  Yr := 0; Mr := 0; Dr := 0;
  AfterScroll := DS.AfterScroll;
  BeforeScroll := DS.BeforeScroll;
  DS.AfterScroll := nil;
  DS.BeforeScroll := nil;
  DS.DisableControls;
  B := DS.GetBookmark;
  try
    DS.First;
    while not DS.Eof do
    begin
      if (DF1.IsNull = False) and (DF2.IsNull = False) then
      begin
        D1 := DF1.AsDateTime;
        D2 := DF2.AsDateTime;
        Period(D1, D2, D, M, Y);
        PeriodAdd(Dr, Mr, Yr, D, M, Y, Dr, Mr, Yr);
      end;
      DS.Next;
    end;
    Result := FormatPeriodRu(Yr, Mr, Dr, Detail);
  finally
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    DS.AfterScroll:=AfterScroll;
    DS.BeforeScroll := BeforeScroll;
    DS.EnableControls;
  end;
end;

function SumPeriod(aForm: TdxForm; const FormName, Date1, Date2: String;
  Detail: Integer): String;
var
  L: String;
begin
  L := AppConfig.Language;
  if L = 'ru' then Result := SumPeriodRu(aForm, FormName, Date1, Date2, Detail)
  else Result := 'Unsupported';
end;

function Block(Vals: array of Variant): Variant;
begin
  Result := Vals[High(Vals)];
end;

function VarExists(const aName: String): Integer;
begin
  if VarList.FindVar(aName) <> nil then Result := 1
  else Result := 0;
end;

function PathLength(const Path: String): Integer;
var
  i: Integer;
begin
  if Path = '' then Exit(0);
  Result := 1;
  for i := 0 to Length(Path) - 1 do
    if Path[i] = '\' then Inc(Result);
end;

function ExtractPath(const Path: String; aStart, aLen: Integer): String;
var
  SL: TStringList;
  i, e: Integer;
begin
  Result := '';
  SL := TStringList.Create;
  SplitStr(Path, '\', SL);
  e := aStart + aLen - 1;
  if (aStart >= 1) and (aStart <= SL.Count) and (e >= 1) and (e <= SL.Count) then
    for i := aStart to e do
    begin
      Result := Result + SL[i-1];
      if i < e then Result := Result + '\'
    end;
  SL.Free;
end;

function IsNewRec(DS: TDataSet): Integer;
begin
  Result := 0;
  if DS.State = dsInsert then Result := 1;
end;

function GetFieldValue(aForm: TdxForm; const FormName, FieldName: String
  ): Variant;
var
  DS: TDataSet;
  F: TField;
  RD: TReportData;
begin
  GetQueryData(aForm, FormName, FieldName, RD, DS, F);
  if (DS <> nil) and DS.Active then Result := F.Value;
end;

function GetOldValue(aForm: TdxForm; DS: TDataSet; const FieldName: String
  ): Variant;
var
  C: TComponent;
begin
  if aForm = nil then raise Exception.Create(rsFormNotAvail);
  Result := Null;
  C := FindComponentByFieldName(aForm, FieldName);
  if C <> nil then
    Result := DS.FieldByName(FieldStr(C)).OldValue
  else
    raise ECalcError.Create(ecFieldNotFound, 0, FieldName);
end;

function BeginYear(D: TDateTime): TDateTime;
begin
  Result := EncodeDate(YearOf(D), 1, 1);
end;

function BeginMonth(D: TDateTime): TDateTime;
begin
  Result := EncodeDate(YearOf(D), MonthOf(D), 1);
end;

function BeginWeek(D: TDateTime): TDateTime;
begin
  Result := IncDay(D, -DayOfTheWeek(D)+1);
end;

function EndYear(D: TDateTime): TDateTime;
begin
  Result := EncodeDate(YearOf(D), 12, 31);
end;

function EndMonth(D: TDateTime): TDateTime;
begin
  Result := IncDay(IncMonth(EncodeDate(YearOf(D), MonthOf(D), 1)), -1);
end;

function EndWeek(D: TDateTime): TDateTime;
begin
  Result := IncDay(D, 7-DayOfTheWeek(D));
end;

function BeginQuarter(D: TDateTime): TDateTime;
var
  y, q: Word;
begin
  y := YearOf(D);
  q := QuarterOf(D);
  case q of
    1: Result := EncodeDate(y, 1, 1);
    2: Result := EncodeDate(y, 4, 1);
    3: Result := EncodeDate(y, 7, 1);
    4: Result := EncodeDate(y, 10, 1);
  end;
end;

function EndQuarter(D: TDateTime): TDateTime;
var
  y, q: Word;
begin
  y := YearOf(D);
  q := QuarterOf(D);
  case q of
    1: Result := EncodeDate(y, 3, 1);
    2: Result := EncodeDate(y, 6, 1);
    3: Result := EncodeDate(y, 9, 1);
    4: Result := EncodeDate(y, 12, 1);
  end;
  Result := IncDay(IncMonth(Result), -1);
end;

function QuarterOf(D: TDateTime): Word;
begin
  case MonthOf(D) of
    1..3: Result := 1;
    4..6: Result := 2;
    7..9: Result := 3;
    10..12: Result := 4;
  end;
end;

function DBUnique(aForm: TdxForm; aDS: TDataSet; const Fields: String): Integer;
var
  SL: TStrings;
  S, SQL: String;
  C: TComponent;
  i: Integer;
  F: TField;
  AnyFieldModified: Boolean;
begin
  Result := 1;
  if aForm = nil then raise Exception.Create(rsFormNotAvail);
  if Trim(Fields) = '' then Exit;
  SL := TStringList.Create;

  try

  SplitStr(Fields, ';', SL);
  SQL := 'select id from ' + TableStr(aForm.Id) + ' where ';
  AnyFieldModified := False;
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    C := FindComponentByFieldName(aForm, S);
    if C = nil then raise Exception.Create(Format(rsFieldNotFound, [S]));
    F := aDS.FieldByName(FieldStr(C));
    if F.IsNull then
      SQL := SQL + F.FieldName + ' is null and '
    else if F is TStringField then
      SQL := SQL + 'LOWER(' + F.FieldName + ')=''' + Utf8LowerCase(F.AsString) + ''' and '
    else if F is TDateTimeField then
      SQL := SQL + F.FieldName + '=''' + F.AsString + ''' and '
    else
      SQL := SQL + F.FieldName + '=' + F.AsString + ' and ';
    if (aDS.Modified) and (Utf8CompareText(F.AsString, VarToStr(F.OldValue)) <> 0) then
      AnyFieldModified:=True;
  end;
  SQL := Copy(SQL, 1, Length(SQL) - 5);
  with DBase.OpenDataSet(SQL) do
  begin
    if aDS.State = dsInsert then
    begin
      if RecordCount > 0 then Result := 0;
    end
    else
    begin
      if (RecordCount > 0) and AnyFieldModified then Result := 0
      else if RecordCount > 1 then Result := 0;
    end;
    Free;
  end;

  finally
    SL.Free;
  end;
end;

function IsUniqueRecords(aForm: TdxForm; const FormName, Fields: String
  ): Integer;
var
  Fm: TdxForm;
  DS: TDataSet;
  Recs, FL: TList;
  SL, Vals: TStringList;
  S: String;
  B: TBookmark;
  AftScr: TDataSetNotifyEvent;
  F: TField;
  i: Integer;
  C: TComponent;
  OldState: TDataSetState;

  // True, если нет совпадений
  function CheckUnique: Boolean;
  var
    j, n: Integer;
    VL: TStringList;
    Fld: TField;
  begin
    Result := True;
    for j := 0 to Recs.Count - 1 do
    begin
      VL := TStringList(Recs[j]);
      for n := 0 to VL.Count - 1 do
      begin
        Fld := TField(FL[n]);
        if Utf8CompareText(Fld.AsString, VL[n]) = 0 then Exit(False);
      end;
    end;
  end;

begin
  Result := 1;
  if aForm = nil then raise Exception.Create(rsFormNotAvail);
  if Trim(Fields) = '' then Exit;
  GetFormData(aForm, FormName, '', Fm, DS, F);
  if DS = nil then
    raise Exception.Create(Format(rsFormQryNotFound, [FormName]));
  Recs := TList.Create;
  FL := TList.Create;
  SL := TStringList.Create;

  try

  SplitStr(Fields, ';', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    C := FindComponentByFieldName(Fm, S);
    if C = nil then raise Exception.Create(Format(rsFieldNotFound, [S]));
    FL.Add(DS.FieldByName(FieldStr(C)));
  end;

  OldState := DS.State;
  B := DS.GetBookmark;
  DS.DisableControls;
  AftScr := DS.AfterScroll;
  DS.AfterScroll := nil;
  try
    DS.First;
    while not DS.Eof do
    begin
      if CheckUnique then
      begin
        Vals := TStringList.Create;
        for i := 0 to FL.Count - 1 do
        begin
          Vals.Add(TField(FL[i]).AsString);
        end;
        Recs.Add(Vals);
      end
      else
      begin
        Result := 0;
        Break;
      end;
      DS.Next;
    end;
  finally
    if Result = 1 then
      DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    DS.AfterScroll:=AftScr;
    DS.EnableControls;
    DS.AfterScroll(DS);
    if OldState in [dsInsert, dsEdit] then
      DS.Edit;
  end;

  finally
    ClearList(Recs);
    Recs.Free;
    FL.Free;
    SL.Free;
  end;
end;

function MsgBox(const Caption, Msg: String): Variant;
begin
  Result := '';
  MessageDlg(Caption, Msg, mtInformation, [mbOk], 0);
end;

function YesNoBox(const Caption, Msg: String): Integer;
begin
  Result := MessageDlg(Caption, Msg, mtConfirmation, [mbYes, mbNo], 0);
  case Result of
    mrYes: Result := 1;
    mrNo: Result := 2;
  end;
end;

function YesNoCancelBox(const Caption, Msg: String): Integer;
begin
  Result := MessageDlg(Caption, Msg, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  case Result of
    mrYes: Result := 1;
    mrNo: Result := 2;
    mrCancel: Result := 3;
  end;
end;

function SetField(aForm: TdxForm; aDS: TDataSet; const aFieldName: String;
  Value: Variant): Variant;
var
  C: TComponent;
begin
  if aForm = nil then raise Exception.Create(rsFormNotAvail);
  C := FindComponentByFieldName(aForm, aFieldName);
  if C = nil then raise Exception.Create(Format(rsFieldNotFound, [aFieldName]));
  if aDS.Active and (aDS.State in [dsInsert, dsEdit]) then
  begin
    aDS.FieldByName(FieldStr(C)).Value:=Value;
    if C is TdxLookupComboBox then
    begin
      if Value <> Null then
        aDS.FieldByName(FieldStr(C) + 'l').Value := GetObjFieldValue(C, Value, True)
      else
        aDS.FieldByName(FieldStr(C) + 'l').SetData(nil);
    end;
  end;
  Result := Value;
end;

function SetLabel(aForm: TdxForm; const aFieldName: String; Value: Variant
  ): Variant;
var
  L: TdxLabel;
begin
  L := FindLabelByFieldName(aForm, aFieldName, True);
  if L = nil then raise Exception.Create(Format(rsFieldNotFound, [aFieldName]));
  L.Caption:=VarToStr(Value);
  L.Value:=Value;
  Result := Value;
end;

function IsEditRec(aDS: TDataSet): Integer;
begin
  if aDS = nil then raise Exception.Create(rsFormNotAvail);
  if aDS.State = dsEdit then Result := 1
  else Result := 0;
end;

function IsModifiedRec(aDS: TDataSet): Integer;
{var
  i: Integer;
  F: TField;}
begin
  Result := 0;
  if aDS = nil then raise Exception.Create(rsFormNotAvail);
  if aDS.Modified then Result := 1;
  {for i := 0 to aDS.Fields.Count - 1 do
  begin
    F := aDS.Fields[i];
    if F.Value <> F.OldValue then Exit(1);
  end;  }
end;

{ TVarList }

function TVarList.GetVars(Index: Integer): PVrData;
begin
  Result := PVrData(Items[Index]);
end;

function TVarList.AddVar(const aName: String; aValue: Variant): PVrData;
begin
  New(Result);
  Result^.Name := aName;
  Result^.Value := aValue;
  Add(Result);
end;

function TVarList.FindVar(const aName: String): PVrData;
var
  i: Integer;
  pV: PVrData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pV := Vars[i];
    if UTF8CompareText(pV^.Name, aName) = 0 then Exit(pV);
  end;
end;

procedure TVarList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(Vars[i]);
  inherited Clear;
end;

end.

