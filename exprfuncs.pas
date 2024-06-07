{-------------------------------------------------------------------------------

    Copyright 2015-2024 Pavel Duborkin ( mydataexpress@mail.ru )

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

-------------------------------------------------------------------------------}

unit ExprFuncs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dxctrls, Db, strconsts, dxreports, Controls, mytypes;

//type
//  EGetQueryDataError = class(Exception);

procedure GetFormData(aForm: TdxForm; const FormName, FieldName: String; out Fm: TdxForm;
  out DS: TDataSet; out F: TField);
function GetQueryData(aForm: TdxForm; const QueryName, FieldName: String;
  out RD: TReportData; out DS: TDataSet; out F: TField): Boolean;
function CalcAggFunc(aForm, aParForm: TdxForm; aDataSet: TDataSet; const FormName,
  FieldName, Condition: String; Func: TRpTotalFunc): Variant;
function FirstLetterUpper(const S: String): String;
function MyFrac(E: Extended; Digits: Byte): Int64;
function GetWeekName(D: TDateTime; Brief: Boolean): String;
function GetMonthName(D: TDateTime; Brief: Boolean): String;
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
function MergeRows(aForm: TdxForm; const FormName, FieldName, Delim: String): String;
function MergeRowsEx(aForm: TdxForm; const FormName, Expr, Delim: String): String;
function GetRecId(aForm: TdxForm; const FormName: String): Variant;
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
//function CalcPeriodRu(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String;
//function SumPeriod(aForm: TdxForm; const FormName, Date1, Date2: String; Detail: Integer): String;
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
function IsEditRec(aForm: TdxForm): Integer;
function IsModifiedRec(aForm: TdxForm): Integer;
function Concat(Vals: array of Variant): String;
//function Fmt(Args: array of Variant): String;
function FNumber(V: Extended; Digits: Integer): String;
function TextFormat(const S: String; Fm: TdxForm): String;
function GetTimeStamp(D, T: Variant): Variant;
function CaseOf(const AValue, AItems: String): String;
function GetFieldText(AForm: TdxForm; const FieldName: String): String;

var
  VarList: TVarList;

implementation

uses
  formmanager, LazUtf8, sqlgen, reportmanager, dbengine,
  apputils, dateutils, dialogs, Math, expressions, Variants, StrUtils,
  dxusers, dximages, dxfiles;

procedure GetFormData(aForm: TdxForm; const FormName, FieldName: String; out
  Fm: TdxForm; out DS: TDataSet; out F: TField);
var
  C: TComponent;
begin
  if (aForm = nil) or (aForm.Id = DummyForm) then raise Exception.Create(rsFormNotAvail);
  Fm := nil; DS := nil; F := nil;

  if MyUtf8CompareText(aForm.FormCaption, FormName) = 0 then Fm := aForm
  else Fm := aForm.FindForm(FormName);
  if Fm <> nil then
  begin
    DS := Fm.DataSet;
    Fm.RequeryIfNeed;
    if FieldName <> '' then
    begin
      C := FindComponentByFieldName(Fm, FieldName);
      if C = nil then raise Exception.Create(Format(rsFieldNotFound, [FieldName]));

      F := DS.FieldByName(FieldStr(C));
    end;
  end;
end;

function GetQueryData(aForm: TdxForm; const QueryName, FieldName: String; out
  RD: TReportData; out DS: TDataSet; out F: TField): Boolean;
var
  QG: TdxQueryGrid;
  Col: TRpGridColumn;
begin
  Result := True;
  if (aForm = nil) or (aForm.Id = DummyForm) then raise Exception.Create(rsFormNotAvail);
  RD := nil; DS := nil; F := nil;
  QG := TdxQueryGrid(aForm.FindQuery(QueryName));
  if QG = nil then Exit;
  RD := ReportMan.FindReport(QG.Id);
  begin
    begin
      DS := QG.DataSource.DataSet;

      QG.RequeryIfNeed;

      if not DS.Active then Exit(False); //raise EGetQueryDataError.Create('Dataset inactive');
      if FieldName <> '' then
      begin
        Col := RD.Grid.FindColumnByTitle(FieldName);
        if Col = nil then raise Exception.Create(Format(rsFieldNotFound, [FieldName]));
        F := DS.FieldByName(Col.FieldNameDS);
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
  FirstValue, IsEOF: Boolean;
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
        ParentForm := aParForm;
        DataSet := DS;
        SkipLabels:=True;
        E := Build(Condition);
      finally
        Free;
      end;
  end
  else
  begin
    if GetQueryData(aForm, FormName, FieldName, Rp, DS, F) then
    begin
      if (Rp <> nil) and (Condition <> '') then
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
    end
    else if Func in [tfSum, tfAvg, tfCount] then
      Exit(0)
    else
      Exit;
  end;
  if DS = nil then
    raise Exception.Create(Format(rsFormQryNotFound, [FormName]));
  // Только форм касается
  if (DS.State in [dsInsert, dsEdit]) and (Fm <> nil) then raise Exception.Create(rsCalcAggFuncError);

  FirstValue := True;
  AfterScroll := DS.AfterScroll;
  BeforeScroll := DS.BeforeScroll;
  DS.AfterScroll := nil;
  DS.BeforeScroll := nil;
  IsEOF := DS.EOF;
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
          if FirstValue then
          begin
            if not F.IsNull then
            begin
	            Mx := F.Value;
  	          Mn := Mx;
    	        FirstValue := False;
            end;
          end
          else if not F.IsNull then
          begin
            if F.Value > Mx then Mx := F.Value;
            if F.Value < Mn then Mn := F.Value;
          end;
        end
        else if Func in [tfSum, tfAvg] then
        begin
          if not F.IsNull then
          begin
	          Sum := Sum + F.AsFloat;
            Inc(N);
          end;
        end
        // Функция TAKE
        else if Func = tfNone then
        begin
          Result := F.Value;
          Break;
        end;
      end;
      if Func = tfCount then Inc(N);
      DS.Next;
    end;
  finally
    FreeAndNil(E);
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);

    // Если до работы функции курсор был в конце набора, то устанавливаем его
    // в конец (GotoBookmark устанавливает на последнюю запись, но флаг
    // EOF не устанавливает, из-за чего в некоторых случаях приводит к
    // зависанию при печати шаблонов (баг от 17.01.2018 Telos)
    if IsEOF and (not DS.EOF) then DS.Last;

    DS.EnableControls;
    DS.AfterScroll:=AfterScroll;
    DS.BeforeScroll:=BeforeScroll;
    if OldState in [dsInsert, dsEdit] then
      DS.Edit;
  end;

  case Func of
    tfSum: Result := Sum;
    tfAvg:
    	if N > 0 then
    		Result := Sum / N
      else
      	Result := 0;
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

function MyFrac(E: Extended; Digits: Byte): Int64;
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
  GetFormData(aForm, FormName, '', Fm, DS, F);
  if Fm = nil then
  begin
    if not GetQueryData(aForm, FormName, '', RD, DS, F) then Exit(0);
  end;
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
  {Result := EncodeDateTime(YearOf(D1), MonthOf(D1), DayOf(D1), HourOf(T1), MinuteOf(T1),
    SecondOf(T1), 0);}
  Result := DateOf(D1) + TimeOf(T1);
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
// ошибки, надо к большему значению добавить миллисекунду.
procedure ShiftDT(D1, T1, D2, T2: TDateTime; out DT1, DT2: TDateTime);
begin
  DT1 := MergeDateTime(D1, T1);
  DT2 := MergeDateTime(D2, T2);
  if DT1 > DT2 then
  begin
    DT1 := RecodeMilliSecond(DT1, 1);
    DT2 := RecodeMilliSecond(DT2, 0);
  end
  else
  begin
    DT1 := RecodeMilliSecond(DT1, 0);
    DT2 := RecodeMilliSecond(DT2, 1);
  end;
  {if MilliSecondOf(T1) = MilliSecondOf(T2) then
  begin
	  if DT1 > DT2 then DT1 := IncMilliSecond(DT1)
  	else if DT2 > DT1 then DT2 := IncMilliSecond(DT2);
  end; }
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

{function FmtDateRu(DT: TDateTime): String;
const
  MStr: array[1..12] of String = ('января', 'февраля', 'марта', 'апреля', 'мая',
    'июня', 'июля', 'августа', 'сентября', 'октября', 'ноября', 'декабря');
var
  Y, M, D: word;
begin
  DecodeDate(DT, Y, M, D);
  Result := IntToStr(D) + ' ' + MStr[M] + ' ' + IntToStr(Y);
end;     }

function FmtDate(DT: TDateTime): String;
var
  Y, M, D: word;
  SL: TStringList;
begin
  SL := TStringList.Create;
  SplitStr(rsFmtDateMonth, ' ', SL);
  DecodeDate(DT, Y, M, D);
  Result := IntToStr(D) + ' ' + SL[M-1] + ' ' + IntToStr(Y);
  SL.Free;
end;

function MergeRows(aForm: TdxForm; const FormName, FieldName, Delim: String
  ): String;
var
  Fm: TdxForm;
  DS: TDataSet;
  F: TField;
  C: TComponent;
  B: TBookmark;
  S: String;
  AftScroll, BefScroll: TDataSetNotifyEvent;
  Rp: TReportData;
  OldState: TDataSetState;
  IsEOF: Boolean;
begin
  if FieldName = '' then raise Exception.Create(rsFieldNameEmpty);
  Result := '';
  GetFormData(aForm, FormName, FieldName, Fm, DS, F);
  if Fm <> nil then
  begin
    C := FindComponentByFieldName(Fm, FieldName);
    if C is TdxLookupComboBox then
      F := DS.FieldByName(FieldStr(C) + 'l');
  end
  else
  begin
    if not GetQueryData(aForm, FormName, FieldName, Rp, DS, F) then Exit;
  end;
  if DS = nil then raise Exception.CreateFmt(rsFormQryNotFound, [FormName]);
  if DS.State in [dsInsert, dsEdit] then raise Exception.Create(rsCalcAggFuncError);

  DS.DisableControls;
  B := DS.GetBookmark;
  IsEOF := DS.EOF;
  OldState := DS.State;
  AftScroll := DS.AfterScroll;
  BefScroll := DS.BeforeScroll;
  DS.AfterScroll := nil;
  DS.BeforeScroll := nil;
  try
    DS.First;
    S := '';
    while not DS.Eof do
    begin
      S := S + F.AsString + Delim;
      DS.Next;
    end;
    SetLength(S, Length(S) - Length(Delim));
    Result := S;
  finally
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    if IsEOF and not DS.EOF then DS.Last;
    DS.AfterScroll:=AftScroll;
    DS.BeforeScroll := BefScroll;
    DS.EnableControls;
    if OldState in [dsInsert, dsEdit] then DS.Edit;
  end;
end;

function MergeRowsEx(aForm: TdxForm; const FormName, Expr, Delim: String
  ): String;
var
  Fm: TdxForm;
  DS: TDataSet;
  F: TField;
  B: TBookmark;
  S: String;
  AftScroll, BefScroll: TDataSetNotifyEvent;
  Rp: TReportData;
  OldState: TDataSetState;
  IsEOF: Boolean;
  E: TExpression;
  V: Variant;
begin
  Result := '';
  GetFormData(aForm, FormName, '', Fm, DS, F);
  if Fm = nil then
  begin
    if not GetQueryData(aForm, FormName, '', Rp, DS, F) then Exit;
  end;
  if DS = nil then raise Exception.CreateFmt(rsFormQryNotFound, [FormName]);
  if DS.State in [dsInsert, dsEdit] then raise Exception.Create(rsCalcAggFuncError);

  if Fm <> nil then
  begin
    with TExpressionBuilder.Create do
    try
      Form := Fm;
      ParentForm := Fm.ParentForm;
      DataSet := DS;
      SkipLabels := True;
      E := Build(Expr);
    finally
      Free;
    end;
  end
  else
  begin
    with TExpressionBuilder.Create do
    try
      Form := aForm;
      ParentForm := aForm.ParentForm;
      DataSet := aForm.DataSet;
      RD := Rp;
      RDSet := DS;
      SkipLabels := True;
      E := Build(Expr);
    finally
      Free;
    end;
  end;
  if E = nil then Exit;

  DS.DisableControls;
  B := DS.GetBookmark;
  IsEOF := DS.EOF;
  OldState := DS.State;
  AftScroll := DS.AfterScroll;
  BefScroll := DS.BeforeScroll;
  DS.AfterScroll := nil;
  DS.BeforeScroll := nil;
  try
    DS.First;
    S := '';
    while not DS.Eof do
    begin
      V := E.Calc;
      if V <> Null then S := S + VarToStr(V) + Delim;
      DS.Next;
    end;
    SetLength(S, Length(S) - Length(Delim));
    Result := S;
  finally
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    if IsEOF and not DS.EOF then DS.Last;
    DS.AfterScroll:=AftScroll;
    DS.BeforeScroll := BefScroll;
    DS.EnableControls;
    if OldState in [dsInsert, dsEdit] then DS.Edit;
  end;
end;

function GetRecId(aForm: TdxForm; const FormName: String): Variant;
var
  Fm: TdxForm;
  F: TField;
  DS: TDataSet;
  RD: TReportData;
begin
  GetFormData(aForm, FormName, '', Fm, DS, F);
  if Fm = nil then
  begin
    if not GetQueryData(aForm, FormName, '', RD, DS, F) then Exit(0);
  end;
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
  if (Fm.ParentField > 0) and (GetFormParentFieldFieldId(Fm) = GetId(C)) then
  begin
    CC := FindById(Fm, Fm.ParentField);
    if GetSourceFId(CC) = GetId(C) then
    begin
      SQL := SqlSelectGroups(Fm.Id, False);
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
  Fm, PFm, AFm: TdxForm;
  C: TComponent;
  RD: TReportData;
  pSr: PRpSource;
  pF: PRpField;
  Sql, FlNm: String;
begin
  Result := Null;
  Fm := FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);

  if Fm.PId > 0 then
    PFm := FormMan.FindForm(Fm.PId)
  else
	  PFm := nil;

  if FieldName <> '' then
  begin
    FlNm := FieldName;
    if FlNm[1] = '!' then
    begin
      Delete(FlNm, 1, 1);
      if PFm <> nil then AFm := PFm
      else AFm := Fm;
    end
    else
    	AFm := Fm;
    if LookupComponent(AFm, FlNm) = nil then
      raise Exception.CreateFmt(rsFieldNotFound, [FlNm]);
    C := FindComponentByFieldName(AFm, ExtractFieldName(FlNm));
  end;
  RD := TReportData.Create;
  RD.Sources.AddSource(pSr);
  if PFm <> nil then
  begin
    pSr^.Id:=PFm.Id;
    pSr^.TId := Fm.Id;
  end
  else
    pSr^.Id := Fm.Id;
  pSr^.Filter:=Filter;
  if FieldName <> '' then
  begin
    pSr^.Fields.AddField(pF);
    SetupRpField(C, FieldName, pF);
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
    if (Func in [tfSum, tfAvg]) and not (GetLowField(pF)^.Tp in [flNumber{, flCounter, flRecId}]) then
      raise Exception.CreateFmt(rsDBSumFieldNotNumeric, [FieldName]);
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
  if pV <> nil then
    Result := pV^.Value;
  //else raise Exception.Create(Format(rsVarNotFound, [aName]));
end;

procedure Period(Date1, Date2: TDateTime; var D, M, Y: Word);
begin
  PeriodBetween(Date1, Date2, Y, M, D);
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

{function CalcPeriodRu(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String;
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
end;  }

function CalcPeriod(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String;
var
  Y, M, D: Word;
begin
  if not Age then
  begin
    if D1 > D2 then D1 := IncDay(D1, 1)
    else D2 := IncDay(D2, 1);
  end;
  Period(D1, D2, D, M, Y);
  Result := FormatPeriodRu(Y, M, D, Detail);
end;

{procedure PeriodAdd(D1, M1, Y1, D2, M2, Y2: Integer; var D, M, Y: Integer);
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

function SumPeriod(aForm: TdxForm; const FormName, Date1, Date2: String;
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
end; }

{function SumPeriod(aForm: TdxForm; const FormName, Date1, Date2: String;
  Detail: Integer): String;
var
  L: String;
begin
  L := AppConfig.Language;
  if L = 'ru' then Result := SumPeriodRu(aForm, FormName, Date1, Date2, Detail)
  else Result := 'Unsupported';
end;  }

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
  Fm: TdxForm;
begin
  if FieldName = '' then raise Exception.Create(rsFieldNameEmpty);
  //GetFormData(aForm, FormName, FieldName, Fm, DS, F);

  if Utf8CompareText(aForm.FormCaption, FormName) = 0 then
    Exit( aForm.Fields[FieldName] );

  Fm := aForm.FindForm(FormName);

  if Fm <> nil then
  begin
    Exit( Fm.Fields[FieldName] );
  end
  else
  begin
    if not GetQueryData(aForm, FormName, FieldName, RD, DS, F) then Exit(Null);
  end;
  if DS <> nil then Result := F.Value
  else raise Exception.Create(Format(rsFormQryNotFound, [FormName]))
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
    raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
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
  S, SQL, Value, AbsValue: String;
  C: TComponent;
  i: Integer;
  F: TField;
  AnyFieldModified: Boolean;
begin
  Result := 1;
  if (aForm = nil) or (aForm.Id = DummyForm) then raise Exception.Create(rsFormNotAvail);
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
    if C is TdxObjectField then
      raise Exception.CreateFmt(rsObjFieldNotUseDBUnique, [GetFieldName(C)]);
    F := aDS.FieldByName(FieldStr(C));
    if F.IsNull then
      SQL := SQL + F.FieldName + ' is null and '
    else if F is TStringField then
      SQL := SQL + 'LOWER(' + F.FieldName + ')=''' +
        EscapeSQuotes(Utf8LowerCase(F.AsString)) + ''' and '
    else if F is TDateTimeField then
      SQL := SQL + F.FieldName + '=''' + F.AsString + ''' and '
    else if F is TFloatField then
    begin
      Value := StringReplace(F.AsString, DefaultFormatSettings.DecimalSeparator, '.', []);
      AbsValue := StringReplace(Value, '-', '', []);
      SQL := SQL + F.FieldName + '>=' + Value + '-' + AbsValue + '*2e-12 and ' +
      	F.FieldName + '<=' + Value + '+' + AbsValue + '*2e-12 and ';
    end
    else if C is TdxRecordId then
      SQL := SQL + 'id=' + F.AsString + ' and '
    else
      SQL := SQL + F.FieldName + '=' + F.AsString + ' and ';
    if (aDS.Modified) and (MyUtf8CompareText(F.AsString, VarToStr(F.OldValue)) <> 0) then
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
  AftScr, BefScr: TDataSetNotifyEvent;
  F: TField;
  i: Integer;
  C: TComponent;
  OldState: TDataSetState;
  RD: TReportData;
  rC: TRpGridColumn;

  // True, если нет совпадений
  function CheckUnique: Boolean;
  var
    j, n, a: Integer;
    VL: TStringList;
    Fld: TField;
  begin
    Result := True;
    for j := 0 to Recs.Count - 1 do
    begin
      VL := TStringList(Recs[j]);
      a := 0;
      for n := 0 to VL.Count - 1 do
      begin
        Fld := TField(FL[n]);
        if MyUtf8CompareText(Fld.AsString, VL[n]) = 0 then Inc(a);
      end;
      if a = VL.Count then Exit(False);
    end;
  end;

begin
  Result := 1;
  if Trim(Fields) = '' then Exit;
  GetFormData(aForm, FormName, '', Fm, DS, F);
  if DS = nil then
  begin
    if not GetQueryData(aForm, FormName, '', RD, DS, F) then Exit;
  end;
  if DS = nil then
    raise Exception.Create(Format(rsFormQryNotFound, [FormName]));
  if DS.State in [dsInsert, dsEdit] then
    raise Exception.Create(rsCalcAggFuncError);
  Recs := TList.Create;
  FL := TList.Create;
  SL := TStringList.Create;

  try

  SplitStr(Fields, ';', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    if Fm <> nil then
    begin
      C := Fm.FindComponentByFldName(S);
      F := DS.FieldByName(FieldStr(C));
    end
    else if RD <> nil then
    begin
      rC := RD.Grid.FindColumnByTitle(S);
      if rC = nil then raise Exception.Create(Format(rsFieldNotFound, [S]));
      F := DS.FieldByName(rC.FieldNameDS);
    end;
    FL.Add(F);
  end;

  OldState := DS.State;
  B := DS.GetBookmark;
  DS.DisableControls;
  BefScr := DS.BeforeScroll;
  AftScr := DS.AfterScroll;
  DS.BeforeScroll := nil;
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
    DS.BeforeScroll:=BefScr;
    DS.AfterScroll:=AftScr;
    DS.EnableControls;
    if Result = 1 then
      DS.GotoBookmark(B);
    DS.FreeBookmark(B);
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
  Result := 0;
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
  S: String;
begin
  if aForm = nil then raise Exception.Create(rsFormNotAvail);
  C := FindComponentByFieldName(aForm, aFieldName);
  if C = nil then raise Exception.Create(Format(rsFieldNotFound, [aFieldName]));
  if aDS.Active and (aDS.State in [dsInsert, dsEdit]) then
  begin
    if C is TdxLookupComboBox then
    begin
      if Value <> Null then
        aDS.FieldByName(FieldStr(C) + 'l').Value := GetObjFieldValue(C, Value, True)
      else
        aDS.FieldByName(FieldStr(C) + 'l').SetData(nil);
      aDS.FieldByName(FieldStr(C)).Value:=Value;
    end
    else if C is TdxDBImage then
      with TdxDBImage(C) do
      begin
        if Value = Null then Clear
        else
        begin
          S := VarToStr(Value);
          if FileExists(S) then LoadFromFile(S)
          else raise Exception.CreateFmt(rsFileNotExists, [S]);
        end;
      end
    else if C is TdxFile then
      with TdxFile(C) do
      begin
        if Value = Null then Clear
        else
        begin
          S := VarToStr(Value);
          if FileExists(S) then LoadFromFile(S)
          else raise Exception.CreateFmt(rsFileNotExists, [S]);
        end;
      end
    else
      aDS.FieldByName(FieldStr(C)).Value:=Value;
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

function IsEditRec(aForm: TdxForm): Integer;
begin
  if (aForm = nil) or (aForm.Id = DummyForm) then raise Exception.Create(rsFormNotAvail);
  //if aDS = nil then raise Exception.Create(rsFormNotAvail);
  if aForm.State = dsEdit then Result := 1
  else Result := 0;
end;

function IsModifiedRec(aForm: TdxForm): Integer;
begin
  Result := 0;
  if (aForm = nil) or (aForm.Id = DummyForm) then raise Exception.Create(rsFormNotAvail);
  if aForm.Modified then Result := 1;
end;

function Concat(Vals: array of Variant): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(Vals) - 1 do
  	Result := Result + VarToStr(Vals[i]);
  Result := Trim(Result);
end;

function FNumber(V: Extended; Digits: Integer): String;
var
  S: String;
  d: Double;
begin
  d := MathRound(V, Digits);
  S := ',0';
  if Digits > 0 then
    S := ',0.' + DupeString('0', Digits);
  Result := FormatFloat(S, d);
end;

function TextFormat(const S: String; Fm: TdxForm): String;
var
  i, Len: Integer;
  Ch: Char;
  FlNm, Expr: String;
  State: (stText, stField, stExpr);

  function NextCh: Char;
  begin
    if i < Len then Result := S[i + 1]
    else Result := #0;
  end;

begin
  Result := '';
  State := stText;
  i := 1; Len := Length(S);
  while i <= Len do
  begin
    Ch := S[i];
    case State of
      stText:
        case Ch of
          '[':
            begin
              if NextCh = '[' then
              begin
                Result := Result + NextCh;
                Inc(i);
              end
              else
              begin
                State := stField;
                FlNm := '';
              end;
            end;
          '{':
            begin
              if NextCh = '{' then
              begin
                Result := Result + NextCh;
                Inc(i);
              end
              else
              begin
                State := stExpr;
                Expr := '';
              end;
            end;
          (*'\':
            if NextCh in ['{', '['] then
            begin
              Result := Result + NextCh;
              Inc(i);
            end
            else Result := Result + Ch;   *)
          #10, #13:
            begin
              if NextCh in [#10, #13] then
                Inc(i);
              Result := Result + ' ';
            end;
          else
            Result := Result + Ch;
        end;
      stField:
        case Ch of
          ']':
            begin
              if (Copy(FlNm, 1, 1) = '!') and (Fm.ParentForm <> nil) then
              begin
                Delete(FlNm, 1, 1);
                Result := Result + Fm.ParentForm.AsS[FlNm];
              end
              else
                Result := Result + Fm.AsS[FlNm];
              State := stText;
            end;
          else
            FlNm := FlNm + Ch;
        end;
      stExpr:
        case Ch of
          '}':
            begin
              if NextCh = '}' then
              begin
                Expr := Expr + NextCh;
                Inc(i);
              end
              else
              begin
                Result := Result + VarToStr(EvalExpr(Expr, Fm));
                State := stText;
              end;
            end;
          (*'\':
            if NextCh = '}' then
            begin
              Expr := Expr + NextCh;
              Inc(i);
            end
            else Expr := Expr + Ch;    *)
          else
            Expr := Expr + Ch;
        end;
    end;
    Inc(i);
  end;
end;

function GetTimeStamp(D, T: Variant): Variant;
var
  DT1, DT2: TDateTime;
begin
  if (D = Null) and (T = Null) then Result := Null
  else
  begin
    if D = Null then D := 0
    else if T = Null then T := 0;
    ShiftDT(D, T, 0, 0, DT1, DT2);
    Result := SecondsBetween(DT1, DT2);
    if DT1 < 0 then Result := -Result;
  end;
end;

function CaseOf(const AValue, AItems: String): String;
var
  SL: TStringListUtf8;
begin
  SL := TStringListUtf8.Create;
  SL.Delimiter:=';';
  SL.StrictDelimiter:=True;
  SL.DelimitedText := AItems;
  Result := SL.Values[AValue];
  SL.Free;
end;

function GetFieldText(AForm: TdxForm; const FieldName: String): String;
var
  C: TComponent;
begin
  C := AForm.FindComponentByFldName(FieldName);
  if C is TControl then
    Result := TControl(C).Caption;
end;

{function Fmt(Args: array of Variant): String;
var
  i, Len, ai: Integer;
  S, Template: String;
  Ch: Char;
begin
  if Length(Args) = 0 then Exit('');
  Template := VarToStr(Args[0]);

  S := '';
  Len := Length(Template);
  ai := 1; i := 1;
  while i <= Len do
  begin
    Ch := Template[i];
    if (Ch = '%') and (LowerCase(Copy(Template, i + 1, 1)) = 's') then
    begin
      if ai < Length(Args) then
      begin
        S := S + VarToStr(Args[ai]);
        Inc(ai);
      end;
      Inc(i);
    end
    else S := S + Ch;
    Inc(i);
  end;
  Result := S;
end; }

end.

