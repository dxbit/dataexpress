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
unit ScriptFuncs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, dxctrls, strconsts, Controls, FileUtil,
  Forms, db, Graphics, dxreports, IniFiles;

procedure MessageBox(const Title, Msg: String);
function CreateForm(const FormName: String): TdxForm;
procedure DestroyForm(var Fm: TdxForm);
procedure GetForms(SL: TStrings);
function MsgDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TModalResult;
function EvalExpr(const Expr: String; Fm: TdxForm): Variant;
function DCount(DataSet: TObject): Integer;
function DSum(DataSet: TObject; const FieldName: String): Extended;
function DAvg(DataSet: TObject; const FieldName: String): Extended;
function DMax(DataSet: TObject; const FieldName: String): Variant;
function DMin(DataSet: TObject; const FieldName: String): Variant;
function DMerge(DataSet: TObject; const FieldName, Delimiter: String): String;
function ToWordsRu(Money: Currency): String;
function RurToWords(Money: Currency): String;
function Nz(V1, V2: Variant): Variant;
function GetCurrentDatabase: String;
function MyTryStrToDate(const S: String; var DT: TDateTime): Boolean;
function MyTryStrToTime(const S: String; var DT: TDateTime): Boolean;
function MyTryStrToDateTime(const S: String; var DT: TDateTime): Boolean;
function MyTryStrToFloat(const S: String; var E: Extended): Boolean;
function MyStrToDateTime(const S: String): TDateTime;
function MyStrToTime(const S: String): TDateTime;
function MyTimeToStr(DT: TDateTime): String;
function GetOutDir: String;
//
function MyExtractFileName(const FileName: string): string;
function MyExtractFileExt(const FileName: string): string;
function MyExtractFilePath(const FileName: string): string;
function MyExtractFileDrive(const FileName: string): string;
function MyExtractFileDir(Const FileName : string): string;
function MyChangeFileExt(const FileName, Extension: string): string;
function MyIncludeTrailingPathDelimiter(Const Path : String) : String;
function MyExcludeLeadingPathDelimiter(Const Path: string): string;
function MyGetTempDir: String;
function MyGetTempFileName: String;
function MyCopyFile(const SrcFilename, DestFilename: string; Flags: TCopyFileFlags): boolean;
procedure MyFindAllFiles(AList: TStrings; const SearchPath: String;
  SearchMask: String; SearchSubDirs: Boolean; DirAttr: Word);
procedure MyFindAllDirectories(AList: TStrings; const SearchPath: String;
  SearchSubDirs: Boolean);
//
function MyStringToColor(const S: String): TColor;
function _MyFrac(AValue: Extended): Extended;
Function MyFormatFloat(Const Format : String; Value : Extended) : String;

function EncodeMD5(const S: String): String;
function EncodeSHA1(const S: String): String;
function EncodeBase64(const S: String): String;
function DecodeBase64(const S: String; Strict: Boolean): String;

procedure ShowPrintErrors(const S: String);

function MyUtf8LowerCase(const S: String): String;
function MyUtf8UpperCase(const S: String): String;
function MyUtf8CompareStr(const S1, S2: String): PtrInt;

function MySameValue(const A, B: Extended; Epsilon: Extended): Boolean;

function YearsBetweenEx(DT1, DT2: TDateTime): Integer;
function MonthsBetweenEx(DT1, DT2: TDateTime): Integer;
function WeeksBetweenEx(DT1, DT2: TDateTime): Integer;
function DaysBetweenEx(DT1, DT2: TDateTime): Integer;
function HoursBetweenEx(DT1, DT2: TDateTime): Int64;
function MinutesBetweenEx(DT1, DT2: TDateTime): Int64;
function SecondsBetweenEx(DT1, DT2: TDateTime): Int64;

implementation

uses
  Variants, LazUtf8, formmanager, sqlgen, datasetprocessor, BGRABitmap,
  expressions, reportmanager, towordsfuncs, exprfuncs, dbengine, apputils,
  dxsqlquery, appsettings, Math, md5, sha1, base64, errorsform, DateUtils;

procedure MessageBox(const Title, Msg: String);
begin
  MessageDlg(Title, Msg, mtInformation, [mbOk], 0);
end;

function CreateForm(const FormName: String): TdxForm;
var
  Fm: TdxForm;
  DSP: TDataSetProcessor;
begin
  Fm := FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);
  if Fm.PId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormName]);
  DSP := TDataSetProcessor.Create;
  DSP.BindForm(Fm.Id, False, vtGridOnly);
  Result := DSP.Form;
end;

procedure DestroyForm(var Fm: TdxForm);
begin
  TDataSetProcessor(Fm.DSP).Free;
  Fm := nil;
end;

procedure GetForms(SL: TStrings);
begin
  FormMan.AllFormsToList(SL);
end;

function MsgDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TModalResult;
begin
  Result := MessageDlg(aCaption, aMsg, DlgType, Buttons, 0);
end;

function EvalExpr(const Expr: String; Fm: TdxForm): Variant;
var
  EB: TExpressionBuilder;
  E: TExpression;
begin
  Result := Null;
  if Trim(Expr) = '' then Exit;

  E := nil;
  EB := TExpressionBuilder.Create;
  try
    EB.SkipLabels:=True;
    if Fm <> nil then
    begin
      EB.Form := Fm;
      EB.ParentForm := Fm;
      if Fm.ParentForm <> nil then
        EB.ParentForm := Fm.ParentForm;
      EB.DataSet := Fm.DataSet;
    end;
    E := EB.Build(Expr);
    Result := E.Calc;
  finally
    EB.Free;
    FreeAndNil(E);
  end;
end;

function GetDataSet(DataSet: TObject): TDataSet;
begin
  Result := nil;
  if DataSet is TdxForm then
    Result := TdxForm(DataSet).DataSet
  else if DataSet is TdxQueryGrid then
    Result := TdxQueryGrid(DataSet).DataSource.DataSet
  else if DataSet is TdxSQLQuery then
    Result := TdxSQLQuery(DataSet).DataSet;
end;

function GetField(DataSet: TObject; const FieldName: String; DS: TDataSet): TField;
var
  C: TComponent;
  RD: TReportData;
  Col: TRpGridColumn;
  //pF: PRpField;
begin
  Result := nil;
  if DataSet is TdxForm then
  begin
    C := FindComponentByFieldName(TdxForm(DataSet), FieldName);
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    Result := DS.FieldByName(FieldStr(C));
  end
  else if C is TdxQueryGrid then
  begin
    RD := ReportMan.FindReport(TdxQueryGrid(DataSet).Id);
    Col := RD.Grid.FindColumnByTitle(FieldName);
    if Col = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    Result := DS.FieldByName(Col.FieldName);
  end
  {else if DataSet is TdxQuery then
  begin
    pF := TdxQuery(DataSet).RD.FindFieldByName(FieldName);
    if pF = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    Result := DS.FieldByName('f' + IntToStr(pF^.Id));
  end;}
end;

function DCount(DataSet: TObject): Integer;
var
  DS: TDataSet;
  BefScr, AftScr: TDataSetNotifyEvent;
  B: TBookmark;
  OldState: TDataSetState;
begin
  Result := 0;
  DS := GetDataSet(DataSet);
  DisableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  try
    DS.Last;
    Result := DS.RecordCount;
  finally
    EnableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  end;
end;

function DSum(DataSet: TObject; const FieldName: String): Extended;
var
  DS: TDataSet;
  BefScr, AftScr: TDataSetNotifyEvent;
  B: TBookmark;
  OldState: TDataSetState;
  F: TField;
begin
  Result := 0;
  DS := GetDataSet(DataSet);
  F := GetField(DataSet, fieldName, DS);
  DisableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  try
    DS.First;
    while not DS.Eof do
    begin
      Result := Result + F.AsFloat;
      DS.Next;
    end;
  finally
    EnableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  end;
end;

function DAvg(DataSet: TObject; const FieldName: String): Extended;
var
  DS: TDataSet;
  BefScr, AftScr: TDataSetNotifyEvent;
  B: TBookmark;
  OldState: TDataSetState;
  F: TField;
  n: Integer;
begin
  Result := 0;
  n := 0;
  DS := GetDataSet(DataSet);
  F := GetField(DataSet, fieldName, DS);
  DisableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  try
    DS.First;
    while not DS.Eof do
    begin
      Result := Result + F.AsFloat;
      Inc(n);
      DS.Next;
    end;
    if n > 0 then Result := Result / n;
  finally
    EnableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  end;
end;

function DMax(DataSet: TObject; const FieldName: String): Variant;
var
  DS: TDataSet;
  BefScr, AftScr: TDataSetNotifyEvent;
  B: TBookmark;
  OldState: TDataSetState;
  F: TField;
begin
  Result := Null;
  DS := GetDataSet(DataSet);
  F := GetField(DataSet, fieldName, DS);
  DisableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  try
    DS.First;
    if not DS.Eof then
    begin
      Result := F.Value;
      DS.Next;
    end;
    while not DS.Eof do
    begin
      if F.Value > Result then Result := F.Value;
      DS.Next;
    end;
  finally
    EnableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  end;
end;

function DMin(DataSet: TObject; const FieldName: String): Variant;
var
  DS: TDataSet;
  BefScr, AftScr: TDataSetNotifyEvent;
  B: TBookmark;
  OldState: TDataSetState;
  F: TField;
begin
  Result := Null;
  DS := GetDataSet(DataSet);
  F := GetField(DataSet, fieldName, DS);
  DisableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  try
    DS.First;
    if not DS.Eof then
    begin
      Result := F.Value;
      DS.Next;
    end;
    while not DS.Eof do
    begin
      if F.Value < Result then Result := F.Value;
      DS.Next;
    end;
  finally
    EnableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  end;
end;

function DMerge(DataSet: TObject; const FieldName, Delimiter: String): String;
var
  DS: TDataSet;
  BefScr, AftScr: TDataSetNotifyEvent;
  B: TBookmark;
  OldState: TDataSetState;
  F: TField;
begin
  Result := '';
  DS := GetDataSet(DataSet);
  F := GetField(DataSet, fieldName, DS);
  DisableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  try
    DS.First;
    while not DS.Eof do
    begin
      Result := Result + F.AsString;
      DS.Next;
      if not DS.Eof then Result := Result + Delimiter;
    end;
  finally
    EnableDataSetScroll(DS, BefScr, AftScr, B, OldState);
  end;
end;

function ToWordsRu(Money: Currency): String;
begin
  Result := ToWords(Money, False);
end;

function RurToWords(Money: Currency): String;
begin
  Result := ToWords(Money, True);
end;

function Nz(V1, V2: Variant): Variant;
begin
  Result := V1;
  if V1 = Null then Result := V2;
end;

{function DBFuncXXX(const FormName, FieldName, Filter: String; Func: TRpToTalFunc
  ): Variant;
var
  PFm, Fm: TdxForm;
  TableName, FlName: String;
  Q: TdxQuery;
begin
  Result := Null;
  Fm := FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);
  TableName := '';
  FlName := FieldName;
  if FlName = '' then FlName := 'Field';
  PFm := Fm;
  if Fm.PId > 0 then
  begin
    TableName := Fm.FormCaption;
    PFm := FormMan.FindForm(Fm.Id);
  end;
  Q := TdxQuery.Create;
  with Q.AddSource(PFm.FormCaption, TableName, Filter, skNone) do
    AddField(FieldName, FieldName, Func);
  Q.Open;
  if (FieldName <> '') or (Func <> tfNone) then
    Result := Q.Fields[FlName]
  // Если не указана ни функция, ни поле, возвращаем ID записи
  else if Q.DataSet.RecordCount > 0 then
  begin
    if Fm.PId > 0 then Result := Q.DataSet.Fields[1].Value
    else Result := Q.DataSet.Fields[0].Value;
  end;
  Q.Free;
end;

function DBCount(const FormName, Filter: String): Integer;
begin
  Result := Nz(DBFuncXXX(FormName, '', Filter, tfCount), 0);
end;

function DBCountD(const FormName, FieldName, Filter: String): Integer;
begin
  Result := Nz(DBFuncXXX(FormName, FieldName, Filter, tfDistCount), 0);
end;

function DBSum(const FormName, FieldName, Filter: String): Extended;
begin
  Result := Nz(DBFuncXXX(FormName, FieldName, Filter, tfSum), 0);
end;

function DBAvg(const FormName, FieldName, Filter: String): Extended;
begin
  Result := Nz(DBFuncXXX(FormName, FieldName, Filter, tfAvg), 0);
end;

function DBMax(const FormName, FieldName, Filter: String): Variant;
begin
  Result := DBFuncXXX(FormName, FieldName, Filter, tfMax);
end;

function DBMin(const FormName, FieldName, Filter: String): Variant;
begin
  Result := DBFuncXXX(FormName, FieldName, Filter, tfMin);
end;

function DBMerge(const FormName, FieldName, Filter: String): String;
begin
  Result := Nz(DBFuncXXX(FormName, FieldName, Filter, tfMerge), '');
end;

function DBMergeAll(const FormName, FieldName, Filter: String): String;
begin
  Result := Nz(DBFuncXXX(FormName, FieldName, Filter, tfMergeAll), '');
end;

function _DBGet(const FormName, FieldName, Filter: String): Variant;
begin
  Result := DBFuncXXX(FormName, FieldName, Filter, tfNone);
end;

function DBGetId(const FormName, Filter: String): Variant;
begin
  Result := DBFuncXXX(FormName, '', Filter, tfNone);
end;

function _DBGetById(const FormName, FieldName: String; Id: Integer): Variant;
begin
  Result := DBGetById(FormName, FieldName, Id);
end;

function _DBUnique(Fm: TdxForm; const Fields: String): Boolean;
begin
  Result := DBUnique(Fm, Fm.DataSet, Fields) = 1;
end;  }

function GetCurrentDatabase: String;
begin
  Result := DBase.Database;
end;

function MyTryStrToDate(const S: String; var DT: TDateTime): Boolean;
begin
  Result := TryStrToDate(S, DT);
end;

function MyTryStrToTime(const S: String; var DT: TDateTime): Boolean;
begin
  Result := TryStrToTime(S, DT);
end;

function MyTryStrToDateTime(const S: String; var DT: TDateTime): Boolean;
begin
  Result := TryStrToDateTime(S, DT);
end;

function MyTryStrToFloat(const S: String; var E: Extended): Boolean;
begin
  Result := TryStrToFloat(S, E);
end;

function MyStrToDateTime(const S: String): TDateTime;
begin
  Result := StrToDateTime(S);
end;

function MyStrToTime(const S: String): TDateTime;
begin
  Result := StrToTime(S);
end;

function MyTimeToStr(DT: TDateTime): String;
begin
  Result := TimeToStr(DT);
end;

function GetOutDir: String;
begin
  Result := AppConfig.OutputDir;
  if Result <> '' then
	  Result := GetAbsolutePath(Result);
end;

function MyExtractFileName(const FileName: string): string;
begin
  Result := ExtractFileName(FileName);
end;

function MyExtractFileExt(const FileName: string): string;
begin
  Result := ExtractFileExt(FileName);
end;

function MyExtractFilePath(const FileName: string): string;
begin
  Result := ExtractFilePath(FileName);
end;

function MyExtractFileDrive(const FileName: string): string;
begin
  Result := ExtractFileDrive(FileName);
end;

function MyExtractFileDir(const FileName: string): string;
begin
  Result := ExtractFileDir(FileName);
end;

function MyChangeFileExt(const FileName, Extension: string): string;
begin
  Result := ChangeFileExt(FileName, Extension);
end;

function MyIncludeTrailingPathDelimiter(const Path: String): String;
begin
  Result := IncludeTrailingPathDelimiter(Path);
end;

function MyExcludeLeadingPathDelimiter(const Path: string): string;
begin
  Result := ExcludeLeadingPathDelimiter(Path);
end;

function MyGetTempDir: String;
begin
	Result := GetTempDir;
end;

function MyGetTempFileName: String;
begin
  Result := SysUtils.GetTempFilename;
end;

function MyCopyFile(const SrcFilename, DestFilename: string;
  Flags: TCopyFileFlags): boolean;
begin
  Result := CopyFile(SrcFileName, DestFileName, Flags);
end;

procedure MyFindAllFiles(AList: TStrings; const SearchPath: String;
  SearchMask: String; SearchSubDirs: Boolean; DirAttr: Word);
begin
  FindAllFiles(AList, SearchPath, SearchMask, SearchSubDirs, DirAttr);
end;

procedure MyFindAllDirectories(AList: TStrings; const SearchPath: String;
  SearchSubDirs: Boolean);
begin
  FindAllDirectories(AList, SearchPath, SearchSubDirs);
end;

function MyStringToColor(const S: String): TColor;
begin
  Result := StringToColor(S);
end;

function _MyFrac(AValue: Extended): Extended;
begin
  Result := Frac(AValue);
end;

function MyFormatFloat(const Format: String; Value: Extended): String;
begin
  Result := FormatFloat(Format, Value);
end;

function EncodeMD5(const S: String): String;
begin
  Result := MD5Print(MD5String(S));
end;

function EncodeSHA1(const S: String): String;
begin
  Result := SHA1Print(SHA1String(S));
end;

function EncodeBase64(const S: String): String;
begin
  Result := EncodeStringBase64(S);
end;

function DecodeBase64(const S: String; Strict: Boolean): String;
begin
  Result := DecodeStringBase64(S, Strict);
end;

procedure ShowPrintErrors(const S: String);
begin
  with TErrorsFm.Create(nil) do
  begin
    ShowForm(S);
    Free;
  end;
end;

function MyUtf8LowerCase(const S: String): String;
begin
	Result := Utf8LowerCase(S);
end;

function MyUtf8UpperCase(const S: String): String;
begin
	Result := Utf8UpperCase(S);
end;

function MyUtf8CompareStr(const S1, S2: String): PtrInt;
begin
  Result := Utf8CompareStr(S1, S2);
end;

function MySameValue(const A, B: Extended; Epsilon: Extended): Boolean;
begin
  Result := SameValue(A, B, Epsilon);
end;

// Оказывается HoursBetween некорректно вычисляет результат.
// Разница между 01.03.2017 12:00:00 и 01.03.2017 13:00:00 будет 0,
// хотя должно быть 1. Все дело в плавающей точке. Чтобы избежать
// ошибки, надо большее значение увеличить на 1 миллисекунду.
procedure ShiftDT(var DT1, DT2: TDateTime);
begin
  if MilliSecondOf(DT1) = MilliSecondOf(DT2) then
  begin
  	if DT1 > DT2 then DT1 := IncMilliSecond(DT1)
    else if DT2 > DT1 then DT2 := IncMilliSecond(DT2);
  end;
end;

function YearsBetweenEx(DT1, DT2: TDateTime): Integer;
begin
  ShiftDT(DT1, DT2);
  Result := YearsBetween(DT1, DT2);
end;

function MonthsBetweenEx(DT1, DT2: TDateTime): Integer;
begin
  ShiftDT(DT1, DT2);
  Result := MonthsBetween(DT1, DT2);
end;

function WeeksBetweenEx(DT1, DT2: TDateTime): Integer;
begin
  ShiftDT(DT1, DT2);
  Result := WeeksBetween(DT1, DT2);
end;

function DaysBetweenEx(DT1, DT2: TDateTime): Integer;
begin
  ShiftDT(DT1, DT2);
  Result := DaysBetween(DT1, DT2);
end;

function HoursBetweenEx(DT1, DT2: TDateTime): Int64;
begin
  ShiftDT(DT1, DT2);
  Result := HoursBetween(DT1, DT2);
end;

function MinutesBetweenEx(DT1, DT2: TDateTime): Int64;
begin
  ShiftDT(DT1, DT2);
  Result := MinutesBetween(DT1, DT2);
end;

function SecondsBetweenEx(DT1, DT2: TDateTime): Int64;
begin
  ShiftDT(DT1, DT2);
  Result := SecondsBetween(DT1, DT2);
end;

end.

