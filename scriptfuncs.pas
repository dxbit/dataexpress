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

unit ScriptFuncs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, dxctrls, strconsts, Controls, FileUtil,
  Forms, db, Graphics, dxreports, TypInfo, laz2_dom, laz2_xmlread, laz2_xmlwrite,
  fpjson, LclType, Clipbrd;

procedure MessageBox(const Title, Msg: String);
function CreateForm(const FormName: String): TdxForm;
procedure DestroyForm(var Fm: TdxForm);
procedure TObjectDestroyForm(Fm: TdxForm);
procedure GetForms(SL: TStrings);
function MsgDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TModalResult;
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
function MyIntToHex(Value: Int64; Digits: Integer): String;
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

function MyUtf8Length(const S: String): PtrInt;
procedure MyUTF8Insert(const source: String; var s: String; StartCharIndex: PtrInt);
function MyUtf8LowerCase(const S: String): String;
function MyUtf8UpperCase(const S: String): String;
function MyUtf8CompareStr(const S1, S2: String): PtrInt;
function MyUTF8StringReplace(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;
procedure MyUTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
function MyStringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;
function MyUTF8ToUTF16(const S: AnsiString): UnicodeString;
function MyUTF16ToUTF8(const S: UnicodeString): AnsiString;

function MySameValue(const A, B: Extended; Epsilon: Extended): Boolean;

function YearsBetweenEx(DT1, DT2: TDateTime): Integer;
function MonthsBetweenEx(DT1, DT2: TDateTime): Integer;
function WeeksBetweenEx(DT1, DT2: TDateTime): Integer;
function DaysBetweenEx(DT1, DT2: TDateTime): Integer;
function HoursBetweenEx(DT1, DT2: TDateTime): Int64;
function MinutesBetweenEx(DT1, DT2: TDateTime): Int64;
function SecondsBetweenEx(DT1, DT2: TDateTime): Int64;
function VarIsNothing(V: Variant): Boolean;

function ShowExprEditor(const Expression, FormName: String): String;

function GetFormatSettings: TFormatSettings;
procedure SetFormatSettings(Settings: TFormatSettings);

procedure SetPropertyValue(Obj: TObject; const PropName: String; Value: Variant);
function GetPropertyValue(Obj: TObject; const PropName: String): Variant;

function ReadXmlFromFile(const FileName: String; Flags: TXMLReaderFlags): TXmlDocument;
function ReadXmlFromStream(Stream: TStream; Flags: TXMLReaderFlags): TXmlDocument;
function ReadXmlFromString(const XmlData: String; Flags: TXMLReaderFlags): TXmlDocument;
procedure ReadXmlNodeFromString(var AParentNode: TDOMNode; const XmlData: String; Flags: TXMLReaderFlags);
procedure WriteXmlToFile(ADoc: TXmlDocument; const FileName: String; Flags: TXMLWriterFlags);
procedure WriteXmlToStream(ADoc: TXmlDocument; Stream: TStream; Flags: TXMLWriterFlags);
procedure WriteXmlToString(ADoc: TXmlDocument; var XmlData: String; Flags: TXMLWriterFlags);
procedure WriteXmlNodeToString(ANode: TDOMNode; var XmlData: String; Flags: TXMLWriterFlags);
//procedure SetXmlNodeAttrValue(ANode: TDOMNode; const AttrName, AttrValue: String);

function ReadJSONFromString(Const JSON : String) : TJSONData;
function ReadJSONFromStream(Const JSON : TStream) : TJSONData;
function ReadJSONFromFile(Const FileName: String) : TJSONData;

function Utf8CharToString(Utf8Char: TUtf8Char): String;
procedure StringToUtf8Char(const S: String; out Utf8Char: TUtf8Char);

function MyRandom(l:longint):longint;
function MyFormat (Const Fmt : String; const Args : Array of const) : String;
function MyClipboard: TClipboard;

implementation

uses
  Variants, LazUtf8, formmanager, sqlgen, datasetprocessor, BGRABitmap,
  expressions, reportmanager, towordsfuncs, dbengine, apputils,
  dxsqlquery, appsettings, Math, md5, sha1, base64, errorsform, DateUtils,
  exprform, StrUtils;

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

procedure TObjectDestroyForm(Fm: TdxForm);
begin
  TDataSetProcessor(Fm.DSP).Free;
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

procedure RequeryDataSetIfNeed(DataSet: TObject);
begin
  if DataSet is TdxForm then
    TdxForm(DataSet).RequeryIfNeed
  else if DataSet is TdxQueryGrid then
    TdxQueryGrid(DataSet).RequeryIfNeed;
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

  RequeryDataSetIfNeed(DataSet);
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
    Result := DS.FieldByName(Col.FieldNameDS);
  end
  {else if DataSet is TdxQuery then
  begin
    pF := TdxQuery(DataSet).RD.FindFieldByName(FieldName);
    if pF = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    Result := DS.FieldByName('f' + IntToStr(pF^.Id));
  end;}
end;

procedure DisableDataSetScroll(DS: TDataSet; var BeforeScroll, AfterScroll: TDataSetNotifyEvent;
  var B: TBookmark; var State: TDataSetState);
begin
  BeforeScroll:=DS.BeforeScroll;
  AfterScroll:=DS.AfterScroll;
  DS.BeforeScroll:=nil;
  DS.AfterScroll:=nil;
  B := DS.GetBookmark;
  State := DS.State;
  DS.DisableControls;
end;

procedure EnableDataSetScroll(DS: TDataSet; BeforeScroll, AfterScroll: TDataSetNotifyEvent;
  var B: TBookmark; State: TDataSetState);
begin
  DS.GotoBookmark(B);
  DS.FreeBookmark(B);
  DS.BeforeScroll:=BeforeScroll;
  DS.AfterScroll:=AfterScroll;
  DS.EnableControls;
  if (DS.State <> State) and (State in [dsInsert, dsEdit]) then
    DS.Edit;
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
      if not F.IsNull then
      begin
	      Result := Result + F.AsFloat;
  	    Inc(n);
      end;
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
    while not DS.Eof do
    begin
      if Result = Null then
        Result := F.Value
      else if (not F.IsNull) and (F.Value > Result) then
      	Result := F.Value;
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
    while not DS.Eof do
    begin
      if Result = Null then
        Result := F.Value
      else if (not F.IsNull) and (F.Value < Result) then
      	Result := F.Value;
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

function MyIntToHex(Value: Int64; Digits: Integer): String;
begin
  Result := IntToHex(Value, Digits);
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

function MyUtf8Length(const S: String): PtrInt;
begin
  Result := UTF8Length(s);
end;

procedure MyUTF8Insert(const source: String; var s: String;
  StartCharIndex: PtrInt);
begin
  UTF8Insert(source, s, StartCharIndex);
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

function MyUTF8StringReplace(const S, OldPattern, NewPattern: String;
  Flags: TReplaceFlags): String;
begin
  Result := UTF8StringReplace(S, OldPattern, NewPattern, Flags);
end;

procedure MyUTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
begin
  Utf8Delete(s, StartCharIndex, CharCount);
end;

function MyStringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
begin
  Result := StringReplace(S, OldPattern, NewPattern, Flags);
end;

function MyUTF8ToUTF16(const S: AnsiString): UnicodeString;
begin
  Result := Utf8ToUtf16(S);
end;

function MyUTF16ToUTF8(const S: UnicodeString): AnsiString;
begin
  Result := Utf16ToUtf8(S);
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

function VarIsNothing(V: Variant): Boolean;
begin
  Result :=
    (TVarData(V).VType = varDispatch)
    and
    (TVarData(V).VDispatch = nil);
end;

function ShowExprEditor(const Expression, FormName: String
  ): String;
var
  Fm: TdxForm;
begin
  if FormName <> '' then
  begin
    Fm := FormMan.FindFormByName(FormName);
    if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);
  end
  else Fm := nil;
  Result := Expression;
  ShowExprForm(etSetValue, nil, Result, Fm, nil, nil, nil);
end;

function GetFormatSettings: TFormatSettings;
begin
  Result := DefaultFormatSettings;
end;

procedure SetFormatSettings(Settings: TFormatSettings);
begin
  DefaultFormatSettings := Settings;
end;

procedure SetPropertyValue(Obj: TObject; const PropName: String; Value: Variant
  );
var
  PInfo: PPropInfo;
begin
  GetLowPropInfo(Obj, PropName, Obj, PInfo);
  if PInfo <> nil then
    SetPropValue(Obj, PInfo, Value)
  else
    raise Exception.CreateFmt('Unknown property: "%s%',
      [Copy(PropName, RPos('.', PropName) + 1, 1024)]);
end;

function GetPropertyValue(Obj: TObject; const PropName: String): Variant;
var
  PInfo: PPropInfo;
begin
  GetLowPropInfo(Obj, PropName, Obj, PInfo);
  if PInfo <> nil then
    Result := GetPropValue(Obj, PInfo)
  else
    raise Exception.CreateFmt('Unknown property: "%s%',
      [Copy(PropName, RPos('.', PropName) + 1, 1024)]);
end;

function ReadXmlFromFile(const FileName: String; Flags: TXMLReaderFlags
  ): TXmlDocument;
begin
  ReadXmlFile(Result, FileName, Flags);
end;

function ReadXmlFromStream(Stream: TStream; Flags: TXMLReaderFlags
  ): TXmlDocument;
begin
  ReadXmlFile(Result, Stream, Flags);
end;

function ReadXmlFromString(const XmlData: String; Flags: TXMLReaderFlags
  ): TXmlDocument;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(XmlData);
  try
    ReadXmlFile(Result, SS, Flags);
  finally
    SS.Free;
  end;
end;

procedure ReadXmlNodeFromString(var AParentNode: TDOMNode;
  const XmlData: String; Flags: TXMLReaderFlags);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(XmlData);
  try
    ReadXMLFragment(AParentNode, SS, Flags);
  finally
    SS.Free;
  end;
end;

procedure WriteXmlToFile(ADoc: TXmlDocument; const FileName: String;
  Flags: TXMLWriterFlags);
begin
  WriteXmlFile(ADoc, FileName, Flags);
end;

procedure WriteXmlToStream(ADoc: TXmlDocument; Stream: TStream;
  Flags: TXMLWriterFlags);
begin
  WriteXmlFile(ADoc, Stream, Flags);
end;

procedure WriteXmlToString(ADoc: TXmlDocument; var XmlData: String;
  Flags: TXMLWriterFlags);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  try
    WriteXmlFile(ADoc, SS, Flags);
    XmlData := SS.DataString;
  finally
    SS.Free;
  end;
end;

procedure WriteXmlNodeToString(ANode: TDOMNode; var XmlData: String;
  Flags: TXMLWriterFlags);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  try
    WriteXML(ANode, SS, Flags);
    XmlData := SS.DataString;
  finally
    SS.Free;
  end;
end;

// Свойство TDomElement.AttribStrings не пропускает атрибуты с национальными символами,
// поэтому пришлось делать свою обертку.
{type
  THackNode = class(TDOMNode);
  THackNodeMap = class(TDOMNamedNodeMap);
  THackDomAttr = class(TDOMAttr);
procedure SetXmlNodeAttrValue(ANode: TDOMNode; const AttrName,
  AttrValue: String);
var
  n: LongWord;
  Node: THackNode;
  Attrs: THackNodeMap;
  Attr: TDOMAttr;
begin
  Node := THackNode(ANode);
  Attrs := THackNodeMap(Node.Attributes);
  Node.Changing;
  if Attrs.FindSorted(AttrName, n) then
    Attr := Attrs.SortedItem[n] as TDOMAttr
  else
  begin
    Attr := Node.FOwnerDocument.CreateAttributeBuf(PChar(AttrName), Length(AttrName));
    THackDOMAttr(Attr).FOwnerElement := TDomElement(ANode);
    if Attrs.FSortedList=nil then Attrs.FSortedList:=TFPList.Create;
    Attrs.FSortedList.Insert(n, Attr);
    if Attrs.FPosList=nil then Attrs.FPosList:=TFPList.Create;
    Attrs.FPosList.Add(Attr);
  end;
  Attr.NodeValue := AttrValue;
end; }

function ReadJSONFromString(const JSON: String): TJSONData;
begin
  Result := GetJSON(JSON);
end;

function ReadJSONFromStream(const JSON: TStream): TJSONData;
begin
  if JSON.Position = 0 then SkipBOM(JSON);
  Result := GetJSON(JSON);
end;

function ReadJSONFromFile(const FileName: String): TJSONData;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
  try
    SkipBOM(FS);
    Result := GetJSON(FS);
  finally
    FS.Free;
  end;
end;

function Utf8CharToString(Utf8Char: TUtf8Char): String;
begin
  Result := Utf8Char;
end;

procedure StringToUtf8Char(const S: String; out Utf8Char: TUtf8Char);
begin
  Utf8Char := S;
end;

function MyRandom(l: longint): longint;
begin
  Result := Random(l);
end;

function MyFormat(const Fmt: String; const Args: array of const): String;
begin
  Result := Format(Fmt, Args);
end;

function MyClipboard: TClipboard;
begin
  Result := Clipboard;
end;

end.

