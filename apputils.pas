{-------------------------------------------------------------------------------

    Copyright 2015-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit AppUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, {Windows, }SysUtils, strconsts, Menus, DBGrids, dxctrls, DXReports,
  Lists, Db, Controls, Graphics, StdCtrls, Forms, LclIntf, ComCtrls, Buttons,
  SynEdit, LclType, ImgList, formmanager, reportmanager, process,
  TypInfo, Types, IBConnection, crossapi;

//const
  //BUILD_DATE = '22.3.12';

type
  EAssertError = class(Exception);

  TRenameObject = (renForm, renQuery, renReport, renField, renComponent,
    renRpField, renObject, renImage);

procedure TestNil(P: Pointer; const Msg: String);
procedure DebugFile(const FileName: String; Value: Variant);
function AppPath: String;
procedure ErrMsg(const Msg: String; Log: Boolean = False; const Context: String = '');
procedure ErrMsgFmt(const Msg: String; Params: array of const; Log: Boolean = False; const Context: String = '');
procedure ClearList(L: TList);
function ConfirmDelete: Boolean;
//procedure SetMenuItemImage(MI: TMenuItem; const ResName: String);
function CreateMenuItem(aMenu: TMenu; const Caption: String; Tag: PtrInt; aShortCut: TShortCut;
  Handler: TNotifyEvent; ImageIndex: Integer = -1): TMenuItem;
function SortColumnToId(Gr: TdxGrid): Integer;
function IdToColumn(Gr: TdxGrid; Id: Integer): TColumn;
procedure OpenFile(const FileName: String);
procedure SplitStr(const S: String; D: Char; SL: TStrings);
procedure SplitStr2(const S, D: String; SL: TStrings);
function GetOutputDir: String;
function GetAbsolutePath(const Path: String): String;
function CopyToStorageFolder(const Src, Dir, Dest: String): String;
function OpenPictureDialog(OnlyPNG: Boolean = False): String;
function OpenPictureDialogMulti(AFiles: TStrings): Boolean;
function SavePictureDialog(DefaultFileName: String; OnlyPNG: Boolean = False): String;
function SaveFileDialog(const aTitle: String; DefaultFileName: String): String;
//procedure ShowImages(Fm: TdxForm);
function IsUrl(S: String): Boolean;
function IsMail(var S: String): Boolean;
procedure OpenUrl(const S: String);
function ReadToken(const S: String; var P: Integer; var Tk: Char): String;
function GetTemplatesDir: String;
procedure DeleteRefFromIntfs(Id: Integer; IsForm: Boolean);
procedure DeleteReferences(C: TComponent);
function Bool2Str(B: Boolean): String;
function Str2Bool(const S: String): Boolean;
function CheckFileName(const S: String): Boolean;
procedure CalcLabelExpr(C: TdxLabel; aDS: TDataSet; aParentForm: TdxForm);
function GetTopControl(WC: TWinControl): TWinControl;
function GetObjFieldValue(Obj: TComponent; aKey: Integer; FullPath: Boolean): String;
//function GetObjFieldKey(Obj: TdxLookupComboBox; aValue: String): Variant;
procedure ClearObjectFieldId(Fm: TdxForm; OId, FId: Integer);
//function ShiftColor(Color: TColor; N: Integer): TColor;
function FindGridById(Fm: TdxForm; aId: Integer): TdxGrid;
function CheckFieldName(const S: String): Boolean;
function CheckFormName(const S: String): Boolean;
//function CheckName2(const S: String): Boolean;
function CheckType(C: TComponent; var Value: String): Boolean;
function FindGridColumn(G: TdxGrid; Id: Integer): TColumn;
//function CheckComponentNames(Fm: TdxForm): Boolean;
function StrToXml(S: String): String;
function XmlToStr(S: String): String;
function HtmlToXml(S: String): String;
function XmlToHtml(S: String): String;
function CheckCompatibles(C1, C2: TComponent): Boolean;
function FindColumnByTag(G: TdxGrid; Tag: Integer): TColumn;
function SetZeros(E: Extended; N: Integer): String;
function StrToCsv(const S: String): String;
function CsvToStr(const S: String): String;
procedure GetTemplates(L: TStrings);
procedure CheckDeleteQuery(Fm: TdxForm; aId: Integer);
function GetUniqueFileName(RecId, FieldId: Integer; FileName: String): String;
function LookupObjectField(ObjF: TdxObjectField; ForceObject: Boolean): TComponent;
//procedure AddScripts;
function LookupFieldValue(aForm, aParForm: TdxForm; aDS: TDataSet;
  const aFieldName: String; aSkipLabels: Boolean): Variant;
function FormLookupFieldValue(aForm: TdxForm; aDS: TDataSet; const aFieldName: String): Variant;
function CreateLabel(AOwner: TWinControl; const aCaption: String): TLabel;
procedure AnchorCtrl(aControl, aTarget: TControl; aSpace: Integer);
function CheckDuplicateQueryName(const aName: String; aRD: TReportData): Boolean;
function GetParentForm(aC: TControl): TCustomForm;
{procedure DisableDataSetScroll(DS: TDataSet; var BeforeScroll, AfterScroll: TDataSetNotifyEvent;
  var B: TBookmark; var State: TDataSetState);
procedure EnableDataSetScroll(DS: TDataSet; BeforeScroll, AfterScroll: TDataSetNotifyEvent;
  var B: TBookmark; State: TDataSetState);          }
//function QueryExists(Fm: TdxForm): Boolean;
procedure DeleteQueries(Fm: TdxForm);
function MathRound(X: Extended; N: Integer): Extended;
function FindFormByRDId(RdId: Integer): TdxForm;
function MaskedTextEmpty(const aText, aMask: String): Boolean;
function ValidText(const aText, aMask: String): Boolean;
function QuoteStr(const S: String): String;
function QStr(const S: String): String;
function EscapeQuotes(const S: String): String;
function ShellExec(const Operation, FileName, Params, WorkDir: String; ShowCmd: LongInt): Boolean;
function Date2Str(D: TDateTime): String;
function Str2Date(const S: String): TDateTime;
function Time2Str(T: TDateTime): String;
procedure Debug(Value: Variant);
function Confirm(const Caption, Msg: String): TModalResult;
procedure Info(const Msg: String);
//function CheckDuplicateLabel(ALabel: TdxLabel; const ACaption: String): Boolean;
//function CheckDuplicateLabel2(ALabel: TdxLabel; const ACaption: String): Boolean;
function CalcLabelWithSameCaptionExists(ALabel: TdxLabel): Boolean;
function GetObjectFieldField(ObjField: TdxObjectField): TComponent;
function IsNeedUserControl: Boolean;
function CheckFloatFieldRange(const FieldName: String; Number: Double; Prec: Integer): Boolean;
function CheckFloatRange(const S: String): String;
procedure SkipBOM(St: TStream);
function EncodeCellText(const S: String): String;
function DecodeCellText(const S: String): String;
function GetFormParentFieldFieldId(Fm: TdxForm): Integer;
//procedure LoadGlyphFromBase64(Glyph: TBitmap; const Buf: String);
//procedure SetDefaultGlyph(Bn: TdxButton);
function IsCorrectParentField(Fm: TdxForm; Obj: TComponent): Boolean;
procedure FieldsToList(Fm: TdxForm; Items: TStrings; Excepts: array of TClass);
procedure ExpandAllNodes(Tree: TCustomTreeView);
procedure CollapseAllNodes(Tree: TCustomTreeView);
procedure ResetLookupComponent(C: TComponent);
procedure CloneComponent(Src, Dest: TComponent);
function CheckDuplicateFieldName(const aName: String; aCmp: TComponent; IsShowExpr: Boolean = False): Boolean;
function EscapeSQuotes(const S: String): String;
//function TryCreateFile(const Dir, Prefix, Ext: String): TFileStream;
function GetOutputFileName(const FileName: String): String;
function IsHierarchyObj(Obj: TComponent): Boolean;
procedure SetDefaultFont(F: TFont);
procedure UpdatePivotFieldCaptions(Fm: TdxForm; RD: TReportData);
procedure SetNodeImageIndex(N: TTreeNode; Idx: Integer);
function GetUnixFileManager: String;
procedure AddFormHeight(Form: TForm);
//procedure UpdateMemoScrollBars(M: TCustomSynEdit);
function MyUtf8CompareText(const S1, S2: String): PtrInt;
function SortStr(const S: String): String;
function IsDataSetModified(Fm: TdxForm): Boolean;
function FieldExists(DSRi: Integer; const FieldName, E: String): Boolean;
function FieldExistsForQuery(const FieldName, E: String): Boolean;
function FormExists(FormName, E: String): Boolean;
//function QueryExistsInQuery(RD: TReportData; const QueryName: String): Boolean;
function BrStr(const S: String): String;
function FindQueryGrid(aForm: TdxForm; Id: Integer): TdxQueryGrid;
procedure DoInitControl(C: TComponent);
function GetDelimitedText(SL: TStrings; const D: String): String;
function ExceptionInvalidValueForField(E: Exception): Boolean;
//function FieldExistsInQuery(DSRi: Integer; RD: TReportData; const FieldName: String): Boolean;
function CheckModuleName(const S: String): Boolean;
//procedure RenameFileNamesCP866ToUtf8(Entries: TFullZipFileEntries; const OutputPath: String);
//function AdjustDPI(Value: Integer): Integer;
function GetComponentField(DS: TDataSet; C: TComponent): TField;
function GetComponentFieldValue(DS: TDataSet; C: TComponent): Variant;
function GetComponentDataSetFieldName(C: TComponent): String;
function IIF(Condition, Value1, Value2: Variant): Variant;
function EscapeSemicolon(const S: String): String;
function UnEscapeSemicolon(const S: String): String;
function CheckSuffixName(S: String): Boolean;
function MakeNumberFormat(Prec: Integer; Group, PadZeros: Boolean): String;
function GetVersionFromFile(const aDir: String): Integer;
procedure ScaleForm(Fm: TdxForm; DesignTimePPI: Integer);
procedure ScaleForms(FMan: TFormManager; DesignTimePPI: Integer);
procedure ScaleReport(RD: TReportData; FromPPI, ToPPI: Integer);
procedure ScaleReports(RMan: TReportManager; FromPPI, ToPPI: Integer);
function ScaleToScreen(I: Integer): Integer;
function ScaleRectToScreen(R: TRect): TRect;
function ScaleTo96(I: Integer): Integer;
function ScaleRectTo96(R: TRect): TRect;
function GetTextHeight(const S: String): Integer;
function GetObjectFullValue(Fm: TdxForm; SourceFId: Integer): Variant;
function GetPrevActiveForm(AForm: TCustomForm): TCustomForm;
procedure GetWindowBounds(AForm: TCustomForm; var R: TRect);
//function TranslitRus2Lat(const Str: string): string;
procedure PositionActiveFormCenter(AForm: TForm);
function CutStr(var S: String; D: Char): String;
function EvalExpr(const Expr: String; Fm: TdxForm): Variant;
function VarTypeToStr(V: Variant): String;
function GetComponentDataTypeStr(C: TComponent): String;
function ExceptionToString(E: Exception; TopSpace, BottomSpace: Boolean): String;
function AroundSpaces(const S: String): String;
function Spaces: String;
function GetFormRealBounds(AForm: TCustomForm): TRect;
procedure CorrectFormPos(APopupParent: TWinControl; AForm: TCustomForm);
procedure SetFormPropPosition(AForm: TCustomForm);
procedure GetLowPropInfo(Obj: TObject; PropName: String; out LowObj: TObject; out PropInfo: PPropInfo);
function TryStrToColor(const ColorStr: String; out Color: TColor): Boolean;
function ReplVertLine(const S: String): String;
function ToHtml(const S: String): String;
procedure SplitComponentName(const AName: String; out ANameStr: String;
  out ANameNum: Integer);
procedure RenameInActions(CurObj: TObject; RenameObject: TRenameObject;
  const OldName, NewName: String);
procedure RenameImagesInForm(Fm: TdxForm; const OldName, NewName: String);
procedure RenameImages(const OldName, NewName: String);
function CheckExistsInActions(CurObj: TObject; RenameObject: TRenameObject;
  const aName: String; const ExtraMsg: String = ''): Boolean;
function CheckImageExistsInActions(const aName: String; out Msg: String): Boolean;
procedure UpdateImagesInForm(Fm: TdxForm);
procedure ReadFile(const FileName: String; out Buf: String);
procedure TrimLineEnding(var S: String);
function GetFullCaption(Fm: TdxForm): String;
function RemoveNonPrintableChars(const S: String; KeepNewLine: Boolean = False): String;
procedure SaveString(const FileName: String; const S: String);
function LoadString(const FileName: String): String;
function ReplacePathDelimiters(const Path: String): String;
procedure LoadMetaFromCache;
function CanCache: Boolean;
function GetTempDirName: String;
procedure ShowMsgForm(const AMsg, ADetails: String);
function IsDeveloper: Boolean;
function IsEmptyApp: Boolean;
function GetBuildDate: TDateTime;
function BuildDateToStr: String;
function CreateGUIDString: String;
function GetListSourceField(C: TComponent): TComponent;
function IsRemoteDatabase(const DBName: String): Boolean;
function ConstructDBOpenFilter(IsOpenDialog: Boolean): String;
function GetDefaultDBExt: String;
procedure LogString(const Msg, Context: String);
function IsNumericComponent(C: TComponent): Boolean;
procedure LoopDetect(const AExpression: String; ACmp: TComponent; AForm: TdxForm;
  ARD: TReportData; AOnlyPrefix: Boolean);
function IsDesignerMode: Boolean;
function GetComponentDisplayFormat(Fm: TdxForm; C: TComponent): String;
procedure SetDSFieldDisplayFormat(F: TField; Fmt: String);
procedure DeleteLCbxListSourceField(Fm: TdxForm; RDId: Integer; const FieldNameDS: String);
function IsTextComponent(C: TComponent): Boolean;
procedure DrawImageFieldIntoGrid(Grid: TDBGrid; Column: TColumn; ImageField: TField; R: TRect);
procedure CalcQueryColor(RD: TReportData; Fm: TdxForm; RDS, DS: TDataSet;
  const TargetField: String; out FieldName: String; out Color: TColor);
function GetPPIndex: Integer;
procedure SetupImageList(IL: TCustomImageList; ResNames: array of String);
procedure SetupPicture(Pic: TPicture; const ResName: String);
procedure SetupBitBtn(Bn: TCustomBitBtn; const ResName: String);
procedure SetupSpeedButton(Bn: TSpeedButton; const ResName: String);
function CreateBitmapFromRes(const ResName: String): TCustomBitmap;
procedure ConvertToDXMainVersion2(AMain, AFmMan: TObject);
procedure SetFileDateTime(const FlNm: String; DT: TDateTime);
procedure SetFileDateTime(Handle: THandle; DT: TDateTime);
function GetFileDateTime(const FlNm: String): TDateTime;
function SameFileDateTime(const FlNm: String; ATime: TDateTime): Boolean;
function TryTextToDate(AText: String; out ResDate: TDateTime): Boolean;
function TextToDate(AText: String): TDateTime;

implementation

uses
  Dialogs, LazUtf8, {$ifdef windows}ShellApi,{$endif} appsettings, FileUtil, dximages,
  expressions, dbengine, Math, sqlgen, dxusers,
  pivotgrid, Variants, maskedit, outputform, mytypes, StrUtils,
  BGRABitmap, LConvEncoding, dxfiles, myctrls, dateutils, scriptmanager,
  uPSRuntime, SQLDb, appimagelists, dxactions, dxmains, LazFileUtils, imagemanager,
  scriptfuncs, msgform, mainframe, designerframe, mylogger;

procedure TestNil(P: Pointer; const Msg: String);
begin
  if P = nil then raise EAssertError.Create(Msg);
end;

procedure DebugFile(const FileName: String; Value: Variant);
var
  mode: Integer;
  S: String;
begin
  S := VarToStr(Value) + LineEnding;
  if not FileExists(FileName) then mode := fmCreate
  else mode := fmOpenWrite + fmShareDenyNone;
  with TFileStream.Create(FileName, mode) do
    try
      Position := Size;
      WriteBuffer(Pointer(S)^, Length(S));
    finally
      Free;
    end;
end;

function AppPath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

procedure ErrMsg(const Msg: String; Log: Boolean; const Context: String);
begin
  if Log then LogString(Msg, Context);
  MessageDlg(rsError, Msg, mtError, [mbOk], 0);
end;

procedure ErrMsgFmt(const Msg: String; Params: array of const; Log: Boolean;
  const Context: String);
begin
  ErrMsg(Format(Msg, Params), Log, Context);
end;

procedure ClearList(L: TList);
begin
  while L.Count > 0 do
  begin
    TObject(L[0]).Free;
    L.Delete(0);
  end;
end;

function ConfirmDelete: Boolean;
begin
  Result := MessageDlg(rsWarning, rsConfirmDelete, mtWarning, [mbYes, mbNo], 0) = mrYes;
end;

{procedure SetMenuItemImage(MI: TMenuItem; const ResName: String);
var
  B: TCustomBitmap;
begin
  B := CreateBitmapFromLazarusResource(ResName);
  MI.Bitmap.Assign(B);
  B.Free;
end;      }

function CreateMenuItem(aMenu: TMenu; const Caption: String; Tag: PtrInt;
  aShortCut: TShortCut; Handler: TNotifyEvent; ImageIndex: Integer): TMenuItem;
begin
  Result := TMenuItem.Create(aMenu);
  Result.Caption:=Caption;
  Result.Tag := Tag;
  Result.ShortCut:=aShortCut;
  Result.OnClick:=Handler;
  Result.ImageIndex:=ImageIndex;
end;

function SortColumnToId(Gr: TdxGrid): Integer;
var
  C: TColumn;
begin
  Result := 0;
  C := Gr.ColumnFromCol(Gr.SortColumn);
  if C <> nil then
    Result := C.Tag;
end;

function IdToColumn(Gr: TdxGrid; Id: Integer): TColumn;
var
  i: Integer;
  C: TColumn;
begin
  Result := nil;
  for i := 0 to Gr.Columns.Count - 1 do
  begin
    C := Gr.Columns[i];
    if C.Tag = Id then
      Exit(C);
  end;
end;

procedure OpenFile(const FileName: String);
begin
  if not FileExists(FileName) then
    ErrMsg(Format(rsFileNotExists, [FileName]), True, 'OpenFile')
  else
    ShellExec('open',FileName,'','',1);
end;

procedure SplitStr(const S: String; D: Char; SL: TStrings);
var
  W: String;
  i: Integer;
begin
  SL.Clear;
  if S = '' then Exit;
  W := '';
  for i := 1 to Length(S) do
  begin
    if S[i] = D then
    begin
      SL.Add(W);
      W := '';
    end
    else
      W := W + S[i];
  end;
  //if W <> '' then
    SL.Add(W);
end;

procedure SplitStr2(const S, D: String; SL: TStrings);
var
  W: String;
  i, Len, DLen: Integer;
begin
  SL.Clear;
  if S = '' then Exit;
  W := '';
  Len := Length(S);
  DLen := Length(D);
  i := 1;
  while i <= Len do
  begin
    if Copy(S, i, DLen) = D then
    begin
      SL.Add(W);
      W := '';
      i := i + DLen;
    end
    else
    begin
      W := W + S[i];
      Inc(i);
    end;
  end;
  //if W <> '' then
    SL.Add(W);
end;

function GetOutputDir: String;
begin
  Result := AppConfig.OutputDir;
  if Result = '' then
    Result := GetTempDir
  // Относительный путь
  else Result := GetAbsolutePath(Result);
end;

function GetAbsolutePath(const Path: String): String;
begin
  if Path <> '' then
    Result := IncludeTrailingPathDelimiter(Path);
  //if (Pos(':', Result) <> 2) and (Pos('/', Result) <> 1) then
  {$ifdef windows}
  if not ((Pos(':', Result) = 2) or (Pos('\\', Result) = 1)) then
  {$else}
  if Pos('/', Result) <> 1 then
  {$endif}
    Result := AppPath + Result;
end;

function CopyToStorageFolder(const Src, Dir, Dest: String): String;
begin
  Result := '';
  if not ForceDirectories(Dir) then
    Exit(Format(rsCouldNotCreateFolder, [Dir]));
  if not CopyFile(Src, Dir + Dest) then
    Exit(Format(rsCantCopyFileToStorageFolder, [Src, Dir + Dest]));
end;

function OpenPictureDialog(OnlyPNG: Boolean): String;
begin
  Result := '';
  with TOpenDialog.Create(nil) do
  try
    Title := rsLoadImage;
    if OnlyPNG then
      Filter := rsPNGFiles
    else
      Filter := rsOpenPicturesFilter;
    Options := Options + [ofFileMustExist];
    if Execute then Result := FileName;
  finally
    Free;
  end;
end;

function OpenPictureDialogMulti(AFiles: TStrings): Boolean;
begin
  with TOpenDialog.Create(nil) do
  try
    Title := rsLoadImage;
    Filter := rsOpenPicturesFilter;
    Options := Options + [ofFileMustExist, ofAllowMultiSelect];
    Result := Execute;
    if Result then AFiles.Assign(Files);
  finally
    Free;
  end;
end;

function SavePictureDialog(DefaultFileName: String; OnlyPNG: Boolean): String;
begin
  Result := '';
  with TSaveDialog.Create(nil) do
  try
    Title := rsSaveImage;
    if OnlyPNG then
      Filter := rsPNGFiles
    else
      Filter := rsSavePicturesFilter;
    Options := Options + [ofPathMustExist, ofOverwritePrompt];
    FileName := DefaultFileName;
    DefaultExt := '.png';//ExtractFileExt(FileName);
    if Execute then Result := FileName;
  finally
    Free;
  end;
end;

function SaveFileDialog(const aTitle: String; DefaultFileName: String): String;
var
  Ext: String;
begin
  Result := '';
  Ext := Copy(ExtractFileExt(DefaultFileName), 2, 255);
  with TSaveDialog.Create(nil) do
  try
    Title := aTitle;
    DefaultExt := '.' + Ext;
    Filter := Format(rsFileFilter, [AnsiUpperCase(Ext), Ext]);
    Options := Options + [ofPathMustExist, ofOverwritePrompt];
    FileName := DefaultFileName;
    if Execute then Result := FileName;
  finally
    Free;
  end;
end;

{procedure ShowImages(Fm: TdxForm);
var
  C: TComponent;
  i: Integer;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxDBImage then
      TdxDBImage(C).ShowImage
    else if (C is TdxMemo) and (GetFieldSize(C) = 0) then
      TdxMemo(C).LoadMemo;
  end;
end;    }

function IsUrl(S: String): Boolean;
begin
  S := Utf8LowerCase(S);
  Result := (Copy(S, 1, 7) = 'http://') or (Copy(S, 1, 8) = 'https://') or
    (Copy(S, 1, 4) = 'www.');
end;

function IsMail(var S: String): Boolean;
begin
  Result := Pos('@', S) > 0;
  if Result and (Pos('mailto:', Utf8LowerCase(S)) = 0) then
    S := 'mailto:' + S;
end;

procedure OpenUrl(const S: String);
begin
  ShellExec('open', S, '', '', 1);
end;

function ReadToken(const S: String; var P: Integer; var Tk: Char): String;
var
  Len, Start: Integer;
  W, Qt: String;
begin
  Len := Length(S);
  Start := P;
  if P > Len then
  begin
    Tk := #0;
    Exit('');
  end;
  case S[P] of
    #1..#32:
      begin
        while (P <= Len) and (S[P] in [#1..#32]) do
        begin
          Inc(P);
        end;
        Tk := ' ';
        Result := Copy(S, Start, P - Start);
      end;
    '[':
      begin
        Tk := '[';
        while (P <= Len) and (S[P] <> ']') do
        begin
          Inc(P);
        end;
        Inc(P);
        Result := Copy(S, Start, P - Start);
      end;
    '=', '<', '>', '+', '-', '*', '|', '&', '(', ')', '#':
      begin
        Tk := '=';
        if (P < Len) and (S[P + 1] in ['>', '=']) and not (S[P] in ['(', ')']) then
        begin
          Inc(P);
        end;
        Inc(P);
        Result := Copy(S, Start, P - Start);
      end;
    '''', '"':
      begin
        Tk := '''';
        Qt := S[P];
        Inc(P);
        while (P <= Len) and (S[P] <> Qt) do
        begin
          Inc(P);
        end;
        Inc(P);
        Result := Copy(S, Start, P - Start);
      end;
    '0'..'9':
      begin
        Tk := '0';
        while S[P] in ['0'..'9', '.'] do
        begin
          Inc(P);
        end;
        Result := Copy(S, Start, P - Start);
      end;
    'a'..'z', 'A'..'Z', '_':
      begin
        Tk := 'a';
        while (P <= Len) and (S[P] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
        begin
          Inc(P);
        end;
        Result := Copy(S, Start, P - Start);
      end;
    '/':
      begin
        if (P < Len) and (S[P + 1] = '/') then
        begin
          while (P <= Len) and not (S[P] in [#13, #10]) do
          begin
            Inc(P);
          end;
          Tk := '/';
          Result := Copy(S, Start, P - Start);
        end
        else if (P < Len) and (S[P + 1] = '*') then
        begin
          Inc(P, 2);
          while P < Len do
          begin
            if (S[P] = '*') and (S[P + 1] = '/') then
            begin
              Inc(P, 2);
              Break;
            end;
            Inc(P);
          end;
          Tk := '/';
          Result := Copy(S, Start, P - Start);
        end
        else
        begin
          Tk := '=';
          Result := S[P];
          Inc(P);
        end;
      end
    else
    begin
      Tk := S[P];
      Result := Tk;
      Inc(P);
    end;
  end;
end;

function GetTemplatesDir: String;
begin
  Result := AppConfig.TemplateDir;
  if Result = '' then
    Result := AppPath + 'templates' + DirectorySeparator
  // Относительный путь
  else Result := GetAbsolutePath(Result);
end;

procedure CheckInsertValues(Fm: TdxForm; Obj: TdxLookupComboBox; Id: Integer);
var
  i: Integer;
  Vl: TInsertValueData;
begin
  for i := Obj.InsertedValues.Count - 1 downto 0 do
  begin
    Vl := Obj.InsertedValues[i];
    if (Vl.SrcField = Id) or (Vl.DestField = Id) then
    begin
    	Obj.InsertedValues.DeleteValue(i);
      Fm.SetFormChanged;
    end;
  end;
end;

procedure CheckListFields(Fm: TdxForm; Obj: TdxLookupComboBox; Id: Integer);
var
  i: Integer;
begin
  for i := Obj.ListFields.Count - 1 downto 0 do
    if Obj.ListFields[i].FieldId = Id then
    begin
      Obj.ListFields.Delete(i);
      Fm.SetFormChanged;
    end;
end;

{procedure CheckFilter(CC: TComponent; Id: Integer);
var
  S, Flt: String;
  SL: TStringList;
  i: Integer;
begin
  Flt := GetComboFilter(CC);
  if Copy(Flt, 1, 7) <> 'FILTER:' then Exit;
  Delete(Flt, 1, 7);
  SL := TStringList.Create;
  SplitStr2(Flt, ' ~~ ', SL);
  for i := SL.Count - 1 downto 0 do
  begin
    S := SL[i];
    if Id = StrToInt(Copy(S, 1, Pos('|', S) - 1)) then SL.Delete(i);
  end;
  Flt := 'FILTER:';
  for i := 0 to SL.Count - 1 do
  begin
    Flt := Flt + SL[i];
    if i < SL.Count - 1 then
      Flt := Flt + ' ~~ ';
  end;
  SetComboFilter(CC, Flt);
  SL.Free;
end;  }

{function GetObjFieldKey(Obj: TdxLookupComboBox; aValue: String): Variant;
var
  Fm: TdxForm;
  S, SQL: String;
  C: TComponent;
  SL: TStringList;
  HasQuotes: Boolean;
  i, PId: Integer;
begin
  Result := Null;
  if (Obj.SourceTId = 0) or (Obj.SourceFId = 0) then Exit;

  Fm := FormMan.FindForm(Obj.SourceTId);
  C := FindById(Fm, Obj.SourceFId);
  HasQuotes := (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo);
  if Fm.ParentField = 0 then
  begin
    SQL := 'select id from ' + TableStr(Fm.Id) + ' where ' +
      FieldStr(C) + '=';
    if HasQuotes then
      SQL := SQL + '''' + aValue + ''''
    else
      SQL := SQL + aValue;
    with DBase.OpenDataSet(SQL) do
    begin
      if RecordCount > 0 then Result := Fields[0].Value;
      Free;
    end;
  end
  else
  begin
    PId := 0;
    SL := TStringList.Create;
    SplitStr(aValue, '\', SL);
    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      SQL := 'select id from ' + TableStr(Fm.Id) + ' where ' +
        FieldStr(C) + '=';
      if HasQuotes then
        SQL := SQL + '''' + S + ''''
      else
        SQL := SQL + S;
      SQL := SQL + ' and ' + FieldStr(Fm.ParentField);
      if i = 0 then SQL := SQL + ' is null'
      else SQL := SQL + '=' + IntToStr(PId);

      with DBase.OpenDataSet(SQL) do
      begin
        if RecordCount > 0 then PId := Fields[0].Value
        else PId := 0;
        Free;
      end;
      if PId = 0 then Break;
    end;
    if PId > 0 then Result := PId;
    SL.Free;
  end;
end; }

procedure ClearObjectFieldId(Fm: TdxForm; OId, FId: Integer);
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxObjectField then
      with TdxObjectField(C) do
        if ObjId = OId then
        begin
          if (FId = 0) or (FId = FieldId) then FieldId := 0;
        end;
  end;
end;

// Обнуляет PriceObjFId
procedure CheckShopping(FId: Integer);
var
  i: Integer;
  Fm: TdxForm;
begin
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if (Fm.PId > 0) and (Fm.HasShop) then
      if Fm.ShopData.PriceObjFId = FId then
        Fm.ShopData.PriceObjFId := 0;
  end;
end;

procedure CheckFillTablePropsForm(Fm: TdxForm; C: TdxLookupComboBox; FmId: Integer);
var
  i: Integer;
  S: String;
begin
  with C do
  begin
    if (SourceTable = FmId) or (SourceTId = FmId) then
    begin
      SourceTable := 0;
      FillFilter := '';
      for i := 0 to FieldsTables.Count - 1 do
      begin
        S := FieldsTables[i];
        FieldsTables[i] := Copy(S, Pos('=', S), 255);
      end;
      Fm.SetFormChanged;
    end;
    if DestTable = FmId then
    begin
      DestTable := 0;
      for i := 0 to FieldsTables.Count - 1 do
      begin
        S := FieldsTables[i];
        FieldsTables[i] := Copy(S, 1, Pos('=', S));
      end;
      Fm.SetFormChanged;
    end;
  end;
end;

procedure _CheckDeleteForm(Fm: TdxForm; DelC, CurC: TComponent);
var
  TId: Integer;
begin
  if (CurC is TdxComboBox) or (CurC is TdxLookupComboBox) or (CurC is TdxMemo) then
  begin
    TId := GetSourceTId(CurC);
    if TId = GetId(DelC) then
    begin
      ResetLookupComponent(CurC);
      Fm.SetFormChanged;
    end;
    if CurC is TdxLookupComboBox then
    	CheckFillTablePropsForm(Fm, TdxLookupComboBox(CurC), GetId(DelC));
  end
end;

procedure CheckFillTablePropsField(Fm: TdxForm; C: TdxLookupComboBox; ObjFmId, FId: Integer);
var
  i: Integer;
  S: String;
  n: LongInt;
begin
  with C do
    if (SourceTable = ObjFmId) or (DestTable = ObjFmId) then
      for i := 0 to FieldsTables.Count - 1 do
      begin
        S := FieldsTables.Names[i];
        if S <> '' then
        begin
          n := StrToInt(S);
          if n = FId then
          begin
            FieldsTables[i] := '=' + FieldsTables.ValueFromIndex[i];
            Fm.SetFormChanged;
          end;
        end;
        S := FieldsTables.ValueFromIndex[i];
        if S <> '' then
        begin
          n := StrToInt(S);
          if n = FId then
          begin
            FieldsTables[i] := FieldsTables.Names[i] + '=';
            Fm.SetFormChanged;
          end;
        end;
      end;
end;

procedure CheckFormTree(Fm: TdxForm; FId: Integer);
var
  i: Integer;
  TF: TdxFormTreeField;
begin
  for i := Fm.Tree.Fields.Count - 1 downto 0 do
  begin
    TF := Fm.Tree.Fields[i];
    if TF.FieldId = FId then
    	Fm.Tree.Fields.Delete(i);
  end;
end;

procedure CheckDeleteField(Fm: TdxForm; DelC, CurC: TComponent);
var
  ObjFm: TdxForm;
  FId: Integer;
begin
  ObjFm := TdxForm(DelC.Owner);
  FId := GetId(DelC);
  if (CurC is TdxComboBox) or (CurC is TdxLookupComboBox) or (CurC is TdxMemo) then
  begin
    if (GetSourceTId(CurC) = ObjFm.Id) and (GetSourceFId(CurC) = FId) then
      SetSourceFId(CurC, 0);
    if CurC is TdxLookupComboBox then
      with TdxLookupComboBox(CurC) do
      begin
        if SourceTId = ObjFm.Id then
          CheckListFields(Fm, TdxLookupComboBox(CurC), FId);
        CheckInsertValues(Fm, TdxLookupComboBox(CurC), FId);
        CheckFillTablePropsField(Fm, TdxLookupComboBox(CurC), ObjFm.Id, FId)
      end
  end
  else if CurC is TdxObjectField then
  begin
    with TdxObjectField(CurC) do
      if (DelC is TdxLookupComboBox) and (ObjId = FId) then
      begin
        ObjId := 0;
        FieldId := 0;
        Fm.SetFormChanged;
      end
      else if (ObjId > 0) and (FieldId = FId) then
      begin
        FieldId := 0;
        Fm.SetFormChanged;
      end;
  end;
end;

procedure DeleteRefFromIntfs(Id: Integer; IsForm: Boolean);
var
  i: Integer;
  Intf: TdxIntf;

  procedure DeleteRefFromMenu(Menu: TdxMenuItemList);
  var
    j: Integer;
    MI: TdxMenuItem;
  begin
    for j := Menu.Count - 1 downto 0 do
    begin
      MI := Menu[j];
      if (MI.Kind = miForm) and IsForm and (MI.Id = Id) then
      begin
        Menu.DeleteItem(MI);
        UserMan.SetIntfsChanged;
      end
      else if (MI.Kind = miReport) and (MI.Id = Id) then
      begin
        Menu.DeleteItem(MI);
        UserMan.SetIntfsChanged;
      end
      else if MI.Kind = miMenu then
        DeleteRefFromMenu(MI.Items);
    end;
  end;

  procedure DeleteRefFromTabs(Tabs: TdxTabList);
  var
    j: Integer;
  begin
    for j := Tabs.Count - 1 downto 0 do
      if Tabs[j] = Id then
      begin
        Tabs.Delete(j);
        UserMan.SetIntfsChanged;
      end;
  end;

begin
  for i := 0 to UserMan.Intfs.Count - 1 do
  begin
    Intf := UserMan.Intfs[i];
    if IsForm then
      DeleteRefFromTabs(Intf.Tabs);
    DeleteRefFromMenu(Intf.Menu);
  end;
end;

procedure DeleteReferences(C: TComponent);
var
  i, j: Integer;
  F, Fm: TdxForm;
  CC: TComponent;
  Col: TColumn;
  CD: TSortColumn;
  R: TdxRole;
  FR: TdxFormRight;
  CR: TdxControlRight;
begin
  for i := 0 to FormMan.FormCount - 1 do
  begin
    F := FormMan.Forms[i];
    for j := 0 to F.ComponentCount - 1 do
    begin
      CC := F.Components[j];
      if C is TdxForm then
        _CheckDeleteForm(F, C, CC)
      else
        CheckDeleteField(F, C, CC);
    end;
  end;
  if C is TdxForm then
  begin
    DeleteRefFromIntfs(TdxForm(C).Id, True);
    Exit;
  end;
  Fm := TdxForm(C.Owner);
  if C is TdxLookupComboBox then
  begin
    with Fm.ShopData do
      if ObjId = GetId(C) then Clear;
    if Fm.ParentField = GetId(C) then Fm.ParentField:=0;
  end
  else if C is TdxCalcEdit then CheckShopping(GetId(C));
  CheckFormTree(Fm, GetId(C));
  {for i := 0 to Fm.ComponentCount - 1 do
  begin
    CC := Fm.Components[i];
    if CC is TdxLookupComboBox then
    begin
      CheckInsertValues(TdxLookupComboBox(CC), GetId(C));
      ClearObjectFieldId(Fm, GetId(CC), GetId(C));
    end;
  end; }
  Col := FindGridColumn(Fm.Grid, GetId(C));
  TestNil(Col, 'DeleteReferences: Col=nil');
  CD := Fm.Grid.SortCols.FindCol(Col);
  if CD <> nil then
    Fm.Grid.SortCols.RemoveCol(CD);
  // !!! Доступ. Удаляем правила доступа к компоненту
  for i := 0 to UserMan.Roles.Count - 1 do
  begin
    R := UserMan.Roles[i];
    for j := 0 to R.FormRights.Count - 1 do
    begin
      FR := R.FormRights[j];
      if FR.FormId = Fm.Id then
      begin
        CR := FR.Controls.FindRight(C.Name);
        if CR <> nil then FR.Controls.DeleteRight(CR);
        UserMan.SetRolesChanged;
      end;
    end;
  end;
end;

function Bool2Str(B: Boolean): String;
begin
  if B then Result := '1'
  else Result := '0';
end;

function Str2Bool(const S: String): Boolean;
begin
  Result := False;
  if S = '1' then Result := True;
end;

function CheckFileName(const S: String): Boolean;
begin
  Result := False;
  if Utf8Length(S) > 255 then
    ErrMsg(rsFilePathTooLong, True, 'CheckFileName')
  else if Utf8Length(ExtractFileName(S)) > 150 then
    ErrMsg(rsFileNameTooLong, True, 'CheckFileName')
  else
    Result := True;
end;

procedure CalcLabelExpr(C: TdxLabel; aDS: TDataSet; aParentForm: TdxForm);
var
  E: TExpression;
begin
  E := nil;
  with TExpressionBuilder.Create do
  try
    Form := TdxForm(C.Owner);
    DataSet := aDS;
    ParentForm := aParentForm;
    E := Build(C.Expression);
    C.Value := E.Calc;
  finally
    Free;
    FreeAndNil(E);
  end;
end;

function GetTopControl(WC: TWinControl): TWinControl;
var
  i, TabOrd, j: Integer;
  C: TControl;
  WinC: TWinControl;
begin
  Result := nil;
  TabOrd := 0;

  for i := 0 to WC.ControlCount - 1 do
  	for j := 0 to WC.ControlCount - 1 do
    begin
      C := WC.Controls[j];
      if C is TWinControl then
      begin
        WinC := TWinControl(C);
        if WinC.TabOrder = TabOrd then
        begin
          if WinC.ControlCount > 0 then Result := GetTopControl(WinC);
          if Result = nil then
          begin
            if WinC.TabStop then Result := WinC
            else
            begin
              Inc(TabOrd);
              Break;
            end;
          end;
          Exit;
        end;
      end;
    end;
end;

function GetObjFieldValue(Obj: TComponent; aKey: Integer; FullPath: Boolean
  ): String;
var
  SQL: String;
  n, p: Integer;
  SrcFm: TdxForm;
begin
  Result := '';
  if (aKey = 0) or (GetSourceTId(Obj) = 0) or (GetSourceFId(Obj) = 0) then Exit;
  SrcFm := FormMan.FindForm(GetSourceTId(Obj));
  SQL := SqlLookupSelect(Obj, nil, nil, nil, False, aKey);
  with DBase.OpenDataSet(SQL) do
  begin
    if RecordCount > 0 then
    	Result := Fields[1].AsString;
    Free;
  end;
  if (not FullPath) and (SrcFm.ParentField > 0) and
    (GetFormParentFieldFieldId(SrcFm) = GetSourceFId(Obj)) then
  begin
    p := Pos('\', Result);
    n := 0;
    while p > 0 do
		begin
    	Delete(Result, 1, p);
      Inc(n);
      p := Pos('\', Result);
    end;
  	Result := DupeString('  ', n) + Result;
  end;
end;

// Содрал с InvertColor
{function ShiftColor(Color: TColor; N: Integer): TColor;
var
  R, G, B: Integer;
begin
  R := Color and $ff;
  G := (Color shr 8) and $ff;
  B := (Color shr 16) and $ff;

  if N > 0 then R := Min(R + N, 255)
  else R := Max(R + N, 0);
  if N > 0 then G := Min(G + N, 255)
  else G := Max(G + N, 0);
  if N > 0 then B := Min(B + N, 255)
  else B := Max(B + N, 0);

  Result := ((B and $ff) shl 16) or ((G and $ff) shl 8) or (R and $ff);
end;    }


function FindGridById(Fm: TdxForm; aId: Integer): TdxGrid;
var
  C: TComponent;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxGrid) and (TdxGrid(C).Id = aId) then
      Exit(TdxGrid(C));
  end;
end;

function CheckFieldName(const S: String): Boolean;
var
  i: Integer;
begin
  Result := True;
  if Trim(S) = '' then
  begin
    ErrMsg(rsEnterName);
    Exit(False);
  end;
  for i := 1 to Length(S) do
    if S[i] in ['''', '"', '|', '!', '<', '>', '[', ']', '{', '}', '/', '='] then
    begin
      ErrMsgFmt(rsInvalidCharInFieldName, [S[i], S]);
      Exit(False);
    end;
end;

function CheckFormName(const S: String): Boolean;
var
  i: Integer;
begin
  Result := True;
  if Trim(S) = '' then
  begin
    ErrMsg(rsEnterName);
    Exit(False);
  end;
  for i := 1 to Length(S) do
    if S[i] in ['''', '"', '|', '!', '<', '>', '[', ']', '{', '}', '/', '\', '?', ':', '*'] then
    begin
      ErrMsgFmt(rsInvalidCharInFormName, [S[i], S]);
      Exit(False);
    end;
end;

function CheckType(C: TComponent; var Value: String): Boolean;
var
  N: integer;
  E: Extended;
  D: TDateTime;
  FS: TFormatSettings;
begin
  Result := True;
  if ((C is TdxLookupComboBox) or (C is TdxCounter) or (C is TdxRecordId)) and (not TryStrToInt(Value, N)) then
    Result := False
  else if (C is TdxCheckBox) and ((Value <> '0') and (Value <> '1')) then
    Result := False
  else if C is TdxCalcEdit then
  begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator:='.';
    if TryStrToFloat(Value, E) then Value := FloatToStr(E, FS)
    else Result := False;
  end
  else if C is TdxDateEdit then
  begin
    if TryTextToDate(Value, D) then Value := Date2Str(D)
    else Result := False;
  end
  else if C is TdxTimeEdit then
  begin
    if TryStrToTime(Value, D) then Value := TimeToStr(D)
    else Result := False;
  end
end;

function FindGridColumn(G: TdxGrid; Id: Integer): TColumn;
var
  j: Integer;
begin
  Result := nil;
  for j := 0 to G.Columns.Count - 1 do
    if G.Columns[j].Tag = Id then Exit(G.Columns[j]);
end;

{function CheckComponentNames(Fm: TdxForm): Boolean;
var
  i: Integer;
  C: TComponent;
begin
  Result := True;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C.Name = '' then
      Exit(False);
  end;
end; }

function StrToXml(S: String): String;
begin
  S := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  S := StringReplace(S, '"', '&quot;', [rfReplaceAll]);
  S := StringReplace(S, '<', '&lt;', [rfReplaceAll]);
  S := StringReplace(S, '>', '&gt;', [rfReplaceAll]);
  Result := S;
end;

function XmlToStr(S: String): String;
begin
  S := StringReplace(S, '&amp;', '&', [rfReplaceAll]);
  S := StringReplace(S, '&quot;', '"', [rfReplaceAll]);
  S := StringReplace(S, '&lt;', '<', [rfReplaceAll]);
  S := StringReplace(S, '&gr;', '>', [rfReplaceAll]);
  S := StringReplace(S, '&gt;', '>', [rfReplaceAll]);
  Result := S;
end;

function HtmlToXml(S: String): String;
begin
  S := StringReplace(S, '&amp;', '#amp;', [rfReplaceAll]);
  S := StringReplace(S, '&quot;', '#quot;', [rfReplaceAll]);
  S := StringReplace(S, '&lt;', '#lt;', [rfReplaceAll]);
  S := StringReplace(S, '&gt;', '#gt;', [rfReplaceAll]);
  Result := StrToXml(S);
end;

function XmlToHtml(S: String): String;
begin
  S := XmlToStr(S);
  S := StringReplace(S, '#amp;', '&amp;', [rfReplaceAll]);
  S := StringReplace(S, '#quot;', '&quot;', [rfReplaceAll]);
  S := StringReplace(S, '#lt;', '&lt;', [rfReplaceAll]);
  S := StringReplace(S, '#gt;', '&gt;', [rfReplaceAll]);
  Result := S;
end;

function CheckCompatibles(C1, C2: TComponent): Boolean;
begin
  Result := True;
  //if ((C1 is TdxEdit) or (C1 is TdxMemo) or (C1 is TdxComboBox)) and
  //  ((C2 is TdxEdit) or (C2 is TdxMemo) or (C2 is TdxComboBox)) then
  if (C2 is TdxEdit) or (C2 is TdxMemo) or (C2 is TdxComboBox) then
  else if ((C1 is TdxCalcEdit) or (C1 is TdxCounter) or (C1 is TdxRecordId) or (C1 is TdxLookupComboBox)) and (C2 is TdxCalcEdit) then
  else if (C1 is TdxRecordId) and (C2 is TdxLookupComboBox) then
  else if (C1 is TdxCalcEdit) and (GetPrecission(C1) = 0) and (C2 is TdxLookupComboBox) then
  else if C1.ClassName = C2.ClassName then
  else Result := False;
end;

function FindColumnByTag(G: TdxGrid; Tag: Integer): TColumn;
var
  i: Integer;
  C: TColumn;
begin
  Result := nil;
  for i := 0 to G.Columns.Count - 1 do
  begin
    C := G.Columns[i];
    if C.Tag = Tag then Exit(C);
  end;
end;

function SetZeros(E: Extended; N: Integer): String;
var
  i: Integer;
  S: String;
begin
  S := IntToStr(Trunc(E));
  Result := S;
  for i := Length(S) to N - 1 do
    Result := '0' + Result;
end;

function StrToCsv(const S: String): String;
var
  i: Integer;
  NeedQuote: Boolean;
begin
  Result := '';
  NeedQuote := False;
  for i := 1 to Length(S) do
  begin
    if S[i] in [#13, #10, '"', ';'] then
    begin
      NeedQuote := True;
      if S[i] = '"' then Result := Result + '"';
    end;
    Result := Result + S[i];
  end;
  if NeedQuote then Result := '"' + Result + '"';
end;

function CsvToStr(const S: String): String;
var
  i, b, l: Integer;
begin
  Result := '';
  if S = '' then Exit;
  b := 1; l := Length(S);

  if S[1] = '"' then
  begin
    b := 2;
    l := l - 1;
  end;

  i := b;
  while i <= l do
  begin
    if (S[i] = '"') and (i < l) and (S[i + 1] = '"') then
    begin
      Result := Result + '"';
      i := i + 2;
    end
    else
    begin
      Result := Result + S[i];
      Inc(i);
    end;
  end;
end;

procedure GetTemplates(L: TStrings);
var
  Dir: String;
  SL: TStringListUtf8;
  Len: Integer;
  S: String;
  i: Integer;
begin
  Dir := GetTemplatesDir;
  Len := Length(Dir);
  SL := TStringListUtf8.Create;
  try
    FindAllFiles(SL, Dir, '*.docx;*.docm;*.odt;*.ods;*.xml;*.html', True);
    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      Delete(S, 1, Len);
      SL[i] := S;
    end;
    SL.Sort;
    L.Assign(SL);
  finally
    SL.Free;
  end;
end;

procedure CheckDeleteQuery(Fm: TdxForm; aId: Integer);
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxPivotGrid then
    begin
      with TdxPivotGrid(C) do
        if Id = aId then Clear;
    end
    else if C is TdxLookupComboBox then
    begin
      with TdxLookupComboBox(C) do
        if ListSource = aId then
          ResetListSource;
    end;
  end;
end;

function GetUniqueFileName(RecId, FieldId: Integer; FileName: String): String;
var
  Ext: String;
begin
  Ext := ExtractFileExt(FileName);
  FileName := ChangeFileExt(FileName, '');
  Result := IntToStr(RecId) + '_' + IntToStr(FieldId) + '_' + FileName;
  Result := Utf8Copy(Result, 1, 255 - Utf8Length(Ext)) + Ext;
end;

function LookupObjectField(ObjF: TdxObjectField; ForceObject: Boolean): TComponent;
var
  Fm: TdxForm;
  Obj: TComponent;
begin
  Result := nil;
  Fm := TdxForm(ObjF.Owner);
  Obj := FindById(Fm, ObjF.ObjId);
  if Obj <> nil then
  begin
    Fm := FormMan.FindForm(GetSourceTId(Obj));
    if Fm <> nil then
    begin
      Result := FindById(Fm, ObjF.FieldId);
      // Если поле объекта само является объектом, то находим его поле.
      if (Result <> nil) and (Result is TdxLookupComboBox) and ForceObject then
      begin
        Fm := FormMan.FindForm(GetSourceTId(Result));
        if Fm <> nil then
        begin
          Result := FindById(Fm, GetSourceFId(Result));
        end;
      end;
    end;
  end;
end;

function LabelFieldExists(const FieldName, E: String): Boolean;
var
  S, SS: String;
  L, i, p: Integer;
  IsField: Boolean;
begin
  Result := False;
  S := Utf8LowerCase(FieldName);
  L := Length(E);
  IsField := False;
  for i := 1 to L do
  begin
    if E[i] = '[' then
    begin
      IsField := True;
      p := i + 1;
    end
    else if IsField and (p = i) and (E[i] in ['!', ':']) then
      p := i + 1
    else if IsField and ((E[i] = ']') or (i = L)) then
    begin
      SS := Utf8LowerCase(Copy(E, p, i - p));
      if S = SS then Exit(True);
      IsField := False;
    end;
  end;
end;

function LookupFieldValue(aForm, aParForm: TdxForm; aDS: TDataSet;
  const aFieldName: String; aSkipLabels: Boolean): Variant;
var
  SL: TStringList;
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  DS, MainDS: TDataSet;
  V: Variant;
  Lbl: TdxLabel;
  Tmp: String;
  Cbx: TdxLookupComboBox;
begin
  Result := Null;
  if aFieldName = '' then raise Exception.Create(rsFieldNameEmpty);

  SL := TStringList.Create;
  SplitStr(aFieldName, '|', SL);
  Fm := aForm;
  DS := aDS;
  if SL[0][1] = '!' then
  begin
    SL[0] := Copy(SL[0], 2, 1024);
    if (Fm.PId > 0) and (aParForm <> nil) then
    begin
      Fm := aParForm;
      DS := Fm.DataSet;//Fm.Grid.DataSource.DataSet;
    end
  end
  // Префикс для указания, что это поле текущей формы. Введено, чтобы различать
  // поля форм и поля запроса/отчета.
  else if SL[0][1] = ':' then
    SL[0] := Copy(SL[0], 2, 1024);
  MainDS := DS;

  try

  C := FindComponentByFieldName(Fm, SL[0]);
  if C <> nil then
    for i := 0 to SL.Count - 1 do
    begin
      //V := DS.FieldByName(FieldStr(C)).Value;
      V := GetComponentFieldValue(DS, C);
      if V = Null then Break;

      if (C is TdxLookupComboBox) and (i < SL.Count - 1) then
      begin
        Fm := FormMan.FindForm(GetSourceTId(C));
        if Fm = nil then Exit;
        if DS <> MainDS then FreeAndNil(DS);
        Cbx := TdxLookupComboBox(C);

        C := FindComponentByFieldName(Fm, SL[i+1]);
	      if C = nil then Break;
        // Если поле объекта отображается в компоненте, то значение берется из
        // компонента без запроса к базе.
        if (GetId(C) = Cbx.SourceFId) and (SL.Count = 2) then
        begin
          Result := MainDS.FieldByName(FieldStr(Cbx.Id) + 'l').Value;
          Break;
        end
        else if C is TdxRecordId then
        begin
          Result := V;
          Break;
        end;

        Tmp := SqlSelectGroups(Fm.Id, True);
        if Tmp <> '' then Tmp := '(' + Tmp + ')'
        else Tmp := TableStr(Fm.Id);

        DS := DBase.OpenDataSet('select ' + GetComponentDataSetFieldName(C)
          {FieldStr(C)} + ' from ' + Tmp + ' where id=' + VarToStr(V));
      end
      else
      begin
        Result := GetComponentFieldValue(DS, C);
        //Result := DS.FieldByName(FieldStr(C)).Value;
        Break;
      end;
    end;

  Lbl := nil;
  if (C = nil) and (SL.Count = 1) and (not aSkipLabels) then
  begin
    Lbl := FindLabelByFieldName(Fm, SL[0], False);
    if Lbl = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);
    if not LabelFieldExists(aFieldName, Lbl.Expression) then
    begin
      if Lbl.Value = unassigned then
      begin
        // Вычисление выражения может спровоцировать зацикливание, если
        // произойдет повторное обращение к надписи из-за того, что в процессе
        // вычисления значение надписи все еще unassigned. Поэтому присваиваем
        // надписи любое значение до вычисления выражения. Хотя в этой
        // функции это маловероятно, но решил подстраховаться.
        Lbl.Value := Null;
        CalcLabelExpr(Lbl, aDS, aParForm);
      end;
      Result := Lbl.Value;
    end;
  end;
  if (C = nil) and (Lbl = nil) then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);

  finally
    if DS <> MainDS then FreeAndNil(DS);
    SL.Free;
  end;
end;

function FormLookupFieldValue(aForm: TdxForm; aDS: TDataSet; const aFieldName: String
  ): Variant;
var
  SL: TStringList;
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  DS: TDataSet;
  V: Variant;
  Tmp: String;
  Cbx: TdxLookupComboBox;
begin
  Result := Null;
  if aFieldName = '' then raise Exception.Create(rsFieldNameEmpty);

  SL := TStringList.Create;
  SplitStr(aFieldName, '|', SL);
  Fm := aForm;
  DS := aDS;

  try

  C := FindComponentByFieldName(Fm, SL[0]);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);

  for i := 0 to SL.Count - 1 do
  begin
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);
    if (C is TdxLookupComboBox) and (i < SL.Count - 1) then
    begin
      V := DS.FieldByName(FieldStr(C)).Value;
      Result := V;
      if V = Null then Break;
      Fm := FormMan.FindForm(GetSourceTId(C));
      if Fm = nil then Exit;
      if DS <> aDS then FreeAndNil(DS);
      Cbx := TdxLookupComboBox(C);

      C := FindComponentByFieldName(Fm, SL[i+1]);
      if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);
      // Если поле объекта отображается в компоненте, то значение берется из
      // компонента без запроса к базе.
      if (GetId(C) = Cbx.SourceFId) and (SL.Count = 2) then
      begin
        Result := aDS.FieldByName(FieldStr(Cbx.Id) + 'l').Value;
        Break;
      end
      else if C is TdxRecordId then Break;

      Tmp := SqlSelectGroups(Fm.Id, True);
      if Tmp <> '' then Tmp := '(' + Tmp + ')'
      else Tmp := TableStr(Fm.Id);

      DS := DBase.OpenDataSet('select ' + GetComponentDataSetFieldName(C)
        {FieldStr(C)} + ' from ' + Tmp + ' where id=' + VarToStr(V));
    end
    else
    begin
      Result := GetComponentFieldValue(DS, C);
      //Result := DS.FieldByName(FieldStr(C)).Value;
      Break;
    end;
  end;

  finally
    if DS <> aDS then FreeAndNil(DS);
    SL.Free;
  end;
end;

function CreateLabel(AOwner: TWinControl; const aCaption: String): TLabel;
begin
  Result := TLabel.Create(AOwner);
  Result.Parent := AOwner;
  Result.Caption := aCaption;
end;

procedure AnchorCtrl(aControl, aTarget: TControl; aSpace: Integer);
begin
  aControl.AnchorSideTop.Side := asrBottom;
  aControl.AnchorSideTop.Control := aTarget;
  aControl.BorderSpacing.Top := aSpace;
  aControl.Width:=aControl.Parent.ClientWidth;
  aControl.Anchors := [akLeft, akTop, akRight];
end;

function CheckDuplicateQueryName(const aName: String; aRD: TReportData): Boolean;
var
  i: Integer;
  Fm: TdxForm;
  RD: TReportData;
begin
  Result := True;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if MyUtf8CompareText(Fm.FormCaption, aName) = 0 then
    begin
      ErrMsg(rsFormNameExists);
      Exit(False);
    end;
  end;
  for i := 0 to ReportMan.ReportCount - 1 do
  begin
    RD := ReportMan.Reports[i];
    if (RD <> aRD) and (MyUtf8CompareText(aName, RD.Name) = 0) then
    begin
      ErrMsg(rsReportNameExists);
      Exit(False);
    end;
  end;
end;

function GetParentForm(aC: TControl): TCustomForm;
begin
  Result := nil;
  if aC.Parent = nil then Exit;
  if aC.Parent is TCustomForm then Exit(TForm(aC.Parent))
  else Result := GetParentForm(aC.Parent);
end;

{function QueryExists(Fm: TdxForm): Boolean;
var
  j: Integer;

  function QExists(Fm: TdxForm): Boolean;
  var
    i: Integer;
    C: TComponent;
  begin
    Result := False;
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if C is TdxQueryGrid then Exit(True);
    end;
  end;

begin
  Result := QExists(Fm);
  if (not Result) and (Fm.PId = 0) then
    for j := 0 to FormMan.FormCount - 1 do
      if FormMan.Forms[j].PId = Fm.Id then
      begin
        Result := QExists(FormMan.Forms[j]);
        if Result then Break;
      end;
  if Result then ErrMsg(rsFirstRemoveQueries);
end; }

procedure DeleteQueries(Fm: TdxForm);

	procedure _Delete(Fm: Tdxform);
  var
    i: Integer;
    C: TComponent;
    RD: TReportData;
  begin
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if C is TdxQueryGrid then
      begin
        RD := ReportMan.FindReport(TdxQueryGrid(C).Id);
    		ReportMan.DeleteReport(RD);
      end;
    end;
  end;

var
  j: Integer;
  Frm: TdxForm;
begin
  _Delete(Fm);
  if Fm.PId = 0 then
  	for j := 0 to FormMan.FormCount - 1 do
    begin
      Frm := FormMan.Forms[j];
      if Frm.PId = Fm.Id then
      	_Delete(Frm);
    end;
end;

// Алгоритм взят с этого форума:
// http://www.planetaexcel.ru/forum/?PAGE_NAME=read&FID=8&TID=3483
function MathRound(X: Extended; N: Integer): Extended;
var
  b: Extended;
begin
  If N < 0 Then
  begin
    b := Power(10, -N) * 1.0;
    Result := Round(X / b + X * 2E-16) * b;
  end
  Else
    Result := RoundTo(X + X * 2E-16,-N);

  If Abs(Result) = 0 Then Result := 0;
End;

function FindFormByRDId(RdId: Integer): TdxForm;
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
begin
  Result := nil;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxQueryGrid then
      	if TdxQueryGrid(C).Id = RdId then Exit(Fm);
    end;
  end;
end;

function MaskedTextEmpty(const aText, aMask: String): Boolean;
var
  S: TCaption;
begin
  Result := True;
  with TMaskEdit.Create(nil) do
  try
	  EditMask := aMask;
  	Text := aText;
    S := Text;
    Text := '';
    Result := Text = S;
  finally
    Free;
  end;
end;


// Таким образом можно проверить корректность ввода по маске, даже если
// фокус не на компоненте.
function ValidText(const aText, aMask: String): Boolean;
begin
  Result := True;
  with TMaskEdit.Create(nil) do
  try
	  EditMask := aMask;
  	Text := aText;
    if IsMasked then
    	try
      	ValidateEdit;
      except
        on E: EDBEditError do
        	Result := False;
      end;
  finally
    Free;
  end;
end;

function QuoteStr(const S: String): String;
begin
  Result := '"' + S + '"';
end;

function QStr(const S: String): String;
begin
  Result := '''' + S + '''';
end;

function EscapeQuotes(const S: String): String;
begin
  Result := StringReplace(S, '"', '""', [rfReplaceAll]);
end;

{function MaskedTextEmpty(const aText, aMask: String): Boolean;
const
  MaskChars = 'LlAaC09#:';
var
  SL: TStringList;
  i, p: Integer;
  S, UCh: String;
  Escape: Boolean;
begin
  Result := True;
  if (aText = '') or (aMask = '') then Exit;

  SL := TStringList.Create;
  SplitStr(aMask, ';', SL);
  S := SL[0];
  SL.Free;

  p := 1;
  Escape := False;
  for i := 1 to Utf8Length(S) do
  begin
    UCh := Utf8Copy(S, i, 1);
    if Escape then
    begin
      Escape := False;
      Inc(p);
      Continue;
    end;
    if UCh = '\' then
    begin
      Escape := True;
      Continue;
    end
    else if UCh[1] in ['!', '<', '>'] then Continue
    else if Pos(UCh[1], MaskChars) > 0 then
    begin
      if Utf8Copy(aText, p, 1) <> ' ' then Exit(False);
    end;
    Inc(p);
  end;
end;  }


procedure ParamsToArray(Params: String; var Arr: TStringArray);
var
  p, Size: Integer;
begin
  Size := 0;
  while Length(Params) > 0 do
  begin
    Trim(Params);
    if Params[1] = '"' then
    begin
      Delete(Params, 1, 1);
      p := Pos('"', Params);
      if p = 0 then Break;
    end
    else
    begin
      p := Pos(' ', Params);
      if p = 0 then p := Length(Params) + 1;
    end;
    SetLength(Arr, Size + 1);
    Arr[Size] := Copy(Params, 1, p - 1);
    Delete(Params, 1, p);
  end;
end;

function RemoveQuotes(const S: String): String;
var
  Len: Integer;
begin
  Len := Length(S);
  if (Copy(S, 1, 1) = '"') and (Copy(S, Len, 1) = '"') then
  	Result := Copy(S, 2, Len - 2)
  else
  	Result := S;
end;

function ShellExec(const Operation, FileName, Params, WorkDir: String;
  ShowCmd: LongInt): Boolean;
var
  OutS, S: String;
  Arr: TStringArray;
begin
  {$ifdef windows}
  Result := ShellExecute(0, PChar(Operation), PChar(Utf8ToWinCP(FileName)),
    PChar(Utf8ToWinCP(Params)), PChar(Utf8ToWinCP(WorkDir)), ShowCmd) > 32;
  {$else}
  S := RemoveQuotes(FileName);
  if IsUrl(S) or IsMail(S) then
    Result := LclIntf.OpenUrl(S)
  else if ExtractFileExt(S) <> '' then
    Result := LclIntf.OpenDocument(S)
  else
  begin
    ParamsToArray(Params, Arr);
    Result := RunCommandInDir(WorkDir, S, Arr, OutS, [poNoConsole]);
    SetLength(Arr, 0);
  end;
  {$endif}
end;

function Date2Str(D: TDateTime): String;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DateSeparator := '-';
  FS.ShortDateFormat := 'YYYY/MM/DD';
  Result := DateToStr(D, FS);
end;

function Str2Date(const S: String): TDateTime;
begin
  Result := StrToDate(S, 'YYYY/MM/DD', '-');
end;

function Time2Str(T: TDateTime): String;
begin
  Result := TimeToStr(T, DefaultSQLFormatSettings);
end;

procedure Debug(Value: Variant);
begin
  LogString(VarToStr(Value), 'Debug');
  ShowOutputForm(VarToStr(Value));
end;

function Confirm(const Caption, Msg: String): TModalResult;
begin
  Result := MessageDlg(Caption, Msg, mtConfirmation, [mbYes, mbNo], 0);
end;

procedure Info(const Msg: String);
begin
  MessageDlg(rsWarning, Msg, mtWarning, [mbOk], 0);
end;

{function CheckDuplicateLabel(ALabel: TdxLabel; const ACaption: String): Boolean;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
begin
  Result := True;
  Fm := TdxForm(ALabel.Owner);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxLabel) and (Trim(TdxLabel(C).Expression) <> '') and (C <> ALabel) and
    	(MyUtf8CompareText(TdxLabel(C).Caption, ACaption) = 0) then
    begin
     ErrMsg(rsCalcLabelCaptionExists);
      Exit(False);
    end;
  end;
end;  }

{function CheckDuplicateLabel2(ALabel: TdxLabel; const ACaption: String): Boolean;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
begin
  Result := True;
  Fm := TdxForm(ALabel.Owner);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxLabel) and (Trim(TdxLabel(C).Expression) <> '') and (C <> ALabel) and
    	(MyUtf8CompareText(TdxLabel(C).Caption, ACaption) = 0) then
    begin
     ErrMsg(rsCalcLabelCaptionExists2);
      Exit(False);
    end;
  end;
end; }

function CalcLabelWithSameCaptionExists(ALabel: TdxLabel): Boolean;
var
  i: Integer;
  C: TComponent;
  Fm: TdxForm;
begin
  Result := False;
  Fm := TdxForm(ALabel.Owner);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxLabel) and (Trim(TdxLabel(C).Expression) <> '') and (C <> ALabel) and
    	(MyUtf8CompareText(TdxLabel(C).Caption, ALabel.Caption) = 0) then
   		Exit(True);
  end;
end;

function GetObjectFieldField(ObjField: TdxObjectField): TComponent;
var
  Fm: TdxForm;
  C: TComponent;
begin
  Result := nil;
  Fm := TdxForm(ObjField.Owner);
  C := FindById(Fm, ObjField.ObjId);
  if C <> nil then
  begin
    Fm := FormMan.FindForm(TdxLookupComboBox(C).SourceTId);
    if Fm <> nil then
	    Result := FindById(Fm, ObjField.FieldId)
  end;
  //Result := C;
end;

function IsNeedUserControl: Boolean;
begin
  //Result := DBase.Remote and (UserMan.CurrentUser <> nil);
  Result := True;
end;

function CheckFloatFieldRange(const FieldName: String; Number: Double; Prec: Integer): Boolean;
begin
  Result := True;
	if ((Number >= 0) and (Number > Power(10, 15 - Prec))) or
  	((Number < 0) and (Number < -Power(10, 15 - Prec))) then
  begin
    ErrMsgFmt(rsFloatFieldOutRange, [FloatToStr(Number), FieldName]);
    Result := False;
  end;
end;

function CheckFloatRange(const S: String): String;
var
  E: Extended;
  Prec, p: SizeInt;
begin
  Result := '';
  if TryStrToFloat(S, E) then
  begin
    p := Pos(DefaultFormatSettings.DecimalSeparator, S);
    if p > 0 then
    begin
      Prec := Length(S) - p;
      if S[1] = '-' then Dec(Prec);
    end
    else Prec := 0;
    if ((E >= 0) and (E > Power(10, 15 - Prec))) or
  		((E < 0) and (E < -Power(10, 15 - Prec))) then
    	Result := Format(rsFloatOutRange, [S]);
  end
  else
  	Result := rsInvalidNumber;
end;

procedure SkipBOM(St: TStream);
var
  MarkHolder: Cardinal;
begin
  if (St.Size > 3) and (St.Position = 0) then
  begin
	  MarkHolder := St.ReadDWord;
  	if (MarkHolder and $00FFFFFF) = $00BFBBEF then
    	St.Position := 3
	  else
  	  St.Position := 0;
  end;
end;

function EncodeCellText(const S: String): String;
begin
  Result := StringReplace(S, '|', '&00A6', [rfReplaceAll]);
  Result := StringReplace(Result, ';', '&003B', [rfReplaceAll]);
end;

function DecodeCellText(const S: String): String;
begin
  Result := StringReplace(S, '&00A6', '|', [rfReplaceAll]);
  Result := StringReplace(Result, '&003B', ';', [rfReplaceAll]);
end;

function GetFormParentFieldFieldId(Fm: TdxForm): Integer;
var
  C: TComponent;
begin
  Result := 0;
  if Fm.ParentField = 0 then Exit;
  C := FindById(Fm, Fm.ParentField);
  if C <> nil then
  	Result := GetSourceFId(C);
end;

function IsCorrectParentField(Fm: TdxForm; Obj: TComponent): Boolean;
var
  C: TComponent;
begin
  Result := False;
  if Fm.Id <> GetSourceTId(Obj) then Exit;
  C := FindById(Fm, GetSourceFId(Obj));
  if (C <> nil) and
    ((C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo)) then
    Result := True;
end;

procedure FieldsToList(Fm: TdxForm; Items: TStrings; Excepts: array of TClass);
var
  i: Integer;
  SL: TStringListUtf8;
  C: TComponent;

  function IsExcept: Boolean;
  var
    j: Integer;
  begin
    Result := False;
    for j := 0 to Length(Excepts) - 1 do
    	if Excepts[j] = C.ClassType then Exit(True);
  end;

begin
  SL := TStringListUtf8.Create;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
  	C := Fm.Components[i];
		if (not HasFId(C)) or IsExcept then Continue;
    SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Items.Assign(SL);
end;

procedure ExpandAllNodes(Tree: TCustomTreeView);
var
  i: Integer;
begin
  for i := 0 to Tree.Items.Count - 1 do
		Tree.Items[i].Expand(False);
end;

procedure CollapseAllNodes(Tree: TCustomTreeView);
var
  i: Integer;
begin
  for i := 0 to Tree.Items.Count - 1 do
		Tree.Items[i].Collapse(False);
end;

procedure ResetLookupComponent(C: TComponent);
var
  Fm: TdxForm;
begin
  SetSourceTId(C, 0);
  SetSourceFId(C, 0);
  SetComboFilter(C, '');
  Fm := TdxForm(C.Owner);
  if C is TdxLookupComboBox then
    with TdxLookupComboBox(C) do
    begin
      if Fm.ShopData.ObjId = Id then Fm.ShopData.Clear;
      InsertedValues.Clear;
      ListFields.Clear;
      ClearInsertTableProps;
      ClearObjectFieldId(Fm, Id, 0);
      ListSource := 0;
      ListKeyField := '';
    end;
end;

procedure CloneComponent(Src, Dest: TComponent);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  MS.WriteComponent(Src);
  MS.Position := 0;
  Dest := MS.ReadComponent(Dest);
  MS.Free;
end;

function CheckDuplicateFieldName(const aName: String; aCmp: TComponent;
  IsShowExpr: Boolean): Boolean;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  S: String;
begin
  Result := True;
  Fm := TdxForm(aCmp.Owner);

  S := '';
  if IsShowExpr then S := ' ' + rsCanRenameLabelMsg;

  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C = aCmp then Continue;
    if HasFId(C) then
    begin
      if MyUtf8CompareText(GetFieldName(C), aName) = 0 then
      begin
        ErrMsg(rsComponentFieldNameExists + S);
        Exit(False);
      end;
    end
    else if (C is TdxLabel) and (Trim(GetExpression(C)) <> '') then
    begin
      if MyUtf8CompareText(TdxLabel(C).FieldName, aName) = 0 then
      begin
        ErrMsg(rsCalcLabelCaptionExists + S);
        Exit(False);
      end;
    end;
  end;
  for i := 0 to Fm.CalcFields.Count - 1 do
  begin
    if MyUtf8CompareText(Fm.CalcFields.Names[i], aName) = 0 then
    begin
      ErrMsg(rsCalcFieldNameExists + S);
      Exit(False);
    end;
  end;
end;

function EscapeSQuotes(const S: String): String;
begin
  Result := StringReplace(S, #39, #39#39, [rfReplaceAll]);
end;

{function TryCreateFile(const Dir, Prefix, Ext: String): TFileStream;
var
  I: Integer;
  TmpDir, FNm: String;
begin
  TmpDir := IncludeTrailingPathDelimiter(Dir) + Prefix;
  I:=0;
  while True do
  begin
    FNm:=Format('%s%.5d.%s',[TmpDir,I, Ext]);
    Inc(I);
    try
	    Result := TFileStream.Create(FNm, fmCreate);
      Break;
    except
      on E: EFCreateError do
      	Continue;
    end;
  end;
end;  }


function GetOutputFileName(const FileName: String): String;
var
  SrcHandle: THandle;
  i: Integer;
  Ext: String;
begin
  Result := FileName;
  Ext := ExtractFileExt(FileName);
  for i := 1 to 100 do
  begin
    if FileExists(Result) then
    begin
      SrcHandle := FileOpen(Result, fmOpenRead + fmShareExclusive);
      if (THandle(SrcHandle)=feInvalidHandle) then
        Result := ChangeFileExt(FileName, '') + SetZeros(i, 3) + Ext
      else
      begin
        FileClose(SrcHandle);
        Break;
      end;
    end
    else Break;
  end;
end;

function IsHierarchyObj(Obj: TComponent): Boolean;
var
  Fm: TdxForm;
begin
  Result := False;
  Fm := FormMan.FindForm(GetSourceTId(Obj));
  if Fm <> nil then
  	Result := Fm.ParentField > 0;
end;

procedure SetDefaultFont(F: TFont);
begin
  F.Name := 'Verdana';
  F.Size := 10;
  F.Style := [];
  F.Color := clBlack;
end;

procedure UpdatePivotFieldCaptions(Fm: TdxForm; RD: TReportData);
var
  i: Integer;
  C: TComponent;

  procedure _UpdateFields(Fields: TFieldCollection);
  var
    j: Integer;
    Col: TRpGridColumn;
    FI: TFieldItem;
  begin
    for j := 0 to RD.Grid.ColumnCount - 1 do
    begin
      Col := RD.Grid.Columns[j];
      FI := Fields.FindFieldByFieldName(Col.FieldNameDS);
      if FI <> nil then FI.Caption := Col.Caption;
    end;
  end;

  procedure _Update(PG: TdxPivotGrid);
  begin
    if PG.Id <> RD.Id then Exit;

    _UpdateFields(PG.RowFields);
		_UpdateFields(PG.ColFields);
    _UpdateFields(PG.DataFields);
  end;

begin
  if RD.IsEmpty then Exit;

  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxPivotGrid then _Update(TdxPivotGrid(C));
  end;
end;


procedure SetNodeImageIndex(N: TTreeNode; Idx: Integer);
begin
  N.ImageIndex := Idx;
  N.SelectedIndex := Idx;
end;

function GetUnixFileManager: String;
begin
  if FileExists('/usr/bin/nautilus') then
    Result := 'nautilus'
  else if FileExists('/usr/bin/thunar') then
    Result := 'thunar'
  else if FileExists('/usr/bin/nemo') then
    Result := 'nemo'
  else if FileExists('/usr/bin/dolphin') then
    Result := 'dolphin'
  else if FileExists('/usr/bin/pcmanfm') then
    Result := 'pcmanfm'
  else
    Result := '';
end;

procedure AddFormHeight(Form: TForm);
begin
  {$ifdef linux}
  Form.Height := Form.Height + 36;
  {$endif}
end;

// В 1.8.2 стали исчезать ползунки
{procedure UpdateMemoScrollBars(M: TCustomSynEdit);
begin
  M.ScrollBars:=ssNone;
  M.ScrollBars:=ssBoth;
end;}

// В линукс-версии 1.8.2-1.8.4 MyUtf8CompareText работает некорректно.
function MyUtf8CompareText(const S1, S2: String): PtrInt;
begin
  {$ifdef windows}
  Result := Utf8CompareText(S1, S2);
  {$else}
  Result := Utf8CompareStr(Utf8LowerCase(S1), Utf8LowerCase(S2));
  {$endif}
end;

function SortStr(const S: String): String;
begin
  with TStringListUtf8.Create do
  begin
    Text := S;
    Sort;
    Result := Text;
    Free;
  end;
end;

function IsDataSetModified(Fm: TdxForm): Boolean;
var
  F: TField;
  i: Integer;
  C: TComponent;
begin
  Result := False;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not IsField(C) then Continue;
    if C is TdxDBImage then
      Result := TdxDBImage(C).WasChanged
    else if C is TdxFile then
      Result := TdxFile(C).WasChanged
    else
    begin
      F := Fm.DataSet.FieldByName(FieldStr(C));
      Result := F.Value <> F.OldValue;
    end;
    if Result then Exit;
  end;
  {for i := 0 to DS.Fields.Count - 1 do
  begin
    F := DS.Fields[i];
    if F.Value <> F.OldValue then
    begin
      Exit(True);
    end;
  end;}
end;

function FieldExists(DSRi: Integer; const FieldName, E: String): Boolean;
var
  S, SS: String;
  L, i, p: Integer;
  IsField: Boolean;
begin
  Result := False;
  S := Utf8LowerCase(FieldName);
  L := Length(E);
  IsField := False;
  for i := 1 to L do
  begin
    if E[i] = '[' then
    begin
      IsField := True;
      p := i + 1;
    end
    else if IsField and (p = i) and
      ( ((DSRi = 0) and (E[i] in ['!', ':'])) or ((DSRi > 0) and (E[i] = ':')) ) then
      p := i + 1
    else if IsField and ((E[i] in [']', '|']) or (i = L)) then
    begin
      SS := Utf8LowerCase(Copy(E, p, i - p));
      if S = SS then Exit(True);
      IsField := False;
    end;
  end;
end;

// Эта версия функции для проверки присутствия поля формы в выходном фильтре или
// в вычисляемом поле запроса
function FieldExistsForQuery(const FieldName, E: String): Boolean;
var
  S, SS: String;
  L, i, p: Integer;
  IsField: Boolean;
begin
  Result := False;
  S := Utf8LowerCase(FieldName);
  L := Length(E);
  IsField := False;
  for i := 1 to L do
  begin
    // Принимаем только поля с префиксами
    if (E[i] = '[') and ( (Copy(E, i+1, 1) = '!') or (Copy(E, i+1, 1) = ':') ) then
    begin
      IsField := True;
      p := i + 2;
    end
    else if IsField and ((E[i] in [']', '|']) or (i = L)) then
    begin
      SS := Utf8LowerCase(Copy(E, p, i - p));
      if S = SS then Exit(True);
      IsField := False;
    end;
  end;
end;

function FormExists(FormName, E: String): Boolean;
begin
  E := Utf8LowerCase(E);
  FormName := Utf8LowerCase(FormName);
  Result := (Utf8Pos('''' + FormName + '''', E) > 0) or (Utf8Pos('"' + FormName + '"', E) > 0);
end;

function BrStr(const S: String): String;
begin
  Result := '[' + S + ']';
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

procedure DoInitControl(C: TComponent);
begin
  if C is TdxCalcEdit then TdxCalcEdit(C).Init
  else if C is TdxCheckBox then TdxCheckBox(C).Init
  else if C is TdxObjectField then TdxObjectField(C).Init
  else if C is TdxCounter then TdxCounter(C).Init
  else if C is TdxRecordId then TdxRecordId(C).Init
end;

// Вместо TStrings.DelimitedText, т. к. последний добавляет в пустую строку
// два QuoteChar. Засранец!
function GetDelimitedText(SL: TStrings; const D: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to SL.Count - 1 do
  begin
    Result := Result + SL[i];
    if i < SL.Count - 1 then Result := Result + D;
  end;
end;

function ExceptionInvalidValueForField(E: Exception): Boolean;
begin
  Result := (E is EDatabaseError) and (Copy(E.Message, 1, 24) = 'Invalid value for field ');
end;

function CheckModuleName(const S: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (AnsiLowerCase(S) = 'main') or (AnsiLowerCase(S) = 'webmain') then
  begin
    ErrMsgFmt(rsModuleNameReserved, [S]);
    Exit;
  end;
  for i := 1 to Length(S) do
    if S[i] in ['\', '/', ':', '*', '?', '"', '<', '>', '|'] then
    begin
      ErrMsg(rsInvalidCharInModuleName);
      Exit;
    end;
  Result := True;
end;

function IsUtf8(const S: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(S) do
    if Ord(S[i]) in [$D0, $D1] then Exit(True);
end;

{procedure RenameFileNamesCP866ToUtf8(Entries: TFullZipFileEntries;
  const OutputPath: String);
var
  i: Integer;
  S, SS: String;
begin
  for i := 0 to Entries.Count - 1 do
  begin
    S := Entries[i].DiskFileName;
    // Учитываем, что в предыдущих версиях имена файлов могут быть в UTF-8, а
    // не в CP866.
    if not IsUtf8(S) then
    begin
      SS := CP866ToUTF8(S);
      if SS <> S then
        RenameFile(OutputPath + S, OutputPath + SS);
    end;
  end;
end;      }

{function AdjustDPI(Value: Integer): Integer;
begin
  Result := Round(Value / (Screen.PixelsPerInch / 96));
end;   }

function GetComponentField(DS: TDataSet; C: TComponent): TField;
begin
  if C is TdxFile then
    Result := DS.FieldByName(FieldStr(C) + 'd')
  else if C is TdxDBImage then
    Result := DS.FieldByName(FieldStr(C) + 'src')
  else if HasFId(C) then
    Result := DS.FieldByName(FieldStr(C))
  else raise Exception.Create('GetComponentField: the component has no field');
end;

function GetComponentFieldValue(DS: TDataSet; C: TComponent): Variant;
begin
  Result := GetComponentField(DS, C).Value;
end;

function GetComponentDataSetFieldName(C: TComponent): String;
begin
  if C is TdxFile then
    Result := FieldStr(C) + 'd'
  else if C is TdxDBImage then
    Result := FieldStr(C) + 'src'
  else
    Result := FieldStr(C)
end;

function IIF(Condition, Value1, Value2: Variant): Variant;
begin
  if Condition = Null then
    raise Exception.Create(rsIIFNullDetect)
  else if not VarIsBool(Condition) then
    raise Exception.Create(rsIIFParamNotLogic);
  if Condition then
    Result := Value1
  else
    Result := Value2;
end;

function EscapeSemicolon(const S: String): String;
begin
  Result := StringReplace(S, ';', '#59', [rfReplaceAll]);
end;

function UnEscapeSemicolon(const S: String): String;
begin
  Result := StringReplace(S, '#59', ';', [rfReplaceAll]);
end;

function CheckSuffixName(S: String): Boolean;
var
  Len: Integer;
begin
  Result := True;
  S := Utf8LowerCase(S);
  Len := Length(S);
  if (Copy(S, Len - 4, 5) = '_text') or (Copy(S, Len - 3, 4) = '_not') or
    (Copy(S, Len - 4, 5) = '_null') or (Copy(S, Len - 5, 6) = '_begin') or
    (Copy(S, Len - 3, 4) = '_end') then
  begin
    ErrMsg(rsEndingsReserved);
    Exit(False);
  end;

end;

function MakeNumberFormat(Prec: Integer; Group, PadZeros: Boolean): String;
begin
  Result := '0';
  if Prec > 0 then
  begin
    if PadZeros then
      Result := '0.' + DupeString('0', Prec)
    else
      Result := '0.' + DupeString('#', Prec)
  end;
  if Group then Result := ',' + Result;
end;

function GetVersionFromFile(const aDir: String): Integer;
var
  FileName, S: String;
begin
  Result := 0;
  FileName := aDir + 'version';
  if not FileExists(FileName) then Exit;

  with TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone) do
  try
    SetLength(S, Size);
    Read(Pointer(S)^, Size);
    TryStrToInt(S, Result);
  finally
    Free;
  end;
end;

procedure ChangeScaleControl(C: TControl; ToPPI, FromPPI: Integer);

  function ScaleV(X: Integer): Integer;
  begin
    Result := MulDiv(X, ToPPI, FromPPI);
  end;

  procedure ChangeScaleGrid(G: TMyDBGrid);
  var
    i: Integer;
    Col: TColumn;
  begin
    G.DefaultRowHeight := ScaleV(G.DefaultRowHeight);
    if not G.TitleFont.IsEqual(G.Font) then
      G.TitleFont.Height := ScaleV(G.TitleFont.Height);
    if not G.Buttons.IsParentFont then
      G.ButtonFont.Height := ScaleV(G.ButtonFont.Height);
    G.ButtonSize := ScaleV(G.ButtonSize);
    for i := 0 to G.Columns.Count - 1 do
    begin
      Col := G.Columns[i];
      Col.Width := ScaleV(Col.Width);
      if not G.Font.IsEqual(Col.Font) then
        Col.Font.Height := ScaleV(Col.Font.Height);
      if not G.TitleFont.IsEqual(Col.Title.Font) then
        Col.Title.Font.Height := ScaleV(Col.Title.Font.Height);
    end;
  end;

  procedure ChangeScalePivotFields(Col: TFieldCollection);
  var
    i: Integer;
    FI: TFieldItem;
  begin
    for i := 0 to Col.Count - 1 do
    begin
      FI := Col[i];
      FI.Font.Height := ScaleV(FI.Font.Height);
      FI.FixedFont.Height := ScaleV(FI.FixedFont.Height);
      FI.TotalFont.Height := ScaleV(FI.TotalFont.Height);
      FI.TotalFixedFont.Height := ScaleV(FI.TotalFixedFont.Height);
      FI.Width := ScaleV(FI.Width);
      FI.Height := ScaleV(FI.Height);
      FI.TotalWidth := ScaleV(FI.TotalWidth);
    end;
  end;

  procedure ChangeScalePivotGrid(G: TdxPivotGrid);
  begin
    G.FixedFont.Height := ScaleV(G.FixedFont.Height);
    G.SelectedFont.Height := ScaleV(G.SelectedFont.Height);
    G.GrandTotalFixedFont.Height := ScaleV(G.GrandTotalFixedFont.Height);
    G.GrandTotalFont.Height := ScaleV(G.GrandTotalFont.Height);
    G.Indent := ScaleV(G.Indent);
    G.GrandTotalWidth := ScaleV(G.GrandTotalWidth);
    ChangeScalePivotFields(G.ColFields);
    ChangeScalePivotFields(G.RowFields);
    ChangeScalePivotFields(G.DataFields);
  end;

begin
  C.Left:=ScaleV(C.Left);
  C.Top:=ScaleV(C.Top);
  C.Width:=ScaleV(C.Width);
  C.Height:=ScaleV(C.Height);
  if not C.IsParentFont then
    C.Font.Height:=ScaleV(C.Font.Height);
  if C is TMyDBGrid then ChangeScaleGrid(TMyDBGrid(C))
  else if C is TdxPivotGrid then ChangeScalePivotGrid(TdxPivotGrid(C));
end;

procedure ScaleReport(RD: TReportData; FromPPI, ToPPI: Integer);

  function ScaleV(X: Integer): Integer;
  begin
    Result := MulDiv(X, ToPPI, FromPPI);
  end;

var
  G: TRpGrid;
  i: Integer;
  Col: TRpGridColumn;
begin
  if FromPPI = ToPPI then Exit;

  G := RD.Grid;
  G.DefaultRowHeight:=ScaleV(G.DefaultRowHeight);
  G.Font.Height:=ScaleV(G.Font.Height);
  G.TitleFont.Height:=ScaleV(G.TitleFont.Height);
  for i := 0 to G.ColumnCount - 1 do
  begin
    Col := G.Columns[i];
    Col.Width := ScaleV(Col.Width);
    Col.Font.Height := ScaleV(Col.Font.Height);
    Col.TitleFont.Height := ScaleV(Col.TitleFont.Height);
  end;
  RD.SetReportChanged;
end;

procedure ScaleForm(Fm: TdxForm; DesignTimePPI: Integer);

  procedure _DoScale(WC: TWinControl);
  var
    i: Integer;
    C: TControl;
  begin
    for i := 0 to WC.ControlCount - 1 do
    begin
      C := WC.Controls[i];
      if (C is TGridButtons) or (C is TSpeedButton) then Continue;
      ChangeScaleControl(C, Screen.PixelsPerInch, DesignTimePPI);
      if C is TWinControl then
        _DoScale(TWinControl(C));
    end;
  end;

begin
  if Screen.PixelsPerInch = DesignTimePPI then Exit;
  Fm.DisableAutoSizing;
  ChangeScaleControl(Fm, Screen.PixelsPerInch, DesignTimePPI);
  ChangeScaleControl(Fm.Grid, Screen.PixelsPerInch, DesignTimePPI);
  ChangeScaleControl(Fm.Tree, Screen.PixelsPerInch, DesignTimePPI);
  _DoScale(Fm);
  Fm.EnableAutoSizing;
  Fm.SetFormChanged;
end;

procedure ScaleForms(FMan: TFormManager; DesignTimePPI: Integer);
var
  i: Integer;
begin
  for i := 0 to FMan.FormCount - 1 do
    ScaleForm(FMan.Forms[i], DesignTimePPI);
end;

procedure ScaleReports(RMan: TReportManager; FromPPI, ToPPI: Integer);
var
  i: Integer;
begin
  for i := 0 to RMan.ReportCount - 1 do
    ScaleReport(RMan.Reports[i], FromPPI, ToPPI);
end;

function ScaleToScreen(I: Integer): Integer;
begin
  Result := MulDiv(I, Screen.PixelsPerInch, 96);
end;

function ScaleRectToScreen(R: TRect): TRect;
begin
  Result := Classes.Rect(ScaleToScreen(R.Left), ScaleToScreen(R.Top),
    ScaleToScreen(R.Right), ScaleToScreen(R.Bottom));
end;

function ScaleTo96(I: Integer): Integer;
begin
  Result := MulDiv(I, 96, Screen.PixelsPerInch);
end;

function ScaleRectTo96(R: TRect): TRect;
begin
  Result := Classes.Rect(ScaleTo96(R.Left), ScaleTo96(R.Top),
    ScaleTo96(R.Right), ScaleTo96(R.Bottom));
end;

function GetTextHeight(const S: String): Integer;
begin
  with TCanvas.Create do
  begin
    Handle := GetDC(0);
    Font.PixelsPerInch:=Screen.PixelsPerInch;
    Result := TextHeight(S);
    ReleaseDC(0, Handle);
    Free;
  end;
end;

function GetObjectFullValue(Fm: TdxForm; SourceFId: Integer): Variant;
var
  S: String;
  V: Variant;
begin
  V := Fm.DataSet.FieldByName(FieldStr(SourceFId)).Value;
  if (Fm.ParentField > 0) and (GetFormParentFieldFieldId(Fm) = SourceFId) then
  begin
    S := Fm.DataSet.FieldByName(FieldStr(Fm.ParentField) + 'l').AsString;
    if S <> '' then
      Result := S + '\' + VarToStr(V)
    else
      Result := V;
  end
  else
    Result := V;
end;

function GetPrevActiveForm(AForm: TCustomForm): TCustomForm;
var
  i: Integer;
  Fm: TCustomForm;
begin
  for i := 0 to Screen.CustomFormZOrderCount - 1 do
  begin
    Fm := Screen.CustomFormsZOrdered[i];
    if (Fm <> AForm) and Fm.Visible and (Fm is TWindow) then Exit(Fm);
  end;
  Result := Application.MainForm;
end;

procedure GetWindowBounds(AForm: TCustomForm; var R: TRect);
begin
  GetWindowRect(AForm.Handle, R);
  {$ifdef linux}
  if AForm.Height = R.Height then
    R.Height := R.Height + GetSystemMetrics(SM_CYCAPTION) +
      GetSystemMetrics(SM_CYFRAME) * 2;
  if AForm.Width = R.Width then
    R.Width := R.Width + GetSystemMetrics(SM_CXFRAME) * 2;
  {$endif}
end;

procedure PositionActiveFormCenter(AForm: TForm);
var
  Fm: TCustomForm;
  R, FmR, AFormR: TRect;
  M: TMonitor;
begin
  Fm := AForm.PopupParent;
  if Fm = nil then Fm := GetPrevActiveForm(AForm);

  GetWindowBounds(Fm, FmR);
  GetWindowBounds(AForm, AFormR);

  M := Screen.MonitorFromRect(FmR);
  if M = nil then Exit;
  R := M.WorkareaRect;

  // Ставим окно по центру активного окна.
  AForm.Left := FmR.Left + FmR.Width div 2 - AFormR.Width div 2;
  AForm.Top := FmR.Top + FmR.Height div 2 - AFormR.Height div 2;
  // Если новое окно больше предыдущего, то сдвигаем окно немного вправо и вниз.
  if (FmR.Width <= AFormR.Width + ScaleToScreen(15)) and (FmR.Height <= AFormR.Height + ScaleToScreen(30)) then
  begin
    AForm.Left := FmR.Left + ScaleToScreen(15);
    AForm.Top := FmR.Top + ScaleToScreen(30);
  end;
  // Если правая и нижняя сторона уходят за пределы рабочей области, то
  // сдвигаем окно влево и вверх к краю рабочей области.
  if AForm.Left + AFormR.Width > R.Right then
    AForm.Left := R.Right - AFormR.Width;
  if AForm.Top + AFormR.Height > R.Bottom then
    AForm.Top := R.Bottom - AFormR.Height;
  // Если левая и верхняя сторона выходят за пределы рабочей области,
  // сдвигаем окно в начало рабочей области.
  if AForm.Left < R.Left then AForm.Left := R.Left;
  if AForm.Top < R.Top then AForm.Top := R.Top;
  // Если размеры окна больше рабочей области, то устанавливаем размер
  // равным рабочей области.
  if AFormR.Height > R.Height then
    AForm.Height := R.Height - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYFRAME) * 2;
  if AFormR.Width > R.Width then
    AForm.Width := R.Width - GetSystemMetrics(SM_CXFRAME) * 2;
end;

{function TranslitRus2Lat(const Str: string): string;
const
  RArray = 'абвгдеёжзийклмнопрстуфхцчшщьыъэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯ';
  arr: array[1..66] of string =
  ('a', 'b', 'v', 'g', 'd', 'e', 'yo', 'zh', 'z', 'i', 'y',
    'k', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'f',
    'kh', 'ts', 'ch', 'sh', 'shch', '''', 'y', '''', 'e', 'yu', 'ya',
    'A', 'B', 'V', 'G', 'D', 'E', 'Yo', 'Zh', 'Z', 'I', 'Y',
    'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U', 'F',
    'Kh', 'Ts', 'Ch', 'Sh', 'Shch', '''', 'Y', '''', 'E', 'Yu', 'Ya');
var
  i, p: Integer;
  Ch: String;
begin
  result := '';
  for i := 1 to Utf8Length(str) do
  begin
    Ch := Utf8Copy(str, i, 1);
    p := Utf8Pos(Ch, RArray);
    if p <> 0 then
      result := result + arr[p]
    else
      result := result + Ch;
  end;
end;    }

function CutStr(var S: String; D: Char): String;
var
  p: SizeInt;
begin
  p := Pos(D, S);
  if p = 0 then
  begin
    Result := S;
    S := '';
  end
  else
  begin
    Result := Copy(S, 1, p - 1);
    Delete(S, 1, p);
  end;
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
    if E <> nil then
      Result := E.Calc;
  finally
    EB.Free;
    FreeAndNil(E);
  end;
end;

function VarTypeToStr(V: Variant): String;
begin
  Result := '';
  if VarIsStr(V) then Result := rsText
  else if VarIsBool(V) then Result := rsBoolean
  else if VarType(V) = varDate then Result := rsDate + '/' + rsTime
  else if VarIsNumeric(V) then Result := rsNumber;
end;

function GetComponentDataTypeStr(C: TComponent): String;
begin
  if (C is TdxEdit) or (C is TdxMemo) or (C is TdxFile) or (C is TdxComboBox) or
    (C is TdxFile) or (C is TdxDBImage) then Result := rsText
  else if (C is TdxCalcEdit) or (C is TdxCheckBox) or (C is TdxCounter) or
    (C is TdxLookupComboBox) or (C is TdxRecordId) then Result := rsNumber
  else if C is TdxDateEdit then Result := rsDate
  else if C is TdxTimeEdit then Result := rsTime
  else Result := rsUnknown;
end;

{TdxConnection : Commit :
-unsuccessful metadata update
-new record size of 312292 bytes is too big
-TABLE T17}
function NewRecordSizeTooBigToMsg(const S: String): String;
var
  p0, p, Bytes, FmId: Integer;
  Fm: TdxForm;
begin
  Result := S;
  p0 := Pos(' size of ', S) + Length(' size of ');
  p := PosEx(' bytes', S, p0);
  Bytes := StrToInt(Copy(S, p0, p - p0));
  p0 := PosEx('TABLE T', S, p) + Length('TABLE T');
  if TryStrToInt(Copy(S, p0, 255), FmId) then
  begin
    Fm := FormMan.FindForm(FmId);
    if Fm <> nil then
      Result := Format(rsLimit64KMsg, [Fm.FormCaption, Bytes, Bytes - 65535,
        (Bytes - 65535) / 4]);
  end;
end;

function TooManyVersionsToMsg(const S: String): String;
var
  p0, p, FmId: Integer;
  Fm: TdxForm;
begin
  Result := S;
  p0 := Pos('TABLE T', S) + Length('TABLE T');
  p := PosEx(LineEnding, S, p0);
  if TryStrToInt(Copy(S, p0, p - p0), FmId) then
  begin
    Fm := FormMan.FindForm(FmId);
    if Fm <> nil then
      Result := Format(rsTableChangesLimitMsg, [Fm.FormCaption]);
  end;
end;

function ExceptionToString(E: Exception; TopSpace, BottomSpace: Boolean
  ): String;
var
  S, Msg: String;
begin
  if ScriptLastError.ExObj = E then
    Result := ScriptLastErrorToString
  else if E is EPSException then
    Result := EPSExceptionToString(EPSException(E))
  else if E is EDataBaseError then
  begin
    //if E is EIBDataBaseError then
   //   Debug(EIBDatabaseError(E).ErrorCode);
    S := WinCPToUtf8(E.Message);
    if Pos('Unable to complete network', S) > 0 then
      Msg := rsDatabaseConnectLost
    else if Pos('Error writing data to the connection', S) > 0 then
      Msg := rsDatabaseConnectLost
    else if Pos('block size exceeds', S) > 0 then
      Msg := rsRecSizeLimit
    else if Pos('new record size', S) > 0 then
      Msg := NewRecordSizeTooBigToMsg(S)
    else if Pos('too many versions', S) > 0 then
      Msg := TooManyVersionsToMsg(S)
    //else if Pos('Error while trying to open file', S) > 0 then
    //  Msg := rsDBInRemoteMode
    else if Pos('index root page is full', S) > 0 then
      Msg := rsTooManyIndex
    else if Pos('is not a valid database', S) > 0 then
      Msg := rsFileNotValidDatabase
    else
      Msg := '';

    Result := Format(rsExceptionClass, [E.ClassName]) + LineEnding +
      Format(rsErrorMessage, [S]);
    if Msg <> '' then
      Result := Msg + Spaces + rsErrorDetails + Spaces + Result;
  end
  else
    Result := Format(rsExceptionClass, [E.ClassName]) + LineEnding +
      Format(rsErrorMessage, [E.Message]);
  if TopSpace then Result := LineEnding + LineEnding + Result;
  if BottomSpace then Result := Result + LineEnding + LineEnding;
end;

function AroundSpaces(const S: String): String;
begin
  Result := LineEnding + LineEnding + S + LineEnding + LineEnding;
end;

function Spaces: String;
begin
  Result := LineEnding + LineEnding;
end;

function GetFormRealBounds(AForm: TCustomForm): TRect;
begin
  if AForm.WindowState = wsMaximized then
    GetWindowNormalPosition(AForm, Result)
  else
    Result := AForm.BoundsRect;
end;

procedure CorrectFormPos(APopupParent: TWinControl; AForm: TCustomForm);
var
  WR, BR: TRect;
  Dt: LongInt;
  M: TMonitor;
begin
  M := Screen.MonitorFromWindow(APopupParent.Handle);
  if M <> nil then
    WR := M.WorkareaRect
  else
    WR := Screen.WorkAreaRect;

  if AForm.Width <= 0 then AForm.Width := 50;
  if AForm.Height <= 0 then AForm.Height := 50;

  BR := AForm.BoundsRect;
  if WR.Contains(BR) then Exit;

  if BR.Right > WR.Right then
  begin
    Dt := BR.Right - WR.Right;
    BR.Right := BR.Right - Dt;
    BR.Left := BR.Left - Dt;
  end;
  if BR.Left < WR.Left then
  begin
    Dt := WR.Left - BR.Left;
    BR.Left := WR.Left;
    BR.Right := BR.Right + Dt;
  end;
  if BR.Bottom > WR.Bottom then
  begin
    Dt := BR.Bottom - WR.Bottom;
    BR.Bottom := BR.Bottom - Dt;
    BR.Top := BR.Top - Dt;
  end;
  if BR.Top < WR.Top then
  begin
    Dt := WR.Top - BR.Top;
    BR.Top := WR.Top;
    BR.Bottom := BR.Bottom + Dt;
  end;
  if BR.Width > WR.Width then BR.Width := WR.Width;
  if BR.Height > WR.Height then BR.Height := WR.Height;
  AForm.BoundsRect := BR;
end;

procedure SetFormPropPosition(AForm: TCustomForm);
begin
  with AForm do
    if (Left = -1) and (Top = -1) then
    else Position := poDesigned;
end;

procedure GetLowPropInfo(Obj: TObject; PropName: String; out LowObj: TObject; out PropInfo: PPropInfo);
var
  p, p2: SizeInt;
  S: String;
  Idx: LongInt;
begin
  LowObj := nil;
  PropInfo := nil;
  while True do
  begin
    p := Pos('.', PropName);
    if p = 0 then Break;

    S := Copy(PropName, 1, p - 1);
    Delete(PropName, 1, p);
    p := Pos('[', S);
    if p > 0 then
    begin
      p2 := Pos(']', S);
      Idx := StrToInt(Copy(S, p + 1, p2 - p - 1));
      Delete(S, p, p2 - p + 1);
    end
    else Idx := -1;
    Obj := GetObjectProp(Obj, S);
    if Idx >= 0 then
    begin
      if Obj is TCollection then
        Obj := TCollection(Obj).Items[Idx];
    end;
  end;
  PropInfo := GetPropInfo(Obj, PropName);
  LowObj := Obj;
end;

function TryStrToColor(const ColorStr: String; out Color: TColor): Boolean;
begin
  if ColorStr = '' then
  begin
    Color := clNone;
    Exit(True);
  end;
  try
    Color := StringToColor(ColorStr);
    Result := True;
  except
    Result := False;
  end;
end;

function ReplVertLine(const S: String): String;
begin
  Result := StringReplace(S, '|', '\', [rfReplaceAll]);
end;

function ToHtml(const S: String): String;
begin
  Result := '<html><head><meta content="text/html;charset=UTF-8" http-equiv="Content-Type">' +
    '</head><body bgcolor=#fff8dc>' + S + '</body></html>';
end;

procedure SplitComponentName(const AName: String; out ANameStr: String;
  out ANameNum: Integer);
var
  L, p: Integer;
begin
  L := Length(AName);
  p := L;
  while (p > 1) do
  begin
    if not (AName[p] in ['0'..'9']) then Break;
    Dec(p);
  end;
  ANameStr := Copy(AName, 1, p);
  if (p < L) and TryStrToInt(Copy(AName, p + 1, L), ANameNum) then
  else ANameNum := 0;
end;

function RenameObjectToString(RenameObject: TRenameObject): String;
begin
  case RenameObject of
    renForm: Result := rsForm;
    renField, renRpField: Result := rsField;
    renObject: Result := rsObject;
    renComponent: Result := rsComponent;
    renQuery: Result := rsQuery;
    renReport: Result := rsReport;
  end;
end;

procedure RenameInActions(CurObj: TObject; RenameObject: TRenameObject;
  const OldName, NewName: String);
var
  i, j: Integer;
  Fm, CurFm: TdxForm;
  AR: TActionRunner;
  Xml: String;
  RD: TReportData;
  C: TComponent;
  Bn: TdxButton;
  RenameOk: Boolean;

  procedure ProcessLines(Lines: TActionLines);
  var
    j: Integer;
    Line: TActionLine;
    A: TBaseAction;
    Ok: Boolean;
  begin
    Ok := False;
    for j := 0 to Lines.Count - 1 do
    begin
      Line := Lines[j];
      if Line.Kind <> alkAction then
        ProcessLines(Line.Lines)
      else
      begin
        A := Line.Action;
        case RenameObject of
          renForm: Ok := A.RenameForm(OldName, NewName);
          renField: Ok := A.RenameField(Fm, CurFm.FormCaption, OldName, NewName);
          renComponent: Ok := A.RenameComponent(Fm, CurFm.FormCaption, OldName, NewName);
          renQuery: Ok := A.RenameQuery(OldName, NewName);
          renReport: Ok := A.RenameReport(OldName, NewName);
          renRpField: Ok := A.RenameRpField(RD, OldName, NewName);
          renImage: Ok := A.RenameImage(OldName, NewName);
        end;
      end;
    end;
    if Ok then RenameOk := True;
  end;

  procedure ProcessRename(var Xml: String);
  begin
    if Xml = '' then Exit;
    AR.Load(Xml);
    ProcessLines(AR.Lines);
    AR.Save(Xml);
  end;

begin
  if CurObj is TdxForm then CurFm := TdxForm(CurObj)
  else if CurObj is TReportData then RD := TReportData(CurObj);

  AR := TActionRunner.Create;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    RenameOk := False;

    Fm := FormMan.Forms[i];
    Xml := Fm.ActionOnCreate;
    ProcessRename(Xml);
    Fm.ActionOnCreate := Xml;

    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxButton then
      begin
        Bn := TdxButton(C);
        Xml := Bn.ActionOnClick;
        ProcessRename(Xml);
        Bn.ActionOnClick := Xml;
      end;
    end;

    if RenameOk then Fm.SetFormChanged;
  end;
  Fm := nil;
  Xml := DXMain.Actions;
  ProcessRename(Xml);
  DXMain.Actions := Xml;
  AR.Free;
end;

procedure RenameImagesInForm(Fm: TdxForm; const OldName, NewName: String);
var
  j: Integer;
  C: TComponent;
  S: String;
begin
  for j := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[j];
    if C is TdxButton then
    begin
      S := TdxButton(C).ImageName;
      if MyUtf8CompareText(S, OldName) = 0 then
      begin
        if NewName <> '' then
          TdxButton(C).RenameImage(NewName)
        else
          TdxButton(C).ImageName := '';
        Fm.SetFormChanged;
      end;
    end
    else if C is TdxImage then
    begin
      S := TdxImage(C).ImageName;
      if MyUtf8CompareText(S, OldName) = 0 then
      begin
        if NewName <> '' then
          TdxImage(C).RenameImage(NewName)
        else
          TdxImage(C).ImageName := '';
        Fm.SetFormChanged;
      end;
    end;
  end;
end;

procedure RenameImages(const OldName, NewName: String);
var
  i: Integer;
begin
  for i := 0 to FormMan.FormCount - 1 do
    RenameImagesInForm(FormMan.Forms[i], OldName, NewName);
end;

function InnerCheckExistInActions(CurObj: TObject; RenameObject: TRenameObject;
  const aName: String; out Msg: String): Boolean;
var
  i, j: Integer;
  Fm, CurFm: TdxForm;
  AR: TActionRunner;
  S: String;
  RD: TReportData;
  C: TComponent;

  procedure ProcessLines(Lines: TActionLines; const BnName: String);
  var
    j: Integer;
    Exists: Boolean;
    Line: TActionLine;
    A: TBaseAction;
  begin
    for j := 0 to Lines.Count - 1 do
    begin
      Line := Lines[j];
      if Line.Kind <> alkAction then
        ProcessLines(Line.Lines, BnName)
      else
      begin
        Exists := False;

        A := Line.Action;
        case RenameObject of
          renForm: if (Fm = nil) or (Fm.FormCaption <> aName) then Exists := A.FormExists(aName);
          renField: Exists := A.FieldExists(Fm, CurFm.FormCaption, aName);
          renObject: Exists := A.ObjectExists(Fm, CurFm.FormCaption, aName);
          renComponent: Exists := A.ComponentExists(Fm, CurFm.FormCaption, aName);
          renQuery: Exists := A.QueryExists(aName);
          renReport: Exists := A.ReportExists(aName);
          renRpField: Exists := A.RpFieldExists(RD, aName);
          renImage: Exists := A.ImageExists(aName);
        end;
        if Exists then
        begin
          if Fm <> nil then
          begin
            S := S + Fm.FormCaption + '->';
            if BnName <> '' then S := S + BnName + '->';
          end
          else S := S + rsStartupActions + '->';
          S := S + A.ActionName + LineEnding;
        end;
      end;
    end;
  end;

  procedure ProcessCheckExists(const Xml, BnName: String);
  begin
    if Xml = '' then Exit;
    AR.Load(Xml);
    ProcessLines(AR.Lines, BnName);
  end;

begin
  S := '';
  if CurObj is TdxForm then CurFm := TdxForm(CurObj)
  else if CurObj is TReportData then RD := TReportData(CurObj);

  AR := TActionRunner.Create;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    ProcessCheckExists(Fm.ActionOnCreate, '');

    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxButton then
        ProcessCheckExists(TdxButton(C).ActionOnClick, GetComponentName(C));
    end;
  end;
  Fm := nil;
  ProcessCheckExists(DXMain.Actions, '');

  AR.Free;
  Result := S <> '';
  Msg := S;
end;

function CheckExistsInActions(CurObj: TObject; RenameObject: TRenameObject;
  const aName: String; const ExtraMsg: String): Boolean;
var
  S: String;
begin
  Result := InnerCheckExistInActions(CurObj, RenameObject, aName, S);
  if Result then
    Info(Format(rsCmpUsedInActions,
      [RenameObjectToString(RenameObject), aName, Spaces + S, ExtraMsg]));
end;

function CheckImageExistsInActions(const aName: String; out Msg: String
  ): Boolean;
begin
  Result := InnerCheckExistInActions(nil, renImage, aName, Msg);
end;

procedure UpdateImagesInForm(Fm: TdxForm);
var
  j: Integer;
  C: TComponent;
  S: String;
begin
  for j := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[j];
    if (C is TdxButton) or (C is TdxImage) then
    begin
      S := GetImageName(C);
      if S <> '' then
      begin
        SetImageName(C, '');
        SetImageName(C, S);
      end;
    end;
  end;
end;

procedure ReadFile(const FileName: String; out Buf: String);
begin
  with TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone) do
  try
    SetLength(Buf, Size);
    Read(Pointer(Buf)^, Size);
  finally
    Free;
  end;
end;

procedure TrimLineEnding(var S: String);
begin
  SetLength(S, Length(S) - Length(LineEnding));
end;

function GetFullCaption(Fm: TdxForm): String;
begin
  Result := Fm.FormCaption;
  if Fm.RecordsCaption <> '' then
    Result := Result + ' (' + Fm.RecordsCaption + ')';
end;

function RemoveNonPrintableChars(const S: String; KeepNewLine: Boolean): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if S[i] >= ' ' then Result := Result + S[i]
    else if (S[i] in [#10, #13]) and KeepNewLine then Result := Result + S[i];
end;

procedure SaveString(const FileName: String; const S: String);
begin
  with TFileStream.Create(FileName, fmCreate) do
  try
    Size := 0;
    Write(Pointer(S)^, Length(S));
  finally
    Free;
  end;
end;

function LoadString(const FileName: String): String;
begin
  Result := '';
  with TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone) do
  try
    SetLength(Result, Size);
    Read(Pointer(Result)^, Size);
  finally
    Free;
  end;
end;

function ReplacePathDelimiters(const Path: String): String;
var
  i: Integer;
begin
  SetLength(Result, Length(Path));
  for i := 1 to Length(Path) do
    if Path[i] in [#33..#47, #58..#64, #91..#96, #123..#126] then
      Result[i] := '_'
    else
      Result[i] := Path[i];
end;

procedure GetDirectoryInfo(const ADir: String; out ALastModified: LongInt; out ADirSize: Int64);
var
  Inf: TRawByteSearchRec;
  LastModif: LongInt;
  Sz: Int64;
  VerFile: String;
begin
  ALastModified := 0; ADirSize := 0;
  VerFile := FormatDateTime('yyyymmdd', GetBuildDate);
  if FindFirst(ADir + GetAllFilesMask, faAnyFile, Inf) <> 0 then Exit;
  repeat
    if (Inf.Name = '.') or (Inf.Name = '..') or (Inf.Name = '') then Continue;
    if (Inf.Attr and faDirectory) > 0 then
    begin
      GetDirectoryInfo(ADir + Inf.Name + PathDelim, LastModif, Sz);
      if Inf.Time > LastModif then LastModif := Inf.Time;
    end
    else
    begin
      // Игнорируем время модификации файла версии, потому что сервер может находится в другом часовом поясе.
      if Inf.Name <> VerFile then
        LastModif := Inf.Time
      else
        LastModif := 0;
      Sz := Inf.Size;
    end;
    if LastModif > ALastModified then ALastModified := LastModif;
    ADirSize := ADirSize + Sz;
  until FindNext(Inf) <> 0;
  FindClose(Inf);
end;

function GetHashNum(Value: Int64): String;
begin
  Result := EncodeMD5(IntToStr(Value) + ';kljsdfpoiujwerы валоыв аф');
end;

function DirAge(const ADir: String): LongInt;
var
  Inf: TRawByteSearchRec;
begin
  Result := 0;
  if FindFirst(ExcludeTrailingPathDelimiter(ADir), faDirectory, Inf) = 0 then
  begin
    Result := Inf.Time;
    FindClose(Inf);
  end;
end;

procedure CreateMetaVersionFile(const ADir: String);
const
  MD5Len = 32;
var
  Nm, S: String;
  LastModif: LongInt;
  DirSz: Int64;
  FS: TFileStream;
begin
  Nm := ADir + FormatDateTime('yyyymmdd', GetBuildDate);
  FS := TFileStream.Create(Nm, fmCreate);
  FS.Free;
  GetDirectoryInfo(ADir, LastModif, DirSz);
  S := GetHashNum(DirAge(ADir) + DirSz + MD5Len);
  SaveString(Nm, S);
  FileSetDate(Nm, DateTimeToFileDate(DXMain.LastModified));
end;

procedure LoadMetaFromCache;
var
  TmpDir: String;
begin
  TmpDir := GetTempDir + ReplacePathDelimiters(DBase.Database) + '.tmp' + PathDelim;
  if not ForceDirectories(TmpDir) then
    raise Exception.CreateFmt('Unable to create cache directory %s', [TmpDir]);

  ImageMan.LoadFromCache(TmpDir);
  FormMan.LoadFromCache(TmpDir);
  ReportMan.LoadFromCache(TmpDir);
  ScriptMan.LoadFromCache(TmpDir);
end;

function CanCache: Boolean;
begin
  Result := AppConfig.Caching and DBase.IsRemote;
end;

function GetTempDirName: String;
begin
  Result := GetTempDir + 'temp' + IntToStr(Random(100000)) + PathDelim;
end;

procedure ShowMsgForm(const AMsg, ADetails: String);
begin
  MsgFm.ShowForm(AMsg, ADetails);
end;

function IsDeveloper: Boolean;
begin
  Result := (UserMan.CurrentUser = nil) or (UserMan.CurrentUser.RoleId < 0);
end;

function IsEmptyApp: Boolean;
begin
  Result := (MainFr = nil) and (DesignFr = nil);
end;

function GetBuildDate: TDateTime;
var
  FS: TFormatSettings;
  S: String;
begin
  FS.DateSeparator := '/';
  FS.ShortDateFormat := 'y/m/d';
  S := {$INCLUDE %DATE};
  Result := StrToDate(S, FS);
end;

function BuildDateToStr: String;
begin
  Result := FormatDateTime('yy.m.d', GetBuildDate);
end;

function CreateGUIDString: String;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
  Delete(Result, 1, 1);
  SetLength(Result, Length(Result) - 1);
end;

function GetListSourceField(C: TComponent): TComponent;
var
  Fm: TdxForm;
begin
  Result := nil;
  if (GetSourceTId(C) = 0) or (GetSourceFId(C) = 0) then Exit;
  Fm := FormMan.FindForm(GetSourceTId(C));
  Result := FindById(Fm, GetSourceFId(C));
end;

function IsRemoteDatabase(const DBName: String): Boolean;
begin
  Result := Pos(':', DBName) > 2;
end;

function ConstructDBOpenFilter(IsOpenDialog: Boolean): String;
begin
  Result := '';
  if AppConfig.SupportDXDB then
  begin
    if IsOpenDialog then
      Result := Result + rsDialogFilterAllBases;
    Result := Result + rsDialogFilterDXDB;
  end;
  Result := Result + rsDialogFilter;
end;

function GetDefaultDBExt: String;
begin
  Result := IIF(AppConfig.SupportDXDB, 'dxdb', 'fdb');
end;

procedure LogString(const Msg, Context: String);
begin
  if (Context <> '') and (Msg <> '') then
    MyLog.WriteToFile(Context + ': ' + Msg)
  else if Context <> '' then
    MyLog.WriteToFile(Context)
  else
    MyLog.WriteToFile(Msg)
end;

function IsNumericComponent(C: TComponent): Boolean;
begin
  Result := (C is TdxCalcEdit) or (C is TdxCounter) or (C is TdxRecordId) or
    (C is TdxLookupComboBox) or (C is TdxCheckBox) or ((C is TdxObjectField) and
      IsNumericComponent( GetObjectFieldField(TdxObjectField(C)) ));
end;

function GetFlName(C: TComponent): String;
begin
  if C is TdxLabel then Result := TdxLabel(C).FieldName
  else Result := GetFieldName(C);
end;

// Проходим по цепочке зависимых полей и ищем циклическую ссылку на текущее поле
procedure LoopDetect(const AExpression: String; ACmp: TComponent; AForm: TdxForm;
  ARD: TReportData; AOnlyPrefix: Boolean);
const
  ChainChars = '->';
var
  FieldsChain, S: String;
  CL: TList;
  EB: TExpressionBuilder;

  // Продвигаеся по цепочке полей и запросов. Если в итоге возвращаемся на
  // текущий компонент, значит есть зацикливание. Компоненты могут
  // ссылаться на самих себя и чтобы не было зацикливания добавлена
  // проверка на Level=0 и добавлен параметр CurC. OnlyPrefix - если
  // выражение является выражением выходного фильтра или вычисляемого поля,
  // где поля формы обозначаются префиксами.
  function _LoopDetect(CurC: TComponent; const Expr, Chain: String; Level: Integer; OnlyPrefix: Boolean): Boolean;
  var
    i, j: Integer;
    C: TComponent;
    E: TExpression;
    S, FlNm: String;
    RD: TReportData;
    b: Boolean;
  begin
    Result := False;
    if CurC <> nil then CL.Add(CurC);
    for i := 0 to AForm.ComponentCount - 1 do
    begin
      C := AForm.Components[i];
      // В выражении поля надписи не поддерживаются
      if (C is TdxLabel) and EB.SkipLabels then Continue
      // Бывает, что поле (или запрос) в цепочке ссылается на само себя и чтобы не было зацикливания
      // в проверке на зацикливание в функцию было добавлено это условие
      else if C = CurC then Continue
      // Т. к. проверка основывается на том, что при зацикливании мы вернемся
      // по цепочке на текущий же компонент, но в то же время компонент
      // может ссылаться на самого себя, мы допускаем совпадение компонентов
      // в начале цепочки.
      else if (C = ACmp) and (Level = 0) then Continue
      // Запрос
      else if C is TdxQueryGrid then
      begin
        if ACmp is TdxLabel then Continue;

        RD := ReportMan.FindReport(GetId(C));
        if FormExists(RD.Name, Expr) then
        begin
          if CL.IndexOf(C) >= 0 then
          begin
            FieldsChain := Chain + ChainChars + RD.Name;
            Exit(True);
          end;

          for j := 0 to RD.Sources.Count - 1 do
            if _LoopDetect(C, RD.Sources[j]^.Filter, Chain + ChainChars +
              Format(rsLoopDetectSourceFilter, [RD.Name, j+1]),
              Level + 1, False) then Exit(True);
          if _LoopDetect(C, RD.SQL, Chain + ChainChars +
            Format(rsLoopDetectSQL, [RD.Name]), Level + 1, True) then Exit(True);
          if _LoopDetect(C, RD.Filter, Chain + ChainChars +
            Format(rsLoopDetectOutputFilter, [RD.Name]),
            Level + 1, True) then Exit(True);
          for j := 0 to RD.CalcFields.Count - 1 do
            if _LoopDetect(C, RD.CalcFields[j]^.Expr, Chain + ChainChars +
              Format(rsLoopDetectCalcField, [RD.Name, RD.CalcFields[j]^.Name]),
              Level + 1, True) then Exit(True);
        end;
        Continue;
      end
      else if not HasExpression(C) then Continue;

      FlNm := GetFlName(C);

      if not OnlyPrefix then
        b := FieldExists(AForm.PId, FlNm, Expr)
      // В выражениях выходного фильтра и вычисляемых полях запроса поля
      // формы идут только с префиксом.
      else
        b := FieldExistsForQuery(FlNm, Expr);

      if b then
      begin
        // Мы вернулись по цепочке на текущий же компонент. Значит это зацикливание.
        if CL.IndexOf(C) >= 0 then
        begin
          FieldsChain := Chain + ChainChars + BrStr(FlNm);
          Exit(True);
        end;

        S := GetExpression(C);
        if Trim(S) <> '' then
        begin
          try
            E := EB.Build(S);
          except
            E := nil;
          end;
          if E <> nil then
          begin
            FreeAndNil(E);
            if _LoopDetect(C, S, Chain + ChainChars + BrStr(FlNm), Level + 1, False) then Exit(True);
          end;
        end;
      end;
    end;

    if CurC <> nil then CL.Remove(CurC);
  end;

begin
  EB := TExpressionBuilder.Create;
  EB.Form := AForm;
  if AForm <> nil then
    EB.ParentForm := FormMan.FindForm(AForm.PId);
  EB.SkipLabels := (ACmp <> nil) and not (ACmp is TdxLabel);
  EB.RD := ARD;

  CL := TList.Create;
  CL.Add(ACmp);
  S := GetComponentName(ACmp);
  if not (ACmp is TdxQueryGrid) then S := BrStr(S);
  if _LoopDetect(nil, AExpression, S, 0, AOnlyPrefix) then
  begin
    CL.Free;
    raise ELoopException.CreateFmt(rsLoopDetected, [FieldsChain]);
  end;
  CL.Free;
  EB.Free;
end;

function IsDesignerMode: Boolean;
begin
  Result := DesignFr <> nil;
end;

function GetComponentDisplayFormat(Fm: TdxForm; C: TComponent): String;
var
  ObjC: TComponent;
  SrcFm: TdxForm;
begin
  Result := '';
  if C is TdxObjectField then
  begin
    with TdxObjectField(C) do
      if (ObjId > 0) and (FieldId > 0) then
      begin
        ObjC := FindById(Fm, ObjId);
        SrcFm := FormMan.FindForm(GetSourceTId(ObjC));
        C := FindById(SrcFm, FieldId);
      end;
  end
  else if C is TdxLookupComboBox then
    with TdxLookupComboBox(C) do
      if (SourceTId > 0) and (SourceFId > 0) then
      begin
        SrcFm := FormMan.FindForm(SourceTId);
        C := FindById(SrcFm, SourceFId);
      end;

  if C is TdxCalcEdit then
    Result := TdxCalcEdit(C).PrecStr
  else if C is TdxTimeEdit then
    Result := TdxTimeEdit(C).TimeFormatStr;
end;

procedure SetDSFieldDisplayFormat(F: TField; Fmt: String);
var
  PInfo: PPropInfo;
begin
  PInfo := GetPropInfo(F, 'DisplayFormat');
  if PInfo <> nil then
    SetPropValue(F, PInfo, Fmt)
end;

procedure DeleteLCbxListSourceField(Fm: TdxForm; RDId: Integer;
  const FieldNameDS: String);
var
  i, j: Integer;
  C: TComponent;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxLookupComboBox then
    begin
      with TdxLookupComboBox(C) do
        if ListSource = RDId then
        begin
          if CompareText(ListKeyField, FieldNameDS) = 0 then
            ListKeyField := '';
          for j := ListFields.Count - 1 downto 0 do
          begin
            if CompareText(ListFields[j].FieldName, FieldNameDS) = 0 then
              ListFields.Delete(j);
          end;
        end;
    end;
  end;
end;

function IsTextComponent(C: TComponent): Boolean;
begin
  Result := (C is TdxMemo) or (C is TdxEdit) or (C is TdxComboBox) or
    (C is TdxFile);
end;

procedure DrawImageFieldIntoGrid(Grid: TDBGrid; Column: TColumn;
  ImageField: TField; R: TRect);

  procedure GetImagePos(Col: TMyDBGridColumn; Bmp: TBGRABitmap; out x, y: Integer);
  var
    al: TAlignment;
    lay: TTextLayout;
  begin
    al := Col.Alignment;
    lay := Col.Layout;
    case al of
      taLeftJustify: x := R.Left + ScaleToScreen(2);
      taCenter: x := R.Left + R.Width div 2 - Bmp.Width div 2;
      taRightJustify: x := R.Right - Bmp.Width - ScaleToScreen(2);
    end;
    case lay of
      tlTop: y := R.Top + ScaleToScreen(2);
      tlCenter: y := R.Top + R.Height div 2 - Bmp.Height div 2;
      tlBottom: y:= R.Bottom - Bmp.Height - ScaleToScreen(2);
    end;
  end;

var
  St: TStream;
  Bmp: TBGRABitmap;
  Col: TMyDBGridColumn;
  x, y: Integer;
begin
  Grid.Canvas.FillRect(R);

  Col := TMyDBGridColumn(Column);
  if (Col.ThumbSize = 0) or ImageField.IsNull then Exit;

  try

  St := ImageField.DataSet.CreateBlobStream(ImageField, bmRead);
  Bmp := TBGRABitmap.Create(0, 0);
  try
    if (St <> nil) and (St.Size > 0) then Bmp.LoadFromStream(St);
    GetImagePos(Col, Bmp, x, y);
    Bmp.Draw(Grid.Canvas, x, y, True);
  finally
    Bmp.Free;
    FreeAndNil(St);
  end;

  except
    ;
  end;
end;

procedure CalcQueryColor(RD: TReportData; Fm: TdxForm; RDS, DS: TDataSet;
  const TargetField: String; out FieldName: String; out Color: TColor);
var
  EB: TExpressionBuilder;
  i: Integer;
  E: TExpression;
  V: Variant;
  CD: TRpColoringData;
begin
  FieldName := ''; Color := clNone;
  EB := TExpressionBuilder.Create;
  EB.RD := RD;
  EB.RDSet := RDS;
  EB.Form := Fm;
  if (Fm <> nil) and (Fm.ParentForm <> nil) then
    EB.ParentForm := Fm.ParentForm
  else
    EB.ParentForm := Fm;
  EB.DataSet := DS;
  EB.SkipLabels := True;
  E := nil;
  try try
    for i := 0 to RD.Coloring.Count - 1 do
    begin
      CD := RD.Coloring[i];
      if CompareText(CD.FieldNameDS, TargetField) <> 0 then Continue;
      FreeAndNil(E);
      E := EB.Build(CD.Expr);
      if E <> nil then
      begin
        V := E.Calc;
        if VarIsBool(V) and (V = True) then
        begin
          Color := CD.Color;
          FieldName := CD.FieldNameDS;
          Exit;
        end;
      end;
    end;
  except
    Exit; // Глушим ошибку, чтобы программа не упала
  end;
  finally
    EB.Free;
    FreeAndNil(E);
  end;
end;

{procedure SetupImageList(IL: TCustomImageList; ResNames: array of String);
const
  Sfx: array of String = ('', '_150', '_200');
  Sz16: array of Integer = (16, 24, 32);
  Sz24: array of Integer = (24, 32, 48);
var
  i, n, Sz: Integer;
begin
  n := ImageMan.GetPPIndex;
  if IL.Width = 16 then Sz := Sz16[n]
  else if IL.Width = 24 then Sz := Sz24[n]
  else raise Exception.Create('SetupImageList: unknown width = ' + IntToStr(IL.Width));

  IL.Clear;
  IL.Scaled := False;
  IL.Width := Sz;
  IL.Height := Sz;
  for i := 0 to High(ResNames) do
    IL.AddLazarusResource(ResNames[i] + Sfx[n]);
end;}

function GetPPIndex: Integer;
var
  PPI, i: Integer;
begin
  PPI := Screen.PixelsPerInch;
  if PPI >= 192 then i := 2
  else if PPI >= 144 then i := 1
  else i := 0;
  Result := i;
end;

procedure SetupImageList(IL: TCustomImageList; ResNames: array of String);
const
  Sfx: array of String = ('', '_150', '_200');
  Mul: array of Double = (1, 1.5, 2);
var
  i, n: Integer;
begin
  n := GetPPIndex;

  IL.Clear;
  IL.Scaled := False;
  IL.Width := Trunc(IL.Width * Mul[n]);
  IL.Height := Trunc(IL.Height * Mul[n]);

  for i := 0 to High(ResNames) do
    IL.AddLazarusResource(ResNames[i] + Sfx[n]);
end;

procedure SetupPicture(Pic: TPicture; const ResName: String);
const
  Sfx: array of String = ('', '_150', '_200');
begin
  Pic.LoadFromLazarusResource(ResName + Sfx[GetPPIndex]);
end;

procedure SetupBitBtn(Bn: TCustomBitBtn; const ResName: String);
const
  Sfx: array of String = ('', '_150', '_200');
begin
  Bn.LoadGlyphFromLazarusResource(ResName + Sfx[GetPPIndex]);
end;

procedure SetupSpeedButton(Bn: TSpeedButton; const ResName: String);
const
  Sfx: array of String = ('', '_150', '_200');
begin
  Bn.LoadGlyphFromLazarusResource(ResName + Sfx[GetPPIndex]);
end;

function CreateBitmapFromRes(const ResName: String): TCustomBitmap;
const
  Sfx: array of String = ('', '_150', '_200');
begin
  Result := CreateBitmapFromLazarusResource(ResName + Sfx[GetPPIndex]);
end;

procedure ConvertToDXMainVersion2(AMain, AFmMan: TObject);
var
  SL: TStringList;
  i: Integer;
  Fm: TdxForm;
  G: TFormGroup;
  Main: TDXMain;
  FmMan: TFormManager;
begin
  Main := TDXMain(AMain);
  if Main.Version = 2 then Exit;
  FmMan := TFormManager(AFmMan);

  SL := TStringList.Create;
  FmMan.FormsToList(SL);
  for i := 0 to SL.Count - 1 do
  begin
    Fm := TdxForm(SL.Objects[i]);
    G := Main.Groups.FindGroup(Fm.FormGroup);
    if G = nil then
    begin
      G := Main.Groups.AddGroup;
      G.Name := Fm.FormGroup;
    end;
    G.IdList.AddValue(Fm.Id);
  end;

  FmMan.SortFormsByIndex(SL);
  for i := 0 to SL.Count - 1 do
  begin
    Fm := TdxForm(SL.Objects[i]);
    if Fm.AutoOpen then
      Main.Tabs.AddValue(Fm.Id);
  end;
  SL.Free;
end;

function GetDefaultDateTime(DT: TDateTime): TDateTime;
begin
  if DT <> 0 then Result := DT
  else Result := EncodeDate(2024, 1, 1);
end;

procedure SetFileDateTime(const FlNm: String; DT: TDateTime);
begin
  FileSetDate(FlNm, DateTimeToFileDate( GetDefaultDateTime(DT) ));
end;

procedure SetFileDateTime(Handle: THandle; DT: TDateTime);
begin
  FileSetDate(Handle, DateTimeToFileDate( GetDefaultDateTime(DT) ));
end;

function GetFileDateTime(const FlNm: String): TDateTime;
begin
  Result := FileDateToDateTime(FileAge(FlNm));
end;

function SameFileDateTime(const FlNm: String; ATime: TDateTime): Boolean;
begin
  Result := FileAge(FlNm) = DateTimeToFileDate(  GetDefaultDateTime(ATime) );
  {if not Result then
  begin
    Debug(FlNm);
    Debug(FileAge(FlNm));
    Debug(DateTimeToFileDate(  GetDefaultDateTime(ATime) ));
  end;}
end;

// Немного измененный код из EditBtn.pas
// Tries to parse string when DateOrder = doNone when string maybe contains
// literal day or monthnames. For example when ShortDateFormat = 'dd-mmm-yyy'
// Returns NullDate upon failure.
function ParseDateNoPredefinedOrder(SDate: String; FS: TFormatSettings; out ResDate: TDateTime): Boolean;
var
  Fmt: String;
  DPos, MPos, YPos: SizeInt;
  DStr, MStr, YStr: String;
  LD, LM, LY: LongInt;
  DD, MM, YY: Word;
const
  Digits = ['0'..'9'];

  procedure GetPositions(out DPos, MPos, YPos: SizeInt);
  begin
    DStr := '';
    MStr := '';
    YStr := '';
    DPos := Pos('D', Fmt);
    MPos := Pos('M', Fmt);
    YPos := Pos('Y', Fmt);
    if (YPos = 0) or (MPos = 0) or (DPos = 0) then Exit;
    if (YPos > DPos) then YPos := 3 else YPos := 1;
    if (DPos < MPos) then
    begin
      if (YPos = 3) then
      begin
        DPos := 1;
        MPos := 2;
      end
      else
      begin
        DPos := 2;
        MPos := 3;
      end;
    end
    else
    begin
      if (YPos = 3) then
      begin
        DPos := 2;
        MPos := 1;
      end
      else
      begin
        DPos := 3;
        MPos := 2;
      end;
    end;
  end;

  procedure ReplaceLiterals;
  var
    i, P: Integer;
    Sub: String;
  begin
    if (Pos('MMMM',Fmt) > 0) then
    begin //long monthnames
      //writeln('Literal monthnames');
      for i := 1 to 12 do
      begin
        Sub := FS.LongMonthNames[i];
        P := Pos(Sub, SDate);
        if (P > 0) then
        begin
          Delete(SDate, P, Length(Sub));
          Insert(IntToStr(i), SDate, P);
          Break;
        end;
      end;
    end
    else
    begin
      if (Pos('MMM',Fmt) > 0) then
      begin //short monthnames
        for i := 1 to 12 do
        begin
          Sub := FS.ShortMonthNames[i];
          P := Pos(Sub, SDate);
          if (P > 0) then
          begin
            Delete(SDate, P, Length(Sub));
            Insert(IntToStr(i), SDate, P);
            Break;
          end;
        end;
      end;
    end;

    if (Pos('DDDD',Fmt) > 0) then
    begin  //long daynames
      //writeln('Literal daynames');
      for i := 1 to 7 do
      begin
        Sub := FS.LongDayNames[i];
        P := Pos(Sub, SDate);
        if (P > 0) then
        begin
          Delete(SDate, P, Length(Sub));
          Break;
        end;
      end;
    end
    else
    begin
      if (Pos('DDD',Fmt) > 0) then
      begin //short daynames
        for i := 1 to 7 do
        begin
          Sub := FS.ShortDayNames[i];
          P := Pos(Sub, SDate);
          if (P > 0) then
          begin
            Delete(SDate, P, Length(Sub));
            Break;
          end;
        end;
      end;
    end;
    SDate := Trim(SDate);
    //writeln('ReplaceLiterals -> ',SDate);
  end;

  procedure Split(out DStr, MStr, YStr: String);
  var
    i, P: Integer;
    Sep: Set of Char;
    Sub: String;
  begin
    DStr := '';
    MStr := '';
    YStr := '';
    Sep := [];
    for i :=  1 to Length(Fmt) do
      if not (Fmt[i] in Digits) then Sep := Sep + [Fmt[i]];
    //get fist part
    P := 1;
    while (P <= Length(SDate)) and (SDate[P] in Digits) do Inc(P);
    Sub := Copy(SDate, 1, P-1);
    Delete(SDate, 1, P);
    if (DPos = 1) then DStr := Sub else if (MPos = 1) then MStr := Sub else YStr := Sub;
    //get second part
    if (SDate = '') then Exit;
    while (Length(SDate) > 0) and (SDate[1] in Sep) do Delete(SDate, 1, 1);
    if (SDate = '') then Exit;
    P := 1;
    while (P <= Length(SDate)) and (SDate[P] in Digits) do Inc(P);
    Sub := Copy(SDate, 1, P-1);
    Delete(SDate, 1, P);
    if (DPos = 2) then DStr := Sub else if (MPos = 2) then MStr := Sub else YStr := Sub;
    //get thirdpart
    if (SDate = '') then Exit;
    while (Length(SDate) > 0) and (SDate[1] in Sep) do Delete(SDate, 1, 1);
    if (SDate = '') then Exit;
    Sub := SDate;
    if (DPos = 3) then DStr := Sub else if (MPos = 3) then MStr := Sub else YStr := Sub;
  end;

  procedure AdjustYear(var YY: Word);
  var
    CY, CM, CD: Word;
  begin
    DecodeDate(Date, CY, CM, CD);
    LY := CY Mod 100;
    CY := CY - LY;
    if ((YY - LY) <= 50) then
      YY := CY + YY
    else
      YY := CY + YY - 100;
  end;

begin
  Result := False;
  if (Length(SDate) < 5) then Exit; //y-m-d is minimum we support
  Fmt := UpperCase(FS.ShortDateFormat); //only care about y,m,d so this will do
  GetPositions(DPos, MPos, YPos);
  ReplaceLiterals;
  if (not (SDate[1] in Digits)) or (not (SDate[Length(SDate)] in Digits)) then Exit;
  Split(Dstr, MStr, YStr);
  if not TryStrToInt(DStr, LD) or
     not TryStrToInt(Mstr, LM) or
     not TryStrToInt(YStr, LY) then Exit;
  DD := LD;
  MM := LM;
  YY := LY;
  if (YY < 100) and (Pos('YYYY', UpperCase(Fmt)) = 0) then
  begin
    AdjustYear(YY);
  end;
  Result := TryEncodeDate(YY, MM, DD, ResDate);
end;

procedure FixDateSeparator(var aText: String; FS: TFormatSettings);
var
  i: Integer;
begin
  for i := 2 to Length(aText) - 1 do
  begin
    if (aText[i] = #32) and (aText[i - 1] in ['0'..'9']) and (aText[i + 1] in ['0'..'9']) then
      aText[i] := FS.DateSeparator;
  end;
end;

function TryTextToDate(AText: String; out ResDate: TDateTime): Boolean;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FixDateSeparator(AText, FS);
  Result := TryStrToDate(AText, ResDate, FS);
  if not Result then
    Result := ParseDateNoPredefinedOrder(AText, FS, ResDate);
end;

function TextToDate(AText: String): TDateTime;
begin
  if not TryTextToDate(AText, Result) then
    raise EConvertError.CreateFmt(rsInvalidDate, [AText]);
end;

end.

