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
unit AppUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strconsts, Menus, DBGrids, dxctrls, DXReports, Lists, Db,
  Controls, Graphics, StdCtrls, Forms, Process, LclIntf;

type
  EAssertError = class(Exception);

procedure TestNil(P: Pointer; const Msg: String);
procedure DebugStr(const S: String);
function AppPath: String;
procedure ErrMsg(const Msg: String);
procedure ErrMsgFmt(const Msg: String; Params: array of const);
procedure ClearList(L: TList);
function ConfirmDelete: Boolean;
procedure SetMenuItemImage(MI: TMenuItem; const ResName: String);
function CreateMenuItem(aMenu: TMenu; const Caption: String; Tag: PtrInt; aShortCut: TShortCut;
  Handler: TNotifyEvent; const ResName: String = ''): TMenuItem;
function SortColumnToId(Gr: TdxGrid): Integer;
function IdToColumn(Gr: TdxGrid; Id: Integer): TColumn;
procedure OpenFile(const FileName: String);
procedure SplitStr(const S: String; D: Char; SL: TStrings);
procedure SplitStr2(const S, D: String; SL: TStrings);
function GetOutputDir: String;
function GetAbsolutePath(const Path: String): String;
function CopyToStorageFolder(const Src, Dir, Dest: String): String;
function OpenPictureDialog(OnlyPNG: Boolean = False): String;
function SavePictureDialog(DefaultFileName: String; OnlyPNG: Boolean = False): String;
function SaveFileDialog(const aTitle: String; DefaultFileName: String): String;
procedure ShowImages(Fm: TdxForm);
function IsUrl(const S: String): Boolean;
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
function GetObjFieldValue(Obj: TObject; aKey: Integer; FullPath: Boolean): String;
function GetObjFieldKey(Obj: TdxLookupComboBox; aValue: String): Variant;
procedure ClearObjectFieldId(Fm: TdxForm; OId, FId: Integer);
function ShiftColor(Color: TColor; N: Integer): TColor;
function FindGridById(Fm: TdxForm; aId: Integer): TdxGrid;
function CheckName(const S: String): Boolean;
function CheckName2(const S: String): Boolean;
function CheckType(C: TComponent; var Value: String): Boolean;
function FindGridColumn(G: TdxGrid; Id: Integer): TColumn;
function CheckComponentNames(Fm: TdxForm): Boolean;
function StrToXml(S: String): String;
function XmlToStr(S: String): String;
procedure SetDSEdit(DS: TDataSet);
function CheckDeleteForm(Fm: TdxForm): Boolean;
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
procedure DisableDataSetScroll(DS: TDataSet; var BeforeScroll, AfterScroll: TDataSetNotifyEvent;
  var B: TBookmark; var State: TDataSetState);
procedure EnableDataSetScroll(DS: TDataSet; BeforeScroll, AfterScroll: TDataSetNotifyEvent;
  var B: TBookmark; State: TDataSetState);
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
procedure Debug(Value: Variant);
function Confirm(const Caption, Msg: String): TModalResult;
procedure Info(const Msg: String);
function CheckDuplicateLabel(ALabel: TdxLabel; const ACaption: String): Boolean;
function CheckDuplicateLabel2(ALabel: TdxLabel; const ACaption: String): Boolean;
function CalcLabelWithSameCaptionExists(ALabel: TdxLabel): Boolean;
//function GetHelpDir: String;
//function GetLangDir: String;

implementation

uses
  Dialogs, LazUtf8, {$ifdef windows}ShellApi,{$endif} appsettings, FileUtil, dximages,
  formmanager, expressions, dbengine, Math, sqlgen, dxusers, reportmanager,
  pivotgrid, Variants, maskedit, outputform, mytypes;

procedure TestNil(P: Pointer; const Msg: String);
begin
  if P = nil then raise EAssertError.Create(Msg);
end;

procedure DebugStr(const S: String);
var
  Fl: String;
  mode: Integer;
begin
  Fl := AppPath + 'debug.txt';
  if not FileExists(Fl) then mode := fmCreate
  else mode := fmOpenWrite;
  with TFileStream.Create(Fl, mode) do
    try
      Position := Size;
      WriteBuffer(Pointer(S)^, Length(S));
      {$ifdef windows}
      WriteBuffer(LineEnding[1], Length(LineEnding));
      {$else}
      WriteBuffer(Pointer(LineEnding)^, Length(LineEnding));
      {$endif}
    finally
      Free;
    end;
end;

function AppPath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

procedure ErrMsg(const Msg: String);
begin
  MessageDlg(rsError, Msg, mtError, [mbOk], 0);
end;

procedure ErrMsgFmt(const Msg: String; Params: array of const);
begin
  ErrMsg(Format(Msg, Params));
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

procedure SetMenuItemImage(MI: TMenuItem; const ResName: String);
var
  B: TCustomBitmap;
begin
  B := CreateBitmapFromLazarusResource(ResName);
  MI.Bitmap.Assign(B);
  B.Free;
end;

function CreateMenuItem(aMenu: TMenu; const Caption: String; Tag: PtrInt;
  aShortCut: TShortCut; Handler: TNotifyEvent; const ResName: String
  ): TMenuItem;
begin
  Result := TMenuItem.Create(aMenu);
  Result.Caption:=Caption;
  Result.Tag := Tag;
  Result.ShortCut:=aShortCut;
  Result.OnClick:=Handler;
  if ResName <> '' then SetMenuItemImage(Result, ResName);
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
    ErrMsg(Format(rsFileNotExists, [FileName]))
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
  Result := Path;
  if (Pos(':', Result) <> 2) and (Pos('/', Result) <> 1) then
    Result := AppPath + Result;
end;

function CopyToStorageFolder(const Src, Dir, Dest: String): String;
begin
  Result := '';
  if not ForceDirectories(Dir) then
    Exit(Format(rsCouldNotCreateFolder, [Dir]));
  if not CopyFile(Src, Dir + Dest) then
    Exit(Format(rsCantCopyFileToStorageFolder, [Dest, Dir + Dest]));
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
    DefaultExt := ExtractFileExt(FileName);
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

procedure ShowImages(Fm: TdxForm);
var
  C: TComponent;
  i: Integer;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxDBImage then
      TdxDBImage(C).ShowImage;
  end;
end;

function IsUrl(const S: String): Boolean;
begin
  Result := (Copy(S, 1, 7) = 'http://') or (Copy(S, 1, 8) = 'https://') or
    (Copy(S, 1, 4) = 'www.');
end;

function IsMail(var S: String): Boolean;
begin
  Result := Pos('@', S) > 0;
  if Result and (Pos('mailto:', S) = 0) then
    S := 'mailto:' + S;
end;

procedure OpenUrl(const S: String);
begin
  ShellExec('open', S, '', '', 1);
end;

function ReadToken(const S: String; var P: Integer; var Tk: Char): String;
var
  Len: Integer;
  W, Qt: String;

  procedure SkipWhites;
  begin
    while (P <= Len) and (S[P] in [#9, #10, #13, #32]) do
      Inc(P);
    // Если это комментарии
    if (P < Len) and (S[P] = '/') then
    begin
      if S[P + 1] = '/' then
      begin
        while (P <= Len) and (not (S[P] in [#10, #13])) do
          Inc(P);
        SkipWhites;
      end
      else if S[P + 1] = '*' then
      begin
        while (P < Len) and (Copy(S, P, 2) <> '*/') do
          Inc(P);
        P := P + 2;
        SkipWhites;
      end;
    end;
  end;

begin
  Len := Length(S);
  SkipWhites;
  if P > Len then
  begin
    Tk := #0;
    Exit('');
  end;
  case S[P] of
    '[':
      begin
        Tk := '[';
        Inc(P); W := '';
        while (P <= Len) and (S[P] <> ']') do
        begin
          W := W + S[P];
          Inc(P);
        end;
        Result := W; Inc(P);
      end;
    '=', '<', '>', '+', '-', '*', '/', '|', '&', '(', ')', '#':
      begin
        Tk := '=';
        W := S[P];
        if (P < Len) and (S[P + 1] in ['>', '=']) then
        begin
          Inc(P);
          W := W + S[P];
        end;
        Inc(P);
        Result := W;
      end;
    '''', '"':
      begin
        Tk := '''';
        Qt := S[P];
        W := ''; Inc(P);
        while (P <= Len) and (S[P] <> Qt) do
        begin
          W := W + S[P];
          Inc(P);
        end;
        Inc(P); Result := W;
      end;
    '0'..'9':
      begin
        Tk := '0';
        W := '';
        while S[P] in ['0'..'9', '.'] do
        begin
          W := W + S[P];
          Inc(P);
        end;
        Result := W;
      end;
    'a'..'z', 'A'..'Z', '_':
      begin
        Tk := 'a'; W := '';
        while (P <= Len) and (S[P] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
        begin
          W := W + S[P];
          Inc(P);
        end;
        Result := W;
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

procedure CheckInsertValues(Obj: TdxLookupComboBox; Id: Integer);
var
  SL: TStringList;
  S: String;
  i, p: Integer;
begin
  SL := TStringList.Create;
  SL.Delimiter:='|';
  SL.StrictDelimiter:=True;
  SL.DelimitedText:=Obj.InsertedValues;
  for i := SL.Count - 1 downto 0 do
  begin
    S := SL[i];
    p := Pos(';', S);
    if (Id = StrToInt(Copy(S, 1, p - 1))) or (Id = StrToInt(Copy(S, p + 1, 255))) then
      SL.Delete(i);
  end;
  Obj.InsertedValues:=SL.DelimitedText;
  SL.Free;
end;

procedure CheckFilter(CC: TComponent; Id: Integer);
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
end;

function GetObjFieldKey(Obj: TdxLookupComboBox; aValue: String): Variant;
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
end;

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

procedure CheckDeleteForm(Fm: TdxForm; DelC, CurC: TComponent);
var
  TId: Integer;
  i: Integer;
  S: String;
begin
  if (CurC is TCustomComboBox) or (CurC is TdxMemo) then
  begin
    TId := GetSourceTId(CurC);
    if TId = GetId(DelC) then
    begin
      SetSourceTId(CurC, 0);
      SetSourceFId(CurC, 0);
      SetComboFilter(CurC, '');

      if CurC is TdxLookupComboBox then
        with TdxLookupComboBox(CurC) do
        begin
          ClearObjectFieldId(Fm, Id, 0);
          InsertedValues:='';
          if Fm.ShopData.ObjId = Id then Fm.ShopData.Clear;
        end;
    end;
    if CurC is TdxLookupComboBox then
      with TdxLookupComboBox(CurC) do
      begin
        if (SourceTable = GetId(DelC)) or (TId = GetId(DelC)) then
        begin
          SourceTable := 0;
          FillFilter := '';
          for i := 0 to FieldsTables.Count - 1 do
          begin
            S := FieldsTables[i];
            FieldsTables[i] := Copy(S, Pos('=', S), 255);
          end;
        end;
        if DestTable = GetId(DelC) then
        begin
          DestTable := 0;
          for i := 0 to FieldsTables.Count - 1 do
          begin
            S := FieldsTables[i];
            FieldsTables[i] := Copy(S, 1, Pos('=', S));
          end;
        end;
      end;
  end
end;

procedure CheckDeleteField(Fm: TdxForm; DelC, CurC: TComponent);
var
  ObjFm: TdxForm;
  FId: Integer;
  i, n: Integer;
  S: String;
begin
  ObjFm := TdxForm(DelC.Owner);
  FId := GetId(DelC);
  if (CurC is TCustomComboBox) or (CurC is TdxMemo) then
  begin
    if (GetSourceTId(CurC) = ObjFm.Id) and (GetSourceFId(CurC) = FId) then
      SetSourceFId(CurC, 0);
    if CurC is TdxLookupComboBox then
      with TdxLookupComboBox(CurC) do
      begin
        if SourceTId = ObjFm.Id then
        begin
          if SourceFId = FId then SourceFId := 0;
          CheckFilter(CurC, FId);
          CheckInsertValues(TdxLookupComboBox(CurC), FId);
        end;
        // Заполнение таблицы
        if (SourceTable = ObjFm.Id) or (DestTable = ObjFm.Id) then
          for i := 0 to FieldsTables.Count - 1 do
          begin
            S := FieldsTables.Names[i];
            if S <> '' then
            begin
              n := StrToInt(S);
              if n = FId then
                FieldsTables[i] := '=' + FieldsTables.ValueFromIndex[i];
            end;
            S := FieldsTables.ValueFromIndex[i];
            if S <> '' then
            begin
              n := StrToInt(S);
              if n = FId then
                FieldsTables[i] := FieldsTables.Names[i] + '=';
            end;
          end;
      end
  end
  else if CurC is TdxObjectField then
  begin
    with TdxObjectField(CurC) do
      if (DelC is TdxLookupComboBox) and (ObjId = FId) then
      begin
        ObjId := 0;
        FieldId := 0;
      end
      else if (ObjId > 0) and (FieldId = FId) then
        FieldId := 0;
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
        Menu.DeleteItem(MI)
      else if (MI.Kind = miReport) and (MI.Id = Id) then
        Menu.DeleteItem(MI)
      else if MI.Kind = miMenu then
        DeleteRefFromMenu(MI.Items);
    end;
  end;

  procedure DeleteRefFromTabs(Tabs: TdxTabList);
  var
    j: Integer;
  begin
    for j := Tabs.Count - 1 downto 0 do
      if Tabs[j] = Id then Tabs.Delete(j);
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
  CD: TSortColData;
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
        CheckDeleteForm(F, C, CC)
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
    if Fm.GroupField = GetId(C) then Fm.GroupField := 0;
    if Fm.ParentField = GetId(C) then Fm.ParentField:=0;
  end
  else if C is TdxCalcEdit then CheckShopping(GetId(C));
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    CC := Fm.Components[i];
    if CC is TdxLookupComboBox then
    begin
      CheckInsertValues(TdxLookupComboBox(CC), GetId(C));
      ClearObjectFieldId(Fm, GetId(CC), GetId(C));
    end;
  end;
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
    ErrMsg(rsFilePathTooLong)
  else if Utf8Length(ExtractFileName(S)) > 150 then
    ErrMsg(rsFileNameTooLong)
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
  i: Integer;
  C: TControl;
  WinC: TWinControl;
begin
  Result := nil;
  for i := 0 to WC.ControlCount - 1 do
  begin
    C := WC.Controls[i];
    if C is TWinControl then
    begin
      WinC := TWinControl(C);
      if WinC.TabOrder = 0 then
      begin
        if WinC.ControlCount > 0 then Result := GetTopControl(WinC);
        if Result = nil then Result := WinC;
        Exit;
      end;
    end;
  end;
end;

function GetObjFieldValue(Obj: TObject; aKey: Integer; FullPath: Boolean
  ): String;
var
  TId, FId, PFId: Integer;
  Fm: TdxForm;
begin
  Result := '';
  if aKey = 0 then Exit;
  TId := GetSourceTId(TComponent(Obj));
  FId := GetSourceFId(TComponent(Obj));
  if (TId > 0) and (FId > 0) then
  begin
    Fm := FormMan.FindForm(TId);
    if Fm.ParentField = 0 then
      with DBase.OpenDataSet('select ' + FieldStr(FId) + ' from ' + TableStr(TId) +
        ' where id=' + IntToStr(aKey)) do
      try
        Result := Fields[0].AsString;
      finally
        Free;
      end
    else
    begin
      PFId := Fm.ParentField;
      while aKey <> 0 do
      begin
        with DBase.OpenDataSet('select ' + FieldStr(FId) + ',' + FieldStr(PFId) +
          ' from ' + TableStr(TId) + ' where id=' + IntToStr(aKey)) do
        try
          if Result = '' then Result := Fields[0].AsString
          else if FullPath then Result := Fields[0].AsString + '\' + Result
          else Result := '  ' + Result;
          aKey := Fields[1].AsInteger;
        finally
          Free;
        end;
      end;
    end;
  end;
end;

// Содрал с InvertColor
function ShiftColor(Color: TColor; N: Integer): TColor;
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
end;


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

function CheckName(const S: String): Boolean;
var
  i: Integer;
begin
  Result := True;
  if Trim(S) = '' then
  begin
    ErrMsg(rsEnterName);
    Exit(False);
  end;
  for i := 0 to Length(S) do
    if S[i] in ['''', '"', '|', '!', '<', '>', '[', ']', '{', '}', '/'] then
    begin
      ErrMsg(rsInvalidChars);
      Exit(False);
    end;
end;

function CheckName2(const S: String): Boolean;
var
  i: Integer;
begin
  Result := True;
  if Trim(S) = '' then
  begin
    ErrMsg(rsEnterName);
    Exit(False);
  end;
  for i := 0 to Length(S) do
    if S[i] in ['''', '"', '|', '!', '<', '>', '[', ']', '{', '}', '/'] then
    begin
      ErrMsg(rsInvalidChars2);
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
  if ((C is TdxLookupComboBox) or (C is TdxCounter)) and (not TryStrToInt(Value, N)) then
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
    if TryStrToDate(Value, D) then Value := Date2Str(D)
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

function CheckComponentNames(Fm: TdxForm): Boolean;
var
  i: Integer;
  C: TComponent;
begin
  Result := True;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C.Name = '' then Exit(False);
  end;
end;

function StrToXml(S: String): String;
begin
  S := StringReplace(S, '"', '&quot;', [rfReplaceAll]);
  S := StringReplace(S, '<', '&lt;', [rfReplaceAll]);
  S := StringReplace(S, '>', '&gr;', [rfReplaceAll]);
  //S := StringReplace(S, #10, '&10;', [rfReplaceAll]);
  //S := StringReplace(S, #13, '&13;', [rfReplaceAll]);
  Result := S;
end;

function XmlToStr(S: String): String;
begin
  S := StringReplace(S, '&quot;', '"', [rfReplaceAll]);
  S := StringReplace(S, '&lt;', '<', [rfReplaceAll]);
  S := StringReplace(S, '&gr;', '>', [rfReplaceAll]);
  //S := StringReplace(S, '&10;', #10, [rfReplaceAll]);
  //S := StringReplace(S, '&13;', #13, [rfReplaceAll]);
  Result := S;
end;

procedure SetDSEdit(DS: TDataSet);
begin
  if not (DS.State in [dsInsert, dsEdit]) then DS.Edit;
end;

function CheckDeleteForm(Fm: TdxForm): Boolean;
var
  j, z: Integer;
  RD: TReportData;
  Sc: TRpSource;
  Frm: TdxForm;

  function CheckField(const Fl: TRpField): Boolean;
  begin
    Result := True;
    if Fl.TId = '' then Exit;
    if StrToInt(Fl.TId) = Fm.Id then
      Exit(False);
    if Fl.Src <> nil then
      Result := CheckField(Fl.Src^);
  end;

  function CheckFields(L: TRpFieldList): Boolean;
  var
    m: Integer;
    Fl: TRpField;
    Frm: TdxForm;
  begin
    Result := True;
    for m := 0 to L.Count - 1 do
    begin
      Fl := L[m]^;
      if not CheckField(Fl) then
      begin
        if RD.Kind = rkReport then
          ErrMsg(Format(rsCantDeleteFormReport, [Fm.FormCaption, RD.Name]))
        else if RD.Kind = rkQuery then
        begin
          Frm := FindFormByRDId(RD.Id);
          ErrMsg(Format(rsCantDeleteFormQuery, [Fm.FormCaption, RD.Name, Frm.FormCaption]))
        end;
        Exit(False);
      end;
    end;
  end;

begin
  Result := True;
  for j := 0 to ReportMan.ReportCount - 1 do
  begin
    RD := ReportMan.Reports[j];
    for z := 0 to RD.Sources.Count - 1 do
    begin
      Sc := RD.Sources[z]^;

      if (StrToInt(Sc.Id) = Fm.Id) or
        ((Sc.TId <> '') and (StrToInt(Sc.TId) = Fm.Id)) then
      begin
        if RD.Kind = rkReport then
	        ErrMsg(Format(rsCantDeleteFormReport, [Fm.FormCaption, RD.Name]))
        else
        begin
          Frm := FindFormByRDId(RD.Id);
          ErrMsg(Format(rsCantDeleteFormQuery, [Fm.FormCaption, RD.Name, Frm.FormCaption]))
        end;
        Exit(False);
      end;

      Result := CheckFields(Sc.Fields);
      if not Result then Exit;
    end;
  end;
end;

function CheckCompatibles(C1, C2: TComponent): Boolean;
begin
  Result := True;
  if ((C1 is TdxEdit) or (C1 is TdxMemo) or (C1 is TdxComboBox)) and
    ((C2 is TdxEdit) or (C2 is TdxMemo) or (C2 is TdxComboBox)) then
  else if ((C1 is TdxCalcEdit) or (C1 is TdxCounter)) and (C2 is TdxCalcEdit) then
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
      with TdxPivotGrid(C) do
        if Id = aId then Clear;
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

// Добавляет для каждой формы модуль скрипта, если его нет.
{procedure AddScripts;
var
  i: Integer;
  Fm: TdxForm;
begin
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if ScriptMan.FindScript(Fm.Id) = nil then
      ScriptMan.AddScript(Fm.Id, '');
  end;
end;     }

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
      DS := Fm.Grid.DataSource.DataSet;
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
      V := DS.FieldByName(FieldStr(C)).Value;
      if V = Null then Break;

      if (C is TdxLookupComboBox) and (i < SL.Count - 1) then
      begin
        Fm := FormMan.FindForm(GetSourceTId(C));
        if Fm = nil then Exit;
        if DS <> MainDS then DS.Free;
        Tmp := SqlSelectGroups(Fm.Id, GetSourceFId(C), True);
        if Tmp <> '' then Tmp := '(' + Tmp + ')'
        else Tmp := TableStr(Fm.Id);

        C := FindComponentByFieldName(Fm, SL[i+1]);
	      if C = nil then Break;

        DS := DBase.OpenDataSet('select ' + FieldStr(C) + ' from ' + Tmp + ' where id='
          + VarToStr(V));
      end
      else
      begin
        Result := DS.FieldByName(FieldStr(C)).Value;
        Break;
      end;
    end;

  Lbl := nil;
  if (C = nil) and (SL.Count = 1) and (not aSkipLabels) then
  begin
    Lbl := FindLabelByFieldName(aForm, SL[0], False);
    if Lbl = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);
    if not LabelFieldExists(aFieldName, Lbl.Expression) then
    begin
      if Lbl.Value = unassigned then
        CalcLabelExpr(Lbl, aDS, aParForm);
      Result := Lbl.Value;
    end;
  end;
  if (C = nil) and (Lbl = nil) then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);

  finally
    if DS <> MainDS then DS.Free;
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
begin
  Result := Null;
  if aFieldName = '' then raise Exception.Create(rsFieldNameEmpty);

  SL := TStringList.Create;
  SplitStr(aFieldName, '|', SL);
  Fm := aForm;
  DS := aDS;

  try

  for i := 0 to SL.Count - 1 do
  begin
    C := FindComponentByFieldName(Fm, SL[i]);
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);
    if C is TdxLookupComboBox then
    begin
      V := DS.FieldByName(FieldStr(C)).Value;
      Result := V;
      if V = Null then Break;
      Fm := FormMan.FindForm(GetSourceTId(C));
      if Fm = nil then Exit;
      if DS <> aDS then DS.Free;
      Tmp := SqlSelectGroups(Fm.Id, GetSourceFId(C), True);
      if Tmp <> '' then Tmp := '(' + Tmp + ')'
      else Tmp := TableStr(Fm.Id);
      DS := DBase.OpenDataSet('select * from ' + Tmp + ' where id='
        + VarToStr(V));
    end
    else
    begin
      Result := DS.FieldByName(FieldStr(C)).Value;
      Break;
    end;
  end;

  finally
    if DS <> aDS then DS.Free;
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
    if Utf8CompareText(Fm.FormCaption, aName) = 0 then
    begin
      ErrMsg(rsFormNameExists);
      Exit(False);
    end;
  end;
  for i := 0 to ReportMan.ReportCount - 1 do
  begin
    RD := ReportMan.Reports[i];
    if (RD <> aRD) and (Utf8CompareText(aName, RD.Name) = 0) then
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
  {if DS.RecordCount > 0 then} DS.GotoBookmark(B);
  DS.FreeBookmark(B);
  DS.BeforeScroll:=BeforeScroll;
  DS.AfterScroll:=AfterScroll;
  DS.EnableControls;
  if (DS.State <> State) and (State in [dsInsert, dsEdit]) then
    DS.Edit;
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
  p, Len, Size: Integer;
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

procedure Debug(Value: Variant);
begin
  if OutputFm <> nil then
    OutputFm.AddMsg(VarToStr(Value));
end;

function Confirm(const Caption, Msg: String): TModalResult;
begin
  Result := MessageDlg(Caption, Msg, mtConfirmation, [mbYes, mbNo], 0);
end;

procedure Info(const Msg: String);
begin
  MessageDlg(rsWarning, Msg, mtWarning, [mbOk], 0);
end;

function CheckDuplicateLabel(ALabel: TdxLabel; const ACaption: String): Boolean;
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
    	(UTF8CompareText(TdxLabel(C).Caption, ACaption) = 0) then
    begin
     ErrMsg(rsCalcLabelCaptionExists);
      Exit(False);
    end;
  end;
end;

function CheckDuplicateLabel2(ALabel: TdxLabel; const ACaption: String): Boolean;
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
    	(UTF8CompareText(TdxLabel(C).Caption, ACaption) = 0) then
    begin
     ErrMsg(rsCalcLabelCaptionExists2);
      Exit(False);
    end;
  end;
end;

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
    	(UTF8CompareText(TdxLabel(C).Caption, ALabel.Caption) = 0) then
   		Exit(True);
  end;
end;

{function GetHelpDir: String;
begin
  Result := AppPath + 'help' + DirectorySeparator;
end;

function GetLangDir: String;
begin
  Result := AppPath + 'languages' + DirectorySeparator;
end;     }

end.

