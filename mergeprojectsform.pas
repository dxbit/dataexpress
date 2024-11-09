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

unit MergeProjectsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, StdCtrls, CheckLst, ExtCtrls, DBGrids, formmanager, DxCtrls,
  reportmanager, dxreports, strconsts, scriptmanager, dxmains, imagemanager;

type
  TFormScriptList = class;
  TQueryFormList = class;

  { TMergeProjectsFm }

  TMergeProjectsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    FormList: TCheckListBox;
    DepForms: TCheckListBox;
    DepReps: TCheckListBox;
    Panel7: TPanel;
    Panel8: TPanel;
    Splitter5: TSplitter;
    SrcForms: TCheckListBox;
    RepList: TCheckListBox;
    DepSources: TCheckListBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    ExtModules: TCheckListBox;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    UserModules: TCheckListBox;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure DepFormsClickCheck(Sender: TObject);
    procedure DepRepsClickCheck(Sender: TObject);
    procedure DepSourcesClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure RepListClick(Sender: TObject);
    procedure RepListClickCheck(Sender: TObject);
    procedure SrcFormsClickCheck(Sender: TObject);
  private
    { private declarations }
    FFmMan: TFormManager;
    FRpMan: TReportManager;
    FScrMan: TScriptManager;
    FImgMan: TImageManager;
    FMain: TDXMain;
    procedure FillForms;
    procedure FillReports;
    procedure FillSources(aRD: TReportData);
    procedure FillDependsReports(aFm: TdxForm);
    procedure FillDependsForms(aFm: TdxForm);
    procedure FillSourceForms(aFm: TdxForm);
    procedure FillExtModules;
    procedure FillUserModules;
    function MakeUniqueMergedFormName(const AnyName: String): String;
    function MakeUniqueMergedFormComponentName(const AnyName: String): String;
    function MakeUniqueMergedModuleName(const AnyName: String): String;
    function MakeUniqueMergedReportName(const AnyName: String): String;
    function MakeUniqueMergedImageName(const AnyName: String; L: TStrings): String;
    procedure ClearBrokenLinksInReports(aForms: TFormScriptList; aReports: TQueryFormList);
    procedure ClearBrokenLinks(aForms: TFormScriptList; aReports: TQueryFormList);
    procedure ChangeQueryId(aForms: TFormScriptList; OldId, NewId: Integer);
    procedure ChangeFieldIdInReports(aReports: TQueryFormList; OldId, NewId: Integer);
    procedure ChangeFieldId(aForm: TdxForm; OldId, NewId: Integer);
    procedure ChangeAllFieldId(aForms: TFormScriptList; OldId, NewId: Integer);
    procedure ChangeFormIdInMain(OldId, NewId: Integer);
    procedure ChangeFormIdInReports(aReports: TQueryFormList; OldId, NewId: Integer);
    procedure ChangeFormId(aForms: TFormScriptList; OldId, NewId: Integer);
    procedure SelReportsToList(aForms: TFormScriptList; L: TQueryFormList);
    procedure SelFormsToList(L: TFormScriptList);
    procedure SelExtModulesToList(L: TList);
    procedure SelUserModulesToList(L: TList);
    procedure GetUsedImages(FL: TFormScriptList; SL: TStrings);
    function AddFormsAndReports(FL: TFormScriptList; RL: TQueryFormList; ExtL, UserL: TList): String;
    function CheckDuplicateNames(FL: TFormScriptList; RL: TQueryFormList; ExtL, UserL: TList): String;
    function DoMerge: Boolean;
  public
    { public declarations }
    OutputMsg, ParseErrors: String;
    Failed: Boolean;
    function ShowForm(const aFileName: String): Integer;
  end;

  TFormScriptItem = class
  public
    Form: TdxForm;
    SD, WSD: TScriptData;
  end;

  { TFormScriptList }

  TFormScriptList = class(TList)
  private
    function GetForms(Index: Integer): TFormScriptItem;
  public
    function AddForm(Fm: TdxForm; SD, WSD: TScriptData): TFormScriptItem;
    function FindForm(FmId: Integer): TFormScriptItem;
    procedure Clear; override;
    property Forms[Index: Integer]: TFormScriptItem read GetForms; default;
  end;

  TQueryFormItem = class
  public
    Form: TdxForm;
    RD: TReportData;
  end;

  { TQueryFormList }

  TQueryFormList = class(TList)
  private
    function GetQueries(Index: Integer): TQueryFormItem;
  public
    procedure AddQuery(Fm: TdxForm; RD: TReportData);
    function FindQuery(RDId: Integer): TQueryFormItem;
    procedure Clear; override;
    property Queries[Index: Integer]: TQueryFormItem read GetQueries; default;
  end;

var
  MergeProjectsFm: TMergeProjectsFm;

function ShowMergeProjectsForm(const aFileName: String): Integer;

implementation

uses
  myzipper, dbengine, apputils, sqlgen, helpmanager, dxfiles, dximages,
  formdesigner, dxactions, pivotgrid;

{$R *.lfm}

procedure ProcessRenameInActions(FL: TFormScriptList; RenameObject: TRenameObject; const OldName,
  NewName: String);
var
  AR: TActionRunner;

  procedure ProcessLines(Lines: TActionLines);
  var
    j: Integer;
    Line: TActionLine;
    A: TBaseAction;
  begin
    for j := 0 to Lines.Count - 1 do
    begin
      Line := Lines[j];
      if Line.Kind <> alkAction then
        ProcessLines(Line.Lines)
      else
      begin
        A := Line.Action;
        case RenameObject of
          renForm: A.RenameForm(OldName, NewName);
          renImage: A.RenameImage(OldName, NewName);
        end;
      end;
    end;
  end;

  procedure ProcessRename(var Xml: String);
  begin
    if Xml = '' then Exit;
    AR.Load(Xml);
    ProcessLines(AR.Lines);
    AR.Save(Xml);
  end;

var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
  Bn: TdxButton;
  Xml: String;
begin
  AR := TActionRunner.Create;
  for i := 0 to FL.Count - 1 do
  begin
    Fm := FL[i].Form;
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
  end;
  AR.Free;
end;

procedure ProcessRenameImages(FL: TFormScriptList; const OldName, NewName: String);
var
  i: Integer;
begin
  for i := 0 to FL.Count - 1 do
    RenameImagesInForm(FL[i].Form, OldName, NewName);
end;

function QryIdExists(RD: TReportData; aId: Integer; IsField: Boolean): Boolean;
var
  i, j: Integer;
  pSr: PRpSource;

  function _ProcessField(pF: PRpField): Boolean;
  begin
    Result := False;
    if not IsField then
    begin
      if pF^.TId = aId then Exit(True)
    end
    else if pF^.FId = aId then Exit(True);
    if pF^.Src <> nil then
      if _ProcessField(pF^.Src) then Exit(True);
  end;

begin
  Result := False;
  for i := 0 to RD.Sources.Count - 1 do
  begin
    pSr := RD.Sources[i];
    if not IsField then
      if pSr^.Id = aId then Exit(True)
      else if pSr^.TId = aId then Exit(True);

    for j := 0 to pSr^.Fields.Count - 1 do
      if _ProcessField(PRpField(pSr^.Fields[j])) then Exit(True);
  end;
end;

procedure QryIdReplace(RD: TReportData; aId, aNewId: Integer; IsField: Boolean);
var
  i, j: Integer;
  pSr: PRpSource;

  procedure _ProcessField(pF: PRpField);
  begin
    if not IsField then
    begin
      if pF^.TId = aId then pF^.TId := aNewId;
    end
    else if pF^.FId = aId then pF^.FId := aNewId;
    if pF^.Src <> nil then
      _ProcessField(pF^.Src);
  end;

begin
  for i := 0 to RD.Sources.Count - 1 do
  begin
    pSr := RD.Sources[i];
    if not IsField then
      if pSr^.Id = aId then pSr^.Id := aNewId
      else if pSr^.TId = aId then pSr^.TId := aNewId;

    for j := 0 to pSr^.Fields.Count - 1 do
      _ProcessField(PRpField(pSr^.Fields[j]));
  end;
end;

function ShowMergeProjectsForm(const aFileName: String): Integer;
begin
  if MergeProjectsFm = nil then
  	MergeProjectsFm := TMergeProjectsFm.Create(Application);
  Result := MergeProjectsFm.ShowForm(aFileName);
end;

{ TFormScriptList }

function TFormScriptList.GetForms(Index: Integer): TFormScriptItem;
begin
  Result := TFormScriptItem(Items[Index]);
end;

function TFormScriptList.AddForm(Fm: TdxForm; SD, WSD: TScriptData
  ): TFormScriptItem;
begin
  Result := TFormScriptItem.Create;
  Result.Form := Fm;
  Result.SD := SD;
  Result.WSD := WSD;
  Add(Result);
end;

function TFormScriptList.FindForm(FmId: Integer): TFormScriptItem;
var
  i: Integer;
  Item: TFormScriptItem;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Item := Forms[i];
    if Item.Form.Id = FmId then Exit(Item);
  end;
end;

procedure TFormScriptList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Forms[i].Free;
  inherited Clear;
end;

{ TQueryFormList }

function TQueryFormList.GetQueries(Index: Integer): TQueryFormItem;
begin
  Result := TQueryFormItem(Items[Index]);
end;

procedure TQueryFormList.AddQuery(Fm: TdxForm; RD: TReportData);
var
  Item: TQueryFormItem;
begin
  Item := TQueryFormItem.Create;
  Item.Form := Fm;
  Item.RD := RD;
  Add(Item);
end;

function TQueryFormList.FindQuery(RDId: Integer): TQueryFormItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Queries[i].RD.Id = RDId then Exit(Queries[i]);
end;

procedure TQueryFormList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Queries[i].Free;
  inherited Clear;
end;

{ TMergeProjectsFm }

procedure TMergeProjectsFm.FormCreate(Sender: TObject);
begin
  FFmMan := TFormManager.Create;
  FRpMan := TReportManager.Create;
  FScrMan := TScriptManager.Create;
  FImgMan := TImageManager.Create;
  FMain := TDXMain.Create;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Caption := rsMergeProjects;
  TabSheet1.Caption := rsForms;
  TabSheet2.Caption := rsReports;
  StaticText1.Caption := rsFormLinkTo;
  StaticText2.Caption := rsFormsLinkToSelFm;
  StaticText3.Caption := rsReportsLinkToFm;
  StaticText4.Caption := rsReportRefersTo;
  StaticText5.Caption := rsExtensionsModules;
  StaticText6.Caption := rsUserModules;
end;

procedure TMergeProjectsFm.DepFormsClickCheck(Sender: TObject);
var
  Fm: TdxForm;
  i: Integer;
begin
  with DepForms do
  begin
    Fm := TdxForm(Items.Objects[ItemIndex]);
    i := FormList.Items.IndexOfObject(Fm);
    FormList.Checked[i] := Checked[ItemIndex];
  end;
end;

procedure TMergeProjectsFm.DepRepsClickCheck(Sender: TObject);
var
  i: Integer;
  RD: TdxForm;
begin
  with DepReps do
  begin
    RD := TdxForm(Items.Objects[ItemIndex]);
    i := RepList.Items.IndexOfObject(RD);
    RepList.Checked[i] := Checked[ItemIndex];
  end;
end;

procedure TMergeProjectsFm.DepSourcesClickCheck(Sender: TObject);
var
  Fm: TdxForm;
  i: Integer;
begin
  with DepSources do
  begin
    Fm := TdxForm(Items.Objects[ItemIndex]);
    i := FormList.Items.IndexOfObject(Fm);
    FormList.Checked[i] := Checked[ItemIndex];
  end;
end;

procedure TMergeProjectsFm.FormDestroy(Sender: TObject);
begin
  FMain.Free;
  FImgMan.Free;
  FScrMan.Free;
  FRpMan.Free;
  FFmMan.Free;
end;

procedure TMergeProjectsFm.FormListClick(Sender: TObject);
var
  Fm: TdxForm;
begin
  with FormList do
  begin
    if ItemIndex >= 0 then
    begin
      Fm := TdxForm(Items.Objects[ItemIndex]);
      FillSourceForms(Fm);
      FillDependsForms(Fm);
      FillDependsReports(Fm);
    end;
  end;
end;

procedure TMergeProjectsFm.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  FormList.SetFocus;
end;

procedure TMergeProjectsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('mergeprojects');
end;

procedure TMergeProjectsFm.RepListClick(Sender: TObject);
var
  RD: TReportData;
begin
  with RepList do
    if ItemIndex >= 0 then
    begin
      RD := TReportData(Items.Objects[ItemIndex]);
      FillSources(RD);
    end;
end;

procedure TMergeProjectsFm.RepListClickCheck(Sender: TObject);
begin
  FormListClick(FormList);
end;

procedure TMergeProjectsFm.SrcFormsClickCheck(Sender: TObject);
var
  Fm: TdxForm;
  i: Integer;
begin
  with SrcForms do
  begin
    Fm := TdxForm(Items.Objects[ItemIndex]);
    i := FormList.Items.IndexOfObject(Fm);
    FormList.Checked[i] := Checked[ItemIndex];
  end;
end;

procedure TMergeProjectsFm.FillForms;
begin
  FormList.Clear;
  FFmMan.FormsToList(FormList.Items);
end;

procedure TMergeProjectsFm.FillReports;
begin
  RepList.Clear;
  FRpMan.GetReports(RepList.Items);
end;

procedure TMergeProjectsFm.FillSources(aRD: TReportData);
var
  i: Integer;
  Fm: TdxForm;

  procedure _AddSource(aFm: TdxForm);
  var
    idx, idx2: Integer;
  begin
    if aFm.PId > 0 then aFm := FFmMan.FindForm(aFm.PId);
    if DepSources.Items.IndexOfObject(aFm) >= 0 then Exit;
    idx := DepSources.Items.AddObject(aFm.FormCaption, aFm);
    idx2 := FormList.Items.IndexOfObject(aFm);
    DepSources.Checked[idx] := FormList.Checked[idx2];
  end;

begin
  DepSources.Clear;
  for i := 0 to FFmMan.FormCount - 1 do
  begin
    Fm := FFmMan.Forms[i];
    if QryIdExists(aRD, Fm.Id, False) then
      _AddSource(Fm);
  end;
end;

procedure TMergeProjectsFm.FillDependsReports(aFm: TdxForm);
var
  i: Integer;
  RD: TReportData;

  procedure _AddRep(aRD: TReportData);
  var
    idx, idx2: Integer;
  begin
    idx := DepReps.Items.AddObject(aRD.Name, aRD);
    idx2 := RepList.Items.IndexOfObject(aRD);
    DepReps.Checked[idx] := RepList.Checked[idx2];
  end;

begin
  DepReps.Clear;
  for i := 0 to FRpMan.ReportCount - 1 do
  begin
    RD := FRpMan.Reports[i];
    if RD.Kind <> rkReport then Continue;
    if QryIdExists(RD, aFm.Id, False) then
      _AddRep(RD);
  end;
end;

procedure TMergeProjectsFm.FillDependsForms(aFm: TdxForm);

  procedure _AddForm(L: TStrings; aForm: TdxForm);
  var
    idx, idx2: Integer;
  begin
    if aForm.PId > 0 then
      aForm := FFmMan.FindForm(aForm.PId);
    if (aForm = aFm) or (L.IndexOfObject(aForm) >= 0) then Exit;
    idx := L.AddObject(aForm.FormCaption, aForm);
    idx2 := FormList.Items.IndexOfObject(aForm);
    DepForms.Checked[idx] := FormList.Checked[idx2];
  end;

var
  L: TStrings;
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
  RD: TReportData;

begin
  DepForms.Clear;
  L := DepForms.Items;
  for i := 0 to FFmMan.FormCount - 1 do
  begin
    Fm := FFmMan.Forms[i];
    if Fm = aFm then Continue;
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if (C is TdxComboBox) or (C is TdxLookupComboBox) or (C is TdxMemo) then
      begin
        if GetSourceTId(C) = aFm.Id then
        begin
          _AddForm(L, Fm);
          Break;
        end;
      end
      else if C is TdxQueryGrid then
      begin
        RD := FRpMan.FindReport(TdxQueryGrid(C).Id);
        if QryIdExists(RD, aFm.Id, False) then
        begin
          _AddForm(L, Fm);
          Break;
        end;
      end
      {else if C is TdxButton then
      begin
        if BnGetFormId(TdxButton(C)) = aFm.Id then
        begin
          _AddForm(Fm);
          Break;
        end;
      end;}
    end;
  end;
end;

procedure TMergeProjectsFm.FillSourceForms(aFm: TdxForm);
var
  L: TStrings;

  procedure _AddForm(FmId: Integer);
  var
    idx, idx2: Integer;
    Fm: TdxForm;
  begin
    Fm := FFmMan.FindForm(FmId);
    if (Fm = nil) or (Fm.PId > 0) or (L.IndexOfObject(Fm) >= 0) or (Fm = aFm) then Exit;

    idx := L.AddObject(Fm.FormCaption, Fm);
    idx2 := FormList.Items.IndexOfObject(Fm);
    SrcForms.Checked[idx] := FormList.Checked[idx2];
  end;

  procedure _ProcessField(pF: PRpField);
  begin
    _AddForm(pF^.TId);
    if pF^.Src <> nil then _ProcessField(pF^.Src);
  end;

  procedure _Fill(aForm: TdxForm);
  var
    i, j, z: Integer;
    C: TComponent;
    RD: TReportData;
    pSr: PRpSource;
  begin
    for i := 0 to aForm.ComponentCount - 1 do
    begin
      C := aForm.Components[i];
      if (C is TdxComboBox) or (C is TdxLookupComboBox) or (C is TdxMemo) then
      begin
        _AddForm(GetSourceTId(C));
      end
      else if C is TdxQueryGrid then
      begin
        RD := FRpMan.FindReport(TdxQueryGrid(C).Id);
        for j := 0 to RD.Sources.Count - 1  do
        begin
          pSr := RD.Sources[j];
          _AddForm(pSr^.Id);
          for z := 0 to pSr^.Fields.Count - 1 do
            _ProcessField(pSr^.Fields[z]);
        end
      end
      {else if C is TdxButton then
        _AddForm(BnGetFormId(TdxButton(C)))}
      else if C is TdxGrid then
      begin
        _Fill(FFmMan.FindForm(TdxGrid(C).Id));
      end;
    end;
  end;

begin
  SrcForms.Clear;
  L := SrcForms.Items;
  _Fill(aFm);
end;

procedure TMergeProjectsFm.FillExtModules;
var
  i: Integer;
  SD: TScriptData;
begin
  ExtModules.Clear;
  for i := 0 to FScrMan.ScriptCount - 1 do
  begin
    SD := FScrMan.Scripts[i];
    if SD.Kind in [skExpr, skWebExpr] then
      ExtModules.Items.AddObject(SD.Name, SD);
  end;
end;

procedure TMergeProjectsFm.FillUserModules;
var
  i: Integer;
  SD: TScriptData;
begin
  UserModules.Clear;
  for i := 0 to FScrMan.ScriptCount - 1 do
  begin
    SD := FScrMan.Scripts[i];
    if SD.Kind <> skUser then Continue;
    UserModules.Items.AddObject(SD.Name, SD);
  end;
end;

function TMergeProjectsFm.MakeUniqueMergedFormName(const AnyName: String
  ): String;
var
  S, Nm: String;
  n: Integer;
begin
  SplitComponentName(AnyName, S, n);
  repeat
    Inc(n);
    Nm := S + IntToStr(n);
  until (FormMan.FindFormByName(Nm) = nil) and (FFmMan.FindFormByName(Nm) = nil);
  Result := Nm;
end;

function TMergeProjectsFm.MakeUniqueMergedFormComponentName(
  const AnyName: String): String;
var
  S, Nm: String;
  n: Integer;
begin
  SplitComponentName(AnyName, S, n);
  repeat
    Inc(n);
    Nm := S + IntToStr(n);
  until (FormMan.FindFormByComponentName(Nm) = nil) and (FFmMan.FindFormByComponentName(Nm) = nil);
  Result := Nm;
end;

function TMergeProjectsFm.MakeUniqueMergedModuleName(const AnyName: String
  ): String;
var
  S, Nm: String;
  n: Integer;
begin
  SplitComponentName(AnyName, S, n);
  repeat
    Inc(n);
    Nm := S + IntToStr(n);
  until (ScriptMan.FindScriptByName(Nm) = nil) and (FScrMan.FindScriptByName(Nm) = nil);
  Result := Nm;
end;

function TMergeProjectsFm.MakeUniqueMergedReportName(const AnyName: String
  ): String;
var
  S, Nm: String;
  n: Integer;
begin
  SplitComponentName(AnyName, S, n);
  repeat
    Inc(n);
    Nm := S + IntToStr(n);
  until (ReportMan.FindByName(Nm) = nil) and (FRpMan.FindByName(Nm) = nil);
  Result := Nm;
end;

function TMergeProjectsFm.MakeUniqueMergedImageName(const AnyName: String;
  L: TStrings): String;
var
  S, Nm: String;
  n: Integer;
begin
  SplitComponentName(AnyName, S, n);
  repeat
    Inc(n);
    Nm := S + IntToStr(n);
  until (ImageMan.ImageExists(Nm) = False) and (FImgMan.ImageExists(Nm) = False);
  Result := Nm;
end;

procedure TMergeProjectsFm.ClearBrokenLinksInReports(aForms: TFormScriptList;
  aReports: TQueryFormList);

  function IsExistsForm(FmId: Integer): Boolean;
  begin
    Result := aForms.FindForm(FmId) <> nil;
  end;

  function _ProcessField(pFl: PRpField): Boolean;
  begin
    Result := True;
    if not IsExistsForm(pFl^.TId) then Exit(False);
    if pFl^.Src <> nil then
      Result := _ProcessField(pFl^.Src);
  end;

  function ProcessFields(FL: TRpFieldList): Boolean;
  var
    z: Integer;
    pF: PRpField;
  begin
    Result := True;
    for z := 0 to FL.Count - 1 do
    begin
      pF := FL[z];
      if pF^.Zero then Continue;
      if not _ProcessField(pF) then Exit(False);
    end;
  end;

var
  i, j: Integer;
  RD: TReportData;
  pSr: PRpSource;
  Fm: TFormScriptItem;
begin
  for i := 0 to aReports.Count - 1 do
  begin
    RD := aReports[i].RD;
    for j := 0 to RD.Sources.Count - 1 do
    begin
      pSr := RD.Sources[j];
      if (not IsExistsForm(pSr^.Id)) or (not ProcessFields(pSr^.Fields)) then
      begin
        RD.Clear;
        Break;
      end
    end;
  end;
end;

procedure TMergeProjectsFm.ClearBrokenLinks(aForms: TFormScriptList; aReports: TQueryFormList);
var
  i, j, V: Integer;
  Fm: TdxForm;
  C: TComponent;
  QItem: TQueryFormItem;
  G: TFormGroup;
begin
  for i := 0 to aForms.Count - 1 do
  begin
    Fm := aForms[i].Form;
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if (C is TdxComboBox) or (C is TdxLookupComboBox) or (C is TdxMemo) then
      begin
        if aForms.FindForm(GetSourceTId(C)) = nil then
          ResetLookupComponent(C);

        if C is TdxLookupComboBox then
          with TdxLookupcomboBox(C) do
          begin
            if ListSource > 0 then
            begin
              QItem := aReports.FindQuery(ListSource);
              if (QItem <> nil) and QItem.RD.IsEmpty then
                ResetListSource;
            end;
          end;
      end;
    end;
  end;

  for i := FMain.Tabs.Count - 1 downto 0 do
  begin
    V := FMain.Tabs[i];
    if aForms.FindForm(V) = nil then FMain.Tabs.DeleteValue(V);
  end;

  for i := FMain.Groups.Count - 1 downto 0 do
  begin
    G := FMain.Groups[i];
    for j := G.IdList.Count - 1 downto 0 do
    begin
      V := G.IdList[j];
      if aForms.FindForm(V) = nil then G.IdList.DeleteValue(V);
    end;
  end;
end;

procedure TMergeProjectsFm.ChangeQueryId(aForms: TFormScriptList; OldId,
  NewId: Integer);
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
begin
  for i := 0 to aForms.Count - 1 do
  begin
    Fm := aForms[i].Form;
    C := FindQueryGrid(Fm, OldId);
    if C <> nil then SetId(C, NewId);
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxLookupComboBox then
        with TdxLookupComboBox(C) do
        begin
          if ListSource = OldId then
            ListSource := NewId;
        end
      else if C is TdxPivotGrid then
        with TdxPivotGrid(C) do
        begin
          if Id = OldId then
            Id := NewId;
        end;
    end;
    {for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxQueryGrid then
      begin
        with TdxQueryGrid(C) do
          if Id = OldId then Id := NewId;
      end
    end;}
  end;
end;

procedure TMergeProjectsFm.ChangeFieldIdInReports(aReports: TQueryFormList;
  OldId, NewId: Integer);
var
  i: Integer;
  RD: TReportData;
begin
  for i := 0 to aReports.Count - 1 do
  begin
    RD := aReports[i].RD;
    QryIdReplace(RD, OldId, NewId, True);
  end;
end;

procedure TMergeProjectsFm.ChangeFieldId(aForm: TdxForm; OldId, NewId: Integer);
var
  i, j, N: Integer;
  C: TComponent;
  SL: TStringList;
  Col: TColumn;
  Vl: TInsertValueData;
  TF: TdxFormTreeField;
begin
  SL := TStringList.Create;
  if aForm.ParentField = OldId then aForm.ParentField:=NewId;
  // Дерево
  for i := 0 to aForm.Tree.Fields.Count - 1 do
  begin
    TF := aForm.Tree.Fields[i];
    if TF.FieldId = OldId then TF.FieldId := NewId;
  end;
  //if aForm.GroupField = OldId then aForm.GroupField := NewId;
  // Ссылки на поля в колонках
  for i := 0 to aForm.Grid.Columns.Count - 1 do
  begin
    Col := aForm.Grid.Columns[i];
    if Col.Tag = OldId then
    begin
      Col.Tag := NewId;
      C := FindById(aForm, OldId);
      Col.FieldName := FieldStr(NewId);
      if C is TdxLookupComboBox then
        Col.FieldName := Col.FieldName + 'l'
      else if C is TdxDBImage then
        Col.FieldName := Col.FieldName + 'thumb'
      else if C is TdxFile then
        Col.FieldName := Col.FieldName + 'd';
    end;
  end;
  // Шопинг
  with aForm.ShopData do
  begin
    if ObjId = OldId then ObjId := NewId;
    if PriceFId = OldId then PriceFId := NewId;
    if PriceObjFId = OldId then PriceObjFId := NewId;
    if QttyFId = OldId then QttyFId := NewId;
  end;

  for i := 0 to aForm.ComponentCount - 1 do
  begin
    C := aForm.Components[i];
    if C is TdxLookupComboBox then
      with TdxLookupComboBox(C) do
      begin
        // источник списка
        if SourceFId = OldId then SourceFId := NewId;
        // Заполнить таблицу
        for j := 0 to FieldsTables.Count - 1 do
        begin
          if TryStrToInt(FieldsTables.Names[j], N) and (N = OldId) then
            FieldsTables[j] := IntToStr(NewId) + '=' + FieldsTables.ValueFromIndex[j]
          else if TryStrToInt(FieldsTables.ValueFromIndex[j], N) and (N = OldId) then
            FieldsTables.ValueFromIndex[j] := IntToStr(NewId);
        end;
        // Вставить значения
        for j := 0 to InsertedValues.Count - 1 do
        begin
          Vl := InsertedValues[j];
          if Vl.SrcField = OldId then Vl.SrcField := NewId;
          if Vl.DestField = OldId then Vl.DestField := NewId;
        end;
        // Поля списка
        for j := 0 to ListFields.Count - 1 do
        	if ListFields[j].FieldId = OldId then ListFields[j].FieldId := NewId;
      end
    // Источник списка
    else if (C is TdxComboBox) or (C is TdxMemo) then
    begin
      if GetSourceFId(C) = OldId then SetSourceFId(C, NewId);
    end
    else if C is TdxObjectField then
      // Ссылки на объект и поля объекта
      with TdxObjectField(C) do
      begin
        if ObjId = OldId then ObjId := NewId
        else if FieldId = OldId then FieldId := NewId;
      end
    // Команды кнопки
    {else if C is TdxButton then
      BnChangeFieldId(TdxButton(C), OldId, NewId);}
  end;
  SL.Free;
end;

procedure TMergeProjectsFm.ChangeAllFieldId(aForms: TFormScriptList; OldId,
  NewId: Integer);
var
  i: Integer;
begin
  for i := 0 to aForms.Count - 1 do
    ChangeFieldId(aForms[i].Form, OldId, NewId);
end;

procedure TMergeProjectsFm.ChangeFormIdInMain(OldId, NewId: Integer);
var
  i, n: Integer;
begin
  n := FMain.Tabs.FindValue(OldId);
  if n >= 0 then FMain.Tabs[n] := NewId;
  for i := 0 to FMain.Groups.Count - 1 do
  begin
    n := FMain.Groups[i].IdList.FindValue(OldId);
    if n >= 0 then
    begin
      FMain.Groups[i].IdList[n] := NewId;
      Exit;
    end;
  end;
end;

procedure TMergeProjectsFm.ChangeFormIdInReports(aReports: TQueryFormList;
  OldId, NewId: Integer);
var
  i: Integer;
  RD: TReportData;
begin
  for i := 0 to aReports.Count - 1 do
  begin
    RD := aReports[i].RD;
    QryIdReplace(RD, OldId, NewId, False);
  end;
end;

procedure TMergeProjectsFm.ChangeFormId(aForms: TFormScriptList; OldId,
  NewId: Integer);
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
begin
  for i := 0 to aForms.Count - 1 do
  begin
    Fm := aForms[i].Form;
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      // Заполнить таблицу, источник списка
      if C is TdxLookupComboBox then
        with TdxLookupComboBox(C) do
        begin
          if SourceTable = OldId then SourceTable := NewId
          else if DestTable = OldId then DestTable := NewId;
          if SourceTId = OldId then SourceTId := NewId;
        end
      // Источник списка
      else if (C is TdxComboBox) or (C is TdxMemo) then
      begin
        if GetSourceTId(C) = OldId then SetSourceTId(C, NewId);
      end
      // Таблица ссылается на подчиненную форму
      else if C is TdxGrid then
      begin
        if GetId(C) = OldId then SetId(C, NewId);
      end
    end;
    // Подчиненная форма ссылается на родительскую
    if Fm.PId = OldId then Fm.PId := NewId;
  end;
end;

// В том числе и запросы.
procedure TMergeProjectsFm.SelReportsToList(aForms: TFormScriptList;
  L: TQueryFormList);
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
  RD: TReportData;
begin
  for i := 0 to aForms.Count - 1 do
  begin
    Fm := aForms[i].Form;
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxQueryGrid then
      begin
        RD := FRpMan.FindReport(TdxQueryGrid(C).Id);
        L.AddQuery(Fm, RD);
      end;
    end;
  end;
  for i := 0 to RepList.Count - 1 do
  begin
    if RepList.Checked[i] then
      L.AddQuery(nil, TReportData(RepList.Items.Objects[i]));
  end;
end;

procedure TMergeProjectsFm.SelFormsToList(L: TFormScriptList);
var
  i, n: Integer;
  Fm, PFm: TdxForm;
begin
  for i := 0 to FFmMan.FormCount - 1 do
  begin
    Fm := FFmMan.Forms[i];
    PFm := Fm;
    // Для подчиненных форм смотрим выбрана ли родительская форма
    if Fm.PId > 0 then
      PFm := FFmMan.FindForm(Fm.PId);
    n := FormList.Items.IndexOfObject(PFm);
    if FormList.Checked[n] then
      L.AddForm(Fm, FScrMan.FindScript(Fm.Id, skForm), FScrMan.FindScript(Fm.Id, skWebForm));
  end;
end;

procedure TMergeProjectsFm.SelExtModulesToList(L: TList);
var
  i: Integer;
begin
  for i := 0 to ExtModules.Count - 1 do
  begin
    if ExtModules.Checked[i] then
      L.Add(ExtModules.Items.Objects[i]);
  end;
end;

procedure TMergeProjectsFm.SelUserModulesToList(L: TList);
var
  i: Integer;
begin
  for i := 0 to UserModules.Count - 1 do
  begin
    if UserModules.Checked[i] then
      L.Add(UserModules.Items.Objects[i]);
  end;
end;

procedure TMergeProjectsFm.GetUsedImages(FL: TFormScriptList; SL: TStrings);
var
  i, j: Integer;
  Fm: TdxForm;
  S: String;
  C: TComponent;
begin
  for i := 0 to FL.Count - 1 do
  begin
    Fm := FL[i].Form;
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      S := GetImageName(C);
      if (S <> '') and (SL.IndexOf(S) < 0) then
        SL.Add(S);
    end;
  end;
end;

function FormSortProc(Item1, Item2: Pointer): Integer;
var
  id1, id2: Integer;
begin
  id1 := Abs(TFormScriptItem(Item1).Form.Id);
  id2 := Abs(TFormScriptItem(Item2).Form.Id);
  Result := id1 - id2;
end;

function ReportSortProc(Item1, Item2: Pointer): Integer;
var
  id1, id2: Integer;
begin
  id1 := Abs(TQueryFormItem(Item1).RD.Id);
  id2 := Abs(TQueryFormItem(Item2).RD.Id);
  Result := id1 - id2;
end;

function CompSortProc(Item1, Item2: Pointer): Integer;
var
  id1, id2: Integer;
begin
  id1 := Abs(GetId(TComponent(Item1)));
  id2 := Abs(GetId(TComponent(Item2)));
  Result := id1 - id2;
end;

function TMergeProjectsFm.DoMerge: Boolean;
var
  RL: TQueryFormList;
  FL: TFormScriptList;
  CL: TList;

  procedure _ProcessForm(Item: TFormScriptItem; NewTId: Integer);
  begin
    ChangeFormId(FL, Item.Form.Id, NewTId);
    ChangeFormIdInReports(RL, Item.Form.Id, NewTId);
    ChangeFormIdInMain(Item.Form.Id, NewTId);
    Item.Form.Id := NewTId;
    if Item.SD <> nil then Item.SD.FmId := NewTId;
    if Item.WSD <> nil then Item.WSD.FmId := NewTId;
  end;

  procedure _ProcessField(C: TComponent; NewFId: Integer);
  begin
    ChangeAllFieldId(FL, GetId(C), NewFId);
    ChangeFieldIdInReports(RL, GetId(C), NewFId);
    SetId(C, NewFId);
  end;

  procedure _ProcessReport(Item: TQueryFormItem; NewRId: Integer);
  begin
    if Item.RD.Kind = rkQuery then
      ChangeQueryId(FL, Item.RD.Id, NewRId);
    Item.RD.Id := NewRId;
  end;

var
  ExtL, UserL: TList;
  MaxTId, MaxFId, MaxRId: Integer;
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
  Msg: String;
begin
  Result := True;
  FL := TFormScriptList.Create;
  RL := TQueryFormList.Create;
  CL := TList.Create;
  ExtL := TList.Create;
  UserL := TList.Create;
  OutputMsg := '';

  try

  SelFormsToList(FL);
  FL.Sort(@FormSortProc);
  SelReportsToList(FL, RL);
  RL.Sort(@ReportSortProc);
  SelExtModulesToList(ExtL);
  SelUserModulesToList(UserL);

  Msg := CheckDuplicateNames(FL, RL, ExtL, UserL);
  if Msg <> '' then
  begin
    if MessageDlg(rsWarning, rsMergePrjBeforeRenameMsg + Spaces + Msg + Spaces +
      rsContinueMergeProjects, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit(False);
  end;

  MaxTId := DBase.GetCurrentId('gen_tid');
  MaxFId := DBase.GetCurrentId('gen_fid');
  MaxRId := DBase.GetCurrentId('gen_rid');

  ClearBrokenLinks(FL, RL);
  ClearBrokenLinksInReports(FL, RL);

  // Сначала меняем Id на 1,2,3,4...
  for i := 0 to FL.Count - 1 do
    _ProcessForm(FL[i], i + 1);
  for i := FL.Count - 1 downto 0 do
    _ProcessForm(FL[i], MaxTId + i + 1);

  for i := 0 to FL.Count - 1 do
  begin
    Fm := FL[i].Form;
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if HasFId(C) then CL.Add(C);
    end;
  end;

  CL.Sort(@CompSortProc);
  for i := 0 to CL.Count - 1 do
    _ProcessField(TComponent(CL[i]), i + 1);
  for i := CL.Count - 1 downto 0 do
    _ProcessField(TComponent(CL[i]), MaxFId + i + 1);

  for i := 0 to RL.Count - 1 do
    _ProcessReport(RL[i], i + 1);
  for i := RL.Count - 1 downto 0 do
    _ProcessReport(RL[i], MaxRId + i + 1);

  DBase.ChangeId('gen_tid', MaxTId + FL.Count + 1);
  DBase.ChangeId('gen_fid', MaxFId + CL.Count + 1);
  DBase.ChangeId('gen_rid', MaxRId + RL.Count + 1);

  OutputMsg := AddFormsAndReports(FL, RL, ExtL, UserL);

  finally
    UserL.Free;
    ExtL.Free;
    CL.Free;
    RL.Free;
    FL.Free;
  end;
end;

function ModuleErrorMessagesToString(SD: TScriptData): String;
var
  i: Integer;
  Msg: TCompilerMsg;
begin
  Result := '';
  for i := 0 to SD.MsgCount - 1 do
  begin
    Msg := SD.Msgs[i];
    if Msg.ErrorType = 'Error' then
      Result := Result + SD.Name + ': ' + Msg.Msg + LineEnding;
  end;
end;

function TMergeProjectsFm.AddFormsAndReports(FL: TFormScriptList;
  RL: TQueryFormList; ExtL, UserL: TList): String;

  procedure CopyImage(const FromName, ToName: String; AIndex: Integer);
  var
    St: TStream;
  begin
    FImgMan.GetImageStream(FromName, AIndex, St);
    if St <> nil then
    begin
      ImageMan.SetImageStream(ToName, AIndex, St);
      St.Free;
    end;
  end;

var
  RenForms, RenFormNames, RenReps, RenQry, RenExt, RenUser, RenImg, Tmp,
    ImgSrcName, ImgDestName: String;
  i: Integer;
  RD: TReportData;
  Fm: TdxForm;
  SD, DestSD: TScriptData;
  UsedImages: TStringList;
  GrpSrc, GrpDest: TFormGroup;
begin
  // Модули расширений
  ParseErrors := '';
  RenExt := '';
  for i := 0 to ExtL.Count - 1 do
  begin
    SD := TScriptData(ExtL[i]);
    if ScriptMan.FindScriptByName(SD.Name) <> nil then
    begin
      Tmp := MakeUniqueMergedModuleName(SD.Name + '_');
      RenExt := RenExt + SD.Name + ' -> ' + Tmp + LineEnding;
      SD.Name := Tmp;
    end;
    DestSD := ScriptMan.AddNewScript(0, SD.Name, SD.Source);
    DestSD.Kind := SD.Kind;
    DestSD.SourceData.LoadFromString(SD.SourceData.SaveToString);
    ScriptMan.ParseExprModule(DestSD);
    ParseErrors := ParseErrors + ModuleErrorMessagesToString(DestSD);
  end;
  // Модули пользователя
  RenUser := '';
  for i := 0 to UserL.Count - 1 do
  begin
    SD := TScriptData(UserL[i]);
    if ScriptMan.FindScriptByName(SD.Name) <> nil then
    begin
      Tmp := MakeUniqueMergedModuleName(SD.Name + '_');
      RenUser := RenUser + SD.Name + ' -> ' + Tmp + LineEnding;
      SD.Name := Tmp;
    end;
    DestSD := ScriptMan.AddNewScript(0, SD.Name, SD.Source);
    DestSD.Kind := skUser;
    DestSD.SourceData.LoadFromString(SD.SourceData.SaveToString);
  end;
  // Формы
  RenForms := '';
  RenFormNames := '';
  for i := 0 to FL.Count - 1 do
  begin
    Fm := FL[i].Form;
    if FormMan.FindFormByName(Fm.FormCaption) <> nil then
    begin
      Tmp := MakeUniqueMergedFormName(Fm.FormCaption + '_');
      RenForms := RenForms + Fm.FormCaption + ' -> ' + Tmp + LineEnding;
      ProcessRenameInActions(FL, renForm, Fm.FormCaption, Tmp);
      Fm.FormCaption := Tmp;
    end;
    if FormMan.FindFormByComponentName(Fm.Name) <> nil then
    begin
      Tmp := MakeUniqueMergedFormComponentName(Fm.Name + '_');
      RenFormNames := RenFormNames + Fm.Name + ' -> ' + Tmp + LineEnding;
      Fm.Name := Tmp;
    end;

    FL[i].Form := FormMan.AddCopyForm(Fm); // Подменяю на созданную копию, чтобы уже в ней переименовывать формы.
    Cache.AddFormWithComponents(Fm);
    FormChanges.AddForm(Fm.Id, 0);

    // Закладки и группы
    if FMain.Tabs.FindValue(Fm.Id) >= 0 then
      DXMain.Tabs.AddValue(Fm.Id);

    GrpSrc := FMain.Groups.FindGroupByFormId(Fm.Id);
    if GrpSrc <> nil then
    begin
      GrpDest := DXMain.Groups.FindGroup(GrpSrc.Name);
      if GrpDest = nil then
      begin
        GrpDest := DXMain.Groups.AddGroup;
        GrpDest.Name := GrpSrc.Name;
      end;
      GrpDest.IdList.AddValue(Fm.Id);
    end;

    SD := FL[i].SD;
    if SD <> nil then
    begin
      DestSD := ScriptMan.AddNewScript(Fm.Id, '', SD.Source);
      DestSD.Kind := skForm;
      DestSD.SourceData.LoadFromString(SD.SourceData.SaveToString);
    end;
    SD := FL[i].WSD;
    if SD <> nil then
    begin
      DestSD := ScriptMan.AddNewScript(Fm.Id, '', SD.Source);
      DestSD.Kind := skWebForm;
      DestSD.SourceData.LoadFromString(SD.SourceData.SaveToString);
    end;
  end;
  // Отчеты
  RenReps := '';
  RenQry := '';
  for i := 0 to RL.Count - 1 do
  begin
    RD := RL[i].RD;
    Fm := RL[i].Form;
    if ReportMan.FindByName(RD.Name) <> nil then
    begin
      Tmp := MakeUniqueMergedReportName(RD.Name + '_');
      if Fm = nil then
        RenReps := RenReps + RD.Name + ' -> ' + Tmp + LineEnding
      else
        RenQry := RenQry + Fm.FormCaption + ': ' + RD.Name + ' -> ' + Tmp + LineEnding;
      RD.Name := Tmp;
    end;
    ReportMan.AddCopyReport(RD);
  end;
  // Изображения из галереи
  RenImg := '';
  UsedImages := TStringList.Create;
  GetUsedImages(FL, UsedImages);
  for i := 0 to UsedImages.Count - 1 do
  begin
    ImgSrcName := UsedImages[i];
    if ImageMan.ImageExists(ImgSrcName) then
    begin
      ImgDestName := MakeUniqueMergedImageName(ImgSrcName + '_', UsedImages);
      ProcessRenameImages(FL, ImgSrcName, ImgDestName);
      ProcessRenameInActions(FL, renImage, ImgSrcName, ImgDestName);
      RenImg := RenImg + ImgSrcName + ' -> ' + ImgDestName + LineEnding;
    end
    else
      ImgDestName := ImgSrcName;
    ImageMan.AddImage(ImgDestName);
    CopyImage(ImgSrcName, ImgDestName, 0);
    CopyImage(ImgSrcName, ImgDestName, 1);
    CopyImage(ImgSrcName, ImgDestName, 2);
  end;
  UsedImages.Free;
  for i := 0 to FL.Count - 1 do
    UpdateImagesInForm(FL[i].Form);

  if RenForms <> '' then RenForms := rsForms + ':' + LineEnding + RenForms + LineEnding;
  if RenFormNames <> '' then RenFormNames := rsComponents + ':' +
    LineEnding + RenFormNames + LineEnding;
  if RenQry <> '' then RenQry := rsQueries + ':' + LineEnding + RenQry + LineEnding;
  if RenReps <> '' then RenReps := rsReports + ':' + LineEnding + RenReps + LineEnding;
  if RenExt <> '' then RenExt := rsExtensionsModules + ':' + LineEnding + RenExt + Lineending;
  if RenUser <> '' then RenUser := rsUserModules + ':' + LineEnding + RenUser + LineEnding;
  if RenImg <> '' then RenImg := rsImages + ':' + LineEnding + RenImg + LineEnding;
  Result := RenForms + RenFormNames + RenQry + RenReps + RenExt + RenUser + RenImg;
  SetLength(Result, Length(Result) - Length(LineEnding) * 2);
end;

function TMergeProjectsFm.CheckDuplicateNames(FL: TFormScriptList;
  RL: TQueryFormList; ExtL, UserL: TList): String;
var
  RenForms, RenFormNames, RenReps, RenQry, RenExt, RenUser, RenImg: String;
  i: Integer;
  RD: TReportData;
  Fm: TdxForm;
  SD: TScriptData;
  SL: TStringList;
begin
  RenForms := '';
  RenFormNames := '';
  for i := 0 to FL.Count - 1 do
  begin
    Fm := FL[i].Form;
    if FormMan.FindFormByName(Fm.FormCaption) <> nil then
      RenForms := RenForms + Fm.FormCaption + LineEnding;
    if FormMan.FindFormByComponentName(Fm.Name) <> nil then
      RenFormNames := RenFormNames + Fm.Name + LineEnding;
  end;
  RenReps := '';
  RenQry := '';
  for i := 0 to RL.Count - 1 do
  begin
    RD := RL[i].RD;
    Fm := RL[i].Form;
    if ReportMan.FindByName(RD.Name) <> nil then
    begin
      if Fm = nil then
        RenReps := RenReps + RD.Name + LineEnding
      else
        RenQry := RenQry + Fm.FormCaption + ': ' + RD.Name + LineEnding;
    end;
  end;
  RenExt := '';
  for i := 0 to ExtL.Count - 1 do
  begin
    SD := TScriptData(ExtL[i]);
    if ScriptMan.FindScriptByName(SD.Name) <> nil then
      RenExt := RenExt + SD.Name + LineEnding;
  end;
  RenUser := '';
  for i := 0 to UserL.Count - 1 do
  begin
    SD := TScriptData(UserL[i]);
    if ScriptMan.FindScriptByName(SD.Name) <> nil then
      RenUser := RenUser + SD.Name + LineEnding;
  end;
  RenImg := '';
  SL := TStringList.Create;
  GetUsedImages(FL, SL);
  for i := 0 to SL.Count - 1 do
    if ImageMan.ImageExists(SL[i]) then
      RenImg := RenImg + SL[i] + LineEnding;
  SL.Free;

  if RenForms <> '' then RenForms := rsForms + ':' + LineEnding + RenForms + LineEnding;
  if RenFormNames <> '' then RenFormNames := rsComponents + ':' +
    LineEnding + RenFormNames + LineEnding;
  if RenQry <> '' then RenQry := rsQueries + ':' + LineEnding + RenQry + LineEnding;
  if RenReps <> '' then RenReps := rsReports + ':' + LineEnding + RenReps + LineEnding;
  if RenExt <> '' then RenExt := rsExtensionsModules + ':' + LineEnding + RenExt + Lineending;
  if RenUser <> '' then RenUser := rsUserModules + ':' + LineEnding + RenUser + LineEnding;
  if RenImg <> '' then RenImg := rsImages + ':' + LineEnding + RenImg + LineEnding;
  Result := RenForms + RenFormNames + RenQry + RenReps + RenExt + RenUser + RenImg;
  SetLength(Result, Length(Result) - Length(LineEnding) * 2);
end;

function TMergeProjectsFm.ShowForm(const aFileName: String): Integer;
var
  TempDir: String;
begin
  Failed := False;
  TempDir := GetTempDir + 'dx' + IntToStr(Random(1000000)) + DirectorySeparator;

  try

  with TMyUnZipper.Create do
  try
    FileName:=aFileName;
    OutputPath:=TempDir;
    UnZipAllFiles;
  finally
    Free;
  end;

  if GetVersionFromFile(TempDir) < 31 then
  begin
    ErrMsg(rsDBNotSupport);
    DeleteDirectory(TempDir, False);
    Exit(mrCancel);
  end;

  FFmMan.LoadFromDir(TempDir);
  FRpMan.LoadFromDir(TempDir);
  FScrMan.LoadFromDir(TempDir);
  FImgMan.LoadFromDir(TempDir);
  FMain.LoadFromDir(TempDir);
  ConvertToDXMainVersion2(FMain, FFmMan);
  ScaleForms(FFmMan, FMain.DesignTimePPI);
  ScaleReports(FRpMan, FMain.DesignTimePPI, Screen.PixelsPerInch);
  FillForms;
  FillReports;
  FillExtModules;
  FillUserModules;

  SrcForms.Clear;
  DepForms.Clear;
  DepReps.Clear;
  DepSources.Clear;

  DeleteDirectory(TempDir, False);

  Result := ShowModal;
  if Result = mrOk then
  begin
    if not DoMerge then Result := mrCancel;
  end;

  except
    on E: Exception do
    begin
      Failed := True;
      ErrMsg(rsMergeProjectsError + ExceptionToString(E, True, False));
    end;
  end;
end;

end.

