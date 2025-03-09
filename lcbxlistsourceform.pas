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

unit LCbxListSourceForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Spin, ButtonPanel, dxctrls, strconsts, DialogGrid, CheckTreeView,
  LclType, Buttons, ExtCtrls;

{ TLCbxListSourceFm }

type
  TLCbxListSourceFm = class(TForm)
    MoreBn: TBitBtn;
    ButtonPanel1: TButtonPanel;
    ListMsgTxt: TStaticText;
    UpdateTree: TComboBox;
    HideList: TCheckBox;
    HideButton: TCheckBox;
    Fields: TDialogGrid;
    Frm: TComboBox;
    Field: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ListWidth: TSpinEdit;
    RowCnt: TSpinEdit;
    procedure FieldChange(Sender: TObject);
    procedure FieldsCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FrmChange(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure HideButtonChange(Sender: TObject);
    procedure ListMsgTxtClick(Sender: TObject);
    procedure MoreBnClick(Sender: TObject);
  private
    { private declarations }
    FLCbx: TdxLookupComboBox;
    procedure FillForms;
    procedure FillFields;
    procedure FillListFields;
    function GetForm: TdxForm;
    function GetField: TComponent;
    procedure SaveFields;
    procedure SetForm;
    procedure SetField;
    procedure LoadFields;
    function ValidateFields: Boolean;
    function Validate: Boolean;
    procedure SetVisibleSearchColumn;
    procedure CheckListSource;
  public
    { public declarations }
    function ShowForm(LCbx: TdxLookupComboBox): Integer;
  end;

var
  LCbxListSourceFm: TLCbxListSourceFm;

function ShowLCbxListSourceForm(LCbx: TdxLookupComboBox): Integer;

implementation

uses
  apputils, formmanager, reportmanager, dxreports, dximages, dxfiles, helpmanager,
  mytypes, designerframe, templatefieldsform, lcbxlistsourcemoreform;

{$R *.lfm}

function ShowLCbxListSourceForm(LCbx: TdxLookupComboBox): Integer;
begin
  if LCbxListSourceFm = nil then
  	LCbxListSourceFm := TLCbxListSourceFm.Create(Application);
  Result := LCbxListSourceFm.ShowForm(LCbx);
end;

// Проверяет, используются ли поля объекта в запросах и отчетах
function ObjectFieldsExistsInReports(FieldId: Integer; var RpName: String): Boolean;
var
  i, j, z: Integer;
  RD: TReportData;
  Sr: TRpSource;
  rF: TRpField;
begin
  Result := False;
	for i := 0 to ReportMan.ReportCount - 1 do
  begin
    RD := ReportMan.Reports[i];
    for j := 0 to RD.Sources.Count - 1 do
    begin
      Sr := RD.Sources[j]^;
      for z := 0 to Sr.Fields.Count - 1 do
      begin
        rF := Sr.Fields[z]^;
        if (rF.FId = FieldId) and (rF.Src <> nil) then
        begin
          RpName := RD.Name;
        	Exit(True);
        end;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

{ TLCbxListSourceFm }

procedure TLCbxListSourceFm.FormShow(Sender: TObject);
begin
  Frm.SetFocus;
end;

procedure TLCbxListSourceFm.FrmChange(Sender: TObject);
begin
  FillFields;
  FillListFields;
  Fields.RowCount := 1;
  SetVisibleSearchColumn;
  LCbxListSourceMoreFm.Clear;
  CheckListSource;
end;

procedure TLCbxListSourceFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('linkform');
end;

procedure TLCbxListSourceFm.HideButtonChange(Sender: TObject);
begin
  UpdateTree.Enabled := not HideButton.Checked;
end;

procedure TLCbxListSourceFm.ListMsgTxtClick(Sender: TObject);
begin
  MoreBn.Click;
end;

procedure TLCbxListSourceFm.MoreBnClick(Sender: TObject);
begin
  LCbxListSourceMoreFm.ShowForm;
  CheckListSource;
end;

procedure TLCbxListSourceFm.FieldsCommand(Sender: TObject;
  Cmd: TDialogGridCommand);
begin
  case Cmd of
  	dgcAppend:
      begin
        Fields.RowCount := Fields.RowCount + 1;
        Fields.Row := Fields.RowCount - 1;
        Fields.Cells[1, Fields.Row] := '100';
        Fields.Cells[2, Fields.Row] := '0';
      end;
    dgcDelete: Fields.DeleteRow(Fields.Row);
  end;
end;

procedure TLCbxListSourceFm.FieldChange(Sender: TObject);
begin
  SetVisibleSearchColumn;
end;

procedure TLCbxListSourceFm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ModalResult = mrOk then
	  CanClose := Validate;
end;

procedure TLCbxListSourceFm.FormCreate(Sender: TObject);
begin
  Caption := rsLinkToForm;
  Label1.Caption := rsForm;
  Label2.Caption := rsField;
  Label3.Caption := rsAdditionalFieldsInList;
  Label4.Caption := rsExtraListWidth;
  Label5.Caption := rsRowCountInList;
  Label7.Caption := rsTreeInListWindow;
  ListMsgTxt.Caption := rsListSourceClickMoreBn;
  UpdateTree.Items[0] := rsRefreshTreeFirstShow;
  UpdateTree.Items[1] := rsRefreshTreeEveryShow;
  Fields.Columns[0].Title.Caption := rsField;
	Fields.Columns[1].Title.Caption := rsColumnWidth;
  Fields.Columns[2].Title.Caption := rsIncludeInSearch;
  HideList.Caption := rsHideList;
  HideButton.Caption := rsHideButton;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;

  AddFormHeight(Self);
end;

procedure TLCbxListSourceFm.FillForms;
begin
  //FormMan.FormsToList(Frm.Items);
  FormMan.SourceFormsToList(Frm.Items);
end;

procedure TLCbxListSourceFm.FillFields;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  Field.Clear;
  Fm := GetForm;
  if Fm = nil then Exit;
  SL := TStringListUtf8.Create;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if HasFId(C) and ((C is TdxEdit) or (C is TdxComboBox) or
      ((C is TdxMemo) and (GetFieldSize(C) > 0)) or
    	(C is TdxCounter) or (C is TdxCalcEdit) or (C is TdxRecordId)) then
      SL.AddObject(GetFieldName(C), C)
  end;
  SL.Sort;
  Field.Items := SL;
  SL.Free;
end;

procedure TLCbxListSourceFm.FillListFields;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  Fm := GetForm;
  if Fm = nil then Exit;
  SL := TStringListUtf8.Create;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if IsField(C) and not ((C is TdxDBImage) or (C is TdxFile)) then
      SL.AddObject(GetFieldName(C), C)
    else if C is TdxRecordId then
      SL.AddObject(GetFieldName(C), C)
  end;
  SL.Sort;
  Fields.Columns[0].PickList := SL;
  SL.Free;
end;

function TLCbxListSourceFm.GetForm: TdxForm;
begin
  Result := nil;
  with Frm do
		if ItemIndex >= 0 then
    	Result := TdxForm(Items.Objects[ItemIndex]);
end;

function TLCbxListSourceFm.GetField: TComponent;
begin
	Result := nil;
  with Field do
		if ItemIndex >= 0 then
    	Result := TComponent(Items.Objects[ItemIndex]);
end;

procedure TLCbxListSourceFm.SaveFields;
var
  L: TLCbxListFields;
  i, N: Integer;
  C: TComponent;
  LF: TLCbxListField;
begin
  L := FLCbx.ListFields;
  L.Clear;
  for i := 1 to Fields.RowCount - 1 do
  begin
    C := TComponent(Fields.Objects[0, i]);
    LF := L.Add;
    LF.FieldId := GetId(C);
    if TryStrToInt(Fields.Cells[1, i], N) then
	    LF.Width := N;
    LF.Searchable := Fields.Columns[2].Visible and Str2Bool(Fields.Cells[2, i]);
  end;
end;

procedure TLCbxListSourceFm.SetForm;
var
  Fm: TdxForm;
begin
  Fm := FormMan.FindForm(FLCbx.SourceTId);
  with Frm do
  	ItemIndex := Items.IndexOfObject(Fm);
end;

procedure TLCbxListSourceFm.SetField;
var
  Fm: TdxForm;
  C: TComponent;
begin
	Fm := GetForm;
  if Fm = nil then Exit;
  C := FindById(Fm, FLCbx.SourceFId);
  with Field do
    ItemIndex := Items.IndexOfObject(C);
end;

procedure TLCbxListSourceFm.LoadFields;
var
  i, r: Integer;
  Fm: TdxForm;
  LF: TLCbxListField;
  C: TComponent;
begin
  Fields.RowCount := 1;
  Fm := GetForm;
  if Fm = nil then Exit;

  r := 1;
  for i := 0 to FLCbx.ListFields.Count - 1 do
  begin
    LF := FLCbx.ListFields[i];
    C := FindById(Fm, LF.FieldId);
    if C = nil then Continue;
		Fields.RowCount := r + 1;
    Fields.Objects[0, r] := C;
    Fields.Cells[0, r] := GetFieldName(C);
    Fields.Cells[1, r] := IntToStr(LF.Width);
    Fields.Cells[2, r] := Bool2Str(LF.Searchable);
    Inc(r);
  end;
end;

function TLCbxListSourceFm.ValidateFields: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Fields.RowCount - 1 do
  begin
    if Fields.Cells[0, i] = '' then
    begin
      ErrMsg(rsFieldNotSel);
      Fields.SetFocus;
			Fields.Row := i; Fields.Col := 0;
      Exit;
    end;
  end;
  Result := True;
end;

function TLCbxListSourceFm.Validate: Boolean;
var
  RpName: String;
begin
  Result := False;
  if Frm.ItemIndex < 0 then
  begin
    Frm.SetFocus;
    ErrMsg(rsFormNotSel);
  end
  else if Field.ItemIndex < 0 then
  begin
    Field.SetFocus;
    ErrMsg(rsFieldNotSel);
  end
  else if (GetForm.Id <> FLCbx.SourceTId) and
  	ObjectFieldsExistsInReports(FLCbx.Id, RpName) then
    ErrMsgFmt(rsCanNotChangeListSourceMsg, [RpName])
  else if not ValidateFields then
  else if not LCbxListSourceMoreFm.Validate then
  else Result := True;

  if Result and (GetForm.Id <> FLCbx.SourceTId) then
    CheckExistsInActions(FLCbx.Owner, renObject, FLCbx.FieldName, LineEnding +
      rsChangeObjSourceActionsMsg);
end;

procedure TLCbxListSourceFm.SetVisibleSearchColumn;
var
  C: TComponent;
  b: Boolean;
  i: Integer;
begin
  C := GetField;
  b := (C = nil) or (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo);
  Fields.Columns[2].Visible := b;
  if not b then
    for i := 1 to Fields.RowCount - 1 do
      Fields.Cells[2, i] := '0';
  LCbxListSourceMoreFm.SetVisibleSearchColumn(b);
end;

procedure TLCbxListSourceFm.CheckListSource;
begin
  if LCbxListSourceMoreFm.HasListSource then
  begin
    Fields.RowCount := 1;
    Fields.Enabled := False;
    ListMsgTxt.Visible := True;

    if not LCbxListSourceMoreFm.ReadyListSource then
    begin
      ListMsgTxt.Caption := rsListSourceNotReady;
      ListMsgTxt.Font.Color := clRed;
    end
    else
    begin
      ListMsgTxt.Caption := rsListSourceClickMoreBn;
      ListMsgTxt.Font.Color := clGray;
    end;
  end
  else
  begin
    ListMsgTxt.Visible := False;
    Fields.Enabled := True;
  end;
end;

function TLCbxListSourceFm.ShowForm(LCbx: TdxLookupComboBox): Integer;
var
  Fm: TdxForm;
  OldFId, OldTId: Integer;
begin
  FLCbx := LCbx;
  OldTId := LCbx.SourceTId;
  OldFId := LCbx.SourceFId;
  Fm := TdxForm(LCbx.Owner);

  FillForms;
  SetForm;
  FillFields;
  SetField;
  FillListFields;
  LoadFields;
  ListWidth.Value := LCbx.ListWidthExtra;
  RowCnt.Value := LCbx.DropDownCount;
  HideList.Checked := LCbx.HideList;
  HideButton.Checked := LCbx.HideButton;
  if LCbx.UpdateTree then UpdateTree.ItemIndex := 1
  else UpdateTree.ItemIndex := 0;

  if LCbxListSourceMoreFm = nil then
    LCbxListSourceMoreFm := TLCbxListSourceMoreFm.Create(Application);
  LCbxListSourceMoreFm.Load(LCbx);

  SetVisibleSearchColumn;
  CheckListSource;

  Result := ShowModal;
	if Result <> mrOk then Exit;

  if OldTId <> GetForm.Id then
  	ResetLookupComponent(LCbx);

  LCbx.SourceTId := GetForm.Id;
  LCbx.SourceFId := GetId(GetField);
  LCbx.ListWidthExtra := ListWidth.Value;
  LCbx.DropDownCount := RowCnt.Value;
  LCbx.HideList := HideList.Checked;
  LCbx.HideButton := HideButton.Checked;
  LCbx.UpdateTree := UpdateTree.ItemIndex = 1;

  LCbxListSourceMoreFm.Save(LCbx);

  if LCbx.ListSource = 0 then SaveFields;

  if OldFId <> GetId(GetField) then
  begin
    if (Fm.ParentField = LCbx.Id) and (not IsCorrectParentField(Fm, LCbx)) then
    	Fm.ParentField := 0;
    DesignFr.NeedAllCalcRecordSize:=True;
  end;
  if OldTId <> GetForm.Id then
    UpdateTemplateFieldsForm;
end;

end.

