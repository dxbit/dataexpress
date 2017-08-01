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
unit ActionPans;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, DxActions, DxCtrls,
  strconsts, DXReports, MyTypes, editbtn, Graphics, Grids, Menus, LclType;

type

  { TBasicActionPanel }

  TBasicActionPanel = class(TCustomPanel)
    procedure ExecCondClick(Sender: TObject);
  private
    FForm: TdxForm;
    FExecCond: TEditButton;
    FConfirmMsg: TEdit;
    FMsg: TEdit;
    FSaveRecord: TCheckBox;
  protected
    FAction: TBasicAction;
    function CreateControls: TControl; virtual;
    function InnerSave: String; virtual;
  public
    constructor Create(AOwner: TComponent; ActionClass: TBasicActionClass); virtual;
    destructor Destroy; override;
    procedure Load(const Xml: String);
    function Save: String;
    procedure Init; virtual;
    function Validate: Boolean; virtual;
    property Form: TdxForm read FForm write FForm;
  end;

  { TGotoFormPanel }

  TGotoFormPanel = class(TBasicActionPanel)
  private
    FForms: TComboBox;
  protected
    function CreateControls: TControl; override;
    function InnerSave: String; override;
  public
    procedure Init; override;
    function Validate: Boolean; override;
  end;

  { TPrintActionPanel }

  TPrintActionPanel = class(TBasicActionPanel)
    procedure ExprButtonClick(Sender: TObject);
  private
    FTempls: TComboBox;
    FExpr: TEditButton;
  protected
    function CreateControls: TControl; override;
    function InnerSave: String; override;
  public
    procedure Init; override;
    function Validate: Boolean; override;
  end;

  { TMassCalcActionPanel }

  TMassCalcActionPanel = class(TBasicActionPanel)
    procedure ExprButtonClick(Sender: TObject);
    procedure FilterButtonClick(Sender: TObject);
    procedure FormsChange(Sender: TObject);
  private
    FForms: TComboBox;
    FFilter: TEditButton;
    FFields: TComboBox;
    FExpr: TEditButton;
    procedure FillFields;
    function CurForm: TdxForm;
    function CurField: TComponent;
  protected
    function CreateControls: TControl; override;
    function InnerSave: String; override;
  public
    procedure Init; override;
    function Validate: Boolean; override;
  end;

  { TOpenReportActionPanel }

  TOpenReportActionPanel = class(TBasicActionPanel)
  private
    FReps: TComboBox;
    function GetRp: TReportData;
  protected
    function CreateControls: TControl; override;
    function InnerSave: String; override;
  public
    procedure Init; override;
    function Validate: Boolean; override;
  end;

  TSaveChangesActionPanel = class(TBasicActionPanel);

  TUserMonitorActionPanel = class(TBasicActionPanel);

  { TCallFuncActionPanel }

  TCallFuncActionPanel = class(TBasicActionPanel)
    procedure ExprButtonClick(Sender: TObject);
  private
    FExpr: TEditButton;
  protected
    function CreateControls: TControl; override;
    function InnerSave: String; override;
  public
    procedure Init; override;
    function Validate: Boolean; override;
  end;

  { TClearFieldsActionPanel }

  TClearFieldsActionPanel = class(TBasicActionPanel)
  private
    FGrid: TStringGrid;
    FPopup: TPopupMenu;
    procedure GridPickListSelect(Sender: TObject);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure MenuHandler(Sender: TObject);
  protected
    function CreateControls: TControl; override;
    function InnerSave: String; override;
  public
    procedure Init; override;
    function Validate: Boolean; override;
  end;

implementation

uses
  formmanager, apputils, exprform, dxfiles, dximages, reportmanager;

{ TClearFieldsActionPanel }

procedure TClearFieldsActionPanel.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
  	0:
      begin
        FGrid.RowCount := FGrid.RowCount + 1;
        FGrid.Row := FGrid.RowCount - 1;
      end;
    1:
      begin
        if Confirm(rsDeleteField, rsAreYouSure) = mrYes then
	        FGrid.DeleteRow(FGrid.Row);
      end;
  end;
end;

procedure TClearFieldsActionPanel.GridSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if Editor is TPickListCellEditor then
  	with TPickListCellEditor(Editor) do
    	Style := csDropDownList
  else
  	Editor := nil;
end;

procedure TClearFieldsActionPanel.GridPickListSelect(Sender: TObject);
begin
  with TPickListCellEditor(FGrid.Editor) do
  	FGrid.Objects[FGrid.Col, FGrid.Row] := Items.Objects[ItemIndex];
end;

function TClearFieldsActionPanel.CreateControls: TControl;
var
  L: TLabel;
  C: TGridColumn;
begin
  FPopup := TPopupMenu.Create(Self);
  FPopup.Items.Add( CreateMenuItem(FPopup, rsAppend, 0, ShortCut(VK_INSERT, []), @MenuHandler, 'add16') );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsDelete, 1, ShortCut(VK_DELETE, [ssCtrl]), @MenuHandler, 'delete16') );
  L := CreateLabel(Self, rsSelectFields);
  FGrid := TStringGrid.Create(Self);
  with FGrid do
  begin
    Parent := Self;
    AutoFillColumns:=True;
    RowCount := 1;
    Flat := True;
    AlternateColor := $00EEEEEE;
    C := Columns.Add;
    PopupMenu := FPopup;
    FixedCols := 0; FixedRows := 0;
    Height := 100;
    Options := Options + [goEditing, goThumbTracking, goDrawFocusSelected] - [goRangeSelect];
    OnSelectEditor:=@GridSelectEditor;
    OnPickListSelect:=@GridPickListSelect;
  end;
  AnchorCtrl(FGrid, L, 1);
  Result := FGrid;
end;

function TClearFieldsActionPanel.InnerSave: String;
var
  i: Integer;
  S: String;
begin
  S := '';
  for i := 0 to FGrid.RowCount - 1 do
  begin
    S := S + IntToStr(GetId(TComponent(FGrid.Objects[0, i])));
    if i < FGrid.RowCount - 1 then
    	S := S + ';'
  end;
  Result := 'fields="' + S + '"';
end;

procedure TClearFieldsActionPanel.Init;
var
  i, r: Integer;
  C: TComponent;
  Fields: TIntList;
  SL: TStringListUtf8;
begin
  inherited Init;
  r := 0;
  Fields := TClearFieldsAction(FAction).Fields;
  for i := 0 to Fields.Count - 1 do
  begin
    C := FindById(FForm, Fields[i]);
    if C = nil then Continue;
    FGrid.RowCount := r + 1;
    FGrid.Cells[0, r] := GetFieldName(C);
    FGrid.Objects[0, r] := C;
    Inc(r);
  end;

  SL := TStringListUtf8.Create;
  for i := 0 to FForm.ComponentCount - 1 do
  begin
    C := FForm.Components[i];
    if IsField(C) then SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  FGrid.Columns[0].PickList := SL;
  SL.Free;
end;

function TClearFieldsActionPanel.Validate: Boolean;
var
  i: Integer;
begin
  Result:=inherited Validate;
  for i := 0 to FGrid.RowCount - 1 do
  begin
    if FGrid.Cells[0, i] = '' then
    begin
      ErrMsg(rsFieldNotSel);
      FGrid.Col := 0; FGrid.Row := i;
      Result := False;
      FGrid.SetFocus;
      Break;
    end;
  end;
end;

{function CreateLabel(AOwner: TWinControl; const aCaption: String): TLabel;
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
end;      }

{ TCallFuncActionPanel }

procedure TCallFuncActionPanel.ExprButtonClick(Sender: TObject);
var
  S: TCaption;
begin
  S := FExpr.Text;
  if ExprFm.ShowForm(rsExpression, nil, S, Form, nil, nil, nil, 'expressions') then
    FExpr.Text := S;
end;

function TCallFuncActionPanel.CreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsExpression);
  FExpr := TEditButton.Create(Self);
  FExpr.Parent := Self;
  FExpr.ButtonCaption:='...';
  FExpr.OnButtonClick:=@ExprButtonClick;
  AnchorCtrl(FExpr, L, 1);
  Result := FExpr;
end;

function TCallFuncActionPanel.InnerSave: String;
begin
  Result:='expression="' + StrToXml(FExpr.Text) + '"';
end;

procedure TCallFuncActionPanel.Init;
begin
  inherited Init;
  FExpr.Text := TCallFuncAction(FAction).Expression;
end;

function TCallFuncActionPanel.Validate: Boolean;
begin
  Result:=inherited Validate;
end;

{ TOpenReportActionPanel }

function TOpenReportActionPanel.GetRp: TReportData;
begin
  Result := nil;
  with FReps do
    if ItemIndex >= 0 then
      Result := TReportData(Items.Objects[ItemIndex]);
end;

function TOpenReportActionPanel.CreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsReport);
  FReps := TComboBox.Create(Self);
  FReps.Parent := Self;
  FReps.Style := csDropDownList;
  AnchorCtrl(FReps, L, 1);
  ReportMan.GetReports(FReps.Items);
  Result := FReps;
end;

procedure TOpenReportActionPanel.Init;
var
  RD: TReportData;
begin
  inherited Init;
  RD := ReportMan.FindReport(TOpenReportAction(FAction).RpId);
  with FReps do
    ItemIndex := Items.IndexOfObject(RD);
end;

function TOpenReportActionPanel.InnerSave: String;
begin
  Result:='rpid="' + IntToStr(GetRp.Id) + '"';
end;

function TOpenReportActionPanel.Validate: Boolean;
begin
  Result:=False;
  if FReps.ItemIndex < 0 then
    ErrMsg(rsReportNotSel)
  else
    Result := True;
end;

{ TMassCalcActionPanel }

procedure TMassCalcActionPanel.ExprButtonClick(Sender: TObject);
var
  S: TCaption;
begin
  S := FExpr.Text;
  if ExprFm.ShowForm(rsExpression, nil, S, CurForm, nil, nil, nil, 'expressions') then
    FExpr.Text := S;
end;

procedure TMassCalcActionPanel.FilterButtonClick(Sender: TObject);
var
  S: TCaption;
begin
  S := FFilter.Text;
  if ExprFm.ShowForm(rsSelCond, nil, S, FForm, CurForm, nil, nil, 'selcond') then
    FFilter.Text := S;
end;

procedure TMassCalcActionPanel.FormsChange(Sender: TObject);
begin
  FillFields;
end;

procedure TMassCalcActionPanel.FillFields;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  FFields.Clear;
  Fm := CurForm;
  if Fm = nil then Exit;
  SL := TStringListUtf8.Create;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (not IsField(C)) or (C is TdxFile) or (C is TdxDBImage) then Continue;
    SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  FFields.Items := SL;
  SL.Free;
end;

function TMassCalcActionPanel.CurForm: TdxForm;
begin
  Result := nil;
  if FForms.ItemIndex >= 0 then
    Result := TdxForm(FForms.Items.Objects[FForms.ItemIndex]);
end;

function TMassCalcActionPanel.CurField: TComponent;
begin
  Result := nil;
  if FFields.ItemIndex >= 0 then
    Result := TComponent(FFields.Items.Objects[FFields.ItemIndex]);
end;

function TMassCalcActionPanel.CreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsForm);
  FForms := TComboBox.Create(Self);
  FForms.Parent := Self;
  FForms.Style := csDropDownList;
  AnchorCtrl(FForms, L, 1);
  FForms.OnChange:=@FormsChange;
  FormMan.FormsToList(FForms.Items);

  L := CreateLabel(Self, rsSelCond);
  AnchorCtrl(L, FForms, 4);
  FFilter := TEditButton.Create(Self);
  FFilter.Parent := Self;
  AnchorCtrl(FFilter, L, 1);
  FFilter.Button.Caption := '...';
  FFilter.OnButtonClick:=@FilterButtonClick;

  L := CreateLabel(Self, rsField);
  AnchorCtrl(L, FFilter, 4);
  FFields := TComboBox.Create(Self);
  FFields.Parent := Self;
  FFields.Style := csDropDownList;
  AnchorCtrl(FFields, L, 1);

  L := CreateLabel(Self, rsExpression);
  AnchorCtrl(L, FFields, 4);
  FExpr := TEditButton.Create(Self);
  FExpr.Parent := Self;
  AnchorCtrl(FExpr, L, 1);
  FExpr.Button.Caption := '...';
  FExpr.OnButtonClick:=@ExprButtonClick;

  Result := FExpr;
end;

procedure TMassCalcActionPanel.Init;
var
  Fm: TdxForm;
  C: TComponent;
  A: TMassCalcAction;
begin
  inherited Init;
  A := TMassCalcAction(FAction);
  Fm := FormMan.FindForm(A.FormId);
  FForms.ItemIndex := FForms.Items.IndexOfObject(Fm);
  FForms.OnChange(FForms);
  if Fm <> nil then
    C := FindById(Fm, A.FieldId);
  FFields.ItemIndex := FFields.Items.IndexOfObject(C);
  FFilter.Text := A.Filter;
  FExpr.Text := A.Expression;
end;

function TMassCalcActionPanel.InnerSave: String;
begin
  Result:='formid="' + IntToStr(CurForm.Id) + '" filter="' +
    StrToXml(FFilter.Text) + '" fieldid="' + IntToStr(GetId(CurField)) + '" expression="' +
    StrToXml(FExpr.Text) + '"';
end;

function TMassCalcActionPanel.Validate: Boolean;
begin
  Result:=False;
  if FForms.ItemIndex < 0 then
    ErrMsg(rsFormNotSel)
  else if FFields.ItemIndex < 0 then
    ErrMsg(rsFieldNotSel)
  else
    Result := True;
end;

{ TPrintActionPanel }

procedure TPrintActionPanel.ExprButtonClick(Sender: TObject);
var
  S: TCaption;
begin
  S := FExpr.Text;
  if ExprFm.ShowForm(rsExpression, nil, S, Form, nil, nil, nil, 'expression') then
    FExpr.Text := S;
end;

function TPrintActionPanel.CreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsTemplateFile);
  FTempls := TComboBox.Create(Self);
  FTempls.Parent := Self;
  AnchorCtrl(FTempls, L, 1);
  FTempls.AutoComplete:=True;
  FTempls.Clear;
  GetTemplates(FTempls.Items);

  L := CreateLabel(Self, rsOrExpression);
  AnchorCtrl(L, FTempls, 4);
  FExpr := TEditButton.Create(Self);
  FExpr.Parent := Self;
  AnchorCtrl(FExpr, L, 1);
  FExpr.Button.Caption:='...';
  FExpr.OnButtonClick:=@ExprButtonClick;

  Result := FExpr;
end;

procedure TPrintActionPanel.Init;
begin
  inherited Init;
  with TPrintAction(FAction) do
  begin
    FTempls.Text := TemplateFile;
    FExpr.Text := Expression;
  end;
end;

function TPrintActionPanel.InnerSave: String;
begin
  Result:='template="' + FTempls.Text + '" expression="' +
    StrToXml(FExpr.Text) + '">';
end;

function TPrintActionPanel.Validate: Boolean;
begin
  Result:=False;
  if (Trim(FTempls.Text) = '') and (Trim(FExpr.Text) = '') then
    ErrMsg(rsTemplateUnknown)
  else
    Result := True;
end;

{ TGotoFormPanel }

function TGotoFormPanel.CreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsForm);
  FForms := TComboBox.Create(Self);
  FForms.Style:=csDropDownList;
  FForms.Parent := Self;
  AnchorCtrl(FForms, L, 1);
  FormMan.FormsToList(FForms.Items);

  Result := FForms;
end;

procedure TGotoFormPanel.Init;
var
  Fm: TdxForm;
begin
  inherited Init;
  Fm := FormMan.FindForm(TGotoFormAction(FAction).FormId);
  FForms.ItemIndex := FForms.Items.IndexOfObject(Fm);
end;

function TGotoFormPanel.InnerSave: String;
var
  Fm: TdxForm;
begin
  Fm := TdxForm(FForms.Items.Objects[FForms.ItemIndex]);
  Result:='formid="' + IntToStr(Fm.Id) + '"';
end;

function TGotoFormPanel.Validate: Boolean;
begin
  Result := False;
  if FForms.ItemIndex < 0 then
    ErrMsg(rsFormNotSel)
  else
    Result := True;
end;

{ TBasicActionPanel }

procedure TBasicActionPanel.ExecCondClick(Sender: TObject);
var
  S: TCaption;
begin
  S := FExecCond.Text;
  if ExprFm.ShowForm(rsExecCond, nil, S, FForm, nil, nil, nil, 'expression') then
    FExecCond.Text := S;
end;

function TBasicActionPanel.CreateControls: TControl;
begin
  Result := nil;
end;

function TBasicActionPanel.InnerSave: String;
begin
  Result := '';
end;

constructor TBasicActionPanel.Create(AOwner: TComponent;
  ActionClass: TBasicActionClass);
var
  L: TLabel;
  LastCtrl: TControl;
begin
  inherited Create(AOwner);
  FAction := ActionClass.Create;

  BevelInner := bvNone;
  BevelOuter := bvNone;
  Caption := '';
  LastCtrl := CreateControls;
  if LastCtrl <> nil then
  begin
    L := CreateLabel(Self, rsCommonProps);
    AnchorCtrl(L, LastCtrl, 8);
    L.Font.Style:=[fsBold];
    LastCtrl := L;
  end;
  L := CreateLabel(Self, rsExecCond);
  if LastCtrl <> nil then
    AnchorCtrl(L, LastCtrl, 8);
  FExecCond := TEditButton.Create(Self);
  FExecCond.Parent := Self;
  AnchorCtrl(FExecCond, L, 1);
  FExecCond.Button.Caption := '...';
  FExecCond.OnButtonClick:=@ExecCondClick;

  L := CreateLabel(Self, rsConfirmationMsg);
  AnchorCtrl(L, FExecCond, 4);
  FConfirmMsg := TEdit.Create(Self);
  FConfirmMsg.Parent := Self;
  AnchorCtrl(FConfirmMsg, L, 1);

  L := CreateLabel(Self, rsSuccessMsg);
  AnchorCtrl(L, FConfirmMsg, 4);
  FMsg := TEdit.Create(Self);
  FMsg.Parent := Self;
  AnchorCtrl(FMsg, L, 1);

  FSaveRecord := TCheckBox.Create(Self);
  FSaveRecord.Parent := Self;
  FSaveRecord.Caption := rsSaveRecord;
  AnchorCtrl(FSaveRecord, FMsg, 4);
end;

destructor TBasicActionPanel.Destroy;
begin
  FAction.Free;
  inherited Destroy;
end;

procedure TBasicActionPanel.Load(const Xml: String);
begin
  FAction.Load(Xml);
end;

function TBasicActionPanel.Save: String;
begin
  Result := '<action execcond="' + StrToXml(FExecCond.Text) + '" confirm="' +
    StrToXml(FConfirmMsg.Text) + '" msg="' + StrToXml(FMsg.Text) + '" saverecord="' +
    Bool2Str(FSaveRecord.Checked) + '" ' + InnerSave + '/>';
end;

procedure TBasicActionPanel.Init;
begin
  FExecCond.Text := FAction.ExecCond;
  FConfirmMsg.Text := FAction.ConfirmMsg;
  FMsg.Text := FAction.Msg;
  FSaveRecord.Checked := FAction.SaveRecord;
end;

function TBasicActionPanel.Validate: Boolean;
begin
  Result := True;
end;

end.

