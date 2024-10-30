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

unit PropDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Dialogs, LCLType, dxctrls, strconsts,
  DXReports, pivotgrid, Forms, StdCtrls, ButtonPanel, ExtCtrls, Graphics;

type

  { TFieldNameDlg }

  TFieldNameDlg = class(TForm)
    procedure HelpBnClick(Sender: TObject);
  private
    FFlNm: TEdit;
    FCmpNm: TEdit;
    FBns: TButtonPanel;
    FCmp: TComponent;
  protected
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function CloseQuery: boolean; override;
    function ShowForm(C: TComponent): Integer;
  end;

  { TComponentNameDlg }

  TComponentNameDlg = class(TForm)
    procedure HelpBnClick(Sender: TObject);
  private
    FCmpNm: TEdit;
    FBns: TButtonPanel;
    FCmp: TComponent;
  protected
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function CloseQuery: boolean; override;
    function ShowForm(C: TComponent): Integer;
  end;

  { TFormNameDlg }

  TFormNameDlg = class(TForm)
    procedure HelpBnClick(Sender: TObject);
  private
    FFmNm: TEdit;
    FRecsCaption: TEdit;
    FRecCaption: TEdit;
    FCmpNm: TEdit;
    FBns: TButtonPanel;
    FCmp: TdxForm;
    procedure FmNmChange(Sender: TObject);
    procedure RecsCaptionChange(Sender: TObject);
  protected
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function CloseQuery: boolean; override;
    function ShowForm(aFm: TdxForm): Integer;
  end;

  { TQueryNameDlg }

  TQueryNameDlg = class(TForm)
    procedure HelpBnClick(Sender: TObject);
  private
    FNm: TEdit;
    FCmpNm: TEdit;
    FBns: TButtonPanel;
    FCmp: TdxQueryGrid;
    FRD: TReportData;
  protected
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function CloseQuery: boolean; override;
    function ShowForm(C: TdxQueryGrid): Integer;
  end;

  { TReportNameDlg }

  TReportNameDlg = class(TForm)
    procedure HelpBnClick(Sender: TObject);
  private
    FRpNm: TEdit;
    FBns: TButtonPanel;
    FRD: TReportData;
  protected
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function CloseQuery: boolean; override;
    function ShowForm(var RpName: String; aRD: TReportData): Integer;
  end;

  { TLabelCaptionDlg }

  TLabelCaptionDlg = class(TForm)
  private
    FLabel: TdxLabel;
    FEdit: TEdit;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function ShowForm(ALabel: TdxLabel): Integer;
    function CloseQuery: boolean; override;
  end;

  { TFieldSizeDlg }

  TFieldSizeDlg = class(TForm)
  private
    FCmp: TComponent;
    FEdit: TEdit;
    FSizeChanged: Boolean;
    FUnlim: TCheckBox;
    procedure HelpBnClick(Sender: TObject);
    procedure UnlimChange(Sender: TObject);
    function CheckUseMemo(out Msg: String): Boolean;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function ShowForm(C: TComponent): Integer;
    function CloseQuery: boolean; override;
    property SizeChanged: Boolean read FSizeChanged;
  end;

function FormNameDlg(Fm: TdxForm): Integer;
function FieldNameDlg(C: TComponent): Integer;
//function PrecissionDlg(C: TComponent): Boolean;
function ControlCaptionDlg(Control: TControl): Integer;
function QueryNameDlg(C: TComponent): Integer;
//function ThumbSizeDlg(C: TdxDBImage): Boolean;
function QuerySourceDlg(C: TComponent): Integer;
procedure QueryGridDlg(C: TdxQueryGrid);
function FieldSizeDlg(C: TComponent): Boolean;
function LookupFilterDlg(C: TComponent): Integer;
function ShowExprDlg(C: TComponent): Integer;
//procedure DelimDlg(C: TdxMemo);
procedure QueryStyleDlg(C: TdxQueryGrid);
procedure QueryCalcDlg(C: TComponent);
function QueryFilterDlg(C: TComponent): Integer;
function DefaultValueDlg(C: TComponent): Integer;
procedure QueryColoringDlg(C: TComponent);
//procedure CheckExprDlg(C: TdxCheckBox);
function ShowCheckExprDlg(C: TComponent): Integer;
//procedure ButtonNameDlg(C: TdxButton);
procedure SelectQueryDlg(Cmp: TdxPivotGrid);
function ComponentNameDlg(C: TComponent): Integer;
function ReportNameDlg(const aCaption: String; var aName: String; aRD: TReportData): Integer;
function ShowLabelCaptionDlg(ALabel: TdxLabel): Integer;
function FontDlg(C: TControl): Integer;

implementation

uses
  reportmanager, ReportForm, newgridform, inputform, LazUtf8,
  exprform, apputils, formmanager, rpstyleform, QueryCalcForm, querycoloringform,
  selectform, dxusers, mainform, helpmanager, appsettings, mytypes,
  fontform, designerframe, templatefieldsform, sqlform;


function ShowLabelCaptionDlg(ALabel: TdxLabel): Integer;
begin
  with TLabelCaptionDlg.CreateNew(nil) do
  try
    Result := ShowForm(ALabel);
  finally
    Free;
  end;
end;

function FontDlg(C: TControl): Integer;
var
  DefFont: TFont;
begin
  if not (C is TdxForm) then
  	DefFont := C.Parent.Font
  else
  	DefFont := nil;

  Result := ShowFontForm(C.Font, DefFont);
  if Result = mrOk then
  begin
    if not (C is TdxForm) then
      SetParentFont(C, C.Font.IsEqual(C.Parent.Font));
  end;
end;

function FormNameDlg(Fm: TdxForm): Integer;
begin
  with TFormNameDlg.CreateNew(MainFm) do
  try
    Result := ShowForm(Fm);
  finally
    Free;
  end;
end;

function FieldNameDlg(C: TComponent): Integer;
begin
  with TFieldNameDlg.CreateNew(MainFm) do
  try
    Result := ShowForm(C);
  finally
    Free;
  end;
end;

{function PrecissionDlg(C: TComponent): Boolean;
var
  S: String;
  N: Integer;
begin
  Result := False;
  S := IntToStr(GetPrecission(C));
  Result := InputInt(rsPrecission, rsEnterValue, 'precission', S, 0, 10);
  if Result and TryStrToInt(S, N) then
    SetPrecission(C, N);
end; }

function ControlCaptionDlg(Control: TControl): Integer;
var
  S: String;
begin
  if Control is TdxLabel then
  	Result := ShowLabelCaptionDlg(TdxLabel(Control))
  else
  begin
    S := Control.Caption;
    if InputStr(rsCaption, rsEnterCaption, '', S, False) then
    begin
      Result := mrOk;
      Control.Caption:=S;
    end
    else Result := mrCancel;
  end;
end;

function QueryNameDlg(C: TComponent): Integer;
begin
  with TQueryNameDlg.CreateNew(MainFm) do
  try
    Result := ShowForm(TdxQueryGrid(C));
    if Result = mrOk then TControl(C).Invalidate;
  finally
    Free;
  end;
end;

{function ThumbSizeDlg(C: TdxDBImage): Boolean;
var
  S: String;
  N: Integer;
begin
  Result := False;
  S := IntToStr(C.ThumbSize);
  Result := InputInt(rsThumbSize, rsEnterValue, 'thumbnails', S, 0, 100);
  if Result and TryStrToInt(S, N) then
    C.ThumbSize:=N;
end;    }

function QuerySourceDlg(C: TComponent): Integer;
var
  RD: TReportData;
  Fm: TdxForm;
begin
  RD := ReportMan.FindReport(GetId(C));
  if RD = nil then Exit;

  Fm := TdxForm(C.Owner);
  Result := ShowReportForm(RD, Fm, TdxQueryGrid(C), True);

  if Result = mrOk then
    RD.SetReportChanged;
end;

procedure QueryGridDlg(C: TdxQueryGrid);
var
  RD: TReportData;
begin
  RD := ReportMan.FindReport(C.Id);
  if ShowRpGridForm(C, RD) = mrOk then
    RD.SetReportChanged;
end;

function FieldSizeDlg(C: TComponent): Boolean;
begin
  with TFieldSizeDlg.CreateNew(nil) do
  begin
    Result := ShowForm(C) = mrOk;
    Free;
  end;
end;

function LookupFilterDlg(C: TComponent): Integer;
var
  Expr: String;
  TId: Integer;
begin
  Result := mrCancel;
  TId := GetSourceTId(C);
  if TId = 0 then
  begin
    ErrMsg(rsSourceNotSel);
    Exit;
  end;
  Expr := GetComboFilter(C);
  Result := ShowExprForm(etListFilter, C, Expr, TdxForm(C.Owner), FormMan.FindForm(TId),
    nil, nil);
  if Result = mrOk then
    SetComboFilter(C, Expr);
end;

function ShowExprDlg(C: TComponent): Integer;
var
  S, OldS: String;
begin
  S := GetExpression(C);
  OldS := S;
  Result := ShowExprForm(etFieldExpr, C, S, TdxForm(C.Owner), nil, nil, nil);
  if Result = mrOk then
  begin
    SetExpression(C, S);
    if (OldS <> S) and ((Trim(OldS) = '') or (Trim(S) = '')) then DesignFr.SummaryTree.UpdateTree;
  end;
end;

{procedure DelimDlg(C: TdxMemo);
var
  S: String;
begin
  S := C.Delimiter;
  if InputStr(rsDelimiter, rsEnterValue, '', S, False) then
    C.Delimiter:=S;
end;  }

procedure QueryStyleDlg(C: TdxQueryGrid);
var
  SL: TStringListUtf8;
  i: Integer;
  RD0, RD: TReportData;
begin
  RD0 := ReportMan.FindReport(C.Id);
  if RD0 = nil then Exit;
  SL := TStringListUtf8.Create;
  for i := 0 to ReportMan.ReportCount - 1 do
  begin
    RD := ReportMan.Reports[i];
    if RD.Kind = rkQuery then
      SL.AddObject(RD.Name, RD);
  end;
  SL.Sort;
  ShowRpStyleForm(RD0, SL, rkQuery);
  SL.Free;
end;

procedure QueryCalcDlg(C: TComponent);
var
  RD: TReportData;
begin
  RD := ReportMan.FindReport(GetId(C));
  if RD = nil then Exit;
  if ShowCalcForm(RD, TdxForm(C.Owner), TdxQueryGrid(C), True) = mrOk then
  begin
    RD.SetReportChanged;
    UpdateTemplateFieldsForm;
  end;
end;

function QueryFilterDlg(C: TComponent): Integer;
var
  RD: TReportData;
  S: String;
begin
  Result := mrCancel;
  RD := ReportMan.FindReport(GetId(C));
  if RD = nil then Exit;
  S := RD.Filter;
  Result := ShowExprForm(etOutputFilter, TdxQueryGrid(C), S, TdxForm(C.Owner),
    nil, nil, RD);
  if Result = mrOk then
  begin
    RD.Filter := S;
    RD.SetReportChanged;
  end;
end;

function DefaultValueDlg(C: TComponent): Integer;
var
  S, OldS: String;
begin
  S := GetDefaultValue(C);
  OldS := S;
  Result := ShowExprForm(etDefaultValue, C, S, TdxForm(C.Owner), nil, nil, nil);
  if Result = mrOk then
  begin
    SetDefaultValue(C, S);
    if OldS <> S then DesignFr.SummaryTree.UpdateTree;
  end;
end;

procedure QueryColoringDlg(C: TComponent);
var
  RD: TReportData;
begin
  RD := ReportMan.FindReport(GetId(C));
  if RD = nil then Exit;
  if ShowQueryColoringForm(RD, TdxForm(C.Owner)) = mrOk then
    RD.SetReportChanged;
end;

{procedure CheckExprDlg(C: TdxCheckBox);
var
  S, OldS: String;
begin
  S := GetCheckExpression(C);
  OldS := S;
  if ShowExprForm(etCheck, C, S, TdxForm(C.Owner), nil, nil, nil) then
  begin
    SetCheckExpression(C, S);
    if (OldS <> S) and ((Trim(OldS) = '') or (Trim(S) = '')) then DesignFr.SummaryTree.UpdateTree;
  end;
end;  }

function ShowCheckExprDlg(C: TComponent): Integer;
var
  S, OldS: String;
begin
  S := GetCheckExpression(C);
  OldS := S;
  Result := ShowExprForm(etCheck, C, S, TdxForm(C.Owner), nil, nil, nil);
  if Result = mrOk then
  begin
    SetCheckExpression(C, S);
    if (OldS <> S) and ((Trim(OldS) = '') or (Trim(S) = '')) then DesignFr.SummaryTree.UpdateTree;
  end
end;

procedure SelectQueryDlg(Cmp: TdxPivotGrid);
var
  SL: TStringListUtf8;
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  RD: TReportData;
begin
  SL := TStringListUtf8.Create;
  Fm := TdxForm(Cmp.Owner);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxQueryGrid then
    begin
      RD := ReportMan.FindReport(TdxQueryGrid(C).Id);
      SL.AddObject(RD.Name, RD);
    end;
  end;
  SL.Sort;

  RD := ReportMan.FindReport(Cmp.Id);
  if ShowSelectForm(rsSelectQuery, 'selectquery', SL, SL.IndexOfObject(RD)) = mrOk then
  begin
    RD := TReportData(SL.Objects[SelectFm.Index]);
    if Cmp.Id <> RD.Id then
      Cmp.Clear;
    Cmp.Id:=RD.Id;
  end;
  SL.Free;
end;

function ComponentNameDlg(C: TComponent): Integer;
begin
  with TComponentNameDlg.CreateNew(MainFm) do
  try
    Result := ShowForm(C);
  finally
    Free;
  end;
end;

function ReportNameDlg(const aCaption: String; var aName: String;
  aRD: TReportData): Integer;
begin
  with TReportNameDlg.CreateNew(MainFm) do
  try
    Caption := aCaption;
    Result := ShowForm(aName, aRD);
  finally
    Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function CreateButtonPanel(AOwner: TComponent): TButtonPanel;
begin
  Result := TButtonPanel.Create(AOwner);
  Result.Parent := TWinControl(AOwner);
  Result.ShowButtons:=[pbOk, pbCancel, pbHelp];
  Result.OkButton.Caption := rsOk;
  Result.CancelButton.Caption := rsCancel;
  Result.HelpButton.Caption := rsHelp;
end;

function CreatePanel(AOwner: TComponent): TPanel;
begin
  Result := TPanel.Create(AOwner);
  Result.Parent := TWinControl(AOwner);
  Result.BevelInner := bvNone;
  Result.BevelOuter := bvNone;
  Result.Caption := '';
  Result.Align := alClient;
  Result.BorderSpacing.Top := 8;
  Result.BorderSpacing.Left := 8;
  Result.BorderSpacing.Right := 8;
end;

procedure RenameComponentInRights(Fm: TdxForm; const OldName, NewName: String);
var
  i: Integer;
  R: TdxRole;
  FR: TdxFormRight;
  CR: TdxControlRight;
begin
  for i := 0 to UserMan.Roles.Count - 1 do
  begin
    R := UserMan.Roles[i];
    FR := R.FormRights.FindRight(Fm.Id);
    if FR <> nil then
    begin
      CR := FR.Controls.FindRight(OldName);
      if CR <> nil then CR.Name:=NewName;
    end;
  end;
end;

function TrySetComponentName(C: TComponent; const aName: String): Boolean;
var
  S: String;
begin
  if aName = '' then
  begin
    ErrMsg(rsComponentNameEmpty);
    Exit(False);
  end;
  try
    C.Name := aName;
    Result := True;
  except
    on E: EComponentError do
    begin
      Result := False;
      S := E.Message;
      if Pos('Duplicate', S) > 0 then
        ErrMsg(rsDuplicateComponentName)
      else if Pos('is not a valid component name', S) > 0 then
        ErrMsg(rsInvalidCmpName)
      else
        ErrMsg(S);
    end;
  end;
end;

function CheckDuplicateFormName(const aName, CmpName: String; aFm: TdxForm): Boolean;
var
  i: Integer;
  Fm: TdxForm;
  RD: TReportData;
begin
  Result := True;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if Fm = aFm then Continue;
    if MyUtf8CompareText(Fm.FormCaption, aName) = 0 then
    begin
      ErrMsg(rsFormNameExists);
      Exit(False);
    end
    else if CompareText(Fm.Name, CmpName) = 0 then
    begin
      ErrMsg(rsFormComponentNameExists);
      Exit(False);
    end;
  end;
  for i := 0 to ReportMan.ReportCount - 1 do
  begin
    RD := ReportMan.Reports[i];
    if MyUtf8CompareText(aName, RD.Name) = 0 then
    begin
      ErrMsg(rsReportNameExists);
      Exit(False);
    end;
  end;
end;

{ TFieldSizeDlg }

procedure TFieldSizeDlg.HelpBnClick(Sender: TObject);
begin
  OpenHelp('fieldsize');
end;

procedure TFieldSizeDlg.UnlimChange(Sender: TObject);
begin
  if FUnlim.Checked then
  begin
    FEdit.Text := '';
    FEdit.Enabled := False;
  end
  else
    FEdit.Enabled := True;
end;

function TFieldSizeDlg.CheckUseMemo(out Msg: String): Boolean;
var
  i, j, CmpId: Integer;
  Fm: TdxForm;
  C: TComponent;
begin
  Msg := '';
  CmpId := GetId(FCmp);
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxLookupComboBox then
      begin
        if GetSourceFId(C) = CmpId then
          Msg := Msg + Fm.FormCaption + '->' + GetFieldName(C) + LineEnding;
      end
      else if C is TdxObjectField then
      begin
        if TdxObjectField(C).FieldId = CmpId then
          Msg := Msg + Fm.FormCaption + '->' + GetFieldName(C) + LineEnding;
      end;
    end;
  end;
  SetLength(Msg, Length(Msg) - Length(LineEnding));
  Result := Msg <> '';
  if Result then
    Msg := Format(rsCantMakeUnlimitedMemo, [Spaces + Msg + Spaces]);
end;

constructor TFieldSizeDlg.CreateNew(AOwner: TComponent; Num: Integer);
var
  Pan: TPanel;
  L: TLabel;
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsFieldSize;
  BorderIcons := [biSystemMenu];
  Position := poOwnerFormCenter;
  Pan := CreatePanel(Self);

  L := CreateLabel(Pan, rsEnterValue);
  FEdit := TEdit.Create(Pan);
  FEdit.Parent := Pan;
  FEdit.NumbersOnly := True;
  AnchorCtrl(FEdit, L, 1);

  FUnlim := TCheckBox.Create(Pan);
  FUnlim.Parent := Pan;
  FUnlim.Caption := rsUnlimited;
  FUnlim.OnChange:=@UnlimChange;
  AnchorCtrl(FUnlim, FEdit, 8);

  with CreateButtonPanel(Self) do
    HelpButton.OnClick:=@HelpBnClick;

  Height := 110;
end;

function TFieldSizeDlg.ShowForm(C: TComponent): Integer;
var
  OldSize, NewSize: Integer;
begin
  FCmp := C;
  FUnlim.Visible := C is TdxMemo;
  if FUnlim.Visible then Height := 130
  else Height := 110;
  OldSize := GetFieldSize(C);
  if OldSize = 0 then
    FUnlim.Checked := True
  else
    FEdit.Text := IntToStr(OldSize);
  Result := ShowModal;
  if Result = mrOk then
  begin
    if FEdit.Text = '' then NewSize := 0
    else NewSize := StrToInt(FEdit.Text);
    SetFieldSize(C, NewSize);
    FSizeChanged := OldSize <> NewSize;
  end;
end;

function TFieldSizeDlg.CloseQuery: boolean;
var
  N: Longint;
  Msg: String;
begin
  if ModalResult <> mrOk then Exit;
  Result := False;
  if FUnlim.Checked then
  begin
    if (GetFieldSize(FCmp) > 0) and CheckUseMemo(Msg) then
      ErrMsg(Msg)
    else
      Result := True;
  end
  else if FEdit.Text = '' then
    ErrMsg(rsEnterValue)
  else if not TryStrToInt(FEdit.Text, N) then
    ErrMsgFmt(rsInvalidNumber, [FEdit.Text])
  else if (N < 1) or (N > 2000) then
    ErrMsg(rsFieldSizeRangeMsg)
  else
    Result := True;
  if not Result and FEdit.CanFocus then FEdit.SetFocus;
end;

{ TLabelCaptionDlg }

constructor TLabelCaptionDlg.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsCaption;
  BorderStyle:=bsDialog;
  Position := poOwnerFormCenter;
  L := TLabel.Create(Self);
  L.Parent := Self;
  L.Left := 8; L.Top := 8;
  L.Caption := rsEnterCaption;
  FEdit := TEdit.Create(Self);
  with FEdit do
  begin
		Parent := Self;
    AnchorSideTop.Control := L;
    AnchorSideTop.Side := asrBottom;
    BorderSpacing.Top := 2;
    Text := '';
    Left := 8;
    Width := 328;
  end;
  with TButtonPanel.Create(Self) do
  begin
    Parent := Self;
    ShowButtons := [pbOk, pbCancel];
    OkButton.Caption := rsOk;
    CancelButton.Caption := rsCancel;
  end;
  ClientWidth := 345;
  ClientHeight := 104;
end;

function TLabelCaptionDlg.ShowForm(ALabel: TdxLabel): Integer;
begin
  FLabel := ALabel;
  FEdit.Text := FLabel.Caption;
  Result := ShowModal;
  if Result = mrOk then
  begin
    FLabel.Caption:=FEdit.Text;
    FLabel.FieldName:=FEdit.Text;
  end;
end;

function TLabelCaptionDlg.CloseQuery: boolean;
var
  S: TCaption;
begin
  Result:=inherited CloseQuery;
  if ModalResult <> mrOk then Exit;

  S := FEdit.Text;

  if Trim(S) = '' then
  begin
    ErrMsg(rsLabelEmpty);
    Result := False;
  end
  else if Trim(FLabel.Expression) <> '' then
  begin
    Result := CheckDuplicateFieldName(S, FLabel) and CheckFieldName(S);
  end;
end;

{ TReportNameDlg }

procedure TReportNameDlg.HelpBnClick(Sender: TObject);
begin
  OpenHelp('queryname');
end;

procedure TReportNameDlg.DoShow;
begin
  inherited DoShow;
  FRpNm.SetFocus;
end;

constructor TReportNameDlg.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
  Pan: TPanel;
begin
  inherited CreateNew(AOwner, Num);
  BorderIcons := [biSystemMenu];
  Position := poOwnerFormCenter;
  Pan := CreatePanel(Self);

  L := CreateLabel(Pan, rsReportName);
  FRpNm := TEdit.Create(Pan);
  FRpNm.Parent := Pan;
  AnchorCtrl(FRpNm, L, 1);

  FBns := CreateButtonPanel(Self);
  FBns.HelpButton.OnClick:=@HelpBnClick;

  Height := 110;
end;

function TReportNameDlg.CloseQuery: boolean;
var
  S: TCaption;
begin
  Result:=inherited CloseQuery;
  if ModalResult <> mrOk then Exit;
  S := FRpNm.Text;
  Result := CheckFormName(S) and CheckDuplicateQueryName(S, FRD);
end;

function TReportNameDlg.ShowForm(var RpName: String; aRD: TReportData): Integer;
begin
  FRD := aRD;
  FRpNm.Text := RpName;
  Result := ShowModal;
  if Result = mrOk then RpName := FRpNm.Text;
end;

{ TQueryNameDlg }

procedure TQueryNameDlg.HelpBnClick(Sender: TObject);
begin
  OpenHelp('queryname');
end;

procedure TQueryNameDlg.DoShow;
begin
  inherited DoShow;
  FNm.SetFocus;
end;

constructor TQueryNameDlg.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
  Pan: TPanel;
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsQueryName;
  BorderIcons := [biSystemMenu];
  Position := poOwnerFormCenter;
  Pan := CreatePanel(Self);

  L := CreateLabel(Pan, rsQueryName);
  FNm := TEdit.Create(Pan);
  FNm.Parent := Pan;
  AnchorCtrl(FNm, L, 1);

  L := CreateLabel(Pan, rsComponentName);
  AnchorCtrl(L, FNm, 8);
  FCmpNm := TEdit.Create(Pan);
  FCmpNm.Parent := Pan;
  AnchorCtrl(FCmpNm, L, 1);

  L.Visible:=AppConfig.ExpertMode;
  FCmpNm.Visible := L.Visible;

  FBns := CreateButtonPanel(Self);
  FBns.HelpButton.OnClick:=@HelpBnClick;

  if AppConfig.ExpertMode then Height := 160
  else Height := 110;
end;

function TQueryNameDlg.CloseQuery: boolean;
var
  OldName, NewName, OldRDName, NewRDName: String;
begin
  Result:=inherited CloseQuery;
  if ModalResult <> mrOk then Exit;
  OldRDName := FRD.Name;
  NewRDName := FNm.Text;
  OldName := FCmp.Name;
  NewName := FCmpNm.Text;

  if CheckFormName(NewRDName) and CheckDuplicateQueryName(NewRDName, FRD) and
    TrySetComponentName(FCmp, NewName) then
  begin
    if OldRDName <> NewRDName then
    begin
      FRD.Name := NewRDName;
      RenameInActions(TdxForm(FCmp.Owner), renQuery, OldRDName, NewRDName);
    end;
    if OldName <> NewName then
    begin
      RenameComponentInRights(TdxForm(FCmp.Owner), OldName, NewName);
      RenameInActions(TdxForm(FCmp.Owner), renComponent, OldName, NewName);
    end;
  end
  else Result := False;
end;

function TQueryNameDlg.ShowForm(C: TdxQueryGrid): Integer;
begin
  FCmp := C;
  FRD := ReportMan.FindReport(C.Id);
  FNm.Text:=FRD.Name;
  FCmpNm.Text := C.Name;
  Result := ShowModal;
  if Result = mrOk then FRD.SetReportChanged;
end;

{ TFormNameDlg }

procedure TFormNameDlg.HelpBnClick(Sender: TObject);
begin
  OpenHelp('formname');
end;

procedure TFormNameDlg.FmNmChange(Sender: TObject);
begin
  FRecsCaption.TextHint := FFmNm.Text;
  if FRecsCaption.Text = '' then
    FRecCaption.TextHint := FFmNm.Text;
end;

procedure TFormNameDlg.RecsCaptionChange(Sender: TObject);
begin
  if FRecsCaption.Text <> '' then
    FRecCaption.TextHint := FRecsCaption.Text
  else
    FRecCaption.TextHint := FFmNm.Text;
end;

procedure TFormNameDlg.DoShow;
begin
  inherited DoShow;
  FFmNm.SetFocus;
end;

constructor TFormNameDlg.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
  Pan: TPanel;
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsFormName;
  BorderIcons := [biSystemMenu];
  Position := poOwnerFormCenter;
  Pan := CreatePanel(Self);

  L := CreateLabel(Pan, rsFormName);
  FFmNm := TEdit.Create(Pan);
  FFmNm.Parent := Pan;
  AnchorCtrl(FFmNm, L, 1);
  FFmNm.OnChange:=@FmNmChange;

  L := CreateLabel(Pan, rsMultipleRecCaption);
  AnchorCtrl(L, FFmNm, 8);
  FRecsCaption := TEdit.Create(Pan);
  FRecsCaption.Parent := Pan;
  AnchorCtrl(FRecsCaption, L, 1);
  FRecsCaption.OnChange:=@RecsCaptionChange;

  L := CreateLabel(Pan, rsOneRecCaption);
  AnchorCtrl(L, FRecsCaption, 8);
  FRecCaption := TEdit.Create(Pan);
  FRecCaption.Parent := Pan;
  AnchorCtrl(FRecCaption, L, 1);

  L := CreateLabel(Pan, rsComponentName);
  AnchorCtrl(L, FRecCaption, 8);
  FCmpNm := TEdit.Create(Pan);
  FCmpNm.Parent := Pan;
  AnchorCtrl(FCmpNm, L, 1);

  L.Visible:=AppConfig.ExpertMode;
  FCmpNm.Visible := L.Visible;

  FBns := CreateButtonPanel(Self);
  FBns.HelpButton.OnClick:=@HelpBnClick;

  if AppConfig.ExpertMode then Height := 260
  else Height := 210;
end;

function TFormNameDlg.CloseQuery: boolean;
var
  OldFormName, NewFormName, OldName, NewName: String;
begin
  Result:=inherited CloseQuery;
  if ModalResult <> mrOk then Exit;
  OldFormName := FCmp.FormCaption;
  NewFormName := FFmNm.Text;
  OldName := FCmp.Name;
  NewName := FCmpNm.Text;

  Result := False;
  if CheckFormName(NewFormName) and  CheckDuplicateFormName(NewFormName, NewName, FCmp) then
  begin
    if TrySetComponentName(FCmp, NewName) then
    begin
      FCmp.FormCaption := NewFormName;
      FCmp.RecordsCaption := FRecsCaption.Text;
      FCmp.RecordCaption := FRecCaption.Text;
      if OldFormName <> NewFormName then
        RenameInActions(FCmp, renForm, OldFormName, NewFormName);
      if OldName <> NewName then
        RenameInActions(FCmp, renComponent, OldName, NewName);
      Result := True;
    end
    else
      FCmpNm.SetFocus;
  end
  else
    FFmNm.SetFocus;
end;

function TFormNameDlg.ShowForm(aFm: TdxForm): Integer;
begin
  FCmp := aFm;
  FFmNm.Text := aFm.FormCaption;
  FRecsCaption.Text := aFm.RecordsCaption;
  FRecCaption.Text := aFm.RecordCaption;
  FCmpNm.Text := aFm.Name;
  Result := ShowModal;
end;

{ TComponentNameDlg }

procedure TComponentNameDlg.HelpBnClick(Sender: TObject);
begin
  OpenHelp('componentname');
end;

procedure TComponentNameDlg.DoShow;
begin
  inherited DoShow;
  FCmpNm.SetFocus;
end;

constructor TComponentNameDlg.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
  Pan: TPanel;
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsComponentName;
  BorderIcons := [biSystemMenu];
  Position := poOwnerFormCenter;
  Pan := CreatePanel(Self);

  L := CreateLabel(Pan, rsComponentName);
  FCmpNm := TEdit.Create(Pan);
  FCmpNm.Parent := Pan;
  AnchorCtrl(FCmpNm, L, 1);

  FBns := CreateButtonPanel(Self);
  FBns.HelpButton.OnClick:=@HelpBnClick;

  Height := 110
end;

function TComponentNameDlg.CloseQuery: boolean;
var
  OldName, NewName: TComponentName;
begin
  Result:=inherited CloseQuery;
  if ModalResult <> mrOk then Exit;
  OldName := FCmp.Name;
  NewName := FCmpNm.Text;
  if TrySetComponentName(FCmp, NewName) then
  begin
    if OldName <> NewName then
    begin
      RenameComponentInRights(TdxForm(FCmp.Owner), OldName, NewName);
      RenameInActions(TdxForm(FCmp.Owner), renComponent, OldName, NewName);
    end;
  end
  else Result := False;
end;

function TComponentNameDlg.ShowForm(C: TComponent): Integer;
begin
  FCmp := C;
  FCmpNm.Text := C.Name;
  Result := ShowModal;
end;

{ TFieldNameDlg }

procedure TFieldNameDlg.HelpBnClick(Sender: TObject);
begin
  OpenHelp('fieldname');
end;

procedure TFieldNameDlg.DoShow;
begin
  inherited DoShow;
  FFlNm.SetFocus;
end;

constructor TFieldNameDlg.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
  Pan: TPanel;
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsFieldName;
  BorderIcons := [biSystemMenu];
  Position := poOwnerFormCenter;
  Pan := CreatePanel(Self);

  L := CreateLabel(Pan, rsFieldName);
  FFlNm := TEdit.Create(Pan);
  FFlNm.Parent := Pan;
  AnchorCtrl(FFlNm, L, 1);

  L := CreateLabel(Pan, rsComponentName);
  AnchorCtrl(L, FFlNm, 8);
  FCmpNm := TEdit.Create(Pan);
  FCmpNm.Parent := Pan;
  AnchorCtrl(FCmpNm, L, 1);

  L.Visible:=AppConfig.ExpertMode;
  FCmpNm.Visible := L.Visible;

  FBns := CreateButtonPanel(Self);
  FBns.HelpButton.OnClick:=@HelpBnClick;

  if AppConfig.ExpertMode then Height := 160
  else Height := 110;
end;

function TFieldNameDlg.CloseQuery: boolean;
var
  OldFieldName, OldName, NewFieldName, NewName: String;
  Fm: TdxForm;
begin
  Result:=inherited CloseQuery;
  if ModalResult <> mrOk then Exit;
  OldFieldName := GetFieldName(FCmp);
  NewFieldName := FFlNm.Text;
  OldName := FCmp.Name;
  NewName := FCmpNm.Text;
  Fm := TdxForm(FCmp.Owner);

  if CheckFieldName(NewFieldName) and CheckDuplicateFieldName(NewFieldName, FCmp) and
    TrySetComponentName(FCmp, NewName) then
  begin
    if OldFieldName <> NewFieldName then
    begin
      SetFieldName(FCmp, NewFieldName);
      RenameInActions(Fm, renField, OldFieldName, NewFieldName);
    end;
    if OldName <> NewName then
    begin
      RenameComponentInRights(Fm, OldName, NewName);
      RenameInActions(Fm, renComponent, OldName, NewName);
    end;
  end
  else Result := False;
end;

function TFieldNameDlg.ShowForm(C: TComponent): Integer;
begin
  FCmp := C;
  FFlNm.Text := GetFieldName(C);
  FCmpNm.Text := C.Name;
  Result := ShowModal;
end;

end.

