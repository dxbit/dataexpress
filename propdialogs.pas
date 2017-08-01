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
unit PropDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Dialogs, LCLType, dxctrls, dximages, strconsts,
  DXReports, pivotgrid, Forms, StdCtrls, ButtonPanel, ExtCtrls;

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
    FCmpNm: TEdit;
    FBns: TButtonPanel;
    FCmp: TdxForm;
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

function FormNameDlg(Fm: TdxForm): Boolean;
function FieldNameDlg(C: TComponent): Boolean;
function PrecissionDlg(C: TComponent): Boolean;
function ControlCaptionDlg(Control: TControl): Boolean;
function QueryNameDlg(C: TComponent): Boolean;
function ThumbSizeDlg(C: TdxDBImage): Boolean;
procedure QuerySourceDlg(C: TComponent);
procedure QueryGridDlg(C: TComponent);
function FieldSizeDlg(C: TComponent): Boolean;
procedure LookupFilterDlg(C: TComponent);
procedure ShowExprDlg(C: TComponent);
procedure DelimDlg(C: TdxMemo);
procedure QueryStyleDlg(C: TdxQueryGrid);
procedure QueryCalcDlg(C: TComponent);
procedure QueryFilterDlg(C: TComponent);
procedure DefaultValueDlg(C: TComponent);
procedure QueryColoringDlg(C: TComponent);
procedure CheckExprDlg(C: TdxCheckBox);
procedure ShowCheckExprDlg(C: TComponent);
//procedure ButtonNameDlg(C: TdxButton);
procedure SelectQueryDlg(Cmp: TdxPivotGrid);
function ComponentNameDlg(C: TComponent): Boolean;
function ReportNameDlg(const aCaption: String; var aName: String; aRD: TReportData): Integer;

implementation

uses
  reportmanager, ReportForm, rpgridform, inputform, LazUtf8,
  exprform, apputils, formmanager, rpstyleform, QueryCalcForm, querycoloringform,
  selectform, dxusers, mainform, helpform, appsettings, mydialogs, mytypes;

function FormNameDlg(Fm: TdxForm): Boolean;
begin
  with TFormNameDlg.CreateNew(MainFm) do
  try
    Result := ShowForm(Fm) = mrOk;
  finally
    Free;
  end;
end;

function FieldNameDlg(C: TComponent): Boolean;
begin
  with TFieldNameDlg.CreateNew(MainFm) do
  try
    Result := ShowForm(C) = mrOk;
  finally
    Free;
  end;
end;

function PrecissionDlg(C: TComponent): Boolean;
var
  S: String;
  N: Integer;
begin
  Result := False;
  S := IntToStr(GetPrecission(C));
  Result := InputInt(rsPrecission, rsEnterValue, 'precission', S, 0, 10);
  if Result and TryStrToInt(S, N) then
    SetPrecission(C, N);
end;

function ControlCaptionDlg(Control: TControl): Boolean;
var
  S: String;
begin
  if Control is TdxLabel then
  	ShowLabelCaptionDlg(TdxLabel(Control))
  else
  begin
    S := Control.Caption;
    Result := InputStr(rsCaption, rsEnterCaption, '', S, False);
    if Result then
    begin
      {begin
        if Trim(S) = '' then
        begin
      	  ErrMsg(rsLabelEmpty);
          Exit;
        end
        else if Trim(TdxLabel(Control).Expression) <> '' then
        begin
      	  if not CheckName(S) then Exit;
        end;
        Control.Caption:=S;
        TdxLabel(Control).Alignment:=taLeftJustify;
        TdxLabel(Control).FieldName:=S;
      end
      else   }
    	  Control.Caption:=S;
    end;
  end;
end;

function QueryNameDlg(C: TComponent): Boolean;
begin
  with TQueryNameDlg.CreateNew(MainFm) do
  try
    Result := ShowForm(TdxQueryGrid(C)) = mrOk;
  finally
    Free;
  end;
end;

function ThumbSizeDlg(C: TdxDBImage): Boolean;
var
  S: String;
  N: Integer;
begin
  Result := False;
  S := IntToStr(C.ThumbSize);
  Result := InputInt(rsThumbSize, rsEnterValue, 'thumbnails', S, 0, 100);
  if Result and TryStrToInt(S, N) then
    C.ThumbSize:=N;
end;

procedure QuerySourceDlg(C: TComponent);
var
  RD: TReportData;
  Fm: TdxForm;
begin
  RD := ReportMan.FindReport(GetId(C));
  Fm := TdxForm(C.Owner);
  if ReportFm.ShowForm(RD, Fm) <> mrOk then Exit;
end;

procedure QueryGridDlg(C: TComponent);
var
  RD: TReportData;
begin
  RD := ReportMan.FindReport(GetId(C));
  RpGridFm.ShowForm(RD);
end;

function FieldSizeDlg(C: TComponent): Boolean;
var
  S: String;
  N: Integer;
begin
  S := IntToStr(GetFieldSize(C));
  Result := InputInt(rsFieldSize, rsEnterValue, 'fieldsize', S, 1, 2000);
  if Result and TryStrToInt(S, N) then
    SetFieldSize(C, N);
end;

procedure LookupFilterDlg(C: TComponent);
var
  Expr: String;
  TId: Integer;
begin
  TId := GetSourceTId(C);
  if TId = 0 then
  begin
    ErrMsg(rsSourceNotSel);
    Exit;
  end;
  Expr := GetComboFilter(C);
  if ExprFm.ShowForm(rsListFilter, C, Expr, TdxForm(C.Owner), FormMan.FindForm(TId),
    nil, nil, 'listfilter') then
      SetComboFilter(C, Expr);
end;

procedure ShowExprDlg(C: TComponent);
var
  S: String;
begin
  S := GetExpression(C);
  if ExprFm.ShowForm(rsExpression, C, S, TdxForm(C.Owner), nil, nil, nil, 'expressions') then
    SetExpression(C, S);
end;

procedure DelimDlg(C: TdxMemo);
var
  S: String;
begin
  S := C.Delimiter;
  if InputStr(rsDelimiter, rsEnterValue, '', S, False) then
    C.Delimiter:=S;
end;

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
  RpStyleFm.ShowForm(RD0, SL);
  SL.Free;
end;

procedure QueryCalcDlg(C: TComponent);
var
  RD: TReportData;
begin
  RD := ReportMan.FindReport(GetId(C));
  if RD = nil then Exit;
  CalcFm.ShowForm(RD, TdxForm(C.Owner));
end;

procedure QueryFilterDlg(C: TComponent);
var
  RD: TReportData;
  S: String;
begin
  RD := ReportMan.FindReport(GetId(C));
  if RD = nil then Exit;
  S := RD.Filter;
  if ExprFm.ShowForm(rsOutputFilter + ': ' + RD.Name, nil, S, TdxForm(C.Owner),
    nil, nil, RD, 'outfilter') then RD.Filter := S;
end;

procedure DefaultValueDlg(C: TComponent);
var
  S: String;
begin
  S := GetDefaultValue(C);
  if ExprFm.ShowForm(rsDefaultValue, C, S, TdxForm(C.Owner), nil, nil, nil, 'defaultvalue') then
    SetDefaultValue(C, S);
end;

procedure QueryColoringDlg(C: TComponent);
var
  RD: TReportData;
begin
  RD := ReportMan.FindReport(GetId(C));
  if RD = nil then Exit;
  QueryColoringFm.ShowForm(RD, TdxForm(C.Owner));
end;

procedure CheckExprDlg(C: TdxCheckBox);
var
  S: String;
begin
  S := GetCheckExpression(C);
  if ExprFm.ShowForm(rsCheckValue, C, S, TdxForm(C.Owner), nil, nil, nil, 'checkvalue') then
    SetCheckExpression(C, S);
end;

{procedure ButtonNameDlg(C: TdxButton);
var
  S: String;
begin
  S := C.ButtonName;
  if InputStr(rsButtonName, rsEnterName, 'buttonname', S, True) then
    C.ButtonName := S;
end;  }

procedure ShowCheckExprDlg(C: TComponent);
var
  S: String;
begin
  S := GetCheckExpression(C);
  if ExprFm.ShowForm(rsCheckValue, C, S, TdxForm(C.Owner), nil,
    nil, nil, 'checkvalue') then SetCheckExpression(C, S);
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
  SelectFm.Index := SL.IndexOfObject(RD);
  if SelectFm.ShowForm(rsSelectQuery, 'selectquery', SL) = mrOk then
  begin
    RD := TReportData(SL.Objects[SelectFm.Index]);
    if Cmp.Id <> RD.Id then
      Cmp.Clear;
    Cmp.Id:=RD.Id;
  end;
  SL.Free;
end;

function ComponentNameDlg(C: TComponent): Boolean;
begin
  with TComponentNameDlg.CreateNew(MainFm) do
  try
    Result := ShowForm(C) = mrOk;
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

function CheckDuplicateFieldName(const aName: String; aCmp: TComponent): Boolean;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
begin
  Result := True;
  Fm := TdxForm(aCmp.Owner);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C = aCmp then Exit;
    if HasFId(C) then
    begin
      if Utf8CompareText(GetFieldName(C), aName) = 0 then
      begin
        ErrMsg(rsComponentFieldNameExists);
        Exit(False);
      end;
    end
    else if (C is TdxLabel) and (Trim(GetExpression(C)) <> '') then
    begin
      if Utf8CompareText(TdxLabel(C).FieldName, aName) = 0 then
      begin
        ErrMsg(rsCalcLabelCaptionExists);
        Exit(False);
      end;
    end;
  end;
  for i := 0 to Fm.CalcFields.Count - 1 do
  begin
    if Utf8CompareText(Fm.CalcFields.Names[i], aName) = 0 then
    begin
      ErrMsg(rsCalcFieldNameExists);
      Exit(False);
    end;
  end;
end;

function CheckDuplicateFormName(const aName: String; aFm: TdxForm): Boolean;
var
  i: Integer;
  Fm: TdxForm;
  RD: TReportData;
begin
  Result := True;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if (Fm <> aFm) and (Utf8CompareText(Fm.FormCaption, aName) = 0) then
    begin
      ErrMsg(rsFormNameExists);
      Exit(False);
    end;
  end;
  for i := 0 to ReportMan.ReportCount - 1 do
  begin
    RD := ReportMan.Reports[i];
    if Utf8CompareText(aName, RD.Name) = 0 then
    begin
      ErrMsg(rsReportNameExists);
      Exit(False);
    end;
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
  Result := (CheckName(S) and CheckDuplicateQueryName(S, FRD));
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
  OldName: TComponentName;
begin
  Result:=inherited CloseQuery;
  if ModalResult <> mrOk then Exit;
  OldName := FCmp.Name;
  if CheckName(FNm.Text) and CheckDuplicateQueryName(FNm.Text, FRD) and
    TrySetComponentName(FCmp, FCmpNm.Text) then
  begin
    FRD.Name := FNm.Text;
    RenameComponentInRights(TdxForm(FCmp.Owner), OldName, FCmp.Name)
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
end;

{ TFormNameDlg }

procedure TFormNameDlg.HelpBnClick(Sender: TObject);
begin
  OpenHelp('formname');
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

  L := CreateLabel(Pan, rsComponentName);
  AnchorCtrl(L, FFmNm, 8);
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

function TFormNameDlg.CloseQuery: boolean;
begin
  Result:=inherited CloseQuery;
  if ModalResult <> mrOk then Exit;
  if CheckName(FFmNm.Text) and CheckDuplicateFormName(FFmNm.Text, FCmp) and
    TrySetComponentName(FCmp, FCmpNm.Text) then
  begin
    FCmp.FormCaption := FFmNm.Text;
  end
  else Result := False;
end;

function TFormNameDlg.ShowForm(aFm: TdxForm): Integer;
begin
  FCmp := aFm;
  FFmNm.Text := aFm.FormCaption;
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
  OldName: TComponentName;
begin
  Result:=inherited CloseQuery;
  if ModalResult <> mrOk then Exit;
  OldName := FCmp.Name;
  if TrySetComponentName(FCmp, FCmpNm.Text) then
    RenameComponentInRights(TdxForm(FCmp.Owner), OldName, FCmp.Name)
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
  OldName: TComponentName;
begin
  Result:=inherited CloseQuery;
  if ModalResult <> mrOk then Exit;
  OldName := FCmp.Name;
  if CheckName(FFlNm.Text) and CheckDuplicateFieldName(FFlNm.Text, FCmp) and
    TrySetComponentName(FCmp, FCmpNm.Text) then
  begin
    RenameComponentInRights(TdxForm(FCmp.Owner), OldName, FCmp.Name);
    SetFieldName(FCmp, FFlNm.Text);
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

