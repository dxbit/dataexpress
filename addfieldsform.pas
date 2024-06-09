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

unit AddFieldsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ButtonPanel, Menus, ExtCtrls, Buttons, ComCtrls, strconsts, dxctrls,
  dialoggrid;

type

  { TAddFieldsFm }

  TAddFieldsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DialogGridButtons1: TDialogGridButtons;
    Grid: TDialogGrid;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridPickListSelect(Sender: TObject);
    procedure GridResetValue(Sender: TObject);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FForm: TdxForm;
    function GridEmpty: Boolean;
    procedure AddFields;
    procedure InitCol3;
  public
    { public declarations }
    function ShowForm(AForm: TdxForm): Integer;
  end;

var
  AddFieldsFm: TAddFieldsFm;

function ShowAddFieldsForm(AForm: TdxForm): Integer;

implementation

uses
  formdesigner, dximages, dxfiles, StdCtrls, LCLType, helpmanager,
  apputils, LazUtf8, DesignerFrame, templatefieldsform, appsettings;

function ShowAddFieldsForm(AForm: TdxForm): Integer;
begin
	if AddFieldsFm = nil then
  	AddFieldsFm := TAddFieldsFm.Create(Application);
  Result := AddFieldsFm.ShowForm(AForm);
end;

function IsNeedFieldSize(const S: String): Boolean;
begin
  Result := (S = rsText) or (S = rsMemo) or (S = rsList) or (S = rsFile);
end;

function IsNeedFieldPrec(const S: String): Boolean;
begin
  Result := (S = rsNumber);
end;

function IsNeedFieldSizePrec(const S: String): Boolean;
begin
  Result := IsNeedFieldSize(S) or IsNeedFieldPrec(S);
end;

{$R *.lfm}

{ TAddFieldsFm }

procedure TAddFieldsFm.FormCreate(Sender: TObject);
begin
  Caption := rsAddFields;
  Grid.Columns[0].Title.Caption := rsFieldName;
  Grid.Columns[1].Title.Caption := rsFieldType;
  Grid.Columns[2].Title.Caption := rsSizePrec;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Grid.Columns[1].PickList.AddStrings([rsText, rsNumber, rsDate, rsTime,
    rsMemo, rsCheckBox, rsList, rsObject, rsObjField, rsImage,
    rsDsgnFile, rsRecordId, rsCounter]);
end;


function CheckDuplicateFieldName(AForm: TdxForm; const aName: String): Boolean;
var
  i: Integer;
  C: TComponent;
begin
  Result := True;
  for i := 0 to AForm.ComponentCount - 1 do
  begin
    C := AForm.Components[i];
    if HasFId(C) then
    begin
      if MyUtf8CompareText(GetFieldName(C), aName) = 0 then
      begin
        ErrMsg(rsComponentFieldNameExists);
        Exit(False);
      end;
    end
    else if (C is TdxLabel) and (Trim(GetExpression(C)) <> '') then
    begin
      if MyUtf8CompareText(TdxLabel(C).FieldName, aName) = 0 then
      begin
        ErrMsg(rsCalcLabelCaptionExists);
        Exit(False);
      end;
    end;
  end;
  for i := 0 to AForm.CalcFields.Count - 1 do
  begin
    if MyUtf8CompareText(AForm.CalcFields.Names[i], aName) = 0 then
    begin
      ErrMsg(rsCalcFieldNameExists);
      Exit(False);
    end;
  end;
end;

procedure TAddFieldsFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
  SL: TStringList;
  S: String;
  N: Longint;
begin
  if ModalResult <> mrOk then
  begin
    if not GridEmpty and (MessageDlg(rsWarning, rsCancelAddFieldsMsg,
      mtWarning, [mbYes, mbNo], 0) <> mrYes) then
      CanClose := False;
    Exit;
  end;

  SL := TStringList.Create;
  for i := 1 to Grid.RowCount -1 do
  begin
    S := Grid.Cells[0, i];
    if not CheckFieldName(S) then
    begin
      Grid.Row := i; Grid.Col := 0;
      CanClose := False; Break;
    end
    else if SL.IndexOf(S) >= 0 then
    begin
      ErrMsg(rsDuplicateFieldName);
      Grid.Row := i; Grid.Col := 0;
      CanClose := False; Break;
    end
    else if not CheckDuplicateFieldName(FForm, S) then
    begin
      Grid.Row := i; Grid.Col := 0;
      CanClose := False; Break;
    end
    else if Grid.Cells[1, i] = '' then
    begin
    	ErrMsg(rsSelectFieldType);
      Grid.Row := i; Grid.Col := 1;
      CanClose := False; Break;
    end
    else if Grid.Cells[2, i] = '-' then
    begin
      if IsNeedFieldPrec(Grid.Cells[1, i]) then
      begin
        ErrMsg(rsEnterPrecisionNum);
        Grid.Row := i; Grid.Col := 2;
	      CanClose := False; Break;
      end
      else if IsNeedFieldSize(Grid.Cells[1, i]) then
      begin
        ErrMsg(rsEnterFieldSize);
        Grid.Row := i; Grid.Col := 2;
	      CanClose := False; Break;
      end;
    end
    else if Grid.Cells[2, i] <> '-' then
    begin
      if TryStrToInt(Grid.Cells[2, i], N) then
      begin
        if IsNeedFieldPrec(Grid.Cells[1, i]) and (N > 10) then
        begin
          ErrMsg(rsPrecisionMustNotExceed);
          Grid.Row := i; Grid.Col := 2;
	        CanClose := False; Break;
      	end
        else if IsNeedFieldSize(Grid.Cells[1, i]) and ((N > 2000) or (N < 1)) then
        begin
          ErrMsg(rsFieldSizeMustRange);
          Grid.Row := i; Grid.Col := 2;
	        CanClose := False; Break;
        end;
      end
      else
      begin
        ErrMsg(rsInvalidNumber);
        Grid.Row := i; Grid.Col := 2;
        CanClose := False; Break;
      end;
    end;
    SL.Add(S);
  end;
  SL.Free;
  if not CanClose then Grid.SetFocus;
end;

procedure TAddFieldsFm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Grid.EditorMode:=False;
  Grid.Editor := nil;
end;

procedure TAddFieldsFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
  Grid.Col:=0;
end;

procedure TAddFieldsFm.GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
begin
  case Cmd of
    dgcAppend:
      begin
        Grid.RowCount := Grid.RowCount + 1;
        Grid.Row := Grid.RowCount - 1;
        Grid.Cells[2, Grid.Row] := '-';
      end;
    dgcDelete:
      Grid.DeleteColRow(False, Grid.Row);
  end;
end;

procedure TAddFieldsFm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DOWN then
  begin
    with Grid do
      if (Row = RowCount - 1) and ((Cells[0, Row] <> '') or (Cells[1, Row] <> '')) then
        Grid.DoCommand(dgcAppend);
  end;
end;

procedure TAddFieldsFm.GridPickListSelect(Sender: TObject);
begin
  if Grid.Col = 1 then InitCol3
end;

procedure TAddFieldsFm.GridResetValue(Sender: TObject);
begin
  if Grid.Col = 1 then InitCol3;
end;

procedure TAddFieldsFm.GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
var
  S: String;
begin
  if (aCol = 1) and (Editor is TListCellEditor) then
    with TListCellEditor(Editor) do
      AutoDropDown:=True
	else if aCol = 2 then
  begin
    S := Grid.Cells[1, aRow];
    if not IsNeedFieldSizePrec(S) then Editor := nil
    else
      with TIntegerCellEditor(Editor) do
      begin
        if S = rsNumber then
        begin
          MaxValue := 10;
          Increment := 1;
        end
        else
        begin
          MaxValue:=2000;
          Increment:=10;
        end;
      end;
  end;
end;

procedure TAddFieldsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('addfields');
end;

function TAddFieldsFm.GridEmpty: Boolean;
var
  i, j: Integer;
begin
  Result := True;
  for i := 1 to Grid.RowCount - 1 do
    for j := 0 to 1 do
      if Grid.Cells[j, i] <> '' then Exit(False);
end;

procedure CreateControl(const FieldName, FieldType, SizePrec: String; out aControl, aLabel: TControl);
var
  C: TControl;
  Parent: TWinControl;
  Own: TComponent;
begin
  aControl := nil; aLabel := nil;
  Parent := FormDesign.SelectedContainer;
  if not ((Parent is TdxGroupBox) or (Parent is TdxTabSheet)) then
    Parent := FormDesign.Container;
  Own := FormDesign.Container;
  if FieldType = rsText then C := TdxEdit.Create(Own)
  else if FieldType = rsNumber then C := TdxCalcEdit.Create(Own)
  else if FieldType = rsDate then C := TdxDateEdit.Create(Own)
  else if FieldType = rsMemo then C := TdxMemo.Create(Own)
  else if FieldType = rsCheckBox then C := TdxCheckBox.Create(Own)
  else if FieldType = rsCounter then C := TdxCounter.Create(Own)
  else if FieldType = rsList then C := TdxComboBox.Create(Own)
  else if FieldType = rsObject then C := TdxLookupComboBox.Create(Own)
  else if FieldType = rsImage then C := TdxDBImage.Create(Own)
  else if FieldType = rsDsgnFile then C := TdxFile.Create(Own)
  else if FieldType = rsObjField then C := TdxObjectField.Create(Own)
  else if FieldType = rsTime then C := TdxTimeEdit.Create(Own)
  else if FieldType = rsRecordId then C := TdxRecordId.Create(Own)
  else Exit;
  C.Parent := Parent;
  FormDesign.OnAddComponent(FormDesign, C);
  SetFieldName(C, FieldName);
  //C.Name:=DesignUniqueName(Own, C.ClassName);
  MakeUniqueName(Own, C);
  aControl := C;
  if C is TdxCheckBox then
  begin
    TdxCheckBox(C).Caption:=FieldName;
    aLabel := nil;
  end
  else
  begin
    aLabel := TdxLabel.Create(Own);
    //aLabel.Name := DesignUniqueName(Own, aLabel.ClassName);
    MakeUniqueName(Own, aLabel);
    aLabel.Caption := FieldName;
    aLabel.Parent := Parent;
  end;
  if IsNeedFieldSize(FieldType) then
  begin
  	SetFieldSize(C, StrToInt(SizePrec));
    Cache.SetFieldSize(C, GetFieldSize(C));
  end
  else if IsNeedFieldPrec(FieldType) then
  	SetPrecission(C, StrToInt(SizePrec));
  DoInitControl(C);
  //FormDesign.OnAddComponent(FormDesign, aLabel);
end;

function GetMaxLabelWidth(LL: TList): Integer;
var
  i, w, mw: Integer;
begin
  mw := 0;
  for i := 0 to LL.Count - 1 do
  begin
    if LL[i] = nil then Continue;
    w := TdxLabel(LL[i]).Width;
    if w > mw then mw := w;
  end;
  Result := mw;
end;

procedure PositionFields(LL, CL: TList);
var
  mw0, mw, i, x, y, dy, GridSizeX, GridSizeY: Integer;
  L, C: TControl;
begin
  GridSizeX := ScaleToScreen(AppConfig.GridSizeX);
  GridSizeY := ScaleToScreen(AppConfig.GridSizeY);
  mw0 := GetMaxLabelWidth(LL);
  mw := mw0 div GridSizeX * GridSizeX + GridSizeX;
  if mw - mw0 < GridSizeX then
  begin
    if Odd(GridSizeX) then Inc(mw, GridSizeX)
    else Inc(mw, GridSizeX div 2);
  end;

  x := GridSizeX; y := GridSizeY;
  for i := 0 to LL.Count - 1 do
  begin
    L := TControl(LL[i]);
    C := TControl(CL[i]);
    if L <> nil then
    begin
      if (C is TdxMemo) or (C is TdxDBImage) then
        dy := ScaleToScreen(3)
      else
        dy := (C.Height div 2 - L.Height div 2);
	    L.Left := x; L.Top := y + dy;
  	  L.Visible := True;
    end;
    x := x + mw;
    C.Left := x; C.Top := y;
    y := y + C.Height + GridSizeY;
    x := GridSizeX;
    C.Visible := True;
  end;
end;

procedure SelectFields(LL, CL: TList);
var
  i: Integer;
  L, C: TControl;
begin
  FormDesign.ClearSelection;
  for i := 0 to LL.Count - 1 do
  begin
    L := TControl(LL[i]);
    C := TControl(CL[i]);
    if L <> nil then
    begin
	    FormDesign.Messenger.DesignComponent(L, True);
  	  FormDesign.Selector.AddToSelection(L);
    end;
    FormDesign.Messenger.DesignComponent(C, True);
    FormDesign.Selector.AddToSelection(C);
  end;
end;

procedure TAddFieldsFm.AddFields;
var
  LL, CL: TList;
  i: Integer;
  L, C: TControl;
begin
  LL := TList.Create;
  CL := TList.Create;
  for i := 1 to Grid.RowCount - 1 do
  begin
    CreateControl(Grid.Cells[0, i], Grid.Cells[1, i], Grid.Cells[2, i], C, L);
    if C = nil then Continue;
    LL.Add(L);
    CL.Add(C);
  end;
  PositionFields(LL, CL);
  SelectFields(LL, CL);
  DesignFr.CompTree.UpdateTree;
  DesignFr.CompTree.SelectComponents(FormDesign.Selected);
  UpdateTemplateFieldsForm;
  LL.Free;
  CL.Free;
end;

procedure TAddFieldsFm.InitCol3;
var
  S: String;
begin
  S := Grid.Cells[1, Grid.Row];
  if S = rsNumber then Grid.Cells[2, Grid.Row] := '0'
  else if (S = rsText) or (S = rsList) then Grid.Cells[2, Grid.Row] := '50'
  else if S = rsFile then Grid.Cells[2, Grid.Row] := '150'
  else if S = rsMemo then Grid.Cells[2, Grid.Row] := '300'
  else Grid.Cells[2, Grid.Row] := '-';
end;

function TAddFieldsFm.ShowForm(AForm: TdxForm): Integer;
begin
  FForm := AForm;
  Grid.RowCount := 1;
  Grid.DoCommand(dgcAppend);
  Result := ShowModal;
  if Result = mrOk then AddFields;
end;

end.

