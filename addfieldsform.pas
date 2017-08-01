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
unit AddFieldsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ButtonPanel, Menus, ExtCtrls, Buttons, ComCtrls, strconsts, dxctrls;

type

  { TAddFieldsFm }

  TAddFieldsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TStringGrid;
    Images: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure ButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridPickListSelect(Sender: TObject);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
  private
    { private declarations }
    FForm: TdxForm;
    procedure AddFields;
    procedure SetControlState;
    procedure StringEditorKeyPress(Sender: TObject; var Key: char);
  public
    { public declarations }
    function ShowForm(AForm: TdxForm): Integer;
  end;

var
  AddFieldsFm: TAddFieldsFm;

implementation

uses
  formdesigner, dximages, dxfiles, StdCtrls, LCLType, helpform,
  JvDesignUtils, apputils, LazUtf8;

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
  MenuItem1.Caption := rsAppend;
  MenuItem2.Caption := rsDelete;
  MenuItem4.Caption := rsMoveUp;
  MenuItem5.Caption := rsMoveDown;
  ToolButton1.Caption := rsAppend;
  ToolButton2.Caption := rsDelete;
  ToolButton3.Caption := rsMoveUp;
  ToolButton4.Caption := rsMoveDown;
  Grid.Columns[0].Title.Caption := rsFieldName;
  Grid.Columns[1].Title.Caption := rsFieldType;
  Grid.Columns[2].Title.Caption := rsSizePrec;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Grid.Columns[1].PickList.AddStrings([rsText, rsNumber, rsDate, rsTime,
    rsMemo, rsCheckBox, rsCounter, rsList, rsObject, rsObjField, rsImage,
    rsDsgnFile]);
  Images.AddLazarusResource('add16');
  Images.AddLazarusResource('delete16');
  Images.AddLazarusResource('up16');
  Images.AddLazarusResource('down16');
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
  for i := 0 to AForm.CalcFields.Count - 1 do
  begin
    if Utf8CompareText(AForm.CalcFields.Names[i], aName) = 0 then
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
  if (ModalResult <> mrOk) and (Grid.RowCount > 1) then
  begin
    if MessageDlg(rsWarning, rsAreYouSure, mtWarning, [mbYes, mbNo], 0) <> mrYes then
      CanClose := False;
    Exit;
  end;

  SL := TStringList.Create;
  for i := 1 to Grid.RowCount -1 do
  begin
    S := Grid.Cells[0, i];
    if not CheckName(S) then
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
end;

procedure TAddFieldsFm.ButtonClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: MenuItem1.Click;
    1: MenuItem2.Click;
    2: MenuItem4.Click;
    3: MenuItem5.Click;
  end;
end;

procedure TAddFieldsFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
  Grid.Col:=0;
  SetControlState;
end;

procedure TAddFieldsFm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  K: Word;
begin
  K := Key; Key := 0;
  case K of
    VK_DELETE: if ssCtrl in Shift then MenuItem2.Click;
    VK_PRIOR: MenuItem4.Click;
    VK_NEXT: MenuItem5.Click;
    else Key := K;
  end;
end;

procedure TAddFieldsFm.GridPickListSelect(Sender: TObject);
var
  S: TCaption;
begin
  S := TPickListCellEditor(Grid.Editor).Text;
  if S = rsNumber then Grid.Cells[2, Grid.Row] := '0'
  else if (S = rsText) or (S = rsList) then Grid.Cells[2, Grid.Row] := '50'
  else if S = rsFile then Grid.Cells[2, Grid.Row] := '150'
  else if S = rsMemo then Grid.Cells[2, Grid.Row] := '300'
  else Grid.Cells[2, Grid.Row] := '-';
end;

procedure TAddFieldsFm.GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if (aCol = 1) and (Editor is TPickListCellEditor) then
    with TPickListCellEditor(Editor) do
    begin
      Style:=csDropDownList;
      AutoComplete:=True;
      AutoDropDown:=True;
    end
	else if aCol = 2 then
  begin
    if not IsNeedFieldSizePrec(Grid.Cells[1, aRow]) then Editor := nil
    else TStringCellEditor(Editor).OnKeyPress:=@StringEditorKeyPress;
  end;
end;

procedure TAddFieldsFm.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  SetControlState;
end;

procedure TAddFieldsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('addfields');
end;

procedure TAddFieldsFm.MenuItem1Click(Sender: TObject);
begin
  Grid.RowCount := Grid.RowCount + 1;
  Grid.Row := Grid.RowCount - 1;
end;

procedure TAddFieldsFm.MenuItem2Click(Sender: TObject);
begin
  Grid.DeleteColRow(False, Grid.Row);
  SetControlState;
end;

procedure TAddFieldsFm.MenuItem4Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row - 1);
  SetControlState;
end;

procedure TAddFieldsFm.MenuItem5Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row + 1);
  SetControlState;
end;

procedure CreateControl(const FieldName, FieldType, SizePrec: String; var aControl, aLabel: TControl);
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
  else Exit;
  C.Parent := Parent;
  FormDesign.OnAddComponent(FormDesign, C);
  SetFieldName(C, FieldName);
  C.Name:=DesignUniqueName(Own, C.ClassName);
  aControl := C;
  if C is TdxCheckBox then
  begin
    TdxCheckBox(C).Caption:=FieldName;
    aLabel := nil;
  end
  else
  begin
    aLabel := TdxLabel.Create(Own);
    aLabel.Name := DesignUniqueName(Own, aLabel.ClassName);
    aLabel.Caption := FieldName;
    aLabel.Parent := Parent;
  end;
  if IsNeedFieldSize(FieldType) then
  	SetFieldSize(C, StrToInt(SizePrec))
  else if IsNeedFieldPrec(FieldType) then
  	SetPrecission(C, StrToInt(SizePrec));
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
  mw, i, x, y: Integer;
  L, C: TControl;
begin
  mw := GetMaxLabelWidth(LL);
  x := 8; y := 8;
  for i := 0 to LL.Count - 1 do
  begin
    L := TControl(LL[i]);
    C := TControl(CL[i]);
    if L <> nil then
    begin
	    L.Left := x; L.Top := y;
  	  L.Visible := True;
    end;
    x := x + mw + 8;
    C.Left := x; C.Top := y;
    y := y + C.Height + 8;
    x := 8;
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
end;

procedure TAddFieldsFm.SetControlState;
begin
  MenuItem1.Enabled:=Grid.Row > 0;
  MenuItem2.Enabled:=(Grid.Row > 0) and (Grid.RowCount > 2);
  MenuItem4.Enabled := Grid.Row > 1;
  MenuItem5.Enabled := (Grid.Row > 0) and (Grid.Row < Grid.RowCount - 1);
  ToolButton1.Enabled := MenuItem1.Enabled;
  ToolButton2.Enabled := MenuItem2.Enabled;
  ToolButton3.Enabled := MenuItem4.Enabled;
  ToolButton4.Enabled := MenuItem5.Enabled;
end;

procedure TAddFieldsFm.StringEditorKeyPress(Sender: TObject; var Key: char);
begin
  if Key in [#8, '0'..'9'] then
  else Key := #0;
end;

function TAddFieldsFm.ShowForm(AForm: TdxForm): Integer;
begin
  FForm := AForm;
  Grid.RowCount := 1;
  Grid.RowCount := 2;
  Result := ShowModal;
  if Result = mrOk then AddFields;
end;

end.

