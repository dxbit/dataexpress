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
unit InsertValuesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, Menus, strconsts, dxctrls, StdCtrls;

type

  { TInsertValuesFm }

  TInsertValuesFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TStringGrid;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridPickListSelect(Sender: TObject);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    { private declarations }
    FCtrl: TdxLookupComboBox;
    procedure FillObjectFields;
    procedure FillFormFields;
    procedure SetControlState;
    function Validate: Boolean;
    procedure Save;
    procedure Load;
  public
    { public declarations }
    procedure ShowForm(C: TdxLookupComboBox);
  end;

var
  InsertValuesFm: TInsertValuesFm;

implementation

uses
  formmanager, apputils, helpform, dxfiles, dximages, mytypes;

{$R *.lfm}

{ TInsertValuesFm }

procedure TInsertValuesFm.FormCreate(Sender: TObject);
begin
  Caption := rsInsertValues;
  Grid.Columns[0].Title.Caption:=rsObjField;
  Grid.Columns[1].Title.Caption:=rsFormField;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  MenuItem1.Caption:=rsAppend;
  MenuItem2.Caption := rsDelete;
  ToolButton1.Caption := rsAppend;
  ToolButton2.Caption := rsDelete;
  ImageList1.AddLazarusResource('add16');
  ImageList1.AddLazarusResource('delete16');
end;

procedure TInsertValuesFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
  SetControlState;
end;

procedure TInsertValuesFm.GridPickListSelect(Sender: TObject);
var
  Cbx: TPickListCellEditor;
begin
  Cbx := TPickListCellEditor(Grid.Editor);
  if Cbx.ItemIndex >= 0 then
    Grid.Objects[Grid.Col, Grid.Row] := Cbx.Items.Objects[Cbx.ItemIndex];
end;

procedure TInsertValuesFm.GridSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if Editor is TPickListCellEditor then
    TPickListCellEditor(Editor).Style:=csDropDownList;
end;

procedure TInsertValuesFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('insertvalues');
end;

procedure TInsertValuesFm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  if ModalResult = mrOk then
    CanClose := Validate;
end;

procedure TInsertValuesFm.MenuItem1Click(Sender: TObject);
var
  r: Integer;
begin
  r := Grid.RowCount;
  Grid.RowCount := r + 1;
  Grid.Row := r;
  SetControlState;
end;

procedure TInsertValuesFm.MenuItem2Click(Sender: TObject);
begin
  Grid.DeleteRow(Grid.Row);
  SetControlState;
end;

procedure TInsertValuesFm.ToolButton1Click(Sender: TObject);
begin
  MenuItem1.Click;
end;

procedure TInsertValuesFm.ToolButton2Click(Sender: TObject);
begin
  MenuItem2.Click;
end;

procedure TInsertValuesFm.FillObjectFields;
var
  SL: TStringListUtf8;
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
begin
  SL := TStringListUtf8.Create;
  Fm := FormMan.FindForm(FCtrl.SourceTId);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxEdit) or (C is TdxCalcEdit) or (C is TdxDateEdit) or
      (C is TdxCheckBox) or (C is TdxMemo) or (C is TdxComboBox) or
      (C is TdxLookupComboBox) or (C is TdxCounter) or (C is TdxTimeEdit) or
      (C is TdxFile) or (C is TdxDBImage) then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Grid.Columns[0].PickList := SL;
  SL.Free;
end;

procedure TInsertValuesFm.FillFormFields;
var
  SL: TStringListUtf8;
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
begin
  SL := TStringListUtf8.Create;
  Fm := TdxForm(FCtrl.Owner);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C = FCtrl then Continue;
    if (C is TdxEdit) or (C is TdxCalcEdit) or (C is TdxDateEdit) or
      (C is TdxCheckBox) or (C is TdxMemo) or (C is TdxComboBox) or
      (C is TdxLookupComboBox) or (C is TdxTimeEdit) or
      (C is TdxFile) or (C is TdxDBImage) then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Grid.Columns[1].PickList := SL;
  SL.Free;
end;

procedure TInsertValuesFm.SetControlState;
begin
  ToolButton2.Enabled:=Grid.Row > 0;
  MenuItem2.Enabled:=Grid.Row > 0;
end;

function TInsertValuesFm.Validate: Boolean;
var
  i: Integer;
  ObjF, FmF: TComponent;
begin
  Result := False;
  for i := 1 to Grid.RowCount - 1 do
  begin
    ObjF := TComponent(Grid.Objects[0, i]);
    FmF := TComponent(Grid.Objects[1, i]);
    if ObjF = nil then
    begin
      ErrMsg(rsObjectFieldNotSel);
      Grid.Row := i; Grid.Col := 0;
      Exit;
    end
    else if FmF = nil then
    begin
      ErrMsg(rsFormFieldNotSel);
      Grid.Row := i; Grid.Col := 1;
      Exit;
    end
    else if not CheckCompatibles(ObjF, FmF) then
    begin
      ErrMsg(rsIncompatibleFields);
      Grid.Row := i;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TInsertValuesFm.Save;
var
  S: String;
  i: Integer;
  ObjF, FmF: TComponent;
begin
  S := '';
  for i := 1 to Grid.RowCount - 1 do
  begin
    ObjF := TComponent(Grid.Objects[0, i]);
    FmF := TComponent(Grid.Objects[1, i]);
    S := S + IntToStr(GetId(ObjF)) + ';' + IntToStr(GetId(FmF));
    if i < Grid.RowCount - 1 then S := S + '|';
  end;
  FCtrl.InsertedValues:=S;
end;

procedure TInsertValuesFm.Load;
var
  SL: TStringList;
  i, r, p, N: Integer;
  OFm, Fm: TdxForm;
  OFId, FId: Integer;
  S: String;
  ObjF, FmF: TComponent;
begin
  Grid.RowCount := 1;
  SL := TStringList.Create;
  SplitStr(FCtrl.InsertedValues, '|', SL);
  OFm := FormMan.FindForm(FCtrl.SourceTId);
  Fm := TdxForm(FCtrl.Owner);
  for i := 0 to SL.Count - 1 do
  begin
    r := i + 1;
    Grid.RowCount := r + 1;
    S := SL[i];
    p := Pos(';', S);
    if TryStrToInt(Copy(S, 1, p - 1), N) then
      OFId := N;
    if TryStrToInt(Copy(S, p + 1, 255), N) then
      FId := N;
    ObjF := FindById(OFm, OFId);
    FmF := FindById(Fm, FId);
    if (ObjF = nil) or (FmF = nil) then Continue;
    Grid.Cells[0, r] := GetFieldName(ObjF);
    Grid.Objects[0, r] := ObjF;
    Grid.Cells[1, r] := GetFieldName(FmF);
    Grid.Objects[1, r] := FmF;
  end;
  SL.Free;
end;

procedure TInsertValuesFm.ShowForm(C: TdxLookupComboBox);
begin
  if (C.SourceTId = 0) or (C.SourceFId = 0) then
  begin
    ErrMsg(rsSourceNotSel);
    Exit;
  end;
  FCtrl := C;
  FillObjectFields;
  FillFormFields;
  Load;
  if ShowModal = mrOk then Save;
end;

end.

