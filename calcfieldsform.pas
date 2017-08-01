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
unit CalcFieldsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ButtonPanel, Menus, strconsts, dxctrls, editbtn, ExtCtrls, Buttons, ComCtrls,
  LclType;

type

  { TCalcFieldsFm }

  TCalcFieldsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Images: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    PopupMenu1: TPopupMenu;
    Grid: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure ButtonClick(Sender: TObject);
    procedure ExprEdButtonClick(Sender: TObject);
    procedure ExprEdEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
  private
    FForm: TdxForm;
    FExprEd: TEditButton;
    procedure FillFields;
    procedure SaveFields;
    procedure SetControlState;
    function CheckFieldNames(var aRow: Integer): Boolean;
    { private declarations }
  public
    { public declarations }
    function ShowForm(Fm: TdxForm): Integer;
  end;

var
  CalcFieldsFm: TCalcFieldsFm;

implementation

uses
  apputils, exprform, helpform, LazUtf8;

{$R *.lfm}

{ TCalcFieldsFm }

procedure TCalcFieldsFm.MenuItem1Click(Sender: TObject);
var
  r: Integer;
begin
  r := Grid.RowCount;
  Grid.RowCount := r + 1;
  Grid.Row:=r;
  SetControlState;
end;

procedure TCalcFieldsFm.FormCreate(Sender: TObject);
begin
  Caption := rsCalcFields;
  Grid.Columns[0].Title.Caption:=rsName;
  Grid.Columns[1].Title.Caption:=rsExpression;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  MenuItem1.Caption:=rsAppend;
  MenuItem2.Caption := rsDelete;
  MenuItem4.Caption := rsMoveUp;
  MenuItem5.Caption := rsMoveDown;
  ToolButton1.Caption := rsAppend;
  ToolButton2.Caption := rsDelete;
  ToolButton3.Caption := rsMoveUp;
  ToolButton4.Caption := rsMoveDown;
  FExprEd := TEditButton.Create(Self);
  FExprEd.Button.Caption:='...';
  FExprEd.OnButtonClick:=@ExprEdButtonClick;
  FExprEd.OnEditingDone:=@ExprEdEditingDone;
  Images.AddLazarusResource('add16');
  Images.AddLazarusResource('delete16');
  Images.AddLazarusResource('up16');
  Images.AddLazarusResource('down16');
end;

procedure TCalcFieldsFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
  SetControlState;
end;

procedure TCalcFieldsFm.GridKeyDown(Sender: TObject; var Key: Word;
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

procedure TCalcFieldsFm.ExprEdButtonClick(Sender: TObject);
var
  S: TCaption;
begin
  S := FExprEd.Text;
  if ExprFm.ShowForm(rsExpression, nil, S, FForm, nil, nil, nil, 'expressions') then
    FExprEd.Text := S;
end;

procedure TCalcFieldsFm.ButtonClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: MenuItem1.Click;
    1: MenuItem2.Click;
    2: MenuItem4.Click;
    3: MenuItem5.Click;
  end;
end;

procedure TCalcFieldsFm.ExprEdEditingDone(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := FExprEd.Text;
end;

procedure TCalcFieldsFm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  if Grid.Editor <> nil then Grid.EditingDone;
end;

procedure TCalcFieldsFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  R: Integer;
begin
  if (ModalResult = mrCancel) and (Grid.RowCount > 1) then
    CanClose := MessageDlg(rsWarning, rsAreYouSure, mtWarning, [mbYes, mbNo], 0) = mrYes
  else if ModalResult = mrOk then
  begin
    CanClose := CheckFieldNames(R);
    if not CanClose then
    begin
      Grid.SetFocus;
      Grid.Row := R; Grid.Col := 0;
    end;
  end;
end;

procedure TCalcFieldsFm.GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if aCol = 0 then Exit;
  FExprEd.BoundsRect := Grid.CellRect(aCol, aRow);
  FExprEd.Text := Grid.Cells[aCol, aRow];
  Editor := FExprEd;
end;

procedure TCalcFieldsFm.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  SetControlState;
end;

procedure TCalcFieldsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('calcfields');
end;

procedure TCalcFieldsFm.MenuItem2Click(Sender: TObject);
begin
  if ConfirmDelete then
    Grid.DeleteRow(Grid.Row);
  SetControlState;
end;

procedure TCalcFieldsFm.MenuItem4Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row - 1);
  SetControlState;
end;

procedure TCalcFieldsFm.MenuItem5Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row + 1);
  SetControlState;
end;

procedure TCalcFieldsFm.FillFields;
var
  i, r: Integer;
begin
  Grid.RowCount := 1;
  for i := 0 to FForm.CalcFields.Count - 1 do
  begin
    r := Grid.RowCount;
    Grid.RowCount := r + 1;
    Grid.Cells[0, r] := FForm.CalcFields.Names[i];
    Grid.Cells[1, r] := FForm.CalcFields.ValueFromIndex[i];
  end;
end;

procedure TCalcFieldsFm.SaveFields;
var
  i: Integer;
  S: String;
begin
  FForm.CalcFields.Clear;
  for i := 1 to Grid.RowCount - 1 do
  begin
    S := Grid.Cells[0, i] + '=' + Grid.Cells[1, i];
    FForm.CalcFields.Add(S);
  end;
end;

procedure TCalcFieldsFm.SetControlState;
begin
  MenuItem2.Enabled := Grid.Row >= 1;
  ToolButton2.Enabled := MenuItem2.Enabled;
  MenuItem4.Enabled := Grid.Row > 1;
  ToolButton3.Enabled := MenuItem4.Enabled;
  MenuItem5.Enabled := Grid.Row < Grid.RowCount - 1;
  ToolButton4.Enabled := MenuItem5.Enabled;
end;

function TCalcFieldsFm.CheckFieldNames(var aRow: Integer): Boolean;
var
  i: Integer;
  S: String;

  function _Check(aNm: String; idx: Integer): Boolean;
  var
    j: Integer;
    C: TComponent;
    SS: String;
  begin
    Result := CheckName(aNm);
    if not Result then Exit;

    for j := idx + 1 to Grid.RowCount - 1 do
    begin
      SS := Grid.Cells[0, j];
      if (SS <> '') and (Utf8CompareText(SS, aNm) = 0) then
      begin
        ErrMsg(rsCalcFieldNameExists);
        Exit(False);
      end;
    end;

    for j := 0 to FForm.ComponentCount - 1 do
    begin
      C := FForm.Components[j];
      if HasFId(C) then
      begin
        if Utf8CompareText(aNm, GetFieldName(C)) = 0 then
        begin
          ErrMsg(rsComponentFieldNameExists);
          Exit(False);
        end;
      end
      else if (C is TdxLabel) and (Trim(GetExpression(C)) <> '') then
      begin
        if Utf8CompareText(aNm, TdxLabel(C).FieldName) = 0 then
        begin
          ErrMsg(rsCalcLabelCaptionExists);
          Exit(False);
        end;
      end;
    end;
  end;

begin
  Result := True;
  for i := 1 to Grid.RowCount - 1 do
  begin
    S := Grid.Cells[0, i];
    if S <> '' then
    begin
      Result := _Check(S, i);
      if not Result then
      begin
        aRow := i;
        Exit;
      end;
    end
    else
    begin
      ErrMsg(rsFieldNameEmpty);
      aRow := i;
      Exit(False);
    end;
  end;
end;

function TCalcFieldsFm.ShowForm(Fm: TdxForm): Integer;
begin
  FForm := Fm;
  FillFields;
  Result := ShowModal;
  if Result = mrOk then
    SaveFields;
end;

end.

