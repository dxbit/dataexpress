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
unit ColoringForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, Menus, strconsts, EditBtn, StdCtrls, dxctrls;

type

  { TColoringFm }

  TColoringFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ColorDlg: TColorDialog;
    Grid: TStringGrid;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure BtnClick(Sender: TObject);
    procedure ExprEdButtonClick(Sender: TObject);
    procedure ExprEdEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
  private
    { private declarations }
    FBtn: TButton;
    FExprEd: TEditButton;
    FForm: TdxForm;
    procedure FillGrid;
    procedure Save;
    procedure SetControlState;
  public
    { public declarations }
    procedure ShowForm(aForm: TdxForm);
  end;

var
  ColoringFm: TColoringFm;

implementation

uses
  exprform, helpform;

{$R *.lfm}

{ TColoringFm }

procedure TColoringFm.FormCreate(Sender: TObject);
begin
  Caption := rsColoring;
  Grid.Columns[0].Title.Caption:=rsColor;
  Grid.Columns[1].Title.Caption:=rsExpression;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  MenuItem1.Caption:=rsAppend;
  MenuItem2.Caption := rsDelete;
  ToolButton1.Caption := rsAppend;
  ToolButton2.Caption := rsDelete;
  ToolButton3.Caption := rsMoveUp;
  ToolButton4.Caption := rsMoveDown;
  FBtn := TButton.Create(Self);
  FBtn.Caption := '...';
  FBtn.OnClick:=@BtnClick;
  FExprEd := TEditButton.Create(Self);
  FExprEd.Button.Caption:='...';
  FExprEd.OnButtonClick:=@ExprEdButtonClick;
  FExprEd.OnEditingDone:=@ExprEdEditingDone;
  ImageList1.AddLazarusResource('add16');
  ImageList1.AddLazarusResource('delete16');
  ImageList1.AddLazarusResource('up16');
  ImageList1.AddLazarusResource('down16');
  ColorDlg.Title:=rsColor;
end;

procedure TColoringFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
  SetControlState;
end;

procedure TColoringFm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if (aCol = 0) and (aRow > 0) and (Grid.Cells[aCol, aRow] > '') then
  begin
    Grid.Canvas.Brush.Color := StringToColor(Grid.Cells[aCol, aRow]);
    Grid.Canvas.FillRect(aRect);
  end;
end;

procedure TColoringFm.GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if aCol = 0 then Editor := FBtn
  else if aCol = 1 then
  begin
    Editor := FExprEd;
    FExprEd.Text := Grid.Cells[aCol, aRow];
  end;
  Editor.BoundsRect := Grid.CellRect(aCol, aRow);
end;

procedure TColoringFm.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  SetControlState;
end;

procedure TColoringFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('coloring');
end;

procedure TColoringFm.MenuItem1Click(Sender: TObject);
var
  r: Integer;
begin
  r := Grid.RowCount;
  Grid.RowCount := r + 1;
  Grid.Cells[0, r] := ColorToString(clWhite);
  Grid.Row := r;
  SetControlState;
end;

procedure TColoringFm.MenuItem2Click(Sender: TObject);
begin
  Grid.DeleteRow(Grid.Row);
  SetControlState;
end;

procedure TColoringFm.ToolButton1Click(Sender: TObject);
begin
  MenuItem1.Click;
end;

procedure TColoringFm.ToolButton2Click(Sender: TObject);
begin
  MenuItem2.Click;
end;

procedure TColoringFm.ToolButton3Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row - 1);
  //Grid.Row := Grid.Row - 1;
  SetControlState;
end;

procedure TColoringFm.ToolButton4Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row + 1);
  //Grid.Row := Grid.Row + 1;
  SetControlState;
end;

procedure TColoringFm.FillGrid;
var
  SL: TStrings;
  S: String;
  i, p, r: Integer;
begin
  Grid.RowCount := 1;
  SL := FForm.Coloring;
  for i := 0 to SL.Count - 1 do
  begin
    r := i + 1;
    Grid.RowCount := r + 1;
    S := SL[i];
    p := Pos(';', S);
    Grid.Cells[0, r] := Copy(S, 1, p - 1);
    Grid.Cells[1, r] := Copy(S, p + 1, 20000);
  end;
end;

procedure TColoringFm.Save;
var
  SL: TStrings;
  i: Integer;
begin
  SL := FForm.Coloring;
  SL.Clear;
  for i := 1 to Grid.RowCount - 1 do
    SL.Add(Grid.Cells[0, i] + ';' + Grid.Cells[1, i]);
end;

procedure TColoringFm.SetControlState;
begin
  MenuItem2.Enabled := Grid.Row > 0;
  ToolButton2.Enabled:=Grid.Row > 0;
  ToolButton3.Enabled:=Grid.Row > 1;
  ToolButton4.Enabled:=(Grid.Row > 0) and (Grid.Row < Grid.RowCount - 1);
end;

procedure TColoringFm.ShowForm(aForm: TdxForm);
begin
  FForm := aForm;
  FillGrid;
  if ShowModal = mrOk then Save;
end;

procedure TColoringFm.ExprEdButtonClick(Sender: TObject);
var
  S: TCaption;
begin
  S := FExprEd.Text;
  if ExprFm.ShowForm(rsExpression, nil, S, FForm, nil, nil, nil, 'expressions') then
    FExprEd.Text := S;
end;

procedure TColoringFm.BtnClick(Sender: TObject);
begin
  ColorDlg.Color:=StringToColor(Grid.Cells[Grid.Col, Grid.Row]);
  if ColorDlg.Execute then
    Grid.Cells[Grid.Col, Grid.Row] := ColorToString(ColorDlg.Color);
end;

procedure TColoringFm.ExprEdEditingDone(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := FExprEd.Text;
end;

end.

