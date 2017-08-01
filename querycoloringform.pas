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
unit QueryColoringForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, Grids, ButtonPanel, dxreports, strconsts, StdCtrls, Buttons,
  dxctrls, editbtn;

type

  { TQueryColoringFm }

  TQueryColoringFm = class(TForm)
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
    procedure GridPickListSelect(Sender: TObject);
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
    FRD: TReportData;
    FFm: TdxForm;
    FBtn: TBitBtn;
    FExprEd: TEditButton;
    procedure FillFields;
    procedure FillGrid;
    procedure Save;
    procedure SetControlState;
  public
    { public declarations }
    function ShowForm(aRD: TReportData; aFm: TdxForm): Integer;
  end;

var
  QueryColoringFm: TQueryColoringFm;

implementation

uses
  apputils, exprform, helpform, mytypes;

{$R *.lfm}

{ TQueryColoringFm }

procedure TQueryColoringFm.GridSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if aRow = 0 then Exit;
  if aCol = 0 then
  begin
    Editor := FBtn;
    Editor.BoundsRect := Grid.CellRect(aCol, aRow);
  end
  else if aCol = 1 then
  begin
    if Editor is TPickListCellEditor then
      TPickListCellEditor(Editor).Style:=csDropDownList;
  end
  else if aCol = 2 then
  begin
    Editor := FExprEd;
    FExprEd.Text := Grid.Cells[aCol, aRow];
    Editor.BoundsRect := Grid.CellRect(aCol, aRow);
  end;
end;

procedure TQueryColoringFm.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  SetControlState;
end;

procedure TQueryColoringFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('rpqrycoloring');
end;

procedure TQueryColoringFm.MenuItem1Click(Sender: TObject);
var
  r: Integer;
begin
  r := Grid.RowCount;
  Grid.RowCount := r + 1;
  Grid.Cells[0, r] := ColorToString(clWhite);
  Grid.Row := r;
  SetControlState;
end;

procedure TQueryColoringFm.MenuItem2Click(Sender: TObject);
begin
  Grid.DeleteRow(Grid.Row);
  SetControlState;
end;

procedure TQueryColoringFm.ToolButton1Click(Sender: TObject);
begin
  MenuItem1.Click;
end;

procedure TQueryColoringFm.ToolButton2Click(Sender: TObject);
begin
  MenuItem2.Click;
end;

procedure TQueryColoringFm.ToolButton3Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row - 1);
  SetControlState;
end;

procedure TQueryColoringFm.ToolButton4Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row + 1);
  SetControlState;
end;

procedure TQueryColoringFm.FormCreate(Sender: TObject);
begin
  Caption := rsColoring;
  Grid.Columns[0].Title.Caption:=rsColor;
  Grid.Columns[1].Title.Caption:=rsFieldName;
  Grid.Columns[2].Title.Caption:=rsExpression;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  MenuItem1.Caption:=rsAppend;
  MenuItem2.Caption := rsDelete;
  ToolButton1.Caption := rsAppend;
  ToolButton2.Caption := rsDelete;
  ToolButton3.Caption := rsMoveUp;
  ToolButton4.Caption := rsMoveDown;
  FBtn := TBitBtn.Create(Self);
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

procedure TQueryColoringFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
  SetControlState;
end;

procedure TQueryColoringFm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if (aCol = 0) and (aRow > 0) and (Grid.Cells[aCol, aRow] > '') then
  begin
    Grid.Canvas.Brush.Color := StringToColor(Grid.Cells[aCol, aRow]);
    Grid.Canvas.FillRect(aRect);
  end;
end;

procedure TQueryColoringFm.GridPickListSelect(Sender: TObject);
begin
  if Grid.Col = 1 then
    with TPickListCellEditor(Grid.Editor) do
      Grid.Objects[Grid.Col, Grid.Row] := Items.Objects[ItemIndex];
end;

procedure TQueryColoringFm.BtnClick(Sender: TObject);
begin
  ColorDlg.Color:=StringToColor(Grid.Cells[Grid.Col, Grid.Row]);
  if ColorDlg.Execute then
    Grid.Cells[Grid.Col, Grid.Row] := ColorToString(ColorDlg.Color);
end;

procedure TQueryColoringFm.ExprEdButtonClick(Sender: TObject);
var
  S: TCaption;
begin
  S := FExprEd.Text;
  if ExprFm.ShowForm(rsExpression, nil, S, FFm, nil, nil, FRD, 'expressions') then
    FExprEd.Text := S;
end;

procedure TQueryColoringFm.ExprEdEditingDone(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := FExprEd.Text;
end;

procedure TQueryColoringFm.FillGrid;
var
  L: TRpColoringList;
  i, r: Integer;
  CD: TRpColoringData;
  Col: TRpGridColumn;
begin
  L := FRD.Coloring;
  Grid.RowCount := 1;
  for i := 0 to L.Count - 1 do
  begin
    CD := L[i];
    r := i + 1;
    Grid.RowCount := r + 1;
    Grid.Cells[0, r] := ColorToString(CD.Color);
    Col := FRD.Grid.FindColumnByFieldName(CD.FieldName);
    if Col <> nil then
      Grid.Cells[1, r] := Col.Caption;
    Grid.Objects[1, r] := Col;
    Grid.Cells[2, r] := CD.Expr;
  end;
end;

procedure TQueryColoringFm.FillFields;
var
  i: Integer;
  SL: TStringListUtf8;
  pF: PRpField;
  Col: TRpGridColumn;
begin
  SL := TStringListUtf8.Create;
  SL.Add('');
  for i := 0 to FRD.Grid.ColumnCount - 1 do
  begin
    Col := FRD.Grid.Columns[i];
    SL.AddObject(Col.Caption, Col);
  end;
  SL.Sort;
  Grid.Columns[1].PickList := SL;
  SL.Free;
end;

procedure TQueryColoringFm.Save;
var
  L: TRpColoringList;
  i: Integer;
  CD: TRpColoringData;
  Col: TRpGridColumn;
begin
  L := FRD.Coloring;
  L.Clear;
  for i := 1 to Grid.RowCount - 1 do
  begin
    CD := L.AddColoring;
    CD.Color:=StringToColor(Grid.Cells[0, i]);
    Col := TRpGridColumn(Grid.Objects[1, i]);
    if Col <> nil then
      CD.FieldName:=Col.FieldName;
    CD.Expr:=Grid.Cells[2, i];
  end;
end;

procedure TQueryColoringFm.SetControlState;
begin
  MenuItem2.Enabled := Grid.Row > 0;
  ToolButton2.Enabled:=Grid.Row > 0;
  ToolButton3.Enabled:=Grid.Row > 1;
  ToolButton4.Enabled:=(Grid.Row > 0) and (Grid.Row < Grid.RowCount - 1);
end;

function TQueryColoringFm.ShowForm(aRD: TReportData; aFm: TdxForm): Integer;
begin
  Result := mrNone;
  FRD := aRD;
  FFm := aFm;
  if FRD.Sources.Count = 0 then
  begin
    ErrMsg(rsSourceNotSel);
    Exit;
  end;
  FillFields;
  FillGrid;
  Result := ShowModal;
  if Result = mrOk then Save;
end;

end.

