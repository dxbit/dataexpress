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
unit NewGridForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, Menus, ExtCtrls, dxctrls, strconsts, lists, myctrls;

type

  { TGrdFm }

  TGrdFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    LineStyleMnu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    LinesMnu: TPopupMenu;
    FlatMnu: TPopupMenu;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    RowHMnu: TPopupMenu;
    RdOnlyMnu: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure RdOnlyMnuPopup(Sender: TObject);
    procedure FlatMnuPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure LineMnuClick(Sender: TObject);
    procedure LinesMnuPopup(Sender: TObject);
    procedure LineStyleMnuClick(Sender: TObject);
    procedure LineStyleMnuPopup(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure RowHMnuPopup(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
    procedure ToolButton14Click(Sender: TObject);
    procedure ToolButton16Click(Sender: TObject);
    procedure ToolButton17Click(Sender: TObject);
    procedure ToolButton18Click(Sender: TObject);
    procedure ToolButton19Click(Sender: TObject);
    procedure ToolButton20Click(Sender: TObject);
    procedure ToolButton21Click(Sender: TObject);
    procedure ToolButton22Click(Sender: TObject);
    procedure ToolButton23Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
  private
    FForm: TdxForm;
    FGrid: TdxGrid;
    Grid: TMyGrid;
    { private declarations }
  public
    { public declarations }
    function ShowForm(aForm: TdxForm): Integer;
  end;

var
  GrdFm: TGrdFm;

implementation

uses
  colorform, fontform, gridcolsform, DBGrids, LazUtf8, helpform, apputils;

{$R *.lfm}

{ TGrdFm }

procedure TGrdFm.ToolButton2Click(Sender: TObject);
begin
  Grid.GridLineColor:=ColorFm.ShowColor(Grid.GridLineColor);
end;

procedure TGrdFm.ToolButton3Click(Sender: TObject);
begin
  LineStyleMnu.Popup;
end;

procedure TGrdFm.ToolButton4Click(Sender: TObject);
begin
  RowHMnu.Popup;
end;

procedure TGrdFm.ToolButton5Click(Sender: TObject);
begin
  LinesMnu.Popup;
end;

procedure TGrdFm.ToolButton7Click(Sender: TObject);
begin
  Grid.Color:=ColorFm.ShowColor(Grid.Color);
end;

procedure TGrdFm.ToolButton9Click(Sender: TObject);
begin
  Grid.AlternateColor:=ColorFm.ShowColor(Grid.AlternateColor);
end;

procedure TGrdFm.LineStyleMnuClick(Sender: TObject);
begin
  Grid.GridLineStyle:=TPenStyle(TMenuItem(Sender).Tag);
end;

procedure TGrdFm.LinesMnuPopup(Sender: TObject);
begin
  MenuItem4.Checked := goHorzLine in Grid.Options;
  MenuItem5.Checked := goVertLine in Grid.Options;
end;

procedure TGrdFm.LineMnuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0:
      if goHorzLine in Grid.Options then
        Grid.Options := Grid.Options - [goHorzLine{, goFixedHorzLine}]
      else
        Grid.Options := Grid.Options + [goHorzLine{, goFixedHorzLine}];
    1:
      if goVertLine in Grid.Options then
        Grid.Options := Grid.Options - [goVertLine{, goFixedVertLine}]
      else
        Grid.Options := Grid.Options + [goVertLine{, goFixedVertLine}];
  end;
end;

procedure TGrdFm.FlatMnuPopup(Sender: TObject);
begin
  MenuItem6.Checked := Grid.Flat;
end;

procedure TGrdFm.RdOnlyMnuPopup(Sender: TObject);
begin
  MenuItem7.Checked := not (goEditing in Grid.Options);
end;

procedure TGrdFm.MenuItem8Click(Sender: TObject);
var
  S: String;
  N: integer;
begin
  S := InputBox(rsRowHeight, rsEnterValue, IntToStr(Grid.DefaultRowHeight));
  if TryStrToInt(S, N) and (N > 0) then
    Grid.DefaultRowHeight:=N;
end;

procedure TGrdFm.MenuItem9Click(Sender: TObject);
begin
  Grid.WordWrap:=not Grid.WordWrap;
  Grid.Repaint;
end;

procedure TGrdFm.FormCreate(Sender: TObject);
begin
  Caption := rsTable;
  ToolButton1.Caption := rsTable;
  ToolButton5.Hint := rsLines;
  ToolButton3.Hint := rsLineStyle;
  ToolButton4.Hint := rsRowHeight;
  ToolButton2.Hint := rsColor;
  ToolButton8.Caption := rsDataArea;
  ToolButton7.Hint := rsColor;
  ToolButton9.Hint := rsAlternateColor;
  ToolButton10.Hint := rsSelectionColor;
  ToolButton11.Hint := rsFont;
  ToolButton12.Caption := rsHeaders;
  ToolButton13.Hint := rsColor;
  ToolButton14.Hint := rsFont;
  ToolButton20.Hint := rsFlat;
  ToolButton15.Caption := rsColumn;
  ToolButton22.Hint := rsVisibleColumns;
  ToolButton16.Hint := rsColor;
  ToolButton17.Hint := rsFont;
  ToolButton6.Caption := rsHeader;
  ToolButton18.Hint := rsColor;
  ToolButton19.Hint := rsFont;
  ToolButton21.Hint := rsTitle;
  ToolButton23.Hint := rsReadOnly;
  MenuItem1.Caption := rsSolid;
  MenuItem2.Caption := rsDash;
  MenuItem3.Caption := rsDot;
  MenuItem4.Caption := rsHorz;
  MenuItem5.Caption := rsVert;
  MenuItem6.Caption := rsFlat;
  MenuITem7.Caption:=rsReadOnly;
  MenuItem8.Caption := rsRowHeight;
  MenuItem9.Caption:=rsWordWrap;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Grid := TMyGrid.Create(Self);
  Grid.Parent := Self;
  Grid.Align := alClient;
end;

procedure TGrdFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TGrdFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('formgrid');
end;

procedure TGrdFm.LineStyleMnuPopup(Sender: TObject);
begin
  MenuItem1.Checked:=Grid.GridLineStyle=psSolid;
  MenuItem2.Checked:=Grid.GridLineStyle=psDash;
  MenuItem3.Checked:=Grid.GridLineStyle=psDot;
end;

procedure TGrdFm.MenuItem6Click(Sender: TObject);
begin
  Grid.Flat := not Grid.Flat;
end;

procedure TGrdFm.MenuItem7Click(Sender: TObject);
begin
  if goEditing in Grid.Options then
    Grid.Options := Grid.Options - [goEditing]
  else
    Grid.Options := Grid.Options + [goEditing]
end;

procedure TGrdFm.RowHMnuPopup(Sender: TObject);
begin
  MenuItem9.Checked:=Grid.WordWrap;
end;

procedure TGrdFm.ToolButton10Click(Sender: TObject);
begin
  Grid.SelectedColor:=ColorFm.ShowColor(Grid.SelectedColor);
end;

procedure TGrdFm.ToolButton11Click(Sender: TObject);
var
  OldFont: TFont;
  i: Integer;
  Col: TGridColumn;
begin
  OldFont := TFont.Create;
  OldFont.Assign(Grid.Font);
  FontFm.ShowForm(Grid.Font);
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    Col := Grid.Columns[i];
    if Col.Font.IsEqual(OldFont) then Col.Font := Grid.Font;
  end;
  OldFont.Free;
end;

procedure TGrdFm.ToolButton13Click(Sender: TObject);
var
  OldColor: TColor;
begin
  OldColor := Grid.FixedColor;
  Grid.FixedColor:=ColorFm.ShowColor(Grid.FixedColor);
  if OldColor <> Grid.FixedColor then
    Grid.FixedHotColor := ShiftColor(Grid.FixedColor, 10);
end;

procedure TGrdFm.ToolButton14Click(Sender: TObject);
var
  OldFont: TFont;
  i: Integer;
  Col: TGridColumn;
begin
  OldFont := TFont.Create;
  OldFont.Assign(Grid.TitleFont);
  FontFm.ShowForm(Grid.TitleFont);
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    Col := Grid.Columns[i];
    if Col.Title.Font.IsEqual(OldFont) then Col.Title.Font := Grid.TitleFont;
  end;
  OldFont.Free;
end;

procedure TGrdFm.ToolButton16Click(Sender: TObject);
begin
  if Grid.SelectedColumn = nil then Exit;
  with Grid.SelectedColumn do
  begin
    Color := ColorFm.ShowColor(Color);
    if Color = clDefault then Color := Grid.Color;
  end;
end;

procedure TGrdFm.ToolButton17Click(Sender: TObject);
begin
  if Grid.SelectedColumn = nil then Exit;
  FontFm.ShowForm(Grid.SelectedColumn.Font);
  if FontFm.IsDefault then Grid.SelectedColumn.Font := Grid.Font;
end;

procedure TGrdFm.ToolButton18Click(Sender: TObject);
begin
  if Grid.SelectedColumn = nil then Exit;
  with Grid.SelectedColumn.Title do
  begin
    Color := ColorFm.ShowColor(Color);
    if Color = clDefault then Color := Grid.FixedColor;
  end;
end;

procedure TGrdFm.ToolButton19Click(Sender: TObject);
begin
  if Grid.SelectedColumn = nil then Exit;
  FontFm.ShowForm(Grid.SelectedColumn.Title.Font);
  if FontFm.IsDefault then Grid.SelectedColumn.Title.Font := Grid.TitleFont;
end;

procedure TGrdFm.ToolButton20Click(Sender: TObject);
begin
  FlatMnu.Popup;
end;

procedure TGrdFm.ToolButton21Click(Sender: TObject);
var
  Col: TGridColumn;
  S: String;
begin
  Col := Grid.SelectedColumn;
  if Col = nil then Exit;
  S := InputBox(rsTitle, rsEnterColumnTitle, Col.Title.Caption);
  Col.Title.Caption := S;
end;

procedure TGrdFm.ToolButton22Click(Sender: TObject);
begin
  GridColsFm.ShowForm(Grid.Columns);
end;

procedure TGrdFm.ToolButton23Click(Sender: TObject);
begin
  RdOnlyMnu.Popup;
end;

procedure CopySortColsToMyGrid(G1: TdxGrid; G2: TMyGrid);
var
  i: Integer;
  L1, L2: TSortColList;
begin
  L1 := G1.SortCols;
  L2 := G2.SortCols;
  L2.Clear;
  for i := 0 to L1.Count - 1 do
    L2.AddCol(G2.Columns[L1[i].Col.Index], L1[i].Desc);
end;

procedure CopySortColsToDxGrid(G1: TMyGrid; G2: TdxGrid);
var
  i: Integer;
  L1, L2: TSortColList;
begin
  L1 := G1.SortCols;
  L2 := G2.SortCols;
  L2.Clear;
  for i := 0 to L1.Count - 1 do
    L2.AddCol(G2.Columns[L1[i].Col.Index], L1[i].Desc);
end;

function TGrdFm.ShowForm(aForm: TdxForm): Integer;
var
  i: Integer;
  C: TGridColumn;
  C2: TColumn;
  Cmp: TComponent;
begin
  FForm := aForm;
  FGrid := aForm.Grid;
  Grid.ColWidths[0] := 25;
  Grid.GridLineColor:=FGrid.GridLineColor;
  Grid.GridLineStyle := FGrid.GridLineStyle;
  Grid.DefaultRowHeight:=FGrid.DefaultRowHeight;
  Grid.Color:=FGrid.Color;
  Grid.AlternateColor:=FGrid.AlternateColor;
  Grid.SelectedColor:=FGrid.SelectedColor;
  Grid.Font := FGrid.Font;
  Grid.FixedColor:=FGrid.FixedColor;
  Grid.FixedHotColor:=FGrid.FixedHotColor;
  Grid.TitleFont := FGrid.TitleFont;
  Grid.Flat:=FGrid.Flat;
  if FGrid.ReadOnly then
    Grid.Options := Grid.Options - [goEditing]
  else
    Grid.Options := Grid.Options + [goEditing];
  if dgRowLines in FGrid.Options then Grid.Options := Grid.Options + [goHorzLine{, goFixedHorzLine}]
  else Grid.Options := Grid.Options - [goHorzLine{, goFixedHorzLine}];
  if dgColLines in FGrid.Options then Grid.Options := Grid.Options + [goVertLine{, goFixedVertLine}]
  else Grid.Options := Grid.Options - [goVertLine{, goFixedVertLine}];
  Grid.Columns.Clear;
  for i := 0 to FGrid.Columns.Count - 1 do
  begin
    C := Grid.Columns.Add;
    C2 := FGrid.Columns[i];
    C.Visible:=C2.Visible;
    C.Font := C2.Font;
    C.Color := C2.Color;
    C.Title.Font := C2.Title.Font;
    C.Title.Color := C2.Title.Color;
    Cmp := FindById(FForm, C2.Tag);
    if C2.Title.Caption = ' ' then
      C.Title.Caption := GetFieldName(Cmp)
    else
      C.Title.Caption := C2.Title.Caption;
    C.Width:=C2.Width;
    C.Index:=C2.Index;
    C.Tag := C2.Tag;
  end;
  CopySortColsToMyGrid(FGrid, Grid);
  Grid.WordWrap:=FGrid.WordWrap;

  Result := ShowModal;
  if Result <> mrOk then Exit;

  FGrid.GridLineColor:=Grid.GridLineColor;
  FGrid.GridLineStyle := Grid.GridLineStyle;
  FGrid.DefaultRowHeight:=Grid.DefaultRowHeight;
  FGrid.Color:=Grid.Color;
  FGrid.AlternateColor:=Grid.AlternateColor;
  FGrid.SelectedColor:=Grid.SelectedColor;
  FGrid.Font := Grid.Font;
  FGrid.FixedColor:=Grid.FixedColor;
  FGrid.FixedHotColor:=Grid.FixedHotColor;
  FGrid.TitleFont := Grid.TitleFont;
  FGrid.Flat:=Grid.Flat;
  FGrid.ReadOnly:=not (goEditing in Grid.Options);
  if goHorzLine in Grid.Options then FGrid.Options := FGrid.Options + [dgRowLines]
  else FGrid.Options := FGrid.Options - [dgRowLines];
  if goVertLine in Grid.Options then FGrid.Options := FGrid.Options + [dgColLines]
  else FGrid.Options := FGrid.Options - [dgColLines];
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    C := Grid.Columns[i];
    C2 := FindColumnByTag(FGrid, C.Tag);
    C2.Visible:=C.Visible;
    C2.Font := C.Font;
    C2.Color := C.Color;
    C2.Title.Font := C.Title.Font;
    C2.Title.Color := C.Title.Color;
    Cmp := FindById(FForm, C2.Tag);
    if Utf8CompareText(GetFieldName(Cmp), C.Title.Caption) = 0 then
      C2.Title.Caption := ' '
    else
      C2.Title.Caption := C.Title.Caption;
    C2.Width:=C.Width;
    // Ширина колонки по умолчанию равна 64 пиксела. Данное значение не
    // сохраняется в базе. Из-за этого ширина колонки будет случайной.
    if C.Width = C.DefaultWidth then C2.Width := C.Width - 1;
    C2.Index:=C.Index;
  end;
  CopySortColsToDxGrid(Grid, FGrid);
  FGrid.WordWrap:=Grid.WordWrap;
end;

end.

