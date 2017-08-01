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
unit RpGridForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, Menus, strconsts, DXReports, myctrls;

type

  { TRpGridFm }

  TRpGridFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    FlatMnu: TPopupMenu;
    ImageList1: TImageList;
    LinesMnu: TPopupMenu;
    LineStyleMnu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    RowHMnu: TPopupMenu;
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
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure FlatMnuPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCanSort(Sender: TObject; Index: Integer; var Cancel: Boolean);
    procedure HelpButtonClick(Sender: TObject);
    procedure LineMnuClick(Sender: TObject);
    procedure LinesMnuPopup(Sender: TObject);
    procedure LineStyleMnuClick(Sender: TObject);
    procedure LineStyleMnuPopup(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
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
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
  private
    { private declarations }
    Grid: TMyGrid;
    FCols: TList;
  public
    { public declarations }
    function ShowForm(RD: TReportData): Integer;
  end;

var
  RpGridFm: TRpGridFm;

implementation

uses
  colorform, fontform, apputils, lists, helpform, gridcolsform;

{$R *.lfm}

{ TRpGridFm }

procedure TRpGridFm.ToolButton2Click(Sender: TObject);
begin
  Grid.GridLineColor:=ColorFm.ShowColor(Grid.GridLineColor);
end;

procedure TRpGridFm.ToolButton3Click(Sender: TObject);
begin
  LineStyleMnu.Popup;
end;

procedure TRpGridFm.ToolButton4Click(Sender: TObject);
begin
  RowHMnu.Popup;
end;

procedure TRpGridFm.ToolButton5Click(Sender: TObject);
begin
  LinesMnu.Popup;
end;

procedure TRpGridFm.ToolButton7Click(Sender: TObject);
begin
  Grid.Color:=ColorFm.ShowColor(Grid.Color);
end;

procedure TRpGridFm.ToolButton9Click(Sender: TObject);
begin
  Grid.AlternateColor:=ColorFm.ShowColor(Grid.AlternateColor);
end;

procedure TRpGridFm.LineStyleMnuClick(Sender: TObject);
begin
  Grid.GridLineStyle:=TPenStyle(TMenuItem(Sender).Tag);
end;

procedure TRpGridFm.LinesMnuPopup(Sender: TObject);
begin
  MenuItem4.Checked := goHorzLine in Grid.Options;
  MenuItem5.Checked := goVertLine in Grid.Options;
end;

procedure TRpGridFm.LineMnuClick(Sender: TObject);
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

procedure TRpGridFm.FlatMnuPopup(Sender: TObject);
begin
  MenuItem6.Checked := Grid.Flat;
end;

procedure TRpGridFm.FormCreate(Sender: TObject);
begin
  Caption := rsReportGrid;
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
  ToolButton16.Hint := rsColor;
  ToolButton17.Hint := rsFont;
  ToolButton6.Caption := rsHeader;
  ToolButton18.Hint := rsColor;
  ToolButton19.Hint := rsFont;
  MenuItem1.Caption := rsSolid;
  MenuItem2.Caption := rsDash;
  MenuItem3.Caption := rsDot;
  MenuItem4.Caption := rsHorz;
  MenuItem5.Caption := rsVert;
  MenuItem6.Caption := rsFlat;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Grid := TMyGrid.Create(Self);
  Grid.Parent := Self;
  Grid.Align := alClient;
  Grid.OnCanSort:=@GridCanSort;
end;

procedure TRpGridFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TRpGridFm.GridCanSort(Sender: TObject; Index: Integer;
  var Cancel: Boolean);
{var
  i: Integer;
  Col: TRpGridColumn; }
begin
  {if index = 0 then Exit;
  i := Grid.Columns[index-1].Tag;
  Col := TRpGridColumn(FCols[i]);
  if LowerCase(Copy(Col.FieldName, 1, 2)) = 'cf' then
  begin
    ErrMsg(rsCantSortCol);
    Cancel := True;
  end;    }
end;

procedure TRpGridFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('querygrid');
end;

procedure TRpGridFm.LineStyleMnuPopup(Sender: TObject);
begin
  MenuItem1.Checked:=Grid.GridLineStyle=psSolid;
  MenuItem2.Checked:=Grid.GridLineStyle=psDash;
  MenuItem3.Checked:=Grid.GridLineStyle=psDot;
end;

procedure TRpGridFm.MenuItem6Click(Sender: TObject);
begin
  Grid.Flat := not Grid.Flat;
end;

procedure TRpGridFm.MenuItem8Click(Sender: TObject);
var
  S: String;
  N: integer;
begin
  S := InputBox(rsRowHeight, rsEnterValue, IntToStr(Grid.DefaultRowHeight));
  if TryStrToInt(S, N) and (N > 0) then
    Grid.DefaultRowHeight:=N;
end;

procedure TRpGridFm.MenuItem9Click(Sender: TObject);
begin
  Grid.WordWrap:=not Grid.WordWrap;
  Grid.Repaint;
end;

procedure TRpGridFm.RowHMnuPopup(Sender: TObject);
begin
  MenuItem9.Checked := Grid.WordWrap;
end;

procedure TRpGridFm.ToolButton10Click(Sender: TObject);
begin
  Grid.SelectedColor:=ColorFm.ShowColor(Grid.SelectedColor);
end;

procedure TRpGridFm.ToolButton11Click(Sender: TObject);
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

procedure TRpGridFm.ToolButton13Click(Sender: TObject);
var
  OldColor: TColor;
begin
  OldColor := Grid.FixedColor;
  Grid.FixedColor:=ColorFm.ShowColor(Grid.FixedColor);
  if OldColor <> Grid.FixedColor then
    Grid.FixedHotColor := ShiftColor(Grid.FixedColor, 10);
end;

procedure TRpGridFm.ToolButton14Click(Sender: TObject);
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
    if Col.Title.Font.IsEqual(OldFont) then
      Col.Title.Font := Grid.TitleFont;
  end;
  OldFont.Free;
end;

procedure TRpGridFm.ToolButton16Click(Sender: TObject);
begin
  if Grid.SelectedColumn = nil then Exit;
  with Grid.SelectedColumn do
  begin
    Color := ColorFm.ShowColor(Color);
    if Color = clDefault then Color := Grid.Color;
  end;
end;

procedure TRpGridFm.ToolButton17Click(Sender: TObject);
begin
  if Grid.SelectedColumn = nil then Exit;
  FontFm.ShowForm(Grid.SelectedColumn.Font);
  if FontFm.IsDefault then Grid.SelectedColumn.Font := Grid.Font;
end;

procedure TRpGridFm.ToolButton18Click(Sender: TObject);
begin
  if Grid.SelectedColumn = nil then Exit;
  with Grid.SelectedColumn.Title do
  begin
    Color := ColorFm.ShowColor(Color);
    if Color = clDefault then Color := Grid.FixedColor;
  end;
end;

procedure TRpGridFm.ToolButton19Click(Sender: TObject);
begin
  if Grid.SelectedColumn = nil then Exit;
  FontFm.ShowForm(Grid.SelectedColumn.Title.Font);
  if FontFm.IsDefault then Grid.SelectedColumn.Title.Font := Grid.TitleFont;
end;

procedure TRpGridFm.ToolButton20Click(Sender: TObject);
begin
  FlatMnu.Popup;
end;

procedure TRpGridFm.ToolButton21Click(Sender: TObject);
begin
  GridColsFm.ShowForm(Grid.Columns);
end;

procedure CopySortColsToMyGrid(G1: TRpGrid; G2: TMyGrid);
var
  i, n: Integer;
  L2: TSortColList;
  L1: TRpGridSortList;
begin
  L1 := G1.SortCols;
  L2 := G2.SortCols;
  L2.Clear;
  for i := 0 to L1.Count - 1 do
  begin
    n := L1[i].Col.Index;
    if (n >= 0) and (n < G2.Columns.Count) then
      L2.AddCol(G2.Columns[n], L1[i].Desc);
  end;
end;

procedure CopySortColsToRpGrid(aCols: TList; G1: TMyGrid; G2: TRpGrid);
var
  i: Integer;
  L1: TSortColList;
  L2: TRpGridSortList;
begin
  L1 := G1.SortCols;
  L2 := G2.SortCols;
  L2.Clear;
  for i := 0 to L1.Count - 1 do
    L2.AddCol(TRpGridColumn(aCols[L1[i].Col.Tag]), L1[i].Desc);
end;

function TRpGridFm.ShowForm(RD: TReportData): Integer;
var
  G: TRpGrid;
  i: Integer;
  C: TRpGridColumn;
  Col: TGridColumn;
  L: TList;
begin
  Caption := rsReportGrid + ': ' + RD.Name;
  Grid.ColWidths[0] := 25;
  Grid.Columns.Clear;
  G := RD.Grid;
  Grid.Color:=G.Color;
  Grid.AlternateColor:=G.AlternateColor;
  Grid.SelectedColor:=G.SelectedColor;
  Grid.FixedColor:=G.FixedColor;
  Grid.FixedHotColor := G.FixedHotColor;
  Grid.GridLineColor:=G.GridLineColor;
  Grid.GridLineStyle:=G.GridLineStyle;
  Grid.Flat:=G.Flat;
  if G.VertLines then Grid.Options:=Grid.Options + [goVertLine]
  else Grid.Options:=Grid.Options - [goVertLine];
  if G.HorzLines then Grid.Options:=Grid.Options + [goHorzLine]
  else Grid.Options:=Grid.Options - [goHorzLine];
  Grid.DefaultRowHeight:=G.DefaultRowHeight;
  Grid.Font := G.Font;
  Grid.TitleFont := G.TitleFont;
  L := TList.Create;
  G.SortColumns(L);
  FCols := L;
  for i := 0 to L.Count - 1 do
  begin
    C := TRpGridColumn(L[i]);
    Col := Grid.Columns.Add;
    Col.Color:=C.Color;
    Col.Font := C.Font;
    Col.Title.Color:=C.FixedColor;
    Col.Title.Font := C.TitleFont;
    Col.Title.Caption:=C.Caption;
    Col.Tag := i;
    Col.Width:=C.Width;
    Col.Visible:=C.Visible;
  end;
  CopySortColsToMyGrid(G, Grid);
  Grid.WordWrap:=G.WordWrap;

  Result := ShowModal;
  if Result <> mrOk then Exit;

  G.Color:=Grid.Color;
  G.AlternateColor:=Grid.AlternateColor;
  G.SelectedColor:=Grid.SelectedColor;
  G.FixedColor:=Grid.FixedColor;
  G.FixedHotColor:=Grid.FixedHotColor;
  G.GridLineColor:=Grid.GridLineColor;
  G.GridLineStyle:=Grid.GridLineStyle;
  G.Flat:=Grid.Flat;
  G.VertLines := goVertLine in Grid.Options;
  G.HorzLines := goHorzLine in Grid.Options;
  G.DefaultRowHeight:=Grid.DefaultRowHeight;
  G.Font.Assign(Grid.Font);
  G.TitleFont.Assign(Grid.TitleFont);
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    Col := Grid.Columns[i];
    C := TRpGridColumn(L[Col.Tag]);
    C.Color:=Col.Color;
    C.Font.Assign(Col.Font);
    C.FixedColor:=Col.Title.Color;
    C.TitleFont.Assign(Col.Title.Font);
    C.Caption:=Col.Title.Caption;
    C.Index:=Col.Index;
    C.Width := Col.Width;
    // Ширина колонки по умолчанию равна 64 пиксела. Данное значение не
    // сохраняется в базе. Из-за этого ширина колонки будет случайной.
    if Col.Width = Col.DefaultWidth then C.Width := Col.Width - 1;

    C.Visible:=Col.Visible;
  end;
  CopySortColsToRpGrid(L, Grid, G);
  G.WordWrap := Grid.WordWrap;
  L.Free;
end;

end.

