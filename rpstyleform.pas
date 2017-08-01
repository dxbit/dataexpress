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
unit RpStyleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CheckLst, ButtonPanel, Menus, ComCtrls, strconsts, DXReports;

type

  { TRpStyleFm }

  TRpStyleFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    List: TCheckListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(RD: TReportData; SL: TStrings): Integer;
  end;

var
  RpStyleFm: TRpStyleFm;

implementation

uses
  helpform;

{$R *.lfm}

{ TRpStyleFm }

procedure TRpStyleFm.FormCreate(Sender: TObject);
begin
  Caption := rsCopyReportStyle;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  MenuItem1.Caption:=rsSelectAll;
  MenuItem2.Caption := rsClearAll;
  ImageList1.AddLazarusResource('check16');
  ImageList1.AddLazarusResource('delete16');
  ToolButton1.Caption := rsSelectAll;
  ToolButton2.Caption := rsClearAll;
end;

procedure TRpStyleFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
end;

procedure TRpStyleFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('copyrpstyle');
end;

procedure TRpStyleFm.MenuItem1Click(Sender: TObject);
begin
  List.CheckAll(cbChecked);
end;

procedure TRpStyleFm.MenuItem2Click(Sender: TObject);
begin
  List.CheckAll(cbUnChecked);
end;

procedure TRpStyleFm.ToolButton1Click(Sender: TObject);
begin
  MenuItem1.Click;
end;

procedure TRpStyleFm.ToolButton2Click(Sender: TObject);
begin
  MenuItem2.Click;
end;

procedure CopyStyle(RD1, RD2: TReportData);
var
  i: Integer;
begin
  RD2.Grid.Color := RD1.Grid.Color;
  RD2.Grid.AlternateColor:=RD1.Grid.AlternateColor;
  RD2.Grid.SelectedColor:=RD1.Grid.SelectedColor;
  RD2.Grid.FixedColor:=RD1.Grid.FixedColor;
  RD2.Grid.FixedHotColor:=RD1.Grid.FixedHotColor;
  RD2.Grid.GridLineColor:=RD1.Grid.GridLineColor;
  RD2.Grid.GridLineStyle:=RD1.Grid.GridLineStyle;
  RD2.Grid.Flat:=RD1.Grid.Flat;
  RD2.Grid.DefaultRowHeight:=RD1.Grid.DefaultRowHeight;
  RD2.Grid.VertLines:=RD1.Grid.VertLines;
  RD2.Grid.HorzLines:=RD1.Grid.HorzLines;
  RD2.Grid.WordWrap:=RD1.Grid.WordWrap;

  RD2.Grid.Font.Assign(RD1.Grid.Font);
  RD2.Grid.TitleFont.Assign(RD1.Grid.TitleFont);
  with RD2.Grid do
    for i := 0 to ColumnCount - 1 do
    begin
      Columns[i].Font.Assign(Font);
      Columns[i].TitleFont.Assign(TitleFont);
      Columns[i].Color:=Color;
      Columns[i].FixedColor:=FixedColor;
    end;
end;

function TRpStyleFm.ShowForm(RD: TReportData; SL: TStrings): Integer;
var
  i: Integer;
begin
  List.Items := SL;
  List.Items.Delete( List.Items.IndexOfObject(RD) );
  Result := ShowModal;
  if Result <> mrOk then Exit;
  for i := 0 to List.Count - 1 do
    if List.Checked[i] then CopyStyle(RD, TReportData(List.Items.Objects[i]));
end;

end.

