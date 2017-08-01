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
unit StyleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  ButtonPanel, StdCtrls, Menus, ComCtrls, strconsts, dxctrls;

type

  { TStyleFm }

  TStyleFm = class(TForm)
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
    FFm: TdxForm;
    { private declarations }
    procedure CopyStyle(aForm: TdxForm);
  public
    { public declarations }
    function ShowForm(aForm: TdxForm; SL: TStrings): Integer;
  end;

var
  StyleFm: TStyleFm;

implementation

uses
  helpform;

{$R *.lfm}

{ TStyleFm }

procedure TStyleFm.FormCreate(Sender: TObject);
begin
  Caption := rsCopyStyle;
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

procedure TStyleFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
end;

procedure TStyleFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('copyformstyle');
end;

procedure TStyleFm.MenuItem1Click(Sender: TObject);
begin
  List.CheckAll(cbChecked);
end;

procedure TStyleFm.MenuItem2Click(Sender: TObject);
begin
  List.CheckAll(cbUnChecked);
end;

procedure TStyleFm.ToolButton1Click(Sender: TObject);
begin
  MenuItem1.Click;
end;

procedure TStyleFm.ToolButton2Click(Sender: TObject);
begin
  MenuItem2.Click;
end;

procedure TStyleFm.CopyStyle(aForm: TdxForm);
begin
  aForm.Font := FFm.Font;
  aForm.Color := FFm.Color;
  aForm.Grid.Color:=FFm.Grid.Color;
  aForm.Grid.AlternateColor:=FFm.Grid.AlternateColor;
  aForm.Grid.SelectedColor:=FFm.Grid.SelectedColor;
  aForm.Grid.DefaultRowHeight:=FFm.Grid.DefaultRowHeight;
  aForm.Grid.Flat:=FFm.Grid.Flat;
  aForm.Grid.Options:=FFm.Grid.Options;
  aForm.Grid.GridLineColor:=FFm.Grid.GridLineColor;
  aForm.Grid.Font := FFm.Grid.Font;
  aForm.Grid.TitleFont := FFm.Grid.TitleFont;
  aForm.Grid.FixedColor:=FFm.Grid.FixedColor;
  aForm.Grid.FixedHotColor:=FFm.Grid.FixedHotColor;
  aForm.Grid.WordWrap:=FFm.Grid.WordWrap;
  aForm.TreeBackColor:=FFm.TreeBackColor;
  aForm.TreeFont := FFm.TreeFont;
  aForm.TreeLineColor:=FFm.TreeLineColor;
  aForm.TreeSelectColor:=FFm.TreeSelectColor;
end;

function TStyleFm.ShowForm(aForm: TdxForm; SL: TStrings): Integer;
var
  i: Integer;
  F: TdxForm;
begin
  FFm := aForm;
  List.Items := SL;
  List.Items.Delete( List.Items.IndexOfObject(aForm) );
  Result := ShowModal;
  if Result <> mrOk then Exit;
  for i := 0 to List.Count - 1 do
  begin
    if not List.Checked[i] then Continue;
    F := TdxForm(List.Items.Objects[i]);
    CopyStyle(F);
  end;
end;

end.

