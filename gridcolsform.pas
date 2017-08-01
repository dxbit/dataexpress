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
unit GridColsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  ButtonPanel, strconsts, Grids, Menus, StdCtrls, ExtCtrls, Buttons, ComCtrls;

type

  { TGridColsFm }

  TGridColsFm = class(TForm)
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
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(aCols: TGridColumns): Integer;
  end;

var
  GridColsFm: TGridColsFm;

implementation

uses
  apputils;

{$R *.lfm}

{ TGridColsFm }

procedure TGridColsFm.FormCreate(Sender: TObject);
begin
  Caption := rsVisibleColumns;
  MenuItem1.Caption := rsSelectAll;
  SetMenuItemImage(MenuItem1, 'check16');
  MenuItem2.Caption := rsClearAll;
  SetMenuItemImage(MenuItem2, 'delete16');
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ImageList1.AddLazarusResource('check16');
  ImageList1.AddLazarusResource('delete16');
  ToolButton1.Caption := rsSelectAll;
  ToolButton2.Caption := rsClearAll;
end;

procedure TGridColsFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
end;

procedure TGridColsFm.MenuItem1Click(Sender: TObject);
begin
  List.CheckAll(cbChecked);
end;

procedure TGridColsFm.MenuItem2Click(Sender: TObject);
begin
  List.CheckAll(cbUnChecked);
end;

procedure TGridColsFm.ToolButton1Click(Sender: TObject);
begin
  MenuItem1.Click;
end;

procedure TGridColsFm.ToolButton2Click(Sender: TObject);
begin
  MenuItem2.Click;
end;

function TGridColsFm.ShowForm(aCols: TGridColumns): Integer;
var
  i: Integer;
  C: TGridColumn;
  Tmp: Boolean;
begin
  List.Clear;
  for i := 0 to aCols.Count - 1 do
  begin
    C := aCols[i];
    List.Items.Add(C.Title.Caption);
    List.Checked[i] := C.Visible;
  end;
  Result := ShowModal;
  if Result <> mrOk then Exit;
  for i := 0 to aCols.Count - 1 do
  begin
    C := aCols[i];
    Tmp := C.Visible;
    C.Visible := List.Checked[i];
    if (not Tmp) and (C.Visible) then C.Width:=100;
  end;
end;

end.

