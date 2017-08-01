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
unit FixedListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel, Spin, strconsts, dxctrls;

type

  { TFixedListFm }

  TFixedListFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    RowCnt: TSpinEdit;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(C: TdxComboBox);
  end;

var
  FixedListFm: TFixedListFm;

implementation

{$R *.lfm}

{ TFixedListFm }

procedure TFixedListFm.FormCreate(Sender: TObject);
begin
  Caption := rsList;
  CheckBox1.Caption:=rsOnlyList;
  Label2.Caption := rsRowCountInList;
  ButtonPanel1.OKButton.Caption := rsOK;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TFixedListFm.ShowForm(C: TdxComboBox);
begin
  Memo1.Lines := C.Items;
  CheckBox1.Checked := C.Style = csDropDownList;
  CheckBox1.Enabled := C.SourceTId = 0;
  RowCnt.Value := C.DropDownCount;
  if ShowModal = mrOk then
  begin
    if (CheckBox1.Checked) and (C.SourceTId = 0) then
      C.Style := csDropDownList
    else C.Style := csDropDown;
    C.Items := Memo1.Lines;
    C.DropDownCount:=RowCnt.Value;
  end;
end;

end.

