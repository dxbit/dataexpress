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
unit CheckPrintOutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts, dxctrls;

type

  { TCheckPrintOutFm }

  TCheckPrintOutFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(Check: TdxCheckBox): Integer;
  end;

var
  CheckPrintOutFm: TCheckPrintOutFm;

implementation

uses
  helpform;

{$R *.lfm}

{ TCheckPrintOutFm }

procedure TCheckPrintOutFm.FormCreate(Sender: TObject);
begin
  Caption := rsPrintOut;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TCheckPrintOutFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('checkout');
end;

function TCheckPrintOutFm.ShowForm(Check: TdxCheckBox): Integer;
begin
  Edit1.Text := Check.CheckedText;
  Edit2.Text := Check.UnCheckedText;
  Result := ShowModal;
  if Result <> mrOk then Exit;
  Check.CheckedText:=Edit1.Text;
  Check.UnCheckedText:=Edit2.Text;
end;

end.

