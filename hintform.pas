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
unit HintForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts, dxctrls;

type

  { THintFm }

  THintFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(C: TdxLabel);
  end;

var
  HintFm: THintFm;

implementation

uses
  helpform;

{$R *.lfm}

{ THintFm }

procedure THintFm.FormCreate(Sender: TObject);
begin
  Caption := rsHint;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure THintFm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure THintFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('Hints');
end;

procedure THintFm.ShowForm(C: TdxLabel);
begin
  Memo1.Text := C.Hint;
  if ShowModal = mrOk then
  begin
    C.Hint := Trim(Memo1.Text);
    C.ShowHint:=C.Hint <> '';
  end;
end;

end.

