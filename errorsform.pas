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
unit ErrorsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TErrorsFm }

  TErrorsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(const Err: String);
  end;

var
  ErrorsFm: TErrorsFm;

implementation

uses
  helpform;

{$R *.lfm}

{ TErrorsFm }

procedure TErrorsFm.FormCreate(Sender: TObject);
begin
  Caption := rsErrors;
  ButtonPanel1.CloseButton.Caption:=rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TErrorsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('errors');
end;

procedure TErrorsFm.ShowForm(const Err: String);
begin
  Memo1.Lines.Text := Err;
  ShowModal;
end;

end.

