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
unit ImportErrorsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TImportErrorsFm }

  TImportErrorsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(SL: TStrings);
  end;

var
  ImportErrorsFm: TImportErrorsFm;

implementation

uses
  helpform;

{$R *.lfm}

{ TImportErrorsFm }

procedure TImportErrorsFm.FormCreate(Sender: TObject);
begin
  Caption := rsImportErrors;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TImportErrorsFm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure TImportErrorsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('importerrors');
end;

procedure TImportErrorsFm.ShowForm(SL: TStrings);
begin
  Memo1.Lines := SL;
  ShowModal;
end;

end.

