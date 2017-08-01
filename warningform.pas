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
unit WarningForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, ExtCtrls, strconsts;

type

  { TWarnFm }

  TWarnFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Image1: TImage;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(const Msg: String): Integer;
  end;

var
  WarnFm: TWarnFm;

implementation

{$R *.lfm}

{ TWarnFm }

procedure TWarnFm.FormCreate(Sender: TObject);
begin
  Caption := rsWarning;
  ButtonPanel1.OKButton.Caption:=rsYesMsg;
  ButtonPanel1.CancelButton.Caption:=rsNoMsg;
end;

function TWarnFm.ShowForm(const Msg: String): Integer;
begin
  Label1.Caption := Msg;
  Result := ShowModal;
end;

end.

