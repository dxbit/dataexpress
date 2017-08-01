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
unit DeleteRecsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, StdCtrls, strconsts;

type

  { TDeleteRecsFm }

  TDeleteRecsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm: Integer;
  end;

var
  DeleteRecsFm: TDeleteRecsFm;

implementation

uses
  helpform;

{$R *.lfm}

{ TDeleteRecsFm }

procedure TDeleteRecsFm.CheckBox1Change(Sender: TObject);
begin
  ButtonPanel1.OKButton.Enabled:=CheckBox1.Checked;
end;

procedure TDeleteRecsFm.FormCreate(Sender: TObject);
begin
  Caption := rsDeleteRecords;
  Label1.Caption := Format(rsDelRecsMsg, [LineEnding + LineEnding, LineEnding + LineEnding]);
  CheckBox1.Caption := rsDelRecsConfirm;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TDeleteRecsFm.FormShow(Sender: TObject);
begin
  ButtonPanel1.CancelButton.SetFocus;
end;

procedure TDeleteRecsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('deleterecs');
end;

function TDeleteRecsFm.ShowForm: Integer;
begin
  CheckBox1.Checked := False;
  ButtonPanel1.OkButton.Enabled:=False;
  Result := ShowModal;
end;

end.

