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
unit ConnectionForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TConnectionFm }

  TConnectionFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DefPwd: TCheckBox;
    Conn: TEdit;
    Pwd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure DefPwdChange(Sender: TObject);
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
  ConnectionFm: TConnectionFm;

implementation

uses
  dbengine, helpform;

{$R *.lfm}

{ TConnectionFm }

procedure TConnectionFm.DefPwdChange(Sender: TObject);
begin
  Pwd.Enabled:=not DefPwd.Checked;
  if DefPwd.Checked then
    Pwd.Text := 'masterkey';
end;

procedure TConnectionFm.FormCreate(Sender: TObject);
begin
  Caption := rsConnect;
  Label1.Caption := rsConnectToDB;
  Label2.Caption := rsPassword;
  DefPwd.Caption := rsDefaultPwd;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TConnectionFm.FormShow(Sender: TObject);
begin
  Conn.SetFocus;
end;

procedure TConnectionFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('connection');
end;

function TConnectionFm.ShowForm: Integer;
begin
  if DBase.Remote then
    Conn.Text := DBase.Database
  else
    Conn.Text := '';
  Pwd.Text:=DBase.Pwd;
  DefPwd.Checked := CompareText(Pwd.Text, 'masterkey') = 0;
  Result := ShowModal;
end;

end.

