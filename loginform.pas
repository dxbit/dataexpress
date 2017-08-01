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
unit LoginForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts, dxusers;

type

  { TLoginFm }

  TLoginFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    SingleMode: TCheckBox;
    Pwd: TEdit;
    Label2: TLabel;
    Users: TComboBox;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UsersChange(Sender: TObject);
  private
    { private declarations }
    procedure FillUsers;
  public
    { public declarations }
    function ShowForm(aUser: TdxUser): Integer;
  end;

var
  LoginFm: TLoginFm;

implementation

uses
  apputils, md5;

{$R *.lfm}

{ TLoginFm }

procedure TLoginFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  U: TdxUser;
begin
  if ModalResult = mrOk then
  begin
    if Users.ItemIndex < 0 then
    begin
      ErrMsg(rsUserNotSel);
      Users.SetFocus;
      CanClose := False;
      Exit;
    end;
    U := TdxUser(Users.Items.Objects[Users.ItemIndex]);
    if md5print(md5string(Pwd.Text)) <> U.Password then
    begin
      ErrMsg(rsInvalidPwd);
      Pwd.SetFocus;
      CanClose := False;
    end;
  end;
end;

procedure TLoginFm.FormCreate(Sender: TObject);
begin
  Caption := rsWelcome;
  Label1.Caption := rsUser;
  Label2.Caption:=rsPassword;
  SingleMode.Caption:=rsSingleUserMode;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
end;

procedure TLoginFm.FormShow(Sender: TObject);
begin
  if Users.ItemIndex < 0 then Users.SetFocus
  else Pwd.SetFocus;
end;

procedure TLoginFm.UsersChange(Sender: TObject);
var
  U: TdxUser;
begin
  if Users.ItemIndex < 0 then Exit;
  U := TdxUser(Users.Items.Objects[Users.ItemIndex]);
  if not U.AllowSingleMode then
    SingleMode.Checked := False;
  SingleMode.Enabled := U.AllowSingleMode;
end;

procedure TLoginFm.FillUsers;
begin
  Users.Clear;
  UserMan.Users.FillStrings(Users.Items);
end;

function TLoginFm.ShowForm(aUser: TdxUser): Integer;
var
  U: TdxUser;
begin
  FillUsers;
  Pwd.Text := '';
  SingleMode.Checked:=False;
  if aUser <> nil then
    with Users do
      ItemIndex := Items.IndexOfObject(aUser);
  Result := ShowModal;
  if Result = mrOk then
  begin
    U := TdxUser(Users.Items.Objects[Users.ItemIndex]);
    UserMan.CurrentUserId:=U.Id;
    UserMan.SingleMode:=SingleMode.Checked;
  end;
end;

end.

