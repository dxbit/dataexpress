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
unit UserForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Buttons, EditBtn, dxusers, strconsts, md5;

type

  { TUserFm }

  TUserFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    SingleMode: TCheckBox;
    MultiAuth: TCheckBox;
    Devel: TCheckBox;
    Pwd: TEditButton;
    Role: TComboBox;
    Nm: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure DevelChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PwdButtonClick(Sender: TObject);
  private
    { private declarations }
    FPwdChanged: Boolean;
    FOldUser: TdxUser;
    procedure FillRoles;
  public
    { public declarations }
    function ShowForm(aU: TdxUser): Integer;
  end;

var
  UserFm: TUserFm;

implementation

uses
  apputils;

{$R *.lfm}

{ TUserFm }

procedure TUserFm.FormShow(Sender: TObject);
begin
  Nm.SetFocus;
end;

procedure TUserFm.PwdButtonClick(Sender: TObject);
begin
  Pwd.Text := '';
  Pwd.PasswordChar:=#0;
  Pwd.ReadOnly:=False;
  Pwd.Button.Enabled:=False;
  Pwd.SetFocus;
  FPwdChanged := True;
end;

procedure TUserFm.DevelChange(Sender: TObject);
begin
  if Devel.Checked then
  begin
    Role.ItemIndex:=-1;
    SingleMode.Checked := True;
    MultiAuth.Checked := True;
  end;
  Role.Enabled:=not Devel.Checked;
  SingleMode.Enabled := not Devel.Checked;
  MultiAuth.Enabled := not Devel.Checked;
end;

procedure TUserFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  U: TdxUser;
begin
  if ModalResult = mrOk then
  begin
    CanClose := False;
    U := UserMan.Users.FindUserByName(Nm.Text);
    if Trim(Nm.Text) = '' then
    begin
      ErrMsg(rsEnterName);
      Nm.SetFocus;
    end
    else if (U <> nil) and (U <> FOldUser) then
    begin
    	ErrMsg(rsAUserWithThisNameExists);
      Nm.SetFocus;
    end
    else if Trim(Pwd.Text) = '' then
    begin
      ErrMsg(rsEnterPwd);
      Pwd.SetFocus;
    end
    else if (Role.ItemIndex < 0) and (not Devel.Checked) then
    begin
      ErrMsg(rsRoleNotSel);
      Role.SetFocus;
    end
    else
      CanClose := True;
  end;
end;

procedure TUserFm.FormCreate(Sender: TObject);
begin
  Caption := rsUser;
  Label1.Caption := rsName;
  Label2.Caption := rsPassword;
  Label3.Caption := rsRole;
  Devel.Caption:=rsDeveloper;
  SingleMode.Caption := rsAllowSingleUserMode;
  MultiAuth.Caption := rsAllowMultiAuth;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Pwd.Button.LoadGlyphFromLazarusResource('edit16');
  Pwd.Button.Hint:=rsChangePwd;
  Pwd.Button.ShowHint:=True;
  Pwd.Button.OnClick:=@PwdButtonClick;
end;

procedure TUserFm.FillRoles;
begin
  Role.Clear;
  UserMan.Roles.FillStrings(Role.Items);
end;

function TUserFm.ShowForm(aU: TdxUser): Integer;
var
  R: TdxRole;
begin
  FOldUser := aU;
  Nm.Text := aU.Name;
  Pwd.Text := aU.Password;
  if Pwd.Text <> '' then
  begin
    Pwd.ReadOnly := True;
    Pwd.PasswordChar:='*';
    Pwd.Button.Enabled:=True;
    FPwdChanged := False;
  end
  else
  begin
    Pwd.ReadOnly := False;
    Pwd.PasswordChar:=#0;
    Pwd.Button.Enabled:=False;
    FPwdChanged := True;
  end;
  FillRoles;
  R := UserMan.Roles.FindRole(aU.RoleId);
  Role.ItemIndex := Role.Items.IndexOfObject(R);
  Devel.Checked := aU.RoleId < 0;
  SingleMode.Checked:=aU.AllowSingleMode;
  MultiAuth.Checked := aU.AllowMultiAuth;
  DevelChange(Devel);
  Result := ShowModal;
  if Result = mrOk then
  begin
    aU.Name := Nm.Text;
    if FPwdChanged then
      aU.Password := md5print(md5string(Pwd.Text));
    if Role.ItemIndex >= 0 then
      aU.RoleId := TdxRole(Role.Items.Objects[Role.ItemIndex]).Id
    else
      aU.RoleId := -1;
    aU.AllowSingleMode:=SingleMode.Checked;
    aU.AllowMultiAuth:=MultiAuth.Checked;
  end;
end;

end.

