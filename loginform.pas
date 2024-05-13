{-------------------------------------------------------------------------------

    Copyright 2015-2024 Pavel Duborkin ( mydataexpress@mail.ru )

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

-------------------------------------------------------------------------------}

unit LoginForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts, dxusers, LclType;

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
  private
    { private declarations }
    procedure FillUsers;
  public
    { public declarations }
    function ShowForm(const aCaption: String; aUser: TdxUser): Integer;
  end;

var
  LoginFm: TLoginFm;

function ShowLoginForm(const aCaption: String; aUser: TdxUser): Integer;

implementation

uses
  apputils, md5;

function ShowLoginForm(const aCaption: String; aUser: TdxUser): Integer;
begin
  if LoginFm = nil then
  	LoginFm := TLoginFm.Create(Application);
  Result := LoginFm.ShowForm(aCaption, aUser);
end;

{$R *.lfm}

{ TLoginFm }

procedure TLoginFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  U: TdxUser;
begin
  if ModalResult = mrOk then
  begin
    if Users.Text = '' then
    begin
      ErrMsg(rsEnterUsername);
      Users.SetFocus;
      CanClose := False;
      Exit;
    end;
    U := UserMan.Users.FindUserByName(Users.Text);
    if (U = nil) or (md5print(md5string(Pwd.Text)) <> U.Password) then
    begin
      ErrMsg(rsInvalidUsernamePwd);
      Pwd.SetFocus;
      CanClose := False;
    end
    else if SingleMode.Checked and (U.AllowSingleMode = False) then
    begin
      ErrMsg(rsSingleModeNotAllowed);
      CanClose := False;
    end;
  end;
end;

procedure TLoginFm.FormCreate(Sender: TObject);
begin
  Caption := '';
  Label1.Caption := rsUser;
  Label2.Caption:=rsPassword;
  SingleMode.Caption:=rsSingleUserMode;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
end;

procedure TLoginFm.FormShow(Sender: TObject);
begin
  if Users.Text = '' then Users.SetFocus
  else Pwd.SetFocus;
end;

procedure TLoginFm.FillUsers;
begin
  Users.Clear;
  UserMan.Users.FillStrings(Users.Items);
end;

function TLoginFm.ShowForm(const aCaption: String; aUser: TdxUser): Integer;
var
  U: TdxUser;
begin
  Caption := Format(rsWelcome, [aCaption]);

  FillUsers;
  Pwd.Text := '';
  SingleMode.Checked:=False;
  if aUser <> nil then
	  Users.Text := aUser.Name;
  Result := ShowModal;
  if Result = mrOk then
  begin
    U := UserMan.Users.FindUserByName(Users.Text);
    UserMan.CurrentUserId:=U.Id;
    UserMan.SingleMode:=SingleMode.Checked;
  end;
end;

end.

