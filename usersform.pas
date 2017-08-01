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
unit UsersForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, StdCtrls, ExtCtrls, dxusers, strconsts;

type

  { TUsersFm }

  TUsersFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Images: TImageList;
    Intfs: TListBox;
    TabSheet3: TTabSheet;
    ToolBar3: TToolBar;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    Users: TListBox;
    Roles: TListBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure IntfsDblClick(Sender: TObject);
    procedure RolesDblClick(Sender: TObject);
    procedure RolesSelectionChange(Sender: TObject; User: boolean);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure UsersDblClick(Sender: TObject);
    procedure UsersSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    function GetFullName(U: TdxUser): String;
    procedure SetControlState;
    function CheckDeleteRole(R: TdxRole): Boolean;
    procedure FillUsers;
    procedure FillRoles;
    procedure FillIntfs;
    procedure ClearDefaultIntf;
  public
    { public declarations }
    function ShowForm(InDesigner: Boolean): Integer;
  end;

var
  UsersFm: TUsersFm;

implementation

uses
  dxctrls, userform, apputils, helpform, newroleform, intfform, mytypes;

{$R *.lfm}

{ TUsersFm }

procedure TUsersFm.FormCreate(Sender: TObject);
begin
  Caption := rsUsers;
  TabSheet1.Caption := rsUsers;
  ToolButton1.Caption := rsAppend;
  ToolButton2.Caption := rsEdit;
  ToolButton3.Caption := rsDelete;
  TabSheet2.Caption := rsRoles;
  ToolButton4.Caption := rsAppend;
  ToolButton5.Caption := rsEdit;
  ToolButton6.Caption := rsDelete;
  ToolButton13.Caption := rsCopy;
  TabSheet3.Caption := rsInterfaces;
  ToolButton7.Caption := rsAppend;
  ToolButton8.Caption := rsEdit;
  ToolButton9.Caption := rsDelete;
  ToolButton10.Caption := rsCopy;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Images.AddLazarusResource('add16');
  Images.AddLazarusResource('edit16');
  Images.AddLazarusResource('delete16');
  Images.AddLazarusResource('copy16');
  SetControlState;
end;

procedure TUsersFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
begin
  if (ModalResult = mrOk) or (ModalResult = mrClose) or
    ((ModalResult = mrCancel) and ButtonPanel1.CloseButton.Visible) then
  begin
    if UserMan.Users.Count > 0 then
    begin
      for i := 0 to UserMan.Users.Count - 1 do
        if UserMan.Users[i].RoleId < 0 then Exit;
      MessageDlg(rsWarning, rsAddDevel, mtWarning, [mbOk], 0);
      CanClose := False;
    end;
  end;
end;

procedure TUsersFm.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  Users.SetFocus;
  SetControlState;
end;

procedure TUsersFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('users');
end;

procedure TUsersFm.IntfsDblClick(Sender: TObject);
begin
  if Intfs.ItemIndex >= 0 then ToolButton8.Click;
end;

procedure TUsersFm.RolesDblClick(Sender: TObject);
begin
  if Roles.ItemIndex >= 0 then ToolButton5.Click;
end;

procedure TUsersFm.RolesSelectionChange(Sender: TObject; User: boolean);
begin
  SetControlState;
end;

procedure TUsersFm.ToolButton10Click(Sender: TObject);
var
  Intf, Src: TdxIntf;
  S: String;
  Id: Integer;
begin
  Src := TdxIntf(Intfs.Items.Objects[Intfs.ItemIndex]);
  Intf := UserMan.Intfs.AddIntf;
  Id := Intf.Id;
  CopyIntf(Src, Intf);
  Intf.Id := Id;
  if IntfFm.ShowForm(Intf) = mrOk then
  begin
    if Intf.IsDefault then
    begin
      ClearDefaultIntf;
      S := Intf.Name + rsDefaultIntf;
    end
    else
      S := Intf.Name;
    Intfs.ItemIndex := Intfs.Items.AddObject(S, Intf);
  end
  else UserMan.Intfs.DeleteIntf(Intf);
  SetControlState;
end;

procedure TUsersFm.ToolButton13Click(Sender: TObject);
var
  R, RR: TdxRole;
  Id: Integer;
begin
  RR := TdxRole(Roles.Items.Objects[Roles.ItemIndex]);
  R := UserMan.Roles.AddRole;
  Id := R.Id;
  CopyRole(RR, R);
  R.Id := Id;
  if NewRoleFm.ShowForm(R) = mrOk then
  begin
    Roles.Items.AddObject(R.Name, R);
    Roles.ItemIndex := Roles.Items.Count - 1;
  end
  else UserMan.Roles.DeleteRole(R);
  SetControlState;
end;

procedure TUsersFm.ToolButton1Click(Sender: TObject);
var
  U: TdxUser;
begin
  U := UserMan.Users.AddUser;
  U.RoleId:=-1;
  if UserFm.ShowForm(U) = mrOk then
  begin
    Users.Items.AddObject(GetFullName(U), U);
    Users.ItemIndex := Users.Items.Count - 1;
  end
  else UserMan.Users.DeleteUser(U);
  SetControlState;
end;

procedure TUsersFm.ToolButton2Click(Sender: TObject);
var
  U: TdxUser;
begin
  U := TdxUser(Users.Items.Objects[Users.ItemIndex]);
  if UserFm.ShowForm(U) = mrOk then
    Users.Items[Users.ItemIndex] := GetFullName(U);
end;

procedure TUsersFm.ToolButton3Click(Sender: TObject);
var
  U: TdxUser;
begin
  if ConfirmDelete then
  begin
    U := TdxUser(Users.Items.Objects[Users.ItemIndex]);
    Users.Items.Delete(Users.ItemIndex);
    UserMan.Users.DeleteUser(U);
    SetControlState;
  end;
end;

procedure TUsersFm.ToolButton4Click(Sender: TObject);
var
  R: TdxRole;
begin
  R := UserMan.Roles.AddRole;
  if NewRoleFm.ShowForm(R) = mrOk then
  begin
    Roles.Items.AddObject(R.Name, R);
    Roles.ItemIndex := Roles.Items.Count - 1;
  end
  else UserMan.Roles.DeleteRole(R);
  SetControlState;
end;

procedure TUsersFm.ToolButton5Click(Sender: TObject);
var
  R: TdxRole;
begin
  R := TdxRole(Roles.Items.Objects[Roles.ItemIndex]);
  if NewRoleFm.ShowForm(R) = mrOk then
    Roles.Items[Roles.ItemIndex] := R.Name;
end;

procedure TUsersFm.ToolButton6Click(Sender: TObject);
var
  R: TdxRole;
begin
  R := TdxRole(Roles.Items.Objects[Roles.ItemIndex]);
  if ConfirmDelete and CheckDeleteRole(R) then
  begin
    Roles.Items.Delete(Roles.ItemIndex);
    UserMan.Roles.DeleteRole(R);
    SetControlState;
  end;
end;

procedure TUsersFm.ToolButton7Click(Sender: TObject);
var
  Intf: TdxIntf;
  S: String;
begin
  Intf := UserMan.Intfs.AddIntf;
  if IntfFm.ShowForm(Intf) = mrOk then
  begin
    if Intf.IsDefault then
    begin
      ClearDefaultIntf;
      S := Intf.Name + rsDefaultIntf;
    end
    else
      S := Intf.Name;
    Intfs.ItemIndex := Intfs.Items.AddObject(S, Intf);
  end
  else UserMan.Intfs.DeleteIntf(Intf);
  SetControlState;
end;

procedure TUsersFm.ToolButton8Click(Sender: TObject);
var
  Intf: TdxIntf;
  S: String;
begin
  Intf := TdxIntf(Intfs.Items.Objects[Intfs.ItemIndex]);
  if IntfFm.ShowForm(Intf) = mrOk then
  begin
    if Intf.IsDefault then
    begin
      ClearDefaultIntf;
      Intf.IsDefault := True;
      S := Intf.Name + rsDefaultIntf;
    end
    else
      S := Intf.Name;
    Intfs.Items[Intfs.ItemIndex] := S;
  end;
end;

procedure TUsersFm.ToolButton9Click(Sender: TObject);
var
  Intf: TdxIntf;
begin
  Intf := TdxIntf(Intfs.Items.Objects[Intfs.ItemIndex]);
  if ConfirmDelete then
  begin
    Intfs.Items.Delete(Intfs.ItemIndex);
    UserMan.Intfs.DeleteIntf(Intf);
    SetControlState;
  end;
end;

procedure TUsersFm.UsersDblClick(Sender: TObject);
begin
  if Users.ItemIndex >= 0 then ToolButton2.Click;
end;

procedure TUsersFm.UsersSelectionChange(Sender: TObject; User: boolean);
begin
  SetControlState;
end;

function TUsersFm.GetFullName(U: TdxUser): String;
var
  R: TdxRole;
  S: String;
begin
  S := U.Name;
  R := U.GetRole;
  if R <> nil then S := S + ' - ' + R.Name
  else S := S + ' - ' + rsDeveloper;
  Result := S;
end;

procedure TUsersFm.SetControlState;
begin
  ToolButton2.Enabled := Users.ItemIndex >= 0;
  ToolButton3.Enabled := Users.ItemIndex >= 0;
  ToolButton5.Enabled := Roles.ItemIndex >= 0;
  ToolButton6.Enabled := Roles.ItemIndex >= 0;
  ToolButton13.Enabled := Roles.ItemIndex >= 0;
  ToolButton8.Enabled := Intfs.ItemIndex >= 0;
  ToolButton9.Enabled := Intfs.ItemIndex >= 0;
  ToolButton10.Enabled := Intfs.ItemIndex >= 0;
end;

function TUsersFm.CheckDeleteRole(R: TdxRole): Boolean;
var
  i: Integer;
  U: TdxUser;
begin
  for i := 0 to UserMan.Users.Count - 1 do
  begin
    U := UserMan.Users[i];
    if U.RoleId = R.Id then
    begin
      ErrMsg(Format(rsRoleUsed, [U.Name]));
      Exit(False);
    end;
  end;
  Result := True;
end;

procedure TUsersFm.FillUsers;
var
  SL: TStringListUtf8;
  i: Integer;
  U: TdxUser;
begin
  Users.Clear;
  SL := TStringListUtf8.Create;
  for i := 0 to UserMan.Users.Count - 1 do
  begin
    U := UserMan.Users[i];
    SL.AddObject(GetFullName(U), U);
  end;
  SL.Sort;
  Users.Items := SL;
  SL.Free;
end;

procedure TUsersFm.FillRoles;
begin
  Roles.Clear;
  UserMan.Roles.FillStrings(Roles.Items);
end;

procedure TUsersFm.FillIntfs;
var
  i: Integer;
  Intf: TdxIntf;
  S: String;
begin
  Intfs.Clear;
  for i := 0 to UserMan.Intfs.Count - 1 do
  begin
    Intf := UserMan.Intfs[i];
    S := Intf.Name;
    if Intf.IsDefault then S := S + rsDefaultIntf;
    Intfs.Items.AddObject(S, Intf);
  end;
end;

procedure TUsersFm.ClearDefaultIntf;
var
  i: Integer;
  Intf: TdxIntf;
begin
  for i := 0 to Intfs.Count - 1 do
  begin
    Intf := TdxIntf(Intfs.Items.Objects[i]);
    Intf.IsDefault := False;
    Intfs.Items[i] := Intf.Name;
  end;
end;

function TUsersFm.ShowForm(InDesigner: Boolean): Integer;
var
  UId: Integer;
begin
  FillUsers;
  FillRoles;
  FillIntfs;
  if InDesigner then
    ButtonPanel1.ShowButtons:=[pbClose, pbHelp]
  else
    ButtonPanel1.ShowButtons:=[pbOk, pbCancel, pbHelp];
  Result := ShowModal;
  if Result = mrOk then
    UserMan.SaveToDb
  else if not InDesigner then
  begin
    UId := UserMan.CurrentUserId;
    UserMan.LoadFromDb;
    UserMan.CurrentUserId:=UId;
  end;
end;

end.

