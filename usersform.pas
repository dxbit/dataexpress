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

unit UsersForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, StdCtrls, ExtCtrls, dxusers, strconsts, LclType;

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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure IntfsDblClick(Sender: TObject);
    procedure RolesDblClick(Sender: TObject);
    procedure RolesSelectionChange(Sender: TObject; User: boolean);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
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
    FModified, FInDesigner: Boolean;
    function GetFullName(U: TdxUser): String;
    procedure SetControlState;
    function CheckDeleteRole(R: TdxRole): Boolean;
    procedure UpdateUserList;
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

function ShowUsersForm(InDesigner: Boolean): Integer;

implementation

uses
  userform, apputils, helpmanager, newroleform, intfform, mytypes, dbengine,
  multirolesform, dxmains;

function ShowUsersForm(InDesigner: Boolean): Integer;
begin
  if UsersFm = nil then
  	UsersFm := TUsersFm.Create(Application);
  Result := UsersFm.Showform(InDesigner);
end;

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
  ToolButton11.Hint := rsCompareAndEditRoles;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Images.AddLazarusResource('add16');
  Images.AddLazarusResource('edit16');
  Images.AddLazarusResource('delete16');
  Images.AddLazarusResource('copy16');
  Images.AddLazarusResource('magic16');
  SetControlState;
end;

procedure TUsersFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrClose;
end;

procedure TUsersFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
begin
  if (ModalResult = mrOk) and not DXMain.CanProjectSave then
    CanClose := False
  else if (ModalResult = mrOk) or FInDesigner then
  begin
    if UserMan.Users.Count > 0 then
    begin
      for i := 0 to UserMan.Users.Count - 1 do
        if UserMan.Users[i].RoleId < 0 then Exit;
      MessageDlg(rsWarning, rsAddDevel, mtWarning, [mbOk], 0);
      CanClose := False;
    end;
  end
  else if (ModalResult <> mrOk) and not FInDesigner then
  begin
    if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
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

// При мультивыделении ItemIndex может не совпадать с выделенным элементом.
procedure SetItemIndexToSelIndex(LB: TListBox);
var
  i: Integer;
begin
  with LB do
  begin
    if (ItemIndex >= 0) and not Selected[ItemIndex] then
      for i := 0 to Count - 1 do
        if Selected[i] then
        begin
          ItemIndex := i;
          Exit;
        end;
  end;
end;

procedure TUsersFm.RolesDblClick(Sender: TObject);
begin
  if Roles.SelCount = 1 then
  begin
    SetItemIndexToSelIndex(Roles);
    ToolButton5.Click;
  end;
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
  if ShowIntfForm(Intf) = mrOk then
  begin
    if Intf.IsDefault then
    begin
      ClearDefaultIntf;
      S := Intf.Name + rsDefaultIntf;
    end
    else
      S := Intf.Name;
    Intfs.ItemIndex := Intfs.Items.AddObject(S, Intf);
    FModified := True;
  end
  else UserMan.Intfs.DeleteIntf(Intf);
  SetControlState;
end;

procedure TUsersFm.ToolButton11Click(Sender: TObject);
var
  RoleList: TList;
  i: Integer;
begin
  RoleList := TList.Create;
  for i := 0 to Roles.Count - 1 do
    if Roles.Selected[i] then
      RoleList.Add(Roles.Items.Objects[i]);
  if ShowMultiRolesForm(RoleList) = mrOk then
    FModified := True;
  RoleList.Free;
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
  if ShowRoleForm(R) = mrOk then
  begin
    Roles.Items.AddObject(R.Name, R);
    Roles.ItemIndex := Roles.Items.Count - 1;
    FModified := True;
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
  if ShowUserForm(U, True) = mrOk then
  begin
    Users.Items.AddObject(GetFullName(U), U);
    Users.ItemIndex := Users.Items.Count - 1;
    FModified := True;
  end
  else UserMan.Users.DeleteUser(U);
  SetControlState;
end;

procedure TUsersFm.ToolButton2Click(Sender: TObject);
var
  U: TdxUser;
begin
  U := TdxUser(Users.Items.Objects[Users.ItemIndex]);
  if ShowUserForm(U, False) = mrOk then
  begin
    Users.Items[Users.ItemIndex] := GetFullName(U);
    FModified := True;
  end;
end;

procedure TUsersFm.ToolButton3Click(Sender: TObject);
var
  U: TdxUser;
  i: Integer;
begin
  if ConfirmDelete then
  begin
    i := Users.ItemIndex;
    U := TdxUser(Users.Items.Objects[Users.ItemIndex]);
    Users.Items.Delete(Users.ItemIndex);
    UserMan.Users.DeleteUser(U);
    if i = Users.Count then Dec(i);
    Users.ItemIndex := i;
    SetControlState;
    FModified := True;
  end;
end;

procedure TUsersFm.ToolButton4Click(Sender: TObject);
var
  R: TdxRole;
begin
  R := UserMan.Roles.AddRole;
  if ShowRoleForm(R) = mrOk then
  begin
    Roles.Items.AddObject(R.Name, R);
    Roles.ItemIndex := Roles.Items.Count - 1;
    Roles.Selected[Roles.ItemIndex] := True;
    FModified := True;
  end
  else UserMan.Roles.DeleteRole(R);
  SetControlState;
end;

procedure TUsersFm.ToolButton5Click(Sender: TObject);
var
  R: TdxRole;
  OldName: String;
begin
  R := TdxRole(Roles.Items.Objects[Roles.ItemIndex]);
  OldName := R.Name;
  if ShowRoleForm(R) = mrOk then
  begin
    Roles.Items[Roles.ItemIndex] := R.Name;
    FModified := True;

    if OldName <> R.Name then
      UpdateUserList;
  end;
end;

procedure TUsersFm.ToolButton6Click(Sender: TObject);
var
  R: TdxRole;
  i: Integer;
begin
  R := TdxRole(Roles.Items.Objects[Roles.ItemIndex]);
  if ConfirmDelete and CheckDeleteRole(R) then
  begin
    i := Roles.ItemIndex;
    Roles.Items.Delete(Roles.ItemIndex);
    UserMan.Roles.DeleteRole(R);
    if i = Roles.Count then Dec(i);
    Roles.ItemIndex := i;
    if (Roles.SelCount = 0) and (i >= 0) then Roles.Selected[i] := True;
    SetControlState;
    FModified := True;
  end;
end;

procedure TUsersFm.ToolButton7Click(Sender: TObject);
var
  Intf: TdxIntf;
  S: String;
begin
  Intf := UserMan.Intfs.AddIntf;
  if ShowIntfForm(Intf) = mrOk then
  begin
    if Intf.IsDefault then
    begin
      ClearDefaultIntf;
      S := Intf.Name + rsDefaultIntf;
    end
    else
      S := Intf.Name;
    Intfs.ItemIndex := Intfs.Items.AddObject(S, Intf);
    FModified := True;
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
  if ShowIntfForm(Intf) = mrOk then
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
    FModified := True;
  end;
end;

procedure TUsersFm.ToolButton9Click(Sender: TObject);
var
  Intf: TdxIntf;
  i: Integer;
begin
  Intf := TdxIntf(Intfs.Items.Objects[Intfs.ItemIndex]);
  if ConfirmDelete then
  begin
    i := Intfs.ItemIndex;
    Intfs.Items.Delete(Intfs.ItemIndex);
    UserMan.Intfs.DeleteIntf(Intf);
    if i = Intfs.Count then Dec(i);
    Intfs.ItemIndex := i;
    SetControlState;
    FModified := True;
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
  ToolButton5.Enabled := (Roles.ItemIndex >= 0) and (Roles.SelCount = 1);
  ToolButton6.Enabled := (Roles.ItemIndex >= 0) and (Roles.SelCount = 1);
  ToolButton13.Enabled := (Roles.ItemIndex >= 0) and (Roles.SelCount = 1);
  ToolButton11.Enabled := Roles.SelCount > 1;
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

procedure TUsersFm.UpdateUserList;
var
  i: Integer;
  U: TdxUser;
begin
  with Users do
  begin
    Items.BeginUpdate;
    for i := 0 to Items.Count - 1 do
    begin
      U := TdxUser(Items.Objects[i]);
      Items[i] := GetFullName(U);
    end;
    Items.EndUpdate;
  end;
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
  CloneUserMan: TdxUserManager;
begin
  if not InDesigner and not DXMain.CanProjectChange then Exit(mrCancel);

  FModified := False;
  FInDesigner := InDesigner;
  FillUsers;
  FillRoles;
  FillIntfs;
  if InDesigner then
  begin
    KeyPreview := True;
    ButtonPanel1.ShowButtons:=[pbClose, pbHelp];
    Result := ShowModal;
  end
  else
  begin
    CloneUserMan := UserMan.CloneManager;
    KeyPReview := False;
    ButtonPanel1.ShowButtons:=[pbOk, pbCancel, pbHelp];
    ButtonPanel1.CancelButton.Caption := rsCancel;
	  Result := ShowModal;
    if Result = mrOk then
      try
        UserMan.SaveToDb;
        DXMain.SetLastModified;
        DBase.Commit;
        CloneUserMan.Free;
      except
        on E: Exception do
        begin
          ErrMsg(rsSaveUsersError + ExceptionToString(E, True, False));
          UserMan.Free;
          UserMan := CloneUserMan;
          Result := mrCancel;
        end;
      end
    else
    begin
      UserMan.Free;
      UserMan := CloneUserMan;
    end;
  end;
end;

end.

