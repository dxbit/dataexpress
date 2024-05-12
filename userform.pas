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
    UserHidden: TCheckBox;
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
    function ShowForm(aU: TdxUser; aNewUser: Boolean): Integer;
  end;

var
  UserFm: TUserFm;

function ShowUserForm(aU: TdxUser; aNewUser: Boolean): Integer;

implementation

uses
  apputils;

function ShowUserForm(aU: TdxUser; aNewUser: Boolean): Integer;
begin
  if UserFm = nil then
  	UserFm := TUserFm.Create(Application);
  Result := UserFm.ShowForm(aU, aNewUser);
end;

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
  UserHidden.Caption := rsDoNotShowInUserList;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Pwd.Button.LoadGlyphFromLazarusResource('edit16');
  Pwd.Button.Hint:=rsChangePwd;
  Pwd.Button.ShowHint:=True;
  Pwd.Button.OnClick:=@PwdButtonClick;
  AddFormHeight(Self);
end;

procedure TUserFm.FillRoles;
begin
  Role.Clear;
  UserMan.Roles.FillStrings(Role.Items);
end;

function TUserFm.ShowForm(aU: TdxUser; aNewUser: Boolean): Integer;
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
  if aNewUser then
    Devel.Checked := False
  else
    Devel.Checked := aU.RoleId < 0;
  SingleMode.Checked:=aU.AllowSingleMode;
  MultiAuth.Checked := aU.AllowMultiAuth;
  UserHidden.Checked := aU.Hidden;
  //DevelChange(Devel);
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
    aU.Hidden := UserHidden.Checked;
  end;
end;

end.

