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

unit ConnectInfoForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, Buttons, ButtonPanel, appsettings, strconsts;

{ TConnectInfoFm }

type
  TConnectInfoFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label8: TLabel;
    User: TEdit;
    Pwd: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    TemplateDir: TDirectoryEdit;
    OutputDir: TDirectoryEdit;
    ConnectName: TEdit;
    DBPwd: TEdit;
    DBPath: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure DBPathAcceptFileName(Sender: TObject; var Value: String);
    procedure DBPathEditingDone(Sender: TObject);
    procedure DBPwdChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FCI: TConnectInfo;
    FModified: Boolean;
    function Validate: Boolean;
    function CheckDuplicateConnects: Boolean;
    procedure SetDefaultConnectName(const Value: String);
    //procedure SetReadOnlyState(RO: Boolean);
  public
    { public declarations }
    function ShowForm(CI: TConnectInfo): Integer;
    property Modified: Boolean read FModified;
  end;

var
  ConnectInfoFm: TConnectInfoFm;

function ShowConnectInfoForm(CI: TConnectInfo): Integer;

implementation

uses
  apputils, LazUtf8, helpmanager;

function ShowConnectInfoForm(CI: TConnectInfo): Integer;
begin
  if ConnectInfoFm = nil then
  	ConnectInfoFm := TConnectInfoFm.Create(Application);
  Result := ConnectInfoFm.ShowForm(CI);
end;

{$R *.lfm}

{ TConnectInfoFm }

procedure TConnectInfoFm.FormCreate(Sender: TObject);
begin
  Caption := rsConnectSettings;
  Label1.Caption := rsConnectName + ' *';
  ConnectName.TextHint := rsExampleConnectName;
  Label2.Caption := rsDBLocation + ' *';
  DBPath.TextHint := rsExampleDBLocation;
  DBPath.DialogTitle := rsDBLocation;
  DBPath.DialogOptions := DBPath.DialogOptions + [ofFileMustExist];
  Label3.Caption := rsPwdSYSDBA;
  DBPwd.TextHint := rsDefaultPwd;
  Label4.Caption := rsTemplatesFolder;
  TemplateDir.TextHint := rsHintTemplateDefaultFolder;
  Label5.Caption := rsOutputFolder;
  OutputDir.TextHint := rsHintOutputDefaultFolder;
  Label6.Caption := rsUsername;
  Label7.Caption := rsPassword;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  AddFormHeight(Self);
end;

procedure TConnectInfoFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FModified := (FCI.Name <> ConnectName.Text) or (FCI.DBPath <> DBPath.Text) or
  	(FCI.DBPwd <> DBPwd.Text) or (FCI.TemplateDir <> TemplateDir.Text) or
    (FCI.OutputDir <> OutputDir.Text) or (FCI.User <> User.Text) or
    (FCI.Pwd <> Pwd.Text);

  if ModalResult = mrOk then
  	CanClose := Validate
  else
  begin
    if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TConnectInfoFm.DBPathEditingDone(Sender: TObject);
begin
  SetDefaultConnectName(DBPath.Text);
end;

procedure TConnectInfoFm.DBPwdChange(Sender: TObject);
begin
  if DBPwd.Text > '' then DBPwd.PasswordChar := '*'
  else DBPwd.PasswordChar := #0;
end;

procedure TConnectInfoFm.DBPathAcceptFileName(Sender: TObject; var Value: String
  );
begin
  SetDefaultConnectName(Value);
end;

procedure TConnectInfoFm.FormShow(Sender: TObject);
begin
  ConnectName.SetFocus;
end;

procedure TConnectInfoFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('connection');
end;

function TConnectInfoFm.Validate: Boolean;
var
  CI: TConnectInfo;
begin
  Result := False;
  if Trim(ConnectName.Text) = '' then
  begin
    ErrMsg(rsEnterConnectName);
    ConnectName.SetFocus;
  end
  else if Pos('|', ConnectName.Text) > 0 then
  begin
    ErrMsg(rsConnectNameInvalidChars);
    ConnectName.SetFocus;
  end
  else if Trim(DBPath.Text) = '' then
  begin
    ErrMsg(rsEnterDBLocation);
    DBPath.SetFocus;
  end
  else
  begin
    CI := AppConfig.Connects.FindConnection(ConnectName.Text);
    if (CI <> nil) and (CI <> FCI) then
    begin
      ErrMsg(rsConnectNameExists);
      ConnectName.SetFocus;
    end
    else
	    Result := CheckDuplicateConnects;
  end;
end;

function TConnectInfoFm.CheckDuplicateConnects: Boolean;
var
  i: Integer;
  CI: TConnectInfo;
  S: String;
begin
  Result := True;
  S := '';
  for i := 0 to AppConfig.Connects.Count - 1 do
  begin
    CI := AppConfig.Connects[i];
    if (FCI <> CI) and (MyUtf8CompareText(DBPath.Text, CI.DBPath) = 0) and
    	(MyUtf8CompareText(User.Text, CI.User) = 0) then
    begin
      if CI.Group <> '' then
	      S := S + CI.Group + ': ';
      S := S + CI.Name + LineEnding;
    end;
  end;
  if S <> '' then
	  Result := Confirm(rsWarning, Format(rsConnectSaveAlreadyExists,
    	[LineEnding + LineEnding + S + LineEnding])) = mrYes;
end;

procedure TConnectInfoFm.SetDefaultConnectName(const Value: String);
begin
  if ConnectName.Text = '' then
    ConnectName.Text := Utf8UpperCase(ExtractFileNameOnly(Value));
end;

{procedure TConnectInfoFm.SetReadOnlyState(RO: Boolean);
begin
  ConnectName.ReadOnly := RO;
  DBPath.ReadOnly := RO;
  Remote.Enabled := not RO;
  DBPwd.ReadOnly := RO;
  DefaultPwd.Enabled := not RO;
  TemplateDir.ReadOnly := RO;
  OutputDir.ReadOnly := RO;
  User.ReadOnly := RO;
  Pwd.ReadOnly := RO;
end; }

function TConnectInfoFm.ShowForm(CI: TConnectInfo): Integer;
begin
  FCI := CI;

  DBPath.Filter := ConstructDBOpenFilter(True);
  DBPath.DefaultExt := GetDefaultDBExt;

  ConnectName.Text := CI.Name;
  DBPath.Text := CI.DBPath;
  DBPwd.Text := CI.DBPwd;
  TemplateDir.Text := CI.TemplateDir;
  OutputDir.Text := CI.OutputDir;
  User.Text := CI.User;
  Pwd.Text := CI.Pwd;

  Result := ShowModal;
  if Result <> mrOk then Exit;

  CI.Name := ConnectName.Text;
  CI.DBPath := DBPath.Text;
  CI.DBPwd := DBPwd.Text;
  CI.TemplateDir := TemplateDir.Text;
  CI.OutputDir := OutputDir.Text;
  CI.User := User.Text;
  CI.Pwd := Pwd.Text;
end;

end.

