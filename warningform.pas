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
    CheckBox1: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(const ATitle, Msg, AButtonCaption: String): Integer;
    procedure ShowInfo(const ATitle, Msg: String);
  end;

var
  WarnFm: TWarnFm;

function ShowWarnForm(const ATitle, Msg, AButtonCaption: String): Integer;
procedure ShowWarnInfo(const ATitle, Msg: String);

implementation

uses
  apputils;

function ShowWarnForm(const ATitle, Msg, AButtonCaption: String): Integer;
begin
  if WarnFm = nil then
  	WarnFm := TWarnFm.Create(Application);
  Result := WarnFm.ShowForm(ATitle, Msg, AButtonCaption);
end;

procedure ShowWarnInfo(const ATitle, Msg: String);
begin
  if WarnFm = nil then
  	WarnFm := TWarnFm.Create(Application);
  WarnFm.ShowInfo(ATitle, Msg);
end;

{$R *.lfm}

{ TWarnFm }

procedure TWarnFm.FormCreate(Sender: TObject);
begin
  Caption := rsWarning;
  CheckBox1.Caption := rsIknow;
  //ButtonPanel1.OKButton.Caption:=rsYes;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  SetupPicture(Image1.Picture, 'alert64');
end;

procedure TWarnFm.CheckBox1Change(Sender: TObject);
begin
  ButtonPanel1.OkButton.Enabled := CheckBox1.Checked;
end;

procedure TWarnFm.FormShow(Sender: TObject);
begin
  if ButtonPanel1.CancelButton.CanFocus then
    ButtonPanel1.CancelButton.SetFocus;
end;

function TWarnFm.ShowForm(const ATitle, Msg, AButtonCaption: String): Integer;
begin
  Label1.Caption := ATitle;
  Label2.Caption := Msg;
  ButtonPanel1.ShowButtons := [pbOk, pbCancel];
  ButtonPanel1.OkButton.Caption := AButtonCaption;
  Self.CancelControl := ButtonPanel1.CancelButton;
  CheckBox1.Visible := True;
  CheckBox1.Checked := False;
  CheckBox1.OnChange(CheckBox1);
  Result := ShowModal;
end;

procedure TWarnFm.ShowInfo(const ATitle, Msg: String);
begin
  Label1.Caption := ATitle;
  Label2.Caption := Msg;
  ButtonPanel1.ShowButtons := [pbOk];
  ButtonPanel1.OkButton.Caption := rsOk;
  Self.CancelControl := ButtonPanel1.OkButton;
  CheckBox1.Visible := False;
  CheckBox1.Checked := True;
  CheckBox1.OnChange(CheckBox1);
  ShowModal;
end;

end.

