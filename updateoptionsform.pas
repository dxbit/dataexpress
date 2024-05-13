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

unit UpdateOptionsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ButtonPanel, SQLDB, updatemanager, strconsts;

type

  { TUpdateOptionsFm }

  TUpdateOptionsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FUpdMan: TUpdateMan;
  public
    function ShowForm(AUpdMan: TUpdateMan): Integer;
  end;

var
  UpdateOptionsFm: TUpdateOptionsFm;

function ShowUpdateOptionsForm(AUpdMan: TUpdateMan): Integer;

implementation

uses
  apputils;

function ShowUpdateOptionsForm(AUpdMan: TUpdateMan): Integer;
begin
  if UpdateOptionsFm = nil then
    UpdateOptionsFm := TUpdateOptionsFm.Create(Application);
  Result := UpdateOptionsFm.ShowForm(AUpdMan);
end;

{$R *.lfm}

{ TUpdateOptionsFm }

procedure TUpdateOptionsFm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure TUpdateOptionsFm.FormCreate(Sender: TObject);
begin
  Caption := rsUpdateSettings;
  GroupBox1.Caption := rsMsgWhenUpdateFound;
  ButtonPanel1.OkButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
end;

function TUpdateOptionsFm.ShowForm(AUpdMan: TUpdateMan): Integer;
begin
  FUpdMan := AUpdMan;
  Memo1.Text := AUpdMan.UserMsg;
  Result := ShowModal;
  if Result = mrOk then
  begin
    AUpdMan.UserMsg := Trim(Memo1.Text);
    AUpdMan.Version := BuildDateToStr;
    AUpdMan.SaveToDb;
  end;
end;

end.

