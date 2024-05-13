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

unit StorageTypeForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ButtonPanel, strconsts;

type

  { TStorageTypeFm }

  TStorageTypeFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DirectoryEdit1: TDirectoryEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(Control: TControl): Integer;
  end;

var
  StorageTypeFm: TStorageTypeFm;

function ShowStorageTypeForm(Control: TControl): Integer;

implementation

uses
  dxctrls, helpmanager, apputils;

function ShowStorageTypeForm(Control: TControl): Integer;
begin
  if StorageTypeFm = nil then
  	StorageTypeFm := TStorageTypeFm.Create(Application);
  Result := StorageTypeFm.ShowForm(Control);
end;

{$R *.lfm}

{ TStorageTypeFm }

procedure TStorageTypeFm.FormCreate(Sender: TObject);
begin
  Caption := rsStorageType;
  RadioButton1.Caption := rsSTDatabase;
  RadioButton2.Caption := rsSTFolder;
  RadioButton3.Caption := rsSTLink;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TStorageTypeFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult <> mrOk then Exit;
  if RadioButton2.Checked and (Trim(DirectoryEdit1.Text) = '') then
  begin
    ErrMsg(rsStorageFolderNotSpec);
    DirectoryEdit1.SetFocus;
    CanClose := False;
  end;
end;

procedure TStorageTypeFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('storagetype');
end;

function TStorageTypeFm.ShowForm(Control: TControl): Integer;
var
  st: Integer;
  sf: String;
begin
  st := GetStorageType(Control);
  sf := GetStorageFolder(Control);
  RadioButton1.Checked := st = StorageTypeDB;
  RadioButton2.Checked := st = StorageTypeFolder;
  RadioButton3.Checked := st = StorageTypeLink;
  DirectoryEdit1.Text := sf;

  Result := ShowModal;
  if Result <> mrOk then Exit;

  if RadioButton1.Checked then st := StorageTypeDB
  else if RadioButton2.Checked then st := StorageTypeFolder
  else if RadioButton3.Checked then st := StorageTypeLink;
  sf := DirectoryEdit1.Text;
  if sf <> '' then sf := IncludeTrailingPathDelimiter(sf);
  SetStorageType(Control, st);
  SetStorageFolder(Control, sf);
end;

end.

