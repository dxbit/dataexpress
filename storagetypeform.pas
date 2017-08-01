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

implementation

uses
  dxctrls, helpform;

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

