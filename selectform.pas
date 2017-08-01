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
unit SelectForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TSelectFm }

  TSelectFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    List: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListSelectionChange(Sender: TObject; User: boolean);
  private
    FIndex: Integer;
    FUrl: String;
    function GetIndex: Integer;
    procedure SetControlState;
    { private declarations }
  public
    { public declarations }
    function ShowForm(const Title, Url: String; SL: TStrings): Integer;
    property Index: Integer read GetIndex write FIndex;
  end;

var
  SelectFm: TSelectFm;

implementation

uses
  helpform;

{$R *.lfm}

{ TSelectFm }

procedure TSelectFm.FormCreate(Sender: TObject);
begin
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TSelectFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
  if FIndex > List.Items.Count - 1 then FIndex := -1;
  List.ItemIndex := FIndex;
  SetControlState;
end;

procedure TSelectFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp(FUrl);
end;

procedure TSelectFm.ListDblClick(Sender: TObject);
begin
  if List.ItemIndex >= 0 then ModalResult := mrOk;
end;

procedure TSelectFm.ListSelectionChange(Sender: TObject; User: boolean);
begin
  SetControlState;
end;

function TSelectFm.GetIndex: Integer;
begin
  Result := List.ItemIndex;
end;

procedure TSelectFm.SetControlState;
begin
  ButtonPanel1.OkButton.Enabled:=List.ItemIndex >=0;
end;

function TSelectFm.ShowForm(const Title, Url: String; SL: TStrings): Integer;
begin
  FUrl := Url;
  Caption := Title;
  List.Clear;
  List.Items.AddStrings(SL);
  ButtonPanel1.HelpButton.Visible := Url > '';
  Result := ShowModal;
end;

end.

