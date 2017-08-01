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
unit StringsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TStringsFm }

  TStringsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FUrl: String;
  public
    { public declarations }
    function ShowForm(const Title, Url: String; SL: TStrings): Integer;
  end;

var
  StringsFm: TStringsFm;

implementation

uses
  helpform;

{$R *.lfm}

{ TStringsFm }

procedure TStringsFm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure TStringsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp(FUrl);
end;

procedure TStringsFm.FormCreate(Sender: TObject);
begin
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

function TStringsFm.ShowForm(const Title, Url: String; SL: TStrings): Integer;
begin
  FUrl := Url;
  ButtonPanel1.HelpButton.Visible:=Url > '';
  Caption := Title;
  Memo1.Clear;
  Memo1.Lines.AddStrings(SL);
  if ShowModal = mrOk then
  begin
    SL.Clear;
    SL.AddStrings(Memo1.Lines);
  end;
  Result := ModalResult;
end;

end.

