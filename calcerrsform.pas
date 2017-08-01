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
unit CalcErrsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Menus, strconsts, Buttons;

type

  { TCalcErrsFm }

  TCalcErrsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FErrs: TStrings;
  public
    { public declarations }
    procedure ShowForm(Errs: TStrings);
  end;

var
  CalcErrsFm: TCalcErrsFm;

implementation

{$R *.lfm}

{ TCalcErrsFm }

procedure TCalcErrsFm.FormCreate(Sender: TObject);
begin
  Caption := rsErrors;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Kind:=bkCustom;
  ButtonPanel1.HelpButton.Caption := rsClear;
  ButtonPanel1.HelpButton.LoadGlyphFromLazarusResource('delete16');
end;

procedure TCalcErrsFm.HelpButtonClick(Sender: TObject);
begin
  Memo1.Clear;
  FErrs.Clear;
end;

procedure TCalcErrsFm.ShowForm(Errs: TStrings);
begin
  FErrs := Errs;
  Memo1.Lines := Errs;
  ShowModal;
end;

end.

