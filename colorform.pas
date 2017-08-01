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
unit ColorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, strconsts, CtrlUtils;

type

  { TColorFm }

  TColorFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ColorSampler1: TColorSampler;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(Control: TControl): Integer;
    function ShowColor(aColor: TColor): TColor;
  end;

var
  ColorFm: TColorFm;

implementation

uses
  dxctrls, helpform;

{$R *.lfm}

{ TColorFm }

procedure TColorFm.FormCreate(Sender: TObject);
begin
  Caption := rsColor;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TColorFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('color');
end;

function TColorFm.ShowForm(Control: TControl): Integer;
begin
  if Control is TdxLabel then
    ColorSampler1.DefaultColor := clNone
  else
    ColorSampler1.DefaultColor:=clDefault;;
  ColorSampler1.SampleColor := Control.Color;
  Result := ShowModal;
  if Result = mrOk then
    Control.Color := ColorSampler1.SampleColor;
end;

function TColorFm.ShowColor(aColor: TColor): TColor;
begin
  ColorSampler1.DefaultColor:=clNone;
  ColorSampler1.SampleColor := aColor;
  if ShowModal = mrOk then
    Result := ColorSampler1.SampleColor
  else Result := aColor;
end;

end.

