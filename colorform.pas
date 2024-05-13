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
    function ShowColor(var aColor: TColor; DefaultColor: TColor): Integer;
  end;

var
  ColorFm: TColorFm;

function ShowColorForm(Control: TControl): Integer;
function ShowSelectColor(var aColor: TColor; DefaultColor: TColor): Integer;

implementation

uses
  dxctrls, helpmanager;

function ShowColorForm(Control: TControl): Integer;
begin
  if ColorFm = nil then
  	ColorFm := TColorFm.Create(Application);
  Result := ColorFm.ShowForm(Control);
end;

function ShowSelectColor(var aColor: TColor; DefaultColor: TColor): Integer;
begin
  if ColorFm = nil then
  	ColorFm := TColorFm.Create(Application);
  Result := ColorFm.ShowColor(aColor, DefaultColor);
end;

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
  {if Control is TdxLabel then
    ColorSampler1.DefaultColor := clNone
  else}
    ColorSampler1.DefaultColor:=clDefault;;
  ColorSampler1.SampleColor := Control.Color;
  Result := ShowModal;
  if Result = mrOk then
  begin
    if Control is TdxLabel then
    begin
      TdxLabel(Control).Transparent := ColorSampler1.SampleColor = ColorSampler1.DefaultColor;
    end;
    Control.Color := ColorSampler1.SampleColor;
  end;
end;

function TColorFm.ShowColor(var aColor: TColor; DefaultColor: TColor): Integer;
begin
  ColorSampler1.DefaultColor:=DefaultColor;
  ColorSampler1.SampleColor := aColor;
  Result := ShowModal;
  if Result = mrOk then
    aColor := ColorSampler1.SampleColor;
end;

end.

