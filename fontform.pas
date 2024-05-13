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

unit FontForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Buttons, ExtCtrls, ButtonPanel, strconsts, CtrlUtils;

type

  { TFontFm }

  TFontFm = class(TForm)
    BitBtn3: TBitBtn;
    ButtonPanel1: TButtonPanel;
    ColorSampler1: TColorSampler;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    ToggleBox1: TToggleBox;
    ToggleBox2: TToggleBox;
    ToggleBox3: TToggleBox;
    ToggleBox4: TToggleBox;
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure Shape3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    FDefaultFont: TFont;
    procedure ReadFont(aFont: TFont);
    procedure WriteFont(aFont: TFont);
  public
    { public declarations }
    function ShowForm(aFont, DefaultFont: TFont): Integer;
    //function IsDefault: Boolean;
  end;

var
  FontFm: TFontFm;

function ShowFontForm(aFont, DefaultFont: TFont): Integer;

implementation

uses
  helpmanager;

function ShowFontForm(aFont, DefaultFont: TFont): Integer;
begin
  if FontFm = nil then
    FontFm := TFontFm.Create(Application);
  Result := FontFm.ShowForm(aFont, DefaultFont);
end;

{$R *.lfm}

{ TFontFm }

procedure TFontFm.FormCreate(Sender: TObject);
begin
  Caption := rsFont;
  Label1.Caption := rsFontName;
  ToggleBox1.Caption := rsFontBold;
  ToggleBox2.Caption := rsFontItalic;
  ToggleBox3.Caption := rsFontUnderline;
  ToggleBox4.Caption := rsFontStrikeOut;
  Label2.Caption := rsColor;
  Label3.Caption := rsSize;
  BitBtn3.Caption := rsReset;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TFontFm.FormShow(Sender: TObject);
begin
  ComboBox1.SetFocus;
end;

procedure TFontFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('font');
end;

procedure TFontFm.Shape3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with TColorDialog.Create(Self) do
  try
    Title := rsSelectColor;
    Color := TShape(Sender).Brush.Color;
    if Execute then
      TShape(Sender).Brush.Color := Color;
  finally
    Free;
  end;
end;

procedure TFontFm.ReadFont(aFont: TFont);
begin
  with ComboBox1 do
    ItemIndex := Items.IndexOf(aFont.Name);
  ToggleBox1.Checked := fsBold in aFont.Style;
  ToggleBox2.Checked := fsItalic in aFont.Style;
  ToggleBox3.Checked := fsUnderline in aFont.Style;
  ToggleBox4.Checked := fsStrikeOut in aFont.Style;
  ColorSampler1.SampleColor := aFont.Color;
  SpinEdit1.Value := aFont.Size;
end;

procedure TFontFm.WriteFont(aFont: TFont);
begin
  if ComboBox1.ItemIndex >= 0 then
    aFont.Name := ComboBox1.Items[ComboBox1.ItemIndex]
  else aFont.Name := 'default';
  aFont.Bold := toggleBox1.Checked;
  aFont.Italic := toggleBox2.Checked;
  aFont.Underline := toggleBox3.Checked;
  aFont.StrikeThrough := toggleBox4.Checked;
  aFont.Color := ColorSampler1.SampleColor;
  aFont.Size := SpinEdit1.Value;
end;

procedure TFontFm.BitBtn3Click(Sender: TObject);
begin
  if FDefaultFont = nil then
  begin
    ComboBox1.Text := 'Verdana';
    ToggleBox1.Checked := False;
    ToggleBox2.Checked := False;
    ToggleBox3.Checked := False;
    ToggleBox4.Checked := False;
    ColorSampler1.SampleColor := clDefault;
    SpinEdit1.Value := 10;
  end
  else
    with FDefaultFont do
    begin
      ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(Name);
      ToggleBox1.Checked := fsBold in Style;
      ToggleBox2.Checked := fsItalic in Style;
      ToggleBox3.Checked := fsUnderline in Style;
      ToggleBox4.Checked := fsStrikeOut in Style;
      ColorSampler1.SampleColor := Color;
      SpinEdit1.Value := Size;
    end;
  {ComboBox1.ItemIndex := -1;
  ToggleBox1.Checked := False;
  ToggleBox2.Checked := False;
  ToggleBox3.Checked := False;
  ToggleBox4.Checked := False;
  ColorSampler1.SampleColor := clDefault;
  SpinEdit1.Value := 0;   }
end;

function TFontFm.ShowForm(aFont, DefaultFont: TFont): Integer;
begin
  FDefaultFont := DefaultFont;
  if DefaultFont <> nil then
	  ColorSampler1.DefaultColor := DefaultFont.Color
  else
  	ColorSampler1.DefaultColor := clDefault;
  ComboBox1.Items := Screen.Fonts;
  ReadFont(aFont);
  Result := ShowModal;
  if Result = mrOk then
    WriteFont(aFont);
end;

{function TFontFm.IsDefault: Boolean;
begin
  Result := (ComboBox1.ItemIndex = -1) and
    (ToggleBox1.Checked = False) and
    (ToggleBox2.Checked = False) and
    (ToggleBox3.Checked = False) and
    (ToggleBox4.Checked = False) and
    (ColorSampler1.SampleColor = clDefault) and
    (SpinEdit1.Value = 0);
end;  }

end.

