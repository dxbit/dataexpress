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

unit imagepropsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  dximages, strconsts;

type

  { TImagePropsFm }

  TImagePropsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CenterChk: TCheckBox;
    GroupBox1: TGroupBox;
    KeepSizeChk: TCheckBox;
    PropChk: TCheckBox;
    StretchChk: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure StretchChkChange(Sender: TObject);
  private
    procedure SetControlState;
  public
    function ShowForm(Img: TdxImage): Integer;
  end;

var
  ImagePropsFm: TImagePropsFm;

function ShowImagePropsForm(Img: TdxImage): Integer;

implementation

function ShowImagePropsForm(Img: TdxImage): Integer;
begin
  if ImagePropsFm = nil then
    ImagePropsFm := TImagePropsFm.Create(Application);
  Result := ImagePropsFm.ShowForm(Img);
end;

{$R *.lfm}

{ TImagePropsFm }

procedure TImagePropsFm.FormCreate(Sender: TObject);
begin
  Caption := rsImageProps;
  StretchChk.Caption := rsShrinkStretch;
  PropChk.Caption := rsAspectRatio;
  KeepSizeChk.Caption := rsDoNotStretch;
  CenterChk.Caption := rsAlignCenter;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
end;

procedure TImagePropsFm.StretchChkChange(Sender: TObject);
begin
  SetControlState;
end;

procedure TImagePropsFm.SetControlState;
begin
  PropChk.Enabled := StretchChk.Checked;
  KeepSizeChk.Enabled := StretchChk.Checked;
end;

function TImagePropsFm.ShowForm(Img: TdxImage): Integer;
begin
  StretchChk.Checked := Img.Stretch;
  PropChk.Checked := Img.Proportional;
  KeepSizeChk.Checked := Img.KeepSize;
  CenterChk.Checked := Img.Center;
  SetControlState;
  Result := ShowModal;
  if Result = mrOk then
  begin
    Img.Stretch := StretchChk.Checked;
    Img.Proportional := PropChk.Checked;
    Img.KeepSize := KeepSizeChk.Checked;
    Img.Center := CenterChk.Checked;
  end;
end;

end.

