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

unit RangeForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ButtonPanel, Buttons, strconsts, dxctrls;

type

  { TRangeFm }

  TRangeFm = class(TForm)
    Bn: TBitBtn;
    ButtonPanel1: TButtonPanel;
    MinV: TFloatSpinEdit;
    MaxV: TFloatSpinEdit;
    Label1: TLabel;
    procedure BnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FCmp: TdxCalcEdit;
  public
    { public declarations }
    procedure ShowForm(C: TdxCalcEdit);
  end;

var
  RangeFm: TRangeFm;

procedure ShowRangeForm(C: TdxCalcEdit);

implementation

uses
  apputils, helpmanager;

procedure ShowRangeForm(C: TdxCalcEdit);
begin
  if RangeFm = nil then
  	RangeFm := TRangeFm.Create(Application);
  RangeFm.ShowForm(C);
end;

{$R *.lfm}

{ TRangeFm }

procedure TRangeFm.FormCreate(Sender: TObject);
begin
  Caption := rsRange;
  Label1.Caption := rsEnterMinMax;
  Bn.Caption := rsAll;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TRangeFm.FormShow(Sender: TObject);
begin
  MinV.SetFocus;
end;

procedure TRangeFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('range');
end;

procedure TRangeFm.BnClick(Sender: TObject);
begin
  MinV.Value := 0;
  MaxV.Value := 0;
end;

procedure TRangeFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
  begin
    if MinV.Value > MaxV.Value then
    begin
      ErrMsg(rsInvalidRange);
      CanClose := False;
    end;
  end;
end;

procedure TRangeFm.ShowForm(C: TdxCalcEdit);
begin
  FCmp := C;
  MinV.Value := C.MinValue;
  MaxV.Value := C.MaxValue;
  MinV.DecimalPlaces:=C.Precission;
  MaxV.DecimalPlaces:=C.Precission;
  if ShowModal = mrOk then
  begin
    C.MinValue := MinV.Value;
    C.MaxValue := MaxV.Value;
  end;
end;

end.

