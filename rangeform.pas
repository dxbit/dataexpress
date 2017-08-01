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

implementation

uses
  apputils, helpform;

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

