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
unit CounterForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ButtonPanel, strconsts, dxctrls;

type

  { TCounterFm }

  TCounterFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Chk: TCheckBox;
    Label1: TLabel;
    Val: TSpinEdit;
    procedure ChkChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(C: TdxCounter);
  end;

var
  CounterFm: TCounterFm;

implementation

uses
  helpform;

{$R *.lfm}

{ TCounterFm }

procedure TCounterFm.FormCreate(Sender: TObject);
begin
  Caption := rsStartWith;
  Chk.Caption:=rsChangeCounter;
  Label1.Caption := rsBeginValue;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TCounterFm.FormShow(Sender: TObject);
begin
  Chk.SetFocus;
end;

procedure TCounterFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('startwith');
end;

procedure TCounterFm.ChkChange(Sender: TObject);
begin
  Val.Enabled:=Chk.Checked;
end;

procedure TCounterFm.ShowForm(C: TdxCounter);
begin
  Chk.Checked:=C.Restart;
  Val.Value:=C.StartWith;
  Chk.OnChange(Chk);
  if ShowModal = mrOk then
  begin
    C.Restart := Chk.Checked;
    C.StartWith:=Val.Value;
  end;
end;

end.

