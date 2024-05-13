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

procedure ShowCounterForm(C: TdxCounter);

implementation

uses
  helpmanager, formdesigner;

procedure ShowCounterForm(C: TdxCounter);
begin
  if CounterFm = nil then
  	CounterFm := TCounterFm.Create(Application);
  CounterFm.ShowForm(C);
end;

{$R *.lfm}

{ TCounterFm }

procedure TCounterFm.FormCreate(Sender: TObject);
begin
  Val.MaxValue:=MaxInt;
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
    Cache.SetCounter(C, Val.Value);
  end;
end;

end.

