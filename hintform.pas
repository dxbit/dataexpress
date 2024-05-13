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

unit HintForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { THintFm }

  THintFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(C: TControl);
  end;

var
  HintFm: THintFm;

procedure ShowHintForm(C: TControl);

implementation

uses
  helpmanager;

procedure ShowHintForm(C: TControl);
begin
  if HintFm = nil then
  	HintFm := THintFm.Create(Application);
  HintFm.ShowForm(C);
end;

{$R *.lfm}

{ THintFm }

procedure THintFm.FormCreate(Sender: TObject);
begin
  Caption := rsHint;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure THintFm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure THintFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('Hints');
end;

procedure THintFm.ShowForm(C: TControl);
begin
  Memo1.Text := C.Hint;
  if ShowModal = mrOk then
  begin
    C.Hint := Trim(Memo1.Text);
    C.ShowHint:=C.Hint <> '';
  end;
end;

end.

