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

unit ErrorsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TErrorsFm }

  TErrorsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(const Err: String);
  end;

var
  ErrorsFm: TErrorsFm;

procedure ShowErrorsForm(const Err: String);

implementation

uses
  helpmanager;

procedure ShowErrorsForm(const Err: String);
begin
  if ErrorsFm = nil then
  	ErrorsFm := TErrorsFm.Create(Application);
  ErrorsFm.ShowForm(Err);
end;

{$R *.lfm}

{ TErrorsFm }

procedure TErrorsFm.FormCreate(Sender: TObject);
begin
  Caption := rsPrintErrors;
  ButtonPanel1.CloseButton.Caption:=rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TErrorsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('errors');
end;

procedure TErrorsFm.ShowForm(const Err: String);
begin
  Memo1.Lines.Text := Err;
  ShowModal;
end;

end.

