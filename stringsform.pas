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

unit StringsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TStringsFm }

  TStringsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FUrl: String;
  public
    { public declarations }
    function ShowForm(const Title, Url: String; SL: TStrings): Integer;
  end;

var
  StringsFm: TStringsFm;

function ShowStringsForm(const Title, Url: String; SL: TStrings): Integer;

implementation

uses
  helpmanager;

function ShowStringsForm(const Title, Url: String; SL: TStrings): Integer;
begin
  if StringsFm = nil then
  	StringsFm := TStringsFm.Create(Application);
  Result := StringsFm.ShowForm(Title, Url, SL);
end;

{$R *.lfm}

{ TStringsFm }

procedure TStringsFm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure TStringsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp(FUrl);
end;

procedure TStringsFm.FormCreate(Sender: TObject);
begin
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

function TStringsFm.ShowForm(const Title, Url: String; SL: TStrings): Integer;
begin
  FUrl := Url;
  ButtonPanel1.HelpButton.Visible:=Url > '';
  Caption := Title;
  Memo1.Clear;
  Memo1.Lines.AddStrings(SL);
  if ShowModal = mrOk then
  begin
    SL.Clear;
    SL.AddStrings(Memo1.Lines);
  end;
  Result := ModalResult;
end;

end.

