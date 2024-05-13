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

unit CheckPrintOutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts, dxctrls;

type

  { TCheckPrintOutFm }

  TCheckPrintOutFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(Check: TdxCheckBox): Integer;
  end;

var
  CheckPrintOutFm: TCheckPrintOutFm;

function ShowCheckPrintOutForm(Check: TdxCheckBox): Integer;

implementation

uses
  helpmanager;

function ShowCheckPrintOutForm(Check: TdxCheckBox): Integer;
begin
  if CheckPrintOutFm = nil then
  	CheckPrintOutFm := TCheckPrintOutFm.Create(Application);
  Result := CheckPrintOutFm.ShowForm(Check);
end;

{$R *.lfm}

{ TCheckPrintOutFm }

procedure TCheckPrintOutFm.FormCreate(Sender: TObject);
begin
  Caption := rsPrintOut;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TCheckPrintOutFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('checkout');
end;

function TCheckPrintOutFm.ShowForm(Check: TdxCheckBox): Integer;
begin
  Edit1.Text := Check.CheckedText;
  Edit2.Text := Check.UnCheckedText;
  Result := ShowModal;
  if Result <> mrOk then Exit;
  Check.CheckedText:=Edit1.Text;
  Check.UnCheckedText:=Edit2.Text;
end;

end.

