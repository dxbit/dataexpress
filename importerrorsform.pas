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

unit ImportErrorsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TImportErrorsFm }

  TImportErrorsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowErrors(SL: TStrings): Integer;
    procedure ShowErrorsAfterImport(SL: TStrings);
  end;

var
  ImportErrorsFm: TImportErrorsFm;

function ShowImportErrorsForm(SL: TStrings): Integer;
procedure ShowAfterImportErrorsForm(SL: TStrings);

implementation

uses
  helpmanager;

// Из-за ошибки "Failed to create win32 control, error: ...", которая
// возникает когда окно появляется во второй раз, пришлось делать пересоздание
// окна.
procedure RecreateImportErrorsForm;
var
  R: TRect;
begin
  FillChar(R, SizeOf(R), 0);
  if ImportErrorsFm <> nil then
    R := ImportErrorsFm.BoundsRect;
  FreeAndNil(ImportErrorsFm);

  ImportErrorsFm := TImportErrorsFm.Create(Application);
  if R.Width > 9 then ImportErrorsFm.BoundsRect := R;
end;

function ShowImportErrorsForm(SL: TStrings): Integer;
begin
  {if ImportErrorsFm = nil then
    ImportErrorsFm := TImportErrorsFm.Create(Application); }
  RecreateImportErrorsForm;
  Result := ImportErrorsFm.ShowErrors(SL);
end;

procedure ShowAfterImportErrorsForm(SL: TStrings);
begin
  {if ImportErrorsFm = nil then
  	ImportErrorsFm := TImportErrorsFm.Create(Application);}
  RecreateImportErrorsForm;
  ImportErrorsFm.ShowErrorsAfterImport(SL);
end;

{$R *.lfm}

{ TImportErrorsFm }

procedure TImportErrorsFm.FormCreate(Sender: TObject);
begin
  Caption := rsWarning;
  ButtonPanel1.OkButton.Caption := rsContinue;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TImportErrorsFm.FormShow(Sender: TObject);
begin
  if Memo1.CanFocus then
	  Memo1.SetFocus;
end;

procedure TImportErrorsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('importerrors');
end;

function TImportErrorsFm.ShowErrors(SL: TStrings): Integer;
begin
  ButtonPanel1.ShowButtons := [pbOk, pbCancel, pbHelp];

  Memo1.Clear;
  Memo1.Lines.Add(rsImportCheckErrorMsg);
  Memo1.Lines.Add('');
  Memo1.Lines.AddStrings(SL);
  Memo1.Lines.Add('');
  Memo1.Lines.Add(rsContinueImportMsg);
  Result := ShowModal;
end;

procedure TImportErrorsFm.ShowErrorsAfterImport(SL: TStrings);
begin
  ButtonPanel1.ShowButtons := [pbClose, pbHelp];

  Memo1.Clear;
  Memo1.Lines.Add(rsAfterImportErrsMsg);
  Memo1.Lines.Add('');
  Memo1.Lines.AddStrings(SL);
  Memo1.Lines.Add('');
  ShowModal;
end;

end.

