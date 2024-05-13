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

unit LicenseForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LclType,
  strconsts;

type

  { TLicenseFm }

  TLicenseFm = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public
    function ShowForm: Integer;
  end;

var
  LicenseFm: TLicenseFm;

function ShowLicenseForm: Integer;

implementation

uses
  apputils;

function ShowLicenseForm: Integer;
begin
  if LicenseFm = nil then
    LicenseFm := TLicenseFm.Create(Application);
  Result := LicenseFm.ShowForm;
end;

{$R *.lfm}

{ TLicenseFm }

procedure TLicenseFm.FormCreate(Sender: TObject);
begin
  Caption := rsLicenseAgreement;
end;

procedure TLicenseFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrClose;
end;

function TLicenseFm.ShowForm: Integer;
var
  FlNm: String;
begin
  FlNm := AppPath + 'license.txt';

  if FileExists(FlNm) then
  begin
    Memo1.Lines.LoadFromFile(FlNm, True);
    Result := ShowModal;
  end
  else
    ErrMsgFmt(rsFileNotExists, [FlNm]);
end;

end.

