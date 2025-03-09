{-------------------------------------------------------------------------------

    Copyright 2015-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit ThumbForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ButtonPanel, dximages, strconsts, helpmanager;

type

  { TThumbFm }

  TThumbFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ShowThumb: TCheckBox;
    Label1: TLabel;
    Size: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private

  public
    function ShowForm(C: TdxDBImage): Integer;
  end;

var
  ThumbFm: TThumbFm;

function ShowThumbForm(C: TdxDBImage): Integer;

implementation

uses
  apputils;

function ShowThumbForm(C: TdxDBImage): Integer;
begin
  if ThumbFm = nil then
    ThumbFm := TThumbFm.Create(Application);
  Result := ThumbFm.ShowForm(C);
end;

{$R *.lfm}

{ TThumbFm }

procedure TThumbFm.FormCreate(Sender: TObject);
begin
  Caption := rsThumb;
  Label1.Caption := rsThumbnailSize;
  ShowThumb.Caption := rsShowThumbnail;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  AddFormHeight(Self);
end;

procedure TThumbFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('thumbnail');
end;

function TThumbFm.ShowForm(C: TdxDBImage): Integer;
begin
  Size.Value := C.ThumbSize;
  ShowThumb.Checked := C.ShowThumbnail;
  Result := ShowModal;
  if Result = mrOk then
  begin
    C.ThumbSize:=Size.Value;
    C.ShowThumbnail:=ShowThumb.Checked;
  end;
end;

end.

