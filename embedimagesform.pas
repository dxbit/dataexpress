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

unit EmbedImagesForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  ButtonPanel, LResources, StdCtrls, ListViewFilterEdit, strconsts;

type

  { TEmbedImagesFm }

  TEmbedImagesFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList24: TImageList;
    ImageList16: TImageList;
    Label1: TLabel;
    List16: TListView;
    List24: TListView;
    Pages: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure List16DblClick(Sender: TObject);
  private

  public
    function ShowForm: Integer;
    procedure GetSelectedImages(SL: TStrings);
  end;

var
  EmbedImagesFm: TEmbedImagesFm;

function ShowEmbedImagesForm: Integer;

implementation

uses
  apputils, StrUtils, imagemanager;

function ShowEmbedImagesForm: Integer;
begin
  if EmbedImagesFm = nil then
    EmbedImagesFm := TEmbedImagesFm.Create(Application);
  Result := EmbedImagesFm.ShowForm;
end;

{$R *.lfm}

{ TEmbedImagesFm }

procedure TEmbedImagesFm.FormCreate(Sender: TObject);
var
  i, n16, n24: Integer;
  Res: TLResource;
  Arr16, Arr24: array of String;
  LI: TListItem;
begin
  Caption := rsSelectEmbeddImages;
  Label1.Caption := rsSelectEmbedImagesHint;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;

  n16 := 0;
  n24 := 0;
  SetLength(Arr16, 500);
  SetLength(Arr24, 500);
  for i := 0 to LazarusResources.Count - 1 do
  begin
    Res := LazarusResources.Items[i];
    if LeftStr(Res.Name, 1) = '_' then
    else if RightStr(Res.Name, 2) = '16' then
    begin
      Arr16[n16] := Res.Name;
      Inc(n16);
    end
    else if RightStr(Res.Name, 2) = '24' then
    begin
      Arr24[n24] := Res.Name;
      Inc(n24);
    end
  end;

  SetLength(Arr16, n16);
  SetLength(Arr24, n24);
  SetupImageList(ImageList16, Arr16);
  SetupImageList(ImageList24, Arr24);

  List16.BeginUpdate;
  for i := 0 to High(Arr16) do
  begin
    LI := List16.Items.Add;
    LI.Caption := Arr16[i];
    LI.ImageIndex := i;
  end;
  List16.EndUpdate;

  List24.BeginUpdate;
  for i := 0 to High(Arr24) do
  begin
    LI := List24.Items.Add;
    LI.Caption := Arr24[i];
    LI.ImageIndex := i;
  end;
  List24.EndUpdate;
end;

procedure TEmbedImagesFm.FormShow(Sender: TObject);
begin
  List16.ClearSelection;
  List24.ClearSelection;
  if Pages.ActivePageIndex = 0 then
    List16.SetFocus
  else
    List24.SetFocus;
end;

procedure TEmbedImagesFm.List16DblClick(Sender: TObject);
begin
  if TListView(Sender).Selected <> nil then
    ModalResult := mrOk;
end;

function TEmbedImagesFm.ShowForm: Integer;
begin
  Result := ShowModal;
end;

procedure TEmbedImagesFm.GetSelectedImages(SL: TStrings);
var
  LV: TListView;
  i: Integer;
  LI: TListItem;
begin
  if Pages.ActivePageIndex = 0 then
    LV := List16
  else
    LV := List24;
  for i := 0 to LV.Items.Count - 1 do
  begin
    LI := LV.Items[i];
    if LI.Selected then
      SL.Add(LI.Caption);
  end;
end;

end.

