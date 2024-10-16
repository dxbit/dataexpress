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

unit VisibleFormsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  ButtonPanel, StdCtrls, ExtCtrls, Buttons, strconsts, DxCtrls;

type

  { TVisibleFormsFm }

  TVisibleFormsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    List: TCheckListBox;
    Panel1: TPanel;
    UpBn: TSpeedButton;
    DownBn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ListSelectionChange(Sender: TObject; User: boolean);
    procedure UpBnClick(Sender: TObject);
    procedure DownBnClick(Sender: TObject);
  private
    { private declarations }
    procedure SetControlState;
  public
    { public declarations }
    function ShowForm: Integer;
  end;

var
  VisibleFormsFm: TVisibleFormsFm;

function ShowVisibleFormsForm: Integer;

implementation

uses
  apputils, helpmanager, formmanager;

function ShowVisibleFormsForm: Integer;
begin
  if VisibleFormsFm = nil then
  	VisibleFormsFm := TVisibleFormsFm.Create(Application);
  Result := VisibleFormsFm.ShowForm;
end;

{$R *.lfm}

{ TVisibleFormsFm }

procedure TVisibleFormsFm.FormCreate(Sender: TObject);
begin
  SetupSpeedButton(UpBn, 'up16');
  SetupSpeedButton(DownBn, 'down16');
  Caption := rsOrderVisibleForms;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TVisibleFormsFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
  SetControlState;
end;

procedure TVisibleFormsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('visibleforms');
end;

procedure TVisibleFormsFm.ListSelectionChange(Sender: TObject; User: boolean);
begin
  SetControlState;
end;

procedure TVisibleFormsFm.UpBnClick(Sender: TObject);
var
  i: Integer;
  Tmp: Boolean;
begin
  i := List.ItemIndex;
  Tmp := List.Checked[i];
  List.Checked[i] := List.Checked[i - 1];
  List.Checked[i - 1] := Tmp;
  List.Items.Exchange(i, i - 1);
  List.ItemIndex := i - 1;
  SetControlState;
end;

procedure TVisibleFormsFm.DownBnClick(Sender: TObject);
var
  i: Integer;
  Tmp: Boolean;
begin
  i := List.ItemIndex;
  Tmp := List.Checked[i];
  List.Checked[i] := List.Checked[i + 1];
  List.Checked[i + 1] := Tmp;
  List.Items.Exchange(i, i + 1);
  List.ItemIndex := i + 1;
  SetControlState;
end;

procedure TVisibleFormsFm.SetControlState;
begin
  UpBn.Enabled := List.ItemIndex > 0;
  DownBn.Enabled := (List.ItemIndex >= 0) and (List.ItemIndex < List.Count - 1);
end;

function TVisibleFormsFm.ShowForm: Integer;
var
  i: Integer;
  Fm: TdxForm;
begin
  FormMan.SortFormsByIndex(List.Items);
  for i := 0 to List.Count - 1 do
  begin
    Fm := TdxForm(List.Items.Objects[i]);
    List.Checked[i] := Fm.AutoOpen;
  end;
  Result := ShowModal;
  if Result <> mrOk then Exit;
  for i := 0 to List.Count - 1 do
  begin
    Fm := TdxForm(List.Items.Objects[i]);
    Fm.AutoOpen := List.Checked[i];
    Fm.Index := i;
  end;
end;

end.

