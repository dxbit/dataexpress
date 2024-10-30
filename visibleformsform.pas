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
  ButtonPanel, StdCtrls, ExtCtrls, Buttons, strconsts, DxCtrls, DialogGrid;

type

  { TVisibleFormsFm }

  TVisibleFormsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TDialogGrid;
    DialogGridButtons1: TDialogGridButtons;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    procedure Load;
    procedure Save;
    function Validate: Boolean;
  public
    { public declarations }
    function ShowForm: Integer;
  end;

var
  VisibleFormsFm: TVisibleFormsFm;

function ShowVisibleFormsForm: Integer;

implementation

uses
  apputils, helpmanager, formmanager, dxmains;

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
  Caption := rsOrderVisibleForms;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TVisibleFormsFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean
  );
begin
  if ModalResult = mrOk then
  begin
    CanClose := Validate;
  end;
end;

procedure TVisibleFormsFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TVisibleFormsFm.GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
var
  r: Integer;
begin
  if Cmd = dgcAppend then
  begin
    r := Grid.RowCount;
    Grid.RowCount := r + 1;
    Grid.Row := r;
  end
  else if Cmd = dgcDelete then
  begin
    Grid.DeleteRow(Grid.Row);
  end;
end;

procedure TVisibleFormsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('visibleforms');
end;

procedure TVisibleFormsFm.Load;
var
  i: Integer;
  Fm: TdxForm;
begin
  Grid.RowCount := DXMain.Tabs.Count;
  for i := 0 to DXMain.Tabs.Count - 1 do
  begin
    Fm := FormMan.FindForm(DXMain.Tabs[i]);
    Grid.Cells[0, i] := Fm.FormCaption;
    Grid.Objects[0, i] := Fm;
  end;
end;

procedure TVisibleFormsFm.Save;
var
  i: Integer;
  Fm: TdxForm;
begin
  DXMain.Tabs.Clear;
  for i := 0 to Grid.RowCount - 1 do
  begin
    Fm := TdxForm(Grid.Objects[0, i]);
    DXMain.Tabs.AddValue(Fm.Id);
  end;
end;

function TVisibleFormsFm.Validate: Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := 0 to Grid.RowCount - 1 do
  begin
    if Grid.Cells[0, i] = '' then
    begin
      ErrMsg(rsFormNotSel);
      Grid.Row := i;
      Exit;
    end;
    for j := i + 1 to Grid.RowCount - 1 do
    begin
      if Grid.Cells[0, j] = Grid.Cells[0, i] then
      begin
        ErrMsg(rsFormAlreadySelected);
        Grid.Row := j;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function TVisibleFormsFm.ShowForm: Integer;
begin
  Load;
  FormMan.FormsToList(Grid.Columns[0].PickList);
  Result := ShowModal;
  if Result = mrOk then Save;
end;

end.

