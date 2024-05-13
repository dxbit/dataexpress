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

unit ColoringForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, Menus, strconsts, StdCtrls, dxctrls, DialogGrid;

type

  { TColoringFm }

  TColoringFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TDialogGrid;
    DialogGridButtons1: TDialogGridButtons;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FForm: TdxForm;
    procedure FillGrid;
    procedure Save;
  public
    { public declarations }
    procedure ShowForm(aForm: TdxForm; ARow: Integer = 0);
  end;

var
  ColoringFm: TColoringFm;

procedure ShowColoringForm(aForm: TdxForm; ARow: Integer = 0);

implementation

uses
  exprform, helpmanager, apputils;

procedure ShowColoringForm(aForm: TdxForm; ARow: Integer);
begin
  if ColoringFm = nil then
  	ColoringFm := TColoringFm.Create(Application);
  ColoringFm.ShowForm(aForm, ARow);
end;

{$R *.lfm}

{ TColoringFm }

procedure TColoringFm.FormCreate(Sender: TObject);
begin
  Caption := rsColoring;
  Grid.FocusColor:=Grid.SelectedColor;
  Grid.Columns[0].Title.Caption:=rsColor;
  Grid.Columns[1].Title.Caption:=rsExpression;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TColoringFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
  begin
    if Grid.Modified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TColoringFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TColoringFm.GridButtonClick(Sender: TObject; aCol, aRow: Integer);
var
  S: TCaption;
begin
  S := Grid.Cells[aCol, aRow];
  if ShowExprForm(etColoring, nil, S, FForm, nil, nil, nil) = mrOk then
    Grid.Cells[aCol, aRow] := S;
end;

procedure TColoringFm.GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
var
  r: Integer;
begin
  case Cmd of
    dgcAppend:
      begin
        r := Grid.RowCount;
        Grid.RowCount := r + 1;
        Grid.Cells[0, r] := ColorToString(clWhite);
        Grid.Row := r;
      end;
    dgcDelete:
      if ConfirmDelete then
        Grid.DeleteRow(Grid.Row);
  end;
end;

procedure TColoringFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('coloring');
end;

procedure TColoringFm.FillGrid;
var
  SL: TStrings;
  S: String;
  i, p, r: Integer;
begin
  Grid.RowCount := 1;
  SL := FForm.Coloring;
  for i := 0 to SL.Count - 1 do
  begin
    r := i + 1;
    Grid.RowCount := r + 1;
    S := SL[i];
    p := Pos(';', S);
    Grid.Cells[0, r] := Copy(S, 1, p - 1);
    Grid.Cells[1, r] := Copy(S, p + 1, 20000);
  end;
end;

procedure TColoringFm.Save;
var
  SL: TStrings;
  i: Integer;
begin
  SL := FForm.Coloring;
  SL.Clear;
  for i := 1 to Grid.RowCount - 1 do
    SL.Add(Grid.Cells[0, i] + ';' + Grid.Cells[1, i]);
end;

procedure TColoringFm.ShowForm(aForm: TdxForm; ARow: Integer);
var
  i: Integer;
begin
  FForm := aForm;
  FillGrid;
  Grid.Modified:=False;
  Grid.ClearSelections;
  if ARow > 0 then
  begin
    Grid.Row := ARow;
    Grid.Col := 1;
  end;
  if ShowModal = mrOk then Save;
end;

end.

