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

unit FilterForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, strconsts, dxctrls, filtercontrol,
  myclasses, LclType;

type
  { TFilterFm }

  TFilterFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Images: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    FForm: TdxForm;
    FGrid: TFormFilterControl;
    { private declarations }
  public
    { public declarations }
    function ShowForm(var Filter: TFilterObject): Integer;
    property Form: TdxForm read FForm write FForm;
  end;

//var
//  FilterFm: TFilterFm;

implementation

uses
  apputils, helpmanager;

{$R *.lfm}

{ TFilterFm }

procedure TFilterFm.FormCreate(Sender: TObject);
begin
  Caption := rsFilter;
  FGrid := TFormFilterControl.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align := alClient;
  FGrid.BorderSpacing.Left:=4;
  FGrid.BorderSpacing.Right:=4;
  FGrid.Buttons := ToolBar1;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  ButtonPanel1.OKButton.Hint:=rsApplyFilter;
  ButtonPanel1.OKButton.Default := False;
  SetupImageList(Images, ['add16', 'delete16']);
  ToolButton1.Caption:=rsAddField;
  ToolButton2.Hint:=rsDeleteField;
  ToolButton4.Caption:=rsAddValue;
  ToolButton5.Hint := rsDeleteValue;
end;

procedure TFilterFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    Key := 0;
    FGrid.EditorMode := False;
    ModalResult := mrOk;
  end;
end;

procedure TFilterFm.FormShow(Sender: TObject);
begin
  FGrid.SetFocus;
  FGrid.Col := 3;
  if FGrid.RowCount > 1 then FGrid.Row := 1;
end;

procedure TFilterFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('filter');
end;

function TFilterFm.ShowForm(var Filter: TFilterObject): Integer;
begin
  FGrid.Form := FForm;
  FGrid.Filter := Filter;
  FGrid.Init;
  FGrid.Load;
  Result := ShowModal;
  if Result = mrOk then
  begin
    FGrid.Save;
    Filter := FGrid.Filter;
  end;
end;

end.

