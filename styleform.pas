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

unit StyleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  ButtonPanel, StdCtrls, Menus, ComCtrls, strconsts, dxctrls;

type

  { TStyleFm }

  TStyleFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    List: TCheckListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    StaticText1: TStaticText;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    FFm: TdxForm;
    { private declarations }
    procedure CopyStyle(aForm: TdxForm);
  public
    { public declarations }
    function ShowForm(aForm: TdxForm): Integer;
  end;

var
  StyleFm: TStyleFm;

function ShowStyleForm(aForm: TdxForm): Integer;

implementation

uses
  apputils, helpmanager, formmanager;

function ShowStyleForm(aForm: TdxForm): Integer;
begin
  if StyleFm = nil then
  	StyleFm := TStyleFm.Create(Application);
  Result := StyleFm.ShowForm(aForm);
end;

{$R *.lfm}

{ TStyleFm }

procedure TStyleFm.FormCreate(Sender: TObject);
begin
  Caption := rsCopyFormStyle;
  StaticText1.Caption := rsDestinationForms;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  MenuItem1.Caption:=rsCheckAll;
  MenuItem2.Caption := rsUncheckAll;
  SetupImageList(ImageList1, ['checkall16', 'uncheckall16']);
  ToolButton1.Caption := rsCheckAll;
  ToolButton2.Caption := rsUncheckAll;
end;

procedure TStyleFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
end;

procedure TStyleFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('copyformstyle');
end;

procedure TStyleFm.MenuItem1Click(Sender: TObject);
begin
  List.CheckAll(cbChecked);
end;

procedure TStyleFm.MenuItem2Click(Sender: TObject);
begin
  List.CheckAll(cbUnChecked);
end;

procedure TStyleFm.ToolButton1Click(Sender: TObject);
begin
  MenuItem1.Click;
end;

procedure TStyleFm.ToolButton2Click(Sender: TObject);
begin
  MenuItem2.Click;
end;

procedure TStyleFm.CopyStyle(aForm: TdxForm);
begin
  aForm.Font := FFm.Font;
  aForm.Color := FFm.Color;
  aForm.Grid.Color:=FFm.Grid.Color;
  aForm.Grid.AlternateColor:=FFm.Grid.AlternateColor;
  aForm.Grid.SelectedColor:=FFm.Grid.SelectedColor;
  aForm.Grid.SelectedTextColor:=FFm.Grid.SelectedTextColor;
  aForm.Grid.InactiveSelectedColor:=FFm.Grid.InactiveSelectedColor;
  aForm.Grid.InactiveSelectedTextColor:=FFm.Grid.InactiveSelectedTextColor;
  aForm.Grid.DefaultRowHeight:=FFm.Grid.DefaultRowHeight;
  aForm.Grid.Flat:=FFm.Grid.Flat;
  aForm.Grid.Options:=FFm.Grid.Options;
  aForm.Grid.GridLineColor:=FFm.Grid.GridLineColor;
  aForm.Grid.GridLineStyle:=FFm.Grid.GridLineStyle;
  aForm.Grid.Font := FFm.Grid.Font;
  aForm.Grid.TitleFont := FFm.Grid.TitleFont;
  aForm.Grid.FixedColor:=FFm.Grid.FixedColor;
  //aForm.Grid.FixedHotColor:=FFm.Grid.FixedHotColor;
  aForm.Grid.WordWrap:=FFm.Grid.WordWrap;
	aForm.Grid.AllowChangeSort:=FFm.Grid.AllowChangeSort;
  aForm.TreeBackColor:=FFm.TreeBackColor;
  aForm.TreeFont := FFm.TreeFont;
  aForm.TreeLineColor:=FFm.TreeLineColor;
  aForm.TreeSelectColor:=FFm.TreeSelectColor;
end;

function TStyleFm.ShowForm(aForm: TdxForm): Integer;
var
  i: Integer;
  F: TdxForm;
begin
  FFm := aForm;
  FormMan.AllFormsToList(List.Items);
  List.Items.Delete( List.Items.IndexOfObject(aForm) );
  Result := ShowModal;
  if Result <> mrOk then Exit;
  for i := 0 to List.Count - 1 do
  begin
    if not List.Checked[i] then Continue;
    F := TdxForm(List.Items.Objects[i]);
    CopyStyle(F);
  end;
end;

end.

