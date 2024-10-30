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

unit RpStyleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CheckLst, ButtonPanel, Menus, ComCtrls, strconsts, DXReports;

type

  { TRpStyleFm }

  TRpStyleFm = class(TForm)
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
    { private declarations }
  public
    { public declarations }
    function ShowForm(RD: TReportData; SL: TStrings; RpKind: TReportKind): Integer;
  end;

var
  RpStyleFm: TRpStyleFm;

function ShowRpStyleForm(RD: TReportData; SL: TStrings; RpKind: TReportKind): Integer;

implementation

uses
  apputils, helpmanager;

function ShowRpStyleForm(RD: TReportData; SL: TStrings; RpKind: TReportKind
  ): Integer;
begin
  if RpStyleFm = nil then
  	RpStyleFm := TRpStyleFm.Create(Application);
  Result := RpStyleFm.ShowForm(RD, SL, RpKind);
end;

{$R *.lfm}

{ TRpStyleFm }

procedure TRpStyleFm.FormCreate(Sender: TObject);
begin
  Caption := rsCopyReportStyle;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  MenuItem1.Caption:=rsCheckAll;
  MenuItem2.Caption := rsUncheckAll;
  SetupImageList(ImageList1, ['checkall16', 'uncheckall16']);
  ToolButton1.Caption := rsCheckAll;
  ToolButton2.Caption := rsUncheckAll;
end;

procedure TRpStyleFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
end;

procedure TRpStyleFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('copystyle');
end;

procedure TRpStyleFm.MenuItem1Click(Sender: TObject);
begin
  List.CheckAll(cbChecked);
end;

procedure TRpStyleFm.MenuItem2Click(Sender: TObject);
begin
  List.CheckAll(cbUnChecked);
end;

procedure TRpStyleFm.ToolButton1Click(Sender: TObject);
begin
  MenuItem1.Click;
end;

procedure TRpStyleFm.ToolButton2Click(Sender: TObject);
begin
  MenuItem2.Click;
end;

procedure CopyStyle(RD1, RD2: TReportData);
var
  i: Integer;
begin
  RD2.Grid.Color := RD1.Grid.Color;
  RD2.Grid.AlternateColor:=RD1.Grid.AlternateColor;
  RD2.Grid.SelectedColor:=RD1.Grid.SelectedColor;
  RD2.Grid.SelectedTextColor:=RD1.Grid.SelectedTextColor;
  RD2.Grid.InactiveSelectedColor:=RD1.Grid.InactiveSelectedColor;
  RD2.Grid.InactiveSelectedTextColor:=RD1.Grid.InactiveSelectedTextColor;
  RD2.Grid.FixedColor:=RD1.Grid.FixedColor;
  //RD2.Grid.FixedHotColor:=RD1.Grid.FixedHotColor;
  RD2.Grid.GridLineColor:=RD1.Grid.GridLineColor;
  RD2.Grid.GridLineStyle:=RD1.Grid.GridLineStyle;
  RD2.Grid.Flat:=RD1.Grid.Flat;
  RD2.Grid.DefaultRowHeight:=RD1.Grid.DefaultRowHeight;
  RD2.Grid.VertLines:=RD1.Grid.VertLines;
  RD2.Grid.HorzLines:=RD1.Grid.HorzLines;
  RD2.Grid.WordWrap:=RD1.Grid.WordWrap;
  RD2.Grid.RowSelect:=RD1.Grid.RowSelect;
  RD2.Grid.RowHighlight:=RD1.Grid.RowHighlight;
  RD2.Grid.CellEllipsis:=RD1.Grid.CellEllipsis;
  RD2.Grid.ShowHints:=RD1.Grid.ShowHints;
  RD2.Grid.ThumbTracking:=RD1.Grid.ThumbTracking;
  RD2.Grid.AllowChangeSort:=RD1.Grid.AllowChangeSort;
  RD2.Grid.ColMove:=RD1.Grid.ColMove;

  RD2.Grid.Font.Assign(RD1.Grid.Font);
  RD2.Grid.TitleFont.Assign(RD1.Grid.TitleFont);
  with RD2.Grid do
    for i := 0 to ColumnCount - 1 do
    begin
      Columns[i].Font.Assign(Font);
      Columns[i].TitleFont.Assign(TitleFont);
      Columns[i].Color:=Color;
      Columns[i].FixedColor:=FixedColor;
    end;
  RD2.SetReportChanged;
end;

function TRpStyleFm.ShowForm(RD: TReportData; SL: TStrings; RpKind: TReportKind
  ): Integer;
var
  i: Integer;
begin
  if RpKind = rkReport then
    StaticText1.Caption := rsDestinationReports
  else
    StaticText1.Caption := rsDestinationQueries;
  List.Items := SL;
  List.Items.Delete( List.Items.IndexOfObject(RD) );
  Result := ShowModal;
  if Result <> mrOk then Exit;
  for i := 0 to List.Count - 1 do
    if List.Checked[i] then CopyStyle(RD, TReportData(List.Items.Objects[i]));
end;

end.

