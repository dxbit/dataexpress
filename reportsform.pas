{
Copyright © 2015-2017 Pavel Duborkin
Author: Pavel Duborkin
E-Mail: 7bit@list.ru, mydataexpress@mail.ru

This file is part of DataExpress.

DataExpress is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

DataExpress is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with DataExpress.  If not, see <http://www.gnu.org/licenses/>.
}
unit ReportsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Menus, ExtCtrls, Buttons, strconsts, DXReports;

type

  { TReportsFm }

  TReportsFm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    ButtonPanel1: TButtonPanel;
    List: TListBox;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    procedure ButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListSelectionChange(Sender: TObject; User: boolean);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
  private
    { private declarations }
    FInDesigner: Boolean;
    FModified: Boolean;
    FDelRps: TList;
    procedure FillReports;
    //procedure CheckQueries;
    procedure SetControlState;
  public
    { public declarations }
    function ShowForm(InDesigner: Boolean): Integer;
    property DelRps: TList read FDelRps;
  end;

var
  ReportsFm: TReportsFm;

implementation

uses
  apputils, reportmanager, ReportForm, rpgridform, rpstyleform,
  helpform, inputform, QueryCalcForm, exprform, totalsform, helptextform,
  querycoloringform, propdialogs;

{$R *.lfm}

{ TReportsFm }

procedure TReportsFm.FormShow(Sender: TObject);
begin
  FDelRps.Clear;
  FillReports;
  List.SetFocus;
  SetControlState;
end;

procedure TReportsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('reports');
end;

procedure TReportsFm.ListDblClick(Sender: TObject);
begin
  if List.ItemIndex >= 0 then
    MenuItem5.Click;
end;

procedure TReportsFm.ListSelectionChange(Sender: TObject; User: boolean);
begin
  SetControlState;
end;

procedure TReportsFm.MenuItem10Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  if CalcFm.ShowForm(RD, nil) = mrOk then FModified := True;
end;

procedure TReportsFm.MenuItem11Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  TotalsFm.ShowForm(RD);
end;

procedure TReportsFm.MenuItem12Click(Sender: TObject);
var
  RD: TReportData;
  OldText: String;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  OldText := RD.HelpText;
  RD.HelpText:=Trim(HelpTextFm.ShowForm(RD.HelpText));
  if OldText <> RD.HelpText then FModified := True;
end;

procedure TReportsFm.MenuItem13Click(Sender: TObject);
var
  SrcRD, RD: TReportData;
  MS: TMemoryStream;
  OldId: Integer;
  S: String;
begin
  SrcRD := TReportData(List.Items.Objects[List.ItemIndex]);
  S := SrcRD.Name;
  if InputStr(rsNewReport, rsReportName, 'queryname', S, True) then
  begin
    RD := ReportMan.CreateNewReport;
    OldId := RD.Id;

    MS := TMemoryStream.Create;
    SrcRD.SaveToStream(MS);
    MS.Position:=0;
    RD.LoadFromStream(MS);
    MS.Free;

    RD.Id := OldId;
    RD.Name := S;
    List.Items.AddObject(S, RD);
    List.ItemIndex := List.Count - 1;
    ReportFm.ShowForm(RD, nil);
    FModified := True;
  end;
end;

procedure TReportsFm.MenuItem14Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  if QueryColoringFm.ShowForm(RD, nil) = mrOk then FModified := True;
end;

procedure TReportsFm.MenuItem1Click(Sender: TObject);
var
  S: String;
  RD: TReportData;
begin
  S := '';
  //if InputStr(rsNewReport, rsReportName, 'queryname', S, True) then
  if ReportNameDlg(rsNewReport, S, nil) = mrOk then
  begin
    RD := ReportMan.CreateNewReport;
    RD.Name := S;
    List.Items.AddObject(S, RD);
    List.ItemIndex := List.Count - 1;
    ReportFm.ShowForm(RD, nil);
    FModified := True;
  end;
end;

procedure TReportsFm.MenuItem2Click(Sender: TObject);
var
  RD: TReportData;
  i: Integer;
begin
  if not ConfirmDelete then Exit;
  i := List.ItemIndex;
  RD := TReportData(List.Items.Objects[i]);
  FDelRps.Add(Pointer(RD.Id));
  ReportMan.DeleteReport(RD);
  List.Items.Delete(i);
  if i > List.Count - 1 then
    i := List.Count - 1;
  List.ItemIndex := i;
  SetControlState;
  FModified := True;
end;

procedure TReportsFm.MenuItem4Click(Sender: TObject);
var
  RD: TReportData;
  i: Integer;
  S: String;
begin
  i := List.ItemIndex;
  RD := TReportData(List.Items.Objects[i]);
  S := RD.Name;
  //if InputStr(rsRenameReport, rsReportName, 'queryname', S, True) then
  if ReportNameDlg(rsRenameReport, S, RD) = mrOk then
  begin
    RD.Name := S;
    List.Items[i] := S;
    FModified := True;
  end;
end;

procedure TReportsFm.MenuItem5Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  if ReportFm.ShowForm(RD, nil) = mrOk then FModified := True;
end;

procedure TReportsFm.MenuItem6Click(Sender: TObject);
var
  S: String;
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  S := RD.Filter;
  if ExprFm.ShowForm(rsOutputFilter + ': ' + RD.Name, nil, S, nil, nil, nil, RD,
    'outfilter') then
  begin
    RD.Filter := S;
    FModified := True;
  end;
end;

procedure TReportsFm.MenuItem7Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  if RpGridFm.ShowForm(RD) = mrOk then FModified := True;
end;

procedure TReportsFm.MenuItem9Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  if RpStyleFm.ShowForm(RD, List.Items) = mrOk then FModified := True;
end;

procedure TReportsFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  if FInDesigner or (ModalResult = mrOk) then
  begin
    for i := 0 to FDelRps.Count - 1 do
      DeleteRefFromIntfs(PtrInt(FDelRps[i]), False);
  end;
  if FInDesigner then Exit;
  if ModalResult = mrOk then
    ReportMan.SaveToDB
  else
    ReportMan.LoadFromDB;
end;

procedure TReportsFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (ModalResult = mrCancel) and FModified then
    CanClose := MessageDlg(rsWarning, rsCancelChangesMsg, mtWarning, [mbYes,
      mbNo], 0) = mrYes
end;

procedure TReportsFm.ButtonClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: MenuItem1.Click;
    1: MenuItem2.Click;
    2: MenuItem4.Click;
    3: MenuItem5.Click;
    5: MenuItem7.Click;
    7: MenuItem10.Click;
    6: MenuItem9.Click;
    8: MenuItem6.Click;
    9: MenuItem11.Click;
    10: MenuItem12.Click;
    11: MenuItem13.Click;
    12: MenuItem14.Click;
  end;
end;

procedure TReportsFm.FormCreate(Sender: TObject);
begin
  Caption := rsReports;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  MenuItem1.Caption := rsAppend;
  SetMenuItemImage(MenuItem1, 'add16');
  MenuItem2.Caption := rsDelete;
  SetMenuItemImage(MenuItem2, 'delete16');
  MenuItem4.Caption := rsRename;
  MenuItem5.Caption := rsSelection;
  SetMenuItemImage(MenuItem5, 'db16');
  MenuItem7.Caption := rsReportGrid;
  SetMenuItemImage(MenuItem7, 'grid16');
  MenuItem9.Caption := rsCopyReportStyle;
  SetMenuItemImage(MenuItem9, 'copy16');
  MenuItem10.Caption := rsCalcFields;
  SetMenuItemImage(MenuItem10, 'sum16');
  MenuItem6.Caption := rsOutputFilter;
  SetMenuItemImage(MenuItem6, 'filter16');
  MenuItem11.Caption := rsTotals;
  MenuItem12.Caption := rsHelpText;
  MenuItem13.Caption:=rsCopy;
  MenuItem14.Caption := rsColoring;
  BitBtn1.Caption := rsAppend;
  BitBtn6.Caption := rsDelete;
  BitBtn2.Caption := rsRename;
  BitBtn3.Caption := rsSelection;
  BitBtn5.Caption := rsReportGrid;
  BitBtn7.Caption := rsCopyReportStyle;
  BitBtn8.Caption:= rsCalcFields;
  BitBtn9.Caption := rsOutputFilter;
  BitBtn10.Caption := rsTotals;
  BitBtn11.Caption := rsHelpText;
  BitBtn4.Caption := rsCopy;
  BitBtn12.Caption := rsColoring;
  FDelRps := TList.Create;
end;

procedure TReportsFm.FormDestroy(Sender: TObject);
begin
  FDelRps.Free;
end;

procedure TReportsFm.FillReports;
begin
  ReportMan.GetReports(List.Items);
end;

procedure TReportsFm.SetControlState;
var
  b: Boolean;
begin
  b := List.ItemIndex >= 0;
  MenuItem2.Enabled := b;
  MenuItem4.Enabled := b;
  MenuItem5.Enabled := b;
  MenuItem7.Enabled := b;
  MenuItem9.Enabled := b;
  MenuItem10.Enabled := b;
  MenuItem6.Enabled := b;
  MenuItem11.Enabled := b;
  MenuItem12.Enabled := b;
  MenuItem13.Enabled := b;
  MenuItem14.Enabled := b;

  BitBtn2.Enabled := b;
  BitBtn3.Enabled := b;
  BitBtn5.Enabled := b;
  BitBtn6.Enabled := b;
  BitBtn7.Enabled := b;
  BitBtn8.Enabled := b;
  BitBtn9.Enabled := b;
  BitBtn10.Enabled := b;
  BitBtn11.Enabled := b;
  BitBtn4.Enabled := b;
  BitBtn12.Enabled := b;
end;

function TReportsFm.ShowForm(InDesigner: Boolean): Integer;
begin
  FInDesigner := InDesigner;
  FModified := False;
  if FInDesigner then
    ButtonPanel1.ShowButtons:=[pbClose, pbHelp]
  else
  begin
    ButtonPanel1.ShowButtons := [pbOk, pbCancel, pbHelp];
    ReportMan.LoadFromDB;
  end;
  Result := ShowModal;
end;

end.

