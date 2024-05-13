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

unit IntfForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ButtonPanel, Grids, ExtCtrls, StdCtrls, Menus, dxusers, DxCtrls,
  strconsts, DXReports, TreeViewEx, mytypes;

type

  { TIntfFm }

  TIntfFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Dflt: TCheckBox;
    Mnu: TTreeViewEx;
    Tabs: TStringGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Nm: TEdit;
    Images: TImageList;
    Label1: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Pop: TPopupMenu;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure DfltChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MnuSelectionChanged(Sender: TObject);
    procedure NmChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PopPopup(Sender: TObject);
    procedure TabsPickListSelect(Sender: TObject);
    procedure TabsSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure TabsSelection(Sender: TObject; aCol, aRow: Integer);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
  private
    { private declarations }
    FIntf, FOldIntf: TdxIntf;
    FModified: Boolean;
    procedure BuildTree;
    procedure FillForms;
    procedure FillTabs;
    procedure SetControlState;
    procedure Save;
  public
    { public declarations }
    function ShowForm(aIntf: TdxIntf): Integer;
  end;

var
  IntfFm: TIntfFm;

function ShowIntfForm(aIntf: TdxIntf): Integer;

implementation

uses
  formmanager, reportmanager, apputils, selectform, helpmanager, appsettings;

function ShowIntfForm(aIntf: TdxIntf): Integer;
begin
  if IntfFm = nil then
  	IntfFm := TIntfFm.Create(Application);
  Result := IntfFm.ShowForm(aIntf);
end;

{$R *.lfm}

{ TIntfFm }

procedure TIntfFm.FormCreate(Sender: TObject);
begin
  FIntf := TdxIntf.Create;
  Images.AddLazarusResource('add16');
  Images.AddLazarusResource('edit16');
  Images.AddLazarusResource('delete16');
  Images.AddLazarusResource('up16');
  Images.AddLazarusResource('down16');
  Images.AddLazarusResource('folder16');
  Images.AddLazarusResource('div16');
  Images.AddLazarusResource('form16');
  Images.AddLazarusResource('grid16');
  Caption := rsInterface;
  Label1.Caption := rsName;
  Dflt.Caption := rsDefault;
  TabSheet1.Caption := rsMenu;
  TabSheet2.Caption := rsTabs;
  ToolButton1.Caption := rsAppend;
  ToolButton2.Caption := rsRename;
  ToolButton3.Caption := rsDelete;
  ToolButton7.Caption := rsMoveUp;
  ToolButton8.Caption := rsMoveDown;
  ToolButton4.Caption := rsAppend;
  ToolButton6.Caption := rsDelete;
  ToolButton9.Caption := rsMoveUp;
  ToolButton10.Caption := rsMoveDown;
  MenuItem1.Caption := rsSection;
  MenuItem5.Caption := rsSubsection;
  MenuItem2.Caption := rsDelimiter;
  MenuItem3.Caption := rsForm;
  MenuItem4.Caption := rsReport;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Mnu.IsWine := AppConfig.IsWine;
end;

procedure TIntfFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  I: TdxIntf;
begin
  if ModalResult = mrOk then
  begin
    CanClose := False;
    I := UserMan.Intfs.FindIntfByName(Nm.Text);
    if Trim(Nm.Text) = '' then
      ErrMsg(rsEnterName)
    else if (I <> nil) and (I <> FOldIntf) then
    	ErrMsg(rsAnIntfWithSameNameExists)
    else
    	CanClose := True;
  end
  else
  begin
    if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TIntfFm.DfltChange(Sender: TObject);
begin
  FModified := True;
end;

procedure TIntfFm.FormDestroy(Sender: TObject);
begin
  FIntf.Free;
end;

procedure TIntfFm.FormShow(Sender: TObject);
begin
  PageControl1.TabIndex := 0;
  Nm.SetFocus;
end;

procedure TIntfFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('interface');
end;

procedure TIntfFm.MenuItem1Click(Sender: TObject);
var
  N: TTreeNode;
  S: string;
  MI: TdxMenuItem;
begin
  S := InputBox(rsNewSection, rsEnterCaption, '');
  if Trim(S) = '' then Exit;
  MI := FIntf.Menu.AddItem;
  MI.Kind := miMenu;
  MI.Caption := S;
  N := Mnu.Items.AddChild(nil, S);
  N.ImageIndex:=5;
  N.SelectedIndex:=5;
  N.Data := MI;
  FModified := True;
end;

procedure TIntfFm.MenuItem2Click(Sender: TObject);
var
  PN, N: TTreeNode;
  MI: TdxMenuItem;
  L: TdxMenuItemList;
begin
  PN := Mnu.Selected;
  L := TdxMenuItem(PN.Data).Items;
  MI := L.AddItem;
  MI.Kind := miDiv;
  N := Mnu.Items.AddChild(PN, '-------');
  N.ImageIndex := 6;
  N.SelectedIndex:=6;
  N.Data := MI;
  PN.Expand(False);
  FModified := True;
end;

procedure TIntfFm.MenuItem3Click(Sender: TObject);
var
  SL: TStringList;
  Fm: TdxForm;
  PN, N: TTreeNode;
  L: TdxMenuItemList;
  MI: TdxMenuItem;
begin
  SL := TStringList.Create;
  FormMan.FormsToList(SL);
  if ShowSelectForm(rsSelectForm, '', SL) = mrOk then
  begin
    Fm := TdxForm(SL.Objects[SelectFm.Index]);
    PN := Mnu.Selected;
    L := TdxMenuItem(PN.Data).Items;
    MI := L.AddItem;
    MI.Kind := miForm;
    MI.Id := Fm.Id;
    N := Mnu.Items.AddChild(PN, GetFullCaption(Fm));
    N.ImageIndex := 7;
    N.SelectedIndex:=7;
    N.Data := MI;
    PN.Expand(False);
    FModified := True;
  end;
  SL.Free;
end;

procedure TIntfFm.MenuItem4Click(Sender: TObject);
var
  SL: TStringList;
  PN, N: TTreeNode;
  L: TdxMenuItemList;
  MI: TdxMenuItem;
  RD: TReportData;
begin
  SL := TStringList.Create;
  ReportMan.GetReports(SL);
  if ShowSelectForm(rsSelectReport, '', SL) = mrOk then
  begin
    RD := TReportData(SL.Objects[SelectFm.Index]);
    PN := Mnu.Selected;
    L := TdxMenuItem(PN.Data).Items;
    MI := L.AddItem;
    MI.Kind := miReport;
    MI.Id := RD.Id;
    N := Mnu.Items.AddChild(PN, RD.Name);
    N.ImageIndex := 8;
    N.SelectedIndex:=8;
    N.Data := MI;
    PN.Expand(False);
    FModified := True;
  end;
  SL.Free;
end;

procedure TIntfFm.MenuItem5Click(Sender: TObject);
var
  PN, N: TTreeNode;
  S: string;
  MI: TdxMenuItem;
  L: TdxMenuItemList;
begin
  S := InputBox(rsNewSection, rsEnterCaption, '');
  if Trim(S) = '' then Exit;
  PN := Mnu.Selected;
  L := TdxMenuItem(PN.Data).Items;
  MI := L.AddItem;
  MI.Kind := miMenu;
  MI.Caption := S;
  N := Mnu.Items.AddChild(PN, S);
  N.ImageIndex := 5;
  N.SelectedIndex:=5;
  N.Data := MI;
  PN.Expand(False);
  FModified := True;
end;

procedure TIntfFm.MnuSelectionChanged(Sender: TObject);
begin
  SetControlState;
end;

procedure TIntfFm.NmChange(Sender: TObject);
begin
  FModified := True;
end;

procedure TIntfFm.PageControl1Change(Sender: TObject);
begin
  SetControlState;
end;

procedure TIntfFm.PopPopup(Sender: TObject);
var
  b: Boolean;
  MI: TdxMenuItem;
begin
  b := Mnu.Selected <> nil;
  if b then MI := TdxMenuItem(Mnu.Selected.Data);
  b := b and (MI.Kind = miMenu);
  MenuItem2.Enabled := b;
  MenuItem3.Enabled := b;
  MenuItem4.Enabled := b;
  MenuItem5.Enabled := b;
end;

procedure TIntfFm.TabsPickListSelect(Sender: TObject);
begin
  with TPickListCellEditor(Tabs.Editor) do
    Tabs.Objects[Tabs.Col, Tabs.Row] := Items.Objects[ItemIndex];
end;

procedure TIntfFm.TabsSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if Editor is TPickListCellEditor then
    with TPickListCellEditor(Editor) do
      Style := csDropDownList;
end;

procedure TIntfFm.TabsSelection(Sender: TObject; aCol, aRow: Integer);
begin
  SetControlState;
end;

procedure TIntfFm.ToolButton10Click(Sender: TObject);
begin
  Tabs.ExchangeColRow(False, Tabs.Row, Tabs.Row + 1);
  SetControlState;
  FModified := True;
end;

procedure TIntfFm.ToolButton1Click(Sender: TObject);
begin
  Pop.Popup;
end;

procedure TIntfFm.ToolButton2Click(Sender: TObject);
var
  MI: TdxMenuItem;
  S: String;
begin
  MI := TdxMenuItem(Mnu.Selected.Data);
  if MI.Kind = miMenu then
  begin
    S := InputBox(rsRenameSection, rsEnterCaption, MI.Caption);
    if (Trim(S) = '') or (S = MI.Caption) then Exit;
    MI.Caption := S;
    Mnu.Selected.Text:=S;
    FModified := True;
  end;
end;

procedure TIntfFm.ToolButton3Click(Sender: TObject);
var
  MI: TdxMenuItem;
  N: TTreeNode;
  L: TdxMenuItemList;
begin
  if not ConfirmDelete then Exit;
  N := Mnu.Selected;
  if N.Parent <> nil then
    L := TdxMenuItem(N.Parent.Data).Items
  else
    L := FIntf.Menu;
  MI := TdxMenuItem(N.Data);
  L.DeleteItem(MI);
  N.Delete;
  SetControlState;
  FModified := True;
end;

procedure TIntfFm.ToolButton4Click(Sender: TObject);
var
  r: Integer;
begin
  r := Tabs.RowCount;
  Tabs.RowCount := r + 1;
  Tabs.Row := r;
  SetControlState;
  FModified := True;
end;

procedure TIntfFm.ToolButton6Click(Sender: TObject);
begin
  Tabs.DeleteRow(Tabs.Row);
  SetControlState;
  FModified := True;
end;

procedure TIntfFm.ToolButton7Click(Sender: TObject);
var
  N, PrN: TTreeNode;
  L: TdxMenuItemList;
begin
  N := Mnu.Selected;
  PrN := N.GetPrevSibling;
  if N.Parent <> nil then
    L := TdxMenuItem(N.Parent.Data).Items
  else
    L := FIntf.Menu;
  L.Exchange(L.IndexOf(N.Data), L.IndexOf(PrN.Data));
  N.MoveTo(PrN, naInsert);
  SetControlState;
  FModified := True;
end;

procedure TIntfFm.ToolButton8Click(Sender: TObject);
var
  N, NxN: TTreeNode;
  L: TdxMenuItemList;
begin
  N := Mnu.Selected;
  NxN := N.GetNextSibling;
  if N.Parent <> nil then
    L := TdxMenuItem(N.Parent.Data).Items
  else
    L := FIntf.Menu;
  L.Exchange(L.IndexOf(N.Data), L.IndexOf(NxN.Data));
  NxN.MoveTo(N, naInsert);
  SetControlState;
  FModified := True;
end;

procedure TIntfFm.ToolButton9Click(Sender: TObject);
begin
  Tabs.ExchangeColRow(False, Tabs.Row, Tabs.Row - 1);
  SetControlState;
  FModified := True;
end;

procedure TIntfFm.BuildTree;

  procedure _Build(PN: TTreeNode; L: TdxMenuItemList);
  var
    i, ii: Integer;
    MI: TdxMenuItem;
    S: String;
    Fm: TdxForm;
    RD: TReportData;
    N: TTreeNode;
  begin
    for i := 0 to L.Count - 1 do
    begin
      MI := L[i];
      if MI.Kind = miDiv then
      begin
        S := '-------';
        ii := 6;
      end
      else if MI.Kind = miMenu then
      begin
        S := MI.Caption;
        ii := 5;
      end
      else if MI.Kind = miForm then
      begin
        Fm := FormMan.FindForm(MI.Id);
        if Fm = nil then Continue;
        S := GetFullCaption(Fm);
        ii := 7;
      end
      else if MI.Kind = miReport then
      begin
        RD := ReportMan.FindReport(MI.Id);
        if RD = nil then Continue;
        S := RD.Name;
        ii := 8;
      end;
      N := Mnu.Items.AddChild(PN, S);
      N.ImageIndex := ii;
      N.SelectedIndex:=ii;
      N.Data := MI;
      _Build(N, MI.Items)
    end;
  end;

var
  j: Integer;
begin
  Mnu.Items.Clear;
  _Build(nil, FIntf.Menu);
  for j := 0 to Mnu.Items.Count - 1 do
    Mnu.Items[j].Expand(False);
end;

procedure TIntfFm.FillForms;
var
  SL: TStringListUtf8;
  Fm: TdxForm;
  i: Integer;
begin
  SL := TStringListUtf8.Create;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if Fm.PId = 0 then
      SL.AddObject(GetFullCaption(Fm), Fm);
  end;
  SL.Sort;
  Tabs.Columns[0].PickList := SL;
  SL.Free;
  //FormMan.FormsToList(Tabs.Columns[0].PickList);
end;

procedure TIntfFm.FillTabs;
var
  i: Integer;
  Fm: TdxForm;
begin
  Tabs.RowCount := 0;
  for i := 0 to FIntf.Tabs.Count - 1 do
  begin
    Fm := FormMan.FindForm(FIntf.Tabs[i]);
    if Fm <> nil then
    begin
      Tabs.RowCount := i + 1;
      Tabs.Cells[0, i] := GetFullCaption(Fm);
      Tabs.Objects[0, i] := Fm;
    end;
  end;
end;

procedure TIntfFm.SetControlState;
var
  N: TTreeNode;
  MI: TdxMenuItem;
begin
  if PageControl1.PageIndex = 0  then
  begin
    N := Mnu.Selected;
    if N <> nil then
      MI := TdxMenuItem(N.Data);
    ToolButton1.Enabled := Tabs.Columns[0].PickList.Count > 0;
    ToolButton2.Enabled := (N <> nil) and (MI.Kind = miMenu);
    ToolButton3.Enabled := N <> nil;
    ToolButton7.Enabled := (N <> nil) and (N.GetPrevSibling <> nil);
    ToolButton8.Enabled := (N <> nil) and (N.GetNextSibling <> nil);
  end
  else if PageControl1.PageIndex = 1 then
  begin
    ToolButton4.Enabled := Tabs.Columns[0].PickList.Count > 0;
    ToolButton6.Enabled := (Tabs.RowCount > 0);
    ToolButton9.Enabled := Tabs.Row > 0;
    ToolButton10.Enabled := (Tabs.Row >= 0) and (Tabs.Row < Tabs.RowCount - 1);
  end;
end;

procedure TIntfFm.Save;
var
  i: Integer;
  Fm: TdxForm;
begin
  FIntf.Tabs.Clear;
  for i := 0 to Tabs.RowCount - 1 do
  begin
    Fm := TdxForm(Tabs.Objects[0, i]);
    if Fm <> nil then FIntf.Tabs.AddTab(Fm.Id);
  end;
end;

function TIntfFm.ShowForm(aIntf: TdxIntf): Integer;
begin
  FOldIntf := aIntf;
  CopyIntf(aIntf, FIntf);
  Nm.Text := FIntf.Name;
  Dflt.Checked := FIntf.IsDefault;
  BuildTree;
  FillTabs;
  FillForms;
  SetControlState;
  FModified := False;
  Result := ShowModal;
  if Result = mrOk then
  begin
    FIntf.Name := Nm.Text;
    FIntf.IsDefault:=Dflt.Checked;
    Save;
    CopyIntf(FIntf, aIntf);
  end;
end;

end.

