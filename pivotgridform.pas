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

unit PivotGridForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Menus, ButtonPanel, Spin, ColorBox, EditBtn, KGrids,
  MemDS, DB, PivotGrid, dxreports, strconsts, dxctrls, TreeViewEx;

type

  { TPivotGridFm }

  TPivotGridFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DataDelim: TEdit;
    SelectFnt: TEditButton;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    ShowTotalX: TCheckBox;
    ShowTotalY: TCheckBox;
    Flat: TCheckBox;
    Indent: TSpinEdit;
    TotalClr: TColorBox;
    TotalFnt: TEditButton;
    Tree: TTreeViewEx;
    WordWrap: TCheckBox;
    Lines: TComboBox;
    TotalCaption: TEdit;
    FixClr: TColorBox;
    Clr: TColorBox;
    TotalFixClr: TColorBox;
    CornerClr: TColorBox;
    SelectClr: TColorBox;
    FixLineClr: TColorBox;
    LineClr: TColorBox;
    IndicatClr: TColorBox;
    FixFnt: TEditButton;
    Fnt: TEditButton;
    TotalFixFnt: TEditButton;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItem1: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PageControl1: TPageControl;
    PopupMenu1: TPopupMenu;
    ScrollBox1: TScrollBox;
    TotalWidth: TSpinEdit;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ControlChange(Sender: TObject);
    procedure FixFntButtonClick(Sender: TObject);
    procedure FixFntKeyPress(Sender: TObject; var Key: char);
    procedure FntButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure SelectFntButtonClick(Sender: TObject);
    procedure TotalFixFntButtonClick(Sender: TObject);
    procedure TotalFntButtonClick(Sender: TObject);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeSelectionChanged(Sender: TObject);
    //procedure UpdateFieldsCaption;
  private
    { private declarations }
    DS: TMemDataSet;
    Grid: TdxPivotGrid;
    FRD: TReportData;
    FLoaded: Boolean;
    FForm: TdxForm;
    FModified: Boolean;
    procedure RefreshDS;
    function GetCollection(idx: Integer): TFieldCollection;
    function GetField: TFieldItem;
    procedure BuildTree;
    procedure FillControls;
    procedure LoadFromComponent(aGrid: TdxPivotGrid);
    procedure SaveToComponent(aGrid: TdxPivotGrid);
    procedure SetControlState;
    procedure BuildGrid;
  public
    { public declarations }
    procedure ShowForm(aGrid: TdxPivotGrid);
  end;

var
  PivotGridFm: TPivotGridFm;

procedure ShowPivotGridForm(aGrid: TdxPivotGrid);

implementation

uses
  apputils, fontform, reportmanager, pivotfieldform, helpmanager, mydialogs,
  appsettings;

procedure ShowPivotGridForm(aGrid: TdxPivotGrid);
begin
  if PivotGridFm = nil then
  	PivotGridFm := TPivotGridFm.Create(Application);
  PivotGridFm.ShowForm(aGrid);
end;

{$R *.lfm}

{ TPivotGridFm }

procedure TPivotGridFm.FormCreate(Sender: TObject);
begin
  Caption := rsPivotTable;
  TabSheet1.Caption := rsFields;
  TabSheet2.Caption := rsProperties;
  Label5.Caption := rsGrandTotal;
  ShowTotalX.Caption := rsShowRowTotal;
  ShowTotalY.Caption := rsShowColumnTotal;
  Label1.Caption := rsCaption;
  Label2.Caption := rsColumnWidthTotal;

  Label3.Caption := rsColor;
  Label4.Caption := rsHeaders;
  Label5.Caption := rsDataArea;
  Label6.Caption := rsHeaderTotal;
  Label9.Caption := rsTotal;
  Label7.Caption := rsCorner;
  Label8.Caption := rsSelected;
  Label18.Caption := rsIndication;
  Label13.Caption := rsHeaderLines;
  Label14.Caption := rsLines;

  Label9.Caption := rsFont;
  Label10.Caption := rsHeaders;
  Label11.Caption := rsDataArea;
  Label12.Caption := rsHeaderTotal;
  Label20.Caption := rsTotal;

  Label16.Caption := rsOther;
  Label17.Caption := rsLines;
  Flat.Caption := rsFlat;
  WordWrap.Caption := rsWordWrap;

  Label21.Caption := rsDataDelimiter;
  Label22.Caption := rsIndent;
  Label23.Caption := rsSelected;

  MenuItem1.Caption := rsAppend;
  MenuItem2.Caption := rsDelete;
  MenuItem4.Caption := rsMoveUp;
  MenuItem5.Caption := rsMoveDown;
  MenuItem13.Caption := rsProperties;

  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;

  DS := TMemDataSet.Create(nil);

  Grid := TdxPivotGrid.Create(Self);
  Grid.Parent := Self;
  Grid.Align := alClient;
  Grid.SizingStyle:=ssLine;
  Grid.DataSet := DS;
  Grid.BorderSpacing.Top:=4;
  Grid.BorderSpacing.Right := 4;
  Grid.Preview := True;
  Lines.Items.AddStrings([StrConsts.rsNone, StrConsts.rsVert,
    StrConsts.rsHorz, StrConsts.rsAll]);
  Tree.IsWine := AppConfig.IsWine;
end;

procedure TPivotGridFm.ControlChange(Sender: TObject);
begin
  if FLoaded then Exit;
  Grid.ShowGrandTotalX := ShowTotalX.Checked;
  Grid.ShowGrandTotalY := ShowTotalY.Checked;
  Grid.GrandTotalCaption := TotalCaption.Text;
  Grid.GrandTotalWidth:=TotalWidth.Value;
  Grid.Colors.FixedCellBkGnd:=FixClr.Selected;
  Grid.Colors.CellBkGnd:=Clr.Selected;
  Grid.GrandTotalFixedColor:=TotalFixClr.Selected;
  Grid.GrandTotalColor := TotalClr.Selected;
  Grid.CornerColor:=CornerClr.Selected;
  Grid.Colors.FocusedCellBkGnd:=SelectClr.Selected;
  Grid.Colors.FixedCellIndication:=IndicatClr.Selected;
  Grid.Colors.FixedCellLines:=FixLineClr.Selected;
  Grid.Colors.CellLines:=LineClr.Selected;
  Grid.FixedFont := FixFnt.Font;
  Grid.Font := Fnt.Font;
  Grid.SelectedFont := SelectFnt.Font;
  Grid.GrandTotalFixedFont := TotalFixFnt.Font;
  Grid.GrandTotalFont := TotalFnt.Font;
  Grid.Options:=Grid.Options - [goFixedVertLine, goFixedHorzLine,
      goVertLine, goHorzLine];
  case Lines.ItemIndex of
    1: Grid.Options:=Grid.Options + [goFixedVertLine, goVertLine];
    2: Grid.Options:=Grid.Options + [goFixedHorzLine, goHorzLine];
    3: Grid.Options:=Grid.Options + [goFixedVertLine, goFixedHorzLine,
      goVertLine, goHorzLine];
  end;
  Grid.Flat:=Flat.Checked;
  Grid.WordWrap:=WordWrap.Checked;
  Grid.DataDelimiter:=DataDelim.Text;
  Grid.Indent := Indent.Value;
  BuildGrid;
  FModified := True;
end;

procedure TPivotGridFm.FixFntButtonClick(Sender: TObject);
begin
  if ShowFontForm(FixFnt.Font, Grid.Font) = mrOk then
  begin
    FixFnt.Text := FixFnt.Font.Name;
    ControlChange(Sender);
  end;
end;

procedure TPivotGridFm.FixFntKeyPress(Sender: TObject; var Key: char);
begin
  Key := #0;
end;

procedure TPivotGridFm.FntButtonClick(Sender: TObject);
begin
  if ShowFontForm(Fnt.Font, FForm.Font) = mrOk then
  begin
    Fnt.Text := FixFnt.Font.Name;
    ControlChange(Sender);
  end;
end;

procedure TPivotGridFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
  begin
    if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TPivotGridFm.FormDestroy(Sender: TObject);
begin
  DS.Free;
end;

procedure TPivotGridFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('pivottable');
end;

procedure TPivotGridFm.MenuItem13Click(Sender: TObject);
var
  N: TTreeNode;
  Col: TFieldCollection;
begin
  N := Tree.Selected;
  Col := GetCollection(N.Parent.Index);
  if ShowPivotFieldForm(N.Text, Col[N.Index], Grid) = mrOk then
  begin
    BuildGrid;
    FModified := True;
  end;
end;

function ShowSelectQFieldForm(RD: TReportData): Integer;
var
  Col: TRpGridColumn;
begin
  Result := -1;
  with TSelectQFieldForm.CreateNew(nil) do
  try
    Caption := rsSelectQueryField;
    if ShowForm(RD) = mrOk then
    begin
      Col := RD.Grid.FindColumnByTitle(FieldName);
      Result := RD.Grid.FindColumnIndex(Col);
    end;
  finally
    Free;
  end;
end;

procedure TPivotGridFm.MenuItem1Click(Sender: TObject);
var
  i, FieldIndex: Integer;
  N: TTreeNode;
  FL: TFieldCollection;
  FI: TFieldItem;
  Col: TRpGridColumn;
  //pF: PRpField;
  //pCF: PRpCalcField;
  Tp: TRpFieldType;
begin
  i := ShowSelectQFieldForm(FRD);
  if (i >= 0) and (Tree.Items.FindNodeWithData(Pointer(i+1)) = nil) then
  begin
    N := Tree.Selected;
    if N.Parent <> nil then N := N.Parent;
    Col := FRD.Grid.Columns[i];
    Tree.Items.AddChildObject(N, Col.Caption, Pointer(i+1));
    N.Expand(False);
    FL := GetCollection(N.Index);
    FI := FL.AddField;
    FI.FieldName:=Col.FieldNameDS;
    FI.Caption:=Col.Caption;
    FieldIndex := FRD.IndexOfNameDS(Col.FieldNameDS);
    Tp := FRD.GetFieldType(FieldIndex);
    {if not Col.IsCalcField then
    begin
      pF := FRD.FindField(Col.FieldId);
      Tp := GetRealRpFieldType(FRD, pF);
    end
    else
    begin
      pCF := FRD.CalcFields.FindField(Col.FieldId);
      Tp:=pCF^.Tp;
    end;   }
    if Tp in [flObject, flBool, flCounter, flRecId] then Tp := flNumber;
    FI.DataType := Tp;
    RefreshDS;
    BuildGrid;
    FModified := True;
  end;
end;

procedure TPivotGridFm.MenuItem2Click(Sender: TObject);
var
  i: Integer;
  N: TTreeNode;
  FL: TFieldCollection;
  S: String;
begin
  N := Tree.Selected;
  i := Integer(N.Data) - 1;
  FL := GetCollection(N.Parent.Index);
  S := FRD.Grid.Columns[i].FieldNameDS;
  FL.FindFieldByFieldName(S).Free;
  N.Delete;
  BuildGrid;
  FModified := True;
end;

procedure TPivotGridFm.MenuItem4Click(Sender: TObject);
var
  N: TTreeNode;
  FL: TFieldCollection;
begin
  N := Tree.Selected;
  FL := GetCollection(N.Parent.Index);
  FL.Exchange(N.Index, N.GetPrev.Index);
  Tree.Selected.MoveTo(N.GetPrev, naInsert);
  BuildGrid;
  FModified := True;
end;

procedure TPivotGridFm.MenuItem5Click(Sender: TObject);
var
  N: TTreeNode;
  FL: TFieldCollection;
begin
  N := Tree.Selected;
  FL := GetCollection(N.Parent.Index);
  FL.Exchange(N.Index, N.GetNext.Index);
  Tree.Selected.MoveTo(N.GetNext, naInsertBehind);
  BuildGrid;
  FModified := True;
end;

procedure TPivotGridFm.PopupMenu1Popup(Sender: TObject);
begin
  SetControlState;
end;

procedure TPivotGridFm.SelectFntButtonClick(Sender: TObject);
begin
  if ShowFontForm(SelectFnt.Font, Grid.Font) = mrOk then
  begin
    SelectFnt.Text := SelectFnt.Font.Name;
    ControlChange(Sender);
  end;
end;

procedure TPivotGridFm.TotalFixFntButtonClick(Sender: TObject);
begin
  if ShowFontForm(TotalFixFnt.Font, Grid.Font) = mrOk then
  begin
    TotalFixFnt.Text := TotalFixFnt.Font.Name;
    ControlChange(Sender);
  end;
end;

procedure TPivotGridFm.TotalFntButtonClick(Sender: TObject);
begin
  if ShowFontForm(TotalFnt.Font, Grid.Font) = mrOk then
  begin
    TotalFnt.Text := TotalFnt.Font.Name;
    ControlChange(Sender);
  end;
end;

procedure TPivotGridFm.TreeDblClick(Sender: TObject);
var
  N: TTreeNode;
begin
  N := Tree.Selected;
  if (N <> nil) and (N.Parent <> nil) then MenuItem13.Click;
end;

procedure TPivotGridFm.TreeSelectionChanged(Sender: TObject);
begin
  SetControlState;
end;

procedure TPivotGridFm.RefreshDS;
var
  i, idx: Integer;
  N: TTreeNode;
  F: TField;
  Col: TRpGridColumn;
begin
  DS.Close;
  DS.FieldDefs.Clear;
  for i := 0 to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    if N.Parent = nil then Continue;
    idx := Integer(N.Data)-1;
    //ShowMessage(FRD.Grid.Columns[idx].Caption);
    DS.FieldDefs.Add(FRD.Grid.Columns[idx].FieldNameDS, ftString, 200);
  end;
  DS.CreateTable;
  DS.Open;
  DS.Insert;
  for i := 0 to DS.Fields.Count - 1 do
  begin
    F := DS.Fields[i];
    Col := FRD.Grid.FindColumnByFieldName(F.FieldName);
    F.AsString := Col.Caption;
  end;
  DS.Post;
end;

function TPivotGridFm.GetCollection(idx: Integer): TFieldCollection;
begin
  case idx of
    0: Result := Grid.RowFields;
    1: Result := Grid.ColFields;
    2: Result := Grid.DataFields;
  end;
end;

function TPivotGridFm.GetField: TFieldItem;
var
  C, R, i: Integer;
begin
  Result := nil;
  C := Grid.Col; R := Grid.Row;
  if (C < Grid.RowFields.Count) and (R >= Grid.ColFields.Count) and
    (Grid.Objects[C, R] <> nil) then
  begin
    i := PtrInt(Grid.Objects[C, R]) - 2;
    if i >= 0 then
      Exit(Grid.RowFields[i]);
  end
  else if (C = Grid.RowFields.Count) and (R >= Grid.ColFields.Count) then
  begin
    Exit(TFieldItem(Grid.Objects[C, R]));
  end
  else if (C > Grid.RowFields.Count) and (R < Grid.ColFields.Count) then
  begin
    i := PtrInt(Grid.Objects[C, R]) - 2;
    if i >= 0 then
      Exit(Grid.ColFields[i])
    else
      Exit(Grid.ColFields[R]);
  end;
end;

procedure TPivotGridFm.BuildTree;

  procedure _Build(N: TTreeNode; FL: TFieldCollection);
  var
    i, m: Integer;
    S: String;
    Col: TRpGridColumn;
  begin
    for i := 0 to FL.Count - 1 do
    begin
      S := FL[i].FieldName;
      Col := FRD.Grid.FindColumnByFieldName(S);
      if Col <> nil then
      begin
        m := FRD.Grid.FindColumnIndex(Col);
        Tree.Items.AddChildObject(N, Col.Caption, Pointer(m+1));
      end;
    end;
    N.Expand(False);
  end;

begin
  Tree.Items.Clear;
  _Build(Tree.Items.Add(nil, rsRows), Grid.RowFields);
  _Build(Tree.Items.Add(nil, rsColumns), Grid.ColFields);
  _Build(Tree.Items.Add(nil, rsDataArea), Grid.DataFields);
end;

procedure TPivotGridFm.FillControls;
begin
  FLoaded := True;
  ShowTotalX.Checked := Grid.ShowGrandTotalX;
  ShowTotalY.Checked := Grid.ShowGrandTotalY;
  TotalCaption.Text := Grid.GrandTotalCaption;
  TotalWidth.Value := Grid.GrandTotalWidth;
  FixClr.Selected := Grid.Colors.FixedCellBkGnd;
  Clr.Selected := Grid.Colors.CellBkGnd;
  TotalFixClr.Selected := Grid.GrandTotalFixedColor;
  TotalClr.Selected := Grid.GrandTotalColor;
  CornerClr.Selected := Grid.CornerColor;
  SelectClr.Selected := Grid.Colors.FocusedCellBkGnd;
  IndicatClr.Selected := Grid.Colors.FixedCellIndication;
  FixLineClr.Selected := Grid.Colors.FixedCellLines;
  LineClr.Selected := Grid.Colors.CellLines;
  FixFnt.Font := Grid.FixedFont;
  FixFnt.Text := Grid.FixedFont.Name;
  Fnt.Font := Grid.Font;
  Fnt.Text := Grid.Font.Name;
  SelectFnt.Font := Grid.SelectedFont;
  SelectFnt.Text := Grid.Selectedfont.Name;
  TotalFixFnt.Font := Grid.GrandTotalFixedFont;
  TotalFixFnt.Text := Grid.GrandTotalFixedFont.Name;
  TotalFnt.Font := Grid.GrandTotalFont;
  TotalFnt.Text := Grid.GrandTotalFont.Name;
  if (goVertLine in Grid.Options) and (goHorzLine in Grid.Options) then Lines.ItemIndex := 3
  else if goHorzLine in Grid.Options then Lines.ItemIndex := 2
  else if goVertLine in Grid.Options then Lines.ItemIndex := 1
  else Lines.ItemIndex := 0;
  Flat.Checked := Grid.Flat;
  WordWrap.Checked := Grid.WordWrap;
  DataDelim.Text := Grid.DataDelimiter;
  Indent.Value := Grid.Indent;
  FLoaded := False;
  BuildGrid;
end;

procedure TPivotGridFm.LoadFromComponent(aGrid: TdxPivotGrid);
begin
  Grid.Assign(aGrid);
  Grid.Options := Grid.Options - [goRowSizing, goColSizing];
  BuildTree;
  RefreshDS;
  BuildGrid;
  FillControls;
end;

procedure TPivotGridFm.SaveToComponent(aGrid: TdxPivotGrid);
begin
  aGrid.Assign(Grid);
  aGrid.Options := aGrid.Options + [goRowSizing, goColSizing];
end;

procedure TPivotGridFm.SetControlState;
var
  N: TTreeNode;
  b: Boolean;
begin
  N := Tree.Selected;
  b := (N <> nil) and (N.Parent <> nil);
  MenuItem1.Enabled := N <> nil;
  MenuItem2.Enabled := b;
  MenuItem4.Enabled := b and (N.GetPrevSibling <> nil);
  MenuItem5.Enabled := b and (N.GetNextSibling <> nil);
  MenuItem13.Enabled := b;
end;

procedure TPivotGridFm.BuildGrid;
var
  R, C: Integer;
begin
  R := Grid.Row; C := Grid.Col;
  Grid.Build;
  if (R < Grid.RowCount) and (R >= Grid.FixedRows) then Grid.Row := R;
  if (C < Grid.ColCount) and (C >= Grid.FixedCols) then Grid.Col := C;
end;

procedure TPivotGridFm.ShowForm(aGrid: TdxPivotGrid);
begin
  FForm := TdxForm(aGrid.Owner);
  FRD := ReportMan.FindReport(aGrid.Id);
  if FRD = nil then Exit;
  if FRD.IsEmpty then
  begin
    ErrMsg(rsSourceNotSel);
    Exit;
  end;
  LoadFromComponent(aGrid);
  FModified := False;
  if ShowModal = mrOk then
    SaveToComponent(aGrid);
end;

end.

