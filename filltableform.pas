unit FillTableForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ButtonPanel, Menus, EditBtn, Buttons, dxctrls, strconsts, DialogGrid,
  LclType;

type

  { TFillTableFm }

  TFillTableFm = class(TForm)
    BitBtn1: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Grid: TDialogGrid;
    SrcTbl: TComboBox;
    DestTbl: TComboBox;
    Filter: TEditButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure DestTblChange(Sender: TObject);
    procedure FilterButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure HelpButtonClick(Sender: TObject);
    procedure SrcTblChange(Sender: TObject);
  private
    { private declarations }
    FCtrl: TdxLookupComboBox;
    FFm, FSFm, FDFm: TdxForm;
    procedure FillSrcTbl;
    procedure FillDestTbl;
    procedure FillSrcFields;
    procedure FillDestFields;
    procedure FillGrid;
    procedure SaveGrid;
    function CheckGrid: Boolean;
  public
    { public declarations }
    function ShowForm(C: TdxLookupComboBox): Integer;
  end;

var
  FillTableFm: TFillTableFm;

function ShowFillTableForm(C: TdxLookupComboBox): Integer;

implementation

uses
  formmanager, apputils, exprform, helpmanager, mytypes;

function ShowFillTableForm(C: TdxLookupComboBox): Integer;
begin
  if FillTableFm = nil then
  	FillTableFm := TFillTableFm.Create(Application);
  Result := FillTableFm.ShowForm(C);
end;

{$R *.lfm}

{ TFillTableFm }

procedure TFillTableFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult <> mrOk then Exit;
  if (SrcTbl.ItemIndex < 0) and (DestTbl.ItemIndex < 0) and (Filter.Text = '') and
    (Grid.RowCount = 1) then Exit;
  CanClose := False;
  if SrcTbl.ItemIndex < 0 then
  begin
    ErrMsg(rsSrcTblNotSel);
    SrcTbl.SetFocus;
  end
  else if DestTbl.ItemIndex < 0 then
  begin
    ErrMsg(rsDestTblNotSel);
    DestTbl.SetFocus;
  end
  else
    CanClose := CheckGrid;
end;

procedure TFillTableFm.FormCreate(Sender: TObject);
begin
  Caption := rsFillTable;
  BitBtn1.Caption:=rsReset;
  Label2.Caption := rsSourceTable;
  Label3.Caption := rsDestTable;
  Label4.Caption := rsFilter;
  Grid.Columns[0].Title.Caption := rsSource;
  Grid.Columns[1].Title.Caption := rsDestination;
  CheckBox1.Caption:=rsPromptBeforeFill;
  CheckBox2.Caption := rsClearTableBeforeFill;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TFillTableFm.FormShow(Sender: TObject);
begin
  SrcTbl.SetFocus;
end;

procedure TFillTableFm.GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
var
  r: Integer;
begin
  case Cmd of
    dgcAppend:
      begin
        r := Grid.RowCount;
        Grid.RowCount := r + 1;
        Grid.Row := r;
      end;
    dgcDelete:
      if ConfirmDelete then
        Grid.DeleteRow(Grid.Row);
  end;
end;

procedure TFillTableFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('filltable');
end;

procedure TFillTableFm.SrcTblChange(Sender: TObject);
var
  i: Integer;
  Fm: TdxForm;
begin
  Fm := TdxForm(SrcTbl.Items.Objects[SrcTbl.ItemIndex]);
  if Fm = FSFm then Exit;
  FSFm := Fm;
  for i := 1 to Grid.RowCount - 1 do
  begin
    Grid.Cells[0, i] := '';
    Grid.Objects[0, i] := nil;
  end;
  FillSrcFields;
end;

procedure TFillTableFm.FilterButtonClick(Sender: TObject);
var
  S: TCaption;
  Fm: TdxForm;
begin
  if SrcTbl.ItemIndex < 0 then
  begin
    ErrMsg(rsSrcTblNotSel);
    SrcTbl.SetFocus;
    Exit;
  end;
  Fm := TdxForm(SrcTbl.Items.Objects[SrcTbl.ItemIndex]);
  S := Filter.Text;
  if ShowExprForm(etSourceTableFilter, nil, S, Fm, nil, nil, nil) = mrOk then
    Filter.Text := S;
end;

procedure TFillTableFm.DestTblChange(Sender: TObject);
var
  i: Integer;
  Fm: TdxForm;
begin
  Fm := TdxForm(DestTbl.Items.Objects[DestTbl.ItemIndex]);
  if Fm = FDFm then Exit;
  FDFm := Fm;
  for i := 1 to Grid.RowCount - 1 do
  begin
    Grid.Cells[1, i] := '';
    Grid.Objects[1, i] := nil;
  end;
  FillDestFields;
end;

procedure TFillTableFm.BitBtn1Click(Sender: TObject);
begin
  if MessageDlg(rsWarning, rsClearSettingsMsg, mtWarning, [mbYes, mbNo], 0) =
    mrNo then Exit;
  SrcTbl.ItemIndex := -1;
  DestTbl.ItemIndex := -1;
  Filter.Text := '';
  Grid.RowCount := 1;
  CheckBox1.Checked := False;
  CheckBox2.Checked := False;
  FSFm := nil;
  FDFm := nil;
  Grid.Columns[0].PickList.Clear;
  Grid.Columns[1].PickList.Clear;
end;

procedure TFillTableFm.FillSrcTbl;
var
  Fm, F: TdxForm;
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  SrcTbl.Clear;
  if FCtrl.SourceTId = 0 then Exit;
  SL := TStringListUtf8.Create;
  Fm := FormMan.FindForm(FCtrl.SourceTId);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxGrid then
    begin
      F := FormMan.FindForm(TdxGrid(C).Id);
      SL.AddObject(F.FormCaption, F);
    end;
  end;
  SL.Sort;
  SrcTbl.Items := SL;
  SL.Free;
end;

procedure TFillTableFm.FillDestTbl;
var
  F: TdxForm;
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  DestTbl.Clear;
  SL := TStringListUtf8.Create;
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TdxGrid then
    begin
      F := FormMan.FindForm(TdxGrid(C).Id);
      SL.AddObject(F.FormCaption, F);
    end;
  end;
  SL.Sort;
  DestTbl.Items := SL;
  SL.Free;
end;

procedure TFillTableFm.FillSrcFields;
var
  Fm: TdxForm;
  i: Integer;
  SL: TStringListUtf8;
  C: TComponent;
begin
  SL := TStringListUtf8.Create;
  Fm := FSFm;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if IsField(C) then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Grid.Columns[0].PickList := SL;
  SL.Free;
end;

procedure TFillTableFm.FillDestFields;
var
  Fm: TdxForm;
  i: Integer;
  SL: TStringListUtf8;
  C: TComponent;
begin
  SL := TStringListUtf8.Create;
  Fm := FDFm;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if IsField(C) then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Grid.Columns[1].PickList := SL;
  SL.Free;
end;

procedure TFillTableFm.FillGrid;
var
  SL: TStrings;
  i, r: Integer;
  SFm, DFm: TdxForm;
  SC, DC: TComponent;
begin
  Grid.RowCount := 1;
  SL := FCtrl.FieldsTables;
  SFm := nil; DFm := nil;
  if SrcTbl.ItemIndex >= 0 then
    SFm := TdxForm(SrcTbl.Items.Objects[SrcTbl.ItemIndex]);
  if DestTbl.ItemIndex >= 0 then
    DFm := TdxForm(DestTbl.Items.Objects[DestTbl.ItemIndex]);
  for i := 0 to SL.Count - 1 do
  begin
    r := i + 1;
    Grid.RowCount := r + 1;
    if (SFm <> nil) and (SL.Names[i] > '') then
    begin
      SC := FindById(SFm, StrToInt(SL.Names[i]));
      Grid.Cells[0, r] := GetFieldName(SC);
      Grid.Objects[0, r] := SC;
    end;
    if (DFm <> nil) and (SL.ValueFromIndex[i] > '') then
    begin
      DC := FindById(DFm, StrToInt(SL.ValueFromIndex[i]));
      Grid.Cells[1, r] := GetFieldName(DC);
      Grid.Objects[1, r] := DC;
    end;
  end;
end;

procedure TFillTableFm.SaveGrid;
var
  i: Integer;
  SC, DC: TComponent;
  S: String;
begin
  FCtrl.FieldsTables.Clear;
  for i := 1 to Grid.RowCount - 1 do
  begin
    SC := TComponent(Grid.Objects[0, i]);
    DC := TComponent(Grid.Objects[1, i]);
    S := IntToStr(GetId(SC)) + '=' + IntToStr(GetId(DC));
    FCtrl.FieldsTables.Add(S);
  end;
end;

function TFillTableFm.CheckGrid: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Grid.RowCount - 1 do
  begin
    if Grid.Objects[0, i] = nil then
    begin
      ErrMsg(rsSrcFldNotSel);
      Grid.SetFocus;
      Grid.Col := 0; Grid.Row := i;
      Exit;
    end
    else if Grid.Objects[1, i] = nil then
    begin
      ErrMsg(rsDestFldNotSel);
      Grid.SetFocus;
      Grid.Col := 1; Grid.Row := i;
      Exit;
    end
    else if not CheckCompatibles(TComponent(Grid.Objects[0, i]),
      TComponent(Grid.Objects[1, i])) then
    begin
      ErrMsg(rsIncompatibleFields);
      Grid.SetFocus;
      Grid.Row := i;
      Exit;
    end;
  end;
  Result := True;
end;

function TFillTableFm.ShowForm(C: TdxLookupComboBox): Integer;
var
  Fm: TdxForm;
begin
  FCtrl := C;
  FSFm := nil;
  FDFm := nil;
  Grid.Columns[0].PickList.Clear;
  Grid.Columns[1].PickList.Clear;

  FFm := TdxForm(C.Owner);
  FillSrcTbl;
  FillDestTbl;
  if C.SourceTable > 0 then
  begin
    Fm := FormMan.FindForm(C.SourceTable);
    FSFm := Fm;
    with SrcTbl do
      ItemIndex := Items.IndexOfObject(Fm);
  end;
  if C.DestTable > 0 then
  begin
    Fm := FormMan.FindForm(C.DestTable);
    FDFm := Fm;
    with DestTbl do
      ItemIndex := Items.IndexOfObject(Fm);
  end;
  if SrcTbl.ItemIndex >= 0 then
    FillSrcFields;
  if DestTbl.ItemIndex >= 0 then
    FillDestFields;
  FillGrid;
  Filter.Text:=C.FillFilter;
  CheckBox1.Checked := C.PromptFillTable;
  CheckBox2.Checked := C.ClearTableBeforeFill;
  Result := ShowModal;
  if Result = mrOk then
  begin
    with SrcTbl do
      if ItemIndex >= 0 then
        C.SourceTable:=FSFm.Id
      else
        C.SourceTable := 0;
    with DestTbl do
      if ItemIndex >= 0 then
        C.DestTable:=FDFm.Id
      else
        C.DestTable := 0;
    C.FillFilter := Filter.Text;
    SaveGrid;
    C.PromptFillTable:=CheckBox1.Checked;
    C.ClearTableBeforeFill:=CheckBox2.Checked;
  end;
end;

end.

