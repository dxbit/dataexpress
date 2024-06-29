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

unit ReportForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, DXReports, DxCtrls, strconsts, editbtn, StdCtrls, Menus,
  ExtCtrls, mydialogs, LclType, Spin, ActionControls, DialogGrid;

type

  { TReportFm }

  TReportFm = class(TForm)
    Bevel1: TBevel;
    ButtonPanel1: TButtonPanel;
    Grid: TStringGridEx;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem9: TMenuItem;
    FieldNameMnu: TPopupMenu;
    ShowFirstRecords: TCheckBox;
    DateFl: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    Panel1: TPanel;
    Period: TComboBox;
    PopupMenu1: TPopupMenu;
    FirstRecordCount: TSpinEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    PickUpBn: TToolButton;
    ToolButton10: TToolButton;
    SqlBn: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure BtnClick(Sender: TObject);
    procedure DateFlChange(Sender: TObject);
    procedure DateFlDropDown(Sender: TObject);
    procedure FieldNameMnuClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCellProcess(Sender: TObject; aCol, aRow: Integer;
      processType: TCellProcessType; var aValue: string);
    procedure GridCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridEnter(Sender: TObject);
    procedure GridExit(Sender: TObject);
    procedure GridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridPickListSelect(Sender: TObject);
    procedure GridResize(Sender: TObject);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure GridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure PeriodChange(Sender: TObject);
    procedure PickUpBnClick(Sender: TObject);
    procedure ShowFirstRecordsChange(Sender: TObject);
    procedure SqlBnClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
  private
    { private declarations }
    FRD: TReportData;
    FFm: TdxForm;
    FExprEd: TExprCellEditor;
    FFieldNameEd: TActionText;
    FMaxFId: Integer;
    FSelFieldFm: TSelectFieldForm;
    FQGrid: TdxQueryGrid;
    FOldFieldNames: TStringList;
    FInDesigner: Boolean;
    FModified: Boolean;
    procedure ExprEditingDone(Sender: TObject);
    procedure FieldNameEditingDone(Sender: TObject);
    function FindFirstCell(r: Integer): Integer;
    function GetFirstFieldType(r: Integer): TRpFieldType;
    procedure Load;
    procedure PickListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure Save;
    procedure SelFieldFmPickUp(Sender: TObject; const AFieldName: String;
      AComponent: TComponent);
    procedure SetControlState;
    procedure FillForms(L: TStrings);
    procedure FillTables(L: TStrings);
    procedure FillIncOut(L: TStrings);
    procedure FillFuncs(L: TStrings);
    procedure FillDateFl;
    function FindDateFieldIndex(const S: String): Integer;
    function Validate: Boolean;
    procedure DoSelectField(APickUp: Boolean);
    function CanOldSqlFieldsDeleted: Boolean;
    function ProcessRenameFieldsInActions: Boolean;
    procedure SetModified;
    procedure ResetModified;
    procedure AddFieldRow;
    procedure PasteField(const AFieldName: String; AComponent: TComponent);
    function FindRpFieldByFieldNameDS(const FieldNameDS: String): Integer;
    function FindRpFieldByName(const FieldName: String): Integer;
    procedure DeleteOldFieldsReferences;
  public
    { public declarations }
    function ShowForm(RD: TReportData; aForm: TdxForm; QGrid: TdxQueryGrid;
      InDesigner: Boolean; SelSource: Integer = 0): Integer;
  end;

var
  ReportFm: TReportFm;

function ShowReportForm(RD: TReportData; aForm: TdxForm; QGrid: TdxQueryGrid;
  InDesigner: Boolean; SelSource: Integer = 0): Integer;

implementation

uses
  formmanager, apputils, exprform, LazUtf8, helpmanager, mainform, mytypes,
  sqlform, dxfiles, dximages;

{$R *.lfm}

function ShowReportForm(RD: TReportData; aForm: TdxForm; QGrid: TdxQueryGrid;
  InDesigner: Boolean; SelSource: Integer): Integer;
var
  BeginSqlMode, OldSqlMode: Boolean;
begin
  BeginSqlMode := RD.SqlMode;
  while True do
  begin
  	if (ReportFm = nil) and not RD.SqlMode then
    	ReportFm := TReportFm.Create(Application)
    else if (SqlModeFm = nil) and RD.SqlMode then
      SqlModeFm := TSqlFm.Create(Application);

    OldSqlMode := RD.SqlMode;
    if RD.SqlMode then
      Result :=  SqlModeFm.ShowSqlModeForm(RD, QGrid)
    else
      Result := ReportFm.ShowForm(RD, aForm, QGrid, InDesigner, SelSource);
    if RD.SqlMode = OldSqlMode then Break;
  end;

  if (Result = mrCancel) and (BeginSqlMode <> RD.SqlMode) then
    RD.SqlMode := BeginSqlMode;

  //Result := ReportFm.ShowForm(RD, aForm, QGrid, InDesigner, SelSource);
end;

{ TReportFm }

function ExtractFieldName(S: String): String;
begin
  if S = '' then Exit('');
  if Copy(S, 1, 1) = '!' then Delete(S, 1, 1);
  Result := StringReplace(S, '|', '_', [rfReplaceAll]);
end;

procedure TReportFm.DoSelectField(APickUp: Boolean);
var
  Fm, Tbl: TdxForm;
  //C: TComponent;
  S: String;
begin
  Fm := TdxForm(Grid.Objects[Grid.Col, 1]);
  Tbl := TdxForm(Grid.Objects[Grid.Col, 2]);
  if (Fm = nil) and (Tbl = nil) then Exit;
  with FSelFieldFm do
  begin
    //C := TComponent(Grid.Objects[Grid.Col, Grid.Row]);
    S := Grid.Cells[Grid.Col, Grid.Row];
    PickUpMode := APickUp;
    if ShowForm2(Tbl, Fm, S) = mrOk then
      PasteField(FieldName, Component);
  end;
end;

function TReportFm.CanOldSqlFieldsDeleted: Boolean;
var
  i, r: Integer;
  SqlF: TSQLField;
begin
  Result := True;
  for i := 0 to FRD.SqlFields.Count - 1 do
  begin
    SqlF := FRD.SqlFields[i];
    r := FindRpFieldByName(SqlF.Name);
    if (r < 0) and CheckExistsInActions(FRD, renRpField, SqlF.Name, '') then Exit(False);
    {if (FRD.Totals.FindTotal(SqlF.FieldNameDS) <> nil) and (FindRpFieldByFieldNameDS(SqlF.FieldNameDS) < 0) then
    begin
      ErrMsgFmt(rsCannotDeleteTotalField, [SqlF.Name]);
      Exit(False);
    end; }
  end;
end;

function TReportFm.ProcessRenameFieldsInActions: Boolean;
var
  i, idx: Integer;
  FieldName: String;
begin
  Result := True;
  // Если сняли флажок "Видимое"
  if FRD.Kind = rkReport then
    for i := 5 to Grid.RowCount - 1 do
    begin
      if Grid.Cells[Grid.ColCount - 2, i] = '0' then
      begin
        idx := FOldFieldNames.IndexOfObject(Grid.Objects[0, i]);
        if idx >= 0 then
        begin
          Grid.Col := Grid.ColCount - 2;
          Grid.Row := i;
          if CheckExistsInActions(FRD, renRpField, FOldFieldNames[idx],
            LineEnding + rsCantHideRpFieldMsg) then Exit(False);
        end;
      end;
    end;
  for i := 5 to Grid.RowCount - 1 do
  begin
    FieldName := Grid.Cells[Grid.ColCount - 4, i];
    idx := FOldFieldNames.IndexOfObject(Grid.Objects[0, i]);
    if (idx >= 0) and (FOldFieldNames[idx] <> FieldName) then
    begin
      if FInDesigner then
        RenameInActions(FRD, renRpField, FOldFieldNames[idx], FieldName)
      else
      begin
        Grid.Col := Grid.ColCount - 4;
        Grid.Row := i;
        if CheckExistsInActions(FRD, renRpField, FOldFieldNames[idx], LineEnding +
          rsCantRenameRpFieldMsg) then Exit(False);
      end;
    end;
  end;
end;

procedure TReportFm.SetModified;
begin
  FModified := True;
end;

procedure TReportFm.ResetModified;
begin
  FModified := False;
end;

procedure TReportFm.AddFieldRow;
var
  i: Integer;
begin
  i := Grid.RowCount;
  Grid.RowCount := i + 1;
  Grid.Row := i;
  Inc(FMaxFId);
  Grid.Objects[0, i] := TObject(PtrInt(FMaxFId));
  Grid.Cells[Grid.ColCount - 2, i] := '1';
  Grid.Cells[Grid.ColCount - 1, i] := '0';
  SetControlState;
  SetModified;
end;

procedure TReportFm.PasteField(const AFieldName: String; AComponent: TComponent
  );
begin
  Grid.Cells[Grid.Col, Grid.Row] := AFieldName;
  Grid.Objects[Grid.Col, Grid.Row] := AComponent;
  if Grid.Cells[Grid.ColCount - 4, Grid.Row] = '' then
    Grid.Cells[Grid.ColCount - 4, Grid.Row] := ExtractFieldName(AFieldName);
  SetModified;
end;

function TReportFm.FindRpFieldByFieldNameDS(const FieldNameDS: String): Integer;
var
  i, c: Integer;
  RpFlDSName: String;
begin
  Result := -1;
  c := Grid.Columns.Count;
  for i := 5 to Grid.RowCount - 1 do
  begin
    RpFlDSName := 'f' + IntToStr(PtrInt(Grid.Objects[0, i]));
    // Если SQL-имя поля совпадает и поле видимо, возвращаем имя поля
    if (CompareText(FieldNameDS, RpFlDSName) = 0) and (Grid.Cells[c - 1, i] = '1') then
      Exit(i);
  end;
end;

function TReportFm.FindRpFieldByName(const FieldName: String): Integer;
var
  i, c: Integer;
  FlNm: String;
begin
  Result := -1;
  c := Grid.Columns.Count;
  for i := 5 to Grid.RowCount - 1 do
  begin
    FlNm := Grid.Cells[c - 3, i];
    // Если имя поля совпадает и поле видимо, возвращаем имя поля
    if (Utf8CompareText(FieldName, FlNm) = 0) and (Grid.Cells[c - 1, i] = '1') then
      Exit(i);
  end;
end;

procedure TReportFm.DeleteOldFieldsReferences;
var
  i: Integer;
  FlNm: String;
begin
  if FFm = nil then Exit;

  for i := 0 to FRD.SqlFields.Count - 1 do
    DeleteLCbxListSourceField(FFm, FRD.Id, FRD.SqlFields[i].FieldNameDS);

  for i := 0 to FOldFieldNames.Count - 1 do
  begin
    FlNm :='f' + IntToStr(Integer(FOldFieldNames.Objects[i]));
    if FindRpFieldByFieldNameDS(FlNm) < 0 then
      DeleteLCbxListSourceField(FFm, FRD.Id, FlNm);
  end;
end;

procedure TReportFm.GridSelection(Sender: TObject; aCol, aRow: Integer);
var
  L: TStrings;
begin
  L := Grid.Columns[aCol - 1].PickList;
  L.Clear;
  if aCol in [1..Grid.ColCount - 5] then
  begin
    if aRow = 1 then
      FillForms(L)
    else if aRow = 2 then
      FillTables(L)
    else if aRow = 4 then
      FillIncOut(L)
  end
  else if (aCol = Grid.ColCount - 3) and (aRow >= 5) then
    FillFuncs(L);
  SetControlState;
end;

procedure TReportFm.GridValidateEntry(sender: TObject; aCol, aRow: Integer;
  const OldValue: string; var NewValue: String);
begin
  if (aCol = Grid.ColCount - 4) and (aRow >= 5) then
    NewValue := RemoveNonPrintableChars(NewValue)
  else if (aCol > 0) and (aCol < Grid.ColCount - 4) and (aRow = 3) then
    NewValue := RemoveNonPrintableChars(NewValue, True);
end;

procedure TReportFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('selection');
end;

procedure TReportFm.MenuItem1Click(Sender: TObject);
var
  C: TGridColumn;
begin
  C := Grid.Columns.Add;
  C.Index:=Grid.ColCount - 6;
  C.Title.Caption := Format(rsSourceNum, [Grid.ColCount - 5]);
  C.Width := ScaleToScreen(120);
  SetControlState;
  SetModified;
end;

procedure TReportFm.MenuItem2Click(Sender: TObject);
begin
  if not ConfirmDelete then Exit;
  Grid.DeleteCol(Grid.Col);
  SetControlState;
  SetModified;
end;

procedure TReportFm.MenuItem4Click(Sender: TObject);
begin
  AddFieldRow;
end;

procedure TReportFm.MenuItem5Click(Sender: TObject);
var
  Msg: String;
  i: Integer;
begin
  if not ConfirmDelete then Exit;
  i := FOldFieldNames.IndexOfObject(TObject(Grid.Objects[0, Grid.Row]));
  if i >= 0 then
  begin
    if FOldFieldNames[i] <> Grid.Cells[Grid.ColCount - 4, Grid.Row] then
      Msg := LineEnding + rsShowOldFieldNameBeforeRename
    else
      Msg := '';
    if CheckExistsInActions(FRD, renRpField, FOldFieldNames[i], Msg) then Exit;
    {FieldNameDS := 'f' + IntToStr(PtrInt(Grid.Objects[0, Grid.Row]));
    if FRD.Totals.FindTotal(FieldNameDS) <> nil then
    begin
      ErrMsgFmt(rsCannotDeleteTotalField, [ Grid.Cells[Grid.ColCount - 3, Grid.Row] ]);
      Exit;
    end; }
  end;

  Grid.DeleteRow(Grid.Row);
  SetControlState;
  SetModified;
end;

procedure TReportFm.MenuItem6Click(Sender: TObject);
begin
  DoSelectField(False);
  SetControlState;
end;

procedure TReportFm.MenuItem7Click(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := '';
  Grid.Objects[Grid.Col, Grid.Row] := nil;
  SetControlState;
  SetModified;
end;

procedure TReportFm.PeriodChange(Sender: TObject);
begin
  SetModified;
end;

procedure TReportFm.PickUpBnClick(Sender: TObject);
begin
  DoSelectField(True);
end;

procedure TReportFm.ShowFirstRecordsChange(Sender: TObject);
begin
  with FirstRecordCount do
  begin
    Enabled := TCheckBox(Sender).Checked;
    if CanFocus then
    begin
      SetFocus;
      SelStart := Length(Text);
    end;
  end;
end;

procedure TReportFm.SqlBnClick(Sender: TObject);
begin
  if Confirm(rsWarning, rsSwitchSQLModeMsg) <> mrYes then Exit;
  FRD.SqlMode := True;
  ModalResult := mrCancel;
end;

procedure TReportFm.ToolButton1Click(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: MenuItem4.Click;
    1: MenuItem5.Click;
    2: MenuItem1.Click;
    3: MenuItem2.Click;
  end;
end;

procedure TReportFm.ToolButton6Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row - 1);
  SetControlState;
  SetModified;
end;

procedure TReportFm.ToolButton7Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row + 1);
  SetControlState;
  SetModified;
end;

procedure TReportFm.ToolButton8Click(Sender: TObject);
begin
  Grid.ExchangeColRow(True, Grid.Col - 1, Grid.Col);
  SetControlState;
  SetModified;
end;

procedure TReportFm.ToolButton9Click(Sender: TObject);
begin
  Grid.ExchangeColRow(True, Grid.Col, Grid.Col + 1);
  SetControlState;
  SetModified;
end;

function TReportFm.FindFirstCell(r: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 1 to Grid.ColCount - 5 do
  begin
    if Grid.Objects[i, r] <> nil then Exit(i);
  end;
end;

procedure TReportFm.ExprEditingDone(Sender: TObject);
begin
  if FExprEd.Modified then SetModified;
end;

procedure TReportFm.FieldNameEditingDone(Sender: TObject);
begin
  if FFieldNameEd.Modified then SetModified;
end;

function TReportFm.GetFirstFieldType(r: Integer): TRpFieldType;
var
  i: Integer;
  C: TComponent;
  S: String;
begin
  Result := flNone;
  for i := 1 to Grid.ColCount - 5 do
  begin
    if Grid.Objects[i, r] <> nil then
    begin
      S := Grid.Cells[i, r];
      if S[1] = '!' then Delete(S, 1, 1);
      C := TComponent(Grid.Objects[i, r]);
      C := LookupComponent(TdxForm(C.Owner), S);
      Exit( GetTypeByComponent(C) );
    end;
  end;
end;

procedure TReportFm.GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if (aCol >= Grid.ColCount - 4) and (aRow in [1..4]) then
    Editor := nil
  else if (aCol >= 1) and (aCol < Grid.ColCount - 4) and (aRow = 3) then
  begin
    Editor := FExprEd;
  end
  else if (aCol = Grid.ColCount - 4) and (aRow >= 5) then
    Editor := FFieldNameEd
  else if (aCol in [1..Grid.ColCount - 5]) and (aRow >= 5) then
  	Editor := nil
  else if Editor is TPickListCellEditor then
    with TPickListCellEditor(Editor) do
    begin
      Style:=csDropDownList;
      ItemIndex := Items.IndexOfObject(Grid.Objects[aCol, aRow]);
      Tag := ItemIndex;
      OnKeyDown:=@PickListKeyDown;
      DropDownCount := 16;
    end
  // Имя поля
  {else if Editor is TStringCellEditor then
    with TStringCellEditor(Editor) do
    begin
      OnEditingDone:=@StringEditingDone;
      PopupMenu := FieldNameMnu;
    end;  }
end;

procedure TReportFm.FormCreate(Sender: TObject);
begin
  Grid.SelectedColor:=clSilver;
  Grid.FocusColor:=Grid.SelectedColor;
  Grid.Cells[0, 1] := rsForm;
  Grid.Cells[0, 2] := rsTable;
  Grid.Cells[0, 3] := rsFilter;
  Grid.Cells[0, 4] := rsSourceType;
  Grid.ValidateOnSetSelection := True;
  Label1.Caption := rsGroupByDate;
  Label2.Caption:=rsPeriod;
  Period.Items.AddStrings([rsDDDay, rsDDWeek, rsDDMonth, rsDDQuarter,
    rsDDHalfYear, rsDDYear]);
  MenuItem4.Caption := rsAppendField;
  MenuItem5.Caption := rsDeleteField;
  MenuItem1.Caption := rsAppendSource;
  MenuItem2.Caption := rsDeleteSource;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  //ButtonPanel1.CancelButton.Cancel := False;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  FExprEd := TExprCellEditor.Create(Self);
  FExprEd.ButtonCaption := '...';
  FExprEd.OnButtonClick:=@BtnClick;
  FExprEd.OnEditingDone:=@ExprEditingDone;
  FFieldNameEd := TActionText.Create(Self);
  FFieldNameEd.OnEditingDone:=@FieldNameEditingDone;
  ImageList1.AddLazarusResource('add16');
  ImageList1.AddLazarusResource('delete16');
  ImageList1.AddLazarusResource('up16');
  ImageList1.AddLazarusResource('down16');
  ImageList1.AddLazarusResource('left16_2');
  ImageList1.AddLazarusResource('right16_2');
  ImageList1.AddLazarusResource('magic16');
  ImageList1.AddLazarusResource('sql16');
  ToolButton1.Caption := rsAppendField;
  ToolButton2.Caption := rsDeleteField;
  ToolButton4.Caption := rsAppendSource;
  ToolButton5.Caption := rsDeleteSource;
  ToolButton6.Caption := rsMoveUp;
  ToolButton7.Caption := rsMoveDown;
  ToolButton8.Caption := rsLeft;
  ToolButton9.Caption := rsRight;
  PickUpBn.Caption := rsPickUp;
  PickUpBn.Hint := rsPickUpBnHint;
  SqlBn.Hint := rsSwitchToSQLMode;
  FSelFieldFm := TSelectFieldForm.CreateNew(Self);
  with FSelFieldFm do
  begin
    Caption := rsSelectSourceField;
    FirstParentForm := True;
    ShowFieldsOfObject := True;
    ShowParFormPrefix := True;
    //ShowImages := False;
    OnPickUp:=@SelFieldFmPickUp;
  end;
  FOldFieldNames := TStringList.Create;
  ShowFirstRecords.Caption := rsShowOnlyFirstRecords;
  Label3.Caption := rsRecords;
end;

procedure TReportFm.FormDestroy(Sender: TObject);
begin
  FOldFieldNames.Free;
  //FreeAndNil(FSelFieldFm);
end;

procedure TReportFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
  SetControlState;
end;

procedure TReportFm.GridCellProcess(Sender: TObject; aCol, aRow: Integer;
  processType: TCellProcessType; var aValue: string);
begin
  if processType = cpPaste then
    SetModified;
end;

procedure TReportFm.GridCheckboxToggled(sender: TObject; aCol, aRow: Integer;
  aState: TCheckboxState);
begin
  if (aCol >= Grid.ColCount - 2) and (aRow in [1..4]) then
    Grid.Cells[aCol, aRow] := '0';
  SetModified;
end;

procedure TReportFm.GridDblClick(Sender: TObject);
begin
  MenuItem6.Click;
end;

procedure TReportFm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if (aCol >= Grid.ColCount - 4) and (aRow in [1..4]) then
  begin
    Grid.Canvas.Brush.Color:=$F0F0F0;
    Grid.Canvas.FillRect(aRect);
  end
  else if (aCol in [1..Grid.ColCount - 5]) and (aRow = 4) then
  begin
    if Grid.Cells[aCol, aRow] = rsIncoming then
    begin
      Grid.Canvas.Brush.Color:=$0099FF99;
	    Grid.Canvas.FillRect(aRect);
    end
    else if Grid.Cells[aCol, aRow] = rsOutcoming then
    begin
      Grid.Canvas.Brush.Color:=$009999FF;
	    Grid.Canvas.FillRect(aRect);
    end;
    Grid.DefaultDrawCell(aCol, aRow, aRect, aState);
  end;
end;

procedure TReportFm.GridEnter(Sender: TObject);
begin
  Grid.SelectedColor := clHighlight;
  Grid.FocusColor:=Grid.SelectedColor;
end;

procedure TReportFm.GridExit(Sender: TObject);
begin
  Grid.SelectedColor := clSilver;
  Grid.FocusColor:=Grid.SelectedColor;
end;

procedure TReportFm.GridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
	HintText:=StringReplace(HintText, '|', '/', [rfReplaceAll]);
end;

procedure TReportFm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    MenuItem6.Click;
  end
  else if (Key in [VK_C, VK_V, VK_X]) and (Shift = [ssModifier]) then
  begin
    if (Grid.Row = 3) and (Grid.Col > 0) and (Grid.Col < Grid.ColCount - 1) then
    else if (Grid.Row >= 5) and (Grid.Col = Grid.ColCount - 4) then
    else Key := 0
  end
  else if (Key = VK_X) and (Shift = [ssShift]) then
    Key := 0
  // Пришлось добавить, т. к. по Ctrl+Del не удалялось поле, а удалялся текст в
  // ячейке.
  else if (Key = VK_DELETE) and (Shift = [ssModifier]) and (Grid.Col = Grid.ColCount - 4) then
  begin
    MenuItem5.Click;
    Key := 0;
  end
  else if (Key = VK_ESCAPE) and not Grid.EditorMode then
  begin
    Key := 0;
    ModalResult := mrCancel;
  end;
end;

procedure TReportFm.GridPickListSelect(Sender: TObject);
var
  c, r: Integer;
  PL: TPickListCellEditor;
  Obj: TObject;

  procedure ClearForm;
  var
    i: Integer;
  begin
    for i := 2 to Grid.RowCount - 1 do
    begin
      Grid.Cells[c, i] := '';
      Grid.Objects[c, i] := nil;
    end;
  end;

  procedure ClearTable;
  var
    Fm: TdxForm;
    i: Integer;
    Cmp: TComponent;
  begin
    Fm := TdxForm(Grid.Objects[c, 1]);
    for i := 5 to Grid.RowCount - 1 do
    begin
      Cmp := TComponent(Grid.Objects[c, i]);
      if (Cmp <> nil) and (Cmp.Owner <> Fm) then
      begin
        Grid.Cells[c, i] := '';
        Grid.Objects[c, i] := nil;
      end;
    end;
  end;

begin
  c := Grid.Col;
  PL := TPickListCellEditor(Grid.Editor);
  if c in [1..Grid.ColCount - 5] then
  begin
    r := Grid.Row;
    if r > 0 then
    begin
      Obj := Grid.Objects[c, r];
      with PL do
        Grid.Objects[c, r] := Items.Objects[ItemIndex];
      if Grid.Objects[c, r] = Obj then Exit;
    end;
    if r = 1 then ClearForm
    else if r = 2 then ClearTable;
    SetModified;
  end
  else if (Grid.Col = Grid.ColCount - 3) and (Grid.Row >= 5) then
  begin
    if Grid.Objects[Grid.Col, Grid.Row] <> PL.Items.Objects[PL.ItemIndex] then
    begin
      Grid.Objects[Grid.Col, Grid.Row] := PL.Items.Objects[PL.ItemIndex];
      SetModified;
    end;
  end;
  if Grid.Row = 1 then SetControlState;
end;

procedure TReportFm.GridResize(Sender: TObject);
begin
  {$ifdef linux}
  if Grid.Flat then Grid.Invalidate;
  {$endif}
end;

procedure TReportFm.BtnClick(Sender: TObject);
var
  S: TCaption;
  Fm, Tbl: TdxForm;
begin
  Fm := TdxForm(Grid.Objects[Grid.Col, 1]);
  Tbl := TdxForm(Grid.Objects[Grid.Col, 2]);
  if Fm = nil then Exit;
  S := FExprEd.Text;

  if ShowExprForm(etSourceFilter, FQGrid, S, FFm, Tbl, Fm, nil) = mrOk then
  begin
    FExprEd.Text := S;
    FExprEd.Modified:=True;
  end;
  FExprEd.SetFocus;
end;

procedure TReportFm.DateFlChange(Sender: TObject);
begin
  SetModified;
end;

procedure TReportFm.DateFlDropDown(Sender: TObject);
begin
  FillDateFl;
end;

procedure TReportFm.FieldNameMnuClick(Sender: TObject);
begin
  //
end;

procedure TReportFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
  begin
    CanClose := Validate;
    if not CanClose then
    begin
      Grid.SetFocus;
      SetControlState;
    end;
  end
  else
  begin
    if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TReportFm.Load;
var
  i, c, r, j: Integer;
  Sr: TRpSource;
  Fm: TdxForm;
  Col: TGridColumn;
  Fl: TRpField;
  S: String;
begin
  FOldFieldNames.Clear;
  DateFl.Clear;
  if (FRD.DateField >= 0) and (FRD.Sources.Count > 0) then
  begin
    Fl := FRD.Sources[0]^.Fields[FRD.DateField]^;
    DateFl.Text := Fl.Name;
  end;
  Period.ItemIndex := Ord(FRD.DateDetail);
  FirstRecordCount.Value := FRD.FirstRecordCount;
  ShowFirstRecords.State := IIF(FRD.FirstRecordCount > 0, cbChecked, cbUnchecked);
  FirstRecordCount.Enabled := ShowFirstRecords.Checked;

  Grid.Columns.Clear;
  Grid.RowCount := 5;
  if FRD.Sources.Count > 0 then
    Grid.RowCount := FRD.Sources[0]^.Fields.Count + 5;
  for i := 0 to FRD.Sources.Count - 1 do
  begin
    c := i + 1;
    Col := Grid.Columns.Add;
    Col.Width := ScaleToScreen(120);
    Col.Title.Caption := Format(rsSourceNum, [i+1]);
    Sr := FRD.Sources[i]^;
    Fm := FormMan.FindForm(Sr.Id);
    Grid.Cells[c, 1] := Fm.FormCaption;
    Grid.Objects[c, 1] := Fm;
    if Sr.TId > 0 then
    begin
      Fm := FormMan.FindForm(Sr.TId);
      Grid.Cells[c, 2] := Fm.FormCaption;
      Grid.Objects[c, 2] := Fm;
    end;
    Grid.Cells[c, 3] := Sr.Filter;
    Grid.Cells[c, 4] := SourceKindToStr(Sr.Kind);
    Grid.Objects[c, 4] := TObject(PtrInt(Sr.Kind));

    for j := 0 to Sr.Fields.Count - 1 do
    begin
      r := j + 5;
      Fl := Sr.Fields[j]^;
      if (Fl.Zero) or (Fl.Tp = flNone) then Continue;
      S := GetFullFieldName(Fl);
      if Fl.TId = Sr.Id then S := '!' + S;
      Grid.Cells[c, r] := S;
      Grid.Objects[c, r] := GetRpFieldComponent(Fl, False);
    end;
  end;

  // имя поля
  Col := Grid.Columns.Add;
  Col.Width := ScaleToScreen(120);
  Col.Title.Caption := rsFieldName;
  // Функция
  Col := Grid.Columns.Add;
  Col.Width := ScaleToScreen(120);
  Col.Title.Caption := rsFunction;
  // Видимое
  Col := Grid.Columns.Add;
  if FRD.Kind = rkReport then
  begin
    Col.Title.Caption := rsVisible;
    Col.ButtonStyle:=cbsCheckboxColumn;
	  Col.Visible:=True;
    Col.Width := ScaleToScreen(70);
  end
  else
  begin
	  Col.Visible:=False;
    Col.Width := 0;
  end;
  // Параметр
  Col := Grid.Columns.Add;
  if FRD.Kind = rkReport then
  begin
    Col.Title.Caption := rsParameter;
    Col.ButtonStyle:=cbsCheckboxColumn;
	  Col.Visible:=True;
    Col.Width := ScaleToScreen(70);
  end
  else
  begin
	  Col.Visible:=False;
    Col.Width := 0;
  end;

  // Заполяем столбцы
  FMaxFId := 0;
  if FRD.Sources.Count > 0 then
  begin
    c := Grid.ColCount;
    Sr := FRD.Sources[0]^;
    for i := 0 to Sr.Fields.Count - 1 do
    begin
      Fl := Sr.Fields[i]^;
      r := i + 5;
      Grid.Cells[c-4, r] := Fl.Name;
      Grid.Cells[c-3, r] := TotalFuncToStr(Fl.Func);
      Grid.Objects[c-3, r] := TObject(PtrInt(Fl.Func));
      Grid.Cells[c-2, r] := Bool2Str(Fl.Visible);
      Grid.Cells[c-1, r] := Bool2Str(Fl.Param);

      // Id поля храним в 1 колонке. У полей разных источников Id одинаковый.
      Grid.Objects[0, r] := TObject(PtrInt(Fl.Id));
      // Сразу определяем максимальный Id.
      if Fl.Id > FMaxFId then FMaxFId := Fl.Id;

      if Fl.Visible then
        FOldFieldNames.AddObject(Fl.Name, TObject(PtrInt(Fl.Id)));
    end;
  end;

  if FRD.Sources.Count = 0 then
    MenuItem1.Click;
end;

procedure TReportFm.PickListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  with TPickListCellEditor(Sender) do
    if Key = VK_ESCAPE then
      // Восстанавливаем прежний объект в ячейке
      if Tag >= 0 then
      begin
        Grid.Objects[Grid.Col, Grid.Row] := Items.Objects[Tag];
        SetModified;
      end;
end;


procedure TReportFm.Save;
var
  i, j, c: Integer;
  pSr: PRpSource;
  pFl: PRpField;
  Cmp: TComponent;
  Tp: TRpFieldType;
begin
  DeleteOldFieldsReferences;

  FRD.SQLFields.Clear;
  FRD.SQL := '';

  FRD.DateField := -1;
  if DateFl.Text <> '' then
    FRD.DateField:=FindDateFieldIndex(DateFl.TexT);
  FRD.DateDetail:=TRpDateDetail(Period.ItemIndex);
  if ShowFirstRecords.Checked then
    FRD.FirstRecordCount:=FirstRecordCount.Value
  else
    FRD.FirstRecordCount:=0;

  FRD.Sources.Clear;
  for i := 1 to Grid.Columns.Count - 4 do
  begin
    FRD.Sources.AddSource(pSr);
    pSr^.Id := TdxForm(Grid.Objects[i, 1]).Id;
    if Grid.Objects[i, 2] <> nil then
      pSr^.TId := TdxForm(Grid.Objects[i, 2]).Id;
    pSr^.Filter := Grid.Cells[i, 3];
    pSr^.Kind:=TRpSourceKind(PtrInt(Grid.Objects[i, 4]));

    c := Grid.Columns.Count;
    for j := 5 to Grid.RowCount - 1 do
    begin
      pSr^.Fields.AddField(pFl);
      Cmp := TComponent(Grid.Objects[i, j]);
      if Cmp = nil then
      begin
        pFl^.Zero := True;
        Tp := GetFirstFieldType(j);
        if Tp <> flNone then pFl^.Tp := Tp
        else pFl^.Tp:=flNumber;
      end
      else
        SetupRpField(Cmp, Grid.Cells[i, j], pFl);
      pFl^.Id := PtrInt(Grid.Objects[0, j]);
      pFl^.Name := Grid.Cells[c - 3, j];
      pFl^.Func := TRpTotalFunc(PtrInt(Grid.Objects[c - 2, j]));
      pFl^.Visible:=Str2Bool(Grid.Cells[c - 1, j]);
      pFl^.Param := Str2Bool(Grid.Cells[c, j]);
    end;
  end;
  RemoveLostFieldsFromReportData(FRD);
  CreateOrUpdateReportGridColumns(FRD);

  if FFm <> nil then
	  UpdatePivotFieldCaptions(FFm, FRD);
end;

procedure TReportFm.SelFieldFmPickUp(Sender: TObject; const AFieldName: String;
  AComponent: TComponent);
var
  r: Integer;
begin
  r := Grid.Row;
  if r < 5 then r := 5;
  with Grid do
    while r < RowCount do
    begin
      if Objects[Col, r] = nil then
      begin
        Row := r;
        PasteField(AFieldName, AComponent);
        Exit;
      end;
      Inc(r);
    end;
  AddFieldRow;
  PasteField(AFieldName, AComponent);
end;

procedure TReportFm.SetControlState;
begin
  MenuItem5.Enabled:=Grid.Row >= 5;
  MenuItem2.Enabled := (Grid.Col > 1) and (Grid.Col < Grid.ColCount - 4);
  MenuItem6.Enabled := (Grid.Col in [1..Grid.ColCount - 5]) and (Grid.Row >= 5);
  MenuItem7.Enabled := MenuItem6.Enabled and (Grid.Objects[Grid.Col, Grid.Row] <> nil);
  ToolButton2.Enabled := MenuItem5.Enabled;
  ToolButton5.Enabled := MenuItem2.Enabled;
  ToolButton6.Enabled := Grid.Row > 5;
  ToolButton7.Enabled := (Grid.Row >= 5) and (Grid.Row < Grid.RowCount - 1);
  ToolButton8.Enabled := (Grid.Col > 1) and (Grid.Col < Grid.ColCount - 4);
  ToolButton9.Enabled := (Grid.Col >= 1) and (Grid.Col < Grid.ColCount - 5);
  PickUpBn.Enabled := (Grid.Col <= Grid.ColCount - 4) and (Grid.Objects[Grid.Col, 1] <> nil);
end;

procedure TReportFm.FillForms(L: TStrings);
begin
  //FormMan.FormsToList(L);
  FormMan.SourceFormsToList(L);
end;

procedure TReportFm.FillTables(L: TStrings);
var
  i, id: Integer;
  F: TdxForm;
  SL: TStringListUtf8;
begin
  L.AddObject('', nil);
  F := TdxForm(Grid.Objects[Grid.Col, 1]);
  if F = nil then Exit;
  id := F.Id;
  SL := TStringListUtf8.Create;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    F := FormMan.Forms[i];
    if F.PId = id then
      SL.AddObject(F.FormCaption, F);
  end;
  SL.Sort;
  L.AddStrings(SL);
  SL.Free;
end;

procedure TReportFm.FillIncOut(L: TStrings);
begin
  L.AddObject('', TObject(0));
  L.AddObject(rsIncoming, TObject(1));
  L.AddObject(rsOutcoming, TObject(2));
end;

procedure TReportFm.FillFuncs(L: TStrings);
begin
  L.AddObject('', nil);
  L.AddObject(rsSum, TObject(tfSum));
  L.AddObject(rsAverage, TObject(tfAvg));
  L.AddObject(rsMaximum, TObject(tfMax));
  L.AddObject(rsMinimum, TObject(tfMin));
  L.AddObject(rsCount, TObject(tfCount));
  L.AddObject(rsBalance, TObject(tfProfit));
  L.AddObject(rsDistinctCount, TObject(tfDistCount));
  L.AddObject(rsMerge, TObject(tfMerge));
  L.AddObject(rsMergeAll, TObject(tfMergeAll));
end;

procedure TReportFm.FillDateFl;
var
  L: TStrings;
  C: TComponent;
  i: Integer;
begin
  DateFl.Clear;
  L := DateFl.Items;
  for i := 5 to Grid.RowCount - 1 do
  begin
    C := TComponent(Grid.Objects[1, i]);
    if (C <> nil) and (C is TdxDateEdit) then
      L.Add(Grid.Cells[Grid.ColCount - 4, i]);
  end;
end;

function TReportFm.FindDateFieldIndex(const S: String): Integer;
var
  i: Integer;
  C: TComponent;
begin
  Result := -1;
  for i := 5 to Grid.RowCount - 1 do
  begin
    if MyUtf8CompareText(Grid.Cells[Grid.ColCount - 4, i], S) = 0 then
    begin
      C := TComponent(Grid.Objects[1, i]);
      if (C <> nil) and (C is TdxDateEdit) then
        Exit(i - 5);
    end;
  end;
end;

function IsCompatibleFields(C1, C2: TComponent): Boolean;
begin
  if (C1 = nil) or (C2 = nil) then
    Result := True
  else if ((C1 is TdxEdit) or (C1 is TdxComboBox) or (C1 is TdxMemo)) and
    ((C2 is TdxEdit) or (C2 is TdxComboBox) or (C2 is TdxMemo)) then
    Result := True
  else if ((C1 is TdxLookupComboBox) or (C1 is TdxRecordId)) and
    ((C2 is TdxLookupComboBox) or (C2 is TdxRecordId)) then
    Result := True
  else if (C1 <> nil) and (C2 <> nil) then
    Result := C1.ClassName = C2.ClassName
  else
    Result := False;
end;

function TReportFm.Validate: Boolean;

  function IsBalanceExists: Boolean;
  var
    m: Integer;
  begin
    Result := False;
    for m := 5 to Grid.RowCount - 1 do
    begin
      if TRpTotalFunc(PtrInt(Grid.Objects[Grid.ColCount - 3, m])) = tfProfit then
        Exit(True);
    end;
  end;

  function CheckValidName(idx: Integer): Boolean;
  var
    S: String;
  begin
    S := Grid.Cells[Grid.ColCount - 4, idx];
    Result := CheckFieldName(S) and CheckSuffixName(S);
  end;

  function CheckDuplicates(idx: Integer): Boolean;
  var
    i: Integer;
    S, SS: String;
  begin
    if Grid.Cells[Grid.ColCount - 2, idx] = '0' then Exit(True);

    S := Grid.Cells[Grid.ColCount - 4, idx];
    //if not CheckFieldName(S) or not CheckSuffixName(S) then Exit(False);

    for i := idx + 1 to Grid.RowCount - 1 do
    begin
      if Grid.Cells[Grid.ColCount - 2, i] = '0' then Continue;  // Пропускаем параметры
      SS := Grid.Cells[Grid.ColCount - 4, i];
      if MyUtf8CompareText(S, SS) = 0 then
      begin
        ErrMsgFmt(rsDuplicateFieldName, [S]);
        Exit(False);
      end;
    end;
  end;

  function CheckDuplicatesCalcFields(idx: Integer): Boolean;
  var
    S: String;
  begin
    if Grid.Cells[Grid.ColCount - 2, idx] = '0' then Exit(True);
    S := Grid.Cells[Grid.ColCount - 4, idx];
    Result := FRD.CalcFields.FindFieldByName(S) = nil;
    if not Result then
      ErrMsgFmt(rsCalcFieldNameExists, [S]);
  end;

  function CheckDuplicatesParams(idx: Integer): Boolean;
  var
    S, SS: String;
    i: Integer;
  begin
    Result := True;
    if FRD.Kind = rkQuery then Exit;
    if Grid.Cells[Grid.ColCount - 1, idx] = '0' then Exit;
    S := Grid.Cells[Grid.ColCount - 4, idx];
    for i := idx + 1 to Grid.RowCount - 1 do
    begin
      if Grid.Cells[Grid.ColCount - 1, i] = '0' then Continue;  // Пропускаем, если это не параметр
      SS := Grid.Cells[Grid.ColCount - 4, i];
      if MyUtf8CompareText(S, SS) = 0 then
      begin
        ErrMsg(rsParamExists);
        Exit(False);
      end;
    end;
  end;

  function CheckDuplicatesPrintFields(idx: Integer): Boolean;
  var
    S, SS: String;
    i: Integer;
  begin
    Result := True;
    if FRD.Kind = rkQuery then Exit;
    if Grid.Cells[Grid.ColCount - 1, idx] = '0' then Exit;
    S := Grid.Cells[Grid.ColCount - 4, idx];
    for i := 0 to FRD.PrintFields.Count - 1 do
    begin
      SS := FRD.PrintFields.Names[i];
      if MyUtf8CompareText(S, SS) = 0 then
      begin
        ErrMsg(rsPrintFieldExists);
        Exit(False);
      end;
    end;
  end;

  function CheckDuplicatesTotals(idx: Integer): Boolean;
  var
    S, SS: String;
    i: Integer;
  begin
    Result := True;
    if FRD.Kind = rkQuery then Exit;
    if Grid.Cells[Grid.ColCount - 1, idx] = '0' then Exit;
    S := Grid.Cells[Grid.ColCount - 4, idx];
    for i := 0 to FRD.Totals.Count - 1 do
    begin
      SS := FRD.Totals[i].Caption;
      if MyUtf8CompareText(S, SS) = 0 then
      begin
        ErrMsg(rsTotalsExists);
        Exit(False);
      end;
    end;
  end;

  function _LookupCmp(Fm: TdxForm; FlNm: String): TComponent;
  begin
    if Copy(FlNm, 1, 1) = '!' then Delete(FlNm, 1, 1);
    Result := LookupComponent(Fm, FlNm);
  end;

var
  i, j, n, c, si: Integer;
  C1, C2: TComponent;
  Fn: TRpTotalFunc;

begin
  Result := False;
  // Выбрана ли форма
  for i := 1 to Grid.ColCount - 5 do
  begin
    if Grid.Objects[i, 1] = nil then
    begin
      ErrMsg(rsFormNotSel);
      Grid.Row := 1; Grid.Col := i;
      Exit;
    end;
  end;
  // Добавлено ли хоть одно поле для выбора
  if Grid.RowCount = 5 then
  begin
    ErrMsg(rsFieldsNotSelected);
    Exit;
  end;
  c := Grid.ColCount;
  for i := 5 to Grid.Rowcount - 1 do
  begin
    n := 0;
    // Выбрано ли поле в строке
    for j := 1 to Grid.ColCount - 5 do
      if Grid.Objects[j, i] <> nil then Inc(n);
    // Не выбирать ни одного поля можно только для функции "Количество".
    if (n = 0) and (TRpTotalFunc(PtrInt(Grid.Objects[Grid.ColCount - 3, i])) <> tfCount) then
    	//(TRpTotalFunc(Grid.Objects[Grid.ColCount - 3, i]) <> tfCount)) then
    begin
      ErrMsg(rsFieldNotSel);
      Grid.Row := i; Grid.Col := 1;
      Exit;
    end
    else
    begin
      // Если функция выбрана, то смотрим тип полей. Он должен быть числовой для некоторых ф-ций.
      if Grid.Objects[c - 3, i] <> nil then
      begin
        Fn := TRpTotalFunc(PtrInt(Grid.Objects[c - 3, i]));
        n := 0;
        for j := 1 to Grid.ColCount - 5 do
        begin
          C1 := TComponent(Grid.Objects[j, i]);
          if (C1 <> nil) and (C1 is TdxLookupComboBox) then
            C1 := _LookupCmp(TdxForm(C1.Owner), Grid.Cells[j, i]);
          if Fn in [tfSum, tfAvg, tfProfit] then
          begin
            if (C1 = nil) or (C1 is TdxCalcEdit) {or (C1 is TdxCounter) or (C1 is TdxRecordId)} then
            else
            begin
              ErrMsg(rsFieldShouldBeNum);
              Grid.Row := i; Grid.Col := j;
              Exit;
            end;
          end
          else if (C1 is TdxDBImage) or (C1 is TdxFile) then
          begin
            ErrMsg(rsFuncNotImageFiles);
            Grid.Row := i; Grid.Col := j;
            Exit;
          end;
          {else if (Fn in [tfMerge, tfMergeAll]) and (Grid.Cells[c-1, i] = '1') then
          begin
            if (C1 <> nil) and (not ((C1 is TdxEdit) or (C1 is TdxMemo) or (C1 is TdxComboBox))) then
            begin
              ErrMsgFmt(rsOnlyTextFieldCanBeParamWithMergeFunc, [Grid.Cells[c-3, i]]);
              Grid.Row := i; Grid.Col := j;
              Exit;
            end;
          end;}
          if C1 <> nil then Inc(n);
        end;
        // Нельзя использовать это поле как параметр, если ни одного поля не выбрано.
        if (Grid.Cells[Grid.ColCount - 1, i] = '1') and (n = 0) then
        begin
          ErrMsg(rsFieldCantParam);
          Grid.Row := i; Grid.Col := c - 4;
          Exit;
        end;
        // Нельзя использовать это поле как параметр, если выбрана функция

      end;
      // Если поле выбрано, то оно должно быть в результате или в параметрах
      if (Grid.Cells[c - 2, i] = '0') and (Grid.Cells[c - 1, i] = '0') then
      begin
        ErrMsg(rsFieldShouldBeVisibleOrParam);
        Grid.Row := i; Grid.Col := c - 2;
        Exit;
      end;
      // имя поля
      if Grid.Cells[Grid.ColCount - 4, i] = '' then
      begin
        ErrMsg(rsEnterFieldName);
        Grid.Row := i; Grid.Col := c - 4;
        Exit;
      end;
      // Ищем первую заполненную ячейку в строке
      si := FindFirstCell(i);
      if si < 0 then si := 10000;  	// Таким образом избегаем цикл.
      // Проверка на совместимость полей
      for j := si to Grid.ColCount - 5 do
      begin
        C1 := TComponent(Grid.Objects[1, i]);
        C2 := TComponent(Grid.Objects[j, i]);
        if C1 is TdxLookupComboBox then
          C1 := _LookupCmp(TdxForm(C1.Owner), Grid.Cells[1, i]);
        if C2 is TdxLookupComboBox then
          C2 := _LookupCmp(TdxForm(C2.Owner), Grid.Cells[j, i]);
        if not IsCompatibleFields(C1, C2) then
        begin
          ErrMsg(rsIncompatibleFields);
          Grid.Row := i; Grid.Col := j;
          Exit;
        end;
      end;
    end;
  end;

  // Хотя бы одно поле должно быть видимым
  n := 0;
  for i := 5 to Grid.RowCount - 1 do
    if Grid.Cells[Grid.ColCount - 2, i] = '1' then Inc(n);
  if n = 0 then
  begin
    ErrMsg(rsNoVisibleFiels);
    Exit;
  end;

  // Проверка имени и дубликатов
  for i := 5 to Grid.RowCount - 1 do
  begin
    if not CheckValidName(i) or
      not CheckDuplicates(i) or not CheckDuplicatesCalcFields(i) or
      not CheckDuplicatesParams(i) or not CheckDuplicatesPrintFields(i) or
      not CheckDuplicatesTotals(i) then
    begin
      Grid.Row := i; Grid.Col := c - 4;
      Exit;
    end;
  end;

  // Проверяем тип источника
  if IsBalanceExists then
  begin
    for i := 1 to Grid.ColCount - 5 do
    begin
      if Grid.Objects[i, 4] = nil then
      begin
        ErrMsg(rsSelectSourceType);
        Grid.Row := 4; Grid.Col := i;
        Exit;
      end;
    end;
  end;

  // Проверяем выбранное поле даты для группировки
  if DateFl.Text <> '' then
  begin
    n := FindDateFieldIndex(DateFl.Text);
    if n < 0 then
    begin
      ErrMsg(rsDateFieldNotFound);
      Exit;
    end;
    // Проверяем, чтобы поле было видимым
    if Grid.Cells[c - 2, n + 5] = '0' then
    begin
      ErrMsg(rsDateFieldShoulBeVisible);
      Grid.Row := n + 5; Grid.Col := c - 2;
      Exit;
    end;
    // Нельзя одновременно использовать группировку и функцию
    if Grid.Objects[c - 3, n + 5] <> nil then
    begin
      ErrMsg(rsCannotUseGroupDateAndFunc);
      Grid.Row := n + 5; Grid.Col := c - 3;
      Exit;
    end;
  end;

  if not CanOldSqlFieldsDeleted then Exit;

  Result := ProcessRenameFieldsInActions;
end;

function TReportFm.ShowForm(RD: TReportData; aForm: TdxForm;
  QGrid: TdxQueryGrid; InDesigner: Boolean; SelSource: Integer): Integer;
begin
  if RD = nil then Exit;
  FInDesigner := InDesigner;
  FRD := RD;
  FFm := aForm;
  FQGrid := QGrid;
  Caption := rsSelectionSet + ': ' + RD.Name;
  Load;
  ResetModified;
  FSelFieldFm.ForgetForms;
  if SelSource = 0 then
  begin
    Grid.Col := 1;
    // Чтобы подставлялся сразу выпадающий список, а не редактор по умолчанию
    Grid.Row := 2; Grid.Row := 1;
    //
  end
  else
  begin
    Grid.Col := SelSource;
    Grid.Row := 2; Grid.Row := 3;
  end;
  Result := ShowModal;
  if Result = mrOk then Save;
end;

end.

