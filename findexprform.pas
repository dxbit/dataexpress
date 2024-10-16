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

unit FindExprForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls, Grids, ExtCtrls, DxCtrls, LazUtf8, LclType, strconsts, Types,
  mytypes;

type

  TComponentProperty = (cpNone, cpExpression, cpDefaultValue, cpCheckValue,
    cpListFilter, cpFillTable, cpCalcField, cpColoring, cpSourceFilter,
    cpOutFilter, cpWhenPrinting, cpSQL);

  { TFindExprFm }

  TFindExprFm = class(TForm)
    FmFilter: TCheckGroup;
    QFilter: TCheckGroup;
    RpFilter: TCheckGroup;
    FindBn: TBitBtn;
    FmGrid: TStringGrid;
    FmNothingPan: TPanel;
    FilterPan: TPanel;
    RpNothingPan: TPanel;
    QNothingPan: TPanel;
    QGrid: TStringGrid;
    PageControl1: TPageControl;
    RpGrid: TStringGrid;
    FmTab: TTabSheet;
    QTab: TTabSheet;
    RpTab: TTabSheet;
    FilterBn: TToggleBox;
    ValueEd: TEdit;
    Label1: TLabel;
    procedure FilterBnChange(Sender: TObject);
    procedure FindBnClick(Sender: TObject);
    procedure FmGridDblClick(Sender: TObject);
    procedure FmGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FmGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FmGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure GridEnter(Sender: TObject);
    procedure GridExit(Sender: TObject);
    procedure QGridDblClick(Sender: TObject);
    procedure QGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure QGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure RpGridDblClick(Sender: TObject);
    procedure RpGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RpGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure ValueEdKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FImages: TImageList;
    procedure CheckAll(Group: TCheckGroup);
    function AnyChecked(Group: TCheckGroup): Boolean;
    procedure ShowFormResult;
    procedure ShowQueryResult;
    procedure ShowReportResult;
    procedure FindInForms;
    procedure FindInQueries;
    procedure FindInReports;
    procedure UpdateTabCaptions;
  public
    procedure ShowForm;
    procedure DeleteForm(Fm: TdxForm);
    procedure DeleteComponent(C: TObject);
    procedure DeleteQuery(Q: TObject);
    procedure DeleteReport(RDId: Integer);
    procedure Reset;
  end;

var
  FindExprFm: TFindExprFm;

procedure ShowFindExprForm;

implementation

uses
  apputils, formmanager, designerframe, exprform, propdialogs, filltableform,
  calcfieldsform, coloringform, dxreports, reportmanager, reportform,
  querycalcform, querycoloringform, reportsform, dximages, dxfiles, sqlform;

{$R *.lfm}

function ComponentPropertyToString(Value: TComponentProperty): String;
begin
  case Value of
    cpExpression: Result := rsExpression;
    cpDefaultValue: Result := rsDefaultValue;
    cpCheckValue: Result := rsCheckValue;
    cpListFilter: Result := rsListFilter;
    cpFillTable: Result := rsFillTable;
    cpCalcField: Result := rsCalcFields;
    cpColoring: Result := rsColoring;
    cpSourceFilter: Result := rsSourceFilter;
    cpOutFilter: Result := rsOutputFilter;
    cpWhenPrinting: Result := rsWhenPrinting;
    cpSQL: Result := rsSQLExpression;
    else Result := '';
  end;
end;

procedure ShowFindExprForm;
begin
  if FindExprFm = nil then
    FindExprFm := TFindExprFm.Create(Application);
  FindExprFm.ShowForm;
end;

{ TFindExprFm }

procedure TFindExprFm.FindBnClick(Sender: TObject);
begin
  FindInForms;
  FindInQueries;
  FindInReports;
  FmNothingPan.Visible := (FmGrid.RowCount = 1) and AnyChecked(FmFilter);
  QNothingPan.Visible := (QGrid.RowCount = 1) and AnyChecked(QFilter);
  RpNothingPan.Visible := (RpGrid.RowCount = 1) and AnyChecked(RpFilter);
  UpdateTabCaptions;
end;

procedure TFindExprFm.FilterBnChange(Sender: TObject);
begin
  if FilterBn.Checked then
  begin
    FilterPan.Height := ScaleToScreen(160);
    FilterPan.Enabled := True;
  end
  else
  begin
    FilterPan.Height := 0;
    FilterPan.Enabled := False;
  end;
end;

procedure TFindExprFm.FmGridDblClick(Sender: TObject);
var
  P: TPoint;
begin
  with FmGrid do
    P := MouseToCell(ScreenToClient(Mouse.CursorPos));
  if (P.y > 0) and (P.x > 0) then ShowFormResult;
end;

procedure TFindExprFm.FmGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  x, y: Integer;
begin
  if aRow = 0 then Exit;
  if (aCol = 2) and (FmGrid.Objects[aCol, aRow] <> nil) then
  begin
    x := aRect.Left + aRect.Width div 2 - ScaleToScreen(8);
    y := aRect.Top + aRect.Height div 2 - ScaleToScreen(8);
    FImages.Draw(FmGrid.Canvas, x, y, PtrInt(FmGrid.Objects[aCol, aRow]) - 1);
  end
end;

procedure TFindExprFm.FmGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then ShowFormResult;
end;

procedure TFindExprFm.FmGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  cp: TComponentProperty;
begin
  if aRow = 0 then Exit;
  if aCol = 3 then
  begin
    cp := TComponentProperty(PtrInt(FmGrid.Objects[4, aRow]));
    if cp = cpColoring then
    begin
      if aState * [gdFocused, gdSelected] = [] then
        FmGrid.Canvas.Brush.Color := TColor(PtrInt(FmGrid.Objects[aCol, aRow]));
    end;
  end;
end;

procedure TFindExprFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.MainForm.SetFocus;
end;

procedure TFindExprFm.FormCreate(Sender: TObject);
begin
  PopupParent := Application.MainForm;
  Caption := rsFindExpressions;

  Label1.Caption := rsValue;
  ValueEd.TextHint := rsAllExpressions;
  FindBn.Caption := rsFind;
  SetupBitBtn(FindBn, 'find16');
  FilterBn.Caption := rsFilter;
  with FmFilter do
  begin
    Caption := rsForms;
    Items.Clear;
    Items.AddStrings([rsExpression, rsDefaultValue, rsCheckValue,
      rsListFilter, rsFillTable, rsCalcFields, rsColoring, rsLabel, rsText,
      rsNumber, rsDate, rsTime, rsCounter, rsMemo, rsCheckBox, rsList, rsObject,
      rsImage, rsFile]);
  end;
  CheckAll(FmFilter);
  with QFilter do
  begin
    Caption := rsQueries;
    Items.Clear;
    Items.AddStrings([rsSourceFilter, rsOutputFilter, rsCalcFields, rsColoring,
      rsSQLExpression]);
  end;
  CheckAll(QFilter);
  with RpFilter do
  begin
    Caption := rsReports;
    Items.Clear;
    Items.AddStrings([rsSourceFilter, rsOutputFilter, rsCalcFields, rsColoring,
      rsWhenPrinting, rsSQLExpression]);
  end;
  CheckAll(RpFilter);
  FilterPan.Height := 0;
  FilterPan.Enabled := False;
  FmTab.Caption := rsForms;
  QTab.Caption := rsQueries;
  RpTab.Caption := rsReports;
  with FmGrid do
  begin
    FocusRectVisible := False;
    AllowOutboundEvents := False;
    Cells[0, 0] := '#';
    Columns[0].Title.Caption := rsForm;
    Columns[2].Title.Caption := rsComponent;
    Columns[3].Title.Caption := rsProperty;
    Columns[4].Title.Caption := rsExpression;
  end;
  with QGrid do
  begin
    FocusRectVisible := False;
    AllowOutboundEvents := False;
    Cells[0, 0] := '#';
    Columns[0].Title.Caption := rsForm;
    Columns[1].Title.Caption := rsQuery;
    Columns[2].Title.Caption := rsProperty;
    Columns[3].Title.Caption := rsSource + '/' + rsField;
    Columns[4].Title.Caption := rsExpression;
  end;
  with RpGrid do
  begin
    FocusRectVisible := False;
    AllowOutboundEvents := False;
    Cells[0, 0] := '#';
    Columns[0].Title.Caption := rsReport;
    Columns[1].Title.Caption := rsProperty;
    Columns[2].Title.Caption := rsSource + '/' + rsField;
    Columns[3].Title.Caption := rsExpression;
  end;
  FmNothingPan.Caption := rsNothingFound;
  QNothingPan.Caption := rsNothingFound;
  RpNothingPan.Caption := rsNothingFound;

  FImages := TImageList.Create(Self);
  SetupImageList(FImages, ['label16', 'text16', 'calc16', 'date16', 'clock16',
    'counter16', 'memo16', 'checkbox16', 'combobox16', 'object16', 'dbimage16',
    'file16', 'calcfield16']);
end;

procedure TFindExprFm.FormDestroy(Sender: TObject);
begin
  // При выходе из программы может быть AV, если не обнилить указатель.
  FindExprFm := nil;
end;

procedure TFindExprFm.FormShow(Sender: TObject);
begin
  ValueEd.SetFocus;
end;

procedure TFindExprFm.GridCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  HintText:=StringReplace(HintText, '|', '/', [rfReplaceAll]);
end;

procedure TFindExprFm.GridEnter(Sender: TObject);
begin
  TStringGrid(Sender).SelectedColor := clHighlight;
end;

procedure TFindExprFm.GridExit(Sender: TObject);
begin
  TStringGrid(Sender).SelectedColor := clSilver;
end;

procedure TFindExprFm.QGridDblClick(Sender: TObject);
var
  P: TPoint;
begin
  with QGrid do
    P := MouseToCell(ScreenToClient(Mouse.CursorPos));
  if (P.y > 0) and (P.x > 0) then ShowQueryResult;
end;

procedure TFindExprFm.QGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then ShowQueryResult;
end;

procedure TFindExprFm.QGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  cp: TComponentProperty;
begin
  if aRow = 0 then Exit;
  if aCol = 4 then
  begin
    cp := TComponentProperty(PtrInt(QGrid.Objects[3, aRow]));
    if cp = cpColoring then
    begin
      if aState * [gdFocused, gdSelected] = [] then
        QGrid.Canvas.Brush.Color := TColor(PtrInt(QGrid.Objects[aCol, aRow]));
    end;
  end;
end;

procedure TFindExprFm.RpGridDblClick(Sender: TObject);
var
  P: TPoint;
begin
  with RpGrid do
    P := MouseToCell(ScreenToClient(Mouse.CursorPos));
  if (P.y > 0) and (P.x > 0) then ShowReportResult;
end;

procedure TFindExprFm.RpGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then ShowReportResult;
end;

procedure TFindExprFm.RpGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  cp: TComponentProperty;
begin
  if aRow = 0 then Exit;
  if aCol = 3 then
  begin
    cp := TComponentProperty(PtrInt(RpGrid.Objects[2, aRow]));
    if cp = cpColoring then
    begin
      if aState * [gdFocused, gdSelected] = [] then
        RpGrid.Canvas.Brush.Color := TColor(PtrInt(RpGrid.Objects[aCol, aRow]));
    end;
  end;
end;

procedure TFindExprFm.ValueEdKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then FindBn.Click;
end;

procedure TFindExprFm.CheckAll(Group: TCheckGroup);
var
  i: Integer;
begin
  with Group do
    for i := 0 to Items.Count - 1 do
      Checked[i] := True;
end;

function TFindExprFm.AnyChecked(Group: TCheckGroup): Boolean;
var
  i: Integer;
begin
  Result := False;
  with Group do
    for i := 0 to Items.Count - 1 do
      if Checked[i] then Exit(True);
end;

procedure TFindExprFm.ShowFormResult;
var
  c, r, i: Integer;
  cp: TComponentProperty;
  Fm, SrcTbl: TdxForm;
  Ctrl: TControl;
  Expr, Tmp: String;
begin
  if FmGrid.RowCount = 1 then Exit;
  c := FmGrid.Col;
  r := FmGrid.Row;
  cp := TComponentProperty(PtrInt(FmGrid.Objects[4, r]));
  Fm := TdxForm(FmGrid.Objects[1, r]);
  if Fm <> nil then
    DesignFr.FormsTreeView.SelectForm(Fm);
  if (c > 2) and (cp <> cpColoring) then
  begin
    Ctrl := TControl(FmGrid.Objects[3, r]);
    if Ctrl <> nil then
    begin
      DesignFr.CompTree.SelectComponents([Ctrl]);
      DesignFr.SelectControl(Ctrl, False);
    end;
  end;
  if c > 3 then
  begin
    case cp of
      cpExpression: if ShowExprDlg(Ctrl) = mrOk then FmGrid.Cells[5, r] := GetExpression(Ctrl);
      cpDefaultValue: if DefaultValueDlg(Ctrl) = mrOk then FmGrid.Cells[5, r] := GetDefaultValue(Ctrl);
      cpCheckValue: if ShowCheckExprDlg(Ctrl) = mrOk then FmGrid.Cells[5, r] := GetCheckExpression(Ctrl);
      cpListFilter: if LookupFilterDlg(Ctrl) = mrOk then FmGrid.Cells[5, r] := GetComboFilter(Ctrl);
      cpFillTable:
        if c > 4 then
          with TdxLookupComboBox(Ctrl) do
          begin
            Expr := FillFilter;
            SrcTbl := FormMan.FindForm(SourceTable);
            if SrcTbl <> nil then
              if ShowExprForm(etSourceTableFilter, nil, Expr, SrcTbl, nil, nil, nil) = mrOk then
              begin
                FillFilter := Expr;
                FmGrid.Cells[5, r] := Expr;
              end;
          end
        else
        begin
          if ShowFillTableForm(TdxLookupComboBox(Ctrl)) = mrOk then
            FmGrid.Cells[5, r] := TdxLookupComboBox(Ctrl).FillFilter;
        end;
      cpCalcField:
        begin
          i := Fm.CalcFields.IndexOfName(FmGrid.Cells[3, r]);
          if c > 4 then
          begin
            if i >= 0 then
            begin
              Expr := Fm.CalcFields.ValueFromIndex[i];
              if ShowExprForm(etFormCalcField, nil, Expr, Fm, nil, nil, nil) = mrOk then
              begin
                Fm.CalcFields.ValueFromIndex[i] := Expr;
                FmGrid.Cells[5, r] := Expr;
              end;
            end;
          end
          else
            ShowCalcFieldsForm(Fm, i + 1);
        end;
      cpColoring:
        begin
          Tmp := ColorToString(TColor(PtrInt(FmGrid.Objects[3, r])));
          i := Fm.Coloring.IndexOf(Tmp + ';' + FmGrid.Cells[5, r]);
          if c = 4 then ShowColoringForm(Fm, i + 1)
          else if c = 5 then
          begin
            if i >= 0 then
            begin
              Expr := FmGrid.Cells[5, r];
              if ShowExprForm(etColoring, nil, Expr, Fm, nil, nil, nil) = mrOk then
              begin
                Fm.Coloring[i] := Tmp + ';' + Expr;
                FmGrid.Cells[5, r] := Expr;
              end;
            end;
          end;
        end;
    end;
  end;
end;

procedure TFindExprFm.ShowQueryResult;
var
  c, r, i: Integer;
  cp: TComponentProperty;
  Fm, SrcFm, SrcTbl: TdxForm;
  RD: TReportData;
  pSr: PRpSource;
  Expr, Tmp: String;
  Ctrl: TdxQueryGrid;
  CF: PRpCalcField;
  Column: TRpGridColumn;
begin
  if QGrid.RowCount = 1 then Exit;
  c := QGrid.Col;
  r := QGrid.Row;
  cp := TComponentProperty(PtrInt(QGrid.Objects[3, r]));
  Fm := TdxForm(QGrid.Objects[1, r]);
  DesignFr.FormsTreeView.SelectForm(Fm);
  if c > 1 then
  begin
    Ctrl := TdxQueryGrid(QGrid.Objects[2, r]);
    DesignFr.CompTree.SelectComponents([Ctrl]);
    DesignFr.SelectControl(Ctrl, False);
  end;
  if c > 2 then
  begin
    RD := ReportMan.FindReport(Ctrl.Id);
    case cp of
      cpSourceFilter:
        begin
          i := StrToInt(QGrid.Cells[4, r]);
          if c > 4 then
          begin
            if RD.Sources.Count >= i then
            begin
              pSr := RD.Sources[i - 1];
              Expr := pSr^.Filter;
              SrcFm := FormMan.FindForm(pSr^.Id);
              SrcTbl := FormMan.FindForm(pSr^.TId);
              if ShowExprForm(etSourceFilter, Ctrl, Expr, Fm, SrcTbl, SrcFm, nil) = mrOk then
              begin
                pSr^.Filter := Expr;
                QGrid.Cells[5, r] := Expr;
              end;
            end;
          end
          else
            ShowReportForm(RD, Fm, Ctrl, True, i);
        end;
      cpOutFilter:
        begin
          if QueryFilterDlg(Ctrl) = mrOk then
            QGrid.Cells[5, r] := RD.Filter;
        end;
      cpCalcField:
        begin
          CF := RD.CalcFields.FindFieldByName(QGrid.Cells[4, r]);
          if c > 4 then
          begin
            if CF <> nil then
            begin
              Expr := CF^.Expr;
              if ShowExprForm(etRpCalcField, Ctrl, Expr, Fm, nil, nil, RD) = mrOk then
              begin
                CF^.Expr := Expr;
                QGrid.Cells[5, r] := Expr;
              end;
            end;
          end
          else
            ShowCalcForm(RD, Fm, Ctrl, True, RD.CalcFields.IndexOf(CF) + 1);
        end;
      cpColoring:
        begin
          Column := RD.Grid.FindColumnByTitle(QGrid.Cells[4, r]);
          if Column <> nil then Tmp := Column.FieldNameDS
          else Tmp := '';
          i := RD.Coloring.FindColoringIndex(TColor(PtrInt(QGrid.Objects[4, r])), Tmp,
            QGrid.Cells[5, r]);
          if c < 5 then
            ShowQueryColoringForm(RD, Fm, i + 1)
          else if i >= 0 then
          begin
            Expr := RD.Coloring[i].Expr;
            if ShowExprForm(etColoring, nil, Expr, Fm, nil, nil, RD) = mrOk then
            begin
              RD.Coloring[i].Expr := Expr;
              QGrid.Cells[5, r] := Expr;
            end;
          end;
        end;
      cpSQL:
        ShowReportForm(RD, Fm, Ctrl, True, 0);
    end;
  end;
end;

procedure TFindExprFm.ShowReportResult;
var
  c, r, i: Integer;
  cp: TComponentProperty;
  Fm, SrcFm, SrcTbl: TdxForm;
  RD: TReportData;
  pSr: PRpSource;
  Expr, Tmp: String;
  CF: PRpCalcField;
  Column: TRpGridColumn;
begin
  if RpGrid.RowCount = 1 then Exit;
  c := RpGrid.Col;
  r := RpGrid.Row;
  cp := TComponentProperty(PtrInt(RpGrid.Objects[2, r]));
  RD := TReportData(RpGrid.Objects[1, r]);
  if c = 1 then
    ShowReportsForm(True, RD);
  if c > 1 then
  begin
    case cp of
      cpSourceFilter:
        begin
          i := StrToInt(RpGrid.Cells[3, r]);
          if c > 3 then
          begin
            if RD.Sources.Count >= i then
            begin
              pSr := RD.Sources[i - 1];
              Expr := pSr^.Filter;
              SrcFm := FormMan.FindForm(pSr^.Id);
              SrcTbl := FormMan.FindForm(pSr^.TId);
              if ShowExprForm(etSourceFilter, nil, Expr, nil, SrcTbl, SrcFm, nil) = mrOk then
              begin
                pSr^.Filter := Expr;
                RpGrid.Cells[4, r] := Expr;
              end;
            end;
          end
          else
            ShowReportForm(RD, nil, nil, True, i);
        end;
      cpOutFilter:
        begin
          Expr := RD.Filter;
          if ShowExprForm(etOutputFilter, nil, Expr, nil, nil, nil, RD) = mrOk then
          begin
            RD.Filter := Expr;
            RpGrid.Cells[4, r] := Expr;
          end;
        end;
      cpCalcField:
        begin
          CF := RD.CalcFields.FindFieldByName(RpGrid.Cells[3, r]);
          if c > 3 then
          begin
            if CF <> nil then
            begin
              Expr := CF^.Expr;
              if ShowExprForm(etRpCalcField, nil, Expr, nil, nil, nil, RD) = mrOk then
              begin
                CF^.Expr := Expr;
                RpGrid.Cells[4, r] := Expr;
              end;
            end;
          end
          else
            ShowCalcForm(RD, nil, nil, True, RD.CalcFields.IndexOf(CF) + 1);
        end;
      cpColoring:
        begin
          Column := RD.Grid.FindColumnByTitle(RpGrid.Cells[3, r]);
          if Column <> nil then Tmp := Column.FieldNameDS
          else Tmp := '';
          i := RD.Coloring.FindColoringIndex(TColor(PtrInt(RpGrid.Objects[3, r])), Tmp,
            RpGrid.Cells[4, r]);
          if c < 4 then
            ShowQueryColoringForm(RD, nil, i + 1)
          else if i >= 0 then
          begin
            Expr := RD.Coloring[i].Expr;
            if ShowExprForm(etColoring, nil, Expr, nil, nil, nil, RD) = mrOk then
            begin
              RD.Coloring[i].Expr := Expr;
              RpGrid.Cells[4, r] := Expr;
            end;
          end;
        end;
      cpWhenPrinting:
        begin
          Fm := CreateReportForm(RD, Tmp);
          i := RD.PrintFields.IndexOfName(RpGrid.Cells[3, r]);
          if c > 3 then
          begin
            if i >= 0 then
            begin
              Expr := RD.PrintFields.ValueFromIndex[i];
              if ShowExprForm(etFormCalcField, nil, Expr, Fm, nil, nil, nil) = mrOk then
              begin
                RD.PrintFields.ValueFromIndex[i] := Expr;
                RpGrid.Cells[4, r] := Expr;
              end;
            end;
          end
          else
          begin
            if ShowReportPrintFieldsForm(Fm, i + 1) = mrOk then
              RD.PrintFields.Assign(Fm.CalcFields)
          end;
          Fm.Free;
        end;
      cpSQL:
        ShowReportForm(RD, nil, nil, True, 0);
    end;
  end;
end;

procedure TFindExprFm.FindInForms;
var
  i, j, ImgIndex: Integer;
  Fm: TdxForm;
  C: TComponent;
  Expr, V: String;
  Clr: TColor;
  FL, CL: TStringListUtf8;

  procedure AddResult(CompProp: TComponentProperty);
  var
    r: Integer;
  begin
    with FmGrid do
    begin
      r := RowCount;
      RowCount := r + 1;
      Cells[1, r] := Fm.FormCaption;
      Objects[1, r] := Fm;
      Objects[2, r] := TObject(PtrInt(ImgIndex));
      if CompProp = cpCalcField then
        Cells[3, r] := Fm.CalcFields.Names[j]
      else if CompProp = cpColoring then
        Objects[3, r] := TObject(PtrInt(Clr))
      else
      begin
        Cells[3, r] := CL[j];
        {if C is TdxLabel then
          Cells[3, r] := TdxLabel(C).FieldName
        else
          Cells[3, r] := GetFieldName(C); }
        Objects[3, r] := C;
      end;
      Cells[4, r] := ComponentPropertyToString(CompProp);
      Objects[4, r] := TObject(PtrInt(CompProp));
      Cells[5, r] := Expr;
    end;
  end;

begin
  FmGrid.RowCount := 1;
  if not AnyChecked(FmFilter) then Exit;
  V := Utf8LowerCase(ValueEd.Text);
  CL := TStringListUtf8.Create;
  FL := TStringListUtf8.Create;
  FormMan.AllFormsToList(FL);
  for i := 0 to FL.Count - 1 do
  begin
    Fm := TdxForm(FL.Objects[i]);
    CL.Clear;
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if HasFId(C) then
        CL.AddObject(GetFieldName(C), C)
      else if (C is TdxLabel) and (TdxLabel(C).FieldName <> '') then
        CL.AddObject(TdxLabel(C).FieldName, C);
    end;
    CL.Sort;
    for j := 0 to CL.Count - 1 do
    begin
      C := TComponent(CL.Objects[j]);
      if (C is TdxLabel) and FmFilter.Checked[7] then ImgIndex := 1
      else if (C is TdxEdit) and FmFilter.Checked[8] then ImgIndex := 2
      else if (C is TdxCalcEdit) and FmFilter.Checked[9] then ImgIndex := 3
      else if (C is TdxDateEdit) and FmFilter.Checked[10] then ImgIndex := 4
      else if (C is TdxTimeEdit) and FmFilter.Checked[11] then ImgIndex := 5
      else if (C is TdxCounter) and FmFilter.Checked[12] then ImgIndex := 6
      else if (C is TdxMemo) and FmFilter.Checked[13] then ImgIndex := 7
      else if (C is TdxCheckBox) and FmFilter.Checked[14] then ImgIndex := 8
      else if (C is TdxComboBox) and FmFilter.Checked[15] then ImgIndex := 9
      else if (C is TdxLookupComboBox) and FmFilter.Checked[16] then ImgIndex := 10
      else if (C is TdxDBImage) and FmFilter.Checked[17] then ImgIndex := 11
      else if (C is TdxFile) and FmFilter.Checked[18] then ImgIndex := 12
      else Continue;
      if FmFilter.Checked[0] then
      begin
        Expr := GetExpression(C);
        if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then AddResult(cpExpression);
      end;
      if FmFilter.Checked[1] then
      begin
        Expr := GetDefaultValue(C);
        if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then AddResult(cpDefaultValue);
      end;
      if FmFilter.Checked[2] then
      begin
        Expr := GetCheckExpression(C);
        if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then AddResult(cpCheckValue);
      end;
      if FmFilter.Checked[3] then
      begin
        Expr := GetComboFilter(C);
        if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then AddResult(cpListFilter);
      end;
      if FmFilter.Checked[4] then
      begin
        if C is TdxLookupComboBox then
        begin
          Expr := TdxLookupComboBox(C).FillFilter;
          if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then AddResult(cpFillTable);
        end;
      end;
    end;

    if FmFilter.Checked[5] then
    begin
      ImgIndex := 13;
      for j := 0 to Fm.CalcFields.Count - 1 do
      begin
        Expr := Fm.CalcFields.ValueFromIndex[j];
        if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then AddResult(cpCalcField);
      end;
    end;

    if FmFilter.Checked[6] then
    begin
      ImgIndex := 0;
      for j := 0 to Fm.Coloring.Count - 1 do
      begin
        Expr := Fm.Coloring[j];
        Clr := StringToColor(Copy(Expr, 1, Pos(';', Expr) - 1));
        Delete(Expr, 1, Pos(';', Expr));
        if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then AddResult(cpColoring);
      end;
    end;
  end;
  FL.Free;
  CL.Free;
end;

procedure TFindExprFm.FindInQueries;
var
  i, j, z: Integer;
  Fm: TdxForm;
  C: TComponent;
  RD: TReportData;
  Src: TRpSource;
  Expr: String;
  V: TCaption;
  CF: TRpCalcField;
  CD: TRpColoringData;
  FL, CL: TStringListUtf8;

  procedure AddResult(cp: TComponentProperty);
  var
    r: Integer;
    Column: TRpGridColumn;
  begin
    with QGrid do
    begin
      r := RowCount;
      RowCount := r + 1;
      Cells[1, r] := Fm.FormCaption;
      Objects[1, r] := Fm;
      Cells[2, r] := RD.Name;
      Objects[2, r] := C;
      Cells[3, r] := ComponentPropertyToString(cp);
      Objects[3, r] := TObject(PtrInt(cp));
      if cp = cpSourceFilter then
        Cells[4, r] := IntToStr(z + 1)
      else if cp = cpCalcField then
        Cells[4, r] := CF.Name
      else if cp = cpColoring then
      begin
        Column := RD.Grid.FindColumnByFieldName(CD.FieldNameDS);
        if Column <> nil then
          Cells[4, r] := Column.Caption;
        Objects[4, r] := TObject(PtrInt(CD.Color));
      end;
      Cells[5, r] := Expr;
    end;
  end;

begin
  QGrid.RowCount := 1;
  if not AnyChecked(QFilter) then Exit;
  V := Utf8LowerCase(ValueEd.Text);
  FL := TStringListUtf8.Create;
  CL := TStringListUtf8.Create;
  FormMan.AllFormsToList(FL);
  for i := 0 to FL.Count - 1 do
  begin
    Fm := TdxForm(FL.Objects[i]);
    CL.Clear;
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxQueryGrid then
      begin
        RD := ReportMan.FindReport(GetId(C));
        CL.AddObject(RD.Name, C);
      end;
    end;
    CL.Sort;
    for j := 0 to CL.Count - 1 do
    begin
      C := TComponent(CL.Objects[j]);
      RD := ReportMan.FindReport(GetId(C));
      if QFilter.Checked[0] then
        for z := 0 to RD.Sources.Count - 1 do
        begin
          Src := RD.Sources[z]^;
          Expr := Src.Filter;
          if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then
            AddResult(cpSourceFilter);
        end;
      if QFilter.Checked[1] then
      begin
        Expr := RD.Filter;
        if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then
            AddResult(cpOutFilter);
      end;
      if QFilter.Checked[2] then
        for z := 0 to RD.CalcFields.Count - 1 do
        begin
          CF := RD.CalcFields[z]^;
          Expr := CF.Expr;
          if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then
            AddResult(cpCalcField);
        end;
      if QFilter.Checked[3] then
        for z := 0 to RD.Coloring.Count - 1 do
        begin
          CD := RD.Coloring[z];
          Expr := CD.Expr;
          if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then
            AddResult(cpColoring);
        end;
      if QFilter.Checked[4] then
      begin
        Expr := RD.SQL;
        if Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0 then
          AddResult(cpSQL);
      end;
    end;
  end;
  FL.Free;
  CL.Free;
end;

procedure TFindExprFm.FindInReports;
var
  i, j: Integer;
  RD: TReportData;
  V, Expr: String;
  CF: TRpCalcField;
  Src: TRpSource;
  CD: TRpColoringData;
  RL: TStringListUtf8;

  procedure AddResult(cp: TComponentProperty);
  var
    r: Integer;
    Column: TRpGridColumn;
  begin
    with RpGrid do
    begin
      r := RowCount;
      RowCount := r + 1;
      Objects[0, r] := TObject(PtrInt(RD.Id));
      Cells[1, r] := RD.Name;
      Objects[1, r] := RD;
      Cells[2, r] := ComponentPropertyToString(cp);
      Objects[2, r] := TObject(PtrInt(cp));
      if cp = cpSourceFilter then
        Cells[3, r] := IntToStr(j + 1)
      else if cp = cpCalcField then
        Cells[3, r] := CF.Name
      else if cp = cpColoring then
      begin
        Column := RD.Grid.FindColumnByFieldName(CD.FieldNameDS);
        if Column <> nil then
          Cells[3, r] := Column.Caption;
        Objects[3, r] := TObject(PtrInt(CD.Color));
      end
      else if cp = cpWhenPrinting then
        Cells[3, r] := RD.PrintFields.Names[j];
      Cells[4, r] := Expr;
    end;
  end;

begin
  RpGrid.RowCount := 1;
  if not AnyChecked(RpFilter) then Exit;
  V := Utf8LowerCase(ValueEd.Text);
  RL := TStringListUtf8.Create;
  ReportMan.GetReports(RL);
  for i := 0 to RL.Count - 1 do
  begin
    RD := TReportData(RL.Objects[i]);
    if RpFilter.Checked[0] then
      for j := 0 to RD.Sources.Count - 1 do
      begin
        Src := RD.Sources[j]^;
        Expr := Src.Filter;
        if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then
          AddResult(cpSourceFilter);
      end;
    if RpFilter.Checked[1] then
    begin
      Expr := RD.Filter;
      if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then
        AddResult(cpOutFilter);
    end;
    if RpFilter.Checked[2] then
      for j := 0 to RD.CalcFields.Count - 1 do
      begin
        CF := RD.CalcFields[j]^;
        Expr := CF.Expr;
        if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then
          AddResult(cpCalcField);
      end;
    if RpFilter.Checked[3] then
      for j := 0 to RD.Coloring.Count - 1 do
      begin
        CD := RD.Coloring[j];
        Expr := CD.Expr;
        if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then
          AddResult(cpColoring);
      end;
    if RpFilter.Checked[4] then
      for j := 0 to RD.PrintFields.Count - 1 do
      begin
        Expr := RD.PrintFields.ValueFromIndex[j];
        if ((V = '') and (Trim(Expr) <> '')) or (Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0) then
          AddResult(cpWhenPrinting);
      end;
    if RpFilter.Checked[5] then
    begin
      Expr := RD.SQL;
      if Utf8Pos(V, Utf8LowerCase(Expr), 1) > 0 then
        AddResult(cpSQL);
    end;
  end;
  RL.Free;
end;

procedure TFindExprFm.UpdateTabCaptions;
var
  n: Integer;
begin
  n := FmGrid.RowCount - 1;
  if n > 0 then
    FmTab.Caption := rsForms + ' (' + IntToStr(n) + ')'
  else
    FmTab.Caption := rsForms;

  n := QGrid.RowCount - 1;
  if n > 0 then
    QTab.Caption := rsQueries + ' (' + IntToStr(n) + ')'
  else
    QTab.Caption := rsQueries;

  n := RpGrid.RowCount - 1;
  if n > 0 then
    RpTab.Caption := rsReports + ' (' + IntToStr(n) + ')'
  else
    RpTab.Caption := rsReports;
end;

procedure TFindExprFm.ShowForm;
begin
  FmNothingPan.Hide;
  QNothingPan.Hide;
  RpNothingPan.Hide;
  FmGrid.SelectedColor := clSilver;
  QGrid.SelectedColor := clSilver;
  RpGrid.SelectedColor := clSilver;
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  Show;
end;

procedure TFindExprFm.DeleteForm(Fm: TdxForm);
var
  i: Integer;
begin
  with FmGrid do
    for i := RowCount - 1 downto 1 do
      if (Fm = Objects[1, i]) or (Fm.Id = TdxForm(Objects[1, i]).PId) then DeleteRow(i);
  with QGrid do
    for i := RowCount - 1 downto 1 do
      if (Fm = Objects[1, i]) or (Fm.Id = TdxForm(Objects[1, i]).PId) then DeleteRow(i);
  UpdateTabCaptions;
end;

procedure TFindExprFm.DeleteComponent(C: TObject);
var
  i: Integer;
begin
  with FmGrid do
    for i := RowCount - 1 downto 1 do
      if C = Objects[3, i] then DeleteRow(i);
  UpdateTabCaptions;
end;

procedure TFindExprFm.DeleteQuery(Q: TObject);
var
  i: Integer;
begin
  with QGrid do
    for i := RowCount - 1 downto 1 do
      if Q = Objects[2, i] then DeleteRow(i);
  UpdateTabCaptions;
end;

procedure TFindExprFm.DeleteReport(RDId: Integer);
var
  i: Integer;
begin
  with RpGrid do
    for i := RowCount - 1 downto 1 do
      if RDId = PtrInt(Objects[0, i]) then DeleteRow(i);
  UpdateTabCaptions;
end;

procedure TFindExprFm.Reset;
begin
  FmGrid.RowCount := 1;
  QGrid.RowCount := 1;
  RpGrid.RowCount := 1;
  UpdateTabCaptions;
  FmNothingPan.Visible := False;
  QNothingPan.Visible := False;
  RpNothingPan.Visible := False;
  ValueEd.Text := '';
end;

end.

