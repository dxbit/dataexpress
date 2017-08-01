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
unit ReportForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, DXReports, DxCtrls, strconsts, editbtn, StdCtrls, Menus,
  ExtCtrls, mydialogs;

type

  { TReportFm }

  TReportFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DateFl: TComboBox;
    ImageList1: TImageList;
    Grid: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
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
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure BtnClick(Sender: TObject);
    procedure DateFlDropDown(Sender: TObject);
    procedure ExprEditingDone(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridPickListSelect(Sender: TObject);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
  private
    { private declarations }
    FRD: TReportData;
    FFm: TdxForm;
    FExprEd: TEditButton;
    FMaxFId: Integer;
    FSelFieldFm: TSelectFieldForm;
    function FindFirstCell(r: Integer): Integer;
    function FindFirstCmp(r: Integer): TComponent;
    procedure Load;
    procedure Save;
    procedure SetControlState;
    procedure FillForms(L: TStrings);
    procedure FillTables(L: TStrings);
    procedure FillIncOut(L: TStrings);
    procedure FillFuncs(L: TStrings);
    procedure FillDateFl;
    function FindDateFieldIndex(const S: String): Integer;
    function Validate: Boolean;
    procedure DoSelectField;
  public
    { public declarations }
    function ShowForm(RD: TReportData; aForm: TdxForm): Integer;
  end;

var
  ReportFm: TReportFm;

implementation

uses
  formmanager, apputils, exprform, LazUtf8, helpform;

{$R *.lfm}

{ TReportFm }

function ExtractFieldName(S: String): String;
{var
  SL: TStringList;  }
begin
  if S = '' then Exit('');
  if Copy(S, 1, 1) = '!' then Delete(S, 1, 1);
  Result := StringReplace(S, '|', '_', [rfReplaceAll]);
  {SL := TStringList.Create;
  SplitStr(S, '|', SL);
  Result := SL[SL.Count - 1];
  SL.Free; }
end;

procedure TReportFm.DoSelectField;
var
  Fm, Tbl: TdxForm;
  C: TComponent;
  S: String;
begin
  Fm := TdxForm(Grid.Objects[Grid.Col, 1]);
  Tbl := TdxForm(Grid.Objects[Grid.Col, 2]);
  if (Fm = nil) and (Tbl = nil) then Exit;
  with FSelFieldFm do
  begin
    C := TComponent(Grid.Objects[Grid.Col, Grid.Row]);
    S := Grid.Cells[Grid.Col, Grid.Row];
    if ShowForm2(Tbl, Fm, C, S) = mrOk then
    begin
      S := FieldName;
    	Grid.Cells[Grid.Col, Grid.Row] := S;
      Grid.Objects[Grid.Col, Grid.Row] := Component;
      if Grid.Cells[Grid.ColCount - 4, Grid.Row] = '' then
      begin
        Grid.Cells[Grid.ColCount - 4, Grid.Row] := ExtractFieldName(S);
      end;
    end;
  end;
end;

procedure TReportFm.GridSelection(Sender: TObject; aCol, aRow: Integer);
var
  L: TStrings;
  //Fm, Tbl: TdxForm;
begin
  L := Grid.Columns[aCol - 1].PickList;
  L.Clear;
  if aCol in [1..Grid.ColCount - 5] then
  begin
    //Fm := TdxForm(Grid.Objects[aCol, 1]);
    //Tbl := TdxForm(Grid.Objects[aCol, 2]);
    if aRow = 1 then
      FillForms(L)
    else if aRow = 2 then
      FillTables(L)
    else if aRow = 4 then
      FillIncOut(L)
    {else if aRow >= 5 then
      FillFields(Fm, Tbl, L, [flText, flNumber, flDate, flBool, flObject, flTime,
        flCounter]);
    if aRow >= 5 then
      L.InsertObject(0, '', nil); }
  end
  else if (aCol = Grid.ColCount - 3) and (aRow >= 5) then
    FillFuncs(L);
  SetControlState;
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
  C.Width := 120;
  SetControlState;
end;

procedure TReportFm.MenuItem2Click(Sender: TObject);
begin
  if not ConfirmDelete then Exit;
  Grid.DeleteCol(Grid.Col);
  SetControlState;
end;

procedure TReportFm.MenuItem4Click(Sender: TObject);
var
  i: Integer;
begin
  i := Grid.RowCount;
  Grid.RowCount := i + 1;
  Grid.Row := i;
  Inc(FMaxFId);
  Grid.Objects[0, i] := TObject(FMaxFId);
  Grid.Cells[Grid.ColCount - 2, i] := '1';
  Grid.Cells[Grid.ColCount - 1, i] := '0';
  SetControlState;
end;

procedure TReportFm.MenuItem5Click(Sender: TObject);
begin
  if not ConfirmDelete then Exit;
  Grid.DeleteRow(Grid.Row);
  SetControlState;
end;

procedure TReportFm.MenuItem6Click(Sender: TObject);
begin
  DoSelectField;
  SetControlState;
end;

procedure TReportFm.MenuItem7Click(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := '';
  Grid.Objects[Grid.Col, Grid.Row] := nil;
  SetControlState;
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
end;

procedure TReportFm.ToolButton7Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row + 1);
  SetControlState;
end;

procedure TReportFm.ToolButton8Click(Sender: TObject);
begin
  Grid.ExchangeColRow(True, Grid.Col - 1, Grid.Col);
  SetControlState;
end;

procedure TReportFm.ToolButton9Click(Sender: TObject);
begin
  Grid.ExchangeColRow(True, Grid.Col, Grid.Col + 1);
  SetControlState;
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

function TReportFm.FindFirstCmp(r: Integer): TComponent;
var
  i: Integer;
begin
  Result := nil;
  for i := 1 to Grid.ColCount - 5 do
  begin
    if Grid.Objects[i, r] <> nil then Exit(TComponent(Grid.Objects[i, r]));
  end;
end;

procedure TReportFm.GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if (aCol >= Grid.ColCount - 4) and (aRow in [1..4]) then
    Editor := nil
  else if (aCol >= 1) and (aCol < Grid.ColCount - 4) and (aRow = 3) then
  begin
    FExprEd.BoundsRect := Grid.CellRect(aCol, aRow);
    Editor := FExprEd;
    FExprEd.Text := Grid.Cells[aCol, aRow];
  end
  else if (aCol in [1..Grid.ColCount - 5]) and (aRow >= 5) then
  	Editor := nil
  else if Editor is TPickListCellEditor then
    with TPickListCellEditor(Editor) do
    begin
      Style:=csDropDownList;
      ItemIndex := Items.IndexOfObject(Grid.Objects[aCol, aRow]);
    end;
end;

procedure TReportFm.FormCreate(Sender: TObject);
begin
  Grid.FocusColor:=Grid.SelectedColor;
  Grid.Cells[0, 1] := rsForm;
  Grid.Cells[0, 2] := rsTable;
  Grid.Cells[0, 3] := rsFilter;
  Grid.Cells[0, 4] := rsSourceType;
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
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  FExprEd := TEditButton.Create(Self);
  FExprEd.Button.Caption := '...';
  FExprEd.OnButtonClick:=@BtnClick;
  FExprEd.OnEditingDone:=@ExprEditingDone;
  ImageList1.AddLazarusResource('add16');
  ImageList1.AddLazarusResource('delete16');
  ImageList1.AddLazarusResource('up16');
  ImageList1.AddLazarusResource('down16');
  ImageList1.AddLazarusResource('left16_2');
  ImageList1.AddLazarusResource('right16_2');
  ToolButton1.Caption := rsAppendField;
  ToolButton2.Caption := rsDeleteField;
  ToolButton4.Caption := rsAppendSource;
  ToolButton5.Caption := rsDeleteSource;
  ToolButton6.Caption := rsMoveUp;
  ToolButton7.Caption := rsMoveDown;
  ToolButton8.Caption := rsLeft;
  ToolButton9.Caption := rsRight;
  FSelFieldFm := TSelectFieldForm.CreateNew(Self);
  with FSelFieldFm do
  begin
    Caption := rsSelectSourceField;
    FirstParentForm := True;
    ShowFieldsOfObject := True;
    ShowParFormPrefix := True;
  end;
end;

procedure TReportFm.FormDestroy(Sender: TObject);
begin
  //FreeAndNil(FSelFieldFm);
end;

procedure TReportFm.FormShow(Sender: TObject);
begin
  Grid.Col := 1; Grid.Row := 1;
  Grid.SetFocus;
  SetControlState;
end;

procedure TReportFm.GridCheckboxToggled(sender: TObject; aCol, aRow: Integer;
  aState: TCheckboxState);
begin
  if (aCol >= Grid.ColCount - 2) and (aRow in [1..4]) then
    Grid.Cells[aCol, aRow] := '0';
end;

procedure TReportFm.GridDblClick(Sender: TObject);
begin
  MenuItem6.Click;
  //if (Grid.Col in [1..Grid.ColCount - 5]) and (Grid.Row >= 5) then
  //	DoSelectField;
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

procedure TReportFm.GridPickListSelect(Sender: TObject);
var
  c, r, c2: Integer;
  PL: TPickListCellEditor;
  Cmp: TComponent;
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
    else if r = 2 then ClearTable
    {else if r > 4 then
    begin
      c2 := Grid.Columns.Count-3;
      if Grid.Objects[c, r] <> nil then
      begin
        if Grid.Cells[c2, r] = '' then
        begin
          Cmp := TComponent(Grid.Objects[c, r]);
          Cmp := LookupComponent(TdxForm(Cmp.Owner), Grid.Cells[c, r]);
          if Cmp <> nil then
            Grid.Cells[c2, r] := GetFieldName(Cmp);
        end;
      end
      else
        Grid.Cells[c, r] := '';
    end; }
  end
  else if (Grid.Col = Grid.ColCount - 3) and (Grid.Row >= 5) then
  begin
    Grid.Objects[Grid.Col, Grid.Row] := PL.Items.Objects[PL.ItemIndex];
  end;
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

  if ExprFm.ShowForm(rsSourceFilter, nil, S, FFm, Tbl, Fm, nil, 'sourcefilter') then
    FExprEd.Text := S;
  FExprEd.SetFocus;
end;

procedure TReportFm.DateFlDropDown(Sender: TObject);
begin
  FillDateFl;
end;

procedure TReportFm.ExprEditingDone(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := FExprEd.Text;
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
  DateFl.Clear;
  if (FRD.DateField >= 0) and (FRD.Sources.Count > 0) then
  begin
    Fl := FRD.Sources[0]^.Fields[FRD.DateField]^;
    DateFl.Text := Fl.Name;
  end;
  Period.ItemIndex := Ord(FRD.DateDetail);

  Grid.Columns.Clear;
  Grid.RowCount := 5;
  if FRD.Sources.Count > 0 then
    Grid.RowCount := FRD.Sources[0]^.Fields.Count + 5;
  for i := 0 to FRD.Sources.Count - 1 do
  begin
    c := i + 1;
    Col := Grid.Columns.Add;
    Col.Width := 120;
    Col.Title.Caption := Format(rsSourceNum, [i+1]);
    Sr := FRD.Sources[i]^;
    Fm := FormMan.FindForm(StrToInt(Sr.Id));
    Grid.Cells[c, 1] := Fm.FormCaption;
    Grid.Objects[c, 1] := Fm;
    if Sr.TId <> '' then
    begin
      Fm := FormMan.FindForm(StrToInt(Sr.TId));
      Grid.Cells[c, 2] := Fm.FormCaption;
      Grid.Objects[c, 2] := Fm;
    end;
    Grid.Cells[c, 3] := Sr.Filter;
    Grid.Cells[c, 4] := SourceKindToStr(Sr.Kind);
    Grid.Objects[c, 4] := TObject(Sr.Kind);

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
  Col.Width := 120;
  Col.Title.Caption := rsFieldName;
  // Функция
  Col := Grid.Columns.Add;
  Col.Width := 120;
  Col.Title.Caption := rsFunction;
  // Видимое
  Col := Grid.Columns.Add;
  Col.Width := 70;
  Col.Title.Caption := rsVisible;
  Col.ButtonStyle:=cbsCheckboxColumn;
  Col.Visible:=FRD.Kind = rkReport;
  // Параметр
  Col := Grid.Columns.Add;
  Col.Width := 70;
  Col.Title.Caption := rsParameter;
  Col.ButtonStyle:=cbsCheckboxColumn;
  Col.Visible:=FRD.Kind = rkReport;

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
      Grid.Objects[c-3, r] := TObject(Fl.Func);
      Grid.Cells[c-2, r] := Bool2Str(Fl.Visible);
      Grid.Cells[c-1, r] := Bool2Str(Fl.Param);

      // Id поля храним в 1 колонке. У полей разных источников Id одинаковый.
      Grid.Objects[0, r] := TObject(Fl.Id);
      // Сразу определяем максимальный Id.
      if Fl.Id > FMaxFId then FMaxFId := Fl.Id;
    end;
  end;

  if FRD.Sources.Count = 0 then
    MenuItem1.Click;
end;


procedure TReportFm.Save;
var
  i, j, c: Integer;
  pSr: PRpSource;
  pFl: PRpField;
  Cmp: TComponent;
  Col: TRpGridColumn;
  S: String;

  function NewCol(const FlName: String): TRpGridColumn;
  begin
    Result := FRD.Grid.AddColumn;
    Result.FieldName:=FlName;
    Result.Index:=FRD.Grid.ColumnCount - 1;
    Result.Width := 100;
    Result.Color:=FRD.Grid.Color;
    Result.FixedColor:=FRD.Grid.FixedColor;
    Result.Font.Assign(FRD.Grid.Font);
    Result.TitleFont.Assign(FRD.Grid.TitleFont);
  end;

  procedure RemoveTotal(Col: TRpGridColumn);
  var
    T: TRpTotalData;
  begin
    repeat
      T := FRD.Totals.FindTotal(Col.FieldName);
      if T <> nil then FRD.Totals.RemoveTotal(T);
    until T = nil;
  end;

  procedure RemoveColoring(Col: TRpGridColumn);
  var
    CD: TRpColoringData;
  begin
    repeat
      CD := FRD.Coloring.FindColoring(Col.FieldName);
      if CD <> nil then FRD.Coloring.DeleteColoring(CD);
    until CD = nil;
  end;

begin
  FRD.DateField := -1;
  if DateFl.Text <> '' then
    FRD.DateField:=FindDateFieldIndex(DateFl.TexT);
  FRD.DateDetail:=TRpDateDetail(Period.ItemIndex);

  FRD.Sources.Clear;
  for i := 1 to Grid.Columns.Count - 4 do
  begin
    FRD.Sources.AddSource(pSr);
    pSr^.Id := IntToStr(TdxForm(Grid.Objects[i, 1]).Id);
    if Grid.Objects[i, 2] <> nil then
      pSr^.TId := IntToStr(TdxForm(Grid.Objects[i, 2]).Id);
    pSr^.Filter := Grid.Cells[i, 3];
    pSr^.Kind:=TRpSourceKind(Grid.Objects[i, 4]);

    c := Grid.Columns.Count;
    for j := 5 to Grid.RowCount - 1 do
    begin
      pSr^.Fields.AddField(pFl);
      Cmp := TComponent(Grid.Objects[i, j]);
      if Cmp = nil then
      begin
        pFl^.Zero := True;
        Cmp := FindFirstCmp(j);
        if (Cmp <> nil) then pFl^.Tp := GetTypeByComponent(Cmp)
        else pFl^.Tp:=flNumber;
      end
      else
        SetupRpField(Cmp, Grid.Cells[i, j], pFl);
      pFl^.Id := Integer(Grid.Objects[0, j]);
      pFl^.Name := Grid.Cells[c - 3, j];
      pFl^.Func := TRpTotalFunc(Grid.Objects[c - 2, j]);
      pFl^.Visible:=Str2Bool(Grid.Cells[c - 1, j]);
      pFl^.Param := Str2Bool(Grid.Cells[c, j]);
    end;
  end;
  // Удаляем лишние столбцы
  for i := FRD.Grid.ColumnCount - 1 downto 0 do
  begin
    Col := FRD.Grid.Columns[i];
    S := Col.FieldName;
    // Пожинаю плоды безрассудства :)
    if (S = 'income') or (S = 'outcome') or (S = 'total') then
    begin
      FRD.Grid.DeleteColumn(Col);
      Continue;
    end
    else if Copy(S, 1, 2) = 'cf' then Continue;
    //
    Delete(S, 1, 1);
    j := StrToInt(S);
    pFl := pSr^.Fields.FindField(j);
    if (pFl = nil) or (not pFl^.Visible) then
    begin
      RemoveTotal(Col);
      RemoveColoring(Col);
      FRD.Grid.DeleteColumn(Col);
    end;
  end;
  // Добавляем новые
  pSr := FRD.Sources[0];
  for i := 0 to pSr^.Fields.Count - 1 do
  begin
    pFl := pSr^.Fields[i];
    if pFl^.Visible = False then Continue;

    S := 'f' + IntToStr(pFl^.Id);
    Col := FRD.Grid.FindColumnByFieldName(S);
    if Col = nil then
      Col := NewCol(S);
    Col.Caption := pFl^.Name;
  end;
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
end;

procedure TReportFm.FillForms(L: TStrings);
begin
  FormMan.FormsToList(L);
end;

procedure TReportFm.FillTables(L: TStrings);
var
  i, id: Integer;
  F: TdxForm;
begin
  F := TdxForm(Grid.Objects[Grid.Col, 1]);
  if F = nil then Exit;
  id := F.Id;
  L.AddObject('', nil);
  for i := 0 to FormMan.FormCount - 1 do
  begin
    F := FormMan.Forms[i];
    if F.PId = id then
      L.AddObject(F.FormCaption, F);
  end;
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
    if Utf8CompareText(Grid.Cells[Grid.ColCount - 4, i], S) = 0 then
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
  {else if (C1 = nil) and ((C2 is TdxCalcEdit) or (C2 is TdxCounter)) then
    Result := True
  else if (C2 = nil) and ((C1 is TdxCalcEdit) or (C1 is TdxCounter)) then
    Result := True    }
  else if ((C1 is TdxEdit) or (C1 is TdxComboBox) or (C1 is TdxMemo)) and
    ((C2 is TdxEdit) or (C2 is TdxComboBox) or (C2 is TdxMemo)) then
    Result := True
  else if (C1 <> nil) and (C2 <> nil) then
    Result := C1.ClassName = C2.ClassName
  else
    Result := False;
end;

function TReportFm.Validate: Boolean;
var
  i, j, n, c, si: Integer;
  C1, C2: TComponent;
  Fn: TRpTotalFunc;

  function IsBalanceExists: Boolean;
  var
    m: Integer;
  begin
    Result := False;
    for m := 5 to Grid.RowCount - 1 do
    begin
      if TRpTotalFunc(Grid.Objects[Grid.ColCount - 3, m]) = tfProfit then
        Exit(True);
    end;
  end;

  function CheckDuplicates(idx: Integer): Boolean;
  var
    i: Integer;
    S, SS: String;
  begin
    if Grid.Cells[Grid.ColCount - 2, idx] = '0' then Exit(True);

    S := Grid.Cells[Grid.ColCount - 4, idx];
    Result := CheckName(S);
    if not Result then Exit;
    for i := idx + 1 to Grid.RowCount - 1 do
    begin
      if Grid.Cells[Grid.ColCount - 2, i] = '0' then Continue;  // Пропускаем параметры
      SS := Grid.Cells[Grid.ColCount - 4, i];
      if Utf8CompareText(S, SS) = 0 then
      begin
        ErrMsg(rsDuplicateFieldName);
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
      ErrMsg(rsCalcFieldNameExists);
  end;

  function _LookupCmp(Fm: TdxForm; FlNm: String): TComponent;
  begin
    if Copy(FlNm, 1, 1) = '!' then Delete(FlNm, 1, 1);
    Result := LookupComponent(Fm, FlNm);
  end;

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
    // Вообще, для функций допустимо не выбирать поля (кроме "среднее"). В этом случае
    // вместо значения поля будет выводится 0, null или пустая строка.
    if (n = 0) and ((Grid.Objects[Grid.ColCount - 3, i] = nil) or
    	(TRpTotalFunc(Grid.Objects[Grid.ColCount - 3, i]) = tfAvg)) then
    begin
      ErrMsg(rsFieldNotSel);
      Grid.Row := i; Grid.Col := 1;
      Exit;
    end
    else
    begin
      // Если функция не выбрана, то поле должно быть выбрано для каждого источника
      {if (Grid.Objects[c - 3, i] = nil) and (n < Grid.ColCount - 5) then
      begin
        ErrMsg(rsNotAllFieldsSel);
        Grid.Row := i; Grid.Col := 1;
        Exit;
      end;  }
      // Если функция выбрана, то смотрим тип полей. Он должен быть числовой для некоторых ф-ций.
      if Grid.Objects[c - 3, i] <> nil then
      begin
        Fn := TRpTotalFunc(Grid.Objects[c - 3, i]);
        n := 0;
        for j := 1 to Grid.ColCount - 5 do
        begin
          C1 := TComponent(Grid.Objects[j, i]);
          if (C1 <> nil) and (C1 is TdxLookupComboBox) then
            C1 := _LookupCmp(TdxForm(C1.Owner), Grid.Cells[j, i]);
          if Fn in [tfSum, tfAvg, tfProfit] then
          begin
            if (C1 = nil) or (C1 is TdxCalcEdit) or (C1 is TdxCounter) then
            else
            begin
              ErrMsg(rsFieldShouldBeNum);
              Grid.Row := i; Grid.Col := j;
              Exit;
            end;
          end;
          if C1 <> nil then Inc(n);
        end;
        // Нельзя использовать это поле как параметр, если ни одного поля не выбрано.
        if (Grid.Cells[Grid.ColCount - 1, i] = '1') and (n = 0) then
        begin
          ErrMsg(rsFieldCantParam);
          Grid.Row := i; Grid.Col := 1;
          Exit;
        end;
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
        Grid.Row := i; Grid.Col := Grid.ColCount - 4;
        Exit;
      end;
      // Ищем первую заполненную ячейку в строке
      si := FindFirstCell(i);
      if si < 0 then si := 10000;
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
    if (not CheckDuplicates(i)) or (not CheckDuplicatesCalcFields(i)) then
    begin
      Grid.Row := i; Grid.Col := Grid.ColCount - 4;
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
    if Grid.Cells[Grid.ColCount - 2, n + 5] = '0' then
    begin
      ErrMsg(rsDateFieldShoulBeVisible);
      Exit;
    end;
  end;

  Result := True;
end;

function TReportFm.ShowForm(RD: TReportData; aForm: TdxForm): Integer;
begin
  if RD = nil then Exit;
  FRD := RD;
  FFm := aForm;
  Caption := rsSelectionSet + ': ' + RD.Name;
  Load;
  Result := ShowModal;
  if Result = mrOk then Save;
end;

end.

