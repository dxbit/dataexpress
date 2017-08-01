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
unit QueryCalcForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, Menus, dxreports, editbtn, strconsts, dxctrls, LclType,
  StdCtrls;

type

  { TCalcFm }

  TCalcFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TStringGrid;
    Images: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure ButtonClick(Sender: TObject);
    procedure ExprEdButtonClick(Sender: TObject);
    procedure ExprEdEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridPickListSelect(Sender: TObject);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
  private
    { private declarations }
    FExprEd: TEditButton;
    FForm: TdxForm;
    FRD: TReportData;
    FMaxFId: Integer;
    procedure EditorKeyPress(Sender: TObject; var Key: char);
    procedure SetControlState;
    procedure LoadFields;
    procedure SaveFields;
    function CheckNames: Boolean;
    function Validate: Boolean;
  public
    { public declarations }
    function ShowForm(aRD: TReportData; aForm: TdxForm): Integer;
  end;

var
  CalcFm: TCalcFm;

implementation

uses
  exprform, apputils, helpform, LazUtf8;

{$R *.lfm}

{ TCalcFm }

procedure TCalcFm.FormCreate(Sender: TObject);
begin
  Grid.Columns[0].Title.Caption:=rsName;
  Grid.Columns[1].Title.Caption:=rsFieldType;
  Grid.Columns[2].Title.Caption:=rsSizePrec;
  Grid.Columns[3].Title.Caption:=rsExpression;
  with Grid.Columns[1].PickList do
  begin
    AddObject(rsText, TObject(flText));
    AddObject(rsNumber, TObject(flNumber));
    AddObject(rsDate, TObject(flDate));
    AddObject(rsTime, TObject(flTime));
  end;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  MenuItem1.Caption:=rsAppend;
  MenuItem2.Caption := rsDelete;
  MenuItem4.Caption := rsMoveUp;
  MenuItem5.Caption := rsMoveDown;
  ToolButton1.Caption := rsAppend;
  ToolButton2.Caption := rsDelete;
  ToolButton3.Caption := rsMoveUp;
  ToolButton4.Caption := rsMoveDown;
  FExprEd := TEditButton.Create(Self);
  FExprEd.Button.Caption:='...';
  FExprEd.OnButtonClick:=@ExprEdButtonClick;
  FExprEd.OnEditingDone:=@ExprEdEditingDone;
  Images.AddLazarusResource('add16');
  Images.AddLazarusResource('delete16');
  Images.AddLazarusResource('up16');
  Images.AddLazarusResource('down16');
end;

procedure TCalcFm.ExprEdButtonClick(Sender: TObject);
var
  S: TCaption;
begin
  S := FExprEd.Text;
  if ExprFm.ShowForm(rsExpression, nil, S, FForm, nil, nil, FRD, 'expressions') then
    FExprEd.Text := S;
end;

procedure TCalcFm.ButtonClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: MenuItem1.Click;
    1: MenuItem2.Click;
    2: MenuItem4.Click;
    3: MenuItem5.Click;
  end;
end;

procedure TCalcFm.ExprEdEditingDone(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := FExprEd.Text;
end;

procedure TCalcFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Grid.Editor <> nil then Grid.EditingDone;
end;

procedure TCalcFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (ModalResult = mrCancel) and (Grid.RowCount > 1) then
    CanClose := MessageDlg(rsWarning, rsAreYouSure, mtWarning, [mbYes, mbNo], 0) = mrYes
  else if ModalResult = mrOk then
    CanClose := Validate;
end;

procedure TCalcFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
  SetControlState;
end;

procedure TCalcFm.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  K: Word;
begin
  K := Key; Key := 0;
  case K of
    VK_DELETE: if ssCtrl in Shift then MenuItem2.Click;
    else Key := K;
  end;
end;

procedure TCalcFm.GridPickListSelect(Sender: TObject);
var
  Tp: TRpFieldType;
begin
  with TPickListCellEditor(Grid.Editor) do
  begin
    Tp := TRpFieldType(Items.Objects[ItemIndex]);
    if Grid.Objects[Grid.Col, Grid.Row] <> Items.Objects[ItemIndex] then
    begin
			Grid.Objects[Grid.Col, Grid.Row] := Items.Objects[ItemIndex];
      case Tp of
        flText: Grid.Cells[2, Grid.Row] := '50';
        flNumber: Grid.Cells[2, Grid.Row] := '0';
        else Grid.Cells[2, Grid.Row] := '-';
      end;
    end;
  end;
end;

procedure TCalcFm.GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  Editor.OnKeyPress:=nil;
  if aCol = 1 then
  	TPickListCellEditor(Editor).Style:=csDropDownList
  else if aCol = 2 then
  begin
    if TRpFieldType(Grid.Objects[1, aRow]) in [flDate, flTime] then Editor := nil
    else Editor.OnKeyPress:=@EditorKeyPress;
  end
  else if aCol = 3 then
  begin
	  FExprEd.BoundsRect := Grid.CellRect(aCol, aRow);
  	FExprEd.Text := Grid.Cells[aCol, aRow];
	  Editor := FExprEd;
  end;
end;

procedure TCalcFm.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  SetControlState;
end;

procedure TCalcFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('querycalcs');
end;

procedure TCalcFm.MenuItem1Click(Sender: TObject);
var
  r: Integer;
begin
  r := Grid.RowCount;
  Grid.RowCount := r + 1;
  Grid.Row:=r;
  Inc(FMaxFId);
  Grid.Objects[0, r] := TObject(FMaxFId);
  SetControlState;
end;

procedure TCalcFm.MenuItem2Click(Sender: TObject);
begin
  if ConfirmDelete then
    Grid.DeleteRow(Grid.Row);
  SetControlState;
end;

procedure TCalcFm.MenuItem4Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row - 1);
  SetControlState;
end;

procedure TCalcFm.MenuItem5Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row + 1);
  SetControlState;
end;

procedure TCalcFm.SetControlState;
begin
  MenuItem2.Enabled := Grid.Row >= 1;
  ToolButton2.Enabled := MenuItem2.Enabled;
  MenuItem4.Enabled := Grid.Row > 1;
  ToolButton3.Enabled := MenuItem4.Enabled;
  MenuItem5.Enabled := Grid.Row < Grid.RowCount - 1;
  ToolButton4.Enabled := MenuItem5.Enabled;
end;

procedure TCalcFm.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if Key in [#8, '0'..'9'] then
  else Key := #0;
end;

procedure TCalcFm.LoadFields;
var
  i, r: Integer;
  F: TRpCalcField;
begin
  Grid.RowCount := 1;
  FMaxFId := 0;
  for i := 0 to FRD.CalcFields.Count - 1 do
  begin
    r := i + 1;
    Grid.RowCount := r + 1;
    F := FRD.CalcFields[i]^;
    Grid.Objects[0, r] := TObject(F.Id);
    Grid.Cells[0, r] := F.Name;
    Grid.Cells[1, r] := RpFieldTypeToStr(F.Tp);
    Grid.Objects[1, r] := TObject(F.Tp);
    if F.Tp in [flText, flNumber] then
	    Grid.Cells[2, r] := IntToStr(F.Size)
    else
    	Grid.Cells[2, r] := '-';
    Grid.Cells[3, r] := F.Expr;
    if F.Id > FMaxFId then FMaxFId := F.Id;
  end;
end;

procedure TCalcFm.SaveFields;
var
  i, n: Integer;
  Col: TRpGridColumn;
  S: String;
  pF: PRpCalcField;

  function NewCol(const aFlNm: String): TRpGridColumn;
  begin
    Result := FRD.Grid.AddColumn;
    Result.FieldName := aFlNm;
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
  FRD.CalcFields.Clear;
  for i := 1 to Grid.RowCount - 1 do
  begin
    FRD.CalcFields.AddField(pF);
    pF^.Id := Integer(Grid.Objects[0, i]);
    pF^.Name := Grid.Cells[0, i];
    pF^.Tp := TRpFieldType(Grid.Objects[1, i]);
    pF^.Size := 0;
    if Grid.Cells[2, i] <> '-' then pF^.Size := StrToInt(Grid.Cells[2, i]);
    pF^.Expr := Grid.Cells[3, i];
  end;
  // Удаляем лишние столбцы
  for i := FRD.Grid.ColumnCount - 1 downto 0 do
  begin
    Col := FRD.Grid.Columns[i];
    S := Col.FieldName;
    if Copy(S, 1, 2) = 'cf' then
    begin
      n := StrToInt(Copy(S, 3, 50));
      if FRD.CalcFields.FindField(n) = nil then
      begin
        RemoveTotal(Col);
        RemoveColoring(Col);
        FRD.Grid.DeleteColumn(Col);
      end;
    end;
  end;
  // Добавляем новые и обновляем заголовки, если надо
  for i := 0 to FRD.CalcFields.Count - 1 do
  begin
    pF := FRD.CalcFields[i];
    S := 'cf' + IntToStr(pF^.Id);
    Col := FRD.Grid.FindColumnByFieldName(S);
    if Col = nil then
      Col := NewCol(S);
    Col.Caption:=pF^.Name;
  end;
end;

function TCalcFm.CheckNames: Boolean;
var
  i: Integer;

  function _Check(idx: Integer): Boolean;
  var
    S, SS: String;
    j: Integer;
    Sr: TRpSource;
    Fl: TRpField;
  begin
    Result := False;
    S := Grid.Cells[0, idx];
    if S = '' then
    begin
      ErrMsg(rsFieldNameEmpty);
      Exit;
    end
    else if not CheckName(S) then
      Exit;
    for j := idx + 1 to Grid.RowCount - 1 do
    begin
      SS := Grid.Cells[0, j];
      if Utf8CompareText(S, SS) = 0 then
      begin
        ErrMsg(rsCalcFieldNameExists);
        Exit;
      end;
    end;
    Sr := FRD.Sources[0]^;
    for j := 0 to Sr.Fields.Count - 1 do
    begin
      Fl := Sr.Fields[j]^;
      if Fl.Visible and (Utf8CompareText(Fl.Name, S) = 0) then
      begin
        ErrMsg(rsQueryFieldNameExists);
        Exit;
      end;
    end;

    Result := True;
  end;

begin
  for i := 1 to Grid.RowCount - 1 do
  begin
    if not _Check(i) then
    begin
      Grid.Row := i;
      Grid.Col := 0;
      Exit(False);
    end;
  end;
  Result := True;
end;

function TCalcFm.Validate: Boolean;
var
  i: Integer;
  Tp: TRpFieldType;
begin
  Result := False;
  if not CheckNames then Exit;

  for i := 1 to Grid.RowCount - 1 do
  begin
    Tp := TRpFieldType(Grid.Objects[1, i]);
    if Tp = flNone then
    begin
      ErrMsg(rsSelectFieldType);
      Grid.Col := 1; Grid.Row := i;
      Exit;
    end
    else if Tp = flText then
    begin
      if Grid.Cells[2, i] = '' then
      begin
        ErrMsg(rsEnterFieldSize);
        Grid.Col := 2; Grid.Row := i;
	      Exit;
      end
      else if StrToInt(Grid.Cells[2, i]) > 1000 then
      begin
        ErrMsg(rsFieldSizeMustNotExceed);
        Grid.Col := 2; Grid.Row := i;
	      Exit;
      end;
    end
    else if Tp = flNumber then
    begin
      if Grid.Cells[2, i] = '' then
      begin
        ErrMsg(rsEnterPrecisionNum);
        Grid.Col := 2; Grid.Row := i;
	      Exit;
      end
      else if StrToInt(Grid.Cells[2, i]) > 10 then
      begin
        ErrMsg(rsPrecisionMustNotExceed);
        Grid.Col := 2; Grid.Row := i;
	      Exit;
      end;
    end;
  end;
  Result := True;
end;

function TCalcFm.ShowForm(aRD: TReportData; aForm: TdxForm): Integer;
begin
  Result := mrNone;
  if aRD.Sources.Count = 0 then
  begin
    ErrMsg(rsSourceNotSel);
    Exit;
  end;
  FRD := aRD;
  Caption := rsCalcFields + ': ' + FRD.Name;
  FForm := aForm;
  LoadFields;
  Result := ShowModal;
  if Result = mrOk then SaveFields;
end;

end.

