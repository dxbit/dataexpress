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
unit TotalsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, Grids, ButtonPanel, strconsts, dxreports, LclType, StdCtrls;

type

  { TTotalsFm }

  TTotalsFm = class(TForm)
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
    procedure ToolButtonClick(Sender: TObject);
  private
    { private declarations }
    FRD: TReportData;
    procedure SetControlState;
    procedure FillFields;
    procedure FillFuncs;
    procedure Load;
    procedure Save;
    function Validate: Boolean;
  public
    { public declarations }
    function ShowForm(aRD: TReportData): Integer;
  end;

var
  TotalsFm: TTotalsFm;

implementation

uses
  apputils, dxctrls, helpform, mytypes;

{$R *.lfm}

{ TTotalsFm }

procedure TTotalsFm.FormCreate(Sender: TObject);
begin
  Grid.Columns[0].Title.Caption:=rsCaption;
  Grid.Columns[1].Title.Caption:=rsField;
  Grid.Columns[2].Title.Caption:=rsFunction;
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
  Images.AddLazarusResource('add16');
  Images.AddLazarusResource('delete16');
  Images.AddLazarusResource('up16');
  Images.AddLazarusResource('down16');
  FillFuncs;
end;

procedure TTotalsFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
  SetControlState;
end;

procedure TTotalsFm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  K: Word;
begin
  K := Key; Key := 0;
  case K of
    VK_DELETE: if ssCtrl in Shift then MenuItem2.Click;
    VK_PRIOR: MenuItem4.Click;
    VK_NEXT: MenuItem5.Click;
    else Key := K;
  end;
end;

procedure TTotalsFm.GridPickListSelect(Sender: TObject);
begin
  if Grid.Col = 1 then
  begin
    with TPickListCellEditor(Grid.Editor) do
      if Grid.Cells[0, Grid.Row] = '' then
        Grid.Cells[0, Grid.Row] := Grid.Cells[1, Grid.Row];
  end
  else if Grid.Col = 2 then
    with TPickListCellEditor(Grid.Editor) do
      Grid.Objects[Grid.Col, Grid.Row] := Items.Objects[ItemIndex];
end;

procedure TTotalsFm.GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if Editor is TPickListCellEditor then
    TPickListCellEditor(Editor).Style:=csDropDownList;
end;

procedure TTotalsFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Grid.Editor <> nil then Grid.EditingDone;
end;

procedure TTotalsFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (ModalResult = mrOk) and (not Validate) then CanClose := False;
end;

procedure TTotalsFm.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  SetControlState;
end;

procedure TTotalsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('totals');
end;

procedure TTotalsFm.MenuItem1Click(Sender: TObject);
var
  r: Integer;
begin
  r := Grid.RowCount;
  Grid.RowCount := r + 1;
  Grid.Row:=r;
  SetControlState;
end;

procedure TTotalsFm.MenuItem2Click(Sender: TObject);
begin
  if ConfirmDelete then
    Grid.DeleteRow(Grid.Row);
  SetControlState;
end;

procedure TTotalsFm.MenuItem4Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row - 1);
  SetControlState;
end;

procedure TTotalsFm.MenuItem5Click(Sender: TObject);
begin
  Grid.ExchangeColRow(False, Grid.Row, Grid.Row + 1);
  SetControlState;
end;

procedure TTotalsFm.ToolButtonClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: MenuItem1.Click;
    1: MenuItem2.Click;
    2: MenuItem4.Click;
    3: MenuItem5.Click;
  end;
end;

procedure TTotalsFm.SetControlState;
begin
  MenuItem2.Enabled := Grid.Row >= 1;
  ToolButton2.Enabled := MenuItem2.Enabled;
  MenuItem4.Enabled := Grid.Row > 1;
  ToolButton3.Enabled := MenuItem4.Enabled;
  MenuItem5.Enabled := Grid.Row < Grid.RowCount - 1;
  ToolButton4.Enabled := MenuItem5.Enabled;
end;

procedure TTotalsFm.FillFields;
var
  SL: TStringListUtf8;
  Sr: TRpSource;
  i: Integer;
  rF: TRpField;
begin
  SL := TStringListUtf8.Create;
  Sl.Add('');
  Sr := FRD.Sources[0]^;
  for i := 0 to Sr.Fields.Count - 1 do
  begin
    rF := FRD.FindField(Sr.Fields[i]^.Id)^;
    if (rF.Visible) {and (rF.Tp = flNumber)} then
      SL.Add(rF.Name);
  end;
  for i := 0 to FRD.CalcFields.Count - 1 do
    SL.Add(FRD.CalcFields[i]^.Name);
  SL.Sort;
  Grid.Columns[1].PickList.Assign(SL);
  SL.Free;
end;

procedure TTotalsFm.FillFuncs;
var
  L: TStrings;
begin
  L := Grid.Columns[2].PickList;
  L.AddObject(rsSum, TObject(PtrInt(tfSum)));
  L.AddObject(rsAverage, TObject(PtrInt(tfAvg)));
  L.AddObject(rsCount, TObject(PtrInt(tfCount)));
  L.AddObject(rsMaximum, TObject(PtrInt(tfMax)));
  L.AddObject(rsMinimum, TObject(PtrInt(tfMin)));
end;

procedure TTotalsFm.Load;
var
  i, r, n: Integer;
  T: TRpTotalData;
  Col: TRpGridColumn;
begin
  Grid.RowCount := 1;
  for i := 0 to FRD.Totals.Count - 1 do
  begin
    T := FRD.Totals[i];
    Col := FRD.Grid.FindColumnByFieldName(T.FieldName);
    //if Col = nil then Continue;
    r := i + 1;
    Grid.RowCount := r + 1;
    Grid.Cells[0, r] := T.Caption;
    if Col <> nil then
	    Grid.Cells[1, r] := Col.Caption;
    n := Grid.Columns[2].PickList.IndexOfObject(TObject(PtrInt(T.Func)));
    if n >= 0 then
    begin
      Grid.Cells[2, r] := Grid.Columns[2].PickList[n];
      Grid.Objects[2, r] := TObject(PtrInt(T.Func));
    end;
  end;
end;

procedure TTotalsFm.Save;
var
  i: Integer;
  T: TRpTotalData;
  Col: TRpGridColumn;
begin
  FRD.Totals.Clear;
  for i := 1 to Grid.RowCount - 1 do
  begin
    T := FRD.Totals.AddTotal;
    T.Caption := Grid.Cells[0, i];
    Col := FRD.Grid.FindColumnByTitle(Grid.Cells[1, i]);
    if Col <> nil then
	    T.FieldName:=Col.FieldName;
    T.Func := TRpTotalFunc(Grid.Objects[2, i]);
  end;
end;

function TTotalsFm.Validate: Boolean;
var
  i: Integer;
  Fn: TRpTotalFunc;
  Col: TRpGridColumn;
  pCF: PRpCalcField;
  Tp: TRpFieldType;
  pF: PRpField;
begin
  Result := False;
  for i := 1 to Grid.RowCount - 1 do
  begin
    Fn := TRpTotalFunc(Grid.Objects[2, i]);
    if Grid.Cells[0, i] = '' then
    begin
      Grid.Row:=i; Grid.Col := 0;
      ErrMsg(rsEnterCaption);
      Exit;
    end
    else if (Grid.Cells[1, i] = '') and (Fn <> tfCount) then
    begin
      Grid.Row:=i; Grid.Col := 1;
      ErrMsg(rsFieldNotSel);
      Exit;
    end
    else if Grid.Cells[2, i] = '' then
    begin
      Grid.Row:=i; Grid.Col := 2;
      ErrMsg(rsFuncNotSel);
      Exit;
    end;
    // Проверка совместимости типа поля и функции
    Col := FRD.Grid.FindColumnByTitle(Grid.Cells[1, i]);
    if Col <> nil then
    begin
      if Col.IsCalcField then
      begin
        pCF := FRD.CalcFields.FindField(Col.FieldId);
        Tp := pCF^.Tp;
      end
      else
      begin
        pF := FRD.FindField(Col.FieldId);
        Tp := pF^.Tp;
      end;
      if (Fn in [tfSum, tfAvg]) and (Tp <> flNumber) then
      begin
        Grid.Row:=i; Grid.Col := 1;
        ErrMsg(rsFieldTypeMustNumeric);
			  Exit;
      end;
    end;
  end;
  Result := True;
end;

function TTotalsFm.ShowForm(aRD: TReportData): Integer;
begin
  Result := mrNone;
  if aRD.Sources.Count = 0 then
  begin
    ErrMsg(rsSourceNotSel);
    Exit;
  end;
  FRD := aRD;
  Caption := rsTotals + ': ' + FRD.Name;
  FillFields;
  Load;
  Result := ShowModal;
  if Result = mrOk then Save;
end;

end.

