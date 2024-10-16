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

unit MultiRolesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ButtonPanel, ComCtrls, Menus, ExtCtrls, strconsts, Types, dxctrls, dxusers,
  LclType;

type

  { TMultiRolesFm }

  TMultiRolesFm = class(TForm)
    AccessMnu: TPopupMenu;
    ButtonPanel1: TButtonPanel;
    Images: TImageList;
    RpGrid: TStringGrid;
    MenuItem8: TMenuItem;
    N1: TMenuItem;
    ObjGrid: TStringGrid;
    SelGrid: TStringGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PageControl1: TPageControl;
    FormsGrid: TStringGrid;
    EditGrid: TStringGrid;
    DelGrid: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    procedure AccessMnuClick(Sender: TObject);
    procedure DelGridDblClick(Sender: TObject);
    procedure DelGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure EditGridDblClick(Sender: TObject);
    procedure EditGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormsGridDblClick(Sender: TObject);
    procedure FormsGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormsGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItem8Click(Sender: TObject);
    procedure ObjGridDblClick(Sender: TObject);
    procedure ObjGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure RpGridDblClick(Sender: TObject);
    procedure RpGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure RpGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure SelGridDblClick(Sender: TObject);
    procedure SelGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
  private
    FRoles: TList;
    FFormList, FAllFormList: TStringList;
    FModified: Boolean;
    function GetFormRight(Grid: TStringGrid; aCol, aRow: Integer): TdxFormRight;
    function GetReportRight(Grid: TStringGrid; aCol, aRow: Integer): TdxReportRight;
    procedure FillFormsGrid;
    procedure FillSelGrid;
    procedure FillEditGrid;
    procedure FillDelGrid;
    procedure FillObjGrid;
    procedure FillRpGrid;
  public
    function ShowForm(ARoles: TList): Integer;
  end;

var
  MultiRolesFm: TMultiRolesFm;

function ShowMultiRolesForm(ARoles: TList): Integer;

implementation

uses
  formmanager, reportmanager, dxreports, apputils, exprform, multictrlrightsform;

function ShowMultiRolesForm(ARoles: TList): Integer;
begin
  if MultiRolesFm = nil then
    MultiRolesFm := TMultiRolesFm.Create(Application);
  Result := MultiRolesFm.ShowForm(ARoles);
end;

{$R *.lfm}

{ TMultiRolesFm }

procedure TMultiRolesFm.FormCreate(Sender: TObject);
begin
  FRoles := TList.Create;
  FFormList := TStringList.Create;
  FAllFormList := TStringList.Create;

  SetupImageList(Images, ['eyes16', 'edit16', 'add16', 'question16', 'check16']);

  FormsGrid.FocusRectVisible := False;
  SelGrid.FocusRectVisible := False;
  EditGrid.FocusRectVisible := False;
  DelGrid.FocusRectVisible := False;
  ObjGrid.FocusRectVisible := False;
  RpGrid.FocusRectVisible := False;

  Caption := rsEditRoles;
  TabSheet1.Caption := rsAccess;
  TabSheet2.Caption := rsSelection;
  TabSheet3.Caption := rsEditing;
  TabSheet4.Caption := rsDeleting;
  TabSheet5.Caption := rsApplySelection;
  TabSheet6.Caption := rsReports;
  MenuItem4.Caption := rsNoAccess;
  MenuItem1.Caption := rsOnlyView;
  MenuItem2.Caption := rsOnlyEdit;
  MenuItem3.Caption := rsFullAccess;
  MenuItem8.Caption := rsComponents;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
end;

procedure TMultiRolesFm.AccessMnuClick(Sender: TObject);
var
  FR: TdxFormRight;
  Fm: TdxForm;
  Acc, c, r, i: Integer;
  Grid: TStringGrid;
begin
  Grid := FormsGrid;
  c := Grid.Col;
  r := Grid.Row;
  FR := GetFormRight(Grid, c, r);
  Acc := TMenuItem(Sender).Tag;
  FR.SetAccess(Acc);
  Grid.InvalidateRow(r);
  Fm := TdxForm(Grid.Objects[0, r]);
  if Fm.Pid = 0 then
  begin
    for i := r + 1 to Grid.RowCount - 1 do
    begin
      FR := GetFormRight(Grid, c, i);
      Fm := TdxForm(Grid.Objects[0, i]);
      if Fm.PId = 0 then Break;
      if Acc in [0, 1] then FR.SetAccess(Acc);
      Grid.InvalidateRow(i);
    end;
  end;
  FModified := True;
end;

procedure TMultiRolesFm.DelGridDblClick(Sender: TObject);
var
  FR: TdxFormRight;
  S: String;
  P: TPoint;
  Fm: TdxForm;
  Grid: TStringGrid;
begin
  Grid := TStringGrid(Sender);
  P := Grid.ScreenToClient(Mouse.CursorPos);
  P := Grid.MouseToCell(P);
  if (P.y > 0) and (P.x > 0) then
  begin
    Fm := TdxForm(Grid.Objects[0, P.y]);
    FR := GetFormRight(Grid, P.x, P.y);
    S := FR.DelCond;
    if ShowExprForm(etDelCond, nil, S, Fm, nil, nil, nil) = mrOk then
    begin
      FR.DelCond := S;
      Grid.Cells[P.x, P.y] := S;
      FModified := True;
    end;
  end;
end;

procedure TMultiRolesFm.DelGridGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
var
  Grid: TStringGrid;
  FR: TdxFormRight;
begin
  if ACol = 0 then Exit;
  Grid := TStringGrid(Sender);
  if ARow = 0 then
    HintText := Grid.Columns[ACol].Title.Caption
  else
  begin
    FR := GetFormRight(Grid, aCol, aRow);
    HintText := FR.DelCond;
  end;
  HintText := ReplVertLine(HintText);
end;

procedure TMultiRolesFm.EditGridDblClick(Sender: TObject);
var
  FR: TdxFormRight;
  S: String;
  P: TPoint;
  Fm: TdxForm;
  Grid: TStringGrid;
begin
  Grid := TStringGrid(Sender);
  P := Grid.ScreenToClient(Mouse.CursorPos);
  P := Grid.MouseToCell(P);
  if (P.y > 0) and (P.x > 0) then
  begin
    Fm := TdxForm(Grid.Objects[0, P.y]);
    FR := GetFormRight(Grid, P.x, P.y);
    S := FR.EditCond;
    if ShowExprForm(etEditCond, nil, S, Fm, nil, nil, nil) = mrOk then
    begin
      FR.EditCond := S;
      Grid.Cells[P.x, P.y] := S;
      FModified := True;
    end;
  end;
end;

procedure TMultiRolesFm.EditGridGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
var
  Grid: TStringGrid;
  FR: TdxFormRight;
begin
  if ACol = 0 then Exit;
  Grid := TStringGrid(Sender);
  if ARow = 0 then
    HintText := Grid.Columns[ACol].Title.Caption
  else
  begin
    FR := GetFormRight(Grid, aCol, aRow);
    HintText := FR.EditCond;
  end;
  HintText := ReplVertLine(HintText);
end;

procedure TMultiRolesFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean
  );
begin
  if ModalResult <> mrOk then
  begin
    if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TMultiRolesFm.FormDestroy(Sender: TObject);
begin
  FAllFormList.Free;
  FFormList.Free;
  FRoles.Free;
end;

procedure TMultiRolesFm.FormsGridDblClick(Sender: TObject);
var
  P: TPoint;
  Grid: TStringGrid;
begin
  Grid := TStringGrid(Sender);
  P := Grid.ScreenToClient(Mouse.CursorPos);
  P := Grid.MouseToCell(P);
  if P.y > 0 then
  begin
    if P.x > 0 then AccessMnu.PopUp
    else MenuItem8.Click;
  end;
end;

procedure TMultiRolesFm.FormsGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  FR: TdxFormRight;
  Cv: TCanvas;
  Bmp: TCustomBitmap;
  x, y, n: Integer;
  Grid: TStringGrid;
begin
  if (aCol = 0) or (aRow = 0) then Exit;

  Grid := TStringGrid(Sender);
  x := aRect.Left + (aRect.Width div 2 - Images.Width div 2);
  y := aRect.Top + (aRect.Height div 2 - Images.Height div 2);
  Cv := Grid.Canvas;
  FR := GetFormRight(Grid, aCol, aRow);

  n := FR.GetAccess;
  if n = 0 then
  else if n in [1..3] then Images.Draw(Cv, x, y, n - 1)
  else Images.Draw(Cv, x, y, 3);
end;

procedure TMultiRolesFm.FormsGridGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
var
  Grid: TStringGrid;
  FR: TdxFormRight;
begin
  if ACol = 0 then Exit;
  Grid := TStringGrid(Sender);
  if ARow = 0 then
    HintText := Grid.Columns[ACol].Title.Caption
  else
  begin
    FR := GetFormRight(Grid, aCol, aRow);
    case FR.GetAccess of
      0: HintText := rsNoAccess;
      1: HintText := rsOnlyView;
      2: HintText := rsOnlyEdit;
      3: HintText := rsFullAccess;
      else HintText := rsIncorrectAccess;
    end;
  end;
  HintText := ReplVertLine(HintText);
end;

procedure TMultiRolesFm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TMultiRolesFm.MenuItem8Click(Sender: TObject);
var
  Grid: TStringGrid;
  Fm: TdxForm;
begin
  Grid := FormsGrid;
  Fm := TdxForm(Grid.Objects[0, Grid.Row]);
  if ShowMultiCtrlRightsForm(Fm, FRoles) = mrOk then
  begin
    if MultiCtrlRightsFm.Modified then FModified := True;
  end;
end;

procedure TMultiRolesFm.ObjGridDblClick(Sender: TObject);
var
  FR: TdxFormRight;
  P: TPoint;
  Grid: TStringGrid;
begin
  Grid := TStringGrid(Sender);
  P := Grid.ScreenToClient(Mouse.CursorPos);
  P := Grid.MouseToCell(P);
  if (P.y > 0) and (P.x > 0) then
  begin
    FR := GetFormRight(Grid, P.x, P.y);
    FR.ApplySelCondToObj := not FR.ApplySelCondToObj;
    Grid.InvalidateCell(P.x, P.y);
  end;
end;

procedure TMultiRolesFm.ObjGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  FR: TdxFormRight;
  Cv: TCanvas;
  x, y: Integer;
  Grid: TStringGrid;
begin
  if (aCol = 0) or (aRow = 0) then Exit;

  Grid := TStringGrid(Sender);
  x := aRect.Left + (aRect.Width div 2 - 8);
  y := aRect.Top + (aRect.Height div 2 - 8);
  Cv := Grid.Canvas;
  FR := GetFormRight(Grid, aCol, aRow);
  if FR.ApplySelCondToObj then Images.Draw(Cv, x, y, 4);
end;

procedure TMultiRolesFm.RpGridDblClick(Sender: TObject);
var
  P: TPoint;
  Grid: TStringGrid;
  RR: TdxReportRight;
begin
  Grid := TStringGrid(Sender);
  P := Grid.ScreenToClient(Mouse.CursorPos);
  P := Grid.MouseToCell(P);
  if (P.y > 0) and (P.x > 0) then
  begin
    RR := GetReportRight(Grid, P.x, P.y);
    RR.Visible := not RR.Visible;
    Grid.InvalidateCell(P.x, P.y);
  end;
end;

procedure TMultiRolesFm.RpGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Cv: TCanvas;
  x, y: Integer;
  Grid: TStringGrid;
  RR: TdxReportRight;
begin
  if (aCol = 0) or (aRow = 0) then Exit;

  Grid := TStringGrid(Sender);
  x := aRect.Left + (aRect.Width div 2 - Images.Width div 2);
  y := aRect.Top + (aRect.Height div 2 - Images.Height div 2);
  Cv := Grid.Canvas;
  RR := GetReportRight(Grid, aCol, aRow);
  if RR.Visible then Images.Draw(Cv, x, y, 0);
end;

procedure TMultiRolesFm.RpGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
var
  Grid: TStringGrid;
  RR: TdxReportRight;
begin
  if ACol = 0 then Exit;
  Grid := TStringGrid(Sender);
  if ARow = 0 then
    HintText := Grid.Columns[ACol].Title.Caption
  else
  begin
    RR := GetReportRight(Grid, aCol, aRow);
    if RR.Visible then HintText := rsViewing
    else HintText := rsNoAccess;
  end;
  HintText := ReplVertLine(HintText);
end;

procedure TMultiRolesFm.SelGridDblClick(Sender: TObject);
var
  FR: TdxFormRight;
  S: String;
  P: TPoint;
  Fm: TdxForm;
  Grid: TStringGrid;
begin
  Grid := TStringGrid(Sender);
  P := Grid.ScreenToClient(Mouse.CursorPos);
  P := Grid.MouseToCell(P);
  if (P.y > 0) and (P.x > 0) then
  begin
    Fm := TdxForm(Grid.Objects[0, P.y]);
    FR := GetFormRight(Grid, P.x, P.y);
    S := FR.SelCond;
    if ShowExprForm(etSelCond, nil, S, nil, Fm, nil, nil) = mrOk then
    begin
      FR.SelCond := S;
      Grid.Cells[P.x, P.y] := S;
      FModified := True;
    end;
  end;
end;

procedure TMultiRolesFm.SelGridGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
var
  Grid: TStringGrid;
  FR: TdxFormRight;
begin
  if ACol = 0 then Exit;
  Grid := TStringGrid(Sender);
  if ARow = 0 then
    HintText := Grid.Columns[ACol].Title.Caption
  else
  begin
    FR := GetFormRight(Grid, aCol, aRow);
    HintText := FR.SelCond;
  end;
  HintText := ReplVertLine(HintText);
end;

function TMultiRolesFm.GetFormRight(Grid: TStringGrid; aCol, aRow: Integer
  ): TdxFormRight;
var
  Fm: TdxForm;
  R: TdxRole;
begin
  Fm := TdxForm(Grid.Objects[0, aRow]);
  R := TdxRole(FRoles[aCol - 1]);
  Result := R.FormRights.FindRight(Fm.Id);
end;

function TMultiRolesFm.GetReportRight(Grid: TStringGrid; aCol, aRow: Integer
  ): TdxReportRight;
var
  R: TdxRole;
  RD: TReportData;
begin
  R := TdxRole(FRoles[aCol - 1]);
  RD := TReportData(Grid.Objects[0, aRow]);
  Result := R.ReportRights.FindRight(RD.Id);
end;

procedure TMultiRolesFm.FillFormsGrid;
var
  SL: TStringList;
  i: Integer;
  Col: TGridColumn;
  R: TdxRole;
begin
  FormsGrid.Columns.Clear;
  SL := FAllFormList;
  FormsGrid.RowCount := SL.Count + 1;
  FormsGrid.FixedRows := 1;

  Col := FormsGrid.Columns.Add;
  Col.Title.Caption := rsForm;

  for i := 0 to FRoles.Count - 1 do
  begin
    R := TdxRole(FRoles[i]);
    Col := FormsGrid.Columns.Add;
    Col.Title.Caption := R.Name;
    Col.SizePriority := 0;
    Col.Width := 80;
  end;

  for i := 0 to SL.Count - 1 do
  begin
    FormsGrid.Cells[0, i + 1] := SL[i];
    FormsGrid.Objects[0, i + 1] := SL.Objects[i];
  end;
end;

procedure TMultiRolesFm.FillSelGrid;
var
  SL: TStringList;
  i, j: Integer;
  Col: TGridColumn;
  R: TdxRole;
  FR: TdxFormRight;
begin
  SelGrid.Columns.Clear;
  SL := FFormList;
  SelGrid.RowCount := SL.Count + 1;
  SelGrid.FixedRows := 1;

  Col := SelGrid.Columns.Add;
  Col.Title.Caption := rsForm;

  for i := 0 to FRoles.Count - 1 do
  begin
    R := TdxRole(FRoles[i]);
    Col := SelGrid.Columns.Add;
    Col.Title.Caption := R.Name;
    Col.SizePriority := 0;
    Col.Width := 80;
  end;

  for i := 0 to SL.Count - 1 do
  begin
    SelGrid.Cells[0, i + 1] := SL[i];
    SelGrid.Objects[0, i + 1] := SL.Objects[i];

    for j := 0 to FRoles.Count - 1 do
    begin
      FR := GetFormRight(SelGrid, j + 1, i + 1);
      SelGrid.Cells[j + 1, i + 1] := FR.SelCond;
    end;
  end;
end;

procedure TMultiRolesFm.FillEditGrid;
var
  SL: TStringList;
  i, j: Integer;
  Col: TGridColumn;
  R: TdxRole;
  FR: TdxFormRight;
begin
  EditGrid.Columns.Clear;
  SL := FAllFormList;
  EditGrid.RowCount := SL.Count + 1;
  EditGrid.FixedRows := 1;

  Col := EditGrid.Columns.Add;
  Col.Title.Caption := rsForm;

  for i := 0 to FRoles.Count - 1 do
  begin
    R := TdxRole(FRoles[i]);
    Col := EditGrid.Columns.Add;
    Col.Title.Caption := R.Name;
    Col.SizePriority := 0;
    Col.Width := 80;
  end;

  for i := 0 to SL.Count - 1 do
  begin
    EditGrid.Cells[0, i + 1] := SL[i];
    EditGrid.Objects[0, i + 1] := SL.Objects[i];

    for j := 0 to FRoles.Count - 1 do
    begin
      FR := GetFormRight(EditGrid, j + 1, i + 1);
      EditGrid.Cells[j + 1, i + 1] := FR.EditCond;
    end;
  end;
end;

procedure TMultiRolesFm.FillDelGrid;
var
  SL: TStringList;
  i, j: Integer;
  Col: TGridColumn;
  R: TdxRole;
  FR: TdxFormRight;
begin
  DelGrid.Columns.Clear;
  SL := FAllFormList;
  DelGrid.RowCount := SL.Count + 1;
  DelGrid.FixedRows := 1;

  Col := DelGrid.Columns.Add;
  Col.Title.Caption := rsForm;

  for i := 0 to FRoles.Count - 1 do
  begin
    R := TdxRole(FRoles[i]);
    Col := DelGrid.Columns.Add;
    Col.Title.Caption := R.Name;
    Col.SizePriority := 0;
    Col.Width := 80;
  end;

  for i := 0 to SL.Count - 1 do
  begin
    DelGrid.Cells[0, i + 1] := SL[i];
    DelGrid.Objects[0, i + 1] := SL.Objects[i];

    for j := 0 to FRoles.Count - 1 do
    begin
      FR := GetFormRight(DelGrid, j + 1, i + 1);
      DelGrid.Cells[j + 1, i + 1] := FR.DelCond;
    end;
  end;
end;

procedure TMultiRolesFm.FillObjGrid;
var
  SL: TStringList;
  i: Integer;
  Col: TGridColumn;
  R: TdxRole;
begin
  ObjGrid.Columns.Clear;
  SL := FFormList;
  ObjGrid.RowCount := SL.Count + 1;
  ObjGrid.FixedRows := 1;

  Col := ObjGrid.Columns.Add;
  Col.Title.Caption := rsForm;

  for i := 0 to FRoles.Count - 1 do
  begin
    R := TdxRole(FRoles[i]);
    Col := ObjGrid.Columns.Add;
    Col.Title.Caption := R.Name;
    Col.SizePriority := 0;
    Col.Width := 80;
  end;

  for i := 0 to SL.Count - 1 do
  begin
    ObjGrid.Cells[0, i + 1] := SL[i];
    ObjGrid.Objects[0, i + 1] := SL.Objects[i];
  end;
end;

procedure TMultiRolesFm.FillRpGrid;
var
  SL: TStringList;
  i: Integer;
  Col: TGridColumn;
  R: TdxRole;
begin
  RpGrid.Columns.Clear;
  SL := TStringList.Create;
  ReportMan.GetReports(SL);
  RpGrid.RowCount := SL.Count + 1;
  RpGrid.FixedRows := 1;

  Col := RpGrid.Columns.Add;
  Col.Title.Caption := rsReport;

  for i := 0 to FRoles.Count - 1 do
  begin
    R := TdxRole(FRoles[i]);
    Col := RpGrid.Columns.Add;
    Col.Title.Caption := R.Name;
    Col.SizePriority := 0;
    Col.Width := 80;
  end;

  for i := 0 to SL.Count - 1 do
  begin
    RpGrid.Cells[0, i + 1] := SL[i];
    RpGrid.Objects[0, i + 1] := SL.Objects[i];
  end;
  SL.Free;
end;

function TMultiRolesFm.ShowForm(ARoles: TList): Integer;
var
  R: TdxRole;
  i: Integer;
begin
  FModified := False;
  for i := 0 to ARoles.Count - 1 do
  begin
    R := TdxRole.Create;
    CopyRole(TdxRole(ARoles[i]), R);
    InitRole(R);
    FRoles.Add(R);
  end;
  FormMan.FormsToList(FFormList);
  FormMan.AllFormsToList(FAllFormList);
  FillFormsGrid;
  FillSelGrid;
  FillEditGrid;
  FillDelGrid;
  FillObjGrid;
  FillRpGrid;
  Result := ShowModal;
  if Result = mrOk then
    for i := 0 to FRoles.Count - 1 do
      CopyRole(TdxRole(FRoles[i]), TdxRole(ARoles[i]));
  ClearList(FRoles);
end;

end.

