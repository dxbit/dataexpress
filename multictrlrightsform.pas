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

unit MultiCtrlRightsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ButtonPanel,
  Menus, strconsts, dxctrls, dxusers, Types, LclType;

type

  { TMultiCtrlRightsFm }

  TMultiCtrlRightsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CmpMnu: TPopupMenu;
    Grid: TStringGrid;
    Images: TImageList;
    NoMnu: TMenuItem;
    ViewMnu: TMenuItem;
    EditMnu: TMenuItem;
    procedure CmpMnuClick(Sender: TObject);
    procedure CmpMnuPopup(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FModified: Boolean;
    FForm: TdxForm;
    FRoles: TList;
    procedure GetCtrlRight(aCol, aRow: Integer; out FR: TdxFormRight; out CR: TdxControlRight);
    procedure FillGrid;
  public
    function ShowForm(AForm: TdxForm; ARoles: TList): Integer;
    property Modified: Boolean read FModified;
  end;

var
  MultiCtrlRightsFm: TMultiCtrlRightsFm;

function ShowMultiCtrlRightsForm(AForm: TdxForm; ARoles: TList): Integer;

implementation

uses
  apputils, myctrls, Buttons, JvDesignImp, StrUtils;

function ShowMultiCtrlRightsForm(AForm: TdxForm; ARoles: TList): Integer;
begin
  if MultiCtrlRightsFm = nil then
    MultiCtrlRightsFm := TMultiCtrlRightsFm.Create(Application);
  Result := MultiCtrlRightsFm.ShowForm(AForm, ARoles);
end;

{$R *.lfm}

{ TMultiCtrlRightsFm }

procedure TMultiCtrlRightsFm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
  begin
    if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TMultiCtrlRightsFm.CmpMnuPopup(Sender: TObject);
var
  C: TComponent;
begin
  C := TComponent(Grid.Objects[0, Grid.Row]);
  EditMnu.Visible := HasFId(C);
end;

procedure TMultiCtrlRightsFm.CmpMnuClick(Sender: TObject);
var
  CR: TdxControlRight;
  Par, CC: TControl;
  FR: TdxFormRight;
  c, r, i: Integer;
begin
  c := Grid.Col;
  r := Grid.Row;
  GetCtrlRight(c, r, FR, CR);
  case TMenuItem(Sender).Tag of
    0:
      begin
        CR.Visible:=False;
        CR.Editing:=False;
      end;
    1:
      begin
        CR.Visible := True;
        CR.Editing:=False;
      end;
    2:
      begin
        CR.Visible:=True;
        CR.Editing:=True;
      end;
  end;
  Grid.InvalidateCell(c, r);
  CC := TControl(Grid.Objects[0, r]);
  Par := CC.Parent;
  for i := r + 1 to Grid.RowCount - 1 do
  begin
    CC := TControl(Grid.Objects[0, i]);
    if CC.Parent <> Par then Grid.InvalidateCell(c, i)
    else Break;
  end;
  FModified := True;
end;

procedure TMultiCtrlRightsFm.FormCreate(Sender: TObject);
begin
  FRoles := TList.Create;
  SetupImageList(Images, ['eyes16', 'edit16']);
  Grid.FocusRectVisible := False;

  Caption := rsAccessToCmp;
  NoMnu.Caption := rsNoAccess;
  ViewMnu.Caption := rsOnlyView;
  EditMnu.Caption := rsEditing;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
end;

procedure TMultiCtrlRightsFm.FormDestroy(Sender: TObject);
begin
  FRoles.Free;
end;

procedure TMultiCtrlRightsFm.GridDblClick(Sender: TObject);
var
  P: TPoint;
begin
  P := Grid.ScreenToClient(Mouse.CursorPos);
  P := Grid.MouseToCell(P);
  if (P.y > 0) and (P.x > 0) then CmpMnu.PopUp;
end;

function IsParentVisible(FR: TdxFormRight; C: TControl): Boolean;
var
  CR: TdxControlRight;
begin
  Result := True;
  if C is TdxForm then Exit;
  CR := FR.Controls.FindRight(C.Name);
  if CR <> nil then
  begin
    if not CR.Visible then Exit(False);
    Result := IsParentVisible(FR, C.Parent);
  end;
end;

procedure TMultiCtrlRightsFm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  CR: TdxControlRight;
  C: TComponent;
  Cv: TCanvas;
  x, y: Integer;
  FR: TdxFormRight;
begin
  if (aCol < 2) or (aRow = 0) then Exit;

  Cv := Grid.Canvas;
  x := aRect.Left + (aRect.Width div 2 - 8);
  y := aRect.Top + (aRect.Height div 2 - 8);

  GetCtrlRight(aCol, aRow, FR, CR);
  C := TComponent(Grid.Objects[0, aRow]);
  if not IsParentVisible(FR, TControl(C).Parent) then
  {else if not HasFId(C) then
  begin
    if CR.Visible then Cv.Draw(x, y, FViewBmp);;
  end  }
  else
  begin
    if CR.Editing and CR.Visible then Images.Draw(Cv, x, y, 1)
    else if CR.Visible then Images.Draw(Cv, x, y, 0);
  end;
end;

procedure TMultiCtrlRightsFm.GridGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
var
  CR: TdxControlRight;
  FR: TdxFormRight;
begin
  if ACol < 2 then Exit;
  if ARow = 0 then HintText := Grid.Columns[aCol].Title.Caption
  else
  begin
    GetCtrlRight(aCol, aRow, FR, CR);
    if CR.Editing and CR.Visible then HintText := rsEditing
    else if CR.Visible then HintText := rsOnlyView
    else HintText := rsNoAccess;
  end;
  HintText := ReplVertLine(HintText);
end;

procedure TMultiCtrlRightsFm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TMultiCtrlRightsFm.GetCtrlRight(aCol, aRow: Integer; out
  FR: TdxFormRight; out CR: TdxControlRight);
var
  C: TComponent;
  R: TdxRole;
  Fm: TdxForm;
begin
  C := TComponent(Grid.Objects[0, aRow]);
  R := TdxRole(FRoles[aCol - 2]);
  Fm := TdxForm(C.Owner);
  FR := R.FormRights.FindRight(Fm.Id);
  CR := FR.Controls.FindRight(C.Name);
end;

procedure TMultiCtrlRightsFm.FillGrid;

  function _AddToList(L: TStrings; aC: TControl; const Pfx: String): Integer;
  var
    i: Integer;
    C, WC: TWinControl;
    S: String;
  begin
    S := Pfx + GetComponentName(aC);
    if aC is TWinControl then
      for i := 0 to L.Count - 1 do
      begin
        WC := TWinControl(aC);
        C := TWinControl(L.Objects[i]);
        if (WC.Parent = C.Parent) and (WC.TabOrder < C.TabOrder) then
        begin
          L.InsertObject(i, S, WC);
          Exit(i+1);
        end;
      end;
    Result := L.AddObject(S, aC);
  end;

  procedure _Fill(aC: TWinControl; L: TStrings; Lvl: Integer);
  var
    i, j, idx: Integer;
    C: TControl;
    Pfx: String;
    LL: TStringList;
  begin
    LL := TStringList.Create;
    Pfx := DupeString('    ', Lvl);
    for i := 0 to aC.ControlCount - 1 do
    begin
      C := aC.Controls[i];
      if (C is TdxLabel) or (C is TSpeedButton) or (C is TdxShape) or (C is TJvDesignHandle) or
        (C is TGridButtons) then Continue;
      idx := _AddToList(L, C, Pfx);
      if (C is TdxPageControl) or (C is TdxTabSheet) or (C is TdxGroupBox) then
      begin
        LL.Clear;
        _Fill(TWinControl(C), LL, Lvl + 1);
        if idx < L.Count - 1 then
          for j := LL.Count - 1 downto 0 do
            L.InsertObject(idx, LL[j], LL.Objects[j])
        else
          for j := 0 to LL.Count - 1 do
            L.AddObject(LL[j], LL.Objects[j]);
      end;
    end;
    LL.Free;
  end;

var
  i, n: Integer;
  R: TdxRole;
  C: TComponent;
  Col: TGridColumn;
  SL: TStringList;
begin
  Grid.Columns.Clear;
  Grid.RowCount := 1;
  Grid.FixedRows := 1;

  Col := Grid.Columns.Add;
  Col.Title.Caption := rsComponent;
  Col := Grid.Columns.Add;
  Col.Title.Caption := rsCmpType;
  Col.SizePriority := 0;
  Col.Width := 100;

  for i := 0 to FRoles.Count - 1 do
  begin
    R := TdxRole(FRoles[i]);
    Col := Grid.Columns.Add;
    Col.Title.Caption := R.Name;
    Col.SizePriority := 0;
    Col.Width := 80;
  end;

  SL := TStringList.Create;
  _Fill(FForm, SL, 0);
  for i := 0 to SL.Count - 1 do
  begin
    n := i + 1;
    Grid.RowCount := n + 1;
    C := TComponent(SL.Objects[i]);
    Grid.Objects[0, n] := C;
    Grid.Cells[0, n] := SL[i];
    Grid.Cells[1, n] := GetComponentType(C);
  end;
  SL.Free;
end;

function TMultiCtrlRightsFm.ShowForm(AForm: TdxForm; ARoles: TList): Integer;
var
  i: Integer;
  R: TdxRole;
  FR: TdxFormRight;
begin
  FModified := False;
  FForm := AForm;
  for i := 0 to ARoles.Count - 1 do
  begin
    R := TdxRole.Create;
    CopyRole(TdxRole(ARoles[i]), R);
    FR := R.FormRights.FindRight(FForm.Id);
    InitFormRights(FR, FForm);
    FRoles.Add(R);
  end;
  FillGrid;
  Result := ShowModal;
  if Result = mrOk then
    for i := 0 to FRoles.Count - 1 do
      CopyRole(TdxRole(FRoles[i]), TdxRole(ARoles[i]));
  ClearList(FRoles);
end;

end.

