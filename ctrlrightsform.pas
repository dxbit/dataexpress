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

unit CtrlRightsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ButtonPanel, dxctrls, dxusers, Buttons, Menus, strconsts, LclType;

type

  { TCtrlRightsFm }

  TCtrlRightsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TStringGrid;
    AccessMnu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    procedure AccessMnuClick(Sender: TObject);
    procedure AccessMnuPopup(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    FFm: TdxForm;
    FFR: TdxFormRight;
    FViewBmp, FEditBmp, FKeyBmp: TCustomBitmap;
    FModified: Boolean;
    //procedure InitRights;
    procedure FillRights;
  public
    { public declarations }
    function ShowForm(aFR: TdxFormRight): Integer;
  end;

var
  CtrlRightsFm: TCtrlRightsFm;

function ShowCtrlRightsForm(aFR: TdxFormRight): Integer;

implementation

uses
  formmanager, strutils, apputils, JvDesignImp, myctrls;

function ShowCtrlRightsForm(aFR: TdxFormRight): Integer;
begin
  if CtrlRightsFm = nil then
  	CtrlRightsFm := TCtrlRightsFm.Create(Application);
  Result := CtrlRightsFm.ShowForm(aFR);
end;

{$R *.lfm}

{ TCtrlRightsFm }

procedure TCtrlRightsFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TCtrlRightsFm.GridDblClick(Sender: TObject);
var
  P: types.TPoint;
begin
  P := Grid.ScreenToClient(Mouse.CursorPos);
  P := Grid.MouseToCell(P);
  if (P.y = 0) or (P.x < 2) then Exit;
  AccessMnu.Tag := P.y;
  AccessMnu.Popup;
end;

procedure TCtrlRightsFm.FormCreate(Sender: TObject);
begin
  Caption := rsAccessToCmp;
  Grid.Columns[0].Title.Caption := rsComponent;
  Grid.Columns[1].Title.Caption := rsCmpType;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.CancelButton.Cancel := True;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  MenuItem1.Caption := rsNoAccess;
  MenuItem2.Caption := rsOnlyView;
  SetMenuItemImage(MenuItem2, 'eyes16');
  MenuItem3.Caption := rsEditing;
  SetMenuItemImage(MenuItem3, 'edit16');
  FFR := TdxFormRight.Create;
  FViewBmp := CreateBitmapFromLazarusResource('eyes16');
  FEditBmp := CreateBitmapFromLazarusResource('edit16');
  FKeyBmp := CreateBitmapFromLazarusResource('key16');
end;

procedure TCtrlRightsFm.AccessMnuClick(Sender: TObject);
var
  r, i: PtrInt;
  CR: TdxControlRight;
  Par, CC: TComponent;
begin
  r := AccessMnu.Tag;
  CR := TdxControlRight(Grid.Objects[0, r]);
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
  Grid.InvalidateCell(2, r);
  Par := TControl(FFm.FindComponent(CR.Name)).Parent;
  for i := r + 1 to Grid.RowCount - 1 do
  begin
    CR := TdxControlRight(Grid.Objects[0, i]);
    CC := FFm.FindComponent(CR.Name);
    if TControl(CC).Parent <> Par then Grid.InvalidateCell(2, i)
    else Break;
  end;
  FModified := True;
end;

procedure TCtrlRightsFm.AccessMnuPopup(Sender: TObject);
var
  r: PtrInt;
  CR: TdxControlRight;
  C: TComponent;
begin
  r := AccessMnu.Tag;
  CR := TdxControlRight(Grid.Objects[0, r]);
  C := FFm.FindComponent(CR.Name);
  if C = nil then Exit;
  MenuItem3.Visible := HasFId(C);
end;

procedure TCtrlRightsFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
  begin
    if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TCtrlRightsFm.FormDestroy(Sender: TObject);
begin
  FViewBmp.Free;
  FEditBmp.Free;
  FKeyBmp.Free;
  FFR.Free;
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

procedure TCtrlRightsFm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Cv: TCanvas;
  Clr: TColor;
  x, y: Integer;
  CR: TdxControlRight;
  C: TComponent;
begin
  if aCol < 2 then Exit;
  Cv := Grid.Canvas;
  if aRow = 0 then Clr := Grid.FixedColor
  else if gdSelected in aState then Clr := Grid.SelectedColor
  else if Odd(aRow) then Clr := Grid.Color
  else Clr := Grid.AlternateColor;
  Cv.Brush.Color := Clr;
  Cv.FillRect(aRect);

  x := aRect.Left + ((aRect.Right - aRect.Left) div 2 - 8);
  y := aRect.Top + ((aRect.Bottom - aRect.Top) div 2 - 8);

  if aRow = 0 then
  begin
    if aCol = 2 then Cv.Draw(x, y, FKeyBmp);
  end
  else
  begin
    CR := TdxControlRight(Grid.Objects[0, aRow]);
    C := FFm.FindComponent(CR.Name);
    if not IsParentVisible(FFR, TControl(C).Parent) then
    {else if not HasFId(C) then
    begin
      if CR.Visible then Cv.Draw(x, y, FViewBmp);;
    end  }
    else
    begin
      if CR.Editing and CR.Visible then Cv.Draw(x, y, FEditBmp)
      else if CR.Visible then Cv.Draw(x, y, FViewBmp);
    end;
  end;
end;

procedure TCtrlRightsFm.GridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
var
  CR: TdxControlRight;
begin
  if ACol < 2 then Exit;
  if ARow = 0 then HintText := rsAccess
  else
  begin
    CR := TdxControlRight(Grid.Objects[0, aRow]);
    if CR.Editing and CR.Visible then HintText := rsEditing
    else if CR.Visible then HintText := rsOnlyView
    else HintText := rsNoAccess;
  end;
end;

procedure TCtrlRightsFm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

{procedure TCtrlRightsFm.InitRights;
var
  i: Integer;
  C: TComponent;
  CR: TdxControlRight;
begin
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if (C is TdxLabel) or (C is TSpeedButton) or (C is TdxShape) or (C is TGridButtons) or
      (C is TJvDesignHandle) then Continue;
    CR := FFR.Controls.FindRight(C.Name);
    if (CR = nil) and (C.Name <> '') then
    begin
      CR := FFR.Controls.AddRight;
      CR.Name := C.Name;
      CR.Visible := True;
      CR.Editing := True;
    end;
  end;
end;   }

procedure TCtrlRightsFm.FillRights;
var
  SL: TStringList;
  i, r: Integer;
  C: TComponent;
  CR: TdxControlRight;

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

begin
  Grid.RowCount := 1;
  SL := TStringList.Create;
  _Fill(FFm, SL, 0);
  for i := 0 to SL.Count - 1 do
  begin
    r := i + 1;
    Grid.RowCount := r + 1;
    C := TComponent(SL.Objects[i]);
    CR := FFR.Controls.FindRight(C.Name);
    Grid.Objects[0, r] := CR;
    Grid.Cells[0, r] := SL[i];
    Grid.Cells[1, r] := GetComponentType(C);
  end;
  SL.Free;
end;

function TCtrlRightsFm.ShowForm(aFR: TdxFormRight): Integer;
begin
  CopyFormRights(aFR, FFR);
  FFm := FormMan.FindForm(aFR.FormId);
  Caption := rsAccessToCmp + ': ' + FFm.FormCaption;
  {if CheckComponentNames(FFm) = False then
  begin
    MessageDlg(rsThereProblem, rsProblemSolution, mtInformation, [mbOk], 0);
    Exit;
  end;       }
  InitFormRights(FFR, FFm);
  FillRights;
  FModified := False;
  Result := ShowModal;
  if Result = mrOk then
    CopyFormRights(FFR, aFR);
end;

end.

