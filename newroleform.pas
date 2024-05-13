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

unit NewRoleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Grids, ButtonPanel, Menus, dxusers, DxCtrls, strconsts,
  LclType;

type

  { TNewRoleFm }

  TNewRoleFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Intfs: TComboBox;
    Edit1: TEdit;
    FRGrid: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PageControl1: TPageControl;
    AccessMnu: TPopupMenu;
    RRGrid: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure AccessMnuClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FRGridDblClick(Sender: TObject);
    procedure FRGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FRGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HelpButtonClick(Sender: TObject);
    procedure IntfsChange(Sender: TObject);
    procedure RRGridDblClick(Sender: TObject);
    procedure RRGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure RRGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
  private
    { private declarations }
    FRole, FOldRole: TdxRole;
    FViewBmp, FEditBmp, FFullBmp, FDelBmp, FErrBmp, FKeyBmp, FCbxBmp, FChkBmp,
      FCmpBmp, FExprBmp, FYelBmp: TCustomBitmap;
    FModified: Boolean;
    //function GetAccess(FR: TdxFormRight): Integer;
    //procedure SetAccess(FR: TdxFormRight; Value: Integer);
    procedure LoadFormRights;
    procedure LoadReportRights;
    procedure FillIntfs;
  public
    { public declarations }
    function ShowForm(aRole: TdxRole): Integer;
  end;

var
  NewRoleFm: TNewRoleFm;

function ShowRoleForm(aRole: TdxRole): Integer;

implementation

uses
  formmanager, reportmanager, apputils, exprform, dxreports, CtrlRightsForm,
  helpmanager;

{$R *.lfm}

function CheckLimitedAccessToCmp(FR: TdxFormRight): Boolean;
var
  i: Integer;
  CR: TdxControlRight;
  Fm: TdxForm;
  C: TComponent;
begin
  Result := False;
  Fm := FormMan.FindForm(FR.FormId);
  for i := 0 to FR.Controls.Count - 1 do
  begin
    CR := FR.Controls[i];
    C := Fm.FindComponent(CR.Name);
    if C = nil then Continue;
    if not HasFId(C) then
    begin
      if not CR.Visible then Exit(True);
    end
    else if (not CR.Editing) or (not CR.Visible) then Exit(True);
  end;
end;

function ShowRoleForm(aRole: TdxRole): Integer;
begin
  if NewRoleFm = nil then
  	NewRoleFm := TNewRoleFm.Create(Application);
  Result := NewRoleFm.ShowForm(aRole);
end;

{ TNewRoleFm }

procedure TNewRoleFm.FormCreate(Sender: TObject);
begin
  FRole := TdxRole.Create;

  Caption := rsRole;
  Label1.Caption := rsName;
  Label2.Caption := rsInterface;
  TabSheet1.Caption := rsAccessToForms;
  TabSheet2.Caption := rsAccessToReports;
  FRGrid.Columns[0].Title.Caption := rsForm;
  MenuItem4.Caption := rsNoAccess;
  MenuItem1.Caption := rsOnlyView;
  SetMenuItemImage(MenuItem1, 'eyes16');
  MenuItem2.Caption := rsOnlyEdit;
  SetMenuItemImage(MenuItem2, 'edit16');
  MenuItem3.Caption := rsFullAccess;
  SetMenuItemImage(MenuItem3, 'add16');
  FRGrid.Hint := rsDblClickChange;
  RRGrid.Hint := rsDblClickChange;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;

  FViewBmp := CreateBitmapFromLazarusResource('eyes16');
  FEditBmp := CreateBitmapFromLazarusResource('edit16');
  FFullBmp := CreateBitmapFromLazarusResource('add16');
  FDelBmp := CreateBitmapFromLazarusResource('delete16');
  FErrBmp := CreateBitmapFromLazarusResource('question16');
  FKeyBmp := CreateBitmapFromLazarusResource('key16');
  FCbxBmp := CreateBitmapFromLazarusResource('cbx16');
  FChkBmp := CreateBitmapFromLazarusResource('check16');
  FCmpBmp := CreateBitmapFromLazarusResource('bricks16');
  FExprBmp := CreateBitmapFromLazarusResource('sum16');
  FYelBmp := CreateBitmapFromLazarusResource('yellow16');
end;

procedure TNewRoleFm.AccessMnuClick(Sender: TObject);
var
  FR: TdxFormRight;
  i, r: PtrInt;
  Fm: TdxForm;
  Acc: Integer;
begin
  r := AccessMnu.Tag;
  FR := TdxFormRight(FRGrid.Objects[0, r]);
  FR.SetAccess(TMenuItem(Sender).Tag);
  FRGrid.InvalidateRow(r);
  Fm := FormMan.FindForm(FR.FormId);
  if Fm.Pid = 0 then
  begin
    Acc := FR.GetAccess;
    for i := r + 1 to FRGrid.RowCount - 1 do
    begin
      FR := TdxFormRight(FRGrid.Objects[0, i]);
      Fm := FormMan.FindForm(FR.FormId);
      if Fm.PId = 0 then Break;
      case Acc of
        0: FR.SetAccess(0);
        1: FR.SetAccess(1);
        //2: SetAccess(FR, 3);
      end;
      FRGrid.InvalidateRow(i);
    end;
  end;
  FModified := True;
end;

procedure TNewRoleFm.Edit1Change(Sender: TObject);
begin
  FModified := True;
end;

procedure TNewRoleFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  R: TdxRole;
begin
  if ModalResult = mrOk then
  begin
    CanClose := False;
    R := UserMan.Roles.FindRoleByName(Edit1.Text);
    if Trim(Edit1.Text) = '' then
    begin
      ErrMsg(rsEnterName);
      Edit1.SetFocus;
    end
    else if (R <> nil) and (R <> FOldRole) then
    begin
    	ErrMsg(rsARoleWithThisNameExists);
      Edit1.SetFocus;
    end
    else
    	CanClose := True;
  end
  else
  begin
    if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TNewRoleFm.FormDestroy(Sender: TObject);
begin
  FViewBmp.Free;
  FEditBmp.Free;
  FFullBmp.Free;
  FDelBmp.Free;
  FErrBmp.Free;
  FKeyBmp.Free;
  FCbxBmp.Free;
  FChkBmp.Free;
  FCmpBmp.Free;
  FExprBmp.Free;
  FYelBmp.Free;
  FRole.Free;
end;

procedure TNewRoleFm.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
end;

procedure TNewRoleFm.FRGridDblClick(Sender: TObject);
var
  P: types.TPoint;
  FR: TdxFormRight;
  Fm: TdxForm;
  S: String;
begin
  P := FRGrid.ScreenToClient(Mouse.CursorPos);
  P := FRGrid.MouseToCell(P);
  if P.y = 0 then Exit;
  FR := TdxFormRight(FRGrid.Objects[0, P.y]);
  if P.x = 0 then
  begin
    if ShowCtrlRightsForm(FR) = mrOk then
    begin
      FRGrid.Objects[6, P.y] := TObject(PtrInt(CheckLimitedAccessToCmp(FR)));
      FRGrid.InvalidateCell(6, P.y);
      FModified := True;
    end;
  end
  else if P.x = 1  then
  begin
    AccessMnu.Tag := P.y;
    AccessMnu.PopUp;
  end
  else
  begin
    Fm := FormMan.FindForm(FR.FormId);
    if P.x = 2 then
    begin
      if Fm.PId = 0 then
      begin
        S := FR.SelCond;
        if ShowExprForm(etSelCond, nil, S, nil, Fm, nil, nil) = mrOk then
        begin
          FR.SelCond := S;
          FModified := True;
        end;
      end
      else ErrMsg(rsNotAvailableSubForm);
    end
    else if P.x = 3 then
    begin
      S := FR.EditCond;
      if ShowExprForm(etEditCond, nil, S, Fm, nil, nil, nil) = mrOk then
      begin
        FR.EditCond := S;
        FModified := True;
      end;
    end
    else if P.x = 4 then
    begin
      S := FR.DelCond;
      if ShowExprForm(etDelCond, nil, S, Fm, nil, nil, nil) = mrOk then
      begin
        FR.DelCond := S;
        FModified := True;
      end;
    end
    else if P.x = 5 then
    begin
      if Fm.PId = 0 then
      begin
        FR.ApplySelCondToObj:=not FR.ApplySelCondToObj;
        FModified := True;
      end
      else ErrMsg(rsNotAvailableSubForm);
    end
    else if P.x = 6 then
    begin
      if ShowCtrlRightsForm(FR) = mrOk then
      begin
        FRGrid.Objects[P.x, P.y] := TObject(PtrInt(CheckLimitedAccessToCmp(FR)));
        FModified := True;
      end;
    end;
    FRGrid.InvalidateCell(P.x, P.y);
  end;
end;

procedure TNewRoleFm.FRGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  FR: TdxFormRight;
  Cv: TCanvas;
  x, y: Integer;
  Bmp: TCustomBitmap;
  FillClr: TColor;
  Fm: TdxForm;
begin
  if aCol = 0 then Exit;

  Cv := FRGrid.Canvas;
  x := aRect.Left + ((aRect.Right - aRect.Left) div 2 - 8);
  y := aRect.Top + ((aRect.Bottom - aRect.Top) div 2 - 8);
  if aRow = 0 then FillClr := FRGrid.FixedColor
  else if gdSelected in aState then FillClr := FRGrid.SelectedColor
  else if Odd(aRow) then FillClr := FRGrid.Color
  else FillClr := FRGrid.AlternateColor;
  Cv.Brush.Color:=FillClr;
  Cv.FillRect(aRect);

  if aRow = 0 then
    case aCol of
      1: Cv.Draw(x, y, FKeyBmp);
      2: Cv.Draw(x, y, FViewBmp);
      3: Cv.Draw(x, y, FEditBmp);
      4: Cv.Draw(x, y, FDelBmp);
      5: Cv.Draw(x, y, FCbxBmp);
      6: Cv.Draw(x, y, FCmpBmp);
    end
  else
  begin
    FR := TdxFormRight(FRGrid.Objects[0, aRow]);
    Fm := FormMan.FindForm(FR.FormId);
    case aCol of
      1:
        begin
          case FR.GetAccess of
            0: Bmp := nil;
            1: Bmp := FViewBmp;
            2: Bmp := FEditBmp;
            3: Bmp := FFullBmp;
            else Bmp := FErrBmp;
          end;
          if Bmp <> nil then Cv.Draw(x, y, Bmp);
        end;
      2: if Trim(FR.SelCond) <> '' then Cv.Draw(x, y, FExprBmp);
      3: if Trim(FR.EditCond) <> '' then Cv.Draw(x, y, FExprBmp);
      4: if Trim(FR.DelCond) <> '' then Cv.Draw(x, y, FExprBmp);
      5: if (Fm.PId = 0) and FR.ApplySelCondToObj then Cv.Draw(x, y, FChkBmp);
      6: if FRGrid.Objects[aCol, aRow] <> nil then Cv.Draw(x, y, FYelBmp);
    end;
  end;
end;

procedure TNewRoleFm.FRGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
var
  FR: TdxFormRight;
begin
  if ACol = 0 then Exit;
  if ARow = 0 then
  begin
    case ACol of
      1: HintText := rsAccess;
      2: HintText := rsSelCond;
      3: HintText := rsEditCond;
      4: HintText := rsDelCond;
      5: HintText := rsApplySelCondToObj;
      6: HintText := rsAccessToCmp;
    end;
  end
  else
  begin
    FR := TdxFormRight(FRGrid.Objects[0, aRow]);
    if ACol = 1 then
    begin
      case FR.GetAccess of
        0: HintText := rsNoAccess;
        1: HintText := rsOnlyView;
        2: HintText := rsOnlyEdit;
        3: HintText := rsFullAccess;
        else HintText := rsIncorrectAccess;
      end;
    end
    else if ACol = 2 then HintText := Trim(FR.SelCond)
    else if ACol = 3 then HintText := Trim(FR.EditCond)
    else if ACol = 4 then HintText := Trim(FR.DelCond)
    else if ACol = 6 then
    begin
      if FRGrid.Objects[ACol, ARow] <> nil then HintText := rsLimitedAccess;
    end;
  end;
  HintText := ReplVertLine(HintText);
end;

procedure TNewRoleFm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TNewRoleFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('role');
end;

procedure TNewRoleFm.IntfsChange(Sender: TObject);
begin
  FModified := True;
end;

procedure TNewRoleFm.RRGridDblClick(Sender: TObject);
var
  P: TPoint;
  RR: TdxReportRight;
begin
  P := RRGrid.ScreenToClient(Mouse.CursorPos);
  P := RRGrid.MouseToCell(P);
  if (P.y = 0) or (P.x = 0) then Exit;
  if P.x = 1 then
  begin
    RR := TdxReportRight(RRGrid.Objects[0, P.y]);
    RR.Visible := not RR.Visible;
    RRGrid.InvalidateCell(P.x, P.y);
    FModified := True;
  end;
end;

procedure TNewRoleFm.RRGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Cv: TCanvas;
  Clr: TColor;
  x, y: Integer;
  RR: TdxReportRight;
begin
  if aCol = 0 then Exit;
  Cv := RRGrid.Canvas;
  x := aRect.Left + ((aRect.Right - aRect.Left) div 2 - 8);
  y := aRect.Top + ((aRect.Bottom - aRect.Top) div 2 - 8);

  if aRow = 0 then Clr := RRGrid.FixedColor
  else if gdSelected in aState then Clr := RRGrid.SelectedColor
  else if Odd(aRow) then Clr := RRGrid.Color
  else Clr := RRGrid.AlternateColor;
  Cv.Brush.Color := Clr;
  Cv.FillRect(aRect);

  if aRow = 0 then
  begin
    if aCol = 1 then Cv.Draw(x, y, FKeyBmp);
  end
  else
  begin
    RR := TdxReportRight(RRGrid.Objects[0, aRow]);
    if aCol = 1 then
    begin
      if RR.Visible then Cv.Draw(x, y, FViewBmp);
    end;
  end;
end;

procedure TNewRoleFm.RRGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
var
  Grid: TStringGrid;
  RR: TdxReportRight;
begin
  if ACol < 1 then Exit;
  Grid := TStringGrid(Sender);
  if ARow = 0 then HintText := rsAccess
  else
  begin
    RR := TdxReportRight(Grid.Objects[0, ARow]);
    if RR.Visible then HintText := rsViewing
    else HintText := rsNoAccess;
  end;
end;

procedure TNewRoleFm.LoadFormRights;
var
  i, r: Integer;
  FR: TdxFormRight;
  Fm: TdxForm;
  SL: TStringList;
begin
  SL := TStringList.Create;
  FormMan.AllFormsToList(SL);
  FRGrid.RowCount:=1;
  for i := 0 to SL.Count - 1 do
  begin
    r := i + 1;
    FRGrid.RowCount := r + 1;
    Fm := TdxForm(SL.Objects[i]);
    FR := FRole.FormRights.FindRight(Fm.Id);
    FRGrid.Objects[0, r] := FR;
    FRGrid.Cells[0, r] := SL[i];
    FRGrid.Cells[5, r] := Bool2Str(FR.ApplySelCondToObj);
    FRGrid.Objects[6, r] := TObject(PtrInt(CheckLimitedAccessToCmp(FR)));
  end;
  SL.Free;
end;

procedure TNewRoleFm.LoadReportRights;
var
  SL: TStringList;
  i, r: Integer;
  RD: TReportData;
  RR: TdxReportRight;
begin
  RRGrid.RowCount := 1;
  SL := TStringList.Create;
  ReportMan.GetReports(SL);
  for i := 0 to SL.Count - 1 do
  begin
    r := i + 1;
    RRGrid.RowCount := r + 1;
    RD := TReportData(SL.Objects[i]);
    RR := FRole.ReportRights.FindRight(RD.Id);
    RRGrid.Objects[0, r] := RR;
    RRGrid.Cells[0, r] := SL[i];
  end;
  SL.Free;
end;

procedure TNewRoleFm.FillIntfs;
var
  i: Integer;
  Intf: TdxIntf;
begin
  Intfs.Clear;
  Intfs.Items.Add(rsDefault);
  for i := 0 to UserMan.Intfs.Count - 1 do
  begin
    Intf := UserMan.Intfs[i];
    Intfs.Items.AddObject(Intf.Name, Intf);
  end;
end;

function TNewRoleFm.ShowForm(aRole: TdxRole): Integer;
begin
  CopyRole(aRole, FRole);
  FOldRole := aRole;
  InitRole(FRole);
  LoadFormRights;
  LoadReportRights;
  FillIntfs;
  with Intfs do
    ItemIndex := Items.IndexOfObject(UserMan.Intfs.FindIntf(FRole.IntfId));
  Edit1.Text := FRole.Name;
  FModified := False;
  Result := ShowModal;
  if Result = mrOk then
  begin
    FRole.Name:=Edit1.Text;
    with Intfs do
      if ItemIndex > 0 then
        FRole.IntfId := TdxIntf(Items.Objects[ItemIndex]).Id
      else
        FRole.IntfId := -1;
    CopyRole(FRole, aRole);
  end;
end;

end.

