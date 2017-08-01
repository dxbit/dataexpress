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
unit NewRoleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Grids, ButtonPanel, Menus, dxusers, DxCtrls, strconsts;

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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FRGridDblClick(Sender: TObject);
    procedure FRGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FRGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure HelpButtonClick(Sender: TObject);
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
    function GetAccess(FR: TdxFormRight): Integer;
    procedure SetAccess(FR: TdxFormRight; Value: Integer);
    procedure InitFormRights;
    procedure LoadFormRights;
    procedure InitReportRights;
    procedure LoadReportRights;
    procedure FillIntfs;
  public
    { public declarations }
    function ShowForm(aRole: TdxRole): Integer;
  end;

var
  NewRoleFm: TNewRoleFm;

implementation

uses
  formmanager, reportmanager, apputils, exprform, dxreports, CtrlRightsForm,
  helpform;

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

{ TNewRoleFm }

procedure TNewRoleFm.FormCreate(Sender: TObject);
begin
  FRole := TdxRole.Create;

  Caption := rsRole;
  Label1.Caption := rsName;
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
  SetAccess(FR, TMenuItem(Sender).Tag);
  FRGrid.InvalidateRow(r);
  Fm := FormMan.FindForm(FR.FormId);
  if Fm.Pid = 0 then
  begin
    Acc := GetAccess(FR);
    for i := r + 1 to FRGrid.RowCount - 1 do
    begin
      FR := TdxFormRight(FRGrid.Objects[0, i]);
      Fm := FormMan.FindForm(FR.FormId);
      if Fm.PId = 0 then Break;
      case Acc of
        0: SetAccess(FR, 0);
        1: SetAccess(FR, 1);
        //2: SetAccess(FR, 3);
      end;
      FRGrid.InvalidateRow(i);
    end;
  end;
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
    if CtrlRightsFm.ShowForm(FR) = mrOk then
    begin
      FRGrid.Objects[6, P.y] := TObject(PtrInt(CheckLimitedAccessToCmp(FR)));
      FRGrid.InvalidateCell(6, P.y);
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
        if ExprFm.ShowForm(rsSelCond + ': ' + Fm.FormCaption, nil, S, nil, Fm, nil,
          nil, 'selcond') then FR.SelCond := S;
      end
      else ErrMsg(rsNotAvailableSubForm);
    end
    else if P.x = 3 then
    begin
      S := FR.EditCond;
      if ExprFm.ShowForm(rsEditCond + ': ' + Fm.FormCaption, nil, S, Fm, nil, nil,
        nil, 'editcond') then FR.EditCond := S;
    end
    else if P.x = 4 then
    begin
      S := FR.DelCond;
      if ExprFm.ShowForm(rsDelCond + ': ' + Fm.FormCaption, nil, S, Fm, nil, nil,
        nil, 'delcond') then FR.DelCond := S;
    end
    else if P.x = 5 then
    begin
      if Fm.PId = 0 then
        FR.ApplySelCondToObj:=not FR.ApplySelCondToObj
      else ErrMsg(rsNotAvailableSubForm);
    end
    else if P.x = 6 then
    begin
      if CtrlRightsFm.ShowForm(FR) = mrOk then
        FRGrid.Objects[P.x, P.y] := TObject(PtrInt(CheckLimitedAccessToCmp(FR)));
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
          case GetAccess(FR) of
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
      case GetAccess(FR) of
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
end;

procedure TNewRoleFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('role');
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
    if aCol = 1 then Cv.Draw(x, y, FViewBmp);
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
begin
  if (ACol = 1) and (ARow = 0) then HintText := rsViewing;
end;

function TNewRoleFm.GetAccess(FR: TdxFormRight): Integer;
begin
  if FR.Visible and FR.Adding and FR.Editing and FR.Deleting then
    Result := 3
  else if FR.Visible and FR.Editing and (not FR.Deleting) and (not FR.Adding) then
    Result := 2
  else if FR.Visible and (not FR.Editing) and (not FR.Deleting) and (not FR.Adding) then
    Result := 1
  else if (not FR.Visible) and (not FR.Editing) and (not FR.Deleting) and (not FR.Adding) then
    Result := 0
  else
    Result := -1;
end;

procedure TNewRoleFm.SetAccess(FR: TdxFormRight; Value: Integer);
begin
  case Value of
    0:
      begin
        FR.Visible:=False;
        FR.Editing:=False;
        FR.Adding:=False;
        FR.Deleting:=False;
      end;
    1:
      begin
        FR.Visible := True;
        FR.Editing:=False;
        FR.Adding:=False;
        FR.Deleting:=False;
      end;
    2:
      begin
        FR.Visible := True;
        FR.Editing:=True;
        FR.Adding:=False;
        FR.Deleting:=False;
      end;
    3:
      begin
        FR.Visible := True;
        FR.Editing:=True;
        FR.Adding:=True;
        FR.Deleting:=True;
      end;
  end;
end;

// Программа не отслеживает добавление/удаление форм. Поэтому в списке могут
// присутствовать удаленные формы, и отсутствовать новые. Приводим список
// в порядок.
procedure TNewRoleFm.InitFormRights;
var
  i: Integer;
  FR: TdxFormRight;
  Fm: TdxForm;
begin
  // Удаляем несуществующие формы
  for i := FRole.FormRights.Count - 1 downto 0 do
  begin
    FR := FRole.FormRights[i];
    Fm := FormMan.FindForm(FR.FormId);
    if Fm = nil then
      FRole.FormRights.DeleteRight(FR);
  end;
  // Добавляем новые
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    FR := FRole.FormRights.FindRight(Fm.Id);
    if FR = nil then
    begin
      FR := FRole.FormRights.AddRight;
      FR.FormId:=Fm.Id;
      FR.Visible:=True;
      FR.Adding:=True;
      FR.Editing:=True;
      FR.Deleting:=True;
      //FR.Exporting:=True;
    end;
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

// Программа не отслеживает добавление/удаление отчетов. Поэтому в списке могут
// присутствовать удаленные отчеты, и отсутствовать новые. Приводим список
// в порядок.
procedure TNewRoleFm.InitReportRights;
var
  i: Integer;
  RD: TReportData;
  RR: TdxReportRight;
begin
  // Удаляем несуществующие
  for i := FRole.ReportRights.Count - 1 downto 0 do
  begin
    RR := FRole.ReportRights[i];
    RD := ReportMan.FindReport(RR.ReportId);
    if RD = nil then
      FRole.ReportRights.DeleteRight(RR);
  end;
  // Добавляем новые
  for i := 0 to ReportMan.ReportCount - 1 do
  begin
    RD := ReportMan.Reports[i];
    if RD.Kind <> rkReport then Continue;
    RR := FRole.ReportRights.FindRight(RD.Id);
    if RR = nil then
    begin
      RR := FRole.ReportRights.AddRight;
      RR.ReportId:=RD.Id;
      RR.Visible:=True;
    end;
  end;
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
  InitFormRights;
  LoadFormRights;
  InitReportRights;
  LoadReportRights;
  FillIntfs;
  with Intfs do
    ItemIndex := Items.IndexOfObject(UserMan.Intfs.FindIntf(FRole.IntfId));
  Edit1.Text := FRole.Name;
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

