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
unit FormDesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JvDesignSurface, JvDesignUtils, Buttons, Controls,
  Forms, Grids, dxctrls, LMessages, strconsts, JvDesignImp;

type

  { TMyController }

  TMyController = class(TJvDesignController)
  private
    FLeftButtonPressed: Boolean;
  protected
    function KeyDown(AKeyCode: Cardinal): Boolean; override;
    function KeyUp(AKeyCode: Cardinal): Boolean; override;
    function MouseDown(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse
      ): Boolean; override;
    function MouseMove(X, Y: Integer; TheMessage: TLMMouse): Boolean; override;
    function MouseUp(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse
      ): Boolean; override;
  end;

  { TMyMessenger }

  TMyMessenger = class(TJvDesignWinControlHookMessenger)
  public
    function IsDesignMessage(ASender: TControl; var AMessage: TLMessage): Boolean;
      override;
  end;

  { TFormDesigner }

  TFormDesigner = class(TJvDesignSurface)
  private
    FActive: Boolean;
    FControlClass: String;
    FNewControl: Boolean;
    FOnKeyDown: TKeyEvent;
    FParent: TWinControl;
    FMoves: TList;
    function GetControl: TControl;
    function GetForm: TdxForm;
    procedure SetActive(AValue: Boolean);
    function IsParentSelect(C: TControl): Boolean;
    function IsParent(C, aParent: TControl): Boolean;
    function GetSelectionRect: TRect;
    procedure AdjustPastedComponents;
    function CheckDeleteComponents: Boolean;
  protected
    procedure SurfaceSelectionChange(Sender: TObject);
    procedure SurfaceAddComponent(Sender: TObject; aComponent: TComponent);
    procedure SurfaceDeleteComponent(Sender: TObject; aComponent: TComponent);
    procedure SurfaceGetAddClass(Sender: TObject; var ioClass: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DesignForm(Fm: TWinControl);
    function GetLeftMostEdge: Integer;
    procedure SetLeftMostEdge;
    function GetTopMostEdge: Integer;
    procedure SetTopMostEdge;
    function GetRightMostEdge: Integer;
    procedure SetRightMostEdge;
    function GetBottomMostEdge: Integer;
    procedure SetBottomMostEdge;
    procedure SetVertCenter;
    procedure SetHorzCenter;
    procedure SetMaxWidth;
    procedure SetMinWidth;
    procedure SetMaxHeight;
    procedure SetMinHeight;
    procedure MoveComponents;
    procedure PasteComponents;
    procedure DeleteComponents;
    procedure UserDeleteComponents;
    procedure CopyComponents;
    procedure ChangeParent;
    procedure FindLost;
    procedure BringToFront;
    procedure SendToBack;
    property Active: Boolean read FActive write SetActive;
    property ControlClass: String read FControlClass write FControlClass;
    property Control: TControl read GetControl;
    property Parent: TWinControl read FParent write FParent;
    property Form: TdxForm read GetForm;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
  end;

var
  FormDesign: TFormDesigner;

implementation

uses
  designerframe, dbengine, sqlgen, formmanager, propsform,
  comctrls, LCLType, dximages, dxfiles, dbctrlsex, apputils, DXReports,
  reportmanager, dialogs, myctrls, scriptform;

{ TFormDesigner }


procedure AssignDesignMenu(C: TComponent);
var
  Bn: TSpeedButton;
begin
  {$ifndef windows}
  if C is TControl then
  begin
  	TControl(C).PopupMenu := DesignFr.PopupMenu1;
    Bn := GetEditButton(C);
    if Bn <> nil then Bn.PopupMenu := DesignFr.PopupMenu1;
  end;
  {$endif}
end;

function GetUniqueFieldName(aComponent: TComponent): String;
var
  S: String;
begin
  S := '';
  if aComponent is TdxEdit then
    S := rsText
  else if aComponent is TdxCalcEdit then
    S := rsNumber
  else if aComponent is TdxDateEdit then
    S := rsDate
  else if aComponent is TdxMemo then
    S := rsMemo
  else if aComponent is TdxCheckBox then
    S := rsCheckBox
  else if aComponent is TdxComboBox then
    S := rsList
  else if aComponent is TdxLookupComboBox then
    S := rsObject
  else if aComponent is TdxDBImage then
    S := rsImage
  else if aComponent is TdxFile then
    S := rsDsgnFile
  else if aComponent is TdxObjectField then
    S := rsObjField
  else if aComponent is TdxTimeEdit then
    S := rsTime
  else if aComponent is TdxCounter then
    S := rsCounter
  ;
  Result := S + IntToStr(GetId(aComponent));
end;

{ TMyMessenger }

// Только для TJvDesignerWinControlMessenger
function TMyMessenger.IsDesignMessage(ASender: TControl; var AMessage: TLMessage
  ): Boolean;
begin
  Result := inherited IsDesignMessage(ASender, AMessage);
	  case AMessage.Msg of
  	  LM_SETFOCUS, LM_KEYDOWN: Container.SetFocus;
  	end;
end;

{ TMyController }

function TMyController.KeyDown(AKeyCode: Cardinal): Boolean;
var
  SState: TShiftState;
  Key: Word;
begin
  SState := GetKeyShiftState;
  if (ssCtrl in SState) and (AKeyCode in [VK_C, VK_X, VK_V]) then
  begin
    case AKeyCode of
      VK_C: TFormDesigner(Surface).CopyComponents;
      VK_X: TFormDesigner(Surface).MoveComponents;
      VK_V: TFormDesigner(Surface).PasteComponents;
    end;
    Exit;
  end
  else
  begin
    Key := AKeyCode;
    with TFormDesigner(Surface) do
      if OnKeyDown <> nil then OnKeyDown(Surface, Key, SState);
  end;
  Result:=inherited KeyDown(AKeyCode);
end;

function TMyController.KeyUp(AKeyCode: Cardinal): Boolean;
begin
  Result := True;
  case AKeyCode of
    VK_ESCAPE: Action(daSelectParent);
    VK_DELETE: TFormDesigner(Surface).UserDeleteComponents;
    VK_PRIOR: TFormDesigner(Surface).BringToFront;
    VK_NEXT: TFormDesigner(Surface).SendToBack;
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT: DesignFr.UpdateStatusBar;
    else Result := False;
  end;
  //Result:=inherited KeyUp(AKeyCode);
end;

function TMyController.MouseDown(Button: TMouseButton; X, Y: Integer;
  TheMessage: TLMMouse): Boolean;
begin
  Result:=inherited MouseDown(Button, X, Y, TheMessage);
  FLeftButtonPressed := True;
  //HidePropsForm;
end;

function TMyController.MouseMove(X, Y: Integer; TheMessage: TLMMouse): Boolean;
begin
  Result:=inherited MouseMove(X, Y, TheMessage);
  if FLeftButtonPressed then HidePropsForm;
end;

function TMyController.MouseUp(Button: TMouseButton; X, Y: Integer;
  TheMessage: TLMMouse): Boolean;
begin
  Result:=inherited MouseUp(Button, X, Y, TheMessage);
  FLeftButtonPressed := False;
end;

procedure TFormDesigner.SurfaceAddComponent(Sender: TObject;
  aComponent: TComponent);

  procedure AddComponent(C: TComponent);
  var
    S: String;
  begin
    SetId(C, DBase.GenId('gen_fid'));
    S := GetFieldName(C);
    if S <> '' then
    begin
      if FindComponentByFieldName(GetForm, S, C) <> nil then
        SetFieldName(C, S + IntToStr(GetId(C)))
    end
    else
      SetFieldName( C, GetUniqueFieldName(C) );
    with TdxForm(Container).Grid.Columns.Add do
    begin
      Tag:=GetId(C);
      FieldName:=FieldStr(C);
      if C is TdxLookupComboBox then
      begin
        FieldName := FieldName + 'l';
        with TdxLookupComboBox(C) do
          InsertedValues:='';
      end
      else if C is TdxCheckBox then
        ButtonStyle:=cbsCheckBoxColumn
      else if C is TdxDBImage then
        FieldName := FieldName + 'thumb'
      else if C is TdxFile then
        FieldName := FieldName + 'd'
      else if C is TdxObjectField then
        with TdxObjectField(C) do
        begin
          ObjId:=0;
          FieldId:=0;
        end;
      Title.Caption := ' ';
      Width := 100;
    end;
  end;

  procedure AddGrid(C: TComponent);
  var
    Fm: TdxForm;
  begin
    Fm := FormMan.CreateNewForm;
    Fm.PId:=TdxForm(Container).Id;
    SetId(C, Fm.Id);
    DesignFr.AddChildForm(Form, Fm);
  end;

  procedure AddQueryGrid(C: TComponent);
  var
    RD, SrcRD: TReportData;
    QG: TdxQueryGrid;
    OldId: Integer;
    MS: TMemoryStream;
  begin
    QG :=  TdxQueryGrid(C);
    RD := ReportMan.CreateNewReport;
    RD.Kind:=rkQuery;
    if QG.Id = 0 then
      RD.Name:=rsQuery + IntToStr(RD.Id)
    else
    begin
      OldId := RD.Id;
      SrcRD := ReportMan.FindReport(QG.Id);
      if SrcRD <> nil then
      begin
        MS := TMemoryStream.Create;
        SrcRD.SaveToStream(MS);
        MS.Position:=0;
        RD.LoadFromStream(MS);
        MS.Free;
        RD.Name := RD.Name + IntToStr(OldId);
        RD.Id := OldId;
      end
      else
        RD.Name:=rsQuery + IntToStr(RD.Id)
    end;
    QG.Id:=RD.Id;
  end;

  procedure ChangeOwner(Ctrl: TWinControl);
  var
    i: Integer;
    C: TComponent;
    Bn: TSpeedButton;
  begin
    for i := Ctrl.ComponentCount - 1 downto 0 do
    begin
      C := Ctrl.Components[i];
      Ctrl.RemoveComponent(C);
      C.Name := DesignUniqueName(Container, C.ClassName);
      Container.InsertComponent(C);
      Messenger.DesignComponent(C, Active);
      if (C is TCustomDBEditButton) or (C is TdxLookupComboBox) then
      begin
        Bn := GetEditButton(C);
        Bn.AnchorToCompanion(akLeft,0,TControl(C));
      end
      else if C is TMyDBGrid then
      	TMyDBGrid(C).PositionButtons;
      if HasFId(C) then AddComponent(C)
      else if C is TdxGrid then
      begin
        if Form.PId = 0 then AddGrid(C)
        else C.Free;
      end
      else if C is TdxQueryGrid then
      begin
        AddQueryGrid(C);
        {if Form.PId = 0 then AddQueryGrid(C)
        else C.Free;}
      end
    end;
  end;

begin
  AssignDesignMenu(aComponent);

  if (FNewControl) and (aComponent is TControl) then
    with TControl(aComponent) do
    begin
      Left := (Left div 8) * 8;
      Top := (Top div 8) * 8;
      FNewControl := False;
    end;
  if HasFId(aComponent) then
    AddComponent(aComponent)
  else if aComponent is TdxGrid then
  begin
    if Form.PId = 0 then
      AddGrid(AComponent)
    else
    begin
      Selector.RemoveFromSelection(TControl(AComponent));         // 29.05.15
      AComponent.Free;
    end;
  end
  else if aComponent is TdxQueryGrid then
  begin
    AddQueryGrid(aComponent);
    {if Form.PId = 0 then AddQueryGrid(aComponent)
    else
    begin
      Selector.RemoveFromSelection(TControl(AComponent));
      AComponent.Free;
    end;  }
  end
  else if aComponent is TTabSheet then
    DeleteComponents
  else if (aComponent is TdxGroupBox) or (aComponent is TdxPageControl) then
    ChangeOwner(TWinControl(aComponent))
  else if (aComponent is TdxLabel) and (Trim(TdxLabel(aComponent).Expression) <> '') then
  begin
    if CalcLabelWithSameCaptionExists(TdxLabel(aComponent)) then
    	with TdxLabel(aComponent) do
	    begin
    	  Caption := Caption + IntToStr(ComponentIndex);
        FieldName := Caption;
  	  end;
  end;

  //else if aComponent is TdxButton then TdxButton(aComponent).ButtonName:=aComponent.Name;
end;

procedure TFormDesigner.SurfaceGetAddClass(Sender: TObject; var ioClass: string
  );
begin
  ioClass := FControlClass;
  TDesignFr(Owner).CursorBn.Down := True;
  FControlClass := '';
  FNewControl := ioClass <> '';
end;

procedure TFormDesigner.SurfaceDeleteComponent(Sender: TObject;
  aComponent: TComponent);

  procedure DeleteGridColumn(C: TComponent);
  var
    i, CId: Integer;
  begin
    CId := GetId(C);
    DeleteReferences(C);
    with TdxForm(Container).Grid do
      for i := Columns.Count - 1 downto 0 do
      begin
        if Columns[i].Tag = CId then
        begin
          Columns.Delete(i);
          Exit;
        end;
      end;
  end;

  procedure DeleteGrid(C: TComponent);
  var
    Fm: TdxForm;
  begin
    Fm := FormMan.FindForm(GetId(C));
    DeleteQueries(Fm);
 		DeleteReferences(Fm);
    with DesignFr.FormList.Items do
      Delete(IndexOfObject(Fm));
    if ScriptFm <> nil then ScriptFm.DeleteForm(Fm.Id);
    FormMan.DeleteForm(Fm.Id);
  end;

  procedure DeleteQueryGrid(C: TComponent);
  var
    RD: TReportData;
  begin
    RD := ReportMan.FindReport(TdxQueryGrid(C).Id);
    ReportMan.DeleteReport(RD);
  end;

  procedure DeleteContainer(Cont: TWinControl);
  var
    i: Integer;
    C: TControl;
  begin
    for i := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[i];
      if (C is TdxGroupBox) or (C is TdxPageControl) or (C is TTabSheet) then
        DeleteContainer(TWinControl(C))
      else if C is TdxGrid then
        DeleteGrid(C)
      else if C is TdxQueryGrid then
        DeleteQueryGrid(C)
      else if HasFId(C) then
        DeleteGridColumn(C);
    end;
  end;

begin
  if aComponent is TdxGrid then
    DeleteGrid(aComponent)
  else if aComponent is TdxQueryGrid then
    DeleteQueryGrid(aComponent)
  else if HasFId(aComponent) then
    DeleteGridColumn(aComponent)
  else if (aComponent is TdxGroupBox) or (aComponent is TTabSheet) or
    (aComponent is TdxPageControl) then DeleteContainer(TWinControl(aComponent));
end;

function TFormDesigner.GetControl: TControl;
begin
  Result := nil;
  if Length(Selected) > 0 then
    Result := Selection[0];
end;

function TFormDesigner.GetForm: TdxForm;
begin
  Result := TdxForm(Container);
end;

procedure TFormDesigner.SetActive(AValue: Boolean);
begin
  FMoves.Clear;
  FActive := AValue;
  inherited SetActive(AValue);
end;

function TFormDesigner.IsParentSelect(C: TControl): Boolean;
begin
  Result := False;
  if C is TdxForm then Exit;
  Result := Selector.IsSelected(C.Parent);
  if not Result then Result := IsParentSelect(C.Parent);
end;

function TFormDesigner.IsParent(C, aParent: TControl): Boolean;
begin
  Result := False;
  if C is TdxForm then Exit;
  Result := C.Parent = aParent;
  if not Result then Result := IsParent(C.Parent, aParent);
end;

function TFormDesigner.GetSelectionRect: TRect;
var
  i, L, x, y, r, b: Integer;
  C: TControl;
begin
  L := Length(Selected);
  x := 20000;
  for i := 0 to L - 1 do
    if Selection[i].Left < x then
      x := Selection[i].Left;
  y := 20000;
  for i := 0 to L - 1 do
    if Selection[i].Top < y then
      y := Selection[i].Top;
  r := 0;
  for i := 0 to L - 1 do
  begin
    C := Selection[i];
    if C.Left + C.Width > r then
      r := C.Left + C.Width;
  end;
  b := 0;
  for i := 0 to L - 1 do
  begin
    C := Selection[i];
    if C.Top + C.Height > b then
      b := C.Top + C.Height;
  end;
  Result := Rect(x, y, r, b);
end;

procedure TFormDesigner.AdjustPastedComponents;
var
  L, dx, dy, pw, ph: Integer;
  R: TRect;
  C: TControl;
  i: Integer;
  Par: TWinControl;
begin
  L := Length(Selected);
  if L = 0 then Exit;
  Par := Selection[0].Parent;
  pw := Par.Width;
  ph := Par.Height;
  R := GetSelectionRect;

  // Если область выделения вписывается в контейнер, то оставляем все как есть,
  // иначе перемещаем все в левый верхний угол.
  if (pw >= R.Right) and (ph >= R.Bottom) then Exit;
  dx := R.Right - pw; dy := R.Bottom - ph;
  if dx < 0 then dx := 0;
  if dy < 0 then dy := 0;
  if R.Left - dx < 0 then dx := R.Left;
  if R.Top - dy < 0 then dy := R.Top;
  for i := 0 to L - 1 do
  begin
    C := Selection[i];
    C.Left:=C.Left - dx;
    C.Top := C.Top - dy;
  end;
  UpdateDesigner;
end;

procedure TFormDesigner.SurfaceSelectionChange(Sender: TObject);
var
  i: Integer;
  C: TControl;
begin
  if Length(Selected) = 0 then
    ShowPropsForm(FParent.Parent, Container)
  else
  begin
    for i := Length(Selected) - 1 downto 0 do
    begin
      C := Selection[i];
      if C is TSpeedButton then
        Selector.RemoveFromSelection(C)
      else if IsParentSelect(C) then
      	Selector.RemoveFromSelection(C);
    end;
    if Length(Selected) = 1 then
      ShowPropsForm(FParent.Parent, Selection[0])
    else
      HidePropsForm;
  end;
  DesignFr.UpdateStatusBar;
  DesignFr.UpdateSelectComponent;
end;

constructor TFormDesigner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMoves := TList.Create;
  OnSelectionChange:=@SurfaceSelectionChange;
  OnGetAddClass:=@SurfaceGetAddClass;
  OnAddComponent:=@SurfaceAddComponent;
  OnDeleteComponent:=@SurfaceDeleteComponent;
  ControllerClass:=TMyController;
  {$ifndef windows}
  MessengerClass := TMyMessenger;
  {$endif}
end;

destructor TFormDesigner.Destroy;
begin
  FMoves.Free;
  inherited Destroy;
end;

procedure TFormDesigner.DesignForm(Fm: TWinControl);
var
  i: Integer;
begin
  if Fm = nil then
    Active := False;
  Container := Fm;
  if Fm <> nil then
  begin
    Active := True;
    {$ifndef windows}
    for i := 0 to Fm.ComponentCount - 1 do
      AssignDesignMenu(Fm.Components[i]);
    {$endif}
  end;
end;

function TFormDesigner.GetLeftMostEdge: Integer;
var
  i, X: Integer;
begin
  Result := 100000;
  for i := 0 to Length(Selected) - 1 do
  begin
    X := Selection[i].Left;
    if X < Result then
      Result := X;
  end;
end;

procedure TFormDesigner.SetLeftMostEdge;
var
  i, MinX: Integer;
begin
  MinX := GetLeftMostEdge;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Left := MinX;
  UpdateDesigner;
end;

function TFormDesigner.GetTopMostEdge: Integer;
var
  i, Y: Integer;
begin
  Result := 100000;
  for i := 0 to Length(Selected) - 1 do
  begin
    Y := Selection[i].Top;
    if Y < Result then
      Result := Y;
  end;
end;

procedure TFormDesigner.SetTopMostEdge;
var
  i, MinY: Integer;
begin
  MinY := GetTopMostEdge;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Top := MinY;
  UpdateDesigner;
end;

function TFormDesigner.GetRightMostEdge: Integer;
var
  i, X: Integer;
begin
  Result := 0;
  for i := 0 to Length(Selected) - 1 do
  begin
    X := Selection[i].Left + Selection[i].Width;
    if X > Result then
      Result := X;
  end;
end;

procedure TFormDesigner.SetRightMostEdge;
var
  i, MaxX: Integer;
begin
  MaxX := GetRightMostEdge;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Left := MaxX - Selection[i].Width;
  UpdateDesigner;
end;

function TFormDesigner.GetBottomMostEdge: Integer;
var
  i, Y: Integer;
begin
  Result := 0;
  for i := 0 to Length(Selected) - 1 do
  begin
    Y := Selection[i].Top + Selection[i].Height;
    if Y > Result then
      Result := Y;
  end;
end;

procedure TFormDesigner.SetBottomMostEdge;
var
  i, MaxY: Integer;
begin
  MaxY := GetBottomMostEdge;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Top := MaxY - Selection[i].Height;
  UpdateDesigner;
end;

procedure TFormDesigner.SetVertCenter;
var
  MinX, MaxX, i: Integer;
begin
  MinX := GetLeftMostEdge;
  MaxX := GetRightMostEdge;
  for i := 0 to Length(Selected) - 1 do
    with Selection[i] do
      Left := (MaxX - MinX) div 2 + MinX - Width div 2;
  UpdateDesigner;
end;

procedure TFormDesigner.SetHorzCenter;
var
  MinY, MaxY, i: Integer;
begin
  MinY := GetTopMostEdge;
  MaxY := GetBottomMostEdge;
  for i := 0 to Length(Selected) - 1 do
    with Selection[i] do
      Top := (MaxY - MinY) div 2 + MinY - Height div 2;
  UpdateDesigner;
end;

procedure TFormDesigner.SetMaxWidth;
var
  MaxN, N, i: Integer;
begin
  MaxN := 0;
  for i := 0 to Length(Selected) - 1 do
  begin
    N := Selection[i].Width;
    if N > MaxN then
      MaxN := N;
  end;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Width:=MaxN;
  UpdateDesigner;
end;

procedure TFormDesigner.SetMinWidth;
var
  MinN, N, i: Integer;
begin
  MinN := 10000;
  for i := 0 to Length(Selected) - 1 do
  begin
    N := Selection[i].Width;
    if N < MinN then
      MinN := N;
  end;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Width:=MinN;
  UpdateDesigner;
end;

procedure TFormDesigner.SetMaxHeight;
var
  MaxN, N, i: Integer;
begin
  MaxN := 0;
  for i := 0 to Length(Selected) - 1 do
  begin
    N := Selection[i].Height;
    if N > MaxN then
      MaxN := N;
  end;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Height:=MaxN;
  UpdateDesigner;
end;

procedure TFormDesigner.SetMinHeight;
var
  MinN, N, i: Integer;
begin
  MinN := 10000;
  for i := 0 to Length(Selected) - 1 do
  begin
    N := Selection[i].Height;
    if N < MinN then
      MinN := N;
  end;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Height:=MinN;
  UpdateDesigner;
end;

procedure TFormDesigner.MoveComponents;
var
  i: Integer;
  C: TControl;
begin
  FMoves.Clear;
  for i := 0 to Length(Selected) - 1 do
  begin
    C := Selection[i];
    if (not (C is TdxTabSheet)) and (not IsParentSelect(C)) then
      FMoves.Add(Selection[i]);
  end;
end;

procedure TFormDesigner.PasteComponents;
begin
  if FMoves.Count > 0 then
  begin
    ChangeParent;
    FMoves.Clear;
  end
  else
    inherited PasteComponents;
  AdjustPastedComponents;
end;

procedure TFormDesigner.DeleteComponents;
var
  i: Integer;
  C: TControl;
  Fm: TdxForm;
begin
  // Если есть таблицы, то проверяем возможность удаления
  for i := 0 to Length(Selected) - 1 do
  begin
    C := Selection[i];
    if C is TdxGrid then
    begin
      Fm := FormMan.FindForm(TdxGrid(C).Id);
    	if not CheckDeleteForm(Fm) then Exit;
    end;
  end;
  for i := 0 to Length(Selected) - 1 do
    FMoves.Remove(Selection[i]);
  inherited DeleteComponents;

  DesignFr.UpdateComponentTree;
  DesignFr.UpdateSummaryTree;
end;

procedure TFormDesigner.UserDeleteComponents;
begin
  if (Length(Selected) > 0) and ConfirmDelete and CheckDeleteComponents then
    DeleteComponents;
end;

procedure TFormDesigner.CopyComponents;
var
  i: Integer;
  C: TControl;
begin
  FMoves.Clear;
  for i := Length(Selected) - 1 downto 0 do
  begin
    C := Selection[i];
    if (C is TdxTabSheet) or (IsParentSelect(C)) then
      Selector.RemoveFromSelection(C);
  end;
  inherited CopyComponents;
end;

procedure TFormDesigner.ChangeParent;
var
  C: TWinControl;
  i: Integer;
  MovC: TControl;
begin
  if Length(Selected) = 0 then
    C := Container
  else
    C := TWinControl(Selection[0]);
  if (C is TdxForm) or (C is TdxGroupBox) or (C is TdxTabSheet) then
  begin
    ClearSelection;
    for i := 0 to FMoves.Count - 1 do
    begin
      MovC := TControl(FMoves[i]);
      if (MovC = C) or IsParent(C, MovC) then Continue;
      //if (MovC = C) or (MovC is TdxTabSheet) or ((MovC is TWinControl) and
      //  (TWinControl(MovC).FindChildControl(C.Name) <> nil)) then Continue;
      //if (MovC <> C) and (MovC is TWinControl) and (not (MovC is TdxTabSheet)) and
      //  (TWinControl(MovC).FindChildControl(C.Name) = nil) then
      MovC.Parent := C;
      Selector.AddToSelection(MovC);
    end;
  end;
end;

procedure TFormDesigner.FindLost;
var
  Fm: TdxForm;
  C: TComponent;
  i: Integer;
begin
  Selector.ClearSelection;
  Fm := GetForm;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TControl) then
      with TControl(C) do
        if (Left + Width < 0) or (Top + Height < 0) then
        begin
          Left := 0; Top := 0;
          Selector.AddToSelection(TControl(C));
        end;
  end;
end;

procedure TFormDesigner.BringToFront;
var
  i: Integer;
  C: TControl;
begin
  for i := 0 to Length(Selected) - 1 do
  begin
    C := Selection[i];
    C.BringToFront;
  end;
end;

procedure TFormDesigner.SendToBack;
var
  i: Integer;
  C: TControl;
begin
  for i := 0 to Length(Selected) - 1 do
  begin
    C := Selection[i];
    C.SendToBack;
  end;
end;

function TFormDesigner.CheckDeleteComponents: Boolean;
var
  C: TControl;
  i, fid, j, z: Integer;
  RD: TReportData;
  Sc: TRpSource;
  L: TList;

  function CheckField(const Fl: TRpField): Boolean;
  begin
    Result := True;
    if Fl.Fid = '' then Exit;
    if StrToInt(Fl.FId) = fid then
      Exit(False);
    if Fl.Src <> nil then
      Result := CheckField(Fl.Src^);
  end;

  function CheckFields(L: TRpFieldList): Boolean;
  var
    m: Integer;
    Fl: TRpField;
    Fm: TdxForm;
  begin
    Result := True;
    for m := 0 to L.Count - 1 do
    begin
      Fl := L[m]^;
      if not CheckField(Fl) then
      begin
        if RD.Kind = rkReport then
	        ErrMsg(Format(rsCantDeleteFieldReport, [GetFieldName(C), RD.Name]))
        else if RD.Kind = rkQuery then
        begin
          Fm := FindFormByRDId(RD.Id);
          ErrMsg(Format(rsCantDeleteFieldQuery, [GetFieldName(C), RD.Name, Fm.FormCaption]))
        end;
        Exit(False);
      end;
    end;
  end;

  procedure AddControls(WC: TWinControl);
  var
    j: Integer;
    C: TControl;
  begin
    for j := 0 to WC.ControlCount - 1 do
    begin
      C := WC.Controls[j];
      L.Add(C);
      if C is TWinControl then AddControls(TWinControl(C));
    end;
  end;

begin
  Result := True;
  L := TList.Create;
  try

  for i := 0 to Length(Selected) - 1 do
  begin
    C := Selection[i];
    L.Add(C);
    if C is TWinControl then AddControls(TWinControl(C));
  end;

  for i := 0 to L.Count - 1 do
  begin
    C := TControl(L[i]);
    if C is TdxGrid then
    begin
      Result := CheckDeleteForm(FormMan.FindForm(TdxGrid(C).Id));
      if not Result then Exit;
    end
    else if C is TdxQueryGrid then
      CheckDeleteQuery(GetForm, TdxQueryGrid(C).Id);

    if not IsField(C) then Continue;
    fid := GetId(C);

    for j := 0 to ReportMan.ReportCount - 1 do
    begin
      RD := ReportMan.Reports[j];
      for z := 0 to RD.Sources.Count - 1 do
      begin
        Sc := RD.Sources[z]^;
        Result := CheckFields(Sc.Fields);
        if not Result then Exit;
      end;
    end;
  end;

  finally
    L.Free;
  end;
end;

end.

