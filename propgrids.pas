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

unit PropGrids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, TypInfo, Types, LMessages, Spin,
  LclType, Laz.VirtualTrees, ColorBox, StdCtrls, FPCanvas, Themes, Dialogs,
  Buttons, StrUtils, Forms, ButtonPanel, LazUtf8, BGRABitmapTypes, strconsts,
  fontform;

const
  LM_BUTTONLINK_CLICK = LM_USER + 1;

type
  TVPGDataKind = (dkGroup, dkProp, dkPropList, dkOption);

  PVPGData = ^TVPGData;
  TVPGData = record
    Name, PropName: String;
    Kind: TVPGDataKind;
    Values: TStringList;
    Source: PVirtualNode;
  end;

  TVirtualPropGrid = class;

  { TMyColorBox }

  TMyColorBox = class(TColorBox)
  private
    procedure GetCustomColors(Sender: TCustomColorBox; AItems: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorName(C: TColor): String;
    procedure SetSelectedColor(AValue: TColor);
  end;

  { TVPGColorLink }

  TVPGColorLink = class(TInterfacedObject, IVTEditLink)
  private
    FControl: TMyColorBox;
    FButton: TSpeedButton;
    FGrid: TVirtualPropGrid;
    FNode: PVirtualNode;
    FData: PVPGData;
    FObj: TObject;
    FOldColor,              // Цвет до начала редактирования
    FCurrentColor: TColor;  // Текущий выбранный цвет
    FCbxDroppedDown: Boolean;
    procedure ButtonClick(Sender: TObject);
    procedure ControlCloseUp(Sender: TObject);
    procedure ControlDropDown(Sender: TObject);
    procedure ControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure AcceptChanges;
    procedure ControlSelect(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    procedure ProcessMessage(var Message: TLMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  { TVPGIntEditLink }

  TVPGIntEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FControl: TSpinEdit;
    FGrid: TVirtualPropGrid;
    FNode: PVirtualNode;
    FData: PVPGData;
    FObj: TObject;
    FOldValue, FCurrentValue: Integer;
    procedure ControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AcceptChanges;
  public
    constructor Create;
    destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    procedure ProcessMessage(var Message: TLMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  { TVPGComboBoxLink }

  TVPGComboBoxLink = class(TInterfacedObject, IVTEditLink)
  private
    FControl: TComboBox;
    FGrid: TVirtualPropGrid;
    FNode: PVirtualNode;
    FData: PVPGData;
    FObj: TObject;
    FOldIndex, FCurrentIndex: Integer;
    FPropInfo: PPropInfo;
    FCbxDroppedDown: Boolean;
    procedure ControlCloseUp(Sender: TObject);
    procedure ControlDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ControlDropDown(Sender: TObject);
    procedure ControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ControlSelect(Sender: TObject);
    procedure AcceptChanges;
  public
    constructor Create;
    destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    procedure ProcessMessage(var Message: TLMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  { TVPGButtonLink }

  TVPGButtonLink = class(TInterfacedObject, IVTEditLink)
  private
    FControl: TSpeedButton;
    FGrid: TVirtualPropGrid;
    FNode: PVirtualNode;
    FData: PVPGData;
    FObj: TObject;
    procedure ControlClick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    procedure ProcessMessage(var Message: TLMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  { TVPGStringLink }

  TVPGStringLink = class(TInterfacedObject, IVTEditLink)
  private
    FControl: TEdit;
    FGrid: TVirtualPropGrid;
    FNode: PVirtualNode;
    FData: PVPGData;
    FObj: TObject;
    FColumn: TColumnIndex;
    FOldValue, FCurrentValue: String;
    procedure AcceptChanges;
    procedure ControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  public
    constructor Create;
    destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    procedure ProcessMessage(var Message: TLMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  { TStringsForm }

  TStringsForm = class(TCustomForm)
  private
    FMemo: TMemo;
    FButtons: TButtonPanel;
  protected
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function ShowForm(SL: TStrings): Integer;
  end;

  TVPGNeedValuesEvent = procedure (Sender: TObject; const PropName: String;
    Values: TStrings) of object;
  TVPGEditObjectEvent = procedure (Sender: TObject; const PropName: String;
    Obj: TObject; var Accept: Boolean) of object;
  TVPGPropValidateEvent = procedure (Sender: TObject; const PropName, PropValue: String;
    var CanChange: Boolean) of object;
  TVPGPropChangeEvent = procedure (Sender: TObject; const PropName: String) of object;
  TVPGEditingPropEvent = procedure (Sender: TObject; const PropName: String;
    var Allowed: Boolean) of object;

  { TVirtualPropGrid }

  TVirtualPropGrid = class(TLazVirtualDrawTree)
  private
    FModified: Boolean;
    FOnEditObject: TVPGEditObjectEvent;
    FOnNeedValues: TVPGNeedValuesEvent;
    FOnPropChange: TVPGPropChangeEvent;
    FOnPropEditing: TVPGEditingPropEvent;
    FOnPropValidate: TVPGPropValidateEvent;
    FTIObject: TObject;
    FColorBox: TMyColorBox;
    FCheckImg, FUnCheckImg: TBitmap;
    FDrawNode: PVirtualNode;
    //FExpands: array of Boolean;
    FNodePathes: TStringList;
    FSelectedPath, FPrevSelectedPath: String;
    FColorLink, FIntLink, FButtonLink, FStringLink, FListLink: IVTEditLink;
    procedure TreeAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect);
    procedure TreeAfterItemPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure TreeCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out AEditLink: IVTEditLink);
    procedure TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var ANodeDataSize: Integer);
    procedure GetPropVal(PD: PVPGData; out PropInfo: PPropInfo; out Value: String);
    procedure DrawCheckBox(C: TCanvas; R: TRect; Value: Boolean);
    procedure DrawStrings(C: TCanvas; R: TRect; SL: TStrings);
    procedure DrawCollection(C: TCanvas; R: TRect; Col: TCollection);
    procedure DrawFont(C: TCanvas; R: TRect; F: TFont);
    procedure DrawPropColor(C: TCanvas; R: TRect; Clr: TColor);
    procedure DrawPropValue(C: TCanvas; R: TRect; PD: PVPGData);
    procedure DrawPropOption(C: TCanvas; R: TRect; PD, ParPD: PVPGData);
    procedure DrawPropList(C: TCanvas; R: TRect; PD: PVPGData);
    procedure GetDefaultItems(const PropName: String; Items: TStrings);
    function ToggleCheckNode(N: PVirtualNode): Boolean;
    procedure TreeKeyPress(Sender: TObject; var Key: char);
    procedure TreeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function GetNodePath(N: PVirtualNode): String;
  protected
    procedure WMVScroll(var Message: TLMVScroll); message LM_VSCROLL;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
      override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoNeedValues(const PropName: String; Items: TStrings);
    procedure DoDrawItem(const PropName, PropValue, AText: String; C: TCanvas; R: TRect);
    procedure DoEditObject(const PropName: String; Obj: TObject; var Accept: Boolean);
    function DoPropValidate(const PropName, PropValue: String): Boolean;
    procedure DoPropChange(Node: PVirtualNode; const PropName: String);
    procedure InvalidateNodeWithParents(N: PVirtualNode);
    procedure InvalidateNodeWithChildren(N: PVirtualNode);
    function AddNode(const AName, PropName: String; Kind: TVPGDataKind;
      Values: array of String; CreateValues: Boolean; AParent, Source: PVirtualNode): PVirtualNode;
    function AddGroup(const AName: String; AParent: PVirtualNode): PVirtualNode;
    function AddProp(const AName, PropName: String; AParent: PVirtualNode): PVirtualNode;
    function AddOption(const AName, PropName: String; AParent: PVirtualNode): PVirtualNode;
    function AddPropList(const AName, PropName: String; AParent, ASource: PVirtualNode): PVirtualNode;
    procedure SaveExpandState;
    procedure RestoreExpandState;
    property TIObject: TObject read FTIObject write FTIObject;
    property Modified: Boolean read FModified write FModified;
  published
    property OnNeedValues: TVPGNeedValuesEvent read FOnNeedValues write FOnNeedValues;
    property OnEditObject: TVPGEditObjectEvent read FOnEditObject write FOnEditObject;
    property OnPropValidate: TVPGPropValidateEvent read FOnPropValidate write
      FOnPropValidate;
    property OnPropChange: TVPGPropChangeEvent read FOnPropChange write
      FOnPropChange;
    property OnPropEditing: TVPGEditingPropEvent read FOnPropEditing write
      FOnPropEditing;
  end;

implementation

uses
  apputils;

function HasOption(const Options, Value: String): Boolean;
begin
  Result := Pos(',' + Value + ',', ',' + Options + ',') > 0;
end;

procedure ExcludeOption(var Options: String; Value: String);
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  SL.Delimiter := ',';
  SL.DelimitedText := Options;
  i := SL.IndexOf(Value);
  if i >= 0 then SL.Delete(i);
  Options := SL.DelimitedText;
  SL.Free;
end;

procedure IncludeOption(var Options: String; Value: String);
begin
  if Options = '' then
    Options := Value
  else
    Options := Options + ',' + Value;
end;

function GetDefaultTextStyle(C: TCanvas): TTextStyle;
begin
  Result := C.TextStyle;
  Result.Layout := tlCenter;
end;

{procedure GetLowPropInfo(Obj: TObject; PropName: String; out LowObj: TObject; out PropInfo: PPropInfo);
var
  p, p2: SizeInt;
  S: String;
  Idx: LongInt;
begin
  while True do
  begin
    p := Pos('.', PropName);
    if p = 0 then Break;

    S := Copy(PropName, 1, p - 1);
    Delete(PropName, 1, p);
    p := Pos('[', S);
    if p > 0 then
    begin
      p2 := Pos(']', S);
      Idx := StrToInt(Copy(S, p + 1, p2 - p - 1));
      Delete(S, p, p2 - p + 1);
    end
    else Idx := -1;
    Obj := GetObjectProp(Obj, S);
    if Idx >= 0 then
    begin
      if Obj is TCollection then
        Obj := TCollection(Obj).Items[Idx];
    end;
  end;
  PropInfo := GetPropInfo(Obj, PropName);
  LowObj := Obj;
end;    }

function GetLowObjProp(Obj: TObject; const PropName: String): TObject;
var
  PropInfo: PPropInfo;
begin
  GetLowPropInfo(Obj, PropName, Obj, PropInfo);
  Result := GetObjectProp(Obj, PropInfo);
end;

function GetStringsProp(Obj: TObject; const PropName: String): String;
var
  PropInfo: PPropInfo;
  p: SizeInt;
  Idx: LongInt;
begin
  GetLowPropInfo(Obj, PropName, Obj, PropInfo);
  if Obj is TStrings then
  begin
    p := RPos('[', PropName);
    Idx := StrToInt(Copy(PropName, p, Length(PropName) - p));
    Result := TStrings(Obj)[Idx];
  end;
end;

procedure SetLowOrdProp(Obj: TObject; const PropName: String; Value: Integer);
var
  PropInfo: PPropInfo;
begin
  GetLowPropInfo(Obj, PropName, Obj, PropInfo);
  SetOrdProp(Obj, PropInfo, Value);
end;

procedure SetLowEnumProp(Obj: TObject; const PropName: String; const Value: String);
var
  PropInfo: PPropInfo;
begin
  GetLowPropInfo(Obj, PropName, Obj, PropInfo);
  SetEnumProp(Obj, PropInfo, Value);
end;

procedure SetLowSetProp(Obj: TObject; const PropName: String; const Value: String);
var
  PropInfo: PPropInfo;
begin
  GetLowPropInfo(Obj, PropName, Obj, PropInfo);
  SetSetProp(Obj, PropInfo, Value);
end;

procedure SetLowStrProp(Obj: TObject; const PropName: String; const Value: String);
var
  PropInfo: PPropInfo;
begin
  GetLowPropInfo(Obj, PropName, Obj, PropInfo);
  SetStrProp(Obj, PropInfo, Value);
end;

{ TStringsForm }

procedure TStringsForm.DoShow;
begin
  inherited DoShow;
  FMemo.SetFocus;
end;

constructor TStringsForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsTextEditor;
  Width := 400;
  Height := 400;
  Position := poOwnerFormCenter;
  BorderIcons:=[biSystemMenu];
  FMemo := TMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Align := alClient;
  FMemo.BorderSpacing.Around := 4;
  FButtons := TButtonPanel.Create(Self);
  FButtons.Parent := Self;
  FButtons.ShowBevel := False;
  FButtons.ShowButtons := [pbOk, pbCancel];
end;

function TStringsForm.ShowForm(SL: TStrings): Integer;
begin
  FMemo.Lines := SL;
  Result := ShowModal;
  if Result = mrOk then
    SL.Assign(FMemo.Lines);
end;

{ TVPGStringLink }

procedure TVPGStringLink.AcceptChanges;
begin
  if FControl.Text = FCurrentValue then Exit;
  if not FGrid.DoPropValidate(FData^.PropName, FControl.Text) then Exit;
  SetLowStrProp(FObj, FData^.PropName, FControl.Text);
  FGrid.DoPropChange(FNode, FData^.PropName);
  FCurrentValue := FControl.Text;
end;

procedure TVPGStringLink.ControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    FControl.Text := FOldValue;
    FControl.SelectAll;
    AcceptChanges;
    Key := 0;
  end
  else if Key = VK_RETURN then
  begin
    AcceptChanges;
    Key := 0;
  end
  else if Key in [VK_DOWN, VK_UP] then
  begin
    FGrid.KeyDown(Key, Shift);
    Key := 0;
  end;
end;

constructor TVPGStringLink.Create;
begin
  FControl := TEdit.Create(nil);
  FControl.Visible := False;
  FControl.OnKeyDown:=@ControlKeyDown;
end;

destructor TVPGStringLink.Destroy;
begin
  FControl.Free;
  inherited Destroy;
end;

function TVPGStringLink.BeginEdit: Boolean; stdcall;
begin
  FControl.Show;
  FControl.SetFocus;
  Result := True;
end;

function TVPGStringLink.CancelEdit: Boolean; stdcall;
begin
  FControl.Hide;
  Result := True;
end;

function TVPGStringLink.EndEdit: Boolean; stdcall;
begin
  AcceptChanges;
  FControl.Hide;
  Result := True;
end;

function TVPGStringLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex): Boolean; stdcall;
var
  S: String;
  PropInfo: PPropInfo;
begin
  Result := True;
  FControl.Height := Node^.NodeHeight;

  FGrid := TVirtualPropGrid(Tree);
  FNode := Node;
  FColumn := Column;
  FData := FGrid.GetNodeData(Node);
  FObj := FGrid.TIObject;

  FGrid.GetPropVal(FData, PropInfo, S);
  FControl.Parent := FGrid;
  FControl.Font := FGrid.Font;
  FControl.Text := S;
  FOldValue := S;
  FCurrentValue := S;
end;

function TVPGStringLink.GetBounds: TRect; stdcall;
begin
  Result := FControl.BoundsRect;
end;

procedure TVPGStringLink.ProcessMessage(var Message: TLMessage); stdcall;
begin
  FControl.WindowProc(Message);
end;

procedure TVPGStringLink.SetBounds(R: TRect); stdcall;
begin
  R.Left := R.Left - FGrid.TextMargin;
  FControl.BoundsRect := R;
end;

{ TVPGButtonLink }

procedure ShowFontDlg(F: TFont);
var
  DefF: TFont;
begin
  DefF := TFont.Create;
  DefF.Name := 'Verdana';
  DefF.Size := 10;
  DefF.Color := clBlack;
  ShowFontForm(F, DefF);
  DefF.Free;
end;

procedure TVPGButtonLink.ControlClick(Sender: TObject);
var
  Obj: TObject;
  Accept: Boolean;
begin
  Obj := GetLowObjProp(FObj, FData^.PropName);
  if Obj is TFont then
    ShowFontDlg(TFont(Obj))
  else if Obj is TStrings then
    with TStringsForm.CreateNew(nil) do
    begin
      Accept := ShowForm(TStrings(Obj)) = mrOk;
      Free;
    end
  else
    FGrid.DoEditObject(FData^.PropName, Obj, Accept);
  if Accept then
    FGrid.InvalidateNodeWithChildren(FNode);
end;

constructor TVPGButtonLink.Create;
begin
  FControl := TSpeedButton.Create(nil);
  FControl.Visible := False;
  FControl.Caption := '...';
  FControl.OnClick:=@ControlClick;
end;

destructor TVPGButtonLink.Destroy;
begin
  FControl.Free;
  inherited Destroy;
end;

function TVPGButtonLink.BeginEdit: Boolean; stdcall;
begin
  FControl.Show;
  //FControl.SetFocus;
  Result := True;
end;

function TVPGButtonLink.CancelEdit: Boolean; stdcall;
begin
  FControl.Hide;
  Result := True;
end;

function TVPGButtonLink.EndEdit: Boolean; stdcall;
begin
  FControl.Hide;
  Result := True;
end;

function TVPGButtonLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex): Boolean; stdcall;
begin
  Result := True;
  FControl.Height := Node^.NodeHeight;

  FGrid := TVirtualPropGrid(Tree);
  FNode := Node;
  FData := FGrid.GetNodeData(Node);
  FObj := FGrid.TIObject;

  FControl.Parent := FGrid;
end;

function TVPGButtonLink.GetBounds: TRect; stdcall;
begin
  Result := FControl.BoundsRect;
end;

procedure TVPGButtonLink.ProcessMessage(var Message: TLMessage); stdcall;
begin
  if Message.msg = LM_BUTTONLINK_CLICK then
    FControl.Click
  else
    FControl.WindowProc(Message);
end;

procedure TVPGButtonLink.SetBounds(R: TRect); stdcall;
begin
  //R.Height := FNode^.NodeHeight - 2;
  R.Left := R.Right - R.Height;
  FControl.BoundsRect := R;
end;

{ TVPGComboBoxLink }

procedure TVPGComboBoxLink.ControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    FControl.ItemIndex := FOldIndex;
    AcceptChanges;
    Key := 0;
  end
  // Использую переменную вместо FControl.DroppedDown, т. к. при нажатии
  // ENTER список уже закрыт и получается список будет всегда раскрываться.
  else if Key = VK_RETURN then
  begin
    if not FCbxDroppedDown then
      FControl.DroppedDown := True
    else
      AcceptChanges;
    Key := 0;
  end
  else if (Key in [VK_DOWN, VK_UP]) and not FCbxDroppedDown then
  begin
    FGrid.KeyDown(Key, Shift);
    Key := 0;
  end;
end;

procedure TVPGComboBoxLink.ControlDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  C: TCanvas;
begin
  C := FControl.Canvas;
  C.FillRect(ARect);
  C.Font := FControl.Font;
  if odSelected in State then
    C.Font.Color := clHighlightText;
  Inc(ARect.Left, 1);
  Inc(ARect.Top, 1);
  Dec(ARect.Bottom, 1);
  FGrid.DoDrawItem(FData^.PropName, FData^.Values.Names[Index],
    FControl.Items[Index], C, ARect);
end;

procedure TVPGComboBoxLink.ControlDropDown(Sender: TObject);
begin
  FCbxDroppedDown := True;
end;

procedure TVPGComboBoxLink.ControlCloseUp(Sender: TObject);
begin
  FCbxDroppedDown := False;
end;

procedure TVPGComboBoxLink.ControlSelect(Sender: TObject);
begin
  AcceptChanges;
end;

procedure TVPGComboBoxLink.AcceptChanges;
var
  N: PVirtualNode;
  PD: PVPGData;
  S: String;
  LowObj: TObject;
  PInfo: PPropInfo;
begin
  if FControl.ItemIndex = FCurrentIndex then Exit;

  with FControl do
    if ItemIndex >= 0 then
      S := FData^.Values.Names[ItemIndex]
    else
      S := '';

  if not FGrid.DoPropValidate(FData^.PropName, S) then Exit;
  if FPropInfo^.PropType^.Kind in [tkInteger, tkEnumeration, tkBool] then
    SetLowOrdProp(FObj, FData^.PropName, StrToInt(S))
  else
    SetLowStrProp(FObj, FData^.PropName, S);
  FGrid.DoPropChange(FNode, FData^.PropName);

  N := FGrid.GetFirst;
  while N <> nil do
  begin
    PD := FGrid.GetNodeData(N);
    if (PD^.Kind = dkPropList) and (PD^.Source = FNode) then
    begin
      GetLowPropInfo(FObj, PD^.PropName, LowObj, PInfo);
      if PInfo^.PropType^.Kind in [tkInteger, tkEnumeration, tkBool] then
        SetOrdProp(LowObj, PInfo, 0)
      else
        SetStrProp(LowObj, PInfo, '');
      PD^.Values.Clear;
      FGrid.DoNeedValues(PD^.PropName, PD^.Values);
      FGrid.DoPropChange(N, PD^.PropName);
    end;
    N := FGrid.GetNext(N);
  end;

  FCurrentIndex := FControl.ItemIndex;
end;

constructor TVPGComboBoxLink.Create;
begin
  FControl := TComboBox.Create(nil);
  FControl.Visible := False;
  //FControl.DropDownCount := 20;
  FControl.Style := csOwnerDrawFixed;
  FControl.OnKeyDown:=@ControlKeyDown;
  FControl.OnSelect:=@ControlSelect;
  FControl.OnDrawItem:=@ControlDrawItem;
  FControl.OnDropDown:=@ControlDropDown;
  FControl.OnCloseUp:=@ControlCloseUp;
  FOldIndex := -1;
end;

destructor TVPGComboBoxLink.Destroy;
begin
  FControl.Free;
  inherited Destroy;
end;

function TVPGComboBoxLink.BeginEdit: Boolean; stdcall;
begin
  FControl.Show;
  FControl.SetFocus;
  Result := True;
end;

function TVPGComboBoxLink.CancelEdit: Boolean; stdcall;
begin
  FControl.Hide;
  Result := True;
end;

function TVPGComboBoxLink.EndEdit: Boolean; stdcall;
begin
  AcceptChanges;
  FControl.Hide;
  Result := True;
end;

function TVPGComboBoxLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  PInfo: PPropInfo;
  S: String;
  i: Integer;
begin
  Result := True;
  FGrid := TVirtualPropGrid(Tree);
  FNode := Node;
  FData := FGrid.GetNodeData(Node);
  FObj := FGrid.TIObject;

  FControl.Parent := FGrid;
  FControl.Font := FGrid.Font;
  FGrid.GetPropVal(FData, PInfo, S);
  FControl.Clear;
  for i := 0 to FData^.Values.Count - 1 do
    FControl.Items.Add(FData^.Values.ValueFromIndex[i]);
  FControl.ItemIndex := FData^.Values.IndexOfName(S);

  if FControl.Items.Count < 20 then
    FControl.DropDownCount := FControl.Items.Count
  else
    FControl.DropDownCount := 20;

  FOldIndex := FControl.ItemIndex;
  FCurrentIndex := FOldIndex;
  FPropInfo := PInfo;
end;

function TVPGComboBoxLink.GetBounds: TRect; stdcall;
begin
  Result := FControl.BoundsRect;
end;

procedure TVPGComboBoxLink.ProcessMessage(var Message: TLMessage); stdcall;
begin
  FControl.WindowProc(Message);
end;

procedure TVPGComboBoxLink.SetBounds(R: TRect); stdcall;
begin
  R.Left := R.Left - FGrid.TextMargin;
  FControl.BoundsRect := R;
  FControl.ItemHeight := ScaleToScreen(Abs(FControl.Font.Height));
end;

{ TVPGColorLink }

procedure TVPGColorLink.ControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    FControl.Selected := FOldColor;
    AcceptChanges;
    Key := 0;
  end
  else if Key = VK_RETURN then
  begin
    if not FCbxDroppedDown then
    begin
      if ssShift in Shift then
        FButton.Click
      else
        FControl.DroppedDown := True
    end
    else
      AcceptChanges;
    Key := 0;
  end
  else if (Key in [VK_DOWN, VK_UP]) and not FCbxDroppedDown then
  begin
    FGrid.KeyDown(Key, Shift);
    Key := 0;
  end
end;

procedure TVPGColorLink.ButtonClick(Sender: TObject);
begin
  with TColorDialog.Create(nil) do
  begin
    Color:=FControl.Selected;
    if Execute then
    begin
      FControl.SetSelectedColor(Color);
      AcceptChanges;
    end;
    Free;
  end;
end;

procedure TVPGColorLink.ControlCloseUp(Sender: TObject);
begin
  FCbxDroppedDown := False;
end;

procedure TVPGColorLink.ControlDropDown(Sender: TObject);
begin
  FCbxDroppedDown := True;
end;

procedure TVPGColorLink.AcceptChanges;
begin
  if FCurrentColor = FControl.Selected then Exit;
  if not FGrid.DoPropValidate(FData^.PropName, IntToStr(FControl.Selected)) then Exit;
  SetLowOrdProp(FObj, FData^.PropName, FControl.Selected);
  FGrid.DoPropChange(FNode, FData^.PropName);
  FCurrentColor := FControl.Selected;
end;

procedure TVPGColorLink.ControlSelect(Sender: TObject);
begin
  AcceptChanges;
end;

constructor TVPGColorLink.Create;
begin
  FControl := TMyColorBox.Create(nil);
  FControl.AutoSize := True;
  FControl.Visible := False;
  FControl.DropDownCount := 20;
  FControl.OnKeyDown:=@ControlKeyDown;
  FControl.OnSelect:=@ControlSelect;
  FControl.OnDropDown:=@ControlDropDown;
  FControl.OnCloseUp:=@ControlCloseUp;
  FButton := TSpeedButton.Create(nil);
  FButton.Caption := '...';
  FButton.OnClick:=@ButtonClick;
end;

destructor TVPGColorLink.Destroy;
begin
  FControl.Free;
  FButton.Free;
  inherited Destroy;
end;

function TVPGColorLink.BeginEdit: Boolean; stdcall;
begin
  FControl.Show;
  FButton.Show;
  FControl.SetFocus;
  Result := True;
end;

function TVPGColorLink.CancelEdit: Boolean; stdcall;
begin
  FControl.Selected := FOldColor;
  AcceptChanges;
  FControl.Hide;
  FButton.Hide;
  Result := True;
end;

function TVPGColorLink.EndEdit: Boolean; stdcall;
begin
  AcceptChanges;
  FControl.Hide;
  FButton.Hide;
  Result := True;
end;

function TVPGColorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex): Boolean; stdcall;
var
  PInfo: PPropInfo;
  S: String;
begin
  Result := True;
  FGrid := TVirtualPropGrid(Tree);
  FNode := Node;
  //FColumn := Column;
  FData := FGrid.GetNodeData(Node);
  FObj := FGrid.TIObject;

  FControl.Parent := FGrid;
  FControl.Font := FGrid.Font;
  FGrid.GetPropVal(FData, PInfo, S);
  FControl.SetSelectedColor(TColor(StrToInt(S)));
  FOldColor := FControl.Selected;
  FCurrentColor := FOldColor;

  FButton.Parent := FGrid;
end;

function TVPGColorLink.GetBounds: TRect; stdcall;
begin
  Result := FControl.BoundsRect;
end;

procedure TVPGColorLink.ProcessMessage(var Message: TLMessage); stdcall;
begin
  FControl.WindowProc(Message);
end;

procedure TVPGColorLink.SetBounds(R: TRect); stdcall;
var
  H: Integer;
begin
  H := R.Height;
  FButton.SetBounds(R.Right - H, R.Top, H, H);
  R.Left := R.Left - FGrid.TextMargin;
  R.Right := R.Right - H;
  FControl.BoundsRect := R;
  FControl.ItemHeight := ScaleToScreen(Abs(FControl.Font.Height));
end;

{ TMyColorBox }

procedure TMyColorBox.GetCustomColors(Sender: TCustomColorBox; AItems: TStrings
  );
var
  i: Integer;
begin
  for i := 0 to CSSColors.Count - 1 do
    AItems.AddObject(CSSColors.Name[i], TObject(PtrInt(BGRAToColor(CSSColors.ByIndex[i]))));
end;

procedure TMyColorBox.SetSelectedColor(AValue: TColor);
begin
  if Copy(Items[0], 1, 4) = 'rgb:' then Items.Delete(0);
  Selected := AValue;
  ItemIndex := Items.IndexOfObject(TObject(PtrInt(AValue)));
  if ItemIndex >= 0 then Exit;

  Items.InsertObject(0, GetColorName(AValue), TObject(PtrInt(AValue)));
  ItemIndex := 0;
end;

constructor TMyColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {Style := [cbCustomColor, cbExtendedColors, cbIncludeDefault, cbIncludeNone,
    cbPrettyNames, cbStandardColors, cbSystemColors];}
  OnGetColors:=@GetCustomColors;
  Style := [cbCustomColors];
end;

function TMyColorBox.GetColorName(C: TColor): String;
var
  i: Integer;
  RGB: LongInt;
begin
  Result := '';
  for i := 0 to Items.Count - 1 do
    if Colors[i] = C then Exit(ColorNames[i]);
  RGB := ColorToRGB(C);
  Result := Format('rgb: %d,%d,%d', [Red(RGB), Green(RGB), Blue(RGB)]);
end;

{ TVPGIntEditLink }

procedure TVPGIntEditLink.ControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    FControl.Value := FOldValue;
    FControl.SelectAll;
    AcceptChanges;
    Key := 0;
  end
  else if Key = VK_RETURN then
  begin
    AcceptChanges;
    Key := 0;
  end
  else if Key in [VK_DOWN, VK_UP] then
  begin
    FGrid.KeyDown(Key, Shift);
    Key := 0;
  end;
end;

procedure TVPGIntEditLink.AcceptChanges;
begin
  if FCurrentValue = FControl.Value then Exit;
  if not FGrid.DoPropValidate(FData^.PropName, FControl.Text) then Exit;
  SetLowOrdProp(FObj, FData^.PropName, FControl.Value);
  FGrid.DoPropChange(FNode, FData^.PropName);
  FCurrentValue := FControl.Value;
end;

constructor TVPGIntEditLink.Create;
begin
  FControl := TSpinEdit.Create(nil);
  FControl.Visible := False;
  FControl.MaxValue := 0;
  FControl.OnKeyDown:=@ControlKeyDown;
end;

destructor TVPGIntEditLink.Destroy;
begin
  FControl.Free;
  inherited Destroy;
end;

function TVPGIntEditLink.BeginEdit: Boolean; stdcall;
begin
  FControl.Show;
  FControl.SetFocus;
  Result := True;
end;

function TVPGIntEditLink.CancelEdit: Boolean; stdcall;
begin
  FControl.Hide;
  Result := True;
end;

function TVPGIntEditLink.EndEdit: Boolean; stdcall;
begin
  AcceptChanges;
  FControl.Hide;
  Result := True;
end;

function TVPGIntEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  PInfo: PPropInfo;
  S: String;
begin
  Result := True;
  FControl.Height := Node^.NodeHeight;

  FGrid := TVirtualPropGrid(Tree);
  FNode := Node;
  //FColumn := Column;
  FData := FGrid.GetNodeData(Node);
  FObj := FGrid.TIObject;

  FControl.Parent := FGrid;
  FControl.Font := FGrid.Font;
  FGrid.GetPropVal(FData, PInfo, S);
  FControl.Text := S;
  FOldValue := FControl.Value;
  FCurrentValue := FOldValue;
end;

function TVPGIntEditLink.GetBounds: TRect; stdcall;
begin
  Result := FControl.BoundsRect;
end;

procedure TVPGIntEditLink.ProcessMessage(var Message: TLMessage); stdcall;
begin
  FControl.WindowProc(Message);
end;

procedure TVPGIntEditLink.SetBounds(R: TRect); stdcall;
begin
  R.Left := R.Left - FGrid.TextMargin;
  FControl.BoundsRect := R;
end;

{ TVirtualPropGrid }

procedure TVirtualPropGrid.TreeAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellRect: TRect);
var
  PD: PVPGData;
  C: TCanvas;
  x: Integer;
  OldBrushStyle: TFPBrushStyle;
  OldPenStyle: TFPPenStyle;
begin
  FDrawNode := Node;
  PD := GetNodeData(Node);
  C := TargetCanvas;
  C.Font := Font;

  OldBrushStyle := C.Brush.Style;
  OldPenStyle := C.Pen.Style;

  if PD^.Kind <> dkGroup then
  begin
    if vsSelected in Node^.States then
      C.Font.Color := clHighlightText;
    if Column = 0 then
    begin
      if Node^.Parent = Sender.RootNode then
        C.Font.Style := [fsBold];
      x := CellRect.Left + Indent * (GetNodeLevel(Node) + 1) +
        Margin + TextMargin;
      C.TextRect(CellRect, x, CellRect.Top, PD^.Name, GetDefaultTextStyle(C));
    end
    else
      case PD^.Kind of
        dkProp: DrawPropValue(C, CellRect, PD);
        dkPropList: DrawPropList(C, CellRect, PD);
        dkOption: DrawPropOption(C, CellRect, PD, GetNodeData(Node^.Parent));
      end;
  end;
  C.Brush.Style := OldBrushStyle;
  C.Pen.Style := OldPenStyle;
end;

procedure TVirtualPropGrid.TreeAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
var
  PD: PVPGData;
  C: TCanvas;
begin
  PD := GetNodeData(Node);
  C := TargetCanvas;
  if PD^.Kind = dkGroup then
  begin
    C.Font := Font;
    if Node^.Parent = Sender.RootNode then
      C.Font.Style := [fsBold];
    if vsSelected in Node^.States then
      C.Font.Color := clHighlightText;
    C.TextRect(ItemRect, ItemRect.Left + Indent *
      (GetNodeLevel(Node) + 1) + Margin + TextMargin,
      ItemRect.Top, PD^.Name, GetDefaultTextStyle(C));
  end;
end;

procedure TVirtualPropGrid.TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PVPGData;
begin
  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);
  Result := Utf8CompareText(Data1^.Name, Data2^.Name);
end;

procedure TVirtualPropGrid.TreeCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out AEditLink: IVTEditLink);
var
  PInfo: PPropInfo;
  S: String;
  PD: PVPGData;
begin
  if Column = 1 then
  begin
    PD := GetNodeData(Node);
    if PD^.Kind = dkProp then
    begin
      GetPropVal(PD, PInfo, S);
      if PInfo^.PropType^.Name = 'TGraphicsColor' then
        AEditLink := FColorLink //TVPGColorLink.Create
      else if PInfo^.PropType^.Kind = tkInteger then
        AEditLink := FIntLink //TVPGIntEditLink.Create
      else if PInfo^.PropType^.Kind = tkClass then
        AEditLink := FButtonLink //TVPGButtonLink.Create
      else if PInfo^.PropType^.Kind = tkAString then
        AEditLink := FStringLink //TVPGStringLink.Create
    end
    else if PD^.Kind = dkPropList then
      AEditLink := FListLink //TVPGComboBoxLink.Create;
  end;
end;

procedure TVirtualPropGrid.TreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  EditNode(Node, 1);
end;

procedure TVirtualPropGrid.TreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PVPGData;
begin
  Data := PVPGData(GetNodeData(Node));
  FreeAndNil(Data^.Values);
  Finalize(Data^.Name);
  Finalize(Data^.PropName);
end;


procedure TVirtualPropGrid.TreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var ANodeDataSize: Integer);
begin
  ANodeDataSize := SizeOf(TVPGData);
end;

procedure TVirtualPropGrid.GetPropVal(PD: PVPGData; out PropInfo: PPropInfo;
  out Value: String);
var
  Obj: TObject;
begin
  GetLowPropInfo(FTIObject, PD^.PropName, Obj, PropInfo);

  case PropInfo^.PropType^.Kind of
    tkInteger, tkEnumeration, tkBool: Value := IntToStr(GetOrdProp(Obj, PropInfo));
    tkSet: Value := GetSetProp(Obj, PropInfo, False);
    tkAString: Value := GetStrProp(Obj, PropInfo);
    else Value := '';
  end;
end;

procedure TVirtualPropGrid.DrawCheckBox(C: TCanvas; R: TRect; Value: Boolean);
var
  Img: TBitmap;
  S: String;
begin
  if Value then Img := FCheckImg
  else Img := FUnCheckImg;
  C.Draw(R.Left + TextMargin, R.Top + (R.Height div 2 - Img.Height div 2), Img);
  if Value then S := rsYes
  else S := rsNo;
  R.Left := R.Left + Img.Width + TextMargin + TextMargin;
  C.TextRect(R, R.Left, R.Top, S, GetDefaultTextStyle(C));
end;

procedure TVirtualPropGrid.DrawStrings(C: TCanvas; R: TRect; SL: TStrings);
begin
  C.TextRect(R, R.Left + TextMargin, R.Top, Format(rsLinesNum, [SL.Count]),
    GetDefaultTextStyle(C));
end;

procedure TVirtualPropGrid.DrawCollection(C: TCanvas; R: TRect; Col: TCollection
  );
begin
  C.TextRect(R, R.Left + TextMargin, R.Top, Format(rsElementsNum, [Col.Count]),
    GetDefaultTextStyle(C));
end;

procedure TVirtualPropGrid.DrawFont(C: TCanvas; R: TRect; F: TFont);
begin
  C.Font.Style := F.Style;
  C.TextRect(R, R.Left + TextMargin, R.Top, F.Name + ', ' + IntToStr(F.Size),
    GetDefaultTextStyle(C));
end;

procedure TVirtualPropGrid.DrawPropColor(C: TCanvas; R: TRect; Clr: TColor);
var
  w, h, x, y: Integer;
begin
  C.Brush.Color := Clr;
  C.Pen.Color := clBlack;
  w := R.Height - Scale96ToScreen(4);
  h := w;
  x := R.Left + TextMargin;
  y := R.Top + Scale96ToScreen(2);
  C.Rectangle(x, y, x + w, y + h);
  R.Left := x + w + TextMargin;
  C.TextRect(R, R.Left, R.Top, FColorBox.GetColorName(Clr), GetDefaultTextStyle(C));
end;

{function GetSetPropText(C: TObject; const PropInfo: PPropInfo; Values: TStringList): String;
var
  SL: TStringList;
  i: Integer;
  S: String;
begin
  Result := '';
  SL := TStringList.Create;
  SL.Delimiter:=',';
  SL.DelimitedText:=GetSetProp(C, PropInfo, False);
  for i := 0 to SL.Count - 1 do
  begin
    S := Values.Values[SL[i]];
    if S = '' then Continue;
    if Result = '' then
      Result := S
    else
      Result := Result + ',' + S;
  end;
  SL.Free;
end;    }

procedure TVirtualPropGrid.DrawPropValue(C: TCanvas; R: TRect; PD: PVPGData);
var
  PInfo: PPropInfo;
  S: String;
  Obj: TObject;
begin
  GetPropVal(PD, PInfo, S);

  if PInfo^.PropType^.Name = 'TGraphicsColor' then
    DrawPropColor(C, R, TColor(StrToInt(S)))
  else
    case PInfo^.PropType^.Kind of
      tkBool: DrawCheckBox(C, R, S = '1');
      tkInteger, tkAString: C.TextRect(R, R.Left + TextMargin, R.Top, S, GetDefaultTextStyle(C));
      tkClass:
        begin
          Obj := GetLowObjProp(FTIObject, PD^.PropName);
          if Obj is TFont then
            DrawFont(C, R, TFont(Obj))
          else if Obj is TCollection then
            DrawCollection(C, R, TCollection(Obj))
          else if Obj is TStrings then
            DrawStrings(C, R, TStrings(Obj))
        end;
    end;
end;

procedure TVirtualPropGrid.DrawPropOption(C: TCanvas; R: TRect; PD,
  ParPD: PVPGData);
var
  S: String;
  Dummy: PPropInfo;
begin
  GetPropVal(ParPD, Dummy, S);
  S := ',' + S + ',';
  DrawCheckBox(C, R, Pos(',' + PD^.PropName + ',', S) > 0);
end;

procedure TVirtualPropGrid.DrawPropList(C: TCanvas; R: TRect; PD: PVPGData);
var
  PInfo: PPropInfo;
  S, K: String;
  i: Integer;
  id: LongInt;
begin
  GetPropVal(PD, PInfo, K);
  S := PD^.Values.Values[K];
  {if PInfo^.PropType^.Kind in [tkInteger, tkEnumeration, tkBool] then
  begin
    id := StrToInt(S);
    i := PD^.Values.IndexOfObject(TObject(id));
    if i >= 0 then
      S := PD^.Values[i]
    else
      S := '';
  end;}
  C.FillRect(R);
  R.Left := R.Left + Margin;
  R.Top := R.Top + Scale96ToScreen(2);
  R.Bottom := R.Bottom - Scale96ToScreen(2);
  DoDrawItem(PD^.PropName, K, S, C, R);
end;

procedure TVirtualPropGrid.GetDefaultItems(const PropName: String;
  Items: TStrings);
var
  PInfo: PPropInfo;
  Obj: TObject;
  i: Integer;
begin
  GetLowPropInfo(FTIObject, PropName, Obj, PInfo);
  case PInfo^.PropType^.Name of
    'TFPBrushStyle':
      with Items do
      begin
        Add(IntToStr(Ord(bsSolid)) + '=' + rsSolid);
        Add(IntToStr(Ord(bsHorizontal)) + '=' + rsHorizontalLines);
        Add(IntToStr(Ord(bsVertical)) + '=' + rsVerticalLines);
        Add(IntToStr(Ord(bsFDiagonal)) + '=' + rsDiagonalToLeft);
        Add(IntToStr(Ord(bsBDiagonal)) + '=' + rsDiagonalToRight);
        Add(IntToStr(Ord(bsCross)) + '=' + rsCross);
        Add(IntToStr(Ord(bsDiagCross)) + '=' + rsDiamond);
        Add(IntToStr(Ord(bsClear)) + '=' + rsNothing);
        {AddObject(rsSolidFill, TObject(bsSolid));
        AddObject(rsHorizontalLines, TObject(bsHorizontal));
        AddObject(rsVerticalLines, TObject(bsVertical));
        AddObject(rsDiagonalToLeft, TObject(bsFDiagonal));
        AddObject(rsDiagonalToRight, TObject(bsBDiagonal));
        AddObject(rsCross, TObject(bsCross));
        AddObject(rsDiamond, TObject(bsDiagCross));
        AddObject(rsClearFill, TObject(bsClear));  }
      end;
    'TFPPenStyle':
      with Items do
      begin
        Add(IntToStr(Ord(psSolid)) + '=' + rsSolid);
        Add(IntToStr(Ord(psDash)) + '=' + rsDash);
        Add(IntToStr(Ord(psDot)) + '=' + rsDot);
        Add(IntToStr(Ord(psDashDot)) + '=' + rsDashDot);
        Add(IntToStr(Ord(psDashDotDot)) + '=' + rsDashDotDot);
        Add(IntToStr(Ord(psClear)) + '=' + rsNothing);
        {AddObject(rsSolidFill, TObject(psSolid));
        AddObject(rsDash, TObject(psDash));
        AddObject(rsDot, TObject(psDot));
        AddObject(rsDashDot, TObject(psDashDot));
        AddObject(rsDashDotDot, TObject(psDashDotDot));
        AddObject(rsClearFill, TObject(psClear));}
      end;
    else
      if (Obj is TFont) and (PInfo^.Name = 'Name') then
      begin
        for i := 0 to Screen.Fonts.Count - 1 do
          Items.Add(Screen.Fonts[i] + '=' + Screen.Fonts[i]);
        //Items.AddStrings(Screen.Fonts);
        //Items.Insert(0, 'default');
      end;
  end;
end;

function TVirtualPropGrid.ToggleCheckNode(N: PVirtualNode): Boolean;
var
  Data, ParData: PVPGData;
  PropInfo: PPropInfo;
  Options, Value: String;
begin
  Result := False;
  Data := GetNodeData(N);
  if not (Data^.Kind in [dkOption, dkProp]) then Exit;
  if Data^.Kind = dkOption then
  begin
    ParData := GetNodeData(N^.Parent);
    GetPropVal(ParData, PropInfo, Options);
    Value := Data^.PropName;
    if HasOption(Options, Value) then
      ExcludeOption(Options, Value)
    else
      IncludeOption(Options, Value);
    SetLowSetProp(FTIObject, ParData^.PropName, Options);
    Result := True;
  end
  else
  begin
    GetPropVal(Data, PropInfo, Value);
    if PropInfo^.PropType^.Kind = tkBool then
    begin
      if Value = '1' then Value := '0'
      else Value := '1';
      SetLowOrdProp(FTIObject, Data^.PropName, StrToInt(Value));
      Result := True;
    end;
  end;
  if Result then InvalidateNodeWithParents(N);
  FModified := True;
end;

procedure TVirtualPropGrid.TreeKeyPress(Sender: TObject; var Key: char);
begin
  if Key in [#13, #32] then
    if not ToggleCheckNode(GetFirstSelected) then
      ToggleNode(GetFirstSelected)
end;

procedure TVirtualPropGrid.TreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Info: THitInfo;
  N: PVirtualNode;
begin
  GetHitTestInfoAt(X, Y, True, Info);
  if Info.HitColumn = 1 then
  begin
    N := Info.HitNode;
    X := X - Header.Columns[0].Width;
    if (X >= 0) and (X <= FCheckImg.Width + TextMargin + TextMargin) then ToggleCheckNode(N);
  end
  else if hiNowhere in Info.HitPositions then
  begin
    EditColumn := 1;
    DoEdit;
  end;
end;

function TVirtualPropGrid.GetNodePath(N: PVirtualNode): String;
var
  Data: PVPGData;
begin
  Result := '';
  while (N <> nil) and (N <> RootNode) do
  begin
    Data := GetNodeData(N);
    if Result = '' then
      Result := Data^.Name
    else
      Result := Data^.Name + '\' + Result;
    N := N^.Parent;
  end;
end;

procedure TVirtualPropGrid.WMVScroll(var Message: TLMVScroll);
begin
  EndEditNode;
  inherited;
end;

procedure TVirtualPropGrid.WMMouseWheel(var Message: TLMMouseEvent);
begin
  EndEditNode;
  inherited;
end;

procedure TVirtualPropGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  N, FN: PVirtualNode;
  IsButton: Boolean;
  Msg: TLMessage;
begin
  FN := FocusedNode;
  if FN = nil then
  begin
    inherited KeyDown(Key, Shift);
    Exit;
  end;

  if IsEditing then
  begin
    IsButton := EditLink is TVPGButtonLink;

    if Key = VK_RETURN then
    begin
      if IsButton and (ssShift in Shift) then
      begin
        Msg.msg := LM_BUTTONLINK_CLICK;
        FButtonLink.ProcessMessage(Msg);
      end;
    end
    else if Key in [VK_UP, VK_DOWN] then
    begin
      EndEditNode;
      SetFocus;

      if not IsButton then
      begin
        if Key = VK_DOWN then
          N := GetNextVisible(FN)
        else
          N := GetPreviousVisible(FN);
        if N = nil then N := FN;
        ClearSelection;
        FocusedNode := nil;
        Selected[N] := True;
        FocusedNode := N;
      end;
    end;

  end;

  if not IsEditing or (EditLink is TVPGButtonLink) then
  begin

    if Key = VK_LEFT then
    begin
      if HasChildren[FN] and Expanded[FN] then
        ToggleNode(FN);
    end
    else if Key = VK_RIGHT then
    begin
      if HasChildren[FN] and not Expanded[FN] then
        ToggleNode(FN);
    end;

  end;

  inherited KeyDown(Key, Shift);
end;

procedure TVirtualPropGrid.DoExit;
begin
  inherited DoExit;
  EndEditNode;
end;

procedure TVirtualPropGrid.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
begin
  inherited DoCanEdit(Node, Column, Allowed);
  if (Column = 1) and Allowed and (FOnPropEditing <> nil) then
    FOnPropEditing(Self, PVPGData(GetNodeData(Node))^.PropName, Allowed);
end;

constructor TVirtualPropGrid.Create(TheOwner: TComponent);
var
  El: TThemedElementDetails;
  Sz: TSize;
begin
  inherited Create(TheOwner);
  with Header.Columns.Add do
  begin
    Text := rsParameter;
    Width := 150;
    Options := Options - [coEditable, coDraggable];
  end;
  with Header.Columns.Add do
  begin
    Text := rsValue;
    Width := 150;
    Options := Options - [coDraggable];
  end;
  with Header do
    Options := Options + [hoVisible, hoAutoResize];
  Header.Height := Scale96ToScreen(24);
  with TreeOptions do
  begin
    SelectionOptions := SelectionOptions + [toExtendedFocus, toFullRowSelect];
    PaintOptions := PaintOptions - [toShowTreeLines] + [toShowVertGridLines,
      toShowHorzGridLines, toHideFocusRect];
    MiscOptions := MiscOptions + [toEditable, toGridExtensions];
  end;
  with Colors do
  begin
    UnfocusedSelectionColor := FocusedSelectionColor;
    UnfocusedSelectionBorderColor := FocusedSelectionBorderColor;
    GridLineColor := clGray;
  end;
  EditDelay := 300;

  OnAfterItemPaint := @TreeAfterItemPaint;
  OnAfterCellPaint := @TreeAfterCellPaint;
  OnGetNodeDataSize := @TreeGetNodeDataSize;
  OnCreateEditor := @TreeCreateEditor;
  OnFocusChanged := @TreeFocusChanged;
  OnMouseUp:=@TreeMouseUp;
  OnKeyPress := @TreeKeyPress;
  OnFreeNode:=@TreeFreeNode;
  //OnCompareNodes:=@TreeCompareNodes;

  FColorBox := TMyColorBox.Create(Self);

  El := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
  Sz := ThemeServices.GetDetailSize(El);
  FCheckImg := TBitmap.Create;
  FCheckImg.SetSize(Sz.Width, Sz.Height);
  ThemeServices.DrawElement(FCheckImg.Canvas.Handle, El,
    Rect(0, 0, Sz.Width, Sz.Height));

  El := ThemeServices.GetElementDetails(tbCheckBoxUnCheckedNormal);
  Sz := ThemeServices.GetDetailSize(El);
  FUnCheckImg := TBitmap.Create;
  FUnCheckImg.SetSize(Sz.Width, Sz.Height);
  ThemeServices.DrawElement(FUnCheckImg.Canvas.Handle, El,
    Rect(0, 0, Sz.Width, Sz.Height));

  FNodePathes := TStringList.Create;

  FColorLink := TVPGColorLink.Create;
  FIntLink := TVPGIntEditLink.Create;
  FButtonLink := TVPGButtonLink.Create;
  FStringLink := TVPGStringLink.Create;
  FListLink := TVPGComboBoxLink.Create;
end;

destructor TVirtualPropGrid.Destroy;
begin
  FListLink := nil;
  FStringLink := nil;
  FButtonLink := nil;
  FIntLink := nil;
  FColorLink := nil;
  FNodePathes.Free;
  FCheckImg.Free;
  FUnCheckImg.Free;
  inherited Destroy;
end;

procedure TVirtualPropGrid.DoNeedValues(const PropName: String; Items: TStrings
  );
begin
  if FOnNeedValues <> nil then
    FOnNeedValues(Self, PropName, Items);
end;

procedure TVirtualPropGrid.DoDrawItem(const PropName, PropValue, AText: String;
  C: TCanvas; R: TRect);
var
  PInfo: PPropInfo;
  Obj: TObject;
  TS: TTextStyle;
begin
  GetLowPropInfo(FTIObject, PropName, Obj, PInfo);
  TS := GetDefaultTextStyle(C);
  case PInfo^.PropType^.Name of
    'TFPBrushStyle':
      begin
        C.Brush.Style := TBrushStyle(StrToInt(PropValue));
        if C.Brush.Style = bsClear then
          C.Brush.Color := clNone
        else
          C.Brush.Color := clBlack;
        C.Pen.Color := clBlack;
        C.Rectangle(R.Left, R.Top, R.Left + R.Height, R.Bottom);
        R.Left := R.Left + R.Height + 4;
        C.TextRect(R, R.Left, R.Top, AText, TS);
      end;
    'TFPPenStyle':
      begin
        C.Pen.Color := clBlack;
        C.Pen.Style := psSolid;
        C.Brush.Color := clWhite;
        C.Brush.Style := bsSolid;
        C.Rectangle(R.Left, R.Top, R.Left + R.Height * 2, R.Bottom);
        C.Pen.Style := TPenStyle(StrToInt(PropValue));
        C.Line(R.Left, R.Top + R.Height div 2, R.Left + R.Height * 2,
          R.Top + R.Height div 2);
        R.Left := R.Left + R.Height * 2 + 4;
        C.TextRect(R, R.Left, R.Top, AText, TS);
      end;
    else
      C.TextRect(R, R.Left, R.Top, AText, TS);
  end;
end;

procedure TVirtualPropGrid.DoEditObject(const PropName: String; Obj: TObject;
  var Accept: Boolean);
begin
  if FOnEditObject <> nil then
    FOnEditObject(Self, PropName, Obj, Accept);
end;

function TVirtualPropGrid.DoPropValidate(const PropName, PropValue: String
  ): Boolean;
begin
  Result := True;
  if FOnPropValidate <> nil then
    OnPropValidate(Self, PropName, PropValue, Result);
end;

procedure TVirtualPropGrid.DoPropChange(Node: PVirtualNode;
  const PropName: String);
begin
  InvalidateNodeWithParents(Node);
  InvalidateNodeWithChildren(Node);
  if FOnPropChange <> nil then
    OnPropChange(Self, PropName);
  FModified := True;
end;

procedure TVirtualPropGrid.InvalidateNodeWithParents(N: PVirtualNode);
begin
  while (N <> nil) and (N <> RootNode) do
  begin
    InvalidateNode(N);
    N := N^.Parent;
  end;
//  InvalidateNode(N);
end;

procedure TVirtualPropGrid.InvalidateNodeWithChildren(N: PVirtualNode);
begin
  InvalidateNode(N);
  N := N^.FirstChild;
  while N <> nil do
  begin
    InvalidateNodeWithChildren(N);
    N := N^.NextSibling;
  end;
end;

function TVirtualPropGrid.AddNode(const AName, PropName: String;
  Kind: TVPGDataKind; Values: array of String; CreateValues: Boolean; AParent,
  Source: PVirtualNode): PVirtualNode;
var
  PD: PVPGData;
begin
  Result := AddChild(AParent);
  PD := GetNodeData(Result);
  PD^.Name := AName;
  PD^.PropName := PropName;
  PD^.Kind := Kind;
  if CreateValues then
  begin
    PD^.Values := TStringList.Create;
    PD^.Values.AddStrings(Values);
  end
  else
    PD^.Values := nil;
  PD^.Source := Source;
  Result^.NodeHeight := Scale96ToScreen(24);
  if CreateValues and (Length(Values) = 0) then
  begin
    GetDefaultItems(PropName, PD^.Values);
    DoNeedValues(PropName, PD^.Values);
  end;
  DefaultNodeHeight := Scale96ToScreen(25);
end;

function TVirtualPropGrid.AddGroup(const AName: String; AParent: PVirtualNode
  ): PVirtualNode;
begin
  Result := AddNode(AName, '', dkGroup, [], False, AParent, nil);
end;

function TVirtualPropGrid.AddOption(const AName, PropName: String;
  AParent: PVirtualNode): PVirtualNode;
begin
  Result := AddNode(AName, PropName, dkOption, [], False, AParent, nil);
end;

function TVirtualPropGrid.AddProp(const AName, PropName: String; AParent: PVirtualNode
  ): PVirtualNode;
begin
  Result := AddNode(AName, PropName, dkProp, [], False, AParent, nil);
end;

function TVirtualPropGrid.AddPropList(const AName, PropName: String; AParent,
  ASource: PVirtualNode): PVirtualNode;
begin
  Result := AddNode(AName, PropName, dkPropList, [], True, AParent, ASource);
end;

procedure TVirtualPropGrid.SaveExpandState;
var
  N: PVirtualNode;
begin
  FNodePathes.Clear;
  N := GetFirstSelected;
  FSelectedPath := GetNodePath(GetFirstSelected);
  if N <> nil then
    FPrevSelectedPath := GetNodePath(N^.PrevSibling)
  else
    FPrevSelectedPath := '';
  N := GetFirst;
  while N <> nil do
  begin
    if Expanded[N] then
      FNodePathes.Add(GetNodePath(N));
    N := GetNext(N);
  end;
end;

procedure TVirtualPropGrid.RestoreExpandState;
var
  N, PrevN: PVirtualNode;
  i: Integer;
  NodePath: String;
begin
  N := GetFirst;
  while N <> nil do
  begin
    //if N^.ChildCount > 0 then
    begin
      NodePath := GetNodePath(N);
      i := FNodePathes.IndexOf(NodePath);
      if i >= 0 then Expanded[N] := True;
      if NodePath = FSelectedPath then
        Selected[N] := True
      else if NodePath = FPrevSelectedPath then
        PrevN := N;
    end;
    N := GetNext(N);
  end;
  if (SelectedCount = 0) and (PrevN <> nil) then
    Selected[PrevN] := True;
end;

end.

