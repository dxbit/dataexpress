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
unit DBCtrlsEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLStrConsts, LCLType, LMessages,
  Graphics, Controls, Forms, FileUtil, Dialogs, Buttons, Calendar,
  CalendarPopup, DBCtrls, db, StdCtrls, CalcForm, {ExtDlgs,}
  ExtCtrls, LResources;

const
  NullDate: TDateTime = 0;

type

  { TCustomDBEditButton }

  TCustomDBEditButton = class(TDBEdit)
  private
    FButton: TSpeedButton;
    FButtonNeedsFocus: Boolean;
    FDirectInput: Boolean;
    FHideButton: Boolean;
    FIsReadOnly: boolean;
    FOnButtonClick : TNotifyEvent;
    function GetButtonHint: TTranslateString;
    function GetButtonWidth: Integer;
    function GetDirectInput: Boolean;
    function GetFlat: Boolean;
    procedure CheckButtonVisible;
    procedure SetButtonHint(const AValue: TTranslateString);
    procedure SetButtonNeedsFocus(const AValue: Boolean);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetDirectInput(const AValue: Boolean);
    procedure SetFlat(const AValue: Boolean);
    procedure SetGlyph(Pic: TBitmap);
    function GetGlyph : TBitmap;
    procedure SetHideButton(AValue: Boolean);
    procedure SetNumGlyphs(ANumber: Integer);
    function GetNumGlyphs:Integer;
    function GetMinHeight: Integer;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  protected
    function GetReadOnly: Boolean; override;
    function GetDefaultGlyph: TBitmap; virtual;
    function GetDefaultGlyphName: String; virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetReadOnly(AValue: Boolean); override;
    procedure DoPositionButton; virtual;
    procedure DoButtonClick (Sender: TObject); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    // New properties.

    property DirectInput : Boolean read GetDirectInput write SetDirectInput default True;
    property Glyph : TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs : Integer read GetNumGlyphs write SetNumGlyphs;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TSpeedButton read FButton;
    property ButtonWidth : Integer read GetButtonWidth write SetButtonWidth;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property ButtonOnlyWhenFocused: Boolean read FButtonNeedsFocus write SetButtonNeedsFocus default False;
    property HideButton: Boolean read FHideButton write SetHideButton;
  end;


  { TDBEditButton }

  TDBEditButton = class(TCustomDBEditButton)
  Public
    property Button;
  published
    property AutoSize;
    property AutoSelect;
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property ButtonHint;
    property CharCase;
    property Color;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property MaxLength;
    property NumGlyphs;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
  end;

  { TDateEdit }

  TAcceptDateEvent = procedure (Sender : TObject; var ADate : TDateTime;
    var AcceptDate: Boolean) of object;
  TCustomDateEvent = procedure (Sender : TObject; var ADate : string) of object;
  TDateOrder = (doNone,doMDY,doDMY,doYMd);

  { TDBDateEditEx }

  TDBDateEditEx = class(TCustomDBEditButton)
  private
    FDateOrder: TDateOrder;
    FDefaultToday: Boolean;
    FDialogTitle: TCaption;
    FDisplaySettings: TDisplaySettings;
    FDroppedDown: Boolean;
    FOnAcceptDate: TAcceptDateEvent;
    FOnCustomDate: TCustomDateEvent;
    FOKCaption: TCaption;
    FCancelCaption: TCaption;
    FDateFormat: string;
    function GetDate: TDateTime;
    function IsStoreTitle: boolean;
    procedure SetDate(Value: TDateTime);
    procedure CalendarPopupReturnDate(Sender: TObject; const ADate: TDateTime);
    procedure CalendarPopupShowHide(Sender: TObject);
    procedure SetDateOrder(const AValue: TDateOrder);
  protected
    function GetDefaultGlyph: TBitmap; override;
    function GetDefaultGlyphName: String; override;
    procedure DoButtonClick(Sender: TObject); override;
    procedure DblClick; override;
    procedure SetDateMask; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DateFormatChanged; virtual;
    function GetDateFormat: string;
    procedure EditingDone; override;
    property Date: TDateTime read GetDate write SetDate;
    property Button;
    property DroppedDown: Boolean read FDroppedDown;
  published
    property DialogTitle: TCaption read FDialogTitle write FDialogTitle stored IsStoreTitle;
    property CalendarDisplaySettings: TDisplaySettings read FDisplaySettings write FDisplaySettings;
    property OnAcceptDate: TAcceptDateEvent read FOnAcceptDAte write FOnAcceptDate;
    property OnCustomDate: TCustomDateEvent read FOnCustomDate write FOnCustomDate;
    property OKCaption: TCaption read FOKCaption write FOKCaption;
    property CancelCaption: TCaption read FCancelCaption write FCancelCaption;
    property ReadOnly;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday default False;
    Property DateOrder : TDateOrder Read FDateOrder Write SetDateOrder;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property CharCase;
    property DirectInput;
    property Flat;
    property Glyph stored False;
    property NumGlyphs;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
  end;

  { TDBCalcEdit }

  TAcceptValueEvent = procedure(Sender: TObject; var AValue: Double; var Action: Boolean) of object;
  TDBCalcEdit = class(TCustomDBEditButton)
  private
    FDialogTitle: String;
    FLayout: TCalculatorLayout;
    FOnAcceptValue: TAcceptValueEvent;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    procedure SetAsFloat(const AValue: Double);
    procedure SetAsInteger(const AValue: Integer);
    function TitleStored: boolean;
  protected
    FCalcDialog : TForm;
    function GetDefaultGlyph: TBitmap; override;
    function GetDefaultGlyphName: String; override;
    procedure DoButtonClick (Sender: TObject); override;
    procedure RunDialog; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    // CalcEdit properties
    property CalculatorLayout : TCalculatorLayout read FLayout write Flayout;
    property AsFloat : Double read GetAsFloat write SetAsFloat;
    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property OnAcceptValue : TAcceptValueEvent read FOnAcceptValue write FOnAcceptValue;
    property DialogTitle : String read FDialogTitle write FDialogTitle stored TitleStored;
    // TDBEditButton properties.
    property ButtonWidth;
    property DirectInput;
    property ButtonOnlyWhenFocused;
    // property Glyph;
    property NumGlyphs;
    property Flat;
    // Other properties
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property AutoSize;
    property AutoSelect;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

  { TComboBoxDS }

  TComboBoxDS = class(TComboBox)
  private
    FKeyField: String;
    FListField: String;
    function GetKeyValue: Integer;
    procedure SetKeyValue(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear; override;
    procedure LoadList(aDataSet: TDataSet; CloseDataSet: Boolean = True);
    property KeyValue: Integer read GetKeyValue write SetKeyValue;
  published
    property KeyField: String read FKeyField write FKeyField;
    property ListField: String read FListField write FListField;
  end;


const
  sResDBDateEdit  = 'DBDateEdit';
  sResDBCalcEdit    = 'DBCalcEdit';

implementation

{ TComboBoxDS }

function TComboBoxDS.GetKeyValue: Integer;
begin
  Result := 0;
  if ItemIndex >= 0 then
    Result := PInteger(Items.Objects[ItemIndex])^;
end;

procedure TComboBoxDS.SetKeyValue(AValue: Integer);
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    if PInteger(Items.Objects[i])^ = AValue then
    begin
      ItemIndex := i;
      Exit;
    end;
  ItemIndex := -1;
end;

constructor TComboBoxDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeyField := 'id';
  FListField := 'name';
end;


procedure TComboBoxDS.Clear;
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    if Items.Objects[i] <> nil then
      Dispose(PInteger(Items.Objects[i]));
  inherited Clear;
end;

procedure TComboBoxDS.LoadList(aDataSet: TDataSet; CloseDataSet: Boolean);
var
  TmpActive: Boolean;
  PKey: PInteger;
begin
  Clear;
  TmpActive := aDataSet.Active;
  aDataSet.Active := True;
  aDataSet.First;
  while aDataSet.Eof = False do
  begin
    New(PKey);
    PKey^ := aDataSet.FieldByName(FKeyField).AsInteger;
    Items.AddObject(aDataSet.FieldByName(FListField).AsString, TObject(PKey));
    aDataSet.Next;
  end;
  if CloseDataSet then
    aDataSet.Active := False
  else
    aDataSet.Active := TmpActive;
end;

{ TCustomEditBtn }

constructor TCustomDBEditButton.Create(AOwner: TComponent);
var
  B: TBitmap;
begin
  inherited Create(AOwner);
  FDirectInput := True;
  FButton := TSpeedButton.Create(Self);
  FButton.Width := Self.Height;
  FButton.Height := Self.Height;
  FButton.FreeNotification(Self);
  CheckButtonVisible;
  FButton.OnClick := @DoButtonClick;
  FButton.Cursor := crArrow;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable];
  B := GetDefaultGlyph;
  if B = nil
  then FButton.LoadGlyphFromLazarusResource(GetDefaultGlyphName)
  else FButton.Glyph := B;
  ControlStyle := ControlStyle - [csSetCaption];
  Flat := True;
end;

destructor TCustomDBEditButton.Destroy;
begin
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TCustomDBEditButton.SetGlyph(Pic: TBitmap);
Begin
  FButton.Glyph:=Pic;
end;

function TCustomDBEditButton.GetButtonWidth: Integer;
begin
  Result:=FButton.Width;
end;

function TCustomDBEditButton.GetDefaultGlyph: TBitmap;
begin
  Result := nil;
end;

function TCustomDBEditButton.GetDefaultGlyphName: String;
begin
  Result := '';
end;

function TCustomDBEditButton.GetButtonHint: TTranslateString;
begin
  Result:=FButton.Hint;
end;

function TCustomDBEditButton.GetDirectInput: Boolean;
begin
  Result := FDirectInput;
end;

function TCustomDBEditButton.GetFlat: Boolean;
begin
  if Assigned(FButton) then
    Result := FButton.Flat
  else
    Result := False;
end;

procedure TCustomDBEditButton.CheckButtonVisible;
begin
  If Assigned(FButton) then
    FButton.Visible:=(csdesigning in ComponentState) or
                     (Visible and (Focused or not FButtonNeedsFocus) and
                     (not FHideButton));
end;

procedure TCustomDBEditButton.SetButtonHint(const AValue: TTranslateString);
begin
  FButton.Hint:=AValue;
end;

procedure TCustomDBEditButton.SetButtonNeedsFocus(const AValue: Boolean);
begin
  if FButtonNeedsFocus<>AValue then
  begin
    FButtonNeedsFocus:=AValue;
    CheckButtonVisible;
  end;
end;

procedure TCustomDBEditButton.SetButtonWidth(const AValue: Integer);
begin
  FButton.Width:=AValue;
end;

procedure TCustomDBEditButton.SetDirectInput(const AValue: Boolean);
begin
  FDirectInput := AValue;
  inherited SetReadOnly((not FDirectInput) or (FIsReadOnly))
end;

procedure TCustomDBEditButton.SetFlat(const AValue: Boolean);
begin
  if Assigned(FButton) then
    FButton.Flat:=AValue;
end;

function TCustomDBEditButton.GetGlyph : TBitmap;
begin
  Result:=FButton.Glyph;
end;

procedure TCustomDBEditButton.SetHideButton(AValue: Boolean);
begin
  if FHideButton=AValue then Exit;
  FHideButton:=AValue;
  CheckButtonVisible;
end;

procedure TCustomDBEditButton.SetNumGlyphs(ANumber: Integer);
begin
  FButton.NumGlyphs:=ANumber;
end;

function TCustomDBEditButton.GetNumGlyphs:Integer;
begin
  Result:=FButton.NumGlyphs;
end;

procedure TCustomDBEditButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
end;

procedure TCustomDBEditButton.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisibleChanged(Msg);

  CheckButtonVisible;
end;

procedure TCustomDBEditButton.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited CMEnabledChanged(Msg);

  if (FButton<>nil) and (not ReadOnly) then
    FButton.Enabled:=Enabled;
end;

function TCustomDBEditButton.GetMinHeight: Integer;
begin
  Result:=23;
end;

procedure TCustomDBEditButton.DoButtonClick (Sender: TObject);
begin
  if not ReadOnly then
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self);
end;

procedure TCustomDBEditButton.Loaded;
begin
  inherited Loaded;
  CheckButtonVisible;
  DoPositionButton;
end;

procedure TCustomDBEditButton.WMKillFocus(var Message: TLMKillFocus);
begin
  if FButtonNeedsFocus then
    FButton.Visible:=False;
  inherited;
end;

function TCustomDBEditButton.GetReadOnly: Boolean;
begin
  Result := FIsReadOnly;
end;

procedure TCustomDBEditButton.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FButton <> nil then
  begin
    DoPositionButton;
    CheckButtonVisible;
  end;
end;

procedure TCustomDBEditButton.SetReadOnly(AValue: Boolean);
begin
  FIsReadOnly := AValue;
  if Assigned(FButton) then
    FButton.Enabled := not FIsReadOnly and Enabled;
  inherited SetReadOnly(FIsReadOnly or (not DirectInput));
end;

procedure TCustomDBEditButton.DoPositionButton;
begin
  if FButton = nil then exit;
  FButton.Parent := Parent;
  FButton.Visible := Visible;
  FButton.AnchorToCompanion(akLeft,0,Self);
end;

procedure TCustomDBEditButton.WMSetFocus(var Message: TLMSetFocus);
begin
  //FButton.Visible:=True;
  CheckButtonVisible;
  inherited;
end;

{ TDBDateEditEx }

function StrToDateDef(cDate: String; dDefault: TDateTime): TDateTime;
begin
  try
    Result := StrToDateTime(cDate)
  except
    Result := dDefault;
  end;
end;

constructor TDBDateEditEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultToday := False;
  FDisplaySettings := [dsShowHeadings, dsShowDayNames];
  DialogTitle := rsPickDate;
  OKCaption := 'OK';
  CancelCaption := 'Cancel';
  DateFormatChanged;
end;

procedure TDBDateEditEx.DateFormatChanged;
begin
  FDateFormat := DefaultFormatSettings.ShortDateFormat;
end;

function TDBDateEditEx.GetDateFormat: string;
begin
  Result := FDateFormat;
end;

procedure TDBDateEditEx.EditingDone;
var
  D: TDateTime;
begin
  inherited EditingDone;
  {if TryStrToDate(StringReplace(Text, ' ', DefaultFormatSettings.DateSeparator,
    [rfReplaceAll]), D) = False then
    Text := ''
  else
    Text := DateTimeToStr(D); }
end;

function TDBDateEditEx.GetDefaultGlyph: TBitmap;
begin
  Result := nil;
end;

function TDBDateEditEx.GetDefaultGlyphName: String;
begin
  Result := sResDBDateEdit;
end;

procedure TDBDateEditEx.DoButtonClick(Sender: TObject);//or onClick
var
  PopupOrigin: TPoint;
  ADate: TDateTime;
begin
  inherited DoButtonClick(Sender);

  PopupOrigin := ControlToScreen(Point(0, Height));
  ADate := GetDate;
  if ADate = NullDate then
    ADate := SysUtils.Date;
  ShowCalendarPopup(PopupOrigin, ADate, CalendarDisplaySettings,
                    @CalendarPopupReturnDate, @CalendarPopupShowHide)
end;

procedure TDBDateEditEx.DblClick;
begin
  inherited DblClick;
  if (not ReadOnly) and (Button.Enabled) then
    DoButtonClick(nil);
end;

procedure TDBDateEditEx.SetDateMask;

Var
  S : String;
  D : TDateTime;
begin
  Case DateOrder of
    doNone :
       begin
       S:=''; // no mask
       FDateFormat:='';
       end;
    doDMY,
    doMDY  :
      begin
      S:='99/99/9999;1;_';
      if DateOrder=doMDY then
        FDateFormat:='mm/dd/yyyy'
      else
        FDateFormat:='dd/mm/yyyy';
      end;
    doYMD  :
      begin
      S:='9999/99/99;1;_';
      FDateFormat:='yyyy/mm/dd';
      end;
  end;
  D:=GetDate;
  EditMask:=S;
  SetDate(D);
end;

Function ParseDate(S : String; Order : TDateOrder; Def: TDateTime) : TDateTime;

Var
  P,N1,N2,N3 : Integer;
  B : Boolean;

begin
  Result:=Def;
  P:=Pos(DefaultFormatSettings.DateSeparator,S);
  If (P=0) then
    Exit;
  N1:=StrToIntDef(Copy(S,1,P-1),-1);
  If (N1=-1) then Exit;
  Delete(S,1,P);
  P:=Pos(DefaultFormatSettings.DateSeparator,S);
  If (P=0) then
    Exit;
  N2:=StrToIntDef(Copy(S,1,P-1),-1);
  If (N1=0) then Exit;
  Delete(S,1,P);
  N3:=StrToIntDef(S,-1);
  If (N3=-1) then
    exit;
  Case Order of
    doYMD : B:=TryEncodeDate(N1,N2,N3,Result);
    doMDY : B:=TryEncodeDate(N3,N1,N2,Result);
    doDMY : B:=TryEncodeDate(N3,N2,N1,Result);
  end;
  If not B then // Not sure if TryEncodeDate touches Result.
    Result:=Def;
end;

function TDBDateEditEx.GetDate: TDateTime;
var
  ADate: string;
begin
  if FDefaultToday then
    Result := SysUtils.Date
  else
    Result := NullDate;
  ADate := Trim(Text);
  if ADate <> '' then
  begin
    if Assigned(FOnCustomDate) then
      FOnCustomDate(Self, ADate);
    if (DateOrder = doNone) then
      Result := StrToDateDef(ADate, Result)
    else
      Result := ParseDate(ADate,DateOrder,Result)
  end;
end;

function TDBDateEditEx.IsStoreTitle: boolean;
begin
  Result := DialogTitle <> rsPickDate;
end;

procedure TDBDateEditEx.SetDate(Value: TDateTime);
begin
  if {not IsValidDate(Value) or }(Value = NullDate) then
  begin
    if DefaultToday then
      Value := SysUtils.Date
    else
      Value := NullDate;
  end;
  if Value = NullDate then
    Text := ''
  else
  begin
    if (FDateFormat = '') then
      Text := DateToStr(Value)
    else
      Text := FormatDateTime(FDateFormat, Value)
  end;

  // *7bit* 18.01.12
  if Field <> nil then
    if Field.DataSet <> nil then
      if Field.DataSet.CanModify then
      begin
        if not (Field.DataSet.State in [dsEdit, dsInsert]) then
          Field.DataSet.Edit;
        if Field.DataSet.State in [dsEdit, dsInsert] then
        	if Field.Value <> Value then
	          Field.Value := Value;
      end;
  //
end;

procedure TDBDateEditEx.CalendarPopupReturnDate(Sender: TObject;
  const ADate: TDateTime);
var
  B: Boolean;
  D: TDateTime;
begin
  try
    B := True;
    D := ADate;
    if Assigned(FOnAcceptDate) then
      FOnAcceptDate(Self, D, B);
    if B then
      Self.Date := D;
  except
    on E:Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TDBDateEditEx.CalendarPopupShowHide(Sender: TObject);
begin
  FDroppedDown := (Sender as TForm).Visible;
end;

procedure TDBDateEditEx.SetDateOrder(const AValue: TDateOrder);
begin
  if FDateOrder=AValue then exit;
  FDateOrder:=AValue;
  SetDateMask;
end;

{ TDBCalcEdit }

function TDBCalcEdit.GetAsFloat: Double;
begin
  Result := StrToFloatDef(Trim(Text), 0.0);
end;

function TDBCalcEdit.GetAsInteger: Integer;
begin
  Result:=StrToIntDef(Text,0);
end;

function TDBCalcEdit.GetDefaultGlyph: TBitmap;
begin
  Result := nil;
end;

function TDBCalcEdit.GetDefaultGlyphName: String;
begin
  Result := sResDBCalcEdit;
end;

procedure TDBCalcEdit.SetAsFloat(const AValue: Double);
begin
  // *7bit* 18.01.12
  if Field <> nil then
    if Field.DataSet <> nil then
      if Field.DataSet.CanModify then
        if Field.DataSet.State in [dsEdit, dsInsert] then
          Field.Value := AValue;
  //

  Text:=FloatToStr(AValue);
end;

procedure TDBCalcEdit.SetAsInteger(const AValue: Integer);
begin
  // *7bit* 18.01.12
  if Field <> nil then
    if Field.DataSet <> nil then
      if Field.DataSet.CanModify then
        if Field.DataSet.State in [dsEdit, dsInsert] then
          Field.Value := AValue;
  //

  Text:=IntToStr(AValue);
end;

function TDBCalcEdit.TitleStored: boolean;
begin
  Result:=FDialogTitle<>rsCalculator;
end;

procedure TDBCalcEdit.DoButtonClick(Sender: TObject);
begin
  inherited DoButtonClick(Sender);
  RunDialog;
end;

procedure TDBCalcEdit.RunDialog;
var
  D : Double;
  B : Boolean;
begin
  D:=AsFloat;
  with CreateCalculatorForm(Self,FLayout,0) do
    try
      Caption:=DialogTitle;
      Value:=D;
      if (ShowModal=mrOK) then
      begin
        D:=Value;
        B:=True;
        If Assigned(FOnAcceptValue) then
          FOnAcceptValue(Self,D,B);
        if B then
          AsFloat:=D;
      end;
    finally
      Free;
    end;
end;

constructor TDBCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FdialogTitle:=rsCalculator;
end;

initialization

{$i dbctrlsex.lrs}

end.


