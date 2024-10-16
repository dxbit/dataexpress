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

unit ActionControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, StdCtrls, Grids, Menus, LclType, EditBtn,
  Graphics, ExtCtrls, CheckLst, dxctrls, strconsts, dxreports, myctrls,
  LMessages, Dialogs, Buttons, FPCanvas, Clipbrd;

type

  { TActionText }

  TActionText = class(TEdit)
  private
    FCol, FRow: Integer;
    FGrid: TCustomGrid;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  protected
    procedure Change; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SelectAll(var Msg: TGridMessage); message GM_SELECTALL;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TActionNumber }

  TActionNumber = class(TActionText)
  private
    FOldNumber: String;
  protected
    procedure KeyPress(var Key: char); override;
  	procedure DoEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditingDone; override;
  end;

  TActionCheckBox = class(TCheckBox);

  { TActionFile }

  TActionFile = class(TFileNameEdit)
  private
    FCol, FRow: Integer;
    FGrid: TCustomGrid;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  protected
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer; KeepBase: Boolean);
      override;
    function RealGetText: TCaption; override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure EditChange; override;
    procedure EditKeyDown(var Key : Word; Shift : TShiftState); override;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SelectAll(var Msg: TGridMessage); message GM_SELECTALL;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TActionFolder }

  TActionFolder = class(TDirectoryEdit)
  private
    FCol, FRow: Integer;
    FGrid: TCustomGrid;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  protected
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer; KeepBase: Boolean);
      override;
    function RealGetText: TCaption; override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure EditChange; override;
    procedure EditKeyDown(var Key : Word; Shift : TShiftState); override;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SelectAll(var Msg: TGridMessage); message GM_SELECTALL;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TActionCbx = class;

  { TExprCellEditor }

  TExprCellEditor = class(TEditButton)
  private
    FCol, FRow: Integer;
    FGrid: TCustomGrid;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  protected
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer; KeepBase: Boolean);
      override;
    procedure EditChange; override;
    procedure EditKeyDown(var Key : Word; Shift : TShiftState); override;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SelectAll(var Msg: TGridMessage); message GM_SELECTALL;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TActionExpr }

  TActionExpr = class(TExprCellEditor)
  private
    FCurForm: TdxForm;
    FNoForm: Boolean;
    FSourceCbx: TActionCbx;
  protected
    function RealGetText: TCaption; override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure ButtonClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetSourceForm: TdxForm;
    property SourceCbx: TActionCbx read FSourceCbx write FSourceCbx;
    property CurForm: TdxForm read FCurForm write FCurForm;
    property NoForm: Boolean read FNoForm write FNoForm;
  end;

  { TActionFilterExpr }

  TActionFilterExpr = class(TExprCellEditor)
  private
    FCurForm: TdxForm;
    FFormCbx: TActionCbx;
    FNoForm: Boolean;
    FSourceCbx: TActionCbx;
  protected
    function RealGetText: TCaption; override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure ButtonClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    property SourceCbx: TActionCbx read FSourceCbx write FSourceCbx;
    property FormCbx: TActionCbx read FFormCbx write FFormCbx;
    property CurForm: TdxForm read FCurForm write FCurForm;
    property NoForm: Boolean read FNoForm write FNoForm;
  end;

  { TActionCbx }

  { TGridComboBox }

  TGridComboBox = class(TComboBox)
  private
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
    FOldObject: TObject;
    procedure SaveOldObject;
    procedure RestoreOldObject;
  protected
    procedure DoEnter; override;
    procedure WndProc(var TheMessage : TLMessage); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure Select; override;
    procedure Change; override;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  TActionCbx = class(TGridComboBox)
  private
    FCurForm: TdxForm;
    FFilter: String;
    FSourceCbx: TActionCbx;
  public
    procedure Fill; virtual;
    property CurForm: TdxForm read FCurForm write FCurForm;
    property SourceCbx: TActionCbx read FSourceCbx write FSourceCbx;
    property Filter: String read FFilter write FFilter;
  end;

  { TSortedActionCbx }

  TSortedActionCbx = class(TActionCbx)
  public
    constructor Create(TheOwner: TComponent); override;
  end;

	{ TFormCbx }

  TFormCbx = class(TSortedActionCbx)
  private
    procedure FillForms;
    procedure FillQueryForms;
  public
    procedure Fill; override;
  end;

  { TChildFormCbx }

  TChildFormCbx = class(TSortedActionCbx)
  public
    procedure Fill; override;
  end;

  { TQueryCbx }

  TQueryCbx = class(TSortedActionCbx)
  public
    procedure Fill; override;
  end;

  { TObjectCbx }

  TObjectCbx = class(TSortedActionCbx)
  public
    procedure Fill; override;
    function GetSourceForm: TdxForm;
  end;

  { TFieldCbx }

  TFieldCbx = class(TSortedActionCbx)
  private
    procedure FillFormFields(Fm: TdxForm);
    procedure FillQueryFields(RD: TReportData);
  public
    procedure Fill; override;
    function GetSourceObject: TObject;
  end;

  { TComponentCbx }

  TComponentCbx = class(TSortedActionCbx)
  public
    procedure Fill; override;
    procedure SelectComponent(const AName: String);
    function GetSourceForm: TdxForm;
  end;

  { TReportCbx }

  TReportCbx = class(TSortedActionCbx)
  public
    procedure Fill; override;
  end;

  { TTemplateCbx }

  TTemplateCbx = class(TActionCbx)
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Fill; override;
  end;

  { TListCbx }

  TListCbx = class(TGridComboBox)
  public
    constructor Create(TheOwner: TComponent); override;
    procedure LoadItems(const S: String);
  end;

  { TActionColorButton }

  TActionColorButton = class(TColorButton)
  private
    FFocused: Boolean;
    procedure SetFocused(AValue: Boolean);
  protected
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
      AState: TButtonState; ATransparent: Boolean; BiDiFlags: Longint): TRect;
      override;
  public
    property Focused: Boolean read FFocused write SetFocused;
  end;

  { TActionColor }

  TActionColor = class(TWinControl)
  private
    FDefaultColor: TColor;
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
    FColor: TActionColorButton;
    procedure ColorChanged(Sender: TObject);
    procedure ColorClick(Sender: TObject);
    //procedure ColorPaint(Sender: TObject);
    procedure HintShow(Sender: TObject; HintInfo: PHintInfo);
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  protected
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    function RealGetText: TCaption; override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(TheOwner: TComponent); override;
    property DefaultColor: TColor read FDefaultColor write FDefaultColor;
  end;

  { TActionImage }

  TActionImage = class(TExprCellEditor)
  protected
    procedure ButtonClick; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TGetDefaultCellTextEvent = procedure (Sender: TObject; Col: Integer; var aText: String) of object;

  { TActionGrid }

  TActionGrid = class(TStringGrid)
  private
    FOnChange: TNotifyEvent;
    FOnGetDefaultCellText: TGetDefaultCellTextEvent;
    procedure AddRow;
    procedure DelRow;
    procedure MoveUpRow;
    procedure MoveDownRow;
    procedure ClearGrid;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure DoChange;
  protected
    function GetCellHintText(ACol, ARow: Integer): string; override;
    procedure ColRowDeleted(IsColumn: Boolean; index: Integer); override;
    procedure ColRowInserted(IsColumn: boolean; index: integer); override;
    procedure ColRowExchanged(IsColumn: Boolean; index, WithIndex: Integer); override;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean; override;
    procedure PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property OnGetDefaultCellText: TGetDefaultCellTextEvent
			read FOnGetDefaultCellText write FOnGetDefaultCellText;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TActionDivider }

  TActionDivider = class(TCustomControl)
  public
    constructor Create(AOwner: TComponent; const AText: String);
  end;

  { TActionOptions }

  TActionOptions = class(TCheckListBox)
  private
    FOnChange: TNotifyEvent;
    procedure DoChange;
  protected
    procedure ClickCheck; override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function ActionFieldTypeToStr(C: TComponent): String;
function ActionRpFieldTypeToStr(Tp: TRpFieldType): String;

implementation

uses
  Forms, formmanager, dxfiles, dximages, apputils, exprform, reportmanager,
  LazUtf8, imagesform, appimagelists;

type
  { THackGrid }

  THackGrid = class(TCustomGrid);

function ActionFieldTypeToStr(C: TComponent): String;
begin
  if ((C is TdxEdit) or (C is TdxMemo) or (C is TdxComboBox)) then Result := 'text'
  else if C is TdxCalcEdit then Result := 'number'
  else if C is TdxDateEdit then Result := 'date'
  else if C is TdxTimeEdit then Result := 'time'
  else if C is TdxLookupComboBox then Result := 'object'
  else if C is TdxCounter then Result := 'counter'
  else if C is TdxCheckBox then Result := 'checkbox'
  else if C is TdxFile then Result := 'file'
  else if C is TdxDBImage then Result := 'image'
  else if C is TdxRecordId then Result := 'recid'
  else Result := '';
end;

function ActionRpFieldTypeToStr(Tp: TRpFieldType): String;
begin
  case Tp of
    flText: Result := 'text';
    flNumber: Result := 'number';
    flObject: Result := 'object';
    flCounter: Result := 'counter';
    flBool: Result := 'checkbox';
    flDate: Result := 'date';
    flTime: Result := 'time';
    flFile: Result := 'file';
    flRecId: Result := 'recid';
    flImage: Result := 'image';
    else Result := '';
  end;
end;

{ TActionColorButton }

procedure TActionColorButton.SetFocused(AValue: Boolean);
begin
  if FFocused=AValue then Exit;
  FFocused:=AValue;
  Invalidate;
end;

function TActionColorButton.DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
  const AOffset: TPoint; AState: TButtonState; ATransparent: Boolean;
  BiDiFlags: Longint): TRect;
var
  Size: TSize;
begin
  {if ButtonColor = clNone then ACanvas.Brush.Style := bsDiagCross;
  Result:=inherited DrawGlyph(ACanvas, AClient, AOffset, AState, ATransparent,
    BiDiFlags);
  ACanvas.Brush.Style := bsSolid; }
  Canvas.Pen.Color := clBlack;
  if FFocused then Canvas.Pen.Width := 3
  else Canvas.Pen.Width := 1;
  if AState = bsDisabled then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Bitmap := GetDisabledPattern;
  end
  else if ButtonColor = clNone then
  begin
    Canvas.Brush.Color := clBlack;
    Canvas.Brush.Style := bsBDiagonal;
  end
  else
  begin
    Canvas.Brush.Bitmap := nil;
    Canvas.Brush.Color := ButtonColor;
  end;
  Size := GetGlyphSize(true,AClient);

  Result := Bounds(AClient.Left + AOffset.X, AClient.Top + AOffset.Y,
                   Size.CX - 1, Size.CY - 1);
  Canvas.Rectangle(Result);
  Canvas.Brush.Style := bsSolid;
end;

{ TActionImage }

procedure TActionImage.ButtonClick;
begin
  inherited ButtonClick;
  if ShowImagesForm(True, Text, True) = mrOk then
    Text := ImagesFm.SelectedImageName;
end;

constructor TActionImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetupSpeedButton(Button, 'image16');
  Button.ShowHint := True;
  Button.Hint := rsSelectFromGallery;
end;

{ TActionColor }

procedure TActionColor.ColorChanged(Sender: TObject);
begin
  if FGrid <> nil then
    FGrid.EditorTextChanged(FCol, FRow, Caption);
end;

procedure TActionColor.ColorClick(Sender: TObject);
begin
  SetFocus;
end;

{procedure TActionColor.ColorPaint(Sender: TObject);
var
  C: TCanvas;
  R: TRect;
begin
  C := FColor.Canvas;
  R := FColor.BoundsRect;
  if Focused then
  begin
    R.Inflate(-ScaleToScreen(3), -ScaleToScreen(3), -ScaleToScreen(4), -ScaleToScreen(4));
    C.DrawFocusRect(R);
  end;
  if FColor.ButtonColor = clNone then
  begin
    C.Brush.Style := bsBDiagonal;
    C.Brush.Color := clBlack;
    C.FloodFill(R.Left + 10, R.Top + 10, clBlack, fsBorder);
    C.Brush.Style := bsSolid;
  end;
end;  }

procedure TActionColor.HintShow(Sender: TObject; HintInfo: PHintInfo);
var
  R, G, B: Byte;
  C: TColor;
  S: String;
begin
  C := FColor.ButtonColor;
  RedGreenBlue(C, R, G, B);
  if ColorToIdent(C, S) then
    S := Format('%s (rgb: %d, %d, %d)', [Copy(S, 3, 255), R, G, B])
  else
    S := Format('rgb: %d, %d, %d', [R, G, B]);
  HintInfo^.HintStr := S;
end;

procedure TActionColor.MenuClick(Sender: TObject);
var
  Clr: TColor;
begin
  case TMenuItem(Sender).Tag of
    0: Clipboard.AsText := ColorToString(FColor.ButtonColor);
    1: if TryStrToColor(Clipboard.AsText, Clr) then FColor.ButtonColor := Clr;
    2: FColor.ButtonColor := clNone;
    3: FColor.ButtonColor := FDefaultColor;
  end;
end;

procedure TActionColor.MenuPopup(Sender: TObject);
begin
  TPopupMenu(Sender).Items[3].Visible := FDefaultColor <> clNone;
end;

procedure TActionColor.KeyDown(var Key: Word; Shift: TShiftState);
  procedure doGridKeyDown;
  begin
    if FGrid<>nil then
      THackGrid(FGrid).KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := THackGrid(FGrid).FastEditing
    else
      Result := False;
  end;

begin
  inherited KeyDown(Key,Shift);
  case Key of
    VK_UP, VK_DOWN:
      doGridKeyDown;
    VK_LEFT, VK_RIGHT:
      if GetFastEntry and (Key in [VK_LEFT, VK_RIGHT]) then
        doGridKeyDown;
    VK_END, VK_HOME:
      ;
    VK_SPACE: FColor.Click;
    VK_ESCAPE, VK_RETURN:
      begin
        doGridKeyDown;
        if (FGrid <> nil) and (key<>0) then
          THackGrid(FGrid).EditorHide;
      end;
  end;
end;

function TActionColor.RealGetText: TCaption;
begin
  Result := ColorToString(FColor.ButtonColor);
end;

procedure TActionColor.RealSetText(const AValue: TCaption);
var
  Clr: TColor;
begin
  if not TryStrToColor(AValue, Clr) then Clr := clNone;
  FColor.ButtonColor := Clr;
end;

procedure TActionColor.WMSetFocus(var Message: TLMSetFocus);
begin
  //FColor.Invalidate;
  FColor.Focused:=True;
end;

procedure TActionColor.WMKillFocus(var Message: TLMKillFocus);
begin
  //FColor.Invalidate;
  FColor.Focused:=False;
end;

procedure TActionColor.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col := FCol;
  Msg.Row := FRow;
  Msg.Value := Caption;
end;

procedure TActionColor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TActionColor.msg_SetValue(var Msg: TGridMessage);
begin
  Caption := Msg.Value;
end;

procedure TActionColor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TActionColor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TActionColor.Create(TheOwner: TComponent);
var
  Pop: TPopupMenu;
begin
  inherited Create(TheOwner);
  Height := ScaleToScreen(25);
  FColor := TActionColorButton.Create(Self);
  FColor.Parent := Self;
  FColor.Align := alClient;
  FColor.OnColorChanged:=@ColorChanged;
  FColor.OnShowHint:=@HintShow;
  FColor.ShowHint:=True;
  //FColor.OnPaint:=@ColorPaint;
  FColor.OnClick:=@ColorClick;
  FDefaultColor := clNone;
  TabStop := True;
  Pop := TPopupMenu.Create(Self);
  Pop.Images := Images16;
  Pop.Items.Add( CreateMenuItem(Pop, rsCopy, 0, ShortCut(VK_C, [ssCtrl]), @MenuClick, IMG16_COPY) );
  Pop.Items.Add( CreateMenuItem(Pop, rsPaste, 1, ShortCut(VK_V, [ssCtrl]), @MenuClick, IMG16_PASTE) );
  Pop.Items.Add( CreateMenuItem(Pop, rsClear, 2, 0, @MenuClick, IMG16_DELETE) );
  Pop.Items.Add( CreateMenuItem(Pop, rsRestoreDefColor, 3, 0, @MenuClick) );
  Pop.OnPopup:=@MenuPopup;
  PopupMenu := Pop;
end;

{ TExprCellEditor }

procedure TExprCellEditor.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipboard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
  end;
end;

procedure TExprCellEditor.MenuPopup(Sender: TObject);
begin
  PopupMenu.Items[0].Enabled := SelText <> '';
  PopupMenu.Items[1].Enabled := SelText <> '';
end;

procedure TExprCellEditor.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer;
  KeepBase: Boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  Button.Width := Height;
  Button.Height := Height;
end;

procedure TExprCellEditor.EditChange;
begin
  inherited EditChange;
  if (FGrid<>nil) and Visible then
  begin
    FGrid.EditorTextChanged(FCol, FRow, Text);
  end;
end;

// Копипаст из TStringCellEditor
procedure TExprCellEditor.EditKeyDown(var Key: Word; Shift: TShiftState);
  function AllSelected: boolean;
  begin
    result := (SelLength>0) and (SelLength=UTF8Length(Text));
  end;
  function AtStart: Boolean;
  begin
    Result:= (SelStart=0);
  end;
  function AtEnd: Boolean;
  begin
    result := ((SelStart+1)>UTF8Length(Text)) or AllSelected;
  end;
  procedure doEditorKeyDown;
  begin
    if FGrid<>nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;
  procedure doGridKeyDown;
  begin
    if FGrid<>nil then
      THackGrid(FGrid).KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := THackGrid(FGrid).FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or THackGrid(FGrid).EditorIsReadOnly then
      Key := 0;
  end;
var
  IntSel: boolean;
begin
  inherited EditKeyDown(Key,Shift);
  if FGrid = nil then Exit;
  case Key of
    VK_F2:
      if AllSelected then begin
        SelLength := 0;
        SelStart := Length(Text);
      end;
    VK_DELETE, VK_BACK:
      CheckEditingKey;
    VK_UP, VK_DOWN:
      doGridKeyDown;
    VK_LEFT, VK_RIGHT:
      if GetFastEntry then begin
        IntSel:=
          ((Key=VK_LEFT) and not AtStart) or
          ((Key=VK_RIGHT) and not AtEnd);
      if not IntSel then begin
          doGridKeyDown;
      end;
    end;
    VK_END, VK_HOME:
      ;
    VK_ESCAPE, VK_RETURN:
      begin
        doGridKeyDown;
        if (key<>0) and (FGrid <> nil) then begin
          //SetEditText(THackGrid(FGrid).FEditorOldValue);
          THackGrid(FGrid).EditorHide;
        end;
      end;
    else
      doEditorKeyDown;
  end;
end;

procedure TExprCellEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text:=Msg.Value;
  SelStart := UTF8Length(Text);
end;

procedure TExprCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col:=FCol;
  Msg.Row:=FRow;
  Msg.Value:=Text;
end;

procedure TExprCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=TMyDBGrid(Msg.Grid);
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TExprCellEditor.msg_SelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

procedure TExprCellEditor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TExprCellEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TExprCellEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Spacing := 2;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuHandler, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuHandler, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuHandler, IMG16_PASTE) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

{ TActionText }

procedure TActionText.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipboard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
  end;
end;

procedure TActionText.MenuPopup(Sender: TObject);
begin
  PopupMenu.Items[0].Enabled := SelText <> '';
  PopupMenu.Items[1].Enabled := SelText <> '';
end;

procedure TActionText.Change;
begin
  inherited Change;
  if (FGrid<>nil) and Visible then
  begin
    FGrid.EditorTextChanged(FCol, FRow, Text);
  end;
end;

// Копипаст из TStringCellEditor
procedure TActionText.KeyDown(var Key: Word; Shift: TShiftState);
  function AllSelected: boolean;
  begin
    result := (SelLength>0) and (SelLength=UTF8Length(Text));
  end;
  function AtStart: Boolean;
  begin
    Result:= (SelStart=0);
  end;
  function AtEnd: Boolean;
  begin
    result := ((SelStart+1)>UTF8Length(Text)) or AllSelected;
  end;
  procedure doEditorKeyDown;
  begin
    if FGrid<>nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;
  procedure doGridKeyDown;
  begin
    if FGrid<>nil then
      THackGrid(FGrid).KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := THackGrid(FGrid).FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or THackGrid(FGrid).EditorIsReadOnly then
      Key := 0;
  end;
var
  IntSel: boolean;
begin
  inherited KeyDown(Key,Shift);
  if FGrid = nil then Exit;
  case Key of
    VK_F2:
      if AllSelected then begin
        SelLength := 0;
        SelStart := Length(Text);
      end;
    VK_DELETE, VK_BACK:
      CheckEditingKey;
    VK_UP, VK_DOWN:
      doGridKeyDown;
    VK_LEFT, VK_RIGHT:
      if GetFastEntry then begin
        IntSel:=
          ((Key=VK_LEFT) and not AtStart) or
          ((Key=VK_RIGHT) and not AtEnd);
      if not IntSel then begin
          doGridKeyDown;
      end;
    end;
    VK_END, VK_HOME:
      ;
    VK_ESCAPE, VK_RETURN:
      begin
        doGridKeyDown;
        if key<>0 then begin
          //SetEditText(THackGrid(FGrid).FEditorOldValue);
          THackGrid(FGrid).EditorHide;
        end;
      end;
    else
      doEditorKeyDown;
  end;
end;

procedure TActionText.msg_SetValue(var Msg: TGridMessage);
begin
  Text:=Msg.Value;
  SelStart := UTF8Length(Text);
end;

procedure TActionText.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col:=FCol;
  Msg.Row:=FRow;
  Msg.Value:=Text;
end;

procedure TActionText.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=TMyDBGrid(Msg.Grid);
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TActionText.msg_SelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

procedure TActionText.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TActionText.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TActionText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuHandler, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuHandler, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuHandler, IMG16_PASTE) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

{ TActionOptions }

procedure TActionOptions.DoChange;
begin
  if FOnChange <> nil then FOnChange(Self);
end;

procedure TActionOptions.ClickCheck;
begin
  inherited ClickCheck;
  DoChange;
end;

{ TActionFilterExpr }

function TActionFilterExpr.RealGetText: TCaption;
begin
  Result:=Edit.Text;
end;

procedure TActionFilterExpr.RealSetText(const AValue: TCaption);
begin
  Edit.Text := AValue;
end;

procedure TActionFilterExpr.ButtonClick;
var
  Fm, CFm: TdxForm;
  S: TCaption;
begin
  if SourceCbx = nil then
    Fm := FCurForm
  else if SourceCbx is TFormCbx then
  	Fm := FormMan.FindFormByName(SourceCbx.Text)
  else
    Fm := nil;

  S := Text;

  if FFormCbx = nil then
    CFm := FCurForm
  else if (FFormCbx is TFormCbx) or (FFormCbx is TChildFormCbx) then
    CFm := FormMan.FindFormByName(FFormCbx.Text)
  else
    CFm := nil;

  if FNoForm then CFm := nil;

  if ShowExprForm(etFilter, nil, S, CFm, Fm, nil, nil) = mrOk then
    Text := S;
end;

constructor TActionFilterExpr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ButtonCaption:='';
  SetupSpeedButton(Button, 'filter16');
end;

{ TActionDivider }

constructor TActionDivider.Create(AOwner: TComponent; const AText: String);
var
  T: TStaticText;
  B: TBevel;
begin
  inherited Create(AOwner);
  T := TStaticText.Create(Self);
  T.AutoSize := True;
  T.Transparent := False;
  T.Parent :=Self;
  T.Caption := AText;
  T.Font.Style := [fsBold];
  B := TBevel.Create(Self);
  B.Shape:=bsTopLine;
  B.Height := 2;
  B.Anchors := [akLeft, akTop, akRight];
  B.Parent := Self;
  ClientHeight := T.Height;
  B.AnchorVerticalCenterTo(Self);
  B.AnchorSideRight.Control := Self;
  B.AnchorSideRight.Side := asrBottom;
  T.AnchorHorizontalCenterTo(Self);
end;

{ TListCbx }

constructor TListCbx.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  DropDownCount := 16;
  Style := csDropDownList;
end;

procedure TListCbx.LoadItems(const S: String);
begin
  SplitStr(S, ';', Items);
end;

{ TTemplateCbx }

constructor TTemplateCbx.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);
  Style := csDropDown;
end;

procedure TTemplateCbx.Fill;
begin
  Clear;
  GetTemplates(Items);
end;

{ TReportCbx }

procedure TReportCbx.Fill;
begin
  Sorted := False;
  Clear;
  ReportMan.GetReports(Items);
  if Items.Count > 0 then Items.Insert(0, '');
end;

{ TComponentCbx }

procedure TComponentCbx.Fill;
var
  S: TCaption;
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  Nm: String;
begin
	Clear;
  Fm := GetSourceForm;
  if Fm = nil then Exit;

  S := '';
  if FFilter <> '' then
	  S := ';' + AnsiLowerCase(FFilter) + ';';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (S = '') or (Pos(';' + AnsiLowerCase(C.ClassName) + ';', S) > 0) then
    begin
      Nm := GetComponentName(C) + ' - ' + GetComponentType(C);
    	Items.AddObject(Nm, C);
    end;
  end;
  if Items.Count > 0 then Items.Add('');
end;

procedure TComponentCbx.SelectComponent(const AName: String);
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to Items.Count - 1 do
  begin
    C := TComponent(Items.Objects[i]);
    if (C <> nil) and (CompareText(C.Name, AName) = 0) then
    begin
      ItemIndex := i;
      Exit;
    end;
  end;
end;

function TComponentCbx.GetSourceForm: TdxForm;
var
  S: TCaption;
begin
  if FSourceCbx <> nil then
  begin
    S := FSourceCbx.Text;
    if (FSourceCbx is TFormCbx) or (FSourceCbx is TChildFormCbx) then
		  Result := FormMan.FindFormByName(S)
    else if FSourceCbx is TObjectCbx then
	  	Result := TObjectCbx(FSourceCbx).GetSourceForm
    else
      Result := nil;
  end
  else
  	Result := FCurForm;
end;

{ TObjectCbx }

procedure TObjectCbx.Fill;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
begin
  Clear;
  Fm := nil;
  if FSourceCbx = nil then
  	Fm := FCurForm
  else if (FSourceCbx is TFormCbx) or (FSourceCbx is TChildFormCbx) then
    Fm := FormMan.FindFormByName(FSourceCbx.Text);
  if Fm = nil then Exit;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxLookupComboBox then
    	Items.Add(GetFieldName(C));
  end;
  if Items.Count > 0 then Items.Add('');
end;

function TObjectCbx.GetSourceForm: TdxForm;
var
  SrcFm: TdxForm;
  C: TComponent;
begin
  Result := nil;
  if FSourceCbx = nil then SrcFm := FCurForm
  else if not ((FSourceCbx is TFormCbx) or (FSourceCbx is TChildFormCbx)) then Exit
  else SrcFm := FormMan.FindFormByName(FSourceCbx.Text);

  if SrcFm <> nil then
  begin
    C := FindComponentByFieldName(SrcFm, Text);
    if (C <> nil) and (C is TdxLookupComboBox) then
      Result := FormMan.FindForm(GetSourceTId(C));
  end;
end;

{ TQueryCbx }

procedure TQueryCbx.Fill;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  RD: TReportData;
begin
  Clear;
  Fm := nil;
  if FSourceCbx = nil then
    Fm := FCurForm
  else if (FSourceCbx is TFormCbx) or (FSourceCbx is TChildFormCbx) then
    Fm := FormMan.FindFormByName(FSourceCbx.Text)
  else if FSourceCbx is TObjectCbx then
  	Fm := TObjectCbx(FSourceCbx).GetSourceForm;
  if Fm = nil then Exit;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxQueryGrid then
    begin
      RD := ReportMan.FindReport(TdxQueryGrid(C).Id);
      Items.Add(RD.Name);
    end;
  end;
  if Items.Count > 0 then Items.Add('');
end;

{ TChildFormCbx }

procedure TChildFormCbx.Fill;
var
  Fm: TdxForm;
  i, FmId: Integer;
begin
  Clear;
  Fm := nil;
  if FSourceCbx = nil then
    Fm := FCurForm
  else if FSourceCbx is TFormCbx then
    Fm := FormMan.FindFormByName(FSourceCbx.Text)
  else if FSourceCbx is TObjectCbx then
  	Fm := TObjectCbx(FSourceCbx).GetSourceForm;
  if Fm = nil then Exit;
  FmId := Fm.Id;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if Fm.PId = FmId then
	  	Items.Add(Fm.FormCaption);
  end;
  if Items.Count > 0 then Items.Add('');
end;

{ TActionExpr }

function TActionExpr.RealGetText: TCaption;
begin
  Result:=Edit.Text;
end;

procedure TActionExpr.RealSetText(const AValue: TCaption);
begin
  Edit.Text := AValue;
end;

procedure TActionExpr.ButtonClick;
var
  Fm: TdxForm;
  S: TCaption;
begin
  inherited ButtonClick;
  Fm := GetSourceForm;
  S := Text;
  if ShowExprForm(etActionExpr, nil, S, Fm, nil, nil, nil) = mrOk then
    Text := S;
end;

constructor TActionExpr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ButtonCaption:='';
  SetupSpeedButton(Button, 'sum16');
end;

function TActionExpr.GetSourceForm: TdxForm;
begin
  if FNoForm then
    Result := nil
  else if SourceCbx = nil then
    Result := FCurForm
  else if (SourceCbx is TFormCbx) or (SourceCbx is TChildFormCbx) then
  	Result := FormMan.FindFormByName(SourceCbx.Text)
  else
    Result := nil;
end;

{ TActionFolder }

procedure TActionFolder.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipboard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
  end;
end;

procedure TActionFolder.MenuPopup(Sender: TObject);
begin
  PopupMenu.Items[0].Enabled := SelText <> '';
  PopupMenu.Items[1].Enabled := SelText <> '';
end;

procedure TActionFolder.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer;
  KeepBase: Boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  Button.Width := Height;
  Button.Height := Height;
end;

function TActionFolder.RealGetText: TCaption;
begin
  Result:=Edit.Text;
end;

procedure TActionFolder.RealSetText(const AValue: TCaption);
begin
  Edit.Text := AValue;
end;

procedure TActionFolder.EditChange;
begin
  inherited EditChange;
  if (FGrid<>nil) and Visible then
  begin
    FGrid.EditorTextChanged(FCol, FRow, Text);
  end;
end;

// Копипаст из TStringCellEditor
procedure TActionFolder.EditKeyDown(var Key: Word; Shift: TShiftState);
  function AllSelected: boolean;
  begin
    result := (SelLength>0) and (SelLength=UTF8Length(Text));
  end;
  function AtStart: Boolean;
  begin
    Result:= (SelStart=0);
  end;
  function AtEnd: Boolean;
  begin
    result := ((SelStart+1)>UTF8Length(Text)) or AllSelected;
  end;
  procedure doEditorKeyDown;
  begin
    if FGrid<>nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;
  procedure doGridKeyDown;
  begin
    if FGrid<>nil then
      THackGrid(FGrid).KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := THackGrid(FGrid).FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or THackGrid(FGrid).EditorIsReadOnly then
      Key := 0;
  end;
var
  IntSel: boolean;
begin
  inherited EditKeyDown(Key,Shift);
  if FGrid = nil then Exit;
  case Key of
    VK_F2:
      if AllSelected then begin
        SelLength := 0;
        SelStart := Length(Text);
      end;
    VK_DELETE, VK_BACK:
      CheckEditingKey;
    VK_UP, VK_DOWN:
      doGridKeyDown;
    VK_LEFT, VK_RIGHT:
      if GetFastEntry then begin
        IntSel:=
          ((Key=VK_LEFT) and not AtStart) or
          ((Key=VK_RIGHT) and not AtEnd);
      if not IntSel then begin
          doGridKeyDown;
      end;
    end;
    VK_END, VK_HOME:
      ;
    VK_ESCAPE, VK_RETURN:
      begin
        doGridKeyDown;
        if (key<>0) and (FGrid <> nil) then
          THackGrid(FGrid).EditorHide;
      end;
    else
      doEditorKeyDown;
  end;
end;

procedure TActionFolder.msg_SetValue(var Msg: TGridMessage);
begin
  Text:=Msg.Value;
  SelStart := UTF8Length(Text);
end;

procedure TActionFolder.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col:=FCol;
  Msg.Row:=FRow;
  Msg.Value:=Text;
end;

procedure TActionFolder.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=TMyDBGrid(Msg.Grid);
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TActionFolder.msg_SelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

procedure TActionFolder.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TActionFolder.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TActionFolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuHandler, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuHandler, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuHandler, IMG16_PASTE) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

{ TActionFile }

procedure TActionFile.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipboard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
  end;
end;

procedure TActionFile.MenuPopup(Sender: TObject);
begin
  PopupMenu.Items[0].Enabled := SelText <> '';
  PopupMenu.Items[1].Enabled := SelText <> '';
end;

procedure TActionFile.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer;
  KeepBase: Boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  Button.Width := Height;
  Button.Height := Height;
end;

function TActionFile.RealGetText: TCaption;
begin
  Result:=Edit.Text;
end;

procedure TActionFile.RealSetText(const AValue: TCaption);
begin
  Edit.Text := AValue;
end;

procedure TActionFile.EditChange;
begin
  inherited EditChange;
  if (FGrid<>nil) and Visible then
  begin
    FGrid.EditorTextChanged(FCol, FRow, Text);
  end;
end;

// Копипаст из TStringCellEditor
procedure TActionFile.EditKeyDown(var Key: Word; Shift: TShiftState);
  function AllSelected: boolean;
  begin
    result := (SelLength>0) and (SelLength=UTF8Length(Text));
  end;
  function AtStart: Boolean;
  begin
    Result:= (SelStart=0);
  end;
  function AtEnd: Boolean;
  begin
    result := ((SelStart+1)>UTF8Length(Text)) or AllSelected;
  end;
  procedure doEditorKeyDown;
  begin
    if FGrid<>nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;
  procedure doGridKeyDown;
  begin
    if FGrid<>nil then
      THackGrid(FGrid).KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := THackGrid(FGrid).FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or THackGrid(FGrid).EditorIsReadOnly then
      Key := 0;
  end;
var
  IntSel: boolean;
begin
  inherited EditKeyDown(Key,Shift);
  if FGrid = nil then Exit;
  case Key of
    VK_F2:
      if AllSelected then begin
        SelLength := 0;
        SelStart := Length(Text);
      end;
    VK_DELETE, VK_BACK:
      CheckEditingKey;
    VK_UP, VK_DOWN:
      doGridKeyDown;
    VK_LEFT, VK_RIGHT:
      if GetFastEntry then begin
        IntSel:=
          ((Key=VK_LEFT) and not AtStart) or
          ((Key=VK_RIGHT) and not AtEnd);
      if not IntSel then begin
          doGridKeyDown;
      end;
    end;
    VK_END, VK_HOME:
      ;
    VK_ESCAPE, VK_RETURN:
      begin
        doGridKeyDown;
        if (key<>0) and (FGrid <> nil) then
          THackGrid(FGrid).EditorHide;
      end;
    else
      doEditorKeyDown;
  end;
end;

procedure TActionFile.msg_SetValue(var Msg: TGridMessage);
begin
  Text:=Msg.Value;
  SelStart := UTF8Length(Text);
end;

procedure TActionFile.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col:=FCol;
  Msg.Row:=FRow;
  Msg.Value:=Text;
end;

procedure TActionFile.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=TMyDBGrid(Msg.Grid);
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TActionFile.msg_SelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

procedure TActionFile.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TActionFile.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TActionFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DialogTitle:=rsSelectFile;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuHandler, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuHandler, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuHandler, IMG16_PASTE) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

{ TActionNumber }

procedure TActionNumber.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
	if Key in [#8, '0'..'9'] then
  else Key := #0;
end;

procedure TActionNumber.DoEnter;
begin
  inherited DoEnter;
  FOldNumber := Text;
end;

constructor TActionNumber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Заменяем стандартное меню на свое, пустое, чтобы не было возможности
  // случайно вставить текст.
  PopupMenu := TPopupMenu.Create(Self);
end;

procedure TActionNumber.EditingDone;
begin
  if (Text <> '') and (CheckFloatRange(Text) <> '') then
	  Text := FOldNumber;
  inherited EditingDone;
end;

{ TActionGrid }

procedure TActionGrid.AddRow;
var
  i: Integer;
  S: String;
begin
  RowCount := RowCount + 1;
  Row := RowCount - 1;
  for i := 0 to Columns.Count - 1 do
  begin
  	if Columns[i].ButtonStyle = cbsCheckboxColumn then
      Cells[i, Row] := '0';
    if FOnGetDefaultCellText <> nil then
    begin
      S := '';
      FOnGetDefaultCellText(Self, i, S);
      if S <> '' then Cells[i, Row] := S;
    end;
  end;
end;

procedure TActionGrid.DelRow;
begin
  if RowCount > FixedRows then
    DeleteRow(Row);
end;

procedure TActionGrid.MoveUpRow;
var
  r: Integer;
begin
  if Row > FixedRows then
  begin
    ExchangeColRow(False, Row, Row - 1);
    if TopRow > Row then
    begin
      r := TopRow - Round(VisibleRowCount / 2);
      if r < FixedRows then r := FixedRows;
      TopRow := r;
    end;
  end;
end;

procedure TActionGrid.MoveDownRow;
var
  r: Integer;
begin
  if (Row >= FixedRows) and (Row < RowCount - 1) then
  begin
    ExchangeColRow(False, Row, Row + 1);
    if TopRow + VisibleRowCount <= Row then
    begin
      r := TopRow + Round(VisibleRowCount / 2);
      if r + VisibleRowCount >= RowCount then r := RowCount - VisibleRowCount;
      TopRow := r;
    end;
  end;
end;

procedure TActionGrid.ClearGrid;
begin
  if RowCount > FixedRows then
    if Confirm(rsClearTable, rsClearTableMsg) = mrYes then
      RowCount := FixedRows;
end;

procedure TActionGrid.MenuHandler(Sender: TObject);
begin
  case TComponent(Sender).Tag of
  	0: AddRow;
    1: DelRow;
    3: MoveUpRow;
    4: MovedownRow;
    6: ClearGrid;
  end;
end;

procedure TActionGrid.MenuPopup(Sender: TObject);
begin
  with PopupMenu do
	begin
    Items[0].Enabled := ColCount > 0;
    Items[1].Enabled := RowCount > FixedRows;
    Items[3].Enabled := Row > FixedRows;
    Items[4].Enabled := (Row >= FixedRows) and (Row < RowCount - 1);
    Items[6].Enabled := RowCount > FixedRows;
  end;
end;

procedure TActionGrid.DoChange;
begin
  if FOnChange <> nil then FOnChange(Self);
end;

function TActionGrid.GetCellHintText(ACol, ARow: Integer): string;
var
  S: String;
begin
  // Заметил глюк при выводе подсказки в гриде, но не могу понять в какой
  // момент это происходит. Сделаю на всякий случай проверку (25.07.2018)
  if (ACol >= ColCount) or (ARow >= RowCount) then Exit;

  Result:=inherited GetCellHintText(ACol, ARow);
  if ARow >= FixedRows then
    S := Cells[ACol, ARow]
  else
    S := Columns[ACol].Title.Caption;
  if Canvas.TextWidth(S) > ColWidths[ACol] then
		Result:=StringReplace(S, '|', '/', [rfReplaceAll]);
end;

procedure TActionGrid.ColRowDeleted(IsColumn: Boolean; index: Integer);
begin
  inherited ColRowDeleted(IsColumn, index);
  DoChange;
end;

procedure TActionGrid.ColRowInserted(IsColumn: boolean; index: integer);
begin
  inherited ColRowInserted(IsColumn, index);
  DoChange;
end;

procedure TActionGrid.ColRowExchanged(IsColumn: Boolean; index,
  WithIndex: Integer);
begin
  inherited ColRowExchanged(IsColumn, index, WithIndex);
  DoChange;
end;

procedure TActionGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  inherited SizeChanged(OldColCount, OldRowCount);
  DoChange;
end;

procedure TActionGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  K: Word;
begin
  K := Key;
  inherited KeyDown(Key, Shift);
  case K of
    VK_DELETE: if Shift = [ssCtrl] then DelRow;
    VK_PRIOR: MoveUpRow;
    VK_NEXT: MoveDownRow;
  end;
end;

function TActionGrid.CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result:=inherited CanGridAcceptKey(Key, Shift);
  if Key in [VK_DELETE, VK_NEXT, VK_PRIOR] then Result := False
  else if (Editor <> nil) and not (Editor is TCustomComboBox) then Result := True
  else if (Key in [VK_C, VK_V, VK_X]) and (ssCtrl in Shift) then Result := False;
end;

procedure TActionGrid.PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState
  );
begin
  inherited PrepareCanvas(aCol, aRow, aState);
  Canvas.Brush.Style := bsSolid;
  if gdSelected in aState then
  begin
		if not Focused then
    begin
      Canvas.Brush.Color := clSilver;
      Canvas.Font.Color := clBlack;
    end;
  end
end;

constructor TActionGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AllowOutboundEvents:=False;
  AutoAdvance:=aaNone;
  FastEditing := False;
  Options := Options - [goColMoving, goRangeSelect] +
  	[goThumbTracking, goCellHints, goDrawFocusSelected];
  CellHintPriority:=chpAllNoDefault;
  ShowHint := True;
  //AutoFillColumns:=True; Если установить сразу, то глючит
  FixedRows := 0;
  FixedCols := 0;
  RowCount := 0;
  ColCount := 0;
  Height := 150;
  Flat := True;
  FocusRectVisible:=False;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsAdd, 0, ShortCut(VK_INSERT, []), @MenuHandler, IMG16_ADD) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsDelete, 1, ShortCut(VK_DELETE, [ssCtrl]), @MenuHandler, IMG16_DELETE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 2, 0, @MenuHandler) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsMoveUp, 3, ShortCut(VK_PRIOR, []), @MenuHandler, IMG16_UP) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsMoveDown, 4, ShortCut(VK_NEXT, []), @MenuHandler, IMG16_DOWN) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 5, 0, @MenuHandler) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClearTable, 6, 0, @MenuHandler) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

{ TSortedActionCbx }

constructor TSortedActionCbx.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Sorted := True;
end;

{ TGridComboBox }

procedure TGridComboBox.SaveOldObject;
begin
  FOldObject := TCustomStringGrid(FGrid).Objects[FCol, FRow];
end;

procedure TGridComboBox.RestoreOldObject;
begin
  TCustomStringGrid(FGrid).Objects[FCol, FRow] := FOldObject;
end;

procedure TGridComboBox.DoEnter;
begin
  inherited DoEnter;
  if FGrid <> nil then SaveOldObject;
end;

procedure TGridComboBox.WndProc(var TheMessage: TLMessage);
begin
  if TheMessage.msg=LM_KILLFOCUS then begin
    if HWND(TheMessage.WParam) = HWND(Handle) then begin
      // lost the focus but it returns to ourselves
      // eat the message.
      TheMessage.Result := 0;
      exit;
    end;
  end;
  inherited WndProc(TheMessage);
end;

procedure TGridComboBox.KeyDown(var Key: Word; Shift: TShiftState);
  function AllSelected: boolean;
  begin
    result := (SelLength>0) and (SelLength=Length(Text));
  end;
  function AtStart: Boolean;
  begin
    Result:= (SelStart=0);
  end;
  function AtEnd: Boolean;
  begin
    result := ((SelStart+1)>Length(Text)) or AllSelected;
  end;
  procedure doEditorKeyDown;
  begin
    if FGrid<>nil then
      THackGrid(FGrid).EditorkeyDown(Self, key, shift);
  end;
  procedure doGridKeyDown;
  begin
    if FGrid<>nil then
      THackGrid(FGrid).KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := THackGrid(FGrid).FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    // if editor is not readonly, start editing
    // else not interested
    if (FGrid=nil) or THackGrid(FGrid).EditorIsReadOnly then
      Key := 0;
  end;
var
  IntSel: boolean;
begin
  inherited KeyDown(Key,Shift);

  if FGrid = nil then
  begin
    if Key = VK_DOWN then
    begin
      if not DroppedDown then
        DroppedDown := True;
    end;
    Exit;
  end;

  case Key of

    VK_F2:
      if AllSelected then begin
        SelLength := 0;
        SelStart := Length(Text);
      end;

    VK_RETURN:
      if DroppedDown then begin
        CheckEditingKey;
        DroppedDown := False;
        if Key<>0 then begin
          doEditorKeyDown;
          Key:=0;
        end;
      end else
        doEditorKeyDown;

    VK_DELETE:
      CheckEditingKey;

    VK_UP:
      if not DroppedDown then
        doGridKeyDown;

    VK_DOWN:
      if not DroppedDown then
        DroppedDown := True;

    VK_LEFT, VK_RIGHT:
      if GetFastEntry then begin
        IntSel:=
          ((Key=VK_LEFT) and not AtStart) or
          ((Key=VK_RIGHT) and not AtEnd);
        if not IntSel then begin
            doGridKeyDown;
      end;
    end;

    VK_END, VK_HOME:
      ;
    VK_ESCAPE:
      begin
        doGridKeyDown;
        RestoreOldObject;
        if FGrid <> nil then THackGrid(FGrid).EditorHide;
      end;
    else
      doEditorKeyDown;
  end;
end;

procedure TGridComboBox.Select;
begin
  if FGrid<>nil then begin
    FGrid.EditorTextChanged(FCol, FRow, Text);
    if ItemIndex >= 0 then
      TCustomStringGrid(FGrid).Objects[FCol, FRow] := Items.Objects[ItemIndex];
    THackGrid(FGrid).PickListItemSelected(Self);
  end;
  inherited Select;
end;

procedure TGridComboBox.Change;
begin
  if FGrid<>nil then
    FGrid.EditorTextChanged(FCol, FRow, Text);
  inherited Change;
end;

procedure TGridComboBox.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col := FCol;
  Msg.Row := FRow;
  Msg.Value:=Text;
end;

procedure TGridComboBox.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TGridComboBox.msg_SetValue(var Msg: TGridMessage);
begin
  Text := Msg.Value;
  SelStart := Length(Text);
end;

procedure TGridComboBox.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TGridComboBox.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TGridComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  DropDownCount := 16;
  Style := csDropDownList;
end;

{TActionCbx}

procedure TActionCbx.Fill;
begin

end;

{ TFormCbx }

procedure TFormCbx.FillForms;
begin
  Sorted := False;
	FormMan.FormsToList(Items);
  if Items.Count > 0 then Items.Insert(0, '');
end;

procedure TFormCbx.FillQueryForms;
var
  S: TCaption;
  RD: TReportData;
begin
  S := FSourceCbx.Text;
  if S = '' then Exit;
  RD := ReportMan.FindQueryByName(S);
  if RD = nil then Exit;
  RD.GetSourceForms(Items);

  if Items.Count > 0 then Items.Add('');
end;

procedure TFormCbx.Fill;
begin
  Clear;
  if FSourceCbx = nil then FillForms
  else if FSourceCbx is TQueryCbx then FillQueryForms;
end;

{ TFieldCbx }

procedure TFieldCbx.FillFormFields(Fm: TdxForm);
var
  S, FlTp: String;
  i: Integer;
  C: TComponent;
begin
  S := ';' + AnsiLowerCase(FFilter) + ';';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not (C is TdxRecordId) and not IsField(C) then Continue;
    FlTp :=  ActionFieldTypeToStr(C);
    if (FFilter = '') or ((FlTp <> '') and (Pos(';' + FlTp + ';', S) > 0))
    	{( (FFilter = '') and IsField(C) ) or
    	( (Pos(';text;', S) > 0) and ((C is TdxEdit) or (C is TdxMemo) or (C is TdxComboBox)) ) or
	    ( (Pos(';number;', S) > 0) and (C is TdxCalcEdit) ) or
      ( (Pos(';date;', S) > 0) and (C is TdxDateEdit) ) or
      ( (Pos(';time;', S) > 0) and (C is TdxTimeEdit) ) or
      ( (Pos(';object;', S) > 0) and (C is TdxLookupComboBox) ) or
      ( (Pos(';counter;', S) > 0) and (C is TdxCounter) ) or
      ( (Pos(';checkbox;', S) > 0) and (C is TdxCheckBox) ) or
      ( (Pos(';file;', S) > 0) and (C is TdxFile) ) or
			( (Pos(';image;', S) > 0) and (C is TdxDBImage) )}
      then Items.Add(GetFieldName(C))
  end;
end;

procedure TFieldCbx.FillQueryFields(RD: TReportData);
var
  i: Integer;
  S, FlTp: String;
begin
  if RD.IsEmpty then Exit;
  S := ';' + AnsiLowerCase(FFilter) + ';';
  for i := 0 to RD.GetFieldCount - 1 do
  begin
    if not RD.GetFieldVisible(i) then Continue;
    FlTp := ActionRpFieldTypeToStr( RD.GetFieldType(i) );

    if (FFilter = '') or ((FlTp <> '') and (Pos(';' + FlTp + ';', S) > 0))
      then Items.Add(RD.GetFieldName(i));
  end;
end;

procedure TFieldCbx.Fill;
var
  {S: String;
  Fm: TdxForm;
  RD: TReportData;}
  Obj: TObject;
begin
  Clear;
  {RD := nil;
  Fm := nil;

  if FSourceCbx <> nil then
  begin
    S := FSourceCbx.Text;
    if (FSourceCbx is TFormCbx) or (FSourceCbx is TChildFormCbx) then
		  Fm := FormMan.FindFormByName(S)
    else if FSourceCbx is TObjectCbx then
	  	Fm := TObjectCbx(FSourceCbx).GetSourceForm
    else if (FSourceCbx is TQueryCbx) or (FSourceCbx is TReportCbx) then
      RD := ReportMan.FindByName(S);
  end
  else
  	Fm := FCurForm;
  if Fm <> nil then FillFormFields(Fm)
  else if RD <> nil then FillQueryFields(RD); }

  Obj := GetSourceObject;
  if Obj is TdxForm then FillFormFields(TdxForm(Obj))
  else if Obj is TReportData then FillQueryFields(TReportData(Obj));

  if Items.Count > 0 then Items.Add('');
end;

function TFieldCbx.GetSourceObject: TObject;
var
  S: TCaption;
begin
  if FSourceCbx <> nil then
  begin
    S := FSourceCbx.Text;
    if (FSourceCbx is TFormCbx) or (FSourceCbx is TChildFormCbx) then
		  Result := FormMan.FindFormByName(S)
    else if FSourceCbx is TObjectCbx then
	  	Result := TObjectCbx(FSourceCbx).GetSourceForm
    else if (FSourceCbx is TQueryCbx) or (FSourceCbx is TReportCbx) then
      Result := ReportMan.FindByName(S);
  end
  else
  	Result := FCurForm;
end;

end.

