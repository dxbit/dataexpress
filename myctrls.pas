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
unit MyCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, StdCtrls, ExtCtrls, EditBtn, Controls, Grids, lists,
  DBGrids, Graphics, LclType, Menus, strconsts, db, Forms, ComCtrls, Buttons,
  LMessages, maskedit;

type
  { TFilterComboBox }

  TFilterComboBox = class(TComboBox)
  private
    FKeys: TList;
    FLookup: Boolean;
    procedure ClearKeys;
    function GetKey: Integer;
    procedure SetKey(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    procedure Clear; override;
    function AddKey(K: Integer): Integer;
    function AsSQL(const FieldName: String): String;
    property Lookup: Boolean read FLookup write FLookup;
    property Key: Integer read GetKey write SetKey;
  end;

  { TFilterEdit }

  TFilterEdit = class(TEdit)
  private
    FNumeric: Boolean;
  protected
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(AOwner: TComponent); override;
    function AsSQL(const FieldName: String): String;
    function Validate: Boolean;
    property Numeric: Boolean read FNumeric write FNumeric;
  end;

  { TFilterRange }

  TFilterRange = class(TPanel)
    procedure EditorEditingDone(Sender: TObject);
  private
    FBegin, FEnd: TFilterEdit;
  protected
    procedure DoOnResize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
    function AsSQL(const FieldName: String): String;
    function Validate: Boolean;
    function Focused: Boolean; override;
    property BeginR: TFilterEdit read FBegin;
    property EndR: TFilterEdit read FEnd;
    property OnEditingDone;
  end;

  { TFilterDate }

  TFilterDate = class(TDateEdit)
  public
    constructor Create(AOwner: TComponent); override;
    function Validate: Boolean;
  end;

  TPeriodType = (ptNone, ptToday, ptThisWeek, ptThisMonth, ptThisYear);

  { TFilterPeriod }

  TFilterPeriod = class(TPanel)
    procedure MenuPopup(Sender: TObject);
  private
    FBegin, FEnd: TFilterDate;
    FPeriod: TPeriodType;
    FPopup: TPopupMenu;
    FBeginText, FEndText: String;
    procedure MenuClick(Sender: TObject);
    procedure SetPeriod(AValue: TPeriodType);
    procedure EditorEditingDone(Sender: TObject);
  protected
    procedure DoOnResize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
    function AsSQL(const FieldName: String): String;
    function Validate: Boolean;
    function Focused: Boolean; override;
    property BeginP: TFilterDate read FBegin;
    property EndP: TFilterDate read FEnd;
    property Period: TPeriodType read FPeriod write SetPeriod;
    property OnEditingDone;
  end;

  { TFilterCheckBox }

  TFilterCheckBox = class(TCheckBox)
  private
    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    function AsSQL(const FieldName: String): String;
    property Value: Integer read GetValue write SetValue;
  end;

  { TMyGrid }

  TMyGridCanSortEvent = procedure (Sender: TObject; Index: Integer;
    var Cancel: Boolean) of object;

  TMyGrid = class(TStringGrid)
  private
    FOnCanSort: TMyGridCanSortEvent;
    FSortCols: TSortColList;
    FUp, FDown: TBitmap;
    FWordWrap: Boolean;
  protected
    procedure DrawCellText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState;
      aText: String); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SortCols: TSortColList read FSortCols;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property OnCanSort: TMyGridCanSortEvent read FOnCanSort write FOnCanSort;
  end;

  { TGridButton }

  TGridButton = class(TSpeedButton)
  public
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer; Raw: boolean=
      false; WithThemeSpace: boolean=true); override;
  end;

  TGridButtonType = (gbnAppend, gbnEdit, gbnDelete, gbnDuplicate, gbnShopping,
    gbnMoveUp, gbnMoveDown, gbnRefresh, gbnGoto);

  TGridButtonSet = set of TGridButtonType;

  TGridButtonClick = procedure (Sender: TObject; Bn: TGridButtonType) of object;

  { TGridButtons }

  TGridButtons = class(TCustomControl)
  private
    FButtons: TList;
    FButtonSize: Integer;
    FFlat: Boolean;
    FOnButtonClick: TGridButtonClick;
    FVisibleButtons: TGridButtonSet;
    FVisibleCaptions: TGridButtonSet;
    procedure ButtonClick(Sender: TObject);
    procedure ClearButtons;
    function GetButtons(Index: TGridButtonType): TGridButton;
    procedure SetButtonSize(AValue: Integer);
    procedure SetFlat(AValue: Boolean);
    procedure SetVisibleButtons(AValue: TGridButtonSet);
    procedure SetVisibleCaptions(AValue: TGridButtonSet);
  protected
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateButtons;
    procedure ShowButton(Bn: TGridButtonType; Value: Boolean);
    procedure EnableButton(Bn: TGridButtonType; Value: Boolean);
    property ButtonSize: Integer read FButtonSize write SetButtonSize;
    property VisibleButtons: TGridButtonSet read FVisibleButtons write SetVisibleButtons;
    property VisibleCaptions: TGridButtonSet read FVisibleCaptions write SetVisibleCaptions;
    property Flat: Boolean read FFlat write SetFlat;
    property Buttons[Index: TGridButtonType]: TGridButton read GetButtons;
    property OnButtonClick: TGridButtonClick read FOnButtonClick write
      FOnButtonClick;
  end;

  { TMaskEditEx }

  TMaskEditEx = class(TMaskEdit)
  public
    procedure ValidateEdit; override;
    function ValidateText: Boolean;
    function MaskTextEmpty: Boolean;
  end;

  TMyDBGrid = class;

  { TMaskCellEditor }

  TMaskCellEditor = class(TStringCellEditor)
  private
    FGrid: TMyDBGrid;
    FCol,FRow:Integer;
  protected
    procedure WndProc(var TheMessage : TLMessage); override;
    procedure Change; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure msg_SetMask(var Msg: TGridMessage); message GM_SETMASK;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SelectAll(var Msg: TGridMessage); message GM_SELECTALL;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(Aowner : TComponent); override;
    procedure EditingDone; override;
  public
    procedure ValidateEdit; override;
    property EditMask;
    //property OnEditingDone;
  end;

  { TMemoCellEditor }

  TMemoCellEditor = class(TCustomMemo)
  private
    FGrid: TMyDBGrid;
    FCol,FRow:Integer;
  protected
    procedure WndProc(var TheMessage : TLMessage); override;
    procedure Change; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure msg_SetMask(var Msg: TGridMessage); message GM_SETMASK;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SelectAll(var Msg: TGridMessage); message GM_SELECTALL;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(Aowner : TComponent); override;
    procedure EditingDone; override;
    property OnEditingDone;
  end;

  { TMyDBGrid }

  TMyDBGrid = class(TDBGrid)
  private
    FAlignButtons: TAlignment;
    FHideButtons: Boolean;
    FOnCanSort: TMyGridCanSortEvent;
    FOnSortColumnChange: TNotifyEvent;
    FSortAZ: Boolean;
    FSortCols: TSortColList;
    FSortColumn: Integer;
    FUp, FDown: TBitmap;
    FOnButtonClick: TGridButtonClick;
    FShowButtons: Boolean;
    FButtons: TGridButtons;
    FMemo: TMemoCellEditor;
    FMaskEdit: TMaskCellEditor;
    FWordWrap: Boolean;
    procedure ButtonClick(Sender: TObject; Bn: TGridButtonType);
    function GetButtonFont: TFont;
    function GetSelectedRowCount: Integer;
    function GetVisibleCaptions: TGridButtonSet;
    procedure ReadSortCols(Reader: TReader);
    procedure SetAlignButtons(AValue: TAlignment);
    procedure SetButtonFont(AValue: TFont);
    procedure SetVisibleCaptions(AValue: TGridButtonSet);
    procedure WriteSortCols(Writer: TWriter);
    function GetButtonsColor: TColor;
    function GetButtonSize: Integer;
    function GetFlatButtons: Boolean;
    function GetVisibleButtons: TGridButtonSet;
    procedure SetButtonsColor(AValue: TColor);
    procedure SetButtonSize(AValue: Integer);
    procedure SetFlatButtons(AValue: Boolean);
    procedure SetShowButtons(AValue: Boolean);
    procedure SetVisibleButtons(AValue: TGridButtonSet);
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  protected
    procedure DrawCellText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState;
      aText: String); override;
    procedure DrawFixedText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure BoundsChanged; override;
    procedure SelectEditor; override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure DoOnResize; override;
  protected
    procedure EditorTextChanged(const aCol, aRow: Integer; const aText: string); override;
    function EditorIsReadOnly: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ColumnFromCol(aCol: Integer): TColumn;
    function ColFromColumn(C: TColumn): Integer;
    procedure RefreshGrid;
    property Buttons: TGridButtons read FButtons;
    property SortCols: TSortColList read FSortCols;
    property OnCanSort: TMyGridCanSortEvent read FOnCanSort write FOnCanSort;
    property Editor;
    property OnSortColumnChange: TNotifyEvent read FOnSortColumnChange write
      FOnSortColumnChange;
  public
    procedure PositionButtons;
    procedure MoveToSelectedRow(i: Integer);
    procedure ClearRowsSelection;
    function CurrentRowSelected: Boolean;
    property SelectedRowCount: Integer read GetSelectedRowCount;
    property Row;
  published
    property SelectedColor;
    property GridLineColor;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons;
    property VisibleButtons: TGridButtonSet read GetVisibleButtons write SetVisibleButtons;
    property VisibleCaptions: TGridButtonSet read GetVisibleCaptions write SetVisibleCaptions;
    property FlatButtons: Boolean read GetFlatButtons write SetFlatButtons;
    property ButtonsColor: TColor read GetButtonsColor write SetButtonsColor;
    property ButtonSize: Integer read GetButtonSize write SetButtonSize;
    property ButtonFont: TFont read GetButtonFont write SetButtonFont;
    property AlignmentButtons: TAlignment read FAlignButtons write
      SetAlignButtons;
    property HideButtonsWhenLostFocus: Boolean read FHideButtons
      write FHideButtons;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property OnButtonClick: TGridButtonClick read FOnButtonClick write
      FOnButtonClick;
    property PopupMenu stored False;
    property ParentFont stored False;
  end;

  { TTreeSearchForm }

  TTreeSearchForm = class(TForm)
  private
    FEdit: TEdit;
    FTree: TTreeView;
    FText: String;
    FIndex: Integer;
    FTreeOnSelect: TNotifyEvent;
    procedure EditChange(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoSearch(aDirect: Integer);
  protected
    procedure Deactivate; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    procedure ShowForm(Tree: TTreeView; const Key: String);
  end;

procedure MaskingControl(aOwner: TComponent; aControl: TControl);
function GetBnCaption(Bn: TGridButtonType): String;

implementation

uses
  QuickSearchForm, apputils, dateutils, LazUtf8, Dialogs;

procedure MaskingControl(aOwner: TComponent; aControl: TControl);
begin
  with TShape.Create(aOwner) do
  begin
    Shape:=stRectangle;
    Brush.Style := bsClear;
    Pen.Color:=clGray;
    Parent := aControl.Parent;
    BoundsRect := aControl.BoundsRect;
  end;
  aControl.Visible:=False;
end;

{ TMaskCellEditor }

procedure TMaskCellEditor.WndProc(var TheMessage: TLMessage);
begin
  if FGrid<>nil then
    case TheMessage.Msg of
      LM_CLEAR,
      LM_CUT,
      LM_PASTE:
        begin
          if FGrid.EditorIsReadOnly then
            exit;
        end;
    end;
  inherited WndProc(TheMessage);
end;

procedure TMaskCellEditor.Change;
begin
  inherited Change;
  if (FGrid<>nil) and Visible then begin
    FGrid.EditorTextChanged(FCol, FRow, Text);
  end;
end;

procedure TMaskCellEditor.KeyDown(var Key: Word; Shift: TShiftState);
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
      FGrid.KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := FGrid.FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or FGrid.EditorIsReadOnly then
      Key := 0;
  end;
var
	IntSel: boolean;
begin
  inherited KeyDown(Key,Shift);
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
    else
      doEditorKeyDown;
  end;
inherited KeyDown(Key, Shift);
end;

procedure TMaskCellEditor.msg_SetMask(var Msg: TGridMessage);
begin

end;

procedure TMaskCellEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text:=Msg.Value;
  SelStart := UTF8Length(Text);
end;

procedure TMaskCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col:=FCol;
  Msg.Row:=FRow;
  Msg.Value:=Text;
end;

procedure TMaskCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=TMyDBGrid(Msg.Grid);
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TMaskCellEditor.msg_SelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

procedure TMaskCellEditor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TMaskCellEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TMaskCellEditor.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  AutoSize := False;
  BorderWidth := 0;
end;

procedure TMaskCellEditor.EditingDone;
begin
  inherited EditingDone;
  if FGrid<>nil then
    FGrid.EditingDone;
end;

procedure TMaskCellEditor.ValidateEdit;
begin

end;

{ TMaskEditEx }

procedure TMaskEditEx.ValidateEdit;
begin

end;

function TMaskEditEx.ValidateText: Boolean;
begin
  Result := MaskedTextEmpty(Text, EditMask) or ValidText(Text, EditMask);
end;

function TMaskEditEx.MaskTextEmpty: Boolean;
begin
  Result := MaskedTextEmpty(Text, EditMask);
end;

{ TMemoCellEditor }

procedure TMemoCellEditor.WndProc(var TheMessage: TLMessage);
begin
  if FGrid<>nil then
    case TheMessage.Msg of
      LM_CLEAR,
      LM_CUT,
      LM_PASTE:
        begin
          if FGrid.EditorIsReadOnly then
            exit;
        end;
    end;
  inherited WndProc(TheMessage);
end;

procedure TMemoCellEditor.Change;
begin
  inherited Change;
  if (FGrid<>nil) and Visible then begin
    FGrid.EditorTextChanged(FCol, FRow, Text);
  end;
end;

procedure TMemoCellEditor.KeyDown(var Key: Word; Shift: TShiftState);
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
      FGrid.KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := FGrid.FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or FGrid.EditorIsReadOnly then
      Key := 0;
  end;
var
  IntSel: boolean;
begin
  inherited KeyDown(Key,Shift);
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
    else
      doEditorKeyDown;
  end;
end;

procedure TMemoCellEditor.msg_SetMask(var Msg: TGridMessage);
begin

end;

procedure TMemoCellEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text:=Msg.Value;
  SelStart := UTF8Length(Text);
end;

procedure TMemoCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col:=FCol;
  Msg.Row:=FRow;
  Msg.Value:=Text;
end;

procedure TMemoCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=TMyDBGrid(Msg.Grid);
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TMemoCellEditor.msg_SelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

procedure TMemoCellEditor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TMemoCellEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TMemoCellEditor.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  AutoSize := False;
  ScrollBars:=ssVertical;
end;

procedure TMemoCellEditor.EditingDone;
begin
  inherited EditingDone;
  if FGrid<>nil then
    FGrid.EditingDone;
end;

{ TGridButton }

procedure TGridButton.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; Raw: boolean; WithThemeSpace: boolean);
var
  w, h: Integer;
begin
  w := 0;
  if ShowCaption then
    w := Canvas.GetTextWidth(Caption) + Glyph.Width + 16;
  if w < 25 then w := 25;
  h := Canvas.GetTextHeight('Yy') + 8;
  if h < 25 then h := 25;
  PreferredWidth:=w;
  PreferredHeight:=h;
end;

{ TGridButtons }

procedure TGridButtons.ButtonClick(Sender: TObject);
begin
  if FOnButtonClick <> nil then
    FOnButtonClick(Sender, TGridButtonType(TComponent(Sender).Tag));
end;

procedure TGridButtons.ClearButtons;
begin
  //for i := 0 to FButtons.Count - 1 do
  //  TGridButton(FButtons[i]).Glyph.Free;
  //ClearList(FButtons);
end;

function TGridButtons.GetButtons(Index: TGridButtonType): TGridButton;
begin
  Result := TGridButton(FButtons[Ord(Index)]);
end;

procedure TGridButtons.SetButtonSize(AValue: Integer);
begin
  if FButtonSize=AValue then Exit;
  FButtonSize:=AValue;
  UpdateButtons;
end;

procedure TGridButtons.SetFlat(AValue: Boolean);
begin
  if FFlat=AValue then Exit;
  FFlat:=AValue;
  UpdateButtons;
end;

procedure TGridButtons.SetVisibleButtons(AValue: TGridButtonSet);
begin
  if FVisibleButtons=AValue then Exit;
  FVisibleButtons:=AValue;
  UpdateButtons;
end;

procedure TGridButtons.SetVisibleCaptions(AValue: TGridButtonSet);
begin
  if FVisibleCaptions=AValue then Exit;
  FVisibleCaptions:=AValue;
  UpdateButtons;
end;

procedure TGridButtons.CMParentColorChanged(var Message: TLMessage);
begin
  if ParentColor then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  inherited;
end;

function GetBnCaption(Bn: TGridButtonType): String;
var
  S: String;
begin
  S := '';
  case Bn of
    gbnAppend: S := rsAppend;
    gbnEdit: S := rsEdit;
    gbnDelete: S := rsDelete;
    gbnDuplicate: S := rsDuplicate;
    gbnShopping: S := rsShopping;
    gbnMoveUp: S := rsMoveUp;
    gbnMoveDown: S := rsMoveDown;
    gbnRefresh: S := rsRefresh;
    gbnGoto: S := rsGoTo;
  end;
  Result := S;
end;

constructor TGridButtons.Create(AOwner: TComponent);
var
  i: TGridButtonType;
  B: TGridButton;
begin
  inherited Create(AOwner);
  ParentColor := False;
  ControlStyle := ControlStyle + [csNoDesignSelectable];
  AutoSize := True;
  ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
  Color := clBtnFace;
  FButtons := TList.Create;
  FButtonSize := 25;
  FFlat := True;
  for i := gbnAppend to gbnGoto do
  begin
    B := TGridButton.Create(Self);
    B.Constraints.MinHeight:=FButtonSize;
    B.Constraints.MinWidth:=FButtonSize;
    B.AutoSize:=True;
    B.Caption := '';
    B.Hint:=GetBnCaption(i);
    B.ShowHint:=True;
    B.ControlStyle:=B.ControlStyle + [csNoDesignSelectable];
    B.Parent := Self;
    B.Tag := Ord(i);
    B.Transparent:=True;
    B.Caption:=GetBnCaption(i);
    B.OnClick:=@ButtonClick;
    FButtons.Add(B);
  end;
end;

destructor TGridButtons.Destroy;
begin
  //ClearButtons;
  FButtons.Free;
  inherited Destroy;
end;

procedure TGridButtons.UpdateButtons;
const
  ImgRes: array [TGridButtonType] of String = ('add16', 'edit16', 'delete16',
    'copy16', 'shopping16', 'up16', 'down16', 'refresh16', 'goto16');
var
  i: TGridButtonType;
  B: TGridButton;
begin
  if (csLoading in ComponentState) or (csReading in ComponentState) then Exit;
  for i := gbnAppend to gbnGoto do
  begin
    B := TGridButton(FButtons[Ord(i)]);
    if i in FVisibleButtons then
    begin
      B.Visible := True;
      B.ControlStyle:=B.ControlStyle - [csNoDesignVisible];
      B.ShowCaption := i in FVisibleCaptions;
      B.ShowHint:=not B.ShowCaption;
      B.LoadGlyphFromLazarusResource(ImgRes[i]);
    end
    else
    begin
      B.Visible := False;
      B.ControlStyle:=B.ControlStyle + [csNoDesignVisible];
      B.Glyph.Clear;
    end;
    B.Flat := FFlat;
    B.Constraints.MinHeight:=FButtonSize;
    B.Constraints.MinWidth:=FButtonSize;
  end;
  AdjustSize;
end;

procedure TGridButtons.ShowButton(Bn: TGridButtonType; Value: Boolean);
begin
  if Bn in FVisibleButtons then
    TGridButton(FButtons[Ord(Bn)]).Visible := Value;
end;

procedure TGridButtons.EnableButton(Bn: TGridButtonType; Value: Boolean);
begin
  TGridButton(FButtons[Ord(Bn)]).Enabled:=Value;
end;

{ TTreeSearchForm }

procedure TTreeSearchForm.EditChange(Sender: TObject);
begin
  FText := Utf8LowerCase(FEdit.Text);
  FIndex := 0;
  DoSearch(1);
end;

procedure TTreeSearchForm.EditEnter(Sender: TObject);
begin
  FEdit.SelStart := Utf8Length(FEdit.Text);
end;

procedure TTreeSearchForm.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DOWN then DoSearch(1)
  else if Key = VK_UP then DoSearch(-1)
  else if Key in [VK_ESCAPE, VK_RETURN] then Close
  else Exit;
  Key := 0;
end;

procedure TTreeSearchForm.DoSearch(aDirect: Integer);
var
  i, OldI: Integer;
  S: String;
begin
  i := FIndex + aDirect;
  OldI := FIndex;
  while True do
  begin
    if (i > FTree.Items.Count - 1) or (i < 0) then Break;
    S := Utf8LowerCase(FTree.Items[i].Text);
    if Utf8Pos(FText, S) > 0 then
    begin
      FTree.ClearSelection;
      FTree.Selected:=FTree.Items[i];
      FIndex := i;
      Exit;
    end;
    i := i + aDirect;
  end;
  FIndex := OldI;
end;

procedure TTreeSearchForm.Deactivate;
begin
  inherited Deactivate;
  Close;
end;

procedure TTreeSearchForm.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  CloseAction := caFree;
  FTree.OnSelectionChanged:=FTreeOnSelect;
  FTreeOnSelect(FTree);
end;

constructor TTreeSearchForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  BorderStyle:=bsNone;
  FEdit := TEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.Align := alClient;
  FEdit.OnChange:=@EditChange;
  FEdit.OnEnter:=@EditEnter;
  FEdit.OnKeyDown:=@EditKeyDown;
end;

procedure TTreeSearchForm.ShowForm(Tree: TTreeView; const Key: String);
var
  P: types.TPoint;
begin
  FTree := Tree;
  FTreeOnSelect := FTree.OnSelectionChanged;
  FTree.OnSelectionChanged:=nil;
  P := FTree.Parent.ClientToScreen(Point(FTree.Left, FTree.Height));
  Left := P.x; Top := P.y - FEdit.Height;
  Width := FTree.Width;
  FEdit.Text := Key;
  FEdit.SelStart := Utf8Length(Key);
  Height := FEdit.Height;
  Show;
end;

{ TMyDBGrid }

procedure TMyDBGrid.ButtonClick(Sender: TObject; Bn: TGridButtonType);
begin
  if FOnButtonClick <> nil then
    FOnButtonClick(Self, Bn);
end;

function TMyDBGrid.GetButtonFont: TFont;
begin
  Result := FButtons.Font;
end;

function TMyDBGrid.GetSelectedRowCount: Integer;
begin
  Result := SelectedRows.Count;
end;

function TMyDBGrid.GetVisibleCaptions: TGridButtonSet;
begin
  Result := FButtons.VisibleCaptions;
end;

procedure TMyDBGrid.ReadSortCols(Reader: TReader);
var
  S: String;
  SL: TStringList;
  i, p, idx: Integer;
begin
  FSortCols.Clear;
  S := Reader.ReadString;
  SL := TStringList.Create;
  SplitStr(S, '|', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    p := Pos(';', S);
    idx := StrToInt(Copy(S, 1, p - 1));
    if (idx >= 0) and (idx < Columns.Count) then
      FSortCols.AddCol(Columns[idx], Str2Bool(Copy(S, p + 1, 255)));
  end;
  SL.Free;
end;

procedure TMyDBGrid.SetAlignButtons(AValue: TAlignment);
begin
  if FAlignButtons=AValue then Exit;
  FAlignButtons:=AValue;
  PositionButtons;
end;

procedure TMyDBGrid.SetButtonFont(AValue: TFont);
begin
  FButtons.Font := AValue;
  FButtons.ParentFont:=FButtons.Font.IsDefault;
end;

procedure TMyDBGrid.SetVisibleCaptions(AValue: TGridButtonSet);
begin
  FButtons.VisibleCaptions:=AValue;
end;

procedure TMyDBGrid.WriteSortCols(Writer: TWriter);
var
  S: String;
  i: Integer;
  CD: TSortColData;
begin
  S := '';
  for i := 0 to FSortCols.Count - 1 do
  begin
    CD := FSortCols[i];
    S := S + IntToStr(CD.Col.Index) + ';' + Bool2Str(CD.Desc);
    if i < FSortCols.Count - 1 then S := S + '|';
  end;
  Writer.WriteString(S);
end;

procedure TMyDBGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  X, Y: Integer;
  CD: TSortColData;
  i, num: Integer;
  C: TColumn;

  procedure DrawArrowUp(X1, Y1: Integer);
  begin
    Canvas.Draw(X, Y, FUp);
  end;

  procedure DrawArrowDown(X1, Y1: Integer);
  begin
    Canvas.Draw(X, Y, FDown);
  end;

begin
  inherited DrawCell(aCol, aRow, aRect, aState);
  if (gdFixed in aState) and (aRow = 0) and (aCol > 0) then
  begin
    C := ColumnFromCol(aCol);
    CD := FSortCols.FindCol(C);
    if CD <> nil then
    begin
      X := aRect.Right - 16;
      Y := aRect.Top + ((aRect.Bottom - aRect.Top) div 2) - 8;
      num := FSortCols.IndexOf(CD);
      for i := 0 to num do
      begin
        if CD.Desc then DrawArrowDown(X, Y)
        else DrawArrowUp(X, Y);
        X := X - 10;
      end;
    end;
  end;
end;

procedure TMyDBGrid.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited UTF8KeyPress(UTF8Key);
  if ReadOnly and (Utf8Key >= ' ') then
    QuickSearchFrm.ShowForm(Utf8Key, Self);
end;

procedure TMyDBGrid.HeaderClick(IsColumn: Boolean; index: Integer);
var
  CD: TSortColData;
  C: TColumn;
  Cancel: Boolean;
begin
  inherited HeaderClick(IsColumn, index);
  if (not IsColumn) or (index = 0) then Exit;
  if (FOnSortColumnChange = nil) or (FOnCanSort = nil) then Exit;
  Cancel := False;
  FOnCanSort(Self, index, Cancel);
  if Cancel then Exit;
  C := ColumnFromCol(index);
  if ssCtrl in GetKeyShiftState then
  begin
    CD := FSortCols.FindCol(C);
    if CD = nil then
    begin
      CD := FSortCols.AddCol(C, False);
      InvalidateCol(index);
    end
    else
    begin
      if not CD.Desc then
      begin
        CD.Desc := True;
        InvalidateCol(index);
      end
      else
      begin
        FSortCols.RemoveCol(CD);
        InvalidateRow(0);
      end;
    end;
  end
  else
  begin
    CD := FSortCols.FindCol(C);
    if (CD = nil) or (FSortCols.Count > 1) then
    begin
      FSortCols.Clear;
      CD := FSortCols.AddCol(C, False);
      InvalidateRow(0);
    end
    else if CD <> nil then
    begin
      if not CD.Desc then
        CD.Desc := True
      else
        FSortCols.RemoveCol(CD);
      InvalidateCol(index);
    end;
  end;
  FOnSortColumnChange(Self);
end;

procedure TMyDBGrid.Loaded;
begin
  inherited Loaded;
  Options := Options + [dgHeaderHotTracking, dgHeaderPushedLook];
  PositionButtons;
  FButtons.Visible:=not FHideButtons;
end;

procedure TMyDBGrid.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('SortCols', @ReadSortCols, @WriteSortCols, FSortCols.Count > 0);
end;

constructor TMyDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := Options + [dgAnyButtonCanSelect, dgDisableInsert, dgDisableDelete,
    dgHeaderPushedLook, dgHeaderHotTracking];
  TitleStyle:=tsLazarus;
  ParentFont := False;
  FocusColor := SelectedColor;
  FSortCols := TSortColList.Create;
  FUp := LoadBitmapFromLazarusResource('up8');
  FDown := LoadBitmapFromLazarusResource('down8');
  FButtons := TGridButtons.Create(Self);
  FButtons.OnButtonClick:=@ButtonClick;
  FMemo := TMemoCellEditor.Create(nil);
end;

destructor TMyDBGrid.Destroy;
begin
  FMemo.Free;
  FreeAndNil(FUp);
  FreeAndNil(FDown);
  FSortCols.Free;
  inherited Destroy;
end;

function TMyDBGrid.ColumnFromCol(aCol: Integer): TColumn;
var
  i: Integer;
begin
  Result := nil;
  i := ColumnIndexFromGridColumn(aCol);
  if (i >= 0) and (i < Columns.Count) then
    Result := Columns[i];
end;

function TMyDBGrid.ColFromColumn(C: TColumn): Integer;
begin
  Result := 0;
  if C <> nil then
    Result := GridColumnFromColumnIndex(C.Index);
end;

procedure TMyDBGrid.RefreshGrid;
begin
  LayoutChanged;
end;

procedure TMyDBGrid.MoveToSelectedRow(i: Integer);
begin
  DataSource.DataSet.GotoBookmark(SelectedRows.Items[i]);
end;

procedure TMyDBGrid.ClearRowsSelection;
begin
  SelectedRows.Clear;
end;

function TMyDBGrid.CurrentRowSelected: Boolean;
begin
  Result := SelectedRows.CurrentRowSelected;
end;

function TMyDBGrid.GetButtonSize: Integer;
begin
  Result := FButtons.ButtonSize;
end;

function TMyDBGrid.GetButtonsColor: TColor;
begin
  Result := FButtons.Color;
end;

function TMyDBGrid.GetFlatButtons: Boolean;
begin
  Result := FButtons.Flat;
end;

function TMyDBGrid.GetVisibleButtons: TGridButtonSet;
begin
  Result := FButtons.VisibleButtons;
end;

procedure TMyDBGrid.SetButtonsColor(AValue: TColor);
begin
  FButtons.Color := AValue;
  FButtons.ParentColor := ((Parent <> nil) and (Parent.Color = AValue));
end;

procedure TMyDBGrid.SetButtonSize(AValue: Integer);
begin
  FButtons.ButtonSize:=AValue;
  PositionButtons;
end;

procedure TMyDBGrid.SetFlatButtons(AValue: Boolean);
begin
  FButtons.Flat:=AValue;
end;

procedure TMyDBGrid.SetShowButtons(AValue: Boolean);
begin
  FShowButtons:=AValue;
  FButtons.Visible:=AValue;
  if FShowButtons then
  begin
    FButtons.ControlStyle:=FButtons.ControlStyle - [csNoDesignVisible];
    FButtons.Parent := Parent;
  end
  else
  begin
    FButtons.ControlStyle:=FButtons.ControlStyle + [csNoDesignVisible];
    FButtons.Parent := nil;
  end;
end;

procedure TMyDBGrid.SetVisibleButtons(AValue: TGridButtonSet);
begin
  FButtons.VisibleButtons:=AValue;
end;

procedure TMyDBGrid.PositionButtons;
begin
  if Parent = nil then Exit;

  if FAlignButtons = taLeftJustify then
  begin
    FButtons.AnchorSideLeft.Control := Self;
    FButtons.AnchorSideRight.Control := Self;
    FButtons.AnchorSideRight.Side := asrBottom;
    FButtons.AnchorSideBottom.Control := Self;
    FButtons.Anchors:=[akLeft, akBottom];
  end
  else
  begin
    FButtons.AnchorSideRight.Control := Self;
    FButtons.AnchorSideRight.Side := asrBottom;
    FButtons.AnchorSideBottom.Control := Self;
    FButtons.Anchors:=[akRight, akBottom];
  end;
end;

procedure TMyDBGrid.WMSetFocus(var Message: TLMSetFocus);
begin
  FButtons.Visible:=True;
  inherited;
end;

procedure TMyDBGrid.WMKillFocus(var Message: TLMKillFocus);
begin
  FButtons.Visible:=not FHideButtons;
  inherited;
end;

procedure TMyDBGrid.DrawCellText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState; aText: String);
var
  TS: TTextStyle;
  C: TColumn;
begin
  C := TColumn(ColumnFromGridColumn(aCol));
  if C <> nil then
  begin
    if (aState * [gdFixed] = []) and (C.Field is TMemoField) then aText := C.Field.AsString;
  end;
  if FWordWrap then
  begin
    TS := Canvas.TextStyle;
    TS.Wordbreak:=True;
    TS.SingleLine:=False;
    TS.Layout:=tlTop;
    Canvas.TextStyle := TS;
  end;
  inherited DrawCellText(aCol, aRow, aRect, aState, aText);
end;

procedure TMyDBGrid.DrawFixedText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  TS: TTextStyle;
begin
  if FWordWrap then
  begin
    TS := Canvas.TextStyle;
    TS.Wordbreak:=True;
    TS.SingleLine:=False;
    TS.Layout:=tlTop;
    Canvas.TextStyle := TS;
  end;
  inherited DrawFixedText(aCol, aRow, aRect, aState);
end;

procedure TMyDBGrid.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if FButtons = nil then Exit;
  FButtons.Parent := NewParent;
  PositionButtons;
  SetShowButtons(FShowButtons);
end;

procedure TMyDBGrid.BoundsChanged;
begin
  inherited BoundsChanged;
  PositionButtons;
end;

procedure TMyDBGrid.SelectEditor;
var
  H: Integer;
  F: TFont;
begin
  inherited SelectEditor;
  if Parent = nil then Exit;
  if (DataLink <> nil) and (Datalink.Active) and (Editor <> nil) and
    (not (Editor is TPickListCellEditor)) and (SelectedField is TStringField) then
  begin
    F := Canvas.Font;
    Canvas.Font := Font;
    try
      H := Canvas.TextHeight('Yy');
    except
      H := 0;
    end;
    Canvas.Font := F;
    if DefaultRowHeight > H + H then Editor := FMemo;
  end;
end;

procedure TMyDBGrid.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  FButtons.Visible:=Value;
end;

procedure TMyDBGrid.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  FButtons.Enabled:=Value;
end;

procedure TMyDBGrid.DoOnResize;
begin
  inherited DoOnResize;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) and
  	(DataSource.DataSet.Active) then
	  DataSource.DataSet.UpdateCursorPos;
end;

procedure TMyDBGrid.EditorTextChanged(const aCol, aRow: Integer;
  const aText: string);
begin
  inherited EditorTextChanged(aCol, aRow, aText);
end;

function TMyDBGrid.EditorIsReadOnly: boolean;
begin
  Result:=inherited EditorIsReadOnly;
end;

{ TMyGrid }

procedure TMyGrid.DrawCellText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState; aText: String);
var
  TS: TTextStyle;
begin
  if FWordWrap then
  begin
    TS := Canvas.TextStyle;
    TS.Wordbreak:=True;
    TS.SingleLine:=False;
    TS.Layout:=tlTop;
    Canvas.TextStyle := TS;
  end;
  inherited DrawCellText(aCol, aRow, aRect, aState, aText);
end;

procedure TMyGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  X, Y: Integer;
  CD: TSortColData;
  i, num: Integer;
  C: TGridColumn;

  procedure DrawArrowUp(X1, Y1: Integer);
  begin
    Canvas.Draw(X, Y, FUp);
  end;

  procedure DrawArrowDown(X1, Y1: Integer);
  begin
    Canvas.Draw(X, Y, FDown);
  end;

begin
  inherited DrawCell(aCol, aRow, aRect, aState);

  if (gdFixed in aState) and (aRow = 0) and (aCol > 0) then
  begin
    C := Columns[ColumnIndexFromGridColumn(aCol)];
    CD := FSortCols.FindCol(C);
    if CD <> nil then
    begin
      X := aRect.Right - 16;
      Y := aRect.Top + ((aRect.Bottom - aRect.Top) div 2) - 8;
      num := FSortCols.IndexOf(CD);
      for i := 0 to num do
      begin
        if CD.Desc then DrawArrowDown(X, Y)
        else DrawArrowUp(X, Y);
        X := X - 10;
      end;
    end;
  end;
end;

procedure TMyGrid.HeaderClick(IsColumn: Boolean; index: Integer);
var
  CD: TSortColData;
  C: TGridColumn;
  Cancel: Boolean;
begin
  inherited HeaderClick(IsColumn, index);
  if (not IsColumn) or (index = 0) then Exit;

  if FOnCanSort <> nil then
  begin
    Cancel := False;
    FOnCanSort(Self, index, Cancel);
    if Cancel then Exit;
  end;
  C := Columns[ColumnIndexFromGridColumn(index)];
  if ssCtrl in GetKeyShiftState then
  begin
    CD := FSortCols.FindCol(C);
    if CD = nil then
    begin
      CD := FSortCols.AddCol(C, False);
      InvalidateCol(index);
    end
    else
    begin
      if not CD.Desc then
      begin
        CD.Desc := True;
        InvalidateCol(index);
      end
      else
      begin
        FSortCols.RemoveCol(CD);
        InvalidateRow(0);
      end;
    end;
  end
  else
  begin
    CD := FSortCols.FindCol(C);
    if (CD = nil) or (FSortCols.Count > 1) then
    begin
      FSortCols.Clear;
      CD := FSortCols.AddCol(C, False);
      InvalidateRow(0);
    end
    else if CD <> nil then
    begin
      if not CD.Desc then
        CD.Desc := True
      else
        FSortCols.RemoveCol(CD);
      InvalidateCol(index);
    end;
  end;
end;

constructor TMyGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortCols := TSortColList.Create;
  FUp := LoadBitmapFromLazarusResource('up8');
  FDown := LoadBitmapFromLazarusResource('down8');
  Options:=Options - [goRangeSelect] + [goHeaderHotTracking, goHeaderPushedLook,
    goEditing, goDrawFocusSelected, goSelectionActive, goColMoving,
    goColSizing, goThumbTracking];
end;

destructor TMyGrid.Destroy;
begin
  FreeAndNil(FUp);
  FreeAndNil(FDown);
  FSortCols.Free;
  inherited Destroy;
end;

{ TFilterCheckBox }

function TFilterCheckBox.GetValue: Integer;
begin
  //Result := 0;
  //if Checked then Result := 1;
  Result := Integer(State);
end;

procedure TFilterCheckBox.SetValue(AValue: Integer);
begin
  //Checked := AValue = 1;
  if AValue in [0..2] then
    State := TCheckBoxState(AValue)
  else State := cbGrayed;
end;

constructor TFilterCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.AllowGrayed:=True;
end;

function TFilterCheckBox.AsSQL(const FieldName: String): String;
begin
  case State of
    cbChecked: Result := FieldName + '=1';
    cbUnchecked: Result := FieldName + '=0';
    cbGrayed: Result := FieldName + ' is null';
  end;
end;

{ TFilterDate }

constructor TFilterDate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Button.LoadGlyphFromLazarusResource('date16');
  Flat := True;
end;

function TFilterDate.Validate: Boolean;
var
  D: TDateTime;
begin
  Result := TryStrToDate(StringReplace(Text, ' ', DefaultFormatSettings.DateSeparator,
    [rfReplaceAll]), D);
  if Result then
    Text := DateToStr(D);
end;

{ TFilterPeriod }

procedure TFilterPeriod.EditorEditingDone(Sender: TObject);
begin
  if (FBegin.Text <> FBeginText) or (FEnd.Text <> FEndText) then FPeriod := ptNone;
  EditingDone;
end;

procedure TFilterPeriod.MenuPopup(Sender: TObject);
begin
  FPopup.Items[2].Checked := FPeriod = ptToday;
  FPopup.Items[3].Checked := FPeriod = ptThisWeek;
  FPopup.Items[4].Checked := FPeriod = ptThisMonth;
  FPopup.Items[5].Checked := FPeriod = ptThisYear;
end;

procedure TFilterPeriod.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0:
      begin
        FBegin.Text := '';
        FEnd.Text := '';
      end;
    1: Period := ptToday;
    2: Period := ptThisWeek;
    3: Period := ptThisMonth;
    4: Period := ptThisYear;
    {1:
      begin
        FBegin.Date := Date;
        FEnd.Date := Date
      end;
    2:
      begin
        FBegin.Date := IncDay(Date, -DayOfTheWeek(Date)+1);
        FEnd.Date := Date;
      end;
    3:
      begin
        FBegin.Date := IncDay(Date, -DayOf(Date)+1);
        FEnd.Date := Date;
      end;
    4:
      begin
        FBegin.Date := EncodeDate(YearOf(Date), 1, 1);
        FEnd.Date := Date;
      end;}
  end;
end;

procedure TFilterPeriod.SetPeriod(AValue: TPeriodType);
begin
  FPeriod:=AValue;
  if AValue <> ptNone then FEnd.Date := Date;
  case AValue of
    ptToday: FBegin.Date := Date;
    ptThisWeek: FBegin.Date := IncDay(Date, -DayOfTheWeek(Date)+1);
    ptThisMonth: FBegin.Date := IncDay(Date, -DayOf(Date)+1);
    ptThisYear: FBegin.Date := EncodeDate(YearOf(Date), 1, 1);
  end;
  FBeginText := FBegin.Text;
  FEndText := FEnd.Text;
end;

procedure TFilterPeriod.DoOnResize;
begin
  inherited DoOnResize;
  FBegin.Width := ClientWidth div 2  - 2;
  FEnd.Left := ClientWidth div 2 + 2;
  FEnd.Width := FBegin.Width;
end;

constructor TFilterPeriod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopup := TPopupMenu.Create(Self);
  FPopup.Items.Add( CreateMenuItem(FPopup, rsClear, 0, 0, @MenuClick, 'delete16') );
  FPopup.Items.Add( CreateMenuItem(FPopup, '-', 0, 0, nil, '') );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsToday, 1, 0, @MenuClick, '') );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsThisWeek, 2, 0, @MenuClick, '') );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsThisMonth, 3, 0, @MenuClick, '') );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsThisYear, 4, 0, @MenuClick, '') );
  FPopup.OnPopup:=@MenuPopup;
  PopupMenu := FPopup;
  BevelOuter := bvNone;
  Caption := '';
  FBegin := TFilterDate.Create(Self);
  FBegin.Parent := Self;
  FBegin.OnEditingDone:=@EditorEditingDone;
  FEnd := TFilterDate.Create(Self);
  FEnd.Parent := Self;
  FEnd.OnEditingDone:=@EditorEditingDone;
  ClientHeight := FEnd.Height;
end;

procedure TFilterPeriod.SetFocus;
begin
  inherited SetFocus;
  FBegin.SetFocus;
end;

function TFilterPeriod.AsSQL(const FieldName: String): String;
begin
  Result := '';
  if FBegin.Text <> '' then
    Result := Result + FieldName + '>=''' + FBegin.Text + ''' and ';
  if FEnd.Text <> '' then
    Result := Result + FieldName + '<=''' + FEnd.Text + ''' and ';
  if (FBegin.Text = '') and (FEnd.Text = '') then
    Result := FieldName + ' is null'
  else
    Result := Copy(Result, 1, Length(Result) - 5);
end;

function TFilterPeriod.Validate: Boolean;
begin
  Result := ((FBegin.Text = '') or FBegin.Validate) and
    ((FEnd.Text = '') or FEnd.Validate);
end;

function TFilterPeriod.Focused: Boolean;
begin
  Result := FBegin.Focused or FEnd.Focused;
end;

{ TFilterEdit }

procedure TFilterEdit.KeyPress(var Key: char);
begin
  if FNumeric and (not (Key in ['0'..'9', ',', '-', '+', #8])) then Key := #0;
  inherited KeyPress(Key);
end;

constructor TFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TFilterEdit.AsSQL(const FieldName: String): String;
begin
  if not Numeric then
  begin
    if Text <> '' then
      Result := FieldName + ' containing ''' + Text + ''''
    else
      Result := FieldName + ' is null ';//'=''' + Text + ''''
  end
  else
    Result := FieldName + '=' + Text;
end;

function TFilterEdit.Validate: Boolean;
var
  E: Extended;
  //FS: TFormatSettings;
begin
  Result := True;
  if FNumeric then
  begin
    //FS := DefaultFormatSettings;
    //FS.DecimalSeparator:='.';
    Result := TryStrToFloat(Text, E);
  end;
end;

{ TFilterRange }

procedure TFilterRange.EditorEditingDone(Sender: TObject);
begin
  EditingDone;
end;

procedure TFilterRange.DoOnResize;
begin
  inherited DoOnResize;
  FBegin.Width := ClientWidth div 2 - 2;
  FEnd.Left := ClientWidth div 2 + 2;
  FEnd.Width := FBegin.Width;
end;

constructor TFilterRange.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Caption := '';
  FBegin := TFilterEdit.Create(Self);
  FBegin.Parent := Self;
  FBegin.Numeric:=True;
  FBegin.OnEditingDone:=@EditorEditingDone;
  FEnd := TFilterEdit.Create(Self);
  FEnd.Parent := Self;
  FEnd.Numeric:=True;
  FEnd.OnEditingDone:=@EditorEditingDone;
  ClientHeight := FEnd.Height;
end;

procedure TFilterRange.SetFocus;
begin
  inherited SetFocus;
  FBegin.SetFocus;
end;

function TFilterRange.AsSQL(const FieldName: String): String;
begin
  Result := '';
  if FBegin.Text <> '' then
    Result := Result + FieldName + '>=' + FBegin.Text + ' and ';
  if FEnd.Text <> '' then
    Result := Result + FieldName + '<=' + FEnd.Text + ' and ';
  if (FBegin.Text = '') and (FEnd.Text = '') then
    Result := FieldName + ' is null'
  else
    Result := Copy(Result, 1, Length(Result) - 5);
end;

function TFilterRange.Validate: Boolean;
begin
  Result := ((FBegin.Text = '') or FBegin.Validate) and
    ((FEnd.Text = '') or FEnd.Validate);
end;

function TFilterRange.Focused: Boolean;
begin
  //Result:=inherited Focused;
  Result := FBegin.Focused or FEnd.Focused;
end;

{ TFilterComboBox }

procedure TFilterComboBox.ClearKeys;
begin
  while FKeys.Count > 0 do
  begin
    Dispose(PInteger(FKeys[0]));
    FKeys.Delete(0);
  end;
end;

function TFilterComboBox.GetKey: Integer;
begin
  Result := 0;
  if ItemIndex >= 0 then
    Result := PInteger(FKeys[ItemIndex])^;
end;

function FindKey(L: TList; K: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to L.Count - 1 do
    if PInteger(L[i])^ = K then
      Exit(i);
end;

procedure TFilterComboBox.SetKey(AValue: Integer);
begin
  ItemIndex := FindKey(FKeys, AValue);
end;

constructor TFilterComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeys := TList.Create;
end;

destructor TFilterComboBox.Destroy;
begin
  ClearKeys;
  FKeys.Free;
  inherited Destroy;
end;

procedure TFilterComboBox.EditingDone;
begin
  inherited EditingDone;
  if ItemIndex >= 0 then
    Text := Items[ItemIndex]
  else
    Text := '';
end;

procedure TFilterComboBox.Clear;
begin
  inherited Clear;
  ClearKeys;
end;

function TFilterComboBox.AddKey(K: Integer): Integer;
var
  pK: PInteger;
begin
  New(pK);
  pK^ := K;
  Result := FKeys.Add(pK);
end;

function TFilterComboBox.AsSQL(const FieldName: String): String;
begin
  if FLookup then
  begin
    if ItemIndex >= 0 then
      Result := FieldName + '=' + IntToStr(Key)
    else
      Result := FieldName + ' is null';
  end
  else
    if Text <> '' then
      Result := FieldName + ' containing ''' + Text + ''''
    else
      Result := FieldName + ' is null';
end;

end.

