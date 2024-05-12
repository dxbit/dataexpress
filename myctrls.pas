unit MyCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, StdCtrls, ExtCtrls, EditBtn, Controls, Grids, lists,
  DBGrids, Graphics, LclType, Menus, strconsts, db, Forms, ComCtrls,
  Buttons, LMessages, maskedit, IpHtml, IpFileBroker, InterfaceBase;

type

  //TMyUtf8KeyPressEvent = procedure (Sender: TObject; var Utf8Key: String) of object;

  { TMyGrid }

  TMyGridCanSortEvent = procedure (Sender: TObject; Index: Integer;
    var Cancel: Boolean) of object;

  TGetCellTextEvent = procedure (Sender: TObject; aCol, aRow: Integer; var aText: String) of object;

  TMyGridColumnDataType = (cdtText, cdtNumber, cdtDate, cdtTime, cdtBool, cdtImage);

  { TMyGridColumn }

  TMyGridColumn = class(TGridColumn)
  private
    FAutoAlignment: Boolean;
    FAutoLayout: Boolean;
    FDataType: TMyGridColumnDataType;
    procedure SetAutoAlignment(AValue: Boolean);
    procedure SetAutoLayout(AValue: Boolean);
  protected
    function GetDefaultAlignment: TAlignment; override;
    function GetDefaultLayout: TTextLayout; override;
  public
    constructor Create(ACollection: TCollection); override;
    property DataType: TMyGridColumnDataType read FDataType write FDataType;
    procedure UpdateAutoLayout;
  published
    property AutoAlignment: Boolean read FAutoAlignment write SetAutoAlignment;
    property AutoLayout: Boolean read FAutoLayout write SetAutoLayout;
  end;

  { TMyGridColumns }

  TMyGridColumns = class(TGridColumns)
  private
    function GetItems(Index: Integer): TMyGridColumn;
    procedure SetItems(Index: Integer; AValue: TMyGridColumn);
  public
    property Items[Index: Integer]: TMyGridColumn read GetItems write SetItems; default;
    function Add: TMyGridColumn;
  end;

  TMyGrid = class(TStringGrid)
  private
    FInactiveSelectedColor: TColor;
    FInactiveSelectedTextColor: TColor;
    FOnCanSort: TMyGridCanSortEvent;
    FOnGetCellText: TGetCellTextEvent;
    FSelectedTextColor: TColor;
    FSortCols: TSortColumns;
    FUp, FDown: TBitmap;
    FBorderLinePos: Integer;
    FWordWrap: Boolean;
    function GetColumns: TMyGridColumns;
    function GetIndicator: Boolean;
    procedure SetColumns(AValue: TMyGridColumns);
    procedure SetInactiveSelectedColor(AValue: TColor);
    procedure SetInactiveSelectedTextColor(AValue: TColor);
    procedure SetIndicator(AValue: Boolean);
    procedure SetSelectedTextColor(AValue: TColor);
    procedure SetWordWrap(AValue: Boolean);
  protected
    function CreateColumns: TGridColumns; override;
    procedure DrawCellText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState;
      aText: String); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    procedure PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SortCols: TSortColumns read FSortCols;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property SelectedTextColor: TColor read FSelectedTextColor write
      SetSelectedTextColor;
    property InactiveSelectedColor: TColor read FInactiveSelectedColor write
    	SetInactiveSelectedColor;
    property InactiveSelectedTextColor: TColor read FInactiveSelectedTextColor write
      SetInactiveSelectedTextColor;
    property Indicator: Boolean read GetIndicator write SetIndicator;
    property BorderLinePos: Integer read FBorderLinePos write FBorderLinePos;
    property OnCanSort: TMyGridCanSortEvent read FOnCanSort write FOnCanSort;
    property OnGetCellText: TGetCellTextEvent read FOnGetCellText write FOnGetCellText;
  published
    property Columns: TMyGridColumns read GetColumns write SetColumns;
  end;

  { TListGrid }

  TListGrid = class(TStringGrid)
  protected
    function GetDefaultRowHeight: integer; override;
    procedure WMVScroll(var message: TLMVScroll); message LM_VSCROLL;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DrawText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState;
      aText: String);
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
  protected
    procedure msg_SetMask(var Msg: TGridMessage); message GM_SETMASK;
    //procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
  public
    procedure ValidateEdit; override;
    property EditMask;
    property Grid: TMyDBGrid read FGrid write FGrid;
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

  { TMyDBGridColumn }

  TMyDBGridColumn = class(TColumn)
  private
    FAutoAlignment: Boolean;
    FAutoLayout: Boolean;
    function IsAlignmentStored: Boolean;
    function IsLayoutStored: Boolean;
    procedure SetAutoAlignment(AValue: Boolean);
    procedure SetAutoLayout(AValue: Boolean);
  protected
    function GetDefaultAlignment: TAlignment; override;
    function GetDefaultLayout: TTextLayout; override;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property AutoAlignment: Boolean read FAutoAlignment write SetAutoAlignment default True;
    property AutoLayout: Boolean read FAutoLayout write SetAutoLayout default True;
    property Alignment stored IsAlignmentStored;
    property Layout stored IsLayoutStored;
  end;

  { TMyDBGridColumns }

  TMyDBGridColumns = class(TDBGridColumns)
  private
    function GetItems(Index: Integer): TMyDBGridColumn;
    procedure SetItems(Index: Integer; AValue: TMyDBGridColumn);
  public
    property Items[Index: Integer]: TMyDBGridColumn read GetItems write SetItems; default;
    function Add: TMyDBGridColumn;
  end;

  { TMyDBGrid }

  TMyDBGrid = class(TDBGrid)
  private
    FAllowChangeSort: Boolean;
    FInactiveSelectedTextColor: TColor;
    FAlignButtons: TAlignment;
    FHideButtons: Boolean;
    FInactiveSelectedColor: TColor;
    FOnCanSort: TMyGridCanSortEvent;
    FOnSortColumnChange: TNotifyEvent;
    FSelectedTextColor: TColor;
    FSortAZ: Boolean;
    FSortCols: TSortColumns;
    FSortColumn: Integer;
    FStopTab: Boolean;
    FUp, FDown: TBitmap;
    FOnButtonClick: TGridButtonClick;
    FShowButtons: Boolean;
    FButtons: TGridButtons;
    FMemo: TMemoCellEditor;
    //FMaskEdit: TMaskCellEditor;
    FWordWrap: Boolean;
    procedure ButtonClick(Sender: TObject; Bn: TGridButtonType);
    function GetButtonFont: TFont;
    function GetColumns: TMyDBGridColumns;
    function GetSelectedRowCount: Integer;
    function GetVisibleCaptions: TGridButtonSet;
    procedure ReadSortCols(Reader: TReader);
    procedure SetAlignButtons(AValue: TAlignment);
    procedure SetButtonFont(AValue: TFont);
    procedure SetColumns(AValue: TMyDBGridColumns);
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
    function  CreateColumns: TGridColumns; override;
    procedure DrawCellText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState;
      aText: String); override;
    procedure DrawFixedText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    function GetTruncCellHintText(aCol, aRow: Integer): string; override;
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
    function EditorIsReadOnly: boolean; override;
    procedure PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditorTextChanged(const aCol, aRow: Integer; const aText: string); override;
    function ColumnFromCol(aCol: Integer): TColumn;
    function ColFromColumn(C: TColumn): Integer;
    //procedure RefreshGrid;
    property Buttons: TGridButtons read FButtons;
    property SortCols: TSortColumns read FSortCols;
    property Editor;
    property OnCanSort: TMyGridCanSortEvent read FOnCanSort write FOnCanSort;
    property OnSortColumnChange: TNotifyEvent read FOnSortColumnChange write
      FOnSortColumnChange;
  public
    procedure PositionButtons;
    procedure MoveToSelectedRow(i: Integer);
    procedure ClearRowsSelection;
    function CurrentRowSelected: Boolean;
    function FindColumnByTitle(const Title: String): TColumn;
    property SelectedRowCount: Integer read GetSelectedRowCount;
    property Col;
    property Row;
  published
    property Columns: TMyDBGridColumns read GetColumns write SetColumns;
    property SelectedColor;
    property GridLineColor;
    property GridLineStyle;
    property SelectedTextColor: TColor read FSelectedTextColor write
      FSelectedTextColor;
    property InactiveSelectedColor: TColor read FInactiveSelectedColor write
    	FInactiveSelectedColor;
    property InactiveSelectedTextColor: TColor read FInactiveSelectedTextColor write
      FInactiveSelectedTextColor;
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
    property AllowChangeSort: Boolean read FAllowChangeSort write
      FAllowChangeSort;
    property OnButtonClick: TGridButtonClick read FOnButtonClick write
      FOnButtonClick;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
    property PopupMenu stored False;
    property ParentFont stored False;
  end;

  { TTreeSearchForm }

  TTreeSearchForm = class(TForm)
  private
    FAccept: Boolean;
    FEdit: TEdit;
    FTree: TTreeView;
    FText: String;
    FIndex: Integer;
    //FTreeOnSelect: TNotifyEvent;
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
    property Accept: Boolean read FAccept write FAccept;
  end;

  THackControl = class(TWinControl)
  public
    property ParentColor;
    property ParentFont;
    property OnDblClick;
    property OnEditingDone;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
  end;

  { THtmlProvider }

  THtmlProvider = class(TIpHtmlDataProvider)
  public
    function CanHandle(const URL: string): Boolean; override;
    function DoCheckURL(const URL: string; var ContentType: string): Boolean; override;
    procedure DoGetImage(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture);
      override;
  end;

procedure MaskingControl(aOwner: TComponent; aControl: TControl);
function GetBnCaption(Bn: TGridButtonType): String;

implementation

uses
  QuickSearchForm, apputils, dateutils, LazUtf8, Dialogs, imagemanager, base64,
  appimagelists, appsettings;

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

{ TMyGridColumns }

function TMyGridColumns.GetItems(Index: Integer): TMyGridColumn;
begin
  Result := TMyGridColumn( inherited Items[Index] );
end;

procedure TMyGridColumns.SetItems(Index: Integer; AValue: TMyGridColumn);
begin
  Items[Index].Assign(AValue);
end;

function TMyGridColumns.Add: TMyGridColumn;
begin
  Result := TMyGridColumn( inherited Add );
end;

{ TMyGridColumn }

procedure TMyGridColumn.SetAutoAlignment(AValue: Boolean);
begin
  //if FAutoAlignment=AValue then Exit;
  FAutoAlignment:=AValue;
  if AValue then Alignment := GetDefaultAlignment;
  Changed(False);
end;

procedure TMyGridColumn.SetAutoLayout(AValue: Boolean);
begin
  //if FAutoLayout=AValue then Exit;
  FAutoLayout:=AValue;
  if AValue then Layout := GetDefaultLayout;
  Changed(False);
end;

constructor TMyGridColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAutoAlignment := True;
  FAutoLayout := True;
end;

procedure TMyGridColumn.UpdateAutoLayout;
begin
  if AutoLayout then
    Layout := GetDefaultLayout;
end;

function TMyGridColumn.GetDefaultAlignment: TAlignment;
begin
  if FAutoAlignment then
  begin
    if FDataType = cdtNumber then
      Result := taRightJustify
    else if FDataType in [cdtBool, cdtImage] then
      Result := taCenter
    else
      Result := taLeftJustify
  end
  else
    Result := taLeftJustify;
end;

function TMyGridColumn.GetDefaultLayout: TTextLayout;
begin
  if FAutoLayout then
  begin
    if FDataType in [cdtBool, cdtImage] then
      Result := tlCenter
    else if TMyGrid(Grid).WordWrap then
      Result := tlTop
    else
      Result := tlCenter;
  end
  else
    Result := tlCenter;
end;

{ TMyDBGridColumns }

function TMyDBGridColumns.GetItems(Index: Integer): TMyDBGridColumn;
begin
  Result := TMyDBGridColumn(inherited Items[Index]);
end;

procedure TMyDBGridColumns.SetItems(Index: Integer; AValue: TMyDBGridColumn);
begin
  Items[Index].Assign(AValue);
end;

function TMyDBGridColumns.Add: TMyDBGridColumn;
begin
  Result := TMyDBGridColumn(inherited Add);
end;

{ TMyDBGridColumn }

function TMyDBGridColumn.IsAlignmentStored: Boolean;
begin
  Result := not FAutoAlignment;
end;

function TMyDBGridColumn.IsLayoutStored: Boolean;
begin
  Result := not FAutoLayout;
end;

procedure TMyDBGridColumn.SetAutoAlignment(AValue: Boolean);
begin
  if FAutoAlignment=AValue then Exit;
  FAutoAlignment:=AValue;
  if AValue then
    Alignment := GetDefaultAlignment;
  Changed(False);
end;

procedure TMyDBGridColumn.SetAutoLayout(AValue: Boolean);
begin
  if FAutoLayout=AValue then Exit;
  FAutoLayout:=AValue;
  if AValue then
    Layout := GetDefaultLayout;
  Changed(False);
end;

function TMyDBGridColumn.GetDefaultAlignment: TAlignment;
begin
  if FAutoAlignment then
  begin
    if (Field <> nil) and (Field.DataType = ftBlob) then
      Result := taCenter
    else
      Result := inherited GetDefaultAlignment
  end
  else
    Result := taLeftJustify;
end;

function TMyDBGridColumn.GetDefaultLayout: TTextLayout;
begin
  Result := inherited GetDefaultLayout;
  if FAutoLayout then
  begin
    if (ButtonStyle = cbsCheckboxColumn) or ((Field <> nil) and (Field.DataType = ftBlob)) then
      Result := tlCenter
    else if TMyDBGrid(Collection.Owner).WordWrap then
      Result := tlTop
  end
end;

constructor TMyDBGridColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAutoAlignment := True;
  FAutoLayout := True;
end;

{ THtmlProvider }

function THtmlProvider.CanHandle(const URL: string): Boolean;
begin
  Result := True;
end;

function THtmlProvider.DoCheckURL(const URL: string; var ContentType: string
  ): Boolean;
begin
  Result := True;
  OpenUrl(Url);
end;

procedure THtmlProvider.DoGetImage(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
var
  S: String;
  St: TStream;
begin
  S := URL; St := nil;
  if AnsiLowerCase(LeftStr(S, 7)) = 'images:' then
  begin
    Delete(S, 1, 7);
    ImageMan.GetImageStreamPPI(S, St);
  end
  else if AnsiLowerCase(LeftStr(S, 7)) = 'base64:' then
  begin
    Delete(S, 1, 7);
    if Trim(S) <> '' then
      St := TStringStream.Create(DecodeStringBase64(S));
  end;
  if St <> nil then
  begin
    Picture := TPicture.Create;
    try try
      Picture.LoadFromStream(St);
    except
      on E: Exception do
        FreeAndNil(Picture);
    end;
    finally
      St.Free;
    end;
  end;
end;

{ TListGrid }

function TListGrid.GetDefaultRowHeight: integer;
var
  TmpCanvas: TCanvas;
begin
  tmpCanvas := GetWorkingCanvas(Canvas);
  tmpCanvas.Font := Font;
  tmpCanvas.Font.PixelsPerInch := Font.PixelsPerInch;
  result := tmpCanvas.TextHeight('Yy')+2;
  if tmpCanvas<>Canvas then
    FreeWorkingCanvas(tmpCanvas);
end;

// При скроллинге списка объекта выравниваем верхнюю строку, чтобы при смене
// выделения не появлялись артефакты
procedure TListGrid.WMVScroll(var message: TLMVScroll);
begin
  inherited WMVScroll(message);
  {if message.ScrollCode = SB_ENDSCROLL then
  begin
    ResetOffset(False, True);
  end;    }
end;

constructor TListGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoFillColumns:=True;
  Options := Options - [goHorzLine, goRangeSelect, goColMoving,
    goColSizing, goEditing] + [goRowSelect, goThumbTracking, goDrawFocusSelected,
    goTruncCellHints, goScrollKeepVisible, goSmoothScroll];
  Flat := True;
  ShowHint := True;
  FocusRectVisible := False;
  SelectedColor := Color;
end;

procedure TListGrid.DrawText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState; aText: String);
begin
  DrawCellText(aCol, aRow, aRect, aState, aText);
end;

{ TMaskCellEditor }

procedure TMaskCellEditor.msg_SetMask(var Msg: TGridMessage);
begin

end;

{procedure TMaskCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  inherited msg_GetValue(Msg);
  if MaskedTextEmpty(Text, EditMask) then Msg.Value := '';
end;}

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
    w := Canvas.GetTextWidth(Caption) + ScaleToScreen(32); //Glyph.Width} + 16;
  if w < ScaleToScreen(25) then w := ScaleToScreen(25);
  h := Canvas.GetTextHeight('Yy') + ScaleToScreen(8);
  if h < ScaleToScreen(25) then h := ScaleToScreen(25);
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
  FButtonSize := ScaleToScreen(25);
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
  FButtons.Free;
  inherited Destroy;
end;

procedure TGridButtons.UpdateButtons;
{const
  ImgRes: array [TGridButtonType] of String = ('add16', 'edit16', 'delete16', 'copy16',
    'shopping16', 'up16', 'down16', 'refresh16', 'goto16');  }
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
      B.Images := Images16;
      B.ImageIndex := Ord(i);
      //B.LoadGlyphFromLazarusResource(ImgRes[i]);
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
  else if Key in [VK_ESCAPE, VK_RETURN] then
  begin
  	FAccept := Key = VK_RETURN;
    Close;
  end
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
  FAccept := True;
  Close;
end;

procedure TTreeSearchForm.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  CloseAction := caFree;
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
  P := FTree.Parent.ClientToScreen(Point(FTree.Left, FTree.Height));
  Left := P.x; Top := P.y - FEdit.Height;
  Width := FTree.Width;
  FEdit.Text := Key;
  FEdit.SelStart := Utf8Length(Key);
  Height := FEdit.Height;
  Show;
end;

{ TMyDBGrid }

// Определяем цвет темный/светлый (нашел на тостере)
function IsLightColor(AValue: TColor): Boolean;
begin
  Result := ( 299 * red(AValue) + 587 * green(AValue) + 114 * blue(AValue) > 150000{127500} );
end;

procedure TMyDBGrid.ButtonClick(Sender: TObject; Bn: TGridButtonType);
begin
  if FOnButtonClick <> nil then
    FOnButtonClick(Self, Bn);
end;

function TMyDBGrid.GetButtonFont: TFont;
begin
  Result := FButtons.Font;
end;

function TMyDBGrid.GetColumns: TMyDBGridColumns;
begin
  Result := TMyDBGridColumns( inherited Columns );
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
  FButtons.ParentFont := FButtons.Font.IsEqual(Self.Parent.Font);// FButtons.Font.IsDefault;
end;

procedure TMyDBGrid.SetColumns(AValue: TMyDBGridColumns);
begin
  inherited Columns := TMyDBGridColumns(AValue);
end;

procedure TMyDBGrid.SetVisibleCaptions(AValue: TGridButtonSet);
begin
  FButtons.VisibleCaptions:=AValue;
  FButtons.Invalidate;
end;

procedure TMyDBGrid.WriteSortCols(Writer: TWriter);
var
  S: String;
  i: Integer;
  CD: TSortColumn;
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
  CD: TSortColumn;
  i, num: Integer;
  C: TColumn;

  procedure DrawArrowUp(X1, Y1: Integer);
  begin
    //Canvas.Draw(X, Y, FUp);
    Images16.Draw(Canvas, X, Y, IMG16_UP8);
  end;

  procedure DrawArrowDown(X1, Y1: Integer);
  begin
    //Canvas.Draw(X, Y, FDown);
    Images16.Draw(Canvas, X, Y, IMG16_DOWN8);
  end;

begin
  inherited DrawCell(aCol, aRow, aRect, aState);

  if (gdFixed in aState) and (aRow = 0) and (aCol >= FixedCols) then
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

function TMyDBGrid.GetTruncCellHintText(aCol, aRow: Integer): string;
begin
  Result:=StringReplace(inherited GetTruncCellHintText(aCol, aRow), '|', '/', [rfReplaceAll]);
end;

procedure TMyDBGrid.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited UTF8KeyPress(UTF8Key);
  if ReadOnly and (Utf8Key >= ' ') then
    ShowQuickSearchForm(Utf8Key, Self);
end;

procedure TMyDBGrid.HeaderClick(IsColumn: Boolean; index: Integer);
var
  CD: TSortColumn;
  C: TColumn;
  Cancel: Boolean;
begin
  inherited HeaderClick(IsColumn, index);
  if (not IsColumn) or (index < FixedCols) or (not FAllowChangeSort) then Exit;
  if (FOnSortColumnChange = nil) {or (FOnCanSort = nil)} then Exit;
  Cancel := False;
  if FOnCanSort <> nil then
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
  Options := Options + [dgHeaderHotTracking, dgHeaderPushedLook, dgDisplayMemoText];
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
  Width := ScaleToScreen(300);
  Height := ScaleToScreen(150);
  ShowHint := True;
  FocusRectVisible:=False;
  FStopTab := True;

  Options := [dgColumnResize, dgTitles, dgIndicator, dgRowLines,
    dgColLines, {dgEditing, }dgAlwaysShowSelection,
  	dgAnyButtonCanSelect, dgDisableInsert, dgDisableDelete,
    dgHeaderPushedLook, dgHeaderHotTracking, dgThumbTracking, dgTruncCellHints,
    dgDisplayMemoText];

  FSelectedTextColor := clHighlightText;
  FInactiveSelectedColor := clSilver;
  FInactiveSelectedTextColor := clBlack;
  FastEditing:=False;
  AutoAdvance:=aaNone;
  TitleStyle:=tsLazarus;
  ParentFont := False;
  FocusColor := SelectedColor;
  FSortCols := TSortColumns.Create;
  FSortCols.Grid := Self;
  //FUp := LoadBitmapFromLazarusResource('up8');
  //FDown := LoadBitmapFromLazarusResource('down8');
  FButtons := TGridButtons.Create(Self);
  FButtons.OnButtonClick:=@ButtonClick;
  FMemo := TMemoCellEditor.Create(nil);
  DefaultRowHeight := GetDefaultRowHeight;
  // Говорят устраняет мерцание в RDP.
  DoubleBuffered := True;
end;

destructor TMyDBGrid.Destroy;
begin
  FMemo.Free;
  //FreeAndNil(FUp);
  //FreeAndNil(FDown);
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

{procedure TMyDBGrid.RefreshGrid;
begin
  LayoutChanged;
end;}

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

function TMyDBGrid.FindColumnByTitle(const Title: String): TColumn;
var
  i: Integer;
  C: TColumn;
begin
  Result := nil;
  for i := 0 to Columns.Count - 1 do
  begin
    C := Columns[i];
    if MyUtf8CompareText(C.Title.Caption, Title) = 0 then Exit(C);
  end;
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

function TMyDBGrid.CreateColumns: TGridColumns;
begin
  Result := TMyDBGridColumns.Create(Self, TMyDBGridColumn);
end;

procedure TMyDBGrid.DrawCellText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState; aText: String);
var
  TS: TTextStyle;
begin
  if FWordWrap then
  begin
    TS := Canvas.TextStyle;
    TS.Wordbreak:=True;
    TS.SingleLine:=False;
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
begin
  inherited SelectEditor;
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
  if (DataSource <> nil) and (DataSource.DataSet <> nil) and
  	(DataSource.DataSet.Active) then
	  DataSource.DataSet.UpdateCursorPos;
  inherited DoOnResize;
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

procedure TMyDBGrid.PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
var
  Clr: TColor;
begin
  inherited PrepareCanvas(aCol, aRow, aState);

  if gdSelected in aState then
  begin
		if not Focused then
    begin
      if FInactiveSelectedColor <> clNone then
      	Canvas.Brush.Color := FInactiveSelectedColor;
      Canvas.Font.Color := FInactiveSelectedTextColor;
    end
    else
    	Canvas.Font.Color := FSelectedTextColor;
  end
  else if (gdFixed in aState) and (gdHot in aState) then
  begin
    Clr := GetColumnColor(aCol, True);
    if not IsSysColor(Clr) then
	    Canvas.Brush.Color := ColorToRGB(Clr) xor $1F1F1F //ShiftColor(Clr, 10)
    else
      Canvas.Brush.Color := cl3DLight;
  end;
end;

{ TMyGrid }

procedure TMyGrid.SetSelectedTextColor(AValue: TColor);
begin
  if FSelectedTextColor=AValue then Exit;
  FSelectedTextColor:=AValue;
  Invalidate;
end;

procedure TMyGrid.SetWordWrap(AValue: Boolean);
var
  i: Integer;
begin
  if FWordWrap=AValue then Exit;
  FWordWrap:=AValue;
  for i := 0 to Columns.Count - 1 do
    Columns[i].UpdateAutoLayout;
  Invalidate;
end;

function TMyGrid.CreateColumns: TGridColumns;
begin
  Result := TMyGridColumns.Create(Self, TMyGridColumn);
end;

procedure TMyGrid.SetInactiveSelectedColor(AValue: TColor);
begin
  if FInactiveSelectedColor=AValue then Exit;
  FInactiveSelectedColor:=AValue;
  Invalidate;
end;

function TMyGrid.GetColumns: TMyGridColumns;
begin
  Result := TMyGridColumns( inherited Columns );
end;

function TMyGrid.GetIndicator: Boolean;
begin
  Result := ColWidths[0] > 0;
end;

procedure TMyGrid.SetColumns(AValue: TMyGridColumns);
begin
  inherited Columns := AValue;
end;

procedure TMyGrid.SetInactiveSelectedTextColor(AValue: TColor);
begin
  if FInactiveSelectedTextColor=AValue then Exit;
  FInactiveSelectedTextColor:=AValue;
  Invalidate;
end;

procedure TMyGrid.SetIndicator(AValue: Boolean);
begin
  if AValue then
  begin
    // Как в TDBGrid
    ColWidths[0] := Scale96ToFont(DEFINDICATORCOLWIDTH);
  end
  else
    ColWidths[0] := 0;
end;

procedure TMyGrid.DrawCellText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState; aText: String);
var
  TS: TTextStyle;
begin
  if FOnGetCellText <> nil then FOnGetCellText(Self, aCol, aRow, aText);
  if FWordWrap then
  begin
    TS := Canvas.TextStyle;
    TS.Wordbreak:=True;
    TS.SingleLine:=False;
    //TS.Layout:=tlTop;
    Canvas.TextStyle := TS;
  end;
  inherited DrawCellText(aCol, aRow, aRect, aState, aText);
end;

procedure TMyGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  X, Y: Integer;
  CD: TSortColumn;
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

  if (gdFixed in aState) and (aRow = 0) and (aCol >= FixedCols) then
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
  CD: TSortColumn;
  C: TGridColumn;
  Cancel: Boolean;
begin
  inherited HeaderClick(IsColumn, index);
  if (not IsColumn) or (index < FixedCols) then Exit;

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

procedure TMyGrid.PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
var
  Clr: TColor;
begin
  inherited PrepareCanvas(aCol, aRow, aState);

  if gdSelected in aState then
  begin
		if not Focused then
    begin
    	Canvas.Brush.Color := FInactiveSelectedColor;
      Canvas.Font.Color := FInactiveSelectedTextColor;
    end
    else
    	Canvas.Font.Color := FSelectedTextColor;
  end
  else if (gdFixed in aState) and (gdHot in aState) then
  begin
    Clr := GetColumnColor(aCol, True);
    if not IsSysColor(Clr) then
	    Canvas.Brush.Color := ColorToRGB(Clr) xor $1F1F1F//ShiftColor(Clr, 10)
    else
      Canvas.Brush.Color := cl3DLight;
  end;
end;

procedure TMyGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldKey: Word;
begin
  OldKey := Key;
  inherited KeyDown(Key, Shift);
  if OldKey = VK_ESCAPE then Key := OldKey;
end;

procedure TMyGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  C, R: Longint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight then
  begin
    MouseToCell(X, Y, C, R);
    if (C >= 0) and (R >= 0) then
    begin
      Col := C; Row := R;
    end;
  end;
end;

procedure TMyGrid.Paint;
var
  x: Integer;
begin
  inherited Paint;
  if FBorderLinePos > 0 then
  begin
    x := FBorderLinePos - GetScrollBarPosition(SB_HORZ);
    if Flat then Inc(x, 2);
    Canvas.Pen.Color := clSilver;
    Canvas.Line(x, 0, x, ClientHeight);
  end;
end;

constructor TMyGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShowHint := True;
  FSortCols := TSortColumns.Create;
  FUp := LoadBitmapFromLazarusResource('up8');
  FDown := LoadBitmapFromLazarusResource('down8');
  FocusRectVisible := False;
  FSelectedTextColor:=clHighlightText;
  FInactiveSelectedColor:=clSilver;
  FInactiveSelectedTextColor:=clBlack;
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
  	goHeaderHotTracking, goHeaderPushedLook, goDrawFocusSelected, goRangeSelect,
    goSelectionActive, goColMoving, goColSizing, goThumbTracking, goTruncCellHints,
    goSmoothScroll];
  TitleFont.Name := 'Verdana';      // Сбрасываем флаг FTitleFontIsDefault
end;

destructor TMyGrid.Destroy;
begin
  FreeAndNil(FUp);
  FreeAndNil(FDown);
  FSortCols.Free;
  inherited Destroy;
end;

end.

