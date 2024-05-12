unit DialogGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, Menus, LclType, Controls, StdCtrls, Graphics,
  LMessages, Spin, EditBtn, Dialogs, Buttons, Clipbrd, LclIntf;

type

  TDialogGridCommand = (dgcAppend, dgcEdit, dgcDelete, dgcMoveUp, dgcMoveDown, dgcCopy, dgcPaste);
  TDialogGridCommands = set of TDialogGridCommand;

  TDialogGrid = class;

  { TDialogGridButtons }

  TDialogGridButtons = class(TWinControl)
  private
    FGrid: TDialogGrid;
    FButtons: array [0..4] of TSpeedButton;
    procedure ButtonClick(Sender: TObject);
    procedure CreateButtons;
    procedure SetGrid(AValue: TDialogGrid);
    procedure UpdateButtons;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Grid: TDialogGrid read FGrid write SetGrid;
    property Align;
    property BorderSpacing;
  end;

  { TListCellEditor }

  TListCellEditor = class(TCustomComboBox)
  private
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
  protected
    procedure WndProc(var TheMessage : TLMessage); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure Change; override;
    procedure Select; override;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EditingDone; override;
    property BorderStyle;
    property OnEditingDone;
  end;

  { TIntegerCellEditor }

  TIntegerCellEditor = class(TSpinEdit)
  private
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
  protected
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure Change; override;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    procedure EditingDone; override;
  end;

  { TFloatCellEditor }

  TFloatCellEditor = class(TFloatSpinEdit)
  private
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
  protected
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure Change; override;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    procedure EditingDone; override;
  end;

  { TColorCellEditor }

  TColorCellEditor = class(TWinControl)
  private
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
    FColor: TColorButton;
    procedure ColorChanged(Sender: TObject);
  protected
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TExpressionCellEditor }

  TExpressionCellEditor = class(TEditButton)
  private
    FCol, FRow: Integer;
    FGrid: TCustomGrid;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  protected
    function GetDefaultGlyphName: string; override;
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

  { TTextCellEditor }

  TTextCellEditor = class(TStringCellEditor)
  private
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  public
    constructor Create(Aowner: TComponent); override;
  end;

  TDialogGridCommandEvent = procedure (Sender: TObject; Cmd: TDialogGridCommand) of object;

  { TDialogGridColumn }

  TDGColumnType = (gctAuto, gctText, gctList, gctInteger, gctFloat, gctColor,
    gctExpr);

  TDialogGridColumn = class(TGridColumn)
  private
    FColumnType: TDGColumnType;
    FIncrement: Integer;
    FMaxValue: Integer;
    FMinValue: Integer;
  public
    constructor Create(ACollection: TCollection); override;
    function AllowCopyPaste: Boolean;
  published
    property ColumnType: TDGColumnType read FColumnType write FColumnType;
    property MinValue: Integer read FMinValue write FMinValue;
    property MaxValue: Integer read FMaxValue write FMaxValue;
    property Increment: Integer read FIncrement write FIncrement;
  end;

  { TDialogGridColumns }

  TDialogGridColumns = class(TGridColumns)
  private
    function GetItems(Index: Integer): TDialogGridColumn;
    procedure SetItems(Index: Integer; AValue: TDialogGridColumn);
  public
    function Add: TDialogGridColumn;
    function ColumnByTitle(const ATitle: String): TDialogGridColumn;
    property Items[Index: Integer]: TDialogGridColumn read GetItems write SetItems; default;
  end;

  { TDialogGrid }

  TDialogGrid = class(TStringGrid)
  private
    FButtons: TDialogGridButtons;
    FModified: Boolean;
    FOnCommand: TDialogGridCommandEvent;
    FOnResetValue: TNotifyEvent;
    FShowCommands: TDialogGridCommands;
    FImages: TImageList;
    FListEdit: TListCellEditor;
    FIntegerEdit: TIntegerCellEditor;
    FFloatEdit: TFloatCellEditor;
    FColorEdit: TColorCellEditor;
    FExprEdit: TExpressionCellEditor;
    FTextEdit: TTextCellEditor;
    procedure ExprEditButtonClick(Sender: TObject);
    procedure SetControlState(ARow: Integer);
    function GetColumns: TDialogGridColumns;
    procedure MenuClick(Sender: TObject);
    procedure SetColumns(AValue: TDialogGridColumns);
    procedure SetShowCommands(AValue: TDialogGridCommands);
    procedure MoveUpRow;
    procedure MoveDownRow;
  protected
    procedure DoCutToClipboard; override;
    procedure DoCopyToClipboard; override;
    procedure DoPasteFromClipboard; override;
    function ValidateEntry(const ACol, ARow: Integer; const OldValue: string;
      var NewValue: string): boolean; override;
    procedure DrawCellText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState;
      aText: String); override;
    function  CreateColumns: TGridColumns; override;
    function SelectCell(aCol, aRow: Integer): boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetDefaultEditor(Column: Integer): TWinControl; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure EditordoResetValue; override;
    function EditorCanAcceptKey(const ch: TUTF8Char): boolean; override;
    procedure ColRowDeleted(IsColumn: Boolean; index: Integer); override;
    procedure ColRowInserted(IsColumn: boolean; index: integer); override;
    procedure ColRowExchanged(IsColumn: Boolean; index, WithIndex: Integer); override;
    function GetTruncCellHintText(ACol, ARow: Integer): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditorTextChanged(const aCol,aRow: Integer; const aText:string); override;
    procedure DoCommand(Cmd: TDialogGridCommand);
    procedure CopyRowsToClipboard;
    procedure PasteRowsFromClipboard;
    property Modified: Boolean read FModified write FModified;
  published
    property Columns: TDialogGridColumns read GetColumns write SetColumns;
    property ShowCommands: TDialogGridCommands read FShowCommands write
			SetShowCommands;
    property OnCommand: TDialogGridCommandEvent read FOnCommand write FOnCommand;
    property OnResetValue: TNotifyEvent read FOnResetValue write FOnResetValue;
  end;

  { TStringGridEx }

  TStringGridEx = class(TStringGrid)
  protected
    procedure DoCutToClipboard; override;
    procedure DoCopyToClipboard; override;
    procedure DoPasteFromClipboard; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

resourcestring
  rsAppend = 'Append';
  rsEdit = 'Edit';
  rsDelete = 'Delete';
  rsMoveUp = 'Move up';
  rsMoveDown = 'Move down';
  rsCut = 'Cut';
  rsCopy = 'Copy';
  rsPaste = 'Paste';
  rsCopyCells = 'Copy cells';
  rsPasteCells = 'Paste cells';

implementation

uses
  LazUtf8;

type
  THackGrid = class(TCustomGrid);

function CreateMenuItem(AOwner: TComponent; const Caption: String; aShortCut: TShortCut;
	ImgIdx, Tag: Integer; Handler: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(AOwner);
  Result.Caption := Caption;
  Result.ShortCut := aShortCut;
  Result.ImageIndex := ImgIdx;
  Result.Tag := Tag;
  Result.OnClick := Handler;
end;

function RemoveNonPrintableChars(const S: String; KeepNewLine: Boolean = False): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if S[i] >= ' ' then Result := Result + S[i]
    else if (S[i] in [#10, #13]) and KeepNewLine then Result := Result + S[i];
end;

{ TStringGridEx }

procedure TStringGridEx.DoCutToClipboard;
begin
  if not GetColumnReadonly(Col) then
  begin
    Clipboard.AsText := Cells[Col, Row];
    Cells[Col, Row] := '';
  end;
end;

procedure TStringGridEx.DoCopyToClipboard;
var
  S: String;
begin
  S := Cells[Col, Row];
  DoCellProcess(Col, Row, cpCopy, S);
  Clipboard.AsText := S;
end;

procedure TStringGridEx.DoPasteFromClipboard;
var
  S: String;
begin
  if Clipboard.HasFormat(CF_TEXT) and not GetColumnReadonly(Col) then
  begin
    S := Clipboard.AsText;
    if not ValidateOnSetSelection or ValidateEntry(Col, Row, Cells[Col, Row], S) then
    begin
      DoCellProcess(Col, Row, cpPaste, S);
      Cells[Col, Row] := S;
    end;
  end;
end;

procedure TStringGridEx.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_X) and (Shift = [ssModifier]) then
  begin
    if not EditorKey then DoCutToClipboard;
  end;
end;

{ TTextCellEditor }

procedure TTextCellEditor.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipboard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
  end;
end;

procedure TTextCellEditor.MenuPopup(Sender: TObject);
begin
  PopupMenu.Items[0].Enabled := SelText <> '';
  PopupMenu.Items[1].Enabled := SelText <> '';
end;

constructor TTextCellEditor.Create(Aowner: TComponent);
var
  IL: TImageList;
begin
  inherited Create(Aowner);
  IL := TImageList.Create(Self);
  IL.AddLazarusResource('_cut16');
  IL.AddLazarusResource('_copy16');
  IL.AddLazarusResource('_paste16');

  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, ShortCut(VK_X, [ssCtrl]), 0, 0, @MenuHandler) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, ShortCut(VK_C, [ssCtrl]), 1, 1, @MenuHandler) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, ShortCut(VK_V, [ssCtrl]), 2, 2, @MenuHandler) );
  PopupMenu.OnPopup:=@MenuPopup;
  PopupMenu.Images := IL;
end;

{ TExpressionCellEditor }

procedure TExpressionCellEditor.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipboard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
  end;
end;

procedure TExpressionCellEditor.MenuPopup(Sender: TObject);
begin
  PopupMenu.Items[0].Enabled := SelText <> '';
  PopupMenu.Items[1].Enabled := SelText <> '';
end;

function TExpressionCellEditor.GetDefaultGlyphName: string;
begin
  Result:='sum24';
end;

procedure TExpressionCellEditor.EditChange;
begin
  inherited EditChange;
  if (FGrid<>nil) and Visible then
  begin
    FGrid.EditorTextChanged(FCol, FRow, Text);
  end;
end;

procedure TExpressionCellEditor.EditKeyDown(var Key: Word; Shift: TShiftState);

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

procedure TExpressionCellEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text:=Msg.Value;
  SelStart := UTF8Length(Text);
end;

procedure TExpressionCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col:=FCol;
  Msg.Row:=FRow;
  Msg.Value:=Text;
end;

procedure TExpressionCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=TCustomGrid(Msg.Grid);
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TExpressionCellEditor.msg_SelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

procedure TExpressionCellEditor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TExpressionCellEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED
end;

constructor TExpressionCellEditor.Create(AOwner: TComponent);
var
  IL: TImageList;
begin
  inherited Create(AOwner);
  IL := TImageList.Create(Self);
  IL.AddLazarusResource('_cut16');
  IL.AddLazarusResource('_copy16');
  IL.AddLazarusResource('_paste16');

  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, ShortCut(VK_X, [ssCtrl]), 0, 0, @MenuHandler) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, ShortCut(VK_C, [ssCtrl]), 1, 1, @MenuHandler) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, ShortCut(VK_V, [ssCtrl]), 2, 2, @MenuHandler) );
  PopupMenu.OnPopup:=@MenuPopup;
  PopupMenu.Images := IL;
end;


{ TColorCellEditor }

procedure TColorCellEditor.ColorChanged(Sender: TObject);
begin
  if FGrid <> nil then
    FGrid.EditorTextChanged(FCol, FRow, ColorToString(FColor.ButtonColor));
end;

procedure TColorCellEditor.KeyDown(var Key: Word; Shift: TShiftState);
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
        if key<>0 then
          THackGrid(FGrid).EditorHide;
      end;
  end;
end;

procedure TColorCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col := FCol;
  Msg.Row := FRow;
  Msg.Value:=ColorToString(FColor.ButtonColor);
end;

procedure TColorCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TColorCellEditor.msg_SetValue(var Msg: TGridMessage);
begin
  FColor.ButtonColor := StringToColor(Msg.Value)
end;

procedure TColorCellEditor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TColorCellEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TColorCellEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FColor := TColorButton.Create(Self);
  FColor.Parent := Self;
  FColor.Align := alClient;
  FColor.OnColorChanged:=@ColorChanged;
end;

{ TDialogGridButtons }

procedure TDialogGridButtons.CreateButtons;
const
  Glp: array [0..4] of String = ('_add16', '_edit16', '_delete16', '_up16', '_down16');
  Cap: array [0..4] of String = (rsAppend, rsEdit, rsDelete, rsMoveUp, rsMoveDown);
var
  i: Integer;
  Bn: TSpeedButton;
begin
  for i := 0 to 4 do
  begin
    Bn := TSpeedButton.Create(Self);
    Bn.ControlStyle:=Bn.ControlStyle + [csNoDesignSelectable];
    Bn.Parent := Self;
    Bn.LoadGlyphFromLazarusResource(Glp[i]);
    Bn.Caption := Cap[i];
    Bn.ShowCaption := True;
    Bn.Flat := True;
    Bn.AutoSize := True;
    Bn.Constraints.MinHeight:=25;
    Bn.Constraints.MinWidth:=25;
    Bn.Tag := i;
    Bn.OnClick:=@ButtonClick;
    FButtons[i] := Bn;
  end;
end;

procedure TDialogGridButtons.SetGrid(AValue: TDialogGrid);
begin
  if FGrid=AValue then Exit;
  FGrid := AValue;
  FGrid.FButtons := Self;
  FGrid.FreeNotification(Self);
  UpdateButtons;
end;

procedure TDialogGridButtons.ButtonClick(Sender: TObject);
begin
  FGrid.DoCommand( TDialogGridCommand(TComponent(Sender).Tag) );
end;

procedure TDialogGridButtons.UpdateButtons;
var
  i: Integer;
  Bn: TSpeedButton;
begin
  if (csLoading in ComponentState) or (csReading in ComponentState) then Exit;

  for i := 0 to 4 do
  begin
    Bn := FButtons[i];
    Bn.Visible := (FGrid = nil) or (TDialogGridCommand(i) in FGrid.ShowCommands);
    if Bn.Visible then
      Bn.ControlStyle:=Bn.ControlStyle - [csNoDesignVisible]
    else
      Bn.ControlStyle:=Bn.ControlStyle + [csNoDesignVisible];
  end;
  Invalidate;
  AdjustSize;
end;

procedure TDialogGridButtons.Loaded;
begin
  inherited Loaded;
  UpdateButtons;
end;

procedure TDialogGridButtons.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FGrid = AComponent) then
  begin
    FGrid := nil;
    UpdateButtons;
  end;
end;

constructor TDialogGridButtons.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  AutoSize := True;
  ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
  Color := clBtnFace;
  CreateButtons;
end;

destructor TDialogGridButtons.Destroy;
begin
  if FGrid <> nil then FGrid.FButtons := nil;
  inherited Destroy;
end;

{ TDialogGridColumn }

constructor TDialogGridColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIncrement := 1;
end;

function TDialogGridColumn.AllowCopyPaste: Boolean;
begin
  Result := FColumnType in [gctText, gctExpr];
end;

{ TFloatCellEditor }

procedure TFloatCellEditor.KeyDown(var Key: Word; Shift: TShiftState);
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
    VK_ESCAPE:
      begin
        doGridKeyDown;
        if key<>0 then begin
          THackGrid(FGrid).EditorHide;
        end;
      end;
    else
      doEditorKeyDown;
  end;
end;

procedure TFloatCellEditor.EditingDone;
begin
  inherited EditingDone;
  if FGrid<>nil then
    FGrid.EditingDone;
end;

procedure TFloatCellEditor.Change;
begin
  inherited Change;
  if (FGrid<>nil) and Visible then
    FGrid.EditorTextChanged(FCol, FRow, Text);
end;

procedure TFloatCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col := FCol;
  Msg.Row := FRow;
  Msg.Value:=Text;
end;

procedure TFloatCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TFloatCellEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text := Msg.Value;
  SelStart := Length(Text);
end;

procedure TFloatCellEditor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TFloatCellEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;


{ TIntegerCellEditor }

procedure TIntegerCellEditor.KeyDown(var Key: Word; Shift: TShiftState);
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
    VK_ESCAPE:
      begin
        doGridKeyDown;
        if key<>0 then begin
          THackGrid(FGrid).EditorHide;
        end;
      end;
    else
      doEditorKeyDown;
  end;
end;

procedure TIntegerCellEditor.EditingDone;
begin
  inherited EditingDone;
  if FGrid<>nil then
  begin
    FGrid.EditorTextChanged(FCol, FRow, IntToStr(Value));
    FGrid.EditingDone;
  end;
end;

procedure TIntegerCellEditor.Change;
begin
  inherited Change;
  if (FGrid<>nil) and Visible then
    FGrid.EditorTextChanged(FCol, FRow, Text);
end;

procedure TIntegerCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col := FCol;
  Msg.Row := FRow;
  Msg.Value:=Text;
end;

procedure TIntegerCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TIntegerCellEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text := Msg.Value;
  SelStart := Length(Text);
end;

procedure TIntegerCellEditor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TIntegerCellEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;


{ TListCellEditor }

procedure TListCellEditor.WndProc(var TheMessage: TLMessage);
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

procedure TListCellEditor.KeyDown(var Key: Word; Shift: TShiftState);
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
    // if editor is not readonly, start editing
    // else not interested
    if (FGrid=nil) or THackGrid(FGrid).EditorIsReadOnly then
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
        THackGrid(FGrid).EditorHide;
      end;
    else
      doEditorKeyDown
  end;
end;

procedure TListCellEditor.EditingDone;
begin
  inherited EditingDone;
  if FGrid<>nil then
  begin
    if ItemIndex >= 0 then
      TCustomStringGrid(FGrid).Objects[FCol, FRow] := Items.Objects[ItemIndex]
    else
      TCustomStringGrid(FGrid).Objects[FCol, FRow] := nil;

    FGrid.EditingDone;
  end;
end;

procedure TListCellEditor.Change;
begin
  inherited Change;
  if (FGrid<>nil) and Visible then
    FGrid.EditorTextChanged(FCol, FRow, Text);
end;

procedure TListCellEditor.Select;
begin
  if FGrid<>nil then
  begin
    FGrid.EditorTextChanged(FCol, FRow, Text);
    THackGrid(FGrid).PickListItemSelected(Self);
  end;
  inherited Select;
end;

procedure TListCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col := FCol;
  Msg.Row := FRow;
  Msg.Value:=Text;
  if ItemIndex >= 0 then
    TCustomStringGrid(FGrid).Objects[FCol, FRow] := Items.Objects[ItemIndex]
  else
    TCustomStringGrid(FGrid).Objects[FCol, FRow] := nil;
end;

procedure TListCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TListCellEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text := Msg.Value;
  SelStart := Length(Text);
end;

procedure TListCellEditor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TListCellEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TListCellEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Style := csDropDownList;
  DropDownCount := 16;
end;


{ TDialogGridColumns }

function TDialogGridColumns.GetItems(Index: Integer): TDialogGridColumn;
begin
  Result := TDialogGridColumn( inherited Items[Index] );
end;

procedure TDialogGridColumns.SetItems(Index: Integer; AValue: TDialogGridColumn
  );
begin
  Items[Index].Assign(AValue);
end;

function TDialogGridColumns.Add: TDialogGridColumn;
begin
  Result := TDialogGridColumn( inherited Add );
end;

function TDialogGridColumns.ColumnByTitle(const ATitle: String
  ): TDialogGridColumn;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Utf8CompareText(Items[i].Title.Caption, aTitle) = 0 then
      Exit(Items[i]);
end;

{ TDialogGrid }

procedure TDialogGrid.MenuClick(Sender: TObject);
var
  Cmd: TDialogGridCommand;
begin
  Cmd := TDialogGridCommand(TComponent(Sender).Tag);
  DoCommand(Cmd);
end;

function TDialogGrid.GetColumns: TDialogGridColumns;
begin
  Result := TDialogGridColumns( inherited Columns );
end;

procedure TDialogGrid.SetColumns(AValue: TDialogGridColumns);
begin
  inherited Columns := TDialogGridColumns(AValue);
end;

procedure TDialogGrid.SetShowCommands(AValue: TDialogGridCommands);
begin
  if FShowCommands=AValue then Exit;
  FShowCommands:=AValue;
  with PopupMenu do
  begin
    Items[0].Visible := dgcAppend in FShowCommands;
    Items[1].Visible := dgcEdit in FShowCommands;
    Items[2].Visible := dgcDelete in FShowCommands;
    Items[4].Visible := dgcMoveUp in FShowCommands;
    Items[5].Visible := dgcMoveDown in FShowCommands;
    Items[7].Visible := dgcCopy in FShowCommands;
    Items[8].Visible := dgcPaste in FShowCommands;

    Items[0].Enabled := Items[0].Visible;
    Items[1].Enabled := Items[1].Visible;
    Items[2].Enabled := Items[2].Visible;
    Items[4].Enabled := Items[4].Visible;
    Items[5].Enabled := Items[5].Visible;
    Items[7].Enabled := Items[7].Visible;
    Items[8].Enabled := Items[8].Visible;

    Items[3].Visible := (Items[0].Visible or Items[1].Visible or Items[2].Visible)
    	and (Items[4].Visible or Items[5].Visible);
    Items[6].Visible := Items[7].Visible or Items[8].Visible;
  end;
  if FButtons <> nil then FButtons.UpdateButtons;
end;

procedure TDialogGrid.MoveUpRow;
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

procedure TDialogGrid.MoveDownRow;
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

procedure TDialogGrid.DoCutToClipboard;
begin
  if not GetColumnReadonly(Col) then
  begin
    Clipboard.AsText := Cells[Col, Row];
    Cells[Col, Row] := '';
  end;
end;

procedure TDialogGrid.DoCopyToClipboard;
var
  S: String;
begin
  S := Cells[Col, Row];
  DoCellProcess(Col, Row, cpCopy, S);
  Clipboard.AsText := S;
end;

procedure TDialogGrid.DoPasteFromClipboard;
var
  S: String;
begin
  if Clipboard.HasFormat(CF_TEXT) and not GetColumnReadonly(Col) then
  begin
    S := Clipboard.AsText;
    if not ValidateOnSetSelection or ValidateEntry(Col, Row, Cells[Col, Row], S) then
    begin
      DoCellProcess(Col, Row, cpPaste, S);
      Cells[Col, Row] := S;
    end;
  end;
end;

function TDialogGrid.ValidateEntry(const ACol, ARow: Integer;
  const OldValue: string; var NewValue: string): boolean;
var
  C: TDialogGridColumn;
begin
  Result := True;
  C := TDialogGridColumn( ColumnFromGridColumn(aCol) );
  if C <> nil then
  begin
    if C.ColumnType = gctText then NewValue := RemoveNonPrintableChars(NewValue)
    else if C.ColumnType = gctExpr then NewValue := RemoveNonPrintableChars(NewValue, True)
  end;
  Result := inherited ValidateEntry(ACol, ARow, OldValue, NewValue);
end;

procedure TDialogGrid.DrawCellText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState; aText: String);
var
  C: TDialogGridColumn;
  Clr: TColor;
  S: String;
begin
  inherited DrawCellText(aCol, aRow, aRect, aState, aText);
  C := TDialogGridColumn( ColumnFromGridColumn(aCol) );
  if (C <> nil) and (C.ColumnType = gctColor) then
  begin
    S := Cells[aCol, aRow];
    if S <> '' then
    begin
      Clr := StringToColor(S);
      Canvas.Brush.Color := Clr;
      Canvas.FillRect(aRect);
      if gdSelected in aState then
      begin
        Canvas.Pen.Width := Scale96ToScreen(4);
        Canvas.Pen.Color := SelectedColor;
        Canvas.Frame(aRect);
      end;
    end;
  end;
end;

procedure TDialogGrid.DoCommand(Cmd: TDialogGridCommand);
begin
  EditorMode := False;
  if CanFocus and HandleAllocated then SetFocus;
  case Cmd of
  	dgcMoveUp: MoveUpRow;//ExchangeColRow(False, Row, Row - 1);
		dgcMoveDown: MoveDownRow;//ExchangeColRow(False, Row, Row + 1);
    dgcCopy: CopyRowsToClipboard;
    dgcPaste: PasteRowsFromClipboard;
  end;
  if FOnCommand <> nil then FOnCommand(Self, Cmd);
  SetControlState(Row);
  if Cmd = dgcAppend then FModified := True;
end;

procedure TDialogGrid.CopyRowsToClipboard;
var
  i, j: Integer;
  RowCopy: Boolean;
  S, Buf: String;
begin
  Buf := '';
  for i := 1 to RowCount - 1 do
  begin
    S := ''; RowCopy := False;
    for j := 0 to ColCount - 1 do
    begin
      if IsCellSelected[j, i] then
      begin
        RowCopy := True;
        S := S + '"' + StringReplace(Cells[j, i], '"', '""', [rfReplaceAll]) + '"';
      end;
      S := S + #9;
    end;
    if RowCopy then
    begin
      SetLength(S, Length(S) - 1);
      Buf := Buf + S + LineEnding;
    end;
  end;
  SetLength(Buf, Length(Buf) - Length(LineEnding));
  if Buf <> '' then
  begin
    S := '';
    for j := 0 to Columns.Count - 1 do
      S := S + Columns[j].Title.Caption + #9;
    SetLength(S, Length(S) - 1);
    Buf := S + LineEnding + Buf;
  end;
  Clipboard.AsText:=Buf;
end;

function ReadValue(St: TStream; out EndLine: Boolean): String;
var
  IsQuote: Boolean;
  S: String;
begin
  Result := '';
  SetLength(S, 1);
  IsQuote := False;
  EndLine := False;

  if St.Read(S[1], 1) = 1 then
  begin
    if S = '"' then IsQuote := True
    else St.Position := St.Position - 1;
  end;

  while (St.Read(S[1], 1) = 1) do
  begin
    if (S = #9) and not IsQuote then
      Break
    else if S = '"' then
    begin
      // Экранированные кавычки - ""
      if not IsQuote then
        Result := Result + S[1]
      else if St.Read(S[1], 1) = 1 then
      begin
        if S = '"' then
          Result := Result + S[1]
        else
        begin
          IsQuote := False;
          St.Position := St.Position - 1;
        end;
      end
      else
        IsQuote := False;
    end
    else if (S[1] in [#10, #13]) and not IsQuote then
    begin
      EndLine := True;
      if (St.Read(S[1], 1) = 1) and (S <> #10) then
        St.Position := St.Position - 1;
      Break;
    end
    else
      Result := Result + S[1];
  end;
  // Если закрывающая кавычка не встретилась, то считаем ее как часть значения.
  if IsQuote then
    Result := '"' + Result;
end;

function IsValidColor(const S: String): Boolean;
begin
  try
    StringToColor(S);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TDialogGrid.PasteRowsFromClipboard;

  procedure PasteValue(ACol: TDialogGridColumn; ARow: Integer; const Value: String);
  var
    C, i: Integer;
    N: Longint;
    E: Extended;
  begin
    C := ACol.Index;
    case ACol.ColumnType of
      gctText, gctExpr: Cells[C, ARow] := Value;
      gctColor:
        begin
          if IsValidColor(Value) then
            Cells[C, ARow] := Value
          else
            Cells[C, ARow] := 'clWhite';
          InvalidateCell(C, ARow);
        end;
      gctInteger:
        if TryStrToInt(Value, N) then
          Cells[C, ARow] := IntToStr(N)
        else if Value = '-' then
          Cells[C, ARow] := Value;
      gctFloat:
        if TryStrToFloat(Value, E) then
          Cells[C, ARow] := FloatToStr(E);
      gctList:
        begin
          i := ACol.PickList.IndexOf(Value);
          if i >= 0 then
          begin
            Cells[C, ARow] := Value;
            Objects[C, ARow] := ACol.PickList.Objects[i];
          end;
        end;
    end;
  end;

var
  St: TMemoryStream;
  Titles: TStringList;
  EL, HasTitles: Boolean;
  S: String;
  i, r, StartRow: Integer;
  Column: TGridColumn;
begin
  HasTitles := False;
  Titles := TStringList.Create;
  St := TMemoryStream.Create;
  if Clipboard.GetFormat(PredefinedClipboardFormat(pcfText), St) then
  begin
    St.Position := 0;
    repeat
      S := ReadValue(St, EL);
      Column := Columns.ColumnByTitle(S);
      Titles.AddObject(S, Column);
      if Column <> nil then HasTitles := True;
    until EL or (S = '');
    if HasTitles then
    begin

      i := 0;
      if Row > 0 then r := Row
      else r := 1;
      StartRow := r;

      while St.Position < St.Size do
      begin
        if i = 0 then
        begin
          InsertColRow(False, r);
          FModified := True;
        end;
        S := ReadValue(St, EL);
        if i < Titles.Count then
        begin
          Column := TGridColumn(Titles.Objects[i]);
          if Column <> nil then
            PasteValue(TDialogGridColumn(Column), r, S);
        end;
        Inc(i);
        if EL then
        begin
          i := 0;
          Inc(r);
        end;
      end;

      Row := r;
      Selection := Rect(0, StartRow, Columns.Count - 1, r);
    end;
  end;
  St.Free;
  Titles.Free;
end;

function TDialogGrid.CreateColumns: TGridColumns;
begin
  Result := TDialogGridColumns.Create(Self, TDialogGridColumn);
end;

function TDialogGrid.SelectCell(aCol, aRow: Integer): boolean;
begin
  Result:=inherited SelectCell(aCol, aRow);
  SetControlState(aRow);
end;

procedure TDialogGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldKey: Word;
begin
  OldKey := Key;
  if Key in [VK_NEXT, VK_PRIOR] then
  begin
    case Key of
      VK_PRIOR: if PopupMenu.Items[4].Visible then PopupMenu.Items[4].Click;
      VK_NEXT: if PopupMenu.Items[5].Visible then PopupMenu.Items[5].Click;
    end;
    Key := 0;
  end
  else if (Key in [VK_X, VK_C, VK_V]) and (Shift * [ssModifier, ssShift] <> []) and
    (Columns.Count > 0) and not Columns[Col].AllowCopyPaste then Key := 0
  else if (Key = VK_X) and (Shift = [ssModifier]) then
  begin
    if not EditorKey then DoCutToClipboard;
  end
  else if (Key = VK_DELETE) and (Shift = [ssModifier]) then Exit;

  inherited KeyDown(Key, Shift);

  // Возвращаем Esc, чтобы можно было закрыть диалог
  if OldKey = VK_ESCAPE then Key := OldKey;
end;

function TDialogGrid.GetDefaultEditor(Column: Integer): TWinControl;
var
  C: TDialogGridColumn;
  ct: TDGColumnType;
begin
  Result := nil;
  if EditingAllowed(Column) then
  begin
    C := TDialogGridColumn(ColumnFromGridColumn(Column));
    if C <> nil then ct := C.ColumnType
    else ct := gctAuto;
    case ct of
      gctList:
        begin
          //FListEdit.Clear;
          FListEdit.Items := C.PickList;
          //FListEdit.DropDownCount := C.DropDownRows;
          Result := FListEdit;
        end;
      gctInteger:
        begin
          FIntegerEdit.MinValue := C.MinValue;
          FIntegerEdit.MaxValue := C.MaxValue;
          FIntegerEdit.Increment := C.Increment;
          Result := FIntegerEdit;
        end;
      gctFloat:
        begin
          FFloatEdit.MinValue := C.MinValue;
          FFloatEdit.MaxValue := C.MaxValue;
          FFloatEdit.Increment := C.Increment;
          Result := FFloatEdit;
        end;
      gctColor: Result := FColorEdit;
      gctExpr: Result := FExprEdit;
      gctText: Result := FTextEdit;
      else
        Result:=inherited GetDefaultEditor(Column);
    end;
  end;
end;

procedure TDialogGrid.DoEnter;
begin
  inherited DoEnter;
  SelectedColor := clHighlight;
end;

procedure TDialogGrid.DoExit;
begin
  inherited DoExit;
  SelectedColor := clSilver;
end;

procedure TDialogGrid.EditordoResetValue;
var
  FOldText: String;
begin
  FOldText := Cells[Col, Row];
  inherited EditordoResetValue;
  if (FOnResetValue <> nil) and (Cells[Col, Row] <> FOldText) then FOnResetValue(Self);
end;

function TDialogGrid.EditorCanAcceptKey(const ch: TUTF8Char): boolean;
begin
  Result:=inherited EditorCanAcceptKey(ch);
  {$ifdef linux}
  // В линукс в редактор попадает нажатый символ или значение стирается (в списках).
  if (Editor is TButtonCellEditor) or (Editor is TColorCellEditor) or (Editor is TListCellEditor) then Result := False;
  {$endif}
end;

procedure TDialogGrid.ColRowDeleted(IsColumn: Boolean; index: Integer);
begin
  inherited ColRowDeleted(IsColumn, index);
  FModified := True;
end;

procedure TDialogGrid.ColRowInserted(IsColumn: boolean; index: integer);
var
  i: Integer;
  C: TDialogGridColumn;
begin
  inherited ColRowInserted(IsColumn, index);
  FModified := True;
  for i := 0 to Columns.Count - 1 do
  begin
    C := Columns[i];
    if C.ColumnType = gctColor then Cells[i, index] := 'clWhite';
  end;
end;

procedure TDialogGrid.ColRowExchanged(IsColumn: Boolean; index,
  WithIndex: Integer);
begin
  inherited ColRowExchanged(IsColumn, index, WithIndex);
  FModified := True;
end;

function TDialogGrid.GetTruncCellHintText(ACol, ARow: Integer): string;
{var
  S: String;
  W: LongInt;}
begin
  Result:=inherited GetTruncCellHintText(ACol, ARow);
  Result :=StringReplace(Result, '|', '/', [rfReplaceAll]);
  {if Result = '' then
  begin
    S := Cells[ACol, ARow];
    W := ColWidths[ACol];
    if (W > 0) and (Canvas.TextWidth(S) > W) then
	    Result :=StringReplace(S, '|', '/', [rfReplaceAll]);
  end;  }
end;

procedure TDialogGrid.SetControlState(ARow: Integer);
var
  SingleSelect: Boolean;
begin
  if PopupMenu = nil then Exit;

  with PopupMenu do
	begin
    SingleSelect := not ((SelectedRangeCount > 1) or (Selection.Height > 0) or (Selection.Width > 0));
    Items[0].Enabled := (dgcAppend in FShowCommands) and SingleSelect;
    Items[1].Enabled := (dgcEdit in FShowCommands) and (ARow >= FixedRows) and SingleSelect;
    Items[2].Enabled := (dgcDelete in FShowCommands) and (ARow >= FixedRows) and SingleSelect;
    Items[4].Enabled := (dgcMoveUp in FShowCommands) and (ARow > FixedRows) and SingleSelect;
    Items[5].Enabled := (dgcMoveDown in FShowCommands) and (ARow >= FixedRows) and (ARow < RowCount - 1) and SingleSelect;
    Items[7].Enabled := (dgcCopy in FShowCommands) and (ARow >= FixedRows);
    Items[8].Enabled := Clipboard.HasFormat(PredefinedClipboardFormat(pcfText)) and SingleSelect;
    if FButtons <> nil then
    begin
      FButtons.FButtons[0].Enabled := Items[0].Enabled;
      FButtons.FButtons[1].Enabled := Items[1].Enabled;
      FButtons.FButtons[2].Enabled := Items[2].Enabled;
      FButtons.FButtons[3].Enabled := Items[4].Enabled;
      FButtons.FButtons[4].Enabled := Items[5].Enabled;
      //FButtons.FButtons[5].Enabled := Items[7].Enabled;
      //FButtons.FButtons[6].Enabled := Items[8].Enabled;
    end;
  end;
end;

procedure TDialogGrid.ExprEditButtonClick(Sender: TObject);
begin
  if OnButtonClick <> nil then OnButtonClick(Self, Col, Row);
end;

constructor TDialogGrid.Create(AOwner: TComponent);
var
  Pop: TPopupMenu;
begin
  inherited Create(AOwner);
  AllowOutboundEvents:=False;
  FixedRows := 1;
  FixedCols := 0;
  RowCount := 2;
  ColCount := 1;
  AlternateColor := $00EEEEEE;
  SelectedColor := clSilver;
  FocusRectVisible := False;
  FastEditing := False;
  Flat := True;

  FImages := TImageList.Create(Self);
  with FImages do
  begin
    AddLazarusResource('_add16');
    AddLazarusResource('_edit16');
    AddLazarusResource('_delete16');
    AddLazarusResource('_up16');
    AddLazarusResource('_down16');
    AddLazarusResource('_copy16');
    AddLazarusResource('_paste16');
  end;

  Pop := TPopupMenu.Create(Self);
  Pop.Items.Add( CreateMenuItem(Pop, rsAppend, ShortCut(VK_INSERT, []), 0, 0, @MenuClick) );
  Pop.Items.Add( CreateMenuItem(Pop, rsEdit, ShortCut(0, []), 1, 1, @MenuClick) );
  Pop.Items.Add( CreateMenuItem(Pop, rsDelete, ShortCut(VK_DELETE, [ssCtrl]), 2, 2, @MenuClick) );
  Pop.Items.Add( CreateMenuItem(Pop, '-', 0, -1, 0, nil) );
  Pop.Items.Add( CreateMenuItem(Pop, rsMoveUp, ShortCut(VK_PRIOR, []), 3, 3, @MenuClick) );
  Pop.Items.Add( CreateMenuItem(Pop, rsMoveDown, ShortCut(VK_NEXT, []), 4, 4, @MenuClick) );
  Pop.Items.Add( CreateMenuItem(Pop, '-', 0, -1, 0, nil) );
  Pop.Items.Add( CreateMenuItem(Pop, rsCopyCells, 0, 5, 5, @MenuClick) );
  Pop.Items.Add( CreateMenuItem(Pop, rsPasteCells, 0, 6, 6, @MenuClick) );
  Pop.Images := FImages;
  PopupMenu := Pop;

  //AutoAdvance:=aaNone;
  Options := Options - [goRangeSelect] + [goEditing, goDrawFocusSelected,
    goThumbTracking, goCellHints];
  AutoFillColumns:=True;
  ShowHint := True;
  ValidateOnSetSelection := True;

  ShowCommands := [dgcAppend, dgcDelete, dgcMoveUp, dgcMoveDown];

  FListEdit := TListCellEditor.Create(Self);
  FIntegerEdit := TIntegerCellEditor.Create(Self);
  FFloatEdit := TFloatCellEditor.Create(Self);
  FColorEdit := TColorCellEditor.Create(Self);
  FExprEdit := TExpressionCellEditor.Create(Self);
  FExprEdit.OnButtonClick:=@ExprEditButtonClick;
  FTextEdit := TTextCellEditor.Create(Self);
end;

procedure TDialogGrid.EditorTextChanged(const aCol, aRow: Integer;
  const aText: string);
begin
  inherited EditorTextChanged(aCol, aRow, aText);
  FModified := True;
end;

end.

