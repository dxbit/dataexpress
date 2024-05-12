unit TreeViewEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Types, InterfaceBase, ExtCtrls;

type

  { TTreeViewEx }

  TTreeViewEx = class(TTreeView)
  private
    FIsWine: Boolean;
    FOldX, FOldY: Integer;
    FHistory: TList;
    FHistoryIndex: Integer;
    FNoAddToHistory: Boolean;
    FOnStateChanged: TNotifyEvent;
    procedure SetIsWine(AValue: Boolean);
    procedure AddNodeToHistory(N: TTreeNode);
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
                          MousePos: TPoint): Boolean; override;
    function DoMouseWheelHorz(Shift: TShiftState; WheelDelta: Integer;
                          MousePos: TPoint): Boolean; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
    procedure KeyPress(var Key: char); override;
    procedure DoSelectionChanged; override;
    procedure DoStateChanged; virtual;
    procedure Delete(Node: TTreeNode); override;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    function GoBackNode: Boolean;
    function GoNextNode: Boolean;
    function CanBackNode: Boolean;
    function CanNextNode: Boolean;
    procedure ClearHistory;
    procedure ClearTree;
    function AddNodePath(const Path: String; APathDelim: Char; DefaultImageIndex: Integer): TTreeNode;
    property IsWine: Boolean read FIsWine write SetIsWine;
  published
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('dxComponents', [TTreeViewEx]);
end;

{ TTreeViewEx }

procedure TTreeViewEx.SetIsWine(AValue: Boolean);
begin
  if FIsWine=AValue then Exit;
  FIsWine:=AValue;
  if AValue then ShowHint := True;
end;

procedure TTreeViewEx.AddNodeToHistory(N: TTreeNode);
var
  i: Integer;
begin
  if FNoAddToHistory or (Items.SelectionCount > 1) then Exit;

  for i := FHistory.Count - 1 downto FHistoryIndex + 1 do
    FHistory.Delete(i);
  FHistoryIndex := FHistory.Add(N);
end;

function TTreeViewEx.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  OldToolTips: Boolean;
begin
  if FIsWine then
  begin
    OldToolTips := ToolTips;
    ToolTips := False;
    Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
    ToolTips := OldToolTips;
    Application.CancelHint;
  end
  else
    Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TTreeViewEx.DoMouseWheelHorz(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  OldToolTips: Boolean;
begin
  if FIsWine then
  begin
    OldToolTips := ToolTips;
    ToolTips := False;
    Result:=inherited DoMouseWheelHorz(Shift, WheelDelta, MousePos);
    ToolTips := OldToolTips;
    Application.CancelHint;
  end
  else
    Result:=inherited DoMouseWheelHorz(Shift, WheelDelta, MousePos);
end;

procedure TTreeViewEx.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldToolTips: Boolean;
begin
  if FIsWine then
  begin
    OldToolTips := ToolTips;
    ToolTips := False;
    inherited MouseMove(Shift, X, Y);
    ToolTips := OldToolTips;
    if (Abs(X - FOldX) > 1) or (Abs(Y - FOldY) > 1) then
    begin
      Application.CancelHint;
    end;
    FOldX := X; FOldY := Y;
  end
  else
    inherited MouseMove(Shift, X, Y);
end;

procedure TTreeViewEx.DoOnShowHint(HintInfo: PHintInfo);
var
  Node: TTreeNode;
  TextRect, IntRect: TRect;
  X, Y: LongInt;
begin
  if FIsWine then
  begin
    X := HintInfo^.CursorPos.X;
    Y := HintInfo^.CursorPos.Y;
    Node := GetNodeAt(X, Y);
    if Node=nil then Exit;
    TextRect := Rect(Node.DisplayTextLeft, Node.Top, Node.DisplayTextRight, Node.Top + Node.Height);
    OffsetRect(TextRect, 0, -ScrolledTop);
    if not PtInRect(TextRect, Point(X, Y))
      or (IntersectRect(IntRect, TextRect, ClientRect) and EqualRect(IntRect, TextRect)) then
    else
      HintInfo^.HintStr := Node.Text;
  end;
  inherited DoOnShowHint(HintInfo);
end;

procedure TTreeViewEx.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if (Key = #8) and CanBackNode then GoBackNode;
end;

procedure TTreeViewEx.DoSelectionChanged;
begin
  inherited DoSelectionChanged;
  if Selected <> nil then AddNodeToHistory(Selected);
  DoStateChanged;
end;

procedure TTreeViewEx.DoStateChanged;
begin
  if FOnStateChanged <> nil then FOnStateChanged(Self);
end;

procedure TTreeViewEx.Delete(Node: TTreeNode);
var
  i: Integer;
begin
  if FHistory <> nil then
  begin
    for i := FHistory.Count - 1 downto 0 do
      if FHistory[i] = Pointer(Node) then
      begin
        FHistory.Delete(i);
        if FHistoryIndex >= i then Dec(FHistoryIndex);
      end;

    // Удаляем идущие подряд узлы
    for i := FHistory.Count - 1 downto 1 do
      if FHistory[i] = FHistory[i - 1] then
      begin
        FHistory.Delete(i);
        if FHistoryIndex >= i then Dec(FHistoryIndex);
      end;
  end;
  inherited Delete(Node);
end;

constructor TTreeViewEx.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  FHistory := TList.Create;
  FHistoryIndex := -1;
end;

destructor TTreeViewEx.Destroy;
begin
  FreeAndNil(FHistory);
  inherited Destroy;
end;

function TTreeViewEx.GoBackNode: Boolean;
begin
  if (FHistoryIndex <= 0) or not TTreeNode(FHistory[FHistoryIndex - 1]).Visible then Exit(False);

  if Selected <> nil then
    Dec(FHistoryIndex);
  FNoAddToHistory := True;
  Items.ClearMultiSelection(True);
  Selected := TTreeNode(FHistory[FHistoryIndex]);
  FNoAddToHistory := False;
  Result := True;
end;

function TTreeViewEx.GoNextNode: Boolean;
begin
  if (FHistoryIndex >= FHistory.Count - 1) or not TTreeNode(FHistory[FHistoryIndex + 1]).Visible then Exit(False);

  if Selected <> nil then
    Inc(FHistoryIndex);
  FNoAddToHistory := True;
  Items.ClearMultiSelection(True);
  Selected := TTreeNode(FHistory[FHistoryIndex]);
  FNoAddToHistory := False;
  Result := True;
end;

function TTreeViewEx.CanBackNode: Boolean;
begin
  Result := FHistoryIndex > 0;
end;

function TTreeViewEx.CanNextNode: Boolean;
begin
  Result := (FHistoryIndex >= 0) and (FHistoryIndex < FHistory.Count - 1);
end;

procedure TTreeViewEx.ClearHistory;
begin
  FHistory.Clear;
  FHistoryIndex := -1;
end;

procedure TTreeViewEx.ClearTree;
begin
  ClearHistory;
  // Чтобы не было AV, при выходе из дизайнера из-за того, что при Clear
  // SelectionCount не обнуляется.
  Items.ClearMultiSelection(True);
  //
  Items.Clear;
end;

function TTreeViewEx.AddNodePath(const Path: String; APathDelim: Char;
  DefaultImageIndex: Integer): TTreeNode;
var
  SL: TStringList;
  PN, N: TTreeNode;
  i: Integer;
begin
  SL := TStringList.Create;
  SL.Delimiter:=APathDelim;
  SL.StrictDelimiter:=True;
  SL.DelimitedText:=Path;
  PN := nil;
  for i := 0 to SL.Count - 1 do
  begin
    if PN = nil then
      N := Items.FindNodeWithText(SL[i])
    else
      N := PN.FindNode(SL[i]);
    if N = nil then
      N := Items.AddChild(PN, SL[i]);
    PN := N;
    PN.ImageIndex := DefaultImageIndex;
    PN.SelectedIndex := DefaultImageIndex;
  end;
  SL.Free;
  Result := PN;
end;

end.
