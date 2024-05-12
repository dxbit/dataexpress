unit CheckTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, Controls, ComCtrls, Themes, treeviewex;

type

  { TCheckTreeView }

  TTreeCheckChangeEvent = procedure (Sender: TObject; ANode: TTreeNode; AValue: Boolean) of object;

  TCheckTreeView = class(TTreeViewEx)
  private
    FMixedMode: Boolean;
    FOnCheckChange: TTreeCheckChangeEvent;
    FStateImages: TImageList;
    procedure InitStateImages;
    procedure ChangeNodeState;
  protected
    function CreateNode: TTreeNode; override;
    procedure Click; override;
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(AnOwner: TComponent); override;
    function NodeChecked(N: TTreeNode): Boolean;
    procedure SetChecked(N: TTreeNode; Value: Boolean);
    procedure CheckAll;
    procedure UnCheckAll;
    procedure UpdateNodesState;
  published
    property MixedMode: Boolean read FMixedMode write FMixedMode default False;
    property OnCheckChange: TTreeCheckChangeEvent read FOnCheckChange
			write FOnCheckChange;
  end;

implementation

type
  TNodeState = 0..2;
  TNodeStates = set of TNodeState;

{ TCheckTreeView }

// Код взят с сайта http://forum.lazarus-ide.org/index.php?topic=24193.0
procedure TCheckTreeView.InitStateImages;
var
	aSize: TSize;
  aBMP: TBitmap;
  aDetails: TThemedElementDetails;
  aRect: TRect;
begin
  aDetails:=ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
  aSize:=ThemeServices.GetDetailSize(aDetails);
  FStateImages.Width:=aSize.cx;
  FStateImages.Height:=aSize.cy;
  aBMP:=TBitmap.Create;
  with aBMP do
    begin
      SetSize(aSize.cx, aSize.cy);
      Transparent:=True;
      TransparentColor:=clForm;
      Brush.Color:=TransparentColor;
      Canvas.FillRect(0,0, Width,Height);
    end;
  aRect:=Rect(0, 0, aSize.cx, aSize.cy);
  ThemeServices.DrawElement(aBMP.Canvas.Handle, aDetails, aRect, nil);
  FStateImages.Add(aBMP, nil);

  aBMP.Canvas.FillRect(0,0, aBmp.Width, aBmp.Height);;
  aDetails:=ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
  ThemeServices.DrawElement(aBMP.Canvas.Handle, aDetails, aRect, nil);
  FStateImages.Add(aBMP, nil);

  aBMP.Canvas.FillRect(0,0, aBmp.Width, aBmp.Height);;
  aDetails:=ThemeServices.GetElementDetails(tbCheckBoxMixedNormal);
  ThemeServices.DrawElement(aBMP.Canvas.Handle, aDetails, aRect, nil);
  FStateImages.Add(aBMP, nil);

  FreeAndNil(aBMP);
end;

procedure TCheckTreeView.ChangeNodeState;

  procedure ChangeChildNodeState(PN: TTreeNode; Idx: Integer);
  var
    i: Integer;
  begin
    for i := 0 to PN.Count - 1 do
    begin
      PN[i].StateIndex := Idx;
      ChangeChildNodeState(PN[i], Idx);
    end;
  end;

  procedure ChangeParentNodeState(PN: TTreeNode);
  var
    i: Integer;
    Mix: TNodeStates;
    N: TTreeNode;
  begin
    if PN = nil then Exit;

    Mix := [];
    for i := 0 to PN.Count - 1 do
    begin
      N := PN[i];
      Include(Mix, N.StateIndex);
    end;
    if Mix - [0] = [] then PN.StateIndex := 0
    else if Mix - [1] = [] then PN.StateIndex := 1
    else PN.StateIndex := 2;
    ChangeParentNodeState(PN.Parent);
  end;

var
  Node: TTreeNode;
begin
  Node := Selected;
  if (Node <> nil) and (Node.StateIndex >= 0) then
  begin
    if Node.StateIndex in [0, 2] then Node.StateIndex := 1
    else if Node.StateIndex = 1 then Node.StateIndex := 0;
    if FMixedMode then
    begin
      ChangeChildNodeState(Node, Node.StateIndex);
      ChangeParentNodeState(Node.Parent);
    end;
    if FOnCheckChange <> nil then FOnCheckChange(Self, Node, Node.StateIndex = 1);
  end;
end;

function TCheckTreeView.CreateNode: TTreeNode;
begin
  Result:=inherited CreateNode;
  Result.StateIndex:=0;
end;

procedure TCheckTreeView.Click;
var
  P: Types.TPoint;
begin
  inherited Click;
  P := Mouse.CursorPos;
  P := ScreenToClient(P);
  if htOnStateIcon in GetHitTestInfoAt(P.X, P.Y) then ChangeNodeState;
end;

procedure TCheckTreeView.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if Key = #32 then ChangeNodeState;
end;

constructor TCheckTreeView.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  ReadOnly:=True;
  FStateImages := TImageList.Create(Self);
  InitStateImages;
  StateImages := FStateImages;
end;

function TCheckTreeView.NodeChecked(N: TTreeNode): Boolean;
begin
  Result := N.StateIndex = 1;
end;

procedure TCheckTreeView.SetChecked(N: TTreeNode; Value: Boolean);
begin
	if Value then N.StateIndex := 1
  else N.StateIndex := 0;
end;

procedure TCheckTreeView.CheckAll;
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].StateIndex := 1;
end;

procedure TCheckTreeView.UnCheckAll;
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].StateIndex := 0;
end;

procedure TCheckTreeView.UpdateNodesState;

  procedure InnerUpdateStates(ANode: TTreeNode);
  var
    i: Integer;
    N: TTreeNode;
    Stat: TNodeStates;
  begin
    if not ANode.HasChildren then Exit;
    Stat := [];
    for i := 0 to ANode.Count - 1 do
    begin
      N := ANode.Items[i];
      InnerUpdateStates(N);
      Include(Stat, N.StateIndex);
    end;
    if Stat - [0] = [] then ANode.StateIndex := 0
    else if Stat - [1] = [] then ANode.StateIndex := 1
    else ANode.StateIndex := 2;
  end;

var
  i: Integer;
  N: TTreeNode;
begin
  for i := 0 to Items.TopLvlCount - 1 do
  begin
    N := Items.TopLvlItems[i];
    InnerUpdateStates(N);
    {if Stat - [0] = [] then N.StateIndex := 0
    else if Stat - [1] = [] then N.StateIndex := 1
    else N.StateIndex := 2;   }
  end;
end;

end.

