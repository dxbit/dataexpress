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

unit FormsTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Menus, dxctrls, strconsts,
  TreeFilterEdit, StdCtrls, LCLType, treepanel;

type

  { TFormsTree }

  TFormsTreeCommand = (ftcAddForm, ftcDeleteForm, ftcScriptEditor);
  TFormsTreeCommandEvent = procedure (Sender: TObject; Command: TFormsTreeCommand) of object;

  TFormsTree = class(TTreePanel)
  private
    FExpertMode: Boolean;
    FOnCommand: TFormsTreeCommandEvent;
    FOnSelectionChanged: TNotifyEvent;
    function GetSelectedForm: TdxForm;
    function GetSelectedGroup: String;
    function CurrentGroupNode: TTreeNode;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure TreeCompare(Sender: TObject; Node1, Node2: TTreeNode;
      var Compare: Integer);
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeSelectionChanged(Sender: TObject);
    procedure AddGroup;
    procedure RenameGroup;
    procedure DeleteGroup;
    procedure RemoveFromGroup;
    procedure UpdateTreeSort;
    function GetGroupPath(Node: TTreeNode): String;
    procedure ChangeGroup(Node: TTreeNode);
    function HasDuplicateGroup(PNode, Node: TTreeNode; const GrName: String): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildTree; override;
    procedure SaveTree;
    procedure AddForm(Fm: TdxForm; ChildForm: Boolean);
    procedure DeleteForm(Fm: TdxForm);
    procedure DeleteSelected;
    procedure SelectForm(Fm: TdxForm);
    procedure RenameSelectedForm(const NewName: String);
    procedure ClearAll;
    procedure SetFocus; override;
    function HasEmptyGroups(out Groups: String): Boolean;
    property SelectedGroup: String read GetSelectedGroup;
    property SelectedForm: TdxForm read GetSelectedForm;
    property ExpertMode: Boolean read FExpertMode write FExpertMode;
    property OnCommand: TFormsTreeCommandEvent read FOnCommand write FOnCommand;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write
			FOnSelectionChanged;
  end;

implementation

uses
  formmanager, apputils, mydialogs, LazUtf8, appimagelists, appsettings,
  dxmains;

{ TFormsTree }

procedure TFormsTree.MenuHandler(Sender: TObject);
var
  n: PtrInt;
begin
  n := TComponent(Sender).Tag;
  case n of
    0..1: if FOnCommand <> nil then FOnCommand(Self, TFormsTreeCommand(n));
  	3: AddGroup;
    4: DeleteGroup;
    5: RenameGroup;
    7: RemoveFromGroup;
    9: ExpandAllNodes(Tree);
    10: CollapseAllNodes(Tree);
    12: if FOnCommand <> nil then FOnCommand(Self, ftcScriptEditor);
  end;
end;

procedure TFormsTree.MenuPopup(Sender: TObject);
begin
  with TPopupMenu(Sender) do
  begin
  	Items[1].Enabled := SelectedForm <> nil;
    Items[4].Enabled := (SelectedGroup <> '') and (Tree.Selected.Count = 0);
    Items[5].Enabled := SelectedGroup <> '';
    Items[7].Enabled := (Tree.Selected <> nil) and
      (Tree.Selected.Parent <> nil) and (Tree.Selected.Parent.Data = nil);
    Items[11].Visible := FExpertMode;
    Items[12].Visible := FExpertMode;
    Items[12].Enabled := SelectedForm <> nil;
  end;
end;

procedure TFormsTree.TreeCompare(Sender: TObject; Node1, Node2: TTreeNode;
  var Compare: Integer);
begin
  if (Node1.Data = nil) and (Node2.Data <> nil) then
  	Compare := 1
  else if (Node1.Data <> nil) and (Node2.Data = nil) then
  	Compare := -1
  else
  	Compare := MyUtf8CompareText(Node1.Text, Node2.Text);
end;

procedure TFormsTree.TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Target: TTreeNode;
begin
  Target := Tree.GetNodeAt(X, Y);
  // Навели на группу
  if Target.Data = nil then
    Tree.Selected.MoveTo(Target, naAddChild)
  // Навели на какую-то форму
  else
  begin
    {if Target.Parent <> nil then
      Tree.Selected.MoveTo(Target.Parent, naAddChild)
    else    }
      Tree.Selected.MoveTo(Target, naAdd)
  end;
  if SelectedForm <> nil then SelectedForm.FormGroup := GetGroupPath(Tree.Selected.Parent)
  else ChangeGroup(Tree.Selected);
  UpdateTreeSort;
end;

procedure TFormsTree.TreeDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  N: TTreeNode;

  function IsSelectedParent(Node: TTreeNode): Boolean;
  begin
    Result := False;
    while Node <> nil do
		begin
      if Node = Tree.Selected then Exit(True);
      Node := Node.Parent;
    end;
  end;

begin
  N := Tree.GetNodeAt(X, Y);
  if (Source <> Sender) or (N = nil) then Accept := False
  else if (SelectedForm <> nil) and (SelectedForm.PId > 0) then Accept := False
  else if (N.Data <> nil) and (TdxForm(N.Data).PId > 0) then Accept := False
  else
    Accept := {(N.Data = nil) and}
    	(not IsSelectedParent(N)) and (N <> Tree.Selected.Parent) and
      ((N.Parent <> Tree.Selected.Parent) or (N.Data = nil));
end;

procedure TFormsTree.TreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  N: TTreeNode;
begin
  if Button = mbLeft then
  begin
	  N := Tree.GetNodeAt(X, Y);
  	if N <> nil then Tree.BeginDrag(False);
  end;
end;

procedure TFormsTree.TreeSelectionChanged(Sender: TObject);
begin
  if FOnSelectionChanged <> nil then
  	FOnSelectionChanged(Self);
end;

procedure TFormsTree.AddGroup;
var
  GrName: String;
  N: TTreeNode;
begin
  GrName := '';
  N := CurrentGroupNode;
  if (ShowGroupNameDlg(GrName) = mrOk) and (not HasDuplicateGroup(N, nil, GrName)) then
  begin
    N := Tree.Items.AddChild(N, GrName);
    N.ImageIndex:=0;
    N.SelectedIndex:=0;
    N.Selected := True;
    UpdateTreeSort;
  end;
end;

procedure TFormsTree.RenameGroup;
var
  GrName: String;
  N: TTreeNode;
begin
  N := CurrentGroupNode;
  GrName := N.Text;
  if (ShowGroupNameDlg(GrName) = mrOk) and (not HasDuplicateGroup(N.Parent, N, GrName)) then
  begin
    N.Text := GrName;
    UpdateTreeSort;
    ChangeGroup(N);
  end;
end;

procedure TFormsTree.DeleteGroup;
begin
  if not ConfirmDelete then Exit;
  Tree.Selected.Delete;
end;

procedure TFormsTree.RemoveFromGroup;
var
  N: TTreeNode;
begin
  N := Tree.Selected;
  N.MoveTo(N.GetParentNodeOfAbsoluteLevel(0), naAdd);
  if SelectedForm <> nil then SelectedForm.FormGroup := ''
  else ChangeGroup(N);
  UpdateTreeSort;
end;

procedure TFormsTree.UpdateTreeSort;
begin
  Tree.SortType:=stNone;
  Tree.SortType:=stBoth;
end;

function TFormsTree.GetGroupPath(Node: TTreeNode): String;
begin
  Result := '';
  while Node <> nil do
  begin
    if Node.Data = nil then
  		Result := Node.Text + '\' + Result;
    Node := Node.Parent;
  end;
  Result := Copy(Result, 1, Length(Result) - 1);
end;

procedure TFormsTree.ChangeGroup(Node: TTreeNode);
var
  i: Integer;
  N: TTreeNode;
  Fm: TdxForm;
begin
  for i := 0 to Node.Count - 1 do
  begin
    N := Node.Items[i];
    Fm := TdxForm(N.Data);
    if Fm <> nil then Fm.FormGroup := GetGroupPath(N.Parent)
    else ChangeGroup(N);
  end;
end;

function TFormsTree.HasDuplicateGroup(PNode, Node: TTreeNode;
  const GrName: String): Boolean;
var
  i: Integer;
  N: TTreeNode;
begin
  Result := False;
  if PNode = nil then
  begin
    for i := 0 to Tree.Items.Count - 1 do
    begin
      N := Tree.Items[i];
      if (N <> Node) and (N.Parent = nil) and (N.Data = nil) and
      	(MyUtf8CompareText(N.Text, GrName) = 0) then
      begin
        Result := True;
        Break;
      end;
    end;
  end
  else
  begin
    for i := 0 to PNode.Count - 1 do
    begin
      N := PNode.Items[i];
      if (N <> Node) and (N.Data = nil) and	(MyUtf8CompareText(N.Text, GrName) = 0) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

  if Result then ErrMsg(rsThisGroupAlreadyExists);
end;

function TFormsTree.GetSelectedForm: TdxForm;
var
  N: TTreeNode;
begin
  N := Tree.Selected;
  if (N = nil) or (N.Data = nil) then Result := nil
  else Result := TdxForm(N.Data);
end;

function TFormsTree.GetSelectedGroup: String;
var
  N: TTreeNode;
begin
  N := Tree.Selected;
  if (N = nil) or (N.Data <> nil) then Result := ''
  else Result := N.Text;
end;

function TFormsTree.CurrentGroupNode: TTreeNode;
begin
  Result := Tree.Selected;
  while Result <> nil do
  begin
    if Result.Data = nil then Exit(Result)
    else Result := Result.Parent;
  end;
end;

constructor TFormsTree.Create(AOwner: TComponent);
var
  Pop: TPopupMenu;
  IL: TImageList;
begin
  inherited Create(AOwner);
  with Tree do
  begin
    Tree.DragMode := dmManual;
    Tree.OnSelectionChanged:=@TreeSelectionChanged;
    Tree.OnMouseDown:=@TreeMouseDown;
    Tree.OnDragOver:=@TreeDragOver;
    Tree.OnDragDrop:=@TreeDragDrop;
    Tree.OnCompare:=@TreeCompare;
  end;

  Filter.TextHint:=rsFindForm;

  Pop := TPopupMenu.Create(Self);
  Pop.Images := Images16;
  Pop.Items.Add( CreateMenuItem(Pop, rsAddForm, 0, 0, @MenuHandler, IMG16_ADD) );
  Pop.Items.Add( CreateMenuItem(Pop, rsDeleteForm, 1, 0, @MenuHandler, IMG16_DELETE) );
  Pop.Items.Add( CreateMenuItem(Pop, '-', 2, 0, nil) );
  Pop.Items.Add( CreateMenuItem(Pop, rsAddGroup, 3, 0, @MenuHandler, IMG16_FOLDER_ADD) );
  Pop.Items.Add( CreateMenuItem(Pop, rsDeleteGroup, 4, 0, @MenuHandler, IMG16_FOLDER_DELETE) );
  Pop.Items.Add( CreateMenuItem(Pop, rsRenameGroup, 5, 0, @MenuHandler) );
  Pop.Items.Add( CreateMenuItem(Pop, '-', 6, 0, nil) );
  Pop.Items.Add( CreateMenuItem(Pop, rsRemoveFromGroup, 7, 0, @MenuHandler) );
  Pop.Items.Add( CreateMenuItem(Pop, '-', 8, 0, nil) );
  Pop.Items.Add( CreateMenuItem(Pop, rsExpandAll, 9, 0, @MenuHandler) );
	Pop.Items.Add( CreateMenuItem(Pop, rsCollapseAll, 10, 0, @MenuHandler) );
  Pop.Items.Add( CreateMenuItem(Pop, '-', 11, 0, nil) );
  Pop.Items.Add( CreateMenuItem(Pop, rsScript, 12, 0, @MenuHandler) );
  Pop.OnPopup:=@MenuPopup;

  IL := TImageList.Create(Self);
  SetupImageList(IL, ['folder16']);

  Tree.PopupMenu := Pop;
  Tree.Images := IL;
end;

procedure TFormsTree.BuildTree;
var
  i, j: Integer;
  Fm, PFm: TdxForm;
  PN: TTreeNode;
  GL: TFormGroupList;
  G: TFormGroup;

  function FindOrAddNode(PN: TTreeNode; const aText: String): TTreeNode;
  var
    j: Integer;
    N: TTreeNode;
  begin
    if PN = nil then
    begin
      for j := 0 to Tree.Items.TopLvlCount - 1 do
      begin
        N := Tree.Items.TopLvlItems[j];
        if (MyUtf8CompareText(N.Text, aText) = 0) and (N.Data = nil) then
        	Exit(N);
      end;
      Result := Tree.Items.AddChild(nil, aText);
    end
    else
    begin
      for j := 0 to PN.Count - 1 do
      begin
        N := PN.Items[j];
        if (MyUtf8CompareText(N.Text, aText) = 0) and (N.Data = nil) then
        	Exit(N);
      end;
      Result := Tree.Items.AddChild(PN, aText);
    end;
    Result.ImageIndex := 0;
    Result.SelectedIndex := 0;
  end;

  function FindOrAddGroup(const GroupName: String): TTreeNode;
  var
    SL: TStringList;
    j: Integer;
    N: TTreeNode;
  begin
    SL := TStringList.Create;
    SplitStr(GroupName, '\', SL);
    N := nil;
    for j := 0 to SL.Count - 1 do
      N := FindOrAddNode(N, SL[j]);
    SL.Free;
    Result := N;
  end;

begin
  inherited BuildTree;
  Tree.Items.Clear;

  GL := DXMain.Groups;
  for i := 0 to GL.Count - 1 do
  begin
    G := GL[i];
    PN := FindOrAddGroup(G.Name);
    for j := 0 to G.IdList.Count - 1 do
    begin
      Fm := FormMan.FindForm(G.IdList[j]);
      Tree.Items.AddChildObject(PN, Fm.FormCaption, Fm);
    end;
  end;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if Fm.PId = 0 then Continue;
    PFm := FormMan.FindForm(Fm.PId);

    // Может быть из-за ошибки в предыдущих версиях
    if PFm = nil then Continue;

    PN := Tree.Items.FindNodeWithData( PFm );
  	Tree.Items.AddChildObject(PN, Fm.FormCaption, Fm);
  end;
  UpdateTreeSort;
end;

procedure TFormsTree.SaveTree;
var
  i: Integer;
  N: TTreeNode;
  Fm: TdxForm;
  GroupName: String;
  GL: TFormGroupList;
  G: TFormGroup;
begin
  GL := DXMain.Groups;
  GL.Clear;
  for i := 0 to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    Fm := TdxForm(N.Data);
    if (Fm = nil) or (Fm.PId > 0) then Continue;

    GroupName := GetGroupPath(N);
    G := GL.FindGroup(GroupName);
    if G = nil then
    begin
      G := GL.AddGroup;
      G.Name := GroupName;
    end;
    G.IdList.AddValue(Fm.Id);
  end;
end;

procedure TFormsTree.AddForm(Fm: TdxForm; ChildForm: Boolean);
var
  PN, N: TTreeNode;
begin
  if ChildForm then
  	PN := Tree.Selected
  else
  begin
	  PN := CurrentGroupNode;
    Fm.FormGroup := GetGroupPath(PN);
  end;
  N := Tree.Items.AddChildObject(PN, Fm.FormCaption, Fm);
  if not ChildForm then N.Selected := True
  else PN.Expand(False);
  UpdateTreeSort;
end;

procedure TFormsTree.DeleteForm(Fm: TdxForm);
begin
  Tree.Items.FindNodeWithData(Fm).Delete;
  Tree.Invalidate;
end;

procedure TFormsTree.DeleteSelected;
begin
  if Tree.Selected <> nil then Tree.Selected.Delete;
end;

procedure TFormsTree.SelectForm(Fm: TdxForm);
begin
  Tree.Items.FindNodeWithData(Fm).Selected := True;
end;

procedure TFormsTree.RenameSelectedForm(const NewName: String);
begin
  if Tree.Selected <> nil then
  begin
    Tree.Selected.Text := NewName;
    UpdateTreeSort;
  end;
end;

procedure TFormsTree.ClearAll;
begin
  Tree.Items.Clear;
  Filter.Text := '';
end;

procedure TFormsTree.SetFocus;
begin
  inherited SetFocus;
  Tree.SetFocus;
end;

function TFormsTree.HasEmptyGroups(out Groups: String): Boolean;

	function HasForms(Node: TTreeNode): Boolean;
  var
    j: Integer;
    N: TTreeNode;
  begin
    Result := False;
    for j := 0 to Node.Count - 1 do
    begin
      N := Node.Items[j];
      if N.Data <> nil then Result := True
    	else if HasForms(N) then Result := True;
    end;
    if not Result then
    	Groups := Groups + GetGroupPath(Node) + LineEnding;
  end;

var
  i: Integer;
  N: TTreeNode;
begin
  Groups := '';
  for i := 0 to Tree.Items.TopLvlCount - 1 do
  begin
    N := Tree.Items.TopLvlItems[i];
    if N.Data = nil then HasForms(N);
  end;
  Result := Groups <> '';
end;

end.

