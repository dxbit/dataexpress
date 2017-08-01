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
unit SummaryTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, dxctrls, strconsts, Controls, StdCtrls,
  ExtCtrls, Graphics, Menus, EditBtn, LclType;

type

  TEditProp = (epDefaultValue, epExpression, epCheckExpression, epListFilter);
  TSelectComponentEvent = procedure (Sender: TObject; Cmp: TComponent) of object;
  TEditComponentEvent = procedure (Sender: TObject; Cmp: TComponent; EditProp: TEditProp) of object;

  { TSummaryTree }

  TSummaryTree = class(TCustomControl)
  private
    FOnEditComponent: TEditComponentEvent;
    FOnSelectComponent: TSelectComponentEvent;
    FTree: TTreeView;
    FMemo: TMemo;
    FText: TLabel;
    FDefValNode, FExprNode, FCheckExprNode, FReqNode, FLblNode,
      FListFltNode: TTreeNode;
    FForm: TdxForm;
    FSearchEdit: TEditButton;
    FSearchText: String;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure SearchEditButtonClick(Sender: TObject);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeSelectionChanged(Sender: TObject);
    procedure FilterTree;
    procedure TreeUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure ClearTree;
    procedure BuildTree;
  public
    constructor Create(AnOwner: TComponent); override;
    procedure UpdateTree;
    procedure LoadTree(aForm: TdxForm);
    procedure ClearAll;
    property Form: TdxForm read FForm;
    property OnSelectComponent: TSelectComponentEvent read
			FOnSelectComponent write FOnSelectComponent;
    property OnEditComponent: TEditComponentEvent read
			FOnEditComponent write FOnEditComponent;
  end;

implementation

uses
  apputils, LazUtf8;

function HasDefVal(C: TComponent): Boolean;
var
  S: String;
  b: Boolean;
begin
  Result := HasDefaultValue(C);
  if not Result then Exit;
  S := Trim(GetDefaultValue(C));
  b := (C is TdxCalcEdit) or (C is TdxCheckBox);
  if b and (S <> '0') then
  else if (not b) and (S <> '') then
  else Result := False;
end;

function HasExpr(C: TComponent): Boolean;
begin
  Result := HasExpression(C) and (Trim(GetExpression(C)) <> '');
end;

function HasCheckExpr(C: TComponent): Boolean;
begin
  Result := HasCheckExpression(C) and (Trim(GetCheckExpression(C)) <> '');
end;

function HasReq(C: TComponent): Boolean;
begin
  Result := HasRequired(C) and (GetRequired(C) = True);
end;

function HasListFilter(C: TComponent): Boolean;
begin
  Result := Trim(GetComboFilter(C)) <> '';
end;

function GetImageIdx(C: TComponent): Integer;
var
  n: Integer;
begin
  n := -1;
  if C is TdxEdit then n := 1
  else if C is TdxCalcEdit then n := 2
  else if C is TdxDateEdit then n := 3
  else if C is TdxTimeEdit then n := 4
  else if C is TdxMemo then n := 5
  else if C is TdxCheckBox then n := 6
  else if C is TdxComboBox then n := 7
  else if C is TdxLookupComboBox then n := 8
  else if C is TdxLabel then n := 9
  else if C is TdxCounter then n := 10;
  Result := n;
end;

procedure SetImageIdx(N: TTreeNode; i: Integer);
begin
  N.ImageIndex:=i;
  N.SelectedIndex:=i;
end;

procedure ExpandOrDeleteNode(var N: TTreeNode);
begin
  if N.Count > 0 then
  begin
    SetImageIdx(N, 0);
    N.Expand(False)
  end
  else
  begin
    N.Delete;
    N := nil;
  end;
end;

{ TSummaryTree }

procedure TSummaryTree.TreeSelectionChanged(Sender: TObject);
var
  N: TTreeNode;
  C: TComponent;
  S: String;
begin
  N := FTree.Selected;
  if N = nil then Exit;
  C := TComponent(N.Data);
  FMemo.Text := '';
  FMemo.Hint := '';
  if C = nil then Exit;
  S := '';
  if C is TdxLabel then
  	S := TdxLabel(C).Expression
  else
  begin
  	if N.Parent = FDefValNode then
    	S := GetDefaultValue(C)
    else if N.Parent = FExprNode then
    	S := GetExpression(C)
    else if N.Parent = FCheckExprNode then
    	S := GetCheckExpression(C)
    else if N.Parent = FListFltNode then
    	S := GetComboFilter(C);
  end;
  FMemo.Text := S;
  FMemo.Hint := StringReplace(S, '|', '/', [rfReplaceAll]);
  if FOnSelectComponent <> nil then
  	FOnSelectComponent(Self, C);
end;

{FDefValNode, FExprNode, FCheckExprNode, FReqNode, FLblNode,
      FListFltNode: TTreeNode;}
procedure TSummaryTree.FilterTree;
var
  i: Integer;
  N, PN: TTreeNode;
  C: TComponent;
  S: String;
begin
  if FSearchText = '' then Exit;
  for i := FTree.Items.Count - 1 downto 0 do
  begin
    N := FTree.Items[i];
    PN := N.Parent;
    C := TComponent(N.Data);
    if C = nil then Continue;
    if PN = FListFltNode then
    	S := GetComboFilter(C)
    else if PN = FLblNode then
    	S := TdxLabel(C).Expression
    else if PN = FCheckExprNode then
    	S := GetCheckExpression(C)
    else if PN = FExprNode then
    	S := GetExpression(C)
    else if PN = FDefValNode then
	    S := GetDefaultValue(C)
    else Continue;
    S := Utf8LowerCase(S);
    if Utf8Pos(FSearchText, S, 1) = 0 then N.Delete;
  end;
end;

procedure TSummaryTree.TreeUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char
  );
begin
  FSearchEdit.SetFocus;
  FSearchEdit.Text:=Utf8Key;
  FSearchEdit.SelStart := 2;
end;

procedure TSummaryTree.TreeDblClick(Sender: TObject);
var
  N: TTreeNode;
  C: TComponent;
  ep: TEditProp;
begin
  N := FTree.Selected;
  if (N = nil) or (N.Parent = nil) then Exit;
  C := TComponent(N.Data);
  if (C <> nil) and (FOnEditComponent <> nil) then
  begin
    if N.Parent = FDefValNode then ep := epDefaultValue
		else if (N.Parent = FExprNode) or (N.Parent = FLblNode) then ep := epExpression
    else if N.Parent = FCheckExprNode then ep := epCheckExpression
    else if N.Parent = FListFltNode then ep := epListFilter;
  	FOnEditComponent(Self, C, ep);
  end;
end;

procedure TSummaryTree.MenuHandler(Sender: TObject);
begin
  if TComponent(Sender).Tag = 0 then
  	BuildTree;
end;

procedure TSummaryTree.MenuPopup(Sender: TObject);
begin
  FTree.PopupMenu.Items[0].Enabled:=FForm <> nil;
end;

procedure TSummaryTree.SearchEditButtonClick(Sender: TObject);
begin
  FSearchEdit.Text := '';
  FSearchText := '';
  BuildTree;
end;

procedure TSummaryTree.SearchEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  S: String;
begin
  if Key = VK_RETURN then
  begin
  	Key := 0;
    S := Utf8LowerCase(Trim(FSearchEdit.Text));
    if FSearchText <> S then
    begin
      FSearchText := S;
    	BuildTree;
    end;
  end
  else if Key = VK_DOWN then
  	FTree.SetFocus;
end;

constructor TSummaryTree.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);

  FSearchEdit := TEditButton.Create(Self);
  with FSearchEdit do
  begin
    Parent := Self;
    Align := alTop;
    BorderSpacing.Top := 2;
    BorderSpacing.Bottom := 2;
    Flat := True;
    Button.LoadGlyphFromLazarusResource('delete16');
    TextHint := rsSearchExpr;
    OnKeyDown:=@SearchEditKeyDown;
    OnButtonClick:=@SearchEditButtonClick;
  end;

  FTree := TTreeView.Create(Self);
  with FTree do
  begin
    Parent := Self;
    Align := alClient;
    ReadOnly := True;
    OnSelectionChanged:=@TreeSelectionChanged;
    OnDblClick:=@TreeDblClick;
    OnUTF8KeyPress:=@TreeUtf8KeyPress;
    PopupMenu := TPopupMenu.Create(Self);
    PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsRefresh,
    	0, 0, @MenuHandler, 'refresh16'));
    PopupMenu.OnPopup:=@MenuPopup;
  end;
  FTree.Images := TImageList.Create(Self);
  with FTree.Images do
	begin
    AddLazarusResource('folder16');
    AddLazarusResource('text16');
    AddLazarusResource('calc16');
    AddLazarusResource('date16');
    AddLazarusResource('clock16');
    AddLazarusResource('memo16');
    AddLazarusResource('checkbox16');
    AddLazarusResource('combobox16');
    AddLazarusResource('object16');
    AddLazarusResource('label16');
    AddLazarusResource('counter16');
  end;

  with TSplitter.Create(Self) do
  begin
    Parent := Self;
    Align := alBottom;
    ResizeStyle:=rsPattern;
  end;
  FMemo := TMemo.Create(Self);
  with FMemo do
  begin
    Parent := Self;
    Align := alBottom;
    ReadOnly:=True;
    ScrollBars := ssBoth;
    WordWrap:=False;
    Font.Name := 'Courier New';
    Font.Size := 9;
    Height := 100;
    ShowHint := True;
  end;
  FText := TLabel.Create(Self);
  with FText do
  begin
    Parent := FTree;
    Font.Color := clGray;
    AnchorVerticalCenterTo(FTree);
    AnchorHorizontalCenterTo(FTree);
    Transparent:=True;
    Caption:=Format(rsFieldsWithExprNotFound, [LineEnding]);
    Visible := False;
    AutoSize := True;
    Alignment := taCenter;
  end;
end;

procedure TSummaryTree.UpdateTree;
begin
  BuildTree;
end;

procedure TSummaryTree.ClearTree;
begin
  FTree.Items.Clear;
  FDefValNode := nil;
  FExprNode := nil;
  FCheckExprNode := nil;
  FReqNode := nil;
  FLblNode := nil;
  FListFltNode := nil;
  FMemo.Text := '';
end;

procedure TSummaryTree.BuildTree;
var
  i: Integer;
  C: TComponent;

  function AddTreeNode(AParent: TTreeNode; Cmp: TComponent): TTreeNode;
  var
    j: Integer;
    S: String;
  begin
    if Cmp is TdxLabel then
    	S := TdxLabel(Cmp).FieldName
    else
	    S := GetFieldName(Cmp);
    for j := 0 to AParent.Count - 1 do
    begin
      if Utf8CompareText(S, AParent.Items[j].Text) < 0 then
      begin
        Result := FTree.Items.InsertObject(AParent.Items[j],
        	S, Cmp);
        SetImageIdx(Result, GetImageIdx(Cmp));
        Exit;
      end;
    end;
    Result := FTree.Items.AddChildObject(APArent, S, Cmp);
    SetImageIdx(Result, GetImageIdx(Cmp));
  end;

begin
	ClearTree;
  FText.Visible := False;
  if FForm = nil then Exit;
  FTree.BeginUpdate;
  FDefValNode := FTree.Items.AddChild(nil, rsDefaultValue);
  FExprNode := FTree.Items.AddChild(nil, rsExpression);
  FCheckExprNode := FTree.Items.AddChild(nil, rsCheckValue);
  FReqNode := FTree.Items.AddChild(nil, rsRequired);
  FLblNode := FTree.Items.AddChild(nil, rsCalcLabels);
  FListFltNode := FTree.Items.AddChild(nil, rsListFilter);
  for i := 0 to FForm.ComponentCount - 1 do
  begin
    C := FForm.Components[i];
    if not HasFId(C) then
    begin
      if (C is TdxLabel) and (Trim(TdxLabel(C).Expression) <> '') then
        AddTreeNode(FLblNode, C);
      	{with FTree.Items.AddChildObject(FLblNode, TdxLabel(C).FieldName, C) do
        begin
          ImageIndex := 9; SelectedIndex := 9;
        end;}
      Continue;
    end;
    if HasDefVal(C) then
      AddTreeNode(FDefValNode, C);
    if HasExpr(C) then
      AddTreeNode(FExprNode, C);
    if HasCheckExpr(C) then
	    AddTreeNode(FCheckExprNode, C);
    if HasReq(C) then
      AddTreeNode(FReqNode, C);
    if HasListFilter(C) then
    	AddTreeNode(FListFltNode, C);
  end;
  FilterTree;

  ExpandOrDeleteNode(FDefValNode);
  ExpandOrDeleteNode(FExprNode);
  ExpandOrDeleteNode(FCheckExprNode);
  if FSearchText = '' then
	  ExpandOrDeleteNode(FReqNode)
  else
  	FReqNode.Delete;
  ExpandOrDeleteNode(FLblNode);
  ExpandOrDeleteNode(FListFltNode);
  FTree.EndUpdate;
  FText.Visible := FTree.Items.Count = 0;
end;

procedure TSummaryTree.LoadTree(aForm: TdxForm);
begin
  FSearchEdit.Text := '';
  FSearchText := '';
  FForm := aForm;
  BuildTree;
end;

procedure TSummaryTree.ClearAll;
begin
  FSearchEdit.Text := '';
  FSearchText := '';
  ClearTree;
  FForm := nil;
end;

end.

