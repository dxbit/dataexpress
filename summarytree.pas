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

unit SummaryTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, dxctrls, strconsts, Controls, StdCtrls,
  ExtCtrls, Graphics, Menus, EditBtn, LclType, TreeFilterEdit, Dialogs,
  treepanel;

type

  TEditProp = (epDefaultValue, epExpression, epCheckExpression, epListFilter);
  TSelectComponentEvent = procedure (Sender: TObject; Cmp: TComponent) of object;
  TEditComponentEvent = procedure (Sender: TObject; Cmp: TComponent; EditProp: TEditProp) of object;

  TSummaryData = class
  public
    Cmp: TComponent;
    ParentNode, Node: TTreeNode;
  end;

  { TSummaryTree }

  TSummaryTree = class(TTreePanel)
  private
    FOnEditComponent: TEditComponentEvent;
    FOnSelectComponent: TSelectComponentEvent;
    FMemo: TMemo;
    FDefValNode, FExprNode, FCheckExprNode, FReqNode, FLblNode,
      FListFltNode, FHiddenNode: TTreeNode;
    FForm: TdxForm;
    FData: TList;

    procedure FilterAfterFilter(Sender: TObject);
    function FilterFilterItem(Item: Pointer; out Done: Boolean): Boolean;
    procedure TreeDblClick(Sender: TObject);
    procedure TreeSelectionChanged(Sender: TObject);
    procedure ClearTree;
    function GetExprByNode(PN: TTreeNode; C: TComponent): String;
    procedure UpdateMemo(N: TTreeNode);
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildTree; override;
    procedure UpdateTree;
    procedure RenameComponent(C: TComponent);
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
  apputils, LazUtf8, dxreports, dximages, dxfiles, dxcharts, pivotgrid;

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

{function GetImageIdx(C: TComponent): Integer;
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
end;   }

function GetImageIdx(C: TComponent): Integer;
const
  Cls: array [2..25] of TClass = (TdxEdit, TdxCalcEdit, TdxDateEdit, TdxTimeEdit,
    TdxMemo, TdxCheckBox, TdxComboBox, TdxLookupComboBox, TdxLabel, TdxCounter,
    TdxObjectField, TdxShape, TdxButton, TdxQueryGrid, TdxGrid, TdxDBImage,
    TdxImage, TdxTabSheet, TdxPageControl, TdxPivotGrid, TdxGroupBox, TdxFile,
    TdxChart, TdxRecordId);
var
  i: Integer;
begin
  Result := -1;
  for i := Low(Cls) to High(Cls) do
    if C.ClassType = Cls[i] then Exit(i);
end;

procedure SetImageIdx(N: TTreeNode; i: Integer);
begin
  N.ImageIndex:=i;
  N.SelectedIndex:=i;
end;

{ TSummaryTree }

procedure TSummaryTree.TreeSelectionChanged(Sender: TObject);
var
  N: TTreeNode;
  D: TSummaryData;
begin
  N := Tree.Selected;
  if N = nil then Exit;
  UpdateMemo(N);
  D := TSummaryData(N.Data);
  if (D <> nil) and (FOnSelectComponent <> nil) then
  	FOnSelectComponent(Self, D.Cmp);
end;

procedure TSummaryTree.TreeDblClick(Sender: TObject);
var
  N, PN: TTreeNode;
  C: TComponent;
  ep: TEditProp;
  D: TSummaryData;
begin
  N := Tree.Selected;
  if (N = nil) or (N.Data = nil) then Exit;
  D := TSummaryData(N.Data);
  if (D <> nil) and (FOnEditComponent <> nil) then
  begin
    C := D.Cmp;
    PN := D.ParentNode;
    if PN = FDefValNode then ep := epDefaultValue
		else if (PN = FExprNode) or (PN = FLblNode) then ep := epExpression
    else if PN = FCheckExprNode then ep := epCheckExpression
    else if PN = FListFltNode then ep := epListFilter
    else Exit;
  	FOnEditComponent(Self, C, ep);
    UpdateMemo(N);
  end;
end;

{procedure TSummaryTree.MenuHandler(Sender: TObject);
begin
  if TComponent(Sender).Tag = 0 then
  	BuildTree;
end;  }

function TSummaryTree.FilterFilterItem(Item: Pointer; out Done: Boolean
  ): Boolean;
var
  SearchText, Expr, NodeText: String;
  D: TSummaryData;
begin
  if Filter.Text = '' then Exit;

  Done := True;
  D := TSummaryData(Item);
  if D = nil then Exit(False);

  SearchText := Utf8LowerCase(Filter.Text);
  Expr := Utf8LowerCase( GetExprByNode(D.ParentNode, D.Cmp) );
  NodeText := Utf8LowerCase(D.Node.Text);
  Done := True;
  Result := (Utf8Pos(SearchText, Expr) > 0) or (Utf8Pos(SearchText, NodeText) > 0);
end;

procedure TSummaryTree.FilterAfterFilter(Sender: TObject);
begin
  if Tree.Selected = nil then
  begin
	  FMemo.Text := '';
  	FMemo.Hint := '';
  end;
end;

{procedure TSummaryTree.MenuPopup(Sender: TObject);
begin
  Tree.PopupMenu.Items[0].Enabled:=FForm <> nil;
end;  }

constructor TSummaryTree.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  FData := TList.Create;

  with Tree do
  begin
    OnSelectionChanged:=@TreeSelectionChanged;
    OnDblClick:=@TreeDblClick;
  end;

  with Filter do
  begin
    TextHint:=rsFindFieldOrExpr;
    BorderSpacing.Bottom := 2;
    OnFilterItem := @FilterFilterItem;
    OnAfterFilter := @FilterAfterFilter;
  end;

  Tree.Images := TImageList.Create(Self);
  SetupImageList(Tree.Images, ['form16', 'folder16', 'text16', 'calc16', 'date16',
    'clock16', 'memo16', 'checkbox16', 'combobox16', 'object16', 'label16',
    'counter16', 'objectfield16', 'shape16', 'button16', 'query16', 'grid16',
    'dbimage16', 'image16', 'tab16', 'tabs16', 'pivottable16', 'groupbox16',
    'file16', 'chart16', 'key16']);

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
end;

destructor TSummaryTree.Destroy;
begin
  ClearList(FData);
  FData.Free;
  inherited Destroy;
end;

procedure TSummaryTree.UpdateTree;
begin
  BuildTree;
end;

procedure TSummaryTree.RenameComponent(C: TComponent);
var
  i: Integer;
  D: TSummaryData;
  N: TTreeNode;
begin
  Tree.BeginUpdate;
  for i := 0 to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    if not (TObject(N.Data) is TSummaryData) then Continue;

    D := TSummaryData(N.Data);
    if D.Cmp = C then
    begin
      if C is TdxLabel then N.Text := TdxLabel(C).FieldName
      else N.Text := GetFieldName(C);
    end;
  end;
  Tree.EndUpdate;
end;

procedure TSummaryTree.ClearTree;
begin
  ClearList(FData);
  Tree.ClearTree;
  FDefValNode := nil;
  FExprNode := nil;
  FCheckExprNode := nil;
  FReqNode := nil;
  FLblNode := nil;
  FListFltNode := nil;
  FHiddenNode := nil;
  FMemo.Text := '';
end;

procedure TSummaryTree.BuildTree;
var
  i: Integer;
  C: TComponent;
  N, NN: TTreeNode;

  function AddTreeNode(AParent: TTreeNode; Cmp: TComponent): TTreeNode;
  var
    j: Integer;
    S: String;
    D: TSummaryData;
  begin
    D := TSummaryData.Create;
    D.Cmp := Cmp;
    D.ParentNode := AParent;
    FData.Add(D);

    if Cmp is TdxLabel then
    	S := TdxLabel(Cmp).FieldName
    else if HasFId(Cmp) then
	    S := GetFieldName(Cmp)
    else
      S := GetComponentName(Cmp);

    for j := 0 to AParent.Count - 1 do
    begin
      if MyUtf8CompareText(S, AParent.Items[j].Text) < 0 then
      begin
        Result := Tree.Items.InsertObject(AParent.Items[j],
        	S, D);
        D.Node := Result;
        SetImageIdx(Result, GetImageIdx(Cmp));
        Exit;
      end;
    end;
    Result := Tree.Items.AddChildObject(AParent, S, D);
    D.Node := Result;
    SetImageIdx(Result, GetImageIdx(Cmp));
  end;

begin
  inherited BuildTree;
  if FForm = nil then Exit;
  Tree.BeginUpdate;
  N := Tree.Items.AddChild(nil, FForm.FormCaption);
  SetImageIdx(N, 0);
  FDefValNode := Tree.Items.AddChild(N, rsDefaultValue);
  FExprNode := Tree.Items.AddChild(N, rsExpression);
  FCheckExprNode := Tree.Items.AddChild(N, rsCheckValue);
  FReqNode := Tree.Items.AddChild(N, rsRequired);
  FLblNode := Tree.Items.AddChild(N, rsCalcLabels);
  FListFltNode := Tree.Items.AddChild(N, rsListFilter);
  FHiddenNode := Tree.Items.AddChild(N, rsHiddenComponents);
  for i := 0 to FForm.ComponentCount - 1 do
  begin
    C := FForm.Components[i];

    if GetHidden(C) then
      AddTreeNode(FHiddenNode, C);
    if not HasFId(C) then
    begin
      if (C is TdxLabel) and (Trim(TdxLabel(C).Expression) <> '') then
        AddTreeNode(FLblNode, C);

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

  for i := N.Count - 1 downto 0 do
  begin
    NN := N.Items[i];
		if NN.Count = 0 then
    	NN.Delete
    else
    	SetImageIdx(NN, 1);
  end;
  N.Expand(True);
  Tree.EndUpdate;
  Tree.Selected := nil;
end;

procedure TSummaryTree.LoadTree(aForm: TdxForm);
begin
  FForm := aForm;
  BuildTree;
end;

procedure TSummaryTree.ClearAll;
begin
  ClearTree;
  Filter.Text := '';
  FForm := nil;
end;

function TSummaryTree.GetExprByNode(PN: TTreeNode; C: TComponent): String;
var
  S: String;
begin
	if PN = FListFltNode then
    S := GetComboFilter(C)
  else if PN = FLblNode then
    S := TdxLabel(C).Expression
  else if PN = FCheckExprNode then
    S := GetCheckExpression(C)
  else if PN = FExprNode then
    S := GetExpression(C)
  else if PN = FDefValNode then
	  S := GetDefaultValue(C);
  Result := S;
end;

procedure TSummaryTree.UpdateMemo(N: TTreeNode);
var
  D: TSummaryData;
  S: String;
begin
  FMemo.Text := '';
  FMemo.Hint := '';
  D := TSummaryData(N.Data);
  if (D = nil) or (not N.Visible) then Exit;
  S := GetExprByNode(D.ParentNode, D.Cmp);
  FMemo.Text := S;
  FMemo.Hint := StringReplace(S, '|', '/', [rfReplaceAll]);
end;

end.

