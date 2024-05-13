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

unit ComponentTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Buttons, Menus, dxctrls, strconsts,
  EditBtn, LclType, StdCtrls, myctrls, treepanel;

type
  TCTSelectComponentEvent = procedure (Sender: TObject; Cmp: array of TObject) of object;

  { TComponentTree }

  TComponentTree = class(TTreePanel)
  private
    FExpertMode: Boolean;
    FForm: TdxForm;
    FOnSelectComponent: TCTSelectComponentEvent;
    FDisableSelect: Boolean;
    procedure TreeSelectionChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTree;
    procedure BuildTree; override;
    procedure LoadTree(aForm: TdxForm);
    procedure ClearAll;
    procedure SelectRoot;
    procedure SelectComponents(Arr: array of TObject);
    procedure RenameComponent(C: TComponent);
    property Form: TdxForm read FForm;
    property OnSelectComponent: TCTSelectComponentEvent read FOnSelectComponent
			write FOnSelectComponent;
    property ExpertMode: Boolean read FExpertMode write FExpertMode;
  end;

implementation

uses
  dxreports, dxfiles, dximages, pivotgrid, JvDesignImp, LazUtf8, dxcharts;

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
  else if C is TdxCounter then n := 10
  else if C is TdxObjectField then n := 11
	else if C is TdxShape then n := 12
 	else if C is TdxButton then n := 13
	else if C is TdxQueryGrid then n := 14
  else if C is TdxGrid then n := 15
  else if C is TdxDBImage then n := 16
  else if C is TdxImage then n := 17
  else if C is TdxTabSheet then n := 18
  else if C is TdxPageControl then n := 19
  else if C is TdxPivotGrid then n := 20
  else if C is TdxGroupBox then n := 21
  else if C is TdxFile then n := 22
  else if C is TdxChart then n := 23
  else if C is TdxRecordId then n := 24;
  Result := n;
end;

procedure SetImageIdx(N: TTreeNode; i: Integer);
begin
  N.ImageIndex:=i;
  N.SelectedIndex:=i;
end;

{ TComponentTree }

procedure TComponentTree.BuildTree;

	procedure _Build(ParentNode: TTreeNode; Control: TControl);
  var
    S: String;
    WC: TWinControl;
    N: TTreeNode;
    i: Integer;
  begin
    if (Control is TSpeedButton) or (Control is TGridButtons) or
    	(Control is TJvDesignHandle) then Exit;

    S := GetComponentName(Control);
    N := Tree.Items.AddChildObject(ParentNode, S, Control);
    SetImageIdx(N, GetImageIdx(Control));

    if not (Control is TWinControl) then Exit;
    WC := TWinControl(Control);
    for i := 0 to WC.ControlCount - 1 do
    	_Build(N, WC.Controls[i]);
  end;

begin
  inherited BuildTree;

  Tree.BeginUpdate;
  {ClearTree;
  Filter.Text := ''; }
  if FForm <> nil then
  begin
	  _Build(nil, FForm);
    SetImageIdx(Tree.Items[0], 0);
	  Tree.Items[0].Expand(True);
  end;
  Tree.EndUpdate;
  Tree.Selected := nil;
end;

procedure TComponentTree.SelectComponents(Arr: array of TObject);
var
  i: Integer;
  N: TTreeNode;
begin
  if FDisableSelect then Exit;
  FDisableSelect := True;
  Tree.BeginUpdate;
  Tree.ClearSelection;
  for i := 0 to Length(Arr) - 1 do
  begin
    N := Tree.Items.FindNodeWithData(Arr[i]);
    if N <> nil then N.Selected := True;
  end;
  Tree.EndUpdate;
  FDisableSelect := False;
end;

procedure TComponentTree.RenameComponent(C: TComponent);
var
  N: TTreeNode;
begin
  N := Tree.Items.FindNodeWithData(C);
  if N <> nil then N.Text := GetComponentName(C);
end;

procedure TComponentTree.TreeSelectionChanged(Sender: TObject);

  function ParentNodeSelected(N: TTreeNode): Boolean;
  begin
    Result := False;
    while N <> nil do
    begin
      if N.Selected or N.MultiSelected then Exit(True);
      N := N.Parent;
    end;
  end;

var
  N: TTreeNode;
  Arr: array of TObject;
  i: Integer;
begin
  if FDisableSelect then Exit;

  if Tree.SelectionCount <= 0 then Exit;

  // В обработчике происходит выделение компонентов в дизайнере и чтобы
  // SelectComponents не срабатывал лишний раз, устанавливает этот флаг
  FDisableSelect := True;

  for i := 0 to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    if (N.Selected or N.MultiSelected) and ParentNodeSelected(N.Parent) then
    begin
      N.Selected := False;
      N.MultiSelected := False;
    end;
  end;

  if FOnSelectComponent <> nil then
  begin
    SetLength(Arr, Tree.SelectionCount);
    for i := 0 to Tree.SelectionCount - 1 do
      Arr[i] := TObject(Tree.Selections[i].Data);
  	FOnSelectComponent(Self, Arr);
    SetLength(Arr, 0);
  end;

  FDisableSelect := False;
end;

{procedure TComponentTree.MenuHandler(Sender: TObject);
begin
  if TComponent(Sender).Tag = 0 then
  	UpdateTree;
end;

procedure TComponentTree.MenuPopup(Sender: TObject);
begin
  Tree.PopupMenu.Items[0].Enabled:=FForm <> nil;
end;    }

constructor TComponentTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with Tree do
	begin
    MultiSelect := True;
    OnSelectionChanged := @TreeSelectionChanged;
  end;

  with Filter do
  begin
    TextHint:=rsFindComponent;
    BorderSpacing.Bottom := 2;
  end;

  Tree.Images := TImageList.Create(Self);
  with Tree.Images do
	begin
    AddLazarusResource('form16');
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
    AddLazarusResource('objectfield16');
    AddLazarusResource('shape16');
    AddLazarusResource('button16');
    AddLazarusResource('query16');
    AddLazarusResource('grid16');
    AddLazarusResource('dbimage16');
    AddLazarusResource('image16');
    AddLazarusResource('tab16');
    AddLazarusResource('tabs16');
    AddLazarusResource('pivottable16');
    AddLazarusResource('groupbox16');
    AddLazarusResource('file16');
    AddLazarusResource('chart16');
    AddLazarusResource('key16');
  end;
end;

procedure TComponentTree.UpdateTree;
begin
  BuildTree;
end;

procedure TComponentTree.LoadTree(aForm: TdxForm);
begin
  FForm := aForm;
  BuildTree;
end;

procedure TComponentTree.ClearAll;
begin
  Tree.ClearTree;
  Filter.Text := '';
  FForm := nil;
end;

procedure TComponentTree.SelectRoot;
begin
  if FDisableSelect or (Tree.Items.Count = 0) then Exit;
  FDisableSelect := True;
  Tree.ClearSelection;
  Tree.Items[0].Selected:=True;
  FDisableSelect := False;
end;

end.

