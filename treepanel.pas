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

unit TreePanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, ComCtrls, TreeFilterEdit,
  Buttons, LclType, dxctrls, treeviewex, strconsts;

type

  { TTreePanel }

  { TTreeFilterEditEx }

  TTreeFilterEditEx = class(TTreeFilterEdit)
  public
    procedure StoreSelection; override;
  end;

  TTreePanel = class(TCustomControl)
  private
    FTree: TTreeViewEx;
    FFilter: TTreeFilterEdit;
    FNextBn, FBackBn: TSpeedButton;
    procedure ButtonClick(Sender: TObject);
    procedure TreeStateChanged(Sender: TObject);
    procedure TreeUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildTree; virtual;
    property Tree: TTreeViewEx read FTree;
    property Filter: TTreeFilterEdit read FFilter;
  end;

implementation

uses
  appsettings;

{ TTreeFilterEditEx }

// Когда выделен самый верхний узел, то при сбросе фильтра выделение этого узла
// сбрасывается. Чтобы этого не произошло переопределяем метод. Все то же самое,
// только наоборот.
procedure TTreeFilterEditEx.StoreSelection;
var
  ANode: TTreeNode;
begin
  inherited StoreSelection;
  if FilteredTreeview = nil then Exit;
  ANode := FilteredTreeview.Selected;
  if ANode = nil then Exit;
  if ANode <> FilteredTreeview.Items.GetFirstVisibleNode then Exit;
  SelectionList.Clear;       // Clear old selection only if there is new one.
  SelectionList.Add(ANode.Text);
end;

{ TTreePanel }

procedure TTreePanel.TreeUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char
  );
begin
  if Utf8Key < ' ' then Exit;
  FFilter.SetFocus;
  FFilter.Text:=Utf8Key;
  FFilter.SelStart := 1;
end;

procedure TTreePanel.ButtonClick(Sender: TObject);
begin
  if Sender = FBackBn then
  begin
    if not FTree.GoBackNode and (FFilter.Text <> '') then
    begin
      FFilter.ForceFilter('');
      FFilter.Text := '';
      FTree.GoBackNode;
      TreeStateChanged(FTree);
    end;
  end
  else if Sender = FNextBn then
  begin
    if not FTree.GoNextNode and (FFilter.Text <> '') then
    begin
      FFilter.ForceFilter('');
      FFilter.Text := '';
      FTree.GoNextNode;
      TreeStateChanged(FTree);
    end;
  end;
end;

procedure TTreePanel.TreeStateChanged(Sender: TObject);
begin
  FBackBn.Enabled := FTree.CanBackNode;
  FNextBn.Enabled := FTree.CanNextNode;
end;

constructor TTreePanel.Create(AOwner: TComponent);
var
  Pan: TPanel;
begin
  inherited Create(AOwner);

  FTree := TTreeViewEx.Create(Self);
  with FTree do
  begin
    Parent := Self;
    Align := alClient;
    ReadOnly := True;
    MultiSelect := False;
    RightClickSelect := True;
    IsWine := AppConfig.IsWine;
    OnUTF8KeyPress:=@TreeUtf8KeyPress;
    OnStateChanged:=@TreeStateChanged;
  end;

  Pan := TPanel.Create(Self);
  with Pan do
  begin
    Parent := Self;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Caption := '';
    Align := alTop;
    AutoSize := True;
    TabOrder := 0;
  end;

  FFilter := TTreeFilterEditEx.Create(Self);
  with FFilter do
  begin
    Parent := Pan;
    Align:=alClient;
    Flat:=True;
    CharCase:=ecNormal;
    FilteredTreeview := FTree;
    BorderSpacing.Top := 2;
    BorderSpacing.Bottom := 2;
  end;

  FNextBn := TSpeedButton.Create(Self);
  with FNextBn do
  begin
    Parent := Pan;
    Width := 25;
    Align := alLeft;
    Flat := True;
    LoadGlyphFromLazarusResource('right16_2');
    Hint := rsGoToNextItem;
    ShowHint := True;
    OnClick:=@ButtonClick;
  end;

  FBackBn := TSpeedButton.Create(Self);
  with FBackBn do
  begin
    Parent := Pan;
    Width := 25;
    Align := alLeft;
    Flat := True;
    LoadGlyphFromLazarusResource('left16_2');
    Hint := rsReturnToPrevItem;
    ShowHint := True;
    OnClick := @ButtonClick;
  end;
end;

procedure TTreePanel.BuildTree;
begin
  Tree.ClearTree;
  Filter.Text := '';
  TreeStateChanged(Tree);
end;

end.

