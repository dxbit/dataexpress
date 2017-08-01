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
unit FormView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Forms, dxctrls, datasetprocessor,
  Graphics, ComCtrls, strconsts, MyClasses, Menus, LclType, Db;

type

  { TFormView }

  TFormView = class(TCustomPanel)
    procedure TreeMnuPopup(Sender: TObject);
    procedure TreeUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    FFormId: Integer;
    FOnTreeDblClick: TNotifyEvent;
    FSplitter: TSplitter;
    FScrollBox: TdxScrollBox;
    FForm: TdxForm;
    FGrid: TdxGrid;
    FProc: TDataSetProcessor;
    FPanel: TPanel;
    FTree: TTreeView;
    FTreeSplit: TSplitter;
    FTreeMnu: TPopupMenu;
    FFormWidth, FFormHeight: Integer;
    FOldDSPStateChange: TNotifyEvent;
    procedure CreateLeftLayout;
    procedure CreateTopLayout;
    procedure CreateRightLayout;
    procedure CreateBottomLayout;
    procedure CreateClientLayout;
    procedure CreateNoneLayout;
    procedure DSPStateChange(Sender: TObject);
    procedure SetScrollBoxSize;
    procedure SetViewType;
    procedure BuildTree;
    procedure TreeDblClick(Sender: TObject);
    procedure TreeSelect(Sender: TObject);
    procedure DSPChangeFilter(Sender: TObject);
    procedure TreeMnuClick(Sender: TObject);
    procedure ExpandAll;
    procedure CollapseAll;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BindForm(FmId: Integer; IsListForm: Boolean; aViewType: TViewType);
    procedure ClearSelection(OnEvent: Boolean);
    procedure SelectGroup(GId: Integer);
    property FormId: Integer read FFormId;
    property Form: TdxForm read FForm;
    property DataSetProc: TDataSetProcessor read FProc;
  public
    constructor CreateView(AOwner: TComponent; const FormName: String; ViewType: TViewType);
    property Grid: TdxGrid read FGrid;
    property Tree: TTreeView read FTree;
    property ScrollBox: TdxScrollBox read FScrollBox;
    property TreeSplitter: TSplitter read FTreeSplit;
    property FormSplitter: TSplitter read FSplitter;
    property OnTreeDblClick: TNotifyEvent read FOnTreeDblClick write
			FOnTreeDblClick;
  end;

implementation

uses
  dialogs, sqlgen, dbengine, apputils, myctrls, formmanager;

{ TFormView }

procedure TFormView.TreeSelect(Sender: TObject);
var
  i: Integer;
  N: TTreeNode;
  L: TList;
begin
  L := TList.Create;
  for i := 0 to FTree.SelectionCount - 1 do
  begin
    N := FTree.Selections[i];
    L.Add(N.Data);
  end;
  try
    //DataSetProc.Post;
    DataSetProc.ApplyTreeSelect(L);
  finally
    L.Free;
  end;
end;

procedure TFormView.DSPChangeFilter(Sender: TObject);
var
  j, Id: Integer;
  N: TTreeNode;
  Flt: TFilterObject;
  F: TFilterField;
begin
  if FForm.GroupField = 0 then Exit;
  FTree.OnSelectionChanged:=nil;
  FTree.ClearSelection;
  Flt := DataSetProc.DataSets[0]^.Filter;
  F := Flt.FindField(FForm.GroupField);
  if F <> nil then
  begin
    for j := 0 to F.Values.Count - 1 do
    	if F.Values[j] <> '' then
      begin
        Id := StrToInt(F.Values[j]);
        N := FTree.Items.FindNodeWithData(Pointer(PtrInt(Id)));
        if N <> nil then FTree.Select(N, [ssCtrl]);
      end;
  end;
  FTree.OnSelectionChanged:=@TreeSelect;
end;

procedure TFormView.TreeMnuClick(Sender: TObject);
begin
  case TMenuItem(SendeR).Tag of
    0: FTree.ClearSelection(False);
    2:
      begin
        FTree.ClearSelection(False);
        BuildTree;
      end;
    4: ExpandAll;
    5: CollapseAll;
  end;
end;

procedure TFormView.ExpandAll;
var
  i: Integer;
begin
  for i := 0 to FTree.Items.Count - 1 do
    FTree.Items[i].Expand(False);
end;

procedure TFormView.CollapseAll;
var
  i: Integer;
begin
  for i := 0 to FTree.Items.Count - 1 do
    FTree.Items[i].Collapse(False);
end;

procedure TFormView.TreeMnuPopup(Sender: TObject);
begin
  FTreeMnu.Items[0].Enabled:=FTree.Items.SelectionCount > 0;
  FTreeMnu.Items[4].Enabled := FTree.Items.Count > 0;
  FTreeMnu.Items[5].Enabled := FTree.Items.Count > 0;
end;

procedure TFormView.TreeUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  with TTreeSearchForm.CreateNew(nil) do
    ShowForm(FTree, Utf8Key);
end;

procedure TFormView.CreateLeftLayout;
begin
  FForm.Parent := FScrollBox;
  with FScrollBox do
  begin
    SetScrollBoxSize;
    Align := alRight;
  end;
  with FSplitter do
  begin
    Parent := Self;
    Align := alRight;
  end;
  with FPanel do
  begin
    Parent := Self;
    Align := alClient;
  end;
end;

procedure TFormView.CreateTopLayout;
begin
  FForm.Parent := FScrollBox;
  with FScrollBox do
  begin
    SetScrollBoxSize;
    Align := alBottom;
  end;
  with FSplitter do
  begin
    Parent := Self;
    Align := alBottom;
  end;
  with FPanel do
  begin
    Parent := Self;
    Align := alClient;
  end;
end;

procedure TFormView.CreateRightLayout;
begin
  with FSplitter do
  begin
    Parent := Self;
    Align := alLeft;
  end;
  FForm.Parent := FScrollBox;
  with FScrollBox do
  begin
    SetScrollBoxSize;
    Align := alLeft;
  end;
  with FPanel do
  begin
    Parent := Self;
    Align := alClient;
  end;
end;

procedure TFormView.CreateBottomLayout;
begin
  with FSplitter do
  begin
    Parent := Self;
    Align := alTop;
  end;
  FForm.Parent := FScrollBox;
  with FScrollBox do
  begin
    SetScrollBoxSize;
    Align := alTop;
  end;
  with FPanel do
  begin
    Parent := Self;
    Align := alClient;
  end;
end;

procedure TFormView.CreateClientLayout;
begin
  FPanel.Parent := Self;
  FPanel.Align := alClient;
end;

procedure TFormView.CreateNoneLayout;
begin
  FForm.Parent := FScrollBox;
  with FScrollBox do
  begin
    Align := alClient;
  end;
end;

procedure TFormView.DSPStateChange(Sender: TObject);
begin
  FTree.Enabled:=not (FProc.MasterSet.State in [dsInsert, dsEdit]);
  FOldDSPStateChange(Sender);
end;

procedure TFormView.SetViewType;
begin
  case FForm.ViewType of
    vtGridLeft: CreateLeftLayout;
    vtGridTop: CreateTopLayout;
    vtGridRight: CreateRightLayout;
    vtGridBottom: CreateBottomLayout;
    vtGridOnly: CreateClientLayout;
    vtWithoutGrid, vtSimpleForm: CreateNoneLayout;
  end;
end;

procedure TFormView.BuildTree;
var
  C: TComponent;
  TId, FId, PId: Integer;
  S: String;
  IsTree: Boolean;

  {procedure AddNode(const Path: String; Key: Integer);
  var
    SL: TStringList;
    i: Integer;
    N, PN: TTreeNode;
  begin
    SL := TStringList.Create;
    SplitStr(Path, '\', SL);
    if SL.Count > 0 then
    begin
      N := FTree.Items.FindTopLvlNode(SL[0]);
      if N = nil then
        N := FTree.Items.AddChild(nil, SL[0]);
      for i := 1 to SL.Count - 1 do
      begin
        PN := N;
        N := N.FindNode(SL[i]);
        if N = nil then
          N := FTree.Items.AddChild(PN, SL[i]);
      end;
      N.Data := Pointer(PtrInt(Key));
    end;
    SL.Free;
  end; }

  procedure AddNode(const Path: String; Key, ParentKey: Integer);
  var
    N, PN: TTreeNode;
    SL: TStringList;
  begin
    if IsTree then
    begin
      SL := TStringList.Create;
      SplitStr(Path, '\', SL);
      if SL.Count > 0 then
    	  S := SL[SL.Count - 1]
      else
    	  S := '';
      SL.Free;
    end
    else S := Path;
    if ParentKey = 0 then PN := nil
    else PN := FTree.Items.FindNodeWithData(Pointer(ParentKey));
    N := FTree.Items.AddChild(PN, S);
    N.Data := Pointer(PtrInt(Key));
  end;

begin
  FTree.Items.Clear;
  if FForm.GroupField = 0 then Exit;
  C := FindById(FForm, FForm.GroupField);
  TId := GetSourceTId(C);
  FId := GetSourceFId(C);
  if (TId = 0) or (FId = 0) then Exit;
  S := SqlSelectGroupsForTree(TId, FId);
  IsTree := S <> '';
  if not IsTree then S := 'select id, ' + FieldStr(FId) + ' from ' + TableStr(TId);
  S := S + ' order by 2';
  with DBase.OpenDataSet(S) do
  try
    while not Eof do
    begin
      if IsTree then PId := Fields[2].AsInteger
    	else PId := 0;
      AddNode(Fields[1].AsString, Fields[0].AsInteger, PId);
      Next;
    end;
  finally
    Free;
  end;
  ExpandAll;
end;

procedure TFormView.TreeDblClick(Sender: TObject);
begin
  if FOnTreeDblClick <> nil then
  	FOnTreeDblClick(Self);
end;

procedure TFormView.SetScrollBoxSize;
var
  W, H: Integer;
begin
  W := FFormWidth;
  H := FFormHeight;
  FScrollBox.SetBounds(0, 0, W, H);
end;

constructor TFormView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  BevelOuter := bvnone;
  FProc := TDataSetProcessor.Create;
  FSplitter := TSplitter.Create(Self);
  with FSplitter do
  begin
    ResizeStyle:=rsPattern;
    Visible := True;
  end;
  FScrollBox := TdxScrollBox.Create(Self);
  with FScrollBox do
  begin
    BorderStyle:=bsNone;
    Parent := Self;
    Visible := True;
    Left := -1;
    Width := 0; Height := 0;
  end;
  FPanel := TPanel.Create(Self);
  FPanel.Caption:='';
  FPanel.BevelInner:=bvNone;
  FPanel.BevelOuter:=bvNone;
  FTreeSplit := TSplitter.Create(Self);
  FTreeSplit.Parent := FPanel;
  FTreeSplit.Align := alLeft;
  FTreeSplit.ResizeStyle:=rsPattern;
  FTree := TTreeView.Create(Self);
  FTree.Parent := FPanel;
  FTree.Align := alLeft;
  FTree.Width := 200;
  //FTree.RightClickSelect:=True;
  FTree.ReadOnly:=True;
  FTree.Options := FTree.Options - [tvoThemedDraw];
  FTree.RowSelect:=True;
  FTree.MultiSelect:=True;
  FTree.OnSelectionChanged:=@TreeSelect;
  FTree.OnDblClick:=@TreeDblClick;
  FTree.OnUTF8KeyPress:=@TreeUtf8KeyPress;
  FTreeMnu := TPopupMenu.Create(Self);
  FTreeMnu.Items.Add( CreateMenuItem(FTreeMnu, rsClearSelection, 0, 0,
    @TreeMnuClick, '') );
  FTreeMnu.Items.Add( CreateMenuItem(FTreeMnu, '-', 1, 0, nil, '') );
  FTreeMnu.Items.Add( CreateMenuItem(FTreeMnu, rsRefresh, 2, 0, @TreeMnuClick, '') );
  FTreeMnu.Items.Add( CreateMenuItem(FTreeMnu, '-', 3, 0, nil, '') );
  FTreeMnu.Items.Add( CreateMenuItem(FTreeMnu, rsExpandAll, 4, 0,
    @TreeMnuClick, '') );
  FTreeMnu.Items.Add( CreateMenuItem(FTreeMnu, rsCollapseAll, 5, 0,
    @TreeMnuClick, '') );
  FTreeMnu.OnPopup:=@TreeMnuPopup;
  FTree.PopupMenu := FTreeMnu;
end;

destructor TFormView.Destroy;
begin
  FreeAndNil(FProc);
  inherited Destroy;
end;

procedure TFormView.BindForm(FmId: Integer; IsListForm: Boolean;
  aViewType: TViewType);
begin
  FFormId:=FmId;
  FProc.BindForm(FFormId, IsListForm, aViewType);
  FForm := FProc.Form;
  FFormWidth := FForm.Width;
  FFormHeight := FForm.Height;
  if (not FForm.ShowScrollBars) and (FForm.ViewType <> vtGridOnly) then
  begin
    FScrollBox.AutoScroll:=False;
    FForm.Align := alClient;
  end;
  if FForm.Color <> clDefault then
    FScrollBox.Color:=FForm.Color
  else
  begin
    FScrollBox.ParentColor := True;
    FForm.ParentColor := True;
  end;
  FGrid := FForm.Grid;
  FGrid.ParentFont := False;
  FGrid.Parent := FPanel;
  FGrid.Align := alClient;
  FTree.Font.Assign(FForm.TreeFont);
  FTree.BackgroundColor:=FForm.TreeBackColor;
  FTree.TreeLineColor:=FForm.TreeLineColor;
  FTree.SelectionColor:=FForm.TreeSelectColor;
  FTree.Width:=FForm.TreeWidth;
  FTree.Visible := FForm.GroupField > 0;
  FTreeSplit.Visible := FForm.GroupField > 0;
  BuildTree;
  if FForm.ViewType <> vtGridOnly then
    FForm.Parent := FScrollBox;
  FForm.Left := 0; FForm.Top:=0;
  ParentFont := False;
  SetViewType;
  DataSetProc.OnChangeFilter:=@DSPChangeFilter;
  FOldDSPStateChange := DataSetProc.OnStateChange;
  DataSetProc.OnStateChange:=@DSPStateChange;
end;

procedure TFormView.ClearSelection(OnEvent: Boolean);
begin
  if not OnEvent then FTree.OnSelectionChanged:=nil;
  FTree.ClearSelection(False);
  FTree.OnSelectionChanged:=@TreeSelect;
end;

procedure TFormView.SelectGroup(GId: Integer);
var
  N: TTreeNode;
begin
  {FTree.OnSelectionChanged:=nil;
  ClearSelection;}
  N := FTree.Items.FindNodeWithData(Pointer(GId));
  if N <> nil then N.Selected:=True;
  //FTree.OnSelectionChanged:=@TreeSelect;
end;

constructor TFormView.CreateView(AOwner: TComponent; const FormName: String;
  ViewType: TViewType);
var
  Fm: TdxForm;
begin
  Fm := FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);
  if Fm.PId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormName]);
  Create(AOwner);
  BindForm(Fm.Id, False, ViewType);
end;

end.

