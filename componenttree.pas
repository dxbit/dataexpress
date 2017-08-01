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
unit ComponentTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Buttons, Menus, dxctrls, strconsts,
  EditBtn, LclType;

type
  TCTSelectComponentEvent = procedure (Sender: TObject; Cmp: TControl) of object;

  { TComponentTree }

  TComponentTree = class(TCustomControl)
  private
    FExpertMode: Boolean;
    FForm: TdxForm;
    FOnSelectComponent: TCTSelectComponentEvent;
    FTree: TTreeView;
    FDisableSelect: Boolean;
    FSearchEdit: TEditButton;
    FSearchText: String;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure SearchEditButtonClick(Sender: TObject);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeSelectionChanged(Sender: TObject);
    procedure FilterTree;
    procedure BuildTree;
    procedure ClearTree;
    procedure TreeUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTree;
    procedure LoadTree(aForm: TdxForm);
    procedure ClearAll;
    procedure SelectComponent(C: TControl);
    property Form: TdxForm read FForm;
    property OnSelectComponent: TCTSelectComponentEvent read FOnSelectComponent
			write FOnSelectComponent;
    property ExpertMode: Boolean read FExpertMode write FExpertMode;
  end;

implementation

uses
  myctrls, dxreports, dxfiles, dximages, pivotgrid, JvDesignImp, apputils,
  LazUtf8;

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
  else if C is TdxFile then n := 22;
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
    N := FTree.Items.AddChildObject(ParentNode, S, Control);
    SetImageIdx(N, GetImageIdx(Control));

    if not (Control is TWinControl) then Exit;
    WC := TWinControl(Control);
    for i := 0 to WC.ControlCount - 1 do
    	_Build(N, WC.Controls[i]);
  end;

begin
  FTree.BeginUpdate;
	ClearTree;
  if FForm <> nil then
  begin
	  _Build(nil, FForm);
  	FilterTree;
    SetImageIdx(FTree.Items[0], 0);
	  FTree.Items[0].Expand(True);
  end;
  FTree.EndUpdate;
end;

procedure TComponentTree.ClearTree;
begin
  FTree.Items.Clear;
end;

procedure TComponentTree.TreeUtf8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  FSearchEdit.SetFocus;
  FSearchEdit.Text:=Utf8Key;
  FSearchEdit.SelStart := 2;
end;

procedure TComponentTree.SelectComponent(C: TControl);
var
  N: TTreeNode;
begin
  N := FTree.Items.FindNodeWithData(C);
  if N <> nil then
  begin
    FDisableSelect := True;
    N.Selected := True;
    FDisableSelect := False;
  end;
end;

procedure TComponentTree.TreeSelectionChanged(Sender: TObject);
var
  N: TTreeNode;
begin
  if FDisableSelect then Exit;

  N := FTree.Selected;
  if N = nil then Exit;

  if FOnSelectComponent <> nil then
  	FOnSelectComponent(Self, TControl(N.Data));
end;

procedure TComponentTree.FilterTree;
var
  i: Integer;
  S: String;
  N: TTreeNode;
begin
  if FSearchText = '' then Exit;

  for i := FTree.Items.Count - 1 downto 1 do
  begin
    N := FTree.Items[i];
    S := Utf8LowerCase(N.Text);
    if (Utf8Pos(FSearchText, S, 1) = 0) and (N.Count = 0) then
    	N.Delete;
  end;
end;

procedure TComponentTree.MenuHandler(Sender: TObject);
begin
  if TComponent(Sender).Tag = 0 then
  	UpdateTree;
end;

procedure TComponentTree.MenuPopup(Sender: TObject);
begin
  FTree.PopupMenu.Items[0].Enabled:=FForm <> nil;
end;

procedure TComponentTree.SearchEditButtonClick(Sender: TObject);
begin
  FSearchEdit.Text := '';
  FSearchText := '';
  UpdateTree;
end;

procedure TComponentTree.SearchEditKeyDown(Sender: TObject; var Key: Word;
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

constructor TComponentTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSearchEdit := TEditButton.Create(Self);
  with FSearchEdit do
  begin
    Parent := Self;
    Align := alTop;
    BorderSpacing.Top := 2;
    BorderSpacing.Bottom := 2;
    Flat := True;
    Button.LoadGlyphFromLazarusResource('delete16');
    TextHint := rsSearchComponent;
    OnKeyDown:=@SearchEditKeyDown;
    OnButtonClick:=@SearchEditButtonClick;
  end;

  FTree := TTreeView.Create(Self);
  with FTree do
	begin
    Parent := Self;
    Align := alClient;
    ReadOnly := True;
    OnSelectionChanged := @TreeSelectionChanged;
    OnUTF8KeyPress:=@TreeUtf8KeyPress;
    PopupMenu := TPopupMenu.Create(Self);
    PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsRefresh,
    	0, 0, @MenuHandler, 'refresh16'));
    PopupMenu.OnPopup:=@MenuPopup;
  end;

  FTree.Images := TImageList.Create(Self);
  with FTree.Images do
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
    AddLazarusResource('db16');
    AddLazarusResource('grid16');
    AddLazarusResource('dbimage16');
    AddLazarusResource('image16');
    AddLazarusResource('tab16');
    AddLazarusResource('tabs16');
    AddLazarusResource('pivottable16');
    AddLazarusResource('groupbox16');
    AddLazarusResource('file16');
  end;
end;

procedure TComponentTree.UpdateTree;
var
  N: TTreeNode;
  OldData: Pointer;
begin
  N := FTree.Selected;
  OldData := nil;
  if N <> nil then
  	OldData := N.Data;
  BuildTree;
  if OldData <> nil then
  begin
	  N := FTree.Items.FindNodeWithData(OldData);
    if N <> nil then
    begin
      FDisableSelect:=True;
      N.Selected := True;
      FDisableSelect:=False;
    end;
  end;
end;

procedure TComponentTree.LoadTree(aForm: TdxForm);
begin
  FSearchEdit.Text := '';
  FSearchText := '';
  FForm := aForm;
  BuildTree;
end;

procedure TComponentTree.ClearAll;
begin
  FSearchEdit.Text := '';
  FSearchText := '';
  ClearTree;
  FForm := nil;
end;

end.

