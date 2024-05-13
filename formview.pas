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

unit FormView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Forms, dxctrls, datasetprocessor,
  Graphics, ComCtrls, strconsts, Menus, LclType, Math;

type

  { TFormView }

  TFormView = class(TCustomPanel)
  private
    FFormId: Integer;
    FSplitter: TSplitter;
    FScrollBox: TdxScrollBox;
    FForm: TdxForm;
    FGrid: TdxGrid;
    FProc: TDataSetProcessor;
    FPanel: TPanel;
    FTree: TdxFormTree;
    FTreeSplit: TSplitter;
    FFormWidth, FFormHeight: Integer;
    procedure CreateLeftLayout;
    procedure CreateTopLayout;
    procedure CreateRightLayout;
    procedure CreateBottomLayout;
    procedure CreateClientLayout;
    procedure CreateNoneLayout;
    procedure SetScrollBoxSize;
    procedure SetViewType;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BindForm(FmId: Integer; IsListForm: Boolean; aViewType: TViewType);
    property FormId: Integer read FFormId;
    property Form: TdxForm read FForm;
    property DataSetProc: TDataSetProcessor read FProc;
  public
    constructor CreateView(AOwner: TComponent; const FormName: String; ViewType: TViewType);
    property Grid: TdxGrid read FGrid;
    property Tree: TdxFormTree read FTree;
    property ScrollBox: TdxScrollBox read FScrollBox;
    property TreeSplitter: TSplitter read FTreeSplit;
    property FormSplitter: TSplitter read FSplitter;
  end;

implementation

uses
  dialogs, myctrls, formmanager, apputils;

{ TFormView }

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
    Constraints.MinHeight:=100;
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

procedure TFormView.SetScrollBoxSize;
var
  W, H: Integer;
  Fm: TForm;
begin
  W := FFormWidth;
  H := FFormHeight;
  Fm := TForm(GetTopParent);
  if (Fm <> nil) and (Fm is TForm) then
  begin
    W := Min(W, Fm.ClientWidth - ScaleToScreen(150));
    H := Min(H, Fm.ClientHeight - ScaleToScreen(250));
    {if W > Fm.ClientWidth - ScaleToScreen(150) then
      W := Fm.ClientWidth - ScaleToScreen(150);
    if H > Fm.ClientHeight - ScaleToScreen(250) then
      H := Fm.ClientHeight - ScaleToScreen(250); }
  end;
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
    HorzScrollBar.Smooth:=True;
    HorzScrollBar.Tracking:=True;
    VertScrollBar.Smooth:=True;
    VertScrollBar.Tracking:=True;
  end;
  FPanel := TPanel.Create(Self);
  FPanel.Caption:='';
  FPanel.BevelInner:=bvNone;
  FPanel.BevelOuter:=bvNone;
  FTreeSplit := TSplitter.Create(Self);
  FTreeSplit.Parent := FPanel;
  FTreeSplit.Align := alLeft;
  FTreeSplit.ResizeStyle:=rsPattern;
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
  FTree := FForm.Tree;
  FTree.Parent := FPanel;
  FTree.Align := alLeft;
  FTree.Visible := FForm.Tree.Fields.Count > 0;
  FTreeSplit.Visible:=FTree.Visible;
  if FForm.ViewType <> vtGridOnly then
    FForm.Parent := FScrollBox;
  FForm.Left := 0; FForm.Top:=0;
  ParentFont := False;
  SetViewType;
  FTree.TabOrder := 0;
  FGrid.TabOrder := 1;
  FScrollBox.TabOrder := 2;
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

