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

unit SelectActionForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ButtonPanel, dxactions, strconsts, LCLType,
  scriptmanager, TreeViewEx;

{ TSelectActionFm }

type
  TSelectActionFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Filter: TTreeFilterEdit;
    ImageList1: TImageList;
    Tree: TTreeViewEx;
    procedure FilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeCompare(Sender: TObject; Node1, Node2: TTreeNode;
      var Compare: Integer);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeSelectionChanged(Sender: TObject);
    procedure TreeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { private declarations }
    FStdNode: TTreeNode;
    FTargets: TActionTargets;
    procedure BuildTree;
    procedure SetControlState;
  public
    { public declarations }
    function ShowForm(var AAction: TExprAction;	var ActionType: TdxActionType;
      Targets: TActionTargets): Integer;
  end;

var
  SelectActionFm: TSelectActionFm;

function ShowSelectActionForm(var AAction: TExprAction;	var ActionType: TdxActionType;
  Targets: TActionTargets): Integer;

implementation

uses
  apputils, LazUtf8, appsettings;

function ShowSelectActionForm(var AAction: TExprAction;
  var ActionType: TdxActionType; Targets: TActionTargets): Integer;
begin
  if SelectActionFm = nil then
  	SelectActionFm := TSelectActionFm.Create(Application);
  Result := SelectActionFm.ShowForm(AAction, ActionType, Targets);
end;

{$R *.lfm}

{ TSelectActionFm }

procedure TSelectActionFm.FormShow(Sender: TObject);
begin
  Tree.SetFocus;
end;

procedure TSelectActionFm.TreeCompare(Sender: TObject; Node1, Node2: TTreeNode;
  var Compare: Integer);
begin
  if (Node1.Data <> nil) and (Node2.Data = nil) then Compare := -1
  else if (Node1.Data = nil) and (Node2.Data <> nil) then Compare := 1
  else Compare := Utf8CompareText(Node1.Text, Node2.Text);
end;

procedure TSelectActionFm.TreeDblClick(Sender: TObject);
begin
  if (Tree.Selected <> nil) and
  	(Tree.Selected.Data <> nil) then ModalResult := mrOk;
end;

procedure TSelectActionFm.TreeSelectionChanged(Sender: TObject);
begin
  SetControlState;
end;

procedure TSelectActionFm.TreeUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if Utf8Key < #32 then Exit;

  Filter.Text := UTF8Key;
  Filter.SetFocus;
  Filter.SelStart := Length(UTF8Key);
  Filter.SelLength := 0;
end;

procedure TSelectActionFm.FormCreate(Sender: TObject);
begin
  Caption := rsSelectAction;
  Filter.TextHint:=rsTreeFilter;
  ButtonPanel1.OkButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  Tree.IsWine := AppConfig.IsWine;
end;

procedure TSelectActionFm.FilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DOWN then
  begin
    Key := 0;
    Tree.SetFocus;
  end;
end;

procedure TSelectActionFm.BuildTree;
var
  i, j: Integer;
  A: TExprAction;
  PN, N: TTreeNode;
  SL: TStringList;
begin
  SL := TStringList.Create;
  with Tree.Items do
  begin
    Clear;
    FStdNode := AddChild(nil, rsBuiltInActions);
    if atButton in FTargets then
    begin
      AddChildObject(FStdNode, rsGoToForm, Pointer(actGotoForm));
      AddChildObject(FStdNode, rsPrint, Pointer(actPrint));
      AddChildObject(FStdNode, rsMassCalc, Pointer(actMassCalc));
      AddChildObject(FStdNode, rsOpenReport, Pointer(actOpenReport));
      AddChildObject(FStdNode, rsSaveChanges, Pointer(actSaveChanges));
      AddChildObject(FStdNode, rsUserMonitor, Pointer(actUserMonitor));
      AddChildObject(FStdNode, rsClearFields, Pointer(actClearFields));
    end;
    AddChildObject(FStdNode, rsCallFunction, Pointer(actCallFunc));
    AddChildObject(FStdNode, rsShowMessage, Pointer(actShowMessage));
    FStdNode.Expand(False);
    FStdNode.ImageIndex:=0;
    FStdNode.SelectedIndex:=0;
  end;

  for i := 0 to ScriptMan.Actions.Count - 1 do
  begin
    A := ScriptMan.Actions[i];
    if not (A.Target in FTargets) then Continue;

    PN := nil;
    SplitStr(A.Group, '/', SL);
    for j := 0 to SL.Count - 1 do
    begin
      if PN = nil then
        N := Tree.Items.FindTopLvlNode(SL[j])
      else
        N := PN.FindNode(SL[j]);

      if N = nil then
      begin
        N := Tree.Items.AddChild(PN, SL[j]);
        N.ImageIndex:=0;
	      N.SelectedIndex:=0;
        if PN <> nil then PN.Expand(False);
      end;
      PN := N;
    end;
    N := Tree.Items.AddChild(PN, A.Name);
    if not PN.Expanded then PN.Expand(False);
    N.Data := A;
  end;

  Tree.SortType:=stNone;
  Tree.SortType:=stText;
  SL.Free;
end;

procedure TSelectActionFm.SetControlState;
begin
  ButtonPanel1.OkButton.Enabled := (Tree.Selected <> nil) and
  	(Tree.Selected.Data <> nil);
end;

function TSelectActionFm.ShowForm(var AAction: TExprAction;
  var ActionType: TdxActionType; Targets: TActionTargets): Integer;
var
  N: TTreeNode;
begin
  FTargets := Targets;
  Filter.Clear;
  BuildTree;
  if ActionType = actCustom then
    N := Tree.Items.FindNodeWithData(AAction)
  else
    N := Tree.Items.FindNodeWithData(Pointer(PtrInt(ActionType)));
  if N <> nil then N.Selected := True;

  Result := ShowModal;

  if Result = mrOk then
  begin
  	N := Tree.Selected;
    if N.Parent = FStdNode then
      ActionType := TdxActionType(PtrInt(N.Data))
    else
    begin
      ActionType := actCustom;
      AAction := TExprAction(N.Data);
    end;
  end;
end;

end.

