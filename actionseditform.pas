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

unit ActionsEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, ButtonPanel, Menus, StdCtrls,
  dxactions, actionpans, strconsts, scriptmanager, dxctrls, treeviewex;

{ TActionsEditFm }

type

  { TChangeEventHunter }

  TChangeEventHunter = class
  private
  	FControls: TList;
    FEvents: array of TNotifyEvent;
    FModified: Boolean;
    FComp: TComponent;
    procedure ControlChange(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure BindComponent(Comp: TComponent);
    property Modified: Boolean read FModified write FModified;
  end;

  TActionsEditFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Link: TLabel;
    Help: TIpHtmlPanel;
    Image1: TImage;
    ImageList1: TImageList;
    Grp: TLabel;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    N1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    HelpPan: TPanel;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    ScrollBox1: TScrollBox;
    HelpSplitter: TSplitter;
    TreeSplitter: TSplitter;
    Title: TLabel;
    Params: TLabel;
    Tree: TTreeViewEx;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure LinkClick(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeEnter(Sender: TObject);
    procedure TreeExit(Sender: TObject);
    procedure TreeKeyPress(Sender: TObject; var Key: char);
    procedure TreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FActionRunner: TActionRunner;
    FForm: TdxForm;
    FPan: TBasicActionPanel;
    FOldNode, FDragNode: TTreeNode;
    FModified: Boolean;
    FHunter: TChangeEventHunter;
    FNewAction: Boolean;
    FOldWidth: Integer;
    FTargets: TActionTargets;
    FSelActionId: String;
    FSelActionType: TdxActionType;
    FActionPos: Integer;
    procedure SetModified;
    function ValidateCurrentAction: Boolean;
    procedure SelectNode(N: TTreeNode);
    function SortSelectedNodes: TList;
    procedure PasteNodes;
    procedure CutNodes;
    procedure CopyNodes;
    function CheckMultiSelection: Boolean;
    procedure SetNodeImage(Node: TTreeNode);
    procedure ClearHelp;
    procedure LoadHelp(A: TBaseAction);
    function CheckCanDropNode(Target: TTreeNode): Boolean;
    function CheckIfElseIfExists(PrevN: TTreeNode): Boolean;
    function CheckElseExists(N: TTreeNode): Boolean;
    procedure SetMenuState;
    procedure SetShortCutMenuState;
    procedure UpdateNodeText(N: TTreeNode);
    function GetNodeLineKind(N: TTreeNode): TActionLineKind;
    function GetParentLines(N: TTreeNode): TActionLines;
		procedure EditComment(Node: TTreeNode; Line: TActionLine);
    procedure EditCondition(Node: TTreeNode; Line: TActionLine);
    procedure EditAction(Node: TTreeNode; Line: TActionLine);
    procedure EditLine(Node: TTreeNode; Line: TActionLine);
    procedure NewAction;
    procedure NewIfCondition;
    procedure NewElseIfCondition;
    procedure NewElseCondition;
    procedure NewComment;
    procedure DeleteIfCondition(N: TTreeNode);
    procedure DeleteLine(N: TTreeNode);
    procedure BuildTree;
    procedure ShowAction(AAction: TBaseAction);
    procedure ShowLine(ALine: TActionLine);
    procedure SelectAction(AType: TdxActionType; const AId: String; ActionPos: Integer);
    procedure SetActionsDisabled(ADisable: Boolean);
    function GetFirstSelectedAction: TBaseAction;
  public
    { public declarations }
    function ShowForm(const ATitle, Xml: String; AForm: TdxForm;
      Targets: TActionTargets; out mr: Integer; AType: TdxActionType = actNone;
      const SelectActionId: String = ''; ActionPos: Integer = 0): String;
  end;

var
  ActionsEditFm: TActionsEditFm;

function ShowActionsEditForm(const Title, Xml: String; AForm: TdxForm;
  Targets: TActionTargets; out mr: Integer; AType: TdxActionType = actNone;
  const SelectActionId: String = ''; ActionPos: Integer = 0): String;

implementation

uses
  apputils, selectactionform, exprform, langmanager, Clipbrd, TypInfo,
  helpmanager, IniFiles, appsettings, myctrls;

function ShowActionsEditForm(const Title, Xml: String; AForm: TdxForm;
  Targets: TActionTargets; out mr: Integer; AType: TdxActionType;
  const SelectActionId: String; ActionPos: Integer): String;
begin
  if ActionsEditFm = nil then
  	ActionsEditFm := TActionsEditFm.Create(Application);
  Result := ActionsEditFm.ShowForm(Title, Xml, AForm, Targets, mr, AType,
    SelectActionId, ActionPos);
end;

{$R *.lfm}

{ TChangeEventHunter }

procedure TChangeEventHunter.ControlChange(Sender: TObject);
var
  i: Integer;
  e: TNotifyEvent;
begin
  FModified := True;
  i := FControls.IndexOf(Sender);
  e := FEvents[i];
  if e <> nil then e(Sender);
end;

constructor TChangeEventHunter.Create;
begin
  FControls := TList.Create;
end;

destructor TChangeEventHunter.Destroy;
begin
  SetLength(FEvents, 0);
  FControls.Free;
  inherited Destroy;
end;

procedure TChangeEventHunter.BindComponent(Comp: TComponent);
var
  i: Integer;
  C: TComponent;
  pInfo: PPropInfo;
  Prop: TMethod;
begin
  FComp := Comp;
  FControls.Clear;
  SetLength(FEvents, 0);
  for i := 0 to Comp.ComponentCount - 1 do
  begin
    C := Comp.Components[i];

    pInfo := GetPropInfo(C, 'OnChange');
    if pInfo = nil then Continue;

    Prop := GetMethodProp(C, 'OnChange');
    FControls.Add(C);
    SetLength(FEvents, Length(FEvents) + 1);
    FEvents[Length(FEvents)-1] := TNotifyEvent(Prop);
    SetMethodProp(C, 'OnChange', TMethod(@ControlChange));
  end;
end;

{ TActionsEditFm }

procedure TActionsEditFm.FormCreate(Sender: TObject);
begin
  //Caption := rsActionsEditorTitle;
  Width := AppConfig.AEWidth;
  Height := AppConfig.AEHeight;
  Tree.Width := AppConfig.AELeftPanWidth;
  HelpPan.Width := AppConfig.AERightPanWidth;
  Link.Hint := rsOpenHomePage;

  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  ButtonPanel1.OkButton.Default := False;
  ButtonPanel1.CancelButton.Cancel := False;

  MenuItem1.Caption := rsAction;
  MenuItem2.Caption := rsIf;
  MenuItem3.Caption := rsElseIf;
  MenuItem4.Caption := rsElseCondition;
  MenuItem5.Caption := rsComment;
  MenuItem7.Caption := rsDelete;
  MenuItem8.Caption := rsCut;
  MenuItem9.Caption := rsCopy;
  MenuItem10.Caption := rsPaste;
  MenuItem12.Caption := rsDisabled;

  SetMenuItemImage(MenuItem1, 'line_action16');
  SetMenuItemImage(MenuItem2, 'line_if16');
  SetMenuItemImage(MenuItem3, 'line_elseif16');
  SetMenuItemImage(MenuItem4, 'line_else16');
  SetMenuItemImage(MenuItem5, 'line_comment16');
  SetMenuItemImage(MenuItem7, 'delete16');
  SetMenuItemImage(MenuItem8, 'cut16');
  SetMenuItemImage(MenuItem9, 'copy16');
  SetMenuItemImage(MenuItem10, 'paste16');

  Params.Caption := rsActionNotSelected;

  ImageList1.AddLazarusResource('line_action8');
  ImageList1.AddLazarusResource('line_if16');
  ImageList1.AddLazarusResource('line_elseif16');
  ImageList1.AddLazarusResource('line_else16');
  ImageList1.AddLazarusResource('line_comment16');

  FActionRunner := TActionRunner.Create;
  FHunter := TChangeEventHunter.Create;
  FOldWidth := ScaleToScreen(Width);

  Help.DataProvider := THtmlProvider.Create(Self);
  Tree.IsWine := AppConfig.IsWine;
end;

procedure TActionsEditFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult <> mrOk then
  begin
		if FModified or FHunter.Modified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
    Exit;
  end;

  if FPan <> nil then
  begin
    CanClose := FPan.Validate;
    if CanClose then FPan.SaveAction
    else FPan.FocusProblem;
  end;
end;

procedure TActionsEditFm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  AppConfig.AEWidth := ScaleTo96(Width);
  AppConfig.AEHeight := ScaleTo96(Height);
  AppConfig.AELeftPanWidth := ScaleTo96(Tree.Width);
  AppConfig.AERightPanWidth := ScaleTo96(HelpPan.Width);
end;

procedure TActionsEditFm.FormDestroy(Sender: TObject);
begin
  FHunter.Free;
	FActionRunner.Free;
end;

procedure TActionsEditFm.FormResize(Sender: TObject);
begin
  // Удерживаем сплиттеры на месте. Бывает, что слетают.
  TreeSplitter.Left := Tree.Width + 10;
  HelpSplitter.Left := HelpPan.Left - 10;
end;

procedure TActionsEditFm.FormShow(Sender: TObject);
begin
  if (FSelActionId <> '') or (FSelActionType <> actNone) then
    SelectAction(FSelActionType, FSelActionId, FActionPos)
  else if Tree.TopItem <> nil then
	  Tree.TopItem.Selected := True;
  Tree.SetFocus;
end;

procedure TActionsEditFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('actionseditor');
end;

procedure TActionsEditFm.LinkClick(Sender: TObject);
var
  EA: TExprAction;
  SD: TScriptData;
begin
  EA := ScriptMan.Actions.FindAction(TActionCustom(TActionLine(Tree.Selected.Data).Action).ActionId);
  SD := ScriptMan.Scripts[EA.SDi];
  OpenUrl(SD.HomePage);
end;

procedure TActionsEditFm.MenuItem10Click(Sender: TObject);
begin
  if ValidateCurrentAction then
  begin
    if FPan <> nil then FPan.SaveAction;
    PasteNodes;
  end;
end;

procedure TActionsEditFm.MenuItem12Click(Sender: TObject);
var
  A: TBaseAction;
begin
  A := GetFirstSelectedAction;
  if A <> nil then
    SetActionsDisabled(not A.Disabled);
end;

procedure TActionsEditFm.MenuItem1Click(Sender: TObject);
begin
  NewAction;
end;

procedure TActionsEditFm.MenuItem2Click(Sender: TObject);
begin
  NewIfCondition;
end;

procedure TActionsEditFm.MenuItem3Click(Sender: TObject);
begin
  NewElseIfCondition;
end;

procedure TActionsEditFm.MenuItem4Click(Sender: TObject);
begin
  NewElseCondition;
end;

procedure TActionsEditFm.MenuItem5Click(Sender: TObject);
begin
  NewComment;
end;

procedure TActionsEditFm.MenuItem7Click(Sender: TObject);
var
  N: TTreeNode;
begin
  N := Tree.Selected;
  if (N = nil) or (N.Data = nil) then Exit;

  if not ConfirmDelete then Exit;
  case GetNodeLineKind(N) of
    alkIf: DeleteIfCondition(N)
    else DeleteLine(N);
  end;
  SetModified;
  Tree.ClearSelection(True);
end;

procedure TActionsEditFm.MenuItem8Click(Sender: TObject);
begin
  if ValidateCurrentAction then
  begin
    if FPan <> nil then FPan.SaveAction;
    FreeAndNil(FPan);
    CutNodes;
  end;
end;

procedure TActionsEditFm.MenuItem9Click(Sender: TObject);
begin
  if ValidateCurrentAction then
  begin
    if FPan <> nil then FPan.SaveAction;
    CopyNodes;
  end;
end;

procedure TActionsEditFm.PopupMenu1Popup(Sender: TObject);
begin
  SetMenuState;
end;

procedure TActionsEditFm.TreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  C: TCanvas;
  alk: TActionLineKind;
begin
  C := Sender.Canvas;
  alk := GetNodeLineKind(Node);
  if alk in [alkIf, alkElseIf, alkElse] then
  begin
  	C.Font.Style := [fsBold];
    C.Font.Color := clBlue;
  end
  else if alk = alkComment then
  begin
    C.Font.Color := clGray;
    C.Font.Style := [fsItalic];
  end
  else if alk = alkNone then
  begin
    C.Font.Style := [];
  	C.Font.Color := clGray
  end
  else
  begin
    if TActionLine(Node.Data).Action.Disabled then
      C.Font.Style := [fsStrikeOut]
    else
      C.Font.Style := [];
    C.Font.Color := clDefault;
  end;
end;

procedure TActionsEditFm.TreeDblClick(Sender: TObject);
var
  N: TTreeNode;
begin
  N := Tree.Selected;
  if (N <> nil) and (N.Data <> nil) then EditLine(N, TActionLine(N.Data))
  else if N <> nil then NewAction;
end;

procedure TActionsEditFm.TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Target, N: TTreeNode;
  Lines: TActionLines;
  i, idx: Integer;
  DragNodes: TList;
  alk: TActionLineKind;
begin
  Target := Tree.GetNodeAt(X, Y);
  if Target = nil then Exit;

  DragNodes := TList.Create;
  Lines := GetParentLines(FDragNode);
  alk := GetNodeLineKind(FDragNode);
  N := FDragNode;

  if alk = alkIf then
  begin
    repeat
      DragNodes.Add(N);
      Lines.Remove(N.Data);
      N := N.GetNextSibling;
    until not (GetNodeLineKind(N) in [alkElseIf, alkElse]);
  end
  else
  begin
    DragNodes.Add(N);
    Lines.Remove(N.Data);
  end;

  Lines := GetParentLines(Target);
  idx := Lines.IndexOf(Target.Data);

  for i := 0 to DragNodes.Count - 1 do
  begin
    N := TTreeNode(DragNodes[i]);
    N.MoveTo(Target, naInsert);
    if idx < 0 then
    	Lines.Add(N.Data)
    else
    begin
		  Lines.Insert(idx, N.Data);
      Inc(idx);
    end;
  end;

  DragNodes.Free;
  Tree.Selected := FDragNode;
end;

procedure TActionsEditFm.TreeDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Sender = Source) and CheckCanDropNode(Tree.GetNodeAt(X, Y));
end;

procedure TActionsEditFm.TreeEnter(Sender: TObject);
begin
  Tree.SelectionColor := clHighlight;
  Tree.SelectionFontColor := clHighlightText;
end;

procedure TActionsEditFm.TreeExit(Sender: TObject);
begin
  Tree.SelectionColor := clSilver;
  Tree.SelectionFontColor := clBlack;
end;

procedure TActionsEditFm.TreeKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    TreeDblClick(Tree);
end;

procedure TActionsEditFm.TreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  N: TTreeNode;
begin
  if Button = mbLeft then
  begin
    N := Tree.GetNodeAt(X, Y);
    if (N <> nil) and (GetNodeLineKind(N) in [alkAction, alkIf, alkComment])
    	and (Tree.SelectionCount = 1) then
    begin
      FDragNode := N;
    	Tree.BeginDrag(False);
    end;
  end;
end;

procedure TActionsEditFm.TreeSelectionChanged(Sender: TObject);
var
  N: TTreeNode;
  Line: TActionLine;
begin
  SetShortCutMenuState;

  N := Tree.Selected;
  // При перетаскивании Tree.Selected = nil, хотя узел выделен
  if (N = nil) and (Tree.SelectionCount = 1) then N := Tree.Selections[0];
  //
  if FOldNode = N then Exit;

  if FPan <> nil then
  begin
    if not FPan.Validate then
    begin
      SelectNode(FOldNode);
      FPan.FocusProblem;
      Exit;
    end;
    FPan.SaveAction;
  end;
  FreeAndNil(FPan);

  if N <> nil then
  begin
    Line := TActionLine(N.Data);
    ShowLine(Line);
    FOldNode := N;
  end
  else
    ShowLine(nil);
end;

procedure TActionsEditFm.SetModified;
begin
  FModified := True;
end;

function TActionsEditFm.ValidateCurrentAction: Boolean;
begin
  Result := True;
  if FPan <> nil then
  begin
    Result := FPan.Validate;
    FPan.FocusProblem;
  end;
end;

procedure TActionsEditFm.SelectNode(N: TTreeNode);
begin
  Tree.LockSelectionChangeEvent;
  Tree.ClearSelection;
	N.Selected := True;
  Tree.UnlockSelectionChangeEvent;
end;

function TActionsEditFm.SortSelectedNodes: TList;

	procedure AddToList(L: TList; Node: TTreeNode);
  var
    i: Integer;
    N: TTreeNode;
  begin
    for i := 0 to L.Count - 1 do
    begin
			N := TTreeNode(L[i]);
			if Node.Index < N.Index then
      begin
        L.Insert(i, Node);
        Exit;
      end;
    end;
    L.Add(Node);
  end;

var
  i: Integer;
begin
  Result := TList.Create;
  for i := 0 to Tree.SelectionCount - 1 do
  	AddToList(Result, Tree.Selections[i]);
end;

procedure TActionsEditFm.PasteNodes;
var
  AR: TActionRunner;
  Buf: String;
  N: TTreeNode;
  i, idx: Integer;
  L: TActionLine;
  Lines: TActionLines;
begin
  N := Tree.Selected;
  Lines := GetParentLines(N);
  idx := Lines.IndexOf(N.Data);

  Buf := Clipboard.AsText;
  AR := TActionRunner.Create;
  AR.Load(Buf);

  if AR.Lines.Count > 0 then
  begin
    for i := 0 to AR.Lines.Count - 1 do
    begin
      L := AR.Lines[i];
      if idx < 0 then
    	  Lines.Add(L)
      else
      begin
			  Lines.Insert(idx, L);
  	    Inc(idx);
      end;
    end;

    L := AR.Lines[0];
    BuildTree;
    N := Tree.Items.FindNodeWithData(L);
    if N <> nil then N.Selected := True;

    // Вручную очищаем список AR, т. к. текущая реализация уничтожает объекты
    for i := AR.Lines.Count - 1 downto 0 do
  	  AR.Lines.Delete(i);

	end;

  AR.Free;
  SetModified;
end;

procedure TActionsEditFm.CutNodes;
var
  AR: TActionRunner;
  Nodes: TList;
  i: Integer;
  N: TTreeNode;
  alk: TActionLineKind;
  Buf: String;
  Lines: TActionLines;
begin
  Nodes := SortSelectedNodes;
  Lines := GetParentLines(Tree.Selected);
  AR := TActionRunner.Create;
  Tree.LockSelectionChangeEvent;

  for i := 0 to Nodes.Count - 1 do
  begin
    N := TTreeNode(Nodes[i]);
		alk := GetNodeLineKind(N);

    if alk = alkIf then
    begin
      repeat
        Lines.Remove(N.Data);
        AR.Lines.Add(N.Data);
        N := N.GetNextSibling;
        N.GetPrevSibling.Delete;
      until not (GetNodeLineKind(N) in [alkElseIf, alkElse]);
    end
    else
    begin
      Lines.Remove(N.Data);
    	AR.Lines.Add(N.Data);
      N.Delete;
    end;
  end;

  AR.Save(Buf);
  Clipboard.AsText := Buf;

  AR.Free;
  Nodes.Free;
  Tree.UnlockSelectionChangeEvent;
  SetModified;
end;

procedure TActionsEditFm.CopyNodes;
var
  AR: TActionRunner;
  Nodes: TList;
  i: Integer;
  N: TTreeNode;
  alk: TActionLineKind;
  Buf: String;
begin
  Nodes := SortSelectedNodes;

  AR := TActionRunner.Create;

  for i := 0 to Nodes.Count - 1 do
  begin
    N := TTreeNode(Nodes[i]);
		alk := GetNodeLineKind(N);

    if alk = alkIf then
    begin
      repeat
        AR.Lines.Add(N.Data);
        N := N.GetNextSibling;
      until not (GetNodeLineKind(N) in [alkElseIf, alkElse]);
    end
    else
    	AR.Lines.Add(N.Data);
  end;

  AR.Save(Buf);
  Clipboard.AsText := Buf;

  // Вручную очищаем список AR, т. к. текущая реализация уничтожает объекты
 	for i := AR.Lines.Count - 1 downto 0 do
 		AR.Lines.Delete(i);

  AR.Free;
  Nodes.Free;
end;

function TActionsEditFm.CheckMultiSelection: Boolean;
var
  N, NS: TTreeNode;
  i: Integer;
begin
  Result := True;
  N := Tree.Selected;
  for i := 0 to Tree.SelectionCount - 1 do
  begin
    NS := Tree.Selections[i];
    if (NS.Parent <> N.Parent) or (GetNodeLineKind(NS) in [alkNone, alkElseIf, alkElse]) then
    begin
      //ErrMsg(rsActionsMultiSelectError);
      Exit(False);
    end;
  end;
end;

procedure TActionsEditFm.SetNodeImage(Node: TTreeNode);
var
  i: Integer;
begin
  case GetNodeLineKind(Node) of
    alkAction: i := 0;
    alkIf: i := 1;
    alkElseIf: i := 2;
    alkElse: i := 3;
    alkComment: i := 4;
    else i := -1;
  end;
  Node.ImageIndex := i;
  Node.SelectedIndex := i;
end;

{function ToHtml(const S: String): String;
begin
  Result := '<html><head><meta content="text/html;charset=UTF-8" http-equiv="Content-Type">' +
    '</head><body bgcolor=#fff8dc>' + S + '</body></html>';
end;}

procedure TActionsEditFm.ClearHelp;
begin
  Title.Caption := rsActionNotSelected;
  Grp.Visible := False;
  Link.Visible := False;
  Help.SetHtmlFromStr(ToHtml(''));
  Params.Visible:=True;
end;

procedure TActionsEditFm.LoadHelp(A: TBaseAction);
var
  SL: TStrings;
  EA: TExprAction;
  GrpNm, HelpText, Info: String;
  SD: TScriptData;
begin
  HelpText := ToHtml('');

  if A.ActionType = actCustom then
  begin
    EA := ScriptMan.Actions.FindAction(TActionCustom(A).ActionId);
		HelpText := ToHtml(EA.Description);
    SD := ScriptMan.Scripts[EA.SDi];
    GrpNm := StringReplace(EA.Group, '/', ' -> ', [rfReplaceAll]);
    Info := rsGroup + ': ' + GrpNm + LineEnding + rsModule + ': ' + SD.GetModuleName;
    if SD.Author <> '' then Info := Info + LineEnding + rsAuthor + ': ' + SD.Author;
    if SD.Version <> '' then Info := Info + LineEnding + rsVersion + ': ' + SD.Version;
    Link.Visible := SD.HomePage <> '';
  end
  else
  begin
    if FileExists(LangMan.ActionsFile) then
    begin
      SL := TStringList.Create;
      with TIniFile.Create(LangMan.ActionsFile) do
      try
        ReadSectionRaw(IntToStr(Ord(A.ActionType)), SL);
        HelpText := ToHtml(SL.Text);
      finally
        Free;
        SL.Free;
      end;
    end;
    GrpNm := rsBuiltInActions;
    Info := rsGroup + ': ' + GrpNm;
    Link.Visible := False;
  end;
  Help.SetHtmlFromStr(HelpText);
  Title.Caption := A.ActionName;
  Grp.Caption := '(' + GrpNm + ')';
  Grp.Hint := Info;
  Grp.Visible := True;
  Params.Visible := False;
end;

function TActionsEditFm.CheckCanDropNode(Target: TTreeNode): Boolean;
var
  alk: TActionLineKind;
  N: TTreeNode;
begin
  Result := False;
  if Target = nil then Exit;
  alk := GetNodeLineKind(Target);
  if alk in [alkElseIf, alkElse] then Exit
  else
  begin
    N := Target.GetParentNodeOfAbsoluteLevel(FDragNode.Level);
    if GetNodeLineKind(N) in [alkIf, alkElseIf, alkElse] then
    begin
      while GetNodeLineKind(N) <> alkIf do
      	N := N.GetPrevSibling;
      if N = FDragNode then Exit;
    end;
  end;
  Result := True;
end;

function TActionsEditFm.CheckIfElseIfExists(PrevN: TTreeNode): Boolean;
begin
  Result := (PrevN <> nil) and (GetNodeLineKind(PrevN) in [alkIf, alkElseIf]);
end;

function TActionsEditFm.CheckElseExists(N: TTreeNode): Boolean;
begin
  if N = nil then Exit(False);
  if GetNodeLineKind(N) = alkIf then N := N.GetNextSibling;
  while GetNodeLineKind(N) in [alkElseIf] do
    N := N.GetNextSibling;
  Result := GetNodeLineKind(N) = alkElse;
end;

procedure TActionsEditFm.SetMenuState;
var
  N: TTreeNode;
  alk: TActionLineKind;
  cms, sc1, ne, ae, ife: Boolean;
  Buf: String;
  Act: TBaseAction;
begin
  N := Tree.Selected;
  alk := GetNodeLineKind(N);
  cms := (N <> nil) and CheckMultiSelection;
  sc1 := Tree.SelectionCount = 1;
  ne := not (alk in [alkElseIf, alkElse]);
  ife := (N <> nil) and CheckIfElseIfExists(N.GetPrevSibling);
  Act := GetFirstSelectedAction;
  Buf := Clipboard.AsText;
  ae := Copy(Buf, 1, 9) = '<actions>';

  MenuItem1.Enabled := sc1 and ne;
  MenuItem2.Enabled := sc1 and ne;
  MenuItem3.Enabled := sc1 and ((alk in [alkIf, alkElseIf, alkElse]) or ife);
  MenuItem4.Enabled := sc1 and ((alk in [alkIf, alkElseIf]) or ife) and
  	(not CheckElseExists(N));
  MenuItem5.Enabled := sc1 and ne;
  MenuItem7.Enabled := sc1 and (alk <> alkNone);
  MenuItem8.Enabled := cms;
	MenuItem9.Enabled := cms;
  MenuItem10.Enabled := sc1 and ne and ae;
  MenuItem12.Enabled := Act <> nil;
  MenuItem12.Checked := (Act <> nil) and Act.Disabled;
end;

procedure TActionsEditFm.SetShortCutMenuState;
var
  N: TTreeNode;
  alk: TActionLineKind;
  sc1: Boolean;
begin
  N := Tree.Selected;
  alk := GetNodeLineKind(N);
  sc1 := Tree.SelectionCount = 1;
  MenuItem7.Enabled := sc1 and (alk <> alkNone);
  MenuItem12.Enabled := True;
end;

procedure TActionsEditFm.UpdateNodeText(N: TTreeNode);
var
  L: TActionLine;
begin
  L := TActionLine(N.Data);
  case L.Kind of
    alkAction: N.Text := L.Action.ActionName;
    alkIf: N.Text := Format(rsIfCondition, [L.Cond]);
    alkElseIf: N.Text := Format(rsElseIfCondition, [L.Cond]);
    alkElse: N.Text := rsElseCondition;
    alkComment: N.Text := L.Text;
  end;
end;

function TActionsEditFm.GetNodeLineKind(N: TTreeNode): TActionLineKind;
begin
  Result := alkNone;
  if (N <> nil) and (N.Data <> nil) then Result := TActionLine(N.Data).Kind;
end;

function TActionsEditFm.GetParentLines(N: TTreeNode): TActionLines;
begin
  if N.Parent = nil then Result := FActionRunner.Lines
	else Result := TActionLine(N.Parent.Data).Lines;
end;

procedure TActionsEditFm.EditComment(Node: TTreeNode; Line: TActionLine);
var
  S: String;
begin
  S := Line.Text;
  if InputQuery(rsComment, rsEnterText, S) then
  begin
    Line.Text := S;
    UpdateNodeText(Node);
    SetModified;
  end;
end;

procedure TActionsEditFm.EditCondition(Node: TTreeNode; Line: TActionLine);
var
  S: String;
begin
  S := Line.Cond;
  if ShowExprForm(etLogicalExpr, nil, S, FForm, nil, nil, nil) = mrOk then
  begin
    Line.Cond:=S;
    UpdateNodeText(Node);
    SetModified;
  end;
end;

procedure TActionsEditFm.EditAction(Node: TTreeNode; Line: TActionLine);
var
  EA: TExprAction;
  ATp: TdxActionType;
  Disabled: Boolean;
begin
  //EA := nil; ATp := actNone;
  ATp := Line.Action.ActionType;
  Disabled := Line.Action.Disabled;
  if ATp = actCustom then
    EA := ScriptMan.Actions.FindAction(TActionCustom(Line.Action).ActionId);
  if (ShowSelectActionForm(EA, ATp, FTargets) = mrOk) and
    (Confirm(rsWarning, rsReplaceActionMsg) = mrYes) then
  begin
    FreeAndNil(Line.Action);
    Line.Action := CreateAction(ATp);
    Line.Action.Disabled := Disabled;
    if ATp = actCustom then
      TActionCustom(Line.Action).ActionId := EA.Id;

    UpdateNodeText(Node);
    FNewAction := True;
    ShowAction(Line.Action);
    SetModified;
  end;
end;

procedure TActionsEditFm.EditLine(Node: TTreeNode; Line: TActionLine);
begin
  if Tree.SelectionCount > 1 then Exit;

  case Line.Kind of
    alkAction: EditAction(Node, Line);
    alkIf, alkElseIf: EditCondition(Node, Line);
    alkComment: EditComment(Node, Line);
  end;
end;

procedure TActionsEditFm.NewAction;
var
	EA: TExprAction;
  N: TTreeNode;
  Line: TActionLine;
  Lines: TActionLines;
  ATp: TdxActionType;
begin
  if not ValidateCurrentAction or (Tree.SelectionCount > 1) then Exit;

  N := Tree.Selected;
  Lines := GetParentLines(N);

  EA := nil; ATp := actNone;
  if ShowSelectActionForm(EA, ATp, FTargets) = mrOk then
  begin
    Line := Lines.AddLine(N.Data);
    Line.Kind := alkAction;
    Line.Action := CreateAction(ATp);
    if ATp = actCustom then
      TActionCustom(Line.Action).ActionId := EA.Id;

    N := Tree.Items.InsertObject(N, '', Line);
    UpdateNodeText(N);
    SetNodeImage(N);
    //N.Selected := True;
    FNewAction := True;
    SelectNode(N);
    SetModified;
  end;
end;

procedure TActionsEditFm.NewIfCondition;
var
  Lines: TActionLines;
  S: String;
  N: TTreeNode;
  Line: TActionLine;
begin
  if not ValidateCurrentAction then Exit;

  N := Tree.Selected;
  Lines := GetParentLines(N);
  S := '';
  if ShowExprForm(etLogicalExpr, nil, S, FForm, nil, nil, nil) = mrOk then
  begin
    Line := Lines.AddLine(N.Data);
    Line.Kind:=alkIf;
    Line.Cond:=S;
  	N := Tree.Items.InsertObject(N, '', Line);
    UpdateNodeText(N);
    SetNodeImage(N);
    N := Tree.Items.AddChild(N, rsInsertAction);
    //N.Selected := True;
    SelectNode(N);
    SetModified;
	end;
end;

procedure TActionsEditFm.NewElseIfCondition;
var
  Lines: TActionLines;
  S: String;
  N: TTreeNode;
  Line: TActionLine;
begin
  N := Tree.Selected;
  if GetNodeLineKind(N) = alkIf then N := N.GetNextSibling;
  Lines := GetParentLines(N);
  S := '';
  if ShowExprForm(etLogicalExpr, nil, S, FForm, nil, nil, nil) = mrOk then
  begin
    Line := Lines.AddLine(N.Data);
    Line.Kind:=alkElseIf;
    Line.Cond:=S;

	  N := Tree.Items.InsertObject(N, '', Line);
    UpdateNodeText(N);
    SetNodeImage(N);
    N := Tree.Items.AddChild(N, rsInsertAction);
    //N.Selected := True;
    SelectNode(N);
    SetModified;
  end;
end;

procedure TActionsEditFm.NewElseCondition;
var
  Lines: TActionLines;
  N: TTreeNode;
  Line: TActionLine;
begin
  N := Tree.Selected;
  if GetNodeLineKind(N) in [alkIf, alkElseIf] then
	  N := N.GetNextSibling;

  while GetNodeLineKind(N) = alkElseIf do
  	N := N.GetNextSibling;

  Lines := GetParentLines(N);
  Line := Lines.AddLine(N.Data);
  Line.Kind:=alkElse;
  N := Tree.Items.InsertObject(N, '', Line);
  UpdateNodeText(N);
  SetNodeImage(N);
  N := Tree.Items.AddChild(N, rsInsertAction);
  //N.Selected := True;
  SelectNode(N);
  SetModified;
end;

procedure TActionsEditFm.NewComment;
var
  Lines: TActionLines;
  N: TTreeNode;
  Line: TActionLine;
  S: String;
begin
  if not ValidateCurrentAction then Exit;

  N := Tree.Selected;

  S := '';
  if InputQuery(rsComment, rsEnterText, S) then
  begin
    Lines := GetParentLines(N);
    Line := Lines.AddLine(N.Data);
    Line.Kind := alkComment;
    Line.Text := S;

    N := Tree.Items.InsertObject(N, '', Line);
	  UpdateNodeText(N);
    SetNodeImage(N);
	  //N.Selected := True;
    SelectNode(N);
    SetModified;
  end;
end;

procedure TActionsEditFm.DeleteIfCondition(N: TTreeNode);
var
  L: TActionLine;
  NextN: TTreeNode;
  alk: TActionLineKind;
begin
  Tree.LockSelectionChangeEvent;

  NextN := N.GetNextSibling;
  DeleteLine(N);

  alk := GetNodeLineKind(NextN);
  if alk in [alkElseIf, alkElse] then
  begin
  	L := TActionLine(NextN.Data);
    L.Kind := alkIf;
    UpdateNodeText(NextN);
    SetNodeImage(NextN);
  end;

  Tree.UnlockSelectionChangeEvent;
end;

procedure TActionsEditFm.DeleteLine(N: TTreeNode);
var
  NextN: TTreeNode;
  Lines: TActionLines;
  L: TActionLine;
begin
  Lines := GetParentLines(N);
  NextN := N.GetNextSibling;
  L := TActionLine(N.Data);
  Lines.DeleteLine(L);
  FreeAndNil(FPan);
  N.Delete;
  NextN.Selected := True;
end;

procedure TActionsEditFm.BuildTree;

	procedure BuildItems(ParentNode: TTreeNode; Lines: TActionLines);
  var
    i: Integer;
    L: TActionLine;
    N: TTreeNode;
  begin
  	for i := 0 to Lines.Count - 1 do
    begin
      L := Lines[i];
      N := Tree.Items.AddChildObject(ParentNode, '', L);
      SetNodeImage(N);
      UpdateNodeText(N);
      if L.Kind in [alkIf, alkElseIf, alkElse] then
      	BuildItems(N, L.Lines);
    end;

    Tree.Items.AddChild(ParentNode, rsInsertAction);
    if ParentNode <> nil then ParentNode.Expand(False);
  end;

begin
  FOldNode := nil;
  Tree.ClearSelection;
  Tree.Items.Clear;
  BuildItems(nil, FActionRunner.Lines);
end;

procedure TActionsEditFm.ShowAction(AAction: TBaseAction);
begin
  FreeAndNil(FPan);
  if AAction is TGotoFormAction then FPan := TGotoFormPanel.CreatePanel(Self, AAction, FForm)
  else if AAction is TPrintAction then FPan := TPrintActionPanel.CreatePanel(Self, AAction, FForm)
  else if AAction is TMassCalcAction then FPan := TMassCalcActionPanel.CreatePanel(Self, AAction, FForm)
  else if AAction is TOpenReportAction then FPan := TOpenReportActionPanel.CreatePanel(Self, AAction, FForm)
  else if AAction is TSaveChangesAction then FPan := TSaveChangesActionPanel.CreatePanel(Self, AAction, FForm)
  else if AAction is TUserMonitorAction then FPan := TUserMonitorActionPanel.CreatePanel(Self, AAction, FForm)
  else if AAction is TCallFuncAction then FPan := TCallFuncActionPanel.CreatePanel(Self, AAction, FForm)
  else if AAction is TClearFieldsAction then FPan := TClearFieldsActionPanel.CreatePanel(Self, AAction, FForm)
  else if AAction is TShowMessageAction then FPan := TShowMessageActionPanel.CreatePanel(Self, AAction, FForm)
  else if AAction is TActionCustom then
  begin
  	FPan := TCustomActionPanel.CreatePanel(Self, AAction, FForm);
    with TCustomActionPanel(FPan) do
    begin
      EAction := ScriptMan.Actions.FindAction(TActionCustom(AAction).ActionId);
      if EAction = nil then
      begin
        ErrMsgFmt(rsActionNotFoundMsg, [TActionCustom(AAction).ActionId]);
        FreeAndNil(FPan);
        ClearHelp;
        Exit;
      end;
    end;
  end;

  try
    FPan.CreateControls;
    FPan.Parent := ScrollBox1;
    FPan.Align:=alClient;
    FPan.Init;
    if FNewAction and (FPan is TCustomActionPanel) then
    	TCustomActionPanel(FPan).SetDefaultValues;
    FNewAction := False;
    FHunter.BindComponent(FPan);
    LoadHelp(AAction);
  except
    on E: Exception do
    begin
      FPan.DeleteControls;
   		//ErrMsgFmt(rsFailedToInitActionCmp, [E.Message]);
      ErrMsg(E.Message);
    end;
  end;
end;

procedure TActionsEditFm.ShowLine(ALine: TActionLine);
begin
  if (ALine <> nil) and (ALine.Kind = alkAction) then ShowAction(ALine.Action)
  else ClearHelp;
end;

procedure TActionsEditFm.SelectAction(AType: TdxActionType; const AId: String;
  ActionPos: Integer);
var
  i: Integer;
  N: TTreeNode;
  AL: TActionLine;
begin
  ClearHelp;
  for i := 0 to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    if N.Data = nil then Continue;
    AL := TActionLine(N.Data);
    if AL.Kind = alkAction then
    begin
      if AL.Action is TActionCustom then
      begin
        if AType = actCustom then
          with TActionCustom(AL.Action) do
            if ActionId = AId then
            begin
              if ActionPos = 0 then
              begin
                Tree.ClearSelection;
                Tree.Selected := N;
                Exit;
              end
              else
                Dec(ActionPos);
            end;
      end
      else if AL.Action.ActionType = AType then
      begin
        if ActionPos = 0 then
        begin
          Tree.ClearSelection;
          Tree.Selected := N;
          Exit;
        end
        else
          Dec(ActionPos);
      end;
    end;
  end;
end;

procedure TActionsEditFm.SetActionsDisabled(ADisable: Boolean);
var
  i: Integer;
  N: TTreeNode;
  AL: TActionLine;
begin
  for i := 0 to Tree.SelectionCount - 1 do
  begin
    N := Tree.Selections[i];
    AL := TActionLine(N.Data);
    if (AL <> nil) and (AL.Kind = alkAction) then
      AL.Action.Disabled := ADisable;
  end;
  Tree.Invalidate;
end;

function TActionsEditFm.GetFirstSelectedAction: TBaseAction;
var
  i: Integer;
  N: TTreeNode;
  AL: TActionLine;
begin
  Result := nil;
  for i := 0 to Tree.SelectionCount - 1 do
  begin
    N := Tree.Selections[i];
    AL := TActionLine(N.Data);
    if (AL <> nil) and (AL.Kind = alkAction) then
      Exit(AL.Action);
  end;
end;

function TActionsEditFm.ShowForm(const ATitle, Xml: String; AForm: TdxForm;
  Targets: TActionTargets; out mr: Integer; AType: TdxActionType;
  const SelectActionId: String; ActionPos: Integer): String;
begin
  Caption := ATitle;//Format(rsActionsEditorTitle, [ATitle]);
  Result := Xml;
  FForm := AForm;
  FTargets := Targets;
	FActionRunner.Load(Xml);
  BuildTree;
  FNewAction := False;
  FModified := False;
  FHunter.Modified := False;
  FSelActionId := SelectActionId;
  FSelActionType := AType;
  FActionPos := ActionPos;

  mr := ShowModal;
  if mr = mrOk then
    FActionRunner.Save(Result);
  FreeAndNil(FPan);
end;

end.

