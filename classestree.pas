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

unit ClassesTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls,
  TreeFilterEdit, Buttons, Menus, LclType,
  LazUtf8, IpHtml, IpFileBroker, StrUtils, strconsts, httpclient, Variants,
  treepanel;

type

  THelpCacheItem = class
  public
    Url, Text: String;
  end;

  { THelpCache }

  THelpCache = class(TList)
  public
    procedure AddItem(const AUrl, AText: String);
    function FindHelp(const AUrl: String): THelpCacheItem;
    procedure Clear; override;
  end;

  { TClassesTree }

  TGetHandlerEvent = procedure (Sender: TObject; const AText: String) of object;

  TClassesTree = class(TTreePanel)
  private
    FFileName: String;
    FHideAncestors: Boolean;
    FHideBaseClasses: Boolean;
    FHideContextHelp: Boolean;
    FIsWeb: Boolean;
    FOnGetEventHandler: TGetHandlerEvent;
    FHelpPan: TIPHtmlPanel;
    FHelpSplit: TSplitter;
    FHttp: THttpClient;
    FCapturedNode: TTreeNode;
    FLeftX, FRightX: Integer;
    FTypeText: String;
    FCache: THelpCache;
    procedure FilterAfterFilter(Sender: TObject);
    function FindProcMemberNode(Node: TTreeNode; const SearchText: String): TTreeNode;
    function FindTypeNode(Node: TTreeNode; const SearchText: String; OnlyVisible: Boolean): TTreeNode;
    function FindClassNode(CurrentNode: TTreeNode; const SearchText: String; OnlyAncestors: Boolean): TTreeNode;
    procedure AddNodes(Target: TTreeNode; const ClsName: String);
    function GetHelpPanelHeight: Integer;
    procedure HttpError(Sender: TObject; const ErrorMsg: String);
    procedure HttpFinish(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    function ProvCanHandle(Sender: TObject; const URL: string): Boolean;
    procedure SetHelpPanelHeight(AValue: Integer);
    procedure SetHideContextHelp(AValue: Boolean);
    procedure TreeAdvancedDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
      DefaultDraw: Boolean);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetTypeFromNodeTextAtX(const NodeText: String; X: Integer;
      out TypeText: String; out LeftX, RightX: Integer): Boolean;
    procedure TreeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function FindNodeWithType(const TypeText: String): TTreeNode;
    procedure TreeSelectionChanged(Sender: TObject);
    function GetHelpUrl: String;
    procedure GetHelp;
    procedure SetHelpPanText(const S: String; Error: Boolean = False);
    function WikiToHtml(S: String): String;
    function GetEventHandler: String;
    procedure DoGetEventHandler;
    procedure UpdateContextHelp;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildTree; override;
    procedure SetFilter(const FilterText: String);
    property FileName: String read FFileName write FFileName;
    property HideAncestors: Boolean read FHideAncestors write FHideAncestors;
    property HideBaseClasses: Boolean read FHideBaseClasses write FHideBaseClasses;
    property HideContextHelp: Boolean read FHideContextHelp write SetHideContextHelp;
    property HelpPanelHeight: Integer read GetHelpPanelHeight write SetHelpPanelHeight;
    property OnGetEventHandler: TGetHandlerEvent read FOnGetEventHandler
      write FOnGetEventHandler;
    property IsWeb: Boolean read FIsWeb write FIsWeb;
  end;

implementation

uses
  apputils, appsettings;

const
  WIKI_URL = 'https://wiki.mydataexpress.ru/';
  BASE_URL = WIKI_URL + 'api:';
  BASE_URL_RAW = WIKI_URL + 'getRaw.php?id=api:';

  NODE_VARS = 0;
  NODE_BASETYPES = 1;
  NODE_WORDS = 2;
  NODE_CONST = 3;
  NODE_EVENTS = 4;
  NODE_TYPES = 5;
  NODE_RECORDS = 6;
  NODE_PROCS = 7;

type

  { TTreeFilterEditEx }

  // Корректно выделяет узел при сбросе фильтра.
  TTreeFilterEditEx = class(TTreeFilterEdit)
  private
    FSelectNode: TTreeNode;
  public
    procedure StoreSelection; override;
    procedure RestoreSelection; override;
  end;

function GetToken(const S: String; Len: Integer; var P: Integer; out Tk: Char): String;
var
  Start: Integer;
begin
  Start := P;
  if P > Len then
  begin
    Tk := #0;
    Exit('');
  end;
  case S[P] of
    #32:
      begin
        while (P <= Len) and (S[P] = #32) do Inc(P);
        Result := Copy(S, Start, P - Start);
        Tk := #32;
      end;
    'a'..'z', 'A'..'Z', '_':
      begin
        Tk := 'a';
        while (P <= Len) and (S[P] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
        begin
          Inc(P);
        end;
        Result := Copy(S, Start, P - Start);
      end;
    else
    begin
      Tk := S[P];
      Result := Tk;
      Inc(P);
    end;
  end;
end;

function NextToken(const S: String; Len: Integer; P: Integer): Char;
begin
  GetToken(S, Len, P, Result);
end;

function ExtractTypeName(N: TTreeNode): String;
begin
  Result := Copy(N.Text, 1, Pos(' =', N.Text) - 1);
end;

function ProcFolderToProcGroup(const ProcFolder: String): Integer;
const
  ProcFolders: array [0..25] of String = ('arrays', 'ole', 'clipbrd', 'datetime',
    'debugging', 'dialogs', 'dll', 'encoding', 'exceptions', 'expressions',
    'fs', 'graphics', 'json', 'math', 'menus', 'metadata',
    'misc', 'sql', 'strings', 'totals', 'utf8strings', 'useraccess',
    'variants', 'web', 'xml', 'dxforms');
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(ProcFolders) do
    if ProcFolders[i] = ProcFolder then Exit(i);
end;

function ConstFolderToConstGroup(const ConstFolder: String): Integer;
const
  ConstFolders: array [0..11] of String = ('colors', 'cursors', 'datetime',
    'fileattrs', 'filemode', 'misc', 'modalresult', 'popupitems', 'streamorigin',
    'toolbuttons', 'variants', 'xml');
 var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(ConstFolders) do
    if ConstFolders[i] = ConstFolder then Exit(i);
end;

{ TTreeFilterEditEx }

procedure TTreeFilterEditEx.StoreSelection;
begin
  if Text <> '' then
    inherited StoreSelection
  else
    FSelectNode := FilteredTreeview.Selected;
end;

procedure TTreeFilterEditEx.RestoreSelection;
begin
  if Text <> '' then
    inherited RestoreSelection
  else
    FilteredTreeview.Selected := FSelectNode;
end;

{ THelpCache }

procedure THelpCache.AddItem(const AUrl, AText: String);
var
  Item: THelpCacheItem;
begin
  Item := THelpCacheItem.Create;
  Item.Url := AUrl;
  Item.Text := AText;
  Add(Item);
end;

function THelpCache.FindHelp(const AUrl: String): THelpCacheItem;
var
  i: Integer;
  Item: THelpCacheItem;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Item := THelpCacheItem(Items[i]);
    if Item.Url = AUrl then Exit(Item);
  end;
end;

procedure THelpCache.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TObject(Items[i]).Free;
  inherited Clear;
end;

{ TClassesTree }

function TClassesTree.FindProcMemberNode(Node: TTreeNode; const SearchText: String): TTreeNode;
var
  i, P, Len: Integer;
  N: TTreeNode;
  S, TkStr: String;
  Tk: Char;
begin
  Result := nil;
  for i := 0 to Node.Count - 1 do
  begin
    N := Node.Items[i];
    if not N.Visible then Continue;
    S := N.Text;
    P := 1; Len := Length(S);
    GetToken(S, Len, P, Tk); // Пропускаем первое слово
    GetToken(S, Len, P, Tk); // Пропускаем пробел
    TkStr := GetToken(S, Len, P, Tk);
    if CompareText(SearchText, TkStr) = 0 then Exit(N);
  end;
end;

procedure TClassesTree.FilterAfterFilter(Sender: TObject);
begin
  Tree.MakeSelectionVisible;
  if (Tree.Selected = nil) or not Tree.Selected.IsVisible then SetHelpPanText('');
end;

function TClassesTree.FindTypeNode(Node: TTreeNode; const SearchText: String;
  OnlyVisible: Boolean): TTreeNode;
var
  i: Integer;
  N: TTreeNode;
  S: String;
begin
  Result := nil;
  for i := 0 to Node.Count - 1 do
  begin
    N := Node.Items[i];
    if OnlyVisible and not N.Visible then Continue;
    S := N.Text;
    S := Copy(S, 1, Pos(' =', S) - 1);
    if CompareText(SearchText, S) = 0 then Exit(N);
  end;
end;

function TClassesTree.FindClassNode(CurrentNode: TTreeNode;
  const SearchText: String; OnlyAncestors: Boolean): TTreeNode;

  function FindChildNode(Node: TTreeNode): TTreeNode;
  var
    j: Integer;
    Child: TTreeNode;
  begin
    Result := nil;
    for j := 0 to Node.Count - 1 do
    begin
      Child := Node.Items[j];
      if not Child.Visible then Continue;
      if CompareText(SearchText, ExtractTypeName(Child)) = 0 then Exit(Child);
    end;
  end;

var
  i, j: Integer;
  N: TTreeNode;
begin
  Result := nil;

  if (CurrentNode <> nil) and not OnlyAncestors then
  begin
    N := CurrentNode.GetParentNodeOfAbsoluteLevel(0);
    if N <> nil then
    begin
      if CompareText(SearchText, ExtractTypeName(N)) = 0 then Exit(N);
      Result := FindChildNode(N);
    end;
    if Result <> nil then Exit;
  end;

  for i := NODE_PROCS + 1 to Tree.Items.TopLvlCount - 1 do
  begin
    N := Tree.Items.TopLvlItems[i];
    if not N.Visible then Continue;
    if not OnlyAncestors then
    begin
      if CompareText(SearchText, ExtractTypeName(N)) = 0 then Exit(N);
    end
    else
    begin
      Result := FindChildNode(N);
      if Result <> nil then Exit;
    end;
  end;
end;

procedure TClassesTree.AddNodes(Target: TTreeNode; const ClsName: String);
var
  p, p2: Integer;
  N, PN, NN: TTreeNode;
  ParentCls, S: String;

  function FindNode: TTreeNode;
  var
    i: Integer;
    N2: TTreeNode;
  begin
    Result := nil;
    for i := NODE_PROCS + 1 to Tree.Items.TopLvlCount - 1 do
    begin
      N2 := Tree.Items.TopLvlItems[i];
      if (Pos(ClsName + ' = class', N2.Text) > 0) then Exit(N2);
    end;
  end;

begin
  N := FindNode;
  if N = nil then Exit;
  S := N.Text;

  PN := Tree.Items.AddChild(Target, S);
  NN := N.GetFirstChild;
  while NN <> nil do
  begin
    if NN.Count = 0 then
      Tree.Items.AddChild(PN, NN.Text);
    NN := NN.GetNextSibling;
  end;

  p := Pos('(', S);
  if p > 0 then
  begin
    Inc(p);
    p2 := Pos(')', S);
    ParentCls := Copy(S, p, p2 - p);
    AddNodes(Target, ParentCls);
  end;
end;

function TClassesTree.GetHelpPanelHeight: Integer;
begin
  Result := FHelpPan.Height;
end;

procedure TClassesTree.HttpError(Sender: TObject; const ErrorMsg: String);
begin
  SetHelpPanText(ErrorMsg, True);
end;

procedure TClassesTree.HttpFinish(Sender: TObject);
var
  S: String;
begin
  with THttpClient(Sender) do
    if ResponseStatusCode in [0, 200] then
    begin
      S := WikiToHtml(Content);
      SetHelpPanText(S);
      FCache.AddItem(Trim(RequestHeaders.Values['url']), S);
    end
    else
      SetHelpPanText(IntToStr(ResponseStatusCode) + ': ' + ResponseStatusText, True);
end;

procedure TClassesTree.MenuItemClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: GetHelp;
    2: DoGetEventHandler;
    4:
      begin
        FHideBaseClasses := not FHideBaseClasses;
        BuildTree;
      end;
    5:
      begin
        FHideAncestors := not FHideAncestors;
        BuildTree;
      end;
    6: HideContextHelp := not HideContextHelp;
    8: Tree.FullCollapse;
  end;
end;

procedure TClassesTree.MenuPopup(Sender: TObject);
var
  N: TTreeNode;
begin
  N := Tree.Selected;
  with TPopupMenu(Sender) do
  begin
    Items[0].Enabled := N <> nil;
    Items[2].Enabled := (N <> nil) and
      ((N.Parent = Tree.Items.TopLvlItems[NODE_EVENTS]) or (Copy(N.Text, 1, 11) = 'property On'));
    Items[4].Checked := FHideBaseClasses;
    Items[5].Checked := FHideAncestors;
    Items[6].Checked := FHideContextHelp;
  end;
end;

function TClassesTree.ProvCanHandle(Sender: TObject; const URL: string
  ): Boolean;
var
  P, Len, i: Integer;
  Tk: Char;
  TkStr, ClassNm, MemberNm, TypeNm, ProcFolder, ProcNm: String;
  N: TTreeNode;
begin
  P := 1; Len := Length(URL);
  TkStr := GetToken(URL, Len, P, Tk);
  if ((TkStr = 'http') or (TkStr = 'https')) and (NextToken(URL, Len, P) = ':') then
    OpenUrl(URL)
  else if (TkStr = 'api') and (NextToken(URL, Len, P) = ':') then
  begin
    GetToken(URL, Len, P, Tk); // :
    TkStr := GetToken(URL, Len, P, Tk);
    GetToken(URL, Len, P, Tk); // :
    // Примеры: api:classes:tdxform: или api:classes:tdxform:m:create
    if TkStr = 'classes' then
    begin
      ClassNm := GetToken(URL, Len, P, Tk);
      GetToken(URL, Len, P, Tk); // :
      TkStr := GetToken(URL, Len, P, Tk);
      if (TkStr = 'm') or (TkStr = 'p') or (TkStr = 'e') then
      begin
        GetToken(URL, Len, P, Tk); // :
        MemberNm := GetToken(URL, Len, P, Tk);

        // Выделяем узел
        N := FindClassNode(Tree.Selected, ClassNm, False);
        if N = nil then N := FindClassNode(Tree.Selected, ClassNm, True);
        if N <> nil then N := FindProcMemberNode(N, MemberNm);
        if N <> nil then Tree.Selected := N;
      end
      else
      begin
        // Выделяем узел
        N := FindClassNode(Tree.Selected, ClassNm, False);
        if N = nil then N := FindClassNode(Tree.Selected, ClassNm, True);
        if N <> nil then Tree.Selected := N;
      end;
    end
    else if TkStr = 'events' then
    begin
      TypeNm := GetToken(URL, Len, P, Tk);
      // Выделяем узел
      N := FindTypeNode(Tree.Items.TopLvlItems[NODE_EVENTS], TypeNm, True);
      if N <> nil then Tree.Selected := N;
    end
    else if TkStr = 'types' then
    begin
      TypeNm := GetToken(URL, Len, P, Tk);
      // Выделяем узел
      N := FindTypeNode(Tree.Items.TopLvlItems[NODE_TYPES], TypeNm, True);
      if N <> nil then
        Tree.Selected := N
      else
      begin
        N := FindTypeNode(Tree.Items.TopLvlItems[NODE_RECORDS], TypeNm, True);
        if N <> nil then Tree.Selected := N;
      end;
    end
    else if TkStr = 'procs' then
    begin
      ProcFolder := GetToken(URL, Len, P, Tk);
      GetToken(URL, Len, P, Tk); // :
      ProcNm := GetToken(URL, Len, P, Tk);
      i := ProcFolderToProcGroup(ProcFolder);
      // Выделяем узел
      N := Tree.Items.TopLvlItems[NODE_PROCS];
      if (i >= 0) and (i < N.Count) then
      begin
        N := N.Items[i];
        N := FindProcMemberNode(N, ProcNm);
        if N <> nil then Tree.Selected := N;
      end;
    end
    else if TkStr = 'const' then
    begin
      TkStr := GetToken(URL, Len, P, Tk);
      i := ConstFolderToConstGroup(TkStr);
      N := Tree.Items.TopLvlItems[NODE_CONST];
      if (i >= 0) and (i < N.Count) then
      begin
        N := N.Items[i];
        Tree.Selected := N;
        N.Expand(False);
      end;
    end
  end
  // Относительная ссылка в вики
  else
    OpenURL('https://wiki.mydataexpress.ru/' + URL);

  Result := False;
end;

procedure TClassesTree.SetHelpPanelHeight(AValue: Integer);
begin
  FHelpPan.Height := AValue;
end;

procedure TClassesTree.SetHideContextHelp(AValue: Boolean);
begin
  if FHideContextHelp=AValue then Exit;
  FHideContextHelp:=AValue;
  FHelpSplit.Visible := not AValue;
  FHelpPan.Visible := not AValue;
  if not AValue then
  begin
    FHelpPan.Top := FHelpSplit.Top + 100;
    UpdateContextHelp;
  end;
end;

procedure TClassesTree.TreeAdvancedDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  R: TRect;
  C: TCanvas;
begin
  if FCapturedNode = Node then
  begin
    C := Tree.Canvas;
    R := Node.DisplayRect(True);
    C.Line(FLeftX + R.Left, R.Bottom - 1, FRightX + R.Left, R.Bottom - 1);
  end;
end;

procedure TClassesTree.TreeDblClick(Sender: TObject);
begin
  DoGetEventHandler;
end;

procedure TClassesTree.TreeMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Node: TTreeNode;
  R: TRect;
  S: String;
  x1, x2: Integer;
begin
  Node := Tree.GetNodeAt(X, Y);
  if Node <> nil then
    R := Node.DisplayRect(True);
  if (Node <> nil) and GetTypeFromNodeTextAtX(Node.Text, X - R.Left, S, x1, x2) then
  begin
    FCapturedNode := Node;
    FTypeText := S;
    FLeftX := x1;
    FRightX := x2;
    Tree.Invalidate;
    Tree.Cursor := crHandPoint;
  end
  else if FCapturedNode <> nil then
  begin
    FCapturedNode := nil;
    Tree.Invalidate;
    Tree.Cursor := crDefault;
  end;
end;

function TClassesTree.GetTypeFromNodeTextAtX(const NodeText: String; X: Integer;
  out TypeText: String; out LeftX, RightX: Integer): Boolean;
var
  P, x1, x2, Len: Integer;
  PrevTk, Tk: Char;
  C: TCanvas;
  S: String;
begin
  Result := False;
  C := Tree.Canvas;
  P := 1;
  Len := Length(NodeText);
  Tk := #0;
  S := '';
  repeat
    if Tk <> ' ' then PrevTk := Tk;
    S := GetToken(NodeText, Len, P, Tk);
    if (Tk = 'a') and ((PrevTk = ':') or ((PrevTk = '(') and (Copy(NodeText, P, 1) = ')'))) then
    begin
      x1 := C.TextWidth(Copy(NodeText, 1, P - Length(S) - 1));
      x2 := C.TextWidth(Copy(NodeText, 1, P));
      if (X >= x1) and (X <= x2) then
      begin
        TypeText := S;
        LeftX := x1;
        RightX := x2;
        Result := True;
        Break;
      end;
    end;
  until Tk = #0;
end;

procedure TClassesTree.TreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  N: TTreeNode;
begin
  if (FCapturedNode <> nil) and (Button = mbLeft) then
  begin
    N := FindNodeWithType(FTypeText);
    if N <> nil then
    begin
      if not N.Visible then
      begin
        Filter.ForceFilter('');
        Filter.Text := '';
      end;
      Tree.Selected := N;
      if N.Parent = Tree.Items.TopLvlItems[NODE_RECORDS] then N.Expand(False);
    end;
  end;
end;

function TClassesTree.FindNodeWithType(const TypeText: String): TTreeNode;

  function CompareType(N: TTreeNode): Boolean;
  begin
    Result := CompareText(Copy(N.Text, 1, Pos(' =', N.Text) - 1), TypeText) = 0;
  end;

  function FindChildNode(Node: TTreeNode): TTreeNode;
  var
    i: Integer;
    N: TTreeNode;
  begin
    Result := nil;
    for i := 0 to Node.Count - 1 do
    begin
      N := Node.Items[i];
      if {N.Visible and }CompareType(N) then Exit(N);
    end;
  end;

var
  N, SelNode: TTreeNode;
  i: Integer;
begin
  Result := nil;

  SelNode := Tree.Selected;
  if SelNode <> nil then
  begin
    // Если навели на предка в скобках, проверяем дочерние узлы
    N := FindChildNode(SelNode);
    if N <> nil then Exit(N);
    if SelNode.Parent <> nil then
    begin
      // Проверяем родительский узел - может он и есть искомый тип
      if CompareType(SelNode.Parent) then Exit(SelNode.Parent);
      // Проверяем дочерние узлы верхнего родителя, тип может являться классом-предком.
      N := FindChildNode(SelNode.GetParentNodeOfAbsoluteLevel(0));
      if N <> nil then Exit(N);
    end;
  end;

  N := FindChildNode(Tree.Items.TopLvlItems[NODE_BASETYPES]);
  if N <> nil then Exit(N);
  N := FindChildNode(Tree.Items.TopLvlItems[NODE_TYPES]);
  if N <> nil then Exit(N);
  N := FindChildNode(Tree.Items.TopLvlItems[NODE_RECORDS]);
  if N <> nil then Exit(N);
  N := FindChildNode(Tree.Items.TopLvlItems[NODE_EVENTS]);
  if N <> nil then Exit(N);
  for i := NODE_PROCS + 1 to Tree.Items.TopLvlCount - 1 do
  begin
    N := Tree.Items.TopLvlItems[i];
    if {N.Visible and }CompareType(N) then Exit(N);
  end;
end;

procedure TClassesTree.TreeSelectionChanged(Sender: TObject);
begin
  UpdateContextHelp;
end;

function TClassesTree.GetHelpUrl: String;

  function ExtractClassName(Node: TTreeNode): String;
  begin
    Result := LowerCase(Copy(Node.Text, 1, Pos(' =', Node.Text) - 1));
  end;

  function ProcGroupToProcFolder(N: TTreeNode): String;
  const
    ProcFolders: array [0..26] of String = ('arrays', 'ole', 'clipbrd', 'cond',
      'datetime', 'debugging', 'dialogs', 'dll', 'encoding', 'exceptions',
      'expressions', 'fs', 'graphics', 'json', 'math', 'menus', 'metadata',
      'misc', 'sql', 'strings', 'totals', 'utf8strings', 'useraccess',
      'variants', 'web', 'xml', 'dxforms');
    ProcFoldersWeb: array [0..17] of String = ('arrays', 'cond', 'datetime',
      'debugging', 'dll', 'encoding', 'exceptions', 'fs', 'json', 'math',
      'misc', 'strings', 'totals', 'utf8strings', 'useraccess', 'variants',
      'web', 'xml');
  var
    i: Integer;
  begin
    i := N.Index;
    if not FIsWeb and (i in [0..26]) then
      Result := ProcFolders[i]
    else if FIsWeb and (i in [0..17]) then
      Result := ProcFoldersWeb[i]
    else
      Result := '';
  end;

var
  N: TTreeNode;
  S, ClsName, ProcFolder, TkStr: String;
  Tk: Char;
  P, Len: Integer;
begin
  Result := '';
  N := Tree.Selected;
  if N = nil then Exit;

  S := N.Text;
  Len := Length(S);
  P := 1;
  TkStr := GetToken(S, Len, P, Tk);
  GetToken(S, Len, P, Tk);      // Пропускаем пробел
  if TkStr = 'property' then
  begin
    if N.Parent = nil then Exit;

    ClsName := ExtractClassName(N.Parent);
    TkStr := LowerCase(GetToken(S, Len, P, Tk));
    if Copy(TkStr, 1, 2) = 'on' then
      Result := 'classes:' + ClsName + ':e:' + TkStr
    else
      Result := 'classes:' + ClsName + ':p:' + TkStr;
  end
  else if (TkStr = 'procedure') or (TkStr = 'function') or (TkStr = 'constructor') then
  begin
    if N.Parent = nil then Exit;

    TkStr := LowerCase(GetToken(S, Len, P, Tk));
    ClsName := ExtractClassName(N.Parent);
    if ClsName <> '' then
      Result := 'classes:' + ClsName + ':m:' + TkStr
    else
    begin
      ProcFolder := ProcGroupToProcFolder(N.Parent);
      if ProcFolder <> '' then
        Result := 'procs:' + ProcFolder + ':' + TkStr;
    end;
  end
  else if Pos(' = class', S) > 0 then
    Result := 'classes:' + LowerCase(TkStr) + ':'
  else if Pos(' = ', S) > 0 then
  begin
    if N.Parent = nil then Exit;

    if N.Parent.Text = 'Events' then
      Result := 'events:' + LowerCase(TkStr)
    else if (N.Parent.Text = 'Types') or (N.Parent.Text = 'Records') then
      Result := 'types:' + LowerCase(TkStr);
  end;
end;

procedure TClassesTree.GetHelp;
var
  S: String;
begin
  S := GetHelpUrl;
  if S <> '' then OpenUrl(BASE_URL + S);
end;

procedure TClassesTree.SetHelpPanText(const S: String; Error: Boolean);
var
  Clr: String;
begin
  if Error then
    Clr := '#ffcabe'
  else
    Clr := '#fff8dc';
  FHelpPan.SetHtmlFromStr('<html><head><meta content="text/html;charset=UTF-8" ' +
    'http-equiv="Content-Type"></head><body bgcolor=' + Clr + '>' + S + '</body></html>');
end;

function TClassesTree.WikiToHtml(S: String): String;
var
  P, Len, OldP: Integer;
  St: Char;
  IsUL: Boolean;
begin
  //Debug(S);
  Result := '';
  // Удаляем заголовок и спецификацию или наследование
  P := Pos('</codedoc>', S);
  if P > 0 then
    Delete(S, 1, P + 10)
  else
  begin
    Delete(S, 1, Pos(#10, S));
    Delete(S, 1, Pos(#10, S));
  end;

  P := 1; Len := Length(S);
  St := #0;
  IsUL := False;
  while P <= Len do
  begin
    case St of
      #0:
        begin
          if (S[P] = '[') and (Copy(S, P+1, 1) = '[') then
          begin
            Inc(P);
            St := 'A';
            Result := Result + '<a href="';
          end
          // Пропускаем
          else if (S[P] = '{') and (Copy(S, P+1, 1) = '{') then
          begin
            Inc(P);
            St := '{';
          end
          else if (S[P] = '*') and (Copy(S, P+1, 1) = '*') then
          begin
            Inc(P);
            St := 'B';
            Result := Result + '<b>';
          end
          else if (S[P] = '/') and (Copy(S, P+1, 1) = '/') then
          begin
            Inc(P);
            St := 'I';
            Result := Result + '<i>';
          end
          else if (S[P] = '''') and (Copy(S, P+1, 1) = '''') then
          begin
            Inc(P);
            St := '''';
            Result := Result + '<b>';
          end
          else if S[P] = #10 then
          begin
            if IsUL then
            begin
              if (Copy(S, P-1, 1) = #10) or (Copy(S, P-2, 1) = #10) then
              begin
                IsUL := False;
                Result := Result + '</ul><br>';
              end;
            end;
            Result := Result + '<br>'
          end
          else if (S[P] = '*') and (Copy(S, P+1, 1) = ' ') and
            (Copy(S, P - 3, 3) = #10'  ') then
          begin
            if not IsUL then
            begin
              Result := Result + '<ul>';
              IsUL := True;
            end;
            Result := Result + '<li>';
          end
          else if (S[P] = '<') and (Copy(S, P + 1, 4) = 'code') then
          begin
            OldP := PosEx('>', S, P) + 1;
            P := PosEx('</code>', S, OldP);
            if P = 0 then Exit;
            Result := Result + '<code>' +
              StringReplace(Copy(S, OldP, P - OldP), #10, '<br>', [rfReplaceAll]) +
              '</code>';
            Inc(P, 7);
          end
          else
            Result := Result + S[P];
        end;
      'A':
        begin
          if S[P] = '|' then
            Result := Result + '">'
          else if (S[P] = ']') and (Copy(S, P+1, 1) = ']') then
          begin
            Inc(P);
            St := #0;
            Result := Result + '</a>';
          end
          else
            Result := Result + S[P];
        end;
      'B':
        begin
          if (S[P] = '*') and (Copy(S, P+1, 1) = '*') then
          begin
            Inc(P);
            St := #0;
            Result := Result + '</b>';
          end
          else
            Result := Result + S[P];
        end;
      'I':
        begin
          if (S[P] = '/') and (Copy(S, P+1, 1) = '/') then
          begin
            Inc(P);
            St := #0;
            Result := Result + '</i>';
          end
          else
            Result := Result + S[P];
        end;
      '''':
        begin
          if (S[P] = '''') and (Copy(S, P+1, 1) = '''') then
          begin
            Inc(P);
            St := #0;
            Result := Result + '</b>';
          end
          else
            Result := Result + S[P];
        end;
      '{':
        begin
          if (S[P] = '}') and (Copy(S, P+1, 1) = '}') then
          begin
            Inc(P);
            St := #0;
          end;
        end;
    end;
    Inc(P);
  end;
end;

function TClassesTree.GetEventHandler: String;
var
  N, NE: TTreeNode;
  S, ProcNm, TypeNm: String;
  p: SizeInt;
begin
  Result := '';
  N := Tree.Selected;
  if N = nil then Exit;
  NE := Tree.Items.TopLvlItems[NODE_EVENTS];
  S := N.Text;
  if N.Parent = NE then
    Result := Copy(S, Pos(' = ', S) + 3, 1000)
  else
  begin
    Delete(S, 1, 11);   // property On
    Delete(S, Length(S) - 4, 5); // [rw]
    p := Pos(':', S);
    ProcNm := Copy(S, 1, p - 1);
    TypeNm := Copy(S, p + 2, 100);
    N := FindTypeNode(NE, TypeNm, False);
    if N <> nil then
    begin
      S := N.Text;
      Result := Copy(S, Pos(' = ', S) + 3, 1000);
      Insert(ProcNm, Result, Pos('(', Result));
    end;
  end;
  if Result <> '' then
    Result := Result + ';' + LineEnding + 'begin' + LineEnding + 'end;' + LineEnding;
end;

procedure TClassesTree.DoGetEventHandler;
var
  S: String;
begin
  if FOnGetEventHandler = nil then Exit;
  S := GetEventHandler;
  if S <> '' then FOnGetEventHandler(Self, S);
end;

procedure TClassesTree.UpdateContextHelp;
var
  Url: String;
  CacheItem: THelpCacheItem;
begin
  if FHideContextHelp then Exit;
  if Tree.Selected = nil then
  begin
    SetHelpPanText('');
    Exit;
  end;
  FHttp.Terminate;
  Url := GetHelpUrl;
  if Url <> '' then
  begin
    CacheItem := FCache.FindHelp(Url);
    if CacheItem <> nil then
      SetHelpPanText(CacheItem.Text)
    else
    begin
      FHttp.AddHeader('url', Url);
      FHttp.Send('GET', BASE_URL_RAW + Url);
    end;
  end
  else
    SetHelpPanText(rsNoContextHelp);
end;

constructor TClassesTree.Create(TheOwner: TComponent);
var
  IL: TImageList;
  Pop: TPopupMenu;
  Prov: TIpHtmlDataProvider;
begin
  inherited Create(TheOwner);
  FCache := THelpCache.Create;

  IL := TImageList.Create(Self);
  with IL do
  begin
    AddLazarusResource('item_grey16');
    AddLazarusResource('item_yellow16');
    AddLazarusResource('item_blue16');
    AddLazarusResource('item_green16');
    AddLazarusResource('item_red16');
    AddLazarusResource('items16');
  end;

  Pop := TPopupMenu.Create(Self);
  Pop.Items.Add(CreateMenuItem(Pop, rsGoToHelp, 0, ShortCut(VK_F1, []), @MenuItemClick, -1));
  Pop.Items.Add(CreateMenuItem(Pop, '-', 1, 0, nil, -1));
  Pop.Items.Add(CreateMenuItem(Pop, rsInsertEventHandler, 2, 0, @MenuItemClick, -1));
  Pop.Items.Add(CreateMenuItem(Pop, '-', 3, 0, nil, -1));
  Pop.Items.Add(CreateMenuItem(Pop, rsHideBaseClasses, 4, 0, @MenuItemClick, -1));
  Pop.Items.Add(CreateMenuItem(Pop, rsHideAncestors, 5, 0, @MenuItemClick, -1));
  Pop.Items.Add(CreateMenuItem(Pop, rsHideContextHelp, 6, 0, @MenuItemClick, -1));
  Pop.Items.Add(CreateMenuItem(Pop, '-', 7, 0, nil, -1));
  Pop.Items.Add(CreateMenuItem(Pop, rsCollapseAll, 8, 0, @MenuItemClick, -1));
  Pop.Items[2].Default := True;
  Pop.OnPopup:=@MenuPopup;

  with Tree do
	begin
    ReadOnly := True;
    Images := IL;
    PopupMenu := Pop;

    OnMouseMove := @TreeMouseMove;
    OnMouseUp:=@TreeMouseUp;
    OnSelectionChanged := @TreeSelectionChanged;
    OnAdvancedCustomDrawItem := @TreeAdvancedDrawItem;
    OnDblClick:=@TreeDblClick;
  end;

  with Filter do
  begin
    TextHint := rsFindNode;
    BorderSpacing.Bottom := 2;
    OnAfterFilter:=@FilterAfterFilter;
  end;

  Prov := TIpHtmlDataProvider.Create(Self);
  with Prov do
  begin
    OnCanHandle:=@ProvCanHandle;
  end;

  FHelpPan := TIPHtmlPanel.Create(Self);
  with FHelpPan do
  begin
    Parent := Self;
    Height := 200;
    Align := alBottom;
    BorderStyle := bsSingle;
    DataProvider := Prov;
    DefaultFontSize := 9;
    DefaultTypeFace := 'Verdana';
    ShowHints := False;
  end;

  FHelpSplit := TSplitter.Create(Self);
  with FHelpSplit do
  begin
    Parent := Self;
    Align := alBottom;
    ResizeStyle := rsPattern;
  end;

  FHttp := THttpClient.Create;
  FHttp.MultiThreaded := True;
  FHttp.OnFinish := @HttpFinish;
  FHttp.OnError := @HttpError;
end;

destructor TClassesTree.Destroy;
begin
  FCache.Free;
  FHttp.Free;
  inherited Destroy;
end;

procedure SetNodeImageIndex(Node: TTreeNode; Index: Integer);
var
  i: Integer;
  N: TTreeNode;
begin
  Node.ImageIndex := 5;
  Node.SelectedIndex := 5;
  for i := 0 to Node.Count - 1 do
  begin
    N := Node.Items[i];
    if N.Count = 0 then
    begin
      N.ImageIndex := Index;
      N.SelectedIndex := Index;
    end
    else
    begin
      N.ImageIndex := 5;
      N.SelectedIndex := 5;
      SetNodeImageIndex(N, Index);
    end;
  end;
end;

procedure SetClassNodeImageIndex(Node: TTreeNode);
var
  i, idx: Integer;
  N: TTreeNode;
  S: String;
begin
  Node.ImageIndex := 5;
  Node.SelectedIndex := 5;
  for i := 0 to Node.Count - 1 do
  begin
    N := Node.Items[i];
    if N.Count = 0 then
    begin
      S := N.Text;
      if Pos('property', S) = 1 then
      begin
        if PosEx('On', S, 10) = 10 then idx := 1
        else if Pos('[rw]', S) > 0 then idx := 3
        else idx := 4;
      end
      else
        idx := 2;
      N.ImageIndex := idx;
      N.SelectedIndex := idx;
    end
    else
    begin
      N.ImageIndex := 5;
      N.SelectedIndex := 5;
      SetClassNodeImageIndex(N);
    end;
  end;
end;

procedure TClassesTree.BuildTree;
var
  i, p, p2: Integer;
  N: TTreeNode;
  S, ClsNm: String;
begin
  inherited BuildTree;

  if not FileExists(FFileName) then
  begin
    Enabled := False;
    Exit;
  end;

  {Tree.Items.Clear;
  Filter.Text := ''; }
  SetHelpPanText('');

  try
    Tree.LoadFromFile(FFileName);
  except
    on E: Exception do
    begin
      Tree.Items.AddChild(nil, E.Message);
      Exit;
    end;
  end;

  Tree.BeginUpdate;

  if not FHideAncestors then
    for i := 0 to Tree.Items.TopLvlCount - 1 do
    begin
      N := Tree.Items.TopLvlItems[i];
      S := N.Text;
      p := Pos(' = class(', S);
      if p > 0 then
      begin
        p := p + 9;
        p2 := Pos(')', S);
        ClsNm := Copy(S, p, p2 - p);
        AddNodes(N, ClsNm);
      end;
    end;

  i := 0;
  while i < Tree.Items.Count do
  begin
    N := Tree.Items[i];
    if (N.Parent = nil) and (Copy(N.Text, 1, 1) = '#') and FHideBaseClasses then
      N.Delete
    else
    begin
      if Copy(N.Text, 1, 1) = '#' then
        N.Text := Copy(N.Text, 2, 1024);
      Inc(i);
    end;
  end;

  SetNodeImageIndex(Tree.Items.TopLvlItems[NODE_VARS], 0);
  SetNodeImageIndex(Tree.Items.TopLvlItems[NODE_BASETYPES], 0);
  SetNodeImageIndex(Tree.Items.TopLvlItems[NODE_WORDS], 0);
  SetNodeImageIndex(Tree.Items.TopLvlItems[NODE_CONST], 1);
  SetNodeImageIndex(Tree.Items.TopLvlItems[NODE_EVENTS], 0);
  SetNodeImageIndex(Tree.Items.TopLvlItems[NODE_TYPES], 0);
  SetNodeImageIndex(Tree.Items.TopLvlItems[NODE_RECORDS], 0);
  SetNodeImageIndex(Tree.Items.TopLvlItems[NODE_PROCS], 2);

  for i := NODE_PROCS + 1 to Tree.Items.TopLvlCount - 1 do
    SetClassNodeImageIndex(Tree.Items.TopLvlItems[i]);

  Tree.EndUpdate;
end;

procedure TClassesTree.SetFilter(const FilterText: String);
begin
  Filter.Text := FilterText;
  Filter.SetFocus;
end;

end.

