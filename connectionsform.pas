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

unit ConnectionsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LazFileUtils, TreeFilterEdit, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons, Menus, appsettings, strconsts,
  LCLType, EditBtn, treeviewex;

{ TConnectsFm }

type
  TConnectAction = (cnaNone, cnaConnect, cnaDesign);

  TConnectsFm = class(TForm)
    CloseBn: TBitBtn;
    ConnectBn: TBitBtn;
    HelpBn: TBitBtn;
    DesignBn: TBitBtn;
    AddBn: TBitBtn;
    EditBn: TBitBtn;
    DelBn: TBitBtn;
    Images: TImageList;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu2: TPopupMenu;
    StatusBar: TStatusBar;
    Tree: TTreeViewEx;
    TreeFilter: TTreeFilterEdit;
    procedure AddBnClick(Sender: TObject);
    procedure ConnectBnClick(Sender: TObject);
    procedure HelpBnClick(Sender: TObject);
    procedure DelBnClick(Sender: TObject);
    procedure DesignBnClick(Sender: TObject);
    procedure EditBnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure TreeCompare(Sender: TObject; Node1, Node2: TTreeNode;
      var Compare: Integer);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    function TreeFilterFilterItem(Item: TObject; out Done: Boolean): Boolean;
    procedure TreeKeyPress(Sender: TObject; var Key: char);
    procedure TreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeSelectionChanged(Sender: TObject);
    procedure TreeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    FConnectAction: TConnectAction;
    FConnectInfo: TConnectInfo;
    function CheckCurrentDatabaseInConnects: Boolean;
    procedure AddConnection(CI: TConnectInfo);
    procedure DoRenameGroups(aNode: TTreeNode);
    procedure SortTree;
    procedure BuildTree;
    procedure Load;
    procedure SetControlState;
    function IsGroupExists(aParent, aCurrent: TTreeNode; const Group: String; IsMove: Boolean): Boolean;
  public
    { public declarations }
    function ShowForm: Integer;
    property ConnectInfo: TConnectInfo read FConnectInfo;
    property ConnectAction: TConnectAction read FConnectAction;
  end;

var
  ConnectsFm: TConnectsFm;

function ShowConnectsForm: Integer;

implementation

uses
  apputils, ConnectInfoForm, mydialogs, LazUtf8, dxusers, helpmanager, mainform,
  dbengine, crypt, Clipbrd;

function ShowConnectsForm: Integer;
begin
  if ConnectsFm = nil then
  	ConnectsFm := TConnectsFm.Create(Application);
  Result := ConnectsFm.ShowForm;
end;

{$R *.lfm}

{ TConnectsFm }

function GetNodePath(N: TTreeNode): String;
begin
  Result := '';
  while N <> nil do
  begin
		Result := N.Text + '\' + Result;
    N := N.Parent;
  end;
  Result := Copy(Result, 1, Length(Result) - 1);
end;

procedure TConnectsFm.FormCreate(Sender: TObject);
begin
  Caption := rsConnectToDB;
  TreeFilter.CharCase := ecNormal;
  TreeFilter.TextHint := rsConnectionsTreeFilter;
  ConnectBn.Caption := rsConnect;
  DesignBn.Caption := rsDesigner;
  AddBn.Caption := rsAppend;
  EditBn.Caption := rsEdit;
  DelBn.Caption := rsDelete;
  HelpBn.Caption := rsHelp;
  CloseBn.Caption := rsClose;

  MenuItem11.Caption := rsConnect;
  MenuItem12.Caption := rsDesigner;
  MenuItem3.Caption := rsAppendConnect;
  MenuItem4.Caption := rsAppendCurrentDB;
  MenuItem5.Caption := rsAddGroup;
  MenuItem1.Caption := rsCopyConnect;
  MenuItem7.Caption := rsEdit;
  MenuItem8.Caption := rsDelete;
  MenuItem10.Caption := rsRemoveFromGroup;
  MenuItem15.Caption := rsOpenFileLocation;
  MenuItem16.Caption := rsCopyURLToClipboard;

  AddBn.Hint := rsAppendConnectToExistDB;
  EditBn.Hint := rsChangeConnectOrGroup;
  DelBn.Hint := rsDeleteConnectOrEmptyGroup;

	Images.AddLazarusResource('folder16');
  Images.AddLazarusResource('db16');
  Tree.IsWine := AppConfig.IsWine;
  Load;
end;

procedure TConnectsFm.FormShow(Sender: TObject);
begin
  TreeFilter.Clear;
  TreeFilter.ResetFilter;
  Tree.SetFocus;
  SetControlState;
end;

procedure TConnectsFm.MenuItem10Click(Sender: TObject);
var
  N: TTreeNode;
begin
  N := Tree.Selected;
  if (N.Data = nil) and IsGroupExists(nil, nil, N.Text, True) then Exit;
  N.MoveTo(nil, naAdd);
  SortTree;
  if N.Data = nil then DoRenameGroups(N)
  else TConnectInfo(N.Data).Group := '';
end;

procedure TConnectsFm.MenuItem11Click(Sender: TObject);
begin
  ConnectBn.Click;
end;

procedure TConnectsFm.MenuItem12Click(Sender: TObject);
begin
  DesignBn.Click;
end;

procedure TConnectsFm.MenuItem15Click(Sender: TObject);
var
  S, FM: String;
begin
  S := TConnectInfo(Tree.Selected.Data).DBPath;
  S := GetAbsolutePath(ExtractFilePath(S)) + ExtractFileName(S);
  {$ifdef windows}
  ShellExec('open', 'explorer.exe', '/select, ' + QuoteStr(S), '', SW_SHOWNORMAL);
  {$else}
  FM := GetUnixFileManager;
  if S <> '' then
    ShellExec('', FM, QuoteStr(ExtractFilePath(S)), '', 0)
  else
    ErrMsg(rsUnableFindFileManager);
  {$endif}
end;

procedure TConnectsFm.MenuItem16Click(Sender: TObject);
var
  CI: TConnectInfo;
  S: String;
begin
  CI := TConnectInfo(Tree.Selected.Data);
  S := 'dxdb://' + CI.DBPath;
  if CI.User <> '' then S := S + '&u:' + CI.User;
  if CI.Pwd <> '' then S := S + '&p:' + Encrypt(CI.Pwd, StartKey, MultKey, AddKey);
  if CI.DBPwd <> '' then S := S + '&dbpwd:' + Encrypt(CI.DBPwd, StartKey, MultKey, AddKey);
  Clipboard.AsText:=S;
  Info(Format(rsURLSuccessCopied, [LineEnding + S]));
end;

procedure TConnectsFm.MenuItem1Click(Sender: TObject);
var
  SrcCI, CI: TConnectInfo;
begin
  SrcCI := TConnectInfo(Tree.Selected.Data);
  CI := AppConfig.Connects.AddConnection;
  CI.Name := Format(rsConnectNameCopy, [SrcCI.Name]);
  CI.DBPath := SrcCI.DBPath;
  CI.DBPwd := SrcCI.DBPwd;
  CI.TemplateDir := SrcCI.TemplateDir;
  CI.OutputDir := SrcCI.OutputDir;
  CI.User := SrcCI.User;
  CI.Pwd := SrcCI.Pwd;
  AddConnection(CI);
end;

procedure TConnectsFm.MenuItem3Click(Sender: TObject);
begin
  AddBn.Click;
end;

procedure TConnectsFm.MenuItem4Click(Sender: TObject);
var
  CI: TConnectInfo;
begin
  if not CheckCurrentDatabaseInConnects then Exit;
  CI := AppConfig.Connects.AddConnection;
  CI.Name := Utf8UpperCase(ExtractFileNameOnly(AppConfig.Database));
  CI.DBPath := AppConfig.Database;
  CI.DBPwd := AppConfig.Pwd;
  CI.TemplateDir := AppConfig.TemplateDir;
  CI.OutputDir := AppConfig.OutputDir;
  if UserMan.CurrentUser <> nil then
	  CI.User := UserMan.CurrentUser.Name;
  AddConnection(CI);
end;

procedure TConnectsFm.MenuItem5Click(Sender: TObject);
var
  S: String;
  N: TTreeNode;
begin
  S := '';
  N := Tree.Selected;

  if ShowGroupNameDlg(S) <> mrOk then Exit;

  if (N <> nil) and (N.Data <> nil) then N := N.Parent;

  if IsGroupExists(N, nil, S, False) then Exit;

  N := Tree.Items.AddChild(N, S);
  N.Selected := True;
  SetNodeImageIndex(N, 0);
  SortTree;
end;

procedure TConnectsFm.MenuItem7Click(Sender: TObject);
begin
  EditBn.Click;
end;

procedure TConnectsFm.MenuItem8Click(Sender: TObject);
begin
  DelBn.Click;
end;

procedure TConnectsFm.PopupMenu2Popup(Sender: TObject);
begin
  SetControlState;
end;

procedure TConnectsFm.TreeCompare(Sender: TObject; Node1, Node2: TTreeNode;
  var Compare: Integer);
begin
  if (Node1.Data = nil) and (Node2.Data <> nil) then
  	Compare := 1
  else if (Node1.Data <> nil) and (Node2.Data = nil) then
  	Compare := -1
  else
  	Compare := MyUtf8CompareText(Node1.Text, Node2.Text);
end;

procedure TConnectsFm.TreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  fs: TFontStyles;
begin
  fs := [];
  if (Node.Data <> nil) and
  	(TConnectInfo(Node.Data).Name = AppConfig.ConnectName) then
  begin
    if DBase.Connected then fs := [fsBold]
    else fs := [fsItalic];
  end;
  Tree.Canvas.Font.Style := fs;
end;

procedure TConnectsFm.TreeDblClick(Sender: TObject);
var
  P: TPoint;
  N: TTreeNode;
begin
  P := Tree.ScreenToClient(Mouse.CursorPos);
  N := Tree.GetNodeAt(P.X, P.Y);
  if (N <> nil) and (N.Data <> nil) and (N = Tree.Selected) then
  begin
    // В 1.8.2 курсор иногда стал принимать вид знака "Стоп". Поэтому принудительно
    // завершает Drag&Drop.
    //Tree.EndDrag(False);
    //
    ConnectBn.Click;
  end;
end;

procedure TConnectsFm.TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Target, N: TTreeNode;
begin
  N := Tree.Selected;
  Target := Tree.GetNodeAt(X, Y);
  if Target.Data <> nil then Target := Target.Parent;
  if (Target = N.Parent) or ((N.Data = nil) and IsGroupExists(Target, nil, N.Text, True)) then Exit;

  if Target <> nil then
	  N.MoveTo(Target, naAddChild)
  else
    N.MoveTo(Target, naAdd);
  SortTree;
  if N.Data = nil then
	  DoRenameGroups(N)
  else
    TConnectInfo(N.Data).Group := GetNodePath(N.Parent);
end;

procedure TConnectsFm.TreeDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);

  function IsSelectedParent(Node: TTreeNode): Boolean;
  begin
    Result := False;
    while Node <> nil do
	  begin
      if Node = Tree.Selected then Exit(True);
      Node := Node.Parent;
    end;
  end;

var
  N: TTreeNode;
begin
  N := Tree.GetNodeAt(X, Y);
  Accept := (Source = Sender) and (N <> nil){ and (N.Data = nil)} and
  	(not IsSelectedParent(N));
end;

function TConnectsFm.TreeFilterFilterItem(Item: TObject; out Done: Boolean
  ): Boolean;
var
  S: String;
  CI: TConnectInfo;
begin
  if TreeFilter.Text = '' then Exit;
  Done := True;
  CI := TConnectInfo(Item);
  if CI = nil then Exit(False)
  else
  begin
	  S := Utf8LowerCase(TreeFilter.Text);
	  Result := (Utf8Pos(S, Utf8LowerCase(CI.Name)) > 0) or
    	(Utf8Pos(S, Utf8LowerCase(CI.DBPath)) > 0);
  end;
end;

procedure TConnectsFm.TreeKeyPress(Sender: TObject; var Key: char);
var
  N: TTreeNode;
begin
  if Key = #13 then
  begin
	  N := Tree.Selected;
  	if (N <> nil) and (N.Data <> nil) then ConnectBn.Click;
  end;
end;

procedure TConnectsFm.TreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and not (ssDouble in Shift) then
  begin
  	if Tree.GetHitTestInfoAt(X, Y) * [htOnLabel, htOnIcon] <> [] then
      Tree.BeginDrag(False);
  end;
end;

procedure TConnectsFm.TreeSelectionChanged(Sender: TObject);
begin
  SetControlState;
end;

procedure TConnectsFm.TreeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if Utf8Key < ' ' then Exit;
  TreeFilter.SetFocus;
  TreeFilter.Text:=Utf8Key;
  TreeFilter.SelStart := 2;
end;

function TConnectsFm.CheckCurrentDatabaseInConnects: Boolean;
var
  i: Integer;
  CI: TConnectInfo;
  S, User: String;
begin
  Result := True;
  if AppConfig.ConnectName <> '' then
  begin
    Info(Format(rsConnectionAlreadyAdded, [AppConfig.ConnectName]));
    Exit(False);
  end;
  if UserMan.CurrentUser <> nil then
  	User := UserMan.CurrentUser.Name
  else
  	User := '';
  S := '';
  for i := 0 to AppConfig.Connects.Count - 1 do
  begin
    CI := AppConfig.Connects[i];
    if (MyUtf8CompareText(CI.DBPath, AppConfig.Database) = 0) and
    	(MyUtf8CompareText(CI.User, User) = 0) then
      begin
        if CI.Group <> '' then
	    		S := S + CI.Group + ': ';
        S := S + CI.Name + LineEnding;
      end;
  end;
  if S <> '' then
  begin
	  Result := Confirm(rsWarning, Format(rsConnectAlreadyExists,
    	[LineEnding + LineEnding + S + LineEnding])) = mrYes;
  end;
end;

function GetParentGroup(N: TTreeNode): TTreeNode;
begin
  Result := N;
  if (N <> nil) and (N.Data <> nil) then Result := N.Parent;
end;

procedure TConnectsFm.AddConnection(CI: TConnectInfo);
var
  N: TTreeNode;
begin
  if ShowConnectInfoForm(CI) = mrOk then
  begin
    N := Tree.Items.AddChildObject(GetParentGroup(Tree.Selected), CI.Name, CI);
    SetNodeImageIndex(N, 1);
    N.Selected := True;
    CI.Group := GetNodePath(N.Parent);
    SortTree;
  end
  else
  	AppConfig.Connects.DeleteConnection(CI);
end;

procedure TConnectsFm.DoRenameGroups(aNode: TTreeNode);
var
  i: Integer;
  N: TTreeNode;
begin
  for i := 0 to aNode.Count - 1 do
  begin
    N := aNode.Items[i];
    if N.Data = nil then DoRenameGroups(N)
    else TConnectInfo(N.Data).Group := GetNodePath(N.Parent);
  end;
end;

procedure TConnectsFm.SortTree;
begin
  Tree.SortType:=stNone;
  Tree.SortType:=stBoth;
end;

procedure TConnectsFm.AddBnClick(Sender: TObject);
begin
  AddConnection(AppConfig.Connects.AddConnection);
end;

procedure TConnectsFm.ConnectBnClick(Sender: TObject);
begin
  FConnectInfo := TConnectInfo(Tree.Selected.Data);
  FConnectAction := cnaConnect;
  ModalResult := mrOk;
end;

procedure TConnectsFm.HelpBnClick(Sender: TObject);
begin
  OpenHelp('connections');
end;

procedure TConnectsFm.DelBnClick(Sender: TObject);
var
  N: TTreeNode;
  CI: TConnectInfo;
  CurCon: Boolean;
begin
  N := Tree.Selected;
  if N.Data <> nil then
  begin
    CI := TConnectInfo(N.Data);
    CurCon := MyUtf8CompareText(CI.Name, AppConfig.ConnectName) = 0;
    if CurCon and DBase.Connected then
    begin
      ErrMsg(rsCantDelCurConnect);
      Exit;
    end;
    if Confirm(rsWarning, rsDeleteConnectInfoFromList) = mrNo then Exit;
    if AppConfig.DeleteRecent(CI.Name) then
	    MainFm.BuildRecentsMenu;
	  AppConfig.Connects.DeleteConnection(CI);
    // Позволяем удалять неактивное текущее соединение, стираем сведения о
    // подключении.
    if CurCon then AppConfig.ClearConnectInfo;
  end
  else if Confirm(rsWarning, rsConfirmDeleteGroup) = mrNo then Exit;
  N.Delete;
end;

procedure TConnectsFm.DesignBnClick(Sender: TObject);
begin
  FConnectInfo := TConnectInfo(Tree.Selected.Data);
  FConnectAction := cnaDesign;
  ModalResult := mrOk;
end;

procedure TConnectsFm.EditBnClick(Sender: TObject);
var
  CI: TConnectInfo;
  N: TTreeNode;
  S, OldName: String;
  i: Integer;
begin
  N := Tree.Selected;
  CI := TConnectInfo(N.Data);
  if CI <> nil then
  begin
    OldName := CI.Name;
	  if ShowConnectInfoForm(CI) = mrOk then
    begin
  		N.Text := CI.Name;
      SortTree;

      if MyUtf8CompareText(CI.Name, OldName) <> 0 then
      begin
        i := AppConfig.FindRecent(OldName);
		    if i >= 0 then
        begin
          AppConfig.Recents[i].ConnectName := CI.Name;
          MainFm.BuildRecentsMenu;
        end;
      end;
      if AppConfig.ConnectName = OldName then
      begin
      	AppConfig.ConnectName := CI.Name;
	      if ConnectInfoFm.Modified and DBase.Connected then
  	    	Info(rsCurrentConnectSettingsChanged);
      end;
    end;
  end
  else
  begin
    S := N.Text;
    if (ShowGroupNameDlg(S) = mrOk) and not IsGroupExists(N.Parent, N, S, False) then
    begin
      N.Text := S;
      DoRenameGroups(N);
      SortTree;
    end;
  end;
end;

procedure TConnectsFm.BuildTree;

  function FindNodeLevel(const S: String; L: Integer): TTreeNode;
  var
    i: Integer;
    N: TTreeNode;
  begin
    Result := nil;
    for i := 0 to Tree.Items.Count - 1 do
    begin
      N := Tree.Items[i];
      if (MyUtf8CompareText(N.Text, S) = 0) and (N.Level = L) then Exit(N);
    end;
  end;

  function FindOrAddGroup(const Group: String): TTreeNode;
  var
    SL: TStringList;
    i, j: Integer;
    N, PN: TTreeNode;
  begin
    N := nil; PN := nil;
    SL := TStringList.Create;
    SplitStr(Group, '\', SL);
    for i := 0 to SL.Count - 1 do
    begin
      N := FindNodeLevel(SL[i], i);
      if N = nil then
      begin
        for j := i to SL.Count - 1 do
        begin
          N := Tree.Items.AddChild(PN, SL[j]);
          SetNodeImageIndex(N, 0);
          PN := N;
        end;
        Break;
      end;
      PN := N;
    end;
    SL.Free;
    Result := N;
  end;

var
  i: Integer;
  DB: TConnectInfo;
  N, PN: TTreeNode;
begin
  Tree.BeginUpdate;
  Tree.Items.Clear;
  for i := 0 to AppConfig.Connects.Count - 1 do
  begin
    DB := AppConfig.Connects[i];
    PN := FindOrAddGroup(DB.Group);
    N := Tree.Items.AddChildObject(PN, DB.Name, DB);
    SetNodeImageIndex(N, 1);
  end;
  for i := 0 to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    if N.Count > 0 then N.Expand(False);
  end;
  SortTree;
  Tree.EndUpdate;
end;

procedure TConnectsFm.Load;
begin
  BuildTree;
end;

procedure TConnectsFm.SetControlState;
var
  N: TTreeNode;
  isDB, isN: Boolean;
begin
  N := Tree.Selected;
  isN := N <> nil;
  isDB := isN and (N.Data <> nil);
  ConnectBn.Enabled := isDB;
  DesignBn.Enabled := isDB;
  EditBn.Enabled := isN;
  DelBn.Enabled := isN and (N.Count = 0);

  MenuItem11.Enabled := isDB;
  MenuItem12.Enabled := isDB;
  MenuItem7.Enabled := isN;
  MenuItem8.Enabled := isN;
  MenuItem1.Enabled := isDB;
  MenuItem10.Enabled := isN and (N.Parent <> nil);
  MenuItem4.Enabled := DBase.Connected;
  MenuItem15.Enabled := isDB and not TConnectInfo(N.Data).IsRemote;
  MenuItem16.Enabled := isDB and TConnectInfo(N.Data).IsRemote;

  if isDB then
  	StatusBar.SimpleText := TConnectInfo(N.Data).DBPath
  else
    StatusBar.SimpleText := '';
end;

function TConnectsFm.IsGroupExists(aParent, aCurrent: TTreeNode;
  const Group: String; IsMove: Boolean): Boolean;
var
  i: Integer;
  N: TTreeNode;
begin
  Result := False;
  //if aParent <> nil then aParent := aParent.Parent;
  for i := 0 to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    if (N.Parent = aParent) and (N.Data = nil) and (N <> aCurrent) and
    	(MyUtf8CompareText(N.Text, Group) = 0) then
    begin
      if IsMove then
      	ErrMsg(rsCantMoveGroupExists)
      else
	      ErrMsg(rsThisGroupAlreadyExists);
      Exit(True);
    end;
  end;
end;

function TConnectsFm.ShowForm: Integer;
var
  CI: TConnectInfo;
  N: TTreeNode;
begin
  FConnectAction := cnaNone;
  FConnectInfo := nil;

  // Текущее подключение может быть удалено другим экземпляром приложения.
  // Сохраняем и если его нет в новом списке, то добавляем.
  if AppConfig.ConnectName <> '' then
  begin
    CI := AppConfig.Connects.FindConnection(AppConfig.ConnectName);
    if CI <> nil then AppConfig.Connects.Remove(CI);
    AppConfig.LoadConnections;
    if AppConfig.Connects.FindConnection(AppConfig.ConnectName) <> nil then
      FreeAndNil(CI)
    else if CI <> nil then
      AppConfig.Connects.Add(CI);
  end
  else
    AppConfig.LoadConnections;

  Load;

  if AppConfig.ConnectName <> '' then
  begin
    CI := AppConfig.Connects.FindConnection(AppConfig.ConnectName);
    if CI <> nil then
    begin
	    N := Tree.Items.FindNodeWithData(CI);
      TestNil(N, 'TConnectsFm.ShowForm N=nil');
  	  N.Selected := True;
    end;
  end
  else
  begin
  	N := Tree.Items.GetFirstNode;
    if N <> nil then N.Selected := True;
  end;
  Result := ShowModal;
  AppConfig.SaveConnections;
end;

end.

