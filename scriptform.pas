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
unit ScriptForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, SynHighlighterPas, Forms, Controls,
  Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Menus, strconsts, dxctrls,
  LclType, EditBtn, Buttons, SynEditTypes, SynEdit, SynEditMarks, SynEditMarkup,
  SynEditMiscClasses, scriptmanager, scriptedit;

type

  { TScriptFm }

  TScriptFm = class(TForm)
    Edit1: TEditButton;
    ImageList2: TImageList;
    ImageList3: TImageList;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MsgImgs: TImageList;
    Msgs: TListView;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Notebook: TNotebook;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    StaticText2: TStaticText;
    StatusBar: TStatusBar;
    SynPasSyn1: TSynPasSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    FullScreenBn: TToolButton;
    ToolButton17: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    Tree: TTreeView;
    Modules: TTreeView;
    procedure Edit1ButtonClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem33Click(Sender: TObject);
    procedure MenuItem34Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure ModulesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ModulesSelectionChanged(Sender: TObject);
    procedure MsgsClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure PopupMenu3Popup(Sender: TObject);
    procedure FullScreenBnClick(Sender: TObject);
    procedure ToolButton17Click(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TreeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { private declarations }
    FOldNode, FMainNode, FModNode, FFmNode, {FExtNode, }FExprNode: TTreeNode;
    FCurrentIndex: Integer;
    FSearchStr: String;
    FSearchText, FReplaceText: String;
    FSearchOptions: TSynSearchOptions;
    Memo: TScriptEdit;
    FModulesWidth, FClassTreeWidth, FMsgsHeight: Integer;
    FFullScreen: Boolean;
    procedure FillModules;
    //procedure UpdateModulesMenuState;
    procedure AddNodes(Target: TTreeNode; const ClsName: String);
    procedure BuildTree;
    procedure FindNextClassItem(const S: String);
    procedure FindPrevClassItem(const S: String);
    procedure DoSearchText(ContinueSearch, Backward: Boolean);
    procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShowSearchForm(Replace: Boolean);
    procedure SetControlState;
    procedure CreateEditor(SD: TScriptData);
    procedure FindEditor(SD: TScriptData);
    procedure DeleteEditor(SD: TScriptData);
    procedure FindBookmark(bm: Integer);
    procedure UpdateStatusBar;
    procedure UpdateCanTestForm;
    procedure ClearModifiedStatus;
    procedure ShowSQLEditor;
    function CanTestForm(var Fm: TdxForm): Boolean;
    procedure AddModule(const ModuleName, Source: String; Kind: TScriptKind);
    procedure FillCompilerMessages;
    procedure EnableFullScreen(Value: Boolean);
  public
    { public declarations }
    procedure Init;
    procedure ShowForm;
    procedure SaveAll;
    procedure RenameForm(FmId: Integer);
    procedure DeleteForm(FmId: Integer);
    procedure GoToBreakpoint(SD: TScriptData; Mark: TSourceMarkData);
  end;

var
  ScriptFm: TScriptFm;

implementation

uses
  apputils, formmanager, selectform, appsettings, inputform,
  LazUtf8, SearchReplaceForm, SqlForm, designerframe, breakpointsform,
  langmanager;

{$R *.lfm}

function CheckModuleName(const S: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not (S[1] in ['A'..'Z', 'a'..'z', '_']) then Exit;
  for i := 2 to Length(S) do
    if not (S[i] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) then Exit;
  Result := True;
end;

function ExportModuleDlg(Kind: TScriptKind): String;
var
  Flt: String;
begin
  Result := '';
  Flt := '';
  case Kind of
    skForm: Flt := rsFormModulesFlt;
    skUser, skMain: Flt := rsUserModulesFlt;
    skExpr: Flt := rsExprModulesFlt;
  end;
  Flt := Flt + rsAllFilesFlt;
  with TSaveDialog.Create(nil) do
  begin
    Title := rsExportModule;
    Options:=Options + [ofOverwritePrompt, ofPathMustExist];
    Filter := Flt;
    if Execute then
    	Result := FileName;
    Free;
  end;
end;

function ImportModuleDlg: String;
var
  Flt: String;
begin
  Result := '';
  Flt := rsAllModulesFlt + rsUserModulesFlt + rsExprModulesFlt +
  	rsAllFilesFlt;
  with TOpenDialog.Create(nil) do
  begin
    Title := rsImportModule;
    Options := Options + [ofFileMustExist];
    Filter := Flt;
  	if Execute then
    	Result := FileName;
    Free;
  end;
end;

{ TScriptFm }

procedure TScriptFm.FillModules;
var
  N, PN: TTreeNode;
  i: Integer;
  SD: TScriptData;
  Fm: TdxForm;
  SL: TStringList;
begin
  Modules.BeginUpdate;
  SL := TStringList.Create;
  Modules.Items.Clear;
  N := Modules.Items.AddChild(nil, 'Main');
  N.Data := ScriptMan.FindScriptByName('Main');
  N.ImageIndex := 7;
  N.SelectedIndex := 7;
  FMainNode := N;

  N := Modules.Items.AddChild(nil, rsUserModules);
  FModNode := N;
  N.ImageIndex := 6;
  N.SelectedIndex := 6;
  ScriptMan.ModulesToList(SL, skUser);
  {for i := 0 to ScriptMan.ScriptCount - 1 do
  begin
    SD := ScriptMan.Scripts[i];
    if  (SD.Kind = skUser) and (SD.Name <> 'Main') then
      SL.AddObject(SD.Name, SD);
  end;   }
  SL.Sort;
  for i := 0 to SL.Count - 1 do
  begin
    SD := TScriptData(SL.Objects[i]);
    Modules.Items.AddChildObject(N, SD.Name, SD);
  end;
  N.Expand(True);

  N := Modules.Items.AddChild(nil, rsFormsModules);
  FFmNode := N;
  N.ImageIndex := 6;
  N.SelectedIndex := 6;
  FormMan.AllFormsToList(SL);
  for i := 0 to SL.Count - 1 do
  begin
    Fm := TdxForm(SL.Objects[i]);
    SD := ScriptMan.FindScript(Fm.Id);
    if SD <> nil then
    begin
      if Fm.PId = 0 then
        PN := Modules.Items.AddChildObject(N, Fm.FormCaption + ' (' + Fm.Name + ')', SD)
      else
        Modules.Items.AddChildObject(PN, Fm.FormCaption + ' (' + Fm.Name + ')', SD);
    end;
  end;
  N.Expand(True);

  {FExtNode := Modules.Items.AddChild(nil, rsExtensions);
  FExtNode.ImageIndex := 6;
  FExtNode.SelectedIndex := 6;  }
  FExprNode := Modules.Items.AddChild(nil, rsExpressionModules);
  FExprNode.ImageIndex := 6;
  FExprNode.SelectedIndex := 6;
  ScriptMan.ModulesToList(SL, skExpr);
  {SL.Clear;
  for i := 0 to ScriptMan.ScriptCount - 1 do
  begin
    SD := ScriptMan.Scripts[i];
    if SD.Kind = skExpr then
    	SL.AddObject(SD.Name, SD);
  end; }
  SL.Sort;

  for i := 0 to SL.Count - 1 do
  begin
  	Modules.Items.AddChildObject(FExprNode, SL[i], SL.Objects[i]);
  end;

  FExprNode.Expand(True);

  for i := 0 to Modules.Items.Count - 1 do
  begin
    N := Modules.Items[i];
    if (N.Parent = nil) or (N = FExprNode) then Continue;
    N.ImageIndex := 7;
    N.SelectedIndex := 7;
  end;

  SL.Free;
  Modules.EndUpdate;
end;

procedure TScriptFm.AddNodes(Target: TTreeNode; const ClsName: String);
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
    for i := 0 to Tree.Items.Count - 1 do
    begin
      N2 := Tree.Items[i];
      if (N2.Parent = nil) and (Pos(ClsName + ' = class', N2.Text) > 0) then Exit(N2);
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

procedure TScriptFm.BuildTree;
var
  i, p, p2: Integer;
  N: TTreeNode;
  S, ClsNm: String;
begin
  if not FileExists(LangMan.ClassesFile) then
  begin
    Panel2.Enabled := False;
    Exit;
  end;

  Tree.BeginUpdate;
  //Tree.LoadFromFile(AppPath + 'languages' + DirectorySeparator + 'classes.dat');
  Tree.LoadFromFile(LangMan.ClassesFile);
  i := 0;
  while i < Tree.Items.Count do
  begin
    N := Tree.Items[i];
    if N.Parent = nil then
    begin
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
    Inc(i);
  end;

  i := 0;
  while i < Tree.Items.Count do
  begin
    N := Tree.Items[i];
    if (N.Parent = nil) and (Copy(N.Text, 1, 1) = '#') and AppConfig.HideParents then
      N.Delete
    else
    begin
      if Copy(N.Text, 1, 1) = '#' then
        N.Text := Copy(N.Text, 2, 1024);
      Inc(i);
    end;
  end;

  for i := 0 to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    if N.Count > 0 then
    begin
      N.ImageIndex:=6;
      N.SelectedIndex:=6;
    end
    else
    begin
      S := N.Text;
      if Pos('property', S) > 0 then
      begin
        if Pos('[rw]', S) > 0 then
        begin
          N.ImageIndex := 4;
          N.SelectedIndex := 4;
        end
        else
        begin
          N.ImageIndex := 5;
          N.SelectedIndex := 5;
        end;
      end
      else if (N.Parent <> nil) and (N.GetParentNodeOfAbsoluteLevel(0).Text = 'Constants') then
      begin
        N.ImageIndex := 2;
        N.SelectedIndex := 2;
      end
      else if (N.Parent <> nil) and (Pos(N.GetParentNodeOfAbsoluteLevel(0).Text, 'Types|Events') > 0) then
      begin
        N.ImageIndex := 1;
        N.SelectedIndex := 1;
      end
      else if (Pos('function', S) > 0) or (Pos('procedure', S) > 0) or
        (Pos('constructor', S) > 0) then
      begin
        N.ImageIndex := 3;
        N.SelectedIndex := 3;
      end
      else
      begin
        N.ImageIndex := 1;
        N.SelectedIndex := 1;
      end;
    end;
  end;
  Tree.EndUpdate;
end;

procedure TScriptFm.FindNextClassItem(const S: String);
var
  i: Integer;
  N: TTreeNode;
  NS: String;
begin
  if S = '' then Exit;
  for i := FCurrentIndex to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    NS := LowerCase(N.Text);
    if Pos(S, NS) > 0 then
    begin
      FCurrentIndex := i;
      N.Selected := True;
      Exit;
    end;
  end;
end;

procedure TScriptFm.FindPrevClassItem(const S: String);
var
  i: Integer;
  N: TTreeNode;
  NS: String;
begin
  if S = '' then Exit;
  for i := FCurrentIndex downto 0 do
  begin
    N := Tree.Items[i];
    NS := LowerCase(N.Text);
    if Pos(S, NS) > 0 then
    begin
      FCurrentIndex := i;
      N.Selected := True;
      Exit;
    end;
  end;
end;

procedure TScriptFm.DoSearchText(ContinueSearch, Backward: Boolean);
var
  n: Integer;
begin
  if FSearchText = '' then Exit;

  if ContinueSearch then
    Include(FSearchOptions, ssoFindContinue)
  else
    Exclude(FSearchOptions, ssoFindContinue);
  if Backward then
    Include(FSearchOptions, ssoBackwards)
  else
    Exclude(FSearchOptions, ssoBackwards);
  n := Memo.SearchReplace(FSearchText, FReplaceText, FSearchOptions);
  if n = 0 then
    MessageDlg(rsWarning, Format(rsSearchTextNotFound, [FSearchText]), mtInformation, [mbOk], 0)
end;

function FindBreakPointMark(Memo: TSynEdit; Line: Integer): TSynEditMark;
var
  Mrk: TSynEditMark;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Memo.Marks.Count - 1 do
  begin
    Mrk := Memo.Marks[i];
    if (Mrk.Line = Line) and (Mrk.IsBookmark = False) then
    	Exit(Mrk);
  end;
end;

procedure TScriptFm.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
  Edit: TScriptEdit;
begin
  if (Key in [VK_0..VK_9]) and (Shift = [ssCtrl]) then
  begin
  	FindBookmark(Key - VK_0);
    Key := 0;
  end
  else if (Key in [VK_0..VK_9]) and (Shift = [ssCtrl, ssShift]) then
  begin
    for i := 0 to Notebook.PageCount - 1 do
    begin
      Edit := TScriptEdit(Notebook.Page[i].Controls[0]);
      if Edit = Memo then Continue;
      Edit.FindAndDeleteBookmark(Key - VK_0);
    end;
  end;
end;

procedure TScriptFm.ShowSearchForm(Replace: Boolean);
var
  Opt: TSynSearchOptions;
  mr: Integer;
  S: String;
begin
  Opt := [];
  S := Memo.GetWordAtRowCol(Memo.CaretXY);
  with SearchReplaceFm do
  begin
    if Memo.SelText <> '' then
      Where.ItemIndex := 1
    else
      Where.ItemIndex := 0;
    if Replace then SetReplace
    else SetFind;
    if S <> '' then SearchText.Text := S;
    mr := ShowForm;
    if mr in [mrOk, mrYesToAll] then
    begin
      if Options.Checked[0] then
        Opt := Opt + [ssoWholeWord];
      if Options.Checked[1] then
        Opt := Opt + [ssoMatchCase];
      if Start.ItemIndex = 1 then
        Opt := Opt + [ssoEntireScope];
      if Where.ItemIndex = 1 then
        Opt := Opt + [ssoSelectedOnly];
      if CheckBox1.Checked then
      begin
        FReplaceText := ReplaceText.Text;
        if mr = mrYesToAll then
          Opt := Opt + [ssoReplaceAll]
        else
          Opt := Opt + [ssoReplace];
      end
      else
        FReplaceText := '';
      FSearchOptions:=Opt;
      FSearchText:=SearchText.Text;
      DoSearchText(False, False);
    end;
  end;
end;

procedure TScriptFm.SetControlState;
var
  N: TTreeNode;
begin
  N := Modules.Selected;
  MenuItem5.Enabled := (N <> nil) and (N.Parent <> nil) and
    ((N.Parent = FModNode) or (N.Parent = FExprNode));
  MenuItem4.Enabled := (N <> nil) and (N.Parent <> nil);
  MenuItem29.Enabled := (N <> nil) and (N.Parent <> nil);
  ToolButton16.Enabled := MenuItem28.Enabled;

  MenuItem12.Enabled := (Memo <> nil) and (Memo.SelText <> '');
  MenuItem13.Enabled := (Memo <> nil) and (Memo.SelText <> '');
  MenuItem14.Enabled := (Memo <> nil) and (Memo.CanPaste);
  MenuItem16.Enabled := (Memo <> nil) and (Memo.CanUndo);
  MenuItem17.Enabled := (Memo <> nil) and (Memo.CanRedo);
  MenuItem20.Enabled := (Memo <> nil) and (FSearchText <> '');
  MenuItem21.Enabled := (Memo <> nil) and (FSearchText <> '');
  MenuItem22.Enabled := (Memo <> nil) and (Memo.GetWordAtRowCol(Memo.CaretXY) <> '');

  ToolButton1.Enabled := MenuItem12.Enabled;
  ToolButton2.Enabled := MenuItem13.Enabled;
  ToolButton3.Enabled := MenuItem14.Enabled;
  ToolButton4.Enabled := MenuItem16.Enabled;
  ToolButton5.Enabled := MenuItem17.Enabled;
  ToolButton6.Enabled := (Memo <> nil);
  ToolButton7.Enabled := (Memo <> nil);
end;

procedure TScriptFm.CreateEditor(SD: TScriptData);
var
  i: Integer;
  Pg: TPage;
  M: TScriptEdit;
begin
  i := Notebook.Pages.Add('');
  Pg := Notebook.Page[i];
  Pg.Tag := PtrInt(SD);
  M := TScriptEdit.Create(Self);
  M.Parent := Pg;
  M.Align := alClient;
  M.OnStatusChange:=@MemoStatusChange;
  M.OnKeyDown:=@MemoKeyDown;
  M.PopupMenu := PopupMenu3;
  M.LoadData(SD);
end;

procedure TScriptFm.FindEditor(SD: TScriptData);
var
  i: Integer;
  Pg: TPage;
begin
  for i := 0 to Notebook.PageCount - 1 do
  begin
    Pg := Notebook.Page[i];
    if Pg.Tag = PtrInt(SD) then
    begin
      Memo := TScriptEdit(Pg.Controls[0]);
      Notebook.PageIndex:=i;
      // Восстанавливаем состояние редактора только один раз.
      if Memo.Tag = 0 then
      begin
        Memo.RestoreState;
      	Memo.Tag := 1;
      end;
      Break;
    end;
  end;
end;

procedure TScriptFm.DeleteEditor(SD: TScriptData);
var
  i: Integer;
  Pg: TPage;
begin
  for i := 0 to Notebook.PageCount - 1 do
  begin
    Pg := Notebook.Page[i];
    if Pg.Tag = PtrInt(SD) then
    begin
      Pg.Free;
      Break;
    end;
  end;
end;

procedure TScriptFm.FindBookmark(bm: Integer);
var
  i, j: Integer;
  Pg: TPage;
  M: TSynEdit;
begin
  for i := 0 to Notebook.PageCount - 1 do
  begin
    Pg := Notebook.Page[i];
    M := TSynEdit(Pg.Controls[0]);
    for j := 0 to M.Marks.Count - 1 do
    begin
      if M.Marks[j].BookmarkNumber = bm then
      begin
        if Notebook.PageIndex <> i then
        	Modules.Selected := Modules.Items.FindNodeWithData(Pointer(Pg.Tag));
        Memo.GotoBookMark(bm);
        Memo.SetFocus;
        Exit;
      end;
    end;
  end;
end;

procedure TScriptFm.UpdateStatusBar;
begin
  if Memo = nil then
  begin
    with StatusBar do
    begin
      Panels[0].Text := '';
      Panels[1].Text := '';
      Panels[2].Text := '';
      Panels[3].Text := '';
    end;
    Exit;
  end;
  with StatusBar do
  begin
    Panels[0].Text := IntToStr(Memo.CaretY) + ': ' + IntToStr(Memo.CaretX);
    if Memo.InsertMode then
      Panels[1].Text := rsInserting
    else
      Panels[1].Text := rsReplacing;
    if Memo.Modified then
      Panels[2].Text := rsModified
    else
      Panels[2].Text := '';
    Panels[3].Text := Modules.Selected.Text;
  end;
end;

procedure TScriptFm.UpdateCanTestForm;
var
  Fm: TdxForm;
begin
  MenuItem28.Enabled := CanTestForm(Fm);
end;

procedure TScriptFm.ClearModifiedStatus;
var
  i: Integer;
  N: TTreeNode;
  SD: TScriptData;
  S: String;
begin
  for i := 0 to Modules.Items.Count - 1 do
  begin
    N := Modules.Items[i];
    SD := TScriptData(N.Data);
    if SD <> nil then
    begin
      S := N.Text;
      if Copy(S, 1, 1) = '*' then
      begin
        Delete(S, 1, 1);
        N.Text := S;
      end;
    end;
  end;
end;

procedure TScriptFm.ShowSQLEditor;
begin
	SqlFm.ShowForm;
end;

function TScriptFm.CanTestForm(var Fm: TdxForm): Boolean;
var
  N: TTreeNode;
  SD: TScriptData;
begin
  Fm := nil;
  Result := False;
  N := Modules.Selected;
  if (N = nil) or (N.Data = nil) then Exit;
  SD := TScriptData(N.Data);
  if SD.FmId = 0 then Exit;
  Fm := FormMan.FindForm(SD.FmId);
  if Fm.PId > 0 then
  	Fm := FormMan.FindForm(Fm.PId);
  Result := True;
end;

procedure TScriptFm.AddModule(const ModuleName, Source: String;
  Kind: TScriptKind);
var
  S: String;
  SD: TScriptData;
  N, PN: TTreeNode;
begin
  S := ModuleName;
  if S = '' then
	  S := InputBox(rsNewModule, rsModuleName, '');
  if S = '' then Exit;
  if not CheckModuleName(S) then
  begin
    ErrMsg(rsInvalidModuleName);
    Exit;
  end
  else if ScriptMan.FindScriptByName(S) <> nil then
  begin
    ErrMsgFmt(rsModuleAlreadyExists, [S]);
    Exit;
  end;
  SD := ScriptMan.AddScript(0, S, Source);
  SD.Kind := Kind;
  CreateEditor(SD);
  PN := FModNode;
  if Kind = skExpr then PN := FExprNode;
  N := Modules.Items.AddChildObject(PN, S, SD);
  N.Parent.Expand(False);
  N.ImageIndex := 7;
  N.SelectedIndex := 7;
  N.Selected := True;
end;

procedure TScriptFm.FillCompilerMessages;
var
  SL: TStringList;
  i: Integer;
  LI: TListItem;
begin
  SL := TStringList.Create;
  ScriptMan.MessagesToList(SL);
  Msgs.Clear;
  for i := 0 to SL.Count - 1 do
  begin
    LI := Msgs.Items.Add;
    LI.SubItems.Add(SL[i]);
    LI.Data:=SL.Objects[i];
    if TCompilerMsg(LI.Data).ErrorType = 'Error' then
	    LI.ImageIndex := 0
    else
    	LI.ImageIndex := 1;
  end;
  SL.Free;

  if not ScriptMan.HasErrors then
  begin
  	LI := Msgs.Items.Insert(0);
    LI.SubItems.Add(rsCompilationSuccess);
    LI.ImageIndex := 2;
    LI.Selected:=True;
  end;
end;

procedure TScriptFm.EnableFullScreen(Value: Boolean);
begin
  if Value then
  begin
    FModulesWidth := Modules.Width;
    FClassTreeWidth := Panel2.Width;
    FMsgsHeight := Msgs.Height;
    Modules.Width := 0;
	  Panel2.Width := 0;
  	Msgs.Height := 0;
  end
  else
  begin
    Modules.Width := FModulesWidth;
	  Panel2.Width := FClassTreeWidth;
  	Msgs.Height := FMsgsHeight;
  end;
  Splitter1.Enabled := not Value;
	Splitter2.Enabled := not Value;
  Splitter3.Enabled := not Value;
end;

procedure TScriptFm.SaveAll;
var
  i: Integer;
  Pg: TPage;
  M: TScriptEdit;
  SD: TScriptData;
begin
  for i := 0 to Notebook.PageCount - 1 do
  begin
    Pg := Notebook.Page[i];
    M := TScriptEdit(Pg.Controls[0]);
    M.Modified:=False;
    SD := TScriptData(Pg.Tag);
    SD.Source := M.Text;
    //if M.Tag = 1 then
    M.SaveState;
  end;
  ClearModifiedStatus;
  UpdateStatusBar;
end;

procedure TScriptFm.RenameForm(FmId: Integer);
var
  i: Integer;
  N: TTreeNode;
  SD: TScriptData;
  Fm: TdxForm;
  S, Ch: String;
begin
  for i := 0 to Modules.Items.Count - 1 do
  begin
    N := Modules.Items[i];
    SD := TScriptData(N.Data);
    if (N.Parent <> nil) and (SD <> nil) and (SD.FmId = FmId) then
    begin
      Fm := FormMan.FindForm(FmId);
      Ch := Copy(N.Text, 1, 1);
      S := Fm.FormCaption + ' (' + Fm.Name + ')';
      if Ch = '*' then S := Ch + S;
      N.Text := S;
      Exit;
    end;
  end;
end;

procedure TScriptFm.DeleteForm(FmId: Integer);
var
  i: Integer;
  N: TTreeNode;
  SD: TScriptData;

  procedure _Delete(Node: TTreeNode);
  var
    SD: TScriptData;
  begin
    SD := TScriptData(Node.Data);
    DeleteEditor(SD);
    ScriptMan.DeleteScript(SD);
    Node.Delete;
  end;

begin
  for i := 0 to Modules.Items.Count - 1 do
  begin
    N := Modules.Items[i];
    SD := TScriptData(N.Data);
    if (SD <> nil) and (SD.FmId = FmId) then
    begin
      while N.Count > 0 do
        _Delete(N.Items[0]);
      _Delete(N);
      Exit;
    end;
  end;
end;

procedure TScriptFm.GoToBreakpoint(SD: TScriptData; Mark: TSourceMarkData);
var
  N: TTreeNode;
begin
  N := Modules.Items.FindNodeWithData(SD);
  if N = nil then Exit;
  N.Selected := True;
  Memo.TopLine:=Mark.Row - 4;
  Memo.CaretXY := Point(1, Mark.Row);
end;

procedure TScriptFm.Init;
var
  i: Integer;
begin
  Memo := nil;
  FillModules;
  BuildTree;
  for i := 0 to ScriptMan.ScriptCount - 1 do
    CreateEditor(ScriptMan.Scripts[i]);
  Modules.Items[0].Selected := True;
end;

procedure TScriptFm.MsgsClick(Sender: TObject);
var
  Msg: TCompilerMsg;
  N: TTreeNode;
  SD: TScriptData;
begin
  if Msgs.Selected = nil then Exit;

  Msg := TCompilerMsg(Msgs.Selected.Data);
  if Msg = nil then Exit;

  if Msg.ModuleName <> '' then
	  SD := ScriptMan.FindScriptByName(Msg.ModuleName)
	else SD := Msg.SD;
  N := Modules.Items.FindNodeWithData(SD);
  N.Selected := True;

  Memo.SetFocus;
  if Msg.Pos = 0 then
	  Memo.CaretXY := Point(Msg.Col, Msg.Row)
  else
  	Memo.SelStart:=Msg.Pos;
end;

procedure TScriptFm.PopupMenu1Popup(Sender: TObject);
var
  Fm: TdxForm;
begin
  MenuItem28.Enabled := CanTestForm(Fm);
end;

procedure TScriptFm.PopupMenu2Popup(Sender: TObject);
begin
  MenuItem6.Checked := AppConfig.HideParents;
end;

procedure TScriptFm.PopupMenu3Popup(Sender: TObject);
begin
  SetControlState;
end;

procedure TScriptFm.FullScreenBnClick(Sender: TObject);
begin
  FFullScreen := not FFullScreen;
  FullScreenBn.Down := FFullScreen;
  EnableFullScreen(FFullScreen);
end;

procedure TScriptFm.ToolButton17Click(Sender: TObject);
var
  i: Integer;
begin
  SaveAll;
  if ShowBreakpointsForm = mrOk then
  	for i := 0 to Notebook.PageCount - 1 do
  		TScriptEdit(Notebook.Page[i].Controls[0]).ReloadMarks;
  if Memo <> nil then Memo.SetFocus;
end;

procedure TScriptFm.ToolButtonClick(Sender: TObject);
begin
  case TToolButton(Sender).Tag of
    0: MenuItem12.Click;
    1: MenuItem13.Click;
    2: MenuItem14.Click;
    3: MenuItem16.Click;
    4: MenuItem17.Click;
    5: MenuItem19.Click;
    6: MenuItem25.Click;
    7: MenuItem24.Click;
    8: MenuItem1.Click;
    9: MenuItem2.Click;
    10: ShowSQLEditor;
    11: DesignFr.Save;
    12: MenuItem28.Click;
  end;
end;

procedure TScriptFm.TreeDblClick(Sender: TObject);
var
  N: TTreeNode;
  S: String;
  p: SizeInt;
begin
  N := Tree.Selected;
  if (Memo = nil) or (N = nil) then Exit;
  if (N.Parent <> nil) and (N.Parent.Text = 'Events') then
  begin
    S := N.Text;
    p := Pos(' = ', S);
    if p > 0 then
  		Memo.SelText := LineEnding + Copy(S, p + 3, 1024) + ';' + LineEnding +
      	'begin' + LineEnding + LineEnding + 'end;';
  end;
end;

procedure TScriptFm.TreeMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  N: TTreeNode;
  CPos: types.TPoint;
begin
  N := Tree.GetNodeAt(X, Y);
  if FOldNode = N then Exit;
  if (N <> nil) and (N.Level * Tree.Indent + Tree.Canvas.TextWidth(N.Text) > Tree.ClientWidth - 36) then
  begin
    CPos := Tree.ClientToScreen(Point(5, Y));
    Tree.Hint := N.Text;
    Application.ActivateHint(CPos);
    FOldNode := N;
  end
  else Application.HideHint;
end;

procedure TScriptFm.TreeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  Edit1.SetFocus;
  Edit1.Text := Utf8Key;
  Edit1.SelStart:=2;
end;

procedure TScriptFm.Edit1Change(Sender: TObject);
begin
  FCurrentIndex := 0;
  FSearchStr := LowerCase(Trim(Edit1.Text));
  FindNextClassItem(FSearchStr);
end;

procedure TScriptFm.Edit1ButtonClick(Sender: TObject);
begin
  Edit1.Text := '';
  Edit1.OnChange(Edit1);
end;

procedure TScriptFm.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DOWN then
  begin
    if FCurrentIndex < Tree.Items.Count - 1 then
      Inc(FCurrentIndex);
    FindNextClassItem(FSearchStr);
    Key := 0;
  end
  else if Key = VK_UP then
  begin
    if FCurrentIndex > 0 then
      Dec(FCurrentIndex);
    FindPrevClassItem(FSearchStr);
    Key := 0;
  end;
end;

procedure TScriptFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if WindowState = wsNormal then
  begin
	  AppConfig.SEFormWidth := Width;
  	AppConfig.SEFormHeight := Height;
    if not FFullScreen then
    begin
		  AppConfig.SEModulesWidth := Modules.Width;
  		AppConfig.SEClassesWidth := Panel2.Width;
	  	AppConfig.SEMsgHeight := Msgs.Height;
    end;
  end;
end;

procedure TScriptFm.FormCreate(Sender: TObject);
begin
  Width := AppConfig.SEFormWidth;
  Height := AppConfig.SEFormHeight;
  Modules.Width := AppConfig.SEModulesWidth;
  Panel2.Width := AppConfig.SEClassesWidth;
  Msgs.Height := AppConfig.SEMsgHeight;

  Caption := rsScriptEditor;
  MenuItem1.Caption := rsAddFormModule;
  MenuItem2.Caption := rsAddUserModule;
  MenuItem5.Caption := rsRenameModule;
  MenuItem4.Caption := rsDelete;
  MenuItem27.Caption := rsImportModule;
  MenuItem29.Caption := rsExportModule;

  MenuItem8.Caption := rsFindToInternet;
  MenuItem9.Caption := rsSearchSettings;
  MenuItem6.Caption := rsHideParents;
  MenuItem11.Caption := rsCollapseAll;

  MenuItem12.Caption := rsCut;
  MenuItem13.Caption := rsCopy;
  MenuItem14.Caption := rsPaste;
  MenuItem16.Caption := rsUndo;
  MenuItem17.Caption := rsRedo;
  MenuItem19.Caption := rsFind;
  MenuItem20.Caption := rsNext;
  MenuItem21.Caption := rsFindPrevious;
  MenuItem25.Caption := rsReplace;
  MenuItem22.Caption := rsFindToInternet;
  MenuItem24.Caption := rsCompile;
  MenuItem28.Caption := rsTestForm;

  MenuItem32.Caption := rsCodeFolding;
  MenuItem33.Caption := rsFoldAllCode;
  MenuItem34.Caption := rsUnfoldAllCode;

  Edit1.TextHint := rsSearchNode;
  StaticText2.Caption := rsCompilerMsgs;

  Edit1.Button.LoadGlyphFromLazarusResource('delete16');
  SetMenuItemImage(MenuItem12, 'cut16');
  SetMenuItemImage(MenuItem13, 'copy16');
  SetMenuItemImage(MenuItem14, 'paste16');
  SetMenuItemImage(MenuItem16, 'undo16');
  SetMenuItemImage(MenuItem17, 'goto16');
  SetMenuItemImage(MenuItem19, 'find16');
  SetMenuItemImage(MenuItem22, 'help16');
  SetMenuItemImage(MenuItem24, 'play16');
  SetMenuItemImage(MenuItem28, 'test16');

  with ImageList3 do
  begin
    AddLazarusResource('cut24');
    AddLazarusResource('copy24');
    AddLazarusResource('paste24');
    AddLazarusResource('undo24');
    AddLazarusResource('redo24');
    AddLazarusResource('find24');
    AddLazarusResource('replace24');
    AddLazarusResource('compile24');
    AddLazarusResource('form24');
    AddLazarusResource('user24');
    AddLazarusResource('sql24');
    AddLazarusResource('save24');
    AddLazarusResource('test24');
    AddLazarusResource('fullscreen24');
    AddLazarusResource('breakpoint24');
  end;
  ToolButton9.Hint := rsAddFormModule;
  ToolButton10.Hint := rsAddUserModule;
  ToolButton1.Hint := rsCut;
  ToolButton2.Hint := rsCopy;
  ToolButton3.Hint := rsPaste;
  ToolButton4.Hint := rsUndo;
  ToolButton5.Hint := rsRedo;
  ToolButton6.Hint := rsFind;
  ToolButton7.Hint := rsReplace;
  ToolButton8.Hint := rsCompile;
  ToolButton12.Hint := rsSQLEditor;
  ToolButton14.Hint := rsSave;
  ToolButton16.Hint := rsTestForm;
  FullScreenBn.Hint := rsMaximizeEditor;
  ToolButton17.Hint := rsBreakpoints;

  with MsgImgs do
  begin
    AddLazarusResource('exprcheck16');
    AddLazarusResource('hint16');
    AddLazarusResource('check16');
  end;
end;

procedure TScriptFm.FormDestroy(Sender: TObject);
begin
  Memo := nil;
end;

procedure TScriptFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F4 then
  begin
    Key := 0;
    Close;
  end
  else if Key = VK_F12 then
  begin
    Key := 0;
    FullScreenBn.Click;
  end;
end;

procedure TScriptFm.MemoStatusChange(Sender: TObject; Changes: TSynStatusChanges
  );
var
  S: String;
begin
  if Memo = nil then Exit;
  SetControlState;
  if scModified in Changes then
  begin
    S := Modules.Selected.Text;
    if Memo.Modified then
      S := '*' + S
    else if Copy(S, 1, 1) = '*' then
      Delete(S, 1, 1);
    Modules.Selected.Text := S;
  end;
  UpdateStatusBar;
end;

procedure TScriptFm.MenuItem11Click(Sender: TObject);
var
  i: Integer;
begin
  Tree.BeginUpdate;
  for i := 0 to Tree.Items.Count - 1 do
    Tree.Items[i].Collapse(False);
  Tree.EndUpdate;
end;

procedure TScriptFm.MenuItem12Click(Sender: TObject);
begin
  Memo.CutToClipboard;
  SetControlState;
end;

procedure TScriptFm.MenuItem13Click(Sender: TObject);
begin
  Memo.CopyToClipboard;
  SetControlState;
end;

procedure TScriptFm.MenuItem14Click(Sender: TObject);
begin
  Memo.PasteFromClipboard;
end;

procedure TScriptFm.MenuItem16Click(Sender: TObject);
begin
  Memo.Undo;
  SetControlState;
end;

procedure TScriptFm.MenuItem17Click(Sender: TObject);
begin
  Memo.Redo;
  SetControlState;
end;

procedure TScriptFm.MenuItem19Click(Sender: TObject);
begin
  ShowSearchForm(False);
  SetControlState;
end;

procedure TScriptFm.MenuItem1Click(Sender: TObject);
var
  SL: TStringList;
  i: Integer;
  Fm, PFm: TdxForm;
  N: TTreeNode;
  SD: TScriptData;
  HasChilds: Boolean;
  SrcCode: String;
begin
  SrcCode := Format('procedure Form_Create;%0:sbegin%0:s%0:send;', [LineEnding]);
  SL := TStringList.Create;
  FormMan.AllFormsToList(SL);
  HasChilds := False;
  for i := SL.Count - 1 downto 0 do
  begin
    Fm := TdxForm(SL.Objects[i]);
    if ScriptMan.FindScript(Fm.Id) <> nil then
    begin
      if (Fm.PId = 0) and HasChilds then HasChilds := False
      else SL.Delete(i);
    end
    else
      HasChilds := Fm.PId > 0;
  end;
  if SelectFm.ShowForm(rsSelectForm, '', SL) = mrOk then
  begin
    i := SelectFm.Index;
    Fm := TdxForm(SL.Objects[i]);
    N := FFmNode;
    if Fm.PId > 0 then
    begin
      SD := ScriptMan.FindScript(Fm.PId);
      if SD = nil then
      begin
        PFm := FormMan.FindForm(Fm.PId);
        SD := ScriptMan.AddScript(PFm.Id, '', SrcCode);
        SD.Kind := skForm;
        CreateEditor(SD);
        N := Modules.Items.AddChildObject(N, PFm.FormCaption + ' (' + PFm.Name + ')', SD);
        N.ImageIndex := 7;
		    N.SelectedIndex := 7;
      end
      else
        N := Modules.Items.FindNodeWithData(SD);
    end;
    SD := ScriptMan.FindScript(Fm.Id);
    if SD = nil then
    begin
      SD := ScriptMan.AddScript(Fm.Id, '', SrcCode);
      SD.Kind := skForm;
      N := Modules.Items.AddChildObject(N, Fm.FormCaption + ' (' + Fm.Name + ')', SD);
      CreateEditor(SD);
    end
    else
      N := Modules.Items.FindNodeWithData(SD);
    N.Parent.Expand(False);
    //Modules.Items[2].Expand(False);
    FFmNode.Expand(False);
    N.ImageIndex := 7;
    N.SelectedIndex := 7;
    N.Selected := True;
  end;
  SL.Free;
end;

procedure TScriptFm.MenuItem20Click(Sender: TObject);
begin
  Exclude(FSearchOptions, ssoEntireScope);
  Exclude(FSearchOptions, ssoSelectedOnly);
  DoSearchText(True, False);
end;

procedure TScriptFm.MenuItem21Click(Sender: TObject);
begin
  Exclude(FSearchOptions, ssoEntireScope);
  Exclude(FSearchOptions, ssoSelectedOnly);
  DoSearchText(True, True);
end;

procedure TScriptFm.MenuItem22Click(Sender: TObject);
var
  S: String;
begin
  S := Memo.GetWordAtRowCol(Memo.CaretXY);
  if S <> '' then OpenUrl(AppConfig.SearchUrl + S);
end;

procedure TScriptFm.MenuItem24Click(Sender: TObject);
begin
  StaticText2.Visible := False;
  SaveAll;
  ScriptMan.CompileAll;
  FillCompilerMessages;
  if ScriptMan.HasErrors and FFullScreen then
  	Msgs.Height:=100;
end;

procedure TScriptFm.MenuItem25Click(Sender: TObject);
begin
  ShowSearchForm(True);
  SetControlState;
end;

procedure TScriptFm.MenuItem26Click(Sender: TObject);
begin
  AddModule('', '', skExpr);
end;

procedure TScriptFm.MenuItem27Click(Sender: TObject);
var
  FileName, S, Ext: String;
  sk: TScriptKind;
begin
  FileName := ImportModuleDlg;
  if FileName = '' then Exit;
  with TFileStream.Create(FileName, fmOpenRead) do
  try
  	SetLength(S, Size);
    Read(Pointer(S)^, Size);
  finally
    Free;
  end;

  Ext := ExtractFileExt(FileName);
  if Ext = '.epas' then
  	sk := skExpr
  else
  	sk := skUser;
  AddModule(ChangeFileExt(ExtractFileName(FileName), ''), S, sk);
end;

procedure TScriptFm.MenuItem28Click(Sender: TObject);
var
  Fm: TdxForm;
begin
  MenuItem24.Click;
  if not ScriptMan.HasErrors and CanTestForm(Fm) then
  begin
	  DesignFr.TestForm(Fm.FormCaption);
    // Во время тестирования модули снова компилируются и ссылки на сообщения
    // предыдущей компиляции становятся недействительными (баг YurAnt 27.06.17).
    // Обновляем список сообщений.
    FillCompilerMessages;
  end;
end;

procedure TScriptFm.MenuItem29Click(Sender: TObject);
var
  N: TTreeNode;
  SD: TScriptData;
  FileName, S: String;
begin
  N := Modules.Selected;
  SD := TScriptData(N.Data);
  if SD = nil then Exit;
  FileName := ExportModuleDlg(SD.Kind);
  if FileName = '' then Exit;
  with TFileStream.Create(FileName, fmCreate) do
  try
    S := SD.Source;
    Write(Pointer(S)^, Length(S));
  finally
    Free;
  end;
end;

procedure TScriptFm.MenuItem2Click(Sender: TObject);
begin
  AddModule('', '', skUser);
end;

procedure TScriptFm.MenuItem33Click(Sender: TObject);
begin
  Memo.FoldAll;
end;

procedure TScriptFm.MenuItem34Click(Sender: TObject);
begin
	Memo.UnfoldAll;
end;

procedure TScriptFm.MenuItem4Click(Sender: TObject);
var
  N: TTreeNode;
  SD: TScriptData;
begin
  if not ConfirmDelete then Exit;
  N := Modules.Selected;
  SD := TScriptData(N.Data);
  if SD <> nil then
  begin
    if SD.FmId > 0 then
      DeleteForm(SD.FmId)
    else
    begin
      DeleteEditor(SD);
      ScriptMan.DeleteScript(SD);
      N.Delete;
    end;
    UpdateCanTestForm;
    SetControlState;
    UpdateStatusBar;
  end;
end;

procedure TScriptFm.MenuItem5Click(Sender: TObject);
var
  N: TTreeNode;
  SD: TScriptData;
  OldS, S: String;
begin
  N := Modules.Selected;
  SD := TScriptData(N.Data);
  OldS := SD.Name;
  S := InputBox(rsRenameModule, rsModuleName, OldS);
  if (S = '') or (S = OldS) then Exit;
  if not CheckModuleName(S) then
  begin
    ErrMsg(rsInvalidModuleName);
    Exit;
  end;
  SD.Name := S;
  N.Text:=S;
end;

procedure TScriptFm.MenuItem6Click(Sender: TObject);
begin
  AppConfig.HideParents:=not AppConfig.HideParents;
  BuildTree;
end;

function GetNodeStr(N: TTreeNode): String;
var
  S: String;
  p, p2: Integer;
begin
  S := N.Text;
  p := 1; p2 := 1024;
  if Pos(' =', S) > 0 then
    p2 := Pos(' ', S)
  else if Pos('procedure', S) > 0 then
  begin
    p := Pos(' ', S) + 1;
    p2 := Pos('(', S);
    if p2 = 0 then p2 := 1024;
  end
  else if Pos('function', S) > 0 then
  begin
    p := Pos(' ', S) + 1;
    p2 := Pos('(', S);
    if p2 = 0 then
      p2 := Pos(':', S);
  end
  else if Pos('property', S) > 0 then
  begin
    p := Pos('[', S);
    p2 := Pos(':', S);
    if p < p2 then p2 := p;
    p := Pos(' ', S) + 1;
  end;

  if (Pos(' = class', N.Text) = 0) and (N.Parent <> nil) and
    (Pos(' = class', N.Parent.Text) > 0) then
    Result := GetNodeStr(N.Parent) + '+' + Copy(S, p, p2 - p)
  else
    Result := Copy(S, p, p2 - p);
end;

procedure TScriptFm.MenuItem8Click(Sender: TObject);
begin
  OpenUrl(AppConfig.SearchUrl + GetNodeStr(Tree.Selected));
end;

procedure TScriptFm.MenuItem9Click(Sender: TObject);
var
  S: String;
begin
  S := AppConfig.SearchUrl;
  if InputStr(rsSearchSettings, rsURLPatternToSearch, 'searchsettings', S, False
    ) then
    AppConfig.SearchUrl:=S;
end;

procedure TScriptFm.ModulesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F5 then
    MenuItem28.Click
  else if Key = VK_F9 then
  	MenuItem24.Click;
  if Key in [VK_F5, VK_F9] then Key := 0;
end;

procedure TScriptFm.ModulesSelectionChanged(Sender: TObject);
var
  N: TTreeNode;
begin
  N := Modules.Selected;
  if (N = nil) or (N.Data = nil) then
  begin
    Notebook.PageIndex:=-1;
    Memo := nil;
  end
  else
    FindEditor(TScriptData(N.Data));
  UpdateCanTestForm;
  SetControlState;
  UpdateStatusBar;
end;

procedure TScriptFm.ShowForm;
begin
  UpdateCanTestForm;
  SetControlState;
  UpdateStatusBar;
  ShowModal;
end;

end.

