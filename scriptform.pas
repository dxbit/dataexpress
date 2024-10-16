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

unit ScriptForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, types, FileUtil, SynHighlighterPas, Forms,
  Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Menus, strconsts,
  dxctrls, LclType, EditBtn, Buttons, SynEditTypes, SynEdit, SynEditMarks,
  scriptmanager, scriptedit, classestree, modulestree, crossapi;

type

  { TScriptFm }

  TScriptFm = class(TForm)
    EditMenuImages: TImageList;
    ToolbarImages: TImageList;
    MenuItem10: TMenuItem;
    GotoFormMnu: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    FindScriptMnu: TMenuItem;
    MenuItem9: TMenuItem;
    MsgImgs: TImageList;
    Msgs: TListView;
    MenuItem1: TMenuItem;
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
    Notebook: TNotebook;
    Panel1: TPanel;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    FindPopup: TPopupMenu;
    EditMenu: TPopupMenu;
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
    procedure FindScriptMnuClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure GotoFormMnuClick(Sender: TObject);
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
    procedure MenuItem37Click(Sender: TObject);
    procedure MenuItem38Click(Sender: TObject);
    procedure MenuItem39Click(Sender: TObject);
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
    procedure EditMenuPopup(Sender: TObject);
    procedure FullScreenBnClick(Sender: TObject);
    procedure Splitter3ChangeBounds(Sender: TObject);
    procedure ToolButton17Click(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
  private
    { private declarations }
    FTree: TClassesTree;
    FModules: TModulesTree;
    //FMainNode, FModNode, FFmNode, FExprNode: TTreeNode;
    FSearchText, FReplaceText: String;
    FSearchOptions: TSynSearchOptions;
    FModulesWidth, FClassTreeWidth, FMsgsHeight: Integer;
    FFullScreen: Boolean;
    procedure DoSearchText(ContinueSearch, Backward: Boolean);
    procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShowSearchForm(Replace: Boolean);
    procedure SetControlState;
    procedure CreateEditor(SD: TScriptData);
    procedure SelectEditor(SD: TScriptData);
    procedure DeleteEditor(SD: TScriptData);
    procedure DeleteAllEditors;
    procedure FindBookmark(bm: Integer);
    procedure TreeGetEventHandler(Sender: TObject; const AText: String);
    procedure UpdateStatusBar;
    procedure UpdateCanTestForm;
    procedure ClearModifiedStatus;
    procedure ShowSQLEditor;
    function IsModuleExists(const ModName: String; CurSD: TScriptData): Boolean;
    function CanTestForm(out Fm: TdxForm): Boolean;
    procedure AddModule(const ModuleName, Source: String; Kind: TScriptKind);
    procedure AddFormModule(Fm: TdxForm; Kind: TScriptKind);
    procedure AddWebMainModule(const Source: String);
    procedure EnableFullScreen(Value: Boolean);
    function GetCurrentSD: TScriptData;
    procedure RestoreSplitters;
    procedure ShowClassesTree(IsWeb: Boolean);
  public
    { public declarations }
    Memo: TScriptEdit;
    procedure Init;
    procedure ShowForm(Fm: TdxForm = nil);
    procedure SaveAll;
    procedure RenameForm(FmId: Integer);
    procedure DeleteForm(FmId: Integer);
    procedure GoToBreakpoint(SD: TScriptData; Mark: TSourceMarkData);
    procedure FillCompilerMessages;
    function FindEditor(SD: TScriptData): TScriptEdit;
    procedure SaveConfig;
    procedure Reset(KeepSelected: Boolean);
    procedure RestoreSelection;
    property Modules: TModulesTree read FModules;
  end;

var
  ScriptFm: TScriptFm;

implementation

uses
  apputils, formmanager, selectform, appsettings,
  LazUtf8, SearchReplaceForm, SqlForm, designerframe, breakpointsform,
  base64, exttemplateform, findactionsform, findscriptform;

{$R *.lfm}

function ExportModuleDlg(Kind: TScriptKind; const ModName: String): String;
var
  Flt: String;
begin
  Result := '';
  Flt := '';
  case Kind of
    skForm: Flt := rsFormModulesFlt;
    skWebForm: Flt := rsWebFormModulesFlt;
    skUser, skMain, skWebMain: Flt := rsUserModulesFlt;
    skExpr: Flt := rsExtModulesFlt;
    skWebExpr: Flt := rsWebExtModulesFlt;
  end;
  Flt := Flt + rsAllFilesFlt;
  with TSaveDialog.Create(nil) do
  begin
    Title := rsExportModule;
    Options:=Options + [ofOverwritePrompt, ofPathMustExist];
    FileName := ModName;
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
  Flt := rsAllModulesFlt + rsUserModulesFlt + rsExtModulesFlt + rsWebExtModulesFlt +
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
  mr := ShowSearchReplaceForm(S, Replace, Memo.SelText <> '');
  if mr in [mrOk, mrYesToAll] then
    with SearchReplaceFm do
    begin
      if Options.Checked[0] then
        Opt := Opt + [ssoWholeWord];
      if Options.Checked[1] then
        Opt := Opt + [ssoMatchCase];
      if Start.ItemIndex = 0 then
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
      //end;
  end;
end;

procedure TScriptFm.SetControlState;
var
  N: TTreeNode;
begin
  N := FModules.Tree.Selected;
  MenuItem5.Enabled := (N <> nil) and (N.Parent <> nil) and
    ((N.Parent = FModules.ModNode) or (N.Parent = FModules.ExprNode) or
    (N.Parent = FModules.WebExprNode));
  MenuItem4.Enabled := (N <> nil) and ((N.Parent <> nil) or (N.Text = 'WebMain'));
  MenuItem29.Enabled := (N <> nil) and (N.Data <> nil);
  ToolButton16.Enabled := MenuItem28.Enabled;

  MenuItem12.Enabled := (Memo <> nil) and (Memo.SelText <> '');
  MenuItem13.Enabled := (Memo <> nil) and (Memo.SelText <> '');
  MenuItem14.Enabled := (Memo <> nil) {$ifdef windows}and (Memo.CanPaste){$endif};
  MenuItem16.Enabled := (Memo <> nil) and (Memo.CanUndo);
  MenuItem17.Enabled := (Memo <> nil) and (Memo.CanRedo);
  MenuItem20.Enabled := (Memo <> nil) and (FSearchText <> '');
  MenuItem21.Enabled := (Memo <> nil) and (FSearchText <> '');
  MenuItem37.Enabled := (Memo <> nil);
  MenuItem38.Enabled := (Memo <> nil);
  MenuItem8.Enabled := (Memo <> nil) and (N <> nil) and (N.Data <> nil) and
    (TScriptData(N.Data).Kind in [skExpr, skWebExpr]);

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
  M.PopupMenu := EditMenu;
  M.LoadData(SD);
end;

procedure TScriptFm.SelectEditor(SD: TScriptData);
var
  i: Integer;
  Pg: TPage;
begin
  TestNil(SD, 'TScriptFm.SelectEditor SD = nil');

  for i := 0 to Notebook.PageCount - 1 do
  begin
    Pg := Notebook.Page[i];
    if Pg.Tag = PtrInt(SD) then
    begin
      Memo := TScriptEdit(Pg.Controls[0]);
      // В 1.8.2 стали пропадать ползунки
      //UpdateMemoScrollBars(Memo);
      //
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
  ShowClassesTree(SD.Kind in [skWebMain, skWebForm, skWebExpr]);
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
      if Pg.Controls[0] = Memo then Memo := nil;
      Pg.Controls[0].Free;
      Pg.Free;
      Break;
    end;
  end;
end;

procedure TScriptFm.DeleteAllEditors;
var
  i: Integer;
  Pg: TPage;
begin
  Memo := nil;
  Notebook.Visible := False;
  for i := Notebook.PageCount - 1 downto 0 do
  begin
    Pg := Notebook.Page[i];
    Pg.Controls[0].Free;
    Pg.Free;
  end;
  Notebook.Visible := True;
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
        	FModules.Tree.Selected := FModules.Tree.Items.FindNodeWithData(Pointer(Pg.Tag));
        Memo.GotoBookMark(bm);
        Memo.SetFocus;
        Exit;
      end;
    end;
  end;
end;

procedure TScriptFm.TreeGetEventHandler(Sender: TObject; const AText: String);
begin
  if Memo <> nil then Memo.SelText := LineEnding + AText;
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
    if FModules.Tree.Selected <> nil then
      Panels[3].Text := FModules.Tree.Selected.Text
    else
      Panels[3].Text := '';
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
  for i := 0 to FModules.Tree.Items.Count - 1 do
  begin
    N := FModules.Tree.Items[i];
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
	ShowSqlForm;
end;

function TScriptFm.IsModuleExists(const ModName: String; CurSD: TScriptData
  ): Boolean;
var
  SD: TScriptData;
begin
  Result := False;
  SD := ScriptMan.FindScriptByName(ModName);
  if (SD <> nil) and (SD <> CurSD) then
  begin
    ErrMsgFmt(rsModuleAlreadyExists, [ModName]);
    Result := True;
  end;
end;

function TScriptFm.CanTestForm(out Fm: TdxForm): Boolean;
var
  N: TTreeNode;
  SD: TScriptData;
begin
  Fm := nil;
  Result := False;
  N := FModules.Tree.Selected;
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
	  S := Trim(InputBox(rsNewModule, rsModuleName, ''));
  if (S = '') or not CheckModuleName(S) or IsModuleExists(S, nil) then Exit;
  SD := ScriptMan.AddScript(0, S, Source);
  SD.Kind := Kind;
  CreateEditor(SD);
  PN := FModules.ModNode;
  if Kind = skExpr then PN := FModules.ExprNode
  else if Kind = skWebExpr then
  begin
    FModules.AddWebExprNode;
    PN := FModules.WebExprNode;
  end;
  N := FModules.Tree.Items.AddChildObject(PN, S, SD);
  N.Parent.Expand(False);
  //N.ImageIndex := 7;
  //N.SelectedIndex := 7;
  N.Selected := True;
end;

procedure TScriptFm.AddFormModule(Fm: TdxForm; Kind: TScriptKind);
var
  N: TTreeNode;
  SD: TScriptData;
  SrcCode: String;
  PFm: TdxForm;
begin
  SrcCode := Format('procedure Form_Create;%0:sbegin%0:s%0:send;', [LineEnding]);
  if Kind = skForm then N := FModules.FmNode
  else if Kind = skWebForm then
  begin
    FModules.AddWebFmNode;
    N := FModules.WebFmNode;
  end;
  if Fm.PId > 0 then
  begin
    SD := ScriptMan.FindScript(Fm.PId, Kind);
    if SD = nil then
    begin
      PFm := FormMan.FindForm(Fm.PId);
      SD := ScriptMan.AddScript(PFm.Id, '', SrcCode);
      SD.Kind := Kind;
      CreateEditor(SD);
      N := FModules.Tree.Items.AddChildObject(N, PFm.FormCaption + ' (' + PFm.Name + ')', SD);
    end
    else
      N := FModules.Tree.Items.FindNodeWithData(SD);
  end;
  SD := ScriptMan.FindScript(Fm.Id, Kind);
  if SD = nil then
  begin
    SD := ScriptMan.AddScript(Fm.Id, '', SrcCode);
    SD.Kind := Kind;
    N := FModules.Tree.Items.AddChildObject(N, Fm.FormCaption + ' (' + Fm.Name + ')', SD);
    CreateEditor(SD);
  end
  else
    N := FModules.Tree.Items.FindNodeWithData(SD);
  N.Parent.Expand(False);
  FModules.FmNode.Expand(False);
  N.Selected := True;
end;

procedure TScriptFm.AddWebMainModule(const Source: String);
var
  SD: TScriptData;
  N: TTreeNode;
begin
  SD := ScriptMan.AddScript(0, 'WebMain', Source);
  SD.Kind := skWebMain;
  CreateEditor(SD);
  N := FModules.Tree.Items.InsertBehind(FModules.Tree.TopItem, 'WebMain');
  N.Data := SD;
  N.Selected := True;
end;

procedure TScriptFm.FillCompilerMessages;
var
  SL: TStringList;
  i: Integer;
  LI: TListItem;
begin
  StaticText2.Visible := False;

  SL := TStringList.Create;
  ScriptMan.MessagesToList(SL);
  Msgs.Clear;
  for i := 0 to SL.Count - 1 do
    if TCompilerMsg(SL.Objects[i]).ErrorType = 'Error' then
    begin
      LI := Msgs.Items.Add;
      LI.SubItems.Add(SL[i]);
      LI.Data:=SL.Objects[i];
      LI.ImageIndex := 0;
    end;

  for i := 0 to SL.Count - 1 do
    if TCompilerMsg(SL.Objects[i]).ErrorType <> 'Error' then
    begin
      LI := Msgs.Items.Add;
      LI.SubItems.Add(SL[i]);
      LI.Data:=SL.Objects[i];
      LI.ImageIndex := 1;
    end;

  SL.Free;

  if not ScriptMan.HasErrors then
  begin
  	LI := Msgs.Items.Insert(0);
    LI.SubItems.Add(rsCompilationSuccess);
    LI.ImageIndex := 2;
    LI.Selected:=True;
  end
  else if FFullScreen then
  	Msgs.Height:=100;
end;

function TScriptFm.FindEditor(SD: TScriptData): TScriptEdit;
var
  i: Integer;
  Pg: TPage;
begin
  Result := nil;
  for i := 0 to Notebook.PageCount - 1 do
  begin
    Pg := Notebook.Page[i];
    if Pg.Tag = PtrInt(SD) then
      Exit(TScriptEdit(Pg.Controls[0]));
  end;
end;

procedure TScriptFm.SaveConfig;
var
  SD: TScriptData;
  FormBounds: TRect;
begin
  SD := GetCurrentSD;
  if SD <> nil then
  begin
	  AppConfig.SESDName := SD.Name;
    if SD.Kind = skForm then
      AppConfig.SEFmId := SD.FmId
    else if SD.Kind = skWebForm then
      AppConfig.SEWebFmId := SD.FmId;
  end
  else
  begin
    AppConfig.SESDName := '';
    AppConfig.SEFmId := 0;
    AppConfig.SEWebFmId := 0;
  end;

  if WindowState in [wsNormal, wsMaximized] then AppConfig.SEFormState:=WindowState;
  FormBounds := ScaleRectTo96(GetFormRealBounds(Self));
  AppConfig.SEFormLeft := FormBounds.Left;
  AppConfig.SEFormTop := FormBounds.Top;
	AppConfig.SEFormWidth := FormBounds.Width;
  AppConfig.SEFormHeight := FormBounds.Height;
  if not FFullScreen then
  begin
		AppConfig.SEModulesWidth := ScaleTo96(FModules.Tree.Width);
  	AppConfig.SEClassesWidth := ScaleTo96(FTree.Width);
	  AppConfig.SEMsgHeight := ScaleTo96(Msgs.Height);
  end
  else
  begin
		AppConfig.SEModulesWidth := ScaleTo96(FModulesWidth);
  	AppConfig.SEClassesWidth := ScaleTo96(FClassTreeWidth);
	  AppConfig.SEMsgHeight := ScaleTo96(FMsgsHeight);
  end;
  AppConfig.SEClassesHelpHeight := ScaleTo96(FTree.HelpPanelHeight);
  AppConfig.SEClassesHideBaseClasses := FTree.HideBaseClasses;
  AppConfig.SEClassesHideAncestors := FTree.HideAncestors;
  AppConfig.SEClassesHideHelp := FTree.HideContextHelp;
end;

procedure TScriptFm.Reset(KeepSelected: Boolean);
var
  N: TTreeNode;
  TopNodeData, SelNodeData: Pointer;
  TopNodeText, SelNodeText: String;
begin
  DeleteAllEditors;

  if KeepSelected then
  begin
    N := FModules.Tree.TopItem;
    TopNodeText := N.Text;
    TopNodeData := N.Data;
    N := FModules.Tree.Selected;
    if N <> nil then
    begin
      SelNodeText := N.Text;
      SelNodeData := N.Data;
    end
    else
    begin
      SelNodeText := '';
      SelNodeData := nil;
    end;
  end;

  Init;

  if KeepSelected then
  begin
    if TopNodeData <> nil then
      FModules.Tree.TopItem := FModules.Tree.Items.FindNodeWithData(TopNodeData)
    else
      FModules.Tree.TopItem := FModules.Tree.Items.FindNodeWithText(TopNodeText);
    if SelNodeData <> nil then
      FModules.Tree.Selected := FModules.Tree.Items.FindNodeWithData(SelNodeData)
    else if SelNodeText <> '' then
      FModules.Tree.Selected := FModules.Tree.Items.FindNodeWithText(SelNodeText)
  end
  else
    FModules.Tree.Items[0].Selected := True;
end;

procedure TScriptFm.RestoreSelection;
var
  SD: TScriptData;
begin
  if AppConfig.SESDName <> '' then
	  SD := ScriptMan.FindScriptByName(AppConfig.SESDName)
  else if AppConfig.SEFmId > 0 then
	  SD := ScriptMan.FindScript(AppConfig.SEFmId, skForm)
  else if AppConfig.SEWebFmId > 0 then
	  SD := ScriptMan.FindScript(AppConfig.SEWebFmId, skWebForm)
  else
	  SD := nil;
  if SD <> nil then
	  FModules.Tree.Items.FindNodeWithData(SD).Selected:=True
  else
    FModules.Tree.Items[0].Selected := True;
end;

procedure TScriptFm.EnableFullScreen(Value: Boolean);
begin
  if Value then
  begin
    FModulesWidth := FModules.Tree.Width;
    FClassTreeWidth := FTree.Width;
    FMsgsHeight := Msgs.Height;
    FModules.Tree.Width := 0;
	  FTree.Width := 0;
  	Msgs.Height := 0;
  end
  else
  begin
    FModules.Tree.Width := FModulesWidth;
	  FTree.Width := FClassTreeWidth;
  	Msgs.Height := FMsgsHeight;
  end;
  Splitter1.Visible := not Value;
	Splitter2.Visible := not Value;
  Splitter3.Visible := not Value;
  RestoreSplitters;
end;

function TScriptFm.GetCurrentSD: TScriptData;
var
  N: TTreeNode;
begin
  N := FModules.Tree.Selected;
  if N <> nil then Result := TScriptData(N.Data)
  else Result := nil;
end;

procedure TScriptFm.RestoreSplitters;
begin
  Splitter1.Left := FModules.Tree.Left + FModules.Tree.Width + 10;
  Splitter2.Top := Msgs.Top - 10;
  Splitter3.Left := FTree.Left - 10;
end;

procedure TScriptFm.ShowClassesTree(IsWeb: Boolean);
begin
  FTree.IsWeb := IsWeb;
  if IsWeb then
  begin
    if ExtractFileName(FTree.FileName) = 'classes.dat' then
    begin
      FTree.FileName := AppPath + 'languages' + PathDelim + 'en' + PathDelim + 'classes_web.dat';
      FTree.BuildTree;
    end;
  end
  else
  begin
    if ExtractFileName(FTree.FileName) = 'classes_web.dat' then
    begin
      FTree.FileName := AppPath + 'languages' + PathDelim + 'en' + PathDelim + 'classes.dat';
      FTree.BuildTree;
    end;
  end;
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
  for i := 0 to FModules.Tree.Items.Count - 1 do
  begin
    N := FModules.Tree.Items[i];
    SD := TScriptData(N.Data);
    if (N.Parent <> nil) and (SD <> nil) and (SD.FmId = FmId) then
    begin
      Fm := FormMan.FindForm(FmId);
      Ch := Copy(N.Text, 1, 1);
      S := Fm.FormCaption + ' (' + Fm.Name + ')';
      if Ch = '*' then S := Ch + S;
      N.Text := S;
      //Exit;
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
    //if FModules.Tree.Selected = Node then FModules.Tree.ClearSelection;
    Node.Delete;
  end;

begin
  for i := FModules.Tree.Items.Count - 1 downto 0 do
  begin
    N := FModules.Tree.Items[i];
    SD := TScriptData(N.Data);
    if (SD <> nil) and (SD.FmId = FmId) then
    begin
      while N.Count > 0 do
        _Delete(N.Items[0]);
      _Delete(N);
      FModules.Tree.Invalidate;
    end;
  end;
  if (FModules.WebFmNode <> nil) and (FModules.WebFmNode.Count = 0) then FModules.DeleteWebFmNode;
end;

procedure TScriptFm.GoToBreakpoint(SD: TScriptData; Mark: TSourceMarkData);
var
  N: TTreeNode;
begin
  N := FModules.Tree.Items.FindNodeWithData(SD);
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
  FModules.BuildTree;
  FTree.BuildTree;
  for i := 0 to ScriptMan.ScriptCount - 1 do
    CreateEditor(ScriptMan.Scripts[i]);
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
  N := FModules.Tree.Items.FindNodeWithData(SD);
  if N <> nil then
	  N.Selected := True
  else
    Exit;

  TestNil(Memo, 'TScriptFm.MsgsClick Memo = nil');

  Memo.SetFocus;
  if Msg.Pos = 0 then
	  Memo.CaretXY := Point(Msg.Col, Msg.Row)
  else
  	Memo.SelStart:=Msg.Pos;
end;

procedure TScriptFm.PopupMenu1Popup(Sender: TObject);
var
  Fm: TdxForm;
  N: TTreeNode;
begin
  MenuItem28.Enabled := CanTestForm(Fm);
  N := FModules.Tree.Selected;
  GotoFormMnu.Enabled := (N <> nil) and (N.Data <> nil) and
    (TScriptData(N.Data).Kind in [skForm, skWebForm]);
  MenuItem11.Enabled := (N <> nil) and (ScriptMan.FindScriptByName('WebMain') = nil);
end;

procedure TScriptFm.EditMenuPopup(Sender: TObject);
begin
  SetControlState;
end;

procedure TScriptFm.FullScreenBnClick(Sender: TObject);
begin
  FFullScreen := not FFullScreen;
  FullScreenBn.Down := FFullScreen;
  EnableFullScreen(FFullScreen);
end;

procedure TScriptFm.Splitter3ChangeBounds(Sender: TObject);
begin
  {if FTree.Visible then
  begin
    if FTreeWeb <> nil then FTreeWeb.Width := FTree.Width;
  end
  else if FTreeWeb <> nil then
    FTree.Width := FTreeWeb.Width;   }
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
    11: DesignFr.Save(False);
    12: MenuItem28.Click;
  end;
end;

procedure TScriptFm.FormActivate(Sender: TObject);
begin
  {$ifdef linux}
  {if FFirstShow then
  begin
    ReCreateOutputForm;
    FFirstShow := False;
  end; }
  {$endif}
end;

procedure TScriptFm.FindScriptMnuClick(Sender: TObject);
begin
  ShowFindScriptForm(Memo.GetWordAtRowCol(Memo.CaretXY));
end;

procedure TScriptFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FindScriptFm <> nil then FindScriptFm.Close;
  Application.MainForm.SetFocus;
end;

procedure TScriptFm.FormCreate(Sender: TObject);
begin
  PopupParent := Application.MainForm;
  FModules := TModulesTree.Create(Self);
  with FModules do
  begin
    Parent := Self;
    Align := alLeft;
    BorderSpacing.Bottom := 4;
    BorderSpacing.Left := 4;
    Tree.OnSelectionChanged := @ModulesSelectionChanged;
    Tree.OnKeyDown := @ModulesKeyDown;
    PopupMenu := PopupMenu1;
  end;

  FTree := TClassesTree.Create(Self);
  with FTree do
  begin
    Parent := Self;
    Align := alRight;
    BorderSpacing.Bottom := 4;
    BorderSpacing.Right := 4;
    Left := Splitter3.Left + 100;
    HelpPanelHeight := AppConfig.SEClassesHelpHeight;
    HideBaseClasses := AppConfig.SEClassesHideBaseClasses;
    HideAncestors := AppConfig.SEClassesHideAncestors;
    HideContextHelp := AppConfig.SEClassesHideHelp;
    Width := AppConfig.SEClassesWidth;
    FileName:=AppPath + 'languages' + PathDelim + 'en' + PathDelim + 'classes.dat';
    OnGetEventHandler:=@TreeGetEventHandler;
  end;

  Caption := rsScriptEditor;
  Left := AppConfig.SEFormLeft;
  Top := AppConfig.SEFormTop;
  Width := AppConfig.SEFormWidth;
  Height := AppConfig.SEFormHeight;
  FModules.Width := AppConfig.SEModulesWidth;
  Msgs.Height := AppConfig.SEMsgHeight;

  SetFormPropPosition(Self);

  MenuItem1.Caption := rsAddFormModule;
  MenuItem2.Caption := rsAddUserModule;
  MenuItem26.Caption := rsAddExtModule;
  MenuItem11.Caption := rsAddWebMainModule;
  MenuItem22.Caption := rsAddFormWebModule;
  MenuItem39.Caption := rsAddExtWebModule;
  MenuItem5.Caption := rsRenameModule;
  MenuItem4.Caption := rsDelete;
  MenuItem27.Caption := rsImportModule;
  MenuItem29.Caption := rsExportModule;
  GotoFormMnu.Caption := rsGoToForm;

  MenuItem12.Caption := rsCut;
  MenuItem13.Caption := rsCopy;
  MenuItem14.Caption := rsPaste;
  MenuItem36.Caption := rsPasteSpecial;
  MenuItem37.Caption := rsPasteGUID;
  MenuItem38.Caption := rsPasteImageAsBase64;
  MenuItem8.Caption := rsPasteExtTemplate;
  MenuItem16.Caption := rsUndo;
  MenuItem17.Caption := rsRedo;
  MenuItem19.Caption := rsFind;
  MenuItem20.Caption := rsNext;
  MenuItem21.Caption := rsFindPrevious;
  MenuItem25.Caption := rsReplace;
  FindScriptMnu.Caption := rsFindInModules;
  MenuItem6.Caption := rsFindInSyntaxPanel;
  MenuItem24.Caption := rsCompile;
  MenuItem28.Caption := rsTestForm;

  MenuItem32.Caption := rsCodeFolding;
  MenuItem33.Caption := rsFoldAllCode;
  MenuItem34.Caption := rsUnfoldAllCode;

  MenuItem9.Caption := rsFindInModules;

  StaticText2.Caption := rsCompilerMsgs;

  SetupImageList(EditMenuImages, ['cut16', 'copy16', 'paste16', 'undo16',
    'goto16', 'find16', 'play16', 'test16']);
  SetupImageList(ToolbarImages, ['save24', 'form24', 'user24', 'cut24', 'copy24',
    'paste24', 'undo24', 'redo24', 'find24', 'replace24', 'compile24', 'test24',
    'sql24', 'fullscreen24', 'breakpoint24']);

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
  ToolButton14.Hint := rsSaveProject;
  ToolButton16.Hint := rsTestForm;
  FullScreenBn.Hint := rsMaximizeEditor;
  ToolButton17.Hint := rsBreakpoints;

  SetupImageList(MsgImgs, ['exprcheck16', 'hint16', 'check16']);
  FModules.Tree.IsWine := AppConfig.IsWine;
end;

procedure TScriptFm.FormDestroy(Sender: TObject);
begin
  SaveConfig;
  Memo := nil;
  ScriptFm := nil;
end;

procedure TScriptFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F4 then
  begin
    Key := 0;
    Close;
  end
  // Предотвращаем выход из дизайнера при активной форме
  else if Key = VK_F11 then
    Key := 0
  else if Key = VK_F12 then
  begin
    Key := 0;
    FullScreenBn.Click;
  end;
end;

procedure TScriptFm.FormShow(Sender: TObject);
begin
  if AppConfig.ScriptFormPosCorrected = False then
  begin
    CorrectFormPos(Self, Self);
    AppConfig.ScriptFormPosCorrected := True;
    WindowState := AppConfig.SEFormState;
  end;
end;

procedure TScriptFm.FormWindowStateChange(Sender: TObject);
begin
  RestoreSplitters;
end;

procedure TScriptFm.GotoFormMnuClick(Sender: TObject);
var
  N: TTreeNode;
  SD: TScriptData;
  Fm: TdxForm;
begin
  N := FModules.Tree.Selected;
  SD := TScriptData(N.Data);
  Fm := FormMan.FindForm(SD.FmId);
  DesignFr.FormsTreeView.SelectForm(Fm);
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
    S := FModules.Tree.Selected.Text;
    if Memo.Modified then
    begin
      if Copy(S, 1, 1) <> '*' then
        S := '*' + S
    end
    else if Copy(S, 1, 1) = '*' then
      Delete(S, 1, 1);
    FModules.Tree.Selected.Text := S;
    ScriptMan.NeedCompile := True;
  end;
  UpdateStatusBar;
end;

procedure TScriptFm.MenuItem11Click(Sender: TObject);
begin
  AddWebMainModule('');
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
  Fm: TdxForm;
  HasChilds: Boolean;
begin
  SL := TStringList.Create;
  FormMan.AllFormsToList(SL);
  HasChilds := False;
  for i := SL.Count - 1 downto 0 do
  begin
    Fm := TdxForm(SL.Objects[i]);
    if ScriptMan.FindScript(Fm.Id, skForm) <> nil then
    begin
      if (Fm.PId = 0) and HasChilds then HasChilds := False
      else SL.Delete(i);
    end
    else
      HasChilds := Fm.PId > 0;
  end;
  if ShowSelectForm(rsSelectForm, '', SL) = mrOk then
  begin
    i := SelectFm.Index;
    Fm := TdxForm(SL.Objects[i]);
    AddFormModule(Fm, skForm);
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
  SL: TStringList;
  i: Integer;
  Fm: TdxForm;
  HasChilds: Boolean;
begin
  SL := TStringList.Create;
  FormMan.AllFormsToList(SL);
  HasChilds := False;
  for i := SL.Count - 1 downto 0 do
  begin
    Fm := TdxForm(SL.Objects[i]);
    if ScriptMan.FindScript(Fm.Id, skWebForm) <> nil then
    begin
      if (Fm.PId = 0) and HasChilds then HasChilds := False
      else SL.Delete(i);
    end
    else
      HasChilds := Fm.PId > 0;
  end;
  if ShowSelectForm(rsSelectForm, '', SL) = mrOk then
  begin
    i := SelectFm.Index;
    Fm := TdxForm(SL.Objects[i]);
    AddFormModule(Fm, skWebForm);
  end;
  SL.Free;
end;

procedure TScriptFm.MenuItem24Click(Sender: TObject);
begin
  SaveAll;
  ScriptMan.CompileAll;
  FillCompilerMessages;
  if not ScriptMan.HasErrors then
    ScriptMan.NeedCompile := False;
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
  with TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone) do
  try
  	SetLength(S, Size);
    Read(Pointer(S)^, Size);
  finally
    Free;
  end;

  Ext := ExtractFileExt(FileName);
  if Ext = '.epas' then
  	sk := skExpr
  else if Ext = '.wepas' then
    sk := skWebExpr
  else
  	sk := skUser;

  FileName := Trim(ChangeFileExt(ExtractFileName(FileName), ''));
  if FileName = '' then Exit;
  if CompareText(FileName, 'webmain') = 0 then
  begin
    if ScriptMan.FindScriptByName('WebMain') <> nil then
      ErrMsg(rsModuleWebMainExists)
    else
      AddWebMainModule(S);
  end
  else
    AddModule(FileName, S, sk);
end;

procedure TScriptFm.MenuItem28Click(Sender: TObject);
var
  Fm: TdxForm;
begin
  if CanTestForm(Fm) then
    DesignFr.TestForm(Fm.FormCaption);
end;

procedure TScriptFm.MenuItem29Click(Sender: TObject);
var
  N: TTreeNode;
  SD: TScriptData;
  FileName, S: String;
begin
  N := FModules.Tree.Selected;
  SD := TScriptData(N.Data);
  if SD = nil then Exit;
  FileName := N.Text;
  if Copy(FileName, 1, 1) = '*' then Delete(FileName, 1, 1);
  FileName := ExportModuleDlg(SD.Kind, FileName);
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
  Memo.FoldAll(0, True);
end;

procedure TScriptFm.MenuItem34Click(Sender: TObject);
begin
	Memo.UnfoldAll;
end;

procedure TScriptFm.MenuItem37Click(Sender: TObject);
var
  GUID: TGUID;
  S: String;
begin
  if CreateGUID(GUID) = 0 then
  begin
    S := GUIDToString(GUID);
  	Memo.SelText := Copy(S, 2, Length(S) - 2);
  end;
end;

procedure TScriptFm.MenuItem38Click(Sender: TObject);
var
  FileName, Buf: String;
begin
  FileName := OpenPictureDialog;
  if FileName = '' then Exit;

  try
    with TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone) do
    try
      SetLength(Buf, Size);
      Read(Pointer(Buf)^, Size);
      Memo.SelText := EncodeStringBase64(Buf);
    finally
      Free;
    end;
  except
    on E: Exception do
    begin
      ErrMsgFmt(rsPasteImgError, [E.Message]);
    end;
  end;
end;

procedure TScriptFm.MenuItem39Click(Sender: TObject);
begin
  AddModule('', '', skWebExpr);
end;

procedure TScriptFm.MenuItem4Click(Sender: TObject);

  function _Delete(N: TTreeNode): TScriptKind;
  var
    i: Integer;
    SD: TScriptData;
    LI: TListItem;
  begin
    SD := TScriptData(N.Data);
    Result := SD.Kind;
    if SD.FmId > 0 then
      for i := N.Count - 1 downto 0 do
        _Delete(N.Items[i]);

    for i := 0 to SD.MsgCount - 1 do
    begin
      LI := Msgs.FindData(0, SD.Msgs[i], True, False);
      if LI <> nil then LI.Delete;
    end;

    if SD.Kind = skExpr then
    begin
      if FindActionsFm <> nil then FindActionsFm.DeleteModule(SD);
    end;

    DeleteEditor(SD);
    ScriptMan.DeleteScript(SD);
    N.Delete;
  end;

var
  Kind: TScriptKind;
begin
  if not ConfirmDelete then Exit;
  Kind := _Delete(FModules.Tree.Selected);

  if Kind = skExpr then
    ScriptMan.CompileExpr
  else if (Kind = skWebForm) and (FModules.WebFmNode.Count = 0) then
    FModules.DeleteWebFmNode
  else if (Kind = skWebExpr) and (FModules.WebExprNode.Count = 0) then
    FModules.DeleteWebExprNode
  else if Kind = skUser then
    ScriptMan.NeedCompile := True;
  UpdateCanTestForm;
  SetControlState;
  UpdateStatusBar;

  {SD := TScriptData(N.Data);
  if SD <> nil then
  begin
    for i := 0 to SD.MsgCount - 1 do
    begin
      LI := Msgs.FindData(0, SD.Msgs[i], True, False);
      if LI <> nil then LI.Delete;
    end;

    Kind := SD.Kind;
    if SD.FmId > 0 then
      DeleteForm(SD.FmId)
    else
    begin
      DeleteEditor(SD);
      ScriptMan.DeleteScript(SD);
      N.Delete;
    end;

  end;   }
end;

procedure TScriptFm.MenuItem5Click(Sender: TObject);
var
  N: TTreeNode;
  SD: TScriptData;
  OldS, S: String;
begin
  N := FModules.Tree.Selected;
  SD := TScriptData(N.Data);
  OldS := SD.Name;
  S := Trim(InputBox(rsRenameModule, rsModuleName, OldS));
  if (S = '') or (S = OldS) or not CheckModuleName(S) or IsModuleExists(S, SD) then Exit;
  SD.Name := S;
  if Memo.Modified then S := '*' + S;
  N.Text:=S;
  ScriptMan.NeedCompile := True;
end;

procedure TScriptFm.MenuItem6Click(Sender: TObject);
var
  S: String;
begin
  S := Memo.GetWordAtRowCol(Memo.CaretXY);
  if S <> '' then FTree.SetFilter(S);
end;

procedure TScriptFm.MenuItem8Click(Sender: TObject);
begin
  if ShowExtTemplateForm = mrOk then
    Memo.SelText := ExtTemplateFm.SourceCode;
end;

procedure TScriptFm.MenuItem9Click(Sender: TObject);
begin
  FindScriptMnu.Click;
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
  else if (Pos('procedure', S) > 0) or (Pos('constructor', S) > 0) then
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
  else if (N.Parent <> nil) and (Pos(' = interface', N.Parent.Text) > 0) then
      Result := GetNodeStr(N.Parent) + '+' + Copy(S, p, p2 - p)
  else if (N.Parent <> nil) and (Pos(' = record', N.Parent.Text) > 0) then
    Result := GetNodeStr(N.Parent) + '+' + Copy(S, p, Pos(':', S) - 1)
  else
    Result := Copy(S, p, p2 - p);
end;

procedure TScriptFm.ModulesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F5 then
    MenuItem28.Click
  else if Key = VK_F9 then
  	MenuItem24.Click;
  if Key in [{VK_F5, }VK_F9] then Key := 0;
end;

procedure TScriptFm.ModulesSelectionChanged(Sender: TObject);
var
  N: TTreeNode;
begin
  N := FModules.Tree.Selected;
  if (N = nil) or (N.Data = nil) then
  begin
    Notebook.PageIndex:=-1;
    Memo := nil;
    ShowClassesTree(False);
  end
  else
    SelectEditor(TScriptData(N.Data));
  UpdateCanTestForm;
  SetControlState;
  UpdateStatusBar;
end;

procedure TScriptFm.ShowForm(Fm: TdxForm);
var
  SD: TScriptData;
  N: TTreeNode;
begin
  if WindowState = wsMinimized then WindowState := wsNormal;
  if not Visible then Show;

  if Fm <> nil then
  begin
    SD := ScriptMan.FindScript(Fm.Id, skForm);
    if SD = nil then
    begin
      if Confirm(rsWarning,
        Format(rsCreateFormModuleMsg, [Fm.FormCaption])) = mrNo then Exit;
      AddFormModule(Fm, skForm);
    end
    else
    begin
      N := FModules.Tree.Items.FindNodeWithData(SD);
      N.Selected := True;
    end;
  end;
  UpdateCanTestForm;
  SetControlState;
  UpdateStatusBar;
end;

end.

