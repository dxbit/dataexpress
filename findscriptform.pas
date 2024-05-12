unit FindScriptForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls, ExtCtrls, LazUtf8, LclType, SynEditTypes, scriptmanager, strconsts,
  DxCtrls;

type
  PSearchTextResult = ^TSearchTextResult;
  TSearchTextResult = record
    SD: TScriptData;
    //Position: Integer;
    Col, Row: Integer;
  end;

  { TFindScriptFm }

  TFindScriptFm = class(TForm)
    NothingPan: TPanel;
    OptionsGrp: TCheckGroup;
    ModulesGrp: TCheckGroup;
    FindBn: TBitBtn;
    ResultLbl: TLabel;
    TextCbx: TComboBox;
    Label1: TLabel;
    Tree: TTreeView;
    procedure FindBnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TextCbxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeSelectionChanged(Sender: TObject);
  private
    FFounds: Integer;
    procedure DeleteTreeNode(N: TTreeNode);
    procedure UpdateResultLabel;
    procedure ClearTree;
  public
    procedure ShowForm(const S: String);
    procedure DeleteModule(SD: TObject);
    procedure Reset;
  end;

var
  FindScriptFm: TFindScriptFm;

procedure ShowFindScriptForm(const S: String);

implementation

uses
  apputils, scriptform, scriptedit, formmanager;

procedure ShowFindScriptForm(const S: String);
begin
  if FindScriptFm = nil then
    FindScriptFm := TFindScriptFm.Create(Application);
  FindScriptFm.ShowForm(S);
end;

{$R *.lfm}

{ TFindScriptFm }

procedure TFindScriptFm.FindBnClick(Sender: TObject);
var
  Txt: String;
  TxtLen, j: Integer;
  SD: TScriptData;
  ModuleName: String;
  ParentNode, TopNode: TTreeNode;
  Editor: TScriptEdit;
  Options: TSynSearchOptions;
  SL: TStringList;
  Fm: TdxForm;

  function CreateSearchTextResult(SD: TScriptData; Col, Row: Integer): PSearchTextResult;
  begin
    New(Result);
    Result^.SD := SD;
    Result^.Col := Col;
    Result^.Row := Row;
  end;

  procedure AddResult;
  var
    pSR: PSearchTextResult;
  begin
    pSR := CreateSearchTextResult(SD, Editor.CaretX - TxtLen, Editor.CaretY);
    Tree.Items.AddChildObject(ParentNode, Format('%s (%d: %d) %s', [ModuleName,
      pSR^.Row, pSR^.Col, Trim(Editor.Lines[Editor.CaretY - 1])]), pSR);
    Inc(FFounds);
  end;

  procedure FindInModule;
  begin
    Options := [ssoEntireScope];
    if OptionsGrp.Checked[0] then Include(Options, ssoWholeWord);
    if OptionsGrp.Checked[1] then Include(Options, ssoMatchCase);
    Editor.Text := ScriptFm.FindEditor(SD).Text;
    while Editor.SearchReplace(Txt, '', Options) > 0 do
    begin
      Exclude(Options, ssoEntireScope);
      Include(Options, ssoFindContinue);
      AddResult;
    end;
  end;

begin
  with TextCbx do
  begin
    if Text = '' then Exit;
    if Items.IndexOf(Text) < 0 then
      Items.Add(Text);
    Txt := Text;
  end;
  FFounds := 0;
  SL := TStringList.Create;
  Editor := TScriptEdit.Create(nil);
  TxtLen := Utf8Length(Txt);
  ClearTree;
  Tree.BeginUpdate;
  Txt := Utf8LowerCase(TextCbx.Text);
  if ModulesGrp.Checked[0] then
  begin
    SD := ScriptMan.FindScriptByName('Main');
    ModuleName := SD.Name;
    ParentNode := Tree.Items.AddChildObject(nil, ModuleName,
      CreateSearchTextResult(SD, 0, 0));
    FindInModule;
    if ParentNode.Count = 0 then DeleteTreeNode(ParentNode);
  end;

  if ModulesGrp.Checked[1] then
  begin
    SD := ScriptMan.FindScriptByName('WebMain');
    if SD <> nil then
    begin
      ModuleName := SD.Name;
      ParentNode := Tree.Items.AddChildObject(nil, ModuleName,
        CreateSearchTextResult(SD, 0, 0));
      FindInModule;
      if ParentNode.Count = 0 then DeleteTreeNode(ParentNode);
    end;
  end;

  if ModulesGrp.Checked[2] then
  begin
    TopNode := Tree.Items.AddChild(nil, rsFormsModules);
    ScriptMan.ModulesToList(SL, skForm);
    for j := 0 to SL.Count - 1 do
    begin
      SD := TScriptData(SL.Objects[j]);
      Fm := FormMan.FindForm(SD.FmId);
      ModuleName := Fm.FormCaption;
      ParentNode := Tree.Items.AddChildObject(TopNode, ModuleName,
        CreateSearchTextResult(SD, 0, 0));
      FindInModule;
      if ParentNode.Count = 0 then DeleteTreeNode(ParentNode);
    end;
    if TopNode.Count = 0 then TopNode.Delete;
  end;

  if ModulesGrp.Checked[3] then
  begin
    TopNode := Tree.Items.AddChild(nil, rsFormWebModules);
    ScriptMan.ModulesToList(SL, skWebForm);
    for j := 0 to SL.Count - 1 do
    begin
      SD := TScriptData(SL.Objects[j]);
      Fm := FormMan.FindForm(SD.FmId);
      ModuleName := Fm.FormCaption;
      ParentNode := Tree.Items.AddChildObject(TopNode, ModuleName,
        CreateSearchTextResult(SD, 0, 0));
      FindInModule;
      if ParentNode.Count = 0 then DeleteTreeNode(ParentNode);
    end;
    if TopNode.Count = 0 then TopNode.Delete;
  end;

  if ModulesGrp.Checked[4] then
  begin
    TopNode := Tree.Items.AddChild(nil, rsExtensionsModules);
    ScriptMan.ModulesToList(SL, skExpr);
    for j := 0 to SL.Count - 1 do
    begin
      SD := TScriptData(SL.Objects[j]);
      ModuleName := SD.Name;
      ParentNode := Tree.Items.AddChildObject(TopNode, ModuleName,
        CreateSearchTextResult(SD, 0, 0));
      FindInModule;
      if ParentNode.Count = 0 then DeleteTreeNode(ParentNode);
    end;
    if TopNode.Count = 0 then TopNode.Delete;
  end;

  if ModulesGrp.Checked[5] then
  begin
    TopNode := Tree.Items.AddChild(nil, rsWebExtensionsModules);
    ScriptMan.ModulesToList(SL, skWebExpr);
    for j := 0 to SL.Count - 1 do
    begin
      SD := TScriptData(SL.Objects[j]);
      ModuleName := SD.Name;
      ParentNode := Tree.Items.AddChildObject(TopNode, ModuleName,
        CreateSearchTextResult(SD, 0, 0));
      FindInModule;
      if ParentNode.Count = 0 then DeleteTreeNode(ParentNode);
    end;
    if TopNode.Count = 0 then TopNode.Delete;
  end;

  if ModulesGrp.Checked[6] then
  begin
    TopNode := Tree.Items.AddChild(nil, rsUserModules);
    ScriptMan.ModulesToList(SL, skUser);
    for j := 0 to SL.Count - 1 do
    begin
      SD := TScriptData(SL.Objects[j]);
      ModuleName := SD.Name;
      ParentNode := Tree.Items.AddChildObject(TopNode, ModuleName,
        CreateSearchTextResult(SD, 0, 0));
      FindInModule;
      if ParentNode.Count = 0 then DeleteTreeNode(ParentNode);
    end;
    if TopNode.Count = 0 then TopNode.Delete;
  end;

  ExpandAllNodes(Tree);
  Tree.EndUpdate;
  Tree.ClearSelection;
  //if Tree.Items.Count > 0 then Tree.Items[0].Selected := True;
  UpdateResultLabel;
  NothingPan.Visible := FFounds = 0;
  Editor.Free;
  SL.Free;
end;

procedure TFindScriptFm.FormCreate(Sender: TObject);
begin
  PopupParent := ScriptFm;
  Caption := rsFindInModules;
  Label1.Caption := rsSearchedText;
  FindBn.Caption := rsFind;
  FindBn.LoadGlyphFromLazarusResource('find16');
  with ModulesGrp do
  begin
    Items.Clear;
    Items.AddStrings(['Main', 'WebMain', rsFormsModules, rsFormWebModules, rsExtensionsModules, rsWebExtensionsModules, rsUserModules]);
    Checked[0] := True;
    Checked[1] := True;
    Checked[2] := True;
    Checked[3] := True;
    Checked[4] := True;
    Checked[5] := True;
    Checked[6] := True;
  end;
  with OptionsGrp do
  begin
    Items.Clear;
    Items.AddStrings([rsWholeWords, rsCaseSensitive]);
  end;
  ResultLbl.Caption := rsSearchResult;
  NothingPan.Caption := rsNothingFound;
end;

procedure TFindScriptFm.FormDestroy(Sender: TObject);
begin
  ClearTree;
  FindScriptFm := nil;
end;

procedure TFindScriptFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key in [VK_F11, VK_F12] then Key := 0;
end;

procedure TFindScriptFm.FormShow(Sender: TObject);
begin
  TextCbx.SetFocus;
end;

procedure TFindScriptFm.TextCbxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then FindBn.Click;
end;

procedure TFindScriptFm.TreeSelectionChanged(Sender: TObject);
var
  N: TTreeNode;
  pSR: PSearchTextResult;
begin
  N := Tree.Selected;
  if (N = nil) or (N.Data = nil) then Exit;
  pSR := PSearchTextResult(N.Data);
  N := ScriptFm.Modules.Tree.Items.FindNodeWithData(pSR^.SD);
  N.Selected := True;
  with ScriptFm.Memo do
    if pSR^.Row > 0 then
    begin
      CaretY := pSR^.Row;
      CaretX := pSR^.Col;
      SelEnd := SelStart + Length(TextCbx.Text);
      //if UTF8CompareText(SelText, TextCbx.Text) <> 0 then SelEnd := SelStart;
    end
    else
    begin
      CaretX := 1;
      CaretY := 1;
    end;
end;

procedure TFindScriptFm.DeleteTreeNode(N: TTreeNode);
begin
  if N.Data <> nil then
    Dispose(PSearchTextResult(N.Data));
  N.Delete;
end;

procedure TFindScriptFm.UpdateResultLabel;
begin
  if FFounds > 0 then
    ResultLbl.Caption := rsSearchResult + ' (' + IntToStr(FFounds) + ')'
  else
    ResultLbl.Caption := rsSearchResult;
end;

procedure TFindScriptFm.ClearTree;
var
  i: Integer;
  N: TTreeNode;
begin
  for i := 0 to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    if N.Data <> nil then Dispose(PSearchTextResult(N.Data));
  end;
  Tree.Items.Clear;
end;

procedure TFindScriptFm.ShowForm(const S: String);
begin
  NothingPan.Visible := False;
  if S <> '' then TextCbx.Text := S;
  Show;
end;

procedure TFindScriptFm.DeleteModule(SD: TObject);
var
  i: Integer;
  N: TTreeNode;
  pSR: PSearchTextResult;
begin
  with Tree do
  begin
    for i := Items.Count - 1 downto 0 do
    begin
      N := Items[i];
      pSR := PSearchTextResult(N.Data);
      if (pSR <> nil) and (pSR^.SD = SD) then
      begin
        Dispose(pSR);
        N.Delete;
        Dec(FFounds);
      end;
    end;
    for i := Items.TopLvlCount - 1 downto 0 do
    begin
      N := Items.TopLvlItems[i];
      if (N.Data = nil) and (N.Count = 0) then N.Delete;
    end;
  end;
  UpdateResultLabel;
end;

procedure TFindScriptFm.Reset;
begin
  ClearTree;
  TextCbx.Text := '';
  FFounds := 0;
  UpdateResultLabel;
end;

end.

