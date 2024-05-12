unit ModulesTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, LclType, dxctrls, treepanel, strconsts;

type

  { TModulesTree }

  TModulesTree = class(TTreePanel)
  private
    FModNode, FFmNode, FExprNode, FWebFmNode, FWebExprNode: TTreeNode;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildTree; override;
    procedure AddWebFmNode;
    procedure AddWebExprNode;
    procedure DeleteWebFmNode;
    procedure DeleteWebExprNode;
    property ModNode: TTreeNode read FModNode;
    property FmNode: TTreeNode read FFmNode;
    property ExprNode: TTreeNode read FExprNode;
    property WebFmNode: TTreeNode read FWebFmNode;
    property WebExprNode: TTreeNode read FWebExprNode;
  end;

implementation

uses
  scriptmanager, formmanager;

{ TModulesTree }

constructor TModulesTree.Create(AOwner: TComponent);
var
  Images: TImageList;
begin
  inherited Create(AOwner);
  Images := TImageList.Create(Self);
  with Images do
  begin
    AddLazarusResource('form16');
    AddLazarusResource('user16');
    AddLazarusResource('ext16');
    AddLazarusResource('webform16');
    AddLazarusResource('webext16');
  end;
  Tree.Images := Images;
  Filter.TextHint := rsFindModule;
end;

procedure TModulesTree.BuildTree;
var
  N, PN: TTreeNode;
  i: Integer;
  SD: TScriptData;
  Fm: TdxForm;
  SL: TStringList;
begin
  inherited BuildTree;
  FFmNode := nil;
  FWebFmNode := nil;
  FExprNode := nil;
  FWebExprNode := nil;
  FModNode := nil;

  Tree.BeginUpdate;
  SL := TStringList.Create;
  N := Tree.Items.AddChild(nil, 'Main');
  N.Data := ScriptMan.FindScriptByName('Main');

  SD := ScriptMan.FindScriptByName('WebMain');
  if SD <> nil then
  begin
    N := Tree.Items.AddChildObject(nil, 'WebMain', SD);
  end;

  N := Tree.Items.AddChild(nil, rsUserModules);
  FModNode := N;
  N.ImageIndex := 1;
  N.SelectedIndex := 1;
  ScriptMan.ModulesToList(SL, skUser);
  SL.Sort;
  for i := 0 to SL.Count - 1 do
  begin
    SD := TScriptData(SL.Objects[i]);
    Tree.Items.AddChildObject(N, SD.Name, SD);
  end;
  N.Expand(True);

  N := Tree.Items.AddChild(nil, rsFormsModules);
  FFmNode := N;
  N.ImageIndex := 0;
  N.SelectedIndex := 0;
  FormMan.AllFormsToList(SL);
  for i := 0 to SL.Count - 1 do
  begin
    Fm := TdxForm(SL.Objects[i]);
    SD := ScriptMan.FindScript(Fm.Id, skForm);
    if SD <> nil then
    begin
      if Fm.PId = 0 then
        PN := Tree.Items.AddChildObject(N, Fm.FormCaption + ' (' + Fm.Name + ')', SD)
      else
        Tree.Items.AddChildObject(PN, Fm.FormCaption + ' (' + Fm.Name + ')', SD);
    end;
  end;
  N.Expand(True);

  if ScriptMan.WebFormsExists then
  begin
    AddWebFmNode;
    N := WebFmNode;
    FormMan.AllFormsToList(SL);
    for i := 0 to SL.Count - 1 do
    begin
      Fm := TdxForm(SL.Objects[i]);
      SD := ScriptMan.FindScript(Fm.Id, skWebForm);
      if SD <> nil then
      begin
        if Fm.PId = 0 then
          PN := Tree.Items.AddChildObject(N, Fm.FormCaption + ' (' + Fm.Name + ')', SD)
        else
          Tree.Items.AddChildObject(PN, Fm.FormCaption + ' (' + Fm.Name + ')', SD);
      end;
    end;
    N.Expand(True);
  end;

  FExprNode := Tree.Items.AddChild(nil, rsExtensionsModules);
  FExprNode.ImageIndex := 2;
  FExprNode.SelectedIndex := 2;
  ScriptMan.ModulesToList(SL, skExpr);
  SL.Sort;
  for i := 0 to SL.Count - 1 do
  begin
  	Tree.Items.AddChildObject(FExprNode, SL[i], SL.Objects[i]);
  end;
  FExprNode.Expand(True);

  if ScriptMan.WebExprExists then
  begin
    AddWebExprNode;
    ScriptMan.ModulesToList(SL, skWebExpr);
    SL.Sort;
    for i := 0 to SL.Count - 1 do
    begin
    	Tree.Items.AddChildObject(FWebExprNode, SL[i], SL.Objects[i]);
    end;
    FWebExprNode.Expand(True);
  end;

  {for i := 0 to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    if (N.Parent = nil) or (N = FExprNode) then Continue;
  end; }

  SL.Free;
  Tree.EndUpdate;
end;

procedure TModulesTree.AddWebFmNode;
begin
  if FWebFmNode <> nil then Exit;
  FWebFmNode := Tree.Items.InsertBehind(FFmNode, rsFormWebModules);
  FWebFmNode.ImageIndex := 3;
  FWebFmNode.SelectedIndex := 3;
end;

procedure TModulesTree.AddWebExprNode;
begin
  if FWebExprNode <> nil then Exit;
  FWebExprNode := Tree.Items.InsertBehind(FExprNode, rsWebExtensionsModules);
  FWebExprNode.ImageIndex := 4;
  FWebExprNode.SelectedIndex := 4;
end;

procedure TModulesTree.DeleteWebFmNode;
begin
  FWebFmNode.Delete;
  FWebFmNode := nil;
end;

procedure TModulesTree.DeleteWebExprNode;
begin
  FWebExprNode.Delete;
  FWebExprNode := nil;
end;

end.

