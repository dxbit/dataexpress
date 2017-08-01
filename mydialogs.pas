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
unit MyDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Forms, ComCtrls, ButtonPanel, StrConsts,
  dxctrls, dxreports, StdCtrls, Graphics;

type

  { TSelectFieldForm }

  TSelectFieldForm = class(TForm)
  private
    FFirstParentForm: Boolean;
    FForm, FParForm: TdxForm;
    FShowCurFormPrefix: Boolean;
    FShowLabels: Boolean;
    FShowFieldsOfObject: Boolean;
    FShowObjectFields: Boolean;
    FShowParFormPrefix: Boolean;
    FTree: TTreeView;
    FCurFormNode, FParFormNode: TTreeNode;
    FButtons: TButtonPanel;
    FImages: TImageList;
    FSelField: TStaticText;
    procedure BuildTree;
    function GetCmp: TComponent;
    function GetFldName: String;
    procedure TreeDblClick(Sender: TObject);
    procedure TreeSelection(Sender: TObject);
    procedure TreeShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure SelectNode(PNode: TTreeNode; const Path: String);
    procedure SetControlState;
  protected
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function ShowForm(Fm, ParFm: TdxForm): Integer;
    function ShowForm2(Fm, ParFm: TdxForm; C: TComponent; const Path: String): Integer;
    property ShowLabels: Boolean read FShowLabels write FShowLabels;
    property ShowCurFormPrefix: Boolean read FShowCurFormPrefix write
    	FShowCurFormPrefix;
    property ShowParFormPrefix: Boolean read FShowParFormPrefix write
    	FShowParFormPrefix;
    property FirstParentForm: Boolean read FFirstParentForm write
    	FFirstParentForm;
    property ShowFieldsOfObject: Boolean read FShowFieldsOfObject write
      FShowFieldsOfObject;
    property ShowObjectFields: Boolean read FShowObjectFields
    	write FShowObjectFields;
    property FieldName: String read GetFldName;
    property Component: TComponent read GetCmp;
  end;

  { TSelectQFieldForm }

  TSelectQFieldForm = class(TForm)
  private
    FTree: TTreeView;
    FQryNode, FCFNode: TTreeNode;
    FButtons: TButtonPanel;
    FImages: TImageList;
    FRD: TReportData;
    function GetFldName: String;
    procedure TreeDblClick(Sender: TObject);
    procedure TreeSelection(Sender: TObject);
    procedure TreeShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure BuildTree;
    procedure SetControlState;
  protected
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function ShowForm(RD: TReportData): Integer;
    property FieldName: String read GetFldName;
  end;

  { TCompileErrorsDlg }

  TCompileErrorsDlg = class(TForm)
  private
    //FMsg: TLabel;
    FMemo: TMemo;
    FButtons: TButtonPanel;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    procedure ShowForm(Errs: TStrings);
  end;

  { TLabelCaptionDlg }

  TLabelCaptionDlg = class(TForm)
  private
    FLabel: TdxLabel;
    FEdit: TEdit;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function ShowForm(ALabel: TdxLabel): Integer;
    function CloseQuery: boolean; override;
  end;

procedure ShowCompilerErrors;
function ShowLabelCaptionDlg(ALabel: TdxLabel): Integer;

implementation

uses
  formmanager, dximages, dxfiles, apputils, scriptmanager, LazUtf8, mytypes;

function GetImageIndex(ClsName: String): Integer;
const
  ClsNames: array [0..12] of String = ('TDXLABEL', 'TDXEDIT',
  	'TDXCALCEDIT', 'TDXDATEEDIT', 'TDXTIMEEDIT', 'TDXMEMO',
    'TDXCOUNTER', 'TDXCHECKBOX', 'TDXCOMBOBOX', 'TDXLOOKUPCOMBOBOX',
    'TDXOBJECTFIELD', 'TDXDBIMAGE', 'TDXFILE');
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(ClsNames) do
  	if ClsName = ClsNames[i] then Exit(i);
end;

procedure ShowCompilerErrors;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  with TCompileErrorsDlg.CreateNew(nil) do
  try
    ScriptMan.MessagesToList(SL);
    ShowForm(SL);
  finally
    SL.Free;
    Free;
  end;
end;

function ShowLabelCaptionDlg(ALabel: TdxLabel): Integer;
begin
  with TLabelCaptionDlg.CreateNew(nil) do
  try
    Result := ShowForm(ALabel);
  finally
    Free;
  end;
end;

{ TLabelCaptionDlg }

constructor TLabelCaptionDlg.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsCaption;
  BorderStyle:=bsDialog;
  Position := poOwnerFormCenter;
  L := TLabel.Create(Self);
  L.Parent := Self;
  L.Left := 8; L.Top := 8;
  L.Caption := rsEnterCaption;
  FEdit := TEdit.Create(Self);
  with FEdit do
  begin
		Parent := Self;
    AnchorSideTop.Control := L;
    AnchorSideTop.Side := asrBottom;
    BorderSpacing.Top := 2;
    Text := '';
    Left := 8;
    Width := 328;
  end;
  with TButtonPanel.Create(Self) do
  begin
    Parent := Self;
    ShowButtons := [pbOk, pbCancel];
    OkButton.Caption := rsOk;
    CancelButton.Caption := rsCancel;
  end;
  ClientWidth := 345;
  ClientHeight := 104;
end;

function TLabelCaptionDlg.ShowForm(ALabel: TdxLabel): Integer;
begin
  FLabel := ALabel;
  FEdit.Text := FLabel.Caption;
  Result := ShowModal;
  if Result = mrOk then
  begin
    FLabel.Caption:=FEdit.Text;
    FLabel.Alignment:=taLeftJustify;
    FLabel.FieldName:=FEdit.Text;
  end;
end;

function TLabelCaptionDlg.CloseQuery: boolean;
var
  S: TCaption;
begin
  Result:=inherited CloseQuery;
  if ModalResult <> mrOk then Exit;

  S := FEdit.Text;

  if Trim(S) = '' then
  begin
    ErrMsg(rsLabelEmpty);
    Result := False;
  end
  else if Trim(FLabel.Expression) <> '' then
    Result := CheckName(S) and CheckDuplicateLabel(FLabel, S);
end;

{ TCompileErrorsDlg }

constructor TCompileErrorsDlg.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsUnsuccessfullCompile;
  {FMsg := TLabel.Create(Self);
  with FMsg do
  begin
    Parent := Self;
    AutoSize := False;
    Align := alTop;
    BorderSpacing.Around := 4;
    Height := 40;
    Layout := tlCenter;
    WordWrap:=True;
    Caption := rsCompileErrorMsg;
  end;   }
  FMemo := TMemo.Create(Self);
  with FMemo do
  begin
    Parent := Self;
    Align := alClient;
    BorderSpacing.Top := 4;
    BorderSpacing.Left := 4;
    BorderSpacing.Right := 4;
    ScrollBars := ssBoth;
    ReadOnly := True;
    Font.Name := 'Courier New';
    Font.Size := 9;
  end;
  FButtons := TButtonPanel.Create(Self);
  with FButtons do
  begin
    Parent := Self;
    ShowBevel := False;
    ShowButtons := [pbClose];
    CloseButton.Caption := rsClose;
  end;
  Width := 350;
  Height := 300;
  Position:=poOwnerFormCenter;
  BorderIcons:=[biSystemMenu];
end;

procedure TCompileErrorsDlg.ShowForm(Errs: TStrings);
begin
  FMemo.Lines.Clear;
  FMemo.Lines.Add(rsCompileErrorMsg);
  FMemo.Lines.Add('');
  FMemo.Lines.AddStrings(Errs);
  ShowModal;
end;

{ TSelectQFieldForm }

procedure TSelectQFieldForm.TreeShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  P: Types.TPoint;
  N: TTreeNode;
  pF: PRpField;
begin
  P := HintInfo^.CursorPos;
  N := FTree.GetNodeAt(P.X, P.Y);
  if (N <> nil) and N.Selected and (N.Data <> nil) then
  begin
    pF := PRpField(N.Data);
  	HintInfo^.HintStr := RpFieldTypeToStr(pF^.Tp);
  end;
end;

procedure TSelectQFieldForm.BuildTree;
var
  SL: TStringListUtf8;
  i: Integer;
  pF: PRpField;
  N: TTreeNode;
  pCF: PRpCalcField;
begin
  if FRD.Sources.Count = 0 then Exit;
  FQryNode := FTree.Items.AddChild(nil, FRD.Name);
  SL := TStringListUtf8.Create;
  for i := 0 to FRD.Sources[0]^.Fields.Count - 1 do
  begin
    pF := FRD.Sources[0]^.Fields[i];
    if pF^.Visible then SL.AddObject(pF^.Name, TObject(GetLowField(pF)));
  end;
  SL.Sort;
  for i := 0 to SL.Count - 1 do
  begin
    pF := PRpField(SL.Objects[i]);
    N := FTree.Items.AddChildObject(FQryNode, SL[i], pF);
    //(flNone, flText, flNumber, flDate, flBool, flObject, flTime, flCounter);
    N.ImageIndex := Ord(pF^.Tp) - 1;
    N.SelectedIndex:=N.ImageIndex;
  end;
  if FRD.CalcFields.Count > 0 then
  begin
    FCFNode := FTree.Items.AddChild(FQryNode, rsCalcFields);
    SL.Clear;
    for i := 0 to FRD.CalcFields.Count - 1 do
    begin
      pCF := FRD.CalcFields[i];
      SL.AddObject(pCF^.Name, TObject(pCF));
    end;
    SL.Sort;
    for i := 0 to SL.Count - 1 do
    begin
      pCF := PRpCalcField(SL.Objects[i]);
      N := FTree.Items.AddChild(FCFNode, SL[i]);
      N.ImageIndex := Ord(pCF^.Tp) - 1;
      N.SelectedIndex := N.ImageIndex;
    end;
  end;
  FQryNode.Expand(True);
  SL.Free;
end;

procedure TSelectQFieldForm.SetControlState;
begin
	FButtons.OKButton.Enabled := (FTree.Selected <> nil) and (FTree.Selected.Parent <> nil);
end;

procedure TSelectQFieldForm.DoShow;
begin
  inherited DoShow;
  FTree.SetFocus;
  SetControlState;
end;

procedure TSelectQFieldForm.TreeDblClick(Sender: TObject);
var
  N: TTreeNode;
begin
  N := FTree.Selected;
  if (N <> nil) and (N <> FQryNode) and (N <> FCFNode) then ModalResult := mrOk;
end;

procedure TSelectQFieldForm.TreeSelection(Sender: TObject);
begin
  SetControlState;
end;

function TSelectQFieldForm.GetFldName: String;
var
  N: TTreeNode;
begin
  Result := '';
  N := FTree.Selected;
  if (N <> nil) and (N <> FQryNode) and (N <> FCFNode) then
  	Result := N.Text;
end;

constructor TSelectQFieldForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  FImages := TImageList.Create(Self);
  FImages.AddLazarusResource('text16');
  FImages.AddLazarusResource('calc16');
  FImages.AddLazarusResource('date16');
  FImages.AddLazarusResource('checkbox16');
  FImages.AddLazarusResource('object16');
  FImages.AddLazarusResource('clock16');
  FImages.AddLazarusResource('counter16');
  //FImages.AddLazarusResource('sum16');
  FTree := TTreeView.Create(Self);
  with FTree do
	begin
    Parent := Self;
    Align := alClient;
    BorderSpacing.Left := 4;
    BorderSpacing.Top := 4;
    BorderSpacing.Right := 4;
    Images := FImages;
    ShowHint:=True;
    ReadOnly := True;
    OnShowHint:=@TreeShowHint;
    OnDblClick:=@TreeDblClick;
    OnSelectionChanged:=@TreeSelection;
  end;
  FButtons := TButtonPanel.Create(Self);
  with FButtons do
  begin
    Parent := Self;
    ShowButtons := [pbOk, pbCancel];
    OkButton.Caption := rsOk;
    CancelButton.Caption := rsCancel;
    ShowBevel := False;
  end;
  Width := 350; Height := 400;
  BorderIcons := [biSystemMenu];
  Position := poOwnerFormCenter;
end;

function TSelectQFieldForm.ShowForm(RD: TReportData): Integer;
begin
  FRD := RD;
  BuildTree;
  Result := ShowModal;
end;

{ TSelectFieldForm }

procedure TSelectFieldForm.BuildTree;

	procedure Build(ParentNode: TTreeNode; Fm: TdxForm);
  var
    SL: TStringListUtf8;
    i: Integer;
    C: TComponent;
    N: TTreeNode;
    Frm: TdxForm;
    S: String;
  begin
    if (ParentNode <> nil) and (ParentNode.Level > 4) then Exit;
    SL := TStringListUtf8.Create;
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if not HasFId(C) then
      begin
        if ShowLabels and (C is TdxLabel) and (Trim(TdxLabel(C).Expression) <> '') then
        	S := TdxLabel(C).FieldName
        else Continue;
      end
      else if (C is TdxDBImage) or (C is TdxFile) then Continue
      else if (C is TdxObjectField) and (not FShowObjectFields) then Continue
      else S := GetFieldName(C);// + ' (' + GetComponentType(C) + ')';
      if FShowParFormPrefix and (ParentNode = FParFormNode) then S := '!' + S
      else if FShowCurFormPrefix and (ParentNode = FCurFormNode) then
      	S := ':' + S;
      SL.AddObject(S, C);
    end;
    SL.Sort;
    for i := 0 to SL.Count - 1 do
    begin
      C := TComponent(SL.Objects[i]);
      N := FTree.Items.AddChildObject(ParentNode, SL[i], C);
      N.ImageIndex:=GetImageIndex(UpperCase(C.ClassName));
      N.SelectedIndex:=N.ImageIndex;
      if (C is TdxLookupComboBox) and ShowFieldsOfObject then
      begin
        Frm := FormMan.FindForm(GetSourceTId(C));
        if Frm <> nil then
        	Build(N, Frm);
      end;
    end;
    SL.Free;
  end;

begin
  FTree.Items.Clear;
  FParFormNode := nil;
  FCurFormNode := nil;
	if (FForm <> nil) and (not FFirstParentForm) then
  begin
	  FCurFormNode := FTree.Items.AddChild(nil, FForm.FormCaption);
  	Build(FCurFormNode, FForm);
    FCurFormNode.Expand(False);
  end;
  if FParForm <> nil then
  begin
    FParFormNode := FTree.Items.AddChild(nil, FParForm.FormCaption);
    Build(FParFormNode, FParForm);
    FParFormNode.Expand(False);
  end;
  if (FForm <> nil) and FFirstParentForm then
  begin
	  FCurFormNode := FTree.Items.AddChild(nil, FForm.FormCaption);
  	Build(FCurFormNode, FForm);
    FCurFormNode.Expand(False);
  end;
  {if FForm.PId > 0 then
  begin
    Fm := FormMan.FindForm(FForm.PId);
    FParFormNode := FTree.Items.AddChild(nil, 'Parent form');
    Build(FParFormNode, Fm);
    FParFormNode.Expand(False);
  end;
  FCurFormNode.Expand(False);  }
end;

function TSelectFieldForm.GetCmp: TComponent;
var
  N: TTreeNode;
begin
  Result := nil;
  N := FTree.Selected;
  while (N <> nil) and (N.Parent <> FCurFormNode) and
		(N.Parent <> FParFormNode) do
    N := N.Parent;
  if N <> nil then Result := TComponent(N.Data);
end;

function TSelectFieldForm.GetFldName: String;
var
  N: TTreeNode;
begin
  Result := '';
  N := FTree.Selected;
	while (N <> nil) and (N.Parent <> nil) do
  begin
    Result := N.Text + '|' + Result;
    N := N.Parent;
  end;
  Result := Copy(Result, 1, Length(Result) - 1);
end;

procedure TSelectFieldForm.TreeDblClick(Sender: TObject);
begin
  if (FTree.Selected <> nil) and (FTree.Selected.Parent <> nil) then
  	ModalResult := mrOk;
end;

procedure TSelectFieldForm.TreeSelection(Sender: TObject);
var
  S: String;
begin
  S := FieldName;
  if S = '' then S := ' ';
  FSelField.Caption := S;
  SetControlState;
end;

procedure TSelectFieldForm.TreeShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  P: Types.TPoint;
  N: TTreeNode;
begin
  P := HintInfo^.CursorPos;
  N := FTree.GetNodeAt(P.X, P.Y);
  if (N <> nil) and N.Selected and (N.Data <> nil) then
  	HintInfo^.HintStr := GetComponentType(TComponent(N.Data));
end;

procedure TSelectFieldForm.SelectNode(PNode: TTreeNode; const Path: String);
var
  SL: TStringList;
  i: Integer;
  N: TTreeNode;
begin
  SL := TStringList.Create;
  SplitStr(Path, '|', SL);
  N := PNode;
  for i := 1 to SL.Count - 1 do
  begin
    N := N.FindNode(SL[i]);
    if N = nil then Break;
  end;
  SL.Free;
  if N <> nil then
  begin
    N.Selected := True;
    //N.ExpandParents;
  end;
end;

procedure TSelectFieldForm.SetControlState;
begin
  FButtons.OKButton.Enabled := (FTree.Selected <> nil) and (FTree.Selected.Parent <> nil);
end;

procedure TSelectFieldForm.DoShow;
begin
  inherited DoShow;
  Position := poDesigned;
  FTree.SetFocus;
  SetControlState;
end;

constructor TSelectFieldForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  FImages := TImageList.Create(Self);
  FImages.AddLazarusResource('label16');
  FImages.AddLazarusResource('text16');
  FImages.AddLazarusResource('calc16');
  FImages.AddLazarusResource('date16');
  FImages.AddLazarusResource('clock16');
  FImages.AddLazarusResource('memo16');
  FImages.AddLazarusResource('counter16');
  FImages.AddLazarusResource('checkbox16');
  FImages.AddLazarusResource('combobox16');
  FImages.AddLazarusResource('object16');
  FImages.AddLazarusResource('objectfield16');
  FImages.AddLazarusResource('image16');
  FImages.AddLazarusResource('file16');
  FTree := TTreeView.Create(Self);
  with FTree do
	begin
    Parent := Self;
    Align := alClient;
    BorderSpacing.Left := 4;
    BorderSpacing.Top := 4;
    BorderSpacing.Right := 4;
    Images := FImages;
    ShowHint:=True;
    ReadOnly := True;
    OnShowHint:=@TreeShowHint;
    OnDblClick:=@TreeDblClick;
    OnSelectionChanged:=@TreeSelection;
  end;
  FSelField := TStaticText.Create(Self);
  with FSelField do
  begin
	  Parent := Self;
    AutoSize := True;
  	Align := alBottom;
    BorderStyle := sbsSingle;
    BorderSpacing.Left := 4;
    BorderSpacing.Top := 4;
    BorderSpacing.Right := 4;
    Caption := ' ';
    Alignment:=taCenter;
    Hint:=rsSelectedField;
    ShowHint := True;
  end;
  FButtons := TButtonPanel.Create(Self);
  with FButtons do
  begin
    Parent := Self;
    ShowButtons := [pbOk, pbCancel];
    OkButton.Caption := rsOk;
    CancelButton.Caption := rsCancel;
    ShowBevel := False;
  end;
  Width := 350; Height := 440;
  BorderIcons := [biSystemMenu];
  Position := poOwnerFormCenter;
end;

function TSelectFieldForm.ShowForm(Fm, ParFm: TdxForm): Integer;
begin
  FForm := Fm;
  FParForm := ParFm;
  BuildTree;
  Result := ShowModal;
end;

function TSelectFieldForm.ShowForm2(Fm, ParFm: TdxForm; C: TComponent;
  const Path: String): Integer;
var
  N: TTreeNode;
begin
  FForm := Fm;
  FParForm := ParFm;
  BuildTree;
  N := FTree.Items.FindNodeWithData(C);
  if N <> nil then
  	SelectNode(N, Path);
  Result := ShowModal;
end;

end.

