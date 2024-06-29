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

unit MyDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Forms, ComCtrls, ButtonPanel, StrConsts,
  dxctrls, dxreports, StdCtrls, Graphics, scriptmanager, LclType, Dialogs,
  treeviewex;

type

  TPickUpEvent = procedure (Sender: TObject; const AFieldName: String; AComponent: TComponent) of object;

  { TSelectFieldForm }

  TSelectFieldForm = class(TForm)
  private
    FFirstParentForm: Boolean;
    FForm, FParForm: TdxForm;
    FOnPickUp: TPickUpEvent;
    FPickUpMode: Boolean;
    FShowCurFormPrefix: Boolean;
    FShowImages: Boolean;
    FShowLabels: Boolean;
    FShowFieldsOfObject: Boolean;
    FShowObjectFields: Boolean;
    FShowParFormPrefix: Boolean;
    FTree: TTreeViewEx;
    FCurFormNode, FParFormNode: TTreeNode;
    FButtons: TButtonPanel;
    FImages: TImageList;
    FSelField: TStaticText;
    procedure BuildTree;
    function GetCmp: TComponent;
    function GetFldName: String;
    procedure SetPickUpMode(AValue: Boolean);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeSelection(Sender: TObject);
    procedure TreeShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure SelectNode(PNode: TTreeNode; const Path: String);
    procedure SetControlState;
  protected
    procedure DoShow; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function ShowForm(Fm, ParFm: TdxForm): Integer;
    function ShowForm2(Tbl, ParFm: TdxForm; const Path: String): Integer;
    procedure ForgetForms;
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
    property ShowImages: Boolean read FShowImages write FShowImages;
    property FieldName: String read GetFldName;
    property Component: TComponent read GetCmp;
    property PickUpMode: Boolean read FPickUpMode write SetPickUpMode;
    property OnPickUp: TPickUpEvent read FOnPickUp write FOnPickUp;
  end;

  { TSelectQFieldForm }

  TSelectQFieldForm = class(TForm)
  private
    FTree: TTreeViewEx;
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
    procedure ForgetReport;
    property FieldName: String read GetFldName;
  end;

  { TCompileErrorsDlg }

  TCompileErrorsDlg = class(TForm)
  private
    FMemo: TMemo;
    FButtons: TButtonPanel;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    procedure ShowForm(Errs: TStrings);
  end;

  { TFormGroupDlg }

  TFormGroupDlg = class(TForm)
  private
    FEdit: TEdit;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function ShowForm(var GrName: String): Integer;
    function CloseQuery: boolean; override;
  end;


procedure ShowCompilerErrors(SD: TScriptData = nil);
function ShowGroupNameDlg(var GrName: String): Integer;

var
  CompilerErrorsDlg: TCompileErrorsDlg;

implementation

uses
  formmanager, dximages, apputils, LazUtf8, mytypes, appsettings;

function GetImageIndex(ClsName: String): Integer;
const
  ClsNames: array [0..13] of String = ('TDXLABEL', 'TDXEDIT',
  	'TDXCALCEDIT', 'TDXDATEEDIT', 'TDXTIMEEDIT', 'TDXMEMO',
    'TDXCOUNTER', 'TDXCHECKBOX', 'TDXCOMBOBOX', 'TDXLOOKUPCOMBOBOX',
    'TDXOBJECTFIELD', 'TDXDBIMAGE', 'TDXFILE', 'TDXRECORDID');
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(ClsNames) do
  	if ClsName = ClsNames[i] then Exit(i);
end;

procedure ShowCompilerErrors(SD: TScriptData);
var
  SL: TStringList;
begin
  if CompilerErrorsDlg = nil then
    CompilerErrorsDlg := TCompileErrorsDlg.CreateNew(Application);

  SL := TStringList.Create;
  with CompilerErrorsDlg do
  try
    if SD = nil then
      ScriptMan.MessagesToList(SL, True)
    else
      ScriptMan.ModuleMessagesToList(SD, SL, True);
    ShowForm(SL);
  finally
    SL.Free;
    //Free;
  end;
end;

function ShowGroupNameDlg(var GrName: String): Integer;
begin
  with TFormGroupDlg.CreateNew(nil) do
  try
		Result := ShowForm(GrName);
  finally
    Free;
  end;
end;

{ TFormGroupDlg }

constructor TFormGroupDlg.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsGroupName;
  BorderStyle:=bsDialog;
  Position := poOwnerFormCenter;
  L := TLabel.Create(Self);
  L.Parent := Self;
  L.Left := 8; L.Top := 8;
  L.Caption := rsEnterGroupName;
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

function TFormGroupDlg.ShowForm(var GrName: String): Integer;
begin
  FEdit.Text := GrName;
  Result := ShowModal;
  if Result = mrOk then
    GrName:=FEdit.Text;
end;

function TFormGroupDlg.CloseQuery: boolean;
begin
  Result:=inherited CloseQuery;
  if ModalResult = mrOk then
  begin
    Result := False;
    if Trim(FEdit.Text) = '' then
    begin
    	ErrMsg(rsGroupNameIsRequired);
      FEdit.SetFocus;
    end
    else if (Pos('\', FEdit.Text) > 0) or (Pos('|', FEdit.Text) > 0) then
    begin
    	ErrMsg(rsGroupNameInvalidChars);
      FEdit.SetFocus;
    end
    else
      Result := True;
  end;
end;

{ TCompileErrorsDlg }

constructor TCompileErrorsDlg.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsUnsuccessfullCompile;
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
  Width := 450;
  Height := 300;
  Position:=poOwnerFormCenter;
  BorderIcons:=[biSystemMenu];
  ShowInTaskBar := stAlways;
  FormStyle := fsStayOnTop;
end;

procedure TCompileErrorsDlg.ShowForm(Errs: TStrings);
begin
  FMemo.Lines.Clear;
  //if FMemo.Lines.Count = 0 then
  //begin
  FMemo.Lines.Add(rsCompileErrorMsg);
  FMemo.Lines.Add('');
  //end;
  FMemo.Lines.AddStrings(Errs);
  Show;
end;

{ TSelectQFieldForm }

procedure TSelectQFieldForm.TreeShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  P: Types.TPoint;
  N: TTreeNode;
  //pF: PRpField;
  FieldIndex: Integer;
begin
  P := HintInfo^.CursorPos;
  N := FTree.GetNodeAt(P.X, P.Y);
  if (N <> nil) and N.Selected and (N.Data <> nil) then
  begin
    FieldIndex := Integer(N.Data) - 1;
  	HintInfo^.HintStr := RpFieldTypeToStr( FRD.GetFieldType(FieldIndex) );
  end;
end;

procedure TSelectQFieldForm.BuildTree;
var
  Fields, CalcFields: TStringListUtf8;
  i, FieldIndex: Integer;
  N: TTreeNode;
begin
  FTree.Items.Clear;
  if FRD.IsEmpty then Exit;

  Fields := TStringListUtf8.Create;
  CalcFields := TStringListUtf8.Create;

  for i := 0 to FRD.GetFieldCount - 1 do
  begin
    if not FRD.IsCalcField(i) then
    begin
      if FRD.GetFieldVisible(i) then
        Fields.AddObject(FRD.GetFieldName(i), TObject(PtrInt(i+1)))
    end
    else
      CalcFields.AddObject(FRD.GetFieldName(i), TObject(PtrInt(i+1)));
  end;

  Fields.Sort;
  CalcFields.Sort;

  FTree.BeginUpdate;
  FQryNode := FTree.Items.AddChild(nil, FRD.Name);

  for i := 0 to Fields.Count - 1 do
  begin
    FieldIndex := Integer(Fields.Objects[i]) - 1;
    N := FTree.Items.AddChildObject(FQryNode, Fields[i], Pointer(FieldIndex+1));
    N.ImageIndex := Ord( FRD.GetFieldType(FieldIndex) ) - 1;
    N.SelectedIndex := N.ImageIndex;
  end;

  if CalcFields.Count > 0 then
  begin
    FCFNode := FTree.Items.AddChild(FQryNode, rsCalcFields);
    for i := 0 to CalcFields.Count - 1 do
    begin
      FieldIndex := Integer(CalcFields.Objects[i]) - 1;
      N := FTree.Items.AddChildObject(FCFNode, CalcFields[i], Pointer(FieldIndex+1));
      N.ImageIndex := Ord( FRD.GetFieldType(FieldIndex) ) - 1;
      N.SelectedIndex := N.ImageIndex;
    end;
  end;
  FQryNode.Expand(True);

  Fields.Free;
  CalcFields.Free;
  FTree.EndUpdate;
end;

procedure TSelectQFieldForm.SetControlState;
begin
	FButtons.OKButton.Enabled := (FTree.Selected <> nil) and (FTree.Selected.Parent <> nil);
end;

procedure TSelectQFieldForm.DoShow;
begin
  inherited DoShow;
  Position := poDesigned;
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
  FImages.AddLazarusResource('file16');
  FImages.AddLazarusResource('key16');
  FImages.AddLazarusResource('dbimage16');
  FTree := TTreeViewEx.Create(Self);
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
    IsWine := AppConfig.IsWine;
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
  if FRD <> RD then
  begin
    FRD := RD;
    BuildTree;
  end;
  Result := ShowModal;
end;

procedure TSelectQFieldForm.ForgetReport;
begin
  FRD := nil;
end;

{ TSelectFieldForm }

procedure TSelectFieldForm.BuildTree;

	procedure Build(ParentNode: TTreeNode; Fm: TdxForm; Depth: Byte);
  var
    SL: TStringListUtf8;
    i: Integer;
    C: TComponent;
    N: TTreeNode;
    Frm: TdxForm;
    S: String;
  begin
    //if (ParentNode <> nil) and (ParentNode.Level > 4) then Exit;
    if Depth > 4 then Exit;
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
      else if (C is TdxDBImage) and (not FShowImages) then Continue
      else if (C is TdxObjectField) and (not FShowObjectFields) then Continue
      else S := GetFieldName(C);
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
        	Build(N, Frm, Depth + 1);
      end;
    end;
    SL.Free;
  end;

begin
  FTree.BeginUpdate;
  FTree.Items.Clear;
  FParFormNode := nil;
  FCurFormNode := nil;
	if (FForm <> nil) and (not FFirstParentForm) then
  begin
	  FCurFormNode := FTree.Items.AddChild(nil, FForm.FormCaption);
  	Build(FCurFormNode, FForm, 0);
    FCurFormNode.Expand(False);
  end;
  if FParForm <> nil then
  begin
    FParFormNode := FTree.Items.AddChild(nil, FParForm.FormCaption);
    Build(FParFormNode, FParForm, 0);
    FParFormNode.Expand(False);
  end;
  if (FForm <> nil) and FFirstParentForm then
  begin
	  FCurFormNode := FTree.Items.AddChild(nil, FForm.FormCaption);
  	Build(FCurFormNode, FForm, 0);
    FCurFormNode.Expand(False);
  end;
  FTree.EndUpdate;
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

procedure TSelectFieldForm.SetPickUpMode(AValue: Boolean);
begin
  if FPickUpMode=AValue then Exit;
  FPickUpMode:=AValue;
  if FPickUpMode then
    FButtons.ShowButtons := [pbClose]
  else
    FButtons.ShowButtons := [pbOk, pbCancel];
end;

procedure TSelectFieldForm.TreeDblClick(Sender: TObject);
begin
  if (FTree.Selected <> nil) and (FTree.Selected.Parent <> nil) then
  begin
    if FPickUpMode then
    begin
      if FOnPickUp <> nil then FOnPickUp(Self, FieldName, Component);
    end
    else
      ModalResult := mrOk;
  end;
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

procedure TSelectFieldForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_ESCAPE then ModalResult := mrClose;
end;

constructor TSelectFieldForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  Width := 350; Height := 440;
  BorderIcons := [biSystemMenu];
  Position := poOwnerFormCenter;

  FShowImages := True;

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
  FImages.AddLazarusResource('dbimage16');
  FImages.AddLazarusResource('file16');
  FImages.AddLazarusResource('key16');
  FTree := TTreeViewEx.Create(Self);
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
    IsWine := AppConfig.IsWine;
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
    CloseButton.Caption := rsClose;
    ShowBevel := False;
  end;
end;

function TSelectFieldForm.ShowForm(Fm, ParFm: TdxForm): Integer;
begin
  if (FForm <> Fm) or (FParForm <> ParFm) then
  begin
    FForm := Fm;
    FParForm := ParFm;
    BuildTree;
  end;
  Result := ShowModal;
end;

function TSelectFieldForm.ShowForm2(Tbl, ParFm: TdxForm; const Path: String
  ): Integer;
var
  N: TTreeNode;
  Fm: TdxForm;
begin
  if (FForm <> Tbl) or (FParForm <> ParFm) then
  begin
    FForm := Tbl;
    FParForm := ParFm;
    BuildTree;
  end;
  if Path <> '' then
  begin
    if Copy(Path, 1, 1) = '!' then Fm := ParFm
    else Fm := Tbl;
    if Fm <> nil then
    begin
      N := FTree.Items.FindNodeWithTextPath(Fm.FormCaption + '/' + StringReplace(Path, '|', '/', [rfReplaceAll]));
      if N <> nil then N.Selected := True;
    end;
  end;
  Result := ShowModal;
end;

procedure TSelectFieldForm.ForgetForms;
begin
  FForm := nil;
  FParForm := nil;
end;

end.

