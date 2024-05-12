unit TabOrderForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, ButtonPanel, Buttons, StdCtrls, strconsts, dxctrls, CheckTreeView,
  LclType;

type

  { TTabOrderFm }

  TTabOrderFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    ImageList1: TImageList;
    SpeedButton4: TSpeedButton;
    Tree: TCheckTreeView;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure TreeCheckChange(Sender: TObject; ANode: TTreeNode; AValue: Boolean
      );
    procedure TreeSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    procedure FillTree(Ctrl: TWinControl; aNode: TTreeNode);
    procedure SetOrder(Ctrl: TWinControl);
    procedure UpdateControlState;
    procedure SetComponentsIndex;
  public
    { public declarations }
    function ShowForm(Fm: TdxForm; Ctrl: TControl): Integer;
  end;

var
  TabOrderFm: TTabOrderFm;

function ShowTabOrderForm(Fm: TdxForm; Ctrl: TControl): Integer;

implementation

uses
  helpmanager, myctrls, dxreports, dxfiles, pivotgrid, apputils, JvDesignImp;

function ShowTabOrderForm(Fm: TdxForm; Ctrl: TControl): Integer;
begin
  if TabOrderFm = nil then
  	TabOrderFm := TTabOrderFm.Create(Application);
  Result := TabOrderFm.ShowForm(Fm, Ctrl);
end;

{$R *.lfm}

{ TTabOrderFm }

function GetImageIdx(C: TComponent): Integer;
var
  n: Integer;
begin
  n := -1;
  if C is TdxEdit then n := 1
  else if C is TdxCalcEdit then n := 2
  else if C is TdxDateEdit then n := 3
  else if C is TdxTimeEdit then n := 4
  else if C is TdxMemo then n := 5
  else if C is TdxCheckBox then n := 6
  else if C is TdxComboBox then n := 7
  else if C is TdxLookupComboBox then n := 8
  else if C is TdxCounter then n := 9
  else if C is TdxObjectField then n := 10
 	else if C is TdxButton then n := 11
	else if C is TdxQueryGrid then n := 12
  else if C is TdxGrid then n := 13
  else if C is TdxTabSheet then n := 14
  else if C is TdxPageControl then n := 15
  else if C is TdxPivotGrid then n := 16
  else if C is TdxGroupBox then n := 17
  else if C is TdxFile then n := 18
  else if C is TdxForm then n := 0
  else if C is TdxRecordId then n := 19;
  Result := n;
end;

procedure TTabOrderFm.FormShow(Sender: TObject);
var
  N: TTreeNode;
begin
  Tree.SetFocus;
  //N := Tree.Items.GetFirstNode;
  //if N <> nil then Tree.Selected := N;
  UpdateControlState;
end;

procedure TTabOrderFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('taborder');
end;

procedure TTabOrderFm.FormCreate(Sender: TObject);
begin
  Caption := rsTabOrder;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  SpeedButton1.Hint := rsMoveUp;
  SpeedButton2.Hint := rsMoveDown;
  SpeedButton3.Hint := rsAutoTabOrder;
  SpeedButton4.Hint := rsDisableTabStopMsg;
  CheckBox1.Caption := rsTabStop;

  with ImageList1 do
	begin
    AddLazarusResource('form16');
    AddLazarusResource('text16');
    AddLazarusResource('calc16');
    AddLazarusResource('date16');
    AddLazarusResource('clock16');
    AddLazarusResource('memo16');
    AddLazarusResource('checkbox16');
    AddLazarusResource('combobox16');
    AddLazarusResource('object16');
    //AddLazarusResource('label16');
    AddLazarusResource('counter16');
    AddLazarusResource('objectfield16');
    //AddLazarusResource('shape16');
    AddLazarusResource('button16');
    AddLazarusResource('query16');
    AddLazarusResource('grid16');
    //AddLazarusResource('dbimage16');
    //AddLazarusResource('image16');
    AddLazarusResource('tab16');
    AddLazarusResource('tabs16');
    AddLazarusResource('pivottable16');
    AddLazarusResource('groupbox16');
    AddLazarusResource('file16');
    AddLazarusResource('key16');
  end;
end;

procedure TTabOrderFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrClose;
end;

procedure TTabOrderFm.SpeedButton1Click(Sender: TObject);
var
  N, PN: TTreeNode;
begin
  N := Tree.Selected;
  PN := N.GetPrevSibling;
  N.MoveTo(PN, naInsert);
  TWinControl(N.Data).TabOrder:=TWinControl(PN.Data).TabOrder;
  UpdateControlState;
end;

procedure TTabOrderFm.SpeedButton2Click(Sender: TObject);
var
  N, NN: TTreeNode;
begin
  N := Tree.Selected;
  NN := N.GetNextSibling;
  NN.MoveTo(N, naInsert);
  TWinControl(NN.Data).TabOrder:=TWinControl(N.Data).TabOrder;
  UpdateControlState;
end;

procedure TTabOrderFm.SpeedButton3Click(Sender: TObject);
var
  C, TopC: Pointer;
begin
  TopC := Tree.TopItem.Data;
  C := Tree.Selected.Data;
  SetOrder(TWinControl(C));
  Tree.Items.Clear;
  FillTree(FFm, nil);
  Tree.Items[0].Expand(True);
  Tree.TopItem := Tree.Items.FindNodeWithData(TopC);
  Tree.Selected := Tree.Items.FindNodeWithData(C);
end;

procedure TTabOrderFm.SpeedButton4Click(Sender: TObject);
var
  i: Integer;
  N: TTreeNode;
  C: TComponent;
begin
  if Confirm(rsWarning, rsDisableTabStopMsg2) = mrNo then Exit;

  for i := 0 to Tree.Items.Count - 1 do
  begin
    N := Tree.Items[i];
    C := TComponent(N.Data);
    if (C is TdxObjectField) or (C is TdxRecordId) or
      ((C is TdxCounter) and TdxCounter(C).ReadOnly) or
      ( HasExpression(C) and HasEditable(C) and
    	(Trim(GetExpression(C)) <> '') and (not GetEditable(C)) ) then
    begin
      //TWinControl(C).TabStop:=False;
      SetStopTab(C, False);
      Tree.SetChecked(N, False);
    end;
  end;
end;

procedure TTabOrderFm.TreeCheckChange(Sender: TObject; ANode: TTreeNode;
  AValue: Boolean);
begin
  //TWinControl(ANode.Data).TabStop := AValue;
  SetStopTab(TComponent(ANode.Data), AValue);
end;

procedure TTabOrderFm.TreeSelectionChanged(Sender: TObject);
begin
  UpdateControlState;
end;

procedure TTabOrderFm.FillTree(Ctrl: TWinControl; aNode: TTreeNode);

  function AddNode: TTreeNode;
  var
    j: Integer;
    C: TWinControl;
  begin
    if aNode <> nil then
      for j := 0 to aNode.Count - 1 do
      begin
        C := TWinControl(aNode.Items[j].Data);
        if Ctrl.TabOrder < C.TabOrder then
        begin
          Result := Tree.Items.Insert(aNode.Items[j], GetComponentName(Ctrl));
          Exit;
        end;
      end;
    Result := Tree.Items.AddChild(aNode, GetComponentName(Ctrl));
  end;

var
  i: Integer;
  N: TTreeNode;
begin
  N := AddNode;
  N.Data := Ctrl;
  N.ImageIndex := GetImageIdx(Ctrl);
  N.SelectedIndex := N.ImageIndex;

  if (Ctrl = FFm) or (Ctrl is TdxGroupBox) or (Ctrl is TdxTabSheet) then N.StateIndex := -1
  else Tree.SetChecked(N, GetStopTab(Ctrl){Ctrl.TabStop});
  for i := 0 to Ctrl.ControlCount - 1 do
  begin
    if (Ctrl.Controls[i] is TWinControl) and (not (Ctrl.Controls[i] is TGridButtons)) and
      (not (Ctrl.Controls[i] is TJvDesignHandle)) then FillTree(TWinControl(Ctrl.Controls[i]), N);
  end;
end;

procedure TTabOrderFm.SetOrder(Ctrl: TWinControl);
var
  i: Integer;
  L: TList;
  C: TWinControl;

  procedure AddToList(CC: TWinControl);
  var
    j: Integer;
    C: TWinControl;
  begin
    for j := 0 to L.Count - 1 do
    begin
      C := TWinControl(L[j]);
      if (CC.Top < C.Top) or ((CC.Top = C.Top) and (CC.Left < C.Left)) then
      begin
        L.Insert(j, CC);
        Exit;
      end;
    end;
    L.Add(CC);
  end;

begin
  L := TList.Create;
  try
    for i := 0 to Ctrl.ControlCount - 1 do
      if Ctrl.Controls[i] is TWinControl then
      begin
        C := TWinControl(Ctrl.Controls[i]);
        AddToList(C);
      end;
    for i := 0 to L.Count - 1 do
    begin
      TWinControl(L[i]).TabOrder := i;
      //TWinControl(L[i]).Parent.SetControlIndex(TControl(L[i]), i);
    end;
  finally
    L.Free;
  end;
end;

procedure TTabOrderFm.UpdateControlState;
begin
  SpeedButton1.Enabled:=(Tree.Selected <> nil) and (Tree.Selected.GetPrevSibling <> nil);
  SpeedButton2.Enabled:=(Tree.Selected <> nil) and (Tree.Selected.GetNextSibling <> nil);
  SpeedButton3.Enabled:=(Tree.Selected <> nil) and (Tree.Selected.Count > 0);
end;

procedure TTabOrderFm.SetComponentsIndex;

  function GetTabOrderStr(C: TWinControl): String;
  begin
    Result := SetZeros(C.TabOrder, 3);
    if C.Parent <> FFm then
      Result := GetTabOrderStr(C.Parent) + Result;
  end;

var
  SL: TStringList;
  i: Integer;
  C: TComponent;
begin
  SL := TStringList.Create;
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TWinControl then
      SL.AddObject(GetTabOrderStr(TWinControl(C)), C);
  end;
  SL.Sort;
  //for i := 0 to FFm.ComponentCount - 1 do
  //  Debug(FFm.Components[i].Name + '-' + IntToStr(FFm.Components[i].ComponentIndex));
  for i := 0 to SL.Count - 1 do
    TComponent(SL.Objects[i]).ComponentIndex := i;
  //Debug('-------------------');
  //for i := 0 to FFm.ComponentCount - 1 do
  //  Debug(FFm.Components[i].Name + '-' + IntToStr(FFm.Components[i].ComponentIndex));
  SL.Free;
end;

function TTabOrderFm.ShowForm(Fm: TdxForm; Ctrl: TControl): Integer;
begin
  FFm := Fm;
  Tree.Items.Clear;
  FillTree(Fm, nil);
  Tree.Items.GetFirstNode.Expand(True);
  if Ctrl = nil then
    Tree.Selected := Tree.Items.GetFirstNode
  else
    Tree.Selected := Tree.Items.FindNodeWithData(Ctrl);
  Result := ShowModal;
  //SetComponentsIndex;
end;

end.

