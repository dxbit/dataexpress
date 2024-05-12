unit TreeForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, ExtCtrls, StdCtrls, Buttons, Spin, Grids, Menus, CtrlUtils,
  dxctrls, strconsts, DialogGrid, LclType;

type

  { TTreeFm }

  TTreeFm = class(TForm)
    DialogGridButtons1: TDialogGridButtons;
    Fields: TDialogGrid;
    Fnt: TBitBtn;
    ButtonPanel1: TButtonPanel;
    BkColor: TColorSampler;
    Label6: TLabel;
    LineColor: TColorSampler;
    SelColor: TColorSampler;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    TrWidth: TSpinEdit;
    Tree: TTreeView;
    ExpandLevels: TSpinEdit;
    procedure BkColorChange(Sender: TObject);
    procedure FieldsCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure FntClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure LineColorChange(Sender: TObject);
    procedure SelColorChange(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    function HasDuplicates(var Row: Integer): Boolean;
    procedure FillFields;
    procedure FillSources;
    procedure Load;
    procedure Save;
  public
    { public declarations }
    procedure ShowForm(aFm: TdxForm);
  end;

var
  TreeFm: TTreeFm;

procedure ShowTreeForm(aFm: TdxForm);

implementation

uses
  FontForm, helpmanager, mytypes, apputils, dxfiles, dximages;

procedure ShowTreeForm(aFm: TdxForm);
begin
  if TreeFm = nil then
  	TreeFm := TTreeFm.Create(Application);
  TreeFm.ShowForm(aFm);
end;

{$R *.lfm}

{ TTreeFm }

procedure TTreeFm.FntClick(Sender: TObject);
begin
  ShowFontForm(Tree.Font, FFm.Font)
  {begin
    if FontFm.IsDefault then
      Tree.Font.Assign(FFm.Font);
  end; }

end;

procedure TTreeFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i, r: Integer;
begin
  if ModalResult <> mrOk then Exit;

  CanClose := False;
  for i := 1 to Fields.RowCount - 1 do
  begin
    if Fields.Cells[0, i] = '' then
    begin
      ErrMsg(rsFieldNotSel);
      Fields.SetFocus;
      Fields.Col := 0; Fields.Row := i;
      Exit;
    end
    else if Fields.Cells[1, i] = '' then
    begin
      ErrMsg(rsNoDataSourceSpecif);
      Fields.SetFocus;
      Fields.Col := 1; Fields.Row := i;
      Exit;
    end
    else if (TdxFormTreeFieldSource(PtrInt(Fields.Objects[1, i])) = tfsObject) and
    	(not (Fields.Objects[0, i] is TdxLookupComboBox)) then
    begin
			ErrMsg(rsObjectMustSelForDataSource);
      Fields.SetFocus;
      Fields.Col := 0; Fields.Row := i;
      Exit;
    end
    else if HasDuplicates(r) then
    begin
      ErrMsg(rsDuplicateField);
      Fields.SetFocus;
			Fields.Col := 0; Fields.Row := r;
      Exit;
    end;
  end;
  CanClose := True;
end;

procedure TTreeFm.FormCreate(Sender: TObject);
begin
  Caption := rsTree;
  Fields.Columns[0].Title.Caption := rsField;
	Fields.Columns[1].Title.Caption := rsDataSource;
  Label2.Caption := rsBackColor;
  Label3.Caption := rsLineColor;
  Label4.Caption:=rsSelectColor;
  Fnt.Caption := rsFont;
  Label5.Caption := rsWidth;
  Label6.Caption := rsExpandLevels;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  Tree.Selected := Tree.Items[0];
  FillSources;
end;

procedure TTreeFm.FormShow(Sender: TObject);
begin
  Fields.SetFocus;
end;

procedure TTreeFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('tree');
end;

procedure TTreeFm.LineColorChange(Sender: TObject);
begin
  Tree.TreeLineColor:=LineColor.SampleColor;
end;

procedure TTreeFm.SelColorChange(Sender: TObject);
begin
  Tree.SelectionColor:=SelColor.SampleColor;
end;

function TTreeFm.HasDuplicates(var Row: Integer): Boolean;
var
  i, j: Integer;
  O1, O2: TObject;
begin
  Result := False;
  for i := 1 to Fields.RowCount - 1 do
  	for j := 2 to Fields.RowCount - 1 do
    begin
      if i = j then Continue;
      O1 := Fields.Objects[0, i];
      O2 := Fields.Objects[0, j];
      Row := j;
      if O1 = O2 then Exit(True);
    end;
end;

procedure TTreeFm.BkColorChange(Sender: TObject);
begin
  Tree.BackgroundColor:=BkColor.SampleColor;
end;

procedure TTreeFm.FieldsCommand(Sender: TObject; Cmd: TDialogGridCommand);
var
  r: Integer;
begin
  case Cmd of
    dgcAppend:
      begin
        r := Fields.RowCount;
        Fields.RowCount := r + 1;
        Fields.Row := r;
        Fields.Cells[1, r] := rsForm;
      end;
    dgcDelete:
      Fields.DeleteRow(Fields.Row);
  end;
end;

procedure TTreeFm.FillFields;
var
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  SL := TStringListUtf8.Create;
  SL.Add('');
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if HasFId(C) and (not (C is TdxFile)) and (not (C is TdxDBImage)) then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Fields.Columns[0].PickList := SL;
  SL.Free;
end;

procedure TTreeFm.FillSources;
begin
  with Fields.Columns[1].PickList do
  begin
    AddObject(rsForm, TObject(tfsForm));
    AddObject(rsObject, TObject(tfsObject));
  end;
end;

procedure TTreeFm.Load;
var
  i, r: Integer;
  C: TComponent;
  TF: TdxFormTreeField;
begin
  r := 1;
  Fields.RowCount := 1;
  for i := 0 to FFm.Tree.Fields.Count - 1 do
  begin
    TF := FFm.Tree.Fields[i];
    C := FindById(FFm, TF.FieldId);
    if C = nil then Continue;
    Fields.RowCount := r + 1;
    Fields.Cells[0, r] := GetFieldName(C);
    Fields.Objects[0, r] := C;
    Fields.Cells[1, r] := TF.FieldSourceToStr;
    Fields.Objects[1, r] := TObject(PtrInt(TF.FieldSource));
    Inc(r);
  end;

  BkColor.DefaultColor:=clWindow;
  BkColor.SampleColor:=FFm.Tree.BackgroundColor;
  LineColor.DefaultColor:=clWindowFrame;
  LineColor.SampleColor:=FFm.Tree.TreeLineColor;
  SelColor.DefaultColor:=clHighlight;
  SelColor.SampleColor:=FFm.Tree.SelectionColor;
  TrWidth.Value := FFm.Tree.Width;
  Tree.BackgroundColor:=FFm.Tree.BackgroundColor;
  Tree.TreeLineColor:=FFm.Tree.TreeLineColor;
  Tree.SelectionColor:=FFm.Tree.SelectionColor;
  Tree.Font := FFm.Tree.Font;
  ExpandLevels.Value := FFm.Tree.ExpandLevels;
end;

procedure TTreeFm.Save;
var
  i: Integer;
  C: TComponent;
begin
  FFm.Tree.Fields.Clear;
  for i := 1 to Fields.RowCount - 1 do
  begin
    C := TComponent(Fields.Objects[0, i]);
    if C = nil then Continue;
    with FFm.Tree.Fields.Add do
    begin
			FieldId := GetId(C);
      FieldSource := TdxFormTreeFieldSource(PtrInt(Fields.Objects[1, i]));
    end;
  end;

  FFm.Tree.BackgroundColor:=BkColor.SampleColor;
  FFm.Tree.TreeLineColor:=LineColor.SampleColor;
  FFm.Tree.SelectionColor:=SelColor.SampleColor;
  FFm.Tree.Font := Tree.Font;
  FFm.Tree.Width:=TrWidth.Value;
  FFm.Tree.ExpandLevels := ExpandLevels.Value;
end;

procedure TTreeFm.ShowForm(aFm: TdxForm);
begin
  FFm := aFm;
  FillFields;
  Load;
  if ShowModal = mrOk then Save;
end;

end.

