unit MemoSourceForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  dxctrls, strconsts;

type

  { TMemoSourceFm }

  TMemoSourceFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Delim: TEdit;
    Field: TComboBox;
    Frm: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    UpdateTree: TComboBox;
    procedure FieldChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FrmChange(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    procedure SetControlState;
    procedure FillFields;
    function SelectedForm: TdxForm;
    function SelectedField: TComponent;
  public
    function ShowForm(Ctrl: TdxMemo): Integer;
  end;

var
  MemoSourceFm: TMemoSourceFm;

function ShowMemoSourceForm(Ctrl: TdxMemo): Integer;

implementation

uses
  formmanager, apputils, HelpManager;

function ShowMemoSourceForm(Ctrl: TdxMemo): Integer;
begin
  if MemoSourceFm = nil then
    MemoSourceFm := TMemoSourceFm.Create(Application);
  Result := MemoSourceFm.ShowForm(Ctrl);
end;

{$R *.lfm}

{ TMemoSourceFm }

procedure TMemoSourceFm.FrmChange(Sender: TObject);
begin
  FillFields;
  SetControlState;
end;

procedure TMemoSourceFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('memopicklist');
end;

procedure TMemoSourceFm.SetControlState;
begin
  Delim.Enabled := SelectedField <> nil;
  UpdateTree.Enabled := SelectedField <> nil;
end;

procedure TMemoSourceFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then Exit;

  CanClose := False;
  if (SelectedField = nil) and (SelectedForm <> nil) then
  begin
    ErrMsg(rsFieldNotSel);
    Field.SetFocus;
  end
  else if (Delim.Text = '') and (SelectedField <> nil) then
  begin
    ErrMsg(rsEnterDelimiter);
    Delim.SetFocus;
  end
  else
    CanClose := True;
end;

procedure TMemoSourceFm.FieldChange(Sender: TObject);
begin
  SetControlState;
end;

procedure TMemoSourceFm.FormCreate(Sender: TObject);
begin
  Caption := rsPickFromListWindow;
  Label1.Caption := rsForm;
  Label2.Caption := rsField;
  Label3.Caption := rsDelimiter;
  Label7.Caption := rsTreeInListWindow;
  UpdateTree.Items[0] := rsRefreshTreeFirstShow;
  UpdateTree.Items[1] := rsRefreshTreeEveryShow;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TMemoSourceFm.FillFields;
var
  i: Integer;
  C: TComponent;
  Fm: TdxForm;
begin
  Field.Clear;
  Fm := SelectedForm;
  if Fm = nil then Exit;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) or
      (C is TdxCounter) or (C is TdxCalcEdit) or (C is TdxRecordId) then
      Field.Items.AddObject(GetFieldName(C), C);
  end;
  Field.Sorted := True;
  Field.Sorted := False;
end;

function TMemoSourceFm.SelectedForm: TdxForm;
begin
  with Frm do
    if ItemIndex > 0 then
      Result := TdxForm(Items.Objects[ItemIndex])
    else
      Result := nil;
end;

function TMemoSourceFm.SelectedField: TComponent;
begin
  with Field do
    if ItemIndex >= 0 then
      Result := TComponent(Items.Objects[ItemIndex])
    else
      Result := nil;
end;

function TMemoSourceFm.ShowForm(Ctrl: TdxMemo): Integer;
var
  Fm: TdxForm;
  C: TComponent;
begin
  Frm.Clear;
  Field.Clear;
  FormMan.FormsToList(Frm.Items);
  Frm.Items.Insert(0, '');
  if Ctrl.SourceTId > 0 then
  begin
    Fm := FormMan.FindForm(Ctrl.SourceTId);
    C := FindById(Fm, Ctrl.SourceFId);
    with Frm do
      ItemIndex := Items.IndexOfObject(Fm);
    FillFields;
    with Field do
      ItemIndex := Items.IndexOfObject(C);
  end;
  Delim.Text := Ctrl.Delimiter;
  if Ctrl.UpdateTree then
    UpdateTree.ItemIndex := 1
  else
    UpdateTree.ItemIndex := 0;
  SetControlState;
  Result := ShowModal;
  if Result = mrOk then
  begin
    if SelectedForm <> nil then
    begin
      Ctrl.SourceTId := SelectedForm.Id;
      Ctrl.SourceFId := GetId(SelectedField);
    end
    else
    begin
      Ctrl.SourceTId := 0;
      Ctrl.SourceFId := 0;
    end;
    Ctrl.Delimiter := Delim.Text;
    Ctrl.UpdateTree := UpdateTree.ItemIndex = 1;
  end;
end;

end.

