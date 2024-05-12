unit InsertValuesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, Menus, strconsts, dxctrls, DialogGrid, StdCtrls, LclType;

type

  { TInsertValuesFm }

  TInsertValuesFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TDialogGrid;
    DialogGridButtons1: TDialogGridButtons;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FCtrl: TdxLookupComboBox;
    procedure FillObjectFields;
    procedure FillFormFields;
    function Validate: Boolean;
    procedure Save;
    procedure Load;
  public
    { public declarations }
    procedure ShowForm(C: TdxLookupComboBox);
  end;

var
  InsertValuesFm: TInsertValuesFm;

procedure ShowInsertValuesForm(C: TdxLookupComboBox);

implementation

uses
  formmanager, apputils, helpmanager, dxfiles, dximages, mytypes;

procedure ShowInsertValuesForm(C: TdxLookupComboBox);
begin
  if InsertValuesFm = nil then
  	InsertValuesFm := TInsertValuesFm.Create(Application);
  InsertValuesFm.ShowForm(C);
end;

{$R *.lfm}

{ TInsertValuesFm }

procedure TInsertValuesFm.FormCreate(Sender: TObject);
begin
  Caption := rsInsertValues;
  Grid.Columns[0].Title.Caption:=rsObjField;
  Grid.Columns[1].Title.Caption:=rsFormField;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TInsertValuesFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TInsertValuesFm.GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
var
  r: Integer;
begin
  case Cmd of
    dgcAppend:
      begin
        r := Grid.RowCount;
        Grid.RowCount := r + 1;
        Grid.Row := r;
      end;
    dgcDelete:
      if ConfirmDelete then
        Grid.DeleteRow(Grid.Row);
  end;
end;

procedure TInsertValuesFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('insertvalues');
end;

procedure TInsertValuesFm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  if ModalResult = mrOk then
    CanClose := Validate;
end;

procedure TInsertValuesFm.FillObjectFields;
var
  SL: TStringListUtf8;
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
begin
  SL := TStringListUtf8.Create;
  Fm := FormMan.FindForm(FCtrl.SourceTId);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxEdit) or (C is TdxCalcEdit) or (C is TdxDateEdit) or
      (C is TdxCheckBox) or (C is TdxMemo) or (C is TdxComboBox) or
      (C is TdxLookupComboBox) or (C is TdxCounter) or (C is TdxTimeEdit) or
      (C is TdxFile) or (C is TdxDBImage) or (C is TdxRecordId) then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Grid.Columns[0].PickList := SL;
  SL.Free;
end;

procedure TInsertValuesFm.FillFormFields;
var
  SL: TStringListUtf8;
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
begin
  SL := TStringListUtf8.Create;
  Fm := TdxForm(FCtrl.Owner);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C = FCtrl then Continue;
    if (C is TdxEdit) or (C is TdxCalcEdit) or (C is TdxDateEdit) or
      (C is TdxCheckBox) or (C is TdxMemo) or (C is TdxComboBox) or
      (C is TdxLookupComboBox) or (C is TdxTimeEdit) or
      (C is TdxFile) or (C is TdxDBImage) then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Grid.Columns[1].PickList := SL;
  SL.Free;
end;

function TInsertValuesFm.Validate: Boolean;
var
  i: Integer;
  ObjF, FmF: TComponent;
begin
  Result := False;
  for i := 1 to Grid.RowCount - 1 do
  begin
    ObjF := TComponent(Grid.Objects[0, i]);
    FmF := TComponent(Grid.Objects[1, i]);
    if ObjF = nil then
    begin
      ErrMsg(rsObjectFieldNotSel);
      Grid.SetFocus;
      Grid.Row := i; Grid.Col := 0;
      Exit;
    end
    else if FmF = nil then
    begin
      ErrMsg(rsFormFieldNotSel);
      Grid.SetFocus;
      Grid.Row := i; Grid.Col := 1;
      Exit;
    end
    else if not CheckCompatibles(ObjF, FmF) then
    begin
      ErrMsg(rsIncompatibleFields);
      Grid.SetFocus;
      Grid.Row := i; Grid.Col := 0;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TInsertValuesFm.Save;
var
  i: Integer;
  ObjF, FmF: TComponent;
  Vl: TInsertValueData;
begin
  FCtrl.InsertedValues.Clear;
  for i := 1 to Grid.RowCount - 1 do
  begin
    ObjF := TComponent(Grid.Objects[0, i]);
    FmF := TComponent(Grid.Objects[1, i]);
    Vl := FCtrl.InsertedValues.AddValue;
    Vl.SrcField:=GetId(ObjF);
    Vl.DestField:=GetId(FmF);
  end;
end;

procedure TInsertValuesFm.Load;
var
  i, r: Integer;
  OFm, Fm: TdxForm;
  ObjF, FmF: TComponent;
  Vl: TInsertValueData;
begin
  Grid.RowCount := 1;
  r := 1;
  OFm := FormMan.FindForm(FCtrl.SourceTId);
  Fm := TdxForm(FCtrl.Owner);
  for i := 0 to FCtrl.InsertedValues.Count - 1 do
  begin
    Vl := FCtrl.InsertedValues[i];
    ObjF := FindById(OFm, Vl.SrcField);
    FmF := FindById(Fm, Vl.DestField);
    if (ObjF = nil) or (FmF = nil) then Continue;
    Grid.RowCount := r + 1;
    Grid.Cells[0, r] := GetFieldName(ObjF);
    Grid.Objects[0, r] := ObjF;
    Grid.Cells[1, r] := GetFieldName(FmF);
    Grid.Objects[1, r] := FmF;
    Inc(r);
  end;
end;

procedure TInsertValuesFm.ShowForm(C: TdxLookupComboBox);
begin
  if (C.SourceTId = 0) or (C.SourceFId = 0) then
  begin
    ErrMsg(rsSourceNotSel);
    Exit;
  end;
  FCtrl := C;
  FillObjectFields;
  FillFormFields;
  Load;
  if ShowModal = mrOk then Save;
end;

end.

