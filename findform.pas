unit FindForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, DBGrids, strconsts, dxctrls, Db, ExtCtrls;

type

  { TFindPointer }

  TFindPointer = class(TImage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TFindFrm }

  TFindFrm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FForm: TdxForm;
    FDS: TDataSet;
    FField: TField;
    FLastFieldIndex: Integer;
    FSearchText: String;
    FAfterScroll, FBeforeScroll: TDataSetNotifyEvent;
    FPointer: TFindPointer;
    function GetSelectedField: TComponent;
    procedure FindNext;
    procedure FindPrior;
    procedure FindNextAll;
    procedure FindPriorAll;
    procedure SelectField(F: TField);
    procedure FillFields;
  public
    { public declarations }
    function ShowForm(aForm: TdxForm; aDS: TDataSet): Integer;
  end;

var
  FindFrm: TFindFrm;

function ShowFindForm(aForm: TdxForm; aDS: TDataSet): Integer;

implementation

uses
  sqlgen, LazUtf8, dximages, dxfiles, DBCtrls, helpmanager, mytypes;

function ShowFindForm(aForm: TdxForm; aDS: TDataSet): Integer;
begin
  if FindFrm = nil then
  	FindFrm := TFindFrm.Create(Application);
  Result := FindFrm.ShowForm(aForm, aDS);
end;

{$R *.lfm}

{ TFindPointer }

constructor TFindPointer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Picture.LoadFromLazarusResource('right16');
  AutoSize:=True;
end;

{ TFindFrm }

procedure TFindFrm.FormCreate(Sender: TObject);
begin
  Caption := rsSearch;
  Label1.Caption := rsSearchText;
  Label2.Caption := rsField;
  CheckBox1.Caption := rsSearchAllFields;
  BitBtn1.Caption := rsPrevious;
  BitBtn2.Caption := rsNext;
  BitBtn3.Caption := rsClose;
  BitBtn4.Caption := rsHelp;
  FPointer := TFindPointer.Create(Self);
  FPointer.Visible := True;
end;

procedure TFindFrm.BitBtn2Click(Sender: TObject);
begin
  FPointer.Parent := nil;
  FSearchText:=Utf8LowerCase(Edit1.Text);
  Inc(FLastFieldIndex);
  if FSearchText = '' then Exit;
  if CheckBox1.Checked then FindNextAll
  else if FField <> nil then FindNext
end;

procedure TFindFrm.BitBtn4Click(Sender: TObject);
begin
  OpenHelp('find');
end;

procedure TFindFrm.CheckBox1Change(Sender: TObject);
begin
  ComboBox1.Enabled := not Checkbox1.Checked;
  FLastFieldIndex := FForm.Grid.SelectedColumn.Index;
end;

procedure TFindFrm.ComboBox1Select(Sender: TObject);
var
  C: TComponent;
begin
  FField := nil;
  with ComboBox1 do
    if ItemIndex >= 0 then
    begin
      C := TComponent(Items.Objects[ItemIndex]);
      if C is TdxLookupComboBox then
        FField := FDS.FindField(FieldStr(C) + 'l')
      else if C is TdxFile then
        FField := FDS.FindField(FieldStr(C) + 'd')
      else
        FField := FDS.FindField(FieldStr(C));
    end;
end;

procedure TFindFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FPointer.Parent := nil;
end;

procedure TFindFrm.BitBtn1Click(Sender: TObject);
begin
  FPointer.Parent := nil;
  FSearchText:=Utf8LowerCase(Edit1.Text);
  Dec(FLastFieldIndex);
  if FSearchText = '' then Exit;
  if CheckBox1.Checked then FindPriorAll
  else if FField <> nil then FindPrior
end;

procedure TFindFrm.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
end;

function FindFocusField(Fm: TdxForm): TComponent;
var
  i: Integer;
  C: TComponent;
begin
  Result:= nil;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxDBImage) or (not HasFId(C)) then Continue;
    if TWinControl(C).Focused then
      Exit(C);
  end;
end;

function TFindFrm.GetSelectedField: TComponent;
var
  C: TComponent;
  Col: TColumn;
begin
  C := nil;
  if FForm.ViewType <> vtGridOnly then
    C := FindFocusField(FForm);
  if (C = nil) and (FForm.ViewType <> vtWithoutGrid) then
  begin
    Col := FForm.Grid.SelectedColumn;
    if Col <> nil then
      C := FindById(FForm, Col.Tag);
  end;
  Result := C;
end;

procedure TFindFrm.FindNext;
var
  S: String;
  ok: Boolean;
  B: TBookmark;
begin
  ok := False;
  FDS.DisableControls;
  B := FDS.Getbookmark;

  try

  while not FDS.Eof do
  begin
    FDS.Next;
    S := Utf8LowerCase(FField.AsString);
    if Utf8Pos(FSearchText, S) > 0 then
    begin
      ok := True;
      Break;
    end;
  end;
  if not ok then
    FDS.GotoBookmark(B)
  else SelectField(FField);

  finally
    FDS.FreeBookmark(B);
    FDS.EnableControls;
  end;
end;

procedure TFindFrm.FindPrior;
var
  S: String;
  B: TBookmark;
  ok: Boolean;
begin
  ok := False;
  FDS.DisableControls;
  B := FDS.GetBookmark;

  try

  while not FDS.Bof do
  begin
    FDS.Prior;
    S := Utf8LowerCase(FField.AsString);
    if Utf8Pos(FSearchText, S) > 0 then
    begin
      ok := True;
      Break;
    end;
  end;
  if not ok then
    FDS.GotoBookmark(B)
  else SelectField(FField);

  finally
    FDS.FreeBookmark(B);
    FDS.EnableControls;
  end;
end;

procedure TFindFrm.FindNextAll;
var
  S: String;
  B: TBookmark;
  ok: Boolean;
  i, OldLastIndex: Integer;
  F: TField;
  Col: TColumn;
begin
  ok := False;
  FDS.DisableControls;
  B := FDS.GetBookmark;

  try

  OldLastIndex := FLastFieldIndex;
  if (FLastFieldIndex > FForm.Grid.Columns.Count - 1) and (not FDS.Eof) then
  begin
    FLastFieldIndex := 0;
    FDS.Next;
  end
  else if FLastFieldIndex < 0 then
    FLastFieldIndex := 0;

  while not FDS.Eof do
  begin
    for i := FLastFieldIndex to FForm.Grid.Columns.Count - 1 do
    begin
      Col := FForm.Grid.Columns[i];
      F := Col.Field;
      if F.IsBlob and (F.DataType <> ftMemo) then Continue;
      S := Utf8LowerCase(F.AsString);
      if Utf8Pos(FSearchText, S) > 0 then
      begin
        FLastFieldIndex := i;
        ok := True;
        Break;
      end;
    end;
    if ok then Break;
    FDS.Next;
    FLastFieldIndex := 0;
  end;
  if not ok then
  begin
    FDS.GotoBookmark(B);
    FLastFieldIndex := OldLastIndex;
  end else SelectField(F);

  finally
    FDS.FreeBookmark(B);
    FDS.EnableControls;
  end;
end;

procedure TFindFrm.FindPriorAll;
var
  S: String;
  B: TBookmark;
  ok: Boolean;
  i, OldLastIndex: Integer;
  F: TField;
  Col: TColumn;
begin
  ok := False;
  FDS.DisableControls;
  B := FDS.GetBookmark;

  try

  OldLastIndex := FLastFieldIndex;
  if (FLastFieldIndex < 0) and (not FDS.Bof) then
  begin
    FLastFieldIndex := FForm.Grid.Columns.Count - 1;
    FDS.Prior;
  end
  else if FLastFieldIndex >= FForm.Grid.Columns.Count then
    FLastFieldIndex := FForm.Grid.Columns.Count - 1;

  while not FDS.Bof do
  begin
    for i := FLastFieldIndex downto 0 do
    begin
      Col := FForm.Grid.Columns[i];
      F := Col.Field;
      if F.IsBlob and (F.DataType <> ftMemo) then Continue;
      S := Utf8LowerCase(F.AsString);
      if Utf8Pos(FSearchText, S) > 0 then
      begin
        FLastFieldIndex := i;
        ok := True;
        Break;
      end;
    end;
    if ok then Break;
    FDS.Prior;
    FLastfieldIndex := FForm.Grid.Columns.Count - 1;
  end;
  if not ok then
  begin
    FDS.GotoBookmark(B);
    FLastFieldIndex := OldLastIndex;
  end
  else SelectField(F);

  finally
    FDS.FreeBookmark(B);
    FDS.EnableControls;
  end;
end;

function FindColumnByFieldName(Gr: TdxGrid; const FieldName: String): TColumn;
var
  i: Integer;
  Col: TColumn;
begin
  Result := nil;
  for i := 0 to Gr.Columns.Count - 1 do
  begin
    Col := Gr.columns[i];
    if CompareText(Col.FieldName, FieldName) = 0 then
      Exit(Col);
  end;
end;

procedure VisibleComponent(C: TControl);
begin
  while not (C.Parent is TdxForm) do
  begin
    if C is TdxTabSheet then
      TdxTabSheet(C).PageControl.ActivePage := TdxTabSheet(C);
    C := C.Parent;
  end;
end;

procedure TFindFrm.SelectField(F: TField);
var
  Col: TColumn;
  FId: Integer;
  C: TComponent;
begin
  Col := FindColumnByFieldName(FForm.Grid, F.FieldName);
  FId := Col.Tag;
  if (FForm.ViewType <> vtWithoutGrid) and (Col.Visible) then
    FForm.Grid.SelectedField := F;
  if FForm.ViewType <> vtGridOnly then
  begin
    C := FindById(FForm, FId);
    if (C is TWinControl) then
    begin
      VisibleComponent(TControl(C));
      with TWinControl(C) do
        if CanFocus then
        begin
          SetFocus;
          FPointer.Parent := Parent;
          FPointer.SetBounds(Left - 20, Top, FPointer.Width, FPointer.Height);
        end;
    end;
  end;
end;

procedure TFindFrm.FillFields;
var
  i: Integer;
  C: TComponent;
  L: TStringListUtf8;
begin
  L := TStringListUtf8.Create;
  for i := 0 to FForm.ComponentCount - 1 do
  begin
    C := FForm.Components[i];
    if (not HasFId(C)) or (C is TdxGrid) or (C is TdxDBImage) then Continue;
    L.AddObject(GetFieldName(C), C);
  end;
  L.Sort;
  ComboBox1.Items := L;
  L.Free;
end;

function TFindFrm.ShowForm(aForm: TdxForm; aDS: TDataSet): Integer;
begin
  FForm := aForm;
  FDS := aDS;
  FAfterScroll:=FDS.AfterScroll;
  FBeforeScroll := FDS.BeforeScroll;
  FDS.AfterScroll:=nil;
  FDS.BeforeScroll := nil;
  Edit1.Text := '';
  CheckBox1.State := cbUnchecked;

  ComboBox1.Enabled := True;
  FillFields;
  ComboBox1.ItemIndex := ComboBox1.Items.IndexOfObject(GetSelectedField);
  ComboBox1Select(ComboBox1);

  try
    Result := ShowModal;
  finally
    FDS.AfterScroll:=FAfterScroll;
    FDS.BeforeScroll := FBeforeScroll;
  end;
end;

end.

