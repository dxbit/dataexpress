unit LCbxListSourceMoreForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  DialogGrid, dxCtrls, dxreports;

type

  { TLCbxListSourceMoreFm }

  TLCbxListSourceMoreFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    LegendLbl: TLabel;
    SourceCbx: TComboBox;
    KeyFieldCbx: TComboBox;
    Fields: TDialogGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FieldsCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure SourceCbxSelect(Sender: TObject);
  private
    FForm: TdxForm;
    function GetSelectedSource: TReportData;
    function GetSelectedSourceId: Integer;
    function GetSelectedKeyField: String;
    procedure SetSelectedSource(RD: TObject);
    procedure SetSelectedKeyField(const FieldNameDS: String);
    procedure FillSources;
    procedure FillKeyFields;
    procedure FillListFields;
    procedure LoadFields(ListFields: TLCbxListFields);
    procedure SaveFields(ListFields: TLCbxListFields);
    function ValidateFields: Boolean;
    procedure SetControlState;
  public
    procedure Load(LCbx: TdxLookupComboBox);
    procedure Save(LCbx: TdxLookupComboBox);
    function ShowForm: Integer;
    function HasListSource: Boolean;
    function ReadyListSource: Boolean;
    procedure SetVisibleSearchColumn(Value: Boolean);
    procedure Clear;
    function Validate: Boolean;
  end;

var
  LCbxListSourceMoreFm: TLCbxListSourceMoreFm;

implementation

uses
  mytypes, apputils, helpmanager, reportmanager, strconsts;

{$R *.lfm}

{ TLCbxListSourceMoreFm }

procedure TLCbxListSourceMoreFm.SourceCbxSelect(Sender: TObject);
begin
  FillKeyFields;
  FillListFields;
  Fields.RowCount := 1;
  SetControlState;
end;

procedure TLCbxListSourceMoreFm.FieldsCommand(Sender: TObject;
  Cmd: TDialogGridCommand);
begin
  if Cmd = dgcAppend then
    with Fields do
    begin
      RowCount := RowCount + 1;
      Row := RowCount - 1;
      Cells[1, Row] := '100';
      Cells[2, Row] := '0';
    end
  else if Cmd = dgcDelete then
    with Fields do
    begin
      DeleteRow(Row);
    end;
end;

procedure TLCbxListSourceMoreFm.FormCreate(Sender: TObject);
begin
  Caption := rsListSource;
  Label1.Caption := rsListSource;
  Label2.Caption := rsKeyField;
  Label3.Caption := rsFieldsInTheList;
  Fields.Columns[0].Title.Caption := rsField;
	Fields.Columns[1].Title.Caption := rsColumnWidth;
  Fields.Columns[2].Title.Caption := rsIncludeInSearch;
  LegendLbl.Caption := rsSetWidthToAutoWidth;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TLCbxListSourceMoreFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('lcbxlistsource');
end;

function TLCbxListSourceMoreFm.GetSelectedSource: TReportData;
begin
  with SourceCbx do
    if ItemIndex > 0 then
      Result := TReportData(Items.Objects[ItemIndex])
    else
      Result := nil;
end;

function TLCbxListSourceMoreFm.GetSelectedSourceId: Integer;
var
  RD: TReportData;
begin
  RD := GetSelectedSource;
  if RD <> nil then Result := RD.Id
  else Result := 0;
end;

procedure TLCbxListSourceMoreFm.SetSelectedSource(RD: TObject);
var
  i: Integer;
begin
  with SourceCbx do
  begin
    i := Items.IndexOfObject(RD);
    if i > 0 then
      ItemIndex := i;
  end;
end;

function TLCbxListSourceMoreFm.GetSelectedKeyField: String;
var
  i: Integer;
begin
  with KeyFieldCbx do
    if ItemIndex >= 0 then
    begin
      i := PtrInt(Items.Objects[ItemIndex]);
      Result := GetSelectedSource.GetFieldNameDS(i-1);
    end
    else
      Result := '';
end;

procedure TLCbxListSourceMoreFm.SetSelectedKeyField(const FieldNameDS: String);
var
  RD: TReportData;
  i: Integer;
begin
  RD := GetSelectedSource;
  if RD = nil then Exit;

  i := RD.IndexOfNameDS(FieldNameDS);
  if i >= 0 then
  begin
    Inc(i);
    with KeyFieldCbx do
      ItemIndex := Items.IndexOfObject(TObject(PtrInt(i)));
  end;
end;

procedure TLCbxListSourceMoreFm.FillSources;
var
  i: Integer;
  C: TComponent;
  RD: TReportData;
begin
  SourceCbx.Clear;
  SourceCbx.Items.Add('');
  for i := 0 to FForm.ComponentCount - 1 do
  begin
    C := FForm.Components[i];
    if not (C is TdxQueryGrid) then Continue;

    RD := ReportMan.FindReport(GetId(C));
    SourceCbx.Items.AddObject(RD.Name, RD);
  end;
end;

procedure TLCbxListSourceMoreFm.FillKeyFields;
var
  RD: TReportData;
  i: Integer;
begin
  KeyFieldCbx.Clear;
  RD := GetSelectedSource;
  if RD = nil then Exit;

  for i := 0 to RD.GetFieldCount - 1 do
  begin
    if RD.GetFieldType(i) in [flNumber, flCounter, flObject, flRecId] then
      KeyFieldCbx.Items.AddObject(RD.GetFieldName(i), TObject(PtrInt(i+1)));
  end;
end;

procedure TLCbxListSourceMoreFm.FillListFields;
var
  SL: TStringListUtf8;
  RD: TReportData;
  i: Integer;
begin
  RD := GetSelectedSource;
  if RD = nil then Exit;

  SL := TStringListUtf8.Create;
  for i := 0 to RD.GetFieldCount - 1 do
    SL.AddObject(RD.GetFieldName(i), TObject(PtrInt(i+1)));
  SL.Sort;
  Fields.Columns[0].PickList := SL;
  SL.Free;
end;

procedure TLCbxListSourceMoreFm.LoadFields(ListFields: TLCbxListFields);
var
  i, idx, r: Integer;
  LF: TLCbxListField;
  RD: TReportData;
begin
  Fields.RowCount := 1;
  RD := GetSelectedSource;
  if RD = nil then Exit;

  r := Fields.RowCount;
  for i := 0 to ListFields.Count - 1 do
  begin
    LF := ListFields[i];
    idx := RD.IndexOfNameDS(LF.FieldName);
    if idx < 0 then Continue;
    Fields.RowCount := r + 1;
    Fields.Objects[0, r] := TObject(PtrInt(idx+1));
    Fields.Cells[0, r] := RD.GetFieldName(idx);
    Fields.Cells[1, r] := IntToStr(LF.Width);
    Fields.Cells[2, r] := IIF(LF.Searchable, '1', '0');
    Inc(r);
  end;
end;

procedure TLCbxListSourceMoreFm.SaveFields(ListFields: TLCbxListFields);
var
  L: TLCbxListFields;
  i, N, idx: Integer;
  LF: TLCbxListField;
  RD: TReportData;
begin
  RD := GetSelectedSource;
  if RD = nil then Exit;

  L := ListFields;
  L.Clear;
  for i := 1 to Fields.RowCount - 1 do
  begin
    idx := PtrInt(Fields.Objects[0, i]) - 1;
    LF := L.Add;
    LF.FieldName := RD.GetFieldNameDS(idx);
    if TryStrToInt(Fields.Cells[1, i], N) then
	    LF.Width := N;
    LF.Searchable := Fields.Columns[2].Visible and Str2Bool(Fields.Cells[2, i]);
  end;
end;

function TLCbxListSourceMoreFm.Validate: Boolean;
begin
  if GetSelectedSource = nil then Exit(True);

  Result := False;
  if GetSelectedKeyField = '' then
  begin
    ErrMsg(rsKeyFieldNotSelected + LineEnding + rsListSourceClickMoreBn);
  end
  else if ValidateFields then Result := True;
end;

function TLCbxListSourceMoreFm.ValidateFields: Boolean;
var
  i: Integer;
begin
  Result := False;
  if Fields.RowCount = 1 then
  begin
    ErrMsg(rsListFieldsNotSpecify + LineEnding + rsListSourceClickMoreBn);
    Exit;
  end;
  for i := 1 to Fields.RowCount - 1 do
  begin
    if Fields.Cells[0, i] = '' then
    begin
      ErrMsg(rsFieldNotSel + LineEnding + rsListSourceClickMoreBn);
			Fields.Row := i; Fields.Col := 0;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TLCbxListSourceMoreFm.SetControlState;
begin
  Fields.Enabled := HasListSource;
end;

procedure TLCbxListSourceMoreFm.SetVisibleSearchColumn(Value: Boolean);
var
  i: Integer;
begin
  Fields.Columns[2].Visible := Value;
  if not Value then
    for i := 1 to Fields.RowCount - 1 do
      Fields.Cells[2, i] := '0';
end;

procedure TLCbxListSourceMoreFm.Clear;
begin
  SourceCbx.ItemIndex := -1;
  KeyFieldCbx.Clear;
  Fields.RowCount := 1;
  Fields.Columns[0].PickList.Clear;
end;

procedure TLCbxListSourceMoreFm.Load(LCbx: TdxLookupComboBox);
begin
  FForm := TdxForm(LCbx.Owner);
  FillSources;
  SetSelectedSource(ReportMan.FindReport(LCbx.ListSource));
  FillKeyFields;
  SetSelectedKeyField(LCbx.ListKeyField);
  FillListFields;
  LoadFields(LCbx.ListFields);
end;

procedure TLCbxListSourceMoreFm.Save(LCbx: TdxLookupComboBox);
begin
  LCbx.ListSource := GetSelectedSourceId;
  LCbx.ListKeyField := GetSelectedKeyField;
  SaveFields(LCbx.ListFields);
end;

function TLCbxListSourceMoreFm.ShowForm: Integer;
begin
  SetControlState;
  Result := ShowModal;
end;

function TLCbxListSourceMoreFm.HasListSource: Boolean;
begin
  Result := GetSelectedSource <> nil;
end;

function TLCbxListSourceMoreFm.ReadyListSource: Boolean;
begin
  Result := HasListSource and (GetSelectedKeyField <> '') and (Fields.RowCount > 1);
end;

end.

