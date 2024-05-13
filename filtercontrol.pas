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

unit FilterControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, strconsts, StdCtrls, dxctrls, myclasses,
  Controls, DXReports, Menus, Graphics, ComCtrls, timeedit, Dialogs,
  LclType, EditBtn;

type

  { TFilterComboBox }

  TFilterComboBox = class(TCustomComboBox)
  private
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
    FIsObject: Boolean;
    FOldItemIndex: Integer;
    function GetKey: Integer;
    procedure SetKey(AValue: Integer);
  protected
    procedure DoEnter; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(TheOwner: TComponent); override;
    property Key: Integer read GetKey write SetKey;
    property IsObject: Boolean read FIsObject write FIsObject;
    property BorderStyle;
    property OnChange;
    property OnEditingDone;
  end;

  { TFilterEdit }

  TFilterEdit = class(TEdit)
  private
    FNumeric: Boolean;
    FOldText: String;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Validate: Boolean;
    procedure Undo; override;
    property Numeric: Boolean read FNumeric write FNumeric;
  end;

  { TFilterRange }

  TFilterRange = class(TCustomControl)
  private
    FGrid: TCustomGrid;
    FCol, FRow: Integer;
    FBegin, FEnd: TFilterEdit;
    FOldValue: String;
    FOnChange: TNotifyEvent;
    procedure EditorEditingDone(Sender: TObject);
    procedure EditorKeyDown(Editor: TFilterEdit; var Key: Word;  Shift: TShiftState);
    procedure BeginKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorChange(Sender: TObject);
    procedure EndKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetValue: String;
    procedure SetValue(AValue: String);
  protected
    procedure DoOnResize; override;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
    function Validate: Boolean;
    function Focused: Boolean; override;
    procedure Clear;
    property Value: String read GetValue write SetValue;
    property BeginR: TFilterEdit read FBegin;
    property EndR: TFilterEdit read FEnd;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TFilterDate }

  TFilterDate = class(TDateEdit)
  private
    FOldText: String;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  protected
    procedure DoEnter; override;
    procedure ButtonClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Validate: Boolean;
    procedure Undo; override;
  end;

  TPeriodType = (ptNone, ptToday, ptThisWeek, ptThisMonth, ptThisYear);

  { TFilterPeriod }

  TFilterPeriod = class(TCustomControl)
    procedure MenuPopup(Sender: TObject);
  private
    FGrid: TCustomGrid;
    FCol, FRow: Integer;
    FBegin, FEnd: TFilterDate;
    FOnChange: TNotifyEvent;
    FPeriod: TPeriodType;
    FPopup: TPopupMenu;
    FBeginText, FEndText, FOldValue: String;
    procedure EditorKeyDown(Editor: TFilterDate; var Key: Word;  Shift: TShiftState);
    procedure BeginKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorChange(Sender: TObject);
    procedure EditorKeyPress(Sender: TObject; var Key: char);
    procedure EndKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetValue: String;
    procedure MenuClick(Sender: TObject);
    procedure SetPeriod(AValue: TPeriodType);
    procedure EditorEditingDone(Sender: TObject);
    procedure SetValue(AValue: String);
  protected
    procedure DoOnResize; override;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
    function Validate: Boolean;
    function Focused: Boolean; override;
    procedure Clear;
    property BeginP: TFilterDate read FBegin;
    property EndP: TFilterDate read FEnd;
    property Period: TPeriodType read FPeriod write SetPeriod;
    property Value: String read GetValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEditingDone;
  end;

  { TTimeIntervalEdit }

  TTimeIntervalEdit = class(TCustomControl)
  private
    FGrid: TCustomGrid;
    FCol, FRow: Integer;
    FBeginT: TTimeEditEx;
    FEndT: TTimeEditEx;
    FOldValue: String;
    FOnChange: TNotifyEvent;
    function GetValue: String;
    procedure EditorEditingDone(Sender: TObject);
    procedure EditorKeyDown(Editor: TTimeEditEx; var Key: Word;  Shift: TShiftState);
    procedure BeginTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorChange(Sender: TObject);
    procedure EditorKeyPress(Sender: TObject; var Key: char);
    procedure EndTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetValue(AValue: String);
  protected
    procedure DoOnResize; override;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
    function Focused: Boolean; override;
    function Validate: Boolean;
    procedure Clear;
    property Value: String read GetValue write SetValue;
    property BeginT: TTimeEditEx read FBeginT;
    property EndT: TTimeEditEx read FEndT;
    property OnEditingDone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TFilterControl }

  TFilterControl = class(TStringGrid)
  private
    FAllowAddFields: Boolean;
    FButtons: TToolBar;
    FEncodePeriod: Boolean;
    FFieldsEdit: TFilterComboBox;
    FNumEdit: TFilterRange;
    FDateEdit: TFilterPeriod;
    FBoolEdit: TFilterComboBox;
    FObjEdit: TFilterComboBox;
    FListEdit: TFilterComboBox;
    FOnChange: TNotifyEvent;
    FTimeEdit: TTimeIntervalEdit;
    FPopup: TPopupMenu;
    procedure ButtonClick(Sender: TObject);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure CheckBoxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure EditorChange(Sender: TObject);
    procedure FillListEdit;
    procedure MenuClick(Sender: TObject);
    procedure EditorEditingDone(Sender: TObject);
    function InitNumEdit: TWinControl;
    function InitDateEdit: TWinControl;
    function InitTimeEdit: TWinControl;
    function InitBoolEdit: TWinControl;
    function InitListEdit: TWinControl;
    function InitObjEdit: TWinControl;
    procedure FillObjEdit;
    procedure FieldsEditingDone(Sender: TObject);
    procedure SetAllowAddFields(AValue: Boolean);
    procedure AddField;
    procedure DeleteField;
    procedure AddValue;
    procedure DeleteValue;
    procedure ClearValue;
    procedure ClearAllValues;
    procedure SetButtons(AValue: TToolBar);
    procedure SetControlState;
    procedure DoChange;
  protected
    procedure SelectEditor; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  protected
    procedure FillFields(L: TStrings); virtual;
    function LookupField(Obj: TObject; const aFieldName: String): TObject; virtual;
    function GetFieldType(Obj: TObject): TRpFieldType; virtual;
    function GetValues(i: Integer): String;
    procedure SetValues(i: Integer; SL: TStringList);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init; virtual;
    procedure Save; virtual;
    procedure Load; virtual;
    procedure Clear; virtual;
    property AllowAddFields: Boolean read FAllowAddFields write SetAllowAddFields;
    property EncodePeriod: Boolean read FEncodePeriod write FEncodePeriod;
    property Buttons: TToolBar read FButtons write SetButtons;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TRpParamsControl }

  TRpParamsControl = class(TFilterControl)
  private
    FRD: TReportData;
  protected
    function LookupField(Obj: TObject; const aFieldName: String): TObject; override;
    function GetFieldType(Obj: TObject): TRpFieldType; override;
    procedure SelectEditor; override;
    function GetValuesStr(i: Integer): String;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Load; override;
    procedure Save; override;
    property RD: TReportData read FRD write FRD;
  end;

  { TFormFilterControl }

  TFormFilterControl = class(TFilterControl)
  private
    FFilter: TFilterObject;
    FForm: TdxForm;
  protected
    procedure FillFields(L: TStrings); override;
    function LookupField(Obj: TObject; const aFieldName: String): TObject; override;
    function GetFieldType(Obj: TObject): TRpFieldType; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Load; override;
    procedure Save; override;
    property Form: TdxForm read FForm write FForm;
    property Filter: TFilterObject read FFilter write FFilter;
  end;

implementation

uses
  sqlgen, dbengine, apputils, formmanager, strutils, DateUtils, mytypes,
  LazUtf8, appimagelists;

type
  THackGrid = class(TCustomGrid);

{ TFormFilterControl }

procedure TFormFilterControl.FillFields(L: TStrings);
var
  SL: TStringListUtf8;
  i: Integer;
  C: TComponent;
begin
  L.Clear;
  SL := TStringListUtf8.Create;
  for i := 0 to FForm.ComponentCount - 1 do
  begin
    C := FForm.Components[i];
   { if (C is TdxEdit) or (C is TdxCalcEdit) or (C is TdxDateEdit) or
      (C is TdxCheckBox) or (C is TdxComboBox) or (C is TdxLookupComboBox) or
      (C is TdxTimeEdit) or (C is TdxCounter) or (C is TdxMemo) or
      ((C is TdxObjectField) and (FForm.PId = 0)) then }
   if not HasFId(C) then Continue;
   if (C is TdxObjectField) and (FForm.PId <> 0) then Continue;
   SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  L.AddStrings(SL);
  SL.Free;
end;

function TFormFilterControl.LookupField(Obj: TObject; const aFieldName: String
  ): TObject;
var
  Fm: TdxForm;
  C: TComponent;
begin
  Result:=inherited LookupField(Obj, aFieldName);
  if (Result <> nil) and (Result is TdxObjectField) then
  begin
    Fm := TdxForm(TComponent(Obj).Owner);
    C := FindById(Fm, TdxObjectField(Obj).ObjId);
    if C <> nil then
    begin
      Fm := FormMan.FindForm(GetSourceTId(C));
      if Fm <> nil then
        Result := FindById(Fm, TdxObjectField(Obj).FieldId);
    end;
  end;
end;

function TFormFilterControl.GetFieldType(Obj: TObject): TRpFieldType;
begin
  Result := GetTypeByComponent(TComponent(LookupField(Obj, '')));
end;

constructor TFormFilterControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EncodePeriod := True;
end;

procedure TFormFilterControl.Load;
var
  i, r: Integer;
  C: TComponent;
  F: TFilterField;
begin
  inherited Load;
  Clear;
  r := 1;
  RowCount := 1;
  for i := 0 to FFilter.Count - 1 do
  begin
    F := FFilter.Fields[i];
    RowCount := r + 1;
    C := FindById(FForm, F.FId);
    if C = nil then Continue;
    Cells[0, r] := GetFieldName(C);
    Objects[0, r] := C;
    Cells[1, r] := Bool2Str(F.IsNot);
    Cells[2, r] := Bool2Str(F.IsNull);
    SetValues(r, F.Values);
    Inc(r);
  end;
  SetControlState;
end;

procedure TFormFilterControl.Save;
var
  i: Integer;
  C: TComponent;
  F: TFilterField;
begin
  FFilter.Clear;
  for i := 1 to RowCount - 1 do
  begin
    C := TComponent(Objects[0, i]);
    if C = nil then Continue;
    F := FFilter.AddField;
    F.FId := GetId(C);
    F.IsNot := Str2Bool(Cells[1, i]);
    F.IsNull := Str2Bool(Cells[2, i]);
    F.Values.DelimitedText := GetValues(i);
  end;
end;

{ TRpParamsControl }

function TRpParamsControl.LookupField(Obj: TObject; const aFieldName: String
  ): TObject;
begin
  Result:=GetRpFieldComponent(PRpField(Obj)^, True);
end;

function TRpParamsControl.GetFieldType(Obj: TObject): TRpFieldType;
var
  pF: PRpField;
begin
  pF := PRpField(Obj);
  if pF^.Func in [tfCount, tfDistCount] then Result := flNumber
  else Result := GetLowField(pF)^.Tp;
end;

procedure TRpParamsControl.SelectEditor;
begin
  inherited SelectEditor;
  if Col = 0 then Editor := nil;
end;

function TRpParamsControl.GetValuesStr(i: Integer): String;
var
  j: Integer;
  S: String;
  pF: PRpField;
begin
  Result := '';
  pF := PRpField(Objects[0, i]);
  for j := 3 to ColCount - 1 do
  begin
    S := Cells[j, i];
    if pF^.Tp = flText then S := EscapeSemicolon(S);
    Result := Result + S;
    if j < ColCount - 1 then
    	Result := Result + ';';
  end;
end;

constructor TRpParamsControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AllowAddFields := False;
end;

procedure TRpParamsControl.Load;
var
  Sr: TRpSource;
  i, r: Integer;
  F: TRpField;
  pF: PRpField;
  SL: TStringList;
begin
  inherited Load;
  Clear;
  if RD.Sources.Count = 0 then Exit;

  SL := TStringList.Create;
  r := 1;
  Sr := RD.Sources[0]^;
  for i := 0 to Sr.Fields.Count - 1 do
  begin
    //pF := Sr.Fields[i];
    pF := RD.FindField(Sr.Fields[i]^.Id);
    F := pF^;
    if not F.Param then Continue;
    RowCount := r + 1;
    Cells[0, r] := F.Name;
    Objects[0, r] := TObject(pF);
    Cells[1, r] := Bool2Str(F.No);
    Cells[2, r] := Bool2Str(F.Nul);
    SplitStr(F.Value, ';', SL);
    SetValues(r, SL);
    Inc(r);
  end;
  SL.Free;
  SetControlState;
end;

procedure TRpParamsControl.Save;
var
  i, j: Integer;
  pF: PRpField;
  Sr: TRpSource;
  F: TRpField;
begin
  for i := 1 to RowCount - 1 do
  begin
    pF := PRpField(Objects[0, i]);
    pF^.No:=Cells[1, i] = '1';
    pF^.Nul := Cells[2, i] = '1';
    pF^.Value := GetValues(i);
    // Текстовое представление параметров (для печати)
    pF^.ValueStr := GetValuesStr(i);
  end;
  if FRD.Sources.Count = 0 then Exit;
  // Копируем значения параметров в остальные источники
  Sr := FRD.Sources[0]^;
  for i := 0 to Sr.Fields.Count - 1 do
  begin
    F := FRD.FindField(Sr.Fields[i]^.Id)^;
    if not F.Param then Continue;
    for j := 1 to FRD.Sources.Count - 1 do
      with FRD.Sources[j]^.Fields[i]^ do
      begin
        No := F.No;
        Nul := F.Nul;
        Value:=F.Value;
      end;
  end;
end;

{ TFilterControl }

procedure TFilterControl.FieldsEditingDone(Sender: TObject);
var
  i: Integer;
begin
  with TFilterComboBox(Sender) do
  begin
    if (ItemIndex >= 0) and (Objects[Col, Row] <> Items.Objects[ItemIndex]) then
    begin
      Cells[Col, Row] := Text;
      Objects[Col, Row] := Items.Objects[ItemIndex];
      for i := 3 to Columns.Count - 1 do
      begin
        Cells[i, Row] := '';
        Objects[i, Row] := nil;
      end;
    end
    else if ItemIndex < 0 then
    begin
      Cells[Col, Row] := '';
      Objects[Col, Row] := nil;
      for i := 3 to Columns.Count - 1 do
      begin
        Cells[i, Row] := '';
        Objects[i, Row] := nil;
      end;
    end;
  end;
  DoChange;
end;

procedure TFilterControl.SetAllowAddFields(AValue: Boolean);
begin
  FAllowAddFields:=AValue;
  FPopup.Items[0].Visible:=AValue;
  FPopup.Items[0].Enabled:=AValue;
  FPopup.Items[1].Visible:=AValue;
  FPopup.Items[1].Enabled:=AValue;
  FPopup.Items[2].Visible:=AValue;
end;

procedure TFilterControl.AddField;
var
  r: Integer;
begin
  r := RowCount;
  RowCount := r + 1;
  Cells[1, r] := '0';
  Cells[2, r] := '0';
  Col := 0;
  Row := r;
  SetControlState;
end;

procedure TFilterControl.DeleteField;
begin
  DeleteRow(Row);
  SetControlState;
  DoChange;
end;

procedure TFilterControl.AddValue;
begin
  with Columns.Add do
  begin
    Title.Caption := rsValue;
    Width := 200;
  end;
  SetControlState;
  DoChange;
end;

procedure TFilterControl.DeleteValue;
begin
  if (Col >= 3) and (ColCount > 4) then DeleteCol(Col);
  SetControlState;
  DoChange;
end;

procedure TFilterControl.ClearValue;
begin
  Cells[Col, Row] := '';
  Objects[Col, Row] := nil;
  if Editor is TFilterRange then TFilterRange(Editor).Clear
  else if Editor is TFilterPeriod then TFilterPeriod(Editor).Clear
  else if Editor is TTimeIntervalEdit then TTimeIntervalEdit(Editor).Clear
  else if Editor <> nil then Editor.Caption := '';
  DoChange;
end;

procedure TFilterControl.ClearAllValues;
var
  i, j: Integer;
begin
  if MessageDlg(rsWarning, rsClearAllValuesMsg, mtWarning, [mbYes, mbNo], 0) =
    mrNo then Exit;
  for i := 1 to RowCount - 1 do
    for j := 3 to ColCount - 1 do
    begin
      Cells[j, i] := '';
      Objects[j, i] := nil;
    end;
  DoChange;
end;

procedure TFilterControl.SetButtons(AValue: TToolBar);
begin
  FButtons:=AValue;
  FButtons.Buttons[0].OnClick:=@ButtonClick;
  FButtons.Buttons[1].OnClick:=@ButtonClick;
  FButtons.Buttons[3].OnClick:=@ButtonClick;
  FButtons.Buttons[4].OnClick:=@ButtonClick;
end;

procedure TFilterControl.SetControlState;
begin
  FPopup.Items[1].Enabled:=(RowCount > 1) and FAllowAddFields;
  FPopup.Items[4].Enabled := (Col >= 3) and (ColCount > 4);
  FPopup.Items[6].Enabled := Row > 0;
  FPopup.Items[6].Enabled := Col >= 3;
  FPopup.Items[7].Enabled:=RowCount > 1;
  if FButtons <> nil then
  begin
    FButtons.Buttons[1].Enabled := Rowcount > 1;
    FButtons.Buttons[4].Enabled := (Col >= 3) and (ColCount > 4);
  end;
end;

procedure TFilterControl.DoChange;
begin
  if FOnChange <> nil then FOnChange(Self);
end;

procedure TFilterControl.ButtonClick(Sender: TObject);
begin
  if Sender = FButtons.Buttons[0] then AddField
  else if Sender = FButtons.Buttons[1] then DeleteField
  else if Sender = FButtons.Buttons[3] then AddValue
  else if Sender = FButtons.Buttons[4] then DeleteValue;
end;

procedure TFilterControl.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  SetControlState;
end;

procedure TFilterControl.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: AddField;
    1: DeleteField;
    3: AddValue;
    4: DeleteValue;
    6: ClearValue;
    7: ClearAllValues;
  end;
end;

procedure TFilterControl.FillListEdit;
var
  Cbx: TdxComboBox;
  SQL: String;
begin
  Cbx := TdxComboBox( LookupField(Objects[0, Row], Cells[0, Row]) );

  if (Cbx.SourceTId > 0) and (Cbx.SourceFId > 0) then
  begin
    FListEdit.Items.Clear;
    FListEdit.Items.Add('');

		SQL := SqlLookupSelect(Cbx, nil, nil, nil, False, 0);
    with DBase.OpenDataSet(SQL) do
    begin
      while not Eof do
      begin
        FListEdit.Items.Add(Fields[1].AsString);
        Next;
      end;
      Free;
    end;
  end
  else
  	FListEdit.Items := Cbx.Items;
end;

procedure TFilterControl.EditorChange(Sender: TObject);
begin
  DoChange;
end;

procedure TFilterControl.CheckBoxToggled(sender: TObject; aCol, aRow: Integer;
  aState: TCheckboxState);
begin
  DoChange;
end;

procedure TFilterControl.EditorEditingDone(Sender: TObject);
begin
  if Sender is TFilterComboBox then
  begin
    with TFilterComboBox(Sender) do
      if ItemIndex >= 0 then
      begin
        Cells[Col, Row] := Text;
        Objects[Col, Row] := Items.Objects[ItemIndex]
      end
      else if IsObject then
      begin
        Cells[Col, Row] := '';
        Objects[Col, Row] := nil;
      end
      else
        Cells[Col, Row] := Text;
  end
  else if Sender = FNumEdit then
    Cells[Col, Row] := FNumEdit.Value
  else if Sender = FDateEdit then
  begin
    Cells[Col, Row] := FDateEdit.Value;
    Objects[Col, Row] := TObject(PtrInt(FDateEdit.Period));
  end
  else if Sender = FTimeEdit then
    Cells[Col, Row] := FTimeEdit.Value;
end;

function TFilterControl.InitNumEdit: TWinControl;
begin
  FNumEdit.Value := Cells[Col, Row];
  Result := FNumEdit;
end;

function TFilterControl.InitDateEdit: TWinControl;
begin
  FDateEdit.Value := Cells[Col, Row];
  Result := FDateEdit;
end;

function TFilterControl.InitTimeEdit: TWinControl;
begin
  FTimeEdit.Value := Cells[Col, Row];
  Result := FTimeEdit;
end;

function TFilterControl.InitBoolEdit: TWinControl;
begin
  with FBoolEdit do
    ItemIndex := Items.IndexOfObject( Objects[Col, Row] );
  Result := FBoolEdit;
end;

function TFilterControl.InitListEdit: TWinControl;
begin
  FillListEdit;
  FListEdit.Text:=Cells[Col, Row];
  Result := FListEdit;
end;

function TFilterControl.InitObjEdit: TWinControl;
begin
  FillObjEdit;
  FObjEdit.Key:=PtrInt(Objects[Col, Row]);
  Result := FObjEdit;
end;

function Abrakadabra(const Path: String): String;
var
  SL: TStringList;
begin
  Result := '';
  SL := TStringList.Create;
  SplitStr(Path, #9, SL);
  if SL.Count > 0 then
    Result := DupeString('  ', SL.Count - 1) + SL[SL.Count - 1];
  SL.Free;
end;

procedure TFilterControl.FillObjEdit;
var
  C: TdxLookupComboBox;
  SQL: String;
  Fm: TdxForm;
  SL: TStringListUtf8;
  i: Integer;
  ShowTree: Boolean;
begin
  ShowTree := False;
  SL := TStringListUtf8.Create;
  FObjEdit.Clear;
  C := TdxLookupComboBox( LookupField(Objects[0, Row], Cells[0, Row]) );
  if (C.SourceTId > 0) and (C.SourceFId > 0) then
  begin
    Fm := FormMan.FindForm(C.SourceTId);
    ShowTree := (Fm.ParentField > 0) and (GetFormParentFieldFieldId(Fm) = C.SourceFId);
    FObjEdit.Items.Add('');
    SQL := SQLLookupSelect(C, nil, nil, nil, False, 0);
    with DBase.OpenDataSet(SQL) do
    try
      while not Eof do
      begin
        if not ShowTree then
          FObjEdit.Items.AddObject( Fields[1].AsString, TObject(PtrInt(Fields[0].AsInteger)) )
        else
        begin
          // Группы могут быть неправильно отсортированы. Из-за этого
          // в списке подгруппы могут быть расположены не в той группе.
          // Это возникает из-за того, что в названии группы может входить
          // символы, которые меньше слеша. Поэтому я меняю слеш на табуляцию -
          // символ, который меньше любого символа в названии...
          SL.AddObject( StringReplace(Fields[1].AsString, '\', #9, [rfReplaceAll]),
          	TObject(PtrInt(Fields[0].AsInteger)) );
        end;
        Next;
      end;
    finally
      Free;
    end;
  end;

  if ShowTree then
  begin
    // ... Теперь сортировка иерархических групп будет правильной
    SL.Sort;
    for i := 0 to SL.Count - 1 do
    	SL[i] := Abrakadabra(SL[i]);
    FObjEdit.Items.AddStrings(SL);
  end;
  SL.Free;
end;

procedure TFilterControl.SelectEditor;
var
  Obj: TObject;
  S: String;
  Tp: TRpFieldType;
  Cmp: TComponent;
begin
  inherited SelectEditor;

  if Col = 0 then
  begin
    Editor := FFieldsEdit;
    Editor.Caption:=Cells[Col, Row];
  end
  else if Col >= 3 then
  begin
    S := Cells[0, Row];
    Obj := Objects[0, Row];
    if Obj <> nil then Tp := GetFieldType(Obj)
    else Tp := flNone;

    case Tp of
      flNone: Editor := nil;
      flNumber, flCounter, flRecId: Editor := InitNumEdit;
      flDate: Editor := InitDateEdit;
      flBool: Editor := InitBoolEdit;
      flObject: Editor := InitObjEdit;
      flTime: Editor := InitTimeEdit;
      flText:
        begin
          Cmp := TComponent(LookupField(Obj, S));
          if Cmp is TdxComboBox then Editor := InitListEdit;
        end;
    end;
    {Obj := TComponent(LookupField(Objects[0, Row], Cells[0, Row]));
    if Obj = nil then Editor := nil
    else if (Obj is TdxCalcEdit) or (Obj is TdxCounter) then Editor := InitNumEdit
    else if Obj is TdxDateEdit then Editor := InitDateEdit
    else if Obj is TdxCheckBox then Editor := InitBoolEdit
    else if Obj is TdxComboBox then Editor := InitListEdit
    else if Obj is TdxLookupComboBox then Editor := InitObjEdit
    else if Obj is TdxTimeEdit then Editor := InitTimeEdit   }
  end;
end;

procedure TFilterControl.DoEnter;
begin
  SelectedColor := clHighlight;
  inherited DoEnter;
end;

procedure TFilterControl.DoExit;
begin
  SelectedColor := clSilver;
  inherited DoExit;
end;

procedure TFilterControl.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldKey: Word;
begin
  OldKey := Key;
  if (Key = VK_RETURN) and (Col in [1..2]) then Key := VK_SPACE
  else if (Key in [VK_C, VK_V]) and (ssCtrl in Shift) and not EditorMode then Key := 0;
  inherited KeyDown(Key, Shift);
  if OldKey = VK_ESCAPE then Key := OldKey;
end;

procedure TFilterControl.FillFields(L: TStrings);
begin

end;

function TFilterControl.LookupField(Obj: TObject; const aFieldName: String
  ): TObject;
begin
  Result := Obj;
end;

function TFilterControl.GetFieldType(Obj: TObject): TRpFieldType;
begin
  Result := flNone;
end;

function TFilterControl.GetValues(i: Integer): String;
var
  Cmp, Obj: TObject;
  j: Integer;
  S: String;
begin
  Result := '';
  Cmp := LookupField(Objects[0, i], Cells[0, i]);
  for j := 3 to ColCount - 1 do
  begin
    S := Cells[j, i];
    Obj := Objects[j, i];
    if Cmp is TdxLookupComboBox then
    begin
      if Obj <> nil then
        S := IntToStr(PtrInt(Obj));
    end
    else if Cmp is TdxCheckBox then
    begin
      if Obj <> nil then
        S := IntToStr(PtrInt(Obj) - 1);
    end
    else if (Cmp is TdxDateEdit) and (Obj <> nil) and FEncodePeriod then
      S := '$' + IntToStr(PtrInt(Obj))
    else if (Cmp is TdxEdit) or (Cmp is TdxMemo) or (Cmp is TdxComboBox) then
      S := EscapeSemicolon(S);
    Result := Result + S;
    if j < ColCount - 1 then
    	Result := Result + ';';
  end;
end;

function PeriodToStr(P: TPeriodType): String;
var
  DB, DE: TDateTime;
begin
  DE := Date;
  case P of
    ptToday: DB := Date;
    ptThisWeek: DB := IncDay(Date, -DayOfTheWeek(Date)+1);
    ptThisMonth: DB := IncDay(Date, -DayOf(Date)+1);
    ptThisYear: DB := EncodeDate(YearOf(Date), 1, 1);
  end;
  Result := DateToStr(DB) + ' .. ' + DateToStr(DE);
end;

procedure TFilterControl.SetValues(i: Integer; SL: TStringList);
var
  j, n: Integer;
  Obj: TObject;
  S: String;
begin
  if SL.Count > 0 then
  begin
    for j := Columns.Count - 3 to SL.Count - 1 do
      AddValue;
    Obj := LookupField(Objects[0, i], Cells[0, i]);
    if Obj <> nil then
    begin
      for j := 3 to ColCount - 1 do
      begin
        n := j - 3;
        if n > SL.Count - 1 then Break;
        S := SL[n];
        if S = '' then Continue;
        //
        if Obj is TdxLookupComboBox then
        begin
          Cells[j, i] := GetObjFieldValue(TComponent(Obj), StrToInt(S), False);
          Objects[j, i] := TObject(PtrInt(StrToInt(S)));
        end
        else if Obj is TdxCheckBox then
        begin
          if Str2Bool(S) then
          begin
            Cells[j, i] := rsYes;
            Objects[j, i] := TObject(2)
          end
          else
          begin
            Cells[j, i] := rsNo;
            Objects[j, i] := TObject(1);
          end;
        end
        else if (Obj is TdxDateEdit) and (Copy(S, 1, 1) = '$') then
        begin
          Objects[j, i] := TObject(PtrInt(StrToInt(Copy(S, 2, 10))));
          Cells[j, i] := PeriodToStr(TPeriodType(PtrInt(Objects[j, i])));
        end
        else if (Obj is TdxEdit) or (Obj is TdxMemo) or (Obj is TdxComboBox) then
          Cells[j, i] := UnEscapeSemicolon(S)
        else
          Cells[j, i] := S;
      end;
    end;
  end;
end;

constructor TFilterControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFieldsEdit := TFilterComboBox.Create(Self);
  FFieldsEdit.IsObject := True;
  {$ifdef windows}
  FFieldsEdit.Style := csDropDownList;
  {$endif}
  FFieldsEdit.OnEditingDone:=@FieldsEditingDone;
  FNumEdit := TFilterRange.Create(Self);
  FNumEdit.OnEditingDone:=@EditorEditingDone;
  FNumEdit.OnChange:=@EditorChange;
  FDateEdit := TFilterPeriod.Create(Self);
  FDateEdit.OnEditingDone:=@EditorEditingDone;
  FDateEdit.OnChange:=@EditorChange;
  FBoolEdit := TFilterComboBox.Create(Self);
  FBoolEdit.Items.AddObject('', nil);
  FBoolEdit.Items.AddObject(rsNo, TObject(1));
  FBoolEdit.Items.AddObject(rsYes, TObject(2));
  {$ifdef windows}
  FBoolEdit.Style:=csDropDownList;
  {$endif}
  FBoolEdit.OnEditingDone:=@EditorEditingDone;
  FBoolEdit.OnChange:=@EditorChange;
  FObjEdit := TFilterComboBox.Create(Self);
  FObjEdit.IsObject := True;
  FObjEdit.AutoComplete:=True;
  FObjEdit.OnEditingDone:=@EditorEditingDone;
  FObjEdit.OnChange:=@EditorChange;
  FListEdit := TFilterComboBox.Create(Self);
  FListEdit.AutoComplete:=True;
  FListEdit.OnEditingDone:=@EditorEditingDone;
  FListEdit.OnChange:=@EditorChange;
  FTimeEdit := TTimeIntervalEdit.Create(Self);
  FTimeEdit.OnEditingDone:=@EditorEditingDone;
  FTimeEdit.OnChange:=@EditorChange;

  FPopup := TPopupMenu.Create(Self);
  FPopup.Images := Images16;
  FPopup.Items.Add( CreateMenuItem(FPopup, rsAddField, 0, ShortCut(VK_INSERT, []), @MenuClick, IMG16_ADD));
  FPopup.Items.Add( CreateMenuItem(FPopup, rsDeleteField, 1, ShortCut(VK_DELETE, [ssCtrl]), @MenuClick, IMG16_DELETE));
  FPopup.Items.Add( CreateMenuItem(FPopup, '-', 2, 0, @MenuClick));
  FPopup.Items.Add( CreateMenuItem(FPopup, rsAddValue, 3, 0, @MenuClick, IMG16_ADD));
  FPopup.Items.Add( CreateMenuItem(FPopup, rsDeleteValue, 4, 0, @MenuClick, IMG16_DELETE));
  FPopup.Items.Add( CreateMenuItem(FPopup, '-', 5, 0, @MenuClick));
  FPopup.Items.Add( CreateMenuItem(FPopup, rsClearValue, 6, 0, @MenuClick));
  FPopup.Items.Add( CreateMenuItem(FPopup, rsClearAllValues, 7, 0, @MenuClick));
  {SetMenuItemImage(FPopup.Items[0], 'add16');
  SetMenuItemImage(FPopup.Items[1], 'delete16');
  SetMenuItemImage(FPopup.Items[3], 'add16');
  SetMenuItemImage(FPopup.Items[4], 'delete16');}
  PopupMenu := FPopup;

  FixedCols := 0;
  RowCount:=1;
  AlternateColor:=$00F0F0F0;
  FocusColor:=clHighlight;
  SelectedColor:=clSilver;
  AutoAdvance:=aaNone;

  Options := Options + [goEditing, goColSizing, goDrawFocusSelected, goThumbTracking] -
  	[goRangeSelect, goVertLine, goHorzLine];
  with Columns.Add do
  begin
    Title.Caption := rsFieldName;
    Width := 150;
  end;
  with Columns.Add do
  begin
    Title.Caption := rsNot;
    Width := 50;
    ButtonStyle:=cbsCheckboxColumn;
  end;
  with Columns.Add do
  begin
    Title.Caption := rsNull;
    Width := 50;
    ButtonStyle:=cbsCheckboxColumn;
  end;
  with Columns.Add do
  begin
    Title.Caption := rsValue;
    Width := 200;
  end;

  Flat := True;
  OnSelection:=@GridSelection;
  OnCheckboxToggled:=@CheckBoxToggled;
  AllowAddFields:=True;
end;

procedure TFilterControl.Init;
begin
  //FillFields(Columns[0].PickList);
  FFieldsEdit.Clear;
  FillFields(FFieldsEdit.Items);
end;

procedure TFilterControl.Save;
begin

end;

procedure TFilterControl.Load;
begin

end;

procedure TFilterControl.Clear;
var
  i: Integer;
begin
  RowCount := 1;
  for i := Columns.Count - 1 downto 4 do
    DeleteCol(i);
  SetControlState;
end;

{ TTimeIntervalEdit }

procedure TTimeIntervalEdit.EditorEditingDone(Sender: TObject);
begin
  EditingDone;
end;

procedure TTimeIntervalEdit.EditorKeyDown(Editor: TTimeEditEx; var Key: Word;
  Shift: TShiftState);
  function AllSelected: boolean;
  begin
    result := (Editor.SelLength>0) and (Editor.SelLength=UTF8Length(Editor.Text));
  end;
  function AtStart: Boolean;
  begin
    Result:= (Editor.SelStart=0);
  end;
  function AtEnd: Boolean;
  begin
    result := ((Editor.SelStart+1)>UTF8Length(Editor.Text)) or AllSelected;
  end;
  procedure doEditorKeyDown;
  begin
    if FGrid<>nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;
  procedure doGridKeyDown;
  begin
    if FGrid<>nil then
      THackGrid(FGrid).KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := THackGrid(FGrid).FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or THackGrid(FGrid).EditorIsReadOnly then
      Key := 0;
  end;
begin
  case Key of
    VK_F2:
      if AllSelected then begin
        Editor.SelLength := 0;
        Editor.SelStart := Length(Editor.Text);
      end;
    VK_DELETE, VK_BACK:
      CheckEditingKey;
    VK_UP, VK_DOWN:
      doGridKeyDown;
    VK_END, VK_HOME:
      ;
    VK_ESCAPE:
      begin
        Value := FOldValue;
        doGridKeyDown;
        THackGrid(FGrid).EditorHide;
      end;
    VK_RETURN:
      begin
        doGridKeyDown;
        THackGrid(FGrid).EditorHide;
      end;
    else
      doEditorKeyDown;
  end;
  if OnKeyDown <> nil then OnKeyDown(Self, Key, Shift);
end;

procedure TTimeIntervalEdit.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if OnKeyPress <> nil then OnKeyPress(Self, Key);
end;

procedure TTimeIntervalEdit.EndTKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_LEFT) and (FEndT.SelStart = 0) then
  begin
    FBeginT.SetFocus;
    FBeginT.SelLength := 0;
    FBeginT.SelStart := Length(FBeginT.Text);
    Key := 0;
  end;
  EditorKeyDown(EndT, Key, Shift);
end;

procedure TTimeIntervalEdit.SetValue(AValue: String);
var
  S: String;
  i: SizeInt;
begin
  FOldValue := AValue;
  S := AValue;
  i := Pos(' .. ', S);
  FBeginT.Text := Copy(S, 1, i - 1);
  FEndT.Text := Copy(S, i + 4, 255);
end;

procedure TTimeIntervalEdit.BeginTKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RIGHT) and (FBeginT.SelStart = Length(FBeginT.Text)) then
  begin
  	FEndT.SetFocus;
    FEndT.SelLength := 0;
    Key := 0;
  end;
  EditorKeyDown(BeginT, Key, Shift);
end;

procedure TTimeIntervalEdit.EditorChange(Sender: TObject);
begin
  if FOnChange <> nil then FOnChange(Self);
end;

procedure TTimeIntervalEdit.DoOnResize;
begin
  inherited DoOnResize;
  FBeginT.Width := ClientWidth div 2  - 2;
  FEndT.Left := ClientWidth div 2 + 2;
  FEndT.Width := FBeginT.Width;
  Height := FBeginT.Height;
end;

procedure TTimeIntervalEdit.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TTimeIntervalEdit.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TTimeIntervalEdit.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TTimeIntervalEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBeginT := TTimeEditEx.Create(Self);
  FBeginT.Parent := Self;
  FBeginT.OnEditingDone:=@EditorEditingDone;
  FBeginT.OnKeyDown:=@BeginTKeyDown;
  FBeginT.OnKeyPress:=@EditorKeyPress;
  FBeginT.OnChange:=@EditorChange;
  FEndT := TTimeEditEx.Create(Self);
  FEndT.Parent := Self;
  FEndT.OnEditingDone:=@EditorEditingDone;
  FEndT.OnKeyDown:=@EndTKeyDown;
  FEndT.OnKeyPress:=@EditorKeyPress;
  FEndT.OnChange:=@EditorChange;
  ClientHeight := FEndT.Height;
end;

procedure TTimeIntervalEdit.SetFocus;
begin
  inherited SetFocus;
  FBeginT.SetFocus;
end;

function TTimeIntervalEdit.Focused: Boolean;
begin
  Result:=FBeginT.Focused or FEndT.Focused;
end;

function TTimeIntervalEdit.Validate: Boolean;
begin
  Result := ((FBeginT.Text = '') or FBeginT.Validate) and
    ((FEndT.Text = '') or FEndT.Validate);
end;

procedure TTimeIntervalEdit.Clear;
begin
  BeginT.Text := '';
  EndT.Text := '';
end;

function TTimeIntervalEdit.GetValue: String;
var
  S1, S2: String;
begin
  S1 := '';
  S2 := '';
  if (BeginT.Text <> '') or (EndT.Text <> '') then
  begin
    if (BeginT.Text <> '') and not BeginT.Validate then BeginT.Undo;
    if (EndT.Text <> '') and not EndT.Validate then EndT.Undo;
    S1 := BeginT.Text;
    S2 := EndT.Text;
  end;

  if (S1 <> '') or (S2 <> '') then
    Result := S1 + ' .. ' + S2
  else
    Result := '';
end;

{ TFilterDate }

procedure TFilterDate.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipboard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
  end;
end;

procedure TFilterDate.MenuPopup(Sender: TObject);
begin
  PopupMenu.Items[0].Enabled := SelText <> '';
  PopupMenu.Items[1].Enabled := SelText <> '';
end;

procedure TFilterDate.DoEnter;
begin
  inherited DoEnter;
  FOldText := Text;
end;

procedure TFilterDate.ButtonClick;
begin
  if CanFocus then SetFocus;
  inherited ButtonClick;
end;

constructor TFilterDate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Button.LoadGlyphFromLazarusResource('date16');
  Flat := True;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuHandler, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuHandler, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuHandler, IMG16_PASTE) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

function TFilterDate.Validate: Boolean;
var
  D: TDateTime;
begin
  Result := TryStrToDate(StringReplace(Text, ' ', DefaultFormatSettings.DateSeparator,
    [rfReplaceAll]), D);
  if Result then
    Text := DateToStr(D);
end;

procedure TFilterDate.Undo;
begin
  Text := FOldText;
end;

{ TFilterPeriod }

procedure TFilterPeriod.EditorEditingDone(Sender: TObject);
begin
  if (FBegin.Text <> FBeginText) or (FEnd.Text <> FEndText) then FPeriod := ptNone;
  EditingDone;
end;

procedure TFilterPeriod.SetValue(AValue: String);
var
  S: String;
  i: SizeInt;
begin
  FOldValue := AValue;
  S := AValue;
  i := Pos(' .. ', S);
  BeginP.Text := Copy(S, 1, i - 1);
  EndP.Text := Copy(S, i + 4, 255);
end;

procedure TFilterPeriod.MenuPopup(Sender: TObject);
begin
  FPopup.Items[2].Checked := FPeriod = ptToday;
  FPopup.Items[3].Checked := FPeriod = ptThisWeek;
  FPopup.Items[4].Checked := FPeriod = ptThisMonth;
  FPopup.Items[5].Checked := FPeriod = ptThisYear;
end;

procedure TFilterPeriod.EditorKeyDown(Editor: TFilterDate; var Key: Word;
  Shift: TShiftState);
  function AllSelected: boolean;
  begin
    result := (Editor.SelLength>0) and (Editor.SelLength=UTF8Length(Editor.Text));
  end;
  function AtStart: Boolean;
  begin
    Result:= (Editor.SelStart=0);
  end;
  function AtEnd: Boolean;
  begin
    result := ((Editor.SelStart+1)>UTF8Length(Editor.Text)) or AllSelected;
  end;
  procedure doEditorKeyDown;
  begin
    if FGrid<>nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;
  procedure doGridKeyDown;
  begin
    if FGrid<>nil then
      THackGrid(FGrid).KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := THackGrid(FGrid).FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or THackGrid(FGrid).EditorIsReadOnly then
      Key := 0;
  end;
begin
  case Key of
    VK_F2:
      if AllSelected then begin
        Editor.SelLength := 0;
        Editor.SelStart := Length(Editor.Text);
      end;
    VK_DELETE, VK_BACK:
      CheckEditingKey;
    VK_UP, VK_DOWN:
      doGridKeyDown;
    VK_END, VK_HOME:
      ;
    VK_ESCAPE:
      begin
        Value := FOldValue;
        doGridKeyDown;
        THackGrid(FGrid).EditorHide;
      end;
    VK_RETURN:
      begin
        doGridKeyDown;
        THackGrid(FGrid).EditorHide;
      end;
    else
      doEditorKeyDown;
  end;
  if OnKeyDown <> nil then OnKeyDown(Self, Key, Shift);
end;

procedure TFilterPeriod.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0:
      begin
        FBegin.Text := '';
        FEnd.Text := '';
      end;
    1: Period := ptToday;
    2: Period := ptThisWeek;
    3: Period := ptThisMonth;
    4: Period := ptThisYear;
  end;
end;

procedure TFilterPeriod.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if OnKeyPress <> nil then OnKeyPress(Self, Key);
end;

procedure TFilterPeriod.EndKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_LEFT) and (FEnd.SelStart = 0) then
  begin
    FBegin.SetFocus;
    FBegin.SelLength := 0;
    FBegin.SelStart := Length(FBegin.Text);
    Key := 0;
  end
  else
    EditorKeyDown(FBegin, Key, Shift);
end;

procedure TFilterPeriod.BeginKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RIGHT) and (FBegin.SelStart = Length(FBegin.Text)) then
  begin
  	FEnd.SetFocus;
    FEnd.SelLength := 0;
    Key := 0;
  end
  else
    EditorKeyDown(FBegin, Key, Shift);
end;

procedure TFilterPeriod.EditorChange(Sender: TObject);
begin
  if FOnChange <> nil then FOnChange(Self);
end;

procedure TFilterPeriod.SetPeriod(AValue: TPeriodType);
begin
  FPeriod:=AValue;
  if AValue <> ptNone then FEnd.Date := Date;
  case AValue of
    ptToday: FBegin.Date := Date;
    ptThisWeek: FBegin.Date := IncDay(Date, -DayOfTheWeek(Date)+1);
    ptThisMonth: FBegin.Date := IncDay(Date, -DayOf(Date)+1);
    ptThisYear: FBegin.Date := EncodeDate(YearOf(Date), 1, 1);
  end;
  FBeginText := FBegin.Text;
  FEndText := FEnd.Text;
end;

procedure TFilterPeriod.DoOnResize;
begin
  inherited DoOnResize;
  FBegin.Width := ClientWidth div 2  - 2;
  FEnd.Left := ClientWidth div 2 + 2;
  FEnd.Width := FBegin.Width;
  Height := FBegin.Height;
end;

procedure TFilterPeriod.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TFilterPeriod.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TFilterPeriod.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TFilterPeriod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopup := TPopupMenu.Create(Self);
  FPopup.Images := Images16;
  FPopup.Items.Add( CreateMenuItem(FPopup, rsClear, 0, 0, @MenuClick, IMG16_DELETE) );
  FPopup.Items.Add( CreateMenuItem(FPopup, '-', 0, 0, nil) );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsToday, 1, 0, @MenuClick) );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsThisWeek, 2, 0, @MenuClick) );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsThisMonth, 3, 0, @MenuClick) );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsThisYear, 4, 0, @MenuClick) );
  FPopup.OnPopup:=@MenuPopup;
  PopupMenu := FPopup;
  Caption := '';
  FBegin := TFilterDate.Create(Self);
  FBegin.Parent := Self;
  FBegin.OnEditingDone:=@EditorEditingDone;
  FBegin.OnKeyDown:=@BeginKeyDown;
  FBegin.OnKeyPress:=@EditorKeyPress;
  FBegin.OnChange:=@EditorChange;
  FEnd := TFilterDate.Create(Self);
  FEnd.Parent := Self;
  FEnd.OnEditingDone:=@EditorEditingDone;
  FEnd.OnKeyDown:=@EndKeyDown;
  FEnd.OnKeyPress:=@EditorKeyPress;
  FEnd.OnChange:=@EditorChange;
  ClientHeight := FEnd.Height;
end;

procedure TFilterPeriod.SetFocus;
begin
  inherited SetFocus;
  FBegin.SetFocus;
end;

function TFilterPeriod.Validate: Boolean;
begin
  Result := ((FBegin.Text = '') or FBegin.Validate) and
    ((FEnd.Text = '') or FEnd.Validate);
end;

function TFilterPeriod.Focused: Boolean;
begin
  Result := FBegin.Focused or FEnd.Focused;
end;

procedure TFilterPeriod.Clear;
begin
  BeginP.Text := '';
  EndP.Text := '';
end;

function TFilterPeriod.GetValue: String;
var
  S1, S2: String;
begin
  S1 := '';
  S2 := '';
  if (BeginP.Text <> '') or (EndP.Text <> '') then
  begin
    if (BeginP.Text <> '') and not BeginP.Validate then BeginP.Undo;
    if (EndP.Text <> '') and not EndP.Validate then EndP.Undo;
    S1 := BeginP.Text;
    S2 := EndP.Text;
  end;

  if (S1 <> '') or (S2 <> '') then
    Result := S1 + ' .. ' + S2
  else
    Result := '';
end;

{ TFilterEdit }

procedure TFilterEdit.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipboard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
  end;
end;

procedure TFilterEdit.MenuPopup(Sender: TObject);
begin
  PopupMenu.Items[0].Enabled := SelText <> '';
  PopupMenu.Items[1].Enabled := SelText <> '';
end;

procedure TFilterEdit.KeyPress(var Key: char);
begin
  if FNumeric then
  begin
    if (Key < #32) or (Key in ['0'..'9', ',', '-', '+']) then
    else Key := #0;
  end;

  inherited KeyPress(Key);
end;

procedure TFilterEdit.DoEnter;
begin
  inherited DoEnter;
  FOldText := Text;
end;

constructor TFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuHandler, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuHandler, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuHandler, IMG16_PASTE) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

function TFilterEdit.Validate: Boolean;
begin
  Result := True;
  if FNumeric then
    Result := CheckFloatRange(Text) = '';
end;

procedure TFilterEdit.Undo;
begin
  Text := FOldText;
end;

{ TFilterRange }

procedure TFilterRange.EditorEditingDone(Sender: TObject);
begin
  with TFilterEdit(Sender) do
    if (Text <> '') and not Validate then Undo;
  EditingDone;
end;

procedure TFilterRange.EditorKeyDown(Editor: TFilterEdit; var Key: Word;
  Shift: TShiftState);
  function AllSelected: boolean;
  begin
    result := (Editor.SelLength>0) and (Editor.SelLength=UTF8Length(Editor.Text));
  end;
  function AtStart: Boolean;
  begin
    Result:= (Editor.SelStart=0);
  end;
  function AtEnd: Boolean;
  begin
    result := ((Editor.SelStart+1)>UTF8Length(Editor.Text)) or AllSelected;
  end;
  procedure doEditorKeyDown;
  begin
    if FGrid<>nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;
  procedure doGridKeyDown;
  begin
    if FGrid<>nil then
      THackGrid(FGrid).KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := THackGrid(FGrid).FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or THackGrid(FGrid).EditorIsReadOnly then
      Key := 0;
  end;
begin
  case Key of
    VK_F2:
      if AllSelected then begin
        Editor.SelLength := 0;
        Editor.SelStart := Length(Editor.Text);
      end;
    VK_DELETE, VK_BACK:
      CheckEditingKey;
    VK_UP, VK_DOWN:
      doGridKeyDown;
    VK_END, VK_HOME:
      ;
    VK_ESCAPE:
      begin
        Value := FOldValue;
        doGridKeyDown;
        THackGrid(FGrid).EditorHide;
      end;
    VK_RETURN:
      begin
        doGridKeyDown;
        THackGrid(FGrid).EditorHide;
      end;
    else
      doEditorKeyDown;
  end;
  if OnKeyDown <> nil then OnKeyDown(Self, Key, Shift);
end;

procedure TFilterRange.EndKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_LEFT) and (FEnd.SelStart = 0) then
  begin
    FBegin.SetFocus;
    FBegin.SelLength := 0;
    FBegin.SelStart := Length(FBegin.Text);
    Key := 0;
  end
  else
    EditorKeyDown(FEnd, Key, Shift);
end;

procedure TFilterRange.BeginKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RIGHT) and (FBegin.SelStart = Length(FBegin.Text)) then
  begin
  	FEnd.SetFocus;
    FEnd.SelLength := 0;
    Key := 0;
  end
  else
    EditorKeyDown(FBegin, Key, Shift);
end;

procedure TFilterRange.EditorChange(Sender: TObject);
begin
  if (FGrid<>nil) and Visible then begin
    THackGrid(FGrid).EditorTextChanged(FCol, FRow, Text);
  end;
  if FOnChange <> nil then FOnChange(Self);
end;

procedure TFilterRange.DoOnResize;
begin
  inherited DoOnResize;
  FBegin.Width := ClientWidth div 2 - 2;
  FEnd.Left := ClientWidth div 2 + 2;
  FEnd.Width := FBegin.Width;
  Height := FBegin.Height;
end;

procedure TFilterRange.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TFilterRange.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TFilterRange.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TFilterRange.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  FBegin := TFilterEdit.Create(Self);
  FBegin.Parent := Self;
  FBegin.Numeric:=True;
  FBegin.OnKeyDown:=@BeginKeyDown;
  FBegin.OnChange:=@EditorChange;
  FBegin.OnEditingDone:=@EditorEditingDone;
  FEnd := TFilterEdit.Create(Self);
  FEnd.Parent := Self;
  FEnd.Numeric:=True;
  FEnd.OnKeyDown:=@EndKeyDown;
  FEnd.OnChange:=@EditorChange;
  FEnd.OnEditingDone:=@EditorEditingDone;
  ClientHeight := FEnd.Height;
end;

procedure TFilterRange.SetFocus;
begin
  inherited SetFocus;
  FBegin.SetFocus;
end;

function TFilterRange.Validate: Boolean;
begin
  Result := ((FBegin.Text = '') or FBegin.Validate) and
    ((FEnd.Text = '') or FEnd.Validate);
end;

function TFilterRange.Focused: Boolean;
begin
  Result := FBegin.Focused or FEnd.Focused;
end;

procedure TFilterRange.Clear;
begin
  BeginR.Text := '';
  EndR.Text := '';
end;

function TFilterRange.GetValue: String;
var
  S1, S2: String;
begin
  S1 := '';
  S2 := '';
  if (BeginR.Text <> '') or (EndR.Text <> '') then
  begin
    if (BeginR.Text <> '') and not BeginR.Validate then BeginR.Undo;
    if (EndR.Text <> '') and not EndR.Validate then EndR.Undo;
    S1 := BeginR.Text;
    S2 := EndR.Text;
  end;

  if (S1 <> '') or (S2 <> '') then
    Result := S1 + ' .. ' + S2
  else
    Result := '';
end;

procedure TFilterRange.SetValue(AValue: String);
var
  S: String;
  i: SizeInt;
begin
  FOldValue := AValue;
  S := AValue;
  i := Pos(' .. ', S);
  BeginR.Text := Copy(S, 1, i - 1);
  EndR.Text := Copy(S, i + 4, 255);
end;

{ TFilterComboBox }

function TFilterComboBox.GetKey: Integer;
begin
  Result := 0;
  if ItemIndex >= 0 then
  	Result := PtrInt(Items.Objects[ItemIndex]);
end;

procedure TFilterComboBox.SetKey(AValue: Integer);
begin
  ItemIndex := Items.IndexOfObject(TObject(PtrInt(AValue)));
end;

procedure TFilterComboBox.DoEnter;
begin
  inherited DoEnter;
  if IsObject and (Text <> '') then
    FOldItemIndex := Items.IndexOf(Text)
  else
    FOldItemIndex := -1;
end;

procedure TFilterComboBox.KeyDown(var Key: Word; Shift: TShiftState);
  function AllSelected: boolean;
  begin
    result := (SelLength>0) and (SelLength=Length(Text));
  end;
  function AtStart: Boolean;
  begin
    Result:= (SelStart=0);
  end;
  function AtEnd: Boolean;
  begin
    result := ((SelStart+1)>Length(Text)) or AllSelected;
  end;
  procedure doEditorKeyDown;
  begin
    if FGrid<>nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;
  procedure doGridKeyDown;
  begin
    if FGrid<>nil then
      THackGrid(FGrid).KeyDown(Key, shift);
  end;
  function GetFastEntry: boolean;
  begin
    if FGrid<>nil then
      Result := THackGrid(FGrid).FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    // if editor is not readonly, start editing
    // else not interested
    if (FGrid=nil) or THackGrid(FGrid).EditorIsReadOnly then
      Key := 0;
  end;
var
  IntSel: boolean;
begin
  inherited KeyDown(Key,Shift);
  case Key of

    VK_F2:
      if AllSelected then begin
        SelLength := 0;
        SelStart := Length(Text);
      end;

    VK_RETURN:
      if DroppedDown then begin
        CheckEditingKey;
        DroppedDown := False;
        if Key<>0 then begin
          doEditorKeyDown;
          Key:=0;
        end;
      end else
        doEditorKeyDown;

    VK_DELETE:
      CheckEditingKey;

    VK_UP:
      if not DroppedDown then
        doGridKeyDown;

    VK_DOWN:
      if not DroppedDown then
        DroppedDown := True;

    VK_LEFT, VK_RIGHT:
      if GetFastEntry then begin
        IntSel:=
          ((Key=VK_LEFT) and not AtStart) or
          ((Key=VK_RIGHT) and not AtEnd);
        if not IntSel then begin
            doGridKeyDown;
      end;
    end;

    VK_END, VK_HOME:
      ;
    VK_ESCAPE:
      begin
        if IsObject then ItemIndex := FOldItemIndex;
        doGridKeyDown;
        THackGrid(FGrid).EditorHide;
      end;
    else
      if not DroppedDown then
        doEditorKeyDown;
  end;
end;

procedure TFilterComboBox.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
  PopupMenu := FGrid.PopupMenu;
end;

procedure TFilterComboBox.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TFilterComboBox.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

constructor TFilterComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  AutoComplete := True;
  // Блокируем системное меню
  PopupMenu := TPopupMenu.Create(Self);
end;

end.

