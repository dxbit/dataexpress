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
unit FilterControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, strconsts, StdCtrls, dxctrls, myclasses,
  Controls, DXReports, Menus, Graphics, myctrls, ComCtrls, timeedit, Dialogs;

type

  { TFilterControl }

  TFilterControl = class(TStringGrid)
    procedure ButtonClick(Sender: TObject);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
  private
    FAllowAddFields: Boolean;
    FButtons: TToolBar;
    FEdit: TFilterEdit;
    FNumEdit: TFilterRange;
    FDateEdit: TFilterPeriod;
    FBoolEdit: TFilterComboBox;
    FObjEdit: TFilterComboBox;
    FListEdit: TFilterComboBox;
    FTimeEdit: TTimeIntervalEdit;
    FPopup: TPopupMenu;
    procedure MenuClick(Sender: TObject);
    procedure EditorEditingDone(Sender: TObject);
    function InitEdit: TWinControl;
    function InitNumEdit: TWinControl;
    function InitDateEdit: TWinControl;
    function InitBoolEdit: TWinControl;
    function InitListEdit(Obj: TObject): TWinControl;
    function InitObjEdit(Obj: TObject): TWinControl;
    function InitTimeEdit(Obj: TObject): TWinControl;
    procedure PickListSelect(Sender: TObject);
    procedure SetAllowAddFields(AValue: Boolean);
    procedure AddField;
    procedure DeleteField;
    procedure AddValue;
    procedure DeleteValue;
    procedure ClearValue;
    procedure ClearAllValues;
    procedure SetButtons(AValue: TToolBar);
    procedure SetControlState;
  protected
    procedure SelectEditor; override;
  protected
    procedure FillFields(L: TStrings); virtual;
    function LookupField(Obj: TObject; const aFieldName: String): TObject; virtual;
    function GetValues(i: Integer): String;
    procedure SetValues(i: Integer; SL: TStringList);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init; virtual;
    procedure Save; virtual;
    procedure Load; virtual;
    procedure Clear; virtual;
    property AllowAddFields: Boolean read FAllowAddFields write SetAllowAddFields;
    property Buttons: TToolBar read FButtons write SetButtons;
  end;

  { TRpParamsControl }

  TRpParamsControl = class(TFilterControl)
  private
    FRD: TReportData;
  protected
    function LookupField(Obj: TObject; const aFieldName: String): TObject; override;
    procedure SelectEditor; override;
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
  public
    procedure Load; override;
    procedure Save; override;
    property Form: TdxForm read FForm write FForm;
    property Filter: TFilterObject read FFilter write FFilter;
  end;

implementation

uses
  sqlgen, dbengine, apputils, formmanager, strutils, DateUtils, mytypes;

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
    if (C is TdxEdit) or (C is TdxCalcEdit) or (C is TdxDateEdit) or
      (C is TdxCheckBox) or (C is TdxComboBox) or (C is TdxLookupComboBox) or
      (C is TdxTimeEdit) or (C is TdxCounter) or (C is TdxMemo) or
      ((C is TdxObjectField) and (FForm.PId = 0)) then
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
  {if (Result <> nil) and (Result is TdxLookupComboBox) then
    Result := LookupComponent(TdxForm(TComponent(Result).Owner), aFieldName);}
end;

procedure TRpParamsControl.SelectEditor;
begin
  inherited SelectEditor;
  if Col = 0 then Editor := nil;
end;

constructor TRpParamsControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AllowAddFields:=False;
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

procedure TFilterControl.PickListSelect(Sender: TObject);
var
  i: Integer;
begin
  if Col = 0 then
    with TPickListCellEditor(Editor) do
    begin
      if Objects[Col, Row] <> Items.Objects[ItemIndex] then
      begin
        Objects[Col, Row] := Items.Objects[ItemIndex];
        for i := 3 to Columns.Count - 1 do
        begin
          Cells[i, Row] := '';
          Objects[i, Row] := nil;
        end;
      end;
    end;
end;

procedure TFilterControl.SetAllowAddFields(AValue: Boolean);
begin
  FAllowAddFields:=AValue;
  FPopup.Items[0].Visible:=AValue;
  FPopup.Items[1].Visible:=AValue;
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
  Row := r;
  SetControlState;
end;

procedure TFilterControl.DeleteField;
begin
  DeleteRow(Row);
  SetControlState;
end;

procedure TFilterControl.AddValue;
begin
  with Columns.Add do
  begin
    Title.Caption := rsValue;
    Width := 200;
  end;
  SetControlState;
end;

procedure TFilterControl.DeleteValue;
begin
  if (Col >= 3) and (ColCount > 4) then DeleteCol(Col);
  SetControlState;
end;

procedure TFilterControl.ClearValue;
begin
  Cells[Col, Row] := '';
  Objects[Col, Row] := nil;
end;

procedure TFilterControl.ClearAllValues;
var
  i, j: Integer;
begin
  if MessageDlg(rsWarning, rsAreYouSure, mtWarning, [mbYes, mbNo], 0) = mrNo then Exit;
  for i := 1 to RowCount - 1 do
    for j := 3 to ColCount - 1 do
    begin
      Cells[j, i] := '';
      Objects[j, i] := nil;
    end;
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
  FPopup.Items[1].Enabled:=RowCount > 1;
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

procedure TFilterControl.EditorEditingDone(Sender: TObject);
begin
  if Sender = FEdit then
  begin
    Cells[Col, Row] := FEdit.Text;
  end
  else if (Sender = FNumEdit) and FNumEdit.Validate then
  begin
    if (FNumEdit.BeginR.Text <> '') or (FNumEdit.EndR.Text <> '') then
      Cells[Col, Row] := FNumEdit.BeginR.Text + ' .. ' +
        FNumEdit.EndR.Text
    else
      Cells[Col, Row] := '';
  end
  else if (Sender = FDateEdit) and FDateEdit.Validate then
  begin
    if (FDateEdit.BeginP.Text <> '') or (FDateEdit.EndP.Text <> '') then
    begin
      Cells[Col, Row] := FDateEdit.BeginP.Text + ' .. ' +
        FDateEdit.EndP.Text;
      Objects[Col, Row] := TObject(PtrInt(FDateEdit.Period));
    end
    else
    begin
      Cells[Col, Row] := '';
      Objects[Col, Row] := nil;
    end;
  end
  else if Sender = FBoolEdit then
  begin
    Cells[Col, Row] := FBoolEdit.Text;
    Objects[Col, Row] := FBoolEdit.Items.Objects[FBoolEdit.ItemIndex];
  end
  else if Sender = FListEdit then
  begin
    Cells[Col, Row] := FListEdit.Text;
  end
  else if Sender = FObjEdit then
  begin
    if FObjEdit.Key > 0 then
    begin
	    Cells[Col, Row] := FObjEdit.Text;
  	  Objects[Col, Row] := TObject(FObjEdit.Key);
    end
    else
    begin
      Cells[Col, Row] := '';
      Objects[Col, Row] := nil;
    end;
  end
  else if (Sender = FTimeEdit) and FTimeEdit.Validate then
  begin
    if (FTimeEdit.BeginT.Text <> '') or (FTimeEdit.EndT.Text <> '') then
      Cells[Col, Row] := FTimeEdit.BeginT.Text + ' .. ' +
        FTimeEdit.EndT.Text
    else
      Cells[Col, Row] := '';
  end;
end;

function TFilterControl.InitEdit: TWinControl;
begin
  FEdit.BoundsRect := CellRect(Col, Row);
  FEdit.Text:=Cells[Col, Row];
  Result := FEdit;
end;

function TFilterControl.InitNumEdit: TWinControl;
var
  S: String;
  i: SizeInt;
begin
  FNumEdit.BoundsRect := CellRect(Col, Row);
  S := Cells[Col, Row];
  i := Pos(' .. ', S);
  FNumEdit.BeginR.Text := Copy(S, 1, i - 1);
  FNumEdit.EndR.Text := Copy(S, i + 4, 255);
  Result := FNumEdit;
end;

function TFilterControl.InitDateEdit: TWinControl;
var
  S: String;
  i: SizeInt;
begin
  FDateEdit.BoundsRect := CellRect(Col, Row);
  S := Cells[Col, Row];
  i := Pos(' .. ', S);
  FDateEdit.BeginP.Text := Copy(S, 1, i - 1);
  FDateEdit.EndP.Text := Copy(S, i + 4, 255);
  FDateEdit.Period:=TPeriodType(Objects[Col, Row]);
  Result := FDateEdit;
end;

function TFilterControl.InitBoolEdit: TWinControl;
begin
  FBoolEdit.BoundsRect := CellRect(Col, Row);
  with FBoolEdit do
    ItemIndex := Items.IndexOfObject( Objects[Col, Row] );
  Result := FBoolEdit;
end;

function TFilterControl.InitListEdit(Obj: TObject): TWinControl;
begin
  FListEdit.BoundsRect := CellRect(Col, Row);
  FListEdit.Items := TdxComboBox(Obj).Items;
  FListEdit.Text:=Cells[Col, Row];
  Result := FListEdit;
end;

function Abrakadabra(const Path: String): String;
var
  SL: TStringList;
begin
  Result := '';
  SL := TStringList.Create;
  SplitStr(Path, '\', SL);
  if SL.Count > 0 then
    Result := DupeString('  ', SL.Count - 1) + SL[SL.Count - 1];
  SL.Free;
end;

function TFilterControl.InitObjEdit(Obj: TObject): TWinControl;
var
  C: TdxLookupComboBox;
  SQL: String;
  Fm: TdxForm;
begin
  FObjEdit.Clear;
  C := TdxLookupComboBox(Obj);
  if (C.SourceTId > 0) and (C.SourceFId > 0) then
  begin
    Fm := FormMan.FindForm(C.SourceTId);
    FObjEdit.AddKey(0);
    FObjEdit.Items.Add('');
    SQL := SQLLookupSelect(C, '');
    with DBase.OpenDataSet(SQL) do
    try
      while not Eof do
      begin
        FObjEdit.AddKey(Fields[0].AsInteger);
        if Fm.ParentField = 0 then
          FObjEdit.Items.Add(Fields[1].AsString)
        else
          FObjEdit.Items.Add(Abrakadabra(Fields[1].AsString));
        Next;
      end;
    finally
      Free;
    end;
  end;

  FObjEdit.BoundsRect := CellRect(Col, Row);
  FObjEdit.Key:=Integer(Objects[Col, Row]);
  Result := FObjEdit;
end;

function TFilterControl.InitTimeEdit(Obj: TObject): TWinControl;
var
  S: String;
  i: SizeInt;
begin
  FTimeEdit.BoundsRect := CellRect(Col, Row);
  S := Cells[Col, Row];
  i := Pos(' .. ', S);
  FTimeEdit.BeginT.Text := Copy(S, 1, i - 1);
  FTimeEdit.EndT.Text := Copy(S, i + 4, 255);
  Result := FTimeEdit;
end;

procedure TFilterControl.SelectEditor;
var
  Obj: TComponent;
begin
  inherited SelectEditor;

  if (Col = 0) and (Editor <> nil) and (Editor is TPickListCellEditor) then
    TPickListCellEditor(Editor).Style:=csDropDownList
  else if Col >= 3 then
  begin
    Obj := TComponent(LookupField(Objects[0, Row], Cells[0, Row]));
    if Obj = nil then Editor := nil
    else if (Obj is TdxEdit) or (Obj is TdxMemo) then Editor := InitEdit
    else if (Obj is TdxCalcEdit) or (Obj is TdxCounter) then Editor := InitNumEdit
    else if Obj is TdxDateEdit then Editor := InitDateEdit
    else if Obj is TdxCheckBox then Editor := InitBoolEdit
    else if Obj is TdxComboBox then Editor := InitListEdit(Obj)
    else if Obj is TdxLookupComboBox then Editor := InitObjEdit(Obj)
    else if Obj is TdxTimeEdit then Editor := InitTimeEdit(Obj)
  end;
end;

procedure TFilterControl.FillFields(L: TStrings);
begin

end;

function TFilterControl.LookupField(Obj: TObject; const aFieldName: String
  ): TObject;
begin
  Result := Obj;
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
        S := IntToStr(Integer(Obj));
    end
    else if Cmp is TdxCheckBox then
    begin
      if Obj <> nil then
        S := IntToStr(Integer(Obj) - 1);
    end
    else if (Cmp is TdxDateEdit) and (Obj <> nil) then
      S := '$' + IntToStr(PtrInt(Obj));
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
          Cells[j, i] := GetObjFieldValue(Obj, StrToInt(S), False);
          Objects[j, i] := TObject(StrToInt(S));
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
          Cells[j, i] := PeriodToStr(TPeriodType(Objects[j, i]));
        end
        else
          Cells[j, i] := S;
      end;
    end;
  end;
end;

constructor TFilterControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEdit := TFilterEdit.Create(Self);
  FEdit.OnEditingDone:=@EditorEditingDone;
  FNumEdit := TFilterRange.Create(Self);
  FNumEdit.OnEditingDone:=@EditorEditingDone;
  FDateEdit := TFilterPeriod.Create(Self);
  FDateEdit.OnEditingDone:=@EditorEditingDone;
  FBoolEdit := TFilterComboBox.Create(Self);
  FBoolEdit.Items.AddObject('', nil);
  FBoolEdit.Items.AddObject(rsNo, TObject(1));
  FBoolEdit.Items.AddObject(rsYes, TObject(2));
  FBoolEdit.Style:=csDropDownList;
  FBoolEdit.OnEditingDone:=@EditorEditingDone;
  FObjEdit := TFilterComboBox.Create(Self);
  FObjEdit.Lookup:=True;
  //FObjEdit.Style := csDropDownList;
  FObjEdit.AutoComplete:=True;
  FObjEdit.OnEditingDone:=@EditorEditingDone;
  FListEdit := TFilterComboBox.Create(Self);
  FListEdit.AutoComplete:=True;
  FListEdit.OnEditingDone:=@EditorEditingDone;
  FTimeEdit := TTimeIntervalEdit.Create(Self);
  FTimeEdit.OnEditingDone:=@EditorEditingDone;

  FPopup := TPopupMenu.Create(Self);
  FPopup.Items.Add( CreateMenuItem(FPopup, rsAddField, 0, 0, @MenuClick, ''));
  FPopup.Items.Add( CreateMenuItem(FPopup, rsDeleteField, 1, 0, @MenuClick, ''));
  FPopup.Items.Add( CreateMenuItem(FPopup, '-', 2, 0, @MenuClick, ''));
  FPopup.Items.Add( CreateMenuItem(FPopup, rsAddValue, 3, 0, @MenuClick, ''));
  FPopup.Items.Add( CreateMenuItem(FPopup, rsDeleteValue, 4, 0, @MenuClick, ''));
  FPopup.Items.Add( CreateMenuItem(FPopup, '-', 5, 0, @MenuClick, ''));
  FPopup.Items.Add( CreateMenuItem(FPopup, rsClearValue, 6, 0, @MenuClick, ''));
  FPopup.Items.Add( CreateMenuItem(FPopup, rsClearAllValues, 7, 0, @MenuClick,
    ''));
  SetMenuItemImage(FPopup.Items[0], 'add16');
  SetMenuItemImage(FPopup.Items[1], 'delete16');
  SetMenuItemImage(FPopup.Items[3], 'add16');
  SetMenuItemImage(FPopup.Items[4], 'delete16');
  PopupMenu := FPopup;

  FixedCols := 0;
  RowCount:=1;
  AlternateColor:=$00F0F0F0;
  FocusColor:=clHighlight;
  SelectedColor:=clHighlight;

  Options := Options + [goEditing, goColSizing, goDrawFocusSelected] - [goRangeSelect, goVertLine, goHorzLine];
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
  OnPickListSelect:=@PickListSelect;
  OnSelection:=@GridSelection;
end;

procedure TFilterControl.Init;
begin
  FillFields(Columns[0].PickList);
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


end.

