unit QueryCalcForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, Menus, dxreports, strconsts, dxctrls,
  StdCtrls, DialogGrid;

type

  { TCalcFm }

  TCalcFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TDialogGrid;
    DialogGridButtons1: TDialogGridButtons;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure GridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure GridPickListSelect(Sender: TObject);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FForm: TdxForm;
    FRD: TReportData;
    FQGrid: TdxQueryGrid;
    FMaxFId: Integer;
    FOldFieldNames: TStringList;
    FInDesigner: Boolean;
    procedure InitCol3;
    procedure LoadFields;
    procedure SaveFields;
    function CheckNames: Boolean;
    function ProcessRenameFieldsInActions: Boolean;
    function Validate: Boolean;
  public
    { public declarations }
    function ShowForm(aRD: TReportData; aForm: TdxForm; QGrid: TdxQueryGrid;
      InDesigner: Boolean; ARow: Integer = 0): Integer;
  end;

var
  CalcFm: TCalcFm;

function ShowCalcForm(aRD: TReportData; aForm: TdxForm; QGrid: TdxQueryGrid;
  InDesigner: Boolean; ARow: Integer = 0): Integer;

implementation

uses
  exprform, apputils, helpmanager, LazUtf8;

function ShowCalcForm(aRD: TReportData; aForm: TdxForm; QGrid: TdxQueryGrid;
  InDesigner: Boolean; ARow: Integer): Integer;
begin
  if CalcFm = nil then
  	CalcFm := TCalcFm.Create(Application);
	Result := CalcFm.ShowForm(aRD, aForm, QGrid, InDesigner, ARow);
end;

{$R *.lfm}

{ TCalcFm }

procedure TCalcFm.FormCreate(Sender: TObject);
begin
  Grid.Columns[0].Title.Caption:=rsFieldName;
  Grid.Columns[1].Title.Caption:=rsFieldType;
  Grid.Columns[2].Title.Caption:=rsSizePrec;
  Grid.Columns[3].Title.Caption:=rsExpression;
  with Grid.Columns[1].PickList do
  begin
    AddObject(rsText, TObject(flText));
    AddObject(rsNumber, TObject(flNumber));
    AddObject(rsDate, TObject(flDate));
    AddObject(rsTime, TObject(flTime));
  end;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  FOldFieldNames := TStringList.Create;
end;

procedure TCalcFm.FormDestroy(Sender: TObject);
begin
  FOldFieldNames.Free;
end;

procedure TCalcFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
  begin
    CanClose := Validate;
    if not CanClose then Grid.SetFocus;
  end
  else
  begin
    if Grid.Modified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TCalcFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TCalcFm.GridButtonClick(Sender: TObject; aCol, aRow: Integer);
var
  S: String;
begin
  S := Grid.Cells[aCol, aRow];
  if ShowExprForm(etRpCalcField, FQGrid, S, FForm, nil, nil, FRD) = mrOk then
    Grid.Cells[aCol, aRow] := S;
end;

procedure TCalcFm.GridColRowInserted(Sender: TObject; IsColumn: Boolean;
  sIndex, tIndex: Integer);
begin
  if not IsColumn then
  begin
    Inc(FMaxFId);
    Grid.Objects[0, sIndex] := TObject(PtrInt(FMaxFId));
  end;
end;

procedure TCalcFm.GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
var
  r, i: Integer;
  Msg: String;
begin
  case Cmd of
    dgcAppend:
      begin
        r := Grid.RowCount;
        Grid.RowCount := r + 1;
        Grid.Row:=r;
        Inc(FMaxFId);
        Grid.Objects[0, r] := TObject(PtrInt(FMaxFId));
        Grid.Cells[2, r] := '-';
      end;
    dgcDelete:
      begin
        if ConfirmDelete then
        begin
          i := FOldFieldNames.IndexOfObject(Grid.Objects[0, Grid.Row]);
          if i >= 0 then
          begin
            if FOldFieldNames[i] <> Grid.Cells[0, Grid.Row] then
              Msg := LineEnding + rsShowOldFieldNameBeforeRename
            else
              Msg := '';
            if CheckExistsInActions(FRD, renRpField, FOldFieldNames[i], Msg) then Exit;
          end;
          Grid.DeleteRow(Grid.Row);
        end;
      end;
  end;
end;

procedure TCalcFm.GridPickListSelect(Sender: TObject);
begin
  if Grid.Col = 1 then InitCol3;
end;

procedure TCalcFm.GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
var
  Tp: TRpFieldType;
begin
  if aCol = 2 then
  begin
    Tp := TRpFieldType(PtrInt(Grid.Objects[1, aRow]));
    if Tp in [flNone, flDate, flTime] then Editor := nil
    else
    begin
      with TIntegerCellEditor(Editor) do
        if Tp = flNumber then
        begin
          MaxValue := 10;
          Increment := 1;
        end
        else
        begin
          MaxValue := 1000;
          Increment := 10;
        end;
    end;
  end;
end;

procedure TCalcFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('querycalcs');
end;

procedure TCalcFm.InitCol3;
var
  S: String;
begin
  S := Grid.Cells[Grid.Col, Grid.Row];
  if S = rsText then Grid.Cells[2, Grid.Row] := '50'
  else if S = rsNumber then Grid.Cells[2, Grid.Row] := '0'
  else Grid.Cells[2, Grid.Row] := '-';
end;

procedure TCalcFm.LoadFields;
var
  i, r: Integer;
  F: TRpCalcField;
begin
  FOldFieldNames.Clear;
  Grid.RowCount := 1;
  FMaxFId := 0;
  for i := 0 to FRD.CalcFields.Count - 1 do
  begin
    r := i + 1;
    Grid.RowCount := r + 1;
    F := FRD.CalcFields[i]^;
    Grid.Objects[0, r] := TObject(PtrInt(F.Id));
    Grid.Cells[0, r] := F.Name;
    Grid.Cells[1, r] := RpFieldTypeToStr(F.Tp);
    Grid.Objects[1, r] := TObject(PtrInt(F.Tp));
    if F.Tp in [flText, flNumber] then
	    Grid.Cells[2, r] := IntToStr(F.Size)
    else
    	Grid.Cells[2, r] := '-';
    Grid.Cells[3, r] := F.Expr;
    if F.Id > FMaxFId then FMaxFId := F.Id;
    FOldFieldNames.AddObject(F.Name, TObject(PtrInt(F.Id)));
  end;
end;

procedure TCalcFm.SaveFields;
var
  i, n: Integer;
  Col: TRpGridColumn;
  S: String;
  pF: PRpCalcField;

  (*function NewCol(const aFlNm: String): TRpGridColumn;
  begin
    Result := FRD.Grid.AddColumn;
    Result.FieldNameDS := aFlNm;
    Result.Index:=FRD.Grid.ColumnCount - 1;
    Result.Width := 100;
    Result.Color:=FRD.Grid.Color;
    Result.FixedColor:=FRD.Grid.FixedColor;
    Result.Font.Assign(FRD.Grid.Font);
    Result.TitleFont.Assign(FRD.Grid.TitleFont);
  end;

  procedure RemoveTotal(Col: TRpGridColumn);
  var
    T: TRpTotalData;
  begin
    repeat
      T := FRD.Totals.FindTotal(Col.FieldNameDS);
      if T <> nil then FRD.Totals.RemoveTotal(T);
    until T = nil;
  end;

  procedure RemoveColoring(Col: TRpGridColumn);
  var
    CD: TRpColoringData;
  begin
    repeat
      CD := FRD.Coloring.FindColoring(Col.FieldNameDS);
      if CD <> nil then FRD.Coloring.DeleteColoring(CD);
    until CD = nil;
  end;    *)

begin
  FRD.CalcFields.Clear;
  for i := 1 to Grid.RowCount - 1 do
  begin
    FRD.CalcFields.AddField(pF);
    pF^.Id := PtrInt(Grid.Objects[0, i]);
    pF^.Name := Grid.Cells[0, i];
    pF^.Tp := TRpFieldType(PtrInt(Grid.Objects[1, i]));
    pF^.Size := 0;
    if Grid.Cells[2, i] <> '-' then pF^.Size := StrToInt(Grid.Cells[2, i]);
    pF^.Expr := Grid.Cells[3, i];
  end;
  RemoveLostFieldsFromReportData(FRD);
  CreateOrUpdateReportGridColumns(FRD);
  (*// Удаляем лишние столбцы
  for i := FRD.Grid.ColumnCount - 1 downto 0 do
  begin
    Col := FRD.Grid.Columns[i];
    S := Col.FieldNameDS;
    if Copy(S, 1, 2) = 'cf' then
    begin
      n := StrToInt(Copy(S, 3, 50));
      if FRD.CalcFields.FindField(n) = nil then
      begin
        RemoveTotal(Col);
        RemoveColoring(Col);
        FRD.Grid.DeleteColumn(Col);
      end;
    end;
  end;
  // Добавляем новые и обновляем заголовки, если надо
  for i := 0 to FRD.CalcFields.Count - 1 do
  begin
    pF := FRD.CalcFields[i];
    S := 'cf' + IntToStr(pF^.Id);
    Col := FRD.Grid.FindColumnByFieldName(S);
    if Col = nil then
      Col := NewCol(S);
    Col.Caption:=pF^.Name;
  end;   *)
end;

function TCalcFm.CheckNames: Boolean;
var
  i: Integer;

  function _Check(idx: Integer): Boolean;
  var
    S, SS, CFName: String;
    j: Integer;
  begin
    Result := False;
    S := Grid.Cells[0, idx];
    if S = '' then
    begin
      ErrMsg(rsFieldNameEmpty);
      Exit;
    end
    else if not CheckFieldName(S) then
      Exit;
    for j := idx + 1 to Grid.RowCount - 1 do
    begin
      SS := Grid.Cells[0, j];
      if MyUtf8CompareText(S, SS) = 0 then
      begin
        ErrMsg(rsCalcFieldNameExists);
        Exit;
      end;
    end;
    for j := 0 to FRD.GetRpSQLFieldCount - 1 do
    begin
      if FRD.GetFieldVisible(j) then
      begin
        CFName := 'cf' + IntToStr(PtrInt(Grid.Objects[0, idx]));
        if MyUtf8CompareText(FRD.GetFieldName(j), S) = 0 then
        begin
          ErrMsg(rsFieldNameExists);
          Exit;
        end
        else if CompareText(FRD.GetFieldNameDS(j), CFName) = 0 then
        begin
          ErrMsg(rsSqlFieldNameExists);
          Exit;
        end;
      end;
    end;

    Result := True;
  end;

begin
  for i := 1 to Grid.RowCount - 1 do
  begin
    if not _Check(i) then
    begin
      Grid.Row := i;
      Grid.Col := 0;
      Exit(False);
    end;
  end;
  Result := True;
end;

function TCalcFm.ProcessRenameFieldsInActions: Boolean;
var
  i, idx: Integer;
begin
  Result := True;
  for i := 1 to Grid.RowCount - 1 do
  begin
    idx := FOldFieldNames.IndexOfObject(Grid.Objects[0, i]);
    if (idx >= 0) and (FOldFieldNames[idx] <> Grid.Cells[0, i]) then
    begin
      if FInDesigner then
        RenameInActions(FRD, renRpField, FOldFieldNames[idx], Grid.Cells[0, i])
      else
      begin
        Grid.Col := 0; Grid.Row := i;
        if CheckExistsInActions(FRD, renRpField, FOldFieldNames[idx], Lineending +
          rsShowOldFieldNameBeforeRename) then Exit(False);
      end;
    end;
  end;
end;

function TCalcFm.Validate: Boolean;
var
  i: Integer;
  Tp: TRpFieldType;
begin
  Result := False;
  if not CheckNames then Exit;

  for i := 1 to Grid.RowCount - 1 do
  begin
    Tp := TRpFieldType(PtrInt(Grid.Objects[1, i]));
    if Tp = flNone then
    begin
      ErrMsg(rsSelectFieldType);
      Grid.Col := 1; Grid.Row := i;
      Exit;
    end
    else if Tp = flText then
    begin
      if Grid.Cells[2, i] = '' then
      begin
        ErrMsg(rsEnterFieldSize);
        Grid.Col := 2; Grid.Row := i;
	      Exit;
      end
      else if StrToInt(Grid.Cells[2, i]) > 1000 then
      begin
        ErrMsg(rsFieldSizeMustNotExceed);
        Grid.Col := 2; Grid.Row := i;
	      Exit;
      end;
    end
    else if Tp = flNumber then
    begin
      if Grid.Cells[2, i] = '' then
      begin
        ErrMsg(rsEnterPrecisionNum);
        Grid.Col := 2; Grid.Row := i;
	      Exit;
      end
      else if StrToInt(Grid.Cells[2, i]) > 10 then
      begin
        ErrMsg(rsPrecisionMustNotExceed);
        Grid.Col := 2; Grid.Row := i;
	      Exit;
      end;
    end;
  end;
  Result := ProcessRenameFieldsInActions;
end;

function TCalcFm.ShowForm(aRD: TReportData; aForm: TdxForm;
  QGrid: TdxQueryGrid; InDesigner: Boolean; ARow: Integer): Integer;
var
  pCF: PRpCalcField;
begin
  FInDesigner := InDesigner;
  Result := mrNone;
  if aRD.IsEmpty then
  begin
    ErrMsg(rsSourceNotSel);
    Exit;
  end;
  FRD := aRD;
  Caption := rsCalcFields + ': ' + FRD.Name;
  FForm := aForm;
  FQGrid := QGrid;
  LoadFields;
  Grid.Modified:=False;
  Grid.ClearSelections;
  if ARow > 0 then
  begin
    Grid.Row := ARow;
    Grid.Col := 0;
  end;
  Result := ShowModal;
  if Result = mrOk then SaveFields;
end;

end.

