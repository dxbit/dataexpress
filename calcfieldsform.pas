unit CalcFieldsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ButtonPanel, Menus, strconsts, dxctrls, ExtCtrls, Buttons, ComCtrls,
  DialogGrid;

type

  { TCalcFieldsFm }

  TCalcFieldsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TDialogGrid;
    DialogGridButtons1: TDialogGridButtons;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure HelpButtonClick(Sender: TObject);
  private
    FForm: TdxForm;
    FHelpIndex: String;
    FIsReport: Boolean;
    procedure FillFields;
    procedure SaveFields;
    function CheckFieldNames(var aRow: Integer): Boolean;
    { private declarations }
  public
    { public declarations }
    function ShowForm(Fm: TdxForm; ARow: Integer = 0): Integer;
    property IsReport: Boolean read FIsReport write FIsReport;
    property HelpIndex: String read FHelpIndex write FHelpIndex;
  end;

var
  CalcFieldsFm: TCalcFieldsFm;
  ReportPrintFieldsFm: TCalcFieldsFm;

function ShowCalcFieldsForm(Fm: TdxForm; ARow: Integer = 0): Integer;
function ShowReportPrintFieldsForm(Fm: TdxForm; ARow: Integer = 0): Integer;

implementation

uses
  apputils, exprform, helpmanager, LazUtf8;

function ShowCalcFieldsForm(Fm: TdxForm; ARow: Integer): Integer;
begin
  if CalcFieldsFm = nil then
  begin
  	CalcFieldsFm := TCalcFieldsFm.Create(Application);
    CalcFieldsFm.Caption := rsCalcFields;
    CalcFieldsFm.HelpIndex := 'calcfields';
  end;
  Result := CalcFieldsFm.ShowForm(Fm, ARow);
end;

function ShowReportPrintFieldsForm(Fm: TdxForm; ARow: Integer): Integer;
begin
  if ReportPrintFieldsFm = nil then
  begin
  	ReportPrintFieldsFm := TCalcFieldsFm.Create(Application);
    ReportPrintFieldsFm.IsReport := True;
    ReportPrintFieldsFm.Caption := rsCalcWhenPrint;
    ReportPrintFieldsFm.HelpIndex := 'reportwhenprinting';
  end;
  Result := ReportPrintFieldsFm.ShowForm(Fm, ARow);
end;

{$R *.lfm}

{ TCalcFieldsFm }

procedure TCalcFieldsFm.FormCreate(Sender: TObject);
begin
  Grid.Columns[0].Title.Caption:=rsFieldName;
  Grid.Columns[1].Title.Caption:=rsExpression;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TCalcFieldsFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TCalcFieldsFm.GridButtonClick(Sender: TObject; aCol, aRow: Integer);
var
  S: TCaption;
begin
  S := Grid.Cells[aCol, aRow];
  if ShowExprForm(etFormCalcField, nil, S, FForm, nil, nil, nil) = mrOk then
    Grid.Cells[aCol, aRow] := S;
end;

procedure TCalcFieldsFm.GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
var
  r: Integer;
begin
  case Cmd of
    dgcAppend:
      begin
        r := Grid.RowCount;
        Grid.RowCount := r + 1;
        Grid.Row:=r;
      end;
    dgcDelete:
      if ConfirmDelete then
        Grid.DeleteRow(Grid.Row);
  end;
end;

procedure TCalcFieldsFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  R: Integer;
begin
  if ModalResult = mrOk then
  begin
    CanClose := CheckFieldNames(R);
    if not CanClose then
    begin
      Grid.SetFocus;
      Grid.Row := R; Grid.Col := 0;
    end;
  end
  else
  begin
    if Grid.Modified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TCalcFieldsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp(FHelpIndex);
end;

procedure TCalcFieldsFm.FillFields;
var
  i, r: Integer;
begin
  Grid.RowCount := 1;
  for i := 0 to FForm.CalcFields.Count - 1 do
  begin
    r := Grid.RowCount;
    Grid.RowCount := r + 1;
    Grid.Cells[0, r] := FForm.CalcFields.Names[i];
    Grid.Cells[1, r] := FForm.CalcFields.ValueFromIndex[i];
  end;
end;

procedure TCalcFieldsFm.SaveFields;
var
  i: Integer;
  S: String;
begin
  FForm.CalcFields.Clear;
  for i := 1 to Grid.RowCount - 1 do
  begin
    S := Grid.Cells[0, i] + '=' + Grid.Cells[1, i];
    FForm.CalcFields.Add(S);
  end;
end;

function TCalcFieldsFm.CheckFieldNames(var aRow: Integer): Boolean;
var
  i: Integer;
  S: String;

  function _Check(aNm: String; idx: Integer): Boolean;
  var
    j: Integer;
    C: TComponent;
    SS: String;
  begin
    Result := True;
    if not CheckFieldName(aNm) then Exit(False);
    if FIsReport and not CheckSuffixName(aNm) then Exit(False);

    for j := idx + 1 to Grid.RowCount - 1 do
    begin
      SS := Grid.Cells[0, j];
      if (SS <> '') and (MyUtf8CompareText(SS, aNm) = 0) then
      begin
        ErrMsg(rsFieldNameExists);
        Exit(False);
      end;
    end;

    for j := 0 to FForm.ComponentCount - 1 do
    begin
      C := FForm.Components[j];
      if HasFId(C) then
      begin
        if MyUtf8CompareText(aNm, GetFieldName(C)) = 0 then
        begin
          if FIsReport then ErrMsg(rsReportComponentFieldNameExists)
          else ErrMsg(rsComponentFieldNameExists);
          Exit(False);
        end;
      end
      else if (C is TdxLabel) and (Trim(GetExpression(C)) <> '') then
      begin
        if MyUtf8CompareText(aNm, TdxLabel(C).FieldName) = 0 then
        begin
          ErrMsg(rsCalcLabelCaptionExists);
          Exit(False);
        end;
      end;
    end;
  end;

begin
  Result := True;
  for i := 1 to Grid.RowCount - 1 do
  begin
    S := Grid.Cells[0, i];
    if S <> '' then
    begin
      Result := _Check(S, i);
      if not Result then
      begin
        aRow := i;
        Exit;
      end;
    end
    else
    begin
      ErrMsg(rsFieldNameEmpty);
      aRow := i;
      Exit(False);
    end;
  end;
end;

function TCalcFieldsFm.ShowForm(Fm: TdxForm; ARow: Integer): Integer;
var
  n: Integer;
begin
  FForm := Fm;
  FillFields;
  if ARow > 0 then
  begin
    Grid.Col := 0;
    Grid.Row := ARow;
  end;
  Grid.Modified:=False;
  Grid.ClearSelections;
  Result := ShowModal;
  if Result = mrOk then
    SaveFields;
end;

end.

