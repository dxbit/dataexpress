unit QueryColoringForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, Grids, ButtonPanel, dxreports, strconsts, StdCtrls, Buttons,
  dxctrls, DialogGrid, Types;

type

  { TQueryColoringFm }

  TQueryColoringFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TDialogGrid;
    DialogGridButtons1: TDialogGridButtons;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FRD: TReportData;
    FFm: TdxForm;
    procedure FillFields;
    procedure FillGrid;
    procedure Save;
    function Validate: Boolean;
  public
    { public declarations }
    function ShowForm(aRD: TReportData; aFm: TdxForm; ARow: Integer = 0): Integer;
  end;

var
  QueryColoringFm: TQueryColoringFm;

function ShowQueryColoringForm(aRD: TReportData; aFm: TdxForm; ARow: Integer = 0): Integer;

implementation

uses
  apputils, exprform, helpmanager, mytypes;

function ShowQueryColoringForm(aRD: TReportData; aFm: TdxForm; ARow: Integer
  ): Integer;
begin
  if QueryColoringFm = nil then
  	QueryColoringFm := TQueryColoringFm.Create(Application);
  Result := QueryColoringFm.ShowForm(aRD, aFm, ARow);
end;

{$R *.lfm}

{ TQueryColoringFm }

procedure TQueryColoringFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('rpqrycoloring');
end;

procedure TQueryColoringFm.FormCreate(Sender: TObject);
begin
  Caption := rsColoring;
  Grid.FocusColor:=Grid.SelectedColor;
  Grid.Columns[0].Title.Caption:=rsColor;
  Grid.Columns[1].Title.Caption:=rsFieldName;
  Grid.Columns[2].Title.Caption:=rsExpression;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TQueryColoringFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TQueryColoringFm.GridButtonClick(Sender: TObject; aCol, aRow: Integer
  );
var
  S: TCaption;
begin
  S := Grid.Cells[aCol, aRow];
  if ShowExprForm(etColoring, nil, S, FFm, nil, nil, FRD) = mrOk then
    Grid.Cells[aCol, aRow] := S;
end;

procedure TQueryColoringFm.GridCommand(Sender: TObject; Cmd: TDialogGridCommand
  );
var
  r: Integer;
begin
  case Cmd of
    dgcAppend:
      begin
        r := Grid.RowCount;
        Grid.RowCount := r + 1;
        Grid.Cells[0, r] := ColorToString(clWhite);
        Grid.Row := r;
      end;
    dgcDelete:
      if ConfirmDelete then
        Grid.DeleteRow(Grid.Row);
  end;
end;

procedure TQueryColoringFm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  TS: TTextStyle;
begin
  if aRow = 0 then Exit;
  if aCol = 1 then
  begin
    if Grid.Cells[aCol, aRow] = '' then
    begin
      TS.Alignment := taLeftJustify;
      TS.Layout := tlCenter;
      TS.SingleLine := True;
      TS.Clipping := True;
      if gdFocused in aState then
        Grid.Canvas.Font.Color := clHighlightText
      else
        Grid.Canvas.Font.Color := clDkGray;
      Grid.Canvas.TextRect(aRect, aRect.Left + varCellpadding, 0, rsAllFieldsList, TS);
    end;
  end;
end;

procedure TQueryColoringFm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  if ModalResult = mrOk then
  	CanClose := Validate
  else
  begin
    if Grid.Modified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TQueryColoringFm.FillGrid;
var
  L: TRpColoringList;
  i, r: Integer;
  CD: TRpColoringData;
  Col: TRpGridColumn;
begin
  L := FRD.Coloring;
  Grid.RowCount := 1;
  for i := 0 to L.Count - 1 do
  begin
    CD := L[i];
    r := i + 1;
    Grid.RowCount := r + 1;
    Grid.Cells[0, r] := ColorToString(CD.Color);
    Col := FRD.Grid.FindColumnByFieldName(CD.FieldNameDS);
    if Col <> nil then
      Grid.Cells[1, r] := Col.Caption;
    //Grid.Objects[1, r] := Col;
    Grid.Cells[2, r] := CD.Expr;
  end;
end;

procedure TQueryColoringFm.FillFields;
var
  i: Integer;
  SL: TStringListUtf8;
  Col: TRpGridColumn;
begin
  SL := TStringListUtf8.Create;
  SL.Add('');
  for i := 0 to FRD.Grid.ColumnCount - 1 do
  begin
    Col := FRD.Grid.Columns[i];
    SL.Add(Col.Caption);
  end;
  SL.Sort;
  Grid.Columns[1].PickList := SL;
  SL.Free;
end;

procedure TQueryColoringFm.Save;
var
  L: TRpColoringList;
  i: Integer;
  CD: TRpColoringData;
  Col: TRpGridColumn;
begin
  L := FRD.Coloring;
  L.Clear;
  for i := 1 to Grid.RowCount - 1 do
  begin
    CD := L.AddColoring;
    CD.Color:=StringToColor(Grid.Cells[0, i]);
    //Col := TRpGridColumn(Grid.Objects[1, i]);
    Col := nil;
    if Grid.Cells[1, i] <> '' then
	    Col := FRD.Grid.FindColumnByTitle(Grid.Cells[1, i]);
    if Col <> nil then
      CD.FieldNameDS:=Col.FieldNameDS;
    CD.Expr:=Grid.Cells[2, i];
  end;
end;

function TQueryColoringFm.Validate: Boolean;
var
  i: Integer;
  S: String;
begin
  Result := True;
  for i := 1 to Grid.RowCount - 1 do
  begin
    S := Grid.Cells[1, i];
    if (S > '') and
    	(FRD.Grid.FindColumnByTitle(S) = nil) then
    begin
      ErrMsgFmt(rsFieldNotFound, [S]);
      Grid.SetFocus;
      Grid.Col := 1; Grid.Row := i;
      Exit(False);
    end;
  end;
end;

function TQueryColoringFm.ShowForm(aRD: TReportData; aFm: TdxForm; ARow: Integer
  ): Integer;
begin
  Result := mrNone;
  FRD := aRD;
  FFm := aFm;
  if FRD.IsEmpty then
  begin
    ErrMsg(rsSourceNotSel);
    Exit;
  end;
  FillFields;
  FillGrid;
  Grid.Modified:=False;
  Grid.ClearSelections;
  if ARow > 0 then Grid.Row := ARow;
  Result := ShowModal;
  if Result = mrOk then Save;
end;

end.

