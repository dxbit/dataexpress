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
unit ReportWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Db, ComCtrls, Menus, Controls, dxreports,
  SqlDb, dsproclists, filtercontrol, ExtCtrls, DBGrids, Grids,
  datasetprocessor, strconsts, dxctrls, Graphics, erroricon;

type
  TTotalData = class
  public
    Field: TField;
    Value: Extended;
    Name: String;
  end;

  { TReportWindow }

  TReportWindow = class(TWindow)
    procedure DataSetAfterClose(DataSet: TDataSet);
    procedure DataSetAfterOpen(DataSet: TDataSet);
    procedure DataSetAfterScroll(DataSet: TDataSet);
    procedure DataSetBeforeClose(DataSet: TDataSet);
    procedure DataSetBeforeOpen(DataSet: TDataSet);
    procedure DataSetBeforeScroll(DataSet: TDataSet);
  private
    procedure DetailMnuHandler(Sender: TObject);
    procedure DetailMnuPopup(Sender: TObject);
    procedure GridCanSort(Sender: TObject; Index: Integer; var Cancel: Boolean);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure GridSortChange(Sender: TObject);
    procedure ReportMenuClick(Sender: TObject);
    procedure ReportsMnuPopup(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
  private
    FRD: TReportData;
    FColors: TQueryColorList;
    FFilterSplitter: TSplitter;
    FFlt: TRpParamsControl;
    FDataSet: TSQLQuery;
    FDataSource: TDataSource;
    FDetailMnu, FReportsMnu: TPopupMenu;
    FQGrid: TdxQueryGrid;
    FImages: TImageList;
    FStatusBar: TStatusBar;
    FToolbar: TToolBar;
    FDSP: TDataSetProcessor;
    FAppendBn, FEditBn, FDeleteBn, FExportBn, FFirstBn, FPriorBn, FNextBn,
      FLastBn, FRefreshBn, FDetailBn: TToolButton;
    FErrs: TCalcError;
    function AddBn(const aHint: String; ImgIdx, aTag: Integer): TToolButton;
    function CalcTotal(DS: TDataSet; aFieldName: String; aFunc: TRpTotalFunc): Extended;
    procedure CalcGrandTotal;
    procedure FillReportsMnu;
    function ParamsExists: Boolean;
    procedure InnerShowReport(Id: Integer);
    procedure CreateDSP;
    //procedure CreateButtons;
    procedure AppendRecord;
    procedure EditRecord;
    procedure DeleteRecord;
    procedure SetControlState;
    procedure UpdateButtons;
    procedure BadReport;
  protected
    procedure DoShow; override;
  public
    constructor CreateWindow;
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    destructor Destroy; override;
    procedure RefreshReport(KeepPos: Boolean);
    function ShowForm(aId: Integer): Integer;
  public
    function ShowReport(const aName: String): Integer;
    property ToolBar: TToolBar read FToolbar;
    property QGrid: TdxQueryGrid read FQGrid;
    property StatusBar: TStatusBar read FStatusBar;
    property FilterSplitter: TSplitter read FFilterSplitter;
    property Filter: TRpParamsControl read FFlt;
  end;

function ShowReportWindow(aId: Integer): Integer;

implementation

uses
  dbengine, sqlgen, apputils, Math, reportmanager, dxusers, expressions,
  LazUtf8, Variants, lists, reportexportform, helpviewform, mainform;

function ShowReportWindow(aId: Integer): Integer;
var
  RpWnd: TReportWindow;
begin
  RpWnd := TReportWindow.CreateNew(nil);
  try
  	if MainFm.OnCreateReportWindow <> nil then MainFm.OnCreateReportWindow(MainFm, RpWnd);
    Result := RpWnd.ShowForm(aId);
  finally
    RpWnd.Free;
  end;
end;

{ TReportWindow }

procedure TReportWindow.DataSetAfterClose(DataSet: TDataSet);
begin
  if FQGrid.OnAfterClose <> nil then
    FQGrid.OnAfterClose(FQGrid);
end;

procedure TReportWindow.DataSetAfterOpen(DataSet: TDataSet);
begin
  if FQGrid.OnAfterOpen <> nil then
    FQGrid.OnAfterOpen(FQGrid);
end;

procedure TReportWindow.DataSetAfterScroll(DataSet: TDataSet);
begin
  if FQGrid.OnAfterScroll <> nil then
    FQGrid.OnAfterScroll(FQGrid);
end;

procedure TReportWindow.DataSetBeforeClose(DataSet: TDataSet);
begin
  if FQGrid.OnBeforeClose <> nil then
    FQGrid.OnBeforeClose(FQGrid);
end;

procedure TReportWindow.DataSetBeforeOpen(DataSet: TDataSet);
begin
  if FQGrid.OnBeforeOpen <> nil then
    FQGrid.OnBeforeOpen(FQGrid);
end;

procedure TReportWindow.DataSetBeforeScroll(DataSet: TDataSet);
begin
  if FQGrid.OnBeforeScroll <> nil then
    FQGrid.OnBeforeScroll(FQGrid);
end;

procedure TReportWindow.DetailMnuHandler(Sender: TObject);
begin
  FRD.DateDetail:=TRpDateDetail(TMenuItem(Sender).Tag);
  RefreshReport(False);
end;

procedure TReportWindow.DetailMnuPopup(Sender: TObject);
begin
  with TPopupMenu(Sender) do
  begin
    Items[0].Checked := FRD.DateDetail = ddDay;
    Items[1].Checked := FRD.DateDetail = ddWeek;
    Items[2].Checked := FRD.DateDetail = ddMonth;
    Items[3].Checked := FRD.DateDetail = ddQuart;
    Items[4].Checked := FRD.DateDetail = ddHalfYear;
    Items[5].Checked := FRD.DateDetail = ddYear;
  end;
end;

procedure TReportWindow.GridCanSort(Sender: TObject; Index: Integer;
  var Cancel: Boolean);
//var
//  S: String;
begin
  {S := UpperCase(TdxQueryGrid(Sender).Columns[index-1].FieldName);
  if Copy(S, 1, 2) = 'CF' then
  begin
    ErrMsg(rsCantSortCol);
    Cancel := True;
  end;}
end;

procedure TReportWindow.GridDblClick(Sender: TObject);
begin
  if IsSimpleReport(FRD) and FEditBn.Enabled then
  begin
    if FQGrid.MouseToCell(FQGrid.ScreenToClient(Mouse.CursorPos)).y > 0 then
	    FEditBn.Click;
  end;
end;

procedure CalcQueryColor(RD: TReportData; RDS: TDataSet; const TargetField: String;
  var FieldName: String; var Color: TColor);
var
  EB: TExpressionBuilder;
  i: Integer;
  E: TExpression;
  V: Variant;
  CD: TRpColoringData;
begin
  FieldName := ''; Color := clNone;
  EB := TExpressionBuilder.Create;
  EB.RD := RD;
  EB.RDSet := RDS;
  EB.SkipLabels:=True;
  E := nil;
  try try
    for i := 0 to RD.Coloring.Count - 1 do
    begin
      CD := RD.Coloring[i];
      if Utf8CompareText(CD.FieldName, TargetField) <> 0 then Continue;
      FreeAndNil(E);
      E := EB.Build(CD.Expr);
      if E <> nil then
      begin
        V := E.Calc;
        if VarIsBool(V) and (V = True) then
        begin
          Color := CD.Color;
          FieldName := CD.FieldName;
          Exit;
        end;
      end;
    end;
  except
    Exit; // Глушим ошибку, чтобы программа не упала
  end;
  finally
    EB.Free;
    FreeAndNil(E);
  end;
end;

{procedure TReportWindow.GridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  CD: TQueryColorData;
  Clr: TColor;
  FlNm: String;
  RecNo: Integer;
begin
  if (FDataSet.RecordCount > 0) and (not (gdSelected in State)) and (FRD.Coloring.Count > 0) then
  begin
    RecNo := FQGrid.Row; //FDataSet.ActiveBuffer;
    Clr := clNone;
    CD := FColors.FindColor(Column.Title.Caption, RecNo);
    if CD = nil then
    begin
      CalcQueryColor(FRD, FDataSet, Column.Title.Caption, FlNm, Clr);
      if Clr = clNone then
      begin
        CD := FColors.FindColor('', RecNo);
        if CD = nil then
          CalcQueryColor(FRD, FDataSet, '', FlNm, Clr);
      end;
      if Clr <> clNone then
      begin
        CD := FColors.AddColor;
        CD.RecNo:=RecNo;
        CD.Color := Clr;
        CD.FieldName := FlNm;
      end;
    end;
    if CD <> nil then
    begin
      FQGrid.Canvas.Brush.Color:=CD.Color;
      FQGrid.Canvas.FillRect(Rect);
    end;
  end;
  FQGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end; }

procedure TReportWindow.GridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  Clr: TColor;
  FlNm: String;
begin
  if (FDataSet.RecordCount > 0) and (not (gdSelected in State)) and (FRD.Coloring.Count > 0) then
  begin
    CalcQueryColor(FRD, FDataSet, Column.FieldName, FlNm, Clr);
    if Clr = clNone then
      CalcQueryColor(FRD, FDataSet, '', FlNm, Clr);
    if Clr <> clNone then
    begin
	    FQGrid.Canvas.Brush.Color:=Clr;
      FQGrid.Canvas.FillRect(Rect);
    end;
  end;
  FQGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TReportWindow.GridSortChange(Sender: TObject);
var
  i: Integer;
  CD: TSortColData;
  Col: TRpGridColumn;
begin
  FRD.Grid.SortCols.Clear;
  for i := 0 to FQGrid.SortCols.Count - 1 do
  begin
    CD := FQGrid.SortCols[i];
    Col := FRD.Grid.FindColumnByFieldName(TColumn(CD.Col).FieldName);
    FRD.Grid.SortCols.AddCol(Col, CD.Desc);
  end;
  RefreshReport(False);
end;

procedure TReportWindow.ReportMenuClick(Sender: TObject);
begin
  with TComponent(Sender) do
    if Tag <> FRD.Id then
      InnerShowReport(Tag);
end;

procedure TReportWindow.ReportsMnuPopup(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FReportsMnu.Items.Count - 1 do
    with FReportsMnu.Items[i] do
      Checked := Tag = FRD.Id;
end;

procedure TReportWindow.ToolButtonClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: FReportsMnu.PopUp;
    1: FDetailMnu.Popup;
    2: FDataSet.First;
    3: FDataSet.Prior;
    4: FDataSet.Next;
    5: FDataSet.Last;
    6: AppendRecord;
    7: EditRecord;
    8: DeleteRecord;
    9: RefreshReport(False);
    10:
      begin
        if FDataSet.Active then
          ReportExportFm.ShowForm(FRD, FQGrid);
      end;
    11: HelpViewFm.ShowForm(FRD.HelpText);
  end;
  SetControlState;
end;

function TReportWindow.AddBn(const aHint: String; ImgIdx, aTag: Integer
  ): TToolButton;
var
  Bn: TToolButton;
begin
  Bn := TToolButton.Create(Self);
  Bn.Caption := '';
  Bn.Hint:=aHint;
  Bn.ImageIndex:=ImgIdx;
  Bn.ShowHint:=True;
  Bn.Tag := aTag;
  Bn.OnClick:=@ToolButtonClick;
  Bn.Parent := FToolBar;
  Result := Bn;
end;

procedure TReportWindow.RefreshReport(KeepPos: Boolean);
var
  Id: LongInt;
begin
  if KeepPos then
    Id := FDataSet.Fields[0].AsInteger;

  FColors.Clear;
  if FRD.Sources.Count = 0 then Exit;
  FFlt.Save;
  FDataSet.Close;
  try
    FDataSet.SQL.Text := SqlReportSelect(FRD, nil, nil, nil);
  except
    on E: EFilterParserError do
    begin
      ErrMsg(Format(rsErrorSrcFlt, [LineEnding]) + E.Message);
      BadReport;
      Exit;
    end;
  end;
  // Чтобы был редактируемым
  FDataSet.DeleteSQL.Text := 'delete * from rdb$database';
  //
  FDataSet.ClearIndexes;
  FDataSet.MaxIndexesCount:=100;
  FDataSet.Open;
  SetQueryDisplayFormat(FRD, FDataSet);
  CalcQuery(FRD, FDataSet, nil, nil, nil, FErrs);
  try
    FilterQuery(FRD, FDataSet, nil, nil, nil);
  except
    on E: Exception do
    begin
      ErrMsg(Format(rsErrorOutFlt, [LineEnding]) + E.Message);
    end;
  end;
  if CalcFieldExistsInSort(FRD) then
	  BuildSortIndexes(FRD, FDataSet);
  CalcGrandTotal;

  if KeepPos then
    FDataSet.Locate('id', Id, []);
  UpdateButtons;
  SetControlState;
end;

function TReportWindow.CalcTotal(DS: TDataSet; aFieldName: String;
  aFunc: TRpTotalFunc): Extended;
var
  F: TField;
  Mn, Mx, Sum, E: Extended;
  Cnt: Integer;
  FirstRec: Boolean;
begin
  Result := 0;
  E := 0;
  FirstRec := True;
  Cnt := 0; Sum := 0;
  if aFieldName <> '' then
	  F := DS.FieldByName(aFieldName);
  DS.DisableControls;
  DS.First;
  try
    while not DS.EOF do
    begin
      if aFieldName <> '' then E := F.AsFloat;
      if FirstRec then
      begin
        Mn := E;
        Mx := E;
        FirstRec := False;
      end;
      Inc(Cnt);
      Sum := Sum + E;
      if E < Mn then Mn := E;
      if E > Mx then Mx := E;
      DS.Next;
    end;
    case aFunc of
      tfSum: Result := Sum;
      tfAvg: Result := Sum / Cnt;
      tfCount: Result := Cnt;
      tfMax: Result := Mx;
      tfMin: Result := Mn;
    end;
  finally
    DS.EnableControls;
    DS.First;
  end;
end;

procedure TReportWindow.CalcGrandTotal;
var
  S: String;
  i: Integer;
  T: TRpTotalData;
  E: Extended;
  BefScr, AftScr: TDataSetNotifyEvent;
  B: TBookmark;
  OldState: TDataSetState;
begin
  DisableDataSetScroll(FDataSet, BefScr, AftScr, B, OldState);
  S := '';
  for i := 0 to FRD.Totals.Count - 1 do
  begin
    T := FRD.Totals[i];
    S := S + T.Caption + ': ';
    try
      E := RoundTo(CalcTotal(FDataSet, T.FieldName, T.Func), -2);
      S := S + FloatToStr(E);
    except
      on Ex: Exception do
        S := S + Ex.Message;
    end;
    if i < FRD.Totals.Count - 1 then S := S + ' | ';
  end;
  StatusBar.SimpleText:=S;
  EnableDataSetScroll(FDataSet, BefScr, AftScr, B, OldState);
end;

procedure TReportWindow.FillReportsMnu;
var
  SL: TStringList;
  i: Integer;
  RD: TReportData;
  MI: TMenuItem;
  Mnu: TPopupMenu;
begin
  SL := TStringList.Create;
  ReportMan.GetReports(SL);
  Mnu := FReportsMnu;
  Mnu.Items.Clear;
  for i := 0 to SL.Count - 1 do
  begin
    RD := TReportData(SL.Objects[i]);
    if not UserMan.CheckRpVisible(RD.Id) then Continue;
    MI := TMenuItem.Create(Mnu);
    MI.Caption:=RD.Name;
    MI.Tag := RD.Id;
    MI.OnClick:=@ReportMenuClick;
    Mnu.Items.Add(MI);
  end;
  SL.Free;
end;

function TReportWindow.ParamsExists: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FRD.Sources[0]^.Fields.Count - 1 do
    if FRD.Sources[0]^.Fields[i]^.Param then Exit(True);
end;

procedure TReportWindow.InnerShowReport(Id: Integer);
begin
  FreeAndNil(FDSP);
  FreeAndNil(FRD);
  FDataSet.Close;
  //FRD := ReportMan.FindReport(Id);
  FRD := ReportMan.CreateReport(Id);
  if FRD.Sources.Count = 0 then
  begin
    ErrMsg(rsSourceNotSel);
    BadReport;
    Exit;
  end;
  Caption := FRD.Name;
  if not ParamsExists then
  begin
    FFilterSplitter.Visible := False;
    FFlt.Visible:=False;
  end
  else
  begin
    FFlt.Visible:=True;
    FFilterSplitter.Visible := True;
    FFilterSplitter.Top := FFlt.Top + FFlt.Height + 10;
  end;
  StatusBar.SimpleText:='';
  StatusBar.Visible := FRD.Totals.Count > 0;
  InitGrid(FQGrid, FRD);
  FFlt.RD := FRD;
  FFlt.Clear;
  FFlt.Load;
  //UpdateButtons;
  RefreshReport(False);
end;

procedure TReportWindow.CreateDSP;
begin
  if FDSP = nil then
  begin
    FDSP := TDataSetProcessor.Create;
    FDSP.BindForm(StrToInt(FRD.Sources[0]^.Id), False, vtGridOnly);
    // Чтобы открывалась форма редактирования, если грид редакируемый в форме.
    FDSP.Form.Grid.ReadOnly := True;
  end;
end;

{procedure TReportWindow.CreateButtons;
var
  i: Integer;
begin
  FToolBar.Parent := nil;
  for i := FToolBar.ButtonCount - 1 downto 0 do
    FToolBar.Buttons[i].Free;

  AddBn(rsSelectReport, 0, 0);
  FToolBar.Buttons[0].Style:=tbsDropdown;
  FToolBar.Buttons[0].DropdownMenu := FReportsMnu;
  if FRD.DateField >= 0 then
    AddBn(rsDetailingPeriod, 1, 1);
  FFirstBn := AddBn(rsMoveFirst, 2, 2);
  FPriorBn := AddBn(rsMovePrevious, 3, 3);
  FNextBn := AddBn(rsMoveNext, 4, 4);
  FLastBn := AddBn(rsMoveLast, 5, 5);
  if IsSimpleReport(FRD) then
  begin
    FAppendBn := AddBn(rsAppend, 6, 6);
    FEditBn := AddBn(rsEdit, 7, 7);
    FDeleteBn := AddBn(rsDelete, 8, 8);
  end;
  AddBn(rsRefresh, 9, 9);
  FExportBn := AddBn(rsExportData, 10, 10);
  if FRD.HelpText <> '' then
    AddBn(rsHelp, 11, 11);
  FToolBar.Parent := Self;
end;   }

procedure TReportWindow.AppendRecord;
begin
  CreateDSP;
  try
    FDSP.OpenRecord(0);
    FDSP.AppendRecord(nil, nil, nil, nil, FRD.Id);
    RefreshReport(False);
  except
    on E: Exception do
      ErrMsg(E.Message);
  end;
end;

procedure TReportWindow.EditRecord;
begin
  CreateDSP;
  try
    if FDSP.OpenRecord(FDataSet.Fields[0].AsInteger) then
    begin
      FDSP.Edit;
      RefreshReport(True);
    end;
  except
    on E: Exception do
      ErrMsg(E.Message);
  end;
end;

procedure TReportWindow.DeleteRecord;
var
  rslt: Boolean;
begin
  CreateDSP;
  try
    rslt := FDSP.OpenRecord(FDataSet.Fields[0].AsInteger);
    if not FDSP.CanDelete then
      ErrMsg(rsCantDelRec)
    else
    begin
      if rslt then FDSP.Delete;
      RefreshReport(False);
    end;
  except
    on E: Exception do
      ErrMsg(E.Message);
  end;
end;

procedure TReportWindow.SetControlState;
var
  b, da: Boolean;
begin
  da := FDataSet.Active;
  b := da and (FDataSet.Fields[0].IsNull = False);
  FDetailBn.Enabled := da;
  FFirstBn.Enabled := da and (not FDataSet.BOF);
  FPriorBn.Enabled := da and (not FDataSet.BOF);
  FNextBn.Enabled := da and (not FDataSet.EOF);
  FLastBn.Enabled := da and (not FDataSet.EOF);
  FAppendBn.Enabled := da;
  FEditBn.Enabled := b;
  FDeleteBn.Enabled := b;
	FRefreshBn.Enabled := da;
  FExportBn.Enabled := b;
  FQGrid.Enabled := da;
end;

procedure TReportWindow.UpdateButtons;
var
  i: Integer;
begin
  with FToolbar do
  begin
    BeginUpdate;
    Buttons[1].Visible := FRD.DateField >= 0;
    Buttons[6].Visible := IsSimpleReport(FRD);
    Buttons[7].Visible := Buttons[6].Visible;
		Buttons[8].Visible := Buttons[6].Visible;
    Buttons[11].Visible := FRD.HelpText <> '';
    for i := 0 to ButtonCount - 1 do
    begin
      Buttons[i].Left:=i * 32;
    end;
    EndUpdate;
  end;
end;

procedure TReportWindow.BadReport;
begin
  FFilterSplitter.Visible := False;
  FFlt.Visible:=False;
	FStatusBar.Visible := False;
  UpdateButtons;
  SetControlState;
end;

procedure TReportWindow.DoShow;
begin
  if FQGrid.CanFocus then
    FQGrid.SetFocus;
  inherited DoShow;
end;

constructor TReportWindow.CreateWindow;
begin
  CreateNew(nil);
  if MainFm.OnCreateReportWindow <> nil then MainFm.OnCreateReportWindow(MainFm, Self);
end;

constructor TReportWindow.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  Position := poOwnerFormCenter;
  BorderIcons:=[biSystemMenu, biMaximize];
  Width := 900;
  Height := 600;

  FColors := TQueryColorList.Create;

  FDataSet := TSQLQuery.Create(Self);
  DBase.AttachDataSet(FDataSet);
  FDataSet.BeforeScroll:=@DataSetBeforeScroll;
  FDataSet.AfterScroll:=@DataSetAfterScroll;
  FDataSet.BeforeOpen:=@DataSetBeforeOpen;
  FDataSet.AfterOpen:=@DataSetAfterOpen;
  FDataSet.BeforeClose:=@DataSetBeforeClose;
  FDataSet.AfterClose:=@DataSetAfterClose;


  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FDataSet;

  FImages := TImageList.Create(Self);
  with FImages do
  begin
    Width := 24;
    Height := 24;
    AddLazarusResource('reports24');
    AddLazarusResource('datedetail24');
    AddLazarusResource('movefirst24');
    AddLazarusResource('moveprior24');
    AddLazarusResource('movenext24');
    AddLazarusResource('movelast24');
    AddLazarusResource('add24');
    AddLazarusResource('edit_24');
    AddLazarusResource('delete24');
    AddLazarusResource('refresh24');
    AddLazarusResource('export24');
    AddLazarusResource('help24');
  end;

  FDetailMnu := TPopupMenu.Create(Self);
  with FDetailMnu do
  begin
    Items.Add( CreateMenuItem(FDetailMnu, rsDDDay, 0, 0, @DetailMnuHandler, '') );
    Items.Add( CreateMenuItem(FDetailMnu, rsDDWeek, 1, 0, @DetailMnuHandler, '') );
    Items.Add( CreateMenuItem(FDetailMnu, rsDDMonth, 2, 0, @DetailMnuHandler, '') );
    Items.Add( CreateMenuItem(FDetailMnu, rsDDQuarter, 3, 0, @DetailMnuHandler, '') );
    Items.Add( CreateMenuItem(FDetailMnu, rsDDHalfYear, 4, 0, @DetailMnuHandler, '') );
    Items.Add( CreateMenuItem(FDetailMnu, rsDDYear, 5, 0, @DetailMnuHandler, '') );
    OnPopup:=@DetailMnuPopup;
  end;

  FReportsMnu := TPopupMenu.Create(Self);
  with FReportsMnu do
    OnPopup:=@ReportsMnuPopup;

  FToolBar := TToolBar.Create(Self);
  with FToolBar do
  begin
    Parent := Self;
    Align := alTop;
    EdgeBorders:=[];
    ButtonWidth := 32;
    ButtonHeight := 32;
    Images := FImages;
    AutoSize := True;
    ShowHint := True;
    List := True;
  end;

  AddBn(rsSelectReport, 0, 0);
  FToolBar.Buttons[0].Style:=tbsDropdown;
  FToolBar.Buttons[0].DropdownMenu := FReportsMnu;
  FDetailBn := AddBn(rsDetailingPeriod, 1, 1);
  FFirstBn := AddBn(rsMoveFirst, 2, 2);
  FPriorBn := AddBn(rsMovePrevious, 3, 3);
  FNextBn := AddBn(rsMoveNext, 4, 4);
  FLastBn := AddBn(rsMoveLast, 5, 5);
  FAppendBn := AddBn(rsAppend, 6, 6);
  FEditBn := AddBn(rsEdit, 7, 7);
  FDeleteBn := AddBn(rsDelete, 8, 8);
  FRefreshBn := AddBn(rsRefresh, 9, 9);
  FExportBn := AddBn(rsExportData, 10, 10);
  AddBn(rsHelp, 11, 11);

  FFilterSplitter := TSplitter.Create(Self);
  with FFilterSplitter do
  begin
    Parent := Self;
    Align := alTop;
    ResizeStyle:=rsPattern;
  end;

  FFlt := TRpParamsControl.Create(Self);
  FFlt.Parent := Self;
  FFlt.Align := alTop;
  FFlt.Top := 60;

  FQGrid := TdxQueryGrid.Create(Self);
  with FQGrid do
  begin
    Parent := Self;
    Align := alClient;
    DataSource := FDataSource;
    OnCanSort:=@GridCanSort;
    OnSortColumnChange:=@GridSortChange;
    OnDrawColumnCell:=@GridDrawColumnCell;
    OnDblClick:=@GridDblClick;
    RpWnd := Self;
  end;

  FStatusBar := TStatusBar.Create(Self);
  with FStatusBar do
  begin
    Parent := Self;
    Align := alBottom;
    SimplePanel := True;
  end;
  FErrs := TCalcError.Create(Self);
  FErrs.Parent := FQGrid;
end;

destructor TReportWindow.Destroy;
begin
  FColors.Free;
  inherited Destroy;
end;

function TReportWindow.ShowForm(aId: Integer): Integer;
begin
  FillReportsMnu;
  InnerShowReport(aId);
  Result := ShowModal;
  FreeAndNil(FDSP);
  FDataSet.Close;
  FreeAndNil(FRD);
end;

function TReportWindow.ShowReport(const aName: String): Integer;
var
  Rp: TReportData;
begin
  Rp := ReportMan.FindByName(aName);
  if Rp = nil then raise Exception.CreateFmt(rsReportNotFound, [aName]);
  FQGrid.Id:=Rp.Id;
  Result := ShowForm(Rp.Id);
end;

end.

