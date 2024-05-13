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

unit ReportWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Db, ComCtrls, Menus, Controls, dxreports,
  SqlDb, dsproclists, filtercontrol, ExtCtrls, DBGrids, Grids,
  datasetprocessor, strconsts, dxctrls, Graphics, erroricon, Dialogs, dbengine;

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
    procedure FilterChange(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure GridSortChange(Sender: TObject);
    procedure ReportMenuClick(Sender: TObject);
    procedure ReportsMnuPopup(Sender: TObject);
    procedure TemplatesMnuClick(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
  private
    FRD: TReportData;
    FColors: TQueryColorList;
    FFilterSplitter: TSplitter;
    FFlt: TRpParamsControl;
    FDataSet: TdxDataSet;
    FDataSource: TDataSource;
    FDetailMnu, FReportsMnu: TPopupMenu;
    FQGrid: TdxQueryGrid;
    FImages: TImageList;
    FStatusBar: TStatusBar;
    FToolbar: TToolBar;
    FDSP: TDataSetProcessor;
    FAppendBn, FEditBn, FDeleteBn, FExportBn, FFirstBn, FPriorBn, FNextBn,
      FLastBn, FRefreshBn, FDetailBn, FPrintBn: TToolButton;
    FErrs: TCalcError;
    FFilterChanged: Boolean;
    FTemplatesMnu: TPopupMenu;
    function AddBn(const aHint: String; ImgIdx, aTag: Integer): TToolButton;
    function CalcTotal(DS: TDataSet; aFieldName: String; aFunc: TRpTotalFunc): Variant;
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
    procedure PrintReport(Idx: Integer);
    procedure BuildTemplatesMenu;
  protected
    procedure DoShow; override;
  public
    constructor CreateWindow;
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    destructor Destroy; override;
    procedure RefreshReport;
    function ShowForm(aId: Integer): Integer;
  public
    function ShowReport(const aName: String): Integer;
    property ToolBar: TToolBar read FToolbar;
    property QGrid: TdxQueryGrid read FQGrid;
    property StatusBar: TStatusBar read FStatusBar;
    property FilterSplitter: TSplitter read FFilterSplitter;
    property Filter: TRpParamsControl read FFlt;
    property RD: TReportData read FRD;
  end;

function ShowReportWindow(aId: Integer): Integer;

implementation

uses
  sqlgen, apputils, reportmanager, dxusers, expressions,
  LazUtf8, Variants, lists, reportexportform, helpviewform, mainform, StrUtils;

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
  SetControlState;
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
  RefreshReport;
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

procedure TReportWindow.FilterChange(Sender: TObject);
begin
  FFilterChanged := True;
end;

procedure TReportWindow.GridDblClick(Sender: TObject);
begin
  if FRD.IsSimple and FEditBn.Enabled and FEditBn.Visible then
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
      if MyUtf8CompareText(CD.FieldNameDS, TargetField) <> 0 then Continue;
      FreeAndNil(E);
      E := EB.Build(CD.Expr);
      if E <> nil then
      begin
        V := E.Calc;
        if VarIsBool(V) and (V = True) then
        begin
          Color := CD.Color;
          FieldName := CD.FieldNameDS;
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
  if Column.Field = nil then Exit;
  if (FDataSet.RecordCount > 0) and (not (gdSelected in State)) and (FRD.Coloring.Count > 0) then
  begin
    CalcQueryColor(FRD, FDataSet, Column.FieldName, FlNm, Clr);
    if Clr = clNone then
      CalcQueryColor(FRD, FDataSet, '', FlNm, Clr);
    if Clr <> clNone then
    begin
      if gdRowHighlight in State then Clr := ColorToRGB(Clr) xor $1F1F1F;
	    FQGrid.Canvas.Brush.Color:=Clr;
      FQGrid.Canvas.FillRect(Rect);
    end;
  end;
  FQGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TReportWindow.GridSortChange(Sender: TObject);
{var
  i: Integer;
  CD: TSortColumn;
  Col: TRpGridColumn; }
begin
  QGrid.SortColsToRpGridSortCols;
  {FRD.Grid.SortCols.Clear;
  for i := 0 to FQGrid.SortCols.Count - 1 do
  begin
    CD := FQGrid.SortCols[i];
    Col := FRD.Grid.FindColumnByFieldName(TColumn(CD.Col).FieldName);
    FRD.Grid.SortCols.AddCol(Col, CD.Desc);
  end;}
  RefreshReport;
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

procedure TReportWindow.TemplatesMnuClick(Sender: TObject);
begin
  PrintReport(TMenuItem(Sender).Tag);
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
    9: RefreshReport;
    10:
      begin
        if FRD.Templates.Count = 1 then PrintReport(0)
        else FTemplatesMnu.PopUp;
      end;
    11:
      begin
        if FDataSet.Active then
        begin
          if FFilterChanged then
          begin
            if Confirm(rsWarning, rsRpParamsChanged) = mrYes then RefreshReport
            else Exit;
          end;
          ShowReportExportForm(FRD, FQGrid);
        end;
      end;
    12: ShowHelpForm(FRD.HelpText);
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

procedure TReportWindow.RefreshReport;
begin
  FFilterChanged := False;

  FColors.Clear;
  if FRD.IsEmpty then Exit;
  FFlt.EditorMode:=False;
  FFlt.Save;
  FDataSet.Close;
  try
    FDataSet.SQL.Text := SqlReportSelect(FRD, nil, nil, nil);
  except
    on E: Exception do
    begin
      // Prop и Expr в E
      FErrs.AddErr('', FRD.Name, rsReport, '', '', E);
      //ErrMsg(Format(rsErrorSrcFlt, [LineEnding]) + E.Message);
      BadReport;
      Exit;
    end;
  end;
  // Чтобы был редактируемым
  FDataSet.DeleteSQL.Text := 'delete * from rdb$database';
  //
  {FDataSet.ClearIndexes;
  FDataSet.IndexName := '';
  FDataSet.MaxIndexesCount:=100;}
  FDataSet.IndexFieldNames := '';
  FDataSet.Open;
  SetQueryDisplayFormat(FRD, FDataSet);
  CalcQuery(FRD, FDataSet, nil, nil, nil, FErrs);
  try
    FilterQuery(FRD, FDataSet, nil, nil, nil);
  except
    on E: Exception do
    begin
      FErrs.AddErr('', FRD.Name, rsReport, rsOutputFilter, FRD.Filter, E);
      //ErrMsg(Format(rsErrorOutFlt, [LineEnding]) + E.Message);
    end;
  end;
  if CalcFieldExistsInSort(FRD) then
	  BuildSortIndexes(FRD, FDataSet);
  CalcGrandTotal;
  UpdateButtons;
  BuildTemplatesMenu;
  SetControlState;
end;

function TReportWindow.CalcTotal(DS: TDataSet; aFieldName: String;
  aFunc: TRpTotalFunc): Variant;
var
  F: TField;
  Mn, Mx, Sum, V: Variant;
  Cnt: Integer;
  FirstRec: Boolean;
begin
  case aFunc of
    tfSum, tfAvg, tfCount: Result := 0;
    else Result := '';
  end;

  FirstRec := True;
  Cnt := 0; Sum := 0;
  if aFieldName <> '' then F := DS.FieldByName(aFieldName)
  else F := nil;
  DS.DisableControls;
  DS.First;
  try
    while not DS.EOF do
    begin
      if F <> nil then
      begin
        V := F.Value;
        if V = Null then
        begin
          DS.Next;
          Continue;
        end;
      end;

      if FirstRec then
      begin
        Mn := V;
        Mx := V;
        FirstRec := False;
      end;
      Inc(Cnt);
      case aFunc of
        tfSum, tfAvg: Sum := Sum + V;
        tfMin: if V < Mn then Mn := V;
        tfMax: if V > Mx then Mx := V;
      end;
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

procedure GetRpFieldType(RD: TReportData; const FieldName: String; out FTp: TRpFieldType;
  {out Prec: Integer} out Fmt: String);
var
  {FId: LongInt;
  F: TRpField;
  C: TComponent;
  CF: TRpCalcField;}
  i: Integer;
begin
  i := RD.IndexOfNameDS(FieldName);

  FTp := RD.GetFieldType(i);
  Fmt := RD.GetDisplayFormat(i);

  (*Fmt := '';
  if Copy(FieldName, 1, 1) = 'f' then
  begin
    FId := StrToInt(Copy(FieldName, 2, 10));
    F := RD.FindField(FId)^;
    FTp := F.Tp;
    if FTp = flNumber then
    begin
      C := GetRpFieldComponent(F, True);
      if C <> nil then
        Fmt := TdxCalcEdit(C).PrecStr;
        //Prec := GetPrecission(C);
    end
    else if FTp = flTime then
    begin
      C := GetRpFieldComponent(F, True);
      if C <> nil then
        Fmt := TdxTimeEdit(C).TimeFormatStr;
    end;
  end
  else
  begin
    FId := StrToInt(Copy(FieldName, 3, 10));
    CF := RD.CalcFields.FindField(FId)^;
    FTp := CF.Tp;
    if FTp = flNumber then
      Fmt := MakeNumberFormat(CF.Size, True, True)
    else if FTp = flTime then
      Fmt := 'hh:nn:ss';
      //Prec := CF.Size;
  end;   *)
end;

procedure TReportWindow.CalcGrandTotal;
var
  S, Fmt: String;
  i: Integer;
  T: TRpTotalData;
  V: Variant;
  FTp: TRpFieldType;
begin
  S := '';
  for i := 0 to FRD.Totals.Count - 1 do
  begin
    T := FRD.Totals[i];
    if FRD.IndexOfNameDS(T.FieldNameDS) < 0 then Continue;

    S := S + T.Caption + ': ';
    try
      if T.Func = tfCount then
      begin
        FTp := flNumber;
        Fmt := ',0';
      end
      else
      //if T.FieldNameDS <> '' then
        GetRpFieldType(FRD, T.FieldNameDS, FTp, Fmt);
      {else
      begin
        FTp := flNumber;
        //Prec := 0;
        Fmt := ',0';
      end;}

      V := CalcTotal(FDataSet, T.FieldNameDS, T.Func);

      if FTp = flNumber then
      begin
        {Fmt := ',0';
        if Prec > 0 then
          Fmt := Fmt + '.' + DupeString('0', Prec); }
        T.Value := FormatFloat(Fmt, V);
        S := S + T.Value;
      end
      else if FTp = flTime then
      begin
        T.Value := FormatDateTime(Fmt, V);
        S := S + T.Value;
      end
      else
      begin
        T.Value := VarToStr(V);
        S := S + T.Value;
      end;
    except
      on Ex: Exception do
        S := S + Ex.Message;
    end;
    if i < FRD.Totals.Count - 1 then S := S + ' | ';
  end;
  StatusBar.SimpleText:=S;
end;

procedure TReportWindow.FillReportsMnu;
var
  SL: TStringList;
  i: Integer;
  Rp: TReportData;
  MI: TMenuItem;
  Mnu: TPopupMenu;
begin
  SL := TStringList.Create;
  ReportMan.GetReports(SL);
  Mnu := FReportsMnu;
  Mnu.Items.Clear;
  for i := 0 to SL.Count - 1 do
  begin
    Rp := TReportData(SL.Objects[i]);
    if not UserMan.CheckRpVisible(Rp.Id) then Continue;
    MI := TMenuItem.Create(Mnu);
    MI.Caption:=Rp.Name;
    MI.Tag := Rp.Id;
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
  for i := 0 to FRD.GetFieldCount - 1 do
    if FRD.GetFieldParam(i) then Exit(True);
end;

procedure TReportWindow.InnerShowReport(Id: Integer);
begin
  FreeAndNil(FDSP);
  FreeAndNil(FRD);
  FDataSet.Close;
  //FRD := ReportMan.FindReport(Id);
  FRD := ReportMan.CreateReport(Id);
  FDataSet.RD := FRD;
  if FRD.IsEmpty then
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
  RefreshReport;
end;

procedure TReportWindow.CreateDSP;
begin
  //!!!
  if FRD.Sources.Count = 0 then Exit;

  if FDSP = nil then
  begin
    FDSP := TDataSetProcessor.Create;
    FDSP.BindForm(FRD.GetEditFormId, False, vtGridOnly);
    // Чтобы открывалась форма редактирования, если грид редакируемый в форме.
    FDSP.Form.Grid.ReadOnly := True;
  end;
end;

procedure TReportWindow.AppendRecord;
var
  NewId: Variant;
begin
  CreateDSP;
  try
    FDSP.OpenRecord(0);
    NewId := FDSP.AppendRecord(nil, nil, nil, FRD.Id);
    if NewId <> Null then
    begin
	    RefreshReport;
      FDataSet.Locate('id', NewId, []);
    end;
  except
    on E: Exception do
      ErrMsg(ExceptionToString(E, False, False), True, 'ReportAppendRecord');
  end;
end;

procedure TReportWindow.EditRecord;
var
  Id: Variant;
begin
  CreateDSP;
  try
    if FDSP.OpenRecord(FDataSet.Fields[0].AsInteger) then
    begin
      if FDSP.EditObject(False) = mrOk then
      begin
        Id := FDataSet['id'];
	      RefreshReport;
        FDataSet.Locate('id', Id, []);
      end;
    end;
  except
    on E: Exception do
      ErrMsg(ExceptionToString(E, False, False), True, 'ReportEditRecord');
  end;
end;

procedure TReportWindow.DeleteRecord;
var
  rslt: Boolean;
begin
  CreateDSP;
  try
    rslt := FDSP.OpenRecord(FDataSet.Fields[0].AsInteger);
    if rslt and FDSP.Delete then
	  	RefreshReport;
  except
    on E: Exception do
      ErrMsg(ExceptionToString(E, False, False), True, 'ReportDeleteRecord');
  end;
end;

procedure TReportWindow.SetControlState;
var
  b, da: Boolean;
begin
  da := FDataSet.Active;
  b := da and (FDataSet.RecordCount > 0);
  FDetailBn.Enabled := da;
  FFirstBn.Enabled := da and (not FDataSet.BOF);
  FPriorBn.Enabled := da and (not FDataSet.BOF);
  FNextBn.Enabled := da and (not FDataSet.EOF);
  FLastBn.Enabled := da and (not FDataSet.EOF);
  FAppendBn.Enabled := da;
  FEditBn.Enabled := b;
  FDeleteBn.Enabled := b;
	FRefreshBn.Enabled := da;
  FExportBn.Enabled := da;
  //FQGrid.Enabled := da;

  if QGrid.OnStateChange <> nil then QGrid.OnStateChange(QGrid);
end;

procedure TReportWindow.UpdateButtons;
var
  i, FmId: Integer;
  CanV, CanA, CanE, CanD: Boolean;
begin
  with FToolbar do
  begin
    BeginUpdate;
    Buttons[1].Visible := FRD.DateField >= 0;
    Buttons[6].Visible := FRD.IsSimple;
    Buttons[7].Visible := Buttons[6].Visible;
		Buttons[8].Visible := Buttons[6].Visible;
    Buttons[10].Visible := FRD.Templates.Count > 0;
    Buttons[12].Visible := FRD.HelpText <> '';

    // !!! Доступ
    if FRD.IsSimple then
    begin
	    FmId := FRD.GetEditFormId;
  	  CanV := UserMan.CheckFmVisible(FmId);
      CanA := UserMan.CheckFmAdding(FmId);
      CanE := UserMan.CheckFmEditing(FmId);
      CanD := UserMan.CheckFmDeleting(FmId);
      Buttons[6].Visible := CanV and CanA;
      Buttons[7].Visible := CanV;
      Buttons[8].Visible := CanV and CanE and CanD;
      if CanE then
      begin
        Buttons[7].ImageIndex := 7;
        Buttons[7].Hint := rsEdit;
      end
      else
      begin
        Buttons[7].ImageIndex := 13;
        Buttons[7].Hint := rsLook;
      end;
    end;

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

procedure TReportWindow.PrintReport(Idx: Integer);
var
  DSP: TDataSetProcessor;
  TemplateName: String;
begin
  if FFilterChanged then
  begin
    if Confirm(rsWarning, rsRpParamsChanged) = mrYes then RefreshReport
    else Exit;
  end;
  TemplateName := FRD.Templates[Idx];
  {$ifdef windows}
  TemplateName := StringReplace(TemplateName, '/', DirectorySeparator, [rfReplaceAll]);
  {$else}
  TemplateName := StringReplace(TemplateName, '\', DirectorySeparator, [rfReplaceAll]);
  {$endif}
  TemplateName := GetTemplatesDir + TemplateName;

  // Проверка файла
  if not FileExists(TemplateName) then
  begin
    ErrMsg(Format(rsTemplateFileNotFound, [TemplateName]));
    Exit;
  end;

  DSP := TDataSetProcessor.Create;
  DSP.BindReport(FRD, FDataSet);
  DSP.OpenReport;
  DSP.Print(TemplateName);
  DSP.UnBindReport;
  DSP.Free;
end;

procedure TReportWindow.BuildTemplatesMenu;
var
  i: Integer;
  S: String;
begin
  FTemplatesMnu.Items.Clear;
  if FRD.Templates.Count <= 1 then Exit;

  for i := 0 to FRD.Templates.Count - 1 do
  begin
    S := ExtractFileName(ChangeFileExt(FRD.Templates[i], ''));
    FTemplatesMnu.Items.Add( CreateMenuItem(FTemplatesMnu, S, i, 0, @TemplatesMnuClick) );
  end;
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
  //Position := poDesigned;//poOwnerFormCenter;
  BorderIcons:=[biSystemMenu, biMaximize];
  Width := ScaleToScreen(900);
  Height := ScaleToScreen(600);

  FColors := TQueryColorList.Create;

  FDataSet := TdxDataSet.Create(Self);
  FDataSet.PacketRecords:=100;
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
    AddLazarusResource('print24');
    AddLazarusResource('export24');
    AddLazarusResource('help24');
    AddLazarusResource('eyes24');
  end;

  FDetailMnu := TPopupMenu.Create(Self);
  with FDetailMnu do
  begin
    Items.Add( CreateMenuItem(FDetailMnu, rsDDDay, 0, 0, @DetailMnuHandler) );
    Items.Add( CreateMenuItem(FDetailMnu, rsDDWeek, 1, 0, @DetailMnuHandler) );
    Items.Add( CreateMenuItem(FDetailMnu, rsDDMonth, 2, 0, @DetailMnuHandler) );
    Items.Add( CreateMenuItem(FDetailMnu, rsDDQuarter, 3, 0, @DetailMnuHandler) );
    Items.Add( CreateMenuItem(FDetailMnu, rsDDHalfYear, 4, 0, @DetailMnuHandler) );
    Items.Add( CreateMenuItem(FDetailMnu, rsDDYear, 5, 0, @DetailMnuHandler) );
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
  FPrintBn := AddBn(rsPrint, 10, 10);
  FExportBn := AddBn(rsExportData, 11, 11);
  AddBn(rsHelp, 12, 12);

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
  FFlt.Height := FFlt.DefaultRowHeight * 6 + 4;
  FFlt.OnChange:=@FilterChange;

  FQGrid := TdxQueryGrid.Create(Self);
  with FQGrid do
  begin
    Parent := Self;
    Align := alClient;
    DataSource := FDataSource;
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

  FTemplatesMnu := TPopupMenu.Create(Self);
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

