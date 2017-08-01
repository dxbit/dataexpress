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
unit DatasetProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, Controls, Db, SqlDb, Menus, dxctrls, strconsts,
  DbGrids, StdCtrls, Grids, ExtCtrls, editform, filterform, Graphics,
  expressions, DXReports, dsproclists, listform, Lists, DBCtrls, Buttons,
  myctrls, PopupNotifier, DxActions, Forms, scriptmanager, myclasses,
  erroricon;

type
  PDataSetRec = ^TDataSetRec;
  TDataSetRec = record
    DataSet: TSQLQuery;
    DataSource: TDataSource;
    Grid: TdxGrid;
    Form: TdxForm;
    SQL: String;
    //Filter: String;
    Filter: TFilterObject;
    Popup: TPopupMenu;
    FilterFm: TFilterFm;
    ExprList: TExprList;
    LblExprList: TExprList;
    DefValList: TExprList;
    ChkExprList: TExprList;
    EditFm: TEditWindow;
    Colors: TColorList;
    Err: TCalcError;
    Adding, Editing, Deleting: Boolean;
    EditCond, DelCond: TExpression;
    EditingCtrls: TList;
    CanShoping: Boolean;
    FilterIndex: Integer;   // Предустановленный фильтр
    TblFilterSet: Boolean;     // Для таблиц (если фильтр установлен)
    HasFields: Boolean;     // На случай, если в форме нет ни одного поля, чтобы не было ошибок.
    RunScript: TRunScript;
  end;

  TDataSetProcessor = class;

  PLookupRec = ^TLookupRec;
  TLookupRec = record
    DataSet: TSQLQuery;
    DataSource: TDataSource;
    Control: TComponent;
    Column: TColumn;
    TId: Integer;
    SQL: String;
    ListFm: TListWindow;
    NeedRefresh: Boolean;
    DSRi: Integer;
    Popup: TPopupMenu;
    DSProc: TDataSetProcessor;
  end;

  PQueryRec = ^TQueryRec;
  TQueryRec = record
    DataSet: TSQLQuery;
    DataSource: TDataSource;
    Grid: TdxQueryGrid;
    Popup: TPopupMenu;
    DSProc: TDataSetProcessor;
    DSRi: Integer;
    Simple: Boolean;
    Colors: TQueryColorList;
    RD: TReportData;
  end;

  { TDatasetProcessor }

  TDataSetProcessor = class
    procedure ButtonClick(Sender: TObject);
    procedure DataSetAfterClose(DataSet: TDataSet);
    procedure DataSetBeforeCancel(DataSet: TDataSet);
    procedure DataSetBeforeClose(DataSet: TDataSet);
    procedure DataSetBeforeDelete(DataSet: TDataSet);
    procedure DataSetBeforeEdit(DataSet: TDataSet);
    procedure DataSetBeforeInsert(DataSet: TDataSet);
    procedure DataSetBeforeOpen(DataSet: TDataSet);
    procedure DataSetBeforeScroll(DataSet: TDataSet);
    procedure LCbxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure QueryAfterClose(DataSet: TDataSet);
    procedure QueryAfterOpen(DataSet: TDataSet);
    procedure QueryBeforeClose(DataSet: TDataSet);
    procedure QueryBeforeOpen(DataSet: TDataSet);
    procedure QueryBeforeScroll(DataSet: TDataSet);
  private
    procedure DescrFileFieldChange(Sender: TField);
    procedure ImageFieldChange(Sender: TField);
    procedure TimerTimer(Sender: TObject);
    procedure QueryGridButtonClick2(Sender: TObject; Bn: TGridButtonType);
    procedure QueryGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DataSetAfterOpen(DataSet: TDataSet);
    procedure DataSetBeforePost(DataSet: TDataSet);
    procedure FieldChange(Sender: TField);
    procedure FieldSetText(Sender: TField; const aText: string);
    procedure GridButtonClick(Sender: TObject; Bn: TGridButtonType);
    procedure GridCellClick(Column: TColumn);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    //procedure GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridCanSort(Sender: TObject; index: Integer; var Cancel: Boolean);
    procedure GridSortColumnChange(Sender: TObject);
    procedure GridValidate(Sender: TObject; var Ok: Boolean);
    procedure LCbxButtonClick(Sender: TObject);
    procedure LCbxChange(Sender: TObject);
    procedure LCbxDropDown(Sender: TObject);
    procedure LookupCtrlClick(Sender: TObject);
    procedure DataSetAfterCancel(DataSet: TDataSet);
    procedure DataSetAfterDelete(DataSet: TDataSet);
    procedure DataSetAfterEdit(DataSet: TDataSet);
    procedure DataSetAfterInsert(DataSet: TDataSet);
    procedure DataSetAfterPost(DataSet: TDataSet);
    procedure DataSetAfterScroll(DataSet: TDataSet);
    procedure GridSelectEditor(Sender: TObject; Column: TColumn;
      var Editor: TWinControl);
    procedure LookupMenuClick(Sender: TObject);
    procedure LookupMenuPopup(Sender: TObject);
    procedure MemoButtonClick(Sender: TObject);
    procedure PickListEditingDone(Sender: TObject);
    procedure PopupMnu(Sender: TObject);
    procedure QueryAfterScroll(DataSet: TDataSet);
    procedure QueryGridButtonClick(Sender: TObject; Bn: TGridButtonType);
    procedure QueryGridCanSort(Sender: TObject; Index: Integer;
      var Cancel: Boolean);
    procedure QueryGridDblClick(Sender: TObject);
    procedure QueryGridSortChange(Sender: TObject);
    procedure QueryMenuHandler(Sender: TObject);
    procedure QueryGotoRec(aQ: TQueryRec; aId: Integer);
    procedure UpdateQueryPopupStates;
    procedure UpdateQueryPopupState(Q: TQueryRec);
    procedure MenuItemClick(Sender: TObject);
    procedure ExchangeRows(DS: TDataSet; MoveUp: Boolean);
  private
    FGotoEnable: Boolean;
    FItems: TList;
    FFm: TdxForm;
    FMaster: PDataSetRec;
    FOnChangeFilter: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FPrinting, FReCalculate, FDuplicateFlag, FDeletingRecs, FIsListForm,
      FRecLocked, FInserting, FLoopDetected: Boolean;
    FLookups: TList;
    FQueries: TList;
    FExprCounter, FLblExprCounter: Integer;
    FListFilter: String;
    FTimer: TTimer;
    FNotif: TPopupNotifier;
    FSimpleMode: Boolean;
    procedure AddEditingCtrls(DSRi: Integer);
    procedure ClearItems;
    procedure BindControls(DSR: TDataSetRec);
    procedure BuildExprs(DSR: PDataSetRec);
    procedure BuildDefVals(DSR: PDataSetRec);
    procedure BuildCheckExprs(DSR: PDataSetRec);
    procedure AddDataSet(aGrid: TdxGrid);
    function GetCanAdd: Boolean;
    function GetCanDelete: Boolean;
    function GetCanEdit: Boolean;
    function GetDataSet(Index: Integer): PDataSetRec;
    function FindDataSet(FmId: Integer): PDataSetRec;
    function CreatePopupMenu(IsMaster, AllowSpace, HasShop: Boolean): TPopupMenu;
    function GetQueries(Index: Integer): PQueryRec;
    procedure RequeryDetail(i: Integer);
    procedure RequeryDetails;
    procedure CloseDetails;
    procedure AddLookup(aGrid: TdxGrid; C: TComponent; DSRi: Integer);
    procedure AddLookups(aGrid: TdxGrid; DSRi: Integer);
    procedure ClearLookups;
    procedure ClearQueries;
    function FindLookupByColumn(C: TColumn): PLookupRec;
    function FindLookupById(Id: Integer): PLookupRec;
    procedure CalcFields(DSR: TDataSetRec; const FieldName: String; Sender: TComponent);
    procedure CalcAggFields(DSR: TDataSetRec; FormName: String);
    function MakeFilter: String;
    procedure AddQueries(DSRi: Integer);
    function CreateQuery(QG: TdxQueryGrid; DSRi: Integer): PQueryRec;
    procedure RequeryQueries(DSRi: Integer);
    procedure RequeryLinkedQueries(CurQ: PQueryRec; const QueryName: String);
    procedure RequeryQueriesWithParams(const FieldName: String; DSRi: Integer);
    procedure CalcExprs(DSR: TDataSetRec; const FieldName: String; aLabel: TdxLabel);
    procedure CalcAggExprs(DSR: TDataSetRec; const FormName: String);
    procedure RefreshComboBox(LR: TLookupRec);
    procedure ChangeObjectFields(Obj: TdxLookupComboBox);
    procedure ApplyQuickFilter;
    function CalcColor(Fm: TdxForm; DS: TDataSet): TColor;
    procedure CalcQueryColor(RD: TReportData; Fm: TdxForm; RDS, DS: TDataSet;
      const TargetField: String; var FieldName: String; var Color: TColor);
    procedure InsertObjectValues(Obj: TdxLookupComboBox);
    procedure FillTableFromObject(Obj: TdxLookupComboBox);
    procedure RefreshLookupsWithParams(Fm: TdxForm; const FieldName: String);
    procedure PrepareListForm(pLR: PLookupRec);
    procedure SetColumnTitles(G: TdxGrid);
    procedure SetGrids;
    function CalcEditCond(Fm: TdxForm; DS: TDataSet; const Cond: String): Boolean;
    procedure UpdatePopupState(DSR: TDataSetRec);
    procedure UpdateControlState(DSR: TDataSetRec);
    procedure DuplicateRec(DSR: TDataSetRec);
    procedure CalcDefVals(DSR: TDataSetRec);
    procedure CheckAccess(pDS: PDataSetRec);
    procedure DoChangeFilter;
    //procedure RefreshRecordFromDB;
    procedure RefreshDataAndEdit(IsDeleteRec: Boolean);
    procedure LockRecord;
    procedure UnLockRecord;
    procedure ShowError(C: TControl; const Msg: String; DSR: TDataSetRec);
    procedure BuildPivotTables(idx: Integer);
    function CheckModifyRecord(IsDeleteRec: Boolean): Integer;
    procedure UnBind;
    procedure MasterSetModified;
  public
    procedure DoStateChange;
    function InnerCheckModifyRecord: Integer;
    function CheckLockRecord(KeepTrans: Boolean): Boolean;
    function InnerCheckLockRecord(Commit: Boolean): Integer;
    procedure ClearListFilter;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BindForm(FmId: Integer; IsListForm: Boolean; aViewType: TViewType);
    function ShowEditForm(DSRi: Integer): Integer;
    procedure Open;
    procedure _Open2(const aFilter: String);
    function OpenRecord(aKey: Integer): Boolean;
    procedure OpenList(const aFilter: String);
    procedure OpenWhere(const Wh: String);
    procedure Close;
    procedure Append;
    procedure AppendRecord(aPFm, aFm: TdxForm; aPDS, aDS: TDataSet; RpId: Integer);
    function AppendObject: Integer;
    procedure Duplicate;
    procedure DuplicateAll;
    procedure Edit;
    function EditObject: Integer;
    procedure Delete;
    procedure InnerDelete;
    procedure DeleteAllRecords;
    procedure Post;
    procedure Refresh;
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prior;
    function Opened: Boolean;
    function MasterSet: TSQLQuery;
    procedure RefreshLookups(TId: Integer);
    function CheckDeleteRecord(TId, RecId: Integer; ShowMsg: Boolean): Boolean;
    procedure RefreshCurRecord;
    procedure OpenFilter(DSRi: Integer);
    procedure ApplyFilter(FmId: Integer; FilterIndex: Integer);
    procedure Recalculate(TId, FId: Integer; aExpr: String);
    function DataSetCount: Integer;
    function QueryCount: Integer;
    procedure ApplyTreeSelect(Keys: TList);
    procedure ForceChangeFields(DSRi: Integer);
    function Validate(DSRi: Integer; ForceChanges: Boolean = True): Boolean;
    procedure Print(TemplateName: String);
    procedure InnerPrint(const TemplateName, OutName: String; var Errs: String; aOpenFile: Boolean);
    procedure RequeryQuery(idx: Integer);
    function IsFilterSet: Boolean;
    property Printing: Boolean read FPrinting write FPrinting;
    property GotoEnable: Boolean read FGotoEnable write FGotoEnable;
    property Form: TdxForm read FFm;
    property CanAdd: Boolean read GetCanAdd;
    property CanEdit: Boolean read GetCanEdit;
    property CanDelete: Boolean read GetCanDelete;
    property DataSets[Index: Integer]: PDataSetRec read GetDataSet;
    property Queries[Index: Integer]: PQueryRec read GetQueries;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnChangeFilter: TNotifyEvent read FOnChangeFilter write FOnChangeFilter;
  end;

function FormExists(FormName, E: String): Boolean;

implementation

uses
  dbengine, formmanager, sqlgen, apputils, mainframe,
  LCLType, StrUtils, dximages, BGRABitmap, BGRABitmapTypes,
  LazUtf8, Dialogs, dxfiles, Variants, reportmanager, dxusers,
  ButtonPanel, warningform, Clipbrd, errorsform, mainform, xmlreport,
  pivotgrid;

function IsTableInput(pDSR: PDataSetRec): Boolean;
begin
  Result := False;
  with pDSR^ do
  	Result := (Form.ViewType = vtGridOnly) and (not Grid.ReadOnly);
end;

function Testing: Boolean;
begin
	Result := MainFr = nil;
end;

function SortColumnsToSQL(G: TdxGrid): String;
var
  S: String;
  L: TSortColList;
  i: Integer;
  CD: TSortColData;
begin
  S := '';
  L := G.SortCols;
  for i := 0 to L.Count - 1 do
  begin
    CD := L[i];
    S := S + TColumn(CD.Col).FieldName;
    if CD.Desc then S := S + ' desc';
    S := S + ',';
  end;
  S := Copy(S, 1, Length(S) - 1);
  Result := S;
end;

function FindComponentByDataField(Fm: TdxForm; const DataField: String): TComponent;
var
  i: Integer;
  C: TComponent;
begin
  Result := nil;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not HasFId(C) then Continue;
    if CompareText(DataField, FieldStr(C)) = 0 then Exit(C);
  end;
end;

{ TDatasetProcessor }


procedure SetDisplayFormat(Gr: TdxGrid; Fm: TdxForm);
var
  i, Id, Prec: Integer;
  C, CC: TComponent;
  Frm: TdxForm;
begin
  for i := 0 to Gr.Columns.Count - 1 do
  begin
    Id := Gr.Columns[i].Tag;
    C := FindById(Fm, Id);
    if C is TdxObjectField then
      with TdxObjectField(C) do
        if (ObjId > 0) and (FieldId > 0) then
        begin
          CC := FindById(Fm, ObjId);
          Frm := FormMan.FindForm(GetSourceTId(CC));
          C := FindById(Frm, FieldId);
        end;
    if (C is TdxCalcEdit) then
      with TNumericField(Gr.Columns[i].Field) do
      begin
        //DisplayFormat:='0';
        //EditFormat := '0.#';
        Prec := GetPrecission(C);
        if Prec > 0 then
        begin
          Gr.Columns[i].DisplayFormat:='0.' + DupeString('0', Prec);;
          //DisplayFormat := '0.' + DupeString('0', Prec);
          //EditFormat := '0.' + DupeString('#', Prec);
        end;
      end
    else if C is TdxTimeEdit then
      with Gr.Columns[i] do
      begin
        DisplayFormat:=TdxTimeEdit(C).TimeFormatStr;
      end
  end;
end;

procedure TDataSetProcessor.GridValidate(Sender: TObject; var Ok: Boolean);
var
  i: PtrInt;
  DSR: TDataSetRec;
begin
  i := TComponent(Sender).Tag;
  DSR := GetDataSet(i)^;

  ForceChangeFields(i);
  if DSR.Form.ConfirmAutoSaveRecord and DSR.DataSet.Modified then
  begin
    case MessageDlg(rsWarning, rsConfirmAutoSaveMsg,	mtConfirmation,
    	[mbYes, mbNo, mbCancel], 0) of
      mrYes: ;
      mrNo: begin
          		DSR.DataSet.Cancel;
          		Exit;
		        end;
      else 	begin
        			Ok := False;
      				Exit;
      			end;
    end;
    {if MessageDlg(rsWarning, rsConfirmAutoSaveMsg,	mtConfirmation,
    	[mbYes, mbNo], 0) = mrNo then
    begin
      Ok := False;
      Exit;
    end;  }
  end;

  Ok := Validate(i, False);
end;

procedure TDataSetProcessor.ButtonClick(Sender: TObject);
var
  Bn: TdxButton;
  Act: TBasicAction;
begin
  Bn := TdxButton(Sender);
  Act := CreateAction(Bn.ActionType);
  if Act <> nil then
    try
      Act.Load(Bn.ActionProps);
      Act.DSProc := Self;
      Act.DSRi := Bn.Owner.Tag;
      Act.Execute;
    finally
      Act.Free;
    end;
end;

procedure TDataSetProcessor.DataSetAfterClose(DataSet: TDataSet);
begin
  with GetDataSet(DataSet.Tag)^ do
    if Form.OnAfterClose <> nil then Form.OnAfterClose(Form);
end;

procedure TDataSetProcessor.DataSetBeforeCancel(DataSet: TDataSet);
begin
  with GetDataSet(DataSet.Tag)^ do
    if Form.OnBeforeCancel <> nil then Form.OnBeforeCancel(Form);
end;

procedure TDataSetProcessor.DataSetBeforeClose(DataSet: TDataSet);
begin
  with GetDataSet(DataSet.Tag)^ do
    if Form.OnBeforeClose <> nil then Form.OnBeforeClose(Form);
end;

procedure TDataSetProcessor.DataSetBeforeDelete(DataSet: TDataSet);
begin
  with GetDataSet(DataSet.Tag)^ do
    if Form.OnBeforeDelete <> nil then Form.OnBeforeDelete(Form);
end;

procedure TDataSetProcessor.DataSetBeforeEdit(DataSet: TDataSet);
begin
  with GetDataSet(DataSet.Tag)^ do
    if Form.OnBeforeEdit <> nil then Form.OnBeforeEdit(Form);
end;

procedure TDataSetProcessor.DataSetBeforeInsert(DataSet: TDataSet);
begin
  with GetDataSet(DataSet.Tag)^ do
    if Form.OnBeforeInsert <> nil then Form.OnBeforeInsert(Form);
end;

procedure TDataSetProcessor.DataSetBeforeOpen(DataSet: TDataSet);
begin
  with GetDataSet(DataSet.Tag)^ do
    if Form.OnBeforeOpen <> nil then Form.OnBeforeOpen(Form);
end;

procedure TDataSetProcessor.DataSetBeforeScroll(DataSet: TDataSet);
begin
  with GetDataSet(DataSet.Tag)^ do
    if Form.OnBeforeScroll <> nil then Form.OnBeforeScroll(Form);
end;

procedure TDataSetProcessor.LCbxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssShift in Shift) then
  begin
    Key := 0;
    if TdxLookupComboBox(Sender).Button.Enabled then
      LCbxButtonClick(Sender);
  end;
end;

procedure TDataSetProcessor.QueryAfterClose(DataSet: TDataSet);
var
  Q: PQueryRec;
begin
  Q := PQueryRec(FQueries[DataSet.Tag]);
  with Q^.Grid do
    if OnAfterClose <> nil then OnAfterClose(Q^.Grid);
end;

procedure TDataSetProcessor.QueryAfterOpen(DataSet: TDataSet);
var
  Q: PQueryRec;
begin
  Q := PQueryRec(FQueries[DataSet.Tag]);
  with Q^.Grid do
    if OnAfterOpen <> nil then OnAfterOpen(Q^.Grid);
end;

procedure TDataSetProcessor.QueryBeforeClose(DataSet: TDataSet);
var
  Q: PQueryRec;
begin
  Q := PQueryRec(FQueries[DataSet.Tag]);
  with Q^.Grid do
    if OnBeforeClose <> nil then OnBeforeClose(Q^.Grid);
end;

procedure TDataSetProcessor.QueryBeforeOpen(DataSet: TDataSet);
var
  Q: PQueryRec;
begin
  Q := PQueryRec(FQueries[DataSet.Tag]);
  with Q^.Grid do
    if OnBeforeOpen <> nil then OnBeforeOpen(Q^.Grid);
end;

procedure TDataSetProcessor.QueryBeforeScroll(DataSet: TDataSet);
var
  Q: PQueryRec;
begin
  Q := PQueryRec(FQueries[DataSet.Tag]);
  with Q^.Grid do
    if OnBeforeScroll <> nil then OnBeforeScroll(Q^.Grid);
end;

procedure TDataSetProcessor.DescrFileFieldChange(Sender: TField);
var
  i: PtrInt;
  DSR: TDataSetRec;
  S: String;
  C: TComponent;
begin
  i := Sender.DataSet.Tag;
  DSR := GetDataSet(i)^;
  S := Sender.FieldName;
  S := Copy(S, 1, Length(S) - 1); // Удаляем ...d
  C := FindComponentByDataField(DSR.Form, S);
  if (C <> nil) and (DSR.Form.OnFieldChange <> nil) then
  	DSR.Form.OnFieldChange(DSR.Form, C, GetFieldName(C));
end;

procedure TDataSetProcessor.ImageFieldChange(Sender: TField);
var
  i: PtrInt;
  DSR: TDataSetRec;
  S: String;
  C: TComponent;
begin
  i := Sender.DataSet.Tag;
  DSR := GetDataSet(i)^;
  S := Sender.FieldName;
  S := Copy(S, 1, Length(S) - 3); // Удаляем ...src
  C := FindComponentByDataField(DSR.Form, S);
  if (C <> nil) and (C is TdxDBImage) and (DSR.Form.OnFieldChange <> nil) then
  	DSR.Form.OnFieldChange(DSR.Form, C, GetFieldName(C));
end;

procedure TDataSetProcessor.TimerTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  FNotif.Hide;
end;

procedure TDataSetProcessor.QueryGridButtonClick2(Sender: TObject;
  Bn: TGridButtonType);
var
  Pop: TPopupMenu;
begin
  Pop := TdxQueryGrid(Sender).PopupMenu;
  Pop.Items[0].Click;
end;

{procedure TDataSetProcessor.QueryGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  Q: TQueryRec;
  CD: TQueryColorData;
  DSR: TDataSetRec;
  Clr: TColor;
  FlNm: String;
  RD: TReportData;
  RecNo: Integer;
begin
  if FPrinting then Exit;
  Q := PQueryRec(FQueries[TComponent(Sender).Tag])^;
  RD := ReportMan.FindReport(Q.Grid.Id);
  if (Q.DataSet.RecordCount > 0) and (not (gdSelected in State)) and (RD.Coloring.Count > 0) then
  begin
    RecNo := Q.Grid.Row; //Q.DataSet.ActiveBuffer;
    Clr := clNone;
    CD := Q.Colors.FindColor(Column.FieldName, RecNo);
    if CD = nil then
    begin
      DSR := GetDataSet(Q.DSRi)^;
      CalcQueryColor(RD, DSR.Form, Q.DataSet, DSR.DataSet, Column.FieldName,
        FlNm, Clr);
      if Clr = clNone then
      begin
        CD := Q.Colors.FindColor('', RecNo);
        if CD = nil then
          CalcQueryColor(RD, DSR.Form, Q.DataSet, DSR.DataSet, '', FlNm, Clr);
      end;
      if Clr <> clNone then
      begin
        CD := Q.Colors.AddColor;
        CD.RecNo:=RecNo;
        CD.Color := Clr;
        CD.FieldName := FlNm;
      end;
    end;
    if CD <> nil then
    begin
      Q.Grid.Canvas.Brush.Color:=CD.Color;
      Q.Grid.Canvas.FillRect(Rect);
    end;
  end;
  TMyDBGrid(Sender).DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;}

procedure TDataSetProcessor.QueryGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  Q: TQueryRec;
  DSR: TDataSetRec;
  Clr: TColor;
  FlNm: String;
  RD: TReportData;
begin
  if FPrinting or FReCalculate then Exit;
  Q := PQueryRec(FQueries[TComponent(Sender).Tag])^;
  //RD := ReportMan.FindReport(Q.Grid.Id);
  RD := Q.RD;
  if (Q.DataSet.RecordCount > 0) and (not (gdSelected in State)) and (RD.Coloring.Count > 0) then
  begin
    DSR := GetDataSet(Q.DSRi)^;
    CalcQueryColor(RD, DSR.Form, Q.DataSet, DSR.DataSet, Column.FieldName,
      FlNm, Clr);
    if Clr = clNone then
      CalcQueryColor(RD, DSR.Form, Q.DataSet, DSR.DataSet, '', FlNm, Clr);
    if Clr <> clNone then
    begin
	    Q.Grid.Canvas.Brush.Color:=Clr;
  	  Q.Grid.Canvas.FillRect(Rect);
    end;
  end;
  TMyDBGrid(Sender).DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TDataSetProcessor.DataSetAfterOpen(DataSet: TDataSet);
var
  DSR: TDataSetRec;
  i: Integer;
  Ch: String;
begin
  DSR := GetDataSet(DataSet.Tag)^;
  SetDisplayFormat(DSR.Grid, DSR.Form);
  for i := 0 to DSR.DataSet.Fields.Count - 1 do
    with DSR.DataSet.Fields[i] do
    begin
      if DataType in [ftBlob] then Continue;
      if DataType = ftString then
      begin
        Ch := Copy(FieldName, Length(FieldName), 1);
        if Pos('SRC', FieldName) > 0 then
        	OnChange := @ImageFieldChange
        else if Pos('DEST', FieldName) > 0 then
        else if Ch = 'L' then
        else if Ch = 'D' then
        	OnChange := @DescrFileFieldChange
        else
        	OnChange := @FieldChange;
      end
      else
      	OnChange:=@FieldChange;

      {if C is TdxFile then
      begin
        DSR.DataSet.FieldByName(FieldStr(C) + 'src').OnChange := @ImageFieldChange;
        DSR.DataSet.FieldByName(FieldStr(C) + 'd').OnChange:=@DescrFileFieldChange;
      end
      else if C is TdxDBImage then
      	DSR.DataSet.FieldByName(FieldStr(C) + 'src').OnChange:=@ImageFieldChange;}

      if DataType in [ftDate, ftFloat, ftString, ftTime] then
        OnSetText:=@FieldSetText;
    end;
  DSR.Colors.Clear;
  if DSR.Form.OnAfterOpen <> nil then DSR.Form.OnAfterOpen(DSR.Form);
end;

procedure TDataSetProcessor.DataSetBeforePost(DataSet: TDataSet);
var
  DSR: TDataSetRec;
  Fm: TdxForm;
  C: TComponent;
  i, FS: Integer;
  F: TField;
begin
  // Посылваем Post дочерней форме - если надо - а также завершаем редактирование
  // ячеек (чтобы данные сохранились).
  if DataSet.Tag = 0 then
  begin
    for i := 1 to FItems.Count - 1 do
    begin
      DSR := GetDataSet(i)^;
      if DSR.Grid.EditorMode and (DSR.Grid.Editor is TPickListCellEditor) then
        TPickListCellEditor(DSR.Grid.Editor).EditingDone;
      if DSR.DataSet.State in [dsEdit, dsInsert] then DSR.DataSet.Post;
    end;
  end;
  //else
  //	MasterSetModified;

  DSR := GetDataSet(DataSet.Tag)^;
  if DSR.Grid.EditorMode and (DSR.Grid.Editor is TPickListCellEditor) then
    TPickListCellEditor(DSR.Grid.Editor).EditingDone;
  Fm := DSR.Form;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not HasFId(C) then Continue;
    if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) then
    begin
      F := DataSet.FieldByName(FieldStr(C));
      FS := GetFieldSize(C);
      if Utf8Length(F.AsString) > FS then F.AsString := Utf8Copy(F.AsString, 1, FS);
      // Очищаем поле с пустой маской
      if C is TdxEdit then
      	with TdxEdit(C) do
        	if EditMask <> '' then
		      begin
						if MaskedTextEmpty(Field.AsString{Text}, EditMask) then
            	Field.SetData(nil);
  		    end;
    end
    else if C is TdxFile then
    begin
      F := DataSet.FieldByName(FieldStr(C) + 'd');
      FS := GetFieldSize(C);
      if Utf8Length(F.AsString) > FS then F.AsString := Utf8Copy(F.AsString, 1, FS);
    end
    else if C is TdxCounter then
    begin
      F := DataSet.FieldByName(FieldStr(C));
      if (DataSet.State = dsInsert) and (F.IsNull) then
        F.Value:=DBase.GenId('gen_' + FieldStr(C));
    end;
  end;
  DSR.Colors.DeleteColor(DSR.DataSet.Fields[0].AsInteger);

  with DSR do
    if Form.OnBeforePost <> nil then Form.OnBeforePost(Form);
end;

procedure TDataSetProcessor.FieldChange(Sender: TField);
var
  DSR: TDataSetRec;
  C: TComponent;
  S: String;
  i: PtrInt;
begin
  i := Sender.DataSet.Tag;
  DSR := GetDataSet(i)^;
  C := FindComponentByDataField(DSR.Form, Sender.FieldName);
  if C <> nil then
  begin
    S := GetFieldName(C);

    if C is TdxMemo then
    begin
      // Для заметок SetText не вызывается, поэтому небольшая хитрость
      if Sender.AsString = '' then
      begin
        Sender.OnChange:=nil;
        Sender.SetData(nil);
        Sender.OnChange:=@FieldChange;
      end;
    end
    // Автоматическое округление числа до указанной точности.
    else if C is TdxCalcEdit then
    begin
      Sender.OnChange:=nil;
      if not Sender.IsNull then
		  	Sender.AsFloat := AppUtils.MathRound(Sender.AsFloat, TdxCalcEdit(C).Precission);
      Sender.OnChange:=@FieldChange;
    end;

    if DSR.Form.OnFieldChange <> nil then DSR.Form.OnFieldChange(DSR.Form, C, S);

    if (C is TdxLookupComboBox) and (not FDuplicateFlag) then
    begin
      ChangeObjectFields(TdxLookupComboBox(C));
      InsertObjectValues(TdxLookupComboBox(C));
      FillTableFromObject(TdxLookupComboBox(C));
    end;

    RefreshLookupsWithParams(DSR.Form, S);
    RequeryQueriesWithParams(S, i);
    if not FLoopDetected then
    begin
	    CalcFields(DSR, S, C);
    	CalcExprs(DSR, S, nil);
    end;
  end;
end;

procedure TDataSetProcessor.FieldSetText(Sender: TField; const aText: string);
var
  D: TDateTime;
  E: Extended;
begin
  //ShowMessage(aText);
  if aText = '' then
  begin
    Sender.SetData(nil);
    Exit;
  end;
  if Sender.DataType in [ftDate] then
  begin
    if TryStrToDate(StringReplace(aText, ' ', DefaultFormatSettings.DateSeparator,
      [rfReplaceAll]), D) then Sender.AsDateTime := D
    else Sender.Value := Sender.Value;
  end
  else if Sender.DataType in [ftFloat] then
  begin
    if TryStrToFloat(aText, E) then
      Sender.AsString:=aText
    else Sender.Value := Sender.Value;
  end
  else if Sender.DataType in [ftTime] then
  begin
    if TryStrToTime(StringReplace(aText, ' ', ':', [rfReplaceAll]), D) then
      Sender.AsDateTime:=D
    else Sender.Value := Sender.Value;
  end
  else if Sender.DataType in [ftString] then
    Sender.AsString := aText;
end;

procedure TDataSetProcessor.GridButtonClick(Sender: TObject; Bn: TGridButtonType
  );
var
  Pop: TPopupMenu;
begin
  Pop := TdxGrid(Sender).PopupMenu;
  case Bn of
    gbnAppend: Pop.Items[0].Click;
    gbnEdit: Pop.Items[1].Click;
    gbnDelete: Pop.Items[2].Click;
    gbnDuplicate: Pop.Items[4].Click;
    gbnShopping: Pop.Items[6].Click;
    gbnMoveUp: Pop.Items[8].Click;
    gbnMoveDown: Pop.Items[9].Click;
  end;
end;

procedure TDataSetProcessor.GridCellClick(Column: TColumn);
var
  DSR: TDataSetRec;
  S: String;
  C: TComponent;
begin
  if Column = nil then Exit;
  DSR := GetDataSet(Column.Grid.Tag)^;
  C := FindById(DSR.Form, Column.Tag);
  if C is TdxEdit then
  begin
    if ssCtrl in GetKeyShiftState then
    begin
      S := Utf8LowerCase(Trim(Column.Field.AsString));
      if IsUrl(S) or IsMail(S) then
        OpenUrl(S);
    end;
  end
  else if (C is TdxLookupComboBox) and (FGotoEnable) then
    if ssCtrl in GetKeyShiftState then
      with TdxLookupComboBox(C) do
        if (KeyValue <> Null) and (not Testing) then
          MainFr.GotoRec(SourceTId, KeyValue);
end;

procedure TDataSetProcessor.GridDblClick(Sender: TObject);
var
  DSR: TDataSetRec;
  G: TDBGrid;
  P: TPoint;
begin
  G := TDBGrid(Sender);
  P := G.ScreenToClient(Mouse.CursorPos);
  P := G.MouseToCell(P);
  if P.y = 0 then Exit;
  if (G.ReadOnly = False) and (P.X > 0) then Exit;
  DSR := GetDataSet(G.Tag)^;
  if G.Tag = 0 then
  begin
    //Edit;
    G.PopupMenu.Items[1].Click;

    // При табличном редактировании окно редактирования не появляется.
    if IsTableInput(@DSR) and (DSR.DataSet.State = dsEdit) then ShowEditForm(G.Tag);
  end
  else
  begin
    if DSR.Popup.Items[1].Enabled then
    begin
      if (DSR.Editing) and (FMaster^.Editing) then
    	  DSR.DataSet.Edit;
  	  UpdateControlState(DSR);
      ShowEditForm(G.Tag);
    end;
    //DSR.Popup.Items[1].Click;
  end;
end;

procedure TDataSetProcessor.GridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  DSR: TDataSetRec;
  C: TComponent;
  x, y, RecId: Integer;
  Bmp: TBGRABitmap;
  St: TStream;
  CD: TColorData;
begin
  if FPrinting or FReCalculate then Exit;
  DSR := GetDataSet(TdxGrid(Sender).Tag)^;
  RecId := DSR.DataSet.Fields[0].AsInteger;
  if (RecId > 0) and (not (gdSelected in State)) and (DSR.Form.Coloring.Count > 0) then
  begin
    CD := DSR.Colors.FindColor(RecId);
    if CD = nil then
      CD := DSR.Colors.AddColor(RecId, CalcColor(DSR.Form, DSR.DataSet));
    if CD.Color <> clNone then
    begin
      DSR.Grid.Canvas.Brush.Color:=CD.Color;
      DSR.Grid.Canvas.FillRect(Rect);
    end;
  end;

  if Column.Field.IsBlob then
  begin
    C := FindById(DSR.Form, Column.Tag);
    if C is TdxDBImage  then
    begin
      TdxGrid(Sender).Canvas.FillRect(Rect);
      if (TdxDBImage(C).ThumbSize > 0) and (not Column.Field.IsNull) then
      begin
        try
        St := Column.Field.DataSet.CreateBlobStream(Column.Field, bmRead);
        Bmp := TBGRABitmap.Create(0, 0);
        try
          if (St <> nil) and (St.Size > 0) then
          Bmp.LoadFromStream(St);
          x := (Rect.Right - Rect.Left) div 2 + Rect.Left - Bmp.Width div 2;
          y := (Rect.Bottom - Rect.Top) div 2 + Rect.Top - Bmp.Height div 2;
          Bmp.Draw(TdxGrid(Sender).Canvas, x, y, True);
        finally
          Bmp.Free;
          FreeAndNil(St);
        end;

        except
          ;
        end;
      end
    end;
  end
  else
  begin
    TdxGrid(Sender).DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

procedure TDataSetProcessor.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  DSR: TDataSetRec;
  i: PtrInt;
begin
  i := TComponent(Sender).Tag;
  if (Key = VK_DOWN) and (i > 0) then
  begin
    if TdxGrid(Sender).ReadOnly then Exit;
    // !!! Доступ
    DSR := GetDataSet(i)^;
    if (UserMan.CheckFmAdding(DSR.Form.Id)) and (DSR.Editing) and (FMaster^.Editing) then
      with TdxGrid(Sender).DataSource.DataSet do
        if (MasterSet.State in [dsInsert, dsEdit]) and (not (State in [dsInsert])) and EOF then
        begin
          Key := 0;
          Append;
        end;
  end
  else if Key = VK_ESCAPE then
  begin
    DSR := GetDataSet(i)^;
    if DSR.DataSet.State in [dsInsert, dsEdit] then
    begin
      ForceChangeFields(i);
      if DSR.Form.ConfirmCancelEditing and DSR.DataSet.Modified then
      begin
        if MessageDlg(rsWarning, rsConfirmCancelEditMsg, mtConfirmation,
          [mbYes, mbNo], 0) <> mrYes then
        begin
          Key := 0;
          Exit;
        end;
      end;
      DSR.DataSet.Cancel;
      UpdateControlState(DSR);
    end;
  end;
end;

procedure TDataSetProcessor.GridCanSort(Sender: TObject; index: Integer;
  var Cancel: Boolean);
{var
  Gr: TdxGrid;
  i: Integer;}
begin
  {Gr := TdxGrid(Sender);
  i := Gr.Tag;}
  if (MasterSet.State in [dsInsert, dsEdit]) or
  	(TdxGrid(Sender).Columns[Index-1].Tag = 0) then
  begin
    Cancel := True;
    Exit;
  end;
  {if i > 0 then
    RequeryDetail(i)
  else
    Refresh;  }
end;

procedure TDataSetProcessor.GridSortColumnChange(Sender: TObject);
var
  Gr: TdxGrid;
  i: PtrInt;
begin
  Gr := TdxGrid(Sender);
  i := Gr.Tag;
  if i > 0 then
    RequeryDetail(i)
  else
    Refresh;
end;

procedure TDataSetProcessor.LCbxButtonClick(Sender: TObject);
var
  pLR: PLookupRec;
  Fm: TListWindow;
  C: TdxLookupComboBox;
begin
  pLR := PLookupRec(FLookups[TComponent(Sender).Tag]);
  C := TdxLookupComboBox(pLR^.Control);
  try
    PrepareListForm(pLR);
  except
    on E: EFilterParserError do
    begin
      ErrMsg(E.Message);
      Exit;
    end;
  end;
  Fm := pLR^.ListFm;
  Fm.DestForm := nil;
  Fm.DestMemo := nil;
  Fm.ValueField:=FieldStr(C.SourceFId);
  Fm.Caption:=C.FieldName;
  Fm.Key := C.KeyValue;
  if Fm.ShowForm = mrOk then
  begin
    if not (C.Field.DataSet.State in [dsInsert, dsEdit]) then
      C.Field.DataSet.Edit;
    if C.Field.DataSet.State in [dsInsert, dsEdit] then
    begin
      C.Field.Value := Fm.Value;
      C.KeyValue := Fm.Key;
      if C.CanFocus then C.SetFocus;
    end;
  end;
end;

procedure TDataSetProcessor.LCbxChange(Sender: TObject);
begin
  LCbxDropDown(Sender);
end;

procedure TDataSetProcessor.LCbxDropDown(Sender: TObject);
var
  pLR: PLookupRec;
begin
  pLR := PLookupRec(FLookups[TComponent(Sender).Tag]);
  if pLR^.NeedRefresh then
  begin
    pLR^.NeedRefresh := False;
    RefreshComboBox(pLR^);
  end;
end;

procedure TDataSetProcessor.LookupCtrlClick(Sender: TObject);
begin
  with TdxLookupComboBox(Sender) do
    if (KeyValue <> Null) and (not Testing) then
      MainFr.GotoRec(SourceTId, KeyValue);
end;

procedure TDataSetProcessor.DataSetAfterCancel(DataSet: TDataSet);
var
  DSR: TDataSetRec;
begin
  DSR := GetDataSet(DataSet.Tag)^;
  if DataSet.Tag = 0 then
  begin
    RequeryDetails;
    DoStateChange;
    //if FMaster^.EditFm = nil then
    //begin
      RequeryQueries(DataSet.Tag);
      CalcExprs(FMaster^, '', nil);
      if FFm.ViewType <> vtGridOnly then
        ShowImages(DSR.Form);
    //end;
    if FRecLocked then UnLockRecord;
  end;
  RefreshLookupsWithParams(DSR.Form, '');
  UpdateControlState(DSR);

  if DSR.Form.OnAfterCancel <> nil then DSR.Form.OnAfterCancel(DSR.Form);
end;

procedure TDataSetProcessor.DataSetAfterDelete(DataSet: TDataSet);
var
  DSR0, DSR: TDataSetRec;
begin
  DSR := GetDataSet(DataSet.Tag)^;
  if DataSet.Tag > 0 then
  begin
    MasterSetModified;
    DSR0 := GetDataSet(0)^;
    CalcAggFields(DSR0, DSR.Form.FormCaption);
    CalcAggExprs(DSR0, DSR.Form.FormCaption);
  end;
  if DSR.Form.OnAfterDelete <> nil then DSR.Form.OnAfterDelete(DSR.Form);
end;

procedure TDataSetProcessor.DataSetAfterEdit(DataSet: TDataSet);
var
  i: Integer;
  DSR: TDataSetRec;
begin
  DSR := GetDataSet(DataSet.Tag)^;
  if DataSet.Tag = 0 then
  begin
    if DBase.Remote and (UserMan.CurrentUser <> nil) and (not UserMan.SingleMode)
      and (not FSimpleMode) then
      LockRecord
    else if not FSimpleMode then
    	if CheckModifyRecord(False) = 1 then Exit;

    if MasterSet.State in [dsInsert, dsEdit] then
      for i := 1 to FItems.Count - 1 do
        UpdatePopupState(GetDataSet(i)^);
  end
  else
  	MasterSetModified;
  DoStateChange;
  UpdateQueryPopupStates;
  if (DSR.Form.OnAfterEdit <> nil) and (DSR.DataSet.State in [dsInsert, dsEdit]) then
  begin
    DSR.Form.OnAfterEdit(DSR.Form);
  end;
end;

function ExtractGroupFieldFilterValue(Flt: TFilterObject; aId: Integer): Integer;
var
  F: TFilterField;
begin
  Result := 0;
  F := Flt.FindField(aId);
  if (F <> nil) and (F.Values.Count > 0) then
  	Result := StrToInt(F.Values[0]);
end;

procedure TDataSetProcessor.DataSetAfterInsert(DataSet: TDataSet);
var
  DSR: TDataSetRec;
  i, id: Integer;
  Fm: TdxForm;
  C: TComponent;
  FNm: String;
  F: TField;
begin
  if FDuplicateFlag then Exit;

  if DataSet.Tag > 0 then MasterSetModified;

  FInserting := True;

  DSR := GetDataSet(DataSet.Tag)^;
  Fm := DSR.Form;
  DataSet['id'] := DBase.GenId('gen_' + TableStr(Fm.Id));
  if DataSet.Tag > 0 then
    DataSet['pid'] := MasterSet['id'];
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    FNm := FieldStr(C);
    if (C is TdxDBImage) or (not HasFId(C)) then Continue;
    F := DataSet.FieldByName(FNm);
    if (C is TdxDateEdit) and (TdxDateEdit(C).DateNow) and (F.IsNull) then
      F.Value:=Date
    else if (C is TdxTimeEdit) and (TdxTimeEdit(C).CurTime) and (F.IsNull) then
      F.Value := Time
    else if (C is TdxLookupComboBox) and (Fm.GroupField = GetId(C)) and (F.IsNull) then
    begin
      id := ExtractGroupFieldFilterValue(DSR.Filter, GetId(C));
      if id > 0 then
      begin
        F.Value:=id;
        DataSet.FieldByName(FieldStr(C) + 'l').Value := GetObjFieldValue(C, id, True);
      end;
    end;
  end;
  CalcDefVals(DSR);
  DoStateChange;
  FInserting := False;
  if DSR.Form.OnAfterInsert <> nil then DSR.Form.OnAfterInsert(DSR.Form);
end;

procedure TDataSetProcessor.DataSetAfterPost(DataSet: TDataSet);

  procedure ApplyDetails;
  var
    i: Integer;
  begin
    for i := 1 to FItems.Count - 1 do
      DBase.ApplyDataSet(GetDataSet(i)^.DataSet);
  end;

var
  DSR0, DSR: TDataSetRec;
begin
  if FDuplicateFlag then Exit;

  DSR := GetDataSet(DataSet.Tag)^;
  if not DSR.HasFields then Exit;

  if DataSet.Tag = 0 then
  begin
    if FSimpleMode then Exit; // !!!!!!!!!!!!!!!!!!

    DBase.ApplyDataSet(TSQLQuery(DataSet));
    ApplyDetails;
    DBase.Commit;
    if not FReCalculate then
    begin
      DoStateChange;
      if not Testing then MainFr.RefreshAllLookups(FMaster^.Form.Id);
    end;
    if FRecLocked then UnLockRecord;
  end
  else
  begin
    DSR0 := GetDataSet(0)^;
    //DSR := GetDataSet(DataSet.Tag)^;
    if not FReCalculate then
    begin
      CalcAggFields(DSR0, DSR.Form.FormCaption);
      CalcAggExprs(DSR0, DSR.Form.FormCaption);
    end;
  end;

  if DSR.Form.OnAfterPost <> nil then DSR.Form.OnAfterPost(DSR.Form);
end;

procedure TDataSetProcessor.DataSetAfterScroll(DataSet: TDataSet);
var
  i: PtrInt;
  DSR: TDataSetRec;
begin
  if FDuplicateFlag then Exit;

  i := DataSet.Tag;
  DSR := GetDataSet(i)^;
  if i = 0 then
  begin
    RequeryDetails;
    //if (DSR.EditFm = nil) or FPrinting then RequeryDetails;
    if (not Printing) and (not FReCalculate) and (not FDeletingRecs) then
    begin
      DoStateChange;
      if FFm.ViewType <> vtGridOnly then
        ShowImages(DSR.Form);
    end;
  end;
  // Пересчет выполняется если идет печать или нет формы редактирования
  //if (not FReCalculate) and (FPrinting or (DSR.EditFm = nil)) and (not FDeletingRecs) then
  if (not FReCalculate) and (not FDeletingRecs) then
  begin
    RefreshLookupsWithParams(DSR.Form, '');
    RequeryQueries(i);
    CalcExprs(DSR, '', nil);
  end;

  // !!! Доступ
  if (not Printing) and (not FReCalculate) and (not FDeletingRecs) then
    CheckAccess(GetDataSet(i));

  if DSR.Form.OnAfterScroll <> nil then DSR.Form.OnAfterScroll(DSR.Form);
end;

procedure TDataSetProcessor.GridSelectEditor(Sender: TObject; Column: TColumn;
  var Editor: TWinControl);
var
  pLR: PLookupRec;
  DSR: TDataSetRec;
  C: TComponent;
begin
  if (TDBGrid(Sender).ReadOnly) or (TDBGrid(Sender).Parent = nil) then Exit;

  DSR := GetDataSet(TComponent(Sender).Tag)^;
  if DSR.Form.PId > 0 then
  begin
    if not (MasterSet.State in [dsInsert, dsEdit]) then
    begin
      Editor := nil;
      Exit;
    end;
  end;
  // !!! Доступ
  if (DSR.Editing = False) or (DSR.DataSet['id'] = Null) then
  begin
    Editor := nil;
    Exit;
  end;
  //
  if Editor is TPickListCellEditor then
  begin
    pLR := FindLookupByColumn(Column);
    if pLR <> nil then
      with TPickListCellEditor(Editor) do
      begin
        AutoComplete:=True;
        Style := TCustomComboBox(pLR^.Control).Style;
        if pLR^.Control is TdxLookupComboBox then
          OnEditingDone:=@PickListEditingDone;
        Tag := FLookups.IndexOf(pLR);
        if pLR^.NeedRefresh then
        begin
          pLR^.NeedRefresh:=False;
          RefreshComboBox(pLR^);
          Items := Column.PickList;
        end;
      end
    else
      with TPickListCellEditor(Editor) do
      begin
        AutoComplete:=True;
        C := FindById(DSR.Form, Column.Tag);
        if C <> nil then Style := TCustomComboBox(C).Style;
        OnEditingDone := nil;
        Tag := -1;
      end;
  end
  else if Editor is TMaskCellEditor then
  begin
    Editor.SetBounds(0, 0, 100, 100);
  end
  else if Column.Field.IsBlob then Editor := nil
  else
  begin
    C := FindById(DSR.Form, Column.Tag);
    if (C <> nil) and (GetExpression(C) > '') and (not GetEditable(C)) then Editor := nil;
  end;
end;

procedure TDataSetProcessor.LookupMenuClick(Sender: TObject);
var
  MI: TMenuItem;
  i: PtrInt;
  Cbx: TdxLookupComboBox;
  pLR: PLookupRec;
  DS: TSQLQuery;
  C: TComponent;

  procedure CheckEdit;
  begin
    SetDSEdit(Cbx.Field.DataSet);
  end;

begin
  MI := TMenuItem(Sender);
  i := TPopupMenu(MI.Owner).Tag;
  pLR := PLookupRec(FLookups[i]);
  Cbx := TdxLookupComboBox(pLR^.Control);
  if Cbx.DataSource.AutoEdit = False then Exit;
  if (MI.Tag in [6..7]) and (pLR^.DSProc = nil) then
  begin
    pLR^.DSProc := TDataSetProcessor.Create;
    pLR^.DSProc.BindForm(pLR^.TId, True, vtDefault);
    C := pLR^.Control;
    if C is TdxLookupComboBox then
      with TdxLookupComboBox(C) do
      begin
        if OnCreateForm <> nil then OnCreateForm(C, pLR^.DSProc.Form);
      end;
  end;
  case MI.Tag of
    0:
      begin
        if Cbx.Text <> '' then
        begin
          CheckEdit;
          Clipboard.AsText:=Cbx.Text;
          Cbx.KeyValue:=Null;
          Cbx.Field.Value:=Null;
        end;
      end;
    1:
      if Cbx.Text <> '' then
        Clipboard.AsText := Cbx.Text;
    2:
      begin
        if Clipboard.AsText <> '' then
        begin
          if pLR^.NeedRefresh then
          begin
            RefreshComboBox(pLR^);
            pLR^.NeedRefresh:=False;
          end;
          CheckEdit;
          Cbx.ItemIndex := Cbx.Items.IndexOf(Clipboard.AsText);
          Cbx.EditingDone;
          if Cbx.CanFocus then Cbx.SetFocus;
        end;
      end;
    4:
      begin
        CheckEdit;
        Cbx.KeyValue:=Null;
        Cbx.Field.Value:=Null;
      end;
    6:
      begin
        pLR^.DSProc.OpenRecord(0);
        if pLR^.DSProc.AppendObject = mrOk then
        begin
          DS := pLR^.DSProc.MasterSet;
          CheckEdit;
          Cbx.KeyValue := DS['id'];
          Cbx.Field.Value := DS.FieldByName(FieldStr(Cbx.SourceFId)).Value;
          if not Testing then MainFr.RefreshAllLookups(Cbx.SourceTId);
          if Cbx.CanFocus then Cbx.SetFocus;
        end;
      end;
    7:
      begin
        if Cbx.KeyValue <> Null then
        begin
          if pLR^.DSProc.OpenRecord(Cbx.KeyValue) then
          begin
            if pLR^.DSProc.EditObject = mrOk then
            begin
              DS := pLR^.DSProc.MasterSet;
              CheckEdit;
              Cbx.KeyValue := DS['id'];
              Cbx.Field.Value := DS.FieldByName(FieldStr(Cbx.SourceFId)).Value;
              if not Testing then MainFr.RefreshAllLookups(Cbx.SourceTId);
              if Cbx.CanFocus then Cbx.SetFocus;
            end;
          end;
        end;
      end;
    9:
      begin
        if (Cbx.KeyValue <> Null) and (not Testing) then
          MainFr.GotoRec(Cbx.SourceTId, Cbx.KeyValue);
      end;
  end;
end;

procedure TDataSetProcessor.LookupMenuPopup(Sender: TObject);
var
  Pop: TPopupMenu;
  LR: TLookupRec;
  Cbx: TdxLookupComboBox;
  ae: Boolean;
begin
  Pop := TPopupMenu(Sender);
  LR := PLookupRec(FLookups[Pop.Tag])^;
  Cbx := TdxLookupComboBox(LR.Control);
  ae := Cbx.DataSource.AutoEdit;
  Pop.Items[0].Enabled:=(Cbx.Text<>'') and (not Cbx.ReadOnly) and ae;
  Pop.Items[1].Enabled := Cbx.Text <> '';
  Pop.Items[2].Enabled := (Clipboard.AsText <> '') and (not Cbx.ReadOnly) and ae;
  Pop.Items[4].Enabled := (Cbx.Text <> '') and (not Cbx.ReadOnly) and ae;
  Pop.Items[7].Enabled := Cbx.KeyValue <> Null;
  Pop.Items[9].Enabled := Cbx.KeyValue <> Null;
end;

procedure TDataSetProcessor.MemoButtonClick(Sender: TObject);
var
  pLR: PLookupRec;
  Fm: TListWindow;
  C: TdxMemo;
begin
  pLR := PLookupRec(FLookups[TComponent(Sender).Tag]);
  C := TdxMemo(pLR^.Control);
  try
    PrepareListForm(pLR);
  except
    on E: EFilterParserError do
    begin
      ErrMsg(E.Message);
      Exit;
    end;
  end;
  Fm := pLR^.ListFm;
  Fm.DestForm := nil;
  Fm.DestMemo := C;
  Fm.Caption:=C.FieldName;
  Fm.ShowForm;
  if C.CanFocus then C.SetFocus;
end;

procedure TDataSetProcessor.PickListEditingDone(Sender: TObject);
var
  i, idx: Integer;
  LR: TLookupRec;
  Cbx: TPickListCellEditor;
begin
  Cbx := TPickListCellEditor(Sender);
  i := Cbx.ItemIndex;
  if (i < 0) and (Cbx.Text <> '') then
  	i := Cbx.Items.IndexOf(Cbx.Text);
  if i < 0 then Cbx.Text := '';
  idx := Cbx.Tag;
  LR := PLookupRec(FLookups[idx])^;
  with TdxLookupComboBox(LR.Control) do
    if Field.DataSet.State in [dsInsert, dsEdit] then
    begin
      if (i < 0) or (Utf8CompareText(Text, Cbx.Text) <> 0) then Text := '';
      ItemIndex:=i;
      EditingDone;
    end;
end;

procedure TDataSetProcessor.PopupMnu(Sender: TObject);
begin
  UpdatePopupState(GetDataSet(TComponent(Sender).Tag)^);
end;

procedure TDataSetProcessor.QueryAfterScroll(DataSet: TDataSet);
var
  Q: PQueryRec;
  S: String;
begin
  Q := PQueryRec(FQueries[DataSet.Tag]);
  //S := ReportMan.FindReport(Q^.Grid.Id).Name;
  S := Q^.RD.Name;
  RequeryLinkedQueries(Q, S);
  UpdateQueryPopupState(Q^);
  with Q^.Grid do
    if OnAfterScroll <> nil then OnAfterScroll(Q^.Grid);
end;

procedure TDataSetProcessor.QueryGridButtonClick(Sender: TObject;
  Bn: TGridButtonType);
var
  Pop: TPopupMenu;
begin
  Pop := TdxQueryGrid(Sender).PopupMenu;
  case Bn of
    gbnAppend: Pop.Items[0].Click;
    gbnEdit: Pop.Items[1].Click;
    gbnDelete: Pop.Items[2].Click;
    gbnGoto: Pop.Items[4].Click;
    gbnRefresh: Pop.Items[6].Click;
  end;
end;

procedure TDataSetProcessor.QueryGridCanSort(Sender: TObject; Index: Integer;
  var Cancel: Boolean);
var
  S: String;
begin
  {S := UpperCase(TdxQueryGrid(Sender).Columns[index-1].FieldName);
  if Copy(S, 1, 2) = 'CF' then
  begin
    ErrMsg(rsCantSortCol);
    Cancel := True;
  end;}
end;

procedure TDataSetProcessor.QueryGridDblClick(Sender: TObject);
var
  i: PtrInt;
  Q: TQueryRec;
  G: TdxQueryGrid;
  P: types.TPoint;
begin
  G := TdxQueryGrid(Sender);
  P := G.ScreenToClient(Mouse.CursorPos);
  P := G.MouseToCell(P);
  if P.y = 0 then Exit;

  i :=G.Tag;
  Q := PQueryRec(FQueries[i])^;
  if Q.Simple then
    if (Q.DataSet.Fields[0].IsNull = False) and Q.Popup.Items[1].Visible then
    begin
      //Q.Popup.Items[1].Enabled := True;
      Q.Popup.Items[1].Click;
    end;
end;

procedure TDataSetProcessor.QueryGridSortChange(Sender: TObject);
var
  G: TdxQueryGrid;
  i: Integer;
  RD: TReportData;
  CD: TSortColData;
  Col: TRpGridColumn;
begin
  G := TdxQueryGrid(Sender);
  //RD := ReportMan.FindReport(G.Id);
  RD := Queries[G.Tag]^.RD;
  RD.Grid.SortCols.Clear;
  for i := 0 to G.SortCols.Count - 1 do
  begin
    CD := G.SortCols[i];
    Col := RD.Grid.FindColumnByFieldName(TColumn(CD.Col).FieldName);
    RD.Grid.SortCols.AddCol(Col, CD.Desc);
  end;
  RequeryQuery(G.Tag);
end;

procedure TDataSetProcessor.QueryMenuHandler(Sender: TObject);
var
  MI: TMenuItem;
  i, mr, Qi: Integer;
  pQ: PQueryRec;
  DSP: TDataSetProcessor;
  DS: TSQLQuery;
  RD: TReportData;
  DSR: TDataSetRec;
  rslt: Boolean;

  procedure RefreshQuery;
  var
    Key: Variant;
    //RD: TReportData;
  begin
    Key := Null;
    if DS.Fields.Count = 0 then Exit;
    if AnsiLowerCase(DS.Fields[0].FieldName) = 'id' then
      Key := DS.Fields[0].Value;
    RequeryQuery(Qi);
    if Key <> Null then
      DS.Locate('id', Key, []);
    //RD := ReportMan.FindReport(pQ^.Grid.Id);
    //CalcAggFields(GetDataSet(pQ^.DSRi)^, RD.Name);
    UpdateQueryPopupState(pQ^);
  end;

begin
  MI := TMenuItem(Sender);
  Qi := TPopupMenu(MI.Owner).Tag;
  pQ := PQueryRec(FQueries[Qi]);
  if MI.Tag in [0..2] then
  begin
    if pQ^.DSProc = nil then
    begin
      //RD := ReportMan.FindReport(pQ^.Grid.Id);
      RD := pQ^.RD;
      pQ^.DSProc := TDataSetProcessor.Create;
      pQ^.DSProc.BindForm(StrToInt(RD.Sources[0]^.Id), False, vtGridOnly);
      pQ^.DSProc.Form.Grid.ReadOnly := True;
      with pQ^.Grid do
        if OnCreateForm <> nil then OnCreateForm(pQ^.Grid, pQ^.DSProc.Form);
    end;
  end;
  DSP := pQ^.DSProc;
  DS := pQ^.DataSet;
  case MI.Tag of
    0:
      begin
        i := pQ^.DSRi;
        DSR := GetDataSet(i)^;
        mr := mrYes;
        if (DSR.DataSet.State = dsInsert) and (FFm.ViewType <> vtSimpleForm) then
        begin
          mr := MessageDlg(rsWarning, rsSaveRecMsg, mtConfirmation, [mbYes, mbNo], 0);
          if (mr = mrYes) and Validate(i) then
          begin
            DSR.DataSet.Post;
            DSR.DataSet.Edit;
          end
          else mr := mrNo;
        end;
        if mr = mrYes then
        begin
          DSP.OpenRecord(0);
          DSP.AppendRecord(FFm, DSR.Form, GetDataSet(0)^.DataSet, DSR.DataSet, pQ^.Grid.Id);
          RefreshQuery;
        end;
      end;
    1:
      begin
        if DS.Fields[0].IsNull = False then
        begin
          if DSP.OpenRecord(DS.Fields[0].AsInteger) then
          begin
            DSP.Edit;
            RefreshQuery;
          end;
        end;
      end;
    2:
      begin
        if DS.Fields[0].IsNull = False then
        begin
          rslt := DSP.OpenRecord(DS.Fields[0].AsInteger);
          if not DSP.CanDelete then
            ErrMsg(rsCantDelRec)
          else
          begin
            if rslt then DSP.Delete;
            RefreshQuery;
          end;
        end;
      end;
    4:
      if DS.Fields[0].IsNull = False then
        QueryGotoRec(pQ^, DS.Fields[0].AsInteger);
    6: RefreshQuery;
  end;
end;

procedure TDataSetProcessor.QueryGotoRec(aQ: TQueryRec; aId: Integer);
var
  RD: TReportData;
begin
  if (aId > 0) and (not Testing) then
  begin
    //RD := ReportMan.FindReport(aQ.Grid.Id);
    RD := aQ.RD;
    MainFr.GotoRec(StrToInt(RD.Sources[0]^.Id), aId);
  end;
end;

procedure TDataSetProcessor.UpdateQueryPopupStates;
var
  i: Integer;
begin
  for i := 0 to FQueries.Count - 1 do
    UpdateQueryPopupState(PQueryRec(FQueries[i])^);
end;

procedure TDataSetProcessor.UpdateQueryPopupState(Q: TQueryRec);
var
  Pop: TPopupMenu;
  b, bedit: Boolean;
  Bns: TGridButtons;
  DSR: TDataSetRec;
begin
  Pop := Q.Popup;
  if Q.Simple then
  begin
    DSR := GetDataSet(Q.DSRi)^;
    b := (Q.DataSet.Active) and (not Q.DataSet.Fields[0].IsNull);
    bedit := DSR.DataSet.State in [dsInsert, dsEdit];
    Pop.Items[0].Enabled := bedit;
    Pop.Items[1].Enabled:=b and bedit;
    Pop.Items[2].Enabled := b and bedit;
    //if FGotoEnable and (FFm.ViewType <> vtGridOnly) then
    Pop.Items[4].Enabled := b;
    Bns := Q.Grid.Buttons;
    Bns.EnableButton(gbnAppend, bedit);
    Bns.EnableButton(gbnEdit, b and bedit);
    Bns.EnableButton(gbnDelete, b and bedit);
    Bns.EnableButton(gbnGoto, b);
  end;
end;

procedure TDataSetProcessor.ExchangeRows(DS: TDataSet; MoveUp: Boolean);
var
  Tmp, Tmp2: LongInt;
  US1, US2: TUpdateStatus;
  AftScr: TDataSetNotifyEvent;
  Ok: Boolean;
begin
  AftScr := DS.AfterScroll;
  DS.AfterScroll := nil;
  US1 := DS.UpdateStatus;
  Tmp := DS.Fields[0].AsInteger;
  if MoveUp then DS.Prior
  else DS.Next;
  US2 := DS.UpdateStatus;
  if ((US1 = usInserted) or (US2 = usInserted)) and (US1 <> US2) then
  begin
    if MoveUp then DS.Next
    else DS.Prior;
    Ok := False;
    if MessageDlg(rsWarning, rsExchgOldNewRow, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Ok := Validate(0);
      if Ok then
      begin
        MasterSet.Post;
        MasterSet.Edit;
      end;
    end;
    DS.AfterScroll := AftScr;
    if Ok then ExchangeRows(DS, MoveUp);
    Exit;
  end;
  Tmp2 := DS.Fields[0].AsInteger;
  DS.Edit;
  DS.Fields[0].AsInteger := Tmp;
  DS.Post;
  if MoveUp then DS.Next
  else DS.Prior;
  DS.Edit;
  DS.Fields[0].AsInteger := Tmp2;
  DS.Post;
  DS.AfterScroll:=AftScr;
end;

{procedure CopyRec(DS: TDataSet; var Buf: array of Variant);
var
  i: Integer;
begin
  for i := 0 to DS.Fields.Count - 1 do
    Buf[i] := DS.Fields[i].Value;
end;

procedure PasteRec(DS: TDataSet; Buf: array of Variant; aId: Variant);
var
  i: Integer;
  FieldChange: TFieldNotifyEvent;
begin
  DS.Edit;
  for i := 0 to DS.Fields.Count - 1 do
  begin
    FieldChange := DS.Fields[i].OnChange;
    DS.Fields[i].OnChange:=nil;
    DS.Fields[i].Value := Buf[i];
    DS.Fields[i].OnChange:=FieldChange;
    DS.Fields[0].Value := aId;
  end;
  DS.Post;
end;

procedure MoveRowUp(DS: TDataSet);
var
  AftInsert, AftScroll, AftPost: TDataSetNotifyEvent;
  Tmp, Tmp2: array of Variant;
begin
  AftInsert := DS.AfterInsert;
  AftScroll := DS.AfterScroll;
  AftPost := DS.AfterPost;
  DS.AfterInsert := nil;
  DS.AfterScroll := nil;
  DS.AfterPost := nil;
  SetLength(Tmp, DS.Fields.Count);
  SetLength(Tmp2, DS.Fields.Count);
  CopyRec(DS, Tmp);
  DS.Prior;
  CopyRec(DS, Tmp2);
  PasteRec(DS, Tmp, Tmp2[0]);
  DS.Next;
  PasteRec(DS, Tmp2, Tmp[0]);
  DS.AfterInsert := AftInsert;
  DS.AfterScroll := AftScroll;
  DS.AfterPost := AftPost;
  DS.Prior;
end;

procedure MoveRowDown(DS: TDataSet);
var
  AftInsert, AftScroll, AftPost: TDataSetNotifyEvent;
  Tmp, Tmp2: array of Variant;
begin
  AftInsert := DS.AfterInsert;
  AftScroll := DS.AfterScroll;
  AftPost := DS.AfterPost;
  DS.AfterInsert := nil;
  DS.AfterScroll := nil;
  DS.AfterPost := nil;
  SetLength(Tmp, DS.Fields.Count);
  SetLength(Tmp2, DS.Fields.Count);
  CopyRec(DS, Tmp);
  DS.Next;
  CopyRec(DS, Tmp2);
  PasteRec(DS, Tmp, Tmp2[0]);
  DS.Prior;
  PasteRec(DS, Tmp2, Tmp[0]);
  DS.AfterInsert := AftInsert;
  DS.AfterScroll := AftScroll;
  DS.AfterPost := AftPost;
  DS.Next;
end;   }

procedure TDataSetProcessor.MenuItemClick(Sender: TObject);
var
  MI: TMenuItem;
  DSR: TDataSetRec;
  i: PtrInt;
  SD: TShopData;
  pLR: PLookupRec;
  Fm: TListWindow;
begin
  MI := TMenuItem(Sender);
  i := TPopupMenu(MI.Owner).Tag;
  DSR := GetDataSet(i)^;
  DSR.Grid.EditorMode:=False;  // избегаем ошибки EDatabaseError (not Edit, Insert state)
  if i = 0 then
  begin
    if (i in [0, 4, 8, 9]) and (not Validate(i)) then Exit;
    case MI.Tag of
      0: if CanAdd then Append;
      1: Edit;
      2: if CanDelete then Delete;
      4: ApplyQuickFilter;
      8: Duplicate;
      9: DuplicateAll;
    end
  end
  else
  begin
    if (MI.Tag in [0, 6, 8, 10, 11]) and (not Validate(i)) then Exit;
    if MI.Tag = 0 then
    begin
      DSR.DataSet.Append;
      if not IsTableInput(@DSR) then
	      ShowEditForm(i)
    end
    else if MI.Tag = 1 then
    begin
      if (DSR.Editing) and (FMaster^.Editing) then
        DSR.DataSet.Edit;
      UpdatePopupState(DSR);
      if not IsTableInput(@DSR) then
	      ShowEditForm(i)
    end
    else if MI.Tag = 2 then
    begin
      if (DSR.DataSet['id'] <> Null) and ConfirmDelete then
        DSR.DataSet.Delete;
    end
    else if (MI.Tag = 6) and (MI.Visible) then
    begin
      SD := DSR.Form.ShopData;
      pLR := FindLookupById(SD.ObjId);
      PrepareListForm(pLR);
      Fm := pLR^.ListFm;
      Fm.DestForm := DSR.Form;
      Fm.DestDS := DSR.DataSet;
      Fm.ValueField:=FieldStr(GetSourceFId(pLR^.Control));
      Fm.DestMemo := nil;
      Fm.DSProc := Self;
      Fm.DSRi := i;
      Fm.Caption := '';
      Fm.ShowForm;
    end
    else if MI.Tag = 8 then
    begin
      try
        DSR.Form.BeginDuplicate;
        DuplicateRec(DSR);
      finally
        DSR.Form.EndDuplicate;
      end;
      if not IsTableInput(@DSR) then
      	ShowEditForm(i)
    end
    else if MI.Tag = 10 then ExchangeRows(DSR.DataSet, True)
    else if MI.Tag = 11 then ExchangeRows(DSR.DataSet, False)
  end;
  UpdateControlState(DSR);
end;

// Для UpdateControlState
procedure TDataSetProcessor.AddEditingCtrls(DSRi: Integer);
var
  i: Integer;
  C: TComponent;
  Bn: TSpeedButton;
  L: TList;
  DSR: PDataSetRec;
begin
  DSR := GetDataSet(DSRi);
  L := TList.Create;
  DSR^.EditingCtrls := L;
  for i := 0 to DSR^.Form.ComponentCount - 1 do
  begin
    C := DSR^.Form.Components[i];
    if (not HasFId(C)) or (C is TdxFile) then Continue;
    Bn := GetEditButton(C);
    if (Bn <> nil) and Bn.Enabled then L.Add(Bn);
  end;
end;

procedure TDataSetProcessor.ClearItems;
var
  pDS: PDataSetRec;
begin
  while FItems.Count > 0 do
  begin
    pDS := PDataSetRec(FItems[0]);
    pDS^.RunScript.Free;
    pDS^.Form.Free;
    pDS^.Colors.Free;
    FreeAndNil(pDS^.EditFm);
    pDS^.ChkExprList.Free;
    pDS^.DefValList.Free;
    pDS^.LblExprList.Free;
    pDS^.ExprList.Free;
    pDS^.Err.Free;
    pDS^.DataSet.Free;
    pDS^.DataSource.Free;
    FreeAndNil(pDS^.FilterFm);
    FreeAndNil(pDS^.Popup);
    FreeAndNil(pDS^.EditCond);
    FreeAndNil(pDS^.DelCond);
    FreeAndNil(pDS^.EditingCtrls);
    FreeAndNil(pDS^.Filter);
    Dispose(pDS);
    FItems.Delete(0);
  end;
end;

procedure TDataSetProcessor.BindControls(DSR: TDataSetRec);
var
  i: Integer;
  C: TComponent;
  Fm: TdxForm;
  DS: TDataSource;
  Col: TColumn;
  Bn: TSpeedButton;
begin
  Fm := DSR.Form;
  DS := DSR.DataSource;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    // !!! Доступ
    if not ((C is TdxLabel) or (C is TSpeedButton) or (C is TdxShape)) then
    begin
      if not UserMan.CheckControlVisible(Fm.Id, C.Name) then
      begin
        if C is TdxTabSheet then
          TdxTabSheet(C).TabVisible := False
        else
          MaskingControl(C.Owner, TControl(C));
        if HasFId(C) then
        begin
          Col := FindGridColumn(DSR.Grid, GetId(C));
          TestNil(Col, 'BindControls: Col=nil (visible)');
          Col.Visible := False;
        end;
      end;
    end;
    //
    if C is TdxButton then TDxButton(C).OnClick:=@ButtonClick;
    if not HasFId(C) then Continue;

    SetDataSource(C, DS);
    if C is TdxLookupComboBox then
    begin
      SetDataField(C, FieldStr(C) + 'l');
      TdxLookupComboBox(C).KeyField := FieldStr(C);
      TdxLookupComboBox(C).OnKeyDown:=@LCbxKeyDown;
    end
    else if not (C is TdxFile) then SetDataField(C, FieldStr(C))
    // Это TdxFile
    else SetDataField(C, FieldStr(C) + 'd');

    if HasExpression(C) and (Trim(GetExpression(C)) <> '') then
    begin
      SetReadOnly(C, not GetEditable(C));
    end;

    if (Fm.PId = 0) and (C is TdxLookupComboBox) and (FGotoEnable) and (FFm.ViewType <> vtGridOnly) then
      TdxLookupComboBox(C).OnCtrlClick:=@LookupCtrlClick;

    if HasMaxLength(C) then SetMaxLength(C, GetFieldSize(C));

    if C is TdxCheckBox then
      TdxCheckBox(C).TabStop:=True
    else if C is TdxObjectField then
    begin
      TdxObjectField(C).ReadOnly:=True;
      Col := FindGridColumn(DSR.Grid, GetId(C));
      Col.ReadOnly := True;
    end
    else if (C is TdxLookupComboBox) and (GetReadOnly(C)) then
      with TdxLookupComboBox(C) do
      begin
        Style:=csSimple;
        Button.Enabled:=False;
      end;

    // !!! Доступ
    if not UserMan.CheckControlEditing(Fm.Id, C.Name) then
    begin
      if C is TdxDBImage then TdxDBImage(C).ReadOnly := True
      else if HasReadOnly(C) then SetReadOnly(C, True);
      Bn := GetEditButton(C);
      if (Bn <> nil) and (not (C is TdxFile)) then Bn.Enabled := False;
      Col := FindGridColumn(DSR.Grid, GetId(C));
      TestNil(Col, 'BindControls: Col=nil (editing)');
      Col.ReadOnly := True;
    end;
    //
  end;
end;

// При компиляции выражений учитывается следующее: для полей недопустимо
// обращаться к надписям.
procedure TDataSetProcessor.BuildExprs(DSR: PDataSetRec);
var
  Fm: TdxForm;
  C: TComponent;
  S, SS: String;
  EB: TExpressionBuilder;
  i: Integer;
  L, LL: TExprList;
  E: TExpression;
begin
  L:=TExprList.Create;
  LL := TExprList.Create;
  DSR^.ExprList := L;
  DSR^.LblExprList := LL;
  Fm := DSR^.Form;
  EB := TExpressionBuilder.Create;
  EB.Form := Fm;
  EB.ParentForm := GetDataSet(0)^.Form;
  EB.DataSet := DSR^.DataSet;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if HasExpression(C) then
    {if (C is TdxCalcEdit) or (C is TdxDateEdit) or (C is TdxLabel) or
      (C is TdxTimeEdit) or (C is TdxEdit) then}
    begin
      S := Trim(GetExpression(C));
      if S > '' then
      try
        if C is TdxLabel then
        begin
          EB.SkipLabels:=False;
          E := EB.Build(S);
          if E <> nil then
            LL.AddExpr(C, E)
        end
        else
        begin
          EB.SkipLabels := True;
          E := EB.Build(S);
          if E <> nil then
            L.AddExpr(C, E);
        end;
      except
        on Ex: Exception do
        begin
          if C is TdxLabel then S := TdxLabel(C).FieldName
          else S := GetFieldName(C);
          DSR^.Err.AddError(S, Ex.Message);
        end;
      end;
    end;
  end;
  // !!! Доступ. Условие на редактирование
  DSR^.EditCond := nil;
  DSR^.DelCond := nil;
  S := ''; SS := '';
  if DSR^.Editing then
    S := Trim(UserMan.GetEditCond(DSR^.Form.Id));
  if DSR^.Deleting and DSR^.Editing then
    SS := Trim(UserMan.GetDelCond(DSR^.Form.Id));
  if (S <> '') or (SS <> '') then
  begin
    EB.SkipLabels:=False;
    try
      if S <> '' then
        DSR^.EditCond := EB.Build(S);
      if SS <> '' then
        DSR^.DelCond := EB.Build(SS);
    except
      on E: Exception do
        ErrMsg(E.Message);
    end;
  end;
  //
  EB.Free;
end;

procedure TDataSetProcessor.BuildDefVals(DSR: PDataSetRec);
var
  Fm: TdxForm;
  C: TComponent;
  S: String;
  EB: TExpressionBuilder;
  i: Integer;
  L: TExprList;
  E: TExpression;
begin
  L:=TExprList.Create;
  DSR^.DefValList := L;
  Fm := DSR^.Form;
  EB := TExpressionBuilder.Create;
  EB.Form := Fm;
  EB.ParentForm := GetDataSet(0)^.Form;
  EB.DataSet := DSR^.DataSet;
  EB.SkipLabels:=True;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    //if (C is TdxCalcEdit) or (C is TdxComboBox) or (C is TdxLookupComboBox) then
    if HasDefaultValue(C) then
    begin
      S := Trim(GetDefaultValue(C));
      if S > '' then
      try
        E := EB.Build(S);
        if E <> nil then
          L.AddExpr(C, E);
      except
        on Ex: Exception do
          DSR^.Err.AddError(GetFieldName(C) + ' - ' + rsDefaultValue, Ex.Message);
      end;
    end;
  end;
  EB.Free;
end;

procedure TDataSetProcessor.BuildCheckExprs(DSR: PDataSetRec);
var
  Fm: TdxForm;
  C: TComponent;
  S: String;
  EB: TExpressionBuilder;
  i: Integer;
  L: TExprList;
  E: TExpression;
begin
  L:=TExprList.Create;
  DSR^.ChkExprList := L;
  Fm := DSR^.Form;
  EB := TExpressionBuilder.Create;
  EB.Form := Fm;
  EB.ParentForm := GetDataSet(0)^.Form;
  EB.DataSet := DSR^.DataSet;
  EB.SkipLabels:=True;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if HasCheckExpression(C) then
    begin
      S := Trim(GetCheckExpression(C));
      if S > '' then
      try
        E := EB.Build(S);
        if E <> nil then
          L.AddExpr(C, E);
      except
        on Ex: Exception do
          DSR^.Err.AddError(GetFieldName(C) + ' - ' + rsCheckValue, Ex.Message);
      end;
    end;
  end;
  EB.Free;
end;

procedure TDataSetProcessor.AddDataSet(aGrid: TdxGrid);
var
  DataSrc: TDataSource;
  DataSet: TSQLQuery;
  Fm: TdxForm;
  pDS: PDataSetRec;
  SQL: String;
  i: Integer;
  Pop: TPopupMenu;
  C: TComponent;
begin
  DataSet := TSQLQuery.Create(nil);
  DataSet.ParseSQL := False;
  DataSet.AfterOpen:=@DataSetAfterOpen;
  DataSet.AfterInsert:=@DataSetAfterInsert;
  DataSet.AfterEdit:=@DataSetAfterEdit;
  DataSet.AfterDelete:=@DataSetAfterDelete;
  DataSet.AfterScroll:=@DataSetAfterScroll;
  DataSet.AfterCancel := @DataSetAfterCancel;
  DataSet.AfterPost := @DataSetAfterPost;
  DataSet.AfterClose:=@DataSetAfterClose;
  DataSet.BeforeOpen:=@DataSetBeforeOpen;
  DataSet.BeforeInsert:=@DataSetBeforeInsert;
  DataSet.BeforeEdit:=@DataSetBeforeEdit;
  DataSet.BeforeDelete:=@DataSetBeforeDelete;
  DataSet.BeforeScroll:=@DataSetBeforeScroll;
  DataSet.BeforeCancel:=@DataSetBeforeCancel;
  DataSet.BeforeClose:=@DataSetBeforeClose;
  DataSet.BeforePost:=@DataSetBeforePost;
  DBase.AttachDataSet(DataSet);
  DataSrc := TDataSource.Create(nil);
  DataSrc.DataSet := DataSet;
  if aGrid.Id = 0 then
    Fm := FFm
  else
    Fm := FormMan.LoadForm(aGrid.Id);
  {if Fm <> FFm then
    Fm.ParentForm := FFm;}
  Fm.DataSet := DataSet;
  SQL := SQLSelectStatement(Fm);
  DataSet.SQL.Text := SQL;
  DataSet.InsertSQL.Text := SQLInsertStatement(Fm);
  DataSet.UpdateSQL.Text := SQLUpdateStatement(Fm);
  DataSet.DeleteSQL.Text := SQLDeleteStatement(Fm);
  aGrid.DataSource := DataSrc;
  aGrid.Form := Fm;
  //if not aGrid.ReadOnly then
    aGrid.OnSelectEditor:=@GridSelectEditor;
  //else
    aGrid.OnDblClick:=@GridDblClick;
  aGrid.OnCanSort:=@GridCanSort;
  aGrid.OnSortColumnChange:=@GridSortColumnChange;
  aGrid.OnDrawColumnCell:=@GridDrawColumnCell;
  aGrid.OnCellClick:=@GridCellClick;
  aGrid.OnVaidate:=@GridValidate;
  aGrid.OnKeyDown:=@GridKeyDown;
  // !!! Доступ
  //if (Fm.PId > 0) and (UserMan.CheckFmAdding(Fm.Id)) then aGrid.OnKeyDown:=@GridKeyDown;
  //
  Pop := CreatePopupMenu(aGrid.Id = 0, aGrid.ReadOnly, Fm.HasShop);
  Pop.OnPopup:=@PopupMnu;
  aGrid.PopupMenu := Pop;
  aGrid.Options:= aGrid.Options - [dgTabs];

  New(pDS);
  i := FItems.Add(pDS);
  pDS^.DataSource := DataSrc;
  pDS^.DataSet := DataSet;
  pDS^.HasFields := IsFieldExist(Fm);
  pDS^.SQL:=SQL;
  pDS^.Form := Fm;
  pDS^.Grid := aGrid;
  pDS^.Popup := Pop;
  pDS^.FilterFm := TFilterFm.Create(nil);
  pDS^.FilterFm.Form := Fm;
  pDS^.EditFm := nil;
  pDS^.Err := TCalcError.Create(nil);
  pDS^.Err.Parent := Fm;
  pDS^.Err.Visible := False;
  pDS^.Filter := TFilterObject.Create(Fm);
  if (Fm.ViewType = vtGridOnly) or (Fm.PId > 0) then
  begin
    pDS^.EditFm := TEditWindow.CreateNew(nil);
    pDS^.EditFm.Form := Fm;
    pDS^.EditFm.DataSet := DataSet;
    pDS^.EditFm.DSP := Self;
    pDS^.EditFm.DSRi:=i;
  end;
  if (Fm.Filters.Count > 0) and (not FIsListForm) then
  begin
    pDS^.Filter.Load(Fm.Filters.ValueFromIndex[0]);
    pDS^.FilterIndex := 0;
  end
  else pDS^.FilterIndex := -1;
  pDS^.TblFilterSet := False;
  pDS^.Colors := TColorList.Create;

  Fm.Tag := i;
  DataSet.Tag:=i;
  aGrid.Tag:=i;
  aGrid.PopupMenu.Tag := i;
  if (Fm.PId > 0) and (aGrid.ShowButtons) then
  begin
    aGrid.OnButtonClick:=@GridButtonClick;
    //aGrid.Buttons.ShowButton(gbnShopping, Fm.HasShop);
  end;

  BindControls(pDS^);

  // !!! Доступ
  if UserMan.CheckFmVisible(Fm.Id) = False then MaskingControl(Fm, aGrid);
  pDS^.Adding:=UserMan.CheckFmAdding(Fm.Id);
  pDS^.Editing:=UserMan.CheckFmEditing(Fm.Id);
  pDS^.Deleting:=UserMan.CheckFmDeleting(Fm.Id);
  //pDS^.Exporting:=UserMan.CheckFmExporting(Fm.Id);
  if Fm.HasShop then
  begin
    C := FindById(Fm, Fm.ShopData.ObjId);
    pDS^.CanShoping := UserMan.CheckFmVisible(GetSourceTId(C));
  end
  else
    pDS^.CanShoping:=False;
  //
  BuildExprs(pDS);
  BuildDefVals(pDS);
  BuildCheckExprs(pDS);
  // !!! Скрипты
  Fm.DSP := Self;
  Fm.DSR := pDS;
  Fm.DSRi := i;
  pDS^.RunScript := TRunScript.Create;
end;

function TDataSetProcessor.GetCanAdd: Boolean;
begin
  Result := FMaster^.Adding;
end;

function TDataSetProcessor.GetCanDelete: Boolean;
begin
  Result := FMaster^.Deleting;
end;

function TDataSetProcessor.GetCanEdit: Boolean;
begin
  Result := FMaster^.Editing;
end;

function TDataSetProcessor.GetDataSet(Index: Integer): PDataSetRec;
begin
  Result := PDataSetRec(FItems[Index]);
end;

function TDataSetProcessor.FindDataSet(FmId: Integer): PDataSetRec;
var
  i: Integer;
  pD: PDataSetRec;
begin
  Result := nil;
  for i := 0 to FItems.Count - 1 do
  begin
    pD := GetDataSet(i);
    if pD^.Form.Id = FmId then Exit(pD);
  end;
end;

function TDataSetProcessor.CreatePopupMenu(IsMaster, AllowSpace,
  HasShop: Boolean): TPopupMenu;
var
  scSpace: Integer;
begin
  scSpace := 0;
  if AllowSpace then scSpace := ShortCut(VK_SPACE, []);
  Result := TPopupMenu.Create(nil);
  Result.Items.Add( CreateMenuItem(Result, rsAppend, 0, ShortCut(VK_INSERT, []),
    @MenuItemClick, 'add16') );
  Result.Items.Add( CreateMenuItem(Result, rsEdit, 1, scSpace, @MenuItemClick, 'edit16') );
  Result.Items.Add( CreateMenuItem(Result, rsDelete, 2, ShortCut(VK_DELETE, [ssCtrl]),
    @MenuItemClick, 'delete16') );
  if IsMaster then
  begin
    Result.Items.Add( CreateMenuItem(Result, '-', 7, 0, nil, '') );
    Result.Items.Add( CreateMenuItem(Result, rsDuplicate, 8, ShortCut(VK_D, [ssCtrl]),
      @MenuItemClick, '') );
    Result.Items.Add( CreateMenuItem(Result, rsDuplicateAll, 9, ShortCut(VK_D, [ssCtrl, ssShift]),
      @MenuItemClick, '') );
    Result.Items.Add( CreateMenuItem(Result, '-', 3, 0, nil, '') );
    Result.Items.Add( CreateMenuItem(Result, rsFilter, 4, 0, @MenuItemClick, 'filter16') );
  end
  else
  begin
    Result.Items.Add( CreateMenuItem(Result, '-', 7, 0, nil, '') );
    Result.Items.Add( CreateMenuItem(Result, rsDuplicate, 8, ShortCut(VK_D, [ssCtrl]),
      @MenuItemClick, '') );
    //if HasShop then
    //begin
    Result.Items.Add( CreateMenuItem(Result, '-', 5, 0, nil, '') );
    Result.Items.Add( CreateMenuItem(Result, rsShopping, 6, 0, @MenuItemClick, 'shopping16') );
    //end;
    Result.Items.Add( CreateMenuItem(Result, '-', 9, 0, nil, '') );
    Result.Items.Add( CreateMenuItem(Result, rsMoveUp, 10, 0, @MenuItemClick, 'up16') );
    Result.Items.Add( CreateMenuItem(Result, rsMoveDown, 11, 0, @MenuItemClick, 'down16') );
  end;
end;

function TDataSetProcessor.GetQueries(Index: Integer): PQueryRec;
begin
  Result := PQueryRec(FQueries[Index]);
end;

function SortColumnToField(Gr: TdxGrid): String;
var
  C: TColumn;
begin
  Result := '';
  C := Gr.ColumnFromCol(Gr.SortColumn);
  if C <> nil then
    Result := C.FieldName;
end;

procedure TDataSetProcessor.RequeryDetail(i: Integer);
var
  S, TNm, Cond: String;
  id: String;
begin
  with GetDataSet(i)^ do
  begin
    DataSet.Close;
    TNm := TableStr(Form.Id);
    id := FMaster^.DataSet.FieldByName('id').AsString;
    if id > '' then
      Cond := TNm + '.pid=' + id
    else
      Cond := '1=0';
    S :=  SQL + ' where ' + Cond;
    if Grid.SortCols.Count > 0 then
      S := S + ' order by ' + SortColumnsToSQL(Grid);
    DataSet.SQL.Text := S;
    if Grid.SortCols.Count = 0 then
      DataSet.IndexFieldNames:='id'
    else
      DataSet.IndexFieldNames:='';

    DataSet.Open;
    // Наткнулся на артефакты в гриде, если нет сортировки. Это проявляется в
    // двоении записей.
    if DataSet.IndexFieldNames = 'id' then DataSet.First
    //
    else if DataSet.RecordCount = 0 then DataSet.AfterScroll(DataSet);
  end;
end;

procedure TDataSetProcessor.RequeryDetails;
var
  i: Integer;
begin
  for i := 1 to FItems.Count - 1 do
    RequeryDetail(i);
end;

procedure TDataSetProcessor.CloseDetails;
var
  i: Integer;
begin
  for i := 1 to FItems.Count - 1 do
    GetDataSet(i)^.DataSet.Close;
end;

procedure TDataSetProcessor.AddLookup(aGrid: TdxGrid; C: TComponent;
  DSRi: Integer);
var
  DataSet: TSQLQuery;
  DataSrc: TDataSource;
  pLR: PLookupRec;
  i: Integer;
  Pop: TPopupMenu;
  DSP: TDataSetProcessor;
begin
  DataSet := TSQLQuery.Create(nil);
  DataSet.ParseSQL := False;
  DBase.AttachDataSet(DataSet);
  DataSrc := TDataSource.Create(nil);
  DataSrc.DataSet := DataSet;
  Pop := nil;
  DSP := nil;
  if C is TdxLookupComboBox then
    with TdxLookupComboBox(C) do
    begin
      OnButtonClick:=@LCbxButtonClick;
      OnDropDown:=@LCbxDropDown;
      OnChange:=@LCbxChange;

      // !!! Доступ
      if UserMan.CheckFmVisible(SourceTId) = False then Button.Enabled:=False;
      //

      Pop := TPopupMenu.Create(nil);
      Pop.Items.Add( CreateMenuItem(Pop, rsCut, 0, ShortCut(VK_X, [ssCtrl]),
        @LookupMenuClick, 'cut16') );
      Pop.Items.Add( CreateMenuItem(Pop, rsCopy, 1, ShortCut(VK_C, [ssCtrl]),
        @LookupMenuClick, 'copy16') );
      Pop.Items.Add( CreateMenuItem(Pop, rsPaste, 2, ShortCut(VK_V, [ssCtrl]),
        @LookupMenuClick, 'paste16') );
      Pop.Items.Add( CreateMenuItem(Pop, '-', 3, 0, nil, '') );
      Pop.Items.Add( CreateMenuItem(Pop, rsClear, 4, ShortCut(VK_DELETE, [ssCtrl]),
        @LookupMenuClick, 'delete16') );
      Pop.Items.Add( CreateMenuItem(Pop, '-', 5, 0, nil, '') );
      Pop.Items.Add( CreateMenuItem(Pop, rsAppend, 6, 0, @LookupMenuClick, 'add16') );
      Pop.Items.Add( CreateMenuItem(Pop, rsEdit, 7, 0, @LookupMenuClick, 'edit16') );
      Pop.Items.Add( CreateMenuItem(Pop, '-', 8, 0, nil, '') );
      Pop.Items.Add( CreateMenuItem(Pop, rsGoto, 9, 0, @LookupMenuClick, 'goto16') );
      // !!! Доступ
      Pop.Items[6].Visible := UserMan.CheckFmAdding(SourceTId);
      //
      Pop.Items[8].Visible:=FGotoEnable and (FFm.ViewType <> vtGridOnly);
      Pop.Items[9].Visible:=FGotoEnable and (FFm.ViewType <> vtGridOnly);
      Pop.OnPopup:=@LookupMenuPopup;
      PopupMenu := Pop;
      Button.PopupMenu := Pop;
    end
  else if C is TdxComboBox then
    with TdxComboBox(C) do
    begin
      OnDropDown:=@LCbxDropDown;
      OnChange:=@LCbxChange;
    end
  else if C is TdxMemo then
    with TdxMemo(C) do
    begin
      OnButtonClick:=@MemoButtonClick;
      // !!! Доступ
      if UserMan.CheckFmVisible(SourceTId) = False then Button.Enabled:=False;
      //
    end;
  New(pLR);
  pLR^.Control := C;
  pLR^.DataSet := DataSet;
  pLR^.DataSource := DataSrc;
  pLR^.TId:=GetSourceTId(C);
  pLR^.Column := FindGridColumn(aGrid, GetId(C));
  pLR^.SQL := '';
  pLR^.ListFm := nil;
  pLR^.NeedRefresh := True;
  pLR^.DSRi:=DSRi;
  pLR^.DSProc:=DSP;
  pLR^.Popup := Pop;
  i := FLookups.Add(pLR);
  C.Tag := i;
  if Pop <> nil then Pop.Tag := i;
end;

procedure TDataSetProcessor.AddLookups(aGrid: TdxGrid; DSRi: Integer);
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
begin
  Fm := GetDataSet(DSRi)^.Form;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TCustomComboBox) or (C is TdxMemo) then
    begin
      if (GetSourceTId(C) > 0) and (GetSourceFId(C) > 0) then
        AddLookup(aGrid, C, DSRi);
    end;
  end;
end;

procedure TDataSetProcessor.ClearLookups;
var
  pLR: PLookupRec;
begin
  while FLookups.Count > 0 do
  begin
    pLR := PLookupRec(FLookups[0]);
    FreeAndNil(pLR^.Popup);
    FreeAndNil(pLR^.DSProc);
    pLR^.DataSet.Free;
    pLR^.DataSource.Free;
    FreeAndNil(pLR^.ListFm);
    Dispose(pLR);
    FLookups.Delete(0);
  end;
end;

procedure TDataSetProcessor.ClearQueries;
var
  pQ: PQueryRec;
begin
  while FQueries.Count > 0 do
  begin
    pQ := PQueryRec(FQueries[0]);
    FreeAndNil(pQ^.DSProc);
    pQ^.DataSet.Free;
    pQ^.DataSource.Free;
    pQ^.Popup.Free;
    pQ^.Colors.Free;
    pQ^.RD.Free;
    Dispose(pQ);
    FQueries.Delete(0);
  end;
end;

function TDataSetProcessor.FindLookupByColumn(C: TColumn): PLookupRec;
var
  i: Integer;
  pLR: PLookupRec;
begin
  Result := nil;
  for i := 0 to FLookups.Count - 1 do
  begin
    pLR := PLookupRec(FLookups[i]);
    if pLR^.Column = C then
      Exit(pLR);
  end;
end;

function TDataSetProcessor.FindLookupById(Id: Integer): PLookupRec;
var
  i: Integer;
  pLR: PLookupRec;
begin
  Result := nil;
  for i := 0 to FLookups.Count - 1 do
  begin
    pLR := PLookupRec(FLookups[i]);
    if GetId(pLR^.Control) = Id then
      Exit(pLR);
  end;
end;

constructor TDataSetProcessor.Create;
begin
  FItems := TList.Create;
  FLookups := TList.Create;
  FQueries := TList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval:=2000;
  FTimer.OnTimer:=@TimerTimer;
  FNotif := TPopupNotifier.Create(nil);
  FNotif.Title:=rsError;
end;

destructor TDataSetProcessor.Destroy;
begin
  UnBind;
  FNotif.Free;
  FTimer.Free;
  ClearQueries;
  FQueries.Free;
  ClearLookups;
  FLookups.Free;
  ClearItems;
  FItems.Free;
  inherited Destroy;
end;

procedure FillPickLists(Gr: TdxGrid; Fm: TdxForm);
var
  i, Id: Integer;
  C: TComponent;
begin
  for i := 0 to Gr.Columns.Count - 1 do
  begin
    Id := Gr.Columns[i].Tag;
    C := FindById(Fm, Id);
    if (C <> nil) and (C is TdxComboBox) and (GetSourceFId(C) = 0) then
      Gr.Columns[i].PickList := TdxComboBox(C).Items;
  end;
end;

procedure TDataSetProcessor.BindForm(FmId: Integer; IsListForm: Boolean;
  aViewType: TViewType);
var
  i: Integer;
  C: TComponent;
  SD: TScriptData;
begin
  FFm := FormMan.LoadForm(FmId);
  FIsListForm := IsListForm;
  if IsListForm then FFm.ViewType:=vtGridOnly
  else if aViewType <> vtDefault then FFm.ViewType:=aViewType;

  FSimpleMode := FFm.ViewType = vtSimpleForm;

  SetColumnTitles(FFm.Grid);
  SetGrids;
  AddDataSet(FFm.Grid);
  AddLookups(FFm.Grid, 0);
  AddEditingCtrls(0);
  AddQueries(0);
  if not FFm.Grid.ReadOnly then
    FillPickLists(FFm.Grid, FFm);
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TdxGrid then
    begin
      AddDataSet(TdxGrid(C));
      AddLookups(TdxGrid(C), FItems.Count - 1);
      AddEditingCtrls(FItems.Count - 1);
      AddQueries(FItems.Count - 1);
      if TdxGrid(C).ReadOnly = False then
        FillPickLists(TdxGrid(C), GetDataSet(FItems.Count - 1)^.Form);
    end
  end;
  FMaster := GetDataSet(0);

  for i := 0 to FItems.Count - 1 do
    with GetDataSet(i)^ do
    begin
      SD := ScriptMan.FindScript(Form.Id);
      if SD <> nil then
      begin
        RunScript.SD := SD;
        RunScript.LoadBin;
        RunScript.BindForm(Form);
        RunScript.TryRunProc('Form_Create', []);
      end;
      if MainFm.OnCreateForm <> nil then
        MainFm.OnCreateForm(MainFm, Form);
    end;
end;

procedure TDataSetProcessor.UnBind;
var
  i, j: Integer;
  pQ: PQueryRec;
begin
  for i := FItems.Count - 1 downto 0 do
  begin
    with GetDataSet(i)^ do
    begin
      for j := 0 to FQueries.Count - 1 do
      begin
        pQ := PQueryRec(FQueries[j]);
				if pQ^.DSRi = i then pQ^.DataSet.Close;
      end;
      DataSet.Close;
      if RunScript.SD <> nil then
        RunScript.TryRunProc('Form_Destroy', []);
      if MainFm.OnDestroyForm <> nil then
        MainFm.OnDestroyForm(MainFm, GetDataSet(i)^.Form);
    end;
  end;
end;

// Если дочерняя запись изменилась, то мы должны гарантировать, что также
// изменилась и родительская запись. В противном случае дочерние данные в базу
// не попадут.
procedure TDataSetProcessor.MasterSetModified;
begin
	if not MasterSet.Modified then
  	MasterSet['id'] := MasterSet['id'];
end;

procedure TDataSetProcessor.Open;
var
  S, Flt: String;
begin
  S := FMaster^.SQL;
  Flt := MakeFilter;
  if Flt <> '' then
    S := S + ' where ' + Flt;
  if FMaster^.Grid.SortCols.Count > 0 then
      S := S + ' order by ' + SortColumnsToSQL(FMaster^.Grid);
  FMaster^.DataSet.SQL.Text := S;
  FMaster^.DataSet.Open;
  if (MasterSet.RecordCount = 0) and (FFm.ViewType <> vtSimpleForm) then
  begin
    //MasterSet.BeforeScroll(MasterSet);
    MasterSet.AfterScroll(MasterSet);
  end;
  RefreshLookups(0);
  if FFm.ViewType = vtSimpleForm then Append;
end;

procedure TDataSetProcessor._Open2(const aFilter: String);
var
  S: String;
begin
  S := FMaster^.SQL;
  if aFilter <> '' then
    S := S + ' where ' + aFilter;
  if FMaster^.Grid.SortCols.Count > 0 then
      S := S + ' order by ' + SortColumnsToSQL(FMaster^.Grid);
  FMaster^.DataSet.SQL.Text := S;
  FMaster^.DataSet.Open;
  if MasterSet.RecordCount = 0 then
    MasterSet.AfterScroll(MasterSet);
  RefreshLookups(0);
end;

function TDataSetProcessor.OpenRecord(aKey: Integer): Boolean;
begin
  Result := True;
  FMaster^.DataSet.Close;
  FMaster^.DataSet.SQL.Text := FMaster^.SQL + ' where ' + TableStr(FFm.Id) +
    '.id=' + IntToStr(aKey);
  FMaster^.DataSet.Open;
  RefreshLookups(0);
  if MasterSet.RecordCount = 0 then
  begin
    if aKey = 0 then MasterSet.AfterScroll(MasterSet)
    else
    begin
      Result := False;
      ErrMsg(rsRecDeletedUser);
    end;
  end;
end;

procedure TDataSetProcessor.OpenList(const aFilter: String);
begin
  FListFilter := aFilter;
  Open;
end;

procedure TDataSetProcessor.OpenWhere(const Wh: String);
var
  SQL: String;
begin
  FMaster^.DataSet.Close;
  SQL := FMaster^.SQL;
  if Wh <> '' then SQL := SQL + ' where ' + Wh;
  FMaster^.DataSet.SQL.Text := SQL;
  FMaster^.DataSet.Open;
end;

procedure TDataSetProcessor.Close;
begin
  FMaster^.DataSet.Close;
  CloseDetails;
  DoStateChange;
end;

procedure TDataSetProcessor.Append;
var
  C: TWinControl;
begin
  FMaster^.Grid.EditorMode:=False;
  FMaster^.DataSet.Append;
  if IsTableInput(FMaster) then
  else if FMaster^.EditFm <> nil then
    ShowEditForm(0)
  else if (not (FFm.ViewType in [vtGridOnly, vtSimpleForm])) and (FFm.Grid.ReadOnly) then
  begin
    C := GetTopControl(FFm);
    if (C <> nil) and (C.CanFocus) then C.SetFocus;
  end;
end;

function FindComponentBySrcTId(Fm: TdxForm; TId: Integer): TComponent;
var
  i: Integer;
  C: TComponent;
begin
  Result := nil;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxLookupComboBox) and (GetSourceTId(C) = TId) then
      Exit(C);
  end;
end;

procedure TDataSetProcessor.AppendRecord(aPFm, aFm: TdxForm; aPDS,
  aDS: TDataSet; RpId: Integer);
var
  RD: TReportData;
  Flt: String;
begin
  FMaster^.Grid.EditorMode:=False;
  MasterSet.Append;
  RD := ReportMan.FindReport(RpId);
  Flt := RD.Sources[0]^.Filter;
  with TDSPAppendRecParser.Create do
  try
    Form := aFm;
    ParForm := aPFm;
    DS := aDS;
    SrcForm := FFm;
    SrcDS := MasterSet;
    Parse(Flt);
  finally
    Free;
  end;
  if FMaster^.EditFm <> nil then
    ShowEditForm(0);
end;

function TDataSetProcessor.AppendObject: Integer;
begin
  Result := mrNone;
  FMaster^.DataSet.Append;
  if FMaster^.EditFm <> nil then
    Result := ShowEditForm(0)
end;

procedure TDataSetProcessor.Duplicate;
var
  C: TWinControl;
  i: Integer;
begin
  FMaster^.Grid.EditorMode:=False;
  try
    FMaster^.Form.BeginDuplicate;
    DuplicateRec(FMaster^);
    // Намерено вызываем AfterPost дочерних форм, чтобы обнулить итоговые поля
    for i := 1 to FItems.Count - 1 do
      DataSetAfterPost(GetDataSet(i)^.DataSet);
  finally
    FMaster^.Form.EndDuplicate;
  end;
  if IsTableInput(FMaster) then
  else if FMaster^.EditFm <> nil then
    ShowEditForm(0)
  else if (FFm.ViewType <> vtGridOnly) and (FFm.Grid.ReadOnly) then
  begin
    C := GetTopControl(FFm);
    if (C <> nil) and (C.CanFocus) then C.SetFocus;
  end;
end;

procedure TDataSetProcessor.DuplicateAll;
var
  Tables, Rows, Vals: TList;
  Fm: TdxForm;
  i: Integer;
  DS: TSQLQuery;
  Ctrl: TWinControl;

  procedure CopyRec(aFm: TdxForm; aDS: TDataSet; aCols: TList);
  var
    pV: PVariant;
    j: Integer;
    C: TComponent;
  begin
    for j := 0 to aFm.ComponentCount - 1 do
    begin
      C := aFm.Components[j];
      if (not HasFId(C)) or (C is TdxFile) or (C is TdxDBImage) or (C is TdxCounter) or
        (GetExpression(C) <> '') then Continue;
      New(pV);
      aCols.Add(pV);
      pV^ := aDS.FieldByName(FieldStr(C)).Value;
      if C is TdxLookupComboBox then
      begin
        New(pV);
        aCols.Add(pV);
        pV^ := aDS.FieldByName(FieldStr(C) + 'l').Value;
      end;
    end;
  end;

  procedure CopyGrid(DSR: PDataSetRec; aRows: TList);
  var
    Frm: TdxForm;
    DSet: TDataSet;
    Cols: TList;
  begin
    // !!! Доступ
    if (not DSR^.Adding) or ((not DSR^.Editing) and (DSR^.EditCond = nil)) then Exit;
    Frm := DSR^.Form;
    DSet := DSR^.DataSet;
    DSet.DisableControls;
    DSet.First;
    while not DSet.Eof do
    begin
      // !!! Доступ
      if DSR^.Editing then
      begin
        Cols := TList.Create;
        aRows.Add(Cols);
        CopyRec(Frm, DSet, Cols);
      end;
      //
      DSet.Next;
    end;
    DSet.EnableControls;
  end;

  procedure PasteRec(aFm: TdxForm; aDS: TDataSet; aCols: TList);
  var
    j, n: Integer;
    C: Tcomponent;
    V: Variant;
  begin
    n := -1;
    for j := 0 to aFm.ComponentCount - 1 do
    begin
      C := aFm.Components[j];
      if (not HasFId(C)) or (C is TdxFile) or (C is TdxDBImage) or (C is TdxCounter) or
        (GetExpression(C) <> '') then Continue;
      Inc(n);
      V := PVariant(aCols[n])^;
      aDS.FieldByName(FieldStr(C)).Value := V;
      if C is TdxLookupComboBox then
      begin
        Inc(n);
        V := PVariant(aCols[n])^;
        aDS.FieldByName(FieldStr(C) + 'l').Value := V;
      end;
    end;
  end;

  procedure PasteGrid(DSR: PDataSetRec; aRows: TList);
  var
    Frm: TdxForm;
    DSet: TDataSet;
    Cols: TList;
    j: Integer;
  begin
    Frm := DSR^.Form;
    DSet := DSR^.DataSet;
    DSet.DisableControls;
    for j := 0 to aRows.Count - 1 do
    begin
      Cols := TList(aRows[j]);
      DSet.Append;
      DSet['id'] := DBase.GenId('gen_' + TableStr(Frm.Id));
      DSet['pid'] := MasterSet['id'];
      PasteRec(Frm, DSet, Cols);
      DSet.Post;
    end;
    DSet.First;
    DSet.EnableControls;
    FDuplicateFlag := False;
    DSet.AfterPost(DSet);
    FDuplicateFlag := True;
  end;

  procedure ClearCols(aCols: TList);
  var
    j: Integer;
  begin
    for j := 0 to aCols.Count - 1 do
      Dispose(PVariant(aCols[j]));
  end;

  procedure ClearRows(aRows: TList);
  var
    Cols: TList;
    j: Integer;
  begin
    for j := 0 to aRows.Count - 1 do
    begin
      Cols := TList(aRows[j]);
      ClearCols(Cols);
      Cols.Free;
    end;
  end;

begin
  FDuplicateFlag := True;
  FMaster^.Grid.EditorMode:=False;

  try

  FMaster^.Form.BeginDuplicate;

  Vals := TList.Create;
  Fm := FMaster^.Form;
  DS := FMaster^.DataSet;
  CopyRec(Fm, DS, Vals);

  Tables := TList.Create;
  for i := 1 to FItems.Count - 1 do
  begin
    Rows := TList.Create;
    Tables.Add(Rows);
    CopyGrid(GetDataSet(i), Rows);
  end;

  DS.Append;
  DS['id'] := DBase.GenId('gen_' + TableStr(Fm.Id));

  PasteRec(Fm, DS, Vals);
  FDuplicateFlag := False;
  DataSetAfterScroll(FMaster^.DataSet);
  FDuplicateFlag := True;

  for i := 1 to FItems.Count - 1 do
  begin
    Rows := TList(Tables[0]);
    PasteGrid(GetDataSet(i), Rows);
    ClearRows(Rows);
    Rows.Free;
    Tables.Delete(0);
  end;

  Tables.Free;
  ClearCols(Vals);
  Vals.Free;
  FDuplicateFlag := False;

  finally
    FMaster^.Form.EndDuplicate;
  end;

  if IsTableInput(FMaster) then
  else if FMaster^.EditFm <> nil then
    ShowEditForm(0)
  else if (FFm.ViewType <> vtGridOnly) and (FFm.Grid.ReadOnly) then
  begin
    Ctrl := GetTopControl(FFm);
    if (Ctrl <> nil) and (Ctrl.CanFocus) then Ctrl.SetFocus;
  end;
end;

procedure TDataSetProcessor.Edit;
var
  C: TWinControl;
begin
  FMaster^.Grid.EditorMode:=False;
  if FMaster^.Editing then
  begin
    FMaster^.DataSet.Edit;
    DoStateChange;
    // В AfterEdit редактирование может быть отменено.
    if not (FMaster^.DataSet.State in [dsInsert, dsEdit]) then Exit;
  end;
  if IsTableInput(FMaster) then
  else if FMaster^.EditFm <> nil then
    ShowEditForm(0)
  else if (FFm.ViewType <> vtGridOnly) and (FFm.Grid.ReadOnly) then
  begin
    C := GetTopControl(FFm);
    if (C <> nil) and (C.CanFocus) then
      C.SetFocus;
  end;
end;

function TDataSetProcessor.EditObject: Integer;
begin
  Result := mrNone;
  if FMaster^.Editing then
  begin
    FMaster^.DataSet.Edit;
    if FMaster^.DataSet.State <> dsEdit then Exit;
    DoStateChange;
  end;
  if FMaster^.EditFm <> nil then
    Result := ShowEditForm(0)
end;

procedure TDataSetProcessor.Delete;

  {function GenDeleteDetails: String;
  var
    id, T: String;
    DSR: TDataSetRec;
    i: Integer;
  begin
    Result := '';
    id := FMaster^.DataSet['id'];
    for i := 1 to FItems.Count - 1 do
    begin
      DSR := GetDataSet(i)^;
      T := TableStr(DSR.Form.Id);
      Result := Result + 'delete from ' + T + ' where pid=' + id + ';';
    end;
  end;  }

  function CheckDetails: Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to FItems.Count - 1 do
      if GetDataSet(i)^.DataSet.RecordCount > 0 then Exit(True);
  end;

  function _CheckAccess: String;
  var
    i: Integer;
    DSR: PDataSetRec;
    DS: TSQLQuery;
    B: TBookmark;
  begin
    Result := '';
    for i := 1 to FItems.Count - 1 do
    begin
      DSR := GetDataSet(i);
      if ((DSR^.DelCond = nil) and (not DSR^.Deleting)) or
        ((DSR^.EditCond = nil) and (not DSR^.Editing)) then Exit(DSR^.Form.FormCaption);
      DS := DSR^.DataSet;
      try
        DS.DisableControls;
        B := DS.GetBookmark;
        DS.First;
        while not DS.Eof do
        begin
          if (not DSR^.Deleting) or (not DSR^.Editing) then Exit(DSR^.Form.FormCaption);
          DS.Next;
        end;
      finally
        DS.GotoBookmark(B);
        DS.FreeBookmark(B);
        DS.EnableControls;
      end;
    end;
  end;

var
  S: String;
begin
  if MasterSet['id'] = Null then Exit;
  // Если есть подчиненные данные, то выводим другое предупреждение
  if CheckDetails then
  begin
    if WarnFm.ShowForm(rsConfirmDelete2) <> mrOk then Exit;
  end
  else if not ConfirmDelete then Exit;
  // Может ее кто-то редактирует?
  if DBase.Remote and (UserMan.CurrentUser <> nil) and (not UserMan.SingleMode) then
    if CheckLockRecord(False) then Exit;

  if CheckModifyRecord(True) > 0 then Exit;
  // Проверяем права доступа
  S := _CheckAccess;
  if S <> '' then
  begin
    ErrMsg(Format(rsDeleteLimitedMsg, [S]));
    Exit;
  end;
  if not CheckDeleteRecord(FMaster^.Form.Id, MasterSet['id'], True) then Exit;
  InnerDelete;
  {FMaster^.Grid.EditorMode:=False;
  DBase.Execute(GenDeleteDetails);
  with FMaster^ do
  begin
    DataSet.Delete;
    DBase.ApplyDataset(DataSet);
    DBase.Commit;
  end;
  DoStateChange;
  MainFr.RefreshAllLookups(FMaster^.Form.Id); }
end;

procedure TDataSetProcessor.InnerDelete;

  function GenDeleteDetails: String;
  var
    id, T: String;
    DSR: TDataSetRec;
    i: Integer;
  begin
    Result := '';
    id := FMaster^.DataSet['id'];
    for i := 1 to FItems.Count - 1 do
    begin
      DSR := GetDataSet(i)^;
      T := TableStr(DSR.Form.Id);
      Result := Result + 'delete from ' + T + ' where pid=' + id + ';';
    end;
  end;

begin
//  if not CheckDeleteRecord(FMaster^.Form.Id, MasterSet['id'], True) then Exit;
  FMaster^.Grid.EditorMode:=False;
  DBase.Execute(GenDeleteDetails);
  with FMaster^ do
  begin
    DataSet.Delete;
    DBase.ApplyDataset(DataSet);
    DBase.Commit;
  end;
  DoStateChange;
  if not Testing then MainFr.RefreshAllLookups(FMaster^.Form.Id);
end;

procedure TDataSetProcessor.DeleteAllRecords;
var
  NoDel: Boolean;
begin
  FMaster^.Grid.EditorMode:=False;
  NoDel := False;
  FDeletingRecs := True;
  MasterSet.DisableControls;
  MasterSet.AfterDelete := nil;
  MasterSet.First;
  try
    while not MasterSet.Eof do
      if CheckDeleteRecord(FMaster^.Form.Id, MasterSet['id'], False) then
        //MasterSet.Delete
      	InnerDelete
      else
      begin
        NoDel := True;
        MasterSet.Next;
      end;
    //DBase.ApplyDataSet(MasterSet);
    //DBase.Commit;
  finally
    MasterSet.AfterDelete:=@DataSetAfterDelete;
    MasterSet.EnableControls;
    FDeletingRecs := False;
  end;
  MasterSet.AfterScroll(MasterSet);
  if NoDel then ErrMsg(rsErrDelRecs);
end;

procedure TDataSetProcessor.Post;
begin
  if FMaster^.DataSet.State in [dsInsert, dsEdit] then
    FMaster^.DataSet.Post;
  //if FMaster^.EditFm = nil then CalcExprs(FMaster^, '');
  CheckAccess(FMaster);
end;

procedure TDataSetProcessor.Refresh;
begin
  Close;
  Open;
  if not Testing then MainFr.RefreshAllLookups(FMaster^.Form.Id);
end;

procedure TDataSetProcessor.First;
begin
  FMaster^.DataSet.First;
  DoStateChange;
end;

procedure TDataSetProcessor.Last;
begin
  FMaster^.DataSet.Last;
  DoStateChange;
end;

procedure TDataSetProcessor.Next;
begin
  FMaster^.DataSet.Next;
  DoStateChange;
end;

procedure TDataSetProcessor.Prior;
begin
  FMaster^.DataSet.Prior;
  DoStateChange;
end;

function TDataSetProcessor.Opened: Boolean;
begin
  Result := FMaster^.DataSet.Active;
end;

function TDataSetProcessor.MasterSet: TSQLQuery;
begin
  Result := FMaster^.DataSet;
end;

procedure TDataSetProcessor.RefreshComboBox(LR: TLookupRec);
var
  Cbx: TCustomDBComboBox;
  Flt: String;
  DSR: TDataSetRec;
begin
  with LR.DataSet do
  begin
    DSR := GetDataSet(LR.DSRi)^;
    try
      Flt := SqlLookupFilter(DSR.Form, GetDataSet(0)^.Form, DSR.DataSet, LR.Control);
      SQL.Text := SqlLookupSelect(LR.Control, Flt);
    except
      on E: Exception do
      begin
        ErrMsg(E.Message);
        Exit;
      end;
    end;
    Open;
    try
      Cbx := TCustomDBComboBox(LR.Control);
      Cbx.Items.Clear;
      while not Eof do
      begin
        Cbx.Items.AddObject(Fields[1].AsString, TObject(Fields[0].AsInteger));
        Next;
      end;
      if not TDBGrid(LR.Column.Grid).ReadOnly then
        LR.Column.PickList.Assign(Cbx.Items);
    finally
      Close;
    end;
  end;
end;

procedure TDataSetProcessor.ChangeObjectFields(Obj: TdxLookupComboBox);
var
  Fm, SFm: TdxForm;
  i: Integer;
  C, SC: TComponent;
  DS: TDataSet;
  S: String;
  F: TField;
begin
  if not (Obj.Field.DataSet.State in [dsInsert, dsEdit]) then Exit;
  DS := nil;
  Fm := TdxForm(Obj.Owner);
  SFm := FormMan.FindForm(Obj.SourceTId);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxObjectField then
      with TdxObjectField(C) do
        if (ObjId = GetId(Obj)) and (FieldId > 0) then
        begin
          if DS = nil then
          begin
            if Obj.KeyValue = Null then S := '0'
            else S := VarToStr(Obj.KeyValue);
            DS := DBase.OpenDataSet('select * from ' + TableStr(Obj.SourceTId) +
              ' where id=' + S);
          end;

          F := DS.FieldByName(FieldStr(FieldId));
          SC := FindById(SFm, FieldId);
          if not (SC is TdxLookupComboBox) then
            Field.Value := F.Value
          else
            Field.Value := GetObjFieldValue(SC, F.AsInteger, True);
        end;
  end;
  FreeAndNil(DS);
end;

procedure TDataSetProcessor.ApplyQuickFilter;
var
  Col: TColumn;
  C: TComponent;
  S: String;
  IsObjField: Boolean;
  F: TFilterField;
begin
  Col := FMaster^.Grid.SelectedColumn;
  C := FindById(FMaster^.Form, Col.Tag);
  IsObjField := (C is TdxObjectField);
  if IsObjField then
    C := LookupObjectField(TdxObjectField(C), False);
  if C = nil then Exit;

  FMaster^.Filter.Clear;
  F := FMaster^.Filter.AddField;
	F.FId := Col.Tag;
  if Col.Field.IsNull then
    F.IsNull := True
  else
  begin
    S := Col.Field.AsString;
    if (C is TdxCalcEdit) or (C is TdxDateEdit) or (C is TdxTimeEdit) or
      (C is TdxCounter) then
      S := S + ' .. ' + S
    else if C is TdxLookupComboBox then
    begin
      if not IsObjField then
        S := FMaster^.DataSet.FieldByName(FieldStr(Col.Tag)).AsString
      else
        S := GetObjFieldKey(TdxLookupComboBox(C), S);
    end;
    F.Values.Add(S);
  end;
  FMaster^.FilterIndex:=-1;
  Refresh;
  DoChangeFilter;
end;

function TDataSetProcessor.CalcColor(Fm: TdxForm; DS: TDataSet): TColor;
var
  EB: TExpressionBuilder;
  SL: TStrings;
  i, p: Integer;
  S: String;
  Clr: TColor;
  E: TExpression;
  V: Variant;
begin
  Result := clNone;
  EB := TExpressionBuilder.Create;
  EB.Form := Fm;
  EB.ParentForm := GetDataSet(0)^.Form;
  EB.DataSet := DS;
  EB.SkipLabels := True;
  E := nil;
  SL := Fm.Coloring;
  try try
    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      p := Pos(';', S);
      Clr := StringToColor(Copy(S, 1, p - 1));
      FreeAndNil(E);
      E := EB.Build(Copy(S, p + 1, 20000));
      if E = nil then Continue;
      V := E.Calc;
      if VarIsBool(V) and (V = True) then
        Exit(Clr);
    end;
  except
    Exit; // Глушим ошибку, чтобы программа не упала
  end;
  finally
    EB.Free;
    FreeAndNil(E);
  end;
end;

procedure TDataSetProcessor.CalcQueryColor(RD: TReportData; Fm: TdxForm; RDS,
  DS: TDataSet; const TargetField: String; var FieldName: String;
  var Color: TColor);
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
  EB.Form := Fm;
  EB.ParentForm := GetDataSet(0)^.Form;
  EB.DataSet := DS;
  EB.SkipLabels := True;
  E := nil;
  try try
    for i := 0 to RD.Coloring.Count - 1 do
    begin
      CD := RD.Coloring[i];
      if CompareText(CD.FieldName, TargetField) <> 0 then Continue;
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

procedure TDataSetProcessor.InsertObjectValues(Obj: TdxLookupComboBox);
var
  Fm: TdxForm;
  OFIdSL, FIdSL, SL: TStringList;
  i, p, Key: Integer;
  S, SS, SQL: String;
  ObjDS, DS: TDataSet;
  C: TComponent;
begin
  if Obj.InsertedValues = '' then Exit;
  Fm := TdxForm(Obj.Owner);
  OFIdSL := TStringList.Create;
  FIdSL := TStringList.Create;
  SL := TStringList.Create;
  SplitStr(Obj.InsertedValues, '|', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    p := Pos(';', S);
    OFIdSL.Add(Copy(S, 1, p - 1));
    FIdSL.Add(Copy(S, p + 1, 255));
  end;

  DS := Obj.DataSource.DataSet;

  // При очистке объекта
  if Obj.KeyValue = Null then
  begin
    for i := 0 to FIdSL.Count - 1 do
    begin
      C := FindById(Fm, StrToInt(FIdSL[i]));
      S := 'f' + FIdSL[i];

      if C is TdxLookupComboBox then
      	TdxLookupComboBox(C).Clear
      else if C is TdxDBImage then
      	TdxDBImage(C).Clear
      else if C is TdxFile then
      	TdxFile(C).Clear
      else
      	Obj.DataSource.DataSet.FieldByName(S).SetData(nil);
    end;

    EXIT;
  end;
  //

  SQL := 'select ';
  for i := 0 to OFIdSL.Count - 1 do
  begin
    S := 'f' + OFIdSL[i];
    SQL := SQL + S + ',';
    C := FindById(Fm, StrToInt(FIdSL[i]));
    if C is TdxFile then
      SQL := SQL + S + 'd,' + S + 'src,' + S + 'dest,'
    else if C is TdxDBImage then
      SQL := SQL + S + 'thumb,' + S + 'src,' + S + 'dest,';
  end;
  SQL := Copy(SQL, 1, Length(SQL) - 1);
  SQL := SQL + ' from ' + TableStr(Obj.SourceTId) + ' where id=' + VarToStr(Obj.KeyValue);
  ObjDS := DBase.OpenDataSet(SQL);
  for i := 0 to OFIdSL.Count - 1 do
  begin
    S := 'f' + FIdSL[i];
    SS := 'f' + OFIdSL[i];
    C := FindById(Fm, StrToInt(FIdSL[i]));
    if C is TdxLookupComboBox then
    begin
      Key := ObjDS.FieldByName(SS).AsInteger;
      if Key > 0 then
      begin
        DS.FieldByName(S + 'l').Value := GetObjFieldValue(C, Key, True);
        DS.FieldByName(S).Value := Key;
      end
      else
      begin
        DS.FieldByName(S + 'l').SetData(nil);
        DS.FieldByName(S).SetData(nil);
      end;
    end
    else if C is TdxFile then
    begin
      DS.FieldByName(S).Value := ObjDS.FieldByName(SS).Value;
      DS.FieldByName(S + 'd').Value := ObjDS.FieldByName(SS + 'd').Value;
      DS.FieldByName(S + 'src').Value := ObjDS.FieldByName(SS + 'src').Value;
      DS.FieldByName(S + 'dest').Value := ObjDS.FieldByName(SS + 'dest').Value;
    end
    else if C is TdxDBImage then
    begin
      DS.FieldByName(S).Value := ObjDS.FieldByName(SS).Value;
      DS.FieldByName(S + 'thumb').Value := ObjDS.FieldByName(SS + 'thumb').Value;
      DS.FieldByName(S + 'src').Value := ObjDS.FieldByName(SS + 'src').Value;
      DS.FieldByName(S + 'dest').Value := ObjDS.FieldByName(SS + 'dest').Value;
      TdxDBImage(C).ShowImage;  // Картинка почему-то не отображается при вставке.
    end
    else
      DS.FieldByName(S).Value := ObjDS.FieldByName(SS).Value;
  end;
  ObjDS.Free;

  SL.Free;
  OFIdSL.Free;
  FIdSL.Free;
end;

procedure TDataSetProcessor.FillTableFromObject(Obj: TdxLookupComboBox);
var
  SFm, DFm: TdxForm;
  SQL: String;
  Grid: TdxGrid;
  DS, SrcDS: TDataSet;
  i, k: Integer;
  SL: TStrings;
  SC, DC: TComponent;
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
begin
  if (Obj.SourceTable = 0) or (Obj.DestTable = 0) or (Obj.FieldsTables.Count = 0) or
    (Obj.KeyValue = Null) then Exit;
  if Obj.PromptFillTable then
  begin
    if MessageDlg(rsWarning, rsPromptFillTable, mtConfirmation,
      [mbYes, mbNo], 0) = mrNo then Exit;
  end;

  SL := Obj.FieldsTables;
  SFm := FormMan.FindForm(Obj.SourceTable);
  DFm := FormMan.FindForm(Obj.DestTable);
  SQL := 'select * from ' + TableStr(SFm.Id)  + ' where pid=' + VarToStr(Obj.KeyValue);
  Grid := FindGridById(FFm, DFm.Id);
  DS := Grid.DataSource.DataSet;

  if Obj.ClearTableBeforeFill then
  begin
    DS.DisableControls;
    DS.First;
    while not DS.EOF do
      DS.Delete;
    DS.EnableControls;
  end;

  EB := nil; E := nil;
  SrcDS := DBase.OpenDataSet(SQL);
  DS.DisableControls;
  with SrcDS do
  try
    if Trim(Obj.FillFilter) > '' then
    begin
      EB := TExpressionBuilder.Create;
      EB.Form := FormMan.FindForm(Obj.SourceTable);
      EB.DataSet := SrcDS;
      EB.SkipLabels:=True;
      try
        E := EB.Build(Obj.FillFilter);
      except
        on Ex: Exception do
          FMaster^.Err.AddError(Format(rsErrorInFillFilter, [Obj.FieldName]),
            Ex.Message);
      end;
      FreeAndNil(EB);
    end;
    while not Eof do
    begin
      if E <> nil then
      begin
        try
          V := E.Calc;
          if VarIsBool(V) and (V = False) then
          begin
            Next;
            Continue;
          end;
        except
          on Ex: Exception do
            FMaster^.Err.AddError(Format(rsErrorInFillFilter, [Obj.FieldName]),
              Ex.Message);
        end;
      end;
      DS.Append;
      for i := 0 to SL.Count - 1 do
      begin
        if (SL.Names[i] = '') or (SL.ValueFromIndex[i] = '') then Continue;

        SC := FindById(SFm, StrToInt(SL.Names[i]));
        DC := FindById(DFm, StrToInt(SL.ValueFromIndex[i]));
        DS.FieldByName(FieldStr(DC)).Value := FieldByName(FieldStr(SC)).Value;
        if DC is TdxLookupComboBox then
        begin
          k := DS.FieldByName(FieldStr(DC)).AsInteger;
          if k > 0 then
            DS.FieldByName(FieldStr(DC) + 'l').Value := GetObjFieldValue(DC, k,
              SFm.ParentField > 0);
        end
        else if DC is TdxFile then
        begin
          DS.FieldByName(FieldStr(DC) + 'src').Value := FieldByName(FieldStr(SC) + 'src').Value;
          DS.FieldByName(FieldStr(DC) + 'dest').Value := FieldByName(FieldStr(SC) + 'dest').Value;
          DS.FieldByName(FieldStr(DC) + 'd').Value := FieldByName(FieldStr(SC) + 'd').Value;
        end
        else if DC is TdxDBImage then
        begin
          DS.FieldByName(FieldStr(DC) + 'src').Value := FieldByName(FieldStr(SC) + 'src').Value;
          DS.FieldByName(FieldStr(DC) + 'dest').Value := FieldByName(FieldStr(SC) + 'dest').Value;
          DS.FieldByName(FieldStr(DC) + 'thumb').Value := FieldByName(FieldStr(SC) + 'thumb').Value;
        end;
      end;
      DS.Post;
      Next;
    end;
  finally
    DS.EnableControls;
    Free;
    FreeAndNil(EB);
    FreeAndNil(E);
  end;
end;

(*function IsParamExists(const Flt: String; const FieldName: String): Boolean;
type
  TState = (stField, stOp, stExpr);
var
  S: String;
  P, BrCnt: Integer;
  Tk: Char;
  St: TState;
begin
  Result := False;
  P := 1;
  St := stField;

  while True do
  begin
    S := ReadToken(Flt, P, Tk);
    if (S = '{') or (S = '}') then Continue;
    case St of
      stField:
        begin
          if Tk <> '[' then Exit;
          St := stOp;
        end;
      stOp:
        begin
          if Tk <> '=' then Exit;
          St := stExpr;
          BrCnt := 0;
        end;
      stExpr:
        begin
          if (Tk = #0) or (((S = '&') or (S = '|')) and (BrCnt = 0)) then
          begin
            if Tk <> #0 then St := stField
            else Break;
          end
          else
          begin
            if S = '(' then Inc(BrCnt)
            else if S = ')' then Dec(BrCnt)
            // !!!!!!!!!!!!!!!!!!!!!
            else if (Tk = '[') then
            begin
              if (Length(S) > 0) and (S[1] = '!') then Delete(S, 1, 1);
              if (Utf8CompareText(FieldName, S) = 0) then Exit(True);
            end;
            //
          end;
        end;
    end;
  end;
end; *)


function FieldExists(const FieldName, E: String): Boolean;
var
  S, SS: String;
  L, i, p: Integer;
  IsField: Boolean;
begin
  {S := Utf8LowerCase(FieldName);
  SS := Utf8LowerCase(E);
  Result := (Utf8Pos('[' + S, SS) > 0) or (Utf8Pos('[!' + S, SS) > 0)
    or (Utf8Pos('[:' + S, SS) > 0);  }

  Result := False;
  S := Utf8LowerCase(FieldName);
  L := Length(E);
  IsField := False;
  for i := 1 to L do
  begin
    if E[i] = '[' then
    begin
      IsField := True;
      p := i + 1;
    end
    else if IsField and (p = i) and (E[i] in ['!', ':']) then
      p := i + 1
    else if IsField and ((E[i] in [']', '|']) or (i = L)) then
    begin
      SS := Utf8LowerCase(Copy(E, p, i - p));
      if S = SS then Exit(True);
      IsField := False;
    end;
  end;
end;

procedure TDataSetProcessor.RefreshLookupsWithParams(Fm: TdxForm;
  const FieldName: String);
var
  i: Integer;
  pLR: PLookupRec;
  Flt: String;
begin
  for i := 0 to FLookups.Count - 1 do
  begin
    pLR := PLookupRec(FLookups[i]);
    Flt := GetComboFilter(pLR^.Control);
    if (Flt <> '') and ((FieldName = '') or FieldExists(FieldName, Flt)) then
    begin
      pLR^.NeedRefresh:=True;
      if not (pLR^.Control is TdxMemo) then
      begin
        if not TDBGrid(pLR^.Column.Grid).ReadOnly then
          if pLR^.Column.PickList.Count = 0 then
            pLR^.Column.PickList.Add('');          // Чтобы был PickListCellEditor
      end;
    end;
  end;
end;

procedure TDataSetProcessor.PrepareListForm(pLR: PLookupRec);
var
  Fm: TListWindow;
  DSR: TDataSetRec;
  C: TComponent;
begin
  Fm := pLR^.ListFm;
  if pLR^.ListFm = nil then
  begin
    Fm := TListWindow.CreateNew(nil);
    pLR^.ListFm := Fm;
    Fm.Load(pLR^.TId);
  end;
  //Фильтр
  DSR := GetDataSet(pLR^.DSRi)^;
  try
    Fm.Filter:=SqlLookupFilter(DSR.Form, GetDataSet(0)^.Form, DSR.DataSet, pLR^.Control);
    C := pLR^.Control;
    if C is TdxLookupComboBox then
      with TdxLookupComboBox(C) do
      begin
        if OnCreateListWindow <> nil then OnCreateListWindow(C, Fm);
        if MainFm.OnCreateListWindow <> nil then MainFm.OnCreateListWindow(MainFm, Fm);
      end
    else if C is TdxMemo then
      with TdxMemo(C) do
      begin
        if OnCreateListWindow <> nil then OnCreateListWindow(C, Fm);
        if MainFm.OnCreateListWindow <> nil then MainFm.OnCreateListWindow(MainFm, Fm);
      end;
  except
    on E: Exception do
      ErrMsg(E.Message);
  end;
end;

procedure TDataSetProcessor.SetColumnTitles(G: TdxGrid);
var
  i: Integer;
  C: TColumn;
  Fm: TdxForm;
  Cm: TComponent;
begin
  Fm := FFm;
  if G.Id > 0 then
    Fm := FormMan.FindForm(G.Id);
  for i := 0 to G.Columns.Count - 1 do
  begin
    C := G.Columns[i];
    if C.Title.Caption <> ' ' then Continue;
    Cm := FindById(Fm, C.Tag);
    C.Title.Caption:=GetFieldName(Cm);
  end;
end;

procedure CopyColumns(sG, dG: TdxGrid);
var
  i: Integer;
begin
  dG.Columns := sG.Columns;
  for i := 0 to sG.Columns.Count - 1 do
    dG.Columns[i].Tag := sG.Columns[i].Tag;
end;

procedure TDataSetProcessor.SetGrids;
var
  i, j: Integer;
  C: TComponent;
  G: TdxGrid;
  Fm: TdxForm;
  SD: TSortColData;
begin
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TdxGrid then
    begin
      G := TdxGrid(C);
      Fm := FormMan.FindForm(G.Id);
      CopyColumns(Fm.Grid, G);
      SetColumnTitles(G);
      G.Color:=Fm.Grid.Color;
      G.AlternateColor:=Fm.Grid.AlternateColor;
      G.FixedColor:=Fm.Grid.FixedColor;
      G.FixedHotColor:=Fm.Grid.FixedHotColor;
      G.SelectedColor:=Fm.Grid.SelectedColor;
      G.FocusColor:=Fm.Grid.SelectedColor;
      G.Font := Fm.Grid.Font;
      G.TitleFont := Fm.Grid.TitleFont;
      G.DefaultRowHeight:=Fm.Grid.DefaultRowHeight;
      G.Flat := Fm.Grid.Flat;
      G.ReadOnly:=Fm.Grid.ReadOnly;
      G.Options:=Fm.Grid.Options;
      G.GridLineColor:=Fm.Grid.GridLineColor;
      G.GridLineStyle:=Fm.Grid.GridLineStyle;
      G.SortCols.Clear;
      for j := 0 to Fm.Grid.SortCols.Count - 1 do
      begin
        SD := Fm.Grid.SortCols[j];
        G.SortCols.AddCol(G.Columns[SD.Col.Index], SD.Desc);
      end;
      G.WordWrap:=Fm.Grid.WordWrap;
    end;
  end;
end;

function TDataSetProcessor.CalcEditCond(Fm: TdxForm; DS: TDataSet;
  const Cond: String): Boolean;
var
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
begin
  Result := True;
  if Trim(Cond) = '' then Exit;
  E := nil;
  EB := TExpressionBuilder.Create;
  try try
    EB.Form := Fm;
    EB.ParentForm := FFm;
    EB.DataSet := DS;
    EB.SkipLabels:=True;
    E := EB.Build(Cond);
    if E <> nil then
    begin
      V := E.Calc;
      if VarIsBool(V) then Result := V;
    end;
  except
    on Ex: Exception do
      ErrMsg(Ex.Message);
  end;
  finally
    FreeAndNil(E);
    EB.Free;
  end;
end;

procedure TDataSetProcessor.UpdatePopupState(DSR: TDataSetRec);
var
  Pop: TPopupMenu;
  IsChild, bbrowse, bcount, IsEdit: Boolean;
  i: Integer;
begin
  // !!! Доступ
  if DSR.DataSet.Active = False then Exit;

  Pop := DSR.Popup;
  IsChild := DSR.Form.PId > 0;
  bbrowse := not (DSR.DataSet.State in [dsInsert, dsEdit]);
  bcount := DSR.DataSet.RecordCount > 0;
  if IsChild then Pop.Items[0].Visible := DSR.Adding and FMaster^.Editing
  else Pop.Items[0].Visible := DSR.Adding;
  Pop.Items[0].Enabled:=Pop.Items[0].Visible and bbrowse;
  Pop.Items[1].Enabled := bbrowse and bcount;
  IsEdit := DSR.Editing and FMaster^.Editing;
  if IsEdit then
  begin
    Pop.Items[1].Caption:=rsEdit;
    SetMenuItemImage(Pop.Items[1], 'edit16');
  end
  else
  begin
    Pop.Items[1].Caption := rsLook;
    SetMenuItemImage(Pop.Items[1], 'eyes16');
  end;
  if IsChild then Pop.Items[2].Visible := DSR.Deleting and DSR.Editing and FMaster^.Editing
  else Pop.Items[2].Visible := DSR.Deleting and DSR.Editing;
  Pop.Items[2].Enabled := Pop.Items[2].Visible and bcount and bbrowse;
  if Pop.Tag = 0 then
  begin
    Pop.Items[3].Visible := DSR.Adding and DSR.Editing;
    Pop.Items[4].Visible := Pop.Items[3].Visible;
    Pop.Items[5].Visible := DSR.Adding and DSR.Editing;
    Pop.Items[4].Enabled := Pop.Items[4].Visible and bcount and bbrowse;
    Pop.Items[5].Enabled := Pop.Items[4].Enabled and bbrowse;
    Pop.Items[7].Enabled := bcount and bbrowse;
  end
  else
  begin
    Pop.Items[4].Visible := DSR.Adding and DSR.Editing and FMaster^.Editing;
    Pop.Items[3].Visible := Pop.Items[4].Visible;
    Pop.Items[4].Enabled := Pop.Items[4].Visible and bcount and bbrowse;
    //if Pop.Items.Count > 6 then
    //begin
    Pop.Items[6].Visible := DSR.CanShoping and DSR.Adding and FMaster^.Editing;
    Pop.Items[5].Visible := Pop.Items[6].Visible;
    Pop.Items[6].Enabled := Pop.Items[6].Visible and bbrowse;
    //end;
    Pop.Items[7].Visible:=FMaster^.Editing and (DSR.Grid.SortCols.Count = 0);
    Pop.Items[8].Visible:=Pop.Items[7].Visible;
    Pop.Items[9].Visible:=Pop.Items[7].Visible;
    Pop.Items[8].Enabled:=Pop.Items[7].Visible and (DSR.DataSet.RecNo > 1) and bbrowse;
    Pop.Items[9].Enabled:=Pop.Items[7].Visible and (DSR.DataSet.RecNo < DSR.DataSet.RecordCount) and bbrowse;
  end;
  if IsChild then
  begin
    if not (MasterSet.State in [dsInsert, dsEdit]) then
    begin
      // Запрещаем редактирование, т. к. флажок игнорирует событие SelectEditor
      DSR.Grid.Options := DSR.Grid.Options - [dgEditing];
      for i := 0 to Pop.Items.Count - 1 do
      	// Просмотр оставляем
      	if (i = 1) and (not IsEdit) then
        else
	        Pop.Items[i].Enabled := False;
    end
    else
      DSR.Grid.Options := DSR.Grid.Options + [dgEditing];
  end;
  if IsChild and DSR.Grid.ShowButtons then
    with DSR.Grid.Buttons do
    begin
      ShowButton(gbnAppend, Pop.Items[0].Visible);
      EnableButton(gbnAppend, Pop.Items[0].Enabled);
      ShowButton(gbnEdit, Pop.Items[1].Visible);
      if gbnEdit in VisibleButtons then
      begin
        Buttons[gbnEdit].Caption:=Pop.Items[1].Caption;
        Buttons[gbnEdit].Glyph.Assign(Pop.Items[1].Bitmap);
      end;
      EnableButton(gbnEdit, Pop.Items[1].Enabled);
      ShowButton(gbnDelete, Pop.Items[2].Visible);
      EnableButton(gbnDelete, Pop.Items[2].Enabled);
      ShowButton(gbnDuplicate, Pop.Items[4].Visible);
      EnableButton(gbnDuplicate, Pop.Items[4].Enabled);
      ShowButton(gbnShopping, Pop.Items[6].Visible);
      EnableButton(gbnShopping, Pop.Items[6].Enabled);
      ShowButton(gbnMoveUp, Pop.Items[8].Visible);
      EnableButton(gbnMoveUp, Pop.Items[8].Enabled);
      ShowButton(gbnMoveDown, Pop.Items[9].Visible);
      EnableButton(gbnMoveDown, Pop.Items[9].Enabled);
    end;
end;

procedure TDataSetProcessor.UpdateControlState(DSR: TDataSetRec);
var
  i: Integer;
  Bn: TSpeedButton;
begin
  // !!! Доступ
  UpdatePopupState(DSR);
  DSR.DataSource.AutoEdit := (DSR.Editing) and (FMaster^.Editing) and (DSR.DataSet['id'] <> Null);
  {if DSR.EditFm <> nil then
  begin
    if DSR.Editing and FMaster^.Editing then DSR.EditFm.ButtonPanel1.ShowButtons:=[pbOk, pbCancel]
    else DSR.EditFm.ButtonPanel1.ShowButtons:=[pbClose];
  end;        }
  //DSR.EditFm.ButtonPanel1.OKButton.Enabled:=
    //(DSR.Editing) and (FMaster^.Editing);
  for i := 0 to DSR.EditingCtrls.Count - 1 do
  begin
    Bn := TSpeedButton(DSR.EditingCtrls[i]);
    Bn.Enabled:=DSR.DataSource.AutoEdit;
  end;
end;

procedure TDataSetProcessor.DuplicateRec(DSR: TDataSetRec);
var
  DS: TSQLQuery;
  Tmp: array of Variant;
  Fm: TdxForm;
  i, n: Integer;
  C: TComponent;
begin
  FDuplicateFlag := True;
  DS := DSR.DataSet;
  Fm := DSR.Form;
  n := 0;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (not HasFId(C)) or (C is TdxFile) or (C is TdxDBImage) or (C is TdxCounter) or
      (GetExpression(C) <> '') then Continue;
    Inc(n);
    SetLength(Tmp, n);
    Tmp[n - 1] := DS.FieldByName(FieldStr(C)).Value;
    if C is TdxLookupComboBox then
    begin
      Inc(n);
      SetLength(Tmp, n);
      Tmp[n - 1] := DS.FieldByName(FieldStr(C) + 'l').Value;
    end;
  end;
  n := 0;
  DS.Append;
  DS['id'] := DBase.GenId('gen_' + TableStr(Fm.Id));
  if Fm.PId > 0 then
  begin
    {MasterSet.Edit;
    if not (MasterSet.State in [dsInsert, dsEdit]) then
    begin
      FDuplicateFlag:=False;
      Exit;
    end;}
    DS['pid'] := MasterSet['id'];
  end;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (not HasFId(C)) or (C is TdxFile) or (C is TdxDBImage) or (C is TdxCounter) or
      (GetExpression(C) <> '') then Continue;
    Inc(n);
    DS.FieldByName(FieldStr(C)).Value := Tmp[n - 1];
    if C is TdxLookupComboBox then
    begin
      Inc(n);
      DS.FieldByName(FieldStr(C) + 'l').Value := Tmp[n - 1];
    end;
  end;
  SetLength(Tmp, 0);
  FDuplicateFlag := False;
  DS.AfterScroll(DS);
end;

procedure TDataSetProcessor.DoStateChange;
begin
  if FOnStateChange <> nil then FOnStateChange(Self);
end;

procedure TDataSetProcessor.CalcDefVals(DSR: TDataSetRec);
var
  i: Integer;
  DF: TExprData;
begin
  for i := 0 to DSR.DefValList.Count - 1 do
  begin
    DF := DSR.DefValList[i];
    try
      if DF.C is TdxLookupComboBox then
      begin
        with TdxLookupComboBox(DF.C) do
          if Field.IsNull then
          begin
	          KeyValue:=DF.E.Calc;
  	        if KeyValue <> Null then
    	        Field.Value := GetObjFieldValue(DF.C, KeyValue, True);
          end;
      end
      else
      	with DSR.DataSet.FieldByName(FieldStr(DF.C)) do
        	if IsNull then Value := DF.E.Calc;
    except
      on E: Exception do
        DSR.Err.AddError(GetFieldName(DF.C) + ' - ' + rsDefaultValue, E.Message);
    end;
  end;
end;

function TDataSetProcessor.ShowEditForm(DSRi: Integer): Integer;
var
  pDS: PDataSetRec;
begin
  pDS := GetDataSet(DSRi);
  if pDS^.Editing and FMaster^.Editing then pDS^.EditFm.Buttons.ShowButtons:=[pbOk, pbCancel]
  else pDS^.EditFm.Buttons.ShowButtons:=[pbClose];
  Result := pDS^.EditFm.ShowForm;
  if Result = mrOk then CheckAccess(pDS)
  else UpdateControlState(pDS^);
end;

procedure TDataSetProcessor.CheckAccess(pDS: PDataSetRec);
var
  V, V2: Variant;
  j: Integer;
begin
  if (pDS^.EditCond <> nil) or (pDS^.DelCond <> nil) then
    try
      if pDS^.EditCond <> nil then
        V := pDS^.EditCond.Calc
      else V := True;
      if pDS^.DelCond <> nil then
        V2 := pDS^.DelCond.Calc
      else V2 := V and UserMan.CheckFmDeleting(pDS^.Form.Id);
      if not VarIsBool(V) then V := True;
      if not VarIsBool(V2) then V2 := True;
      pDS^.Editing:=V;
      pDS^.Deleting:=V2;
    except
      on E: Exception do
        ErrMsg(E.Message);
    end;

  UpdateControlState(pDS^);
  if pDS = FMaster then
  begin
    for j := 1 to FItems.Count - 1 do
      UpdateControlState(GetDataSet(j)^);
    DoStateChange;
  end;
  UpdateQueryPopupStates;
end;

procedure TDataSetProcessor.DoChangeFilter;
begin
  if FOnChangeFilter <> nil then FOnChangeFilter(Self);
end;

function TDataSetProcessor.CheckModifyRecord(IsDeleteRec: Boolean): Integer;
var
  S: String;
begin
  Result := InnerCheckModifyRecord;
  if Result > 0 then
  begin
    MasterSet.Cancel;
    FMaster^.Grid.EditorMode:=False;
  end;
  case Result of
    1:
      begin
        if UserMan.CurrentUser <> nil then S := rsRecDeletedUserRefresh
        else S := rsRecDeletedYouRefresh;
	      if MessageDlg(rsWarning, S, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  	      Refresh;
      end;
    2:
      begin
        if UserMan.CurrentUser <> nil then S := rsRecChangedUser
        else S := rsRecChangedYou;
        if MessageDlg(rsWarning, S, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          RefreshDataAndEdit(IsDeleteRec)
        else
          Result := 3;                                      // Пользователь отменил действие
      end;
  end;
end;

function TDataSetProcessor.InnerCheckModifyRecord: Integer;
var
  DSR: TDataSetRec;
  S: String;
  DS: TSQLQuery;
  i: Integer;
  F: TField;
begin
  Result := 0;                                            // Изменений нет
  DSR := FMaster^;
  if DSR.HasFields = False then Exit;
  DS := DSR.DataSet;
  S := SqlSimpleSelectStatement(DSR.Form, DSR.DataSet.Fields[0].AsInteger);
  with DBase.OpenDataSet(S) do
  try
    if RecordCount = 0 then Result := 1                   // Запись удалена другим пользователем
    else
      for i := 0 to Fields.Count - 1 do
      begin
        F := DS.FieldByName(Fields[i].FieldName);
        if F.Value <> Fields[i].Value then
        begin
          Result := 2;                                    // Запись изменена другим пользователем
          Break;
        end;
      end;
  finally
    Free;
  end;
end;

{procedure TDataSetProcessor.RefreshRecordFromDB;
var
  DS: TSQLQuery;
  S: String;
  i: Integer;
begin
  DS := FMaster^.DataSet;
  S := FMaster^.SQL + ' where ' + TableStr(FFm.Id) + '.id=' + IntToStr(DS.Fields[0].AsInteger);
  with DBase.OpenDataSet(S) do
  try
    for i := 1 to Fields.Count - 1 do
      DS.Fields[i].Value := Fields[i].Value;
    DS.Post;
    DS.AfterScroll(DS);
  finally
    Free;
  end;
end; }

procedure TDataSetProcessor.RefreshDataAndEdit(IsDeleteRec: Boolean);
var
  Key: Variant;
begin
  Key := MasterSet.Fields[0].Value;
  Refresh;
  if MasterSet.Locate('id', Key, []) then
  begin
    MasterSet.AfterEdit:=nil;
    if CanEdit and (not IsDeleteRec) then
      MasterSet.Edit;
    DoStateChange;
    MasterSet.AfterEdit:=@DataSetAfterEdit;
  end;
end;

// KeepTrans - оставляет транзакцию открытой, если запись свободна, для LockRecord.
function TDataSetProcessor.CheckLockRecord(KeepTrans: Boolean): Boolean;
var
  //S: String;
  U: TdxUser;
  uid: LongInt;
begin
  Result := False;
  uid := InnerCheckLockRecord(False);
  if uid >= 0 then
  begin
    DBase.SafeCommit;
    MasterSet.Cancel;
    FMaster^.Grid.EditorMode:=False;
    U := UserMan.Users.FindUser(uid);
    if U <> nil then
    begin
      MessageDlg(rsAccessDenied, Format(rsRecEditUser, [U.Name]), mtWarning,
      	[mbOk], 0);
	    Result := True;
  	end;
  end
  else if not KeepTrans then
    DBase.SafeCommit;
end;

function TDataSetProcessor.InnerCheckLockRecord(Commit: Boolean): Integer;
var
  S: String;
begin
  Result := -1;
  S := 'select uid from dx_lock where fmid=' + IntToStr(FFm.Id) + ' and recid=' +
    IntToStr(MasterSet.Fields[0].AsInteger);
  with DBase.OpenDataSet(S, 'DX_LOCK') do
  try
    if RecordCount > 0 then Result := Fields[0].AsInteger;
  finally
    Free;
  end;
  if Commit then DBase.SafeCommit;
end;

procedure TDataSetProcessor.ClearListFilter;
begin
  FListFilter := '';
end;

procedure TDataSetProcessor.LockRecord;
var
  S: String;
begin
  if FReCalculate then Exit;
  if CheckLockRecord(True) then Exit;
  if CheckModifyRecord(False) in [0, 2] then
  begin
    S := 'insert into dx_lock (uid, fmid, recid, dtime, cid) values (' +
      IntToStr(UserMan.CurrentUserId) + ',' + IntToStr(FMaster^.Form.Id) + ',' +
      IntToStr(MasterSet.Fields[0].AsInteger) + ',CURRENT_TIMESTAMP,' +
      IntToStr(UserMan.ConnId) + ');';
    DBase.Execute(S, 'DX_LOCK');
    FRecLocked := True;
  end
  else
    DBase.SafeCommit;
end;

procedure TDataSetProcessor.UnLockRecord;
var
  S: String;
begin
  S := 'delete from dx_lock where uid=' + IntToStr(UserMan.CurrentUserId) +
    ' and fmid=' + IntToStr(FMaster^.Form.Id) + ' and recid=' +
    IntToStr(MasterSet.Fields[0].AsInteger) + ';';
  DBase.Execute(S, 'DX_LOCK');
  FRecLocked := False;
end;

procedure ShowField(C: TControl);
var
  Par: TWinControl;
begin
  while C.Parent <> C.Owner do
  begin
    Par := C.Parent;
    if Par is TdxTabSheet then
      with TdxTabSheet(Par).PageControl do
        ActivePage := TdxTabSheet(Par);
    C := Par;
  end;
end;

procedure TDataSetProcessor.ShowError(C: TControl; const Msg: String;
  DSR: TDataSetRec);
var
  P: types.TPoint;
  Col: TColumn;
begin
  if (DSR.EditFm <> nil) and (DSR.EditFm.Visible = False) then
  begin
    Col := FindGridColumn(DSR.Grid, GetId(C));
    if Col <> nil then
      DSR.Grid.SelectedField := Col.Field;
    ErrMsg(Msg);
  end
  else
  begin
    ShowField(C);
    if (C is TWinControl) and (TWinControl(C).CanFocus) then
      TWinControl(C).SetFocus;
    P := C.ClientToScreen(Point(20, C.Height - 8));
    FNotif.Text:=Msg;
    FNotif.ShowAtPos(P.x, P.y);
    FTimer.Enabled := True;
  end;
end;

procedure TDataSetProcessor.BuildPivotTables(idx: Integer);
var
  Q: TQueryRec;
  DSR: TDataSetRec;
  i: Integer;
  C: TComponent;
begin
  if FPrinting or FReCalculate then Exit;

  Q := PQueryRec(FQueries[idx])^;
  DSR := GetDataSet(Q.DSRi)^;
  for i := 0 to DSR.Form.ComponentCount - 1 do
  begin
    C := DSR.Form.Components[i];
    if C is TdxPivotGrid then
      with TdxPivotGrid(C) do
      begin
        if Id = Q.Grid.Id then
        begin
          DataSet := Q.DataSet;
          Build;
        end;
      end;
  end;
end;

function TDataSetProcessor.Validate(DSRi: Integer; ForceChanges: Boolean
  ): Boolean;
var
  i: Integer;
  C: TComponent;
  Fl: TField;
  L: TExprList;
  S: String;
  Fm: TdxForm;
  DS: TSQLQuery;
  DSR: TDataSetRec;
begin
  if FSimpleMode then Exit(True);

  // Сначала проверяем дочерние.
  if DSRi = 0 then
    for i := 1 to FItems.Count - 1 do
    begin
      Result := Validate(i);
      if not Result then Exit;
    end;

  DSR := GetDataSet(DSRi)^;
  Fm := DSR.Form;
  DS := DSR.DataSet;

  if not (DS.State in [dsInsert, dsEdit]) then Exit(True);

	if ForceChanges then ForceChangeFields(DSRi);

  Result := False;
  // Обязательные поля
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if HasRequired(C) then
    begin
      if (C is TdxCounter) and (DS.State in [dsInsert]) then Continue;
      Fl := DS.FieldByName(FieldStr(C));
      if GetRequired(C) and Fl.IsNull then
      begin
        ShowError(TControl(C), Format(rsFieldIsRequired, [GetFieldName(C)]), DSR);
        Exit;
      end
      else if C is TdxCalcEdit then
        with TdxCalcEdit(C) do
        begin
          if ((MinValue <> 0) or (MaxValue <> 0)) and
            ((Fl.AsFloat < MinValue) or (Fl.AsFloat > MaxValue)) then
          begin
           	ShowError(TControl(C), Format(rsRangeMsg,
             	[FieldName, FloatToStr(MinValue), FloatToStr(MaxValue)]), DSR);
            Exit;
          end;
        end;
    end;
  end;
  // Маска ввода
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxEdit then
    	with TdxEdit(C) do
        if (EditMask <> '') and (not ValidateText) then
        begin
         	ShowError(TControl(C), rsMaskErrorMsg, DSR);
        	Exit;
        end;
  end;

  // Проверка значений
  L := TExprList(DSR.ChkExprList);
  for i := 0 to L.Count - 1 do
  begin
    try
      S := VarToStr(L[i].E.Calc);
      if Trim(S) <> '' then
      begin
        ShowError(TControl(L[i].C), S, DSR);
        Exit;
      end;
    except
      on E: Exception do
      begin
        ShowError(TControl(L[i].C), E.Message, DSR);
        Exit;
      end;
    end;
  end;
  Result := True;
  if DSR.Form.OnValidate <> nil then DSR.Form.OnValidate(DSR.Form, Result);
end;

procedure TDataSetProcessor.Print(TemplateName: String);
var
  Errs: String;
begin
  //if not Validate(0) then Exit;
  //Post;
  //RefreshCurRecord;

  {$ifdef windows}
  TemplateName := StringReplace(TemplateName, '/', DirectorySeparator, [rfReplaceAll]);
  {$else}
  TemplateName := StringReplace(TemplateName, '\', DirectorySeparator, [rfReplaceAll]);
  {$endif}

  // Проверка файла
  if not FileExists(TemplateName) then
  begin
    ErrMsg(Format(rsTemplateFileNotFound, [TemplateName]));
    Exit;
  end;
  //
  {Screen.Cursor:=crHourGlass;
  MainFm.Lock(True);
  Application.ProcessMessages;
  FPrinting := True;
  try try
    FMaster^.Form.State := fsPrint;
    if FMaster^.Form.OnBeforePrint <> nil then FMaster^.Form.OnBeforePrint(FMaster^.Form);
    ReportToXXX(Self, TemplateName, Errs);
  except
    on E: Exception do
    begin
      ErrMsg(rsPrnError + LineEnding + LineEnding + E.Message);
      Errs := '';
    end;
  end;
  finally
    Screen.Cursor := crDefault;
    MainFm.Lock(False);
    FPrinting := False;
    FMaster^.Form.State := fsBrowse;
    if FMaster^.Form.OnAfterPrint <> nil then FMaster^.Form.OnAfterPrint(FMaster^.Form);
    //RefreshCurRecord;
  end;        }
  try
    InnerPrint(TemplateName, '', Errs, True);
  except
    on E: Exception do
    begin
      ErrMsg(rsPrnError + LineEnding + LineEnding + E.Message);
      Errs := '';
    end;
  end;
  if Errs <> '' then ErrorsFm.ShowForm(Errs);
end;

procedure TDataSetProcessor.InnerPrint(const TemplateName, OutName: String;
  var Errs: String; aOpenFile: Boolean);
var
  OldState: TDataSetState;
begin
  OldState := MasterSet.State;
  Screen.Cursor:=crHourGlass;
  if not Testing then MainFm.Lock(True);
  Application.ProcessMessages;
  FPrinting := True;
  try
    if FMaster^.Form.OnBeforePrint <> nil then FMaster^.Form.OnBeforePrint(FMaster^.Form);
    ReportToXXX(Self, TemplateName, OutName, Errs, aOpenFile);
  finally
    Screen.Cursor := crDefault;
    if not Testing then MainFm.Lock(False);
    FPrinting := False;
    if (OldState in [dsInsert, dsEdit]) and (MasterSet.State <> OldState) then
    begin
      MasterSet.Edit;
    end;
    if FMaster^.Form.OnAfterPrint <> nil then FMaster^.Form.OnAfterPrint(FMaster^.Form);
  end;
end;

procedure TDataSetProcessor.RefreshLookups(TId: Integer);
var
  i: Integer;
  pLR: PLookupRec;
  pQ: PQueryRec;
begin
  for i := 0 to FLookups.Count - 1 do
  begin
    pLR := PLookupRec(FLookups[i]);
    if (pLR^.TId = TId) or (TId = 0) then
    begin
      pLR^.NeedRefresh:=True;
      if not (pLR^.Control is TdxMemo) then
      begin
        if not TDBGrid(pLR^.Column.Grid).ReadOnly then
          if pLR^.Column.PickList.Count = 0 then
            pLR^.Column.PickList.Add('');          // Чтобы был PickListCellEditor
      end;
    end;
    if pLR^.ListFm <> nil then pLR^.ListFm.RefreshLookups(TId);
  end;

  for i := 0 to FQueries.Count - 1 do
  begin
    pQ := PQueryRec(FQueries[i]);
    if pQ^.DSProc <> nil then
      pQ^.DSProc.RefreshLookups(TId);
  end;
end;

function FormExists(FormName, E: String): Boolean;
begin
  E := Utf8LowerCase(E);
  FormName := Utf8LowerCase(FormName);
  Result := (Utf8Pos('''' + FormName + '''', E) > 0) or (Utf8Pos('"' + FormName + '"', E) > 0);
end;

procedure TDataSetProcessor.CalcFields(DSR: TDataSetRec;
  const FieldName: String; Sender: TComponent);
var
  i, fid: Integer;
  V: Variant;
  E: TExpression;
  fn: String;
  C: TComponent;
begin
  // Обнаружения зацикливаний
  if FExprCounter > 50 then
  begin
    FExprCounter := 0;
    FLoopDetected := True;
    raise Exception.Create(Format(rsLoopDetected, [FieldName]));
  end;
  Inc(FExprCounter);
  //

  for i := 0 to DSR.ExprList.Count - 1 do
  begin
    E := DSR.ExprList[i].E;
    C := DSR.ExprList[i].C;
    fid := GetId(DSR.ExprList[i].C);

    // Чтобы не было зацикливания
    if fid = GetId(Sender) then Continue;

    fn := FieldStr(fid);
    if FieldExists(FieldName, GetExpression(C)) then
    try
      V := E.Calc;
      if DSR.DataSet.State in [dsInsert, dsEdit] then
      begin
        DSR.DataSet.FieldByName(fn).Value:=V;

        if C is TdxLookupComboBox then
          with TdxLookupComboBox(C) do
            if KeyValue <> Null then Field.Value:=GetObjFieldValue(C, KeyValue, True)
            else Field.SetData(nil);
      end;
    except
      on E: Exception do
        DSR.Err.AddError(GetFieldName(DSR.ExprList[i].C), E.Message);
    end;
  end;

  Dec(FExprCounter);
end;

procedure TDataSetProcessor.CalcAggFields(DSR: TDataSetRec; FormName: String);
var
  i, fid: Integer;
  V: Variant;
  E: TExpression;
  fn: String;
  C: TComponent;
begin
  for i := 0 to DSR.ExprList.Count - 1 do
  begin
    E := DSR.ExprList[i].E;
    C := DSR.ExprList[i].C;
    fid := GetId(C);
    fn := FieldStr(fid);
    if FormExists(FormName, GetExpression(C)) then
    try
      V := E.Calc;
      if DSR.DataSet.State in [dsInsert, dsEdit] then
        DSR.DataSet.FieldByName(fn).Value:=V;
    except
      on E: Exception do
        DSR.Err.AddError(GetFieldName(DSR.ExprList[i].C), E.Message);
    end;
  end;
end;

function TDataSetProcessor.MakeFilter: String;
var
  i: Integer;
  S: String;
  PFm, Fm: TdxForm;
begin
  Result := '';
  PFm := GetDataSet(0)^.Form;
  S := SqlFormFilter(PFm, GetDataSet(0)^.Filter);
  if S <> '' then Result := S + ' and ';
  for i := 1 to FItems.Count - 1 do
  begin
    Fm := GetDataSet(i)^.Form;
    S := SqlFormFilter(Fm, GetDataSet(i)^.Filter);
    if S > '' then
    begin
      Result := Result + 'exists (select ' + TableStr(Fm.Id) + '.id from ' +
        TableStr(Fm.Id) + ' where ' + TableStr(Fm.Id) + '.pid=' +
        TableStr(PFm.Id) + '.id and ' + S + ') and ';
    end;
    GetDataSet(i)^.TblFilterSet:=S > '';
  end;
  // !!! Доступ
  S := UserMan.GetSelCond(PFm.Id);
  if S <> '' then
  begin
    S := SqlSelCondFilter(PFm, S);
    Result := Result + '(' + S + ') and '
  end;
  // Фильтр списка
  if FListFilter <> '' then
    Result := Result + '(' + FListFilter + ') and ';
  //
  Result := Copy(Result, 1, Length(Result) - 5);
end;

{function QueryLinkExists(RD: TReportData; aName: String): Boolean;
var
  i: Integer;
  Sr: TRpSource;
  S: String;
begin
  Result := False;
  aName := Utf8LowerCase(aName);
  for i := 0 to RD.Sources.Count - 1 do
  begin
    Sr := RD.Sources[i]^;
    S := Utf8LowerCase(Sr.Filter);
    if Utf8Pos(aName, S) > 0 then Exit(True);
  end;
  S := Utf8LowerCase(RD.Filter);
  if Utf8Pos(aName, S) > 0 then Exit(True);
end;

function _SetZeros(E: Extended; N: Integer): String;
var
  i: Integer;
  S: String;
begin
  S := IntToStr(Trunc(E));
  Result := S;
  for i := Length(S) to N - 1 do
    Result := '0' + Result;
end;

function GetTabOrderPath(C: TWinControl): String;
begin
  Result := '';
  if C = nil then Exit;
  Result := GetTabOrderPath(C.Parent) + _SetZeros(C.TabOrder, 4);
end;   }

function QueryExistsInQuery(RD: TReportData; const QueryName: String): Boolean;
var
  j: Integer;
  CF: TRpCalcField;
begin
  Result := True;
  for j := 0 to RD.Sources.Count - 1 do
  begin
    if FormExists(QueryName, RD.Sources[j]^.Filter) then Exit;
  end;
  if FormExists(QueryName, RD.Filter) then Exit;
  for j := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[j]^;
    if FormExists(QueryName, CF.Expr) then Exit;
  end;
  Result := False;
end;

// Запросы добавляются в список в порядке подчиненности: первые идут родительские.
procedure TDataSetProcessor.AddQueries(DSRi: Integer);
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;

  procedure _Add(pQ: PQueryRec);
  var
    i: Integer;
    Qry: TQueryRec;
    RD: TReportData;
    QueryName: String;
  begin
    if pQ = nil then Exit;
    //QueryName := ReportMan.FindReport(pQ^.Grid.Id).Name;
    QueryName := pQ^.RD.Name;
    for i := 0 to FQueries.Count - 1 do
    begin
      Qry := PQueryRec(FQueries[i])^;
      //RD := ReportMan.FindReport(Qry.Grid.Id);
      RD := Qry.RD;
      if QueryExistsInQuery(RD, QueryName) then
      begin
        FQueries.Insert(i, pQ);
        pQ^.Grid.DSP := Self;
        pQ^.Grid.QRi:=i;
        Exit;
      end;
    end;
    i := FQueries.Add(pQ);
    pQ^.Grid.DSP := Self;
    pQ^.Grid.QRi := i;
  end;

begin
  Fm := GetDataSet(DSRi)^.Form;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxQueryGrid) then
      _Add(CreateQuery(TdxQueryGrid(C), DSRi));
  end;
  for i := 0 to FQueries.Count - 1 do
    with PQueryRec(FQueries[i])^ do
    begin
      DataSet.Tag := i;
      Grid.Tag := i;
      Popup.Tag:=i;
    end;
end;

function TDataSetProcessor.CreateQuery(QG: TdxQueryGrid; DSRi: Integer
  ): PQueryRec;
var
  pQ: PQueryRec;
  RD: TReportData;
  FmId: Integer;
  CanV, CanA, CanD: Boolean;
  Bns: TGridButtons;
begin
  Result := nil;
  RD := ReportMan.FindReport(QG.Id);
  if RD.Sources.Count = 0 then Exit;
  RD := ReportMan.CreateReport(QG.Id);
  New(pQ);
  //n := FQueries.Add(pQ);
  pQ^.RD := RD;
  pQ^.DataSet := TSQLQuery.Create(nil);
  pQ^.DataSet.ParseSQL := False;
  pQ^.DataSet.DeleteSQL.Text := 'delete * from rdb$database';
  DBase.AttachDataSet(pQ^.DataSet);
  pQ^.DataSource := TDataSource.Create(nil);
  pQ^.DataSource.DataSet := pQ^.DataSet;
  //pQ^.DataSet.Tag := n;
  pQ^.DataSet.BeforeScroll:=@QueryBeforeScroll;
  pQ^.DataSet.AfterScroll:=@QueryAfterScroll;
  pQ^.DataSet.BeforeOpen:=@QueryBeforeOpen;
  pQ^.DataSet.AfterOpen:=@QueryAfterOpen;
  pQ^.DataSet.BeforeClose:=@QueryBeforeClose;
  pQ^.DataSet.AfterClose:=@QueryAfterClose;
  pQ^.Grid := QG;
  QG.ReadOnly:=True;
  QG.OnDblClick:=@QueryGridDblClick;
  QG.Options:= QG.Options - [dgTabs];
  QG.DataSource := pQ^.DataSource;
  //QG.Tag := n;
  InitGrid(QG, RD);
  pQ^.Popup := TPopupMenu.Create(nil);
  pQ^.DSProc := nil;
  pQ^.Simple := IsSimpleReport(RD);
  if pQ^.Simple then
  begin
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsAppend, 0,
      ShortCut(VK_INSERT, []), @QueryMenuHandler, 'add16') );
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsEdit, 1,
      ShortCut(VK_SPACE, []), @QueryMenuHandler, 'edit16') );
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsDelete, 2,
      ShortCut(VK_DELETE, [ssCtrl]), @QueryMenuHandler, 'delete16') );
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, '-', 3, 0, @QueryMenuHandler, '') );
    //if FGotoEnable and (FFm.ViewType <> vtGridOnly) then
    //begin
      pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsGoTo, 4, 0, @QueryMenuHandler, 'goto16') );
      pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, '-', 5, 0, @QueryMenuHandler, '') );
    //end;
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsRefresh, 6, 0, @QueryMenuHandler, 'refresh16') );

    // !!! Доступ
    FmId := StrToInt(RD.Sources[0]^.Id);
    CanV := UserMan.CheckFmVisible(FmId);
    CanA := UserMan.CheckFmAdding(FmId);
    //CanE := UserMan.CheckFmEditing(FmId);
    CanD := UserMan.CheckFmDeleting(FmId);
    with pQ^.Popup do
    begin
      Items[0].Visible:=CanV and CanA;
      Items[1].Visible:=CanV;
      Items[2].Visible:=CanV and CanD;
      Items[3].Visible:=CanV and FGotoEnable and (FFm.ViewType <> vtGridOnly);
      Items[4].Visible:=CanV and FGotoEnable and (FFm.ViewType <> vtGridOnly);
      Items[5].Visible:= Items[0].Visible or Items[1].Visible or Items[2].Visible
        or Items[3].Visible;
    end;


    if QG.ShowButtons then
    begin
      Bns := pQ^.Grid.Buttons;
      Bns.ShowButton(gbnAppend, CanV and CanA);
      Bns.ShowButton(gbnEdit, CanV);
      Bns.ShowButton(gbnDelete, CanV and CanD);
      Bns.ShowButton(gbnGoto, CanV and FGotoEnable and (FFm.ViewType <> vtGridOnly));
      QG.OnButtonClick:=@QueryGridButtonClick;
    end;
  end
  else
  begin
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsRefresh, 6, 0, @QueryMenuHandler, 'refresh16') );
    if QG.ShowButtons then
    begin
      Bns := pQ^.Grid.Buttons;
      Bns.ShowButton(gbnAppend, False);
      Bns.ShowButton(gbnEdit, False);
      Bns.ShowButton(gbnDelete, False);
      Bns.ShowButton(gbnGoto, False);
      QG.OnButtonClick:=@QueryGridButtonClick2;
    end;
  end;
  QG.PopupMenu := pQ^.Popup;
  //pQ^.Popup.Tag := QG.Tag;
  pQ^.DSRi := DSRi;
  QG.OnCanSort:=@QueryGridCanSort;
  QG.OnSortColumnChange:=@QueryGridSortChange;
  QG.OnDrawColumnCell:=@QueryGridDrawColumnCell;
  pQ^.Colors := TQueryColorList.Create;
  Result := pQ;
end;

procedure TDataSetProcessor.RequeryQuery(idx: Integer);
var
  Q: TQueryRec;
  RD: TReportData;
  DSR: TDataSetRec;
  //Errs: String;
begin
  if FInserting then Exit;

  Q := PQueryRec(FQueries[idx])^;
  Q.Colors.Clear;
  //RD := ReportMan.FindReport(Q.Grid.Id);
  RD := Q.RD;
  if RD.Sources.Count = 0 then Exit;

  Q.DataSet.Close;
  DSR := GetDataSet(Q.DSRi)^;
  try
    Q.DataSet.SQL.Text:=SqlReportSelect(RD, DSR.Form, GetDataSet(0)^.Form, DSR.DataSet);
  except
    on E: EFilterParserError do
    begin
      DSR.Err.AddError(Format(rsErrorSrcFlt, [RD.Name]), E.Message);
      Exit;
    end;
  end;

  Q.DataSet.ClearIndexes;
  Q.DataSet.MaxIndexesCount := 100;
  Q.DataSet.Fields.Clear;
  Q.DataSet.Open;
  SetQueryDisplayFormat(RD, Q.DataSet);
  CalcQuery(RD, Q.DataSet, DSR.Form, FMaster^.Form, DSR.DataSet, DSR.Err);
  try
    FilterQuery(RD, Q.DataSet, DSR.Form, FMaster^.Form, DSR.DataSet);
  except
    on E: Exception do
      DSR.Err.AddError(Format(rsErrorOutFlt, [RD.Name]), E.Message);
  end;
  if CalcFieldExistsInSort(RD) then
  	BuildSortIndexes(RD, Q.DataSet);

  CalcAggFields(DSR, RD.Name);
  CalcAggExprs(DSR, RD.Name);
  if Q.DataSet.RecordCount = 0 then QueryAfterScroll(Q.DataSet);// UpdateQueryPopupState(Q);
  BuildPivotTables(idx);
end;

function TDataSetProcessor.IsFilterSet: Boolean;
var
  i: Integer;
  DSP: TDataSetRec;
begin
  Result := False;
  for i := 0 to DataSetCount - 1 do
  begin
    DSP := DataSets[i]^;
    if DSP.Filter.ValuesExists then Exit(True);
  end;
end;

procedure TDataSetProcessor.RequeryQueries(DSRi: Integer);
var
  i: Integer;
begin
  for i := 0 to FQueries.Count - 1 do
    if PQueryRec(FQueries[i])^.DSRi = DSRi then
      RequeryQuery(i);
end;

procedure TDataSetProcessor.RequeryLinkedQueries(CurQ: PQueryRec;
  const QueryName: String);
var
  i: Integer;
  Q: TQueryRec;
  RD: TReportData;
begin
  for i := 0 to FQueries.Count - 1 do
  begin
    if PQueryRec(FQueries[i]) = CurQ then Continue;
    Q := PQueryRec(FQueries[i])^;
    //RD := ReportMan.FindReport(Q.Grid.Id);
    RD := Q.RD;
    if QueryExistsInQuery(RD, QueryName) then RequeryQuery(i);
  end;
end;

procedure TDataSetProcessor.RequeryQueriesWithParams(const FieldName: String;
  DSRi: Integer);
var
  i, j: Integer;
  Q: TQueryRec;
  RD: TReportData;
  CF: TRpCalcField;
  Found: Boolean;
begin
  for i := 0 to FQueries.Count - 1 do
  begin
    Q := PQueryRec(FQueries[i])^;
    if DSRi <> Q.DSRi then Continue;
    //RD := ReportMan.FindReport(Q.Grid.Id);
    RD := Q.RD;
    Found := False;
    for j := 0 to RD.Sources.Count - 1 do
    begin
      if FieldExists(FieldName, RD.Sources[j]^.Filter) then
      begin
        RequeryQuery(i);
        Found := True;
        Break;
      end;
    end;
    if (not Found) and FieldExists(FieldName, RD.Filter) then
    begin
      RequeryQuery(i);
      Found := True;
    end;
    if not Found then
      for j := 0 to RD.CalcFields.Count - 1 do
      begin
        CF := RD.CalcFields[j]^;
        if FieldExists(FieldName, CF.Expr) then
        begin
          RequeryQuery(i);
          Break;
        end;
      end;
  end;
end;

// Принцип работы. TdxLabel имеет свойство Value, куда записывается вычисленное
// значение. Если это свойство неопределено, то выражение вычисляется.
// При этом выражение может быть вычислено в процессе вычисления
// выражения другой надписи. Это позволяет уменьшить количество повторных вычислений.
procedure TDataSetProcessor.CalcExprs(DSR: TDataSetRec;
  const FieldName: String; aLabel: TdxLabel);
var
  i: Integer;
  L: TList;
  LE: TExprData;
  Lbl: TdxLabel;
begin
  if FInserting then Exit;

  // Обнаружения зацикливаний
  if FLblExprCounter > 50 then
  begin
    FLblExprCounter := 0;
    raise Exception.Create(Format(rsLoopDetected, [FieldName]));
  end;
  Inc(FLblExprCounter);
  //

  L := TList.Create;
  for i := 0 to DSR.LblExprList.Count - 1 do
  begin
    LE := DSR.LblExprList[i];
    Lbl := TdxLabel(LE.C);
    if Lbl = aLabel then Continue;
    if (FieldName = '') or (FieldExists(FieldName, Lbl.Expression)) then
    begin
      Lbl.Value := unassigned;
      L.Add(LE);
    end;
  end;

  for i := 0 to L.Count - 1 do
  begin
    LE := TExprData(L[i]);
    Lbl := TdxLabel(LE.C);
    if Lbl.Value = unassigned then
    try
      Lbl.Value := LE.E.Calc;
      if not FPrinting then
        Lbl.Caption := VarToStr(Lbl.Value);
      CalcExprs(DSR, Lbl.FieldName, Lbl);
    except
      on E: Exception do
        if not Printing then
        begin
          Lbl.Caption := Lbl.FieldName;
          DSR.Err.AddError(Lbl.FieldName, E.Message);
        end;
    end
    else if not FPrinting then
      Lbl.Caption := VarToStr(Lbl.Value);
  end;
  L.Free;
  Dec(FLblExprCounter);
end;

procedure TDataSetProcessor.CalcAggExprs(DSR: TDataSetRec;
  const FormName: String);
var
  i: Integer;
  L: TExprList;
  LE: TExprData;
  Lbl: TdxLabel;
begin
  L := DSR.LblExprList;
  for i := 0 to L.Count - 1 do
  begin
    LE := L[i];
    if FormExists(FormName, GetExpression(LE.C)) then
    try
      Lbl := TdxLabel(LE.C);
      Lbl.Value := LE.E.Calc;
      Lbl.Caption := VarToStr(Lbl.Value);
      CalcExprs(DSR, Lbl.FieldName, Lbl);
    except
      on E: Exception do
      begin
        Lbl.Caption := Lbl.FieldName;
        DSR.Err.AddError(Lbl.FieldName, E.Message);
      end;
    end;
  end;
end;

function TDataSetProcessor.CheckDeleteRecord(TId, RecId: Integer;
  ShowMsg: Boolean): Boolean;
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
  TNm, S, FNm: String;
begin
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    TNm := TableStr(Fm.Id);
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if (C is TdxLookupComboBox) and (TdxLookupComboBox(C).SourceTId = TId) then
      begin
        FNm := FieldStr(C);
        S := 'select first 1 id from ' + TNm + ' where ' + FNm + '=' + IntToStr(RecId);
        with DBase.OpenDataSet(S) do
        try
          if RecordCount > 0 then
          begin
            if ShowMsg then ErrMsg(Format(rsCantDeleteRecord, [Fm.FormCaption]));
            Exit(False);
          end;
        finally
          Free;
        end;
      end;
    end;
  end;
  Result := True;
end;

procedure TDataSetProcessor.RefreshCurRecord;
begin
  DataSetAfterScroll(MasterSet);
end;

procedure TDataSetProcessor.OpenFilter(DSRi: Integer);
var
  pD: PDataSetRec;
  OldFilter, NewFilter: String;
begin
  pD := GetDataSet(DSRi);
  pD^.Filter.Save(OldFilter);
  if pD^.FilterFm.ShowForm(pD^.Filter) = mrOk then
  begin
    Refresh;
    pD^.Filter.Save(NewFilter);
    if OldFilter <> NewFilter then
      pD^.FilterIndex:=-1;
    DoChangeFilter;
  end;
end;

procedure TDataSetProcessor.ApplyFilter(FmId: Integer; FilterIndex: Integer);
var
  pD: PDataSetRec;
begin
  pD := FindDataSet(FmId);
  pD^.Filter.Load(pD^.Form.Filters.ValueFromIndex[FilterIndex]);
  pD^.FilterIndex:=FilterIndex;
  Refresh;
  DoChangeFilter;
end;

procedure TDataSetProcessor.Recalculate(TId, FId: Integer; aExpr: String);
var
  DS0, DS: TDataSet;
  DSR: TDataSetRec;
  i: Integer;
  E: TExpression;
  Fl: TField;
  EB: TExpressionBuilder;
  ED: TExprData;
begin
  E := nil;
  aExpr := Trim(aExpr);
  DSR := FindDataSet(TId)^;
  if aExpr <> '' then
  begin
    EB := TExpressionBuilder.Create;
    EB.Form := DSR.Form;
    EB.ParentForm := FMaster^.Form;
    EB.DataSet := DSR.DataSet;
    EB.SkipLabels:=True;
    try try
      E := EB.Build(aExpr);
    except
      on Ex: Exception do
      begin
        ShowMessage(Ex.Message);
        Exit;
      end;
    end;
    finally
      EB.Free;
    end;
  end;

  if (E = nil) and (aExpr <> '') then Exit;
  if aExpr = '' then
  begin
    ED := DSR.ExprList.FindExpr(FId);
    if ED = nil then Exit;
    E := ED.E;
  end;

  try
    DSR.Form.BeginRecalc;

    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;

    DS0 := FMaster^.DataSet;
    DS := DSR.DataSet;

    FReCalculate:=True;
    for i := 0 to FItems.Count - 1 do
      GetDataSet(i)^.DataSet.DisableControls;
    //i := FItems.IndexOf(FindDataSet(TId));

    if DS0 = DS then
      Fl := DS0.FieldByName(FieldStr(FId));

    DS0.First;
    while not DS0.EOF do
    begin
      RequeryQueries(i);
      DS0.Edit;
      if DS <> DS0 then
      begin
        Fl := DS.FieldByName(FieldStr(FId));
        DS.First;
        while not DS.Eof do
        begin
          DS.Edit;
          Fl.Value:=E.Calc;
          DS.Post;
          DS.Next;
        end;
      end
      else
        Fl.Value := E.Calc;
      DS0.Post;
      DS0.Next;
    end;

  finally

    for i := 0 to FItems.Count - 1 do
      GetDataSet(i)^.DataSet.EnableControls;
    FReCalculate:=False;
    DataSetAfterScroll(DS0);

    if aExpr <> '' then FreeAndNil(E);
    Screen.Cursor:=crDefault;

    DSR.Form.EndRecalc;
  end;
end;

function TDataSetProcessor.DataSetCount: Integer;
begin
  Result := FItems.Count;
end;

function TDataSetProcessor.QueryCount: Integer;
begin
  Result := FQueries.Count;
end;

procedure TDataSetProcessor.ApplyTreeSelect(Keys: TList);
var
  i: Integer;
  F: TFilterField;
begin
  F := FMaster^.Filter.Findfield(FFm.GroupField);
  if F = nil then
  	F := FMaster^.Filter.AddField;
  F.FId := FFm.GroupField;
  F.Values.Clear;
  for i := 0 to Keys.Count - 1 do
  	F.Values.Add(IntToStr(PtrInt(Keys[i])));

  Refresh;
end;

// Принудительно сохраняем значение в активное поле перед проверкой ввода или
// перед проверкой на изменение
procedure TDataSetProcessor.ForceChangeFields(DSRi: Integer);
var
  DSR: TDataSetRec;
  Gr: TdxGrid;
  LR: TLookupRec;
  Frm: TCustomForm;
  C: TWinControl;
  i: Integer;
begin
  if DSRi = 0 then
  	for i := 1 to FItems.Count - 1 do
    	ForceChangeFields(i);

  // При изменении поля в самой таблице возможно ошибочно пройти проверку,
  // когда на самом деле значение некорректно. Это происходит
  // потому что validate срабатывает раньше, чем изменения попадут в поле.
  // Досрочно сохраняем изменения в поле. ЭТО В ТАБЛИЦЕ.
  DSR := GetDataSet(DSRi)^;
  Gr := DSR.Grid;
  if not (DSR.DataSet.State in [dsInsert, dsEdit]) then
  begin
    //if Gr.Editor <> nil then Gr.EditorMode := False;
  	Exit;
  end;

  if (Gr.Editor <> nil) and (Gr.Editor.Focused) then
  begin
    if (Gr.Editor is TPickListCellEditor) and (Gr.Editor.Tag >= 0) then
    begin
      LR := PLookupRec(FLookups[Gr.Editor.Tag])^;
      if LR.Control is TdxLookupComboBox then
      begin
        TdxLookupComboBox(LR.Control).Field.Value:=Gr.Editor.Caption;
        TPickListCellEditor(Gr.Editor).OnEditingDone:=nil;  // чтобы избежать повтороного вызова
        PickListEditingDone(Gr.Editor);
        Gr.EditorMode:=False;
      end
      // Компонент "Список".
      else
      begin
        Gr.SelectedField.Text := Gr.Editor.Caption;
      	Gr.EditorMode:=False;
      end;
    end
    else
    begin
      Gr.SelectedField.Text := Gr.Editor.Caption;
      Gr.EditorMode:=False;
    end;
  end;
  //

  // Сохраняем изменения в поле с фокусом, т. к. изменения не успевают попасть
  // в поле - проверка срабатывает раньше. ЭТО В ФОРМЕ.
  Frm := GetParentForm(DSR.Form);
  C := nil;
  if Frm <> nil then
  	C := Frm.ActiveControl;

  if (C <> nil) and (HasFId(C)) then
  begin
    if C is TdxLookupComboBox then TWinControl(C).EditingDone
    else if not (C is TdxCheckBox) then
    begin
      with DSR.DataSet.FieldByName(FieldStr(C)) do
        if Text <> TWinControl(C).Caption then
          Text := TWinControl(C).Caption;
    end;
  end;
end;

{procedure TDataSetProcessor.UpdateQueryRecs;
var
  i: Integer;
begin
  for i := 0 to FQueries.Count - 1 do
    with PQueryRec(FQueries)^ do
      RD := ReportMan.FindReport(Grid.Id);
end;         }

end.

