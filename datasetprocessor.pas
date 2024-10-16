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

unit DatasetProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, Controls, Db, SqlDb, Menus, dxctrls, strconsts,
  DbGrids, StdCtrls, Grids, ExtCtrls, editform, filterform, Graphics,
  expressions, DXReports, dsproclists, listform, Lists, DBCtrls, Buttons,
  myctrls, PopupNotifier, DxActions, Forms, ComCtrls, scriptmanager,
  myclasses, erroricon;

type
  TDataSetProcessor = class;

  PDataSetRec = ^TDataSetRec;
  TDataSetRec = record
    DataSet: TSQLQuery;
    DataSource: TDataSource;
    Grid: TdxGrid;
    Form: TdxForm;
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
    CanEdit, CanDelete: Boolean;				// Передаем в MainFrame
    EditingCtrls: TList;
    CanShoping: Boolean;
    FilterIndex: Integer;   // Предустановленный фильтр
    //TblFilterSet: Boolean;     // Для таблиц (если фильтр установлен)
    HasFields: Boolean;     // На случай, если в форме нет ни одного поля, чтобы не было ошибок.
    RunScript: TRunScript;
    MaxId: Integer;				// Используется в простых формах для определения следующего
    											// Id вместо ненужного обращения к базе.
    NeedRefresh: Boolean;
    NewRecord: Boolean;
    DeletedRecId: Integer;      // Удаляемая запись
    RowsExchanged: Boolean;     // перемещались ли строки в какой-либо таблице
    DetailsChanged: Boolean;    // Были ли изменения где-либо в подчиненной форме
    Images: TList;
  end;

  PLookupRec = ^TLookupRec;
  TLookupRec = record
    //DataSet: TSQLQuery;
    //DataSource: TDataSource;
    Control: TComponent;
    Column: TColumn;
    TId: Integer;
    //SQL: String;
    ListFm: TListWindow;
    NeedRefresh: Boolean;
    DSRi: Integer;
    //Popup: TPopupMenu;
    DSProc: TDataSetProcessor;
    LCbx: TdxLookupComboBox;
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
    NeedRefresh, Changed, ParentChanged: Boolean;
  end;

  { TCopyPasteMenu }

  TCopyPasteMenu = class(TPopupMenu)
  private
    FControl: TCustomEdit;
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property Control: TCustomEdit read FControl write FControl;
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
    //procedure LCbxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure QueryAfterClose(DataSet: TDataSet);
    procedure QueryAfterOpen(DataSet: TDataSet);
    procedure QueryBeforeClose(DataSet: TDataSet);
    procedure QueryBeforeOpen(DataSet: TDataSet);
    procedure QueryBeforeScroll(DataSet: TDataSet);
  private
    procedure FieldGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    //procedure DescrFileFieldChange(Sender: TField);
    procedure FloatCellKeyPress(Sender: TObject; var Key: char);
    procedure FileImageFieldChange(Sender: TField);
    //procedure CellMaskEditEditingDone(Sender: TObject);
    procedure GridCellClick(Column: TColumn);
    procedure GridPrepareCanvas(sender: TObject; DataCol: Integer;
      Column: TColumn; AState: TGridDrawState);
    procedure LCbxFillGrid(LCbx: TdxLookupComboBox; DS: TDataSet; OnlyClear: Boolean);
    procedure LCbxSetDisplayFormat(LCbx: TdxLookupComboBox; DS: TDataSet);
    procedure LCbxSetupParams(LCbx: TdxLookupComboBox; RD: TReportData);
    procedure LCbxClearParams(RD: TReportData);
    procedure LCbxFilterData(Sender: TObject; const Text: String);
    procedure LCbxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LCbxKeyMatch(Sender: TObject);
    procedure ListFieldSetText(Sender: TField; const aText: string);
    procedure QueryGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure QueryGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure QueryGridPrepareCanvas(sender: TObject; DataCol: Integer;
      Column: TColumn; AState: TGridDrawState);
    procedure TimerTimer(Sender: TObject);
    procedure QueryGridButtonClick2(Sender: TObject; Bn: TGridButtonType);
    procedure DataSetAfterOpen(DataSet: TDataSet);
    procedure DataSetBeforePost(DataSet: TDataSet);
    procedure FieldChange(Sender: TField);
    procedure FieldSetText(Sender: TField; const aText: string);
    procedure GridButtonClick(Sender: TObject; Bn: TGridButtonType);
    procedure GridCellCgrlick(Column: TColumn);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridCanSort(Sender: TObject; index: Integer; var Cancel: Boolean);
    procedure GridSortColumnChange(Sender: TObject);
    procedure GridValidate(Sender: TObject; var Ok: Boolean);
    procedure LCbxButtonClick(Sender: TObject);
    procedure ComboBoxNeedData(Sender: TObject);
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
    //procedure LookupMenuPopup(Sender: TObject);
    procedure MemoButtonClick(Sender: TObject);
    //procedure PickListEditingDone(Sender: TObject);
    procedure QueryAfterScroll(DataSet: TDataSet);
    procedure QueryGridButtonClick(Sender: TObject; Bn: TGridButtonType);
    procedure QueryGridDblClick(Sender: TObject);
    procedure QueryGridSortChange(Sender: TObject);
    procedure QueryMenuHandler(Sender: TObject);
    procedure QueryGotoRec(aQ: TQueryRec; aId: Integer);
    procedure UpdateQueryPopupStates(DSRi: Integer);
    procedure UpdateQueryPopupState(Q: TQueryRec);
    procedure MenuItemClick(Sender: TObject);
    procedure SetNeedRefresh(DSRi: Integer; QueriesOnly: Boolean = False);
    procedure SetNeedRefreshQueriesWithParams(DSRi: Integer; const FieldName: String);
    function SetNeedRefreshLinkedQueries(CurQ: PQueryRec): Boolean;
    procedure SetNeedBuildPivot(QRi: Integer);
  private
    FCallerObject: TComponent;
    FGotoEnable: Boolean;
    FItems: TList;
    FFm: TdxForm;
    FMaster: PDataSetRec;
    FOnStateChange: TNotifyEvent;
    FPrinting, FReCalculate, FDuplicateFlag, FIsListForm,
      FInserting, FIsRequeryAll, FIsCancel: Boolean;
    FCalcCounter: Integer;
    FLookups: TList;
    FQueries: TList;
    FTimer: TTimer;
    FNotif: TPopupNotifier;
    FSimpleMode: Boolean;
    FCPMenu: TCopyPasteMenu;
    procedure ExchangeRows(DSRi: Integer; MoveUp: Boolean);
    procedure AddEditingCtrls(DSRi: Integer);
    procedure ClearItems;
    procedure BindControls(DSR: TDataSetRec);
    procedure BuildExprs(DSR: PDataSetRec);
    procedure BuildDefVals(DSR: PDataSetRec);
    procedure BuildCheckExprs(DSR: PDataSetRec);
    procedure AddDataSet(aGrid: TdxGrid);
    function GetDataSet(Index: Integer): PDataSetRec;
    function FindDataSet(FmId: Integer): PDataSetRec;
    function CreatePopupMenu(IsMaster, AllowSpace: Boolean): TPopupMenu;
    function GetQueries(Index: Integer): PQueryRec;
    procedure RequeryDetails;
    procedure CloseDetails;
    procedure AddLookup(aGrid: TdxGrid; C: TComponent; DSRi: Integer);
    procedure AddLookups(aGrid: TdxGrid; DSRi: Integer);
    procedure ClearLookups;
    procedure ClearQueries;
    function FindLookupByColumn(C: TColumn): PLookupRec;
    function FindLookupById(Id: Integer): PLookupRec;
    procedure CalcFields(DSRi: Integer; const FieldName: String; Sender: TComponent);
    procedure CalcAggFields(DSRi: Integer; FormName: String);
    //function MakeSubFilter: String;
    procedure AddQueries(DSRi: Integer);
    function CreateQuery(QG: TdxQueryGrid; DSRi: Integer): PQueryRec;
    procedure RequeryQueries(aDSRi: Integer);
    procedure CalcExprs(DSRi: Integer);
    procedure RefreshComboBox(LR: TLookupRec);
    procedure ChangeObjectFields(Obj: TdxLookupComboBox);
    procedure ApplyQuickFilter(AClearFilter: Boolean);
    procedure ClearAllFilters;
    function CalcColor(Fm: TdxForm; DS: TDataSet): TColor;
    //procedure CalcQueryColor(RD: TReportData; Fm: TdxForm; RDS, DS: TDataSet;
    //  const TargetField: String; out FieldName: String; out Color: TColor);
    procedure InsertObjectValues(Obj: TdxLookupComboBox);
    procedure FillTableFromObject(Obj: TdxLookupComboBox);
    procedure RefreshLookupsWithParams(const FieldName: String);
    procedure PrepareListForm(pLR: PLookupRec);
    procedure SetColumnTitles(G: TdxGrid);
    procedure SetGrids;
    //function CalcEditCond(Fm: TdxForm; DS: TDataSet; const Cond: String): Boolean;
    procedure UpdatePopupState(DSRi: Integer);
    procedure UpdateControlState(DSRi: Integer);
    procedure CalcDefVals(DSR: TDataSetRec);
    procedure RefreshDataAndEdit;
    procedure ShowError(C: TControl; const Msg: String; DSR: TDataSetRec);
    procedure BuildPivotTables(idx: Integer);
    //procedure BuildAllPivotTables(DSRi: Integer);
    procedure UnBind;
    procedure MasterSetModified;
    procedure InsertTreeValues;
    procedure InsertCallerObjectValues;
    function CheckEditAccess(DSRi: Integer): Boolean;
    function CheckDeleteAccess(DSRi: Integer; Details: Boolean): Boolean;
    //function CheckRecordLock: Integer;
    function CheckRecordModify: Integer;
    function LockRecord(DoLock: Boolean): Integer;
    procedure UnlockRecord;
    procedure LockDSScroll(DS: TDataSet);
    procedure UnlockDSScroll(DS: TDataSet);
    procedure LockDSEdit(DS: TDataSet);
    procedure UnlockDSEdit(DS: TDataSet);
    procedure LockDSPost(DS: TDataSet);
    procedure UnlockDSPost(DS: TDataSet);
    procedure LockDSDelete(DS: TDataSet);
    procedure UnlockDSDelete(DS: TDataSet);
    procedure RefreshAllLookups(FmId: Integer);
    procedure RepaintAllGrids;
    //procedure ClearAllChildData(DSRi: Integer);
    procedure ClearCalcLabels(DSRi: Integer; const FieldName: String);
    procedure ClearAggCalcLabels(DSRi: Integer; const FormName: String);
    procedure GetTabOrderComponents(Fm: TdxForm; List: TList);
    procedure ClearChartSources(Fm: TdxForm);
    procedure FindImages(DSR: PDataSetRec);
    procedure UpdateRecIdFields(DSR: PDataSetRec);
  public
    constructor Create;
    destructor Destroy; override;
    procedure BindForm(FmId: Integer; IsListForm: Boolean; aViewType: TViewType);
    procedure BindDummyForm;
    procedure ClearDummyForm;
    procedure BindReport(RD: TReportData; RDS: TSQLQuery);
    procedure UnBindReport;
    function ShowEditForm(DSRi: Integer): Integer;
    procedure Open;
    //procedure _Open2(const aFilter: String; UseSelCond: Boolean; aForm: TdxForm);
    function OpenRecord(aKey: Integer): Boolean;
    procedure OpenReport;
    procedure Close;
    procedure Append;
    function AppendRecord(aPFm, aFm: TdxForm; aDS: TDataSet; RpId: Integer): Variant;
    function AppendObject: Integer;
    procedure DuplicateRecord(DSRi: Integer; All: Boolean);
    function InnerEdit(DSRi: Integer; ShowMsg, DoEditing, CallFromScript: Boolean): TAccessStatus;
    procedure Edit;
    function EditObject(ViewOnly: Boolean): Integer;
    function InnerDelete(DSRi: Integer; ShowMsg, DoDeleting, CallFromScript: Boolean): TAccessStatus;
    function Delete: Boolean;
    procedure DeleteChildRecords(RecId: Integer);
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
    procedure ApplyFilter(FilterIndex: Integer);
    procedure Recalculate(TId, FId: Integer; aExpr: String; UpdateObjects: Boolean);
    function DataSetCount: Integer;
    function QueryCount: Integer;
    procedure ForceChangeFields(DSRi: Integer);
    function CheckHierarchyToLoopbackReferences(DSR: TDataSetRec): Boolean;
    function Validate(DSRi: Integer; ForceChanges: Boolean = True): Boolean;
    function Print(TemplateName: String): Boolean;
    procedure InnerPrint(const TemplateName: String; var OutName: String; out Errs: String; AOpenFile, ChangeOutName: Boolean);
    procedure RequeryQuery(idx: Integer; Id: Integer = 0; ItSelf: Boolean = False);
    procedure RequeryDetail(i: Integer);
    function IsFilterSet: Boolean;
    procedure DoStateChange;
    function AnyDataSetModified(DSRi: Integer): Boolean;
    function RunAction(const ActionData: String; ADSRi: Integer): Variant;
    procedure RefreshAllData(DSRi: Integer);
    procedure HideNotif;
    procedure ShowImages(DSRi: Integer);
    procedure PrepareBeforeShowEditForm(DSRi: Integer);
    //procedure ClearQueryChangedFlag(DSRi: Integer);
    //property Printing: Boolean read FPrinting write FPrinting;
    property GotoEnable: Boolean read FGotoEnable write FGotoEnable;
    property Form: TdxForm read FFm;
    property DataSets[Index: Integer]: PDataSetRec read GetDataSet;
    property Queries[Index: Integer]: PQueryRec read GetQueries;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property CallerObject: TComponent read FCallerObject write FCallerObject;
    //property ReadOnly: Boolean read FReadOnly write FReadOnly;
  end;

implementation

uses
  dbengine, formmanager, sqlgen, apputils, mainframe,
  LCLType, StrUtils, dximages, BGRABitmap, BGRABitmapTypes,
  LazUtf8, Dialogs, dxfiles, Variants, reportmanager, dxusers,
  ButtonPanel, warningform, Clipbrd, errorsform, mainform, xmlreport,
  pivotgrid, formview, designerframe, DateUtils, dxmains, mydialogs, dxcharts,
  appimagelists, appsettings, exprfuncs;

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
  L: TSortColumns;
  i: Integer;
  CD: TSortColumn;
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
  SetLength(S, Length(S) - 1);
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

{ TCopyPasteMenu }

procedure TCopyPasteMenu.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: FControl.CutToClipboard;
    1: FControl.CopyToClipboard;
    2: FControl.PasteFromClipboard;
  end;
end;

procedure TCopyPasteMenu.MenuPopup(Sender: TObject);
begin
  Items[0].Enabled := not FControl.ReadOnly and (FControl.SelText <> '');
  Items[1].Enabled := FControl.SelText <> '';
  Items[2].Enabled := not FControl.ReadOnly;
end;

constructor TCopyPasteMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Images := Images16;
  Items.Add( CreateMenuItem(Self, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuHandler, IMG16_CUT) );
  Items.Add( CreateMenuItem(Self, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuHandler, IMG16_COPY) );
  Items.Add( CreateMenuItem(Self, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuHandler, IMG16_PASTE) );
  OnPopup:=@MenuPopup;
end;


{ TDatasetProcessor }

procedure SetDisplayFormat(Gr: TdxGrid; Fm: TdxForm);
var
  i, Id: Integer;
  C, CC: TComponent;
  Frm: TdxForm;
  S: String;
begin
  for i := 0 to Gr.Columns.Count - 1 do
  begin
    Id := Gr.Columns[i].Tag;
    C := FindById(Fm, Id);
    if C is TdxObjectField then
    begin
      with TdxObjectField(C) do
        if (ObjId > 0) and (FieldId > 0) then
        begin
          CC := FindById(Fm, ObjId);
          Frm := FormMan.FindForm(GetSourceTId(CC));
          C := FindById(Frm, FieldId);
        end;
    end
    else if C is TdxLookupComboBox then
    	with TdxLookupComboBox(C) do
      	if (SourceTId > 0) and (SourceFId > 0) then
        begin
          Frm := FormMan.FindForm(SourceTId);
          C := FindById(Frm, SourceFId);
        end;

    if (C is TdxCalcEdit) then
      with TdxCalcEdit(C) do
      begin
        S := PrecStr;
        Gr.Columns[i].DisplayFormat := S;
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
  if DSR.Form.ConfirmAutoSaveRecord and AnyDataSetModified(i) {DSR.DataSet.Modified} then
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
  end;

  if (i = 0) and (not MasterSet.Modified) then MasterSetModified;
  Ok := Validate(i, False);
end;

procedure TDataSetProcessor.ButtonClick(Sender: TObject);
begin
  with TdxButton(Sender) do
	  RunAction(ActionOnClick, Owner.Tag);
end;

procedure TDataSetProcessor.DataSetAfterClose(DataSet: TDataSet);
begin
  if FDuplicateFlag or FReCalculate or FPrinting then Exit;

  with GetDataSet(DataSet.Tag)^ do
  begin
    if Form.OnAfterClose <> nil then Form.OnAfterClose(Form);
  end;
end;

procedure TDataSetProcessor.DataSetBeforeCancel(DataSet: TDataSet);
begin
  with GetDataSet(DataSet.Tag)^ do
  begin
    NewRecord := DataSet.State = dsInsert;
    if Form.OnBeforeCancel <> nil then Form.OnBeforeCancel(Form);
  end;
end;

procedure TDataSetProcessor.DataSetBeforeClose(DataSet: TDataSet);
begin
  if FDuplicateFlag or FReCalculate or FPrinting then Exit;

  with GetDataSet(DataSet.Tag)^ do
  begin
    if Form.OnBeforeClose <> nil then Form.OnBeforeClose(Form);
  end;
end;

procedure TDataSetProcessor.DataSetBeforeDelete(DataSet: TDataSet);
begin
  with GetDataSet(DataSet.Tag)^ do
  begin
    if (dgMultiselect in Grid.Options) and  Grid.HandleAllocated then
      Grid.SelectedRows.CurrentRowSelected := False;
    DeletedRecId:=DataSet.FieldByName('id').AsInteger;
    if Form.OnBeforeDelete <> nil then Form.OnBeforeDelete(Form);
  end;
end;

procedure TDataSetProcessor.DataSetBeforeEdit(DataSet: TDataSet);
begin
  if FReCalculate then Exit;

  with GetDataSet(DataSet.Tag)^ do
  begin
    if Form.OnBeforeEdit <> nil then Form.OnBeforeEdit(Form);
  end;
end;

procedure TDataSetProcessor.DataSetBeforeInsert(DataSet: TDataSet);
begin
  if FDuplicateFlag then Exit;

  with GetDataSet(DataSet.Tag)^ do
  begin
    if Form.OnBeforeInsert <> nil then Form.OnBeforeInsert(Form);
  end;
end;

procedure TDataSetProcessor.DataSetBeforeOpen(DataSet: TDataSet);
begin
  if FDuplicateFlag or FReCalculate or FPrinting then Exit;

  with GetDataSet(DataSet.Tag)^ do
  begin
    if Form.OnBeforeOpen <> nil then Form.OnBeforeOpen(Form);
  end;
end;

procedure TDataSetProcessor.DataSetBeforeScroll(DataSet: TDataSet);
begin
  if FDuplicateFlag or FReCalculate or FPrinting then Exit;

  with GetDataSet(DataSet.Tag)^ do
  begin
    if Form.OnBeforeScroll <> nil then Form.OnBeforeScroll(Form);
  end;
end;

procedure TDataSetProcessor.QueryAfterClose(DataSet: TDataSet);
var
  Q: PQueryRec;
begin
  if FDuplicateFlag or FReCalculate or FPrinting then Exit;

  Q := PQueryRec(FQueries[DataSet.Tag]);
  with Q^.Grid do
    if OnAfterClose <> nil then OnAfterClose(Q^.Grid);
end;

procedure TDataSetProcessor.QueryAfterOpen(DataSet: TDataSet);
var
  Q: TQueryRec;
begin
  Q := PQueryRec(FQueries[DataSet.Tag])^;
  SetQueryDisplayFormat(Q.RD, Q.DataSet);

  {if FDuplicateFlag or FReCalculate or FPrinting then Exit;
  // Перенес в RequeryQuery (12.04.2020)
  with Q.Grid do
    if OnAfterOpen <> nil then OnAfterOpen(Q.Grid);}
end;

procedure TDataSetProcessor.QueryBeforeClose(DataSet: TDataSet);
var
  Q: PQueryRec;
begin
  if FDuplicateFlag or FReCalculate or FPrinting then Exit;

  Q := PQueryRec(FQueries[DataSet.Tag]);
  with Q^.Grid do
    if OnBeforeClose <> nil then OnBeforeClose(Q^.Grid);
end;

procedure TDataSetProcessor.QueryBeforeOpen(DataSet: TDataSet);
var
  Q: PQueryRec;
begin
  if FDuplicateFlag or FReCalculate or FPrinting then Exit;

  Q := PQueryRec(FQueries[DataSet.Tag]);
  with Q^.Grid do
    if OnBeforeOpen <> nil then OnBeforeOpen(Q^.Grid);
end;

procedure TDataSetProcessor.QueryBeforeScroll(DataSet: TDataSet);
var
  Q: PQueryRec;
begin
  if FDuplicateFlag or FReCalculate or FPrinting then Exit;

  Q := PQueryRec(FQueries[DataSet.Tag]);
  with Q^.Grid do
    if OnBeforeScroll <> nil then OnBeforeScroll(Q^.Grid);
end;

procedure TDataSetProcessor.FieldGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if not Sender.IsNull then
    aText := FormatDateTime(TDateTimeField(Sender).DisplayFormat, Sender.AsDateTime);
end;

{procedure TDataSetProcessor.DescrFileFieldChange(Sender: TField);
var
  i: PtrInt;
  DSR: TDataSetRec;
  S: String;
  C: TComponent;
begin
  if FDuplicateFlag then Exit;

  i := Sender.DataSet.Tag;
  DSR := GetDataSet(i)^;
  S := Sender.FieldName;
  S := Copy(S, 1, Length(S) - 1); // Удаляем ...d
  C := FindComponentByDataField(DSR.Form, S);

  S := GetFieldName(C);
  DSR.Form.ChangedField := S;
  RunAction(DSR.Form.ActionOnFieldChange, i);
  if (C <> nil) and (DSR.Form.OnFieldChange <> nil) then
  	DSR.Form.OnFieldChange(DSR.Form, C, S);
end;    }

procedure TDataSetProcessor.FloatCellKeyPress(Sender: TObject; var Key: char);
var
  DSR: TDataSetRec;
  OldKey: Char;
begin
  OldKey := Key;
  DSR := DataSets[TComponent(Sender).Tag]^;
  DSR.Grid.EditorKeyPress(Sender, Key);
  if (Key = #0) and (OldKey in [' ', '.']) then Key := DefaultFormatSettings.DecimalSeparator;
end;

procedure TDataSetProcessor.FileImageFieldChange(Sender: TField);
var
  i: PtrInt;
  DSR: TDataSetRec;
  S: String;
  C: TComponent;
begin
  if FDuplicateFlag then Exit;

  i := Sender.DataSet.Tag;
  DSR := GetDataSet(i)^;
  S := Sender.FieldName;
  S := Copy(S, 1, Length(S) - 3); // Удаляем ...src
  C := FindComponentByDataField(DSR.Form, S);

  S := GetFieldName(C);
  CalcFields(i, S, C);
  ClearCalcLabels(i, S);
  if not DSR.Form.IsHide then CalcExprs(i);
  //DSR.Form.FieldChanged:=S;
  //RunAction(DSR.Form.ActionOnFieldChange, i);
  if (C <> nil) and (DSR.Form.OnFieldChange <> nil) then
  	DSR.Form.OnFieldChange(DSR.Form, C, S);
end;

{procedure TDataSetProcessor.CellMaskEditEditingDone(Sender: TObject);
var
  Grid: TdxGrid;
  E: TMaskCellEditor;
begin
  E := TMaskCellEditor(Sender);
  Grid := TdxGrid(E.Grid);
  if MaskedTextEmpty(E.Text, E.EditMask) and
    (Grid.DataSource.DataSet.State in [dsInsert, dsEdit]) then
    Grid.SelectedField.SetData(nil);
end;   }

procedure TDataSetProcessor.LCbxFilterData(Sender: TObject; const Text: String);
var
  SQL: String;
  DS: TSQLQuery;
  LR: TLookupRec;
  Cbx: TdxLookupComboBox;
  Fm: TdxForm;
  QGrid: TdxQueryGrid;
  QryOpened: Boolean;
  RD: TReportData;
begin
  LR := PLookupRec(FLookups[TComponent(Sender).Tag])^;
  Cbx := TdxLookupComboBox(Sender);

  if Cbx.ListSource = 0 then
  begin
    DS := nil;
    try try
  	  SQL := SqlLCbxSelect(Cbx, GetDataSet(LR.DSRi)^.Form, Text, Trim(Text) <> '');
      DS := DBase.OpenDataSet(SQL);
      LCbxSetDisplayFormat(Cbx, DS);
      LCbxFillGrid(Cbx, DS, False);
    except
      on E: EFilterParserError do
      begin
        DataSets[LR.DSRi]^.Err.AddErrC(Cbx, rsFilter, Cbx.Filter, E);
        LCbxFillGrid(Cbx, nil, True);
      end;
    end;
    finally
      FreeAndNil(DS);
    end;
  end
  else if (Cbx.ListKeyField <> '') and (Cbx.ListFields.Count > 0) then
  begin
    SetTypedText(Text);
    Fm := TdxForm(LR.Control.Owner);
    QGrid := FindQueryGrid(Fm, Cbx.ListSource);

    if QGrid.DSP <> nil then
    begin

      RD := PQueryRec(FQueries[QGrid.QRi])^.RD;
      if not RD.SqlMode then
      begin
        RD.SearchText := Trim(Text);
        LCbxSetupParams(Cbx, RD);
      end;

      QryOpened := QGrid.Opened;
      QGrid.Refresh;
      LCbxFillGrid(Cbx, QGrid.DataSource.DataSet, False);
      if not QryOpened then QGrid.Close;

      if not RD.SqlMode then
      begin
        RD.SearchText := '';
        LCbxClearParams(RD);
      end;

    end;
  end;
end;

procedure TDataSetProcessor.LCbxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  LCbx: TdxLookupComboBox;
  pLR: PLookupRec;
begin
  if ((Key = VK_RETURN) and (Shift * [ssShift] = [])) or (Key = VK_ESCAPE) then
  begin
	  LCbx := TdxLookupComboBox(Sender);
    if not LCbx.DroppedDown then
    begin
  	  pLR := PLookupRec(FLookups[LCbx.Tag]);
  	  TdxGrid(pLR^.Column.Grid).EditorMode := False;
      // Глушим клавишу, чтобы не сработала кнопка по умолчанию в окне редактирования.
      Key := 0;
    end;
  end;
end;

procedure TDataSetProcessor.LCbxKeyMatch(Sender: TObject);
var
  Cbx: TdxLookupComboBox;
begin
  Cbx := TdxLookupComboBox(Sender);
  if Cbx.Field.DataSet.State in [dsInsert, dsEdit] then
  begin
    if Cbx.KeyValue = Null then
    	Cbx.Field.SetData(nil)
    else
    begin
	    Cbx.Field.Value := GetObjFieldValue(Cbx, Cbx.KeyValue, True);
    end;
  end;
end;

procedure TDataSetProcessor.ListFieldSetText(Sender: TField; const aText: string
  );
var
  S: String;
  E: Extended;
begin
  if Sender.DataType = ftFloat then
  begin
    S := StringReplace(aText, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);
    if TryStrToFloat(S, E) then
      Sender.AsString:=S
    else
      Sender.Value := Sender.Value;
  end
end;

procedure TDataSetProcessor.QueryGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  FlNm: String;
  ThumbF: TField;
  Col: TMyDBGridColumn;
begin
  Col := TMyDBGridColumn(Column);
  if Col.Field = nil then Exit;
  if Col.IsImage then
  begin
    FlNm := Column.Field.FieldName;
    ThumbF := Column.Field.DataSet.FieldByName(FlNm + 'thumb');
    DrawImageFieldIntoGrid(TDBGrid(Sender), Column, ThumbF, Rect);
  end;
end;

procedure TDataSetProcessor.QueryGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Q: TQueryRec;
begin
  if Key = VK_RETURN then
  begin
    Q := Queries[TComponent(Sender).Tag]^;
  	with Q.Popup.Items[1] do
    	if Visible and Enabled then Click;
  end;
end;

procedure TDataSetProcessor.QueryGridPrepareCanvas(sender: TObject;
  DataCol: Integer; Column: TColumn; AState: TGridDrawState);
var
  Q: TQueryRec;
  DSR: TDataSetRec;
  Clr: TColor;
  FlNm: String;
  RD: TReportData;
begin
  if Column.Field = nil then Exit;
  Q := PQueryRec(FQueries[TComponent(Sender).Tag])^;
  RD := Q.RD;
  if (Q.DataSet.RecordCount > 0) and (not (gdSelected in AState)) and (RD.Coloring.Count > 0) then
  begin
    DSR := GetDataSet(Q.DSRi)^;
    CalcQueryColor(RD, DSR.Form, Q.DataSet, DSR.DataSet, Column.FieldName,
      FlNm, Clr);
    if Clr = clNone then
      CalcQueryColor(RD, DSR.Form, Q.DataSet, DSR.DataSet, '', FlNm, Clr);
    if Clr <> clNone then
    begin
      if gdRowHighlight in AState then Clr := ColorToRGB(Clr) xor $1F1F1F;
	    Q.Grid.Canvas.Brush.Color:=Clr;
    end;
  end;
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
  DSR: TDataSetRec;
  Clr: TColor;
  FlNm: String;
  RD: TReportData;
begin
  if Column.Field = nil then Exit;
  Q := PQueryRec(FQueries[TComponent(Sender).Tag])^;
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
      if gdRowHighlight in State then Clr := ColorToRGB(Clr) xor $1F1F1F;
	    Q.Grid.Canvas.Brush.Color:=Clr;
  	  Q.Grid.Canvas.FillRect(Rect);
    end;
  end;
  TMyDBGrid(Sender).DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;    }

procedure TDataSetProcessor.DataSetAfterOpen(DataSet: TDataSet);
var
  DSR: TDataSetRec;
  i: Integer;
  C: TComponent;
begin
  DSR := GetDataSet(DataSet.Tag)^;

  SetDisplayFormat(DSR.Grid, DSR.Form);
  for i := 0 to DSR.Form.ComponentCount - 1 do
  begin
    C := DSR.Form.Components[i];
    if not HasFId(C) then Continue;
    if (C is TdxDBImage) or (C is TdxFile) then
    begin
      DataSet.FieldByName(FieldStr(C) + 'src').OnChange:=@FileImageFieldChange;
    end
    else
    begin
    	with DataSet.FieldByName(FieldStr(C)) do
      begin
      	OnChange := @FieldChange;
        if DataType in [ftDate, ftFloat, ftString, ftTime, ftInteger] then
        begin
	        OnSetText:=@FieldSetText;
          if DataType = ftTime then OnGetText:=@FieldGetText;
        end;
      end;
      if C is TdxLookupComboBox then
        with DataSet.FieldByName(FieldStr(C) + 'l') do
        begin
          if DataType = ftFloat then
            OnSetText:=@ListFieldSetText;
        end;
    end;

    if HasMaxLength(C) then SetMaxLength(C, GetFieldSize(C));
  end;

  DSR.Colors.Clear;

  if FDuplicateFlag or FReCalculate or FPrinting then Exit;

  //RunAction(DSR.Form.ActionOnAfterOpen, DataSet.Tag);

  if DSR.Form.OnAfterOpen <> nil then DSR.Form.OnAfterOpen(DSR.Form);
end;

procedure TDataSetProcessor.DataSetBeforePost(DataSet: TDataSet);
var
  DSR: TDataSetRec;
  Fm: TdxForm;
  C: TComponent;
  i{, FS}: Integer;
  F: TField;
begin
  // Посылаем Post дочерним формам.
  if DataSet.Tag = 0 then
  begin
    for i := 1 to FItems.Count - 1 do
    begin
      DSR := GetDataSet(i)^;
      if DSR.DataSet.State in [dsEdit, dsInsert] then DSR.DataSet.Post;
    end;
  end;

  DSR := GetDataSet(DataSet.Tag)^;
  Fm := DSR.Form;

  // Работают счетчики
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxCounter then
    begin
      F := DataSet.FieldByName(FieldStr(C));
      if (DataSet.State = dsInsert) and (F.IsNull) then
        F.Value:=DBase.GenId('gen_' + FieldStr(C));
    end;
  end;

  DSR.Colors.DeleteColor(DSR.DataSet.Fields[0].AsInteger);

  if FDuplicateFlag or FReCalculate then Exit;

  with DSR do
  begin
    //RunAction(Form.ActionOnBeforePost, Form.Tag);
    if Form.OnBeforePost <> nil then Form.OnBeforePost(Form);
  end;
end;

function TruncTime(Fmt: TdxTimeFormat; DT: TDateTime): TDateTime;
var
  n: Word;
begin
  n := RecodeLeaveFieldAsIs;
  DT := DT - Trunc(DT);     // Оставляем только время
  case Fmt of
    ttHH: Result := RecodeTime(DT, n, 0, 0, 0);
    ttHHMM: Result := RecodeTime(DT, n, n, 0, 0);
    ttHHMMSS: Result := RecodeTime(DT, n, n, n, 0);
  end;
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
  if C = nil then Exit;
  S := GetFieldName(C);

  if not Sender.IsNull then
  begin

    // Автоматическое округление числа до указанной точности.
    if C is TdxCalcEdit then
    begin
		  if CheckFloatFieldRange(GetFieldName(C), Sender.AsFloat, GetPrecission(C)) then
      begin
        Sender.OnChange:=nil;
        //if not Sender.IsNull then
		      Sender.AsFloat := AppUtils.MathRound(Sender.AsFloat, TdxCalcEdit(C).Precission);
	      Sender.OnChange:=@FieldChange;
      end
      else
    	  Sender.SetData(nil);
    end
    // Отсекаем время
    else if C is TdxDateEdit then
    begin
      Sender.OnChange:=nil;
      Sender.AsDateTime := Trunc(Sender.AsDateTime);
      Sender.OnChange:=@FieldChange;
    end
    // Автоматическое усечение времени до указанного формата
    else if C is TdxTimeEdit then
    begin
      Sender.OnChange:=nil;
      Sender.AsDateTime := TruncTime(TdxTimeEdit(C).TimeFormat, Sender.AsDateTime);
      Sender.OnChange:=@FieldChange;
    end
    // Пустая строка/маска в Null или обрезка лишних символов
    else if Sender.DataType = ftString then
    begin
      Sender.OnChange:=nil;
      if (C is TdxEdit) and (TdxEdit(C).EditMask <> '') and
        MaskedTextEmpty(Sender.AsString, TdxEdit(C).EditMask) then
        Sender.SetData(nil)
      else if Sender.AsString = '' then
        Sender.SetData(nil)
      else if not (C is TdxObjectField) then
        Sender.AsString := Utf8Copy(Sender.AsString, 1, GetFieldSize(C));
      Sender.OnChange:=@FieldChange;
    end;

  end;

  if FDuplicateFlag then
  begin
    CalcFields(i, S, C);
    Exit;
  end;

  SetNeedRefreshQueriesWithParams(i, S);
  ClearCalcLabels(i, S);
  RefreshLookupsWithParams(S);

  if C is TdxLookupComboBox then
  begin
    ChangeObjectFields(TdxLookupComboBox(C));
    InsertObjectValues(TdxLookupComboBox(C));
    if not FInserting then FillTableFromObject(TdxLookupComboBox(C));
  end;

  CalcFields(i, S, C);

  if not FInserting and not FRecalculate and (not DSR.Form.IsHide or FIsRequeryAll) then
  begin
    RequeryQueries(i);
    if not FIsRequeryAll then CalcExprs(i);
  end;

  if DSR.Form.OnFieldChange <> nil then DSR.Form.OnFieldChange(DSR.Form, C, S);
end;

procedure TDataSetProcessor.FieldSetText(Sender: TField; const aText: string);
var
  D: TDateTime;
  E: Extended;
  DSR: TDataSetRec;
  C: TComponent;
  N: Longint;
  S: String;
begin
  DSR := DataSets[Sender.DataSet.Tag]^;
  if aText = '' then
  begin
    if Sender.DataType = ftFloat then
    begin
      C := FindComponentByDataField(DSR.Form, Sender.FieldName);
      if (C <> nil) and (C is TdxCalcEdit) and TdxCalcEdit(C).NullToZero then
      	Sender.Value := 0
      else
      	Sender.SetData(nil);
    end
    else
    	Sender.SetData(nil);
    Exit;
  end;
  if Sender.DataType = ftDate then
  begin
    if TryStrToDate(StringReplace(aText, ' ', DefaultFormatSettings.DateSeparator,
      [rfReplaceAll]), D) then Sender.AsDateTime := D
    else Sender.Value := Sender.Value;
  end
  else if Sender.DataType = ftFloat then
  begin
    S := StringReplace(aText, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);
    if TryStrToFloat(S, E) then
    begin
      C := FindComponentByDataField(DSR.Form, Sender.FieldName);
      if (C = nil) or CheckFloatFieldRange(GetFieldName(C), E, GetPrecission(C)) then
		    Sender.AsString:=S
      else
	      Sender.Value := Sender.Value;
    end
    else Sender.Value := Sender.Value;
  end
  else if Sender.DataType = ftInteger then
  begin
    if TryStrToInt(aText, N) then
    	Sender.AsString := aText
    else
      Sender.Value := Sender.Value;
  end
  else if Sender.DataType = ftTime then
  begin
    if TryStrToTime(StringReplace(aText, ' ', ':', [rfReplaceAll]), D) then
    begin
      C := FindComponentByDataField(DSR.Form, Sender.FieldName);
      Sender.AsDateTime:=D;//TruncTime(TdxTimeEdit(C).TimeFormat, D);
    end
    else Sender.Value := Sender.Value;
  end
  else if Sender.DataType in [ftString] then
    Sender.AsString := aText;
end;

procedure TDataSetProcessor.GridButtonClick(Sender: TObject; Bn: TGridButtonType
  );
var
  Pop: TPopupMenu;
  G: TdxGrid;
begin
  G := TdxGrid(Sender);
  if G.CanFocus then G.SetFocus;
  Pop := DataSets[G.Tag]^.Popup;
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

procedure TDataSetProcessor.GridCellCgrlick(Column: TColumn);
begin

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
  else if (C is TdxLookupComboBox) and (FGotoEnable) and
    ((DSR.Form.PId = 0) or (FMaster^.Form.ViewType <> vtGridOnly)) then
  begin
    if ssCtrl in GetKeyShiftState then
      with TdxLookupComboBox(C) do
        if (KeyValue <> Null) and (not Testing) then
          MainFr.GotoRec(SourceTId, KeyValue);
  end;
end;

procedure TDataSetProcessor.GridPrepareCanvas(sender: TObject;
  DataCol: Integer; Column: TColumn; AState: TGridDrawState);
var
  DSR: TDataSetRec;
  RecId: LongInt;
  CD: TColorData;
  Clr: TColor;
begin
  if Column.Field = nil then Exit;
  DSR := GetDataSet(TdxGrid(Sender).Tag)^;
  RecId := DSR.DataSet.Fields[0].AsInteger;
  if (RecId > 0) and (not (gdSelected in AState)) and (DSR.Form.Coloring.Count > 0) then
  begin
    CD := DSR.Colors.FindColor(RecId);
    if CD = nil then
      CD := DSR.Colors.AddColor(RecId, CalcColor(DSR.Form, DSR.DataSet));
    if CD.Color <> clNone then
    begin
      Clr := CD.Color;
      if gdRowHighlight in AState then Clr := ColorToRGB(Clr) xor $1F1F1F;
      DSR.Grid.Canvas.Brush.Color:=Clr;
    end;
  end;
end;

procedure TDataSetProcessor.LCbxFillGrid(LCbx: TdxLookupComboBox; DS: TDataSet;
  OnlyClear: Boolean);
var
  r, i, idx: Integer;
  Grid: TDropDownList;
  DSFields: TList;
  SrcFm: TdxForm;
  RD: TReportData;
  LF: TLCbxListField;
  C: TComponent;

  procedure AddColumn(const Caption: String; W: Integer; IsCheckBox, ASearchable: Boolean);
  begin
    with Grid.Columns.Add do
    begin
			Title.Caption := Caption;
      if W > 0 then
      begin
	      SizePriority := 0;
        Width := W;
      end
      else
        SizePriority := 1;
      if IsCheckBox then ButtonStyle:=cbsCheckboxColumn;
      Searchable := ASearchable;
    end;
  end;

begin
  Grid := LCbx.DropDownList;

  SrcFm := FormMan.FindForm(LCbx.SourceTId);
  RD := ReportMan.FindReport(LCbx.ListSource);

  Grid.BeginUpdate;
  Grid.Columns.Clear;
  if LCbx.ListSource = 0 then
  begin
    C := FindById(SrcFm, LCbx.SourceFId);
    AddColumn(GetFieldName(C), 0, False, True);
    for i := 0 to LCbx.ListFields.Count - 1 do
    begin
      LF := LCbx.ListFields[i];
      C := FindById(SrcFm, LF.FieldId);
      AddColumn(GetFieldName(C), LF.Width, C is TdxCheckBox, LF.Searchable);
    end;
  end
  else
  begin
    for i := 0 to LCbx.ListFields.Count - 1 do
    begin
      LF := LCbx.ListFields[i];
      idx := RD.IndexOfNameDS(LF.FieldName);
      AddColumn(RD.GetFieldName(idx), LF.Width, C is TdxCheckBox, LF.Searchable);
    end;
  end;

  if LCbx.ListFields.Count = 0 then
    Grid.Options := Grid.Options - [loTitles]
  else
    Grid.Options := Grid.Options + [loTitles];

  Grid.EndUpdate;

  if OnlyClear then
  begin
    Grid.RowCount := Grid.FixedRows;
    Grid.RowCount := Grid.RowCount + 1;
    Exit;
  end;

  DSFields := TList.Create;

  if LCbx.ListSource = 0 then
  begin
    for i := 0 to DS.Fields.Count - 1 do
      DSFields.Add(DS.Fields[i]);
  end
  else
  begin
    DSFields.Add(DS.FieldByName(LCbx.ListKeyField));
    for i := 0 to LCbx.ListFields.Count - 1 do
      DSFields.Add(DS.FieldByName(LCbx.ListFields[i].FieldName));
  end;

  DS.Last;
  DS.First;

  Grid.BeginUpdate;
  Grid.RowCount := DS.RecordCount + Grid.FixedRows;
  Grid.Row := 0;

  r := Grid.FixedRows;
  while not DS.EOF do
  begin
    Grid.RecId[r] := TField(DSFields[0]).AsInteger;
    for i := 1 to DSFields.Count - 1 do
      with TField(DSFields[i]) do
        if DataType <> ftMemo then
          Grid.Cells[i-1, r] := DisplayText
        else
          Grid.Cells[i-1, r] := AsString;
    DS.Next;
    Inc(r);
  end;
  Grid.EndUpdate;

  DSFields.Free;
end;

procedure TDataSetProcessor.LCbxSetDisplayFormat(LCbx: TdxLookupComboBox;
  DS: TDataSet);
var
  SrcFm: TdxForm;
  C: TComponent;
  i: Integer;
begin
  SrcFm := FormMan.FindForm(LCbx.SourceTId);
  C := FindById(SrcFm, LCbx.SourceFId);
  SetDSFieldDisplayFormat(DS.Fields[1], GetComponentDisplayFormat(SrcFm, C));
  for i := 0 to LCbx.ListFields.Count - 1  do
  begin
    C := FindById(SrcFm, LCbx.ListFields[i].FieldId);
    SetDSFieldDisplayFormat(DS.Fields[i+2], GetComponentDisplayFormat(SrcFm, C));
  end;
end;

procedure TDataSetProcessor.LCbxSetupParams(LCbx: TdxLookupComboBox;
  RD: TReportData);
var
  L: TLCbxListFields;
  i, idx: Integer;
  LF: TLCbxListField;
  pF: PRpField;
begin
  L := LCbx.ListFields;
  for i := 0 to L.Count - 1 do
  begin
    LF := L[i];
    if not LF.Searchable then Continue;
    idx := RD.IndexOfNameDS(LF.FieldName);
    if idx < 0 then Continue;

    pF := RD.TryGetRpField(idx);
    if pF <> nil then pF^.TextSearch := True;
  end;
end;

procedure TDataSetProcessor.LCbxClearParams(RD: TReportData);
var
  i: Integer;
begin
  if RD.Sources.Count = 0 then Exit;

  for i := 0 to RD.Sources[0]^.Fields.Count - 1 do
    RD.Sources[0]^.Fields[i]^.TextSearch := False;
end;

procedure TDataSetProcessor.GridDblClick(Sender: TObject);
var
  G: TDBGrid;
  P: TPoint;
begin
  G := TDBGrid(Sender);
  if dgTitles in G.Options then
  begin
    P := G.ScreenToClient(Mouse.CursorPos);
    P := G.MouseToCell(P);
    if P.y = 0 then Exit;
  end;
  DataSets[G.Tag]^.Popup.Items[1].Click;
end;

procedure TDataSetProcessor.GridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  Col: TMyDBGridColumn;
begin
  if Column.Field = nil then Exit;
  Col := TMyDBGridColumn(Column);
  if Col.IsImage then
    DrawImageFieldIntoGrid(TDBGrid(Sender), Col, Column.Field, Rect);
end;

(*procedure TDataSetProcessor.GridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);

  procedure GetImagePos(Col: TMyDBGridColumn; Bmp: TBGRABitmap; out x, y: Integer);
  var
    al: TAlignment;
    lay: TTextLayout;
  begin
    al := Col.Alignment;
    lay := Col.Layout;
    case al of
      taLeftJustify: x := Rect.Left + ScaleToScreen(2);
      taCenter: x := Rect.Left + Rect.Width div 2 - Bmp.Width div 2;
      taRightJustify: x := Rect.Right - Bmp.Width - ScaleToScreen(2);
    end;
    case lay of
      tlTop: y := Rect.Top + ScaleToScreen(2);
      tlCenter: y := Rect.Top + Rect.Height div 2 - Bmp.Height div 2;
      tlBottom: y:= Rect.Bottom - Bmp.Height - ScaleToScreen(2);
    end;
  end;

var
  DSR: TDataSetRec;
  C: TComponent;
  x, y: Integer;
  Bmp: TBGRABitmap;
  St: TStream;
begin
  if Column.Field = nil then Exit;
  DSR := GetDataSet(TdxGrid(Sender).Tag)^;

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
          if (St <> nil) and (St.Size > 0) then Bmp.LoadFromStream(St);
          GetImagePos(TMyDBGridColumn(Column), Bmp, x, y);
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
  end;
end; *)

procedure TDataSetProcessor.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  DSR: TDataSetRec;
  i: PtrInt;
begin
  i := TComponent(Sender).Tag;

  if Key = VK_RETURN then
  begin
    DSR := DataSets[i]^;
    if not (DSR.DataSet.State in [dsInsert, dsEdit]) then
	    DSR.Popup.Items[1].Click
    else
    begin
      // Если таблица редактируемая, то после перехода в редактирование и ,
      // последующего нажатия ENTER редактор ячейки не появляется. Принудительно
      // заставляем сработать события SelectEditor.
      with TdxGrid(Sender) do
    	  if (not ReadOnly) and (Editor = nil) then ForceSelectEditor;
    end;
  end
  else if Key = VK_ESCAPE then
  begin
    DSR := GetDataSet(i)^;
    if DSR.DataSet.State in [dsInsert, dsEdit] then
    begin
      ForceChangeFields(i);
      if DSR.Form.ConfirmCancelEditing and AnyDataSetModified(i) {DSR.DataSet.Modified} then
      begin
        if MessageDlg(rsWarning, rsConfirmCancelEditMsg, mtConfirmation,
          [mbYes, mbNo], 0) <> mrYes then
        begin
          //Key := 0;
          Exit;
        end;
      end;
      DSR.DataSet.Cancel;
      //UpdateControlState(DSR);
    end;
  end;
end;

procedure TDataSetProcessor.GridCanSort(Sender: TObject; index: Integer;
  var Cancel: Boolean);
var
  G: TdxGrid;
  Col: TColumn;
begin
  G := TdxGrid(Sender);
  Col := G.Columns[Index-G.FixedCols];
  if (MasterSet.State in [dsInsert, dsEdit]) or (Col.Tag = 0) then
    Cancel := True;
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
  mr: Integer;
begin
  pLR := PLookupRec(FLookups[TComponent(Sender).Tag]);
  C := TdxLookupComboBox(Sender);
  if C.CanFocus then C.SetFocus;
  try
    PrepareListForm(pLR);
  except
    on E: ESQLSelectStatementError do
    begin
      DataSets[pLR^.DSRi]^.Err.AddErrC(C, rsFilter, C.Filter, E);
      Exit;
    end;
  end;
  Fm := pLR^.ListFm;
  Fm.DestForm := nil;
  Fm.DestMemo := nil;
  Fm.ValueField := C.SourceFId;
  Fm.Caption:=C.FieldName;
  Fm.Key := C.KeyValue;
  try
    mr := Fm.ShowForm;
  except
    on E: ESQLSelectStatementError do
    begin
      DataSets[pLR^.DSRi]^.Err.AddErrC(C, rsFilter, C.Filter, E);
      Exit;
    end;
  end;
  if mr = mrOk then
  begin
    if C.Field.DataSet.State in [dsInsert, dsEdit] then
    begin
      C.Field.Value := Fm.Value;
      C.KeyValue := Fm.Key;
    end;
  end;
  if C.CanFocus then
  begin
    C.SetFocus;
    C.SelStart := Utf8Length(C.Text);
  end;
end;

procedure TDataSetProcessor.ComboBoxNeedData(Sender: TObject);
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

  procedure CancelUpdates;
  var
    i: Integer;
    DS: TSQLQuery;
  begin
    for i := 1 to DataSetCount - 1 do
    begin
      DS := DataSets[i]^.DataSet;
      if DS.State in [dsInsert, dsEdit] then DS.Cancel;
      if DS.ChangeCount > 0 then
      begin
        if not FFm.IsHide then RequeryDetail(i)
        else DataSets[i]^.NeedRefresh := True;
      end;
      DataSets[i]^.RowsExchanged:=False;
    end;
  end;

  procedure CancelQueries(ParDSRi, DSRi: Integer);
  var
    i: Integer;
    pQ: PQueryRec;
    IsHide: Boolean;
  begin
    IsHide := GetDataSet(DSRi)^.Form.IsHide;
    for i := 0 to QueryCount - 1 do
    begin
      pQ := Queries[i];
      if pQ^.DSRi <> DSRi then Continue;
      if pQ^.Changed then
      begin
        pQ^.Changed := False;
        pQ^.NeedRefresh := True;
        SetNeedRefreshLinkedQueries(pQ);
      end;
      if (ParDSRi <> DSRi) and pQ^.ParentChanged then
      begin
        pQ^.ParentChanged := False;
        pQ^.NeedRefresh := True;
        SetNeedRefreshLinkedQueries(pQ);
      end;
      if not IsHide and pQ^.NeedRefresh then
      begin
        FIsCancel := True;
        RequeryQuery(i);
        BuildPivotTables(i);
        FIsCancel := False;
      end;
      if not pQ^.NeedRefresh and (pQ^.DataSet.RecNo > 1) then pQ^.DataSet.First;
    end;

    if DSRi = 0 then
      for i := 1 to DataSetCount - 1 do
        CancelQueries(0, i);
  end;

  procedure ClearChangedLabels(ParDSRi, DSRi: Integer);
  var
    i: Integer;
    LE: TExprData;
    DSR: TDataSetRec;
  begin
    DSR := GetDataSet(DSRi)^;
    for i := 0 to DSR.LblExprList.Count - 1 do
    begin
      LE := DSR.LblExprList[i];
      if LE.Changed then
      begin
        LE.Changed := False;
        TdxLabel(LE.C).Value := unassigned;
      end;
      if (ParDSRi <> DSRi) and LE.ParentChanged then
      begin
        LE.ParentChanged := False;
        TdxLabel(LE.C).Value := unassigned;
      end;
    end;
    if DSRi = 0 then
      for i := 1 to DataSetCount - 1 do
        ClearChangedLabels(0, i);
  end;

  procedure ClearQueryChangedFlag(aDSRi: Integer);
  var
    i: Integer;
  begin
    for i := 0 to QueryCount - 1 do
      with Queries[i]^ do
        if DSRi = aDSRi then
        begin
          Changed := False;
          ParentChanged := False;
        end;
  end;

  procedure ClearLabelChangedFlag(DSRi: Integer);
  var
    L: TExprList;
    i: Integer;
  begin
    L := GetDataSet(DSRi)^.LblExprList;
    for i := 0 to L.Count - 1 do
      with L[i] do
      begin
        Changed := False;
        ParentChanged := False;
      end;
  end;

var
  DSR: TDataSetRec;
  i: PtrInt;
begin
  DSR := DataSets[DataSet.Tag]^;
  i := DataSet.Tag;
  if i = 0 then
  begin
    if not FSimpleMode then UnLockRecord;
    FMaster^.RowsExchanged:=False;
    FMaster^.DetailsChanged:=False;
  end;

  if DSR.NewRecord then
  begin
    SetNeedRefresh(i);
    ClearCalcLabels(i, '');
    ClearLabelChangedFlag(i);
    ClearQueryChangedFlag(i);
    if not DSR.Form.IsHide then
      RefreshAllData(i);
  end
  else
  begin
    if i = 0 then CancelUpdates;
    CancelQueries(i, i);
    ClearChangedLabels(i, i);

    if not DSR.Form.IsHide then
    begin
      CalcExprs(i);
      ShowImages(i);
      RefreshLookupsWithParams('');
    end;
  end;

  UpdateQueryPopupStates(i);
  UpdateControlState(i);

  //RunAction(DSR.Form.ActionOnAfterCancel, DataSet.Tag);
  if DSR.Form.OnAfterCancel <> nil then DSR.Form.OnAfterCancel(DSR.Form);
end;

procedure TDataSetProcessor.DataSetAfterDelete(DataSet: TDataSet);
var
  DSR: TDataSetRec;
begin
  DSR := GetDataSet(DataSet.Tag)^;
  if DataSet.Tag > 0 then
  begin
    MasterSetModified;
    FMaster^.DetailsChanged := True;
    CalcAggFields(0, DSR.Form.FormCaption);
    ClearAggCalcLabels(0, DSR.Form.FormCaption);
    if not FFm.IsHide then CalcExprs(0);
  end
  else if not FSimpleMode then
  begin
    DeleteChildRecords(DSR.DeletedRecId);
    DBase.ApplyDataset(DSR.DataSet);
    DBase.Commit;

    RefreshAllLookups(FMaster^.Form.Id);
  end;
  UpdateControlState(DataSet.Tag);

  //RunAction(DSR.Form.ActionOnAfterDelete, DataSet.Tag);
  if DSR.Form.OnAfterDelete <> nil then DSR.Form.OnAfterDelete(DSR.Form);
end;

procedure TDataSetProcessor.DataSetAfterEdit(DataSet: TDataSet);
var
  DSR: TDataSetRec;
begin
  if FRecalculate then Exit;

  DSR := GetDataSet(DataSet.Tag)^;

  //if DataSet.Tag > 0 then	MasterSetModified;
  UpdateQueryPopupStates(DataSet.Tag);
  UpdateControlState(DataSet.Tag);

  //RunAction(DSR.Form.ActionOnAfterEdit, DataSet.Tag);
  if DSR.Form.OnAfterEdit <> nil then DSR.Form.OnAfterEdit(DSR.Form);
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
  pDSR: PDataSetRec;
  DSR: TDataSetRec;
  i: Integer;
  Fm: TdxForm;
  C: TComponent;
  FNm: String;
  F: TField;
begin
  //ClearAllChildData(DataSet.Tag);

  FInserting := True;

  pDSR := GetDataSet(DataSet.Tag);
  DSR := pDSR^;
  Fm := DSR.Form;
  if not FSimpleMode then
	  DataSet['id'] := DBase.GenId('gen_' + TableStr(Fm.Id))
  else
  begin
    Inc(pDSR^.MaxId);
    DataSet['id'] := pDSR^.MaxId;
  end;
  if DataSet.Tag > 0 then
  begin
    DataSet['pid'] := MasterSet['id'];
    DataSet['oldid'] := DataSet['id'];
  end;

  if not FDuplicateFlag then
  begin

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
      else if C is TdxRecordId then
        F.Value := DataSet['id']
    end;
    CalcDefVals(DSR);
    if Fm.PId = 0 then
    begin
      InsertCallerObjectValues;
      InsertTreeValues;
    end;

    //RunAction(DSR.Form.ActionOnAfterInsert, DataSet.Tag);
    if DSR.Form.OnAfterInsert <> nil then DSR.Form.OnAfterInsert(DSR.Form);
  end
  else
  begin
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if not (C is TdxRecordId) then Continue;
      DataSet.FieldByName(FieldStr(C)).Value := DataSet['id'];
    end;
  end;

  FInserting := False;
end;

procedure TDataSetProcessor.DataSetAfterPost(DataSet: TDataSet);

  procedure ApplyDetails;
  var
    i: Integer;
  begin
    for i := 1 to FItems.Count - 1 do
    begin
      // Применяем изменения только для обновленных наборов. Если набор не обновлен,
      // то ApplyDataSet приводит к удалению данных (баг от jurist23rus).
      if not GetDataSet(i)^.NeedRefresh then
        DBase.ApplyDataSet(GetDataSet(i)^.DataSet);
    end;
  end;

  procedure ClearQueryChangedFlag(ParDSRi, DSRi: Integer);
  var
    i: Integer;
    pQ: PQueryRec;
  begin
    for i := 0 to QueryCount - 1 do
    begin
      pQ := Queries[i];
      if pQ^.DSRi = DSRi then
      begin
        if (ParDSRi > 0) and pQ^.Changed then pQ^.ParentChanged := True;
        if ParDSRi <> DSRi then pQ^.ParentChanged := False;
        pQ^.Changed := False;
      end;
    end;
  end;

  procedure ClearLabelChangedFlag(ParDSRi, DSRi: Integer);
  var
    i: Integer;
    DSR: TDataSetRec;
    LE: TExprData;
  begin
    DSR := GetDataSet(DSRi)^;
    for i := 0 to DSR.LblExprList.Count - 1 do
    begin
      LE := DSR.LblExprList[i];
      if (ParDSRi > 0) and LE.Changed then LE.ParentChanged := True;
      if ParDSRi <> DSRi then LE.ParentChanged:=False;
      LE.Changed:=False;
    end;

    if DSRi = 0 then
      for i := 1 to DataSetCount - 1 do
        ClearLabelChangedFlag(0, i);
  end;

  procedure ResetOldId(DS: TDataSet);
  var
    B: TBookmark;
  begin
    B := DS.GetBookmark;
    LockDSScroll(DS);
    LockDSEdit(DS);
    LockDSPost(DS);
    DS.DisableControls;
    try
      DS.First;
      while not DS.Eof do
      begin
        DS.Edit;
        DS['oldid'] := DS['id'];
        DS.Post;
        DS.Next;
      end;
    finally
      DS.GotoBookmark(B);
      DS.FreeBookmark(B);
      DS.EnableControls;
      UnlockDSScroll(DS);
      UnlockDSEdit(DS);
      UnlockDSPost(DS);
    end;
  end;

var
  DSR: TDataSetRec;
  i, j: PtrInt;
begin
  i := DataSet.Tag;
  DSR := GetDataSet(i)^;
  if not DSR.HasFields or FDuplicateFlag then Exit;

  if i = 0 then
  begin
    if not FSimpleMode then
    begin

      DBase.ApplyDataSet(TSQLQuery(DataSet));
      ApplyDetails;
      DBase.Commit;
      if not FReCalculate then
      begin
	      UnLockRecord;
  	    RefreshAllLookups(FFm.Id);
      end;

      for j := 1 to FItems.Count - 1 do
        with GetDataSet(j)^ do
          if not NeedRefresh and RowsExchanged then
          begin
            ResetOldId(DataSet);
            RowsExchanged:=False;
          end;
    end;
    FMaster^.RowsExchanged:=False;
    FMaster^.DetailsChanged:=False;
  end
  else
  begin
    CalcAggFields(0, DSR.Form.FormCaption);
    if not FReCalculate then
    begin
      ClearAggCalcLabels(0, DSR.Form.FormCaption);
      if not FFm.IsHide then
        CalcExprs(0);
    end;
    MasterSetModified;
    FMaster^.DetailsChanged := True;
  end;

  if FReCalculate then Exit;

  ClearQueryChangedFlag(i, i);
  ClearLabelChangedFlag(i, i);
  UpdateQueryPopupStates(i);
  UpdateControlState(i);

  //RunAction(DSR.Form.ActionOnAfterPost, DataSet.Tag);
  if DSR.Form.OnAfterPost <> nil then DSR.Form.OnAfterPost(DSR.Form);
end;

procedure TDataSetProcessor.DataSetAfterScroll(DataSet: TDataSet);
var
  i: PtrInt;
  DSR: TDataSetRec;
begin
  i := DataSet.Tag;
  SetNeedRefresh(i);
  ClearCalcLabels(i, '');

  if FRecalculate or FDuplicateFlag or FPrinting then Exit;

  DSR := GetDataSet(i)^;

  if not DSR.Form.IsHide then RefreshAllData(i);
  RefreshLookupsWithParams('');

  UpdateControlState(i);

  //RunAction(DSR.Form.ActionOnAfterScroll, DataSet.Tag);
  if DSR.Form.OnAfterScroll <> nil then DSR.Form.OnAfterScroll(DSR.Form);
end;

procedure TDataSetProcessor.GridSelectEditor(Sender: TObject; Column: TColumn;
  var Editor: TWinControl);
var
  pLR: PLookupRec;
  DSR: TDataSetRec;
  C: TComponent;
  H: Integer;
  Grid: TdxGrid;
  F: TFont;
  i: PtrInt;
  R: TRect;
begin
  if (TDBGrid(Sender).ReadOnly) or (TDBGrid(Sender).Parent = nil) then
  begin
    Editor := nil;
    Exit;
  end;

  i := TComponent(Sender).Tag;
  DSR := GetDataSet(i)^;
  if not (DSR.DataSet.State in [dsInsert, dsEdit]) then
  begin
  	Editor := nil;
    Exit;
  end;

  Grid := TdxGrid(Sender);

  // Сбрасываем флаг "Только чтение".
  if Editor is TCustomEdit then TCustomEdit(Editor).ReadOnly := False
  else if Editor is TPickListCellEditor then TPickListCellEditor(Editor).ReadOnly := False;

  pLR := FindLookupByColumn(Column);

  C := FindById(DSR.Form, Column.Tag);
  if C is TdxLookupComboBox then
  begin
    Editor := pLR^.LCbx;
    R := Grid.CellRect(Grid.Col, Grid.Row);
    R.Top := R.Top + (R.Height div 2 - pLR^.LCbx.Height div 2);
    pLR^.LCbx.Left := R.Left;
    pLR^.LCbx.Top := R.Top;
    pLR^.LCbx.Width := R.Width - pLR^.LCbx.GetButtonWidths - ScaleToScreen(1);
  end
  else if C is TdxComboBox then
  begin
    if Editor is TPickListCellEditor then
    	with TPickListCellEditor(Editor) do
      begin
      	AutoComplete:=True;
        C := FindById(DSR.Form, Column.Tag);
        if C <> nil then Style := TCustomComboBox(C).Style;
        if (pLR <> nil) and (pLR^.NeedRefresh) then
        begin
          pLR^.NeedRefresh:=False;
          RefreshComboBox(pLR^);
          Items := Column.PickList;
        end;
      end;
  end
  else if (C is TdxEdit) and (TdxEdit(C).EditMask <> '') then
  begin
    Grid.MaskEdit.EditMask := TdxEdit(C).EditMask;
    Editor := Grid.MaskEdit;
  end
  else if (C is TdxEdit) or (C is TdxMemo) then
  begin
    H := 0;
    if Grid.HandleAllocated then
    begin
      F := Grid.Canvas.Font;
      Grid.Canvas.Font := Grid.Font;
      H := Grid.Canvas.TextHeight('Yy');
      Grid.Canvas.Font := F;
    end;
    if Grid.DefaultRowHeight > H + H then
    begin
      Editor := Grid.Memo;
    end;
  end
  else if C is TdxCalcEdit then
  begin
    Editor.Tag := i;
    Editor.OnKeyPress:=@FloatCellKeyPress
  end
  else if C is TdxCounter then
    TCustomEdit(Editor).ReadOnly := TdxCounter(C).ReadOnly
  else if (C is TdxObjectField) or (C is TdxFile) or (C is TdxRecordId) then
    TCustomEdit(Editor).ReadOnly := True
  else if Column.Field.IsBlob then Editor := nil;

  if (C <> nil) and (GetExpression(C) > '') and (not GetEditable(C)) then
  begin
    if Editor is TCustomEdit then
			TCustomEdit(Editor).ReadOnly := True
    else
	    Editor := nil;
  end;

  if Editor <> nil then
  begin
		Editor.Color := clWindow;
    Editor.Font := Grid.Font;
    Editor.Font.Color := clWindowText;
  end;
  if (Editor is TCustomEdit) and not (Editor is TdxLookupComboBox) then
  begin
    FCPMenu.Control := TCustomEdit(Editor);
    Editor.PopupMenu := FCPMenu;
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
  Fm: TdxForm;

begin
  MI := TMenuItem(Sender);
  i := TPopupMenu(MI.Owner).Tag;
  pLR := PLookupRec(FLookups[i]);
  Cbx := TdxLookupComboBox(pLR^.Control);
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
    6:
      begin
        pLR^.DSProc.OpenRecord(0);
        pLR^.DSProc.CallerObject := Cbx;
        if pLR^.DSProc.AppendObject = mrOk then
        begin
          DS := pLR^.DSProc.MasterSet;
          Fm := pLR^.DSProc.Form;
          Cbx.Field.Value := GetObjectFullValue(Fm, Cbx.SourceFId);
          Cbx.KeyValue := DS['id'];
          RefreshAllLookups(Cbx.SourceTId);
        end;
      end;
    7:
      begin
        if Cbx.KeyValue <> Null then
        begin
          if pLR^.DSProc.OpenRecord(Cbx.KeyValue) then
          begin
            if pLR^.DSProc.EditObject(False) = mrOk then
            begin
              DS := pLR^.DSProc.MasterSet;
              Fm := pLR^.DSProc.Form;
              Cbx.Field.Value := GetObjectFullValue(Fm, Cbx.SourceFId);
              Cbx.KeyValue := DS['id'];
              RefreshAllLookups(Cbx.SourceTId);
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

{procedure TDataSetProcessor.LookupMenuPopup(Sender: TObject);
var
  Pop: TPopupMenu;
  LR: TLookupRec;
  Cbx: TdxLookupComboBox;
  ae: Boolean;
begin
  Pop := TPopupMenu(Sender);
  LR := PLookupRec(FLookups[Pop.Tag])^;
  Cbx := TdxLookupComboBox(LR.Control);
  if Cbx.CanFocus then Cbx.SetFocus;
  ae := Cbx.DataSource.DataSet.State in [dsInsert, dsEdit];
  Pop.Items[0].Enabled:=(Cbx.Text<>'') and (not Cbx.ReadOnly) and ae;
  Pop.Items[1].Enabled := Cbx.Text <> '';
  Pop.Items[2].Enabled := (Clipboard.AsText <> '') and (not Cbx.ReadOnly) and ae;
  Pop.Items[4].Enabled := (Cbx.Text <> '') and (not Cbx.ReadOnly) and ae;
  Pop.Items[6].Enabled := ae;
  Pop.Items[7].Enabled := ae and (Cbx.KeyValue <> Null);
  Pop.Items[9].Enabled := Cbx.KeyValue <> Null;
end;  }

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
      ErrMsg(E.Message, True, 'MemoButtonClick ' + C.FieldName);
      Exit;
    end;
  end;
  Fm := pLR^.ListFm;
  Fm.DestForm := nil;
  Fm.DestMemo := C;
  Fm.Caption:=C.FieldName;
  try
    Fm.ShowForm;
  except
    on E: ESQLSelectStatementError do
    begin
      DataSets[pLR^.DSRi]^.Err.AddErrC(C, rsFilter, C.Filter, E);
      Exit;
    end;
  end;
  if C.CanFocus then
  begin
    C.SetFocus;
    C.SelStart:=Utf8Length(C.Text);
  end;
end;

{procedure TDataSetProcessor.PickListEditingDone(Sender: TObject);
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
      if (i < 0) or (MyUtf8CompareText(Text, Cbx.Text) <> 0) then Text := '';
      ItemIndex:=i;
      EditingDone;
    end;
end;  }

procedure TDataSetProcessor.QueryAfterScroll(DataSet: TDataSet);
var
  pQ: PQueryRec;
  i: Integer;
begin
  pQ := PQueryRec(FQueries[DataSet.Tag]);

  if FDuplicateFlag then Exit
  else if FPrinting or FRecalculate then
  begin
    SetNeedRefreshLinkedQueries(pQ);
    Exit;
  end;

  if not FIsRequeryAll then
  begin
    i := pQ^.DSRi;
    if SetNeedRefreshLinkedQueries(pQ) and not GetDataSet(i)^.Form.IsHide then
    begin
      RequeryQueries(i);
      CalcExprs(i);
    end;
  end;

  // Вызов переместил в RequeryQueries и RequeryQuery (09.07.2019), т. к.
  // меню ручных запросов не обновлялось
  //UpdateQueryPopupState(pQ^);

  with pQ^.Grid do
    if OnAfterScroll <> nil then OnAfterScroll(pQ^.Grid);
end;

procedure TDataSetProcessor.QueryGridButtonClick(Sender: TObject;
  Bn: TGridButtonType);
var
  Pop: TPopupMenu;
  G: TdxQueryGrid;
begin
  G := TdxQueryGrid(Sender);
  if G.CanFocus then G.SetFocus;
  Pop := PQueryRec(FQueries[G.Tag])^.Popup;
  case Bn of
    gbnAppend: Pop.Items[0].Click;
    gbnEdit: Pop.Items[1].Click;
    gbnDelete: Pop.Items[2].Click;
    gbnGoto: Pop.Items[4].Click;
    gbnRefresh: Pop.Items[6].Click;
  end;
end;

procedure TDataSetProcessor.QueryGridDblClick(Sender: TObject);
var
  i: PtrInt;
  Q: TQueryRec;
  G: TdxQueryGrid;
  P: types.TPoint;
begin
  G := TdxQueryGrid(Sender);
  if dgTitles in G.Options then
  begin
    P := G.ScreenToClient(Mouse.CursorPos);
    P := G.MouseToCell(P);
    if P.y = 0 then Exit;
  end;

  i := G.Tag;
  Q := PQueryRec(FQueries[i])^;
  if Q.DataSet.Active and Q.Simple then
    if (Q.DataSet.Fields[0].IsNull = False) and Q.Popup.Items[1].Visible then
    begin
      Q.Popup.Items[1].Click;
    end;
end;

procedure TDataSetProcessor.QueryGridSortChange(Sender: TObject);
var
  G: TdxQueryGrid;
  {i: Integer;
  RD: TReportData;
  CD: TSortColumn;
  Col: TRpGridColumn; }
begin
  G := TdxQueryGrid(Sender);
  {RD := Queries[G.Tag]^.RD;
  RD.Grid.SortCols.Clear;
  for i := 0 to G.SortCols.Count - 1 do
  begin
    CD := G.SortCols[i];
    Col := RD.Grid.FindColumnByFieldName(TColumn(CD.Col).FieldName);
    RD.Grid.SortCols.AddCol(Col, CD.Desc);
  end;  }
  G.SortColsToRpGridSortCols;
  RequeryQuery(G.Tag, 0, True);
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
  NewId: Variant;

  procedure RefreshQueryAfterAppend(NewRec: Integer);
  begin
    RequeryQuery(Qi, NewRec, True);
    //DS.Locate('id', NewRec, []);
    //UpdateQueryPopupState(pQ^);
  end;

  procedure RefreshQueryAfterEdit;
  var
    Key: Variant;
  begin
    Key := DS.Fields[0].Value;
    RequeryQuery(Qi, Key, True);
		//DS.Locate('id', Key, []);
    //UpdateQueryPopupState(pQ^);
  end;

  procedure RefreshQuery;
  begin
    RequeryQuery(Qi, 0, True);
    //UpdateQueryPopupState(pQ^);
  end;

begin
  MI := TMenuItem(Sender);
  Qi := TPopupMenu(MI.Owner).Tag;
  pQ := PQueryRec(FQueries[Qi]);
  i := pQ^.DSRi;
  DSR := GetDataSet(i)^;
  if MI.Tag in [0..2] then
  begin
    if pQ^.DSProc = nil then
    begin
      RD := pQ^.RD;
      pQ^.DSProc := TDataSetProcessor.Create;
      pQ^.DSProc.BindForm(RD.GetEditFormId, False, vtGridOnly);
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
        mr := mrYes;
        if (i = 0) and (MasterSet.State = dsInsert) and
          (FFm.ViewType <> vtSimpleForm) then
        begin
          mr := MessageDlg(rsWarning, rsSaveRecMsg, mtConfirmation, [mbYes, mbNo], 0);
          if (mr = mrYes) and Validate(i) then
          begin
            MasterSet.Post;
            InnerEdit(i, True, True, False);
          end
          else mr := mrNo;
        end;
        if mr = mrYes then
        begin
          DSP.OpenRecord(0);
          try
            NewId := DSP.AppendRecord(FFm, DSR.Form, DSR.DataSet, pQ^.Grid.Id);
            if NewId <> Null then
	            RefreshQueryAfterAppend(NewId);
          // По идее не должно быть этого исключения, т. к. ESourceFilterError
          // сработает при открытии запроса, а в закрытом запросе команда "Добавить"
          // недоступна.
          except
            on E: EFilterParserError do
              DSR.Err.AddErr(TdxForm(pQ^.Grid.Owner).FormCaption, pQ^.RD.Name, rsQuery, rsSourceFilter, pQ^.RD.GetSourceFilter, E);
          end;
        end;
      end;
    1:
      begin
        if DS.Fields[0].IsNull = False then
        begin
          if DSP.OpenRecord(DS.Fields[0].AsInteger) then
          begin
            if DSP.EditObject(not (DSR.DataSet.State in [dsInsert, dsEdit])) = mrOk then
	            RefreshQueryAfterEdit;
          end;
        end;
      end;
    2:
      begin
        if DS.Fields[0].IsNull = False then
        begin
          rslt := DSP.OpenRecord(DS.Fields[0].AsInteger);
          if rslt and DSP.Delete then
          	RefreshQuery;
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
    RD := aQ.RD;
    MainFr.GotoRec(RD.GetEditFormId, aId);
  end;
end;

procedure TDataSetProcessor.UpdateQueryPopupStates(DSRi: Integer);
var
  i: Integer;
  Q: TQueryRec;
begin
  for i := 0 to FQueries.Count - 1 do
  begin
    Q := PQueryRec(FQueries[i])^;
    if Q.DSRi = DSRi then UpdateQueryPopupState(Q);
  end;
end;

procedure TDataSetProcessor.UpdateQueryPopupState(Q: TQueryRec);
var
  Pop: TPopupMenu;
  bCanEdit, bDSEdit, bActive, bHasRec: Boolean;
  Bns: TGridButtons;
  DSR: TDataSetRec;
begin
  Pop := Q.Popup;
  if Q.Simple then
  begin
    DSR := GetDataSet(Q.DSRi)^;
    bDSEdit := DSR.DataSet.State in [dsInsert, dsEdit];
    bCanEdit := UserMan.CheckFmEditing(Q.RD.GetEditFormId) and bDSEdit;
    bActive := Q.DataSet.Active;
    bHasRec := bActive and (Q.DataSet.RecordCount > 0);
    Pop.Items[0].Enabled := bActive and bDSEdit and Pop.Items[0].Visible;
    Pop.Items[1].Enabled:= bHasRec and Pop.Items[1].Visible;
    Pop.Items[2].Enabled := bHasRec and bDSEdit and Pop.Items[2].Visible;
    Pop.Items[4].Enabled := bHasRec and Pop.Items[4].Visible;

    if bCanEdit then
    begin
      Pop.Items[1].ImageIndex := IMG16_EDIT;
      Pop.Items[1].Caption := rsEdit;
    end
    else
    begin
      Pop.Items[1].ImageIndex := IMG16_EYES;
      Pop.Items[1].Caption := rsLook;
    end;

    Bns := Q.Grid.Buttons;
    Bns.EnableButton(gbnAppend, Pop.Items[0].Enabled);
    Bns.EnableButton(gbnEdit, Pop.Items[1].Enabled);
    if Bns.Buttons[gbnEdit].ShowCaption then
	    Bns.Buttons[gbnEdit].Caption:=Pop.Items[1].Caption
    else
	    Bns.Buttons[gbnEdit].Hint:=Pop.Items[1].Caption;
    //Bns.Buttons[gbnEdit].Glyph.Assign(Pop.Items[1].Bitmap);
    Bns.Buttons[gbnEdit].ImageIndex := Pop.Items[1].ImageIndex;
    Bns.EnableButton(gbnDelete, Pop.Items[2].Enabled);
    Bns.EnableButton(gbnGoto, Pop.Items[4].Enabled);
  end;
  if Q.Grid.OnStateChange <> nil then Q.Grid.OnStateChange(Q.Grid);
end;

procedure TDataSetProcessor.ExchangeRows(DSRi: Integer; MoveUp: Boolean);
var
  Tmp, Tmp2: LongInt;
  US1, US2: TUpdateStatus;
  Ok: Boolean;
  DS: TSQLQuery;
  pDSR: PDataSetRec;
begin
  pDSR := GetDataSet(DSRi);
  DS := pDSR^.DataSet;
  if MoveUp and DS.BOF then Exit
  else if DS.EOF then Exit;

  LockDSScroll(DS);
  US1 := DS.UpdateStatus;
  Tmp := DS.Fields[0].AsInteger;
  if MoveUp then DS.Prior
  else DS.Next;
  US2 := DS.UpdateStatus;
  if ((US1 = usInserted) or (US2 = usInserted)) and (US1 <> US2) then
  begin
    if MoveUp then DS.Next
    else DS.Prior;
    UnlockDSScroll(DS);
    Ok := False;
    if MessageDlg(rsWarning, rsExchgOldNewRow, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Ok := Validate(0);
      if Ok then
      begin
        MasterSet.Post;
        Ok := InnerEdit(0, True, True, False) = asOk;
      end;
    end;
    if Ok then ExchangeRows(DSRi, MoveUp);
    Exit;
  end;
  UnlockDSScroll(DS);

  Tmp2 := DS.Fields[0].AsInteger;
  DS.Edit;
  DS.Fields[0].AsInteger := Tmp;
  UpdateRecIdFields(pDSR);
  DS.Post;

  if MoveUp then DS.Next
  else DS.Prior;

  DS.Edit;
  DS.Fields[0].AsInteger := Tmp2;
  UpdateRecIdFields(pDSR);
  DS.Post;
  FMaster^.RowsExchanged:=True;
  pDSR^.RowsExchanged:=True;
end;

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
  //DSR.Grid.EditorMode:=False;  // избегаем ошибки EDatabaseError (not Edit, Insert state)
  if i = 0 then
  begin
    if (MI.Tag in [0, 4, 8, 9]) and (not Validate(i)) then Exit;
    case MI.Tag of
      0: Append;
      1: Edit;
      2: Delete;
      4: ApplyQuickFilter(True);
      8: DuplicateRecord(i, False);
      9: DuplicateRecord(i, True);
      10: ApplyQuickFilter(False);
      11: ClearAllFilters;
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
      InnerEdit(i, True, True, False);
    	if not IsTableInput(@DSR) then
	     	ShowEditForm(i);
    end
    else if MI.Tag = 2 then
    begin
      if ConfirmDelete then InnerDelete(i, True, True, False);
    end
    else if (MI.Tag = 6) and (MI.Visible) then
    begin
      SD := DSR.Form.ShopData;
      pLR := FindLookupById(SD.ObjId);
      PrepareListForm(pLR);
      Fm := pLR^.ListFm;
      Fm.DestForm := DSR.Form;
      Fm.DestDS := DSR.DataSet;
      Fm.ValueField := GetSourceFId(pLR^.Control);
      Fm.DestMemo := nil;
      Fm.DSProc := Self;
      Fm.DSRi := i;
      Fm.Caption := '';
      Fm.ShowForm;
    end
    else if MI.Tag = 8 then DuplicateRecord(i, False)
    else if MI.Tag = 10 then ExchangeRows(i, True)
    else if MI.Tag = 11 then ExchangeRows(i, False)
  end;
  UpdateControlState(i);
end;

procedure TDataSetProcessor.SetNeedRefresh(DSRi: Integer; QueriesOnly: Boolean);
var
  j: Integer;
  pQ: PQueryRec;
begin
  if not QueriesOnly then
  begin
    if DSRi = 0 then
      for j := 1 to DataSetCount - 1 do
        DataSets[j]^.NeedRefresh := True;
  end;

  for j := 0 to QueryCount - 1 do
  begin
    pQ := Queries[j];
    if (pQ^.DSRi = DSRi) {and not pQ^.Grid.ManualRefresh} then
    begin
      Queries[j]^.NeedRefresh := True;
      SetNeedBuildPivot(j);
    end;
  end;
end;

procedure TDataSetProcessor.SetNeedRefreshQueriesWithParams(DSRi: Integer;
  const FieldName: String);
var
  i: Integer;
  Q: TQueryRec;
  IsParent: Boolean;
  pQ: PQueryRec;
begin
  IsParent := Copy(FieldName, 1, 1) = '!';
  for i := 0 to FQueries.Count - 1 do
  begin
    pQ := PQueryRec(FQueries[i]);
    Q := pQ^;
    if (DSRi <> Q.DSRi) {or (Q.Grid.ManualRefresh)} then Continue;
    if Q.RD.FieldExistsInExpr(DSRi, FieldName) then
    begin
      pQ^.NeedRefresh := True;
      if not FRecalculate then
      begin
        if not IsParent then
          pQ^.Changed := True
        else
          pQ^.ParentChanged := True;
      end;
      SetNeedBuildPivot(i);
      SetNeedRefreshLinkedQueries(pQ);
      ClearAggCalcLabels(DSRi, Q.RD.Name);
    end;
  end;

  if DSRi = 0 then
  begin
    for i := 1 to DataSetCount - 1 do
      SetNeedRefreshQueriesWithParams(i, '!' + FieldName);
  end;
end;

function TDataSetProcessor.SetNeedRefreshLinkedQueries(CurQ: PQueryRec
  ): Boolean;
var
  i: Integer;
  Q: TQueryRec;
  QueryName: String;
  pQ: PQueryRec;
begin
  Result := False;
  QueryName := CurQ^.RD.Name;
  for i := 0 to FQueries.Count - 1 do
  begin
    pQ := PQueryRec(FQueries[i]);
    Q := pQ^;
    if (pQ = CurQ) or (Q.DSRi <> CurQ^.DSRi) {or Q.Grid.ManualRefresh} then Continue;

    if Q.RD.QueryExistsInExpr(QueryName) then
    begin
      PQueryRec(FQueries[i])^.NeedRefresh:=True;
      SetNeedBuildPivot(i);
      ClearAggCalcLabels(Q.DSRi, Q.RD.Name);
      SetNeedRefreshLinkedQueries(pQ);
      Result := True;
    end;
  end;
end;

procedure TDataSetProcessor.SetNeedBuildPivot(QRi: Integer);
var
  i: Integer;
  C: TComponent;
  Q: TQueryRec;
  Fm: TdxForm;
begin
  Q := PQueryRec(Queries[QRi])^;
  Fm := GetDataSet(Q.DSRi)^.Form;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxPivotGrid then
    begin
      with TdxPivotGrid(C) do
        if Id = Q.Grid.Id then NeedBuild := True;
    end
    else if C is TdxChart then
      with TdxChart(C) do
        if Query = Q.Grid.Id then NeedBuild := True;
  end;
end;

function TDataSetProcessor.RunAction(const ActionData: String; ADSRi: Integer
  ): Variant;
begin
  Result := Null;
  if ActionData = '' then Exit;

  with TActionRunner.Create do
  try
  	DSProc := Self;
    DSRi := ADSRi;
    Load(ActionData);
    Result := Run;
  finally
    Free;
  end;
end;

procedure TDataSetProcessor.RefreshAllData(DSRi: Integer);
var
  DSR: TDataSetRec;
begin
  DSR := DataSets[DSRi]^;
  if not FDuplicateFlag then
  begin
    if DSRi = 0 then RequeryDetails
    // Надо обновить таблицу, если окно редактирования родительской формы не открыто
    else if DSR.NeedRefresh then RequeryDetail(DSRi);
  end;

  FIsRequeryAll:=True;
  RequeryQueries(DSRi);

  CalcExprs(DSRi);
  FIsRequeryAll:=False;
  if not DSR.Form.IsHide then
    ShowImages(DSRi);
end;

procedure TDataSetProcessor.HideNotif;
begin
  FNotif.Hide;
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
    if not HasFId(C) then Continue;
    Bn := GetEditButton(C);
    if (Bn <> nil) and Bn.Enabled then L.Add(C);
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
    pDS^.Images.Free;
    Dispose(pDS);
    FItems.Delete(0);
  end;
end;

procedure SelectFirstVisiblePage(Pages: TPageControl);
var
  i: Integer;
  T: TTabSheet;
begin
  for i := 0 to Pages.PageCount - 1 do
  begin
    T := Pages.Pages[i];
    if T.TabVisible then
    begin
      Pages.ActivePage := T;
      Exit;
    end;
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
          with TdxTabSheet(C) do
          begin
            TabVisible := False;
            TdxPageControl(PageControl).SetActiveFirstVisiblePage;
        	end
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

    if GetHidden(C) then
    begin
      TControl(C).Visible := False;
      if C is TdxTabSheet then
        with TdxTabSheet(C) do
        begin
          TabVisible := False;
          if PageControl.ActivePage = C then
            SelectFirstVisiblePage(PageControl);
        end;
    end;

    if C is TdxButton then
    	with TdxButton(C) do
	    begin
  	    OnClick:=@ButtonClick;
    	end;
    if not HasFId(C) then Continue;

    SetDataSource(C, DS);
    if C is TdxLookupComboBox then
    	with TdxLookupComboBox(C) do
      begin
        DataField := FieldStr(C) + 'l';
        KeyField := FieldStr(C);
      end
    else if not (C is TdxFile) then SetDataField(C, FieldStr(C))
    // Это TdxFile
    else SetDataField(C, FieldStr(C) + 'd');

    if HasExpression(C) and (Trim(GetExpression(C)) <> '') and (not GetEditable(C)) then
    begin
      SetReadOnly(C, True);
      if C is TdxComboBox then TdxComboBox(C).Style:=csSimple;
    end;

    if (Fm.PId = 0) and (C is TdxLookupComboBox) and (FGotoEnable) and (FFm.ViewType <> vtGridOnly) then
      TdxLookupComboBox(C).OnCtrlClick:=@LookupCtrlClick;

    if C is TdxObjectField then
    begin
      TdxObjectField(C).ReadOnly:=True;
    end
    else if C is TdxDBImage then
    begin
      Col := FindGridColumn(DSR.Grid, GetId(C));
      with TMyDBGridColumn(Col) do
      begin
        IsImage := True;
        ThumbSize := TdxDBImage(C).ThumbSize;
      end;
    end;

    // !!! Доступ
    if not UserMan.CheckControlEditing(Fm.Id, C.Name) then
    begin
      if C is TdxDBImage then TdxDBImage(C).ReadOnly := True
      else if C is TdxFile then TdxFile(C).CanEdit := False
      else if C is TdxComboBox then
        with TdxComboBox(C) do
        begin
          ReadOnly := True;
          Style := csSimple;
        end
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
  S, FNm: String;
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
    begin
      S := GetExpression(C);
      if Trim(S) > '' then
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
          {if C is TdxLabel then FNm := TdxLabel(C).FieldName
          else FNm := GetFieldName(C);}
          DSR^.Err.AddErrC(C, rsExpression, S, Ex);
        end;
      end;
    end;
  end;
  // !!! Доступ. Условие на редактирование/удаление
  DSR^.EditCond := nil;
  DSR^.DelCond := nil;
  if DSR^.Editing then
    try
      S := UserMan.GetEditCond(DSR^.Form.Id);
      if Trim(S) <> '' then DSR^.EditCond := EB.Build(S);
    except
      on Ex: Exception do
        DSR^.Err.AddErr('', DSR^.Form.FormCaption, rsForm, rsEditCond, S, Ex);
    end;

  if DSR^.Deleting and DSR^.Editing then
    try
      S := UserMan.GetDelCond(DSR^.Form.Id);
      if Trim(S) <> '' then DSR^.DelCond := EB.Build(S);
    except
      on Ex: Exception do
        DSR^.Err.AddErr('', DSR^.Form.FormCaption, rsForm, rsDelCond, S, Ex);
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
    if HasDefaultValue(C) then
    begin
      S := GetDefaultValue(C);
      if Trim(S) > '' then
      try
        E := EB.Build(S);
        if E <> nil then
          L.AddExpr(C, E);
      except
        on Ex: Exception do
          DSR^.Err.AddErrC(C, rsDefaultValue, S, Ex);
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
  List: TList;
begin
  L:=TExprList.Create;
  DSR^.ChkExprList := L;
  Fm := DSR^.Form;
  EB := TExpressionBuilder.Create;
  EB.Form := Fm;
  EB.ParentForm := GetDataSet(0)^.Form;
  EB.DataSet := DSR^.DataSet;
  EB.SkipLabels:=True;

  List := TList.Create;
  GetTabOrderComponents(Fm, List);
  for i := 0 to List.Count - 1 do
  begin
    C := TComponent(List[i]);
    if HasCheckExpression(C) and UserMan.CanInput(C) then
    begin
      S := GetCheckExpression(C);
      if Trim(S) > '' then
      try
        E := EB.Build(S);
        if E <> nil then
          L.AddExpr(C, E);
      except
        on Ex: Exception do
          DSR^.Err.AddErrC(C, rsCheckValue, S, Ex);
      end;
    end;
  end;
  EB.Free;
  List.Free;
end;

procedure TDataSetProcessor.AddDataSet(aGrid: TdxGrid);
var
  DataSrc: TDataSource;
  DataSet: TSQLQuery;
  Fm: TdxForm;
  pDS: PDataSetRec;
  i: Integer;
  Pop: TPopupMenu;
  C: TComponent;
begin
  DataSet := TdxDataSet.Create(nil);
  DataSet.PacketRecords:=100;
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
  DataSrc.AutoEdit := False;
  if aGrid.Id = 0 then
    Fm := FFm
  else
    Fm := FormMan.LoadForm(aGrid.Id);
  Fm.DataSet := DataSet;
  TdxDataSet(DataSet).Form := Fm;
  DataSet.InsertSQL.Text := SQLInsertStatement(Fm);
  //DataSet.UpdateSQL.Text := SQLUpdateStatement(Fm); // Динамически генерируем запрос из измененных полей
  DataSet.DeleteSQL.Text := SQLDeleteStatement(Fm);
  aGrid.DataSource := DataSrc;
  aGrid.Form := Fm;
  aGrid.OnSelectEditor:=@GridSelectEditor;
  aGrid.OnDblClick:=@GridDblClick;
  aGrid.OnCanSort:=@GridCanSort;
  aGrid.OnSortColumnChange:=@GridSortColumnChange;
  aGrid.OnDrawColumnCell:=@GridDrawColumnCell;
  aGrid.OnPrepareCanvas:=@GridPrepareCanvas;
  aGrid.OnCellClick:=@GridCellClick;
  aGrid.OnVaidate:=@GridValidate;
  aGrid.OnKeyDown:=@GridKeyDown;
  Pop := CreatePopupMenu(aGrid.Id = 0, True);
  aGrid.PopupMenu := Pop;
  aGrid.Options:= aGrid.Options - [dgTabs];

  New(pDS);
  i := FItems.Add(pDS);
  pDS^.DataSource := DataSrc;
  pDS^.DataSet := DataSet;
  pDS^.HasFields := aGrid.Columns.Count > 0; //IsFieldExist(Fm);
  // Если нет полей, то добавляем невидимое поле id, чтобы в компоненте
  // не отображались служебные поля
  if not pDS^.HasFields then
  	with aGrid.Columns.Add do
    begin
      FieldName := 'id';
      Visible := False;
    end;
  pDS^.Form := Fm;
  pDS^.Grid := aGrid;
  pDS^.Popup := Pop;
  pDS^.FilterFm := nil; //TFilterFm.Create(nil);
  //pDS^.FilterFm.Form := Fm;
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
  //pDS^.TblFilterSet := False;
  pDS^.Colors := TColorList.Create;
  pDS^.Images := TList.Create;

  Fm.Tag := i;
  DataSet.Tag:=i;
  aGrid.Tag:=i;
  aGrid.PopupMenu.Tag := i;
  if (Fm.PId > 0) and (aGrid.ShowButtons) then
  begin
    aGrid.OnButtonClick:=@GridButtonClick;
  end;
  Fm.Tree.IsWine := AppConfig.IsWine;

  BindControls(pDS^);
  FindImages(pDS);
  ClearChartSources(Fm);

  // !!! Доступ
  if UserMan.CheckFmVisible(Fm.Id) = False then MaskingControl(Fm, aGrid);
  pDS^.Adding:=UserMan.CheckFmAdding(Fm.Id);
  pDS^.Editing:=UserMan.CheckFmEditing(Fm.Id);
  pDS^.Deleting:=UserMan.CheckFmDeleting(Fm.Id);
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

  pDS^.MaxId := 0;
  pDS^.NeedRefresh:=True;
  pDS^.NewRecord := False;
  pDS^.RowsExchanged := False;
  pDS^.DetailsChanged := False;
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

function TDataSetProcessor.CreatePopupMenu(IsMaster, AllowSpace: Boolean): TPopupMenu;
var
  scSpace: Integer;
begin
  scSpace := 0;
  if AllowSpace then scSpace := ShortCut(VK_SPACE, []);
  Result := TPopupMenu.Create(nil);
  Result.Images := Images16;
  Result.Items.Add( CreateMenuItem(Result, rsAppend, 0, ShortCut(VK_INSERT, []),
    @MenuItemClick, IMG16_ADD) );
  Result.Items.Add( CreateMenuItem(Result, rsEdit, 1, scSpace, @MenuItemClick, IMG16_EDIT) );
  Result.Items.Add( CreateMenuItem(Result, rsDelete, 2, ShortCut(VK_DELETE, [ssCtrl]),
    @MenuItemClick, IMG16_DELETE) );
  if IsMaster then
  begin
    Result.Items.Add( CreateMenuItem(Result, '-', 7, 0, nil) );
    Result.Items.Add( CreateMenuItem(Result, rsDuplicate, 8, ShortCut(VK_D, [ssCtrl]),
      @MenuItemClick, IMG16_COPY) );
    Result.Items.Add( CreateMenuItem(Result, rsDuplicateAll, 9, ShortCut(VK_D, [ssCtrl, ssShift]),
      @MenuItemClick) );
    Result.Items.Add( CreateMenuItem(Result, '-', 3, 0, nil) );
    Result.Items.Add( CreateMenuItem(Result, rsFilter, 4, 0, @MenuItemClick, IMG16_FILTER) );
    Result.Items.Add( CreateMenuItem(Result, rsAddToFilter, 10, 0, @MenuItemClick) );
    Result.Items.Add( CreateMenuItem(Result, rsClearFilter, 11, 0, @MenuItemClick) );
  end
  else
  begin
    Result.Items.Add( CreateMenuItem(Result, '-', 7, 0, nil) );
    Result.Items.Add( CreateMenuItem(Result, rsDuplicate, 8, ShortCut(VK_D, [ssCtrl]),
      @MenuItemClick, IMG16_COPY) );
    Result.Items.Add( CreateMenuItem(Result, '-', 5, 0, nil) );
    Result.Items.Add( CreateMenuItem(Result, rsShopping, 6, 0, @MenuItemClick, IMG16_SHOPPING) );
    Result.Items.Add( CreateMenuItem(Result, '-', 9, 0, nil) );
    Result.Items.Add( CreateMenuItem(Result, rsMoveUp, 10, 0, @MenuItemClick, IMG16_UP) );
    Result.Items.Add( CreateMenuItem(Result, rsMoveDown, 11, 0, @MenuItemClick, IMG16_DOWN) );
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
    NeedRefresh := False;

    DataSet.Close;
    TNm := TableStr(Form.Id);
    id := FMaster^.DataSet.FieldByName('id').AsString;
    if id > '' then
      Cond := TNm + '.pid=' + id
    else
      Cond := '1=0';
    S := SqlSelectStatement(Form, nil, False, False, nil, Cond, -1, False);
    if Grid.SortCols.Count > 0 then
      S := S + ' order by ' + SortColumnsToSQL(Grid);
    DataSet.SQL.Text := S;
    if Grid.SortCols.Count = 0 then
      DataSet.IndexFieldNames:='id'
    else
      DataSet.IndexFieldNames:='';

    DataSet.Open;
    if DataSet.RecordCount = 0 then
      // Может быть ScrollEventsDisabled!
      if DataSet.AfterScroll <> nil then DataSet.AfterScroll(DataSet);
  end;
end;

procedure TDataSetProcessor.RequeryDetails;
var
  i: Integer;
begin
  for i := 1 to FItems.Count - 1 do
    if GetDataSet(i)^.NeedRefresh then
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
  pLR: PLookupRec;
  i: Integer;
  Pop: TPopupMenu;
  DSP: TDataSetProcessor;
  LCbx: TdxLookupComboBox;
begin
  Pop := nil;
  DSP := nil;
  LCbx := nil;
  if C is TdxLookupComboBox then
  begin
    with TdxLookupComboBox(C) do
    begin
      OnButtonClick := @LCbxButtonClick;
      OnNeedData := @LCbxFilterData;
      OnKeyMatch := @LCbxKeyMatch;

      // !!! Доступ
      if UserMan.CheckFmVisible(SourceTId) = False then Button.Enabled:=False;
      //

      Pop := PopupMenu;
      // !!! Доступ

      Pop.Items[5].Visible := UserMan.CheckFmVisible(SourceTId);
      Pop.Items[6].Visible := UserMan.CheckFmAdding(SourceTId);
      Pop.Items[7].Visible := Pop.Items[5].Visible;
      if UserMan.CheckFmEditing(SourceTId) = False then
      begin
        Pop.Items[7].ImageIndex := IMG16_EYES;
        Pop.Items[7].Caption := rsLook;
      end;
      //
      Pop.Items[8].Visible:=(DSRi = 0) and FGotoEnable and (FFm.ViewType <> vtGridOnly) and (Pop.Items[5].Visible);
      Pop.Items[9].Visible:=Pop.Items[8].Visible;
      // Вне зависимости от правил доступа на видимость пунктов влияет видимость кнопок.
      if HideList then
      begin
        Pop.Items[0].Visible := False;
        Pop.Items[2].Visible := False;
      end;
      if HideButton and HideList then
      begin
        Pop.Items[3].Visible := False;
        Pop.Items[4].Visible := False;
        Pop.Items[6].Visible := False;
      end;
      //
      OnMenuClick := @LookupMenuClick;

      LCbx := TdxLookupComboBox.Create(nil);
      LCbx.Id := Id;
      LCbx.FieldName := FieldName;
      LCbx.SourceTId := SourceTId;
      LCbx.SourceFId := SourceFId;
      LCbx.Filter := Filter;
      LCbx.KeyField := KeyField;
      LCbx.ListFields.Assign(ListFields);
      LCbx.DropDownCount := DropDownCount;
      LCbx.ListWidthExtra := ListWidthExtra;
      LCbx.HideButton := HideButton;
      LCbx.HideList := HideList;
      LCbx.ReadOnly := ReadOnly;
      LCbx.DataSource := DataSource;
      LCbx.DataField := DataField;
      LCbx.Button.Transparent := False;
      LCbx.Button.Color := aGrid.Color;
      LCbx.DropDownButton.Transparent := False;
      LCbx.DropDownButton.Color := aGrid.Color;
      LCbx.DataSource := DataSource;
      LCbx.KeyField := KeyField;
      LCbx.ListSource := ListSource;
      LCbx.ListKeyField := ListKeyField;
			LCbx.OnNeedData := OnNeedData;
      LCbx.OnKeyMatch := OnKeyMatch;
      LCbx.OnButtonClick := OnButtonClick;
      LCbx.OnKeyDown:=@LCbxKeyDown;
      LCbx.PopupMenu.Items[5].Visible := Pop.Items[5].Visible;
      LCbx.PopupMenu.Items[6].Visible := Pop.Items[6].Visible;
      LCbx.PopupMenu.Items[7].Visible := Pop.Items[7].Visible;
      if UserMan.CheckFmEditing(SourceTId) = False then
      begin
        LCbx.PopupMenu.Items[7].ImageIndex := IMG16_EYES;
        LCbx.PopupMenu.Items[7].Caption := rsLook;
      end;
      LCbx.PopupMenu.Items[8].Visible := Pop.Items[8].Visible;
      LCbx.PopupMenu.Items[9].Visible := Pop.Items[9].Visible;
      LCbx.OnMenuClick:=@LookupMenuClick;

      // Форма скрыта
      if not Pop.Items[5].Visible then
        LCbx.HideButton := True;
    end;

  end
  else if C is TdxComboBox then
    with TdxComboBox(C) do
    begin
      OnNeedData:=@ComboBoxNeedData;
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
  pLR^.TId:=GetSourceTId(C);
  pLR^.Column := FindGridColumn(aGrid, GetId(C));
  pLR^.ListFm := nil;
  pLR^.NeedRefresh := True;
  pLR^.DSRi:=DSRi;
  pLR^.DSProc:=DSP;
  pLR^.LCbx := LCbx;
  i := FLookups.Add(pLR);
  C.Tag := i;
  if Pop <> nil then Pop.Tag := i;
  if LCbx <> nil then
  begin
    LCbx.Tag := i;
    LCbx.PopupMenu.Tag := i;
  end;
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
    if (C is TdxComboBox) or (C is TdxMemo) or (C is TdxLookupComboBox) then
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
    //FreeAndNil(pLR^.Popup);
    FreeAndNil(pLR^.DSProc);
    {pLR^.DataSet.Free;
    pLR^.DataSource.Free; }
    FreeAndNil(pLR^.LCbx);
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
var
  i: Integer;
begin
  FItems := TList.Create;
  FLookups := TList.Create;
  FQueries := TList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval:=2000;
  FTimer.OnTimer:=@TimerTimer;
  FNotif := TPopupNotifier.Create(nil);
  FNotif.Title:=rsWarning;
  {$ifdef linux}
  // В линукс надпись в подсказке может иметь цвет фона, поэтому меняем
  // цвет надписи на системный.
  with FNotif.vNotifierForm do
    for i := 0 to ControlCount - 1 do
    begin
      if Controls[i] is TLabel then
        TLabel(Controls[i]).Font.Color:=clInfoText;
    end;
  {$endif}
  FCPMenu := TCopyPasteMenu.Create(nil);
end;

destructor TDataSetProcessor.Destroy;
begin
  UnBind;
  FCPMenu.Free;
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
  //if not FFm.Grid.ReadOnly then
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
      SD := ScriptMan.FindScript(Form.Id, skForm);
      if SD <> nil then
      begin
        if DXMain.AllowDynamicForms and (SD.Bin = '') then
        begin
          SD.Form := Form;
          ScriptMan.CompileModule(SD);
          if ScriptMan.HasErrorsInModule(SD) then
            ShowCompilerErrors(SD);
        end;
        RunScript.SD := SD;
        RunScript.LoadBin;
        RunScript.BindForm(Form);
        RunScript.TryRunProc('Form_Create', []);
      end;
      if MainFm.OnCreateForm <> nil then
        MainFm.OnCreateForm(MainFm, Form);
      RunAction(Form.ActionOnCreate, i);
    end;
end;

procedure TDataSetProcessor.BindDummyForm;
var
  pDS: PDataSetRec;
begin
  FFm := TdxForm.Create(nil);
  FFm.Id := DummyForm;
  New(pDS);
  pDS^.Form := FFm;
  FItems.Add(pDS);
end;

procedure TDataSetProcessor.ClearDummyForm;
var
  pDS: PDataSetRec;
begin
  pDS := GetDataSet(0);
  pDS^.Form.Free;
  Dispose(pDS);
  FItems.Clear;
end;

// Специально для печати отчетов. Создается фиктивная форма с набором
// компонентов.
procedure TDataSetProcessor.BindReport(RD: TReportData; RDS: TSQLQuery);
var
  SQL: String;
  pDS: PDataSetRec;
  pQ: PQueryRec;
  DS: TSQLQuery;
  DSrc, QSrc: TDataSource;
  i: Integer;
  C: TComponent;
  QG: TdxQueryGrid;
begin
  FFm := CreateReportForm(RD, SQL);
  if SQL <> '' then SQL := ',' + SQL;

  DS := TdxDataSet.Create(nil);
  DBase.AttachDataSet(DS);
  DSrc := TDataSource.Create(nil);
  DSrc.DataSet := DS;
  DS.SQL.Text := Format('select 0 as id %s from rdb$database', [SQL]);

  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if {IsField(C)} HasFId(C) then SetDataSource(C, DSrc);
  end;

  New(pDS);
  pDS^.Form := FFm;
  pDS^.Grid := FFm.Grid;
  pDS^.DataSet := DS;
  pDS^.DataSource := DSrc;
  pDS^.ExprList := TExprList.Create;
  pDS^.LblExprList := TExprList.Create;
  pDS^.EditFm := TEditWindow.CreateNew(nil);
  FItems.Add(pDS);
  FFm.DSP := Self;
  FFm.DSR := pDS;
  FFm.DataSet := DS;

  QG := TdxQueryGrid(FFm.FindComponent('QGrid'));
  QG.DSP := Self;
  QSrc := TDataSource.Create(nil);
  QSrc.DataSet := RDS;
  QG.DataSource := QSrc;

  New(pQ);
  pQ^.DSRi:=0;
  pQ^.RD := RD;
  pQ^.DataSource := QSrc;
  pQ^.DataSet := RDS;
  pQ^.DSProc := Self;
  pQ^.Grid := QG;
  pQ^.Colors := TQueryColorList.Create;
  pQ^.NeedRefresh := False;
  FQueries.Add(pQ);

  FMaster := pDS;
end;

procedure TDataSetProcessor.UnBindReport;
var
  pQ: PQueryRec;
  pD: PDataSetRec;
begin
  pQ := PQueryRec(FQueries[0]);
  pQ^.Colors.Free;
  pQ^.DataSource.Free;
  Dispose(pQ);
  FQueries.Clear;
  pD := PDataSetRec(FItems[0]);
  pD^.EditFm.Free;
  pD^.LblExprList.Free;
  pD^.ExprList.Free;
  pD^.DataSource.Free;
  pD^.DataSet.Free;
  Dispose(pD);
  FItems.Clear;
  FFm.Free;
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
      if MainFm.OnDestroyForm <> nil then
        MainFm.OnDestroyForm(MainFm, Form);
      if Form.OnDestroy <> nil then
        Form.OnDestroy(Form);
      if RunScript.SD <> nil then
        RunScript.TryRunProc('Form_Destroy', []);
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

function GetNodePath(N: TTreeNode): String;
begin
  Result := '';
  while N.Parent <> nil do
  begin
    Result := N.Text + '\' + Result;
    N := N.Parent;
  end;
  Result := Copy(Result, 1, Length(Result) - 1);
end;

procedure TDataSetProcessor.InsertTreeValues;
var
  N: TTreeNode;
  TF: TdxFormTreeField;
  DS: TSQLQuery;
  FNm: String;
  ND: TdxTreeNodeData;
  C: TComponent;
begin
  N := FFm.Tree.Selected;
  if (FFm.Tree.SelectionCount > 1) or (N = nil) or (N.Parent = nil) then Exit;

  DS := MasterSet;

  while N.Parent <> nil do
  begin
    ND := TdxTreeNodeData(N.Data);
	  TF := ND.Field;
  	C := FindById(FFm, TF.FieldId);
    FNm := FieldStr(TF.FieldId);
    if C is TdxLookupComboBox then
    begin
      if ND.RecId > 0 then
      begin
        if ND.Field.FieldSource = tfsForm then
	  	    DS.FieldByName(FNm + 'l').Value := N.Text
        else
        	DS.FieldByName(FNm + 'l').Value := GetNodePath(N);
	      DS.FieldByName(FNm).Value:=ND.RecId;
    	end
      else
        TdxLookupComboBox(C).ClearData;
    end
    else if C is TdxObjectField then
    	// Пропускаем
    else
    begin
      if N.Text <> '' then
	    	DS.FieldByName(FNm).Value := N.Text
      else
        DS.FieldByName(FNm).SetData(nil);
    end;
    // Для объектов только одна итерация
    if TF.FieldSource = tfsObject then Break;
    //
    N := N.Parent;
  end
end;

procedure TDataSetProcessor.InsertCallerObjectValues;
var
  Flt: String;
  ObjFm: TdxForm;
begin
  if CallerObject = nil then Exit;
  Flt := GetComboFilter(CallerObject);
  if Trim(Flt) = '' then Exit;
  if MasterSet.State in [dsInsert, dsEdit] then
  begin
    ObjFm := TdxForm(CallerObject.Owner);
    with TDSPAppendRecParser.Create do
    try
      Form := ObjFm;
      ParForm := ObjFm.ParentForm;
      DS := ObjFm.DataSet;
      SrcForm := FFm;
      SrcDS := MasterSet;
      Parse(Flt);
    finally
      Free;
    end;
  end;
end;

function TDataSetProcessor.CheckEditAccess(DSRi: Integer): Boolean;
var
  DSR: TDataSetRec;
  V: Variant;
begin
  DSR := GetDataSet(DSRi)^;
  Result := DSR.Editing;
  if not Result then Exit;

  if (DSR.EditCond <> nil) and (DSR.DataSet.RecordCount > 0) then
    try
	    V := DSR.EditCond.Calc;
      if VarIsBool(V) then
      	Result := V;
    except
      on E: Exception do
        DSR.Err.AddErr('', DSR.Form.FormCaption, rsForm, rsEditCond, UserMan.GetEditCond(DSR.Form.Id), E);
      	//ErrMsg(E.Message);
    end;
end;

function TDataSetProcessor.CheckDeleteAccess(DSRi: Integer; Details: Boolean
  ): Boolean;
var
  DSR: TDataSetRec;
  V: Variant;
  i: Integer;
  DS: TSQLQuery;
  B: TBookMark;
begin
  DSR := GetDataSet(DSRi)^;
  Result := DSR.Deleting and CheckEditAccess(DSRi);
  if not Result then Exit;

  if (DSR.DelCond <> nil) and (DSR.DataSet.RecordCount > 0) then
    try
	    V := DSR.DelCond.Calc;
      if VarIsBool(V) then
      	Result := V;
    except
      on E: Exception do
        DSR.Err.AddErr('', DSR.Form.FormCaption, rsForm, rsDelCond, UserMan.GetDelCond(DSR.Form.Id), E);
      //ErrMsg(E.Message);
    end;

  if not Result then Exit;

  if (DSRi = 0) and Details then
  begin
    for i := 1 to FItems.Count - 1 do
    begin
      DSR := GetDataSet(i)^;
      if DSR.DataSet.RecordCount = 0 then Continue;
      Result := DSR.Deleting and CheckEditAccess(i);
      if not Result then Exit;
      if DSR.DelCond = nil then Continue;
      DS := DSR.DataSet;

      LockDSScroll(DS);
      DS.DisableControls;
      B := DS.GetBookmark;
      DS.First;
      while (not DS.Eof) and (Result) do
      begin
        try
	        V := DSR.DelCond.Calc;
        except
          on E: Exception do
      	    ErrMsg(E.Message, True, 'CheckDeleteAccess');
        end;
        if VarIsBool(V) then
    	    Result := V;
        DS.Next;
      end;
      DS.GotoBookmark(B);
      DS.FreeBookmark(B);
      DS.EnableControls;
      UnlockDSScroll(DS);
      if not Result then Break;
    end;
  end;
end;

{function TDataSetProcessor.CheckRecordLock: Integer;
var
  S: String;
begin
  Result := -1;
  S := 'select uid from dx_lock where fmid=' + IntToStr(FFm.Id) + ' and recid=' +
    IntToStr(MasterSet.Fields[0].AsInteger);
  with DBase.OpenDataSet(S) do
  try
    if RecordCount > 0 then Result := Fields[0].AsInteger;
  finally
    Free;
  end;
end; }

function TDataSetProcessor.CheckRecordModify: Integer;
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
  S := SqlSimpleSelectStatement(DSR.Form, DSR.DataSet.FieldByName('id').AsInteger);
  if S = '' then Exit;

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

function TDataSetProcessor.LockRecord(DoLock: Boolean): Integer;
var
  S: String;
begin
  Result := -1;
  if not IsNeedUserControl or (FFm.LockMode <> lmPessimistic) then Exit;

  S := 'select uid from dx_lock where fmid=' + IntToStr(FFm.Id) + ' and recid=' +
    IntToStr(MasterSet.Fields[0].AsInteger);
  with DBase.OpenDataSet(S{, 'DX_LOCK'}) do
  begin
    if RecordCount > 0 then Result := Fields[0].AsInteger;
    Free;
  end;
  if (Result >= 0) or (not DoLock) then
  begin
    //DBase.SafeCommit;
    Exit;
  end;

  S := 'insert into dx_lock (uid, fmid, recid, dtime, cid) values (' +
    IntToStr(UserMan.CurrentUserId) + ',' + IntToStr(FMaster^.Form.Id) + ',' +
    IntToStr(MasterSet.Fields[0].AsInteger) + ',CURRENT_TIMESTAMP,' +
    IntToStr(UserMan.ConnId) + ');';
  DBase.Execute(S);
end;

procedure TDataSetProcessor.UnlockRecord;
var
  S: String;
begin
  if not IsNeedUserControl or (FFm.LockMode <> lmPessimistic) then Exit;
  S := 'delete from dx_lock where uid=' + IntToStr(UserMan.CurrentUserId) +
    ' and fmid=' + IntToStr(FMaster^.Form.Id) + ' and recid=' +
    IntToStr(MasterSet.Fields[0].AsInteger) + ';';
  DBase.Execute(S);
end;

procedure TDataSetProcessor.LockDSScroll(DS: TDataSet);
begin
  DS.BeforeScroll := nil;
  DS.AfterScroll := nil;
end;

procedure TDataSetProcessor.UnlockDSScroll(DS: TDataSet);
begin
  DS.BeforeScroll := @DataSetBeforeScroll;
  DS.AfterScroll := @DataSetAfterScroll;
end;

procedure TDataSetProcessor.LockDSEdit(DS: TDataSet);
begin
	DS.BeforeEdit := nil;
  DS.AfterEdit := nil;
end;

procedure TDataSetProcessor.UnlockDSEdit(DS: TDataSet);
begin
	DS.BeforeEdit := @DataSetBeforeEdit;
  DS.AfterEdit := @DataSetAfterEdit;
end;

procedure TDataSetProcessor.LockDSPost(DS: TDataSet);
begin
  DS.BeforePost := nil;
  DS.AfterPost := nil;
end;

procedure TDataSetProcessor.UnlockDSPost(DS: TDataSet);
begin
  DS.BeforePost := @DataSetBeforePost;
  DS.AfterPost := @DataSetAfterPost;
end;

procedure TDataSetProcessor.LockDSDelete(DS: TDataSet);
begin
  DS.BeforeDelete := nil;
  DS.AfterDelete := nil;
end;

procedure TDataSetProcessor.UnlockDSDelete(DS: TDataSet);
begin
  DS.BeforeDelete := @DataSetBeforeDelete;
  DS.AfterDelete := @DataSetAfterDelete;
end;

procedure TDataSetProcessor.RefreshAllLookups(FmId: Integer);
begin
  if not Testing then MainFr.RefreshAllLookups(FmId)
  else TDataSetProcessor(MyTestForm.DSP).RefreshLookups(FmId);
end;

procedure TDataSetProcessor.RepaintAllGrids;
var
  i: Integer;
begin
  for i := 0 to DataSetCount - 1 do
    DataSets[i]^.Grid.Repaint;
  for i := 0 to QueryCount - 1 do
  	Queries[i]^.Grid.Repaint;
end;

{procedure TDataSetProcessor.ClearAllChildData(DSRi: Integer);

  procedure ClearDataSet(DS: TDataSet);
  var
    BeforeDelete, AfterDelete, BeforeScroll, AfterScroll: TDataSetNotifyEvent;
  begin
    BeforeDelete := DS.BeforeDelete;
    AfterDelete := DS.AfterDelete;
    BeforeScroll := DS.BeforeScroll;
    AfterScroll := DS.AfterScroll;
    DS.BeforeDelete := nil;
    DS.AfterDelete := nil;
    DS.BeforeScroll := nil;
    DS.AfterScroll := nil;
    DS.DisableControls;
    while not DS.EOF do
      DS.Delete;
    DS.BeforeDelete := BeforeDelete;
    DS.AfterDelete := AfterDelete;
    DS.BeforeScroll := BeforeScroll;
    DS.AfterScroll := AfterScroll;
    DS.EnableControls;
  end;

var
  i: Integer;
  Q: TQueryRec;
begin
  if DSRi = 0 then
	  for i := 1 to DataSetCount - 1 do
  		ClearDataSet(DataSets[i]^.DataSet);
  for i := 0 to QueryCount - 1 do
  begin
    Q := Queries[i]^;
    if (Q.DSRi = DSRi) and not Q.Grid.ManualRefresh then
    	ClearDataSet(Q.DataSet);
  end;
end;   }

procedure TDataSetProcessor.ClearCalcLabels(DSRi: Integer;
  const FieldName: String);
var
  i: Integer;
  DSR: TDataSetRec;
  LE: TExprData;
  Lbl: TdxLabel;
begin
  DSR := GetDataSet(DSRi)^;
  for i := 0 to DSR.LblExprList.Count - 1 do
  begin
    LE := DSR.LblExprList[i];
    Lbl := TdxLabel(LE.C);
    if (FieldName = '') or
    ( FieldExists(DSRi, FieldName, Lbl.Expression) and (FieldName <> Lbl.FieldName) ) then
    begin
      Lbl.Value := unassigned;

      if not FIsRequeryAll and (FieldName <> '') then
      begin
        if Copy(FieldName, 1, 1) <> '!' then
          LE.Changed := True
        else
          LE.ParentChanged := True;
      end;
    end;
  end;
  if (DSRi = 0) and (FieldName <> '') then
    for i := 1 to DataSetCount - 1 do
      ClearCalcLabels(i, '!' + FieldName);
end;

procedure TDataSetProcessor.ClearAggCalcLabels(DSRi: Integer;
  const FormName: String);
var
  DSR: TDataSetRec;
  L: TExprList;
  i: Integer;
  LE: TExprData;
  Lbl: TdxLabel;
begin
  DSR := GetDataSet(DSRi)^;
  L := DSR.LblExprList;
  for i := 0 to L.Count - 1 do
  begin
    LE := L[i];
    if FormExists(FormName, GetExpression(LE.C)) then
    begin
      Lbl := TdxLabel(LE.C);
      Lbl.Value := unassigned;
      ClearCalcLabels(DSRi, Lbl.FieldName);
    end;
  end;
end;

procedure TDataSetProcessor.GetTabOrderComponents(Fm: TdxForm; List: TList);

  procedure AddToList(L: TList; WC: TWinControl);
  var
    i: Integer;
    t: TTabOrder;
  begin
    t := WC.TabOrder;
    for i := 0 to L.Count - 1 do
    begin
      if t < TWinControl(L[i]).TabOrder then
      begin
        L.Insert(i, WC);
        Exit;
      end;
    end;
    L.Add(WC);
  end;

  procedure ProcessSort(WC: TWinControl);
  var
    i: Integer;
    C: TControl;
    L: TList;
  begin
    L := TList.Create;

    for i := 0 to WC.ControlCount - 1 do
    begin
      C := WC.Controls[i];
      if (C.Owner = Fm) and (C is TWinControl) then
        AddToList(L, TWinControl(C));
    end;

    for i := 0 to L.Count - 1 do
    begin
      List.Add(L[i]);
      ProcessSort(TWinControl(L[i]));
    end;

    L.Free;
  end;

var
  i: Integer;
  C: TComponent;
begin
  ProcessSort(Fm);
  // Изображения не TWinControl, проверяем их последними
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxDBImage then List.Add(C);
  end;
end;

procedure TDataSetProcessor.ClearChartSources(Fm: TdxForm);
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxChart then
      with TdxChart(C) do
      begin
        Source.Clear;
        LabelSource.Clear;
      end;
  end;
end;

procedure TDataSetProcessor.FindImages(DSR: PDataSetRec);
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  Images: TList;
begin
  Fm := DSR^.Form;
  Images := DSR^.Images;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxDBImage then Images.Add(C)
  end;
end;

procedure TDataSetProcessor.UpdateRecIdFields(DSR: PDataSetRec);
var
  i: Integer;
  C: TComponent;
  F: TField;
  Fm: TdxForm;
begin
  Fm := DSR^.Form;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxRecordId then
    begin
      F := Fm.DataSet.FieldByName(FieldStr(C));
      F.Value := Fm.DataSet['id'];
    end;
  end;
end;

procedure TDataSetProcessor.ShowImages(DSRi: Integer);
var
  Images: TList;
  i: Integer;
begin
  Images := GetDataSet(DSRi)^.Images;
  for i := 0 to Images.Count - 1 do
    TdxDBImage(Images[i]).ShowImage;
end;

procedure TDataSetProcessor.PrepareBeforeShowEditForm(DSRi: Integer);
begin
  SetNeedRefresh(DSRi, True);
  ClearCalcLabels(DSRi, '');
  RefreshLookupsWithParams('');
  UpdateControlState(DSRi);
end;

function TDataSetProcessor.AnyDataSetModified(DSRi: Integer): Boolean;
var
  i: Integer;
begin
  if DSRi = 0 then
  begin
    Result := FMaster^.RowsExchanged or FMaster^.DetailsChanged;
    if not Result then
      for i := 0 to DataSetCount - 1 do
      begin
  	    if (DataSets[i]^.DataSet.Modified and IsDataSetModified(DataSets[i]^.Form)) then Exit(True);
      end;
  end
  else
  	Result := DataSets[DSRi]^.DataSet.Modified and IsDataSetModified(DataSets[DSRi]^.Form)
end;

procedure TDataSetProcessor.Open;
var
  S: String;
  Ok: Boolean;
begin
  if FFm.ViewType <> vtSimpleForm then
  begin
    Ok := False;
    try
  	  S := SqlSelectStatement(FFm, nil, FFm.UseSelCond, True, FCallerObject, '', -1, False);
      Ok := True;
    except
      on E: ESQLSelectStatementError do
        if E.Prop = rsSelCond then
          FMaster^.Err.AddErr('', FFm.FormCaption, rsForm, rsSelCond, UserMan.GetSelCond(FFm.Id), E)
        // Вызов окна списка объекта или заметки
        else raise;
    end;

    // Если есть ошибка в условии отбора, то открываем набор без условия
    if not Ok then
      S := SqlSelectStatement(FFm, nil, False, True, FCallerObject, '', -1, False);

	  if FMaster^.Grid.SortCols.Count > 0 then
      S := S + ' order by ' + SortColumnsToSQL(FMaster^.Grid);
  end
  else
		S := SqlSelectStatement(FFm, nil, False, False, nil, '', 0, False);

  FMaster^.DataSet.SQL.Text := S;
  FMaster^.DataSet.Open;
  if (MasterSet.RecordCount = 0) and (FFm.ViewType <> vtSimpleForm) then
  begin
    MasterSet.AfterScroll(MasterSet);
  end;
  RefreshLookups(0);
  if FFm.ViewType = vtSimpleForm then Append;
end;

function TDataSetProcessor.OpenRecord(aKey: Integer): Boolean;
begin
  Result := True;
  FMaster^.DataSet.Close;
  FMaster^.DataSet.SQL.Text := SqlSelectStatement(FFm, nil, False, False, nil, '', aKey, False);
  FMaster^.DataSet.Open;
  RefreshLookups(0);
  if MasterSet.RecordCount = 0 then
  begin
    if aKey = 0 then MasterSet.AfterScroll(MasterSet)
    else
    begin
      Result := False;
      ErrMsg(rsRecHasBeenDeleted);
    end;
  end;
end;

procedure TDataSetProcessor.OpenReport;
var
  i: Integer;
  F: TField;
  C: TComponent;
begin
  MasterSet.Open;
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TdxCalcEdit then
    begin
      F := MasterSet.FieldByName(FieldStr(C));
      if F is TNumericField then
        TNumericField(F).DisplayFormat := TdxCalcEdit(C).PrecStr;
    end;
  end;
end;

procedure TDataSetProcessor.Close;
begin
  FMaster^.DataSet.Close;
  CloseDetails;
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

{function FindComponentBySrcTId(Fm: TdxForm; TId: Integer): TComponent;
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
end;}

function TDataSetProcessor.AppendRecord(aPFm, aFm: TdxForm; aDS: TDataSet;
  RpId: Integer): Variant;
var
  RD: TReportData;
  Flt: String;
begin
  Result := Null;
  //FMaster^.Grid.EditorMode:=False;
  MasterSet.Append;
  RD := ReportMan.FindReport(RpId);
  Flt := RD.GetSourceFilter;
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
    if ShowEditForm(0) = mrOk then Result := MasterSet['id'];
end;

function TDataSetProcessor.AppendObject: Integer;
begin
  Result := mrNone;
  FMaster^.DataSet.Append;
  Result := ShowEditForm(0);
end;

procedure TDataSetProcessor.DuplicateRecord(DSRi: Integer; All: Boolean);
var
  Tables, Rows, Vals: TList;
  Fm: TdxForm;
  i: Integer;
  DS: TSQLQuery;
  Ctrl: TWinControl;
  DSR: TDataSetRec;

  procedure AddValue(aCols: TList; const V: Variant);
  var
    pV: PVariant;
  begin
		New(pV);
    aCols.Add(pV);
    pV^ := V;
  end;

  procedure CopyRec(aFm: TdxForm; aDS: TDataSet; aCols: TList);
  var
    j: Integer;
    C: TComponent;
    FNm: String;
  begin
    for j := 0 to aFm.ComponentCount - 1 do
    begin
      C := aFm.Components[j];
      FNm := FieldStr(C);
      if not HasFId(C) or (C is TdxCounter) or (C is TdxRecordId) {or
        ((Trim(GetExpression(C)) <> '') and not GetEditable(C))} then Continue;

      AddValue(aCols, aDS.FieldByName(FNm).Value);
      if C is TdxLookupComboBox then
        AddValue(aCols, aDS.FieldByName(FNm + 'l').Value)
      else if ObjectFieldIsObject(C) then
        AddValue(aCols, aDS.FieldByName(FNm + 'id').Value)
      else if C is TdxFile then
      begin
      	AddValue(aCols, aDS.FieldByName(FNm + 'd').Value);
        AddValue(aCols, aDS.FieldByName(FNm + 'src').Value);
        AddValue(aCols, aDS.FieldByName(FNm + 'dest').Value);
      end
      else if C is TdxDBImage then
      begin
      	AddValue(aCols, aDS.FieldByName(FNm + 'thumb').Value);
        AddValue(aCols, aDS.FieldByName(FNm + 'src').Value);
        AddValue(aCols, aDS.FieldByName(FNm + 'dest').Value);
      end;
    end;
  end;

  procedure CopyGrid(DSR: PDataSetRec; aRows: TList);
  var
    Frm: TdxForm;
    DSet: TDataSet;
    Cols: TList;
  begin
    Frm := DSR^.Form;
    DSet := DSR^.DataSet;
    LockDSScroll(DSet);
    DSet.DisableControls;
    DSet.First;
    while not DSet.Eof do
    begin
      Cols := TList.Create;
      aRows.Add(Cols);
      CopyRec(Frm, DSet, Cols);
      DSet.Next;
    end;
    UnlockDSScroll(DSet);
    DSet.EnableControls;
  end;

  function GetV(aCols: TList; i: Integer): Variant;
  begin
    Result := PVariant(aCols[i])^;
  end;

  procedure PasteRec(aFm: TdxForm; aDS: TDataSet; aCols: TList);
  var
    j, n: Integer;
    C: TComponent;
    FNm: String;
  begin
    n := 0;
    for j := 0 to aFm.ComponentCount - 1 do
    begin
      C := aFm.Components[j];
      if not HasFId(C) or (C is TdxCounter) or (C is TdxRecordId) {or
        ((Trim(GetExpression(C)) <> '') and not GetEditable(C))} then Continue;
      FNm := FieldStr(C);
      if C is TdxLookupComboBox then
      begin
        aDS.FieldByName(FNm + 'l').Value := GetV(aCols, n + 1);
        aDS.FieldByName(FieldStr(C)).Value := GetV(aCols, n);
        Inc(n);
      end
      else if ObjectFieldIsObject(C) then
      begin
        aDS.FieldByName(FNm + 'id').Value := GetV(aCols, n + 1);
        aDS.FieldByName(FieldStr(C)).Value := GetV(aCols, n);
        Inc(n);
      end
      else if C is TdxFile then
      begin
        aDS.FieldByName(FieldStr(C)).Value := GetV(aCols, n);
        aDS.FieldByName(FNm + 'd').Value := GetV(aCols, n + 1);
        aDS.FieldByName(FNm + 'dest').Value := GetV(aCols, n + 3);
        aDS.FieldByName(FNm + 'src').Value := GetV(aCols, n + 2);
        n := n + 3;
      end
      else if C is TdxDBImage then
      begin
        aDS.FieldByName(FieldStr(C)).Value := GetV(aCols, n);
        aDS.FieldByName(FNm + 'thumb').Value := GetV(aCols, n + 1);
        aDS.FieldByName(FNm + 'dest').Value := GetV(aCols, n + 3);
        aDS.FieldByName(FNm + 'src').Value := GetV(aCols, n + 2);
        n := n + 3;
      end
      else
        aDS.FieldByName(FieldStr(C)).Value := GetV(aCols, n);

      Inc(n);
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
    CalcAggFields(0{Frm.DSRi}, Frm.FormCaption);
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
  DSR := DataSets[DSRi]^;
  DSR.Grid.EditorMode:=False;
  if DSR.Form.OnBeforeDuplicate <> nil then DSR.Form.OnBeforeDuplicate(DSR.Form);

  FDuplicateFlag := True;

  Vals := TList.Create;
  Fm := DSR.Form;
  DS := DSR.DataSet;
  CopyRec(Fm, DS, Vals);

  if (DSRi = 0) and All then
  begin

    if Fm.IsHide then RequeryDetails;// RefreshAllData(0);

    Tables := TList.Create;
    for i := 1 to FItems.Count - 1 do
    begin
      Rows := TList.Create;
      Tables.Add(Rows);
      CopyGrid(GetDataSet(i), Rows);
    end;

  end;

  DS.Append;
  RequeryDetails;
  PasteRec(Fm, DS, Vals);

  if (DSRi = 0) and All then
  begin

    for i := 1 to DataSetCount - 1 do
    begin
      Rows := TList(Tables[0]);
      PasteGrid(DataSets[i], Rows);
      ClearRows(Rows);
      Rows.Free;
      Tables.Delete(0);
    end;
    Tables.Free;
    ClearCols(Vals);

  end;
  Vals.Free;

  if (DSRi = 0) and not All then
    for i := 1 to DataSetCount - 1 do
      CalcAggFields(0, GetDataSet(i)^.Form.FormCaption);

  if not Fm.IsHide then
  begin
    RefreshAllData(DSRi);

    UpdateQueryPopupStates(DSRi);
    UpdateControlState(DSRi);
  end;

  FDuplicateFlag := False;

  if DSR.Form.OnAfterDuplicate <> nil then DSR.Form.OnAfterDuplicate(DSR.Form);

  if IsTableInput(@DSR) then
  else if DSR.EditFm <> nil then
    ShowEditForm(DSRi)
  else if (Fm.ViewType <> vtGridOnly) and (Fm.Grid.ReadOnly) then
  begin
    Ctrl := GetTopControl(Fm);
    if (Ctrl <> nil) and (Ctrl.CanFocus) then Ctrl.SetFocus;
  end;
end;

function TDataSetProcessor.InnerEdit(DSRi: Integer; ShowMsg, DoEditing,
  CallFromScript: Boolean): TAccessStatus;
var
  UId, r: Integer;
  U: TdxUser;
begin
  Result := asOk;

  if FSimpleMode and (DSRi = 0) then
  begin
    MasterSet.Edit;
    Exit;
  end
  else if GetDataSet(DSRi)^.DataSet.RecordCount = 0 then
    Exit(asCantEdit);

  if (not CallFromScript) and (not CheckEditAccess(DSRi)) then
  begin
    Result := asCantEdit;
    Exit;
  end;

  if DSRi = 0 then
  begin

    r := CheckRecordModify;
    if r > 0 then
    begin
      if r = 1 then
    	  Result := asDeleted
      else if r = 2 then
	      Result := asModified;
      Exit;
    end;

    UId := LockRecord(DoEditing);
    if UId >= 0 then
    begin
      Result := asLocked;
      if ShowMsg then
      begin
        U := UserMan.Users.FindUser(UId);
        if U <> nil then
          Info(Format(rsRecEditUser, [U.Name]));
      end;
      Exit;
    end;

  end
  else
  begin
    if (Result = asOk) and (not (MasterSet.State in [dsInsert, dsEdit])) then
    begin
    	Result := asCantEdit;
      Exit;
    end;
  end;

  if DoEditing then GetDataSet(DSRi)^.DataSet.Edit;
end;

procedure TDataSetProcessor.Edit;
var
  C: TWinControl;
  St: TAccessStatus;
begin
  if FSimpleMode then
  begin
    MasterSet.Edit;
    Exit;
  end;

  St := InnerEdit(0, True, True, False);
  if St = asModified then
  begin
    if Confirm(rsWarning, rsRecChangesDetect) = mrYes then
    begin
      RefreshDataAndEdit;
      Exit;
    end
    else Exit;
  end
  else if St = asDeleted then
  begin
    if Confirm(rsWarning, rsRecHasBeenDeletedRefresh) = mrYes then
    begin
      Refresh;
      Exit;
    end
    else Exit;
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

function TDataSetProcessor.EditObject(ViewOnly: Boolean): Integer;
begin
  Result := mrNone;
  if ViewOnly or (InnerEdit(0, True, True, False) <> asDeleted) then
	  Result := ShowEditForm(0)
end;

function TDataSetProcessor.InnerDelete(DSRi: Integer; ShowMsg, DoDeleting,
  CallFromScript: Boolean): TAccessStatus;
begin
  Result := InnerEdit(DSRi, False, False, CallFromScript);

  if Result <> asOk then
  begin
    if ShowMsg then
      case Result of
        asCantEdit: Info(rsCantDelRec);
        asLocked: Info(rsRecEditAnotherUser);
      end;
    // Проверяем ведь на возможность удаления...
    if Result = asCantEdit then Result := asCantDelete;
    Exit;
  end;

  if (not CallFromScript) and (not CheckDeleteAccess(DSRi, True)) then
  begin
    Result := asCantDelete;
    if ShowMsg then
    	Info(rsCantDelRec);
    Exit;
  end;

  if DSRi = 0 then
  begin

    if not CheckDeleteRecord(FFm.Id, MasterSet['id'], ShowMsg) then
    begin
      Result := asHasRef;
      Exit;
    end;

    if DoDeleting then MasterSet.Delete;  //DoDelete;
  end
  else
  begin
    if (Result = asOk) and (not (MasterSet.State in [dsInsert, dsEdit])) then
    begin
    	Result := asCantDelete;
      Exit;
    end;
    if DoDeleting then GetDataSet(DSRi)^.DataSet.Delete;
  end;
end;

function TDataSetProcessor.Delete: Boolean;

  {function CheckDetails: Boolean;
  var
    i: Integer;
    pDS: PDataSetRec;
  begin
    Result := False;
    for i := 1 to FItems.Count - 1 do
    begin
      pDS := GetDataSet(i);
      if pDS^.NeedRefresh then RequeryDetail(i);
      if pDS^.DataSet.RecordCount > 0 then Exit(True);
    end;
  end; }

var
  St: TAccessStatus;
begin
  Result := False;
  if MasterSet['id'] = Null then Exit;
  // Если есть подчиненные данные, то выводим другое предупреждение
  {if CheckDetails then
  begin
    if ShowWarnForm(rsConfirmDelete2) <> mrOk then Exit;
  end
  else if not ConfirmDelete then Exit;}
  if Confirm(rsWarning, rsDeleteRecordMsg) <> mrYes then Exit;

  St := InnerDelete(0, True, True, False);
  if St = asModified then
  begin
    if Confirm(rsWarning, rsRecChangesDetect) = mrYes then
    begin
      Refresh;
      Exit;
    end;
  end
  else if St = asDeleted then
  begin
    if Confirm(rsWarning, rsRecHasBeenDeletedRefresh) = mrYes then
    begin
      Refresh;
      Exit;
    end;
  end
  else if St <> asOk then Exit;

  Result := True;
end;

procedure TDataSetProcessor.DeleteChildRecords(RecId: Integer);
var
  SQL: String;
  DSR: TDataSetRec;
  i: Integer;
begin
  SQL := '';
  for i := 1 to FItems.Count - 1 do
  begin
    DSR := GetDataSet(i)^;
    SQL := SQL + 'delete from ' + TableStr(DSR.Form.Id) + ' where pid=' +
      IntToStr(RecId) + ';';
  end;
  DBase.Execute(SQL);
end;

procedure TDataSetProcessor.DeleteAllRecords;
var
  NoDel: Boolean;
  id: Integer;
begin
  //FMaster^.Grid.EditorMode:=False;
  NoDel := False;
  LockDSScroll(MasterSet);
  LockDSDelete(MasterSet);
  MasterSet.DisableControls;
  MasterSet.First;
  try try
    while not MasterSet.Eof do
    begin
      id := MasterSet['id'];
      if CheckDeleteRecord(FMaster^.Form.Id, id, False) then
      begin
        MasterSet.Delete;
        DeleteChildRecords(id);
        DBase.ApplyDataSet(MasterSet);
        DBase.Commit;
      end
      else
      begin
        NoDel := True;
        MasterSet.Next;
      end;
    end;
  except
    on E: Exception do
      ErrMsg(rsDeleteRecsError + ExceptionToString(E, True, False), True, 'DeleteAllRecords');
  end;
  finally
    MasterSet.EnableControls;
    UnlockDSScroll(MasterSet);
    UnlockDSDelete(MasterSet);
    MasterSet.AfterScroll(MasterSet);
  end;
  // При удалении иерархических данных, запись может удалиться позже, после
  // удаления подчиненных элементов.
  if NoDel and (MasterSet.RecordCount > 0) then ErrMsg(rsErrDelRecs, True, 'DeleteAllRecords');
end;

procedure TDataSetProcessor.Post;
begin
  if FMaster^.DataSet.State in [dsInsert, dsEdit] then
    FMaster^.DataSet.Post;
end;

procedure TDataSetProcessor.Refresh;
begin
  Close;
  Open;
  RefreshAllLookups(FMaster^.Form.Id);
end;

procedure TDataSetProcessor.First;
begin
  FMaster^.DataSet.First;
end;

procedure TDataSetProcessor.Last;
begin
  FMaster^.DataSet.Last;
end;

procedure TDataSetProcessor.Next;
begin
  FMaster^.DataSet.Next;
end;

procedure TDataSetProcessor.Prior;
begin
  FMaster^.DataSet.Prior;
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
  Cbx: TdxComboBox;
  DSR: TDataSetRec;
  DS: TSQLQuery;
  SQL: String;
begin
  DSR := GetDataSet(LR.DSRi)^;

  try
    SQL := SqlLookupSelect(LR.Control, DSR.Form, GetDataSet(0)^.Form, DSR.DataSet, True, 0);
  except
    on E: Exception do
    begin
      DSR.Err.AddErrC(LR.Control, rsFilter, GetComboFilter(LR.Control), E);
      //ErrMsg(E.Message);
      Exit;
    end;
  end;
  DS := DBase.OpenDataSet(SQL);
  try
    Cbx := TdxComboBox(LR.Control);
    Cbx.Items.Clear;
    while not DS.Eof do
    begin
      Cbx.Items.Add(DS.Fields[1].AsString);
      DS.Next;
    end;
    if not TDBGrid(LR.Column.Grid).ReadOnly then
      LR.Column.PickList.Assign(Cbx.Items);
  finally
    DS.Free;
  end;
end;

procedure TDataSetProcessor.ChangeObjectFields(Obj: TdxLookupComboBox);
var
  Fm, SrcFm: TdxForm;
  RecId: Variant;
  FL, SrcFL: TList;
  i: Integer;
  C, SrcC: TComponent;
  SQL, FNm, SrcFNm, S: String;
  SrcDS: TSQLQuery;
  DS: TDataSet;
  HierarchyFId: Integer;
begin
  SrcFm := FormMan.FindForm(Obj.SourceTId);
  if SrcFm = nil then Exit;

  Fm := TdxForm(Obj.Owner);
  RecId := Obj.KeyValue;
  DS := Fm.DataSet;

  FL := TList.Create;
  SrcFL := TList.Create;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxObjectField) and (TdxObjectField(C).ObjId = Obj.Id) then
    begin
      SrcC := FindById(SrcFm, TdxObjectField(C).FieldId);
      if SrcC <> nil then
      begin
        FL.Add(C);
        SrcFL.Add(SrcC);
      end;
    end;
  end;

  HierarchyFId := 0;
  if SrcFm.ParentField > 0 then
  begin
    SrcC := FindById(SrcFm, SrcFm.ParentField);
    // Добавляем родительский объект, если форма иерархическая и в полях объекта
    // нет родительского объекта, а поле объекта "Название" присутствует.
    // Таким образом SrcFL.Count > FL.Count на 1.
    if SrcFL.IndexOf(SrcC) < 0 then
    begin
      C := FindById(SrcFm, GetSourceFId(SrcC));
      if SrcFL.IndexOf(C) >= 0 then
      begin
        SrcFL.Add(SrcC);
        HierarchyFId := GetId(C);
      end;
    end
    // Если есть и родитель, и "название", то запоминает id "названия".
    else
    begin
      C := FindById(SrcFm, GetSourceFId(SrcC));
      if SrcFL.IndexOf(C) >= 0 then
        HierarchyFId := GetId(C);
    end;
  end;

  if FL.Count > 0 then
  begin
  	if RecId <> Null then
  	begin
      SQL := SqlSelectStatement(SrcFm, SrcFL, False, False, nil, '', RecId, False);

    	SrcDS := DBase.OpenDataSet(SQL);
      if SrcDS.RecordCount > 0 then
		  	for i := 0 to FL.Count - 1 do
        begin
          SrcC := TComponent(SrcFL[i]);
          SrcFNm := FieldStr(SrcC);
          C := TComponent(FL[i]);
          FNm := FieldStr(C);
          if SrcC is TdxLookupComboBox then
          begin
            if (GetSourceTId(SrcC) > 0) and (GetSourceFId(SrcC) > 0) then
            begin
              DS.FieldByName(FNm + 'id').Value := SrcDS.FieldByName(SrcFNm).Value;
              DS.FieldByName(FNm).Value:=SrcDS.FieldByName(SrcFNm + 'l').Value;
            end;
          end
          else
          begin
            if GetId(SrcC) = HierarchyFId then
            begin
              S := SrcDS.FieldByName(FieldStr(SrcFm.ParentField) + 'l').AsString;
              if S <> '' then S := S + '\';
              DS.FieldByName(FNm).Value := S + SrcDS.FieldByName(SrcFNm).AsString;
            end
            else
              DS.FieldByName(FNm).Value := SrcDS.FieldByName(SrcFNm).Value;
          end;
        end;
      SrcDS.Free;
    end
    else
    begin
      for i := 0 to FL.Count - 1 do
      begin
        SrcC := TComponent(SrcFL[i]);
        C := TComponent(FL[i]);
        FNm := FieldStr(C);
        if SrcC is TdxLookupComboBox then
        	DS.FieldByName(FNm + 'id').SetData(nil);
        DS.FieldByName(FNm).SetData(nil);
      end;
    end;
  end;
  FL.Free;
  SrcFL.Free;
end;

procedure TDataSetProcessor.ApplyQuickFilter(AClearFilter: Boolean);
var
  Col: TColumn;
  C: TComponent;
  S: String;
  IsObjField: Boolean;
  F: TFilterField;
  Field: TField;
begin
  Col := FMaster^.Grid.SelectedColumn;
  C := FindById(FMaster^.Form, Col.Tag);
  IsObjField := (C is TdxObjectField);
  if IsObjField then
    C := LookupObjectField(TdxObjectField(C), False);
  if C = nil then Exit;
  if (C is TdxDBImage) or (C is TdxFile) then Field := GetComponentField(MasterSet, C)
  else Field := Col.Field;

  if AClearFilter then FMaster^.Filter.Clear;

  F := FMaster^.Filter.FindField(Col.Tag);
  if F = nil then
  begin
    F := FMaster^.Filter.AddField;
    F.FId := Col.Tag;
  end;

  if Field.IsNull then
    F.IsNull := True
  else
  begin
    S := Field.AsString;
    if (C is TdxCalcEdit) or (C is TdxDateEdit) or (C is TdxTimeEdit) or
      (C is TdxCounter) or (C is TdxRecordId) then
      S := S + ' .. ' + S
    else if C is TdxLookupComboBox then
    begin
      if not IsObjField then
        S := MasterSet.FieldByName(FieldStr(Col.Tag)).AsString
      else
      	S := MasterSet.FieldByName(FieldStr(Col.Tag) + 'id').AsString
    end;
    if F.Values.IndexOf(S) < 0 then
      F.Values.Add(S);
  end;
  FMaster^.FilterIndex:=-1;
  Refresh;
  FFm.Tree.SoftClearSelection;
end;

procedure TDataSetProcessor.ClearAllFilters;
var
  i: Integer;
begin
  FMaster^.Filter.Clear;
  FMaster^.FilterIndex:=-1;
  for i := 1 to DataSetCount - 1 do
    DataSets[i]^.Filter.Clear;
  Refresh;
  FFm.Tree.SoftClearSelection;
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

(*procedure TDataSetProcessor.CalcQueryColor(RD: TReportData; Fm: TdxForm; RDS,
  DS: TDataSet; const TargetField: String; out FieldName: String; out
  Color: TColor);
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
      if CompareText(CD.FieldNameDS, TargetField) <> 0 then Continue;
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
end;    *)

procedure TDataSetProcessor.InsertObjectValues(Obj: TdxLookupComboBox);
var
  Fm, SrcFm: TdxForm;
  RecId: Variant;
  FL, SrcFL: TList;
  i: Integer;
  C, SrcC: TComponent;
  SQL, FNm, SrcFNm: String;
  SrcDS: TSQLQuery;
  DS: TDataSet;
  Vl: TInsertValueData;
begin
  Fm := TdxForm(Obj.Owner);
  RecId := Obj.KeyValue;
  DS := Fm.DataSet;

  SrcFm := FormMan.FindForm(Obj.SourceTId);

  FL := TList.Create;
  SrcFL := TList.Create;
  for i := 0 to Obj.InsertedValues.Count - 1 do
  begin
    Vl := Obj.InsertedValues[i];
    SrcC := FindById(SrcFm, Vl.SrcField);
    C := FindById(Fm, Vl.DestField);
    if (SrcC = nil) or (C = nil) then Continue;
    Fl.Add(C);
    SrcFL.Add(SrcC);
  end;

  if FL.Count > 0 then
  begin
  	if RecId <> Null then
  	begin
      SQL := SqlSelectStatement(SrcFm, SrcFL, False, False, nil, '', RecId, False);

    	SrcDS := DBase.OpenDataSet(SQL);
      if SrcDS.RecordCount > 0 then
		  	for i := 0 to FL.Count - 1 do
        begin
          SrcC := TComponent(SrcFL[i]);
          SrcFNm := FieldStr(SrcC);
          C := TComponent(FL[i]);
          FNm := FieldStr(C);
          if SrcC is TdxLookupComboBox then
          begin
            DS.FieldByName(FNm + 'l').Value := SrcDS.FieldByName(SrcFNm + 'l').Value;
            DS.FieldByName(FNm).Value:=SrcDS.FieldByName(SrcFNm).Value;
          end
          else if C is TdxLookupComboBox then
          begin
            DS.FieldByName(FNm + 'l').Value := GetObjFieldValue(C, SrcDS.FieldByName(SrcFNm).Value, True);
            DS.FieldByName(FNm).Value := SrcDS.FieldByName(SrcFNm).Value;
          end
          else if C is TdxFile then
          begin
            DS.FieldByName(FNm).Value := SrcDS.FieldByName(SrcFNm).Value;
            DS.FieldByName(FNm + 'd').Value := SrcDS.FieldByName(SrcFNm + 'd').Value;
            DS.FieldByName(FNm + 'dest').Value := SrcDS.FieldByName(SrcFNm + 'dest').Value;
            DS.FieldByName(FNm + 'src').Value := SrcDS.FieldByName(SrcFNm + 'src').Value;
            // Просто меняем значение поля для определения, что blob был изменен.
            DS.FieldByName(FNm + 'c').AsInteger:=DS.FieldByName(FNm + 'c').AsInteger+1;
          end
          else if C is TdxDBImage then
          begin
            DS.FieldByName(FNm).Value := SrcDS.FieldByName(SrcFNm).Value;
            DS.FieldByName(FNm + 'thumb').Value := SrcDS.FieldByName(SrcFNm + 'thumb').Value;
            DS.FieldByName(FNm + 'dest').Value := SrcDS.FieldByName(SrcFNm + 'dest').Value;
            DS.FieldByName(FNm + 'src').Value := SrcDS.FieldByName(SrcFNm + 'src').Value;
            // Просто меняем значение поля для определения, что blob был изменен.
            DS.FieldByName(FNm + 'c').AsInteger:=DS.FieldByName(FNm + 'c').AsInteger+1;
            TdxDBImage(C).ShowImage;
          end
          else
            DS.FieldByName(FNm).Value:=SrcDS.FieldByName(SrcFNm).Value;
        end;
      SrcDS.Free;
    end
    else
    begin
      for i := 0 to FL.Count - 1 do
      begin
        C := TComponent(FL[i]);
        FNm := FieldStr(C);
        if C is TdxLookupComboBox then
        	TdxLookupComboBox(C).ClearData
        else if C is TdxFile then
        	TdxFile(C).Clear
        else if C is TdxDBImage then
        	TdxDBImage(C).Clear
        else
	        DS.FieldByName(FNm).SetData(nil);
      end;
    end;
  end;
  FL.Free;
  SrcFL.Free;
end;

procedure TDataSetProcessor.FillTableFromObject(Obj: TdxLookupComboBox);
var
  SFm, DFm: TdxForm;
  SQL: String;
  //Grid: TdxGrid;
  DS, SrcDS: TDataSet;
  i, k: Integer;
  SL: TStrings;
  SC, DC: TComponent;
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
  pDSR: PDataSetRec;
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
  SQL := 'select * from ' + TableStr(SFm.Id)  + ' where pid=' + VarToStr(Obj.KeyValue);

  pDSR := FindDataSet(Obj.DestTable);
  TestNil(pDSR, 'pDSR=nil FillTableFromObject');
  DFm := pDSR^.Form;
  DFm.RequeryIfNeed;
  DS := pDSR^.DataSet;

  {Grid := FindGridById(FFm, DFm.Id);
  DS := Grid.DataSource.DataSet;  }

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
          FMaster^.Err.AddErrC(Obj, rsFillTableFilter, Obj.FillFilter, Ex);
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
            FMaster^.Err.AddErrC(Obj, rsFillTableFilter, Obj.FillFilter, Ex);
        end;
      end;
      DS.Append;
      for i := 0 to SL.Count - 1 do
      begin
        if (SL.Names[i] = '') or (SL.ValueFromIndex[i] = '') then Continue;

        SC := FindById(SFm, StrToInt(SL.Names[i]));
        DC := FindById(DFm, StrToInt(SL.ValueFromIndex[i]));
        if DC is TdxLookupComboBox then
        begin
          k := FieldByName(FieldStr(SC)).AsInteger;
          if k > 0 then
          begin
            DS.FieldByName(FieldStr(DC) + 'l').Value := GetObjFieldValue(DC, k, True);
            DS.FieldByName(FieldStr(DC)).Value := k;
          end;
        end
        else if DC is TdxFile then
        begin
          DS.FieldByName(FieldStr(DC)).Value := FieldByName(FieldStr(SC)).Value;
          DS.FieldByName(FieldStr(DC) + 'dest').Value := FieldByName(FieldStr(SC) + 'dest').Value;
          DS.FieldByName(FieldStr(DC) + 'd').Value := FieldByName(FieldStr(SC) + 'd').Value;
          DS.FieldByName(FieldStr(DC) + 'src').Value := FieldByName(FieldStr(SC) + 'src').Value;
        end
        else if DC is TdxDBImage then
        begin
          DS.FieldByName(FieldStr(DC)).Value := FieldByName(FieldStr(SC)).Value;
          DS.FieldByName(FieldStr(DC) + 'dest').Value := FieldByName(FieldStr(SC) + 'dest').Value;
          DS.FieldByName(FieldStr(DC) + 'thumb').Value := FieldByName(FieldStr(SC) + 'thumb').Value;
          DS.FieldByName(FieldStr(DC) + 'src').Value := FieldByName(FieldStr(SC) + 'src').Value;
        end
        else
          DS.FieldByName(FieldStr(DC)).Value := FieldByName(FieldStr(SC)).Value;
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

procedure TDataSetProcessor.RefreshLookupsWithParams(const FieldName: String);
var
  i: Integer;
  pLR: PLookupRec;
  Flt: String;
begin
  for i := 0 to FLookups.Count - 1 do
  begin
    pLR := PLookupRec(FLookups[i]);
    Flt := GetComboFilter(pLR^.Control);
    if (Flt <> '') and ((FieldName = '') or FieldExists(pLR^.DSRi, FieldName, Flt)) then
    begin
      pLR^.NeedRefresh:=True;
      if pLR^.Control is TdxComboBox then
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
  C: TComponent;
begin
  Fm := pLR^.ListFm;
  if pLR^.ListFm = nil then
  begin
    Fm := TListWindow.CreateNew(nil);
    pLR^.ListFm := Fm;
    Fm.Load(pLR^.TId);
    TFormView(Fm.FormView).DataSetProc.CallerObject := pLR^.Control;

    C := pLR^.Control;
    if C is TdxLookupComboBox then
      with TdxLookupComboBox(C) do
      begin
        if OnCreateListWindow <> nil then OnCreateListWindow(C, Fm);
        if MainFm.OnCreateListWindow <> nil then MainFm.OnCreateListWindow(MainFm, Fm);
        Fm.UpdateTreeWhenShow := UpdateTree;
      end
    else if C is TdxMemo then
      with TdxMemo(C) do
      begin
        if OnCreateListWindow <> nil then OnCreateListWindow(C, Fm);
        if MainFm.OnCreateListWindow <> nil then MainFm.OnCreateListWindow(MainFm, Fm);
        Fm.UpdateTreeWhenShow := UpdateTree;
      end;
    if not Fm.UpdateTreeWhenShow then
      TFormView(Fm.FormView).Form.UpdateTree;
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
    TestNil(Cm, 'Field with id ' + IntToStr(C.Tag) + ' not found.');
    C.Title.Caption:=GetFieldName(Cm);
  end;
end;

procedure CopyColumns(sG, dG: TdxGrid);
var
  i: Integer;
begin
  dG.Columns := sG.Columns;
  for i := 0 to sG.Columns.Count - 1 do
  begin
    dG.Columns[i].Tag := sG.Columns[i].Tag;
    // Если в дизайнере установлено "Только чтение", то все столбцы тоже
    // только для чтения и разрешение в скрипте редактировать таблицу не
    // будет иметь эффекта. Поэтому сбрасываем флаг со столбцов.
    dG.Columns[i].ReadOnly:=False;
  end;
end;

procedure TDataSetProcessor.SetGrids;
var
  i, j: Integer;
  C: TComponent;
  G: TdxGrid;
  Fm: TdxForm;
  SD: TSortColumn;
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
      //G.FixedHotColor:=Fm.Grid.FixedHotColor;
      G.SelectedColor:=Fm.Grid.SelectedColor;
      G.SelectedTextColor:=Fm.Grid.SelectedTextColor;
      G.InactiveSelectedColor:=Fm.Grid.InactiveSelectedColor;
      G.InactiveSelectedTextColor:=Fm.Grid.InactiveSelectedTextColor;
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
      G.AllowChangeSort:=Fm.Grid.AllowChangeSort;
    end;
  end;
end;

{function TDataSetProcessor.CalcEditCond(Fm: TdxForm; DS: TDataSet;
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
end; }

procedure TDataSetProcessor.UpdatePopupState(DSRi: Integer);
var
  Pop: TPopupMenu;
  bEdit, bNotEdit, bHasRec, bMasterEdit, bNoSort, bCanEdit, bCanDel, hf: Boolean;
  DSR: TDataSetRec;
begin
  DSR := DataSets[DSRi]^;
  if DSR.DataSet.Active = False then Exit;

  bEdit := DSR.DataSet.State in [dsInsert, dsEdit];
  bNotEdit := not bEdit;
  bHasRec := DSR.DataSet.RecordCount > 0;
  bMasterEdit := MasterSet.State in [dsInsert, dsEdit];
  bNoSort := DSR.Grid.SortCols.Count = 0;
  bCanEdit := DSR.Editing and CheckEditAccess(DSRi);
  bCanDel := bCanEdit and DSR.Deleting and CheckDeleteAccess(DSRi, False);
  hf := DSR.HasFields;

  Pop := DSR.Popup;

  if bCanEdit then
  begin
    Pop.Items[1].Caption:=rsEdit;
    Pop.Items[1].ImageIndex := IMG16_EDIT;
  end
  else
  begin
    Pop.Items[1].Caption := rsLook;
    Pop.Items[1].ImageIndex := IMG16_EYES;
  end;

  if DSR.Form.PId = 0 then
  begin
    Pop.Items[0].Visible := DSR.Adding;
    Pop.Items[0].Enabled := DSR.Adding and bNotEdit and hf;
    Pop.Items[1].Enabled := bNotEdit and bHasRec and hf;
    Pop.Items[2].Visible := DSR.Deleting;
    Pop.Items[2].Enabled := bCanDel and bNotEdit and bHasRec and hf;
    Pop.Items[4].Visible := DSR.Adding and DSR.Editing;
    Pop.Items[4].Enabled := DSR.Adding and bCanEdit and bNotEdit and bHasRec and hf;
    Pop.Items[5].Visible := Pop.Items[4].Visible;
    Pop.Items[5].Enabled := Pop.Items[4].Enabled;
    Pop.Items[6].Visible := Pop.Items[5].Visible;
    Pop.Items[7].Enabled := bHasRec and bNotEdit and hf;
    Pop.Items[8].Enabled := Pop.Items[7].Enabled;
    Pop.Items[9].Enabled := bNotEdit and hf and DSR.Form.HasFilterValues;
  end
  else
  begin
    if not bMasterEdit then
    begin
      Pop.Items[1].Caption := rsLook;
      Pop.Items[1].ImageIndex := IMG16_EYES;
    end;
    Pop.Items[0].Visible := DSR.Adding;
    Pop.Items[0].Enabled := DSR.Adding and bNotEdit and bMasterEdit and hf;
    Pop.Items[1].Enabled := bNotEdit and bHasRec and hf;
    Pop.Items[2].Visible := DSR.Deleting;
    Pop.Items[2].Enabled := bCanDel and bNotEdit and bHasRec and bMasterEdit and hf;
    Pop.Items[3].Visible := DSR.Adding and DSR.Editing;
    Pop.Items[4].Visible := Pop.Items[3].Visible;
    Pop.Items[4].Enabled := DSR.Adding and bCanEdit and bNotEdit and bHasRec and bMasterEdit and hf;
    Pop.Items[5].Visible := DSR.CanShoping and DSR.Adding;
    Pop.Items[6].Visible := Pop.Items[5].Visible;
    Pop.Items[6].Enabled := DSR.Adding and bNotEdit and bMasterEdit and hf;
    Pop.Items[7].Visible := bNoSort and DSR.Editing;
    Pop.Items[8].Visible := bNoSort and DSR.Editing;
    Pop.Items[9].Visible := bNoSort and DSR.Editing;
    Pop.Items[8].Enabled := DSR.Editing and bMasterEdit and bNotEdit and (DSR.DataSet.RecNo > 1) and hf;
    Pop.Items[9].Enabled := DSR.Editing and bMasterEdit and bNotEdit and (DSR.DataSet.RecNo < DSR.DataSet.RecordCount) and hf;
  end;

  if DSR.Grid.ShowButtons then
    with DSR.Grid.Buttons do
    begin
      ShowButton(gbnAppend, Pop.Items[0].Visible);
      EnableButton(gbnAppend, Pop.Items[0].Enabled);
      EnableButton(gbnEdit, Pop.Items[1].Enabled);
      if Buttons[gbnEdit].ShowCaption then
	      Buttons[gbnEdit].Caption:=Pop.Items[1].Caption
      else
	      Buttons[gbnEdit].Hint:=Pop.Items[1].Caption;
      //Buttons[gbnEdit].Glyph.Assign(Pop.Items[1].Bitmap);
      Buttons[gbnEdit].ImageIndex := Pop.Items[1].ImageIndex;
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

  DataSets[DSRi]^.CanEdit := bCanEdit;
  DataSets[DSRi]^.CanDelete := Pop.Items[2].Enabled;
end;

procedure TDataSetProcessor.UpdateControlState(DSRi: Integer);
var
  i: Integer;
  //Bn: TSpeedButton;
  DSR: TDataSetRec;
begin
  UpdatePopupState(DSRi);
  DSR := DataSets[DSRi]^;
  if DSR.Form.PId = 0 then
  begin
    DSR.Form.Tree.Enabled := not (DSR.DataSet.State in [dsInsert, dsEdit]);
  	for i := 1 to FItems.Count - 1 do
    	UpdatePopupState(i);
  end;

  for i := 0 to DSR.EditingCtrls.Count - 1 do
  begin
    SetEnableEditButton(TComponent(DSR.EditingCtrls[i]),
      DSR.DataSource.DataSet.State in [dsInsert, dsEdit]);
  end;
  if DSR.EditFm <> nil then
  begin
    if DSR.DataSet.State in [dsInsert, dsEdit] then
    begin
    	DSR.EditFm.Buttons.CloseButton.Visible := False;
      DSR.EditFm.Buttons.OKButton.Visible := True;
      DSR.EditFm.Buttons.CancelButton.Visible := True;

      if DSR.DataSet.State = dsInsert then
        DSR.EditFm.Caption := Format(rsNewRecordCaption, [DSR.Form.GetRecordCaption])
      else
      	DSR.EditFm.Caption := Format(rsEditingCaption, [DSR.Form.GetRecordCaption]);
    end
    else
    begin
    	DSR.EditFm.Buttons.CloseButton.Visible := True;
      DSR.EditFm.Buttons.OKButton.Visible := False;
      DSR.EditFm.Buttons.CancelButton.Visible := False;

      DSR.EditFm.Caption := Format(rsViewOnlyCaption, [DSR.Form.GetRecordCaption]);
    end;
  end;

  if DSRi = 0 then
  begin
    DoStateChange;
    for i := 0 to DataSetCount - 1 do
      with DataSets[i]^ do
        if Form.OnStateChange <> nil then Form.OnStateChange(Form);
    for i := 0 to QueryCount - 1 do
      with Queries[i]^ do
        if Grid.OnStateChange <> nil then Grid.OnStateChange(Grid);
  end
  else if DSR.Form.OnStateChange <> nil then DSR.Form.OnStateChange(DSR.Form);
end;

procedure TDataSetProcessor.DoStateChange;
begin
  if FOnStateChange <> nil then FOnStateChange(Self);
end;

procedure TDataSetProcessor.CalcDefVals(DSR: TDataSetRec);
var
  i: Integer;
  DF: TExprData;
  K: Variant;
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
	          K:=DF.E.Calc;
  	        if K <> Null then
            begin
    	        Field.Value := GetObjFieldValue(DF.C, K, True);
              KeyValue := K;
            end;
          end;
      end
      else
      	with DSR.DataSet.FieldByName(FieldStr(DF.C)) do
        	if IsNull then Value := DF.E.Calc;
    except
      on E: Exception do
        DSR.Err.AddErrC(DF.C, rsDefaultValue, GetDefaultValue(DF.C), E);
    end;
  end;
end;

function TDataSetProcessor.ShowEditForm(DSRi: Integer): Integer;
var
  pDS: PDataSetRec;
begin
  pDS := GetDataSet(DSRi);
  Result := pdS^.EditFm.ShowForm;
end;

procedure TDataSetProcessor.RefreshDataAndEdit;
var
  Key: Variant;
begin
  Key := MasterSet.Fields[0].Value;
  Refresh;
  if MasterSet.Locate('id', Key, []) then
    Edit;
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
    MessageDlg(rsWarning, Msg, mtWarning, [mbOk], 0)
    //ErrMsg(Msg);
  end
  else
  begin
    ShowField(C);
    P := C.ClientToScreen(Point(20, C.Height - 8));
    FNotif.Text:=Msg;
    FNotif.ShowAtPos(P.x, P.y);
    FTimer.Enabled := True;
    if (C is TWinControl) and (TWinControl(C).CanFocus) then
      TWinControl(C).SetFocus;
  end;
end;

procedure TDataSetProcessor.BuildPivotTables(idx: Integer);
var
  Q: TQueryRec;
  DSR: TDataSetRec;
  i: Integer;
  C: TComponent;
  pQ: PQueryRec;
begin
  if FPrinting or FReCalculate then Exit;

  pQ := PQueryRec(FQueries[idx]);
  Q := pQ^;
  DSR := GetDataSet(Q.DSRi)^;
  for i := 0 to DSR.Form.ComponentCount - 1 do
  begin
    C := DSR.Form.Components[i];
    if C is TdxPivotGrid then
    begin
      with TdxPivotGrid(C) do
        if (Id = Q.Grid.Id) and NeedBuild then Build;
    end
    else if C is TdxChart then
      with TdxChart(C) do
        if (Query = Q.Grid.Id) and NeedBuild then Build;
  end;
end;

{procedure TDataSetProcessor.BuildAllPivotTables(DSRi: Integer);
var
  i: Integer;
  pQ: PQueryRec;
begin
  for i := 0 to QueryCount - 1 do
  begin
    pQ := Queries[i];
    if pQ^.DSRi = DSRi then
      BuildPivotTables(i);
  end;
end;    }

function TDataSetProcessor.CheckHierarchyToLoopbackReferences(DSR: TDataSetRec
  ): Boolean;
var
  FId, RecId: Integer;
  GrpId: LongInt;
  IDs: String;
  C: TdxLookupComboBox;
begin
  Result := True;
  FId := DSR.Form.ParentField;
  if FId = 0 then Exit;
  C := TdxLookupComboBox(FindById(DSR.Form, FId));
  if (C = nil) or (C.SourceTId = 0) or (C.KeyValue = Null) then Exit;

	GrpId := C.KeyValue;
  RecId := DSR.DataSet.FieldByName('id').AsInteger;
  if GrpId = RecId then
  begin
    ShowError(TControl(C), rsHierarchyCantReferItself, DSR);
    Result := False;
  end
  else
  begin
    IDs := SQLSelectIDs(C.SourceTId, IntToStr(RecId));
    if Pos('\' + IntToStr(GrpId) + '\', IDs) > 0 then
    begin
      ShowError(TControl(C), rsHierarchyCantReferChild, DSR);
      Result := False;
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
  List: TList;
begin
  if FSimpleMode and (DSRi = 0) then Exit(True);

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

  List := TList.Create;
  GetTabOrderComponents(Fm, List);

  try

  Result := False;
  // Обязательные поля
  for i := 0 to List.Count - 1 do
  begin
    C := TComponent(List[i]);
    if HasRequired(C) then
    begin
      if (C is TdxCounter) and (DS.State in [dsInsert]) then Continue;
      Fl := GetComponentField(DS, C);
      // В иерархических справочниках допускается пустое поле, даже если
      // установлено свойство "Обязательное".
      if GetRequired(C) and Fl.IsNull and (GetId(C) <> Fm.ParentField) and
        UserMan.CanInput(C) then
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
    end
  end;
  // Маска ввода
  for i := 0 to List.Count - 1 do
  begin
    C := TComponent(List[i]);
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

  finally
    List.Free;
  end;

  Result := CheckHierarchyToLoopbackReferences(DSR);
  if not Result then Exit;

  if DSR.Form.OnValidate <> nil then DSR.Form.OnValidate(DSR.Form, Result);
end;

function TDataSetProcessor.Print(TemplateName: String): Boolean;
var
  Errs, OutputFile: String;
begin
  Result := False;
  {$ifdef windows}
  TemplateName := StringReplace(TemplateName, '/', DirectorySeparator, [rfReplaceAll]);
  {$else}
  TemplateName := StringReplace(TemplateName, '\', DirectorySeparator, [rfReplaceAll]);
  {$endif}

  // Проверка файла
  if not FileExists(TemplateName) then
  begin
    ErrMsg(Format(rsTemplateFileNotFound, [TemplateName]), True, 'Print');
    Exit;
  end;
  //

  try
    OutputFile := '';
    InnerPrint(TemplateName, OutputFile, Errs, True, True);
    if Errs <> '' then ShowErrorsForm(Errs);
    Result := True;
  except
    on E: Exception do
    begin
      ErrMsg(rsPrnError + LineEnding + LineEnding + E.Message, True, 'Print');
      //Errs := '';
    end;
  end;
end;

procedure TDataSetProcessor.InnerPrint(const TemplateName: String;
  var OutName: String; out Errs: String; AOpenFile, ChangeOutName: Boolean);
var
  OldState: TDataSetState;
  Tmp: String;
  Ok: Boolean;
begin
  OldState := MasterSet.State;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  try
    Tmp := ''; Ok := True;
  	FFm.DoPrintEvent(paBeginPrint, TemplateName, '', Tmp, Ok);
  	FPrinting := True;
    ReportToXXX(Self, TemplateName, OutName, Errs, AOpenFile, ChangeOutName);
  finally
    Screen.Cursor := crDefault;
    FPrinting := False;
    if (OldState in [dsInsert, dsEdit]) and (MasterSet.State <> OldState) then InnerEdit(0, False, True, False);
    RepaintAllGrids;
    FFm.DoPrintEvent(paEndPrint, TemplateName, '', Tmp, Ok);

    if not FFm.IsHide then
      RefreshAllData(0);
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
      if pLR^.Control is TdxComboBox then
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

procedure TDataSetProcessor.CalcFields(DSRi: Integer; const FieldName: String;
  Sender: TComponent);
var
  i, fid: Integer;
  V: Variant;
  E: TExpression;
  fn: String;
  C: TComponent;
  DSR: TDataSetRec;
begin
  // Обнаружения зацикливаний
  if FCalcCounter > 100 then
  begin
    FCalcCounter := 0;
    raise Exception.Create(rsLoopDetectedCalc);
  end;
  Inc(FCalcCounter);
  //

  DSR := GetDataSet(DSRi)^;
  for i := 0 to DSR.ExprList.Count - 1 do
  begin
    E := DSR.ExprList[i].E;
    C := DSR.ExprList[i].C;
    fid := GetId(DSR.ExprList[i].C);

    // Чтобы не было зацикливания
    if fid = GetId(Sender) then Continue;

    fn := FieldStr(fid);
    if FieldExists(DSRi, FieldName, GetExpression(C)) then
    try
      V := E.Calc;
      if DSR.DataSet.State in [dsInsert, dsEdit] then
      begin
        if C is TdxLookupComboBox then
          with TdxLookupComboBox(C) do
            if V <> Null then Field.Value:=GetObjFieldValue(C, V, True)
            else Field.SetData(nil);
        DSR.DataSet.FieldByName(fn).Value:=V;
      end;
    except
      on Ex: Exception do
        DSR.Err.AddErrC(C, rsExpression, GetExpression(C), Ex);
    end;
  end;
  Dec(FCalcCounter);
end;

procedure TDataSetProcessor.CalcAggFields(DSRi: Integer; FormName: String);
var
  i, fid: Integer;
  V: Variant;
  E: TExpression;
  fn: String;
  C: TComponent;
  DSR: TDataSetRec;
begin
  DSR := GetDataSet(DSRi)^;
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
      begin
        if C is TdxLookupComboBox then
          with TdxLookupComboBox(C) do
            if V <> Null then Field.Value:=GetObjFieldValue(C, V, True)
            else Field.SetData(nil);
        DSR.DataSet.FieldByName(fn).Value:=V;
      end;
    except
      on Ex: Exception do
        DSR.Err.AddErrC(C, rsExpression, GetExpression(C), Ex);
    end;
  end;
end;

{function TDataSetProcessor.MakeSubFilter: String;
var
  i: Integer;
  S: String;
  Fm: TdxForm;
begin
  Result := '';
  for i := 1 to FItems.Count - 1 do
  begin
    Fm := GetDataSet(i)^.Form;
    S := SqlFormFilter(Fm, GetDataSet(i)^.Filter);
    if S > '' then
    begin
      Result := Result + 'exists (select ' + TableStr(Fm.Id) + '.id from ' +
        TableStr(Fm.Id) + ' where ' + TableStr(Fm.Id) + '.pid=' +
        TableStr(FFm.Id) + '.id and ' + S + ') and ';
    end;
    GetDataSet(i)^.TblFilterSet:=S > '';
  end;
  Result := Copy(Result, 1, Length(Result) - 5);
end;}

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
    QueryName := pQ^.RD.Name;
    for i := 0 to FQueries.Count - 1 do
    begin
      Qry := PQueryRec(FQueries[i])^;
      RD := Qry.RD;
      if RD.QueryExistsInExpr(QueryName) then
      begin
        FQueries.Insert(i, pQ);
        //pQ^.Grid.DSP := Self;
        //pQ^.Grid.QRi:=i;
        Exit;
      end;
    end;
    {i := }FQueries.Add(pQ);
    //pQ^.Grid.DSP := Self;
    //pQ^.Grid.QRi := i;
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
      Popup.Tag := i;
      Grid.DSP := Self;
      Grid.QRi := i;
    end;
end;

function TDataSetProcessor.CreateQuery(QG: TdxQueryGrid; DSRi: Integer
  ): PQueryRec;

  procedure BindPivotTables(Fm: TdxForm; QDS: TDataSet);
  var
    i: Integer;
    C: TComponent;
  begin
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if C is TdxPivotGrid then
        with TdxPivotGrid(C) do
          if Id = QG.Id then DataSet := QDS;
    end;
  end;

var
  pQ: PQueryRec;
  RD: TReportData;
  FmId: Integer;
  CanV, CanA, CanE, CanD: Boolean;
  Bns: TGridButtons;
  DSR: TDataSetRec;
begin
  Result := nil;
  RD := ReportMan.FindReport(QG.Id);
  if RD.IsEmpty then Exit;
  DSR := DataSets[DSRi]^;
  RD := ReportMan.CreateReport(QG.Id);
  New(pQ);
  pQ^.RD := RD;
  pQ^.DataSet := TdxDataSet.Create(nil);
  PQ^.DataSet.PacketRecords:=100;
  pQ^.DataSet.ParseSQL := False;
  // Делаю датасет редактируемым
  pQ^.DataSet.DeleteSQL.Text := 'delete * from rdb$database';
  //
  DBase.AttachDataSet(pQ^.DataSet);
  pQ^.DataSource := TDataSource.Create(nil);
  pQ^.DataSource.DataSet := pQ^.DataSet;
  pQ^.DataSet.BeforeScroll:=@QueryBeforeScroll;
  pQ^.DataSet.AfterScroll:=@QueryAfterScroll;
  pQ^.DataSet.BeforeOpen:=@QueryBeforeOpen;
  pQ^.DataSet.AfterOpen:=@QueryAfterOpen;
  pQ^.DataSet.BeforeClose:=@QueryBeforeClose;
  pQ^.DataSet.AfterClose:=@QueryAfterClose;
  TdxDataSet(pQ^.DataSet).RD := RD;
  pQ^.Grid := QG;
  QG.ReadOnly:=True;
  QG.OnDblClick:=@QueryGridDblClick;
  //QG.Options:= QG.Options - [dgTabs];
  QG.DataSource := pQ^.DataSource;
  InitGrid(QG, RD);
  pQ^.Popup := TPopupMenu.Create(nil);
  pQ^.Popup.Images := Images16;
  pQ^.DSProc := nil;
  pQ^.Simple := RD.IsSimple;
  if pQ^.Simple then
  begin
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsAppend, 0,
      ShortCut(VK_INSERT, []), @QueryMenuHandler, IMG16_ADD) );
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsEdit, 1,
      ShortCut(VK_SPACE, []), @QueryMenuHandler, IMG16_EDIT) );
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsDelete, 2,
      ShortCut(VK_DELETE, [ssCtrl]), @QueryMenuHandler, IMG16_DELETE) );
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, '-', 3, 0, @QueryMenuHandler) );
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsGoTo, 4, 0, @QueryMenuHandler, IMG16_GOTO) );
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, '-', 5, 0, @QueryMenuHandler) );
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsRefresh, 6, 0, @QueryMenuHandler, IMG16_REFRESH) );

    // !!! Доступ
    FmId := RD.GetEditFormId;
    CanV := UserMan.CheckFmVisible(FmId);
    CanA := UserMan.CheckFmAdding(FmId);
    CanE := UserMan.CheckFmEditing(FmId);
    CanD := UserMan.CheckFmDeleting(FmId);
    with pQ^.Popup do
    begin
      Items[0].Visible:=CanV and CanA;
      Items[1].Visible:=CanV;
      Items[2].Visible:=CanV and CanD;
      Items[3].Visible:=CanV and FGotoEnable and (DSR.Form.ViewType <> vtGridOnly);
      Items[4].Visible:=CanV and FGotoEnable and (DSR.Form.ViewType <> vtGridOnly);
      Items[5].Visible:= Items[0].Visible or Items[1].Visible or Items[2].Visible
        or Items[3].Visible;
      if CanE = False then
      begin
        Items[1].ImageIndex := IMG16_EYES;
        Items[1].Caption := rsLook;
      end;
    end;


    if QG.ShowButtons then
    begin
      Bns := pQ^.Grid.Buttons;
      Bns.ShowButton(gbnAppend, CanV and CanA);
      Bns.ShowButton(gbnEdit, CanV);
      Bns.ShowButton(gbnDelete, CanV and CanD);
      Bns.ShowButton(gbnGoto, CanV and FGotoEnable and (DSR.Form.ViewType <> vtGridOnly));
      QG.OnButtonClick:=@QueryGridButtonClick;
    end;
  end
  else
  begin
    pQ^.Popup.Items.Add( CreateMenuItem(pQ^.Popup, rsRefresh, 6, 0, @QueryMenuHandler, IMG16_REFRESH) );
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
  pQ^.DSRi := DSRi;
  QG.OnSortColumnChange:=@QueryGridSortChange;
  //QG.OnDrawColumnCell:=@QueryGridDrawColumnCell;
  QG.OnPrepareCanvas:=@QueryGridPrepareCanvas;
  QG.OnDrawColumnCell:=@QueryGridDrawColumnCell;
  if pQ^.Simple then QG.OnKeyDown:=@QueryGridKeyDown;
  pQ^.Colors := TQueryColorList.Create;
  pQ^.NeedRefresh:=True;
  pQ^.Changed:=False;
  pQ^.ParentChanged:=False;
  BindPivotTables(DSR.Form, pQ^.DataSet);
  Result := pQ;
end;

procedure TDataSetProcessor.RequeryQuery(idx: Integer; Id: Integer;
  ItSelf: Boolean);
var
  Q: TQueryRec;
  RD: TReportData;
  DSR: TDataSetRec;
  pQ: PQueryRec;
begin
  if FInserting then Exit;

  pQ := PQueryRec(FQueries[idx]);
  pQ^.NeedRefresh := False;
  Q := pQ^;

  Q.Colors.Clear;
  RD := Q.RD;
  if RD.IsEmpty then Exit;

  Q.DataSet.Close;
  DSR := GetDataSet(Q.DSRi)^;
  try
    Q.DataSet.SQL.Text:=SqlReportSelect(RD, DSR.Form, GetDataSet(0)^.Form, DSR.DataSet);
  except
    on E: Exception do
    begin
      // Prop и Expr есть в E
      DSR.Err.AddErr(TdxForm(Q.Grid.Owner).FormCaption, RD.Name, rsQuery, '', '', E);
      UpdateQueryPopupState(Q);
      Exit;
    end;
  end;

  // Такое может быть, если это подчиненный запрос и в результате вычисления
  // выражения в фильтре обновился родительский запрос, который в свою очередь
  // обновляет все подчиненные запросы.
  if Q.DataSet.Active then Exit;

  {Q.DataSet.ClearIndexes;
  Q.DataSet.IndexName := '';
  Q.DataSet.MaxIndexesCount := 100;}
  Q.DataSet.IndexFieldNames := '';
  Q.DataSet.Fields.Clear;
  Q.Grid.DisableScrollEvents;
  Q.DataSet.Open;
  //SetQueryDisplayFormat(RD, Q.DataSet);
  CalcQuery(RD, Q.DataSet, DSR.Form, FMaster^.Form, DSR.DataSet, DSR.Err);
  try
    FilterQuery(RD, Q.DataSet, DSR.Form, FMaster^.Form, DSR.DataSet);
  except
    on E: Exception do
      DSR.Err.AddErr(TdxForm(Q.Grid.Owner).FormCaption, RD.Name, rsQuery, rsOutputFilter, RD.Filter, E);
  end;
  if CalcFieldExistsInSort(RD) then
  begin
  	BuildSortIndexes(RD, Q.DataSet);
    Q.DataSet.First;
  end;

  // Перенес из QueryAfterOpen
  if not (FDuplicateFlag or FReCalculate or FPrinting) then
    with Q.Grid do
      if OnAfterOpen <> nil then OnAfterOpen(Q.Grid);
  //

  if not FIsCancel then
    CalcAggFields(Q.DSRi, RD.Name);

  if ItSelf then
  begin
    ClearAggCalcLabels(Q.DSRi, RD.Name);
    SetNeedBuildPivot(idx);
  end;

  if Id > 0 then Q.DataSet.Locate('id', Id, []);
  Q.Grid.EnableScrollEvents;
  if not Q.Grid.ScrollEventsDisabled then
    QueryAfterScroll(Q.DataSet);

  if ItSelf and not GetDataSet(Q.DSRi)^.Form.IsHide then
  begin
    BuildPivotTables(idx);
    CalcExprs(Q.DSRi);
  end;

  UpdateQueryPopupState(Q);
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

procedure TDataSetProcessor.RequeryQueries(aDSRi: Integer);
var
  i: Integer;
  Q: TQueryRec;
begin
  for i := 0 to FQueries.Count - 1 do
  begin
    Q := PQueryRec(FQueries[i])^;
    if Q.DSRi = aDSRi then
      if not Q.Grid.ManualRefresh then
      begin
        if Q.NeedRefresh then RequeryQuery(i);
        BuildPivotTables(i);
      end
      else UpdateQueryPopupState(Q);
  end;
end;

// Принцип работы. TdxLabel имеет свойство Value, куда записывается вычисленное
// значение. Если это свойство неопределено, то выражение вычисляется.
// При этом выражение может быть вычислено в процессе вычисления
// выражения другой надписи. Это позволяет уменьшить количество повторных вычислений.
procedure TDataSetProcessor.CalcExprs(DSRi: Integer);
var
  i: Integer;
  LE: TExprData;
  Lbl: TdxLabel;
  DSR: TDataSetRec;
begin
  if FInserting or FPrinting then Exit;

  // Обнаружения зацикливаний
  if FCalcCounter > 100 then
  begin
    FCalcCounter := 0;
    raise Exception.Create(rsLoopDetectedCalc);
  end;
  Inc(FCalcCounter);
  //

  DSR := GetDataSet(DSRi)^;
  for i := 0 to DSR.LblExprList.Count - 1 do
  begin
    LE := DSR.LblExprList[i];
    Lbl := TdxLabel(LE.C);
    if Lbl.Value = unassigned then
    try
      // Вычисление выражения может спровоцировать зацикливание, если
      // произойдет повторное обращение к надписи из-за того, что в процессе
      // вычисления значение надписи все еще unassigned. Поэтому присваиваем
      // надписи любое значение до вычисления выражения. Глюк этот обнаружил
      // в базе "Школьное питание" (тема на форуме "Составление меню"). Сам
      // же это расширение делал. Раньше все работало, но после изменения
      // алгоритма обновления подчиненных данных и надписей стало зацикливаться.
      // (04.09.2018)
      Lbl.Value := Null;

      Lbl.Value := LE.E.Calc;
      Lbl.Caption := VarToStr(Lbl.Value);
      ClearCalcLabels(DSRi, Lbl.FieldName);
      CalcExprs(DSRi);
    except
      on E: Exception do
      begin
        Lbl.Caption := Lbl.FieldName;
        DSR.Err.AddErrC(Lbl, rsExpression, GetExpression(Lbl), E);
      end;
    end
    else
      Lbl.Caption := VarToStr(Lbl.Value);
  end;
  Dec(FCalcCounter);
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

  if (FCallerObject <> nil) and (FCallerObject is TdxLookupComboBox) and
  	(GetSourceTId(FCallerObject) = TId) and
    (TdxLookupComboBox(FCallerObject).KeyValue <> Null) and
    (TdxLookupComboBox(FCallerObject).KeyValue = RecId) then
  begin
    Fm := TdxForm(TComponent(FCallerObject).Owner);
    if ShowMsg then ErrMsg(Format(rsCantDeleteRecord, [Fm.FormCaption]));
    Exit(False);
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
  if pD^.FilterFm = nil then
  begin
    pD^.FilterFm := TFilterFm.Create(nil);
    pD^.FilterFm.Form := pD^.Form;
  end;
  if pD^.FilterFm.ShowForm(pD^.Filter) = mrOk then
  begin
    pD^.Filter.Save(NewFilter);
    if OldFilter <> NewFilter then
    begin
      pD^.FilterIndex:=-1;
      Refresh;
      FFm.Tree.SoftClearSelection;
    end;
  end;
end;

procedure TDataSetProcessor.ApplyFilter(FilterIndex: Integer);
var
  pD: PDataSetRec;
begin
  pD := DataSets[0];
  pD^.Filter.Load(pD^.Form.Filters.ValueFromIndex[FilterIndex]);
  pD^.FilterIndex:=FilterIndex;
  Refresh;
  FFm.Tree.SoftClearSelection;
end;

procedure TDataSetProcessor.Recalculate(TId, FId: Integer; aExpr: String;
  UpdateObjects: Boolean);
var
  DS0, DS: TDataSet;
  DSR: TDataSetRec;
  i: Integer;
  E: TExpression;
  Fl: TField;
  EB: TExpressionBuilder;
  ED: TExprData;
  C: TComponent;
  LCbx: TdxLookupComboBox;

  procedure SetFieldValue(Value: Variant);
  var
    LCbxFl: TField;
  begin
    if (LCbx <> nil) and UpdateObjects then
    begin
      LCbxFl := DS.FieldByName(FieldStr(FId) + 'l');
      if Value = Null then LCbxFl.SetData(nil)
      else LCbxFl.Value := GetObjFieldValue(LCbx, Value, True);
    end;
    Fl.Value := Value;
  end;

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
    try
      E := EB.Build(aExpr);
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

  C := FindById(DSR.Form, FId);
  if C is TdxLookupComboBox then
    LCbx := TdxLookupComboBox(C)
  else LCbx := nil;

  try
    FReCalculate := True;

    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;

    DS0 := FMaster^.DataSet;
    DS := DSR.DataSet;

    for i := 0 to FItems.Count - 1 do
      GetDataSet(i)^.DataSet.DisableControls;

    if DS0 = DS then
      Fl := DS0.FieldByName(FieldStr(FId));

    DS0.First;
    while not DS0.EOF do
    begin
      DS0.Edit;
      if DS <> DS0 then
      begin
        RequeryDetail(DSR.Form.DSRi);
        Fl := DS.FieldByName(FieldStr(FId));
        DS.First;
        while not DS.Eof do
        begin
          DS.Edit;
          //Fl.Value:=E.Calc;
          SetFieldValue(E.Calc);
          DS.Post;
          DS.Next;
        end;
      end
      else
        SetFieldValue(E.Calc);
        //Fl.Value := E.Calc;
      DS0.Post;
      DS0.Next;

    end;

  finally

    for i := 0 to FItems.Count - 1 do
      GetDataSet(i)^.DataSet.EnableControls;
    FReCalculate:=False;
    DataSetAfterScroll(DS0);
    RepaintAllGrids;

    if aExpr <> '' then FreeAndNil(E);
    Screen.Cursor:=crDefault;
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


// Принудительно сохраняем значение в активное поле перед проверкой ввода или
// перед проверкой на изменение
procedure TDataSetProcessor.ForceChangeFields(DSRi: Integer);
var
  DSR: TDataSetRec;
  Gr: TdxGrid;
  //LR: TLookupRec;
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
  	Exit;
  end;

  if (Gr.Editor <> nil) and (Gr.Editor.Focused) then
  begin
    if Gr.Editor is TdxLookupComboBox then
      TdxLookupComboBox(Gr.Editor).ApplyChanges
    {else if (Gr.Editor is TMaskCellEditor) and
      MaskedTextEmpty(Gr.Editor.Caption, TMaskCellEditor(Gr.Editor).EditMask) then
    begin
      if Gr.SelectedField.Value <> Null then
        Gr.SelectedField.SetData(nil)
    end  }
    else
      if Gr.Editor.Caption <> Gr.SelectedField.Text then
        Gr.SelectedField.Text := Gr.Editor.Caption;
    Gr.EditorMode:=False;
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
    if C is TdxLookupComboBox then TdxLookupComboBox(C).ApplyChanges
    {else if (C is TdxEdit) and (TdxEdit(C).EditMask <> '') and TdxEdit(C).MaskTextEmpty then
    begin
      with DSR.DataSet.FieldByName(FieldStr(C)) do
        if Value <> Null then
          SetData(nil);
    end }
    else if not ((C is TdxCheckBox) or (C is TdxFile)) then
    begin
      with DSR.DataSet.FieldByName(FieldStr(C)) do
        if Text <> TWinControl(C).Caption then
          Text := TWinControl(C).Caption;
    end;
  end;
end;

end.

