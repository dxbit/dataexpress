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

unit DXReports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, DBGrids, dxctrls, Db, strconsts, myctrls, Lists,
  SqlDb, Dialogs, Grids, erroricon, myfpsqltree;

type

  { ESourceFilterError }

  ESourceFilterError = class(Exception)
  private
    FExpr: String;
    FPosition: Integer;
    FSourceNum: Integer;
  public
    constructor Create(const Msg, Expr: String; ASourceNum, APos: Integer);
    property Expr: String read FExpr;
    property SourceNum: Integer read FSourceNum;
    property Position: Integer read FPosition;
  end;

  TRpSourceKind = (skNone, skIncome, skOutcome);
  TRpFieldType = (flNone, flText, flNumber, flDate, flBool, flObject, flTime,
    flCounter, flFile, flRecId);
  TRpFieldTypes = set of TRpFieldType;
  TRpTotalFunc = (tfNone, tfSum, tfAvg, tfMax, tfMin, tfCount, tfProfit, tfDistCount,
    tfMergeAll, tfMerge);

  TRpFieldList = class;

  PRpField = ^TRpField;
  TRpField = record
    TId, FId: Integer;
    Name: String;
    Param, Visible, No, Nul, Zero: Boolean;
    AllZeros: Boolean; // Устанавливается для полей с функцией "Количество", если не указано ни одного поля
    Tp: TRpFieldType;
    Value: String;
    ValueStr: String;     // Текстовое представление параметра
    Parent, Src: PRpField;
    Func: TRpTotalFunc;
    Id: Integer;
  end;

  PRpSource = ^TRpSource;
  TRpSource = record
    Id, TId: Integer;
    Kind: TRpSourceKind;
    Fields: TRpFieldList;
    Filter: String;
  end;

  { TRpFieldList }

  TRpFieldList = class(TList)
  private
    function GetFields(Index: Integer): PRpField;
  public
    function AddField(var F: PRpField): Integer;
    procedure Clear; override;
    function FindField(Id: Integer): PRpField;
    function FindFieldByName(const S: String): PRpField;
    property Fields[Index: Integer]: PRpField read GetFields; default;
  end;

  { TRpSourceList }

  TRpSourceList = class(TList)
  private
    function GetSources(Index: Integer): PRpSource;
  public
    function AddSource(var S: PRpSource): Integer;
    procedure Clear; override;
    property Sources[Index: Integer]: PRpSource read GetSources; default;
  end;

  PRpCalcField = ^TRpCalcField;
  TRpCalcField = record
    Id: Integer;
    Name: String;
    Expr: String;
    Tp: TRpFieldType;
    Size: Integer;
  end;

  { TRpCalcFieldList }

  TRpCalcFieldList = class(TList)
  private
    function GetFields(Index: Integer): PRpCalcField;
  public
    function AddField(var pF: PRpCalcField): Integer;
    procedure Clear; override;
    function FindField(Id: Integer): PRpCalcField;
    function FindFieldByName(const S: String): PRpCalcField;
    function FindFieldByNameDS(const S: String): PRpCalcField;
    property Fields[Index: Integer]: PRpCalcField read GetFields; default;
  end;

  { TSQLField }

  TSQLField = class
  private
    FDisplayFormat: String;
    FFieldNameDS: String;
    FName: String;
    FTp: TRpFieldType;
  public
    procedure CopyFrom(SrcF: TSQLField);
  published
    property FieldNameDS: String read FFieldNameDS write FFieldNameDS;
    property Name: String read FName write FName;
    property Tp: TRpFieldType read FTp write FTp;
    property DisplayFormat: String read FDisplayFormat write FDisplayFormat;
  end;

  { TSQLFieldList }

  TSQLFieldList = class(TList)
  private
    function GetFields(Index: Integer): TSQLField;
  public
    function AddField: TSQLField;
    //function InsertFromDataSetField(AIndex: Integer; F: TField): TSQLField;
    function FindFieldDS(const FieldNameDS: String): TSQLField;
    function FindByName(const FieldName: String): TSQLField;
    procedure DeleteField(AIndex: Integer);
    procedure Clear; override;
    procedure CopyFrom(SourceList: TSQLFieldList);
    property Fields[Index: Integer]: TSQLField read GetFields; default;
  end;

  { TRpGridColumn }

  TRpGridColumn = class
  private
    FAlignment: TAlignment;
    FAutoAlignment: Boolean;
    FAutoLayout: Boolean;
    FCaption: String;
    FColor: TColor;
    FFieldNameDS: String;
    FFixedColor: TColor;
    FFont: TFont;
    FIndex: Integer;
    FLayout: TTextLayout;
    FTitleAlignment: TAlignment;
    FTitleFont: TFont;
    FTitleLayout: TTextLayout;
    FVisible: Boolean;
    FWidth: Integer;
    //function GetFieldId: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    //function IsCalcField: Boolean;
    property Font: TFont read FFont write FFont;
    property Color: TColor read FColor write FColor;
    property TitleFont: TFont read FTitleFont write FTitleFont;
    property FixedColor: TColor read FFixedColor write FFixedColor;
    property Caption: String read FCaption write FCaption;
    property FieldNameDS: String read FFieldNameDS write FFieldNameDS;
    //property FieldId: Integer read GetFieldId;
    property Width: Integer read FWidth write FWidth;
    property Index: Integer read FIndex write FIndex;
    property Visible: Boolean read FVisible write FVisible;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Layout: TTextLayout read FLayout write FLayout;
    property AutoAlignment: Boolean read FAutoAlignment write FAutoAlignment;
    property AutoLayout: Boolean read FAutoLayout write FAutoLayout;
    property TitleAlignment: TAlignment read FTitleAlignment write FTitleAlignment;
    property TitleLayout: TTextLayout read FTitleLayout write FTitleLayout;
  end;

  { TRpGridSortData }

  TRpGridSortData = class
  public
    Col: TRpGridColumn;
    Desc: Boolean;
  end;

  { TRpGridSortList }

  TRpGridSortList = class(TList)
  private
    function GetCols(Index: Integer): TRpGridSortData;
  public
    function AddCol(Col: TRpGridColumn; aDesc: Boolean): TRpGridSortData;
    function FindCol(Col: TRpGridColumn): TRpGridSortData;
    procedure RemoveCol(CD: TRpGridSortData);
    procedure Clear; override;
    property Cols[Index: Integer]: TRpGridSortData read GetCols; default;
  end;

  { TRpGrid }

  TRpGrid = class
  private
    FAllowChangeSort: Boolean;
    FAlternateColor: TColor;
    FCellEllipsis: Boolean;
    FColMove: Boolean;
    FColor: TColor;
    FDefaultRowHeight: Integer;
    FFixedColor: TColor;
    //FFixedHotColor: TColor;
    FFlat: Boolean;
    FFont: TFont;
    FGridLineColor: TColor;
    FGridLineStyle: TPenStyle;
    FHorzLines: Boolean;
    FInactiveSelectedColor: TColor;
    FInactiveSelectedTextColor: TColor;
    FIndicator: Boolean;
    FRowHighlight: Boolean;
    FRowSelect: Boolean;
    FSelectedColor: TColor;
    FSelectedTextColor: TColor;
    FShowHints: Boolean;
    FShowRowDeleteButton: Boolean;
    FSortCols: TRpGridSortList;
    FThumbTracking: Boolean;
    FTitleFont: TFont;
    FVertLines: Boolean;
    FColumns: TList;
    FWordWrap: Boolean;
    function GetColumns(Index: Integer): TRpGridColumn;
  public
    constructor Create;
    destructor Destroy; override;
    function ColumnCount: Integer;
    function AddColumn: TRpGridColumn;
    function AddDefaultColumn: TRpGridColumn;
    function FindColumnByFieldName(const FieldName: String): TRpGridColumn;
    function FindColumnByTitle(const S: String): TRpGridColumn;
    function FindColumnIndex(Col: TRpGridColumn): Integer;
    procedure DeleteColumn(Col: TRpGridColumn);
    procedure SortColumns(L: TList);
    procedure ClearColumns;
    property Color: TColor read FColor write FColor;
    property AlternateColor: TColor read FAlternateColor write FAlternateColor;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor;
    property SelectedTextColor: TColor read FSelectedTextColor write
      FSelectedTextColor;
    property InactiveSelectedColor: TColor read FInactiveSelectedColor write
    	FInactiveSelectedColor;
    property InactiveSelectedTextColor: TColor read FInactiveSelectedTextColor write
      FInactiveSelectedTextColor;
    property FixedColor: TColor read FFixedColor write FFixedColor;
    //property FixedHotColor: TColor read FFixedHotColor write FFixedHotColor;
    property GridLineColor: TColor read FGridLineColor write FGridLineColor;
    property GridLineStyle: TPenStyle read FGridLineStyle write FGridLineStyle;
    property Font: TFont read FFont write FFont;
    property TitleFont: TFont read FTitleFont write FTitleFont;
    property DefaultRowHeight: Integer read FDefaultRowHeight write
      FDefaultRowHeight;
    property VertLines: Boolean read FVertLines write FVertLines;
    property HorzLines: Boolean read FHorzLines write FHorzLines;
    property Flat: Boolean read FFlat write FFlat;
    property Columns[Index: Integer]: TRpGridColumn read GetColumns;
    property SortCols: TRpGridSortList read FSortCols;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property RowSelect: Boolean read FRowSelect write FRowSelect;
    property CellEllipsis: Boolean read FCellEllipsis write FCellEllipsis;
    property ShowHints: Boolean read FShowHints write FShowHints;
    property ThumbTracking: Boolean read FThumbTracking write FThumbTracking;
    property AllowChangeSort: Boolean read FAllowChangeSort write FAllowChangeSort;
    property ColMove: Boolean read FColMove write FColMove;
    property RowHighlight: Boolean read FRowHighlight write FRowHighlight;
    property Indicator: Boolean read FIndicator write FIndicator;
    property ShowRowDeleteButton: Boolean read FShowRowDeleteButton write FShowRowDeleteButton;
  end;


  TRpDateDetail = (ddDay, ddWeek, ddMonth, ddQuart, ddHalfYear, ddYear);
  TReportKind = (rkReport, rkQuery);

  { TRpTotalData }

  TRpTotalData = class
  public
    Caption: String;
    FieldNameDS: String;
    Func: TRpTotalFunc;
    Value: String;      // для печати в шаблонах
  end;

  { TRpTotalList }

  TRpTotalList = class(TList)
  private
    function GetTotals(Index: Integer): TRpTotalData;
  public
    function AddTotal: TRpTotalData;
    function FindTotal(const AFieldNameDS: String): TRpTotalData;
    procedure RemoveTotal(T: TRpTotalData);
    procedure Clear; override;
    property Totals[Index: Integer]: TRpTotalData read GetTotals; default;
  end;

  { TRpColoringData }

  TRpColoringData = class
  public
    Color: TColor;
    FieldNameDS: String;
    Expr: String;
  end;

  { TRpColoringList }

  TRpColoringList = class(TList)
  private
    function GetColorings(Index: Integer): TRpColoringData;
  public
    function AddColoring: TRpColoringData;
    function FindColoring(const FieldName: String): TRpColoringData;
    function FindColoringIndex(AColor: TColor; const AFieldNameDS, Expr: String): Integer;
    procedure DeleteColoring(CD: TRpColoringData);
    procedure Clear; override;
    property Colorings[Index: Integer]: TRpColoringData read GetColorings; default;
  end;

  { TReportData }

  TReportData = class
  private
    FCalcFields: TRpCalcFieldList;
    FColoring: TRpColoringList;
    FDateDetail: TRpDateDetail;
    FDateField: Integer;
    FFilter: String;
    FFirstRecordCount: Integer;
    FGrid: TRpGrid;
    FHelpText: String;
    FId: Integer;
    FName: String;
    FKind: TReportKind;
    FPrintFields: TStringList;
    //FPivotGrid: TObject;
    FSortOrder: String;
    FSources: TRpSourceList;
    FSQL: String;
    FSqlMode: Boolean;
    FTemplates: TStringList;
    FTotals: TRpTotalList;
    FVersion: Integer;
    FSQLFields: TSQLFieldList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(St: TStream);
    procedure LoadFromStream(St: TStream);
    procedure Clear;
    function FindField(aId: Integer): PRpField;
    function FindFieldByName(const aName: String): PRpField;

    function TryGetRpField(AIndex: Integer): PRpField;
    function TryGetCalcField(AIndex: Integer): PRpCalcField;
    function TryGetSQLField(AIndex: Integer): TSQLField;
    function GetFieldCount: Integer;
    function GetRpSQLFieldCount: Integer;
    function IsEmpty: Boolean;
    procedure CheckFieldIndex(AIndex: Integer);
    function GetFieldNameDS(AIndex: Integer): String;
    function GetFieldName(AIndex: Integer): String;
    function GetFieldType(AIndex: Integer): TRpFieldType;
    function GetFieldVisible(AIndex: Integer): Boolean;
    function GetFieldParam(AIndex: Integer): Boolean;
    function GetDisplayFormat(AIndex: Integer): String;
    function IsCalcField(AIndex: Integer): Boolean;
    function IndexOfName(AFieldName: String): Integer;
    function IndexOfNameDS(AFieldNameDS: String): Integer;
    procedure GetSourceForms(AFormList: TStrings);
    function QueryExistsInExpr(AQueryName: String): Boolean;
    function FieldExistsInExpr(DSRi: Integer; AFieldName: String): Boolean;
    function GetEditFormId: Integer;
    function GetSourceFilter: String;
    function IsSimple: Boolean;
    function HasParentIdField: Boolean;

    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
    property Sources: TRpSourceList read FSources;
    property DateField: Integer read FDateField write FDateField;
    property DateDetail: TRpDateDetail read FDateDetail write FDateDetail;
    property SortOrder: String read FSortOrder write FSortOrder;
    property Grid: TRpGrid read FGrid;
    //property PivotGrid: TObject read FPivotGrid;
    property Kind: TReportKind read FKind write FKind;
    property Filter: String read FFilter write FFilter;
    property CalcFields: TRpCalcFieldList read FCalcFields;
    property Totals: TRpTotalList read FTotals;
    property Coloring: TRpColoringList read FColoring;
    property HelpText: String read FHelpText write FHelpText;
    property Version: Integer read FVersion write FVersion;
    property Templates: TStringList read FTemplates;
    property PrintFields: TStringList read FPrintFields;
    property FirstRecordCount: Integer read FFirstRecordCount write FFirstRecordCount;
    property SqlMode: Boolean read FSqlMode write FSqlMode;
    property SqlFields: TSQLFieldList read FSqlFields;
    property SQL: String read FSQL write FSQL;
  end;

  //TdxQueryLinkType = (lkNone, lkForm, lkField);

  { TdxQueryGrid }

  TdxQueryGrid = class(TMyDBGrid)
  private
    FDSP: TObject;
    FId: Integer;
    //FLinkType: TdxQueryLinkType;
    FManualRefresh: Boolean;
    FOnAfterClose: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnAfterScroll: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnBeforeOpen: TNotifyEvent;
    FOnBeforeScroll: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FOnCreateForm: TCreateFormEvent;
    //FOnPrintFieldEvent: TPrintFieldEvent;
    //FParams: String;
    FQRi: Integer;
    FRpWnd: TObject;
    FScrollEventsCounter: Integer;
    FOldAfterScroll, FOldBeforeScroll: TDataSetNotifyEvent;
    function GetEditable: Boolean;
    function GetFields(aName: String): Variant;
    function GetAsDT(Index: String): TDateTime;
    function GetAsF(Index: String): Extended;
    function GetAsI(Index: String): Integer;
    function GetAsS(Index: String): String;
    function GetQueryName: String;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MoveBy(Distance: Integer);
    procedure MoveFirst;
    procedure MovePrior;
    procedure MoveNext;
    procedure MoveLast;
    procedure MoveTo(aRecNo: Integer);
    function EOF: Boolean;
    function BOF: Boolean;
    function RecNo: Integer;
    function RecId: Integer;
    procedure EnableControls;
    procedure DisableControls;
    function ControlsDisabled: Boolean;
    function RecordCount: Integer;
    function Locate(const FieldNames: String; FieldValues: array of Variant; aOptions: TLocateOptions): Boolean;
    function GotoRecord(aRecId: Integer): Boolean;
    procedure Refresh;
    procedure Close;
    procedure DisableScrollEvents;
    procedure EnableScrollEvents;
    procedure RequeryIfNeed;
    function ScrollEventsDisabled: Boolean;
    procedure SortColsToRpGridSortCols;
    property DSP: TObject read FDSP write FDSP;
    property QRi: Integer read FQRi write FQRi;
    property RpWnd: TObject read FRpWnd write FRpWnd;
    property QueryName: String read GetQueryName;
    property Fields[aName: String]: Variant read GetFields; default;
    property AsI[Index: String]: Integer read GetAsI;
    property AsF[Index: String]: Extended read GetAsF;
    property AsDT[Index: String]: TDateTime read GetAsDT;
    property AsS[Index: String]: String read GetAsS;
    property OnCreateForm: TCreateFormEvent read FOnCreateForm write FOnCreateForm;
  published
    property Id: Integer read FId write FId;

    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnAfterScroll: TNotifyEvent read FOnAfterScroll write fOnAfterScroll;

    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnBeforeScroll: TNotifyEvent read FOnBeforeScroll write FOnBeforeScroll;

    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;

    //property OnPrintField: TPrintFieldEvent read FOnPrintFieldEvent write FOnPrintFieldEvent;

    //property LinkType: TdxQueryLinkType read FLinkType write FLinkType stored False;
    //property Params: String read FParams write FParams stored False;
    property ManualRefresh: Boolean read FManualRefresh write FManualRefresh;
    property Editable: Boolean read GetEditable;
  end;

  { TdxQField }

  {TdxQField = class
  private
    FpF: PRpField;
    FFieldName: String;
    function GetFieldType: TRpFieldType;
    function GetFunc: TRpTotalFunc;
    function GetName: String;
  public
    property FieldName: String read FFieldName write FFieldName;
    property Name: String read GetName;
    property Func: TRpTotalFunc read GetFunc;
    property FieldType: TRpFieldType read GetFieldType;
  end;

  { TdxQSource }

  TdxQSource = class
  private
    FpSr: PRpSource;
    FFields: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddField(const aFieldName, aName: String; aFunc: TRpTotalFunc): TdxQField;
  end;  }

  {TdxQuery = class
  private
    FSources: TList;
    FRD: TReportData;
    FDataSet: TSQLQuery;
    function GetAsDT(Index: String): TDateTime;
    function GetAsF(Index: String): Extended;
    function GetAsI(Index: String): Integer;
    function GetAsS(Index: String): String;
    function GetFields(aName: String): Variant;
    procedure SetZeroFieldType;
  public
    constructor Create;
    destructor Destroy; override;
    function AddSource(const aFormName, aSubFormName, aFilter: String; aKind: TRpSourceKind): TdxQSource;
    procedure GroupByDate(const aDateField: String; aDateDetail: TRpDateDetail);
    procedure AddSorting(const aName: String; Desc: Boolean);
    procedure Open;
    procedure Close;
    function Opened: Boolean;
    procedure MoveBy(Distance: Integer);
    procedure MoveFirst;
    procedure MovePrior;
    procedure MoveNext;
    procedure MoveLast;
    procedure MoveTo(aRecNo: Integer);
    function BOF: Boolean;
    function EOF: Boolean;
    function RecNo: Integer;
    function RecId: Integer;
    function RecordCount: Integer;
    property Fields[Name: String]: Variant read GetFields; default;
    property AsI[Index: String]: Integer read GetAsI;
    property AsF[Index: String]: Extended read GetAsF;
    property AsDT[Index: String]: TDateTime read GetAsDT;
    property AsS[Index: String]: String read GetAsS;
    property DataSet: TSQLQuery read FDataSet;
    property RD: TReportData read FRD;
  end;  }

function NewRpField: PRpField;
function GetTopField(Fl: PRpField): PRpField;
function GetLowField(Fl: PRpField): PRpField;
procedure InitGrid(Grid: TMyDBGrid; RD: TReportData);
function GetTypeByComponent(C: TComponent): TRpFieldType;
function RpFieldTypeToStr(Tp: TRpFieldType): String;
//procedure FillFields(aFm, aTbl: TdxForm; L: TStrings; Types: TRpFieldTypes);
procedure SetupRpField(C: TComponent; FlNm: String; pFld: PRpField);
function GetRpFieldComponent(F: TRpField; aLow: Boolean): TComponent;
function GetFullFieldName(F: TRpField): String;
procedure SetQueryDisplayFormat(RD: TReportData; DS: TDataSet);
function SourceKindToStr(sk: TRpSourceKind): String;
function TotalFuncToStr(tf: TRpTotalFunc): String;
//function IsSimpleReport(RD: TReportData): Boolean;
function CalcFieldExistsInSort(RD: TReportData): Boolean;
//function SortFieldsToStr(RD: TReportData): String;
function SqlReportSelect(RD: TReportData; Fm, PFm: TdxForm; DS: TDataSet): String;
//procedure AddCalcFields(RD: TReportData; DS: TDataSet);
procedure CalcQuery(aRD: TReportData; DS: TDataSet; aForm, aParForm: TdxForm; FormDS: TDataSet; Errs: TCalcError);
procedure FilterQuery(aRD: TReportData; DS: TDataSet; aForm, aParForm: TdxForm; FormDS: TDataSet);
procedure BuildSortIndexes(RD: TReportData; DataSet: TSQLQuery);
//function GetRealRpFieldType(RD: TReportData; Fl: PRpField): TRpFieldType;
function CreateReportForm(RD: TReportData; out SQL: String): TdxForm;
function RpFieldIsCheckBox(RD: TReportData; const FieldNameDS: String): Boolean;
procedure RemoveLostFieldsFromReportData(RD: TReportData);
procedure CreateOrUpdateReportGridColumns(RD: TReportData);

implementation

uses
  apputils, SAX, saxbasereader, LazUtf8, formmanager, sqlgen, expressions, strutils,
  Variants, DateUtils, reportmanager, datasetprocessor, reportwindow,
  scriptfuncs, dxfiles, dximages, dbengine, dxsqlquery;

type

  { TRpReader }

  TRpReader = class(TSaxBaseReader)
  private
    FSrc: PRpSource;
    FParent: PRpField;
    FColumn: TRpGridColumn;
    function GetColor(Atts: TSAXAttributes; const aName: String; DefaultColor: TColor): TColor;
    function GetPenStyle(Atts: TSAXAttributes; const aName: String): TPenStyle;
    procedure ReadSortCols;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
    procedure DoEndElement(const NamespaceURI, LocalName, QName: SAXString); override;
  public
    RD: TReportData;
  end;

  { TSQLReportTotalsFilter }

  {TSQLReportTotalsFilter = class(TSQLFilterParser)
  private
    FRD: TReportData;
  protected
    function FieldNameParse(const aFieldName: String): String; override;
    function CheckValue(var Value: String): Boolean; override;
  public
    property RD: TReportData read FRD write FRD;
  end; }

function GetJoinTypeByRpField(Fl: TRpField): String;
var
  Fm: TdxForm;
  PC, C: TComponent;
begin
  Fm := FormMan.FindForm(Fl.TId);
  C := FindById(Fm, Fl.FId);
  PC := C;
  if Fl.Parent <> nil then
  begin
    Fl := Fl.Parent^;
    Fm := FormMan.FindForm(Fl.TId);
    PC := FindById(Fm, Fl.FId);
  end;
  Result := GetJoinType(PC, C);
end;

function NewRpField: PRpField;
begin
  New(Result);
  FillChar(Result^, SizeOf(TRpField), 0);
end;

function GetTopField(Fl: PRpField): PRpField;
begin
  if Fl^.Parent <> nil then Result := GetTopField(Fl^.Parent)
  else Result := Fl;
end;

function GetLowField(Fl: PRpField): PRpField;
begin
  if Fl^.Src <> nil then Result := GetLowField(Fl^.Src)
  else Result := Fl;
end;

procedure InitGrid(Grid: TMyDBGrid; RD: TReportData);
var
  L: TList;
  i, n: Integer;
  C: TRpGridColumn;
  Col: TMyDBGridColumn;
  G: TRpGrid;
  L1: TRpGridSortList;
  L2: TSortColumns;
begin
  L := TList.Create;
  G := RD.Grid;
  G.SortColumns(L);
  Grid.Columns.Clear;
  for i := 0 to L.Count - 1 do
  begin
    C := TRpGridColumn(L[i]);
    Col := Grid.Columns.Add;
    Col.Title.Caption := C.Caption;
    Col.Width:=C.Width;
    Col.FieldName:=C.FieldNameDS;
    Col.Color:=C.Color;
    Col.Font := C.Font;
    Col.Title.Color:=C.FixedColor;
    Col.Title.Font := C.TitleFont;
    if not C.AutoAlignment then
    begin
      Col.Alignment := C.Alignment;
      Col.AutoAlignment:=C.AutoAlignment;
    end;
    if not C.AutoLayout then
    begin
      Col.Layout := C.Layout;
      Col.AutoLayout := C.AutoLayout;
    end;
    Col.Visible:=C.Visible;
    Col.Title.Alignment:=C.TitleAlignment;
    Col.Title.Layout:=C.TitleLayout;
    if {not C.IsCalcField and }RpFieldIsCheckBox(RD, C.FieldNameDS) then
      Col.ButtonStyle := cbsCheckboxColumn;
  end;
  L.Free;
  Grid.Color := G.Color;
  Grid.AlternateColor:=G.AlternateColor;
  Grid.FixedColor:=G.FixedColor;
  //Grid.FixedHotColor := G.FixedHotColor;
  Grid.SelectedColor:=G.SelectedColor;
  Grid.SelectedTextColor:=G.SelectedTextColor;
  Grid.InactiveSelectedColor:=G.InactiveSelectedColor;
  Grid.InactiveSelectedTextColor:=G.InactiveSelectedTextColor;
  Grid.FocusColor:=G.SelectedColor;
  Grid.GridLineColor:=G.GridLineColor;
  Grid.GridLineStyle:=G.GridLineStyle;
  Grid.DefaultRowHeight:=G.DefaultRowHeight;
  Grid.Flat:=G.Flat;
  Grid.Options := [dgColumnResize, dgTitles, {dgIndicator, }dgAlwaysShowSelection,
  	dgAnyButtonCanSelect, dgDisableInsert, dgDisableDelete,
    dgHeaderPushedLook, dgHeaderHotTracking, dgDisplayMemoText];

  if G.VertLines then Grid.Options := Grid.Options + [dgColLines];
  //else Grid.Options := Grid.Options - [dgColLines];
  if G.HorzLines then Grid.Options := Grid.Options + [dgRowLines];
  //else Grid.Options := Grid.Options - [dgRowLines];
  Grid.Font := G.Font;
  Grid.TitleFont := G.TitleFont;
  Grid.WordWrap:=G.WordWrap;
  if G.RowSelect then Grid.Options := Grid.Options + [dgRowSelect];
  if G.RowHighlight then Grid.Options := Grid.Options + [dgRowHighlight];
  if G.CellEllipsis then Grid.Options := Grid.Options + [dgCellEllipsis];
  if G.ShowHints then Grid.Options := Grid.Options + [dgTruncCellHints];
  if G.ThumbTracking then Grid.Options := Grid.Options + [dgThumbTracking];
  if G.ColMove then Grid.Options := Grid.Options + [dgColumnMove];
  Grid.AllowChangeSort := G.AllowChangeSort;
  if G.Indicator then Grid.Options := Grid.Options + [dgIndicator];

  L1 := G.SortCols;
  L2 := Grid.SortCols;
  L2.Clear;
  for i := 0 to L1.Count - 1 do
  begin
    n := L1[i].Col.Index;
    if (n >= 0) and (n < Grid.Columns.Count) then
      L2.AddCol(Grid.Columns[n], L1[i].Desc);
  end;
  Grid.ReadOnly:=True;
end;

function GetTypeByComponent(C: TComponent): TRpFieldType;
begin
  Result := flNone;
  if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) then
    Result := flText
  else if C is TdxCalcEdit then
    Result := flNumber
  else if C is TdxDateEdit then
    Result := flDate
  else if C is TdxCheckBox then
    Result := flBool
  else if C is TdxLookupComboBox then
    Result := flObject
  else if C is TdxTimeEdit then
    Result := flTime
  else if C is TdxCounter then
    Result := flCounter
  else if C is TdxFile then
    Result := flFile
  else if C is TdxDBImage then
    Result := flText
  else if C is TdxRecordId then
    Result := flRecId
end;

//(flNone, flText, flNumber, flDate, flBool, flObject, flTime, flCounter);
function RpFieldTypeToStr(Tp: TRpFieldType): String;
const
  TpS: array [TRpFieldType] of String = ('', rsText, rsNumber, rsDate,
  	rsCheckBox, rsObject, rsTime, rsCounter, rsFile, rsRecordId);
begin
  Result := TpS[Tp];
end;

{procedure FillFields(aFm, aTbl: TdxForm; L: TStrings; Types: TRpFieldTypes);
var
  n: Integer;
  Objs: TList;          // Решает проблему зацикливания, ограничивая глубину рекурсии
  SL: TUtf8StringList;

  procedure _FillFields(Fm: TdxForm; LL: TStrings); forward;

  procedure _FillObjectFields(C: TComponent; LL: TStrings);
  var
    Fm: TdxForm;
    n, i: Integer;
    FNm: String;
  begin
    if Objs.IndexOf(C) >= 0 then Exit;
    Objs.Add(C);
    Fm := FormMan.FindForm(GetSourceTId(C));
    if Fm = nil then Exit;
    n := LL.Count;
    _FillFields(Fm, LL);
    FNm := GetFieldName(C);
    for i := n to LL.Count - 1 do
    begin
      LL[i] := FNm + '|' + LL[i];
      LL.Objects[i] := C;
    end;
    Objs.Remove(C);
  end;

  procedure _FillFields(Fm: TdxForm; LL: TStrings);
  var
    i: Integer;
    C: TComponent;
    Tp: TRpFieldType;
    FlNm: String;
  begin
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      Tp := GetTypeByComponent(C);
      if Tp in Types then
      begin
        FlNm := GetFieldName(C);
        LL.AddObject(FlNm, C);
        if Tp = flObject then
          _FillObjectFields(C, LL);
      end;
    end;
  end;

begin
  Objs := TList.Create;
  SL := TUtf8StringList.Create;
  L.Clear;
  if aFm = nil then Exit;
  _FillFields(aFm, SL);
  SL.Sort;
  L.AddStrings(SL);
  if aTbl <> nil then
  begin
    L.AddObject('--------------------', nil);
    n := L.Count;
    SL.Clear;
    _FillFields(aTbl, SL);
    SL.Sort;
    L.AddStrings(SL);
    if n = L.Count then L.Delete(L.Count - 1);
  end;
  Objs.Free;
  SL.Free;
end;   }

procedure SetupRpField(C: TComponent; FlNm: String; pFld: PRpField);
var
  L: TStringList;
  i: Integer;
  Fm: TdxForm;
begin
  L := TStringList.Create;
  SplitStr(FlNm, '|', L);   // Все поля - объекты, кроме последнего
  for i := 0 to L.Count - 1 do
  begin
    Fm := TdxForm(C.Owner);
    pFld^.TId:=Fm.Id;
    pFld^.FId:=GetId(C);
    pFld^.Tp := GetTypeByComponent(C);
    if (pFld^.Tp = flObject) and (i < L.Count - 1) then
    begin
      pFld^.Src:=NewRpField;
      pFld^.Src^.Parent := pFld;
      pFld := pFld^.Src;
      Fm := FormMan.FindForm(GetSourceTId(C));
      if Fm <> nil then
        C := FindComponentByFieldName(Fm, L[i + 1]);
      if (Fm = nil) or (C = nil) then
      begin
        Dispose(pFld^.Src);
        pFld^.Src := nil;
        Break;
      end;
    end;
  end;
  L.Free;
end;

function GetRpFieldComponent(F: TRpField; aLow: Boolean): TComponent;
var
  Fm: TdxForm;
begin
  Result := nil;
  if aLow then F := GetLowField(@F)^;
  if F.TId = 0 then Exit;
  Fm := FormMan.FindForm(F.TId);
  Result := FindById(Fm, F.FId);
end;

function GetFullFieldName(F: TRpField): String;
var
  C: TComponent;
begin
  C := GetRpFieldComponent(F, False);
  TestNil(C, 'GetFullFieldName: C = nil');
  Result := GetFieldName(C);
  if F.Src <> nil then
    Result := Result + '|' + GetFullFieldName(F.Src^);
end;

{function FindSrcField(RD: TReportData; idx: Integer): PRpField;
var
  i: Integer;
  Sr: TRpSource;
  pFl: PRpField;
begin
  Result := nil;
  for i := 0 to RD.Sources.Count - 1 do
  begin
    Sr := RD.Sources[i]^;
    if idx < Sr.Fields.Count then
    begin
      pFl := Sr.Fields[idx];
      if not pFl^.Zero then
        Exit(pFl);
    end;
  end;
end;   }

procedure SetQueryDisplayFormat(RD: TReportData; DS: TDataSet);
var
  //L: TRpFieldList;
  i: Integer;
  {F, LowF: TRpField;
  Fm: TdxForm;
  C: TComponent; }
  Fl: TField;
  //pF: PRpField;
  //CF: TRpCalcField;
  //S: String;
begin
  for i := 0 to RD.GetFieldCount - 1 do
  begin
    if not RD.GetFieldVisible(i) then Continue;
    Fl := DS.FieldByName( RD.GetFieldNameDS(i) );
    if Fl is TNumericField then
      TNumericField(Fl).DisplayFormat := RD.GetDisplayFormat(i)
    else if Fl is TDateTimeField then
      TDateTimeField(Fl).DisplayFormat := RD.GetDisplayFormat(i);
  end;

  {L := RD.Sources[0]^.Fields;
  for i := 0 to L.Count - 1 do
  begin
    F := PRpField(L[i])^;
    if (F.Visible = False) or (F.Tp = flNone) then Continue;
    if F.Zero then
    begin
      pF := FindSrcField(RD, i);  // Почти то же самое, что и RD.FindField
      // Так может быть, если ни одного поля не выбрано, а итоговая ф-ция Count
      if pF = nil then Continue;
      F := pF^;
    end;
    LowF := GetLowField(@F)^;
    Fm := FormMan.FindForm(LowF.TId);
    C := FindById(Fm, LowF.FId);
    Fl := DS.FieldByName('f' + IntToStr(F.Id));
    if (C is TdxCalcEdit) and (not (F.Func in [tfMerge, tfMergeAll, tfCount, tfDistCount])) then
      TNumericField(Fl).DisplayFormat := GetPrecStr(C)
    else if (C is TdxTimeEdit) and (F.Func = tfNone) then
      TTimeField(Fl).DisplayFormat:=TdxTimeEdit(C).TimeFormatStr;
  end;
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i]^;
    if CF.Tp = flNumber then
    begin
      S := ',0';
      if CF.Size > 0 then S := ',0.' + DupeString('0', CF.Size);
      TNumericField(DS.FieldByName('cf' + IntToStr(CF.Id))).DisplayFormat:=S;
    end
    else if CF.Tp = flTime then
    	TTimeField(DS.FieldByName('cf' + IntToStr(CF.Id))).DisplayFormat:='hh:mm:ss';
  end;   }
end;

function CheckNumber(const S: String): String;
begin
  Result := StringReplace(S, DefaultFormatSettings.DecimalSeparator, '.', []);
end;

function SqlSourceFilter(Src: TRpSource; Fm, PFm: TdxForm; DS: TDataSet;
  AliasSL: TStrings; var JoinStr: String): String;
var
  EB: TExpressionBuilder;
  P: TSQLSourceFilterParser;
begin
  EB := TExpressionBuilder.Create;
  EB.Form := Fm;
  EB.ParentForm := PFm;
  EB.DataSet := DS;
  EB.SkipLabels:=True;
  P := TSQLSourceFilterParser.Create;
  P.ExprBuilder := EB;
  P.ParentForm := FormMan.FindForm(Src.Id);
  if Src.TId > 0 then
    P.Form := FormMan.FindForm(Src.TId);
  P.AliasSL := AliasSL;
  P.JoinStr:=JoinStr;
  try
    Result := P.Parse(Src.Filter);
    JoinStr := P.JoinStr;
  finally
    P.Free;
    EB.Free;
  end;
end;

function SqlSourceSelect2(Src: TRpSource; Fm, PFm: TdxForm; DS: TDataSet): String;
var
  FStr, JStr, Flt: String;
  AliasSL: TStringList;
  i: Integer;
  Fl: TRpField;

  function GetAliasName(const Fl: TRpField): String;
  begin
    if Fl.Parent = nil then
      Result := TableStr(Fl.TId)
    else
      Result := GetAliasName(Fl.Parent^) + '_' + FieldStr(Fl.Parent^.FId) +
      	'_' + TableStr(Fl.TId);
  end;

  function GetAliasOrTblNm(const Fl: TRpField): String;
  begin
    Result := GetAliasName(Fl);
    if Fl.Parent <> nil then
      Result := AliasStr(AliasSL, Result);
  end;

  procedure ProcessJoin(Fl: TRpField);
  var
    S, Tmp, AliasNm, ParentAliasNm: String;
  begin
    S := GetAliasName(Fl);
    if AliasSL.IndexOf(S) < 0 then
    begin
      AliasSL.Add(S);
      AliasNm := AliasStr(AliasSL, S);
      ParentAliasNm := GetAliasOrTblNm(Fl.Parent^);

      Tmp := SqlSelectGroups(Fl.TId, True);
      if Tmp <> '' then Tmp := '(' + Tmp + ')'
      else Tmp := TableStr(Fl.TId);
      JStr := JStr + GetJoinTypeByRpField(Fl.Parent^) + Tmp + ' ' + AliasNm +
        ' on ' + ParentAliasNm + '.' + FieldStr(Fl.Parent^.FId) + '=' + AliasNm + '.id';
    end;
  end;

  procedure ProcessField(const TopFl, Fl: TRpField);
  var
    TblNm, FlNm: String;
  begin
    if Fl.Parent <> nil then ProcessJoin(Fl);

    if (Fl.Tp = flObject) and (Fl.Src <> nil) then
      ProcessField(TopFl, Fl.Src^)
    else
    begin
      TblNm := GetAliasOrTblNm(Fl); // AliasStr(AliasSL, GetAliasName(Fl));
      if TopFl.Func = tfProfit then
      begin
        if Src.Kind = skIncome then
          FStr := FStr + TblNm + '.' + FieldStr(Fl.FId) + ' as income' + IntToStr(TopFl.Id) +
            ',0 as outcome' + IntToStr(TopFl.Id) + ','
        else
          FStr := FStr + '0 as income' + IntToStr(TopFl.Id) + ',' + TblNm + '.' + FieldStr(Fl.FId) +
            ' as outcome' + IntToStr(TopFl.Id) + ',';
      end
      else
      begin
        FlNm := FieldStr(Fl.FId);
        if Fl.Tp = flFile then FlNm := FlNm + 'd'
        else if Fl.Tp = flRecId then FlNm := 'id';
        FStr := FStr + TblNm + '.' + FlNm;
        FStr := FStr + ' as ' + FieldStr(TopFl.Id) + ',';
      end;
    end;
  end;

  {procedure ProcessSumField(Fl: TRpField);
  begin
    if Src.Kind = skIncome then
      FStr := FStr + TableStr(Fl.TId) + '.' + FieldStr(Fl.FId) + ' as income' + IntToStr(Fl.Id) +
        ',0 as outcome' + IntToStr(Fl.Id) + ','
    else
      FStr := FStr + '0 as income' + IntToStr(Fl.Id) + ',' + TableStr(Fl.TId) + '.' + FieldStr(Fl.FId) +
        ' as outcome' + IntToStr(Fl.Id) + ',';
  end;  }

  procedure ProcessZero(Fl: TRpField);
  begin
    if Fl.Func = tfProfit then
      FStr := FStr + '0 as income' + IntToStr(Fl.Id) + ', 0 as outcome' + IntToStr(Fl.Id) + ','
    else
    begin
      case Fl.Func of
        tfSum: FStr := FStr + '0 as f' + IntToStr(Fl.Id) + ',';
        tfCount:
          if Fl.AllZeros then
            FStr := FStr + '0 as f' + IntToStr(Fl.Id) + ','
          else
	          FStr := FStr + 'null as f' + IntToStr(Fl.Id) + ',';
        else
          FStr := FStr + 'null as f' + IntToStr(Fl.Id) + ',';
      end;
    end;
  end;

begin
  AliasSL := TStringList.Create;

  try

  FStr := ''; JStr := '';
  for i := 0 to Src.Fields.Count - 1 do
  begin
    Fl := Src.Fields[i]^;
    if Fl.Tp = flNone then Continue;
    if Fl.Zero then
      ProcessZero(Fl)
    //else if Fl.Func = tfProfit then
    //  ProcessSumField(Fl)
    else
      ProcessField(Fl, Fl);
  end;
  FStr := Copy(FStr, 1, Length(FStr) - 1);
  Result := 'select ' + FStr + ' from ' + TableStr(Src.Id);
  if Src.TId > 0 then
    Result := Result + ' left join ' + TableStr(Src.TId) + ' on ' + TableStr(Src.Id) + '.id=' +
      TableStr(Src.TId) + '.pid';
  Flt := '';
  if Trim(Src.Filter) <> '' then
    Flt := SqlSourceFilter(Src, Fm, PFm, DS, AliasSL, JStr);
  Result := Result + JStr;
  if Flt <> '' then
    Result := Result + ' where ' + Flt;

  finally
    AliasSL.Free;
  end;
end;

function SourceKindToStr(sk: TRpSourceKind): String;
begin
  Result := '';
  case sk of
    skIncome: Result := rsIncoming;
    skOutcome: Result := rsOutcoming;
  end;
end;

function TotalFuncToStr(tf: TRpTotalFunc): String;
begin
  Result := '';
  case tf of
    tfSum: Result := rsSum;
    tfAvg: Result := rsAverage;
    tfMax: Result := rsMaximum;
    tfMin: Result := rsMinimum;
    tfCount: Result := rsCount;
    tfProfit: Result := rsBalance;
    tfDistCount: Result := rsDistinctCount;
    tfMergeAll: Result := rsMergeAll;
    tfMerge: Result := rsMerge;
  end;
end;

{function IsSimpleReport(RD: TReportData): Boolean;
var
  Sr: TRpSource;
  i: Integer;
  Fl: TRpField;
begin
  Result := False;
  if RD.Sources.Count = 1 then
  begin
    Sr := RD.Sources[0]^;
    for i := 0 to Sr.Fields.Count - 1 do
    begin
      Fl := Sr.Fields[i]^;
      if Fl.Func <> tfNone then Exit;
    end;
    Result := RD.DateField < 0;
  end;
end; }

function GetFuncSql(Fl: TRpField): String;
var
  Nm: String;
  C: TComponent;
  Pr: Integer;
begin
  Result := '';
  Nm := 'f' + IntToStr(Fl.Id);
  case Fl.Func of
    tfSum: Result:= 'coalesce(sum(' + Nm + '),0)';
    tfAvg: Result:= 'coalesce(avg(' + Nm + '),0)';
    tfMax: Result:= 'max(' + Nm + ')';
    tfMin: Result:= 'min(' + Nm + ')';
    tfCount: Result:= 'count(' + Nm + ')';
    tfProfit: Result:= 'coalesce(sum(income' + IntToStr(Fl.Id) + ')-sum(outcome' + IntToStr(Fl.Id) + '),0)';
    tfDistCount: Result := 'count(distinct ' + Nm + ')';
    tfMergeAll, tfMerge:
      begin
        C := GetRpFieldComponent(Fl, True);
        if C is TdxCalcEdit then
        begin
          Pr := GetPrecission(C);
          if Pr > 0 then
            Nm := 'replace(substring(' + Nm + ' from 1 for position(''.'' in ' + Nm + ')+' +
              IntToStr(Pr) + '),''.'',''' + DefaultFormatSettings.DecimalSeparator +
              ''')'
          else
            Nm := 'substring(' + Nm + ' from 1 for position(''.'' in ' + Nm + ')-1)';
        end;
        Result := 'list(';
        if Fl.Func = tfMerge then Result := Result + 'distinct ';
        Result := Result + Nm + ',''; '')';
      end;
    //tfMerge: Result := 'list(distinct ' + Nm + ',''; '')';
  end;
end;

function GetHavingClause(Fl: TRpField): String;
var
  i, p: Integer;
  S, V1, V2, Fn, rS, Tmp, AbsValue1, AbsValue2: String;
  SL: TStringList;
  C: TComponent;
  Tp: TRpFieldType;
begin
  rS := '';
  Result := '';
  Fn := GetFuncSql(Fl);
  if Fl.Func in [tfCount, tfDistCount] then Tp := flNumber
  else Tp := GetLowField(@Fl)^.Tp;

  SL := TStringList.Create;
  SplitStr(Fl.Value, ';', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    if S = '' then Continue;
    Tmp := '';
    p := Pos(' .. ', S);
    if p > 0 then
    begin
      V1 := Copy(S, 1, p - 1);
      V2 := Copy(S, p + 4, 255);
    end
    else V1 := S;
    if Tp = flNumber then
    begin
      V1 := CheckNumber(Copy(S, 1, p - 1));
      AbsValue1 := StringReplace(V1, '-', '', []);
      V2 := CheckNumber(Copy(S, p + 4, 255));
      AbsValue2 := StringReplace(V2, '-', '', []);
      if V1 <> '' then
        Tmp := Tmp + Fn + '>=' + V1 + '-' + AbsValue1 + '*2e-12 and ';
      if V2 <> '' then
        Tmp := Tmp + Fn + '<=' + V2 + '+' + AbsValue2 + '*2e-12 and ';
      Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
      rS := rS + '(' + Tmp + ') or ';
    end
    else if Tp = flDate then
    begin
      if V1 <> '' then
      	Tmp := Tmp + Fn + '>=''' + V1 + ''' and ';
    	if V2 <> '' then
        Tmp := Tmp + Fn + '<=''' + V2 + ''' and ';
      Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
      rS := rS + '(' + Tmp + ') or ';
    end
    else if Tp = flTime then
    begin
      C := GetRpFieldComponent(Fl, True);
      CheckTime(TdxTimeEdit(C).TimeFormat, V1, V2);
      if V1 <> '' then
      	Tmp := Tmp + Fn + '>=''' + V1 + ''' and ';
    	if V2 <> '' then
        Tmp := Tmp + Fn + '<=''' + V2 + ''' and ';
      Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
      rS := rS + '(' + Tmp + ') or ';
    end
    else if Tp in [flCounter, flRecId] then
    begin
      if V1 <> '' then
      	Tmp := Tmp + Fn + '>=' + V1 + ' and ';
    	if V2 <> '' then
        Tmp := Tmp + Fn + '<=' + V2 + ' and ';
      Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
      rS := rS + '(' + Tmp + ') or ';
    end
    else if Tp = flText then
    begin
      rS := rS + Fn + ' containing ''' + EscapeSQuotes(V1) + ''' or ';
    end
    else if Tp = flObject then
    begin
      C := GetRpFieldComponent(Fl, True);
      Tmp := SqlSelectIDs(GetSourceTId(C), V1);
      if Tmp <> '' then rS := rS + '''' + Tmp + ''' containing ''\'' || ' + Fn + ' || ''\'' or '
      else rS := rS + Fn + '=' + V1 + ' or ';
    end;
  end;
  if Fl.Nul then
    rS := rS + Fn + ' is null or ';
  rS := Copy(rS, 1, Length(rS) - 4);
  if rS <> '' then
  begin
    if Fl.No then
      rS := 'not (' + rS + ')';
    Result := '(' + rS + ')';
  end;
  SL.Free;
end;

function GetWhereClause(aFl: TRpField): String;
var
  S, Bg, Ed, FlNm, W, Tmp, AbsValue: String;
  p: Integer;
  i: Integer;
  SL: TStringList;
  C: TComponent;
  Tp: TRpFieldType;
begin
  Result := '';
  SL := TStringList.Create;
  SplitStr(aFl.Value, ';', SL);
  FlNm := 'f' + IntToStr(aFl.Id);
  Tp := GetLowField(@aFl)^.Tp;

  W := '';
  if aFl.Nul then W := W + FlNm + ' is null or ';
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    if S = '' then Continue;
    case Tp of
      flText, flFile:
        begin
          S := UnEscapeSemicolon(S);
          W := W + FlNm + ' containing ''' + EscapeSQuotes(S) + ''' or ';
        end;
      flDate:
        begin
          {if Copy(S, 1, 1) = '$' then
          begin
            Ed := DateToStr(Date);
            case TPeriodType(StrToInt(Copy(S, 2, 10))) of
              ptToday: Bg := DateToStr(Date);
              ptThisWeek: Bg := DateToStr( IncDay(Date, -DayOfTheWeek(Date)+1) );
              ptThisMonth: Bg := DateToStr( IncDay(Date, -DayOf(Date)+1) );
              ptThisYear: Bg := DateToStr( EncodeDate(YearOf(Date), 1, 1) );
            end;
          end
          else  }
          //if not DateCodeToPeriod(S, Bg, Ed) then
          //begin
            p := Pos(' .. ', S);
            Bg := Copy(S, 1, p - 1);
            Ed := Copy(S, p + 4, 1024);
          //end;
          Tmp := '';
          if Bg <> '' then
            Tmp := Tmp + FlNm + '>=''' + Bg + ''' and ';
          if Ed <> '' then
            Tmp := Tmp + FlNm + '<=''' + Ed + ''' and ';
          Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
          if Tmp <> '' then
            W := W + '(' + Tmp + ') or ';
        end;
			flTime:
        begin
          p := Pos(' .. ', S);
          Bg := Copy(S, 1, p - 1);
          Ed := Copy(S, p + 4, 1024);
          C := GetRpFieldComponent(aFl, False);
          CheckTime(TdxTimeEdit(C).TimeFormat, Bg, Ed);
          Tmp := '';
          if Bg <> '' then
            Tmp := Tmp + FlNm + '>=''' + Bg + ''' and ';
          if Ed <> '' then
            Tmp := Tmp + FlNm + '<=''' + Ed + ''' and ';
          Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
          if Tmp <> '' then
            W := W + '(' + Tmp + ') or ';
        end;
      flNumber:
        begin
          p := Pos(' .. ', S);
          Bg := Copy(S, 1, p - 1);
          Ed := Copy(S, p + 4, 1024);
          Tmp := '';
          if Bg <> '' then
          begin
            Bg := CheckNumber(Bg);
            AbsValue := StringReplace(Bg, '-', '', []);
            Bg := Bg + '-' + AbsValue + '*2e-12';
            Tmp := Tmp + FlNm + '>=' + Bg + ' and ';
          end;
          if Ed <> '' then
          begin
            Ed := CheckNumber(Ed);
            AbsValue := StringReplace(Ed, '-', '', []);
            Ed := Ed + '+' + AbsValue + '*2e-12';
            Tmp := Tmp + FlNm + '<=' + Ed + ' and ';
          end;
          Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
          if Tmp <> '' then
            W := W + '(' + Tmp + ') or ';
        end;
      flCounter, flRecId:
        begin
          p := Pos(' .. ', S);
          Bg := Copy(S, 1, p - 1);
          Ed := Copy(S, p + 4, 1024);
          Tmp := '';
          if Bg <> '' then
            Tmp := Tmp + FlNm + '>=' + Bg + ' and ';
          if Ed <> '' then
            Tmp := Tmp + FlNm + '<=' + Ed + ' and ';
          Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
          if Tmp <> '' then
            W := W + '(' + Tmp + ') or ';
        end;
      flBool: W := W + FlNm + '=' + S + ' or ';
      flObject:
        begin
          C := GetRpFieldComponent(aFl, True);
          Tmp := SqlSelectIDs(GetSourceTId(C), S);
          if Tmp <> '' then W := W + '''' + Tmp + ''' containing ''\'' || ' + FlNm + ' || ''\'' or '
          else W := W + FlNm + '=' + S + ' or ';
        end;
    end;
  end;
  SL.Free;
  W := Copy(W, 1, Length(W) - 4);
  if W <> '' then
  begin
    if aFl.No then W := 'not (' + W + ')';
    Result := '(' + W + ')';
  end;
end;

// Устанавливает в полях флаг "Не выбрано ни одного поля для функции Количество".
// В этом случае, если для функции "Количество" не указаны поля, то считается
// количество записей. Если указано хотя бы одно поле, то считаются записи для
// источников, в которых поле указано.
procedure SetAllZeros(RD: TReportData);
var
  i: Integer;

  function IsAllZeros(idx: Integer): Boolean;
  var
    j: Integer;
  begin
    Result := True;
    for j := 0 to RD.Sources.Count - 1 do
      if RD.Sources[j]^.Fields[idx]^.Zero = False then Exit(False);
  end;

  procedure _SetAllZeros(idx: Integer);
  var
    j: Integer;
  begin
    for j := 0 to RD.Sources.Count - 1 do
      RD.Sources[j]^.Fields[idx]^.AllZeros := True;
  end;

begin
  for i := 0 to RD.Sources[0]^.Fields.Count - 1 do
    if IsAllZeros(i) then _SetAllZeros(i);
end;

function CalcFieldExistsInSort(RD: TReportData): Boolean;
var
  i: Integer;
  S: String;
begin
  Result := False;
  for i := 0 to RD.Grid.SortCols.Count - 1 do
  begin
    S := UpperCase(Copy(RD.Grid.SortCols[i].Col.FieldNameDS, 1, 2));
    if S = 'CF' then Exit(True);
  end;
end;

{function SortFieldsToStr(RD: TReportData): String;
var
  i: Integer;
  Col: TRpGridSortData;
begin
  Result := '';
  for i := 0 to RD.Grid.SortCols.Count - 1 do
  begin
    Col := RD.Grid.SortCols[i];
    Result := Result + Col.Col.FieldName;
    if Col.Desc then Result := Result + ' desc';
    Result := Result + ',';
  end;
  Result := Copy(Result, 1, Length(Result) - 1);
end;}

procedure CheckSources(RD: TReportData);
var
  i: Integer;
  Fm: TdxForm;
begin
  for i := 0 to RD.Sources.Count - 1 do
  begin
    Fm := FormMan.FindForm(RD.Sources[i]^.Id);
    if Fm.ViewType = vtSimpleForm then
      raise Exception.CreateFmt(rsSimpleFormCantDataSource, [Fm.FormCaption]);
  end;
end;

function InnerSqlReportSelect(RD: TReportData; Fm, PFm: TdxForm; DS: TDataSet): String;
var
  Sr: TRpSource;
  FStr, GStr, FromStr, Nm, Srt, S, Hav, Wh: String;
  i: Integer;
  Fl: TRpField;
  Sim, NeedGroup: Boolean;
  Col: TRpGridSortData;
  CF: TRpCalcField;
begin
  Result := '';
  if RD.IsEmpty then Exit;
  CheckSources(RD);
  SetAllZeros(RD);

  Sr := RD.Sources[0]^;

  FStr := '';
  GStr := '';
  Hav := '';
  Wh := '';
  NeedGroup := False;
  for i := 0 to Sr.Fields.Count - 1 do
  begin
    Fl := RD.FindField(Sr.Fields[i]^.Id)^;
    if Fl.Tp = flNone then Continue;
    Nm := 'f' + IntToStr(Fl.Id);

    if Fl.Param then
    begin
      if Fl.Func <> tfNone then
      begin
        S := GetHavingClause(Fl);
        if S <> '' then
          Hav := Hav + S + ' and '
      end
      else
      begin
        S := GetWhereClause(Fl);
        if S <> '' then
          Wh := Wh + S + ' and '
      end;
    end;

    if not Fl.Visible then Continue;

    if i = RD.DateField then
    begin
      case RD.DateDetail of
        ddDay: FStr := FStr + Nm + ',';
        ddWeek: FStr := FStr + Format('(extract (year from %0:s) || ''.'' || trim(iif(extract (week from %0:s) < 10, ''0'', '''') || (extract (week from %0:s)))) as %0:s,', [Nm]);
        ddMonth: FStr := FStr + Format('(extract (year from %0:s) || ''.'' || trim(iif(extract (month from %0:s) < 10, ''0'', '''') || (extract (month from %0:s)))) as %0:s,', [Nm]);
        ddQuart: FStr := FStr + Format('(extract (year from %0:s)  || ''.'' || ROUND((CAST(EXTRACT(MONTH FROM %0:s) AS FLOAT)/3 + 0.3))) as %0:s,', [Nm]);
        ddHalfYear: FStr := FStr + Format('(extract (year from %0:s)  || ''.'' ||iif(extract(month from %0:s) <= 6, 1, 2)) as %0:s,', [Nm]);
        ddYear: FStr := FStr + Format('(extract (year from %0:s)) as %0:s,', [Nm]);     // ROUND(CAST(EXTRACT(MONTH FROM %0:s)/3 + 0.3 AS FLOAT),0))
      end;
      GStr := GStr + Nm + ',';
      NeedGroup := True;
    end
    else if Fl.Func = tfNone then
    begin
      FStr := FStr + Nm + ',';
      GStr := GStr + Nm + ',';
    end
    else
    begin
      FStr := FStr + GetFuncSql(Fl) + ' as ' + Nm + ',';
      NeedGroup := True;
    end;
  end;
  Wh := Copy(Wh, 1, Length(Wh) - 5);

  Hav := Copy(Hav, 1, Length(Hav) - 5);

  FromStr := '';
  for i := 0 to RD.Sources.Count - 1 do
  begin
    try
      FromStr := FromStr + SqlSourceSelect2(RD.Sources[i]^, Fm, PFm, DS);
    except
      on E: EFilterParserError do
        raise ESourceFilterError.Create(E.Message, RD.Sources[i]^.Filter, i+1, E.Position);
    end;
    if i < RD.Sources.Count - 1 then FromStr := FromStr + ' union all ';
  end;

  Sim := RD.IsSimple;
  if Sim then
  begin
    S := ' ' + TableStr(RD.Sources[0]^.Id) + '.id as id';
    if RD.Sources[0]^.TId > 0 then S := S + ',' + TableStr(RD.Sources[0]^.TId) + '.id as tid';
    if FStr <> '' then S := S + ',';
    Insert(S, FromStr, 7);
    if RD.Sources[0]^.TId > 0 then FStr := 'tid,' + FStr;
    FStr := 'id,' + FStr;
  end;
  // Добавляем поля-пустышки для вычисляемых полей
  (*for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i]^;
    case CF.Tp of
      flNumber: S := 'CAST(0.0 AS DOUBLE PRECISION)';
      flDate: S := 'CURRENT_DATE';
      flTime: S := 'CURRENT_TIME';
      else S := Format('(CAST('' '' AS VARCHAR(%d)))', [CF.Size]) //S := '(''' + DupeString(' ', CF.Size) + ''')';
    end;
    //FStr := FStr + '(''' + DupeString(' ', 200) + ''') as cf' +
    FStr := FStr + S + ' as cf' + IntToStr(CF.Id) + ',';
  end; *)
  //
  FStr := Copy(FStr, 1, Length(FStr) - 1);
  GStr := Copy(GStr, 1, Length(GStr) - 1);
  Result := 'select ';
  if RD.FirstRecordCount > 0 then
    Result := Result + 'first ' + IntToStr(RD.FirstRecordCount) + ' ';
  Result := Result + FStr + ' from (' + FromStr + ') ';
  if Wh <> '' then
    Result := Result + ' where ' + Wh;
  if (not Sim) and NeedGroup then
  begin
    if GStr <> '' then
      Result := Result + ' group by ' + GStr;
    if Hav <> '' then
      Result := Result + ' having ' + Hav;
  end;

  // Сортировка. Пропускаем вычисляемые поля.
  Srt := '';
  for i := 0 to RD.Grid.SortCols.Count - 1 do
  begin
    Col := RD.Grid.SortCols[i];
    if UpperCase(Copy(Col.Col.FieldNameDS, 1, 2)) = 'CF' then Continue;
    Srt := Srt + Col.Col.FieldNameDS;
    if Col.Desc then Srt := Srt + ' desc';
    Srt := Srt + ',';
  end;
  SetLength(Srt, Length(Srt) - 1);
  if Srt <> '' then
    Result := Result + ' order by ' + Srt;
  //Debug(Result);
end;

function InnerSqlReportSelectSQL(RD: TReportData; Fm: TdxForm): String;
var
  i, FieldIndex: Integer;
  Col: TRpGridSortData;
  SortOrder: String;
begin
  Result := ParseSQL(RD.SQL, Fm);
  SortOrder := '';
  for i := 0 to RD.Grid.SortCols.Count - 1 do
  begin
    Col := RD.Grid.SortCols[i];
    FieldIndex := RD.IndexOfNameDS(Col.Col.FieldNameDS);
    // Если есть сортировка по вычисляемому полю, то будем сортировать локально
    if RD.IsCalcField(FieldIndex) then
    begin
      SortOrder := '';
      Break;
    end;
    SortOrder := SortOrder + IntToStr(FieldIndex + 1) +
      IIF(Col.Desc, ' desc', '') + ',';
  end;
  SetLength(SortOrder, Length(SortOrder) - 1);
  if SortOrder <> '' then
    Result := Result + ' order by ' + SortOrder;
end;

function SqlReportSelect(RD: TReportData; Fm, PFm: TdxForm; DS: TDataSet): String;
begin
  if not RD.SqlMode then
    Result := InnerSqlReportSelect(RD, Fm, PFm, DS)
  else
    Result := InnerSqlReportSelectSQL(RD, Fm);
end;

{procedure AddCalcFields(RD: TReportData; DS: TDataSet);
var
  F: TField;
  i: Integer;
  CF: TRpCalcField;
begin
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
		CF := RD.CalcFields[i]^;
    case CF.Tp of
      flNumber: F := TFloatField.Create(DS);
      flDate: F := TDateField.Create(DS);
			flTime: F := TTimeField.Create(DS);
      else // flText
      begin
        F := TStringField.Create(DS);
        TStringField(F).Size:=200;
      end;
    end;
    F.FieldName := 'CF' + IntToStr(CF.Id);
    F.Name := F.FieldName;
    F.DataSet := DS;
    //DS.Fields.Add(F);
  end;
end; }

procedure CalcQuery(aRD: TReportData; DS: TDataSet; aForm, aParForm: TdxForm;
  FormDS: TDataSet; Errs: TCalcError);
var
  EB: TExpressionBuilder;
  EL, FL: TList;
  i: Integer;
  S, FmCap: String;
  E: TExpression;
  F: TField;
  AftScr, BefScr: TDataSetNotifyEvent;
  CF: TRpCalcField;
  V: Variant;
begin
  if aRD.CalcFields.Count = 0 then Exit;
  EB := TExpressionBuilder.Create;
  EB.SkipLabels:=True;
  EB.RD := aRD;
  EB.RDSet := DS;
  EB.DataSet := FormDS;
  EB.Form := aForm;
  EB.ParentForm := aParForm;
  EL := TList.Create;
  FL := TList.Create;
  for i := 0 to aRD.CalcFields.Count - 1 do
  begin
    CF := aRD.CalcFields[i]^;
    S := CF.Expr;
    if Trim(S) <> '' then
    begin
      try
        E := EB.Build(S);
        EL.Add(E);         // Здесь nil уместен (см. else)
      except
        on Ex: Exception do
        begin
          EL.Add(nil);
          if aForm <> nil then FmCap := aForm.Caption
          else FmCap := '';
          Errs.AddErr(FmCap, aRD.Name,
            IIF(aRD.Kind = rkQuery, rsQuery, rsReport),
            Format(rsCalcField, [CF.Name]), CF.Expr, Ex);
        end;
      end;
    end
    else
    begin
      EL.Add(nil);
    end;
    FL.Add(DS.FieldByName('cf' + IntToStr(CF.Id)));
  end;
  EB.Free;

  // Снимаем флаг обязательный
  //for i := 0 to DS.Fields.Count - 1 do
  //  DS.Fields[i].Required:=False;
  //

  AftScr := DS.AfterScroll;
  BefScr := DS.BeforeScroll;
  DS.AfterScroll := nil;
  DS.BeforeScroll:=nil;
  DS.DisableControls;
  DS.First;
  while not DS.Eof do
  begin
    DS.Edit;
    for i := 0 to EL.Count - 1 do
    begin
      CF := aRD.CalcFields[i]^;
      E := TExpression(EL[i]);
      F := TField(FL[i]);
      if E = nil then
      begin
        F.Value := Null;
      end
      else
        try
          V := E.Calc;
          if (CF.Tp = flNumber) and (V <> Null) then
            V := MathRound(V, CF.Size);
          if DS.State <> dsEdit then DS.Edit;
          F.Value := V;
        except
          on Ex: Exception do
          begin
            F.Value := Null;
            Errs.AddErr('', aRD.Name, IIF(aRD.Kind = rkQuery, rsQuery, rsReport),
              Format(rsCalcField, [CF.Name]), CF.Expr, Ex);
          end;
        end;
    end;
    DS.Post;
    DS.Next;
  end;
  DS.First;
  DS.EnableControls;
  DS.AfterScroll:=AftScr;
  DS.BeforeScroll:=BefScr;

  ClearList(EL);
  EL.Free;
  FL.Free;
end;

procedure FilterQuery(aRD: TReportData; DS: TDataSet; aForm, aParForm: TdxForm;
  FormDS: TDataSet);
var
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
  AftScr, BefScr: TDataSetNotifyEvent;
begin
  if Trim(aRD.Filter) = '' then Exit;
  EB := TExpressionBuilder.Create;
  EB.RD := aRD;
  EB.RDSet := DS;
  EB.DataSet := FormDS;
  EB.Form := aForm;
  EB.ParentForm := aParForm;
  EB.SkipLabels:=True;
  try
    E := EB.Build(aRD.Filter);
  finally
    EB.Free;
  end;

  if E = nil then Exit;

  try
    AftScr := DS.AfterScroll;
    BefScr := DS.BeforeScroll;
    DS.AfterScroll := nil;
    DS.BeforeScroll := nil;
    DS.DisableControls;
    DS.First;
    while not DS.Eof do
    begin
      V := E.Calc;
      if VarIsBool(V) and (V = False) then
        DS.Delete
      else
        DS.Next;
    end;
  finally
    DS.First;
    DS.EnableControls;
    DS.AfterScroll:=AftScr;
    DS.BeforeScroll := BefScr;
    if BefScr <> nil then DS.BeforeScroll(DS);
    if AftScr <> nil then DS.AfterScroll(DS);
    E.Free;
  end;
end;

procedure BuildSortIndexes(RD: TReportData; DataSet: TSQLQuery);
var
  i: Integer;
  Col: TRpGridColumn;
  SCol: TRpGridSortData;
  FieldNames: String;
begin
  if DataSet.RecordCount = 0 then Exit;
  {if not DataSet.IndexDefs.Updated then
  	DataSet.IndexDefs.Update; }
  FieldNames := '';
  //DescFieldNames := '';
  for i := 0 to RD.Grid.SortCols.Count - 1 do
  begin
    SCol := RD.Grid.SortCols[i];
    Col := SCol.Col;
    {FieldNames := FieldNames + Col.FieldNameDS + ';';
    if SCol.Desc then
      DescFieldNames := DescFieldNames + Col.FieldNameDS + ';'}
    FieldNames := FieldNames + Col.FieldNameDS;
    if SCol.Desc then FieldNames := FieldNames + ' DESC';
    FieldNames := FieldNames + ';';
  end;
  FieldNames := Copy(FieldNames, 1, Length(FieldNames) - 1);
  //DescFieldNames := Copy(DescFieldNames, 1, Length(DescFieldNames) - 1);
  DataSet.IndexFieldNames:=FieldNames;
  {if FieldNames <> '' then
  begin
	  DataSet.AddIndex('MY_INDEX', FieldNames, [], DescFieldNames);
  	DataSet.IndexName:='MY_INDEX';
  end; }
end;

{function GetRealRpFieldType(RD: TReportData; Fl: PRpField): TRpFieldType;
begin
  Result := GetLowField(Fl)^.Tp;
  if RD.Sources.Count = 0 then Exit;
  if (RD.DateField >= 0) and (RD.Sources[0]^.Fields[RD.DateField]^.Id = Fl^.Id) then
  begin
    if RD.DateDetail = ddYear then Result := flNumber
    else if RD.DateDetail <> ddDay then Result := flText;
  end
  else if Fl^.Func <> tfNone then
  begin
    if Fl^.Func in [tfCount, tfDistCount] then Result := flNumber
    else if Fl^.Func in [tfMerge, tfMergeAll] then Result := flText;
  end
end; }

function CreateReportForm(RD: TReportData; out SQL: String): TdxForm;

  function CreateNumberField(Fm: TdxForm; LowF: TRpField;
    const FieldName, Value: String; var n: Integer): String;
  var
    C, Cmp: TdxCalcEdit;
    S: String;
    Frm: TdxForm;
  begin
    C := TdxCalcEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;

    if LowF.TId > 0 then
    begin
      Frm := FormMan.FindForm(LowF.TId);
      Cmp := TdxCalcEdit(FindById(Frm, LowF.FId));
      C.Precission := Cmp.Precission;
      C.GroupDigits := Cmp.GroupDigits;
    end
    else
      C.GroupDigits := True;

    if Value > '' then
    begin
      S := StringReplace(Value, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);
      S := StringReplace(S, DefaultFormatSettings.DecimalSeparator, '.', []);
      Result := Format('%s as f%d,', [S, n]);
    end
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateTotalNumberField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    S: String;
    p: SizeInt;
    C: TdxCalcEdit;
  begin
    S := StringReplace(Value, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);
    S := StringReplace(S, DefaultFormatSettings.DecimalSeparator, '.', []);
    p := Pos('.', S);
    C := TdxCalcEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.GroupDigits := True;
    if p > 0 then C.Precission := Length(S) - p;
    if S > '' then Result := Format('%s as f%d,', [S, n])
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateDateField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxDateEdit;
    DT: TDateTime;
  begin
    C := TdxDateEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    if (Value > '') and TryStrToDate(Value, DT) then
      Result := Format('(CAST(''%s'' as DATE)) as f%d,', [Date2Str(DT), n])
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateTimeField(Fm: TdxForm; LowF: TRpField;
    const FieldName, Value: String; var n: Integer): String;
  var
    C: TdxTimeEdit;
    Frm: TdxForm;
    Cmp: TdxTimeEdit;
    DT: TDateTime;
  begin
    Frm := FormMan.FindForm(LowF.TId);
    Cmp := TdxTimeEdit(FindById(Frm, LowF.FId));
    C := TdxTimeEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.TimeFormat := Cmp.TimeFormat;
    if (Value > '') and TryStrToTime(Value, DT) then
      Result := Format('(CAST(''%s'' as TIME)) as f%d,', [Time2Str(DT), n])
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateTotalTimeField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxTimeEdit;
    DT: TDateTime;
  begin
    C := TdxTimeEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.TimeFormat:=ttHHMMSS;
    if (Value > '') and TryStrToTime(Value, DT) then
      Result := Format('(CAST(''%s'' as TIME)) as f%d,', [Time2Str(DT), n])
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateCounterField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxCounter;
  begin
    C := TdxCounter.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    if Value > '' then Result := Format('%s as f%d,', [Value, n])
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateRecIdField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxRecordId;
  begin
    C := TdxRecordId.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    if Value > '' then Result := Format('%s as f%d,', [Value, n])
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateObjectField(Fm: TdxForm; LowF: TRpField;
    const FieldName, Value: String; var n: Integer): String;
  var
    C, Cmp: TdxLookupComboBox;
    Frm: TdxForm;
    S: String;
  begin
    Frm := FormMan.FindForm(LowF.TId);
    Cmp := TdxLookupComboBox(FindById(Frm, LowF.FId));
    C := TdxLookupComboBox.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.SourceTId := Cmp.SourceTId;
    C.SourceFId := Cmp.SourceFId;
    if Value > '' then
    begin
      S := GetObjFieldValue(C, StrToInt(Value), True);
      Result := Format('%s as f%d,', [Value, n]) +
        Format('(CAST(''%s'' as VARCHAR(%d))) as f%dl,', [S, Utf8Length(S), n])
    end
    else Result := Format('null as f%0:d,null as f%0:dl,', [n]);
    Inc(n);
  end;

  function CreateCheckBoxField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxCheckBox;
  begin
    C := TdxCheckBox.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    if Value > '' then Result := Format('%s as f%d,', [Value, n])
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateTextField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxEdit;
  begin
    C := TdxEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    if Value > '' then
      Result := Format('(CAST(''%s'' as VARCHAR(%d))) as f%d,', [Value, Utf8Length(Value), n])
    else
      Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateTotalField(Fm: TdxForm; TF: TRpTotalData;
    var n: Integer): String;
  var
    S: String;
    pF: PRpField;
    Tp: TRpFieldType;
    //pCF: PRpCalcField;
    i: Integer;
  begin
    S := TF.FieldNameDS;
    pF := nil; //pCF := nil;
    if TF.Func = tfCount {in [tfCount, tfSum, tfAvg]} then
      Tp := flNumber
    else
    begin
      i := RD.IndexOfNameDS(S);
      Tp := RD.GetFieldType(i);
      pF := RD.TryGetRpField(i);

      {if Copy(S, 1, 1) = 'f' then
      begin
        System.Delete(S, 1, 1);
        pF := GetLowField(RD.FindField(StrToInt(S)));
        Tp := pF^.Tp;
      end
      else
      begin
        if S > '' then
        begin
          System.Delete(S, 1, 2);
          pCF := RD.CalcFields.FindField(StrToInt(S));
          Tp := pCF^.Tp;
        end;
      end; }
    end;

    case Tp of
      flNumber:
        begin
          if pF <> nil then Result := CreateNumberField(Fm, pF^, TF.Caption, TF.Value, n)
          else Result := CreateTotalNumberField(Fm, TF.Caption, TF.Value, n);
        end;
      flDate: Result := CreateDateField(Fm, TF.Caption, TF.Value, n);
      flTime:
        begin
          if pF <> nil then Result := CreateTimeField(Fm, pF^, TF.Caption, TF.Value, n)
          else Result := CreateTotalTimeField(Fm, TF.Caption, TF.Value, n);
        end;
      flCounter: Result := CreateCounterField(Fm, TF.Caption, TF.Value, n);
      flRecId: Result := CreateRecIdField(Fm, TF.Caption, TF.Value, n);
      else Result := CreateTextField(Fm, TF.Caption, TF.Value, n);
    end;
  end;

  function EmptyStrIfNotParams(const S: String): String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to Length(S) do
      if S[i] <> ';' then Exit(S);
  end;

  function RDParamsToFields(Fm: TdxForm): String;
  var
    i, n: Integer;
    F, LowF: TRpField;
    SL: TStringList;
    Value, S, S1, S2: String;
    p: SizeInt;
    T: TRpTotalData;
  begin
    n := 1;
    Result := '';
    SL := TStringList.Create;

    if RD.Sources.Count > 0 then
    begin
      for i := 0 to RD.Sources[0]^.Fields.Count - 1 do
      begin
        F := RD.Sources[0]^.Fields[i]^;
        LowF := GetLowField(@F)^;
        if F.Param then
        begin
          // Поле_text содержит текстовое представление всех значений параметра

          Result := Result +
            CreateTextField(Fm, F.Name + '_text', EmptyStrIfNotParams(F.ValueStr), n) +
            CreateCheckBoxField(Fm, F.Name + '_not', Bool2Str(F.No), n) +
            CreateCheckBoxField(Fm, F.Name + '_null', Bool2Str(F.Nul), n);
          SplitStr(F.Value, ';', SL);
          // Поля создаются только для одного значения параметра, остальные игнорируются.
          if SL.Count > 0 then Value := SL[0]
          else Value := '';
          if LowF.Tp in [flNumber, flDate, flTime, flCounter, flRecId] then
          begin
            {if (LowF.Tp = flDate) and DateCodeToPeriod(Value, S1, S2) then
              Value := S1 + ' .. ' + S2
            else
            begin }
              S := Value;
              if S = '' then S := ' .. ';
              p := Pos(' .. ', S);
              S1 := Copy(S, 1, p - 1);
              S2 := Copy(S, p + 4, 200);
            //end;
            Result := Result + CreateTextField(Fm, F.Name, Value, n);
            case LowF.Tp of
              flNumber: Result := Result + CreateNumberField(Fm, LowF, F.Name + '_begin', S1, n) +
                CreateNumberField(Fm, LowF, F.Name + '_end', S2, n);
              flDate: Result := Result + CreateDateField(Fm, F.Name + '_begin', S1, n) +
                CreateDateField(Fm, F.Name + '_end', S2, n);
              flTime: Result := Result + CreateTimeField(Fm, LowF, F.Name + '_begin', S1, n) +
                CreateTimeField(Fm, LowF, F.Name + '_end', S2, n);
              flCounter: Result := Result + CreateCounterField(Fm, F.Name + '_begin', S1, n) +
                CreateCounterField(Fm, F.Name + '_end', S2, n);
              flRecId: Result := Result + CreateRecIdField(Fm, F.Name + '_begin', S1, n) +
                CreateRecIdField(Fm, F.Name + '_end', S2, n);
            end;
          end
          else if LowF.Tp = flObject then
            Result := Result + CreateObjectField(Fm, LowF, F.Name, Value, n)
          else if LowF.Tp = flBool then
            Result := Result + CreateCheckBoxField(Fm, F.Name, Value, n)
          else
          begin
            Result := Result + CreateTextField(Fm, F.Name, UnEscapeSemicolon(Value), n);
          end;
          //
        end;
      end;
    end;

    for i := 0 to RD.Totals.Count - 1 do
    begin
      T := RD.Totals[i];
      if RD.IndexOfNameDS(T.FieldNameDS) < 0 then Continue;
      Result := Result + CreateTotalField(Fm, T, n);
    end;
    Result := Copy(Result, 1, Length(Result) - 1);
    SL.Free;
  end;

begin
  Result := TdxForm.Create(nil);
  Result.FormCaption := rsReportWindow;
  Result.ViewType := vtGridOnly;
  Result.CalcFields := RD.PrintFields;
  SQL := RDParamsToFields(Result);
  //Debug(SQL);
  with TdxQueryGrid.Create(Result) do
  begin
    Name := 'QGrid';
    ManualRefresh := True;
    Id := RD.Id;
  end;
end;

function RpFieldIsCheckBox(RD: TReportData; const FieldNameDS: String): Boolean;
var
  pF: PRpField;
  i, n: Integer;
  SqlF: TSQLField;
begin
  Result := False;
  n := RD.IndexOfNameDS(FieldNameDS);
  if n < 0 then Exit;
  pF := RD.TryGetRpField(n);
  SqlF := RD.TryGetSqlField(n);

  if pF <> nil then
  begin
    if (GetLowField(pF)^.Tp <> flBool) or (pF^.Func <> tfNone) then Exit;
    for i := 1 to RD.Sources.Count - 1 do
    begin
      pF := RD.Sources[i]^.Fields[n];
      if pF^.Zero or (GetLowField(pF)^.Tp <> flBool) then Exit;
    end;
    Result := True;
  end
  else if SqlF <> nil then
    Result := SqlF.Tp = flBool;
end;

procedure RemoveLostFieldsFromReportData(RD: TReportData);
var
  i, FieldIndex: Integer;
  FlNm: String;
  Col: TRpGridColumn;
  Tot: TRpTotalData;
  CD: TRpColoringData;
begin
  for i := RD.Grid.ColumnCount - 1 downto 0 do
  begin
    Col := RD.Grid.Columns[i];
    FlNm := RD.Grid.Columns[i].FieldNameDS;
    FieldIndex := RD.IndexOfNameDS(FlNm);
    if (FieldIndex < 0) or not RD.GetFieldVisible(FieldIndex) then
    begin
      RD.Grid.DeleteColumn(Col);

      repeat
        Tot := RD.Totals.FindTotal(Col.FieldNameDS);
        if Tot <> nil then RD.Totals.RemoveTotal(Tot);
      until Tot = nil;

      repeat
        CD := RD.Coloring.FindColoring(Col.FieldNameDS);
        if CD <> nil then RD.Coloring.DeleteColoring(CD);
      until CD = nil;
    end;
  end;
end;

procedure CreateOrUpdateReportGridColumns(RD: TReportData);
var
  i: Integer;
  Col: TRpGridColumn;
begin
  for i := 0 to RD.GetFieldCount - 1 do
  begin
    if not RD.GetFieldVisible(i) then Continue;
    Col := RD.Grid.FindColumnByFieldName( RD.GetFieldNameDS(i) );
    if Col = nil then
    begin
      Col := RD.Grid.AddDefaultColumn;
      Col.FieldNameDS := RD.GetFieldNameDS(i);
      Col.Caption := RD.GetFieldName(i);
    end
    else
      Col.Caption := RD.GetFieldName(i);
  end;
end;

{ ESourceFilterError }

constructor ESourceFilterError.Create(const Msg, Expr: String; ASourceNum,
  APos: Integer);
begin
  inherited Create(Msg);
  FExpr := Expr;
  FPosition := APos;
  FSourceNum := ASourceNum;
end;

{ TdxQField }

{function TdxQField.GetFieldType: TRpFieldType;
begin
  Result := FpF^.Tp;
end;

function TdxQField.GetFunc: TRpTotalFunc;
begin
  Result := FpF^.Func;
end;

function TdxQField.GetName: String;
begin
  Result := FpF^.Name;
end;   }

{ TdxQuery }

(*procedure TdxQuery.SetZeroFieldType;
var
  i, j: Integer;
  pSr: PRpSource;
  pF: PRpField;
begin
  for i := 0 to FRD.Sources.Count - 1 do
  begin
    pSr := FRD.Sources[i];
    for j := 0 to pSr^.Fields.Count - 1 do
    begin
      pF := pSr^.Fields[j];
      if pF^.Zero then
      begin
        pF^.Tp := FRD.FindField(pF^.Id)^.Tp;
      end;
    end;
  end;
end;

function TdxQuery.GetFields(aName: String): Variant;
var
  pF: PRpField;
begin
  pF := FRD.FindFieldByName(aName);
  if pF = nil then raise Exception.CreateFmt(rsFieldNotFound, [aName]);
  Result := FDataSet.FieldByName('f' + IntToStr(pF^.Id)).Value;
end;

function TdxQuery.GetAsDT(Index: String): TDateTime;
begin
  Result := Nz(Fields[Index], 0);
end;

function TdxQuery.GetAsF(Index: String): Extended;
begin
  Result := Nz(Fields[Index], 0);
end;

function TdxQuery.GetAsI(Index: String): Integer;
begin
  Result := Nz(Fields[Index], 0);
end;

function TdxQuery.GetAsS(Index: String): String;
begin
  Result := Nz(Fields[Index], '');
end;

constructor TdxQuery.Create;
begin
  FRD := TReportData.Create;
  FSources := TList.Create;
  FDataSet := TSQLQuery.Create(nil);
  DBase.AttachDataSet(FDataSet);
end;

destructor TdxQuery.Destroy;
begin
  Close;
  ClearList(FSources);
  FSources.Free;
  FDataSet.Free;
  FRD.Free;
  inherited Destroy;
end;

function TdxQuery.AddSource(const aFormName, aSubFormName, aFilter: String;
  aKind: TRpSourceKind): TdxQSource;
var
  pSr: PRpSource;
  Fm: TdxForm;
begin
  Result := TdxQSource.Create;
  FRD.Sources.AddSource(pSr);
  Result.FpSr:=pSr;
  Fm := FormMan.FindFormByName(aFormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [aFormName]);
  pSr^.Id:=IntToStr(Fm.Id);
  if aSubFormName <> '' then
  begin
    Fm := FormMan.FindFormByName(aSubFormName);
    if Fm = nil then
    begin
      Result.Free;
      pSr^.Fields.Free;
      Dispose(pSr);
      raise Exception.CreateFmt(rsFormNotFound, [aFormName]);
    end;
    pSr^.TId:=IntToStr(Fm.Id);
  end;
  pSr^.Filter:=aFilter;
  pSr^.Kind:=aKind;
  FSources.Add(Result);
end;

procedure TdxQuery.GroupByDate(const aDateField: String;
  aDateDetail: TRpDateDetail);
var
  pF: PRpField;
begin
  pF := FRD.FindFieldByName(aDateField);
  if pF = nil then raise Exception.CreateFmt(rsFieldNotFound, [aDateField]);
  FRD.DateField:=pF^.Id;
  FRD.DateDetail:=aDateDetail;
end;

procedure TdxQuery.AddSorting(const aName: String; Desc: Boolean);
var
  Col: TRpGridColumn;
  pF: PRpField;
begin
  pF := FRD.FindFieldByName(aName);
  if pF = nil then raise Exception.CreateFmt(rsFieldNotFound, [aName]);
  Col := FRD.Grid.AddColumn;
  Col.FieldName := FieldStr(pF^.Id);
  FRD.Grid.SortCols.AddCol(Col, Desc);
end;

procedure TdxQuery.Open;
begin
  FDataSet.SQL.Text := SqlReportSelect(FRD, nil, nil, nil);
  FDataSet.Open;
end;

procedure TdxQuery.Close;
begin
  FDataSet.Close;
end;

function TdxQuery.Opened: Boolean;
begin
  Result := FDataSet.Active;
end;

procedure TdxQuery.MoveBy(Distance: Integer);
begin
  FDataSet.MoveBy(Distance);
end;

procedure TdxQuery.MoveFirst;
begin
  FDataSet.First;
end;

procedure TdxQuery.MovePrior;
begin
  FDataSet.Prior;
end;

procedure TdxQuery.MoveNext;
begin
  FDataSet.Next;
end;

procedure TdxQuery.MoveLast;
begin
  FDataSet.Last;
end;

procedure TdxQuery.MoveTo(aRecNo: Integer);
begin
  MoveBy(aRecNo - RecNo);
end;

function TdxQuery.BOF: Boolean;
begin
  Result := FDataSet.BOF;
end;

function TdxQuery.EOF: Boolean;
begin
  Result := FDataSet.EOF;
end;

function TdxQuery.RecNo: Integer;
begin
  Result := FDataSet.RecNo;
end;

function TdxQuery.RecId: Integer;
begin
  Result := FDataSet.FieldByName('id').AsInteger;
end;

function TdxQuery.RecordCount: Integer;
begin
  Result := FDataSet.RecordCount;
end;  *)

{ TdxQSource }

{constructor TdxQSource.Create;
begin
  FFields := TList.Create;
end;

destructor TdxQSource.Destroy;
begin
  ClearList(FFields);
  FFields.Free;
  inherited Destroy;
end;

function TdxQSource.AddField(const aFieldName, aName: String; aFunc: TRpTotalFunc
  ): TdxQField;
var
  pF: PRpField;
  S, FlNm: String;
  Fm: TdxForm;
  p, i: Integer;
  C: TComponent;
begin
  Result := TdxQField.Create;
  Result.FieldName := aFieldName;
  i := FpSr^.Fields.AddField(pF);
  Result.FpF := pF;
  if aFieldName <> '' then
  begin
    S := aFieldName;
    if Copy(S, 1, 1) = '!' then
    begin
      Delete(S, 1, 1);
      Fm := FormMan.FindForm(StrToInt(FpSr^.Id));
    end
    else
      Fm := FormMan.FindForm(StrToInt(FpSr^.TId));
    p := Pos('|', S);
    if p > 0 then FlNm := Copy(S, 1, p - 1)
    else FlNm := S;
    C := FindComponentByFieldName(Fm, FlNm);
    if C = nil then
    begin
      Result.Free;
      Dispose(pF);
      raise Exception.CreateFmt(rsFieldNotFound, [FlNm]);
    end;
    SetupRpField(C, S, pF);
  end
  else
  begin
    pF^.Zero := True;
    pF^.Tp := flNumber;
  end;
  pF^.Name:=aName;
  pF^.Func:=aFunc;
  pF^.Visible := True;
  pF^.Id := i;
  FFields.Add(Result);
end;     }

{ TRpCalcFieldList }

function TRpCalcFieldList.GetFields(Index: Integer): PRpCalcField;
begin
  Result := PRpCalcField(Items[Index]);
end;

function TRpCalcFieldList.AddField(var pF: PRpCalcField): Integer;
begin
  New(pF);
  pF^.Id:=0;
  pF^.Name := '';
  pF^.Expr := '';
  pF^.Tp := flText;
  pF^.Size := 200;
  Result := Add(pF);
end;

procedure TRpCalcFieldList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(PRpCalcField(Items[i]));
  inherited Clear;
end;

function TRpCalcFieldList.FindField(Id: Integer): PRpCalcField;
var
  i: Integer;
  pF: PRpCalcField;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pF := Fields[i];
    if pF^.Id = Id then Exit(pF);
  end;
end;

function TRpCalcFieldList.FindFieldByName(const S: String): PRpCalcField;
var
  i: Integer;
  pF: PRpCalcField;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pF := Fields[i];
    if MyUtf8CompareText(pF^.Name, S) = 0 then Exit(pF);
  end;
end;

function TRpCalcFieldList.FindFieldByNameDS(const S: String): PRpCalcField;
var
  i: Integer;
  pF: PRpCalcField;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pF := Fields[i];
    if MyUtf8CompareText('cf' + IntToStr(pF^.Id), S) = 0 then Exit(pF);
  end;
end;

{ TSQLField }

procedure TSQLField.CopyFrom(SrcF: TSQLField);
begin
  FName := SrcF.Name;
  FFieldNameDS := SrcF.FieldNameDS;
  FTp := SrcF.Tp;
  FDisplayFormat := SrcF.DisplayFormat;
end;

{ TSQLFieldList }

function TSQLFieldList.GetFields(Index: Integer): TSQLField;
begin
  Result := TSQLField(Items[Index]);
end;

function TSQLFieldList.AddField: TSQLField;
begin
  Result := TSQLField.Create;
  Add(Result);
end;

{function TSQLFieldList.InsertFromDataSetField(AIndex: Integer; F: TField
  ): TSQLField;
begin
  Result := TSQLField.Create;
  Result.FieldNameDS := F.FieldName;
  Result.Name := F.FieldName;
  Result.Tp := FieldTypeToRpFieldType(F);
  Insert(AIndex, Result);
end;    }

function TSQLFieldList.FindFieldDS(const FieldNameDS: String): TSQLField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Fields[i].FieldNameDS, FieldNameDS) = 0 then
      Exit(Fields[i]);
end;

function TSQLFieldList.FindByName(const FieldName: String): TSQLField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Utf8CompareText(Fields[i].Name, FieldName) = 0 then
      Exit(Fields[i]);
end;

procedure TSQLFieldList.DeleteField(AIndex: Integer);
begin
  Fields[AIndex].Free;
  Delete(AIndex);
end;

procedure TSQLFieldList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Fields[i].Free;
  inherited Clear;
end;

procedure TSQLFieldList.CopyFrom(SourceList: TSQLFieldList);
var
  i: Integer;
  F: TSQLField;
begin
  Clear;
  for i := 0 to SourceList.Count - 1 do
  begin
    F := AddField;
    F.CopyFrom(SourceList[i]);
  end;
end;

{ TRpColoringList }

function TRpColoringList.GetColorings(Index: Integer): TRpColoringData;
begin
  Result := TRpColoringData(Items[Index]);
end;

function TRpColoringList.AddColoring: TRpColoringData;
begin
  Result := TRpColoringData.Create;
  Add(Result);
end;

function TRpColoringList.FindColoring(const FieldName: String): TRpColoringData;
var
  i: Integer;
  CD: TRpColoringData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    CD := Colorings[i];
    if CompareText(CD.FieldNameDS, FieldName) = 0 then Exit(CD);
  end;
end;

function TRpColoringList.FindColoringIndex(AColor: TColor; const AFieldNameDS,
  Expr: String): Integer;
var
  i: Integer;
  CD: TRpColoringData;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    CD := Colorings[i];
    if (CD.Color = AColor) and (CD.FieldNameDS = AFieldNameDS) and (CD.Expr = Expr) then
      Exit(i);
  end;
end;

procedure TRpColoringList.DeleteColoring(CD: TRpColoringData);
begin
  Remove(CD);
  CD.Free;
end;

procedure TRpColoringList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Colorings[i].Free;
  inherited Clear;
end;

{ TdxQueryGrid }

function TdxQueryGrid.GetFields(aName: String): Variant;
var
  RD: TReportData;
  C: TRpGridColumn;
begin
  RD := ReportMan.FindReport(FId);
  C := RD.Grid.FindColumnByTitle(aName);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [aName]);
  RequeryIfNeed;
  Result := DataSource.DataSet.FieldByName(C.FieldNameDS).Value;
end;

function TdxQueryGrid.GetEditable: Boolean;
begin
  Result := ReportMan.FindReport(FId).IsSimple;
end;

constructor TdxQueryGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  VisibleButtons := [gbnAppend, gbnEdit, gbnDelete, gbnRefresh, gbnGoto];
end;

procedure TdxQueryGrid.MoveBy(Distance: Integer);
begin
  RequeryIfNeed;
  DataSource.DataSet.MoveBy(Distance);
end;

procedure TdxQueryGrid.MoveFirst;
begin
  RequeryIfNeed;
  DataSource.DataSet.First;
end;

procedure TdxQueryGrid.MovePrior;
begin
  RequeryIfNeed;
  DataSource.DataSet.Prior;
end;

procedure TdxQueryGrid.MoveNext;
begin
  RequeryIfNeed;
  DataSource.DataSet.Next;
end;

procedure TdxQueryGrid.MoveLast;
begin
  RequeryIfNeed;
  DataSource.DataSet.Last;
end;

procedure TdxQueryGrid.MoveTo(aRecNo: Integer);
begin
  RequeryIfNeed;
  MoveBy(aRecNo - RecNo);
end;

function TdxQueryGrid.EOF: Boolean;
begin
  RequeryIfNeed;
  Result := DataSource.DataSet.EOF;
end;

function TdxQueryGrid.BOF: Boolean;
begin
  RequeryIfNeed;
  Result := DataSource.DataSet.BOF;
end;

function TdxQueryGrid.RecNo: Integer;
begin
  RequeryIfNeed;
  Result := DataSource.DataSet.RecNo;
end;

function TdxQueryGrid.RecId: Integer;
begin
  RequeryIfNeed;
  Result := DataSource.DataSet.FieldByName('id').AsInteger;
end;

procedure TdxQueryGrid.EnableControls;
begin
  DataSource.DataSet.EnableControls;
end;

procedure TdxQueryGrid.DisableControls;
begin
  DataSource.DataSet.DisableControls;
end;

function TdxQueryGrid.ControlsDisabled: Boolean;
begin
  Result := DataSource.DataSet.ControlsDisabled;
end;

function TdxQueryGrid.RecordCount: Integer;
begin
  RequeryIfNeed;
  Result := DataSource.DataSet.RecordCount;
end;

function TdxQueryGrid.Locate(const FieldNames: String;
  FieldValues: array of Variant; aOptions: TLocateOptions): Boolean;
var
  RD: TReportData;
  C: TRpGridColumn;
  VArr: Variant;
  i: Integer;
  S: String;
  SL: TStringList;
begin
  RequeryIfNeed;
  RD := ReportMan.FindReport(FId);
  S := '';
  SL := TStringList.Create;
  try
    SplitStr(FieldNames, ';', SL);
    for i := 0 to SL.Count - 1 do
    begin
      C := RD.Grid.FindColumnByTitle(SL[i]);
      if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [SL[i]]);
      S := S + C.FieldNameDS;
      if i < SL.Count - 1 then S := S + ';';
    end;
    VArr := VarArrayOf(FieldValues);
    Result := DataSource.DataSet.Locate(S, VArr, aOptions);
  finally
    SL.Free;
    VarClear(VArr);
  end;
end;

function TdxQueryGrid.GotoRecord(aRecId: Integer): Boolean;
begin
  RequeryIfNeed;
  Result := DataSource.DataSet.Locate('id', aRecId, []);
end;

procedure TdxQueryGrid.Refresh;
begin
  SortColsToRpGridSortCols;
  if FRpWnd = nil then
    TDataSetProcessor(FDSP).RequeryQuery(FQRi, 0, True)
  else
    TReportWindow(FRpWnd).RefreshReport;
end;

procedure TdxQueryGrid.Close;
begin
  TDataSetProcessor(FDSP).Queries[FQRi]^.DataSet.Close;
end;

procedure TdxQueryGrid.DisableScrollEvents;
begin
  if FScrollEventsCounter = 0 then
  begin
    FOldAfterScroll := DataSource.DataSet.AfterScroll;
	  FOldBeforeScroll := DataSource.DataSet.BeforeScroll;
    DataSource.DataSet.AfterScroll := nil;
    DataSource.DataSet.BeforeScroll := nil;
  end;
  Inc(FScrollEventsCounter);
end;

procedure TdxQueryGrid.EnableScrollEvents;
begin
  if FScrollEventsCounter = 0 then Exit;
  Dec(FScrollEventsCounter);
  if FScrollEventsCounter = 0 then
  begin
    DataSource.DataSet.AfterScroll := FOldAfterScroll;
    DataSource.DataSet.BeforeScroll := FOldBeforeScroll;
  end;
end;

function TdxQueryGrid.GetAsDT(Index: String): TDateTime;
begin
  Result := Nz(Fields[Index], 0);
end;

function TdxQueryGrid.GetAsF(Index: String): Extended;
begin
  Result := Nz(Fields[Index], 0);
end;

function TdxQueryGrid.GetAsI(Index: String): Integer;
begin
  Result := Nz(Fields[Index], 0);
end;

function TdxQueryGrid.GetAsS(Index: String): String;
begin
  Result := Nz(Fields[Index], '');
end;

function TdxQueryGrid.GetQueryName: String;
begin
  Result := ReportMan.FindReport(FId).Name;
end;

procedure TdxQueryGrid.Paint;
var
  TS: TTextStyle;
  RD: TReportData;
begin
  inherited Paint;
  if csDesigning in ComponentState then
  begin
    TS := Canvas.TextStyle;
    TS.Alignment := taCenter;
    TS.Layout := tlCenter;
    TS.SingleLine := False;
    TS.WordBreak := True;
    RD := ReportMan.FindReport(FId);
    Canvas.Font := RD.Grid.Font;
    Canvas.Font.Color := clWindowText;
    Canvas.TextRect(ClientRect, 0, 0, RD.Name, TS);
  end;
end;

procedure TdxQueryGrid.RequeryIfNeed;
var
  Q: TQueryRec;
begin
  if FDSP = nil then Exit; // Если отчет
  with TDataSetProcessor(FDSP) do
  begin
    Q := Queries[FQRi]^;
    if not FManualRefresh and Q.NeedRefresh then
    begin
      if (Q.DSRi > 0) and DataSets[Q.DSRi]^.NeedRefresh then
        RequeryDetail(Q.DSRi);
      RequeryQuery(FQRi);
      // Хотя в RequeryQuery флаг сбрасывается, его может снова установить
      // родительский запрос
      Queries[FQRi]^.NeedRefresh := False;
    end;
  end;
end;

function TdxQueryGrid.ScrollEventsDisabled: Boolean;
begin
  Result := FScrollEventsCounter > 0;
end;

procedure TdxQueryGrid.SortColsToRpGridSortCols;
var
  RD: TReportData;
  CD: TSortColumn;
  C: TRpGridColumn;
  i: Integer;
begin
  if FDSP <> nil then
    RD := TDataSetProcessor(FDSP).Queries[FQRi]^.RD
  else
    RD := TReportWindow(FRpWnd).RD;
  RD.Grid.SortCols.Clear;
  for i := 0 to SortCols.Count - 1 do
  begin
    CD := SortCols[i];
    C := RD.Grid.FindColumnByFieldName(TColumn(CD.Col).FieldName);
    RD.Grid.SortCols.AddCol(C, CD.Desc);
  end;
end;

{ TRpTotalList }

function TRpTotalList.GetTotals(Index: Integer): TRpTotalData;
begin
  Result := TRpTotalData(Items[Index]);
end;

function TRpTotalList.AddTotal: TRpTotalData;
begin
  Result := TRpTotalData.Create;
  Add(Result);
end;

function TRpTotalList.FindTotal(const AFieldNameDS: String): TRpTotalData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(AFieldNameDS, Totals[i].FieldNameDS) = 0 then Exit(Totals[i]);
end;

procedure TRpTotalList.RemoveTotal(T: TRpTotalData);
begin
  Remove(T);
  T.Free;
end;

procedure TRpTotalList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Totals[i].Free;
  inherited Clear;
end;

{ TRpGridSortList }

function TRpGridSortList.GetCols(Index: Integer): TRpGridSortData;
begin
  Result := TRpGridSortData(Items[Index]);
end;

function TRpGridSortList.AddCol(Col: TRpGridColumn; aDesc: Boolean
  ): TRpGridSortData;
begin
  Result := TRpGridSortData.Create;
  Result.Col := Col;
  Result.Desc := aDesc;
  Add(Result);
end;

function TRpGridSortList.FindCol(Col: TRpGridColumn): TRpGridSortData;
var
  i: Integer;
  CD: TRpGridSortData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    CD := Cols[i];
    if CD.Col = Col then Exit(CD);
  end;
end;

procedure TRpGridSortList.RemoveCol(CD: TRpGridSortData);
begin
  Remove(CD);
  CD.Free;
end;

procedure TRpGridSortList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Cols[i].Free;
  inherited Clear;
end;

{ TSQLReportTotalsFilter }

{function TSQLReportTotalsFilter.FieldNameParse(const aFieldName: String
  ): String;
var
  pFl: PRpField;
  idx: Integer;
begin
  Result := '';
  pFl := RD.Sources[0]^.Fields.FindFieldByName(aFieldName);
  if pFl = nil then Exit;
  idx := RD.Sources[0]^.Fields.IndexOf(pFl);
  if pFl^.Func <> tfNone then
    Result := GetFuncSql(pFl^, idx);
end;

function TSQLReportTotalsFilter.CheckValue(var Value: String): Boolean;
var
  FS: TFormatSettings;
  E: Extended;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator:='.';
  Result:=TryStrToFloat(Value, E, FS);
  if Result then Value := FloatToStr(E, FS);
end; }

{ TRpGridColumn }

{function TRpGridColumn.GetFieldId: Integer;
begin
  if not IsCalcField then
  	Result := StrToInt(Copy(FFieldNameDS, 2, 100))
  else
		Result := StrToInt(Copy(FFieldNameDS, 3, 100))
end;      }

constructor TRpGridColumn.Create;
begin
  FFont := TFont.Create;
  FTitleFont := TFont.Create;
  FVisible := True;
  FAutoAlignment := True;
  FAutoLayout := True;
  FTitleLayout := tlCenter;
end;

destructor TRpGridColumn.Destroy;
begin
  FTitleFont.Free;
  FFont.Free;
  inherited Destroy;
end;

{function TRpGridColumn.IsCalcField: Boolean;
begin
  Result := LowerCase(Copy(FFieldNameDS, 1, 2)) = 'cf';
end;    }

{ TRpGrid }

function TRpGrid.GetColumns(Index: Integer): TRpGridColumn;
begin
  Result := TRpGridColumn(FColumns[Index]);
end;

constructor TRpGrid.Create;
begin
  FFont := TFont.Create;
  FFont.Name:='Verdana';
  FFont.Size := 10;
  FTitleFont := TFont.Create;
  FTitleFont.Name:='Verdana';
  FTitleFont.Size := 10;
  FColumns := TList.Create;
  FSortCols := TRpGridSortList.Create;
  FGridLineColor:=clSilver;
  FGridLineStyle:=psSolid;
  FFixedColor:=clBtnFace;
  //FFixedHotColor:=cl3DLight;
  FSelectedColor:= clHighlight;
  FSelectedTextColor:=clHighlightText;
  FInactiveSelectedColor:=clSilver;
  FInactiveSelectedTextColor:=clBlack;
  FColor := clWindow;
  FAlternateColor:=FColor;
  FShowHints:=True;
  FThumbTracking:=True;
  FAllowChangeSort:=True;
  FIndicator := True;
end;

destructor TRpGrid.Destroy;
begin
  FSortCols.Free;
  ClearList(FColumns);
  FColumns.Free;
  FFont.Free;
  FTitleFont.Free;
  inherited Destroy;
end;

function TRpGrid.ColumnCount: Integer;
begin
  Result := FColumns.Count;
end;

function TRpGrid.AddColumn: TRpGridColumn;
begin
  Result := TRpGridColumn.Create;
  Result.Color:=clWindow;
  Result.FixedColor:=clBtnFace;
  FColumns.Add(Result);
end;

function TRpGrid.AddDefaultColumn: TRpGridColumn;
begin
  Result := AddColumn;
  Result.Index := ColumnCount - 1;
  Result.Width := ScaleToScreen(100);
  Result.Color := Color;
  Result.FixedColor := FixedColor;
  Result.Font.Assign(Font);
  Result.TitleFont.Assign(TitleFont);
end;

function TRpGrid.FindColumnByFieldName(const FieldName: String): TRpGridColumn;
var
  i: Integer;
  C: TRpGridColumn;
begin
  Result := nil;
  for i := 0 to ColumnCount - 1 do
  begin
    C := Columns[i];
    if CompareText(C.FieldNameDS, FieldName) = 0 then Exit(C);
  end;
end;

function TRpGrid.FindColumnByTitle(const S: String): TRpGridColumn;
var
  i: Integer;
  C: TRpGridColumn;
begin
  Result := nil;
  for i := 0 to ColumnCount - 1 do
  begin
    C := Columns[i];
    if MyUtf8CompareText(C.Caption, S) = 0 then Exit(C);
  end;
end;

function TRpGrid.FindColumnIndex(Col: TRpGridColumn): Integer;
begin
  Result := FColumns.IndexOf(Col);
end;

procedure TRpGrid.DeleteColumn(Col: TRpGridColumn);
var
  SC: TRpGridSortData;
  C: TRpGridColumn;
  i: Integer;
begin
  SC := FSortCols.FindCol(Col);
  for i := 0 to ColumnCount - 1 do
  begin
    C := Columns[i];
    if C.Index > Col.Index then C.Index := C.Index - 1;
  end;
  if SC <> nil then
    FSortCols.RemoveCol(SC);
  FColumns.Remove(Col);
  Col.Free;
end;

procedure TRpGrid.SortColumns(L: TList);
var
  i: Integer;

  procedure AddToList(C: TRpGridColumn);
  var
    j: Integer;
  begin
    for j := 0 to L.Count - 1 do
      if C.Index < TRpGridColumn(L[j]).Index then
      begin
        L.Insert(j, C);
        Exit;
      end;
    L.Add(C);
  end;

begin
  for i := 0 to ColumnCount - 1 do
    AddToList(Columns[i]);
end;

procedure TRpGrid.ClearColumns;
begin
  ClearList(FColumns);
end;

{ TRpReader }

function TRpReader.GetColor(Atts: TSAXAttributes; const aName: String;
  DefaultColor: TColor): TColor;
var
  S: SAXString;
begin
  Result := clDefault;
  S := Atts.GetValue('', aName);
  if S <> '' then Result := StringToColor(S)
  else Result := DefaultColor;
end;

function TRpReader.GetPenStyle(Atts: TSAXAttributes; const aName: String
  ): TPenStyle;
var
  S: SAXString;
begin
  Result := psSolid;
  S := Atts.GetValue('', aName);
  if S = 'psDash' then Result := psDash
  else if S = 'psDot' then Result := psDot;
end;

procedure TRpReader.ReadSortCols;
var
  SL: TStringList;
  i{, n}: Integer;
  S: String;
  Desc: Boolean;
  Col: TRpGridColumn;
begin
  SL := TStringList.Create;
  SplitStr(RD.SortOrder, ';', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    if S = '' then Continue;
    Desc := False;
    if S[1] = '*' then
    begin
      Desc := True;
      Delete(S, 1, 1);
    end;
    if S[1] in ['0'..'9'] then S := 'f' + S;
    Col := RD.Grid.FindColumnByFieldName(S);
    //n := StrToInt(S);
    //Col := RD.Grid.FindColumnByFieldName('f' + IntToStr(n));
    if Col <> nil then
      RD.Grid.SortCols.AddCol(Col, Desc);
  end;
  SL.Free;
end;

procedure TRpReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  pS: PRpSource;
  pF: PRpField;
  G: TRpGrid;
  C: TRpGridColumn;
  Fnt: TFont;
  T: TRpTotalData;
  CD: TRpColoringData;
  pCalcF: PRpCalcField;
  SqlF: TSQLField;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'source' then
  begin
    RD.Sources.AddSource(pS);
    pS^.Kind:=TRpSourceKind(GetInt(Atts, 'kind'));
    pS^.Id:=GetInt(Atts, 'id');
    pS^.TId := GetInt(Atts, 'tid');
    pS^.Filter := XmlToStr(Atts.GetValue('', 'filter'));
    FSrc := pS;
  end
  else if LocalName = 'field' then
  begin
    pS := FSrc;
    if FParent = nil then
    begin
      pS^.Fields.AddField(pF);
      FParent := pF;
    end
    else
    begin
      pF := NewRpField;
      FParent^.Src:=pF;
      pF^.Parent:=FParent;
      FParent := pF;
    end;
    pF^.Name := Atts.GetValue('', 'name');
    pF^.Tp:= TRpFieldType(GetInt(Atts, 'type'));
    pF^.TId := GetInt(Atts, 'tid');
    pF^.FId := GetInt(Atts, 'fid');
    pF^.Param:=GetBool(Atts, 'param');
    pF^.Visible := GetBool(Atts, 'visible');
    pF^.No:=GetBool(Atts, 'not');
    pF^.Nul:=GetBool(Atts, 'null');
    pF^.Zero:=GetBool(Atts, 'zero');
    pF^.Value := Atts.GetValue('', 'value');
    pF^.Func := TRpTotalFunc(GetInt(Atts, 'func'));
    pF^.Id := GetInt(Atts, 'id');
  end
  else if LocalName = 'sqlfield' then
  begin
    SqlF := RD.SqlFields.AddField;
    SqlF.Name := XmlToStr(GetStr(Atts, 'name'));
    SqlF.FieldNameDS := XmlToStr(GetStr(Atts, 'field'));
    SqlF.Tp := TRpFieldType(GetInt(Atts, 'tp'));
    SqlF.DisplayFormat := XmlToStr(GetStr(Atts, 'format'));
  end
  else if LocalName = 'reportdata' then
  begin
    RD.Id:= GetInt(Atts, 'id');
    RD.Name:=Atts.GetValue('', 'name');
    RD.SortOrder:=Atts.GetValue('', 'sortorder');
    RD.Kind:=TReportKind(GetInt(Atts, 'kind'));
    RD.DateField:=GetInt(Atts, 'datefield');
    RD.DateDetail:=TRpDateDetail(GetInt(Atts, 'datedetail'));
    RD.FirstRecordCount:=GetInt(Atts, 'first');
    RD.Filter := XmlToStr(Atts.GetValue('', 'filter'));
    RD.HelpText:=XmlToHtml(Atts.GetValue('', 'helptext'));
    RD.SQL := XmlToStr(GetStr(Atts, 'sql'));
    RD.SqlMode := GetBool(Atts, 'sqlmode');
    RD.Version := GetInt(Atts, 'version');
  end
  else if LocalName = 'grid' then
  begin
    G := RD.Grid;
    G.Color:=GetColor(Atts, 'color', clWindow);
    G.AlternateColor:=GetColor(Atts, 'alternatecolor', clWindow);
    G.SelectedColor:=GetColor(Atts, 'selectedcolor', clHighlight);
    G.SelectedTextColor:=GetColor(Atts, 'selectedtextcolor', clHighlightText);
    G.InactiveSelectedColor:=GetColor(Atts, 'inactiveselectedcolor', clSilver);
    G.InactiveSelectedTextColor:=GetColor(Atts, 'inactiveselectedtextcolor', clBlack);
    G.FixedColor:=GetColor(Atts, 'fixedcolor', clBtnFace);
    //G.FixedHotColor:=GetColor(Atts, 'fixedhotcolor', cl3DLight);
    G.GridLineColor:=GetColor(Atts, 'gridlinecolor', clSilver);
    G.GridLineStyle:=GetPenStyle(Atts, 'gridlinestyle');
    G.DefaultRowHeight:=GetInt(Atts, 'defaultrowheight');
    G.VertLines:=GetBool(Atts, 'vertlines');
    G.HorzLines:=GetBool(Atts, 'horzlines');
    G.Flat:=GetBool(Atts, 'flat');
    G.WordWrap:=GetBool(Atts, 'wordwrap');
    G.RowSelect:=GetBool(Atts, 'rowselect');
    G.RowHighlight:=GetBool(Atts, 'rowhighlight');
    G.CellEllipsis:=GetBool(Atts, 'cellellipsis');
    G.ShowHints:=GetBool(Atts, 'showhints');
    G.ThumbTracking:=GetBool(Atts, 'thumbtracking');
    G.ColMove:=GetBool(Atts, 'colmove');
    G.AllowChangeSort:=GetBool(Atts, 'allowchangesort');
    G.Indicator:=GetBool(Atts, 'indicator', True);
    G.ShowRowDeleteButton:=GetBool(Atts, 'rowdelbn', False);
  end
  else if LocalName = 'column' then
  begin
    C := RD.Grid.AddColumn;
    C.Color := GetColor(Atts, 'color', clWindow);
    C.FixedColor:=GetColor(Atts, 'fixedcolor', clBtnFace);
    C.Width:=GetInt(Atts, 'width');
    C.FieldNameDS:=Atts.GetValue('', 'fieldname');
    C.Caption := Atts.GetValue('', 'caption');
    C.Index := GetInt(Atts, 'index');
    if Atts.GetValue('', 'visible') <> '' then
      C.Visible:=GetBool(Atts, 'visible');
    if Atts.GetValue('', 'autoalignment') <> '' then
    begin
      C.AutoAlignment:=GetBool(Atts, 'autoalignment');
      C.Alignment:=TAlignment(GetInt(Atts, 'alignment'));
    end;
    if Atts.GetValue('', 'autolayout') <> '' then
    begin
      C.AutoLayout:=GetBool(Atts, 'autolayout');
      C.Layout:=TTextLayout(GetInt(Atts, 'layout'));
    end;
    if Atts.GetValue('', 'titlealignment') <> '' then
      C.TitleAlignment:=TAlignment(GetInt(Atts, 'titlealignment'));
    if Atts.GetValue('', 'titlelayout') <> '' then
      C.TitleLayout:=TTextLayout(GetInt(Atts, 'titlelayout'));
    FColumn := C;
  end
  else if (LocalName = 'font') or (LocalName = 'titlefont') then
  begin
    if FColumn <> nil then
    begin
      if LocalName = 'font' then Fnt := FColumn.Font
      else if LocalName = 'titlefont' then Fnt := FColumn.TitleFont;
    end
    else
    begin
      if LocalName = 'font' then Fnt := RD.Grid.Font
      else if LocalName = 'titlefont' then Fnt := RD.Grid.TitleFont;
    end;
    Fnt.Name := Atts.GetValue('', 'name');
    Fnt.Height:=GetInt(Atts, 'height');
    // Со временем убрать
    if Fnt.Height = 0 then
      Fnt.Size:=GetInt(Atts, 'size');
    Fnt.Color := GetColor(Atts, 'color', clBlack);
    if GetBool(Atts, 'bold') then Fnt.Style := Fnt.Style + [fsBold];
    if GetBool(Atts, 'italic') then Fnt.Style := Fnt.Style + [fsItalic];
    if GetBool(Atts, 'underline') then Fnt.Style := Fnt.Style + [fsUnderline];
    if GetBool(Atts, 'strikeout') then Fnt.Style := Fnt.Style + [fsStrikeOut];
  end
  else if LocalName = 'calcfield' then
  begin
    RD.CalcFields.AddField(pCalcF);
    pCalcF^.Name := Atts.GetValue('', 'name');
    pCalcF^.Expr := XmlToStr(Atts.GetValue('', 'expression'));
    pCalcF^.Id := GetInt(Atts, 'id');
    pCalcF^.Size := GetInt(Atts, 'size');
    pCalcF^.Tp := TRpFieldType(GetInt(Atts, 'tp'));
    if pCalcF^.Tp = flNone then
    begin
      pCalcF^.Tp := flText;
      pCalcF^.Size := 200;
    end;
  end
  else if LocalName = 'total' then
  begin
    T := RD.Totals.AddTotal;
    T.Caption := Atts.GetValue('', 'caption');
    T.FieldNameDS:=Atts.GetValue('', 'field');
    T.Func:=TRpTotalFunc(GetInt(Atts, 'func'));
  end
  else if LocalName = 'coloringdata' then
  begin
    CD := RD.Coloring.AddColoring;
    CD.Color:=StringToColor(Atts.GetValue('', 'color'));
    CD.FieldNameDS:=XmlToStr(Atts.GetValue('', 'fieldname'));
    CD.Expr:=XmlToStr(Atts.GetValue('', 'expression'));
  end
  else if LocalName = 'template' then
    RD.Templates.Add(XmlToStr(GetStr(Atts, 'filename')))
  else if LocalName = 'printfield' then
    RD.PrintFields.Add(XmlToStr(GetStr(Atts, 'name')) + '=' +
      XmlToStr(GetStr(Atts, 'value')))
end;

procedure TRpReader.DoEndElement(const NamespaceURI, LocalName, QName: SAXString
  );
begin
  inherited DoEndElement(NamespaceURI, LocalName, QName);
  if LocalName = 'field' then
  begin
    if FParent <> nil then
      FParent := FParent^.Parent;
  end
  else if LocalName = 'column' then
    FColumn := nil
  else if LocalName = 'grid' then
    ReadSortCols;
end;

{ TRpSourceList }

function TRpSourceList.GetSources(Index: Integer): PRpSource;
begin
  Result := PRpSource(Items[Index]);
end;

function TRpSourceList.AddSource(var S: PRpSource): Integer;
begin
  New(S);
  FillChar(S^, SizeOf(TRpSource), 0);
  S^.Fields := TRpFieldList.Create;
  S^.Filter := '';
  Result := Add(S);
end;

procedure TRpSourceList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Sources[i]^.Fields.Free;
    Dispose(Sources[i]);
  end;
  inherited Clear;
end;

{ TRpFieldList }

function TRpFieldList.GetFields(Index: Integer): PRpField;
begin
  Result := PRpField(Items[Index]);
end;

function TRpFieldList.AddField(var F: PRpField): Integer;
begin
  F := NewRpField;
  Result := Add(F);
end;

procedure DisposeField(F: PRpField);
begin
  if F^.Src <> nil then
  begin
    DisposeField(F^.Src);
    Dispose(F^.Src);
  end;
end;

procedure TRpFieldList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    DisposeField(Fields[i]);
    Dispose(Fields[i]);
  end;
  inherited Clear;
end;

function TRpFieldList.FindField(Id: Integer): PRpField;
var
  i: Integer;
  pF: PRpField;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pF := Fields[i];
    if pF^.Id = Id then Exit(pF);
  end;
end;

function TRpFieldList.FindFieldByName(const S: String): PRpField;
var
  i: Integer;
  pF: PRpField;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pF := Fields[i];
    if MyUtf8CompareText(S, pF^.Name) = 0 then Exit(pF);
  end;
end;

{ TReportData }

constructor TReportData.Create;
begin
  FDateField := -1;
  FSources := TRpSourceList.Create;
  FGrid := TRpGrid.Create;
  FGrid.Color := clWindow;
  FGrid.AlternateColor:=clWindow;
  FGrid.SelectedColor:=clHighlight;
  FGrid.FixedColor:=clBtnFace;
  FGrid.GridLineColor:=clSilver;
  FGrid.DefaultRowHeight:=GetTextHeight('Fj') + 5;// 20;
  FGrid.Flat:=False;
  FGrid.GridLineStyle:=psSolid;
  FGrid.VertLines:=True;
  FGrid.HorzLines:=True;
  FCalcFields := TRpCalcFieldList.Create;
  FTotals := TRpTotalList.Create;
  FColoring := TRpColoringList.Create;
  FTemplates := TStringList.Create;
  FPrintFields := TStringList.Create;
  FSQLFields := TSQLFieldList.Create;
end;

destructor TReportData.Destroy;
begin
  FSQLFields.Free;
  FPrintFields.Free;
  FTemplates.Free;
  FColoring.Free;
  FTotals.Free;
  FCalcFields.Free;
  FGrid.Free;
  FSources.Free;
  inherited Destroy;
end;

procedure TReportData.SaveToStream(St: TStream);

  procedure WrStr(const S: String);
  begin
    St.Write(Pointer(S)^, Length(S));
  end;

  procedure WrFld(F: TRpField);
  begin
    WrStr('<field name="' + F.Name + '" type="' + IntToStr(Ord(F.Tp)) +
      '" tid="' + IntToStr(F.TId) + '" fid="' + IntToStr(F.FId) + '" param="' + Bool2Str(F.Param) +
      '" visible="' + Bool2Str(F.Visible) + '" not="' + Bool2Str(F.No) +
      '" null="' + Bool2Str(F.Nul) + '" zero="' + Bool2Str(F.Zero) +
      '" value="' + F.Value + '" func="' + IntToStr(Ord(F.Func)) +
      '" id="' + IntToStr(F.Id) + '">');
    if F.Src <> nil then
      WrFld(F.Src^);
    WrStr('</field>');
  end;

  procedure WrSrc(S: TRpSource);
  var
    i: Integer;
  begin
    WrStr('<source kind="' + IntToStr(Ord(S.Kind)) + '" id="' + IntToStr(S.Id) +
      '" tid="' + IntToStr(S.TId) + '" filter="' + StrToXml(S.Filter) + '">');
    WrStr('<fields>');
    for i := 0 to S.Fields.Count - 1 do
      WrFld(S.Fields[i]^);
    WrStr('</fields>');
    WrStr('</source>');
  end;

  function PenStyle2Str(PS: TPenStyle): String;
  const
    PSStr: array [TPenStyle] of String = ('psSolid', 'psDash', 'psDot',
      'psDashDot', 'psDashDotDot', 'psinsideFrame', 'psPattern', 'psClear');
  begin
    Result := PSStr[PS];
  end;

  procedure WrFont(F: TFont; const TagName: String);
  begin
    WrStr('<' + TagName + ' name="' + F.Name +
      '" height="' + IntToStr(F.Height) +
      '" color="' + ColorToString(F.Color) + '" bold="' + Bool2Str(fsBold in F.Style) +
      '" italic="' + Bool2Str(fsItalic in F.Style) + '" underline="' +
      Bool2Str(fsUnderline in F.Style) + '" strikeout="' +
      Bool2Str(fsStrikeOut in F.Style) + '"/>');
  end;

  procedure WrColumn(C: TRpGridColumn);
  begin
    WrStr('<column color="' + ColorToString(C.Color) + '" fixedcolor="' +
      ColorToString(C.FixedColor) + '" caption="' + C.Caption +
      '" width="' + IntToStr(C.Width) + '" fieldname="' + C.FieldNameDS +
      '" index="' + IntToStr(C.Index) + '" visible="' + Bool2Str(C.Visible));
    if not C.AutoAlignment then
      WrStr('" autoalignment="0" alignment="' + IntToStr(Ord(C.Alignment)));
    if not C.AutoLayout then
      WrStr('" autolayout="0" layout="' + IntToStr(Ord(C.Layout)));
    if C.TitleAlignment <> taLeftJustify then
      WrStr('" titlealignment="' + IntToStr(Ord(C.TitleAlignment)));
    if C.TitleLayout <> tlCenter then
      WrStr('" titlelayout="' + IntToStr(Ord(C.TitleLayout)));
    WrStr('">');
    WrFont(C.Font, 'font');
    WrFont(C.TitleFont, 'titlefont');
    WrStr('</column>');
  end;

  procedure WrGrid(G: TRpGrid);
  var
    i: Integer;
  begin
    WrStr('<grid color="' + ColorToString(G.Color) + '" alternatecolor="' +
      ColorToString(G.AlternateColor) + '" selectedcolor="' + ColorToString(G.SelectedColor) +
      '" selectedtextcolor="' + ColorToString(G.SelectedTextColor) +
      '" inactiveselectedcolor="' + ColorToString(G.InactiveSelectedColor) +
      '" inactiveselectedtextcolor="' + ColorToString(G.InactiveSelectedTextColor) +
      '" fixedcolor="' + ColorToString(G.FixedColor) +
      {'" fixedhotcolor="' + ColorToString(G.FixedHotColor) + }
      '" gridlinecolor="' + ColorToString(G.GridLineColor) +
      '" gridlinestyle="' + PenStyle2Str(G.GridLineStyle) +
      '" defaultrowheight="' + IntToStr(G.DefaultRowHeight) +
      '" vertlines="' + Bool2Str(G.VertLines) +
      '" horzlines="' + Bool2Str(G.HorzLines) +
      '" flat="' + Bool2Str(G.Flat) +
      '" wordwrap="' + Bool2Str(G.WordWrap) +
      '" rowselect="' + Bool2Str(G.RowSelect) +
      '" rowhighlight="' + Bool2Str(G.RowHighlight) +
      '" cellellipsis="' + Bool2Str(G.CellEllipsis) +
      '" showhints="' + Bool2Str(G.ShowHints) +
      '" thumbtracking="' + Bool2Str(G.ThumbTracking) +
      '" colmove="' + Bool2Str(G.ColMove) +
      '" allowchangesort="' + Bool2Str(G.AllowChangeSort) +
      '" indicator="' + Bool2Str(G.Indicator) +
      '" rowdelbn="' + Bool2Str(G.ShowRowDeleteButton) +
      '">');
    WrFont(G.Font, 'font');
    WrFont(G.TitleFont, 'titlefont');
    WrStr('<columns>');
    for i := 0 to G.ColumnCount - 1 do
      WrColumn(G.Columns[i]);
    WrStr('</columns></grid>');
  end;

  procedure WrCalcFields;
  var
    i: Integer;
    CF: TRpCalcField;
  begin
    WrStr('<calcfields>');
    for i := 0 to FCalcFields.Count - 1 do
    begin
      CF := FCalcFields[i]^;
      WrStr('<calcfield name="' + StrToXml(CF.Name) + '" expression="' +
        StrToXml(CF.Expr) + '" id="' + IntToStr(CF.Id) +
        '" tp="' + IntToStr(Ord(CF.Tp)) + '" size="' + IntToStr(CF.Size) + '"/>');
    end;
    WrStr('</calcfields>');
  end;

  procedure WrSortCols;
  var
    S, Tmp: String;
    i: Integer;
    Col: TRpGridSortData;
  begin
    S := '';
    for i := 0 to Grid.SortCols.Count - 1 do
    begin
      Col := Grid.SortCols[i];
      Tmp := Col.Col.FieldNameDS;
      if Col.Desc then Tmp := '*' + Tmp;
      S := S + Tmp;
      if i < Grid.SortCols.Count - 1 then S := S + ';';
    end;
    SortOrder := S;
  end;

  procedure WrTotals;
  var
    i: Integer;
    T: TRpTotalData;
  begin
    WrStr('<totals>');
    for i := 0 to FTotals.Count - 1 do
    begin
      T := FTotals[i];
      WrStr('<total caption="' + T.Caption + '" field="' + T.FieldNameDS +
        '" func="' + IntToStr(Ord(T.Func)) + '"/>');
    end;
    WrStr('</totals>');
  end;

  procedure WrColoring;
  var
    i: Integer;
    C: TRpColoringData;
  begin
    WrStr('<coloring>');
    for i := 0 to FColoring.Count - 1 do
    begin
      C := FColoring[i];
      WrStr('<coloringdata color="' + ColorToString(C.Color) + '" ' +
        'fieldname="' + C.FieldNameDS + '" expression="' + StrToXml(C.Expr) + '"/>');
    end;
    WrStr('</coloring>');
  end;

  procedure WrTemplates;
  var
    i: Integer;
  begin
    WrStr('<templates>');
    for i := 0 to FTemplates.Count - 1 do
      WrStr('<template filename="' + StrToXml(FTemplates[i]) + '"/>');
    WrStr('</templates>');
  end;

  procedure WrPrintFields;
  var
    i: Integer;
  begin
    WrStr('<printfields>');
    for i := 0 to FPrintFields.Count - 1 do
      WrStr('<printfield name="' + StrToXml(FPrintFields.Names[i]) +
        '" value="' + StrToXml(FPrintFields.ValueFromIndex[i]) + '"/>');
    WrStr('</printfields>');
  end;

  procedure WrSqlFields;
  var
    i: Integer;
    F: TSQLField;
  begin
    WrStr('<sqlfields>');
    for i := 0 to FSqlFields.Count - 1 do
    begin
      F := FSqlFields[i];
      WrStr('<sqlfield name="' + StrToXml(F.Name) + '" field="' + StrToXml(F.FieldNameDS) +
        '" tp="' + IntToStr(Ord(F.Tp)) + '" format="' + StrToXml(F.DisplayFormat) + '">');
    end;
    WrStr('</sqlfields>');
  end;

var
  i: Integer;

begin
  WrSortCols;
  WrStr('<reportdata id="' + IntToStr(FId) + '" name="' + FName +
    '" sortorder="' + FSortOrder + '" kind="' + IntToStr(Ord(FKind)) +
    '" datefield="' + IntToStr(FDateField) +
    '" datedetail="' + IntToStr(Ord(FDateDetail)) +
    '" first="' + IntToStr(FFirstRecordCount) +
    '" filter="' + StrToXml(FFilter) + '" helptext="' + HtmlToXml(FHelpText) +
    '" sql="' + StrToXml(FSQL) + '" sqlmode="' + Bool2Str(FSqlMode) +
    '" version="' + IntToStr(FVersion) + '">');
  WrStr('<sources>');
  for i := 0 to Sources.Count - 1 do
    WrSrc(Sources[i]^);
  WrStr('</sources>');
  WrCalcFields;
  WrTotals;
  WrColoring;
  WrGrid(Grid);
  WrTemplates;
  WrPrintFields;
  WrSqlFields;
  WrStr('</reportdata>');
end;

procedure TReportData.LoadFromStream(St: TStream);
begin
  with TRpReader.Create do
  try
    RD := Self;
    ParseStream(St);
  finally
    Free;
  end;
end;

procedure TReportData.Clear;
begin
  Sources.Clear;
  DateField := 0;
  DateDetail:=ddDay;
  Filter:='';
  Grid.ClearColumns;
  Grid.SortCols.Clear;
end;

function TReportData.FindField(aId: Integer): PRpField;
var
  i: Integer;
  pF: PRpField;
begin
  pF := nil;
  for i := 0 to Sources.Count - 1 do
  begin
    pF := Sources[i]^.Fields.FindField(aId);
    if (pF <> nil) and (not pF^.Zero) then Exit(pF);
  end;
  Result := pF;
end;

function TReportData.FindFieldByName(const aName: String): PRpField;
var
  i: Integer;
  pF: PRpField;
begin
  pF := nil;
  for i := 0 to Sources.Count - 1 do
  begin
    pF := Sources[i]^.Fields.FindFieldByName(aName);
    if (pF <> nil) and (not pF^.Zero) then Exit(pF);
  end;
  Result := pF;
end;

function TReportData.TryGetRpField(AIndex: Integer): PRpField;
var
  i: Integer;
  pF: PRpField;
begin
  Result := nil;
  if FSources.Count = 0 then Exit;

  if FSources[0]^.Fields.Count > AIndex then
  begin
    for i := 0 to FSources.Count - 1 do
    begin
      pF := FSources[i]^.Fields[AIndex];
      if not pF^.Zero then Exit(pF);
    end;

    // Нет выбранных полей - это поле с функцией "Количество".
    Result := FSources[0]^.Fields[AIndex];
  end;
end;

function TReportData.TryGetCalcField(AIndex: Integer): PRpCalcField;
var
  n: Integer;
begin
  Result := nil;
  if FSources.Count > 0 then
    n := FSources[0]^.Fields.Count
  else
    n := FSQLFields.Count;

  if n + FCalcFields.Count > AIndex then
    Result := FCalcFields[AIndex - n];
end;

function TReportData.TryGetSQLField(AIndex: Integer): TSQLField;
begin
  Result := nil;
  if FSQLFields.Count > AIndex then
    Result := FSQLFields[AIndex];
end;

function TReportData.GetFieldCount: Integer;
var
  n: Integer;
begin
  if FSources.Count > 0 then
    n := FSources[0]^.Fields.Count
  else
    n := FSQLFields.Count;
  Result := n + FCalcFields.Count;
end;

function TReportData.GetRpSQLFieldCount: Integer;
begin
  if FSources.Count > 0 then
    Result := FSources[0]^.Fields.Count
  else
    Result := FSQLFields.Count;
end;

function TReportData.IsEmpty: Boolean;
begin
  Result := (FSources.Count = 0) and (FSQLFields.Count = 0);
end;

procedure TReportData.CheckFieldIndex(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= GetFieldCount) then
    raise Exception.Create('Field index out of range.');
end;

function TReportData.GetFieldNameDS(AIndex: Integer): String;
var
  pF: PRpField;
  pCF: PRpCalcField;
  SqlF: TSQLField;
begin
  CheckFieldIndex(AIndex);

  pF := TryGetRpField(AIndex);
  if pF <> nil then Exit('f' + IntToStr(pF^.Id));

  SqlF := TryGetSQLField(AIndex);
  if SqlF <> nil then Exit(SqlF.FieldNameDS);

  pCF := TryGetCalcField(AIndex);
  if pCF <> nil then Exit('cf' + IntToStr(pCF^.Id));
end;

function TReportData.GetFieldName(AIndex: Integer): String;
var
  pF: PRpField;
  pCF: PRpCalcField;
  SqlF: TSQLField;
begin
  CheckFieldIndex(AIndex);

  pF := TryGetRpField(AIndex);
  if pF <> nil then Exit(pF^.Name);

  SqlF := TryGetSQLField(AIndex);
  if SqlF <> nil then Exit(SqlF.Name);

  pCF := TryGetCalcField(AIndex);
  if pCF <> nil then Exit(pCF^.Name);
end;

function TReportData.GetFieldType(AIndex: Integer): TRpFieldType;
var
  pF: PRpField;
  pCF: PRpCalcField;
  SqlF: TSQLField;
begin
  CheckFieldIndex(AIndex);

  pF := TryGetRpField(AIndex);
  if pF <> nil then
  begin
    if pF^.Func in [tfMerge, tfMergeAll] then Exit(flText)
    else if pF^.Func in [tfCount, tfDistCount] then Exit(flNumber)
    else if (pF^.Tp = flDate) and (DateField = AIndex) then
    begin
      if FDateDetail = ddDay then Exit(flDate)
      else if FDateDetail = ddYear then Exit(flNumber)
      else Exit(flText);
    end
    else Exit(GetLowField(pF)^.Tp);
  end;

  SqlF := TryGetSQLField(AIndex);
  if SqlF <> nil then Exit(SqlF.Tp);

  pCF := TryGetCalcField(AIndex);
  if pCF <> nil then Exit(pCF^.Tp);
end;

function TReportData.IndexOfNameDS(AFieldNameDS: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to GetFieldCount - 1 do
    if CompareText(GetFieldNameDS(i), AFieldNameDS) = 0 then Exit(i);
end;

function TReportData.GetFieldVisible(AIndex: Integer): Boolean;
var
  pF: PRpField;
begin
  CheckFieldIndex(AIndex);
  pF := TryGetRpField(AIndex);
  if pF <> nil then Exit(pF^.Visible);

  // В ином случае...
  Result := True;
end;

function TReportData.GetFieldParam(AIndex: Integer): Boolean;
var
  pF: PRpField;
begin
  CheckFieldIndex(AIndex);
  pF := TryGetRpField(AIndex);
  if pF <> nil then Exit(pF^.Param);

  // В ином случае...
  Result := False;
end;

function TReportData.GetDisplayFormat(AIndex: Integer): String;
var
  pF, pLowF: PRpField;
  pCF: PRpCalcField;
  SqlF: TSQLField;
  Fm: TdxForm;
  C: TComponent;
begin
  CheckFieldIndex(AIndex);
  Result := '';

  pF := TryGetRpField(AIndex);
  if pF <> nil then
  begin
    if pF^.Zero then Exit;

    pLowF := GetLowField(pF);
    Fm := FormMan.FindForm(pLowF^.TId);
    C := FindById(Fm, pLowF^.FId);
    if (C is TdxCalcEdit) and not (pF^.Func in [tfMerge, tfMergeAll, tfCount, tfDistCount]) then
      Result := TdxCalcEdit(C).PrecStr
    else if (C is TdxTimeEdit) and (pF^.Func = tfNone) then
      Result := TdxTimeEdit(C).TimeFormatStr;
    Exit;
  end;

  SqlF := TryGetSQLField(AIndex);
  if SqlF <> nil then
    Exit(SqlF.DisplayFormat);

  pCF := TryGetCalcField(AIndex);
  if pCF <> nil then
  begin
    if pCF^.Tp = flNumber then
    begin
      Result := ',0';
      if pCF^.Size > 0 then Result := ',0.' + DupeString('0', pCF^.Size);
    end
    else if pCF^.Tp = flTime then
    	Result := 'hh:mm:ss';
  end;
end;

function TReportData.IsCalcField(AIndex: Integer): Boolean;
begin
  CheckFieldIndex(AIndex);
  if (TryGetRpField(AIndex) = nil) and (TryGetSQLField(AIndex) = nil) and
    (TryGetCalcField(AIndex) <> nil) then Result := True
  else
    Result := False;
end;

function TReportData.IndexOfName(AFieldName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to GetFieldCount - 1 do
    if Utf8CompareText(GetFieldName(i), AFieldName) = 0 then Exit(i);
end;

procedure ExtractSourceFormsFromSQL(const SQL: String; AFormList: TStrings);
begin
  with TdxSQLParser.Create do
  try try
    ExtractForms := True;
    Parse(SQL);
    AFormList.AddStrings(SourceForms);
  except
    ;
  end;
  finally
    Free;
  end;
end;

procedure TReportData.GetSourceForms(AFormList: TStrings);
var
  i: Integer;
  Fm: TdxForm;
begin
  AFormList.Clear;

  if not FSqlMode then
    for i := 0 to FSources.Count - 1 do
    begin
      Fm := FormMan.FindForm(FSources[i]^.Id);
      if AFormList.IndexOf(Fm.FormCaption) < 0 then
        AFormList.Add(Fm.FormCaption);
    end
  else
    ExtractSourceFormsFromSQL(FSQL, AFormList);
end;

function TReportData.QueryExistsInExpr(AQueryName: String): Boolean;
var
  j: Integer;
  CF: TRpCalcField;
begin
  Result := True;
  for j := 0 to FSources.Count - 1 do
  begin
    if FormExists(AQueryName, FSources[j]^.Filter) then Exit;
  end;
  if FormExists(AQueryName, FSQL) then Exit;
  if FormExists(AQueryName, FFilter) then Exit;
  for j := 0 to FCalcFields.Count - 1 do
  begin
    CF := FCalcFields[j]^;
    if FormExists(AQueryName, CF.Expr) then Exit;
  end;
  Result := False;
end;

function TReportData.FieldExistsInExpr(DSRi: Integer; AFieldName: String
  ): Boolean;
var
  i: Integer;
  CF: TRpCalcField;
begin
  Result := False;
  for i := 0 to FSources.Count - 1 do
    if FieldExists(DSRi, AFieldName, FSources[i]^.Filter) then Exit(True);
  if FieldExistsForQuery(AFieldName, FFilter) then Exit(True);
  for i := 0 to FCalcFields.Count - 1 do
  begin
    CF := FCalcFields[i]^;
    if FieldExistsForQuery(AFieldName, CF.Expr) then Exit(True);
  end;
  Result := FieldExistsForQuery(AFieldName, FSQL);
end;

function TReportData.GetEditFormId: Integer;
begin
  if FSources.Count = 1 then
    Result := FSources[0]^.Id
  else
    Result := 0;
end;

function TReportData.GetSourceFilter: String;
begin
  if FSources.Count = 1 then
    Result := FSources[0]^.Filter
  else
    Result := '';
end;

function TReportData.IsSimple: Boolean;
var
  Sr: TRpSource;
  i: Integer;
  Fl: TRpField;
begin
  Result := False;
  if FSources.Count = 1 then
  begin
    Sr := FSources[0]^;
    for i := 0 to Sr.Fields.Count - 1 do
    begin
      Fl := Sr.Fields[i]^;
      if Fl.Func <> tfNone then Exit;
    end;
    Result := FDateField < 0;
  end;
end;

function TReportData.HasParentIdField: Boolean;
begin
  Result := (FSources.Count = 1) and (FSources[0]^.TId > 0);
end;

initialization
  RegisterClass(TdxQueryGrid);

end.

