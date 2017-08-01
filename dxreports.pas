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
unit DXReports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, DBGrids, dxctrls, Db, strconsts, myctrls, Lists,
  SqlDb, Dialogs, erroricon;

type
  TRpSourceKind = (skNone, skIncome, skOutcome);
  TRpFieldType = (flNone, flText, flNumber, flDate, flBool, flObject, flTime,
    flCounter);
  TRpFieldTypes = set of TRpFieldType;
  TRpTotalFunc = (tfNone, tfSum, tfAvg, tfMax, tfMin, tfCount, tfProfit, tfDistCount,
    tfMergeAll, tfMerge);

  TRpFieldList = class;

  PRpField = ^TRpField;
  TRpField = record
    TId, FId: String;
    Name: String;
    Param, Visible, No, Nul, Zero: Boolean;
    AllZeros: Boolean; // Устанавливается для полей с функцией "Количество", если не указано ни одного поля
    Tp: TRpFieldType;
    Value: String;
    Parent, Src: PRpField;
    Func: TRpTotalFunc;
    Id: Integer;
  end;

  PRpSource = ^TRpSource;
  TRpSource = record
    Id, TId: String;
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
    property Fields[Index: Integer]: PRpCalcField read GetFields; default;
  end;

  { TRpGridColumn }

  TRpGridColumn = class
  private
    FCaption: String;
    FColor: TColor;
    FFieldName: String;
    FFixedColor: TColor;
    FFont: TFont;
    FIndex: Integer;
    FTitleFont: TFont;
    FVisible: Boolean;
    FWidth: Integer;
    function GetFieldId: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function IsCalcField: Boolean;
    property Font: TFont read FFont write FFont;
    property Color: TColor read FColor write FColor;
    property TitleFont: TFont read FTitleFont write FTitleFont;
    property FixedColor: TColor read FFixedColor write FFixedColor;
    property Caption: String read FCaption write FCaption;
    property FieldName: String read FFieldName write FFieldName;
    property FieldId: Integer read GetFieldId;
    property Width: Integer read FWidth write FWidth;
    property Index: Integer read FIndex write FIndex;
    property Visible: Boolean read FVisible write FVisible;
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
    FAlternateColor: TColor;
    FColor: TColor;
    FDefaultRowHeight: Integer;
    FFixedColor: TColor;
    FFixedHotColor: TColor;
    FFlat: Boolean;
    FFont: TFont;
    FGridLineColor: TColor;
    FGridLineStyle: TPenStyle;
    FHorzLines: Boolean;
    FSelectedColor: TColor;
    FSortCols: TRpGridSortList;
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
    function FindColumnByFieldName(const FieldName: String): TRpGridColumn;
    function FindColumnByTitle(const S: String): TRpGridColumn;
    function FindColumnIndex(Col: TRpGridColumn): Integer;
    procedure DeleteColumn(Col: TRpGridColumn);
    procedure SortColumns(L: TList);
    procedure ClearColumns;
    property Color: TColor read FColor write FColor;
    property AlternateColor: TColor read FAlternateColor write FAlternateColor;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor;
    property FixedColor: TColor read FFixedColor write FFixedColor;
    property FixedHotColor: TColor read FFixedHotColor write FFixedHotColor;
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
  end;


  TRpDateDetail = (ddDay, ddWeek, ddMonth, ddQuart, ddHalfYear, ddYear);
  TReportKind = (rkReport, rkQuery);

  { TRpTotalData }

  TRpTotalData = class
  public
    Caption: String;
    FieldName: String;
    Func: TRpTotalFunc;
  end;

  { TRpTotalList }

  TRpTotalList = class(TList)
  private
    function GetTotals(Index: Integer): TRpTotalData;
  public
    function AddTotal: TRpTotalData;
    function FindTotal(const FieldName: String): TRpTotalData;
    procedure RemoveTotal(T: TRpTotalData);
    procedure Clear; override;
    property Totals[Index: Integer]: TRpTotalData read GetTotals; default;
  end;

  { TRpColoringData }

  TRpColoringData = class
  public
    Color: TColor;
    FieldName: String;
    Expr: String;
  end;

  { TRpColoringList }

  TRpColoringList = class(TList)
  private
    function GetColorings(Index: Integer): TRpColoringData;
  public
    function AddColoring: TRpColoringData;
    function FindColoring(const FieldName: String): TRpColoringData;
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
    FGrid: TRpGrid;
    FHelpText: String;
    FId: Integer;
    FName: String;
    FKind: TReportKind;
    //FPivotGrid: TObject;
    FSortOrder: String;
    FSources: TRpSourceList;
    FTotals: TRpTotalList;
    FVersion: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(St: TStream);
    procedure LoadFromStream(St: TStream);
    procedure Clear;
    function FindField(aId: Integer): PRpField;
    function FindFieldByName(const aName: String): PRpField;
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
  end;

  TdxQueryLinkType = (lkNone, lkForm, lkField);

  { TdxQueryGrid }

  TdxQueryGrid = class(TMyDBGrid)
  private
    FDSP: TObject;
    FId: Integer;
    FLinkType: TdxQueryLinkType;
    FOnAfterClose: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnAfterScroll: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnBeforeOpen: TNotifyEvent;
    FOnBeforeScroll: TNotifyEvent;
    FOnCreateForm: TCreateFormEvent;
    FOnPrintFieldEvent: TPrintFieldEvent;
    FParams: String;
    FQRi: Integer;
    FRpWnd: TObject;
    function GetFields(aName: String): Variant;
    function GetAsDT(Index: String): TDateTime;
    function GetAsF(Index: String): Extended;
    function GetAsI(Index: String): Integer;
    function GetAsS(Index: String): String;
    function GetQueryName: String;
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
    function Locate(const FieldName: String; FieldValue: Variant; aOptions: TLocateOptions): Boolean;
    function GotoRecord(aRecId: Integer): Boolean;
    procedure Refresh;
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

    property OnPrintField: TPrintFieldEvent read FOnPrintFieldEvent write FOnPrintFieldEvent;

    property LinkType: TdxQueryLinkType read FLinkType write FLinkType stored False;
    property Params: String read FParams write FParams stored False;
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
function IsSimpleReport(RD: TReportData): Boolean;
function CalcFieldExistsInSort(RD: TReportData): Boolean;
//function SortFieldsToStr(RD: TReportData): String;
function SqlReportSelect(RD: TReportData; Fm, PFm: TdxForm; DS: TDataSet): String;
//procedure AddCalcFields(RD: TReportData; DS: TDataSet);
procedure CalcQuery(aRD: TReportData; DS: TDataSet; aForm, aParForm: TdxForm; FormDS: TDataSet; Errs: TCalcError);
procedure FilterQuery(aRD: TReportData; DS: TDataSet; aForm, aParForm: TdxForm; FormDS: TDataSet);
procedure BuildSortIndexes(RD: TReportData; DataSet: TSQLQuery);

implementation

uses
  apputils, SAX, saxbasereader, LazUtf8, formmanager, sqlgen, expressions, strutils,
  Variants, DateUtils, reportmanager, dbengine, datasetprocessor, reportwindow,
  scriptfuncs;

type

  { TRpReader }

  TRpReader = class(TSaxBaseReader)
  private
    FSrc: PRpSource;
    FParent: PRpField;
    FColumn: TRpGridColumn;
    function GetColor(Atts: TSAXAttributes; const aName: String): TColor;
    function GetPenStyle(Atts: TSAXAttributes; const aName: String): TPenStyle;
    procedure ReadSortCols;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
    procedure DoEndElement(const NamespaceURI, LocalName, QName: SAXString); override;
  public
    RD: TReportData;
  end;

  { TSQLSourceFilterParser }

  TSQLSourceFilterParser = class(TSQLFilterParser)
  private
    FAliasSL: TStrings;
    FForm: TdxForm;
    FJoinStr, FFieldName, FOp, FValue: String;
    FParForm: TdxForm;
    FCmp: TComponent;
  protected
    function FieldNameParse(const aFieldName: String): String; override;
    function CheckValue(var Value: String): Boolean; override;
    function CheckOp(const Op: String): Boolean; override;
    function GetAnotherStr: String; override;
  public
    property Form: TdxForm read FForm write FForm;
    property ParentForm: TdxForm read FParForm write FParForm;
    property JoinStr: String read FJoinStr write FJoinStr;
    property AliasSL: TStrings read FAliasSL write FAliasSL;
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
  C: TComponent;
begin
  Fm := FormMan.FindForm(StrToInt(Fl.TId));
  C := FindById(Fm, StrToInt(Fl.FId));
  Result := GetJoinType(C);
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
  Col: TColumn;
  G: TRpGrid;
  L1: TRpGridSortList;
  L2: TSortColList;
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
    Col.FieldName:=C.FieldName;
    Col.Color:=C.Color;
    Col.Font := C.Font;
    Col.Title.Color:=C.FixedColor;
    Col.Title.Font := C.TitleFont;
    Col.Visible:=C.Visible;
  end;
  L.Free;
  Grid.Color := G.Color;
  Grid.AlternateColor:=G.AlternateColor;
  Grid.FixedColor:=G.FixedColor;
  Grid.FixedHotColor := G.FixedHotColor;
  Grid.SelectedColor:=G.SelectedColor;
  Grid.FocusColor:=G.SelectedColor;
  Grid.GridLineColor:=G.GridLineColor;
  Grid.GridLineStyle:=G.GridLineStyle;
  Grid.DefaultRowHeight:=G.DefaultRowHeight;
  Grid.Flat:=G.Flat;
  if G.VertLines then Grid.Options := Grid.Options + [dgColLines]
  else Grid.Options := Grid.Options - [dgColLines];
  if G.HorzLines then Grid.Options := Grid.Options + [dgRowLines]
  else Grid.Options := Grid.Options - [dgRowLines];
  Grid.Font := G.Font;
  Grid.TitleFont := G.TitleFont;
  Grid.WordWrap:=G.WordWrap;

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
end;

//(flNone, flText, flNumber, flDate, flBool, flObject, flTime, flCounter);
function RpFieldTypeToStr(Tp: TRpFieldType): String;
const
  TpS: array [TRpFieldType] of String = ('', rsText, rsNumber, rsDate,
  	rsCheckBox, rsObject, rsTime, rsCounter);
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
    pFld^.TId:=IntToStr(Fm.Id);
    pFld^.FId:=IntToStr(GetId(C));
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
  if aLow then F := GetLowField(@F)^;
  Fm := FormMan.FindForm(StrToInt(F.TId));
  Result := FindById(Fm, StrToInt(F.FId));
end;

function GetFullFieldName(F: TRpField): String;
var
  C: TComponent;
begin
  C := GetRpFieldComponent(F, False);
  Result := GetFieldName(C);
  if F.Src <> nil then
    Result := Result + '|' + GetFullFieldName(F.Src^);
end;

function FindSrcField(RD: TReportData; idx: Integer): PRpField;
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
end;

procedure SetQueryDisplayFormat(RD: TReportData; DS: TDataSet);
var
  L: TRpFieldList;
  i: Integer;
  F, LowF: TRpField;
  Fm: TdxForm;
  C: TComponent;
  Fl: TField;
  pF: PRpField;
  CF: TRpCalcField;
  S: String;
begin
  L := RD.Sources[0]^.Fields;
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
    Fm := FormMan.FindForm(StrToInt(LowF.TId));
    C := FindById(Fm, StrToInt(LowF.FId));
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
      S := '0';
      if CF.Size > 0 then S := '0.' + DupeString('0', CF.Size);
      TNumericField(DS.FieldByName('cf' + IntToStr(CF.Id))).DisplayFormat:=S;
    end
    else if CF.Tp = flTime then
    	TTimeField(DS.FieldByName('cf' + IntToStr(CF.Id))).DisplayFormat:='hh:mm:ss';
  end;
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
  P.ParentForm := FormMan.FindForm(StrToInt(Src.Id));
  if Src.TId <> '' then
    P.Form := FormMan.FindForm(StrToInt(Src.TId));
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
  Srcs: TStringList;
  i: Integer;
  Fl: TRpField;

  function GetAliasName(const Fl: TRpField): String;
  begin
    if Fl.Parent = nil then
      Result := 't' + Fl.TId
    else
      Result := GetAliasName(Fl.Parent^) + '_f' + Fl.Parent^.FId + '_t' + Fl.TId;
  end;

  procedure ProcessJoin(Fl: TRpField);
  var
    S, Tmp: String;
  begin
    S := GetAliasName(Fl);
    if Srcs.IndexOf(S) < 0 then
    begin
      Tmp := SqlSelectGroups(StrToInt(Fl.TId), StrToInt(Fl.FId), True);
      if Tmp <> '' then Tmp := '(' + Tmp + ')'
      else Tmp := 't' + Fl.TId;
      JStr := JStr + GetJoinTypeByRpField(Fl.Parent^) + Tmp + ' ' + S +
        ' on ' + GetAliasName(Fl.Parent^) + '.f' + Fl.Parent^.FId + '=' + S + '.id';
      Srcs.Add(S);
    end;
  end;

  procedure ProcessObjectField(const Fl: TRpField; FlId: Integer);
  var
    S: String;
  begin
    ProcessJoin(Fl);
    S := GetAliasName(Fl);
    if (Fl.Tp = flObject) and (Fl.Src <> nil) then
      ProcessObjectField(Fl.Src^, FlId)
    else
      FStr := FStr + S + '.f' + Fl.FId + ' as f' + IntToStr(FlId) + ',';
  end;

  procedure ProcessField(const Fl: TRpField);
  begin
    if (Fl.Tp = flObject) and (Fl.Src <> nil) then
      ProcessObjectField(Fl.Src^, Fl.Id)
    else
      FStr := FStr + 't' + Fl.TId + '.f' + Fl.FId + ' as f' + IntToStr(Fl.Id) + ',';
  end;

  procedure ProcessSumField(Fl: TRpField);
  begin
    if Src.Kind = skIncome then
      FStr := FStr + 't' + Fl.TId + '.f' + Fl.FId + ' as income' + IntToStr(Fl.Id) +
        ',0 as outcome' + IntToStr(Fl.Id) + ','
    else
      FStr := FStr + '0 as income' + IntToStr(Fl.Id) + ',t' + Fl.TId + '.f' + Fl.FId +
        ' as outcome' + IntToStr(Fl.Id) + ',';
  end;

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
      	tfMerge, tfMergeAll:
          FStr := FStr + ''''' as f' + IntToStr(Fl.Id) + ',';
        else
          FStr := FStr + 'null as f' + IntToStr(Fl.Id) + ',';
      end;
      {if (Fl.Tp = flNumber) and (Fl.Func in [tfSum, tfAvg, tfCount]) then
        FStr := FStr + '0 as f' + IntToStr(Fl.Id) + ','
      else if (Fl.Tp = flText) and (Fl.Func in [tfMerge, tfMergeAll]) then
        FStr := FStr + ''''' as f' + IntToStr(Fl.Id) + ','
      else
        FStr := FStr + 'null as f' + IntToStr(Fl.Id) + ',';}
    end;
  end;

begin
  Srcs := TStringList.Create;

  try

  FStr := ''; JStr := '';
  for i := 0 to Src.Fields.Count - 1 do
  begin
    Fl := Src.Fields[i]^;
    if Fl.Tp = flNone then Continue;
    if Fl.Zero then
      ProcessZero(Fl)
    else if Fl.Func = tfProfit then
      ProcessSumField(Fl)
    else
      ProcessField(Fl);
  end;
  FStr := Copy(FStr, 1, Length(FStr) - 1);
  Result := 'select ' + FStr + ' from t' + Src.Id;
  if Src.TId <> '' then
    Result := Result + ' left join t' + Src.TId + ' on t' + Src.Id + '.id=t' +
      Src.TId + '.pid';
  Flt := '';
  if Trim(Src.Filter) <> '' then
    Flt := SqlSourceFilter(Src, Fm, PFm, DS, Srcs, JStr);
  Result := Result + JStr;
  if Flt <> '' then
    Result := Result + ' where ' + Flt;

  finally
    Srcs.Free;
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

function IsSimpleReport(RD: TReportData): Boolean;
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
end;

function GetFuncSql(Fl: TRpField): String;
var
  Nm: String;
begin
  Result := '';
  Nm := 'f' + IntToStr(Fl.Id);
  case Fl.Func of
    tfSum: Result:= 'sum(' + Nm + ')';
    tfAvg: Result:= 'avg(' + Nm + ')';
    tfMax: Result:= 'max(' + Nm + ')';
    tfMin: Result:= 'min(' + Nm + ')';
    tfCount: Result:= 'count(' + Nm + ')';
    tfProfit: Result:= '(sum(income' + IntToStr(Fl.Id) + ')-sum(outcome' + IntToStr(Fl.Id) + '))';
    tfDistCount: Result := 'count(distinct ' + Nm + ')';
    tfMergeAll: Result := 'list(' + Nm + ',''; '')';
    tfMerge: Result := 'list(distinct ' + Nm + ',''; '')';
  end;
end;

function GetHavingClause(Fl: TRpField): String;
var
  i, p: Integer;
  S, V1, V2, Fn, rS, Tmp: String;
  SL: TStringList;
begin
  rS := '';
  Result := '';
  Fn := GetFuncSql(Fl);
  SL := TStringList.Create;
  SplitStr(Fl.Value, ';', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    p := Pos(' .. ', S);
    if p > 0 then
    begin
      Tmp := '';
      V1 := CheckNumber(Copy(S, 1, p - 1));
      V2 := CheckNumber(Copy(S, p + 4, 255));
      if V1 <> '' then
        Tmp := Tmp + Fn + '>=' + V1 + ' and ';
      if V2 <> '' then
        Tmp := Tmp + Fn + '<=' + V2 + ' and ';
      Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
      rS := rS + '(' + Tmp + ') or ';
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
end;

function GetWhereClause(aFl: TRpField): String;
var
  S, Bg, Ed, FlNm, W, Tmp: String;
  p: Integer;
  i: Integer;
  SL: TStringList;
  Fl: TRpField;
  C: TComponent;
begin
  Result := '';
  SL := TStringList.Create;
  SplitStr(aFl.Value, ';', SL);
  Fl := GetLowField(@aFl)^;
  FlNm := 'f' + IntToStr(aFl.Id);

  W := '';
  if aFl.Nul then W := W + FlNm + ' is null or ';
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    if S = '' then Continue;
    case Fl.Tp of
      flText: W := W + FlNm + ' containing ''' + S + ''' or ';
      flDate:
        begin
          if Copy(S, 1, 1) = '$' then
          begin
            Ed := DateToStr(Date);
            case TPeriodType(StrToInt(Copy(S, 2, 10))) of
              ptToday: Bg := DateToStr(Date);
              ptThisWeek: Bg := DateToStr( IncDay(Date, -DayOfTheWeek(Date)+1) );
              ptThisMonth: Bg := DateToStr( IncDay(Date, -DayOf(Date)+1) );
              ptThisYear: Bg := DateToStr( EncodeDate(YearOf(Date), 1, 1) );
            end;
          end
          else
          begin
            p := Pos(' .. ', S);
            Bg := Copy(S, 1, p - 1);
            Ed := Copy(S, p + 4, 1024);
          end;
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
          if Bg <> '' then
          begin
            Bg := CheckNumber(Bg);
            Bg := Bg + '-' + Bg + '*2e-12';
            Tmp := Tmp + FlNm + '>=' + Bg + ' and ';
          end;
          if Ed <> '' then
          begin
            Ed := CheckNumber(Ed);
            Ed := Ed + '+' + Ed + '*2e-12';
            Tmp := Tmp + FlNm + '<=' + Ed + ' and ';
          end;
          Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
          if Tmp <> '' then
            W := W + '(' + Tmp + ') or ';
        end;
      flCounter:
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
    S := UpperCase(Copy(RD.Grid.SortCols[i].Col.FieldName, 1, 2));
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

function SqlReportSelect(RD: TReportData; Fm, PFm: TdxForm; DS: TDataSet): String;
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
  if RD.Sources.Count = 0 then Exit;
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
    FromStr := FromStr + SqlSourceSelect2(RD.Sources[i]^, Fm, PFm, DS);
    if i < RD.Sources.Count - 1 then FromStr := FromStr + ' union all ';
  end;

  Sim := IsSimpleReport(RD);
  if Sim then
  begin
    S := ' t' + RD.Sources[0]^.Id + '.id as id';
    if RD.Sources[0]^.TId <> '' then S := S + ', t' + RD.Sources[0]^.TId + '.id as tid';
    if FStr <> '' then S := S + ',';
    Insert(S, FromStr, 7);
    if RD.Sources[0]^.TId <> '' then FStr := 'tid,' + FStr;
    FStr := 'id,' + FStr;
  end;
  // Добавляем поля-пустышки для вычисляемых полей
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i]^;
    case CF.Tp of
      flNumber: S := '0.0';
      flDate: S := 'CURRENT_DATE';
      flTime: S := 'CURRENT_TIME';
      else S := '(''' + DupeString(' ', CF.Size) + ''')';
    end;
    //FStr := FStr + '(''' + DupeString(' ', 200) + ''') as cf' +
    FStr := FStr + S + ' as cf' + IntToStr(CF.Id) + ',';
  end;
  //
  FStr := Copy(FStr, 1, Length(FStr) - 1);
  GStr := Copy(GStr, 1, Length(GStr) - 1);
  Result := 'select ' + FStr + ' from (' + FromStr + ') ';
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
    if UpperCase(Copy(Col.Col.FieldName, 1, 2)) = 'CF' then Continue;
    Srt := Srt + Col.Col.FieldName;
    if Col.Desc then Srt := Srt + ' desc';
    Srt := Srt + ',';
  end;
  Srt := Copy(Srt, 1, Length(Srt) - 1);
  if Srt <> '' then
    Result := Result + ' order by ' + Srt;
  //DebugStr(Result);
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
  S: String;
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
          Errs.AddError(aRD.Name + '->' + CF.Name, Ex.Message);
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
  for i := 0 to DS.Fields.Count - 1 do
    DS.Fields[i].Required:=False;
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
          if (CF.Tp = flNumber) and (V <> Null) then V := MathRound(V, CF.Size);
          F.Value := V;
        except
          on Ex: Exception do
          begin
  					F.Value := Null;
            Errs.AddError(aRD.Name + '->' + CF.Name, Ex.Message);
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
  DescFieldNames, FieldNames: String;
begin
  if DataSet.RecordCount = 0 then Exit;
  if not DataSet.IndexDefs.Updated then
  	DataSet.IndexDefs.Update;
  FieldNames := '';
  DescFieldNames := '';
  for i := 0 to RD.Grid.SortCols.Count - 1 do
  begin
    SCol := RD.Grid.SortCols[i];
    Col := SCol.Col;
    FieldNames := FieldNames + Col.FieldName + ';';
    if SCol.Desc then
      DescFieldNames := DescFieldNames + Col.FieldName + ';';
  end;
  FieldNames := Copy(FieldNames, 1, Length(FieldNames) - 1);
  DescFieldNames := Copy(DescFieldNames, 1, Length(DescFieldNames) - 1);
  if FieldNames <> '' then
  begin
	  DataSet.AddIndex('MY_INDEX', FieldNames, [], DescFieldNames);
  	DataSet.IndexName:='MY_INDEX';
  end;
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
    if Utf8CompareText(pF^.Name, S) = 0 then Exit(pF);
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
    if CompareText(CD.FieldName, FieldName) = 0 then Exit(CD);
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
  Result := DataSource.DataSet.FieldByName(C.FieldName).Value;
end;

constructor TdxQueryGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  VisibleButtons := [gbnAppend, gbnEdit, gbnDelete, gbnRefresh, gbnGoto];
end;

procedure TdxQueryGrid.MoveBy(Distance: Integer);
begin
  DataSource.DataSet.MoveBy(Distance);
end;

procedure TdxQueryGrid.MoveFirst;
begin
  DataSource.DataSet.First;
end;

procedure TdxQueryGrid.MovePrior;
begin
  DataSource.DataSet.Prior;
end;

procedure TdxQueryGrid.MoveNext;
begin
  DataSource.DataSet.Next;
end;

procedure TdxQueryGrid.MoveLast;
begin
  DataSource.DataSet.Last;
end;

procedure TdxQueryGrid.MoveTo(aRecNo: Integer);
begin
  MoveBy(aRecNo - RecNo);
end;

function TdxQueryGrid.EOF: Boolean;
begin
  Result := DataSource.DataSet.EOF;
end;

function TdxQueryGrid.BOF: Boolean;
begin
  Result := DataSource.DataSet.BOF;
end;

function TdxQueryGrid.RecNo: Integer;
begin
  Result := DataSource.DataSet.RecNo;
end;

function TdxQueryGrid.RecId: Integer;
begin
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
  Result := DataSource.DataSet.RecordCount;
end;

function TdxQueryGrid.Locate(const FieldName: String; FieldValue: Variant;
  aOptions: TLocateOptions): Boolean;
var
  RD: TReportData;
  C: TRpGridColumn;
begin
  RD := ReportMan.FindReport(FId);
  C := RD.Grid.FindColumnByTitle(FieldName);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
  Result := DataSource.DataSet.Locate(C.FieldName, FieldValue, aOptions);
end;

function TdxQueryGrid.GotoRecord(aRecId: Integer): Boolean;
begin
  Result := DataSource.DataSet.Locate('id', aRecId, []);
end;

procedure TdxQueryGrid.Refresh;
begin
  if FRpWnd = nil then
    TDataSetProcessor(FDSP).RequeryQuery(FQRi)
  else
    TReportWindow(FRpWnd).RefreshReport(False);
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

function TRpTotalList.FindTotal(const FieldName: String): TRpTotalData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(FieldName, Totals[i].FieldName) = 0 then Exit(Totals[i]);
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

{ TSQLSourceFilterParser }

function TSQLSourceFilterParser.FieldNameParse(const aFieldName: String
  ): String;
var
  Fm: TdxForm;
  SL: TStringList;
  i: Integer;
  FieldName, S, ParentAlias, ParentField, AliasName, Tmp: String;
  C: TComponent;
begin
  Result := '';
  FieldName := aFieldName;
  if Length(FieldName) = 0 then Exit;
  Fm := FForm;
  if FieldName[1] = '!' then
  begin
    Fm := FParForm;
    Delete(FieldName, 1, 1);
  end;
  if (Length(FieldName) = 0) or (Fm = nil) then Exit;
  AliasName := '';
  SL := TStringList.Create;

  try

  SplitStr(FieldName, '|', SL);
  for i := 0 to SL.Count - 1 do
  begin
    if i > 0 then AliasName := AliasName + '_';
    AliasName := AliasName + TableStr(Fm.Id);
    S := SL[i];
    C := FindComponentByFieldName(Fm, S);
    if C = nil then Exit;
    if (i > 0) and (AliasSL.IndexOf(AliasName) < 0) then
    begin
      Tmp := SqlSelectGroups(Fm.Id, GetId(C), True);
      if Tmp <> '' then Tmp := '(' + Tmp + ')'
      else Tmp := TableStr(Fm.Id);
      JoinStr := JoinStr + GetJoinType(C) + Tmp + ' ' + AliasName +
        ' on ' + ParentAlias + '.' + ParentField + '=' + AliasName + '.id';

      AliasSL.Add(AliasName);
    end;
    ParentAlias := AliasName;
    ParentField := FieldStr(C);
    if C is TdxLookupComboBox then
    begin
      if i < SL.Count - 1 then
      begin
        AliasName := AliasName + '_' + ParentField;
        Fm := FormMan.FindForm(GetSourceTId(C));
        if Fm = nil then Exit;
      end;
    end
    else if i < SL.Count - 1 then Exit;
  end;
  AliasName := AliasName + '.' + FieldStr(C);
  FCmp := C;
  Result := AliasName;

  finally
    SL.Free;
  end;
  FFieldName := Result;
end;

function TSQLSourceFilterParser.CheckValue(var Value: String): Boolean;
var
  Tmp: String;
begin
  Result := CheckType(FCmp, Value);
  // экранируем апострофы
  if (FCmp is TdxEdit) or (FCmp is TdxMemo) or (FCmp is TdxComboBox) or
    (FCmp is TdxDateEdit) then
    Value := '''' + StringReplace(Value, #39, #39#39, [rfReplaceAll]) + ''''
  else if FCmp is TdxCalcEdit then
  begin
    if FOp = '>=' then
    	Value := Value + '-' + Value + '*2e-12'
    else if FOp = '<=' then
    	Value := Value + '+' + Value + '*2e-12';
  end
  else if FCmp is TdxTimeEdit then
  begin
    if FOp = '>=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Value, Tmp);
      Value := '''' + Value + '''';
    end
    else if FOp = '<=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Tmp, Value);
      Value := '''' + Value + '''';
    end
    else
    	Value := '''' + Value + '''';
  end;
  FValue := Value;
end;

function TSQLSourceFilterParser.CheckOp(const Op: String): Boolean;
begin
  FOp := Op;
  Result:=inherited CheckOp(Op);
  if (FCmp is TdxLookupComboBox) and (Op <> '=') and (Op <> '<>') then Result := False;
end;

function TSQLSourceFilterParser.GetAnotherStr: String;
var
  Fm: TdxForm;
  TId: Integer;
  Ed: String;
begin
  Result := '';
  if FCmp is TdxLookupComboBox then
  begin
    TId := GetSourceTId(FCmp);
    Fm := FormMan.FindForm(TId);
    if (Fm <> nil) and (Fm.ParentField > 0) then
    begin
      FValue := SqlSelectIDs(TId, FValue);
      Result := '''' + FValue + ''' containing ''\'' || ' + FFieldName + ' || ''\''';
      if FOp = '<>' then Result := 'not (' + Result + ')';
    end;
  end
  else if FCmp is TdxCalcEdit then
  begin
    if FOp = '=' then
    begin
      Result := FFieldName + '>=' + FValue + '-' + FValue + '*2e-12 and ' +
      	FFieldName + '<=' + FValue + '+' + FValue + '*2e-12';
    end;
  end
  else if FCmp is TdxTimeEdit then
  begin
    if FOp = '=' then
    begin
      Ed := FValue;
      CheckTime(TdxTimeEdit(FCmp).TimeFormat, FValue, Ed);
      Result := FFieldName + '>=''' + FValue + ''' and ' +
      	FFieldName + '<=''' + Ed + '''';
    end;
  end;
end;

{ TRpGridColumn }

function TRpGridColumn.GetFieldId: Integer;
begin
  if not IsCalcField then
  	Result := StrToInt(Copy(FFieldName, 2, 100))
  else
		Result := StrToInt(Copy(FFieldName, 3, 100))
end;

constructor TRpGridColumn.Create;
begin
  FFont := TFont.Create;
  FTitleFont := TFont.Create;
  FVisible := True;
end;

destructor TRpGridColumn.Destroy;
begin
  FTitleFont.Free;
  FFont.Free;
  inherited Destroy;
end;

function TRpGridColumn.IsCalcField: Boolean;
begin
  Result := LowerCase(Copy(FFieldName, 1, 2)) = 'cf';
end;

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
  FFixedHotColor:=cl3DLight;
  FSelectedColor:= clHighlight;
  FColor := clWindow;
  FAlternateColor:=FColor;;
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

function TRpGrid.FindColumnByFieldName(const FieldName: String): TRpGridColumn;
var
  i: Integer;
  C: TRpGridColumn;
begin
  Result := nil;
  for i := 0 to ColumnCount - 1 do
  begin
    C := Columns[i];
    if CompareText(C.FieldName, FieldName) = 0 then Exit(C);
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
    if Utf8CompareText(C.Caption, S) = 0 then Exit(C);
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

function TRpReader.GetColor(Atts: TSAXAttributes; const aName: String): TColor;
var
  S: SAXString;
begin
  Result := clDefault;
  S := Atts.GetValue('', aName);
  if S <> '' then Result := StringToColor(S);
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
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'source' then
  begin
    RD.Sources.AddSource(pS);
    pS^.Kind:=TRpSourceKind(GetInt(Atts, 'kind'));
    pS^.Id:=Atts.GetValue('', 'id');
    pS^.TId := Atts.GetValue('', 'tid');
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
    pF^.TId := Atts.GetValue('', 'tid');
    pF^.FId := Atts.GetValue('', 'fid');
    pF^.Param:=GetBool(Atts, 'param');
    pF^.Visible := GetBool(Atts, 'visible');
    pF^.No:=GetBool(Atts, 'not');
    pF^.Nul:=GetBool(Atts, 'null');
    pF^.Zero:=GetBool(Atts, 'zero');
    pF^.Value := Atts.GetValue('', 'value');
    pF^.Func := TRpTotalFunc(GetInt(Atts, 'func'));
    pF^.Id := GetInt(Atts, 'id');
  end
  else if LocalName = 'reportdata' then
  begin
    RD.Id:= GetInt(Atts, 'id');
    RD.Name:=Atts.GetValue('', 'name');
    RD.SortOrder:=Atts.GetValue('', 'sortorder');
    RD.Kind:=TReportKind(GetInt(Atts, 'kind'));
    RD.DateField:=GetInt(Atts, 'datefield');
    RD.DateDetail:=TRpDateDetail(GetInt(Atts, 'datedetail'));
    RD.Filter := XmlToStr(Atts.GetValue('', 'filter'));
    RD.HelpText:=XmlToStr(Atts.GetValue('', 'helptext'));
    RD.Version := GetInt(Atts, 'version');
  end
  else if LocalName = 'grid' then
  begin
    G := RD.Grid;
    G.Color:=GetColor(Atts, 'color');
    G.AlternateColor:=GetColor(Atts, 'alternatecolor');
    G.SelectedColor:=GetColor(Atts, 'selectedcolor');
    G.FixedColor:=GetColor(Atts, 'fixedcolor');
    G.FixedHotColor:=GetColor(Atts, 'fixedhotcolor');
    G.GridLineColor:=GetColor(Atts, 'gridlinecolor');
    G.GridLineStyle:=GetPenStyle(Atts, 'gridlinestyle');
    G.DefaultRowHeight:=GetInt(Atts, 'defaultrowheight');
    G.VertLines:=GetBool(Atts, 'vertlines');
    G.HorzLines:=GetBool(Atts, 'horzlines');
    G.Flat:=GetBool(Atts, 'flat');
    G.WordWrap:=GetBool(Atts, 'wordwrap');
  end
  else if LocalName = 'column' then
  begin
    C := RD.Grid.AddColumn;
    C.Color := GetColor(Atts, 'color');
    C.FixedColor:=GetColor(Atts, 'fixedcolor');
    C.Width:=GetInt(Atts, 'width');
    C.FieldName:=Atts.GetValue('', 'fieldname');
    C.Caption := Atts.GetValue('', 'caption');
    C.Index := GetInt(Atts, 'index');
    if Atts.GetValue('', 'visible') <> '' then
      C.Visible:=GetBool(Atts, 'visible');
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
    Fnt.Size:=GetInt(Atts, 'size');
    Fnt.Color := GetColor(Atts, 'color');
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
    T.FieldName:=Atts.GetValue('', 'field');
    T.Func:=TRpTotalFunc(GetInt(Atts, 'func'));
  end
  else if LocalName = 'coloringdata' then
  begin
    CD := RD.Coloring.AddColoring;
    CD.Color:=StringToColor(Atts.GetValue('', 'color'));
    CD.FieldName:=XmlToStr(Atts.GetValue('', 'fieldname'));
    CD.Expr:=XmlToStr(Atts.GetValue('', 'expression'));
  end
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
    if Utf8CompareText(S, pF^.Name) = 0 then Exit(pF);
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
  FGrid.DefaultRowHeight:=20;
  FGrid.Flat:=False;
  FGrid.GridLineStyle:=psSolid;
  FGrid.VertLines:=True;
  FGrid.HorzLines:=True;
  FCalcFields := TRpCalcFieldList.Create;
  FTotals := TRpTotalList.Create;
  FColoring := TRpColoringList.Create;
  //FPivotGrid := TdxPivotGrid.Create(nil);
end;

destructor TReportData.Destroy;
begin
  //FPivotGrid.Free;
  FColoring.Free;
  FTotals.Free;
  FCalcFields.Free;
  FGrid.Free;
  FSources.Free;
  inherited Destroy;
end;

procedure TReportData.SaveToStream(St: TStream);
var
  i: Integer;

  procedure WrStr(const S: String);
  begin
    St.Write(Pointer(S)^, Length(S));
  end;

  procedure WrFld(F: TRpField);
  begin
    WrStr('<field name="' + F.Name + '" type="' + IntToStr(Ord(F.Tp)) +
      '" tid="' + F.TId + '" fid="' + F.FId + '" param="' + Bool2Str(F.Param) +
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
    WrStr('<source kind="' + IntToStr(Ord(S.Kind)) + '" id="' + S.Id +
      '" tid="' + S.TId + '" filter="' + StrToXml(S.Filter) + '">');
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
    WrStr('<' + TagName + ' name="' + F.Name + '" size="' + IntToStr(F.Size) +
      '" color="' + ColorToString(F.Color) + '" bold="' + Bool2Str(fsBold in F.Style) +
      '" italic="' + Bool2Str(fsItalic in F.Style) + '" underline="' +
      Bool2Str(fsUnderline in F.Style) + '" strikeout="' +
      Bool2Str(fsStrikeOut in F.Style) + '"/>');
  end;

  procedure WrColumn(C: TRpGridColumn);
  begin
    WrStr('<column color="' + ColorToString(C.Color) + '" fixedcolor="' +
      ColorToString(C.FixedColor) + '" caption="' + C.Caption +
      '" width="' + IntToStr(C.Width) + '" fieldname="' + C.FieldName +
      '" index="' + IntToStr(C.Index) + '" visible="' + Bool2Str(C.Visible) + '">');
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
      '" fixedcolor="' + ColorToString(G.FixedColor) +
      '" fixedhotcolor="' + ColorToString(G.FixedHotColor) + '" gridlinecolor="' +
      ColorToString(G.GridLineColor) + '" gridlinestyle="' + PenStyle2Str(G.GridLineStyle) +
      '" defaultrowheight="' + IntToStr(G.DefaultRowHeight) + '" vertlines="' +
      Bool2Str(G.VertLines) + '" horzlines="' + Bool2Str(G.HorzLines) +
      '" flat="' + Bool2Str(G.Flat) + '" wordwrap="' + Bool2Str(G.WordWrap) + '">');
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
    i{, n}: Integer;
    Col: TRpGridSortData;
  begin
    S := '';
    for i := 0 to Grid.SortCols.Count - 1 do
    begin
      Col := Grid.SortCols[i];
      Tmp := Col.Col.FieldName;
      if Col.Desc then Tmp := '*' + Tmp;
      S := S + Tmp;
      if i < Grid.SortCols.Count - 1 then S := S + ';';
      {Delete(Tmp, 1, 1);
      if TryStrToInt(Tmp, n) then
      begin
        if Col.Desc then S := S + '*';
        S := S + Tmp;
        if i < Grid.SortCols.Count - 1 then S := S + ';';
      end;   }
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
      WrStr('<total caption="' + T.Caption + '" field="' + T.FieldName +
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
        'fieldname="' + C.FieldName + '" expression="' + StrToXml(C.Expr) + '"/>');
    end;
    WrStr('</coloring>');
  end;

  {procedure WrPivotField(FI: TFieldItem);
  begin
    WrStr('<pivotfield fieldname="' + FI.FieldName +
      '" caption="' + StrToXml(FI.Caption) +
      '" totalcaption="' + StrToXml(FI.TotalCaption) +
      '" width="' + IntToStr(FI.Width) +
      '" height="' + IntToStr(FI.Height) +
      '" totalwidth="' + IntToStr(FI.TotalWidth) +
      '" func="' + IntToStr(Ord(FI.Func)) +
      '" showtotal="' + Bool2Str(FI.ShowTotal) +
      '" fixclr="' + ColorToString(FI.FixedColor) +
      '" clr="' + ColorToString(FI.Color) +
      '" totalfixclr="' + ColorToString(FI.TotalFixedColor) +
      '" totalclr="' + ColorToString(FI.totalColor) +
      '" valign="' + IntToStr(Ord(FI.VAlign)) +
      '" halign="' + IntToStr(Ord(FI.HAlign)) +
      '" datatype="' + IntToStr(Ord(FI.DataType)) + '>');
    WrFont(FI.FixedFont, 'fixfont');
    WrFont(FI.Font, 'font');
    WrFont(FI.TotalFixedFont, 'totalfixfont');
    WrFont(FI.TotalFont, 'totalfont');
    WrStr('</pivotfield>');
  end;

  procedure WrPivotGrid(Gr: TdxPivotGrid);
  var
    i: Integer;
  begin
    WrStr('<pivotgrid totalfixclr="' + ColorToString(Gr.GrandTotalFixedColor) +
      '" fixclr="' + ColorToString(Gr.Colors.FixedCellBkGnd) +
      '" totalclr="' + ColorToString(Gr.GrandTotalColor) +
      '" cornerclr="' + ColorToString(Gr.CornerColor) +
      '" clr="' + ColorToString(Gr.Colors.CellBkGnd) +
      '" linesclr="' + ColorToString(Gr.Colors.CellLines) +
      '" fixlinesclr="' + ColorToString(Gr.Colors.FixedCellLines) +
      '" indicatclr="' + ColorToString(Gr.Colors.FixedCellIndication) +
      '" selectclr="' + ColorToString(Gr.Colors.SelectedCellBkGnd) +
      '" totalx="' + Bool2Str(Gr.ShowGrandTotalX) +
      '" totaly="' + Bool2Str(Gr.ShowGrandTotalY) +
      '" totalcaption="' + StrToXml(Gr.GrandTotalCaption) +
      '" totalwidth="' + IntToStr(Gr.GrandTotalWidth) +
      '" flat="' + Bool2Str(Gr.Flat) +
      '" wordwrap="' + Bool2Str(Gr.WordWrap) +
      '" datadelim="' + StrToXml(Gr.DataDelimiter) +
      '" indent="' + IntToStr(Gr.Indent) +
      '" hlines="' + Bool2Str(goHorzLine in Gr.Options) +
      '" vlines="' + Bool2Str(goVertLine in Gr.Options) + '">');
    WrFont(Gr.FixedFont, 'fixedfont');
    WrFont(Gr.Font, 'font');
    WrFont(Gr.SelectedFont, 'selectfont');
    WrFont(Gr.GrandTotalFixedFont, 'totalfixfont');
    WrFont(Gr.GrandTotalFont, 'totalfont');
    WrStr('<rowfields>');
    for i := 0 to Gr.RowFields.Count - 1 do
      WrPivotField(Gr.RowFields[i]);
    WrStr('</rowfields>');
    WrStr('<colfields>');
    for i := 0 to Gr.ColFields.Count - 1 do
      WrPivotField(Gr.ColFields[i]);
    WrStr('</colfields>');
    WrStr('<datafields>');
    for i := 0 to Gr.DataFields.Count - 1 do
      WrPivotField(Gr.DataFields[i]);
    WrStr('</datafields>');
    WrStr('</pivotgrid>');
  end;      }

begin
  WrSortCols;
  WrStr('<reportdata id="' + IntToStr(FId) + '" name="' + FName +
    '" sortorder="' + FSortOrder + '" kind="' + IntToStr(Ord(FKind)) +
    '" datefield="' + IntToStr(FDateField) +
    '" datedetail="' + IntToStr(Ord(FDateDetail)) +
    '" filter="' + StrToXml(FFilter) + '" helptext="' + StrToXml(FHelpText) +
    '" version="' + IntToStr(FVersion) + '">');
  WrStr('<sources>');
  for i := 0 to Sources.Count - 1 do
    WrSrc(Sources[i]^);
  WrStr('</sources>');
  WrCalcFields;
  WrTotals;
  WrColoring;
  WrGrid(Grid);
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

initialization
  RegisterClass(TdxQueryGrid);

end.

