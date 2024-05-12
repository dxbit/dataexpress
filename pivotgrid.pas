unit PivotGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, KGrids, KFunctions, DB, Dialogs, Graphics, KGraphics,
  dxreports;

type

  {TRpTotalFunc = (tfNone, tfSum, tfAvg, tfMax, tfMin, tfCount, tfProfit, tfDistCount,
    tfMergeAll, tfMerge);}

  { TFieldItem }

  TFieldItem = class(TCollectionItem)
  private
    FCaption: String;
    FColor: TColor;
    FDataType: TRpFieldType;
    FFieldName: String;
    FFixedColor: TColor;
    FFixedFont: TFont;
    FFont: TFont;
    FFunc: TRpTotalFunc;
    FHAlign: TKHAlign;
    FHeight: Integer;
    FShowTotal: Boolean;
    FTotalColor: TColor;
    FTotalCaption: String;
    FTotalFixedColor: TColor;
    FTotalFixedFont: TFont;
    FTotalFont: TFont;
    FTotalWidth: Integer;
    FVAlign: TKVAlign;
    FWidth: Integer;
    procedure SetFixedFont(AValue: TFont);
    procedure SetFont(AValue: TFont);
    procedure SetTotalFixedFont(AValue: TFont);
    procedure SetTotalFont(AValue: TFont);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property FieldName: String read FFieldName write FFieldName;
    property Caption: String read FCaption write FCaption;
    property TotalCaption: String read FTotalCaption write FTotalCaption;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property TotalWidth: Integer read FTotalWidth write FTotalWidth;
    property Func: TRpTotalFunc read FFunc write FFunc;
    property ShowTotal: Boolean read FShowTotal write FShowTotal;
    property FixedColor: TColor read FFixedColor write FFixedColor;
    property TotalFixedColor: TColor read FTotalFixedColor write FTotalFixedColor;
    property Color: TColor read FColor write FColor;
    property TotalColor: TColor read FTotalColor write FTotalColor;
    property FixedFont: TFont read FFixedFont write SetFixedFont;
    property TotalFixedFont: TFont read FTotalFixedFont write SetTotalFixedFont;
    property Font: TFont read FFont write SetFont;
    property TotalFont: TFont read FTotalFont write SetTotalFont;
    property VAlign: TKVAlign read FVAlign write FVAlign;
    property HAlign: TKHAlign read FHAlign write FHAlign;
    property DataType: TRpFieldType read FDataType write FDataType;
  end;

  { TFieldCollection }

  TFieldCollection = class(TCollection)
  private
    function GetFields(Index: Integer): TFieldItem;
  public
    function AddField: TFieldItem;
    function FindFieldByFieldName(const S: String): TFieldItem;
    property Fields[Index: Integer]: TFieldItem read GetFields; default;
  end;

  PPivotValue = ^TPivotValue;
  TPivotValue = record
    V: Variant;
    dt: TRpFieldType;
  end;

  { TPivotValues }

  TPivotValues = class(TList)
  private
    function GetValues(Index: Integer): PPivotValue;
  public
    procedure Clear; override;
    function FindValue(aV: Variant): Integer;
    procedure SortValues;
    procedure SortValuesPreview;
    property Values[Index: Integer]: PPivotValue read GetValues; default;
  end;

  { TdxPivotGrid }

  TdxPivotGrid = class(TKGrid)
  private
    FColFields: TFieldCollection;
    FDataFields: TFieldCollection;
    FRowFields: TFieldCollection;
    procedure SetColFields(AValue: TFieldCollection);
    procedure SetDataFields(AValue: TFieldCollection);
    procedure SetRowFields(AValue: TFieldCollection);
  private
    FCornerColor: TColor;
    FDataDelimiter: String;
    FDS: TDataSet;
    FFixedFont: TFont;
    FGrandTotalCaption: String;
    FGrandTotalColor: TColor;
    FGrandTotalFixedColor: TColor;
    FGrandTotalFixedFont: TFont;
    FGrandTotalFont: TFont;
    FGrandTotalWidth: Integer;
    FId: Integer;
    FIndent: Integer;
    FNeedBuild: Boolean;
    FOnBuild: TNotifyEvent;
    FPreview: Boolean;
    FSelectedFont: TFont;
    FShowGrandTotalX: Boolean;
    FShowGrandTotalY: Boolean;
    FStopTab: Boolean;
    FWordWrap: Boolean;
    FBuilded: Boolean;
    procedure GetValues(const aField: String; dt: TRpFieldType; VL: TPivotValues;
      Filter: array of Variant);
    procedure BuildRows;
    procedure BuildCols;
    procedure FillData;
    procedure CalcRowTotal(r: Integer);
    procedure CalcColTotal(c: Integer);
    procedure HideTotals;
    procedure SetFixedFont(AValue: TFont);
    procedure SetGrandTotalFixedFont(AValue: TFont);
    procedure SetGrandTotalFont(AValue: TFont);
    procedure SetSelectedFont(AValue: TFont);
    procedure CheckFields(FL: TFieldCollection);
  protected
    function DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TKGridDrawState
      ): Boolean; override;
    procedure Paint; override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Build;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure ClearGrid; override;
    procedure GetCellStyle(ACol, ARow: Integer; out AColor: TColor; out AFont: TFont; out AHAlign: TKHAlign; out AVAlign: TKVAlign; IsPrint: Boolean);
    property DataSet: TDataSet read FDS write FDS;
    property Preview: Boolean read FPreview write FPreview;
    property NeedBuild: Boolean read FNeedBuild write FNeedBuild;
  published
    property RowFields: TFieldCollection read FRowFields write SetRowFields;
    property ColFields: TFieldCollection read FColFields write SetColFields;
    property DataFields: TFieldCollection read FDataFields write SetDataFields;
    property GrandTotalFixedColor: TColor read FGrandTotalFixedColor write FGrandTotalFixedColor;
    property GrandTotalColor: TColor read FGrandTotalColor write FGrandTotalColor;
    property GrandTotalFixedFont: TFont read FGrandTotalFixedFont write SetGrandTotalFixedFont;
    property GrandTotalFont: TFont read FGrandTotalFont write SetGrandTotalFont;
    property GrandTotalWidth: Integer read FGrandTotalWidth write
      FGrandTotalWidth;
    property CornerColor: TColor read FCornerColor write FCornerColor;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property GrandTotalCaption: String read FGrandTotalCaption write FGrandTotalCaption;
    property ShowGrandTotalX: Boolean read FShowGrandTotalX write FShowGrandTotalX;
    property ShowGrandTotalY: Boolean read FShowGrandTotalY write FShowGrandTotalY;
    property Id: Integer read FId write FId;
    property FixedFont: TFont read FFixedFont write SetFixedFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property DataDelimiter: String read FDataDelimiter write FDataDelimiter;
    property Indent: Integer read FIndent write FIndent;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property OnBuild: TNotifyEvent read FOnBuild write FOnBuild;

    property PopupMenu stored False;
    property TabStop stored False;
  end;

function PivotGridToHtml(Grid: TdxPivotGrid): String;

implementation

uses
  LazUtf8, Variants, apputils, dxctrls;

type
  PCalcTotal = ^TCalcTotal;
  TCalcTotal = record
    First: Boolean;
    sum: Extended;
    V: Variant;
    n: Integer;
    RowN: Integer;
  end;

  PSpanInfo = ^TSpanInfo;
  TSpanInfo = record
    R, C, RSpan, CSpan: Integer;
  end;

function ColorToHtml(Color: TColor): String;
var
  S: String;
  RGB: LongInt;
begin
  //if ColorToIdent(Color, S) then Exit('""');
  RGB := ColorToRGB(Color);
  S := HexStr(RGB, 8);
  Result := '#' + Copy(S, 7, 2) + Copy(S, 5, 2) + Copy(S, 3, 2);
end;

function FontToHtml(Font: TFont): String;
begin
  Result := '';
  if fsItalic in Font.Style then Result := Result + 'italic ';
  if fsBold in Font.Style then Result := Result + 'bold ';
  if Font.Size > 0 then
  	Result := Result + IntToStr(Font.Size) + 'pt ';
  if (Font.Name > '') and (Font.Name <> 'default') then
  	Result := Result + Font.Name
  else
  	Result := Result + 'verdana';
  if Result <> '' then
	  Result := 'font: ' + Result + ';';
  if Font.Color <> clDefault then
  	Result := Result + 'color: ' + ColorToHtml(Font.Color) + ';';
end;

function VAlignToHtml(V: TKVAlign): String;
begin
  case V of
    valTop: Result := 'top';
    valCenter: Result := 'middle';
    valBottom: Result := 'bottom';
  end;
end;

function HAlignToHtml(V: TKHAlign): String;
begin
  case V of
    halLeft: Result := 'left';
    halCenter: Result := 'center';
    halRight: Result := 'right';
  end;
end;

function PivotGridToHtml(Grid: TdxPivotGrid): String;
var
  S: String;
  r, c, i: Integer;
  Span: TKCellSpan;
  Spans: TList;
  pSI: PSpanInfo;
  Clr: TColor;
  Fnt: TFont;
  HA: TKHAlign;
  VA: TKVAlign;

  function CellSpaned(Col, Row: Integer): Boolean;
  var
    j: Integer;
  begin
    Result := False;
    for j := Spans.Count - 1 downto 0 do
    begin
      with PSpanInfo(Spans[j])^ do
      	if (Col >= C) and (Row >= R) and (Col <= C + CSpan - 1) and (Row <= R + RSpan - 1) then
        	Exit(True);
    end;
  end;

  function GetTableWidth: Integer;
  var
    j: Integer;
  begin
    Result := 0;
    for j := 0 to Grid.ColCount - 1 do
    	Result := Result + Grid.ColWidths[j];
  end;

begin
  if Grid.NeedBuild then Grid.Build;

  Spans := TList.Create;

  S := Format('<table width=%d bgcolor=%s border=1 bordercolor=%s ' +
  	'style="border-collapse: collapse;' + FontToHtml(Grid.Font) + '">',
    [GetTableWidth, ColorToHtml(Grid.Colors.CellBkgnd), ColorToHtml(Grid.Colors.CellLines)]);
  for r := 0 to Grid.RowCount - 1 do
  begin
    S := S + Format('<tr height=%dpx>', [Grid.RowHeights[r]]);
  	for c := 0 to Grid.ColCount - 1 do
    begin
      if CellSpaned(c, r) then Continue;

      S := S + '<td';

      Grid.GetCellStyle(c, r, Clr, Fnt, HA, VA, True);
      if Clr <> clNone then
	      S := S + ' bgcolor=' + ColorToHtml(Clr);
    	S := S + Format(' width=%dpx', [Grid.ColWidths[c]]);

      Span := Grid.CellSpan[c, r];
      if (Span.ColSpan > 1) or (Span.RowSpan > 1) then
      begin
        New(pSI);
        pSI^.C := c; pSI^.R := r;
        pSI^.CSpan := Span.ColSpan; pSI^.RSpan := Span.RowSpan;
        Spans.Add(pSI);

        if Span.ColSpan > 1 then
          S := S + Format(' colspan=%d', [Span.ColSpan]);
        if Span.RowSpan > 1 then
          S := S + Format(' rowspan=%d', [Span.RowSpan]);
      end;

      if HA <> halLeft then
      	S := S + ' align=' + HAlignToHtml(HA);
      if VA <> valCenter then
      	S := S + ' valign=' + VAlignToHtml(VA);

      if (Fnt <> nil) and (not Fnt.IsDefault) then
      	S := S + ' style="' + FontToHtml(Fnt) + '"';

      S := S + '>';
      // Если текст не умещается в ячейку, то она будет автоматически расширена.
      // Это нежелательно, когда надо скрыть ячейки, установив ширину в 0.
      // Поэтому для нулевых текст пропускаем.
      if Grid.ColWidths[c] > 0 then
      	S := S + Grid.Cells[c, r];
      S := S + '</td>';
    end;
    S := S + '</tr>';
  end;
  Result := S + '</table>';

  for i := 0 to Spans.Count - 1 do
  	Dispose(PSpanInfo(Spans[i]));
end;




{ TPivotValues }

function SortFunc(Item1, Item2: Pointer): Integer;
var
  V1, V2: PPivotValue;
begin
  V1 := PPivotValue(Item1);
  V2 := PPivotValue(Item2);
  if V1^.dt = flText then
    Result := MyUtf8CompareText(VarToStr(V1^.V), VarToStr(V2^.V))
  else
  begin
    if V1^.V > V2^.V then Result := 1
    else if V1^.V < V2^.V then Result := -1
    else Result := 0;
  end;
end;

function SortFuncPreview(Item1, Item2: Pointer): Integer;
var
  V1, V2: PPivotValue;
begin
  V1 := PPivotValue(Item1);
  V2 := PPivotValue(Item2);
  Result := MyUtf8CompareText(VarToStr(V1^.V), VarToStr(V2^.V));
end;

function TPivotValues.GetValues(Index: Integer): PPivotValue;
begin
  Result := PPivotValue(Items[Index]);
end;

procedure TPivotValues.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(Values[i]);
  inherited Clear;
end;

function TPivotValues.FindValue(aV: Variant): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Values[i]^.V = aV then Exit(i);
end;

procedure TPivotValues.SortValues;
begin
  Sort(@SortFunc);
end;

procedure TPivotValues.SortValuesPreview;
begin
  Sort(@SortFuncPreview);
end;

{ TFieldItem }

procedure TFieldItem.SetFixedFont(AValue: TFont);
begin
  FFixedFont.Assign(AValue);
end;

procedure TFieldItem.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TFieldItem.SetTotalFixedFont(AValue: TFont);
begin
  FTotalFixedFont.Assign(AValue);
end;

procedure TFieldItem.SetTotalFont(AValue: TFont);
begin
  FTotalFont.Assign(AValue);
end;

constructor TFieldItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FFont := TFont.Create;
  FTotalFont := TFont.Create;
  FFixedFont := TFont.Create;
  FTotalFixedFont := TFont.Create;
  FHAlign := halLeft;
  FVAlign := valCenter;
  Width := ScaleToScreen(80); Height := ScaleToScreen(20);
  TotalWidth := ScaleToScreen(80);
  Color:=clNone; TotalColor := clNone;
  FixedColor := clNone; TotalFixedColor := clNone;
end;

destructor TFieldItem.Destroy;
begin
  FFont.Free;
  FTotalFont.Free;
  FFixedFont.Free;
  FTotalFixedFont.Free;
  inherited Destroy;
end;

procedure TFieldItem.Assign(Source: TPersistent);
var
  Src: TFieldItem;
begin
  if Source is TFieldItem then
  begin
    Src := TFieldItem(Source);
    FieldName := Src.FieldName;
    Caption := Src.Caption;
    Width := Src.Width;
    Height := Src.Height;
    TotalWidth := Src.TotalWidth;
    Func := Src.Func;
    ShowTotal := Src.ShowTotal;
    TotalCaption := Src.TotalCaption;
    FixedColor := Src.FixedColor;
    Color := Src.Color;
    TotalColor := Src.TotalColor;
    TotalFixedColor := Src.TotalFixedColor;
    FixedFont := Src.FixedFont;
    Font := Src.Font;
    TotalFont := Src.TotalFont;
    TotalFixedFont := Src.TotalFixedFont;
    VAlign := Src.VAlign;
    HAlign := Src.HAlign;
    DataType := Src.DataType;
  end
  else
    inherited Assign(Source);
end;

{ TFieldCollection }

function TFieldCollection.GetFields(Index: Integer): TFieldItem;
begin
  Result := TFieldItem(Items[Index]);
end;

function TFieldCollection.AddField: TFieldItem;
begin
  Result := TFieldItem(Add);
end;

function TFieldCollection.FindFieldByFieldName(const S: String): TFieldItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Fields[i].FieldName, S) = 0 then Exit(Fields[i]);
end;

{ TdxPivotGrid }

procedure TdxPivotGrid.SetColFields(AValue: TFieldCollection);
begin
  FColFields.Assign(AValue);
end;

procedure TdxPivotGrid.SetDataFields(AValue: TFieldCollection);
begin
  FDataFields.Assign(AValue);
end;

procedure TdxPivotGrid.SetRowFields(AValue: TFieldCollection);
begin
  FRowFields.Assign(AValue);
end;

procedure TdxPivotGrid.GetValues(const aField: String; dt: TRpFieldType;
  VL: TPivotValues; Filter: array of Variant);
var
  F: TField;
  V: Variant;
  pV: PPivotValue;

  function GetFieldValue(F: TField): Variant;
  begin
  	case dt of
      flText: Result := F.Text;
      flNumber: Result := F.AsFloat;
      flDate, flTime: Result := F.AsDateTime;
      else Result := Null;
    end
  end;

  function IsEqual: Boolean;
  var
    i: Integer;
    FF: TField;
  begin
    Result := True;
    if Length(Filter) = 0 then Exit;

    i := 0;
    while i < Length(Filter) do
    begin
      FF := FDS.FieldByName(Filter[i]);
      if VarToStr(FF.Value) <> VarToStr(Filter[i + 1]) then Exit(False);
      //if GetFieldValue(FF) <> Filter[i + 1] then Exit(False);
      i := i + 2;
    end;
  end;

begin
  VL.Clear;
  FDS.First;
  F := FDS.FieldByName(aField);
  while not FDS.Eof do
  begin
    if IsEqual then
    begin
      V := '';
      if not FPreview then
        {case dt of
          flText: V := F.Text;
          flNumber: V := F.AsFloat;
          flDate, flTime: V := F.AsDateTime;
        end }
      	V := GetFieldValue(F)
      else
        V := F.Text;

      if VL.FindValue(V) < 0 then
      begin
        New(pV);
        pV^.V := V;
        pV^.dt := dt;
        VL.Add(pV);
      end;
    end;
    FDS.Next;
  end;
  if not FPreview then
    VL.SortValues
  else
    VL.SortValuesPreview;
end;

procedure TdxPivotGrid.BuildRows;
var
  RowN: Integer;
  NN, N, j: Integer;

  function _Build(Idx: Integer; Filter: array of Variant): Integer;
  var
    Flt: array of Variant;
    i, j, OldRowN, RowSpn, RN: Integer;
    VL: TPivotValues;
  begin
    OldRowN := RowN;
    SetLength(Flt, Length(Filter) + 2);
    for i := 0 to Length(Filter) - 1 do
      Flt[i] := Filter[i];

    VL := TPivotValues.Create;
    GetValues(FRowFields[Idx].FieldName, FRowFields[Idx].DataType, VL, Filter);
    for i := 0 to VL.Count - 1 do
    begin
      RowCount := RowN + NN;
      Cells[Idx, RowN] := VarToStr(VL[i]^.V);
      Objects[Idx, RowN] := TObject(Idx + 2);
      CellSpan[Idx, RowN] := MakeCellSpan(N - Idx, NN);
      for j := 0 to NN - 1 do
      begin
        Cells[N, RowN + j] := FDataFields[j].Caption;
        Objects[N, RowN + j] := FDataFields[j];
      end;

      RN := RowN;
      RowN := RowN + NN;
      Flt[Length(Flt) - 2] := FRowFields[Idx].FieldName;
      Flt[Length(Flt) - 1] := VL[i]^.V;
      if Idx < FRowFields.Count - 1 then
      begin
        RowSpn := _Build(Idx + 1, Flt);
        CellSpan[Idx, RN + NN] := MakeCellSpan(1, RowSpn);
      end;
    end;

    VL.Free;
    SetLength(Flt, 0);
    Result := RowN - OldRowN;
  end;

begin
  FixedCols := FRowFields.Count + 1;
  for j := 0 to FRowFields.Count - 2 do
    ColWidths[j] := FIndent;

  //RowCount := 100000;
  RowN := FColFields.Count;
  NN := FDataFields.Count;
  N := FRowFields.Count;
  _Build(0, []);
  RowCount := RowN + NN;
  Cells[0, RowN] := '';
  CellSpan[0, RowN] := MakeCellSpan(FRowFields.Count, NN);
  for j := 0 to NN - 1 do
  begin
    Cells[N, RowN + j] := FDataFields[j].Caption;
    Objects[N, RowN + j] := FDataFields[j];
  end;
  Objects[0, RowN] := TObject(1);
  Cells[0, RowN] := FGrandTotalCaption;

  N := FRowFields.Count;
  ColWidths[N - 1] := FRowFields[N - 1].Width;
  ColWidths[N] := FDataFields[0].Width;
  for j := FColFields.Count to RowCount - 1 do
  begin
    RowHeights[j] := TFieldItem(Objects[FRowFields.Count, j]).Height;
  end;
end;

{procedure ShowFilter(Filter: array of Variant);
var
  n: Integer;
  S: String;
begin
  n := 0;
  S := '';
  while n < Length(Filter) do
  begin
    S := S + VarToStr(Filter[n]) + '=' + VarToStr(Filter[n+1]) + LineEnding;
    n := n + 2;
  end;
  ShowMessage(S);
end;    }

// В промежуточныъ итогах хранится индекс поля. Он увеличен на 2, потому что
// индекс поля начинается с 0 и необходимо хранить "маячок" общего итога (значение 1).
// Число столбцов увеличивается на 100. Увеличение столбцов медленная операция,
// т. к. нужно выделить память для каждой строки. Чтобы уменьшить число операций
// выделения, число столбцов увеличивается не на 1, а на 100.
procedure TdxPivotGrid.BuildCols;
var
  ColN, i, j: Integer;

  function _Build(Idx: Integer; Filter: array of Variant): Integer;
  var
    VL: TPivotValues;
    i, OldColN, CN, ColSpn: Integer;
    Flt: array of Variant;
  begin
    OldColN := ColN;
    SetLength(Flt, Length(Filter) + 2);
    for i := 0 to Length(Filter) - 1 do
      Flt[i] := Filter[i];

    VL := TPivotValues.Create;
    //ShowFilter(Filter);
    GetValues(FColFields[Idx].FieldName, FColFields[Idx].DataType, VL, Filter);
    for i := 0 to VL.Count - 1 do
    begin
      //ColCount := ColN + 1;
      if ColN > ColCount - 1 then
        ColCount := ColCount + 100;
      Cells[ColN, Idx] := VarToStr(VL[i]^.V);
      CN := ColN;
      Flt[Length(Flt) - 2] := FColFields[Idx].FieldName;
      Flt[Length(Flt) - 1] := VL[i]^.V;
      if Idx < FColFields.Count - 1 then
      begin
        ColSpn := _Build(Idx + 1, Flt);
        CellSpan[CN, Idx] := MakeCellSpan(ColSpn, 1);
      end;
      if FColFields[Idx].ShowTotal then
      begin
        //ColCount := ColCount + 1;
        if ColN > ColCount - 1 then ColCount := ColCount + 100;
        Inc(ColN);
        CellSpan[ColN, Idx] := MakeCellSpan(1, FColFields.Count - Idx);
        Objects[ColN, Idx] := TObject(Idx+2);
        Cells[ColN, Idx] := FColFields[Idx].TotalCaption;
      end;
      Inc(ColN);
    end;
    VL.Free;
    SetLength(Flt, 0);
    Result := ColN - OldColN;
    Dec(ColN);
  end;

begin
  FixedRows := FColFields.Count;
  ColCount := 100;
  ColN := FRowFields.Count + 1;
  _Build(0, []);
  ColCount := ColN + 2;//ColCount + 1;
  Objects[ColCount - 1, 0] := TObject(1);
  Cells[ColCount - 1, 0] := FGrandTotalCaption;

  for i := FRowFields.Count + 1 to ColCount - 1 do
  begin
    ColWidths[i] := FColFields[FColFields.Count - 1].Width;
    for j := FColFields.Count - 2 downto 0 do
      if Objects[i, j] <> nil then
      begin
        ColWidths[i] := FColFields[j].TotalWidth;
      end;
  end;
  if FShowGrandTotalY then ColWidths[ColCount - 1] := FGrandTotalWidth;
  for i := 0 to FColFields.Count - 1 do
    RowHeights[i] := FColFields[i].Height;
end;

procedure TdxPivotGrid.FillData;
var
  r, c, i: Integer;
  DFL: TList;
  SL: TStringList;
  S: String;

  function FindRow(Vals: TStringList): Integer;
  var
    i, RowN, NN: Integer;
  begin
    RowN := FColFields.Count;
    NN := FDataFields.Count;
    for i := 0 to FRowFields.Count - 1 do
    begin
      while RowN < RowCount - NN do
      begin
        if (PtrInt(Objects[i, RowN]) - 2 = i) and (Cells[i, RowN] = Vals[i]) then Break;
        RowN := RowN + NN;
      end;
    end;
    Result := RowN;
  end;

  function FindCol(Vals: TStringList): Integer;
  var
    i, ColN: Integer;
  begin
    Result := 0;
    ColN := FRowFields.Count;
    for i := 0 to FColFields.Count - 1 do
    begin
      while ColN < ColCount - 1 do
      begin
        if Cells[ColN, i] = Vals[i] then
          Break;
        Inc(ColN);
      end;
    end;
    Result := ColN;
  end;

begin
  DFL := TList.Create;
  SL := TStringList.Create;
  FDS.First;
  for i := 0 to FDataFields.Count - 1 do
    DFL.Add(FDS.FieldByName(FDataFields[i].FieldName));

  while FDS.EOF = False do
  begin
    SL.Clear;
    for i := 0 to FRowFields.Count - 1 do
      SL.Add(FDS.FieldByName(FRowFields[i].FieldName).AsString);
    r := FindRow(SL);
    SL.Clear;
    for i := 0 to FColFields.Count - 1 do
      SL.Add(FDS.FieldByName(FColFields[i].FieldName).AsString);
    c := FindCol(SL);
    if (r >= FixedRows) and (c >= FixedCols) then
    begin
      for i := 0 to DFL.Count - 1 do
        if DFL[i] <> nil then
        begin
          S := Cells[c, r + i];
          if TField(DFL[i]).IsNull then Continue;
          if S <> '' then S := S + FDataDelimiter;
          Cells[c, r + i] := S + TField(DFL[i]).Text;
        end;
    end;
    FDS.Next;
  end;
  DFL.Free;
  SL.Free;
end;

function ConvertValue(const S: String; dt: TRpFieldType; out
  V: Variant): Boolean;
var
  E: Extended;
  N: integer;
  D: TDateTime;
  SS: String;
begin
  Result := True;
  case dt of
    flNumber:
      begin
        SS := StringReplace(S, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);
        Result := TryStrToFloat(SS, E);
        if Result then V := E;
      end;
    flObject, flBool, flCounter, flRecId:
      begin
        Result := TryStrToInt(S, N);
        if Result then V := N;
      end;
    flDate, flTime:
      begin
        Result := TryStrToDateTime(S, D);
        if Result then V := D;
      end
    else
      V := S;
  end;
end;

procedure _DoCalc(pCT: PCalcTotal; fn: TRpTotalFunc; V: Variant);
begin
  if pCT^.First then
  begin
    pCT^.V := V;
    pCT^.n := 1;
    pCT^.First := False;
  end
  else
  begin
    case fn of
      tfSum, tfAvg: pCT^.V := pCT^.V + V;
      tfMax: if V > pCT^.V then pCT^.V := V;
      tfMin: if V < pCT^.V then pCT^.V := V;
    end;
	  Inc(pCT^.n)
  end;
end;

procedure DoCalc(pCT: PCalcTotal; fn: TRpTotalFunc; dt: TRpFieldType; const S, Delim: String);
var
  V: Variant;
  SL: TStringList;
  i: Integer;
begin
  if (fn = tfNone) or (S = '') then Exit;

  if ConvertValue(S, dt, V) then
  	_DoCalc(pCT, fn, V)
  // Считаем множественное значение в ячейке
  else
  begin
    SL := TStringList.Create;
    SplitStr2(S, Delim, SL);
    for i := 0 to SL.Count - 1 do
    begin
      if SL[i] = '' then Continue;
      if ConvertValue(SL[i], dt, V) then
  			_DoCalc(pCT, fn, V);
    end;
    SL.Free;
  end;
end;

function GetResult(pCT: PCalcTotal; fn: TRpTotalFunc; dt: TRpFieldType; const Fmt: String): String;
var
  V: Variant;
begin
  Result := '';
  V := Null;
  case fn of
    tfSum, tfMax, tfMin: V := pCT^.V;
    tfAvg: V := pCT^.V / pCT^.n;
    tfCount: V := pCT^.n;
  end;
  if V <> Null then
    case dt of
      flDate: Result := DateToStr(V);
      flTime: Result := TimeToStr(V);
      else
      begin
        if (dt = flNumber) and (fn <> tfCount) and (Fmt <> '') then
          Result := FormatFloat(Fmt, V)
        else
	        Result := VarToStr(V);
      end;
    end;
end;

function GetValueDisplayFormat(FDS: TDataSet; FI: TFieldItem): String;
var
  F: TField;
begin
  Result := '';
  if FI.Func = tfCount then Exit;
  F := FDS.FieldByName(FI.FieldName);
  if F is TNumericField then
  	Result := TNumericField(F).DisplayFormat;
end;

procedure TdxPivotGrid.CalcRowTotal(r: Integer);
var
  i, j, idx: Integer;
  fn: TRpTotalFunc;
  Tots: TList;
  pCT: PCalcTotal;
  dt: TRpFieldType;
  FI: TFieldItem;
  fmt: String;

  function FindIdx(c: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to FColFields.Count - 1 do
      if Objects[c, i] <> nil then Exit(PtrInt(Objects[c, i]) - 1);
  end;

  // Строки с итогами имеют объединенные ячейки
  function IsRowTotal(r: Integer): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to FRowFields.Count - 1 do
      if CellSpan[i, r].ColSpan > 1 then Exit(True);
  end;

begin
  FI := TFieldItem(Objects[FRowFields.Count, r]);
  fn := FI.Func;
  if fn = tfNone then Exit;
  dt := FI.DataType;
  fmt := GetValueDisplayFormat(FDS, FI);

  // Функция "Количество" неправильно считает итог в итоговой строке, меняем ее на "Сумма".
  if (fn = tfCount) and IsRowTotal(r) then fn := tfSum;

  Tots := TList.Create;
  for i := 0 to FColFields.Count do        { + Общий итог}
  begin
    New(pCT);
    pCT^.First := True;
    Tots.Add(pCT);
  end;

  for i := RowFields.Count + 1 to ColCount - 1 do
  begin
    idx := FindIdx(i);

    if idx < 0 then
    begin
      for j := 0 to Tots.Count - 1 do
      begin
        if j > 0 then
          if not FColFields[j-1].ShowTotal then Continue;
        pCT := PCalcTotal(Tots[j]);
	      DoCalc(pCT, fn, dt, Cells[i, r], FDataDelimiter);
      end;
    end
    else
    begin
      pCT := PCalcTotal(Tots[idx]);
      if not pCT^.First then
      begin
        Cells[i, r] := GetResult(pCT, fn, dt, fmt);
        pCT^.First := True;
      end;
    end;
  end;

  for i := 0 to Tots.Count - 1 do
    Dispose(PCalcTotal(Tots[i]));
  Tots.Free;
end;

procedure TdxPivotGrid.CalcColTotal(c: Integer);
var
  Tots: TList;
  pCT: PCalcTotal;
  i, j, idx, oldIdx, NN, RowN: Integer;
  fn: TRpTotalFunc;
  dt: TRpFieldType;
  FI: TFieldItem;
  fmt: String;

  function FindIdx(r: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := FRowFields.Count - 1 downto 0 do
      if Objects[i, r] <> nil then Exit(PtrInt(Objects[i, r]) - 1);
  end;

begin
  Tots := TList.Create;
  for i := 0 to FRowFields.Count do        { + Общий итог}
  begin
    New(pCT);
    pCT^.First := True;
    pCT^.RowN := 0;
    Tots.Add(pCT);
  end;

  oldIdx := -1;
  NN := FDataFields.Count;
  for i := 0 to NN - 1 do
  begin
    RowN := FColFields.Count + i;
    FI := TFieldItem(Objects[FRowFields.Count, RowN]);
    fn := FI.Func;
    dt := FI.DataType;
    fmt := GetValueDisplayFormat(FDS, FI);

    PCalcTotal(Tots[0])^.RowN:=RowCount - NN + i;
    while RowN < RowCount do
    begin
      idx := FindIdx(RowN - i);
      if idx < oldIdx then
      begin
        for j := idx to oldIdx-1 do
        begin
          pCT := PCalcTotal(Tots[j]);
          if not pCT^.First then
          begin
            Cells[c, pCT^.RowN] := GetResult(pCT, fn, dt, fmt);
            pCT^.First := True;
          end;
        end;
      end
      else if idx = Tots.Count - 1 then
      begin
        for j := 0 to Tots.Count - 1 do
        begin
          pCT := PCalcTotal(Tots[j]);
          DoCalc(pCT, fn, dt, Cells[c, RowN], FDataDelimiter);
        end;
      end;
      oldIdx := idx;
      PCalcTotal(Tots[idx])^.RowN := RowN;
      RowN := RowN + NN;
    end;
  end;

  for i := 0 to Tots.Count - 1 do
    Dispose(PCalcTotal(Tots[i]));
  Tots.Free;
end;

procedure TdxPivotGrid.HideTotals;
var
  i, j, z, RowN, NN: Integer;
begin
  NN := FDataFields.Count;
  for i := FRowFields.Count - 2 downto 0 do
  begin
    if not FRowFields[i].ShowTotal then
    begin
      RowN := FColFields.Count;
      while RowN < RowCount - NN do
      begin
        if Objects[i, RowN] <> nil then
        begin
          for j := FRowFields.Count to ColCount - 1 do
          begin
            for z := 0 to NN - 1 do
            begin
              if j = FRowFields.Count then
              begin
                RowHeights[RowN + z] := 0;
              end;
              Cells[j, RowN + z] := '';
            end;
            CellSpan[j, RowN] := MakeCellSpan(1, NN);
          end;
          CellSpan[i, RowN] := MakeCellSpan(FRowFields.Count - i + 1, NN);
          RowHeights[RowN] := FRowFields[i].Height;
        end;
        RowN := RowN + NN;
      end;
    end;
  end;
  if not FShowGrandTotalX then
    RowCount := RowCount - NN;
  if not ShowGrandTotalY then
    ColCount := ColCount - 1;
end;

procedure TdxPivotGrid.SetFixedFont(AValue: TFont);
begin
  FFixedFont.Assign(AValue);
end;

{procedure TdxPivotGrid.HideTotals;
var
  i, j, NN: Integer;
  S: String;

  procedure ChangeParentRowSpan;
  begin

  end;

begin
  NN := FDataFields.Count;
  CellSpan[0, RowCount - NN] := MakeCellSpan(1, 1);
  for i := FRowFields.Count - 2 downto 0 do
  begin
    if not FRowFields[i].ShowTotal then
    begin
      for j := RowCount - NN - NN downto FColFields.Count do
        if Objects[i, j] <> nil then
        begin
          CellSpan[i, j] := MakeCellSpan(1, 1);
          DeleteRows(j+1, NN-1);
          Cells[NN, j] := '';
          CellSpan[i, j] := MakeCellSpan(NN - i + 1, 1);
          Objects[NN, j] := FRowFields[i];                 // Подменяем FDataFields
          RowHeights[j] := FRowFields[i].Height;
        end;
    end;
  end;
  //CellSpan[0, RowCount - NN] := MakeCellSpan(FRowFields.Count - 1, NN);
end;   }

procedure TdxPivotGrid.SetGrandTotalFixedFont(AValue: TFont);
begin
  FGrandTotalFixedFont.Assign(AValue);
end;

procedure TdxPivotGrid.SetGrandTotalFont(AValue: TFont);
begin
  FGrandTotalFont.Assign(AValue);
end;

procedure TdxPivotGrid.SetSelectedFont(AValue: TFont);
begin
  FSelectedFont.Assign(AValue);
end;

procedure TdxPivotGrid.CheckFields(FL: TFieldCollection);
var
  i: Integer;
  F: TField;
begin
  for i := FL.Count - 1 downto 0 do
  begin
    F := FDS.FindField(FL[i].FieldName);
    if F = nil then FL.Delete(i);
  end;
end;

function TdxPivotGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TKGridDrawState): Boolean;
var
  ColFN, RowFN: Integer;
  Clr: TColor;
  Fnt: TFont;
  VA: TKVAlign;
  HA: TKHAlign;

  {function GetRowIdx(c: Integer): Integer;
  var
    j: Integer;
  begin
    Result := -1;
    for j := 0 to FColFields.Count - 2 do
      if CellSpan[c, j].RowSpan > 1 then Exit(j);
  end;

  function GetColIdx(r: Integer): Integer;
  var
    j: Integer;
  begin
    Result := -1;
    for j := 0 to FRowFields.Count - 1 do
      if Objects[j, r] <> nil then Exit(j);
  end;  }

begin
  if not FBuilded then
  begin
    Result := inherited DrawCell(ACol, ARow, ARect, AState);
    Exit;
  end;

  if FWordWrap then
  begin
    CellPainter.Attributes:=[taWordBreak, taClip, taTrimWhiteSpaces];
  end
  else
    CellPainter.Attributes:=[taClip];

  ColFN := FColFields.Count;
  RowFN := FRowFields.Count;

  GetCellStyle(ACol, ARow, Clr, Fnt, HA, VA, False);
  if gdFocused in AState then
  begin
    Fnt := FSelectedFont;
    Clr := Colors.FocusedCellBkGnd;
  end;

  if (ACol > RowFN) and (ARow >= ColFN) and (Objects[RowFN, ARow] <> nil) then
  begin
    with TFieldItem(Objects[RowFN, ARow]) do
    begin
      Self.CellPainter.VAlign:=VA;
      Self.CellPainter.HAlign:=HA;
    end;
  end;

  {if (not FBuilded) or (AState * [gdFocused] <> []) then
  begin
    Canvas.Font.Assign(FSelectedFont);
    Result := inherited DrawCell(ACol, ARow, ARect, AState);
    Exit;
  end;  }

  if (Fnt <> nil) and Fnt.IsDefault and ((ACol < FixedCols) or (ARow < FixedRows)) then
    Fnt := FFixedFont;

  if (Fnt <> nil) and (not Fnt.IsDefault) then
    Canvas.Font.Assign(Fnt);
  if Clr <> clNone then
    Canvas.Brush.Color := Clr;

  Result:=inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TdxPivotGrid.Paint;
var
  TS: TTextStyle;
begin
  inherited Paint;
  if csDesigning in ComponentState then
  begin
    TS := Canvas.TextStyle;
    TS.Alignment := taCenter;
    TS.Layout := tlCenter;
    TS.SingleLine := False;
    TS.WordBreak := True;
    Canvas.TextRect(ClientRect, 0, 0, Name, TS);
  end;
end;

procedure TdxPivotGrid.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  Invalidate;
end;

procedure SetDefaultFont(F: TFont);
begin
  F.Name := 'Verdana';
  F.Size := 10;
end;

constructor TdxPivotGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStopTab := True;
  Width := ScaleToScreen(300);
  Height := ScaleToScreen(150);
  DefaultRowHeight := ScaleToScreen(DefaultRowHeight);
  DefaultColWidth := ScaleToScreen(DefaultColWidth);
  FRowFields := TFieldCollection.Create(TFieldItem);
  FColFields := TFieldCollection.Create(TFieldItem);
  FDataFields := TFieldCollection.Create(TFieldItem);
  SetDefaultFont(Font);
  FFixedFont := TFont.Create;
  SetDefaultFont(FFixedFont);
  FSelectedFont := TFont.Create;
  SetDefaultFont(FSelectedFont);
  FGrandTotalFont := TFont.Create;
  SetDefaultFont(FGrandTotalFont);
  FGrandTotalFixedFont := TFont.Create;
  SetDefaultFont(FGrandTotalFixedFont);
  FGrandTotalColor := Colors.FixedCellBkGnd;// clNone;
  FGrandTotalFixedColor := Colors.FixedCellBkGnd;// clNone;
  FCornerColor := Colors.FixedCellBkGnd;// clNone;
  FGrandTotalWidth:=DefaultColWidth;
  FDataDelimiter := '; ';
  Indent := ScaleToScreen(10);
  Options := Options - [goHeader, goThemedCells, goIndicateHiddenCells, goRangeSelect] +
    [goIndicateSelection, goRowSizing, goColSizing];
  MinColWidth:=1;
  MinRowHeight:=1;
  ColCount := 2;
  RowCount := 2;
end;

destructor TdxPivotGrid.Destroy;
begin
  FSelectedFont.Free;
  FFixedFont.Free;
  FGrandTotalFixedFont.Free;
  FGrandTotalFont.Free;
  FDataFields.Free;
  FColFields.Free;
  FRowFields.Free;
  inherited Destroy;
end;

procedure TdxPivotGrid.Build;
var
  i: Integer;
  AftScr: TDataSetNotifyEvent;
  B: TBookmark;
begin
  if not FPreview then FindQueryGrid(TdxForm(Owner), FId).RequeryIfNeed;

  FNeedBuild := False;

  CheckFields(FRowFields);
  CheckFields(FColFields);
  CheckFields(FDataFields);

  LockUpdate;
  FBuilded := False;
  AftScr := FDS.AfterScroll;
  FDS.AfterScroll := nil;
  B := FDS.GetBookmark;
  FDS.DisableControls;
  try try
    ClearGrid;

    if (FColFields.Count = 0) or (FRowFields.Count = 0) or (FDataFields.Count = 0)
    	or (FDS.RecordCount = 0) then Exit;

    BuildRows;
    BuildCols;
    CellSpan[0, 0] := MakeCellSpan(FRowFields.Count + 1, FColFields.Count);
    CellSpan[ColCount - 1, 0] := MakeCellSpan(1, FColFields.Count);
    FillData;
    for i := RowFields.Count + 1 to ColCount - 1 do
      CalcColTotal(i);
    for i := FColFields.Count to RowCount - 1 do
      CalcRowTotal(i);
    HideTotals;
    FBuilded := True;
  except
    on E: Exception do
    begin
      ClearGrid;
      ErrMsg(ExceptionToString(E, False, False));
    end;
  end;
  finally
    UnlockUpdate;
    FDS.GotoBookmark(B);
    FDS.FreeBookmark(B);
    FDS.EnableControls;
    FDS.AfterScroll := AftScr;
  end;

  if FOnBuild <> nil then FOnBuild(Self);
end;

procedure TdxPivotGrid.Assign(Source: TPersistent);
var
  Src: TdxPivotGrid;
begin
  if Source is TdxPivotGrid then
  begin
    Src := TdxPivotGrid(Source);
    Colors := Src.Colors;
    CornerColor:=Src.CornerColor;
    GrandTotalColor:=Src.GrandTotalColor;
    GrandTotalFixedColor:=Src.GrandTotalFixedColor;
    GrandTotalFont := Src.GrandTotalFont;
    GrandTotalFixedFont := Src.GrandTotalFixedFont;
    GrandTotalWidth := Src.GrandTotalWidth;
    GrandTotalCaption:=Src.GrandTotalCaption;
    ShowGrandTotalX:=Src.ShowGrandTotalX;
    ShowGrandTotalY:=Src.ShowGrandTotalY;
    Font := Src.Font;
    FixedFont := Src.FixedFont;
    SelectedFont := Src.SelectedFont;
    RowFields := Src.RowFields;
    ColFields := Src.ColFields;
    DataFields := Src.DataFields;
    Flat := Src.Flat;
    WordWrap := Src.WordWrap;
    Options := Src.Options;
    OptionsEx := Src.OptionsEx;
    DataDelimiter := Src.DataDelimiter;
    Indent := Src.Indent;
  end
  else
    inherited Assign(Source);
end;

procedure TdxPivotGrid.Clear;
begin
  FRowFields.Clear;
  FColFields.Clear;
  FDataFields.Clear;
  FId := 0;
end;

procedure TdxPivotGrid.ClearGrid;
begin
  inherited ClearGrid;
  ColCount := 0;
  RowCount := 0;
  FixedCols := 0;
  FixedRows := 0;
end;

procedure TdxPivotGrid.GetCellStyle(ACol, ARow: Integer; out AColor: TColor;
  out AFont: TFont; out AHAlign: TKHAlign; out AVAlign: TKVAlign;
  IsPrint: Boolean);
var
  i, ColFN, RowFN, DatFN: Integer;
  Clr: TColor;
  Fnt: TFont;

  function GetRowIdx(c: Integer): Integer;
  var
    j: Integer;
  begin
    Result := -1;
    for j := 0 to FColFields.Count - 2 do
      if CellSpan[c, j].RowSpan > 1 then Exit(j);
  end;

  function GetColIdx(r: Integer): Integer;
  var
    j: Integer;
  begin
    Result := -1;
    for j := 0 to FRowFields.Count - 1 do
      if Objects[j, r] <> nil then Exit(j);
  end;

begin
  Clr := clNone;
  Fnt := nil;
  ColFN := FColFields.Count;
  RowFN := FRowFields.Count;
  DatFN := FDataFields.Count;

  // На случай, если таблица изменяется в скрипте.
  //if ((ARow <> 0) and (ACol <> 0)) or (Objects[RowFN, ARow] = nil) then Exit;

  // Левый верхний угол
  if (ARow = 0) and (ACol = 0) then
    Clr := FCornerColor
  // Общий итог по оси Х фикс.
  else if (ACol <= RowFN) and (ARow >= RowCount - DatFN) and FShowGrandTotalX then
  begin
    Clr := TFieldItem(Objects[RowFN, ARow]).FixedColor;
    Fnt := TFieldItem(Objects[RowFN, ARow]).FixedFont;
    if Clr = clNone then
      Clr := FGrandTotalFixedColor;
    if Fnt.IsDefault then
      Fnt := FGrandTotalFixedFont;
  end
  // Обший итог по оси Х данные
  else if (ACol > RowFN) and (ARow >= RowCount - DatFN) and FShowGrandTotalX then
  begin
    Clr := TFieldItem(Objects[RowFN, ARow]).Color;
    Fnt := TFieldItem(Objects[RowFN, ARow]).Font;

    if Clr = clNone then
      Clr := FGrandTotalColor;
    if Fnt.IsDefault then
      Fnt := FGrandTotalFont;

    if ShowGrandTotalY and (ACol = ColCount - 1) then
    else
    begin
      i := GetRowIdx(ACol);
      if i >= 0 then
      begin
        if Clr = clNone then
          Clr := FColFields[i].TotalColor;
        if Fnt.IsDefault then
          Fnt := FColFields[i].TotalFont;
      end;
    end;
  end
  // Общий итог по оси У фикс.
  else if (ARow = 0) and (ACol = ColCount - 1) and FShowGrandTotalY then
  begin
    Clr := FGrandTotalFixedColor;
    Fnt := FGrandTotalFixedFont;
  end
  // Общий итог по оси У данные
  else if (ARow >= ColFN) and (ACol = ColCount - 1) and FShowGrandTotalY then
  begin
    Clr := TFieldItem(Objects[RowFN, ARow]).Color;
    Fnt := TFieldItem(Objects[RowFN, ARow]).Font;
    if Clr = clNone then
      Clr := FGrandTotalColor;
    if Fnt.IsDefault then
      Fnt := FGrandTotalFont;
  end
  // Поля строк
  else if ACol < RowFN then
  begin
    Clr := FRowFields[ACol].FixedColor;
    Fnt := FRowFields[ACol].FixedFont;
  end
  // Поля данных фикс.
  else if (ACol = RowFN) and (ARow >= ColFN) then
  begin
    Clr := TFieldItem(Objects[RowFN, ARow]).FixedColor;
    Fnt := TFieldItem(Objects[RowFN, ARow]).FixedFont;
    if ((Clr = clNone) or (Fnt.IsDefault)) and (Objects[ACol, ARow] <> nil) then
    begin
      i := TFieldItem(Objects[ACol, ARow]).Index;
      i := GetColIdx(ARow - i);
      if Clr = clNone then
        Clr := FRowFields[i].FixedColor;
      if Fnt.IsDefault then
        Fnt := FRowFields[i].FixedFont;
    end;
  end
  // Поля данных данные
  else if (ACol > RowFN) and (ARow >= ColFN) then
  begin
    // Поля данных
    Clr := TFieldItem(Objects[RowFN, ARow]).Color;
    Fnt := TFieldItem(Objects[RowFN, ARow]).Font;
    i := TFieldItem(Objects[RowFN, ARow]).Index;
    i := GetColIdx(ARow - i);
    if (Clr = clNone) or (Fnt.IsDefault) or (FRowFields[i].ShowTotal = False) then
    begin
      // Поля строк данные
      if (Clr = clNone) or ((FRowFields[i].ShowTotal = False) and
        (i < RowFN - 1)) then Clr := FRowFields[i].Color;
      if (Fnt.IsDefault) or ((FRowFields[i].ShowTotal = False) and
        (i < RowFN - 1)) then Fnt := FRowFields[i].Font;

      if (Clr = clNone) or (Fnt.IsDefault) then
        i := GetRowIdx(ACol);
      // Поля столбцов данные
      if Clr = clNone then
      begin
        if i < 0 then
          Clr := FColFields[ColFN - 1].Color
        // Промежуточный итог по оси У
        else
          Clr := FColFields[i].TotalColor;
      end;
      if Fnt.IsDefault then
      begin
        if i < 0 then
          Fnt := FColFields[ColFN - 1].Font
        // Промежуточный итог по оси У
        else
          Fnt := FColFields[i].TotalFont;
      end;
    end;
  end
  // Поля столбцов
  else if ARow < FColFields.Count then
  begin
    Clr := FColFields[ARow].FixedColor;
    Fnt := FColFields[ARow].FixedFont;
    if ARow < FColFields.Count - 1 then
    begin
      if CellSpan[ACol, ARow].RowSpan > 1 then
      begin
        Clr := FColFields[ARow].TotalFixedColor;
        Fnt := FColFields[ARow].TotalFixedFont;
      end;
    end;
  end;

  // Стандартные
  if IsPrint {and (Clr = clNone) or (Fnt = nil) or (Fnt.IsDefault)} then
  begin
    if (ARow < FixedRows) or (ACol < FixedCols) then
    begin
      if Clr = clNone then
	      Clr := Colors.FixedCellBkGnd;
      if (Fnt = nil) or (Fnt.IsDefault) then
	      Fnt := FixedFont;
    end;
  end;

  AVAlign := valCenter;
  AHAlign := halLeft;
  if (ACol > RowFN) and (ARow >= ColFn) and (Objects[RowFN, ARow] <> nil) then
  begin
    with TFieldItem(Objects[RowFN, ARow]) do
    begin
      AVAlign:=VAlign;
      AHAlign:=HAlign;
    end;
  end;

  //if (ACol = 0) and (ARow = 1) then ShowMessage('Stop');
  AColor := Clr; AFont := Fnt;
end;

initialization
  RegisterClass(TdxPivotGrid);

end.

