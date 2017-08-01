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
    FPreview: Boolean;
    FSelectedFont: TFont;
    FShowGrandTotalX: Boolean;
    FShowGrandTotalY: Boolean;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Build;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    property DataSet: TDataSet read FDS write FDS;
    property Preview: Boolean read FPreview write FPreview;
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

    property PopupMenu stored False;
  end;

implementation

uses
  LazUtf8, Variants, apputils;

type
  PCalcTotal = ^TCalcTotal;
  TCalcTotal = record
    First: Boolean;
    sum: Extended;
    V: Variant;
    n: Integer;
    RowN: Integer;
  end;

{ TPivotValues }

function SortFunc(Item1, Item2: Pointer): Integer;
var
  V1, V2: PPivotValue;
begin
  V1 := PPivotValue(Item1);
  V2 := PPivotValue(Item2);
  if V1^.dt = flText then
    Result := Utf8CompareText(VarToStr(V1^.V), VarToStr(V2^.V))
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
  Result := Utf8CompareText(VarToStr(V1^.V), VarToStr(V2^.V));
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
  Width := 80; Height := 20;
  TotalWidth := 80;
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
      //if Utf8CompareText(FF.AsString, Filter[i + 1]) <> 0 then Exit(False);
      if FF.Value <> Filter[i + 1] then Exit(False);
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
        case dt of
          flText: V := F.Text;
          flNumber: V := F.AsFloat;
          flDate, flTime: V := F.AsDateTime;
        end
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
      //if RowN > RowCount - 1 then RowCount := RowCount + 1000;
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
        if (Integer(Objects[i, RowN]) - 2 = i) and  {(Utf8CompareText(Cells[i, RowN], Vals[i]) = 0) then}
          (Cells[i, RowN] = Vals[i]) then
          Break;
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
    //ColN := 2;
    ColN := FRowFields.Count;
    for i := 0 to FColFields.Count - 1 do
    begin
      while ColN < ColCount - 1 do
      begin
        //if Utf8CompareText(Cells[ColN, i], Vals[i]) = 0 then
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
    //if (r > 0) and (c > 0) then
    if (r >= FixedRows) and (c >= FixedCols) then
    begin
      for i := 0 to DFL.Count - 1 do
        if DFL[i] <> nil then
        begin
          S := Cells[c, r + i];
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
begin
  Result := True;
  case dt of
    flNumber:
      begin
        Result := TryStrToFloat(S, E);
        if Result then V := E;
      end;
    flObject, flBool, flCounter:
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

procedure DoCalc(pCT: PCalcTotal; fn: TRpTotalFunc; dt: TRpFieldType; const S: String);
var
  V: Variant;
begin
  if (fn = tfNone) or (S = '') then Exit;
  if not ConvertValue(S, dt, V) then Exit;

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
    Inc(pCT^.n);
  end;
end;

function GetResult(pCT: PCalcTotal; fn: TRpTotalFunc; dt: TRpFieldType): String;
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
      else Result := VarToStr(V);
    end;
end;

procedure TdxPivotGrid.CalcRowTotal(r: Integer);
var
  i, j, idx: Integer;
  //E: Extended;
  fn: TRpTotalFunc;
  Tots: TList;
  pCT: PCalcTotal;
  dt: TRpFieldType;

  function FindIdx(c: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to FColFields.Count - 1 do
      if Objects[c, i] <> nil then Exit(Integer(Objects[c, i]) - 1);
  end;

begin
  fn := TFieldItem(Objects[FRowFields.Count, r]).Func;
  if fn = tfNone then Exit;
  dt := TFieldItem(Objects[FRowFields.Count, r]).DataType;

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
    //if (idx < 0) and (not TryStrToFloat(Cells[i, r], E)) then Continue;

    if idx < 0 then
    begin
      for j := 0 to Tots.Count - 1 do
      begin
        if j > 0 then
          if not FColFields[j-1].ShowTotal then Continue;
        pCT := PCalcTotal(Tots[j]);
        DoCalc(pCT, fn, dt, Cells[i, r]);
        {if pCT^.First then
        begin
          pCT^.n:=1;
          pCT^.sum := E;
          pCT^.mx := E;
          pCT^.mn := E;
          pCT^.First := False;
        end
        else
        begin
          Inc(pCT^.n);
          pCT^.sum := pCT^.sum + E;
          if E > pCT^.mx then pCT^.mx := E;
          if E < pCT^.mn then pCT^.mn := E;
        end;  }
      end;
    end
    else
    begin
      pCT := PCalcTotal(Tots[idx]);
      if not pCT^.First then
      begin
        Cells[i, r] := GetResult(pCT, fn, dt);
        pCT^.First := True;
        {case fn of
          tfSum: E := pCT^.sum;
          tfAvg: E := pCT^.sum / pCT^.n;
          tfMax: E := pCT^.mx;
          tfMin: E := pCT^.mn;
          tfCount: E := pCT^.n;
        end;
        if fn <> tfNone then
          Cells[i, r] := FloatToStr(E);}
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
  //E, T: Extended;
  fn: TRpTotalFunc;
  dt: TRpFieldType;

  function FindIdx(r: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := FRowFields.Count - 1 downto 0 do
      if Objects[i, r] <> nil then Exit(Integer(Objects[i, r]) - 1);
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
    fn := TFieldItem(Objects[FRowFields.Count, RowN]).Func;
    dt := TFieldItem(Objects[FRowFields.Count, RowN]).DataType;
    PCalcTotal(Tots[0])^.RowN:=RowCount - NN + i;
    while RowN < RowCount do
    begin
      idx := FindIdx(RowN - i);// Integer(Objects[0, RowN - i]) - 1;
      if idx < oldIdx then
      begin
        for j := idx to oldIdx-1 do
        begin
          pCT := PCalcTotal(Tots[j]);
          if not pCT^.First then
          begin
            Cells[c, pCT^.RowN] := GetResult(pCT, fn, dt);
            {case fn of
              tfSum: T := pCT^.sum;
              tfAvg: T := pCT^.sum / pCT^.n;
              tfMax: T := pCT^.mx;
              tfMin: T := pCT^.mn;
              tfCount: T := pCT^.n;
            end;
            if fn <> tfNone then
              Cells[c, pCT^.RowN] := FloatToStr(T);}
            pCT^.First := True;
          end;
        end;
      end
      else if idx = Tots.Count - 1 then
      begin
        //if TryStrToFloat(Cells[c, RowN], E) then
          for j := 0 to Tots.Count - 1 do
          begin
            pCT := PCalcTotal(Tots[j]);
            DoCalc(pCT, fn, dt, Cells[c, RowN]);
            {if pCT^.First then
            begin
              pCT^.n:=1;
              pCT^.sum := E;
              pCT^.mx := E;
              pCT^.mn := E;
              pCT^.First := False;
            end
            else
            begin
              Inc(pCT^.n);
              pCT^.sum := pCT^.sum + E;
              if E > pCT^.mx then pCT^.mx := E;
              if E < pCT^.mn then pCT^.mn := E;
            end; }
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
  //if (ColFN = 0) or (RowFN = 0) or (DatFN = 0) or (csDesigning in ComponentState) then
  if FWordWrap then
  begin
    CellPainter.Attributes:=[taWordBreak, taClip, taTrimWhiteSpaces];
  end
  else
    CellPainter.Attributes:=[taClip];

  if (not FBuilded) or (AState * [gdFocused] <> []) then
  begin
    Canvas.Font.Assign(FSelectedFont);
    Result := inherited DrawCell(ACol, ARow, ARect, AState);
    Exit;
  end;

  Clr := clNone;
  Fnt := nil;
  ColFN := FColFields.Count;
  RowFN := FRowFields.Count;
  DatFN := FDataFields.Count;

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

  if (Fnt <> nil) and Fnt.IsDefault and ((ACol < FixedCols) or (ARow < FixedRows)) then
    Fnt := FFixedFont;

  if (Fnt <> nil) and (not Fnt.IsDefault) then
    Canvas.Font.Assign(Fnt);
  if Clr <> clNone then
    Canvas.Brush.Color := Clr;
  if (ACol > RowFN) and (ARow >= ColFn) and (Objects[RowFN, ARow] <> nil) then
  begin
    with TFieldItem(Objects[RowFN, ARow]) do
    begin
      Self.CellPainter.VAlign:=VAlign;
      Self.CellPainter.HAlign:=HAlign;
    end;
  end;

  Result:=inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure SetDefaultFont(F: TFont);
begin
  F.Name := 'Verdana';
  F.Size := 10;
end;

constructor TdxPivotGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  FGrandTotalColor := clNone;
  FGrandTotalFixedColor := clNone;
  FCornerColor := clNone;
  FGrandTotalWidth:=DefaultColWidth;
  FDataDelimiter := '; ';
  Indent := 10;
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
    ColCount := 2;
    RowCount := 2;
    if (FColFields.Count = 0) or (FRowFields.Count = 0) or (FDataFields.Count = 0) then Exit;

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
      ColCount := 2;
      RowCount := 2;
      ErrMsg(E.Message);
    end;
  end;
  finally
    UnlockUpdate;
    FDS.GotoBookmark(B);
    FDS.FreeBookmark(B);
    FDS.EnableControls;
    FDS.AfterScroll := AftScr;
  end;
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

initialization
  RegisterClass(TdxPivotGrid);

end.

