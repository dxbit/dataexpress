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

unit DXCharts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, TAGraph, TASeries, TAStyles, TAChartAxis,
  TAChartUtils, TALegend, TAChartAxisUtils, TASources, TABGRAUtils,
  TADrawerCanvas, TADrawerBGRA, TAGUIConnector, BGRABitmap, BGRABitmapTypes,
  TADrawUtils, TACustomSource, Dialogs, DXReports, db, FPCanvas,
  TypInfo, TACustomSeries, strconsts;

// В procedure TChartSeries.Assign(ASource: TPersistent); была опечатка
// Source, а надо ASource.

type

  { TdxDrawer }

  // Добавил в procedure TBGRABitmapDrawer.SetFont(AFont: TFPCustomFont)
  // установку сглаживания шрифта, если угол не прямой.
  TdxDrawer = class(TBGRABitmapDrawer, IChartTCanvasDrawer)
  public
    constructor Create(ABitmap: TBGRABitmap);
    destructor Destroy; override;
    procedure SetSize(ASize: TPoint);
    procedure PaintOnCanvas(ACanvas: TCanvas; const ARect: TRect);
    function GetCanvas: TCanvas;
    property Canvas: TCanvas read GetCanvas;
  end;

  { TdxGUIConnector }

  TdxGUIConnector = class(TChartGUIConnector)
  public
    procedure CreateDrawer(var AData: TChartGUIConnectorData); override;
    procedure SetBounds(var AData: TChartGUIConnectorData); override;
    procedure Display(var AData: TChartGUIConnectorData); override;
  end;

  { TdxFieldYItem }

  TdxFieldYItem = class(TCollectionItem)
  private
    FFieldName: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property FieldName: String read FFieldName write FFieldName;
  end;

  { TdxFieldYCollection }

  TdxFieldYCollection = class(TCollection)
  private
    function GetFields(Index: Integer): TdxFieldYItem;
  public
    property Fields[Index: Integer]: TdxFieldYItem read GetFields; default;
  end;

  { TdxChartSource }

  TdxChartSource = class(TListChartSource)
  private
    FYFields: TdxFieldYCollection;
    FLabelField: String;
    FQuery: Integer;
    procedure SetYFields(AValue: TdxFieldYCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Query: Integer read FQuery write FQuery;
    property LabelField: String read FLabelField write FLabelField;
    property YFields: TdxFieldYCollection read FYFields write SetYFields;

    property DataPoints stored False;
    property Sorted stored False;
    property XErrorBarData stored False;
    property YErrorBarData stored False;
  end;

  TdxChartStyles = class(TChartStyles)

  end;

  TdxBarEffects = (befNone, befPhong, befChocolate);

  { TdxBarSeries }

  TdxBarSeries = class(TBarSeries)
  private
    FEffects: TdxBarEffects;
    procedure BeforeDrawBar(ASender: TBarSeries; ACanvas: TCanvas;
      const ARect: TRect; APointIndex, AStackIndex: Integer;
      var ADoDefaultDrawing: Boolean);
    procedure SetEffects(AValue: TdxBarEffects);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Effects: TdxBarEffects read FEffects write SetEffects;
  end;

  { TdxPieSeries }

  TdxPieSeries = class(TPieSeries)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxLineSeries }

  TdxLineSeries = class(TLineSeries)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxAreaSeries }

  TdxAreaSeries = class(TAreaSeries)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxChart }

  TdxChart = class(TChart)
  private
    FNeedBuild: Boolean;
    FSaveImageHeight: Integer;
    FSaveImageWidth: Integer;
    FSaveOriginalSize: Boolean;
    FStyles: TdxChartStyles;
    FSource, FLabelSource: TdxChartSource;
    function GetChartSeries: TChartSeries;
    function GetChartLabelSource: TdxChartSource;
    function GetChartSource: TdxChartSource;
    function GetChartStyles: TdxChartStyles;
    function GetLabelField: String;
    function GetQuery: Integer;
    function GetStyles: TChartStyleList;
    function GetYCount: Integer;
    function GetYFields: TdxFieldYCollection;
    procedure SetLabelField(AValue: String);
    procedure SetQuery(AValue: Integer);
    procedure SetStyles(AValue: TChartStyleList);
    procedure SetYCount(AValue: Integer);
    procedure SetYFields(AValue: TdxFieldYCollection);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetChildren(AProc: TGetChildProc; ARoot: TComponent); override;
    procedure FillDemoSource;
    procedure Build;
    procedure Init;
    procedure SaveImageToFile(const FileName: String);
    property StylesComponent: TdxChartStyles read FStyles;
    property NeedBuild: Boolean read FNeedBuild write FNeedBuild;
  published
    property ChartSeries: TChartSeries read GetChartSeries stored False;
    property Query: Integer read GetQuery write SetQuery;
    property LabelField: String read GetLabelField write SetLabelField;
    property YFields: TdxFieldYCollection read GetYFields write SetYFields;
    property YCount: Integer read GetYCount write SetYCount;
    property Styles: TChartStyleList read GetStyles write SetStyles;
    property Source: TdxChartSource read FSource stored False;
    property LabelSource: TdxChartSource read FLabelSource stored False;
    property SaveImageWidth: Integer read FSaveImageWidth write FSaveImageWidth;
    property SaveImageHeight: Integer read FSaveImageHeight write FSaveImageHeight;
    property SaveOriginalSize: Boolean read FSaveOriginalSize write FSaveOriginalSize;

    property PopupMenu stored False;
  end;

implementation

uses
  dxctrls, apputils, sqlgen;

{ TdxFieldYItem }

procedure TdxFieldYItem.Assign(Source: TPersistent);
begin
  with TdxFieldYItem(Source) do
    Self.FieldName := FieldName;
end;

{ TdxAreaSeries }

constructor TdxAreaSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Marks.LabelFont.Name := 'Verdana';
  Marks.LabelFont.Size := 10;
  Marks.LabelFont.Color := clBlack;
  Marks.LinkPen.Color := clBlack;
end;

{ TdxLineSeries }

constructor TdxLineSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Marks.LabelFont.Name := 'Verdana';
  Marks.LabelFont.Size := 10;
  Marks.LabelFont.Color := clBlack;
  Marks.LinkPen.Color := clBlack;
end;

{ TdxPieSeries }

constructor TdxPieSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Marks.LabelFont.Name := 'Verdana';
  Marks.LabelFont.Size := 10;
  Marks.LabelFont.Color := clBlack;
  Marks.LinkPen.Color := clBlack;
end;

{ TdxDrawer }

constructor TdxDrawer.Create(ABitmap: TBGRABitmap);
begin
  inherited Create(ABitmap);
  IChartDrawer(Self).DoGetFontOrientation := @CanvasGetFontOrientationFunc;
end;

destructor TdxDrawer.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FBitmap);
end;

procedure TdxDrawer.SetSize(ASize: TPoint);
begin
  FBitmap.SetSize(ASize.X, ASize.Y);
end;

procedure TdxDrawer.PaintOnCanvas(ACanvas: TCanvas; const ARect: TRect);
begin
  FBitmap.Draw(ACanvas, ARect);
end;

function TdxDrawer.GetCanvas: TCanvas;
begin
  Result := FBitmap.Canvas;
end;

{ TdxGUIConnector }

procedure TdxGUIConnector.CreateDrawer(var AData: TChartGUIConnectorData);
begin
  AData.FDrawer := TdxDrawer.Create(TBGRABitmap.Create(0, 0));
  //AData.FDrawer.DoGetFontOrientation := @CanvasGetFontOrientationFunc;
end;

procedure TdxGUIConnector.SetBounds(var AData: TChartGUIConnectorData);
begin
  AData.FDrawerBounds.TopLeft := Point(0, 0);
  AData.FDrawerBounds.BottomRight :=
    AData.FBounds.BottomRight - AData.FBounds.TopLeft;
  (AData.FDrawer as TdxDrawer).SetSize(
    AData.FDrawerBounds.BottomRight);
end;

procedure TdxGUIConnector.Display(var AData: TChartGUIConnectorData);
begin
  (AData.FDrawer as TdxDrawer).PaintOnCanvas(
    AData.FCanvas, AData.FBounds);
end;

{ TdxBarSeries }

procedure TdxBarSeries.SetEffects(AValue: TdxBarEffects);
begin
  if FEffects=AValue then Exit;
  FEffects:=AValue;
  UpdateParentChart;
end;

function MyBarColor(ASeries: TBarSeries; APointIndex, AStackIndex: Integer): TBGRAPixel;
begin
  with ASeries do
    Result := ColorToBGRA(ColorToRGB(
      ColorDef(Source[APointIndex]^.Color, Styles.Styles[AStackIndex].Brush.Color)), 255 - Transparency);
end;

procedure MyDrawPhong3DBar(
  ASeries: TBarSeries; ACanvas: TCanvas; ARect: TRect; APointIndex,
  AStackIndex: Integer);

  procedure DrawContour(var ABar: TBGRABitmap; var ADrawnRect: TRect);
  var
    size: TPoint;
    temp: TBGRABitmap;
    marginValue, depth: integer;
    margin: TPoint;
    SPen: TPen;
  begin
    margin := point(0, 0);
    SPen := ASeries.Styles.Styles[AStackIndex].Pen;
    if SPen.Style = psClear then exit;
    size := ARect.BottomRight - ARect.TopLeft;
    if SPen.Width > 1 then begin
      marginValue := (SPen.Width + 1) div 2;
      margin := Point(marginValue, marginValue);
      temp := TBGRABitmap.Create(
        ABar.Width + 2 * margin.X, ABar.Height + 2 * margin.Y);
      temp.PutImage(margin.X, margin.Y, ABar, dmSet);
      BGRAReplace(ABar, temp);
      ADrawnRect.TopLeft -= margin;
      ADrawnRect.BottomRight += margin;
    end;
    depth := ASeries.Depth;
    with ABar.CanvasBGRA do begin
      Pen.Assign(SPen);
      Brush.Style := bsClear;
      Rectangle(margin.x, margin.y + depth, margin.x + size.x - depth,
        margin.y + size.y - 0);
      if depth > 0 then
      begin
        MoveTo(margin.x, margin.y + depth);
        LineTo(margin.x + depth, margin.y);
        LineTo(margin.x + size.x - 1, margin.y);
        LineTo(margin.x + size.x - 1, margin.y + size.y - 1 - depth);
        LineTo(margin.x + size.x - 2 - depth, margin.y + size.y - 1);
        MoveTo(margin.x + size.x - 1, margin.y);
        LineTo(margin.x + size.x - 2 - depth, margin.y + depth + 1);
      end;
    end;
  end;

var
  bar: TBGRABitmap;
begin
  bar := CreatePhong3DBar(
    MyBarColor(ASeries, APointIndex, AStackIndex),
    Point(ASeries.ParentChart.ClientWidth div 2, 0), ARect, ASeries.Depth);
  try
    DrawContour(bar, ARect);
    with ARect.TopLeft do
      bar.Draw(ACanvas, X, Y, false);
  finally
    bar.Free;
  end;
end;

procedure MyDrawChocolateBar(
  ASeries: TBarSeries; ACanvas: TCanvas; ARect: TRect;
  APointIndex, AStackIndex: Integer; ARounded: boolean);
var
  bar: TBGRABitmap;
  border: Integer;
begin
  // Таким образом мы отличаем вертикальную гистограмму от горизонтальной.
  if ASeries.AxisIndexX = -1 then
    border := (ARect.Right - ARect.Left) div 8
  else
    border := (ARect.Bottom - ARect.Top) div 8;

  ARect.Top += -border div 2 + 1;
  ARect.Bottom += border div 2 + 1;
  bar := CreateChocolateBar(
    MyBarColor(ASeries, APointIndex, AStackIndex),
    Point(ASeries.ParentChart.ClientWidth div 2, 0),
    ARect, border, ARounded, []);
  try
    with ARect.TopLeft do
      bar.Draw(ACanvas, X, Y, false);
  finally
    bar.Free;
  end;
end;

procedure TdxBarSeries.BeforeDrawBar(ASender: TBarSeries; ACanvas: TCanvas;
  const ARect: TRect; APointIndex, AStackIndex: Integer;
  var ADoDefaultDrawing: Boolean);
begin
  ADoDefaultDrawing:=False;
  case FEffects of
    befPhong: MyDrawPhong3DBar(ASender, ACanvas, ARect, APointIndex, AStackIndex);
    befChocolate: MyDrawChocolateBar(ASender, ACanvas, ARect, APointIndex, AStackIndex, False);
    else
      ADoDefaultDrawing:=True;
  end;
end;

constructor TdxBarSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Marks.LabelFont.Name := 'Verdana';
  Marks.LabelFont.Size := 10;
  Marks.LabelFont.Color := clBlack;
  Marks.LinkPen.Color := clBlack;
  OnBeforeDrawBar:=@BeforeDrawBar;
end;

procedure TdxBarSeries.Assign(ASource: TPersistent);
begin
  if ASource is TdxBarSeries then
    with TdxBarSeries(ASource) do
      Self.Effects := Effects;
  inherited Assign(ASource);
  // И с какого хуя копируются обработчики событий??? Не пойму...
  OnBeforeDrawBar := @BeforeDrawBar;
end;

{ TdxChartSource }

procedure TdxChartSource.SetYFields(AValue: TdxFieldYCollection);
begin
  FYFields.Assign(AValue);
end;

constructor TdxChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FYFields := TdxFieldYCollection.Create(TdxFieldYItem);
end;

destructor TdxChartSource.Destroy;
begin
  FYFields.Free;
  inherited Destroy;
end;

{ TdxFieldYCollection }

function TdxFieldYCollection.GetFields(Index: Integer): TdxFieldYItem;
begin
  Result := TdxFieldYItem(Items[Index]);
end;

{ TdxChart }

function TdxChart.GetChartSeries: TChartSeries;
begin
  if Series.Count > 0 then Result := TChartSeries(Series[0])
  else Result := nil;
end;

function TdxChart.GetChartLabelSource: TdxChartSource;
begin
  if (ChartSeries is TdxBarSeries) and (TdxBarSeries(ChartSeries).AxisIndexX >= 0) then
    Result := TdxChartSource(GetObjectProp(LeftAxis.Marks, 'Source'))
  else
    Result := nil;
end;

function TdxChart.GetChartSource: TdxChartSource;
begin
  Result := TdxChartSource(GetObjectProp(ChartSeries, 'Source'));
end;

function TdxChart.GetChartStyles: TdxChartStyles;
begin
  if not (ChartSeries is TdxPieSeries) then
    Result := TdxChartStyles(GetObjectProp(ChartSeries, 'Styles'))
  else
    Result := nil;
end;

function TdxChart.GetLabelField: String;
begin
  Result := FSource.LabelField;
end;

function TdxChart.GetQuery: Integer;
begin
  Result := FSource.Query;
end;

function TdxChart.GetStyles: TChartStyleList;
begin
  Result := FStyles.Styles;
end;

function TdxChart.GetYCount: Integer;
begin
  Result := FSource.YCount;
end;

function TdxChart.GetYFields: TdxFieldYCollection;
begin
  Result := FSource.YFields;
end;

procedure TdxChart.SetLabelField(AValue: String);
begin
  FSource.LabelField := AValue;
end;

procedure TdxChart.SetQuery(AValue: Integer);
begin
  FSource.Query := AValue;
end;

procedure TdxChart.SetStyles(AValue: TChartStyleList);
begin
  FStyles.Styles.Assign(AValue);
  Broadcaster.Broadcast(Self);
end;

procedure TdxChart.SetYCount(AValue: Integer);
begin
  FSource.YCount := AValue;
end;

procedure TdxChart.SetYFields(AValue: TdxFieldYCollection);
begin
  FSource.YFields := AValue;
end;

procedure TdxChart.Loaded;
begin
  inherited Loaded;
  if ChartSeries = nil then Exit;
  Init;
  FillDemoSource;
end;

procedure TdxChart.GetChildren(AProc: TGetChildProc; ARoot: TComponent);
begin
  //inherited GetChildren(AProc, ARoot);
  if ChartSeries <> nil then
    AProc(ChartSeries);
end;

procedure TdxChart.FillDemoSource;
const
  Mon: array [0..11] of String = ('Январь', 'Февраль', 'Март', 'Апрель',
    'Май', 'Июнь', 'Июль', 'Август', 'Сентябрь', 'Октябрь', 'Ноябрь', 'Декабрь');
var
  i, j: Integer;
  Vals: array of Double;
begin
  Source.BeginUpdate;
  Source.Clear;
  Source.YCount := Source.YFields.Count;
  if LabelSource <> nil then LabelSource.Clear;
  SetLength(Vals, Source.YCount);
  for i := 0 to 11 do
  begin
    for j := 0 to Source.YFields.Count - 1 do
    begin
      if Source.YFields[j].FieldName <> '' then
        Vals[j] := Random(10000)
      else
        Vals[j] := 0;
    end;
    Source.AddXYList(i, Vals, Mon[i]);
    if LabelSource <> nil then
      LabelSource.Add(0, i, Mon[i]);
  end;
  Source.EndUpdate;
end;

procedure TdxChart.Build;
var
  Fm: TdxForm;
  QGrid: TdxQueryGrid;
  DS: TDataSet;
  B: TBookMark;
  Vals: array of Double;
  S: String;
  F, LblF: TField;
  YList: TList;
  i, n: Integer;
begin
  Fm := TdxForm(Owner);
  QGrid := FindQueryGrid(Fm, Query);
  if QGrid = nil then Exit;
  QGrid.RequeryIfNeed;
  FNeedBuild := False;

  FSource.BeginUpdate;
  FLabelSource.BeginUpdate;
  FSource.Clear;
  FLabelSource.Clear;

  YList := TList.Create;

  SetLength(Vals, YCount);
  DS := QGrid.DataSource.DataSet;
  B := DS.GetBookmark;
  QGrid.DisableScrollEvents;
  DS.DisableControls;
  DS.First;

  LblF:= DS.FindField(LabelField);
  S := '';
  for i := 0 to YFields.Count - 1 do
    YList.Add(DS.FindField(YFields[i].FieldName));

  n := 1;
  while not DS.Eof do
  begin
    F := DS.FindField(LabelField);
    if LblF <> nil then
      S := LblF.AsString;

    for i := 0 to YList.Count - 1 do
    begin
      F := TField(YList[i]);
      if F <> nil then
        Vals[i] := F.AsFloat
      else
        Vals[i] := 0;
    end;
    FSource.AddXYList(n, Vals, S);
    FLabelSource.Add(0, n, S);
    Inc(n);
    DS.Next;
  end;
  DS.GotoBookmark(B);
  DS.FreeBookmark(B);
  DS.EnableControls;
  QGrid.EnableScrollEvents;

  FSource.EndUpdate;
  FLabelSource.EndUpdate;

  YList.Free;
end;

procedure TdxChart.Init;
begin
  SetObjectProp(ChartSeries, 'Source', FSource);
  if (ChartSeries is TdxBarSeries) and (TdxBarSeries(ChartSeries).AxisIndexX >= 0) then
  begin
    LeftAxis.Marks.Source := FLabelSource;
    LeftAxis.Marks.Style := smsLabel;
  end
  else
  begin
    BottomAxis.Marks.Source := FSource;
    BottomAxis.Marks.Style := smsLabel;
  end;
  LeftAxis.Marks.LabelBrush.Style := bsSolid;
  LeftAxis.Title.LabelBrush.Style := bsSolid;
  BottomAxis.Marks.LabelBrush.Style := bsSolid;
  BottomAxis.Title.LabelBrush.Style := bsSolid;
  if not (ChartSeries is TdxPieSeries) then
    SetObjectProp(ChartSeries, 'Styles', FStyles);
end;

procedure TdxChart.SaveImageToFile(const FileName: String);
var
  Bmp: TBGRABitmap;
  W, H: Integer;
begin
  if FNeedBuild then Build;
  if FSaveOriginalSize then
  begin
    W := Width;
    H := Height;
  end
  else
  begin
    W := FSaveImageWidth;
    H := FSaveImageHeight;
  end;
  Bmp := TBGRABitmap.Create(W, H);
  try
    Draw(TdxDrawer.Create(Bmp), Rect(0, 0, Bmp.Width, Bmp.Height));
    Bmp.SaveToFile(FileName);
  finally
    //Bmp.Free;
  end;
end;

constructor TdxChart.Create(AOwner: TComponent);
begin
  OnInitBuiltinTools:=nil;
  inherited Create(AOwner);
  Font.Name := 'Verdana';
  Font.Size := 10;
  Color := clWhite;
  BackColor := clWhite;
  AllowZoom := False;
  Title.Brush.Color:=clWhite;
  Title.Frame.Style := psSolid;
  Title.Frame.Visible := False;
  Title.Text.Text := rsChart;
  Title.Font.Name := 'Verdana';
  Title.Font.Size := 10;
  Title.Font.Color := clBlack;
  Foot.Brush.Color:=clWhite;
  Foot.Frame.Style := psSolid;
  Foot.Frame.Visible := False;
  Foot.Text.Text := rsFoot;
  Foot.Font.Name := 'Verdana';
  Foot.Font.Size := 10;
  Foot.Font.Color := clBlack;

  FStyles := TdxChartStyles.Create(Self);
  FSource := TdxChartSource.Create(Self);
  FLabelSource := TdxChartSource.Create(Self);

  LeftAxis.AxisPen.Width := 2;
  LeftAxis.Title.Frame.Style := psSolid;
  LeftAxis.Title.Frame.Visible := False;
  LeftAxis.Title.Caption := rsValues;
  LeftAxis.Title.LabelFont.Name := 'Verdana';
  LeftAxis.Title.LabelFont.Size := 10;
  LeftAxis.Title.LabelFont.Color := clBlack;
  LeftAxis.Marks.LabelFont.Name := 'Verdana';
  LeftAxis.Marks.LabelFont.Size := 10;
  LeftAxis.Marks.LabelFont.Color := clBlack;
  LeftAxis.Marks.LabelBrush.Style := bsSolid;
  LeftAxis.Marks.Frame.Visible := False;
  LeftAxis.Marks.Frame.Style := psSolid;

  BottomAxis.AxisPen.Width := 2;
  BottomAxis.Title.Frame.Style := psSolid;
  BottomAxis.Title.Frame.Visible := False;
  BottomAxis.Title.Caption := rsCategories;
  BottomAxis.Title.LabelFont.Name := 'Verdana';
  BottomAxis.Title.LabelFont.Size := 10;
  BottomAxis.Title.LabelFont.Color := clBlack;
  BottomAxis.Marks.LabelFont.Name := 'Verdana';
  BottomAxis.Marks.LabelFont.Size := 10;
  BottomAxis.Marks.LabelFont.Color := clBlack;
  BottomAxis.Marks.LabelBrush.Style := bsSolid;
  BottomAxis.Marks.Frame.Visible := False;
  BottomAxis.Marks.Frame.Style := psSolid;

  Legend.Font.Name := 'Verdana';
  Legend.Font.Size := 10;
  Legend.Font.Color := clBlack;

  GUIConnector := TdxGUIConnector.Create(Self);

  FSaveImageWidth := 640;
  FSaveImageHeight := 480;
  FSaveOriginalSize := True;
end;

initialization
  RegisterClass(TdxChart);
  RegisterClass(TdxBarSeries);
  RegisterClass(TdxPieSeries);
  RegisterClass(TdxLineSeries);
  RegisterClass(TdxAreaSeries);
  RegisterClass(TdxChartSource);
  RegisterClass(TdxChartStyles);

end.

