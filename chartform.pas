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

unit ChartForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, ButtonPanel, Menus, Buttons, Laz.VirtualTrees,
  propgrids, TAGraph, TASeries, TAStyles, dxcharts, TATextElements,
  TACustomSeries, TALegend, TATransformations, TAChartAxis, TACustomSource,
  TAChartUtils, TAChartAxisUtils, TARadialSeries, TATypes, TAMultiSeries,
  dxctrls, dxreports, reportmanager, mytypes, typinfo, strconsts;

type
  { TChartFm }

  TChartFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ChartKind: TComboBox;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    N1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    procedure ChartKindChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure AddProps;
    {procedure InitDemoSource;
    procedure FillDemoSource;}
    function PickColor: TColor;
    procedure ResetAxis;
    procedure AddMinor;
    procedure MinorToTick;
    procedure TickToMinor;
    function CreateBarChart: TdxBarSeries;
    function CreatePieChart: TdxPieSeries;
    function CreateLineChart: TdxLineSeries;
    function CreateAreaChart: TdxAreaSeries;
    procedure InitBarChart;
    procedure InitHorizontalBarChart;
    procedure InitPieChart;
    procedure InitLineChart;
    procedure InitAreaChart;
    procedure SetChartSeries(Series: TChartSeries);
  private
    FGrid: TVirtualPropGrid;
    FChart: TdxChart;
    FCmp: TdxChart;
    procedure GridNeedValues(Sender: TObject; const PropName: String; Values: TStrings);
    procedure GridPropChange(Sender: TObject; const PropName: String);
    procedure CopyChart(S, D: TdxChart);
    procedure FillQueries(Items: TStrings);
    procedure FillLabelFields(Items: TStrings);
    procedure FillYFields(Items: TStrings);
  public
    function ShowForm(C: TdxChart): Integer;
  end;

var
  ChartFm: TChartFm;

function ShowChartForm(C: TdxChart): Integer;

implementation

uses
  LazUtf8, helpmanager, apputils;

type
  THackBasicPointSeries = class(TBasicPointSeries)
  public
    property MarkPositions;
  end;

function ShowChartForm(C: TdxChart): Integer;
begin
  if ChartFm = nil then
    ChartFm := TChartFm.Create(Application);
  Result := ChartFm.ShowForm(C);
end;

{$R *.lfm}

{ TChartFm }

function CustomSortStrings(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := MyUtf8CompareText(List.ValueFromIndex[Index1], List.ValueFromIndex[Index2]);
end;

procedure TChartFm.FormCreate(Sender: TObject);
begin
  FChart := TdxChart.Create(Self);
  FChart.Parent := Self;
  FChart.Align := alClient;

  FGrid := TVirtualPropGrid.Create(Self);
  FGrid.Parent := Panel1;
  FGrid.Width := 400;
  FGrid.Align := alClient;
  FGrid.OnNeedValues:=@GridNeedValues;
  FGrid.OnPropChange:=@GridPropChange;
  FGrid.PopupMenu := PopupMenu1;
  FGrid.TIObject := FChart;

  Label1.Caption := rsChartType;
  ChartKind.Items.AddStrings([rsVerticalBars, rsHorizontalBars, rsPie,
    rsGraph, rsArea]);
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  ButtonPanel1.OKButton.Default := False;
end;

procedure TChartFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('chart');
end;

procedure TChartFm.ChartKindChange(Sender: TObject);
begin
  case ChartKind.ItemIndex of
    0: InitBarChart;
    1: InitHorizontalBarChart;
    2: InitPieChart;
    3: InitLineChart;
    4: InitAreaChart;
  end;
end;

procedure TChartFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FGrid.EndEditNode;
  if ModalResult <> mrOk then
  begin
    if FGrid.Modified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TChartFm.MenuItem1Click(Sender: TObject);
begin
  FChart.Source.YFields.Add;
  with TChartStyle(FChart.Styles.Add) do
  begin
    UseFont := False;
    Brush.Color := PickColor;
  end;
  AddProps;
  FChart.FillDemoSource;
end;

procedure TChartFm.MenuItem2Click(Sender: TObject);
var
  N: PVirtualNode;
  i: Cardinal;
begin
  N := FGrid.GetFirstSelected;
  if N = nil then Exit;
  i := N^.Index - 2;
  FChart.Source.YFields.Delete(i);
  FChart.Styles.Delete(i);
  AddProps;
  FChart.FillDemoSource;
end;

procedure TChartFm.MenuItem3Click(Sender: TObject);
var
  N: PVirtualNode;
  i: Cardinal;
begin
  N := FGrid.GetFirstSelected;
  if N = nil then Exit;
  i := N^.Index - 2;
  FChart.Source.YFields.Exchange(i, i-1);
  FChart.Styles.Exchange(i, i-1);
  AddProps;
  FChart.FillDemoSource;
  // Перемещаем выделение на предыдущий узел
  N := FGrid.GetFirstSelected;
  if N <> nil then
  begin
    FGrid.Selected[N] := False;
    N := N^.PrevSibling;
    if N <> nil then FGrid.Selected[N] := True;
  end;
end;

procedure TChartFm.MenuItem4Click(Sender: TObject);
var
  N: PVirtualNode;
  i: Cardinal;
begin
  N := FGrid.GetFirstSelected;
  if N = nil then Exit;
  i := N^.Index - 2;
  FChart.Source.YFields.Exchange(i, i+1);
  FChart.Styles.Exchange(i, i+1);
  AddProps;
  FChart.FillDemoSource;
  // Перемещаем выделение на следующий узел
  N := FGrid.GetFirstSelected;
  if N <> nil then
  begin
    FGrid.Selected[N] := False;
    N := N^.NextSibling;
    if N <> nil then FGrid.Selected[N] := True;
  end;
end;

procedure TChartFm.PopupMenu1Popup(Sender: TObject);
var
  N: PVirtualNode;
  Data: PVPGData;
  IsPie: Boolean;
begin
  FGrid.EndEditNode;
  N := FGrid.GetFirstSelected;
  //if N = nil then Exit;
  IsPie := FChart.ChartSeries is TdxPieSeries;
  Data := FGrid.GetNodeData(N);
  MenuItem1.Enabled := (N <> nil) and not IsPie;
  MenuItem2.Enabled := (N <> nil) and (FChart.Source.YFields.Count > 1) and
    (Pos('ChartSeries.Source.YFields[', Data^.PropName) > 0) and not IsPie;
  MenuItem3.Enabled := (N <> nil) and MenuItem2.Enabled and (N^.Index - 2 > 0) and not IsPie;
  MenuItem4.Enabled := (N <> nil) and MenuItem2.Enabled and
    (N^.Index - 2 < FChart.Styles.Count - 1) and not IsPie;
end;

procedure TChartFm.AddProps;

  procedure AddFont(const S: String; PN: PVirtualNode);
  var
    N, N2: PVirtualNode;
  begin
    with FGrid do
    begin
      N := AddProp(rsFont, Copy(S, 1, Length(S) - 1), PN);
      AddPropList(rsName, S + 'Name', N, nil);
      AddProp(rsColor, S + 'Color', N);
      AddProp(rsSize, S + 'Size', N);
      AddProp(rsOrientation, S + 'Orientation', N);
      N2 := AddProp(rsStyle, S + 'Style', N);
      AddOption(rsBold, 'fsBold', N2);
      AddOption(rsItalic, 'fsItalic', N2);
      AddOption(rsUnderline, 'fsUnderline', N2);
     // AddOption('Зачеркнутый', 'fsStrikeOut', N2);
    end;
  end;

var
  N, N2, N3: PVirtualNode;
  i: Integer;
  S: String;
  IsBar, IsPie, IsLine, IsArea: Boolean;
begin
  with FGrid do
  begin
    IsBar := FChart.ChartSeries is TBarSeries;
    IsPie := FChart.ChartSeries is TPieSeries;
    IsLine := FChart.ChartSeries is TLineSeries;
    IsArea := FChart.ChartSeries is TAreaSeries;

    SaveExpandState;
    BeginUpdate;
    Clear;

    N := AddGroup(rsDataSource, nil);
    N2 := AddPropList(rsQuery, 'ChartSeries.Source.Query', N, nil);
    AddPropList(rsMarks, 'ChartSeries.Source.LabelField', N, N2);
    if IsPie then
    begin
      AddPropList(rsValues, 'ChartSeries.Source.YFields[0].FieldName', N, N2);
    end
    else
      for i := 0 to FChart.Source.YFields.Count - 1 do
        AddPropList(Format(rsSeriesNum, [i+1]), Format('ChartSeries.Source.'
          +'YFields[%d].FieldName', [i]), N, N2);
    ToggleNode(N);

    if not IsPie then
    begin
      N := AddGroup(rsSeries, nil);
      AddPropList(rsGroupingType, 'ChartSeries.Stacked', N, nil);
      for i := 0 to FChart.Source.YFields.Count - 1 do
      begin
        S := Format('ChartSeries.Styles.Styles[%d].', [i]);
        N2 := AddGroup(Format(rsSeriesNum, [i+1]), N);
        if IsBar or IsArea then
          N3 := AddProp(rsFilling, S + 'Brush.Color', N2)
        else if IsLine then
          N3 := AddProp(rsPoint, S + 'Brush.Color', N2);
        AddPropList(rsStyle, S + 'Brush.Style', N3, nil);
        if IsBar or IsArea then
          N3 := AddProp(rsFrame, S + 'Pen.Color', N2)
        else if IsLine then
          N3 := AddProp(rsLine, S + 'Pen.Color', N2);
        AddPropList(rsStyle, S + 'Pen.Style', N3, nil);
        AddProp(rsWidth, S + 'Pen.Width', N3);
        AddProp(rsMark, S + 'Text', N2);
      end;
    end;

    N := AddGroup(rsChart, nil);
    AddProp(rsDepth, 'ChartSeries.Depth', N);
    if IsBar then
    begin
      AddPropList(rsEffects, 'ChartSeries.Effects', N, nil);
      AddProp(rsDensityPerc, 'ChartSeries.BarWidthPercent', N);
    end
    else if IsLine then
    begin
      AddProp(rsLines, 'ChartSeries.ShowLines', N);
      N2 := AddProp(rsPoints, 'ChartSeries.ShowPoints', N);
      AddProp(rsWidth, 'ChartSeries.Pointer.HorizSize', N2);
      AddProp(rsHeight, 'ChartSeries.Pointer.VertSize', N2);
      N3 := AddProp(rsFrame, 'ChartSeries.Pointer.Pen.Color', N2);
      AddPropList(rsStyle, 'ChartSeries.Pointer.Pen.Style', N3, nil);
      AddProp(rsWidth, 'ChartSeries.Pointer.Pen.Width', N3);
      AddPropList(rsShape, 'ChartSeries.Pointer.Style', N2, nil);
    end
    else if IsArea then
    begin
      N2 := AddGroup(rsAreaLines, N);
      AddProp(rsColor, 'ChartSeries.AreaLinesPen.Color', N2);
      AddPropList(rsStyle, 'ChartSeries.AreaLinesPen.Style', N2, nil);
      AddProp(rsWidth, 'ChartSeries.AreaLinesPen.Width', N2);
    end
    else if IsPie then
    begin
      N2 := AddProp(rsEdges, 'ChartSeries.EdgePen.Color', N);
      AddPropList(rsStyle, 'ChartSeries.EdgePen.Style', N2, nil);
      AddProp(rsWidth, 'ChartSeries.EdgePen.Width', N2);
    end;
    N2 := AddProp(rsShadow, 'ChartSeries.Shadow.Visible', N);
    AddProp(rsColor, 'ChartSeries.Shadow.Color', N2);
    AddProp(rsTransparency, 'ChartSeries.Shadow.Transparency', N2);
    AddProp(rsOffsetX, 'ChartSeries.Shadow.OffsetX', N2);
    AddProp(rsOffsetY, 'ChartSeries.Shadow.OffsetY', N2);

    N2 := AddProp(rsMarksOfData, 'ChartSeries.Marks.Visible', N);
    AddPropList(rsContent, 'ChartSeries.Marks.Style', N2, nil);
    AddPropList(rsPosition, 'ChartSeries.MarkPositions', N2, nil);

    if IsBar or IsArea then
      AddProp(rsByCenter, 'ChartSeries.MarkPositionCentered', N2)
    else if IsPie then
      AddProp(rsRotateLabels, 'ChartSeries.RotateLabels', N2);

    AddFont('ChartSeries.Marks.LabelFont.', N2);
    N3 := AddProp(rsBackground, 'ChartSeries.Marks.LabelBrush.Color', N2);
    N3 := AddProp(rsFrame, 'ChartSeries.Marks.Frame.Visible', N2);
    AddProp(rsColor, 'ChartSeries.Marks.Frame.Color', N3);
    AddProp(rsWidth, 'ChartSeries.Marks.Frame.Width', N3);
    AddPropList(rsShape, 'ChartSeries.Marks.Shape', N2, nil);
    N3 := AddGroup(rsMargins, N2);
    AddProp(rsMarginLeft, 'ChartSeries.Marks.Margins.Left', N3);
    AddProp(rsMarginTop, 'ChartSeries.Marks.Margins.Top', N3);
    AddProp(rsMarginRight, 'ChartSeries.Marks.Margins.Right', N3);
    AddProp(rsMarginBottom, 'ChartSeries.Marks.Margins.Bottom', N3);
    AddProp(rsDistance, 'ChartSeries.Marks.Distance', N2);
    AddProp(rsCalloutAngle, 'ChartSeries.Marks.CalloutAngle', N2);
    N3 := AddProp(rsLinkLines, 'ChartSeries.Marks.LinkPen.Visible', N2);
    AddProp(rsColor, 'ChartSeries.Marks.LinkPen.Color', N3);
    AddPropList(rsStyle, 'ChartSeries.Marks.LinkPen.Style', N3, nil);
    AddProp(rsWidth, 'ChartSeries.Marks.LinkPen.Width', N3);
    AddProp(rsDistance, 'ChartSeries.Marks.LinkDistance', N3);
    AddPropList(rsOverlapPolicy, 'ChartSeries.Marks.OverlapPolicy', N2, nil);

    N := AddGroup(rsGeneralView, nil);

    AddProp(rsBackground, 'Color', N);
    AddProp(rsBuildArea, 'BackColor', N);
    N2 := AddProp(rsFrame, 'Frame.Visible', N);
    //AddProp('Показать', 'Frame.Visible', N2);
    AddProp(rsColor, 'Frame.Color', N2);
    AddPropList(rsStyle, 'Frame.Style', N2, nil);
    AddProp(rsWidth, 'Frame.Width', N2);
    if not IsPie then
    begin
      N2 := AddGroup(rsMargins, N);
      AddProp(rsMarginLeft, 'Margins.Left', N2);
      AddProp(rsMarginTop, 'Margins.Top', N2);
      AddProp(rsMarginRight, 'Margins.Right', N2);
      AddProp(rsMarginBottom, 'Margins.Bottom', N2);
    end;
    N2 := AddGroup(rsMarginsExternal, N);
    AddProp(rsMarginLeft, 'MarginsExternal.Left', N2);
    AddProp(rsMarginTop, 'MarginsExternal.Top', N2);
    AddProp(rsMarginRight, 'MarginsExternal.Right', N2);
    AddProp(rsMarginBottom, 'MarginsExternal.Bottom', N2);

    if not IsPie then
    begin
      N := AddProp(rsCategoryAxis, 'AxisList[1].Visible', nil);
      N2 := AddProp(rsMarks, 'AxisList[1].Marks.Visible', N);
      AddFont('AxisList[1].Marks.LabelFont.', N2);
      N3 := AddProp(rsBackground, 'AxisList[1].Marks.LabelBrush.Color', N2);
      //AddPropList('Стиль', 'AxisList[1].Marks.LabelBrush.Style', N3, nil);
      N3 := AddProp(rsFrame, 'AxisList[1].Marks.Frame.Visible', N2);
      AddProp(rsColor, 'AxisList[1].Marks.Frame.Color', N3);
      //AddPropList('Стиль', 'AxisList[1].Marks.Frame.Style', N3, nil);
      AddProp(rsWidth, 'AxisList[1].Marks.Frame.Width', N3);
      AddPropList(rsShape, 'AxisList[1].Marks.Shape', N2, nil);
      N3 := AddGroup(rsMargins, N2);
      AddProp(rsMarginLeft, 'AxisList[1].Marks.Margins.Left', N3);
      AddProp(rsMarginTop, 'AxisList[1].Marks.Margins.Top', N3);
      AddProp(rsMarginRight, 'AxisList[1].Marks.Margins.Right', N3);
      AddProp(rsMarginBottom, 'AxisList[1].Marks.Margins.Bottom', N3);
      AddProp(rsDistanceToAxis, 'AxisList[1].Marks.Distance', N2);
      AddPropList(rsOverlapPolicy, 'AxisList[1].Marks.OverlapPolicy', N2, nil);
      N2 := AddProp(rsAxisLine, 'AxisList[1].AxisPen.Visible', N);
      AddProp('Цвет', 'AxisList[1].AxisPen.Color', N2);
      //AddPropList('Стиль', 'AxisList[1].AxisPen.Style', N2, nil);
      AddProp(rsWidth, 'AxisList[1].AxisPen.Width', N2);
      if IsBar then
      begin
        N2 := AddProp(rsGrid, 'AxisList[1].Minors[0].Grid.Visible', N);
        AddProp(rsColor, 'AxisList[1].Minors[0].Grid.Color', N2);
        AddPropList(rsStyle, 'AxisList[1].Minors[0].Grid.Style', N2, nil);
        AddProp(rsWidth, 'AxisList[1].Minors[0].Grid.Width', N2);
        N2 := AddGroup(rsTicks, N);
        AddProp(rsColor, 'AxisList[1].Minors[0].TickColor', N2);
        AddProp(rsLength, 'AxisList[1].Minors[0].TickLength', N2);
        AddProp(rsInnerLength, 'AxisList[1].Minors[0].TickInnerLength', N2);
      end
      else
      begin
        N2 := AddProp(rsGrid, 'AxisList[1].Grid.Visible', N);
        AddProp(rsColor, 'AxisList[1].Grid.Color', N2);
        AddPropList(rsStyle, 'AxisList[1].Grid.Style', N2, nil);
        AddProp(rsWidth, 'AxisList[1].Grid.Width', N2);
        N2 := AddGroup(rsTicks, N);
        AddProp(rsColor, 'AxisList[1].TickColor', N2);
        AddProp(rsLength, 'AxisList[1].TickLength', N2);
        AddProp(rsInnerLength, 'AxisList[1].TickInnerLength', N2);
      end;
      N2 := AddProp(rsTitle, 'AxisList[1].Title.Visible', N);
      AddProp(rsText, 'AxisList[1].Title.Caption', N2);
      AddFont('AxisList[1].Title.LabelFont.', N2);
      N3 := AddProp(rsBackground, 'AxisList[1].Title.LabelBrush.Color', N2);
      //AddPropList('Стиль', 'AxisList[1].Title.LabelBrush.Style', N3, nil);
      N3 := AddProp(rsFrame, 'AxisList[1].Title.Frame.Visible', N2);
      AddProp(rsColor, 'AxisList[1].Title.Frame.Color', N3);
      //AddPropList('Стиль', 'AxisList[1].Title.Frame.Style', N3, nil);
      AddProp(rsWidth, 'AxisList[1].Title.Frame.Width', N3);
      AddPropList(rsShape, 'AxisList[1].Title.Shape', N2, nil);
      N3 := AddGroup(rsMargins, N2);
      AddProp(rsMarginLeft, 'AxisList[1].Title.Margins.Left', N3);
      AddProp(rsMarginTop, 'AxisList[1].Title.Margins.Top', N3);
      AddProp(rsMarginRight, 'AxisList[1].Title.Margins.Right', N3);
      AddProp(rsMarginBottom, 'AxisList[1].Title.Margins.Bottom', N3);
      AddProp(rsDistanceToAxis, 'AxisList[1].Title.Distance', N2);

      N := AddProp(rsValueAxis, 'AxisList[0].Visible', nil);
      N2 := AddProp(rsMarks, 'AxisList[0].Marks.Visible', N);
      AddFont('AxisList[0].Marks.LabelFont.', N2);
      N3 := AddProp(rsBackground, 'AxisList[0].Marks.LabelBrush.Color', N2);
      //AddPropList('Стиль', 'AxisList[0].Marks.LabelBrush.Style', N3, nil);
      N3 := AddProp(rsFrame, 'AxisList[0].Marks.Frame.Visible', N2);
      AddProp(rsColor, 'AxisList[0].Marks.Frame.Color', N3);
      //AddPropList('Стиль', 'AxisList[0].Marks.Frame.Style', N3, nil);
      AddProp(rsWidth, 'AxisList[0].Marks.Frame.Width', N3);
      AddPropList(rsShape, 'AxisList[0].Marks.Shape', N2, nil);
      N3 := AddGroup(rsMargins, N2);
      AddProp(rsMarginLeft, 'AxisList[0].Marks.Margins.Left', N3);
      AddProp(rsMarginTop, 'AxisList[0].Marks.Margins.Top', N3);
      AddProp(rsMarginRight, 'AxisList[0].Marks.Margins.Right', N3);
      AddProp(rsMarginBottom, 'AxisList[0].Marks.Margins.Bottom', N3);
      AddProp(rsDistanceToAxis, 'AxisList[0].Marks.Distance', N2);
      N2 := AddProp(rsAxisLine, 'AxisList[0].AxisPen.Visible', N);
      AddProp(rsColor, 'AxisList[0].AxisPen.Color', N2);
      //AddPropList('Стиль', 'AxisList[0].AxisPen.Style', N2, nil);
      AddProp(rsWidth, 'AxisList[0].AxisPen.Width', N2);
      N2 := AddProp(rsGrid, 'AxisList[0].Grid.Visible', N);
      AddProp(rsColor, 'AxisList[0].Grid.Color', N2);
      AddPropList(rsStyle, 'AxisList[0].Grid.Style', N2, nil);
      AddProp(rsWidth, 'AxisList[0].Grid.Width', N2);
      N2 := AddGroup(rsTicks, N);
      AddProp(rsColor, 'AxisList[0].TickColor', N2);
      AddProp(rsLength, 'AxisList[0].TickLength', N2);
      AddProp(rsInnerLength, 'AxisList[0].TickInnerLength', N2);
      N2 := AddGroup(rsIntervals, N);
      AddProp(rsMaxLength, 'AxisList[0].Intervals.MaxLength', N2);
      AddProp(rsMinLength, 'AxisList[0].Intervals.MinLength', N2);
      N2 := AddProp(rsTitle, 'AxisList[0].Title.Visible', N);
      AddProp(rsText, 'AxisList[0].Title.Caption', N2);
      AddFont('AxisList[0].Title.LabelFont.', N2);
      N3 := AddProp(rsBackground, 'AxisList[0].Title.LabelBrush.Color', N2);
      //AddPropList('Стиль', 'AxisList[0].Title.LabelBrush.Style', N3, nil);
      N3 := AddProp(rsFrame, 'AxisList[0].Title.Frame.Visible', N2);
      AddProp(rsColor, 'AxisList[0].Title.Frame.Color', N3);
      //AddPropList('Стиль', 'AxisList[0].Title.Frame.Style', N3, nil);
      AddProp(rsWidth, 'AxisList[0].Title.Frame.Width', N3);
      AddPropList(rsShape, 'AxisList[0].Title.Shape', N2, nil);
      N3 := AddGroup(rsMargins, N2);
      AddProp(rsMarginLeft, 'AxisList[0].Title.Margins.Left', N3);
      AddProp(rsMarginTop, 'AxisList[0].Title.Margins.Top', N3);
      AddProp(rsMarginRight, 'AxisList[0].Title.Margins.Right', N3);
      AddProp(rsMarginBottom, 'AxisList[0].Title.Margins.Bottom', N3);
      AddProp(rsDistanceToAxis, 'AxisList[0].Title.Distance', N2);
    end;

    N := AddProp(rsTitle, 'Title.Visible', nil);
    AddProp(rsText, 'Title.Text', N);
    AddFont('Title.Font.', N);
    N2 := AddProp(rsBackground, 'Title.Brush.Color', N);
    //AddPropList('Стиль', 'Title.Brush.Style', N2, nil);
    N2 := AddProp(rsFrame, 'Title.Frame.Visible', N);
    AddProp(rsColor, 'Title.Frame.Color', N2);
    //AddPropList('Стиль', 'Title.Frame.Style', N2, nil);
    AddProp(rsWidth, 'Title.Frame.Width', N2);
    AddPropList(rsShape, 'Title.Shape', N, nil);
    N2 := AddGroup(rsMargins, N);
    AddProp(rsMarginLeft, 'Title.Margins.Left', N2);
    AddProp(rsMarginTop, 'Title.Margins.Top', N2);
    AddProp(rsMarginRight, 'Title.Margins.Right', N2);
    AddProp(rsMarginBottom, 'Title.Margins.Bottom', N2);
    AddProp(rsMargin, 'Title.Margin', N);

    N := AddProp(rsFoot, 'Foot.Visible', nil);
    AddProp(rsText, 'Foot.Text', N);
    AddFont('Foot.Font.', N);
    N2 := AddProp(rsBackground, 'Foot.Brush.Color', N);
    //AddPropList('Стиль', 'Foot.Brush.Style', N2, nil);
    N2 := AddProp(rsFrame, 'Foot.Frame.Visible', N);
    AddProp(rsColor, 'Foot.Frame.Color', N2);
    //AddPropList('Стиль', 'Foot.Frame.Style', N2, nil);
    AddProp(rsText, 'Foot.Frame.Width', N2);
    AddPropList(rsShape, 'Foot.Shape', N, nil);
    N2 := AddGroup(rsMArgins, N);
    AddProp(rsMarginLeft, 'Foot.Margins.Left', N2);
    AddProp(rsMarginTop, 'Foot.Margins.Top', N2);
    AddProp(rsMarginRight, 'Foot.Margins.Right', N2);
    AddProp(rsMarginBottom, 'Foot.Margins.Bottom', N2);
    AddProp(rsMargin, 'Foot.Margin', N);

    N := AddProp(rsLegend, 'Legend.Visible', nil);
    AddPropList(rsPosition, 'Legend.Alignment', N, nil);
    AddPropList(rsDisplay, 'Legend.UseSidebar', N, nil);
    AddProp(rsColumns, 'Legend.ColumnCount', N);
    AddProp(rsTransparency, 'Legend.Transparency', N);
    N2 := AddProp(rsBackground, 'Legend.BackgroundBrush.Color', N);
    //AddPropList('Стиль', 'Legend.BackgroundBrush.Style', N2, nil);
    AddFont('Legend.Font.', N);
    N2 := AddProp(rsFrame, 'Legend.Frame.Visible', N);
    AddProp(rsColor, 'Legend.Frame.Color', N2);
    //AddPropList('Стиль', 'Legend.Frame.Style', N2, nil);
    AddProp(rsWidth, 'Legend.Frame.Width', N2);
    N2 := AddProp(rsHorizontalGrid, 'Legend.GridHorizontal.Visible', N);
    AddProp(rsColor, 'Legend.GridHorizontal.Color', N2);
    AddPropList(rsStyle, 'Legend.GridHorizontal.Style', N2, nil);
    AddProp(rsWidth, 'Legend.GridHorizontal.Width', N2);
    N2 := AddProp(rsVerticalGrid, 'Legend.GridVertical.Visible', N);
    AddProp(rsColor, 'Legend.GridVertical.Color', N2);
    AddPropList(rsStyle, 'Legend.GridVertical.Style', N2, nil);
    AddProp(rsWidth, 'Legend.GridVertical.Width', N2);
    N2 := AddProp(rsSymbolFrame, 'Legend.SymbolFrame.Visible', N);
    AddProp(rsColor, 'Legend.SymbolFrame.Color', N2);
    //AddPropList('Стиль', 'Legend.SymbolFrame.Style', N2, nil);
    AddProp(rsWidth, 'Legend.SymbolFrame.Width', N2);
    AddProp(rsSymbolWidth, 'Legend.SymbolWidth', N);
    AddProp(rsMarginsLeftRight, 'Legend.MarginX', N);
    AddProp(rsMarginsTopBottom, 'Legend.MarginY', N);
    AddProp(rsSpacing, 'Legend.Spacing', N);
    RestoreExpandState;
    EndUpdate;
  end;
end;

function TChartFm.PickColor: TColor;
const
  Colors: array [1..10] of TColor = (clRed, clGreen, clYellow, clBlue, clTeal,
    clLime,  clFuchsia, clAqua, clMoneyGreen, clSkyBlue);
var
  i, j: Integer;
  Used: Boolean;
begin
  Result := clSilver;
  for i := 1 to 10 do
  begin
    Used := False;
    for j := 0 to FChart.Styles.Count - 1 do
      if FChart.Styles[j].Brush.Color = Colors[i] then
      begin
        Used := True;
        Break;
      end;
    if not Used then Exit(Colors[i]);
  end;
end;

procedure TChartFm.ResetAxis;
begin
  with FChart do
    if AxisList[0].Alignment = calBottom then
    begin
      AxisList[0].Alignment := calLeft;
      AxisList[1].Alignment := calBottom;
      //BottomAxis.Marks.Source := FSource;
    end;
end;

procedure TChartFm.AddMinor;
var
  M: TChartMinorAxis;
begin
  FChart.BottomAxis.Minors.Clear;
  M := FChart.BottomAxis.Minors.Add;
  M.Intervals.Options := [aipUseCount];
  M.Intervals.Count := 2;
  M.Visible := True;
end;

procedure TChartFm.MinorToTick;
begin
  with FChart.BottomAxis do
    if Minors.Count > 0 then
    begin
      TickColor := Minors[0].TickColor;
      TickLength := Minors[0].TickLength;
      TickInnerLength := Minors[0].TickInnerLength;
      Grid.Assign(Minors[0].Grid);
      //Minors.Clear;
    end;
end;

procedure TChartFm.TickToMinor;
begin
  with FChart.BottomAxis do
    if Minors.Count > 0 then
    begin
      Minors[0].TickColor := TickColor;
      Minors[0].TickLength := TickLength;
      Minors[0].TickInnerLength := TickInnerLength;
      Minors[0].Grid.Assign(Grid);
      TickLength := 0;
      TickInnerLength := 0;
      Grid.Visible := False;
    end;
end;

function TChartFm.CreateBarChart: TdxBarSeries;
begin
  Result := TdxBarSeries.Create(FChart);
  with Result do
  begin
    Stacked:=False;
    Marks.Style:=smsValue;
    Marks.YIndex:=-1;
    Marks.Visible:=False;
    Legend.Multiplicity:=lmStyle;
  end;
end;

function TChartFm.CreatePieChart: TdxPieSeries;
begin
  Result := TdxPieSeries.Create(FChart);
  with Result do
  begin
    Marks.Style := smsValue;
    Marks.Visible:=False;
    Legend.Multiplicity:=lmPoint;
  end;
end;

function TChartFm.CreateLineChart: TdxLineSeries;
begin
  Result := TdxLineSeries.Create(FChart);
  with Result do
  begin
    Marks.Style := smsValue;
    Marks.YIndex:=-1;
    Marks.Visible := False;
    Legend.Multiplicity:=lmStyle;
  end;
end;

function TChartFm.CreateAreaChart: TdxAreaSeries;
begin
  Result := TdxAreaSeries.Create(FChart);
  with Result do
  begin
    Marks.Style := smsValue;
    Marks.YIndex:=-1;
    Marks.Visible := False;
    Legend.Multiplicity:=lmStyle;
  end;
end;

procedure TChartFm.InitBarChart;
var
  Bars: TdxBarSeries;
begin
  ResetAxis;
  Bars := CreateBarChart;
  with Bars do
  begin
    AxisIndexX:=-1;
    AxisIndexY:=-1;
  end;
  with FChart do
  begin
    AxisVisible := True;
    MinorToTick;
    AddMinor;
    TickToMinor;
    BottomAxis.Marks.Style := smsLabel;
  end;
  SetChartSeries(Bars);
end;

procedure TChartFm.InitHorizontalBarChart;
var
  Bars: TdxBarSeries;
begin
  ResetAxis;
  Bars := CreateBarChart;
  with Bars do
  begin
    AxisIndexX:=1;
    AxisIndexY:=0;
  end;
  with FChart do
  begin
    AxisVisible := True;
    MinorToTick;
    AddMinor;
    TickToMinor;
    BottomAxis.Marks.Style := smsLabel;
    AxisList[0].Alignment := calBottom;
    AxisList[1].Alignment := calLeft;
  end;
  SetChartSeries(Bars);
end;

procedure TChartFm.InitPieChart;
var
  Pie: TdxPieSeries;
  i: Integer;
begin
  ResetAxis;
  for i := FChart.Source.YFields.Count - 1 downto 1 do
  begin
    FChart.Source.YFields.Delete(i);
    Fchart.Styles.Delete(i);
  end;
  Pie := CreatePieChart;
  FChart.AxisVisible := False;
  MinorToTick;
  SetChartSeries(Pie);
end;

procedure TChartFm.InitLineChart;
var
  Line: TdxLineSeries;
begin
  ResetAxis;
  Line := CreateLineChart;
  FChart.AxisVisible := True;
  MinorToTick;
  SetChartSeries(Line);
end;

procedure TChartFm.InitAreaChart;
var
  Area: TdxAreaSeries;
begin
  ResetAxis;
  Area := CreateAreaChart;
  FChart.AxisVisible := True;
  MinorToTick;
  SetChartSeries(Area);
end;

procedure TChartFm.SetChartSeries(Series: TChartSeries);
begin
  if FChart.ChartSeries <> nil then
  begin
    with FChart.ChartSeries do
    begin
      Series.Marks.LabelBrush.Assign(Marks.LabelBrush);
      Series.Marks.LabelFont.Assign(Marks.LabelFont);
      Series.Marks.Frame.Assign(Marks.Frame);
      Series.Marks.LinkPen.Assign(Marks.LinkPen);
      Series.Marks.Margins.Assign(Marks.Margins);
      Series.Marks.Shape := Marks.Shape;
      Series.Marks.CalloutAngle := Marks.CalloutAngle;
      Series.Marks.Distance := Marks.Distance;
      Series.Marks.LinkDistance := Marks.LinkDistance;
      Series.Shadow.Assign(Shadow);
      Series.Depth := Depth;
    end;
    if (Series is TBasicPointSeries) and (FChart.ChartSeries is TBasicPointSeries) then
      with THackBasicPointSeries(FChart.ChartSeries) do
        THackBasicPointSeries(Series).MarkPositions := MarkPositions;
    if (Series is TdxBarSeries) and (FChart.ChartSeries is TdxBarSeries) then
      with TdxBarSeries(FChart.ChartSeries) do
        TdxBarSeries(Series).Effects := Effects;
  end;
  FChart.ClearSeries;
  FChart.AddSeries(Series);
  FChart.Init;
  FChart.FillDemoSource;
  AddProps;
end;

procedure TChartFm.GridNeedValues(Sender: TObject; const PropName: String;
  Values: TStrings);
begin
  if Pos('Shape', PropName) > 0 then
  begin
    Values.Add(IntToStr(Ord(clsRectangle)) + '=' + rsRectangle);
    Values.Add(IntToStr(Ord(clsRoundRect)) + '=' + rsRoundRect);
    Values.Add(IntToStr(Ord(clsRoundSide)) + '=' + rsRoundSide);
    Values.Add(IntToStr(Ord(clsEllipse)) + '=' + rsEllipse);
    {Values.AddObject(rsRectangle, TObject(clsRectangle));
    Values.AddObject(rsRoundRect, TObject(clsRoundRect));
    Values.AddObject(rsRoundSide, TObject(clsRoundSide));
    Values.AddObject(rsEllipse, TObject(clsEllipse)); }
  end
  else if PropName = 'ChartSeries.Source.Query' then
  begin
    FillQueries(Values);
  end
  else if PropName = 'ChartSeries.Source.LabelField' then
  begin
    FillLabelFields(Values);
  end
  else if Pos('ChartSeries.Source.YFields[', PropName) > 0 then
  begin
    FillYFields(Values);
  end
  else if PropName = 'ChartSeries.Stacked' then
  begin
    Values.Add('0=' + rsSideBySide);
    Values.Add('1=' + rsStack);
    {Values.Add(rsSideBySide);
    Values.AddObject(rsStack, TObject(1));  }
  end
  else if PropName = 'ChartSeries.MarkPositions' then
  begin
    if FChart.ChartSeries is TPieSeries then
    begin
      Values.Add(IntToStr(Ord(pmpAround)) + '=' + rsAround);
      Values.Add(IntToStr(Ord(pmpInside)) + '=' + rsInside);
      Values.Add(IntToStr(Ord(pmpLeftRight)) + '=' + rsOutSide);
      {Values.AddObject(rsAround, TObject(pmpAround));
      Values.AddObject(rsInside, TObject(pmpInside));
      Values.AddObject(rsOutSide, TObject(pmpLeftRight));}
    end
    else
    begin
      Values.Add(IntToStr(Ord(lmpOutside)) + '=' + rsOutSide);
      Values.Add(IntToStr(Ord(lmpInside)) + '=' + rsInside);
      {Values.AddObject(rsOutSide, TObject(lmpOutside));
      Values.AddObject(rsInside, TObject(lmpInside)); }
    end
  end
  else if PropName = 'Legend.UseSidebar' then
  begin
    Values.Add('0=' + rsInSide);
    Values.Add('1=' + rsOutside);
    {Values.AddObject(rsOutSide, TObject(1));
    Values.AddObject(rsInside, TObject(0));}
  end
  else if PropName = 'Legend.Alignment' then
  begin
    Values.Add(IntToStr(Ord(laTopCenter)) + '=' + rsTopCenter);
    Values.Add(IntToStr(Ord(laBottomCenter)) + '=' + rsBottomCenter);
    Values.Add(IntToStr(Ord(laTopRight)) + '=' + rsTopRight);
    Values.Add(IntToStr(Ord(laCenterRight)) + '=' + rsCenterRight);
    Values.Add(IntToStr(Ord(laBottomRight)) + '=' + rsBottomRight);
    Values.Add(IntToStr(Ord(laTopLeft)) + '=' + rsTopLeft);
    Values.Add(IntToStr(Ord(laCenterLeft)) + '=' + rsCenterLeft);
    Values.Add(IntToStr(Ord(laBottomLeft)) + '=' + rsBottomLeft);
    {Values.AddObject(rsTopCenter, TObject(laTopCenter));
    Values.AddObject(rsBottomCenter, TObject(laBottomCenter));
    Values.AddObject(rsTopRight, TObject(laTopRight));
    Values.AddObject(rsCenterRight, TObject(laCenterRight));
    Values.AddObject(rsBottomRight, TObject(laBottomRight));
    Values.AddObject(rsTopLeft, TObject(laTopLeft));
    Values.AddObject(rsCenterLeft, TObject(laCenterLeft));
    Values.AddObject(rsBottomLeft, TObject(laBottomLeft)); }
  end
  else if PropName = 'ChartSeries.Marks.Style' then
  begin
    Values.Add(IntToStr(Ord(smsLabel)) + '=' + rsMark);
    Values.Add(IntToStr(Ord(smsValue)) + '=' + rsValue);
    Values.Add(IntToStr(Ord(smsLabelValue)) + '=' + rsMarkAndValue);
    {Values.AddObject(rsMark, TObject(smsLabel));
    Values.AddObject(rsValue, TObject(smsValue));
    Values.AddObject(rsMarkAndValue, TObject(smsLabelValue)); }
  end
  else if PropName = 'ChartSeries.Pointer.Style' then
  begin
    Values.Add(IntToStr(Ord(psRectangle)) + '=' + rsRectangle);
    Values.Add(IntToStr(Ord(psCircle)) + '=' + rsCircle);
    Values.Add(IntToStr(Ord(psDiamond)) + '=' + rsDiamond);
    Values.Add(IntToStr(Ord(psTriangle)) + '=' + rsTriangle);
    Values.Add(IntToStr(Ord(psStar)) + '=' + rsStart);
    Values.Add(IntToStr(Ord(psFullStar)) + '=' + rsFullStar);
    Values.Add(IntToStr(Ord(psHexagon)) + '=' + rsHexagon);
    {Values.AddObject(rsRectangle, TObject(psRectangle));
    Values.AddObject(rsCircle, TObject(psCircle));
    Values.AddObject(rsDiamond, TObject(psDiamond));
    Values.AddObject(rsTriangle, TObject(psTriangle));
    Values.AddObject(rsStar, TObject(psStar));
    Values.AddObject(rsFullStar, TObject(psFullStar));
    Values.AddObject(rsHexagon, TObject(psHexagon));  }
  end
  else if PropName = 'ChartSeries.Effects' then
  begin
    Values.Add('0=' + rsNothing);
    Values.Add(IntToStr(Ord(befPhong)) + '=' + rsGlow);
    Values.Add(IntToStr(Ord(befChocolate)) + '=' + rsChocolate);
    {Values.Add(rsNothing);
    Values.AddObject(rsGlow, TObject(befPhong));
    Values.AddObject(rsChocolate, TObject(befChocolate));}
  end
  else if Pos('OverlapPolicy', PropName) > 0 then
  begin
    Values.Add('0=' + rsIgnore);
    Values.Add('1=' + rsHideNighbour);
  end;
end;

procedure TChartFm.GridPropChange(Sender: TObject; const PropName: String);
begin
  if Pos('ChartSeries.Source.', PropName) = 1 then
    FChart.FillDemoSource;
end;

procedure TChartFm.CopyChart(S, D: TdxChart);
var
  Src, Dest: TChartSeries;
  IsBar, IsPie, IsLine, IsArea: Boolean;
  i: Integer;
begin
  Src := S.ChartSeries;
  IsBar := Src is TBarSeries;
  IsPie := Src is TPieSeries;
  IsLine := Src is TLineSeries;
  IsArea := Src is TAreaSeries;

  if IsBar then Dest := TdxBarSeries.Create(D)
  else if IsPie then Dest := TdxPieSeries.Create(D)
  else if IsLine then Dest := TdxLineSeries.Create(D)
  else if IsArea then Dest := TdxAreaSeries.Create(D)
  else Dest := nil;//CreateBarChart;

  D.AxisList[1].Minors.Clear;
  D.ClearSeries;
  D.Styles.Clear;
  if Dest <> nil then
    D.AddSeries(Dest);

  D.DisableRedrawing;
  // Источник данных
  D.Query := S.Query;
  D.LabelField := S.LabelField;
  D.YFields := S.YFields;

  if Src <> nil then
  begin
    // Ряды
    if not IsPie then
    begin
      SetOrdProp(Dest, 'Stacked', GetOrdProp(Src, 'Stacked'));
      for i := 0 to S.Styles.Count - 1 do
        with TChartStyle(D.Styles.Add) do
        begin
          Brush.Assign(S.Styles[i].Brush);
          Pen.Assign(S.Styles[i].Pen);
          UseFont := False;
          Text := S.Styles[i].Text;
        end;
    end;
    // Диаграмма
    Dest.Depth := Src.Depth;
    if IsBar then
    begin
      SetOrdProp(Dest, 'Effects', GetOrdProp(Src, 'Effects'));
      SetOrdProp(Dest, 'BarWidthPercent', GetOrdProp(Src, 'BarWidthPercent'));
    end
    else if IsLine then
    begin
      SetOrdProp(Dest, 'ShowLines', GetOrdProp(Src, 'ShowLines'));
      SetOrdProp(Dest, 'ShowPoints', GetOrdProp(Src, 'ShowPoints'));
      TdxLineSeries(Dest).Pointer.HorizSize := TdxLineSeries(Src).Pointer.HorizSize;
      TdxLineSeries(Dest).Pointer.VertSize := TdxLineSeries(Src).Pointer.VertSize;
      TdxLineSeries(Dest).Pointer.Pen.Assign(TdxLineSeries(Src).Pointer.Pen);
    end
    else if IsArea then
    begin
      TdxAreaSeries(Dest).AreaLinesPen.Assign(TdxAreaSeries(Src).AreaLinesPen);
    end
    else if IsPie then
    begin
      TdxPieSeries(Dest).EdgePen.Assign(TdxPieSeries(Src).EdgePen);
    end;
    Dest.Shadow.Visible := Src.Shadow.Visible;
    Dest.Shadow.Color := Src.Shadow.Color;
    Dest.Shadow.Transparency := Src.Shadow.Transparency;
    Dest.Shadow.OffsetX := Src.Shadow.OffsetX;
    Dest.Shadow.OffsetY := Src.Shadow.OffsetY;
    Dest.Marks.Visible := Src.Marks.Visible;
    Dest.Marks.Style := Src.Marks.Style;
    SetOrdProp(Dest, 'MarkPositions', GetOrdProp(Src, 'MarkPositions'));
    if IsBar or isArea then
      SetOrdProp(Dest, 'MarkPositionCentered', GetOrdProp(Src, 'MarkPositionCentered'))
    else if IsPie then
      SetOrdProp(Dest, 'RotateLabels', GetOrdProp(Src, 'RotateLabels'));
    Dest.Marks.LabelFont.Assign(Src.Marks.LabelFont);
    Dest.Marks.LabelBrush.Color := Src.Marks.LabelBrush.Color;
    Dest.Marks.Frame.Visible := Src.Marks.Frame.Visible;
    Dest.Marks.Frame.Color := Src.Marks.Frame.Color;
    Dest.Marks.Frame.Width := Src.Marks.Frame.Width;
    Dest.Marks.Shape := Src.Marks.Shape;
    Dest.Marks.Margins.Left := Src.Marks.Margins.Left;
    Dest.Marks.Margins.Top := Src.Marks.Margins.Top;
    Dest.Marks.Margins.Right := Src.Marks.Margins.Right;
    Dest.Marks.Margins.Bottom := Src.Marks.Margins.Bottom;
    Dest.Marks.Distance := Src.Marks.Distance;
    Dest.Marks.CalloutAngle := Src.Marks.CalloutAngle;
    Dest.Marks.LinkPen.Visible := Src.Marks.LinkPen.Visible;
    Dest.Marks.LinkPen.Color := Src.Marks.LinkPen.Color;
    Dest.Marks.LinkPen.Style := Src.Marks.LinkPen.Style;
    Dest.Marks.LinkPen.Width := Src.Marks.LinkPen.Width;
    Dest.Marks.LinkDistance := Src.Marks.LinkDistance;
    Dest.Marks.OverlapPolicy := Src.Marks.OverlapPolicy;
    Dest.AxisIndexX := Src.AxisIndexX;
    Dest.AxisIndexY := Src.AxisIndexY;
    Dest.Marks.YIndex := Src.Marks.YIndex;
    Dest.Legend.Multiplicity := Src.Legend.Multiplicity;
  end;
  // Общий вид
  D.AxisVisible := S.AxisVisible;
  D.Color := S.Color;
  D.BackColor := S.BackColor;
  D.Frame.Assign(S.Frame);
  if not IsPie then
  begin
    D.Margins.Left := S.Margins.Left;
    D.Margins.Top := S.Margins.Top;
    D.Margins.Right := S.Margins.Right;
    D.Margins.Bottom := S.Margins.Bottom;
  end;
  D.MarginsExternal.Left := S.MarginsExternal.Left;
  D.MarginsExternal.Top := S.MarginsExternal.Top;
  D.MarginsExternal.Right := S.MarginsExternal.Right;
  D.MarginsExternal.Bottom := S.MarginsExternal.Bottom;
  //if not IsPie then
  begin
    // Ось категорий
    D.AxisList[1].Alignment := S.AxisList[1].Alignment;
    D.AxisList[1].Visible := S.AxisList[1].Visible;
    D.AxisList[1].Marks.Source := nil;
    D.AxisList[1].Marks.Visible := S.AxisList[1].Marks.Visible;
    D.AxisList[1].Marks.LabelFont.Assign(S.AxisList[1].Marks.LabelFont);
    D.AxisList[1].Marks.LabelBrush.Color := S.AxisList[1].Marks.LabelBrush.Color;
    //D.AxisList[1].Marks.LabelBrush.Style := S.AxisList[1].Marks.LabelBrush.Style;
    D.AxisList[1].Marks.Frame.Visible := S.AxisList[1].Marks.Frame.Visible;
    D.AxisList[1].Marks.Frame.Color := S.AxisList[1].Marks.Frame.Color;
    D.AxisList[1].Marks.Frame.Width := S.AxisList[1].Marks.Frame.Width;
    D.AxisList[1].Marks.Shape := S.AxisList[1].Marks.Shape;
    D.AxisList[1].Marks.Margins.Left := S.AxisList[1].Marks.Margins.Left;
    D.AxisList[1].Marks.Margins.Top := S.AxisList[1].Marks.Margins.Top;
    D.AxisList[1].Marks.Margins.Right := S.AxisList[1].Marks.Margins.Right;
    D.AxisList[1].Marks.Margins.Bottom := S.AxisList[1].Marks.Margins.Bottom;
    D.AxisList[1].Marks.Distance := S.AxisList[1].Marks.Distance;
    D.AxisList[1].Marks.OverlapPolicy := S.AxisList[1].Marks.OverlapPolicy;
    D.AxisList[1].AxisPen.Assign(S.AxisList[1].AxisPen);
    if IsBar then
    begin
      //D.AxisList[1].Minors.Clear;
      D.AxisList[1].Minors.Add;
      D.AxisList[1].Minors[0].Grid.Assign(S.AxisList[1].Minors[0].Grid);
      D.AxisList[1].Minors[0].TickColor := S.AxisList[1].Minors[0].TickColor;
      D.AxisList[1].Minors[0].TickLength := S.AxisList[1].Minors[0].TickLength;
      D.AxisList[1].Minors[0].TickInnerLength := S.AxisList[1].Minors[0].TickInnerLength;
      D.AxisList[1].Minors[0].Intervals.Assign(S.AxisList[1].Minors[0].Intervals);
    end;
    D.AxisList[1].Grid.Assign(S.AxisList[1].Grid);
    D.AxisList[1].TickColor := S.AxisList[1].TickColor;
    D.AxisList[1].TickLength := S.AxisList[1].TickLength;
    D.AxisList[1].TickInnerLength := S.AxisList[1].TickInnerLength;
    D.AxisList[1].Title.Visible := S.AxisList[1].Title.Visible;
    D.AxisList[1].Title.Caption := S.AxisList[1].Title.Caption;
    D.AxisList[1].Title.LabelFont.Assign(S.AxisList[1].Title.LabelFont);
    D.AxisList[1].Title.LabelBrush.Color := S.AxisList[1].Title.LabelBrush.Color;
    D.AxisList[1].Title.Frame.Visible := S.AxisList[1].Title.Frame.Visible;
    D.AxisList[1].Title.Frame.Color := S.AxisList[1].Title.Frame.Color;
    D.AxisList[1].Title.Frame.Width := S.AxisList[1].Title.Frame.Width;
    D.AxisList[1].Title.Shape := S.AxisList[1].Title.Shape;
    D.AxisList[1].Title.Margins.Left := S.AxisList[1].Title.Margins.Left;
    D.AxisList[1].Title.Margins.Top := S.AxisList[1].Title.Margins.Top;
    D.AxisList[1].Title.Margins.Right := S.AxisList[1].Title.Margins.Right;
    D.AxisList[1].Title.Margins.Bottom := S.AxisList[1].Title.Margins.Bottom;
    D.AxisList[1].Title.Distance := S.AxisList[1].Title.Distance;
    // Ось значений
    D.AxisList[0].Alignment := S.AxisList[0].Alignment;
    D.AxisList[0].Visible := S.AxisList[0].Visible;
    D.AxisList[0].Marks.Source := nil;
    D.AxisList[0].Marks.Visible := S.AxisList[0].Marks.Visible;
    D.AxisList[0].Marks.LabelFont.Assign(S.AxisList[0].Marks.LabelFont);
    D.AxisList[0].Marks.LabelBrush.Color := S.AxisList[0].Marks.LabelBrush.Color;
    D.AxisList[0].Marks.Frame.Visible := S.AxisList[0].Marks.Frame.Visible;
    D.AxisList[0].Marks.Frame.Color := S.AxisList[0].Marks.Frame.Color;
    D.AxisList[0].Marks.Frame.Width := S.AxisList[0].Marks.Frame.Width;
    D.AxisList[0].Marks.Shape := S.AxisList[0].Marks.Shape;
    D.AxisList[0].Marks.Margins.Left := S.AxisList[0].Marks.Margins.Left;
    D.AxisList[0].Marks.Margins.Top := S.AxisList[0].Marks.Margins.Top;
    D.AxisList[0].Marks.Margins.Right := S.AxisList[0].Marks.Margins.Right;
    D.AxisList[0].Marks.Margins.Bottom := S.AxisList[0].Marks.Margins.Bottom;
    D.AxisList[0].Marks.Distance := S.AxisList[0].Marks.Distance;
    D.AxisList[0].AxisPen.Assign(S.AxisList[0].AxisPen);
    D.AxisList[0].Grid.Assign(S.AxisList[0].Grid);
    D.AxisList[0].TickColor := S.AxisList[0].TickColor;
    D.AxisList[0].TickLength := S.AxisList[0].TickLength;
    D.AxisList[0].TickInnerLength := S.AxisList[0].TickInnerLength;
    D.AxisList[0].Intervals.Assign(S.AxisList[0].Intervals);
    D.AxisList[0].Title.Visible := S.AxisList[0].Title.Visible;
    D.AxisList[0].Title.Caption := S.AxisList[0].Title.Caption;
    D.AxisList[0].Title.LabelFont.Assign(S.AxisList[0].Title.LabelFont);
    D.AxisList[0].Title.LabelBrush.Color := S.AxisList[0].Title.LabelBrush.Color;
    D.AxisList[0].Title.Frame.Visible := S.AxisList[0].Title.Frame.Visible;
    D.AxisList[0].Title.Frame.Color := S.AxisList[0].Title.Frame.Color;
    D.AxisList[0].Title.Frame.Width := S.AxisList[0].Title.Frame.Width;
    D.AxisList[0].Title.Shape := S.AxisList[0].Title.Shape;
    D.AxisList[0].Title.Margins.Left := S.AxisList[0].Title.Margins.Left;
    D.AxisList[0].Title.Margins.Top := S.AxisList[0].Title.Margins.Top;
    D.AxisList[0].Title.Margins.Right := S.AxisList[0].Title.Margins.Right;
    D.AxisList[0].Title.Margins.Bottom := S.AxisList[0].Title.Margins.Bottom;
    D.AxisList[0].Title.Distance := S.AxisList[0].Title.Distance;
  end;
  // Заголовок
  D.Title.Visible := S.Title.Visible;
  D.Title.Text := S.Title.Text;
  D.Title.Font.Assign(S.Title.Font);
  D.Title.Brush.Color := S.Title.Brush.Color;
  D.Title.Frame.Assign(S.Title.Frame);
  D.Title.Shape := S.Title.Shape;
  D.Title.Margins.Left := S.Title.Margins.Left;
  D.Title.Margins.Top := S.Title.Margins.Top;
  D.Title.Margins.Right := S.Title.Margins.Right;
  D.Title.Margin := S.Title.Margin;
  // Подвал
  D.Foot.Visible := S.Foot.Visible;
  D.Foot.Text := S.Foot.Text;
  D.Foot.Font.Assign(S.Foot.Font);
  D.Foot.Brush.Color := S.Foot.Brush.Color;
  D.Foot.Frame.Assign(S.Foot.Frame);
  D.Foot.Shape := S.Foot.Shape;
  D.Foot.Margins.Left := S.Foot.Margins.Left;
  D.Foot.Margins.Top := S.Foot.Margins.Top;
  D.Foot.Margins.Right := S.Foot.Margins.Right;
  D.Foot.Margin := S.Foot.Margin;
  // Легенда
  D.Legend.Visible := S.Legend.Visible;
  D.Legend.Alignment := S.Legend.Alignment;
  D.Legend.UseSidebar := S.Legend.UseSidebar;
  D.Legend.ColumnCount := S.Legend.ColumnCount;
  D.Legend.Transparency := S.Legend.Transparency;
  D.Legend.BackgroundBrush.Color := S.Legend.BackgroundBrush.Color;
  D.Legend.Font.Assign(S.Legend.Font);
  D.Legend.Frame.Visible := S.Legend.frame.Visible;
  D.Legend.Frame.Color := S.Legend.Frame.Color;
  D.Legend.Frame.Width := S.Legend.Frame.Width;
  D.Legend.GridHorizontal.Assign(S.Legend.GridHorizontal);
  D.Legend.GridVertical.Assign(S.Legend.GridVertical);
  D.Legend.SymbolFrame.Assign(S.Legend.SymbolFrame);
  D.Legend.SymbolWidth := S.Legend.SymbolWidth;
  D.Legend.MarginX := S.Legend.MarginX;
  D.Legend.MarginY := S.Legend.MarginY;
  D.Legend.Spacing := S.Legend.Spacing;

  D.EnableRedrawing;
end;

procedure TChartFm.FillQueries(Items: TStrings);
var
  C: TComponent;
  i: Integer;
  Fm: TdxForm;
  SL: TStringListUtf8;
  RD: TReportData;
begin
  SL := TStringListUtf8.Create;
  Fm := TdxForm(FCmp.Owner);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxQueryGrid then
    begin
      RD := ReportMan.FindReport(GetId(C));
      //SL.AddObject(RD.Name, TObject(GetId(C)));
      SL.Add(IntToStr(GetId(C)) + '=' + RD.Name);
    end;
  end;
  SL.Sort;
  Items.AddStrings(SL);
  SL.Free;
end;

procedure TChartFm.FillLabelFields(Items: TStrings);
var
  RD: TReportData;
  i: Integer;
  SL: TStringList;
begin
  RD := ReportMan.FindReport(FChart.Query);
  if (RD = nil) or RD.IsEmpty then Exit;
  SL := TStringList.Create;
  for i := 0 to RD.GetFieldCount - 1 do
  begin
    if RD.GetFieldVisible(i) and (RD.GetFieldType(i) in [flText, flDate, flTime, flNumber]) then
      SL.Add(RD.GetFieldNameDS(i) + '=' + RD.GetFieldName(i));
  end;

  {for i := 0 to RD.Sources[0]^.Fields.Count - 1 do
  begin
    F := RD.Sources[0]^.Fields[i]^;
    LowF := GetLowField(@F)^;
    if F.Visible and (LowF.Tp in [flText, flDate, flTime, flNumber]) then
      SL.Add('f' + IntToStr(F.Id) + '=' + F.Name);
  end;
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i]^;
    if CF.Tp = flText then
      SL.Add('cf' + IntToStr(CF.Id) + '=' + CF.Name);
  end;    }
  SL.CustomSort(@CustomSortStrings);
  Items.AddStrings(SL);
  SL.Free;
end;

procedure TChartFm.FillYFields(Items: TStrings);
var
  RD: TReportData;
  i: Integer;
  SL: TStringList;
begin
  RD := ReportMan.FindReport(FChart.Query);
  if (RD = nil) or RD.IsEmpty then Exit;
  SL := TStringList.Create;

  for i := 0 to RD.GetFieldCount - 1 do
  begin
    if RD.GetFieldVisible(i) and (RD.GetFieldType(i) = flNumber) then
      SL.Add(RD.GetFieldNameDS(i) + '=' + RD.GetFieldName(i));
  end;

  {for i := 0 to RD.Sources[0]^.Fields.Count - 1 do
  begin
    F := RD.Sources[0]^.Fields[i]^;
    LowF := GetLowField(@F)^;
    if F.Visible and (LowF.Tp = flNumber) then
      SL.Add('f' + IntToStr(F.Id) + '=' + F.Name);
  end;
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i]^;
    if CF.Tp = flNumber then
      SL.Add('cf' + IntToStr(CF.Id) + '=' + CF.Name);
  end;  }
  SL.CustomSort(@CustomSortStrings);
  Items.AddStrings(SL);
  SL.Free;
end;

function TChartFm.ShowForm(C: TdxChart): Integer;
var
  i: Integer;
begin
  FCmp := C;
  FGrid.Clear;

  CopyChart(C, FChart);

  if FChart.Styles.Count = 0 then
    with TChartStyle(FChart.Styles.Add) do
    begin
      Brush.Color := clRed;
      UseFont := False;
    end;
  if FChart.YFields.Count = 0 then
    FChart.Source.YFields.Add;

  if FChart.ChartSeries = nil then
    InitBarChart
  else
  begin
    FChart.Init;
    FChart.FillDemoSource;
    AddProps;
  end;

  if FChart.ChartSeries is TdxBarSeries then
  begin
    if TdxBarSeries(FChart.ChartSeries).AxisIndexX < 0 then i := 0
    else i := 1;
  end
  else if FChart.ChartSeries is TdxPieSeries then i := 2
  else if FChart.ChartSeries is TdxLineSeries then i := 3
  else if FChart.ChartSeries is TdxAreaSeries then i := 4;
  ChartKind.ItemIndex := i;
  //ChartKind.OnChange(ChartKind);

  FGrid.Modified:=False;
  Result := ShowModal;
  if Result = mrOk then
  begin
    CopyChart(FChart, C);
    C.Init;
    C.FillDemoSource;
  end;
end;

end.

