{-------------------------------------------------------------------------------

    Copyright 2015-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit ShapeForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ButtonPanel, strconsts, dxctrls, CtrlUtils, ShapeEx;

type

  { TShapeFm }

  TShapeFm = class(TForm)
    BClearRbn: TRadioButton;
    ButtonPanel1: TButtonPanel;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    PenColor: TColorSampler;
    BrushColor: TColorSampler;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CircleRbn: TRadioButton;
    PSolidRbn: TRadioButton;
    PDashRbn: TRadioButton;
    PDashDotRbn: TRadioButton;
    PDashDotDotRbn: TRadioButton;
    PDotRbn: TRadioButton;
    PClearRbn: TRadioButton;
    BSolidRbn: TRadioButton;
    BBDiagRbn: TRadioButton;
    BVertLineRbn: TRadioButton;
    BFDiagRbn: TRadioButton;
    EllipseRbn: TRadioButton;
    BHorzLineRbn: TRadioButton;
    BCrossRbn: TRadioButton;
    BDiagCrossRbn: TRadioButton;
    Shape34: TShape;
    TriangleDownRbn: TRadioButton;
    TriangleLeftRbn: TRadioButton;
    TriangleRightRbn: TRadioButton;
    StarRbn: TRadioButton;
    StarDownRbn: TRadioButton;
    VertLineRbn: TRadioButton;
    HorzLineRbn: TRadioButton;
    SquareRbn: TRadioButton;
    BDiagRbn: TRadioButton;
    FDiagRbn: TRadioButton;
    CrossRbn: TRadioButton;
    DiagCrossRbn: TRadioButton;
    RectRbn: TRadioButton;
    RoundSquareRbn: TRadioButton;
    RoundRectRbn: TRadioButton;
    SquareDiamondRbn: TRadioButton;
    DiamondRbn: TRadioButton;
    TriangleRbn: TRadioButton;
    Shape1: TShape;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Shape15: TShape;
    Shape16: TShape;
    Shape17: TShape;
    Shape18: TShape;
    Shape19: TShape;
    Shape2: TShape;
    Shape20: TShape;
    Shape21: TShape;
    Shape22: TShape;
    Shape23: TShape;
    Shape24: TShape;
    Shape25: TShape;
    Shape26: TShape;
    Shape27: TShape;
    Shape28: TShapeEx;
    Shape29: TShapeEx;
    Shape3: TShape;
    Shape30: TShapeEx;
    Shape31: TShapeEx;
    Shape32: TShapeEx;
    Shape33: TShapeEx;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    SampleShp: TShapeEx;
    LineWidthSpn: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure RadioButtonChange(Sender: TObject);
    procedure Shape16MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    procedure UpdateSample;
    procedure UpdateOptions(Shape: TdxShape);
  public
    { public declarations }
    function ShowForm(Shape: TdxShape): Integer;
  end;

var
  ShapeFm: TShapeFm;

function ShowShapeForm(Shape: TdxShape): Integer;

implementation

function ShowShapeForm(Shape: TdxShape): Integer;
begin
  if ShapeFm = nil then
  	ShapeFm := TShapeFm.Create(Application);
  Result := ShapeFm.ShowForm(Shape);
end;

{$R *.lfm}

{ TShapeFm }

procedure TShapeFm.Shape16MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with TColorDialog.Create(Self) do
  try
    Title := rsSelectColor;
    Color := TShape(Sender).Brush.Color;
    if Execute then
      TShape(Sender).Brush.Color := Color;
  finally
    Free;
  end;
end;

procedure TShapeFm.UpdateSample;
var
  St: TShapeType;
  Ste: TShapeTypeEx;
  Ps: TPenStyle;
  Bs: TBrushStyle;
begin
  St := stRectangle;
  Ste := steNone;

  if CircleRbn.Checked then St := stCircle
  else if EllipseRbn.Checked then St:= stEllipse
  else if SquareRbn.Checked then St := stSquare
  else if RectRbn.Checked then St := stRectangle
  else if RoundSquareRbn.Checked then St := stRoundSquare
  else if RoundRectRbn.Checked then St := stRoundRect
  else if SquareDiamondRbn.Checked then St := stSquaredDiamond
  else if DiamondRbn.Checked then St := stDiamond
  else if TriangleRbn.Checked then St := stTriangle
  else if TriangleDownRbn.Checked then St := stTriangleDown
  else if TriangleLeftRbn.Checked then St := stTriangleLeft
  else if TriangleRightRbn.Checked then St := stTriangleRight
  else if StarRbn.Checked then St := stStar
  else if StarDownRbn.Checked then St := stStarDown
  else if VertLineRbn.Checked then Ste := steVertLine
  else if HorzLineRbn.Checked then Ste := steHorzLine
  else if BDiagRbn.Checked then Ste := steBDiagonal
  else if FDiagRbn.Checked then Ste := steFDiagonal
  else if CrossRbn.Checked then Ste := steCross
  else if DiagCrossRbn.Checked then Ste := steDiagCross;

  if PSolidRbn.Checked then Ps := psSolid
  else if PDashRbn.Checked then Ps := psDash
  else if PDashDotDotRbn.Checked then Ps := psDashDotDot
  else if PDotRbn.Checked then Ps := psDot
  else if PDashDotRbn.Checked then Ps := psDashDot
  else if PClearRbn.Checked then Ps := psClear;

  if BSolidRbn.Checked then Bs := bsSolid
  else if BBDiagRbn.Checked then Bs := bsBDiagonal
  else if BFDiagRbn.Checked then Bs := bsFDiagonal
  else if BHorzLineRbn.Checked then Bs := bsHorizontal
  else if BVertLineRbn.Checked then Bs := bsVertical
  else if BCrossRbn.Checked then Bs := bsCross
  else if BDiagCrossRbn.Checked then Bs := bsDiagCross
  else if BClearRbn.Checked then Bs := bsClear;

  with SampleShp do
  begin
    if Ste <> steNone then
    begin
      Shape := stRectangle;
      ShapeEx := Ste;
    end
    else
    begin
      Shape := St;
      ShapeEx := steNone;
    end;
    Pen.Width := LineWidthSpn.Value;
    Pen.Color := PenColor.SampleColor;
    Pen.Style := Ps;
    Brush.Color := BrushColor.SampleColor;
    Brush.Style := Bs;
  end;
end;

procedure TShapeFm.UpdateOptions(Shape: TdxShape);
var
  St: TShapeType;
  Ste: TShapeTypeEx;
  Ps: TPenStyle;
  Bs: TBrushStyle;
begin
  St := Shape.Shape;
  Ste := Shape.ShapeEx;
  Ps := Shape.Pen.Style;
  Bs := Shape.Brush.Style;

  LineWidthSpn.Value := Shape.Pen.Width;
  PenColor.SampleColor := Shape.Pen.Color;
  BrushColor.SampleColor := Shape.Brush.Color;

  PSolidRbn.Checked := Ps = psSolid;
  PDashRbn.Checked := Ps = psDash;
  PDashDotDotRbn.Checked := Ps = psDashDotDot;
  PDotRbn.Checked := Ps = psDot;
  PDashDotRbn.Checked := Ps = psDashDot;
  PClearRbn.Checked := Ps = psClear;

  BSolidRbn.Checked := Bs = bsSolid;
  BBDiagRbn.Checked := Bs = bsBDiagonal;
  BFDiagRbn.Checked := Bs = bsFDiagonal;
  BHorzLineRbn.Checked := Bs = bsHorizontal;
  BVertLineRbn.Checked := Bs = bsVertical;
  BCrossRbn.Checked := Bs = bsCross;
  BDiagCrossRbn.Checked := Bs = bsDiagCross;
  BClearRbn.Checked := Bs = bsClear;

  CircleRbn.Checked := St = stCircle;
  EllipseRbn.Checked := St = stEllipse;
  SquareRbn.Checked := St = stSquare;
  RectRbn.Checked := St = stRectangle;
  RoundSquareRbn.Checked := St = stRoundSquare;
  RoundRectRbn.Checked := St = stRoundRect;
  SquareDiamondRbn.Checked := St = stSquaredDiamond;
  DiamondRbn.Checked := St = stDiamond;
  TriangleRbn.Checked := St = stTriangle;
  TriangleDownRbn.Checked := St = stTriangleDown;
  TriangleLeftRbn.Checked := St = stTriangleLeft;
  TriangleRightRbn.Checked := St = stTriangleRight;
  StarRbn.Checked := St = stStar;
  StarDownRbn.Checked := St = stStarDown;
  VertLineRbn.Checked := Ste = steVertLine;
  HorzLineRbn.Checked := Ste = steHorzLine;
  BDiagRbn.Checked := Ste = steBDiagonal;
  FDiagRbn.Checked := Ste = steFDiagonal;
  CrossRbn.Checked := Ste = steCross;
  DiagCrossRbn.Checked := Ste = steDiagCross;
end;

procedure TShapeFm.FormCreate(Sender: TObject);
begin
  Caption := rsShape;
  GroupBox1.Caption := rsShapeType;
  GroupBox2.Caption := rsLineStyle;
  GroupBox3.Caption := rsFillStyle;
  Label1.Caption := rsLineWidth;
  Label2.Caption := rsLineColor;
  Label3.Caption := rsFillColor;
  PenColor.DefaultColor := clBlack;
  BrushColor.DefaultColor := clWhite;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
end;

procedure TShapeFm.RadioButtonChange(Sender: TObject);
begin
  UpdateSample;
end;

function TShapeFm.ShowForm(Shape: TdxShape): Integer;
begin
  UpdateOptions(Shape);

  Result := ShowModal;
  if Result = mrOk then
  begin
    Shape.Shape := SampleShp.Shape;
    Shape.ShapeEx := SampleShp.ShapeEx;
    Shape.Brush := SampleShp.Brush;
    Shape.Pen := SampleShp.Pen;
  end;
end;

end.

