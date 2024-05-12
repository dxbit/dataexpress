unit ShapeForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ButtonPanel, strconsts, dxctrls, CtrlUtils;

type

  { TShapeFm }

  TShapeFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    PenColor: TColorSampler;
    BrushColor: TColorSampler;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RadioButton1: TRadioButton;
    RadioButton10: TRadioButton;
    RadioButton11: TRadioButton;
    RadioButton12: TRadioButton;
    RadioButton13: TRadioButton;
    RadioButton14: TRadioButton;
    RadioButton15: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
    Shape1: TShape;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Shape15: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    SpinEdit1: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure Shape16MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
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

procedure TShapeFm.FormCreate(Sender: TObject);
begin
  Caption := rsShape;
  GroupBox1.Caption := rsShapeType;
  GroupBox2.Caption := rsLineStyle;
  Label1.Caption := rsLineWidth;
  Label2.Caption := rsLineColor;
  Label3.Caption := rsFillColor;
  PenColor.DefaultColor := clBlack;
  BrushColor.DefaultColor := clWhite;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
end;

function TShapeFm.ShowForm(Shape: TdxShape): Integer;
begin
  RadioButton1.Checked := Shape.Shape = stCircle;
  RadioButton2.Checked := Shape.Shape = stEllipse;
  RadioButton3.Checked := Shape.Shape = stSquare;
  RadioButton4.Checked := Shape.Shape = stRectangle;
  RadioButton5.Checked := Shape.Shape = stRoundSquare;
  RadioButton6.Checked := Shape.Shape = stRoundRect;
  RadioButton7.Checked := Shape.Shape = stSquaredDiamond;
  RadioButton8.Checked := Shape.Shape = stDiamond;
  RadioButton9.Checked := Shape.Shape = stTriangle;

  RadioButton10.Checked := Shape.Pen.Style = psSolid;
  RadioButton11.Checked := Shape.Pen.Style = psDash;
  RadioButton12.Checked := Shape.Pen.Style = psDashDotDot;
  RadioButton13.Checked := Shape.Pen.Style = psDot;
  RadioButton14.Checked := Shape.Pen.Style = psDashDot;
  RadioButton15.Checked := Shape.Pen.Style = psClear;

  SpinEdit1.Value := Shape.Pen.Width;
  PenColor.SampleColor := Shape.Pen.Color;
  BrushColor.SampleColor := Shape.Brush.Color;

  Result := ShowModal;
  if Result <> mrOk then Exit;

  if RadioButton1.Checked then Shape.Shape := stCircle
  else if RadioButton2.Checked then Shape.Shape := stEllipse
  else if RadioButton3.Checked then Shape.Shape := stSquare
  else if RadioButton4.Checked then Shape.Shape := stRectangle
  else if RadioButton5.Checked then Shape.Shape := stRoundSquare
  else if RadioButton6.Checked then Shape.Shape := stRoundRect
  else if RadioButton7.Checked then Shape.Shape := stSquaredDiamond
  else if RadioButton8.Checked then Shape.Shape := stDiamond
  else if RadioButton9.Checked then Shape.Shape := stTriangle;

  if RadioButton10.Checked then Shape.Pen.Style := psSolid
  else if RadioButton11.Checked then Shape.Pen.Style := psDash
  else if RadioButton12.Checked then Shape.Pen.Style := psDashDotDot
  else if RadioButton13.Checked then Shape.Pen.Style := psDot
  else if RadioButton14.Checked then Shape.Pen.Style := psDashDot
  else if RadioButton15.Checked then Shape.Pen.Style := psClear;

  Shape.Pen.Width := SpinEdit1.Value;
  Shape.Pen.Color := PenColor.SampleColor;
  Shape.Brush.Color := BrushColor.SampleColor;
end;

end.

