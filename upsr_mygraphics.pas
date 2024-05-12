
unit uPSR_MyGraphics;
interface
uses
  uPSRuntime, uPSUtils;



procedure RIRegisterTGRAPHICSOBJECT(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFont(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPEN(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBRUSH(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCanvas(cl: TPSRuntimeClassImporter);
procedure RIRegisterTGraphic(CL: TPSRuntimeClassImporter);
procedure RIRegisterTBitmap(CL: TPSRuntimeClassImporter; Streams: Boolean);
//procedure RIRegisterTPicture(CL: TPSRuntimeClassImporter);
procedure RIRegisterTIcon(CL: TPSRuntimeClassImporter);

procedure RIRegister_Graphics(Cl: TPSRuntimeClassImporter; Streams: Boolean);

implementation

uses
  Classes, Graphics, LCLType, scriptfuncs;

procedure TFontHandleR(Self: TFont; var T: Longint); begin T := Self.Reference.Handle; end;
procedure TFontPixelsPerInchR(Self: TFont; var T: Longint); begin T := Self.PixelsPerInch; end;
procedure TFontPixelsPerInchW(Self: TFont; T: Longint); begin Self.PixelsPerInch := T; end;
procedure TFontStyleR(Self: TFont; var T: TFontStyles); begin T := Self.Style; end;
procedure TFontStyleW(Self: TFont; T: TFontStyles); begin Self.Style:= T; end;

procedure RIRegisterTFont(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFont) do
  begin
    RegisterConstructor(@TFont.Create, 'Create');
    RegisterPropertyHelper(@TFontHandleR, nil, 'Handle');
    RegisterPropertyHelper(@TFontPixelsPerInchR, @TFontPixelsPerInchW, 'PixelsPerInch');
    RegisterPropertyHelper(@TFontStyleR, @TFontStyleW, 'Style');
  end;
end;
procedure TCanvasHandleR(Self: TCanvas; var T: Longint); begin T := Self.Handle; end;
procedure TCanvasHandleW(Self: TCanvas; T: Longint); begin Self.Handle:= T; end;
procedure TCanvasTextStyleR(Self: TCanvas; var T: TTextStyle); begin T := Self.TextStyle; end;
procedure TCanvasTextStyleW(Self: TCanvas; const T: TTextStyle); begin Self.TextStyle := T; end;

procedure TCanvasPixelsR(Self: TCanvas; var T: TColor; X,Y: Longint); begin T := Self.Pixels[X,Y]; end;
procedure TCanvasPixelsW(Self: TCanvas; T: TColor; X, Y: Longint); begin Self.Pixels[X,Y]:= T; end;
procedure TCanvasArc(Self : TCanvas; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); begin Self.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4); end;
procedure TCanvasChord(Self : TCanvas; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); begin self.Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4); end;
procedure TCanvasRectangle(Self : TCanvas; X1,Y1,X2,Y2 : integer); begin self.Rectangle(x1,y1,x2,y2); end;
procedure TCanvasRoundRect(Self : TCanvas; X1, Y1, X2, Y2, X3, Y3 : integer); begin self.RoundRect(X1, Y1, X2, Y2, X3, Y3); end;
procedure TCanvasEllipse(Self : TCanvas;X1, Y1, X2, Y2: Integer); begin self.Ellipse(X1, Y1, X2, Y2); end;
procedure TCanvasFillRect(Self : TCanvas; const Rect: TRect); begin self.FillRect(rect); end;
procedure TCanvasFloodFill(Self : TCanvas; X, Y: Integer; Color: TColor; FillStyle: TFillStyle); begin self.FloodFill(x,y,color,fillstyle); end;

type
  TTextStyleRec = record
    Alignment: Byte;
    Layout: Byte;
    SingleLine: boolean;
    Clipping  : boolean;
    ExpandTabs: boolean;
    ShowPrefix: boolean;
    Wordbreak : boolean;
    Opaque    : boolean;
    SystemFont: Boolean;
    RightToLeft: Boolean;
    EndEllipsis: Boolean;
  end;

procedure TCanvasTextRect(Self: TCanvas; ARect: TRect; X, Y: integer; const Text: string; TextStyle: TTextStyleRec);
var
  TS: TTextStyle;
begin
  with TextStyle do
  begin
    if Alignment in [0..2] then TS.Alignment:=TAlignment(Alignment);
    if Layout in [0..2] then TS.Layout:=TTextLayout(Layout);
    TS.SingleLine:=SingleLine;
    TS.Clipping:=Clipping;
    TS.ExpandTabs:=ExpandTabs;
    TS.ShowPrefix:=ShowPrefix;
    TS.Wordbreak:=WordBreak;
    TS.Opaque:=Opaque;
    TS.SystemFont:=SystemFont;
    TS.RightToLeft:=RightToLeft;
    TS.EndEllipsis:=EndEllipsis;
  end;
  Self.TextRect(ARect, X, Y, Text, TS);
end;

procedure RIRegisterTCanvas(cl: TPSRuntimeClassImporter); // requires TPersistent
begin
  with Cl.Add(TCanvas) do
  begin
    RegisterMethod(@TCanvasArc, 'Arc');
    RegisterMethod(@TCanvasChord, 'Chord');
    RegisterMethod(@TCanvasRectangle, 'Rectangle');
    RegisterMethod(@TCanvasRoundRect, 'RoundRect');
    RegisterMethod(@TCanvasEllipse, 'Ellipse');
    RegisterMethod(@TCanvasFillRect, 'FillRect');
    RegisterMethod(@TCanvasFloodFill, 'FloodFill');
    RegisterMethod(@TCanvas.Draw, 'Draw');
    RegisterMethod(@TCanvasTextRect, 'TextRect');

    RegisterMethod(@TCanvas.Lineto, 'LineTo');
    RegisterMethod(@TCanvas.Moveto, 'MoveTo');
    RegisterMethod(@TCanvas.Pie, 'Pie');
    RegisterMethod(@TCanvas.Refresh, 'Refresh');
    RegisterMethod(@TCanvas.TextHeight, 'TextHeight');
    RegisterMethod(@TCanvas.TextOut, 'TextOut');
    RegisterMethod(@TCanvas.TextWidth, 'TextWidth');
    RegisterPropertyHelper(@TCanvasHandleR, @TCanvasHandleW, 'Handle');
    RegisterPropertyHelper(@TCanvasPixelsR, @TCanvasPixelsW, 'Pixels');
    RegisterPropertyHelper(@TCanvasTextStyleR, @TCanvasTextStyleW, 'TextStyle');
  end;
end;


procedure TGRAPHICSOBJECTONCHANGE_W(Self: TGraphicsObject; T: TNotifyEvent); begin Self.OnChange := t; end;
procedure TGRAPHICSOBJECTONCHANGE_R(Self: TGraphicsObject; var T: TNotifyEvent); begin T :=Self.OnChange; end;


procedure RIRegisterTGRAPHICSOBJECT(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TGRAPHICSOBJECT) do
  begin
    RegisterPropertyHelper(@TGRAPHICSOBJECTONCHANGE_R, @TGRAPHICSOBJECTONCHANGE_W, 'OnChange');
  end;
end;

procedure RIRegisterTPEN(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPEN) do
  begin
    RegisterConstructor(@TPEN.CREATE, 'Create');
  end;
end;

procedure RIRegisterTBRUSH(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TBRUSH) do
  begin
    RegisterConstructor(@TBRUSH.CREATE, 'Create');
  end;
end;

procedure TGraphicOnChange_W(Self: TGraphic; const T: TNotifyEvent); begin Self.OnChange := T; end;
procedure TGraphicOnChange_R(Self: TGraphic; var T: TNotifyEvent); begin T := Self.OnChange; end;
procedure TGraphicWidth_W(Self: TGraphic; const T: Integer); begin Self.Width := T; end;
procedure TGraphicWidth_R(Self: TGraphic; var T: Integer); begin T := Self.Width; end;
procedure TGraphicModified_W(Self: TGraphic; const T: Boolean); begin Self.Modified := T; end;
procedure TGraphicModified_R(Self: TGraphic; var T: Boolean); begin T := Self.Modified; end;
procedure TGraphicHeight_W(Self: TGraphic; const T: Integer); begin Self.Height := T; end;
procedure TGraphicHeight_R(Self: TGraphic; var T: Integer); begin T := Self.Height; end;
procedure TGraphicEmpty_R(Self: TGraphic; var T: Boolean); begin T := Self.Empty; end;

{procedure TGraphicLoadFromStream(Sender: TGraphic; Stream: TStream);
begin Sender.LoadFromStream(Stream); end;
procedure TGraphicSaveToStream(Sender: TGraphic; Stream: TStream);
begin Sender.SaveToStream(Stream); end;  }

procedure RIRegisterTGraphic(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TGraphic) do
  begin
    RegisterVirtualConstructor(@TGraphic.Create, 'Create');
    RegisterVirtualMethod(@TGraphic.LoadFromFile, 'LoadFromFile');
    RegisterVirtualMethod(@TGraphic.SaveToFile, 'SaveToFile');
    RegisterPropertyHelper(@TGraphicEmpty_R,nil,'Empty');
    RegisterPropertyHelper(@TGraphicHeight_R,@TGraphicHeight_W,'Height');
    RegisterPropertyHelper(@TGraphicWidth_R,@TGraphicWidth_W,'Width');
    RegisterEventPropertyHelper(@TGraphicOnChange_R,@TGraphicOnChange_W,'OnChange');

    RegisterPropertyHelper(@TGraphicModified_R,@TGraphicModified_W,'Modified');
  end;
end;

procedure TBitmapTransparentColor_R(Self: TBitmap; var T: TColor); begin T := Self.TransparentColor; end;
procedure TBitmapTransparentColor_W(Self: TBitmap; T: TColor); begin Self.TransparentColor := T; end;
procedure TBitmapPalette_W(Self: TBitmap; const T: HPALETTE); begin Self.Palette := T; end;
procedure TBitmapPalette_R(Self: TBitmap; var T: HPALETTE); begin T := Self.Palette; end;
procedure TBitmapMonochrome_W(Self: TBitmap; const T: Boolean); begin Self.Monochrome := T; end;
procedure TBitmapMonochrome_R(Self: TBitmap; var T: Boolean); begin T := Self.Monochrome; end;
procedure TBitmapHandle_W(Self: TBitmap; const T: HBITMAP); begin Self.Handle := T; end;
procedure TBitmapHandle_R(Self: TBitmap; var T: HBITMAP); begin T := Self.Handle; end;
procedure TBitmapCanvas_R(Self: TBitmap; var T: TCanvas); begin T := Self.Canvas; end;

procedure TBitmapLoadFromStream(Sender: TBitmap; Stream: TStream);
begin Sender.LoadFromStream(Stream); end;

procedure RIRegisterTBitmap(CL: TPSRuntimeClassImporter; Streams: Boolean);
begin
  with CL.Add(TBitmap) do
  begin
    if Streams then begin
      RegisterMethod(@TBitmapLoadFromStream, 'LoadFromStream');
      RegisterMethod(@TBitmap.SaveToStream, 'SaveToStream');
    end;
    RegisterPropertyHelper(@TBitmapCanvas_R,nil,'Canvas');
    RegisterPropertyHelper(@TBitmapHandle_R,@TBitmapHandle_W,'Handle');

    RegisterPropertyHelper(@TBitmapMonochrome_R,@TBitmapMonochrome_W,'Monochrome');
    RegisterPropertyHelper(@TBitmapPalette_R,@TBitmapPalette_W,'Palette');
    RegisterPropertyHelper(@TBitmapTransparentColor_R,@TBitmapTransparentColor_W,'TransparentColor');
  end;
end;

{procedure TPictureBitmap_W(Self: TPicture; const T: TBitmap); begin Self.Bitmap := T; end;
procedure TPictureBitmap_R(Self: TPicture; var T: TBitmap); begin T := Self.Bitmap; end;

procedure RIRegisterTPicture(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TPicture) do
    registerPropertyHelper(@TPictureBitmap_R,@TPictureBitmap_W,'Bitmap');
end; }

procedure TIconLoadFromStream(Sender: TIcon; Stream: TStream);
begin Sender.LoadFromStream(Stream); end;

procedure TIconLoadFromStringBase64(Sender: TIcon; const StrBase64: String);
var
  St: TStringStream;
begin
  St := TStringStream.Create(DecodeBase64(StrBase64, False));
  try
    TIconLoadFromStream(Sender, St);
  finally
    St.Free;
  end;
end;

procedure RIRegisterTIcon(CL: TPSRuntimeClassImporter);
begin
  with Cl.Add(TIcon) do
  begin
    RegisterMethod(@TIconLoadFromStream, 'LoadFromStream');
    RegisterMethod(@TIcon.SaveToStream, 'SaveToStream');
    RegisterMethod(@TIconLoadFromStringBase64, 'LoadFromStringBase64');
  end;
end;

procedure RIRegister_Graphics(Cl: TPSRuntimeClassImporter; Streams: Boolean);
begin
  RIRegisterTGRAPHICSOBJECT(cl);
  RIRegisterTFont(Cl);
  RIRegisterTCanvas(cl);
  RIRegisterTPEN(cl);
  RIRegisterTBRUSH(cl);
  RIRegisterTGraphic(CL);
  RIRegisterTBitmap(CL, Streams);
  //RIRegisterTPicture(CL);
  RIRegisterTIcon(CL);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)

end.





