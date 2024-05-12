{ Compiletime Graphics support }
unit uPSC_MyGraphics;

interface
uses
  uPSCompiler, uPSUtils;



procedure SIRegister_Graphics_TypesAndConsts(Cl: TPSPascalCompiler);
procedure SIRegister_Graphics_TypesAndConstsWeb(Cl: TPSPascalCompiler);
procedure SIRegisterTGRAPHICSOBJECT(Cl: TPSPascalCompiler);
procedure SIRegisterTFont(Cl: TPSPascalCompiler; Web: Boolean);
procedure SIRegisterTPEN(Cl: TPSPascalCompiler; Web: Boolean);
procedure SIRegisterTBRUSH(Cl: TPSPascalCompiler; Web: Boolean);
procedure SIRegisterTCanvas(cl: TPSPascalCompiler);
procedure SIRegisterTGraphic(CL: TPSPascalCompiler);
procedure SIRegisterTBitmap(CL: TPSPascalCompiler; Streams: Boolean);
//procedure SIRegisterTPicture(CL: TPSPascalCompiler);
procedure SIRegisterTIcon(CL: TPSPascalCompiler);

procedure SIRegister_Graphics(Cl: TPSPascalCompiler; Streams: Boolean);
procedure SIRegister_GraphicsWeb(Cl: TPSPascalCompiler);

implementation
uses
  Graphics;

procedure SIRegister_Graphics_TypesAndConstsWeb(Cl: TPSPascalCompiler);
begin
  cl.AddConstantN('clBlack', 'Integer').Value^.ts32 := $000000;
   cl.AddConstantN('clMaroon', 'Integer').Value^.ts32 := $000080;
   cl.AddConstantN('clGreen', 'Integer').Value^.ts32 := $008000;
   cl.AddConstantN('clOlive', 'Integer').Value^.ts32 := $008080;
   cl.AddConstantN('clNavy', 'Integer').Value^.ts32 := $800000;
   cl.AddConstantN('clPurple', 'Integer').Value^.ts32 := $800080;
   cl.AddConstantN('clTeal', 'Integer').Value^.ts32 := $808000;
   cl.AddConstantN('clGray', 'Integer').Value^.ts32 := $808080;
   cl.AddConstantN('clSilver', 'Integer').Value^.ts32 := $C0C0C0;
   cl.AddConstantN('clRed', 'Integer').Value^.ts32 := $0000FF;
   cl.AddConstantN('clLime', 'Integer').Value^.ts32 := $00FF00;
   cl.AddConstantN('clYellow', 'Integer').Value^.ts32 := $00FFFF;
   cl.AddConstantN('clBlue', 'Integer').Value^.ts32 := $FF0000;
   cl.AddConstantN('clFuchsia', 'Integer').Value^.ts32 := $FF00FF;
   cl.AddConstantN('clAqua', 'Integer').Value^.ts32 := $FFFF00;
   cl.AddConstantN('clLtGray', 'Integer').Value^.ts32 := $C0C0C0;
   cl.AddConstantN('clDkGray', 'Integer').Value^.ts32 := $808080;
   cl.AddConstantN('clWhite', 'Integer').Value^.ts32 := $FFFFFF;
   cl.AddConstantN('clNone', 'Integer').Value^.ts32 := $1FFFFFFF;
   cl.AddConstantN('clDefault', 'Integer').Value^.ts32 := $20000000;

   cl.AddConstantN('clMoneyGreen', 'Integer').Value^.ts32 := $C0DCC0;
   cl.AddConstantN('clSkyBlue', 'Integer').Value^.ts32 := $F0CAA6;
   cl.AddConstantN('clCream', 'Integer').Value^.ts32 := $F0FBFF;
   cl.AddConstantN('clMedGray', 'Integer').Value^.ts32 := $A4A0A0;

   Cl.addTypeS('TFontStyle', '(fsBold, fsItalic, fsUnderline, fsStrikeOut)');
   Cl.addTypeS('TFontStyles', 'set of TFontStyle');

   cl.addTypeS('TColor', 'Integer');
   cl.AddTypeS('TPenStyle', '(psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear, psInsideFrame)');
   cl.AddTypeS('TBrushStyle', '(bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross)');
end;

procedure SIRegisterTGRAPHICSOBJECT(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TGraphicsObject') do
  begin
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
  end;
end;

procedure SIRegisterTFont(Cl: TPSPascalCompiler; Web: Boolean);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicsObject'), 'TFont') do
  begin
    if not Web then
    begin
      RegisterMethod('constructor Create;');
      RegisterProperty('Handle', 'Integer', iptR);
      RegisterProperty('Pitch', 'TFontPitch', iptRW);
      RegisterProperty('PixelsPerInch', 'Integer', iptRW);
    end;
    RegisterProperty('Color', 'TColor', iptRW);
    RegisterProperty('Height', 'Integer', iptRW);
    RegisterProperty('Name', 'string', iptRW);
    RegisterProperty('Size', 'Integer', iptRW);
    RegisterProperty('Style', 'TFontStyles', iptrw);
  end;
end;

procedure SIRegisterTCanvas(cl: TPSPascalCompiler); // requires TPersistent
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TCanvas') do
  begin
    RegisterMethod('procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);');
    RegisterMethod('procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);');
    RegisterMethod('procedure Draw(X, Y: Integer; Graphic: TGraphic);');
    RegisterMethod('procedure Ellipse(X1, Y1, X2, Y2: Integer);');
    RegisterMethod('procedure FillRect(const Rect: TRect);');
    RegisterMethod('procedure FloodFill(X, Y: Integer; Color: TColor; FillStyle: TFillStyle);');
    RegisterMethod('procedure LineTo(X, Y: Integer);');
    RegisterMethod('procedure MoveTo(X, Y: Integer);');
    RegisterMethod('procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);');
    RegisterMethod('procedure Rectangle(X1, Y1, X2, Y2: Integer);');
    RegisterMethod('procedure Refresh;');
    RegisterMethod('procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);');
    RegisterMethod('function TextHeight(Text: string): Integer;');
    RegisterMethod('procedure TextOut(X, Y: Integer; Text: string);');
    RegisterMethod('function TextWidth(Text: string): Integer;');
    RegisterMethod('procedure TextRect(ARect: TRect; X, Y: integer; const Text: string; TextStyle: TTextStyleRec)');
    RegisterProperty('Handle', 'Integer', iptRw);
    RegisterProperty('Pixels', 'TColor Integer Integer', iptRW);
    RegisterProperty('Brush', 'TBrush', iptR);
    //RegisterProperty('CopyMode', 'Byte', iptRw);
    RegisterProperty('Font', 'TFont', iptR);
    RegisterProperty('Pen', 'TPen', iptR);
    //RegisterProperty('TextStyle', 'TTextStyle', iptRW);
  end;
end;

procedure SIRegisterTPEN(Cl: TPSPascalCompiler; Web: Boolean);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicsObject'), 'TPen') do
  begin
    if not Web then
    begin
      RegisterMethod('constructor Create');
      RegisterProperty('Mode', 'TPenMode', iptrw);
    end;
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Style', 'TPenStyle', iptrw);
    RegisterProperty('Width', 'Integer', iptrw);
  end;
end;

procedure SIRegisterTBRUSH(Cl: TPSPascalCompiler; Web: Boolean);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicsObject'), 'TBrush') do
  begin
    if not Web then
    begin
      RegisterMethod('constructor Create');
    end;
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Style', 'TBrushStyle', iptrw);
  end;
end;

procedure SIRegister_Graphics_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  cl.AddConstantN('clScrollBar', 'Integer').Value^.ts32 := clScrollBar;
  cl.AddConstantN('clBackground', 'Integer').Value^.ts32 := clBackground;
  cl.AddConstantN('clActiveCaption', 'Integer').Value^.ts32 := clActiveCaption;
  cl.AddConstantN('clInactiveCaption', 'Integer').Value^.ts32 := clInactiveCaption;
  cl.AddConstantN('clMenu', 'Integer').Value^.ts32 := clMenu;
  cl.AddConstantN('clWindow', 'Integer').Value^.ts32 := clWindow;
  cl.AddConstantN('clWindowFrame', 'Integer').Value^.ts32 := clWindowFrame;
  cl.AddConstantN('clMenuText', 'Integer').Value^.ts32 := clMenuText;
  cl.AddConstantN('clWindowText', 'Integer').Value^.ts32 := clWindowText;
  cl.AddConstantN('clCaptionText', 'Integer').Value^.ts32 := clCaptionText;
  cl.AddConstantN('clActiveBorder', 'Integer').Value^.ts32 := clActiveBorder;
  cl.AddConstantN('clInactiveBorder', 'Integer').Value^.ts32 := clInactiveCaption;
  cl.AddConstantN('clAppWorkSpace', 'Integer').Value^.ts32 := clAppWorkSpace;
  cl.AddConstantN('clHighlight', 'Integer').Value^.ts32 := clHighlight;
  cl.AddConstantN('clHighlightText', 'Integer').Value^.ts32 := clHighlightText;
  cl.AddConstantN('clBtnFace', 'Integer').Value^.ts32 := clBtnFace;
  cl.AddConstantN('clBtnShadow', 'Integer').Value^.ts32 := clBtnShadow;
  cl.AddConstantN('clGrayText', 'Integer').Value^.ts32 := clGrayText;
  cl.AddConstantN('clBtnText', 'Integer').Value^.ts32 := clBtnText;
  cl.AddConstantN('clInactiveCaptionText', 'Integer').Value^.ts32 := clInactiveCaptionText;
  cl.AddConstantN('clBtnHighlight', 'Integer').Value^.ts32 := clBtnHighlight;
  cl.AddConstantN('cl3DDkShadow', 'Integer').Value^.ts32 := cl3DDkShadow;
  cl.AddConstantN('cl3DLight', 'Integer').Value^.ts32 := cl3DLight;
  cl.AddConstantN('clInfoText', 'Integer').Value^.ts32 := clInfoText;
  cl.AddConstantN('clInfoBk', 'Integer').Value^.ts32 := clInfoBk;

  cl.AddConstantN('clBlack', 'Integer').Value^.ts32 := $000000;
  cl.AddConstantN('clMaroon', 'Integer').Value^.ts32 := $000080;
  cl.AddConstantN('clGreen', 'Integer').Value^.ts32 := $008000;
  cl.AddConstantN('clOlive', 'Integer').Value^.ts32 := $008080;
  cl.AddConstantN('clNavy', 'Integer').Value^.ts32 := $800000;
  cl.AddConstantN('clPurple', 'Integer').Value^.ts32 := $800080;
  cl.AddConstantN('clTeal', 'Integer').Value^.ts32 := $808000;
  cl.AddConstantN('clGray', 'Integer').Value^.ts32 := $808080;
  cl.AddConstantN('clSilver', 'Integer').Value^.ts32 := $C0C0C0;
  cl.AddConstantN('clRed', 'Integer').Value^.ts32 := $0000FF;
  cl.AddConstantN('clLime', 'Integer').Value^.ts32 := $00FF00;
  cl.AddConstantN('clYellow', 'Integer').Value^.ts32 := $00FFFF;
  cl.AddConstantN('clBlue', 'Integer').Value^.ts32 := $FF0000;
  cl.AddConstantN('clFuchsia', 'Integer').Value^.ts32 := $FF00FF;
  cl.AddConstantN('clAqua', 'Integer').Value^.ts32 := $FFFF00;
  cl.AddConstantN('clLtGray', 'Integer').Value^.ts32 := $C0C0C0;
  cl.AddConstantN('clDkGray', 'Integer').Value^.ts32 := $808080;
  cl.AddConstantN('clWhite', 'Integer').Value^.ts32 := $FFFFFF;
  cl.AddConstantN('clNone', 'Integer').Value^.ts32 := $1FFFFFFF;
  cl.AddConstantN('clDefault', 'Integer').Value^.ts32 := $20000000;

  cl.AddConstantN('clMoneyGreen', 'Integer').Value^.ts32 := $C0DCC0;
  cl.AddConstantN('clSkyBlue', 'Integer').Value^.ts32 := $F0CAA6;
  cl.AddConstantN('clCream', 'Integer').Value^.ts32 := $F0FBFF;
  cl.AddConstantN('clMedGray', 'Integer').Value^.ts32 := $A4A0A0;

  Cl.addTypeS('TFontStyle', '(fsBold, fsItalic, fsUnderline, fsStrikeOut)');
  Cl.addTypeS('TFontStyles', 'set of TFontStyle');

  cl.AddTypeS('TFontPitch', '(fpDefault, fpVariable, fpFixed)');
  cl.AddTypeS('TPenStyle', '(psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear, psInsideFrame)');
  cl.AddTypeS('TPenMode', '(pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy, pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge, pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor)');
  cl.AddTypeS('TBrushStyle', '(bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross)');
  cl.addTypeS('TColor', 'Integer');
  cl.AddTypeS('TFillStyle', '(fsSurface, fsBorder)');

  cl.AddTypeS('TTextLayout', '(tlTop, tlCenter, tlBottom)');
  //Cl.AddTypeS('TTextStyle', 'record Alignment: TAlignment; Layout: TTextLayout; SingleLine: Boolean; Clipping: Boolean; ExpandTabs: Boolean; ShowPrefix: Boolean; WordBreak: Boolean; Opaque: Boolean; SystemFont: Boolean; RightToLeft: Boolean; EndEllipsis: Boolean; end;');
  Cl.AddTypeS('TTextStyleRec', 'record Alignment: Byte; Layout: Byte; SingleLine: Boolean; Clipping: Boolean; ExpandTabs: Boolean; ShowPrefix: Boolean; WordBreak: Boolean; Opaque: Boolean; SystemFont: Boolean; RightToLeft: Boolean; EndEllipsis: Boolean; end;');

  cl.addTypeS('HBITMAP', 'Integer');
  cl.addTypeS('HPALETTE', 'Integer');
end;

procedure SIRegisterTGraphic(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TPersistent'),'TGraphic') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure LoadFromFile(const FileName: string)');
    RegisterMethod('procedure SaveToFile(const FileName: string)');
    //RegisterMethod('procedure LoadFromStream(Stream: TStream); virtual; abstract');
    //RegisterMethod('procedure SaveToStream(Stream: TStream); virtual; abstract');
    RegisterProperty('Empty', 'Boolean', iptr);
    RegisterProperty('Height', 'Integer', iptrw);
    RegisterProperty('Modified', 'Boolean', iptrw);
    RegisterProperty('Width', 'Integer', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    IsAbstract:=True;
  end;
end;

procedure SIRegisterTBitmap(CL: TPSPascalCompiler; Streams: Boolean);
begin
  with CL.AddClassN(CL.FindClass('TGraphic'),'TBitmap') do
  begin
    if Streams then begin
      RegisterMethod('procedure LoadFromStream(Stream: TStream)');
      RegisterMethod('procedure SaveToStream(Stream: TStream)');
    end;
    RegisterProperty('Canvas', 'TCanvas', iptr);
    RegisterProperty('Handle', 'HBITMAP', iptrw);
    RegisterProperty('Monochrome', 'Boolean', iptrw);
    RegisterProperty('Palette', 'HPALETTE', iptrw);
    RegisterProperty('TransparentColor', 'TColor', iptrw);
  end;
end;

{procedure SIRegisterTPicture(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TPersistent'),'TPicture') do
  begin
    RegisterProperty('Bitmap','TBitmap',iptrw);
  end;
end; }

procedure SIRegisterTIcon(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TGraphic'), 'TIcon') do
  begin
    RegisterMethod('procedure LoadFromStream(Stream: TStream)');
    RegisterMethod('procedure SaveToStream(Stream: TStream)');
    RegisterMethod('procedure LoadFromStringBase64(const StrBase64: String)');
  end;
end;

procedure SIRegister_Graphics(Cl: TPSPascalCompiler; Streams: Boolean);
begin
  SIRegister_Graphics_TypesAndConsts(Cl);
  SIRegisterTGRAPHICSOBJECT(Cl);
  SIRegisterTGraphic(Cl);
  SIRegisterTFont(Cl, False);
  SIRegisterTPEN(cl, False);
  SIRegisterTBRUSH(cl, False);
  SIRegisterTCanvas(cl);
  SIRegisterTBitmap(Cl, Streams);
  //SIRegisterTPicture(cl);
  SIRegisterTIcon(cl);
end;

procedure SIRegister_GraphicsWeb(Cl: TPSPascalCompiler);
begin
  SIRegister_Graphics_TypesAndConstsWeb(Cl);
  SIRegisterTFont(Cl, True);
  SIRegisterTPEN(cl, True);
  SIRegisterTBRUSH(cl, True);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)

End.
