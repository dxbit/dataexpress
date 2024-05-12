
unit uPSR_MyButtons;
interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegisterTSPEEDBUTTON(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBITBTN(Cl: TPSRuntimeClassImporter);

procedure RIRegister_Buttons(Cl: TPSRuntimeClassImporter);

implementation
uses
  Classes, Controls, Buttons, BGRABitmap, Graphics, scriptfuncs;

procedure TSpeedButtonLoadGlyphFromFile(Self: TSpeedButton; const FileName: String);
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(FileName, True);
  try
    Self.OnChangeBounds := nil;  // Отменяем обработчик, т. к. он не дает сменить значок.
    Self.Glyph := Bmp.Bitmap;
  finally
    Bmp.Free;
  end;
end;

procedure TSpeedButtonLoadGlyphFromStream(Self: TSpeedButton; St: TStream);
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(St);
  try
    Self.OnChangeBounds := nil;  // Отменяем обработчик, т. к. он не дает сменить значок.
    Self.Glyph := Bmp.Bitmap;
  finally
    Bmp.Free;
  end;
end;

procedure TSpeedButtonLoadGlyphFromStringBase64(Self: TSpeedButton; S: String);
var
  St: TStringStream;
begin
  St := TStringStream.Create(DecodeBase64(S, False));
  try
    TSpeedButtonLoadGlyphFromStream(Self, St);
  finally
    St.Free;
  end;
end;

procedure RIRegisterTSPEEDBUTTON(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSPEEDBUTTON) do
  begin
    RegisterMethod(@TSpeedButton.Click, 'Click');
    RegisterMethod(@TSpeedButtonLoadGlyphFromFile, 'LoadGlyphFromFile');
    RegisterMethod(@TSpeedButtonLoadGlyphFromStream, 'LoadGlyphFromStream');
    RegisterMethod(@TSpeedButtonLoadGlyphFromStringBase64, 'LoadGlyphFromStringBase64');
  end;
end;

procedure TCustomBitBtnLoadGlyphFromFile(Self: TBitBtn; const FileName: String);
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(FileName, True);
  try
    Self.Glyph := Bmp.Bitmap;
  finally
    Bmp.Free;
  end;
end;

procedure TCustomBitBtnLoadGlyphFromStream(Self: TBitBtn; St: TStream);
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(St);
  try
    Self.Glyph := Bmp.Bitmap;
  finally
    Bmp.Free;
  end;
end;

procedure TCustomBitBtnLoadGlyphFromStringBase64(Self: TBitBtn; const S: String);
var
  St: TStringStream;
begin
  St := TStringStream.Create(DecodeBase64(S, False));
  try
    TCustomBitBtnLoadGlyphFromStream(Self, St);
  finally
    St.Free;
  end;
end;

procedure RIRegisterTBITBTN(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCUSTOMBITBTN) do
  begin
    RegisterMethod(@TCustomBitBtnLoadGlyphFromFile, 'LoadGlyphFromFile');
    RegisterMethod(@TCustomBitBtnLoadGlyphFromStream, 'LoadGlyphFromStream');
    RegisterMethod(@TCustomBitBtnLoadGlyphFromStringBase64, 'LoadGlyphFromStringBase64');
  end;

  with Cl.Add(TBITBTN) do
  begin

  end;
end;

procedure RIRegister_Buttons(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTSPEEDBUTTON(cl);
  RIRegisterTBITBTN(cl);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)


end.
