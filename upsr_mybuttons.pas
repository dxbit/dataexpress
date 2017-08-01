{
This file is a modified version of the file included in the PascalScript package.
Modified the file by Pavel Duborkin (e-mail: 7bit@list.ru, mydataexpress@mail.ru).
The changes made are marked with comments that begin with "// 7bit" and end with "//"
}
unit uPSR_MyButtons;
interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegisterTSPEEDBUTTON(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBITBTN(Cl: TPSRuntimeClassImporter);

procedure RIRegister_Buttons(Cl: TPSRuntimeClassImporter);

implementation
uses
  Classes, Controls, Buttons, BGRABitmap, Graphics;

procedure TSpeedButtonLoadGlyphFromFile(Self: TSpeedButton; const FileName: String);
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(FileName, True);
  try
    Self.OnChangeBounds:=nil;  // Отменяем обработчик, т. к. он не дает сменить значок.
    Self.Glyph := Bmp.Bitmap;
  finally
    Bmp.Free;
  end;
end;

procedure RIRegisterTSPEEDBUTTON(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSPEEDBUTTON) do
  begin
    RegisterMethod(@TSpeedButtonLoadGlyphFromFile, 'LoadGlyphFromFile');
  end;
end;

procedure TBitBtnLoadGlyphFromFile(Self: TBitBtn; const FileName: String);
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

procedure RIRegisterTBITBTN(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TBITBTN) do
  begin
    RegisterMethod(@TBitBtnLoadGlyphFromFile, 'LoadGlyphFromFile');
  end;
end;

procedure RIRegister_Buttons(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTSPEEDBUTTON(cl);
  RIRegisterTBITBTN(cl);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)


end.
