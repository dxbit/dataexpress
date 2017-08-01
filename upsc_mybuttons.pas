{
This file is a modified version of the file included in the PascalScript package.
Modified the file by Pavel Duborkin (e-mail: 7bit@list.ru, mydataexpress@mail.ru).
The changes made are marked with comments that begin with "// 7bit" and end with "//"
}

{ Compiletime Buttons support }
unit uPSC_MyButtons;
interface
uses
  uPSCompiler, uPSUtils;

{
  Will register files from:
    Buttons
 
  Requires
      STD, classes, controls and graphics and StdCtrls
}
procedure SIRegister_Buttons_TypesAndConsts(Cl: TPSPascalCompiler);

procedure SIRegisterTSPEEDBUTTON(Cl: TPSPascalCompiler);
procedure SIRegisterTBITBTN(Cl: TPSPascalCompiler);

procedure SIRegister_Buttons(Cl: TPSPascalCompiler);

implementation

procedure SIRegisterTSPEEDBUTTON(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicControl'), 'TSpeedButton') do
  begin
    RegisterMethod('procedure LoadGlyphFromFile(const FileName: String)');
    RegisterProperty('AllowAllUp', 'Boolean', iptrw);
    RegisterProperty('GroupIndex', 'Integer', iptrw);
    RegisterProperty('Down', 'Boolean', iptrw);
    RegisterProperty('Flat', 'Boolean', iptrw);
    RegisterProperty('Glyph', 'TBitmap', iptrw);
    RegisterProperty('Layout', 'TButtonLayout', iptrw);
    RegisterProperty('Margin', 'Integer', iptrw);
    //RegisterProperty('NumGlyphs', 'Byte', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('Spacing', 'Integer', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnPaint', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);
    RegisterProperty('Transparent', 'Boolean', iptrw);
  end;
end;

procedure SIRegisterTBITBTN(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomButton'), 'TBitBtn') do
  begin
    RegisterMethod('procedure LoadGlyphFromFile(const FileName: String)');
    RegisterProperty('Glyph', 'TBitmap', iptrw);
    RegisterProperty('Kind', 'TBitBtnKind', iptrw);
    RegisterProperty('Layout', 'TButtonLayout', iptrw);
    RegisterProperty('Margin', 'Integer', iptrw);
    //RegisterProperty('NumGlyphs', 'Byte', iptrw);
    //RegisterProperty('Style', 'TButtonStyle', iptrw);
    RegisterProperty('Spacing', 'Integer', iptrw);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
  end;
end;



procedure SIRegister_Buttons_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TButtonLayout', '(blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom)');
  Cl.AddTypeS('TButtonState', '(bsUp, bsDisabled, bsDown, bsExclusive)');
  //Cl.AddTypeS('TButtonStyle', '(bsAutoDetect, bsWin31, bsNew)');
  Cl.AddTypeS('TBitBtnKind', '(bkCustom, bkOK, bkCancel, bkHelp, bkYes, bkNo, bkClose, bkAbort, bkRetry, bkIgnore, bkAll)');

end;

procedure SIRegister_Buttons(Cl: TPSPascalCompiler);
begin
  SIRegister_Buttons_TypesAndConsts(cl);
  SIRegisterTSPEEDBUTTON(cl);
  SIRegisterTBITBTN(cl);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)


end.




