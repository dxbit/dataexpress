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

    Based on the original uPSC_Buttons.pas file of the RemObject Pascal Script
    component.
-------------------------------------------------------------------------------}



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
    RegisterMethod('procedure Click');
    RegisterMethod('procedure LoadGlyphFromFile(const FileName: String)');
    RegisterMethod('procedure LoadGlyphFromStream(Stream: TStream)');
    RegisterMethod('procedure LoadGlyphFromStringBase64(const StrBase64: String)');
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
    //RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnPaint', 'TNotifyEvent', iptrw);
    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
    RegisterProperty('Transparent', 'Boolean', iptrw);
  end;
end;

procedure SIRegisterTBITBTN(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomButton'), 'TCustomBitBtn') do
  begin
    RegisterMethod('procedure LoadGlyphFromFile(const FileName: String)');
    RegisterMethod('procedure LoadGlyphFromStream(Stream: TStream)');
    RegisterMethod('procedure LoadGlyphFromStringBase64(const StrBase64: String)');
  end;

  with Cl.AddClassN(cl.FindClass('TCustomBitBtn'), 'TBitBtn') do
  begin
    RegisterProperty('Glyph', 'TBitmap', iptrw);
    RegisterProperty('Kind', 'TBitBtnKind', iptrw);
    RegisterProperty('Layout', 'TButtonLayout', iptrw);
    RegisterProperty('Margin', 'Integer', iptrw);
    //RegisterProperty('NumGlyphs', 'Byte', iptrw);
    //RegisterProperty('Style', 'TButtonStyle', iptrw);
    RegisterProperty('Spacing', 'Integer', iptrw);
    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
    RegisterProperty('ParentFont', 'Boolean', iptrw);
  end;
end;



procedure SIRegister_Buttons_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TButtonLayout', '(blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom)');
  //Cl.AddTypeS('TButtonState', '(bsUp, bsDisabled, bsDown, bsExclusive)');
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




