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

    Based on the original uPSC_ExtCtrls.pas file of the RemObject Pascal Script
    component.
-------------------------------------------------------------------------------}

{ Compiletime Extctrls support }
unit uPSC_MyExtctrls;

interface
uses
  uPSCompiler, uPSUtils;

(*
   Will register files from:
     ExtCtrls
 
Requires:
  STD, classes, controls, graphics {$IFNDEF PS_MINIVCL}, stdctrls {$ENDIF}
*)

procedure SIRegister_ExtCtrls_TypesAndConsts(cl: TPSPascalCompiler);

procedure SIRegisterTSHAPE(Cl: TPSPascalCompiler);
//procedure SIRegisterTIMAGE(Cl: TPSPascalCompiler);
procedure SIRegisterTPAINTBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTBEVEL(Cl: TPSPascalCompiler);
procedure SIRegisterTTIMER(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMPANEL(Cl: TPSPascalCompiler);
procedure SIRegisterTPANEL(Cl: TPSPascalCompiler);
{procedure SIRegisterTPAGE(Cl: TPSPascalCompiler);
procedure SIRegisterTNOTEBOOK(Cl: TPSPascalCompiler);
procedure SIRegisterTHEADER(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMRADIOGROUP(Cl: TPSPascalCompiler);
procedure SIRegisterTRADIOGROUP(Cl: TPSPascalCompiler);}

procedure SIRegister_ExtCtrls(cl: TPSPascalCompiler);

implementation
procedure SIRegisterTSHAPE(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicControl'), 'TShape') do
  begin
    RegisterProperty('Brush', 'TBrush', iptrw);
    RegisterProperty('Pen', 'TPen', iptrw);
    RegisterProperty('Shape', 'TShapeType', iptrw);
    RegisterProperty('OnPaint', 'TNotifyEvent', iptrw);

    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;

{procedure SIRegisterTIMAGE(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicControl'), 'TImage') do
  begin
    RegisterProperty('Canvas', 'TCanvas', iptr);
    //RegisterProperty('AutoSize', 'Boolean', iptrw);
    RegisterProperty('Center', 'Boolean', iptrw);
    RegisterProperty('Picture', 'TPicture', iptrw);
    RegisterProperty('Stretch', 'Boolean', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnPaint', 'TNotifyEvent', iptrw);
    RegisterProperty('OnPictureChanged', 'TNotifyEvent', iptrw);
    RegisterProperty('Proportional', 'Boolean', iptrw);
    RegisterProperty('Transparent', 'Boolean', iptrw);

    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);
  end;
end;  }

procedure SIRegisterTPAINTBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicControl'), 'TPaintBox') do
  begin
    RegisterProperty('Canvas', 'TCanvas', iptr);
    //RegisterProperty('ParentColor', 'Boolean', iptrw);
    //RegisterProperty('ParentFont', 'Boolean', iptrw);
    //RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnPaint', 'TNotifyEvent', iptrw);

    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;

procedure SIRegisterTBEVEL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicControl'), 'TBevel') do
  begin
    RegisterProperty('Shape', 'TBevelShape', iptrw);
    RegisterProperty('Style', 'TBevelStyle', iptrw);
  end;
end;

procedure SIRegisterTTIMER(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TComponent'), 'TTimer') do
  begin
    RegisterProperty('Enabled', 'Boolean', iptrw);
    RegisterProperty('Interval', 'Cardinal', iptrw);
    RegisterProperty('OnTimer', 'TNotifyEvent', iptrw);
  end;
end;

procedure SIRegisterTCUSTOMPANEL(Cl: TPSPascalCompiler);
begin
  Cl.AddClassN(cl.FindClass('TCustomControl'), 'TCustomPanel');
end;

procedure SIRegisterTPANEL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomPanel'), 'TPanel') do
  begin
    RegisterProperty('Alignment', 'TAlignment', iptrw);
    RegisterProperty('BevelInner', 'TPanelBevel', iptrw);
    RegisterProperty('BevelOuter', 'TPanelBevel', iptrw);
    RegisterProperty('BevelWidth', 'TBevelWidth', iptrw);
    RegisterProperty('BorderWidth', 'TBorderWidth', iptrw);
    //RegisterProperty('ParentColor', 'Boolean', iptrw);
    //RegisterProperty('ParentFont', 'Boolean', iptrw);
    //RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnResize', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;

{procedure SIRegisterTPAGE(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomControl'), 'TPage') do
  begin
    RegisterProperty('Caption', 'string', iptrw);
  end;
end;
procedure SIRegisterTNOTEBOOK(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomControl'), 'TNotebook') do
  begin
    RegisterProperty('ActivePage', 'string', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('PageIndex', 'Integer', iptrw);
    RegisterProperty('Pages', 'TStrings', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);
    RegisterProperty('OnPageChanged', 'TNotifyEvent', iptrw);

    RegisterProperty('CTL3D', 'Boolean', iptrw);
    RegisterProperty('DragCursor', 'LongInt', iptrw);
    RegisterProperty('DragMode', 'TDragMode', iptrw);
    RegisterProperty('ParentCtl3D', 'Boolean', iptrw);
    RegisterProperty('ParentShowHint', 'Boolean', iptrw);
    RegisterProperty('PopupMenu', 'TPopupMenu', iptrw);
    RegisterProperty('OnDragDrop', 'TDragDropEvent', iptrw);
    RegisterProperty('OnDragOver', 'TDragOverEvent', iptrw);
    RegisterProperty('OnEndDrag', 'TEndDragEvent', iptrw);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnStartDrag', 'TStartDragEvent', iptrw);
  end;
end;

procedure SIRegisterTHEADER(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomControl'), 'THeader') do
  begin
    RegisterProperty('SectionWidth', 'Integer Integer', iptrw);
    RegisterProperty('AllowResize', 'Boolean', iptrw);
    RegisterProperty('BorderStyle', 'TBorderStyle', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('Sections', 'TStrings', iptrw);
    RegisterProperty('OnSizing', 'TSectionEvent', iptrw);
    RegisterProperty('OnSized', 'TSectionEvent', iptrw);

    RegisterProperty('ParentShowHint', 'Boolean', iptrw);
    RegisterProperty('PopupMenu', 'TPopupMenu', iptrw);
  end;
end;

procedure SIRegisterTCUSTOMRADIOGROUP(Cl: TPSPascalCompiler);
begin
  Cl.AddClassN(cl.FindClass('TCustomGroupBox'), 'TCustomRadioGroup');
end;

procedure SIRegisterTRADIOGROUP(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomRadioGroup'), 'TRadioGroup') do
  begin
    RegisterProperty('Caption', 'string', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Columns', 'Integer', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('ItemIndex', 'Integer', iptrw);
    RegisterProperty('Items', 'TStrings', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);

    RegisterProperty('CTL3D', 'Boolean', iptrw);
    RegisterProperty('DragCursor', 'LongInt', iptrw);
    RegisterProperty('DragMode', 'TDragMode', iptrw);
    RegisterProperty('ParentCtl3D', 'Boolean', iptrw);
    RegisterProperty('ParentShowHint', 'Boolean', iptrw);
    RegisterProperty('PopupMenu', 'TPopupMenu', iptrw);
    RegisterProperty('OnDragDrop', 'TDragDropEvent', iptrw);
    RegisterProperty('OnDragOver', 'TDragOverEvent', iptrw);
    RegisterProperty('OnEndDrag', 'TEndDragEvent', iptrw);
    RegisterProperty('OnStartDrag', 'TStartDragEvent', iptrw);
  end;
end;   }

procedure SIRegister_ExtCtrls_TypesAndConsts(cl: TPSPascalCompiler);
begin
  cl.AddTypeS('TShapeType', '(stRectangle, stSquare, stRoundRect, stRoundSquare, stEllipse, stCircle)');
  cl.AddTypeS('TBevelStyle', '(bsLowered, bsRaised)');
  cl.AddTypeS('TBevelShape', '(bsBox, bsFrame, bsTopLine, bsBottomLine, bsLeftLine, bsRightLine,bsSpacer)');
  cl.AddTypeS('TPanelBevel', '(bvNone, bvLowered, bvRaised,bvSpace)');
  cl.AddTypeS('TBevelWidth', 'LongInt');
  cl.AddTypeS('TBorderWidth', 'LongInt');
  cl.AddTypeS('TSectionEvent', 'procedure(Sender: TObject; ASection, AWidth: Integer)');
end;

procedure SIRegister_ExtCtrls(cl: TPSPascalCompiler);
begin
  SIRegister_ExtCtrls_TypesAndConsts(cl);

  SIRegisterTSHAPE(Cl);
  //SIRegisterTIMAGE(Cl);
  SIRegisterTPAINTBOX(Cl);
  SIRegisterTBEVEL(Cl);
  SIRegisterTTIMER(Cl);
  SIRegisterTCUSTOMPANEL(Cl);
  SIRegisterTPANEL(Cl);
  //SIRegisterTPAGE(Cl);
  //SIRegisterTNOTEBOOK(Cl);
  //SIRegisterTHEADER(Cl);
  //SIRegisterTCUSTOMRADIOGROUP(Cl);
  //SIRegisterTRADIOGROUP(Cl);
end;

end.





