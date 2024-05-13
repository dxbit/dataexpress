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

    Based on the original uPSC_StdCtrls.pas file of the RemObject Pascal Script
    component.
-------------------------------------------------------------------------------}

{ Compiletime STDCtrls support }
unit uPSC_MyStdCtrls;

interface
uses
  uPSCompiler, uPSUtils;

{
   Will register files from:
     stdctrls
 
Requires:
  STD, classes, controls and graphics
}

procedure SIRegister_StdCtrls_TypesAndConsts(cl: TPSPascalCompiler);



procedure SIRegisterTCUSTOMGROUPBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTGROUPBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMLABEL(Cl: TPSPascalCompiler);
procedure SIRegisterTLABEL(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMEDIT(Cl: TPSPascalCompiler);
procedure SIRegisterTEDIT(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMMEMO(Cl: TPSPascalCompiler);
procedure SIRegisterTMEMO(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMCOMBOBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTCOMBOBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTBUTTONCONTROL(Cl: TPSPascalCompiler);
procedure SIRegisterTBUTTON(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMCHECKBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTCHECKBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTRADIOBUTTON(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMLISTBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTLISTBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTSCROLLBAR(Cl: TPSPascalCompiler);

procedure SIRegister_StdCtrls(cl: TPSPascalCompiler);


implementation

procedure SIRegisterTCUSTOMGROUPBOX(Cl: TPSPascalCompiler);
begin
  Cl.AddClassN(cl.FindClass('TCustomControl'), 'TCustomGroupBox');
end;


procedure SIRegisterTGROUPBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomGroupBox'), 'TGroupBox') do
  begin
    RegisterProperty('Caption', 'string', iptrw);
    //RegisterProperty('ParentColor', 'Boolean', iptrw);
    //RegisterProperty('ParentFont', 'Boolean', iptrw);
    //RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);

    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw); }
  end;
end;





procedure SIRegisterTCUSTOMLABEL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicControl'), 'TCustomLabel') do
  begin
    RegisterProperty('Canvas', 'TCanvas', iptr);
  end;
end;


procedure SIRegisterTLABEL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomLabel'), 'TLabel') do
  begin
    RegisterProperty('Alignment', 'TAlignment', iptrw);
    //RegisterProperty('AutoSize', 'Boolean', iptrw);
    RegisterProperty('FocusControl', 'TWinControl', iptrw);
    RegisterProperty('Layout', 'TTextLayout', iptrw);
    //RegisterProperty('ParentColor', 'Boolean', iptrw);
    //RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('ShowAccelChar', 'Boolean', iptrw);
    RegisterProperty('Transparent', 'Boolean', iptrw);
    RegisterProperty('WordWrap', 'Boolean', iptrw);
    //RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;







procedure SIRegisterTCUSTOMEDIT(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TCustomEdit') do
  begin
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure ClearSelection');
    RegisterMethod('procedure SelectAll');

    RegisterProperty('Modified', 'Boolean', iptrw);
    RegisterProperty('SelLength', 'Integer', iptrw);
    RegisterProperty('SelStart', 'Integer', iptrw);
    RegisterProperty('SelText', 'string', iptrw);
    RegisterProperty('Text', 'string', iptrw);
    RegisterProperty('TextHint', 'String', iptrw);

    RegisterProperty('Alignment', 'TAlignment', iptRW);
    RegisterProperty('BorderStyle', 'TBorderStyle', iptRW);
    RegisterProperty('CanUndo', 'Boolean', iptR);
    RegisterProperty('CaretPos', 'TPoint', iptRW);
    RegisterProperty('CharCase', 'TEditCharCase', iptRW);
    RegisterProperty('EchoMode', 'TEchoMode', iptRW);
    RegisterProperty('HideSelection', 'Boolean', iptRW);
    RegisterProperty('MaxLength', 'Integer', iptRW);
    RegisterProperty('NumbersOnly', 'Boolean', iptRW);
    RegisterProperty('OnChange', 'TNotifyEvent', iptRW);
    RegisterProperty('PasswordChar', 'Char', iptRW);
    RegisterProperty('ReadOnly', 'Boolean', iptRW);
    //RegisterProperty('Text', 'String', iptRW);

    RegisterMethod('procedure CopyToClipboard');
    RegisterMethod('procedure CutToClipboard');
    RegisterMethod('procedure PasteFromClipboard');
    RegisterMethod('procedure Undo');
  end;
end;




procedure SIRegisterTEDIT(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomEdit'), 'TEdit') do
  begin
    RegisterProperty('AutoSelect', 'Boolean', iptrw);
    RegisterProperty('AutoSize', 'Boolean', iptrw);
    //RegisterProperty('ParentColor', 'Boolean', iptrw);
    //RegisterProperty('ParentFont', 'Boolean', iptrw);
    //RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    //RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    //RegisterProperty('OnEditingDone', 'TNotifyEvent', iptrw);

    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;




procedure SIRegisterTCUSTOMMEMO(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomEdit'), 'TCustomMemo') do
  begin
    RegisterProperty('Lines', 'TStrings', iptrw);

    RegisterProperty('HorzScrollBar', 'TControlScrollBar', iptRW);
    RegisterProperty('VertScrollBar', 'TControlScrollBar', iptRW);
    RegisterProperty('ScrollBars', 'TScrollStyle', iptRW);
    RegisterProperty('WantReturns', 'Boolean', iptRW);
    RegisterProperty('WantTabs', 'Boolean', iptRW);
    RegisterProperty('WordWrap', 'Boolean', iptRW);
  end;
end;


procedure SIRegisterTMEMO(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomMemo'), 'TMemo') do
  begin
    //RegisterProperty('ParentColor', 'Boolean', iptrw);
    //RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    //RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    //RegisterProperty('OnEditingDone', 'TNotifyEvent', iptrw);
    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;





procedure SIRegisterTCUSTOMCOMBOBOX(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TComboBoxAutoCompleteTextOption', '(cbactEnabled, cbactEndOfLineComplete, cbactRetainPrefixCase, cbactSearchCaseSensitive, cbactSearchAscending)');
  Cl.AddTypeS('TComboBoxAutoCompleteText', 'set of TComboBoxAutoCompleteTextOption');
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TCustomComboBox') do
  begin
    RegisterProperty('DroppedDown', 'Boolean', iptrw);
    RegisterProperty('Items', 'TStrings', iptrw);
    RegisterProperty('ItemIndex', 'Integer', iptrw);

    RegisterMethod('procedure Clear');
    RegisterMethod('procedure SelectAll');
    RegisterProperty('Canvas', 'TCanvas', iptr);
    RegisterProperty('SelLength', 'Integer', iptrw);
    RegisterProperty('SelStart', 'Integer', iptrw);
    RegisterProperty('SelText', 'string', iptrw);

    RegisterProperty('AutoComplete', 'Boolean', iptRW);
    RegisterProperty('AutoCompleteText', 'TComboBoxAutoCompleteText', iptRW);
    RegisterProperty('AutoDropDown', 'Boolean', iptRW);
    RegisterProperty('AutoSelect', 'Boolean', iptRW);
    RegisterProperty('AutoSelected', 'Boolean', iptRW);
    RegisterProperty('ArrowKeysTraverseList', 'Boolean', iptRW);
    RegisterProperty('DropDownCount', 'Integer', iptRW);
    RegisterProperty('ReadOnly', 'Boolean', iptRW);
    RegisterProperty('Style', 'TComboBoxStyle', iptRW);
    RegisterProperty('Text', 'String', iptRW);
  end;
end;


procedure SIRegisterTCOMBOBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomComboBox'), 'TComboBox') do
  begin
    RegisterProperty('ItemWidth', 'Integer', iptrw);
    RegisterProperty('ItemHeight', 'Integer', iptrw);
    RegisterProperty('MaxLength', 'Integer', iptrw);
    //RegisterProperty('ParentColor', 'Boolean', iptrw);
    //RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('Sorted', 'Boolean', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    //RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDropDown', 'TNotifyEvent', iptrw);
    RegisterProperty('OnCloseUp', 'TNotifyEvent', iptRW);
    RegisterProperty('OnDrawItem', 'TDrawItemEvent', iptrw);
    RegisterProperty('OnMeasureItem', 'TMeasureItemEvent', iptrw);
    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
    RegisterProperty('OnSelect', 'TNotifyEvent', iptrw);
    //RegisterProperty('OnEditingDone', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDrawItem', 'TDrawItemEvent', iptRW);
  end;
end;



procedure SIRegisterTBUTTONCONTROL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TButtonControl') do
  begin
  end;
end;



procedure SIRegisterTBUTTON(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TButtonControl'),  'TCustomButton') do
  begin
    RegisterMethod('procedure Click');
    RegisterProperty('Cancel', 'Boolean', iptrw);
    RegisterProperty('Default', 'Boolean', iptrw);
    RegisterProperty('ModalResult', 'LongInt', iptrw);
  end;

  with Cl.AddClassN(cl.FindClass('TCustomButton'),  'TButton') do
  begin
    RegisterProperty('ParentFont', 'Boolean', iptrw);

    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;



procedure SIRegisterTCUSTOMCHECKBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TButtonControl'), 'TCustomCheckBox') do
  begin
    RegisterProperty('AllowGrayed', 'Boolean', iptRW);
    RegisterProperty('State', 'TCheckBoxState', iptRW);
    RegisterProperty('OnChange', 'TNotifyEvent', iptRW);
  end;
end;



procedure SIRegisterTCHECKBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomCheckBox'), 'TCheckBox') do
  begin
    RegisterProperty('Checked', 'Boolean', iptrw);
    //RegisterProperty('ParentColor', 'Boolean', iptrw);
    //RegisterProperty('ParentFont', 'Boolean', iptrw);
    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;





procedure SIRegisterTRADIOBUTTON(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomCheckBox'), 'TRadioButton') do
  begin
    RegisterProperty('Checked', 'Boolean', iptrw);
    //RegisterProperty('ParentColor', 'Boolean', iptrw);
    //RegisterProperty('ParentFont', 'Boolean', iptrw);
    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;



procedure SIRegisterTCUSTOMLISTBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TCustomListBox') do
  begin
    RegisterProperty('Items', 'TStrings', iptrw);
    RegisterProperty('ItemIndex', 'Integer', iptrw);
    RegisterProperty('SelCount', 'Integer', iptr);
    RegisterProperty('Selected', 'Boolean Integer', iptrw);

    RegisterProperty('BorderStyle', 'TBorderStyle', iptRW);
    RegisterProperty('Columns', 'Integer', iptRW);
    RegisterProperty('ExtendedSelect', 'Boolean', iptRW);
    //RegisterProperty('IntegralHeight', 'Boolean', iptRW);
    RegisterProperty('ItemHeight', 'Integer', iptRW);
    RegisterProperty('MultiSelect', 'Boolean', iptRW);
    RegisterProperty('Sorted', 'Boolean', iptRW);
    RegisterProperty('Style', 'TListBoxStyle', iptRW);

    RegisterMethod('procedure Clear');
    RegisterMethod('function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer');
    RegisterMethod('function ItemRect(Index: Integer): TRect');
    RegisterProperty('Canvas', 'TCanvas', iptr);
    RegisterProperty('TopIndex', 'Integer', iptrw);
  end;
end;



procedure SIRegisterTLISTBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomListBox'), 'TListBox') do
  begin
    //RegisterProperty('ParentColor', 'Boolean', iptrw);
    //RegisterProperty('ParentFont', 'Boolean', iptrw);
    //RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);

    RegisterProperty('OnDrawItem', 'TDrawItemEvent', iptrw);
    RegisterProperty('OnMeasureItem', 'TMeasureItemEvent', iptrw);
    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;






procedure SIRegisterTSCROLLBAR(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TScrollBar') do
  begin
    RegisterProperty('Kind', 'TScrollBarKind', iptrw);
    RegisterProperty('Max', 'Integer', iptrw);
    RegisterProperty('Min', 'Integer', iptrw);
    RegisterProperty('PageSize', 'Integer', iptrw);
    RegisterProperty('Position', 'Integer', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);

    RegisterProperty('LargeChange', 'TScrollBarInc', iptrw);
    RegisterProperty('SmallChange', 'TScrollBarInc', iptrw);
    RegisterProperty('OnScroll', 'TScrollEvent', iptrw);
  end;
end;



procedure SIRegister_StdCtrls_TypesAndConsts(cl: TPSPascalCompiler);
begin
  Cl.addTypeS('TOwnerDrawStateType', '(odSelected, odGrayed, odDisabled, odChecked, odFocused, odDefault, odHotLight, odInactive, odNoAccel,  odNoFocusRect, odReserved1, odReserved2, odComboBoxEdit, odBackgroundPainted)');
  cl.AddTypeS('TOwnerDrawState', 'set of TOwnerDrawStateType');
  cl.AddTypeS('TEditCharCase', '(ecNormal, ecUpperCase, ecLowerCase)');
  cl.AddTypeS('TEchoMode', '(emNormal, emNone, emPassword)');
  cl.AddTypeS('TScrollStyle', '(ssNone, ssHorizontal, ssVertical, ssBoth)');
  cl.AddTypeS('TComboBoxStyle', '(csDropDown, csSimple, csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable)');
  cl.AddTypeS('TDrawItemEvent', 'procedure(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState)');
  cl.AddTypeS('TMeasureItemEvent', 'procedure(Control: TWinControl; Index: Integer; var Height: Integer)');
  cl.AddTypeS('TCheckBoxState', '(cbUnchecked, cbChecked, cbGrayed)');
  cl.AddTypeS('TListBoxStyle', '(lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable)');
  cl.AddTypeS('TScrollCode', '(scLineUp, scLineDown, scPageUp, scPageDown, scPosition, scTrack, scTop, scBottom, scEndScroll)');
  cl.AddTypeS('TScrollEvent', 'procedure(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer)');
end;


procedure SIRegister_stdctrls(cl: TPSPascalCompiler);
begin
  SIRegister_StdCtrls_TypesAndConsts(cl);
  SIRegisterTCUSTOMGROUPBOX(Cl);
  SIRegisterTGROUPBOX(Cl);
  SIRegisterTCUSTOMLABEL(Cl);
  SIRegisterTLABEL(Cl);
  SIRegisterTCUSTOMEDIT(Cl);
  SIRegisterTEDIT(Cl);
  SIRegisterTCUSTOMMEMO(Cl);
  SIRegisterTMEMO(Cl);
  SIRegisterTCUSTOMCOMBOBOX(Cl);
  SIRegisterTCOMBOBOX(Cl);
  SIRegisterTBUTTONCONTROL(Cl);
  SIRegisterTBUTTON(Cl);
  SIRegisterTCUSTOMCHECKBOX(Cl);
  SIRegisterTCHECKBOX(Cl);
  SIRegisterTRADIOBUTTON(Cl);
  SIRegisterTCUSTOMLISTBOX(Cl);
  SIRegisterTLISTBOX(Cl);
  SIRegisterTSCROLLBAR(Cl);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)


end.





