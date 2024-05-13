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

    Based on the original uPSC_Forms.pas file of the RemObject Pascal Script
    component.
-------------------------------------------------------------------------------}

{ Compiletime Forms support }
unit uPSC_MyForms;

interface
uses
  uPSCompiler, uPSUtils;

procedure SIRegister_Forms_TypesAndConsts(Cl: TPSPascalCompiler);


procedure SIRegisterTCONTROLSCROLLBAR(Cl: TPSPascalCompiler);
procedure SIRegisterTSCROLLINGWINCONTROL(Cl: TPSPascalCompiler);
procedure SIRegisterTSCROLLBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTFORM(Cl: TPSPascalCompiler);
procedure SIRegisterTAPPLICATION(Cl: TPSPascalCompiler);

procedure SIRegister_Forms(Cl: TPSPascalCompiler);

implementation

procedure SIRegisterTCONTROLSCROLLBAR(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TControlScrollBar') do
  begin
    RegisterProperty('Kind', 'TScrollBarKind', iptr);
    RegisterProperty('ScrollPos', 'Integer', iptr);
    RegisterProperty('Increment', 'TScrollBarInc', iptrw);
    RegisterProperty('Range', 'Integer', iptrw);
    RegisterProperty('Page', 'TScrollBarInc', iptRW);
    RegisterProperty('Position', 'Integer', iptrw);
    RegisterProperty('Tracking', 'Boolean', iptrw);
    RegisterProperty('Visible', 'Boolean', iptrw);
    RegisterProperty('Smooth', 'Boolean', iptrw);
  end;
end;

procedure SIRegisterTSCROLLINGWINCONTROL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomControl'), 'TScrollingWinControl') do
  begin
    RegisterMethod('procedure ScrollBy(DeltaX, DeltaY: Integer)');
    RegisterProperty('HorzScrollBar', 'TControlScrollBar', iptrw);
    RegisterProperty('VertScrollBar', 'TControlScrollBar', iptrw);
  end;
end;

procedure SIRegisterTSCROLLBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TScrollingWinControl'), 'TScrollBox') do
  begin
    //RegisterProperty('BorderStyle', 'TBorderStyle', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('AutoScroll', 'Boolean', iptrw);
    //RegisterProperty('ParentColor', 'Boolean', iptrw);
    //RegisterProperty('ParentFont', 'Boolean', iptrw);
    //RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);

    RegisterProperty('OnResize', 'TNotifyEvent', iptrw);
    RegisterProperty('PopupMenu', 'TPopupMenu', iptrw);
    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;

procedure SIRegisterTFORM(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TShowInTaskbar', '(stDefault, stAlways, stNever)');
  Cl.AddTypeS('TStringArray', 'array of string');
  Cl.AddTypeS('TDropFilesEvent', 'procedure (Sender: TObject; const FileNames: TStringArray)');

  with Cl.AddClassN(cl.FindClass('TScrollingWinControl'), 'TForm') do
  begin
    RegisterMethod('constructor CreateNew(AOwner: TComponent)');
    RegisterMethod('procedure Close');
    RegisterMethod('procedure Hide');
    RegisterMethod('procedure Show');
    RegisterMethod('function ShowModal: Integer; virtual;');
    RegisterProperty('Active', 'Boolean', iptr);
    RegisterProperty('ActiveControl', 'TWinControl', iptrw);
    RegisterProperty('AllowDropFiles', 'Boolean', iptrw);
    RegisterProperty('BorderIcons', 'TBorderIcons', iptrw);
    RegisterProperty('BorderStyle', 'TFormBorderStyle', iptrw);
    RegisterProperty('AutoScroll', 'Boolean', iptrw);
    RegisterProperty('FormStyle', 'TFormStyle', iptrw);
    RegisterProperty('KeyPreview', 'Boolean', iptrw);
    RegisterProperty('Position', 'TPosition', iptrw);
    RegisterProperty('OnActivate', 'TNotifyEvent', iptrw);
    //RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnClose', 'TCloseEvent', iptrw);
    RegisterProperty('OnCloseQuery', 'TCloseQueryEvent', iptrw);
    RegisterProperty('OnCreate', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDestroy', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDeactivate', 'TNotifyEvent', iptrw);
    RegisterProperty('OnHide', 'TNotifyEvent', iptrw);
    RegisterProperty('OnResize', 'TNotifyEvent', iptrw);
    RegisterProperty('OnShow', 'TNotifyEvent', iptrw);
    RegisterProperty('OnWindowStateChange', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDropFiles', 'TDropFilesEvent', iptrw);

    RegisterMethod('function CloseQuery: Boolean');
    //RegisterMethod('procedure DefocusControl(Control: TWinControl; Removing: Boolean)');
    //RegisterMethod('procedure FocusControl(Control: TWinControl)');
    //RegisterMethod('function SetFocusedControl(Control: TWinControl): Boolean');
    RegisterProperty('AlphaBlend', 'Boolean', iptrw);
    RegisterProperty('AlphaBlendValue', 'Byte', iptrw);
    RegisterProperty('ModalResult', 'LongInt', iptrw);
    RegisterProperty('Icon', 'TIcon', iptrw);
    RegisterProperty('Menu', 'TMainMenu', iptrw);
    RegisterProperty('PixelsPerInch', 'Integer', iptrw);
    RegisterProperty('ShowInTaskBar', 'TShowInTaskbar', iptrw);
    RegisterProperty('WindowState', 'TWindowState', iptrw);
    {RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);}
  end;
end;

procedure SIRegisterTAPPLICATION(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TComponent'), 'TApplication') do
  begin
    RegisterMethod('procedure ProcessMessages');
    RegisterMethod('procedure Terminate');
    RegisterProperty('ExeName', 'NativeString', iptr);
    //RegisterProperty('Hint', 'NativeString', iptrw);
    RegisterProperty('MainForm', 'TForm', iptr);
    RegisterProperty('ShowHint', 'Boolean', iptrw);
    RegisterProperty('Terminated', 'Boolean', iptr);
    RegisterProperty('Title', 'NativeString', iptrw);

    //RegisterMethod('procedure CancelHint');
    RegisterProperty('HintColor', 'TColor', iptrw);
    RegisterProperty('HintPause', 'Integer', iptrw);
    RegisterProperty('HintShortPause', 'Integer', iptrw);
    RegisterProperty('HintHidePause', 'Integer', iptrw);
  end;
end;

procedure SIRegister_Forms_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  cl.AddTypeS('TScrollBarKind', '(sbHorizontal, sbVertical)');
  cl.AddTypeS('TScrollBarInc', 'SmallInt');
  cl.AddTypeS('TWindowState', '(wsNormal, wsMinimized, wsMaximized)');
  cl.AddTypeS('TFormStyle', '(fsNormal, fsMDIChild, fsMDIForm, fsStayOnTop, fsSplash, fsSystemStayOnTop)');
  cl.AddTypeS('TPosition', '(poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly, poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter)');
  //cl.AddTypeS('TPrintScale', '(poNone, poProportional, poPrintToFit)');
  cl.AddTypeS('TCloseAction', '(caNone, caHide, caFree, caMinimize)');
  cl.AddTypeS('TCloseEvent' ,'procedure(Sender: TObject; var Action: TCloseAction)');
  cl.AddTypeS('TCloseQueryEvent' ,'procedure(Sender: TObject; var CanClose: Boolean)');
  cl.AddTypeS('TBorderIcon' ,'(biSystemMenu, biMinimize, biMaximize, biHelp)');
  cl.AddTypeS('TBorderIcons', 'set of TBorderIcon');
end;

procedure SIRegister_Forms(Cl: TPSPascalCompiler);
begin
  SIRegister_Forms_TypesAndConsts(cl);

  SIRegisterTCONTROLSCROLLBAR(cl);
  SIRegisterTScrollingWinControl(cl);
  SIRegisterTSCROLLBOX(cl);
  SIRegisterTForm(Cl);
  SIRegisterTApplication(Cl);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)


end.

