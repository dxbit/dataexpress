{ Menus Import Unit }
Unit uPSC_MyMenus;
Interface
Uses uPSCompiler;

procedure SIRegisterTPOPUPMENU(CL: TPSPascalCompiler);
procedure SIRegisterTMAINMENU(CL: TPSPascalCompiler);
procedure SIRegisterTMENU(CL: TPSPascalCompiler);
procedure SIRegisterTMENUITEM(CL: TPSPascalCompiler);
procedure SIRegister_Menus(Cl: TPSPascalCompiler);

implementation

procedure SIRegisterTPOPUPMENU(CL: TPSPascalCompiler);
begin
	With cl.AddClassN(Cl.FindClass('TMenu'),'TPopupMenu') do
	begin
		{cc := Cl.FindClass('TControl');
		if cc <> nil then
			RegisterProperty('PopupMenu', 'TPopupMenu', iptRW);
		with Cl.FindClass('TForm') do
		begin
			RegisterProperty('PopupMenu', 'TPopupMenu', iptRW);
		end;    }
	  RegisterMethod('constructor Create(AOwner: TComponent)');
	  RegisterMethod('procedure PopupXY(X, Y: Integer)');
    RegisterMethod('procedure Popup');
	  RegisterProperty('PopupComponent', 'TComponent', iptrw);
	  RegisterProperty('Alignment', 'TPopupAlignment', iptrw);
	  RegisterProperty('AutoPopup', 'Boolean', iptrw);
    RegisterProperty('TrackButton', 'TTrackButton', iptrw);
    RegisterProperty('OnPopup', 'TNotifyEvent', iptrw);
  end;
end;

procedure SIRegisterTMAINMENU(CL: TPSPascalCompiler);
begin
  With cl.AddClassN(Cl.FindClass('TMenu'),'TMainMenu') do
  begin
  end;
end;

procedure SIRegisterTMENU(CL: TPSPascalCompiler);
begin
  With Cl.FindClass('TMenu') do
  begin
    RegisterMethod('constructor Create(AOwner: TComponent)');
    RegisterProperty('Images', 'TCustomImageList', iptrw);
    RegisterProperty('Handle', 'HMENU', iptr);
    RegisterProperty('Items', 'TMenuItem', iptr);
  end;
end;

procedure SIRegisterTMENUITEM(CL: TPSPascalCompiler);
begin
  cl.AddClassN(Cl.FindClass('TComponent'),'TMenu');

  With cl.AddClassN(Cl.FindClass('TComponent'),'TMenuItem') do
  begin
    RegisterMethod('constructor Create(AOwner: TComponent)');
    RegisterMethod('procedure Insert(Index: Integer; Item: TMenuItem)');
    RegisterMethod('procedure Delete(Index: Integer)');
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure Click');
    RegisterMethod('function Find(const ACaption: string): TMenuItem');
    RegisterMethod('function IndexOf(Item: TMenuItem): Integer');
    RegisterMethod('function GetImageList: TCustomImageList');
    RegisterMethod('function GetParentComponent: TComponent');
    RegisterMethod('function GetParentMenu: TMenu');
    RegisterMethod('function HasParent: Boolean');
    RegisterMethod('procedure Add(Item: TMenuItem)');
    RegisterMethod('procedure Remove(Item: TMenuItem)');
    RegisterProperty('Handle', 'HMENU', iptr);
    RegisterProperty('Count', 'Integer', iptr);
    RegisterProperty('Items', 'TMenuItem Integer', iptr);
    SetDefaultPropery('Items');
    RegisterProperty('MenuIndex', 'Integer', iptrw);
    RegisterProperty('Parent', 'TMenuItem', iptr);

    RegisterProperty('Bitmap', 'TBitmap', iptrw);
    RegisterProperty('Caption', 'string', iptrw);
    RegisterProperty('Checked', 'Boolean', iptrw);
    RegisterProperty('SubMenuImages', 'TCustomImageList', iptrw);
    RegisterProperty('Default', 'Boolean', iptrw);
    RegisterProperty('Enabled', 'Boolean', iptrw);
    RegisterProperty('GroupIndex', 'Byte', iptrw);
    RegisterProperty('Hint', 'string', iptrw);
    RegisterProperty('ImageIndex', 'Integer', iptrw);
    RegisterProperty('RadioItem', 'Boolean', iptrw);
    RegisterProperty('ShortCut', 'TShortCut', iptrw);
    RegisterProperty('Visible', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
  end;
end;

procedure SIRegister_Menus(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('HMENU', 'Cardinal');
  Cl.AddTypeS('HACCEL', 'Cardinal');
  cl.AddTypeS('HWND', 'LongInt');


  Cl.AddTypeS('TShiftStateEnum', '(ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble, ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum, ssScroll, ssTriple, ssQuad, ssExtra1, ssExtra2)');
  //Cl.addTypeS('TEShiftState','(ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble, ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum, ssScroll, ssTriple, ssQuad)');
  Cl.addTypeS('TShiftState','set of TShiftStateEnum');
  //cl.addClassN(cl.FindClass('Exception'),'EMenuError');
 // Cl.addTypeS('TMenuBreak', '(mbNone, mbBreak, mbBarBreak)');

  //Cl.addTypeS('TMenuItemAutoFlag', '(maAutomatic, maManual, maParent)');
  //Cl.AddTypeS('TMenuAutoFlag', 'TMenuItemAutoFlag');
  Cl.addTypeS('TShortCut', 'Word');
  //cl.addClassN(cl.FindClass('TActionLink'),'TMenuActionLink');
  SIRegisterTMENUITEM(Cl);
  //Cl.addTypeS('TMenuChangeEvent', 'procedure (Sender: TObject; Source: TMenuItem; Rebuild: Boolean)');

  Cl.addTypeS('TFindItemKind', '(fkCommand, fkHandle, fkShortCut)');
  SIRegisterTMENU(Cl);
  SIRegisterTMAINMENU(Cl);
  Cl.addTypeS('TPopupAlignment', '(paLeft, paRight, paCenter)');
  Cl.addTypeS('TTrackButton', '(tbRightButton, tbLeftButton)');
  //Cl.addTypeS('TMenuAnimations', '(maLeftToRight, maRightToLeft, maTopToBottom, maBottomToTop, maNone)');

  //Cl.addTypeS('TMenuAnimation', 'set of TMenuAnimations');
  SIRegisterTPOPUPMENU(Cl);
  //Cl.addTypeS('TCMenuItem', 'TMenuItem');

  Cl.AddDelphiFunction('function ShortCut(const Key: Word; const Shift : TShiftState) : TShortCut');
  Cl.AddDelphiFunction('procedure ShortCutToKey(const ShortCut : TShortCut; var Key: Word; var Shift : TShiftState)');

end;

end.
