{
This file is a modified version of the file included in the PascalScript package.
Modified the file by Pavel Duborkin (e-mail: 7bit@list.ru, mydataexpress@mail.ru).
The changes made are marked with comments that begin with "// 7bit" and end with "//"
}
{ Compiletime Controls support }
unit uPSC_MyControls;
interface
uses
  uPSCompiler, uPSUtils;

{
  Will register files from:
    Controls
 
  Register the STD, Classes (at least the types&consts) and Graphics libraries first
 
}

procedure SIRegister_Controls_TypesAndConsts(Cl: TPSPascalCompiler);

procedure SIRegisterTSizeConstraints(Cl: TPSPascalCompiler);
procedure SIRegisterTControl(Cl: TPSPascalCompiler);
procedure SIRegisterTWinControl(Cl: TPSPascalCompiler); 
procedure SIRegisterTGraphicControl(cl: TPSPascalCompiler); 
procedure SIRegisterTCustomControl(cl: TPSPascalCompiler); 

procedure SIRegister_Controls(Cl: TPSPascalCompiler);


implementation

procedure SIRegisterTSizeConstraints(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TSizeConstraints') do
  begin
    RegisterProperty('MaxHeight', 'Integer', iptRW);
    RegisterProperty('MaxWidth', 'Integer', iptRW);
    RegisterProperty('MinHeight', 'Integer', iptRW);
    RegisterProperty('MinWidth', 'Integer', iptRW);
  end;
end;

procedure SIRegisterTControl(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TComponent'), 'TControl') do
  begin
    //RegisterMethod('constructor Create(AOwner: TComponent);');
    RegisterMethod('procedure BringToFront;');
    RegisterMethod('procedure Hide;');
    RegisterMethod('procedure Invalidate; virtual;');
    RegisterMethod('procedure Refresh;');
    RegisterMethod('procedure SendToBack;');
    RegisterMethod('procedure Show;');
    RegisterMethod('procedure Update; virtual;');
    RegisterMethod('procedure SetBounds(X,Y,w,h: Integer); virtual;');
    RegisterProperty('Left', 'Integer', iptRW);
    RegisterProperty('Top', 'Integer', iptRW);
    RegisterProperty('Width', 'Integer', iptRW);
    RegisterProperty('Height', 'Integer', iptRW);
    RegisterProperty('Hint', 'string', iptRW);
    RegisterProperty('Align', 'TAlign', iptRW);
    RegisterProperty('ClientHeight', 'LongInt', iptRW);
    RegisterProperty('ClientWidth', 'LongInt', iptRW);
    RegisterProperty('ShowHint', 'Boolean', iptRW);
    RegisterProperty('Visible', 'Boolean', iptRW);
    RegisterProperty('Enabled', 'Boolean', iptrw);
    RegisterProperty('Cursor', 'TCursor', iptrw);
    RegisterProperty('Constraints', 'TSizeConstraints', iptr);

    RegisterMethod('function HasParent: Boolean');
    RegisterMethod('function ClientToScreen(Point: TPoint): TPoint');
    RegisterMethod('function ScreenToClient(Point: TPoint): TPoint');
    RegisterMethod('function GetTopParent: TControl');
  end;
end;

procedure SIRegisterTWinControl(Cl: TPSPascalCompiler); // requires TControl
begin
  with Cl.AddClassN(cl.FindClass('TControl'), 'TWinControl') do
  begin

    with Cl.FindClass('TControl') do
    begin
      RegisterProperty('Parent', 'TWinControl', iptRW);
    end;

    RegisterProperty('Handle', 'LongInt', iptR);
    RegisterProperty('Showing', 'Boolean', iptR);
    RegisterProperty('TabOrder', 'Integer', iptRW);
    RegisterProperty('TabStop', 'Boolean', iptRW);
    RegisterMethod('function CanFocus: Boolean');
    RegisterMethod('function Focused: Boolean');
    RegisterProperty('Controls', 'TControl Integer', iptr);
    RegisterProperty('ControlCount', 'Integer', iptr);

    RegisterMethod('procedure SetFocus; virtual;');
    RegisterMethod('procedure PaintTo(DC: LongInt; X,Y: Integer)');

    RegisterMethod('function ContainsControl(Control: TControl): Boolean');

    RegisterProperty('Brush', 'TBrush', iptr);
  end;
end;
procedure SIRegisterTGraphicControl(cl: TPSPascalCompiler); // requires TControl
begin
  Cl.AddClassN(cl.FindClass('TControl'), 'TGraphicControl');
end;

procedure SIRegisterTCustomControl(cl: TPSPascalCompiler); // requires TWinControl
begin
  Cl.AddClassN(cl.FindClass('TWinControl'), 'TCustomControl');
end;

procedure SIRegister_Controls_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  cl.AddTypeS('TMouseButton', '(mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2)');
  //cl.AddTypeS('TDragMode', '(dmManual, dmAutomatic)');
  //cl.AddTypeS('TDragState', '(dsDragEnter, dsDragLeave, dsDragMove)');
  //cl.AddTypeS('TDragKind', '(dkDrag, dkDock)');
  cl.AddTypeS('TMouseEvent', 'procedure (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);');
  cl.AddTypeS('TMouseMoveEvent', 'procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer);');
  cl.AddTypeS('TKeyEvent', 'procedure (Sender: TObject; var Key: Word; Shift: TShiftState);');
  cl.AddTypeS('TKeyPressEvent', 'procedure(Sender: TObject; var Key: Char);');
  //cl.AddTypeS('TDragOverEvent', 'procedure(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean)');
  //cl.AddTypeS('TDragDropEvent', 'procedure(Sender, Source: TObject; X, Y: Integer)');

  //cl.AddTypeS('TEndDragEvent', 'procedure(Sender, Target: TObject; X, Y: Integer)');

  cl.addTypeS('TAlign', '(alNone, alTop, alBottom, alLeft, alRight, alClient)');

  cl.addTypeS('TAnchorKind', '(akTop, akLeft, akRight, akBottom)');
  cl.addTypeS('TAnchors','set of TAnchorKind');
  cl.AddTypeS('TModalResult', 'Integer');
  cl.AddTypeS('TCursor', 'Integer');
  cl.AddTypeS('TPoint', 'record X,Y: LongInt; end;');

  cl.AddConstantN('mrNone', 'Integer').Value^.ts32 := 0;
  cl.AddConstantN('mrOk', 'Integer').Value^.ts32 := 1;
  cl.AddConstantN('mrCancel', 'Integer').Value^.ts32 := 2;
  cl.AddConstantN('mrAbort', 'Integer').Value^.ts32 := 3;
  cl.AddConstantN('mrRetry', 'Integer').Value^.ts32 := 4;
  cl.AddConstantN('mrIgnore', 'Integer').Value^.ts32 := 5;
  cl.AddConstantN('mrYes', 'Integer').Value^.ts32 := 6;
  cl.AddConstantN('mrNo', 'Integer').Value^.ts32 := 7;
  cl.AddConstantN('mrAll', 'Integer').Value^.ts32 := 8;
  cl.AddConstantN('mrNoToAll', 'Integer').Value^.ts32 := 9;
  cl.AddConstantN('mrYesToAll', 'Integer').Value^.ts32 := 10;
  cl.AddConstantN('mrClose', 'Integer').Value^.ts32 := 11;
  cl.AddConstantN('crDefault', 'Integer').Value^.ts32 := 0;
  cl.AddConstantN('crNone', 'Integer').Value^.ts32 := -1;
  cl.AddConstantN('crArrow', 'Integer').Value^.ts32 := -2;
  cl.AddConstantN('crCross', 'Integer').Value^.ts32 := -3;
  cl.AddConstantN('crIBeam', 'Integer').Value^.ts32 := -4;
  cl.AddConstantN('crSizeNESW', 'Integer').Value^.ts32 := -6;
  cl.AddConstantN('crSizeNS', 'Integer').Value^.ts32 := -7;
  cl.AddConstantN('crSizeNWSE', 'Integer').Value^.ts32 := -8;
  cl.AddConstantN('crSizeWE', 'Integer').Value^.ts32 := -9;
  cl.AddConstantN('crUpArrow', 'Integer').Value^.ts32 := -10;
  cl.AddConstantN('crHourGlass', 'Integer').Value^.ts32 := -11;
  cl.AddConstantN('crDrag', 'Integer').Value^.ts32 := -12;
  cl.AddConstantN('crNoDrop', 'Integer').Value^.ts32 := -13;
  cl.AddConstantN('crHSplit', 'Integer').Value^.ts32 := -14;
  cl.AddConstantN('crVSplit', 'Integer').Value^.ts32 := -15;
  cl.AddConstantN('crMultiDrag', 'Integer').Value^.ts32 := -16;
  cl.AddConstantN('crSQLWait', 'Integer').Value^.ts32 := -17;
  cl.AddConstantN('crNo', 'Integer').Value^.ts32 := -18;
  cl.AddConstantN('crAppStart', 'Integer').Value^.ts32 := -19;
  cl.AddConstantN('crHelp', 'Integer').Value^.ts32 := -20;
  cl.AddConstantN('crHandPoint', 'Integer').Value^.ts32 := -21;
end;

procedure SIRegister_Controls(Cl: TPSPascalCompiler);
begin
  SIRegister_Controls_TypesAndConsts(cl);
  SIRegisterTSizeConstraints(Cl);
  SIRegisterTControl(Cl);
  SIRegisterTWinControl(Cl);
  SIRegisterTGraphicControl(cl);
  SIRegisterTCustomControl(cl);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)

end.
