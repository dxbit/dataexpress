
unit uPSR_MyControls;

interface
uses
  uPSRuntime, uPSUtils, Menus;




procedure RIRegisterTSizeConstraints(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTControl(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTWinControl(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTGraphicControl(cl: TPSRuntimeClassImporter);
procedure RIRegisterTCustomControl(cl: TPSRuntimeClassImporter);

procedure RIRegister_Controls(Cl: TPSRuntimeClassImporter);

implementation

uses
  Classes, Controls, Graphics, myctrls;


procedure RIRegisterTSizeConstraints(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TSizeConstraints);
end;

procedure TControlAlignR(Self: TControl; var T: Byte); begin T := Byte(Self.Align); end;
procedure TControlAlignW(Self: TControl; T: Byte); begin Self.Align:= TAlign(T); end;

procedure TControlClientHeightR(Self: TControl; var T: Longint); begin T := Self.ClientHeight; end;
procedure TControlClientHeightW(Self: TControl; T: Longint); begin Self.ClientHeight := T; end;

procedure TControlClientWidthR(Self: TControl; var T: Longint); begin T := Self.ClientWidth; end;
procedure TControlClientWidthW(Self: TControl; T: Longint); begin Self.ClientWidth:= T; end;

procedure TControlShowHintR(Self: TControl; var T: Boolean); begin T := Self.ShowHint; end;
procedure TControlShowHintW(Self: TControl; T: Boolean); begin Self.ShowHint:= T; end;

procedure TControlVisibleR(Self: TControl; var T: Boolean); begin T := Self.Visible; end;
procedure TControlVisibleW(Self: TControl; T: Boolean); begin Self.Visible:= T; end;

procedure TControlParentR(Self: TControl; var T: TWinControl); begin T := Self.Parent; end;
procedure TControlParentW(Self: TControl; T: TWinControl); begin Self.Parent:= T; end;

procedure TControlConstraintsR(Self: TControl; var T: TSizeConstraints); begin T := Self.Constraints; end;

procedure TCONTROLSHOWHINT_W(Self: TCONTROL; T: BOOLEAN); begin Self.SHOWHINT := T; end;
procedure TCONTROLSHOWHINT_R(Self: TCONTROL; var T: BOOLEAN); begin T := Self.SHOWHINT; end;
procedure TCONTROLENABLED_W(Self: TCONTROL; T: BOOLEAN); begin Self.ENABLED := T; end;
procedure TCONTROLENABLED_R(Self: TCONTROL; var T: BOOLEAN); begin T := Self.ENABLED; end;

procedure TControlClientRectR(Self: TControl; var T: TRect); begin T := Self.ClientRect; end;
procedure TControlBoundsRectR(Self: TControl; var T: TRect); begin T := Self.BoundsRect; end;
procedure TControlBoundsRectW(Self: TControl; T: TRect); begin Self.BoundsRect := T; end;

procedure TControlOnShowHintR(Self: TControl; var T: TControlShowHintEvent); begin T := Self.OnShowHint; end;
procedure TControlOnShowHintW(Self: TControl; T: TControlShowHintEvent); begin Self.OnShowHint := T; end;
procedure TControlOnResizeR(Self: TControl; var T: TNotifyEvent); begin T := Self.OnResize; end;
procedure TControlOnResizeW(Self: TControl; T: TNotifyEvent); begin Self.OnResize := T; end;
procedure TControlOnChangeBoundsR(Self: TControl; var T: TNotifyEvent); begin T := Self.OnChangeBounds; end;
procedure TControlOnChangeBoundsW(Self: TControl; T: TNotifyEvent); begin Self.OnChangeBounds := T; end;
procedure TControlOnClickR(Self: TControl; var T: TNotifyEvent); begin T := Self.OnClick; end;
procedure TControlOnClickW(Self: TControl; T: TNotifyEvent); begin Self.OnClick := T; end;
procedure TControlPopupMenuR(Self: TControl; var T: TPopupMenu); begin T := Self.PopupMenu; end;
procedure TControlPopupMenuW(Self: TControl; T: TPopupMenu); begin Self.PopupMenu:= T; end;
procedure TControlParentFontR(Self: TControl; var T: Boolean); begin T := THackControl(Self).ParentFont; end;
procedure TControlParentFontW(Self: TControl; T: Boolean); begin THackControl(Self).ParentFont := T; end;
procedure TControlParentColorR(Self: TControl; var T: Boolean); begin T := THackControl(Self).ParentColor; end;
procedure TControlParentColorW(Self: TControl; T: Boolean); begin THackControl(Self).ParentColor := T; end;
procedure TControlFontR(Self: TControl; var T: TFont); begin T := Self.Font; end;
procedure TControlFontW(Self: TControl; T: TFont); begin Self.Font:= T; end;
procedure TControlColorR(Self: TControl; var T: TColor); begin T := Self.Color; end;
procedure TControlColorW(Self: TControl; T: TColor); begin Self.Color:= T; end;
procedure TControlCaptionR(Self: TControl; var T: String); begin T := Self.Caption; end;
procedure TControlCaptionW(Self: TControl; T: String); begin Self.Caption:= T; end;
procedure TControlBorderSpacingR(Self: TControl; var T: TControlBorderSpacing); begin T := Self.BorderSpacing; end;
procedure TControlBorderSpacingW(Self: TControl; T: TControlBorderSpacing); begin Self.BorderSpacing:= T; end;
procedure TControlAutoSizeR(Self: TControl; var T: Boolean); begin T := Self.AutoSize; end;
procedure TControlAutoSizeW(Self: TControl; T: Boolean); begin Self.AutoSize:= T; end;
procedure TControlAnchorsR(Self: TControl; var T: TAnchors); begin T := Self.Anchors; end;
procedure TControlAnchorsW(Self: TControl; T: TAnchors); begin Self.Anchors:= T; end;
procedure TControlOnMouseWheelR(Self: TControl; var T: TMouseWheelEvent); begin T := THackControl(Self).OnMouseWheel; end;
procedure TControlOnMouseWheelW(Self: TControl; T: TMouseWheelEvent); begin THackControl(Self).OnMouseWheel := T; end;
procedure TControlOnMouseDownR(Self: TControl; var T: TMouseEvent); begin T := THackControl(Self).OnMouseDown; end;
procedure TControlOnMouseDownW(Self: TControl; T: TMouseEvent); begin THackControl(Self).OnMouseDown := T; end;
procedure TControlOnMouseEnterR(Self: TControl; var T: TNotifyEvent); begin T := THackControl(Self).OnMouseEnter; end;
procedure TControlOnMouseEnterW(Self: TControl; T: TNotifyEvent); begin THackControl(Self).OnMouseEnter := T; end;
procedure TControlOnMouseLeaveR(Self: TControl; var T: TNotifyEvent); begin T := THackControl(Self).OnMouseLeave; end;
procedure TControlOnMouseLeaveW(Self: TControl; T: TNotifyEvent); begin THackControl(Self).OnMouseLeave := T; end;
procedure TControlOnMouseMoveR(Self: TControl; var T: TMouseMoveEvent); begin T := THackControl(Self).OnMouseMove; end;
procedure TControlOnMouseMoveW(Self: TControl; T: TMouseMoveEvent); begin THackControl(Self).OnMouseMove := T; end;
procedure TControlOnMouseUpR(Self: TControl; var T: TMouseEvent); begin T := THackControl(Self).OnMouseUp; end;
procedure TControlOnMouseUpW(Self: TControl; T: TMouseEvent); begin THackControl(Self).OnMouseUp := T; end;
procedure TControlOnDblClickR(Self: TControl; var T: TNotifyEvent); begin T := THackControl(Self).OnDblClick; end;
procedure TControlOnDblClickW(Self: TControl; T: TNotifyEvent); begin THackControl(Self).OnDblClick := T; end;
procedure TControlOnEditingDoneR(Self: TControl; var T: TNotifyEvent); begin T := THackControl(Self).OnEditingDone; end;
procedure TControlOnEditingDoneW(Self: TControl; T: TNotifyEvent); begin THackControl(Self).OnEditingDone := T; end;

procedure RIRegisterTControl(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TControlBorderSpacing) do;

  with Cl.Add(TControl) do
  begin
    //RegisterVirtualConstructor(@TControl.Create, 'Create');
    RegisterMethod(@TControl.BRingToFront, 'BringToFront');
    RegisterMethod(@TControl.Hide, 'Hide');
    RegisterVirtualMethod(@TControl.Invalidate, 'Invalidate');
    RegisterMethod(@TControl.Refresh, 'Refresh');
    RegisterMethod(@TControl.SendToBack, 'SendToBack');
    RegisterMethod(@TControl.Show, 'Show');
    RegisterVirtualMethod(@TControl.Update, 'Update');
    RegisterVirtualMethod(@TControl.SetBounds, 'SetBounds');

    RegisterPropertyHelper(@TControlShowHintR, @TControlShowHintW, 'ShowHint');
    RegisterPropertyHelper(@TControlAlignR, @TControlAlignW, 'Align');
    RegisterPropertyHelper(@TControlClientHeightR, @TControlClientHeightW, 'ClientHeight');
    RegisterPropertyHelper(@TControlClientWidthR, @TControlClientWidthW, 'ClientWidth');
    RegisterPropertyHelper(@TControlVisibleR, @TControlVisibleW, 'Visible');
    RegisterPropertyHelper(@TCONTROLENABLED_R, @TCONTROLENABLED_W, 'Enabled');

    RegisterPropertyHelper(@TControlParentR, @TControlParentW, 'Parent');
    RegisterPropertyHelper(@TControlConstraintsR, nil, 'Constraints');

    RegisterPropertyHelper(@TControlClientRectR, nil, 'ClientRect');
    RegisterPropertyHelper(@TControlBoundsRectR, @TControlBoundsRectW, 'BoundsRect');

    RegisterPropertyHelper(@TControlAnchorsR, @TControlAnchorsW, 'Anchors');
    RegisterPropertyHelper(@TControlAutoSizeR, @TControlAutoSizeW, 'AutoSize');
    RegisterPropertyHelper(@TControlBorderSpacingR, @TControlBorderSpacingW, 'BorderSpacing');
    RegisterPropertyHelper(@TControlCaptionR, @TControlCaptionW, 'Caption');
    RegisterPropertyHelper(@TControlColorR, @TControlColorW, 'Color');
    RegisterPropertyHelper(@TControlFontR, @TControlFontW, 'Font');
    RegisterPropertyHelper(@TControlParentColorR, @TControlParentColorW, 'ParentColor');
    RegisterPropertyHelper(@TControlParentFontR, @TControlParentFontW, 'ParentFont');
    RegisterPropertyHelper(@TControlPopupMenuR, @TControlPopupMenuW, 'PopupMenu');
    RegisterEventPropertyHelper(@TControlOnChangeBoundsR, @TControlOnChangeBoundsW, 'OnChangeBounds');
    RegisterEventPropertyHelper(@TControlOnClickR, @TControlOnClickW, 'OnClick');
    RegisterEventPropertyHelper(@TControlOnResizeR, @TControlOnResizeW, 'OnResize');
    RegisterEventPropertyHelper(@TControlOnShowHintR, @TControlOnShowHintW, 'OnShowHint');
    RegisterEventPropertyHelper(@TControlOnMouseWheelR, @TControlOnMouseWheelW, 'OnMouseWheel');
    RegisterEventPropertyHelper(@TControlOnMouseDownR, @TControlOnMouseDownW, 'OnMouseDown');
    RegisterEventPropertyHelper(@TControlOnMouseEnterR, @TControlOnMouseEnterW, 'OnMouseEnter');
    RegisterEventPropertyHelper(@TControlOnMouseLeaveR, @TControlOnMouseLeaveW, 'OnMouseLeave');
    RegisterEventPropertyHelper(@TControlOnMouseMoveR, @TControlOnMouseMoveW, 'OnMouseMove');
    RegisterEventPropertyHelper(@TControlOnMouseUpR, @TControlOnMouseUpW, 'OnMouseUp');
    RegisterEventPropertyHelper(@TControlOnDblClickR, @TControlOnDblClickW, 'OnDblClick');
    RegisterEventPropertyHelper(@TControlOnEditingDoneR, @TControlOnEditingDoneW, 'OnEditingDone');

    RegisterMethod(@TControl.HasParent, 'HasParent');
    RegisterMethod(@TCONTROL.CLIENTTOSCREEN, 'ClientToScreen');
    RegisterMethod(@TCONTROL.SCREENTOCLIENT, 'ScreenToClient');
    RegisterMethod(@TCONTROL.GetTopParent, 'GetTopParent');
  end;
end;
procedure TWinControlHandleR(Self: TWinControl; var T: Longint); begin T := Self.Handle; end;
procedure TWinControlShowingR(Self: TWinControl; var T: Boolean); begin T := Self.Showing; end;


procedure TWinControlTabOrderR(Self: TWinControl; var T: Longint); begin T := Self.TabOrder; end;
procedure TWinControlTabOrderW(Self: TWinControl; T: Longint); begin Self.TabOrder:= T; end;

procedure TWinControlTabStopR(Self: TWinControl; var T: Boolean); begin T := Self.TabStop; end;
procedure TWinControlTabStopW(Self: TWinControl; T: Boolean); begin Self.TabStop:= T; end;
procedure TWINCONTROLBRUSH_R(Self: TWINCONTROL; var T: TBRUSH); begin T := Self.BRUSH; end;
procedure TWINCONTROLCONTROLS_R(Self: TWINCONTROL; var T: TCONTROL; t1: INTEGER); begin t := Self.CONTROLS[t1]; end;
procedure TWINCONTROLCONTROLCOUNT_R(Self: TWINCONTROL; var T: INTEGER); begin t := Self.CONTROLCOUNT; end;

procedure TWinControlBorderWidthR(Self: TWinControl; var T: Integer); begin T := Self.BorderWidth; end;
procedure TWinControlBorderWidthW(Self: TWinControl; T: Integer); begin Self.BorderWidth := T; end;
procedure TWinControlOnUtf8KeyPressR(Self: TWinControl; var T: TUtf8KeyPressEvent); begin T := Self.OnUtf8KeyPress; end;
procedure TWinControlOnUtf8KeyPressW(Self: TWinControl; T: TUtf8KeyPressEvent); begin Self.OnUtf8KeyPress := T; end;
procedure TWinControlOnKeyUpR(Self: TWinControl; var T: TKeyEvent); begin T := Self.OnKeyUp; end;
procedure TWinControlOnKeyUpW(Self: TWinControl; T: TKeyEvent); begin Self.OnKeyUp := T; end;
procedure TWinControlOnKeyPressR(Self: TWinControl; var T: TKeyPressEvent); begin T := Self.OnKeyPress; end;
procedure TWinControlOnKeyPressW(Self: TWinControl; T: TKeyPressEvent); begin Self.OnKeyPress := T; end;
procedure TWinControlOnKeyDownR(Self: TWinControl; var T: TKeyEvent); begin T := Self.OnKeyDown; end;
procedure TWinControlOnKeyDownW(Self: TWinControl; T: TKeyEvent); begin Self.OnKeyDown := T; end;
procedure TWinControlOnExitR(Self: TWinControl; var T: TNotifyEvent); begin T := Self.OnExit; end;
procedure TWinControlOnExitW(Self: TWinControl; T: TNotifyEvent); begin Self.OnExit := T; end;
procedure TWinControlOnEnterR(Self: TWinControl; var T: TNotifyEvent); begin T := Self.OnEnter; end;
procedure TWinControlOnEnterW(Self: TWinControl; T: TNotifyEvent); begin Self.OnEnter := T; end;

procedure RIRegisterTWinControl(Cl: TPSRuntimeClassImporter); // requires TControl
begin
  with Cl.Add(TWinControl) do
  begin
    RegisterPropertyHelper(@TWinControlHandleR, nil, 'Handle');
    RegisterPropertyHelper(@TWinControlShowingR, nil, 'Showing');
    RegisterPropertyHelper(@TWinControlTabOrderR, @TWinControlTabOrderW, 'TabOrder');
    RegisterPropertyHelper(@TWinControlTabStopR, @TWinControlTabStopW, 'TabStop');
    RegisterMethod(@TWINCONTROL.CANFOCUS, 'CanFocus');
    RegisterMethod(@TWINCONTROL.FOCUSED, 'Focused');
    RegisterPropertyHelper(@TWINCONTROLCONTROLS_R, nil, 'Controls');
    RegisterPropertyHelper(@TWINCONTROLCONTROLCOUNT_R, nil, 'ControlCount');
    RegisterMethod(@TWINCONTROL.PAINTTO, 'PaintTo');
		RegisterVirtualMethod(@TWinControl.SetFocus, 'SetFocus');
		RegisterMethod(@TWINCONTROL.CONTAINSCONTROL, 'ContainsControl');
    RegisterPropertyHelper(@TWINCONTROLBRUSH_R, nil, 'Brush');

    RegisterPropertyHelper(@TWinControlBorderWidthR, @TWinControlBorderWidthW, 'BorderWidth');
    RegisterEventPropertyHelper(@TWinControlOnEnterR, @TWinControlOnEnterW, 'OnEnter');
    RegisterEventPropertyHelper(@TWinControlOnExitR, @TWinControlOnExitW, 'OnExit');
    RegisterEventPropertyHelper(@TWinControlOnKeyDownR, @TWinControlOnKeyDownW, 'OnKeyDown');
    RegisterEventPropertyHelper(@TWinControlOnKeyPressR, @TWinControlOnKeyPressW, 'OnKeyPress');
    RegisterEventPropertyHelper(@TWinControlOnKeyUpR, @TWinControlOnKeyUpW, 'OnKeyUp');
    RegisterEventPropertyHelper(@TWinControlOnUtf8KeyPressR, @TWinControlOnUtf8KeyPressW, 'OnUtf8KeyPress');
  end;
end;

procedure TGraphicControlCanvasR(Self: TGraphicControl; var T: TCanvas); begin T := Self.Canvas; end;

procedure RIRegisterTGraphicControl(cl: TPSRuntimeClassImporter); // requires TControl
begin
  with Cl.Add(TGraphicControl) do
  begin
    RegisterPropertyHelper(@TGraphicControlCanvasR, nil, 'Canvas');
  end;
end;

procedure TCustomControlOnPaintR(Self: TCustomControl; var T: TNotifyEvent); begin T := Self.OnPaint; end;
procedure TCustomControlOnPaintW(Self: TCustomControl; T: TNotifyEvent); begin Self.OnPaint := T; end;
procedure TCustomControlCanvasR(Self: TCustomControl; var T: TCanvas); begin T := Self.Canvas; end;
procedure TCustomControlBorderStyleR(Self: TCustomControl; var T: TBorderStyle); begin T := Self.BorderStyle; end;
procedure TCustomControlBorderStyleW(Self: TCustomControl; T: TBorderStyle); begin Self.BorderStyle := T; end;

procedure RIRegisterTCustomControl(cl: TPSRuntimeClassImporter); // requires TControl
begin
  with Cl.Add(TCustomControl) do
  begin
    RegisterPropertyHelper(@TCustomControlBorderStyleR, @TCustomControlBorderStyleW, 'BorderStyle');
    RegisterPropertyHelper(@TCustomControlCanvasR, nil, 'Canvas');
    RegisterEventPropertyHelper(@TCustomControlOnPaintR, @TCustomControlOnPaintW, 'OnPaint');
  end;
end;

(*----------------------------------------------------------------------------*)


procedure RIRegister_Controls(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTSizeConstraints(Cl);
  RIRegisterTControl(Cl);
  RIRegisterTWinControl(Cl);
  RIRegisterTGraphicControl(cl);
  RIRegisterTCustomControl(cl);

end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)


end.
