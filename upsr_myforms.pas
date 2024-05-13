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

    Based on the original uPSR_Forms.pas file of the RemObject Pascal Script
    component.
-------------------------------------------------------------------------------}

unit uPSR_MyForms;

interface
uses
  uPSRuntime, uPSUtils;

procedure RIRegisterTCONTROLSCROLLBAR(Cl: TPSRuntimeClassImporter);
procedure RIRegisterSCROLLINGWINCONTROL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTSCROLLBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFORM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTAPPLICATION(Cl: TPSRuntimeClassImporter);

procedure RIRegister_Forms(Cl: TPSRuntimeClassImporter);

implementation
uses
  sysutils, classes, Controls, Forms, Graphics, LazUtf8;

procedure TCONTROLSCROLLBARKIND_R(Self: TCONTROLSCROLLBAR; var T: TSCROLLBARKIND); begin T := Self.KIND; end;
procedure TCONTROLSCROLLBARSCROLLPOS_R(Self: TCONTROLSCROLLBAR; var T: INTEGER); begin t := Self.SCROLLPOS; end;

procedure RIRegisterTCONTROLSCROLLBAR(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCONTROLSCROLLBAR) do
  begin
    RegisterPropertyHelper(@TCONTROLSCROLLBARKIND_R, nil, 'Kind');
    RegisterPropertyHelper(@TCONTROLSCROLLBARSCROLLPOS_R, nil, 'ScrollPos');
  end;
end;

procedure RIRegisterSCROLLINGWINCONTROL(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TScrollingWinControl) do
  begin
    RegisterMethod(@TScrollingWinControl.ScrollBy, 'ScrollBy');
  end;
end;

procedure RIRegisterTSCROLLBOX(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSCROLLBOX) do
  begin

  end;
end;

procedure TFORMMODALRESULT_W(Self: TFORM; T: TMODALRESULT); begin Self.MODALRESULT := T; end;
procedure TFORMMODALRESULT_R(Self: TFORM; var T: TMODALRESULT); begin T := Self.MODALRESULT; end;
procedure TFORMACTIVE_R(Self: TFORM; var T: BOOLEAN); begin T := Self.ACTIVE; end;
procedure TFORMCANVAS_R(Self: TFORM; var T: TCANVAS); begin T := Self.CANVAS; end;
procedure TFORMCLIENTHANDLE_R(Self: TFORM; var T: Longint); begin T := Self.CLIENTHANDLE; end;

{ Innerfuse Pascal Script Class Import Utility (runtime) }

procedure RIRegisterTFORM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFORM) do
  begin
    RegisterVirtualConstructor(@TFORM.CREATENEW, 'CreateNew');
    RegisterMethod(@TFORM.CLOSE, 'Close');
    RegisterMethod(@TFORM.HIDE, 'Hide');
    RegisterMethod(@TFORM.SHOW, 'Show');
    RegisterVirtualMethod(@TFORM.SHOWMODAL, 'ShowModal');
    RegisterPropertyHelper(@TFORMACTIVE_R, nil, 'Active');

    RegisterMethod(@TFORM.CLOSEQUERY, 'CloseQuery');
    //RegisterMethod(@TFORM.DEFOCUSCONTROL, 'DefocusControl');
    //RegisterMethod(@TFORM.FOCUSCONTROL, 'FocusControl');
    //RegisterMethod(@TFORM.SETFOCUSEDCONTROL, 'SetFocusedControl');
    RegisterPropertyHelper(@TFORMCANVAS_R, nil, 'Canvas');
    RegisterPropertyHelper(@TFORMMODALRESULT_R, @TFORMMODALRESULT_W, 'ModalResult');
  end;
end;


procedure TAPPLICATIONEXENAME_R(Self: TAPPLICATION; var T: STRING); begin T := Self.EXENAME; end;
//procedure TAPPLICATIONHINT_R(Self: TAPPLICATION; var T: STRING); begin T := Self.HINT; end;
//procedure TAPPLICATIONHINT_W(Self: TAPPLICATION; T: STRING); begin Self.HINT := T; end;
procedure TAPPLICATIONHINTCOLOR_R(Self: TAPPLICATION; var T: TCOLOR); begin T := Self.HINTCOLOR; end;
procedure TAPPLICATIONHINTCOLOR_W(Self: TAPPLICATION; T: TCOLOR); begin Self.HINTCOLOR := T; end;
procedure TAPPLICATIONHINTPAUSE_R(Self: TAPPLICATION; var T: INTEGER); begin T := Self.HINTPAUSE; end;
procedure TAPPLICATIONHINTPAUSE_W(Self: TAPPLICATION; T: INTEGER); begin Self.HINTPAUSE := T; end;
procedure TAPPLICATIONHINTSHORTPAUSE_R(Self: TAPPLICATION; var T: INTEGER); begin T := Self.HINTSHORTPAUSE; end;
procedure TAPPLICATIONHINTSHORTPAUSE_W(Self: TAPPLICATION; T: INTEGER); begin Self.HINTSHORTPAUSE := T; end;
procedure TAPPLICATIONHINTHIDEPAUSE_R(Self: TAPPLICATION; var T: INTEGER); begin T := Self.HINTHIDEPAUSE; end;
procedure TAPPLICATIONHINTHIDEPAUSE_W(Self: TAPPLICATION; T: INTEGER); begin Self.HINTHIDEPAUSE := T; end;
procedure TAPPLICATIONMAINFORM_R(Self: TAPPLICATION; var T: TFORM); begin T := Self.MAINFORM; end;
procedure TAPPLICATIONSHOWHINT_R(Self: TAPPLICATION; var T: BOOLEAN); begin T := Self.SHOWHINT; end;
procedure TAPPLICATIONSHOWHINT_W(Self: TAPPLICATION; T: BOOLEAN); begin Self.SHOWHINT := T; end;
procedure TAPPLICATIONTERMINATED_R(Self: TAPPLICATION; var T: BOOLEAN); begin T := Self.TERMINATED; end;
procedure TAPPLICATIONTITLE_R(Self: TAPPLICATION; var T: STRING); begin T := Self.TITLE; end;
procedure TAPPLICATIONTITLE_W(Self: TAPPLICATION; T: STRING); begin Self.TITLE := T; end;


procedure RIRegisterTAPPLICATION(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TAPPLICATION) do
  begin
    //RegisterMethod(@TAPPLICATION.BRINGTOFRONT, 'BringToFront');
    RegisterMethod(@TAPPLICATION.PROCESSMESSAGES, 'ProcessMessages');
    RegisterMethod(@TAPPLICATION.TERMINATE, 'Terminate');
    RegisterPropertyHelper(@TAPPLICATIONEXENAME_R, nil, 'ExeName');
    //RegisterPropertyHelper(@TAPPLICATIONHINT_R, @TAPPLICATIONHINT_W, 'Hint');
    RegisterPropertyHelper(@TAPPLICATIONMAINFORM_R, nil, 'MainForm');
    RegisterPropertyHelper(@TAPPLICATIONSHOWHINT_R, @TAPPLICATIONSHOWHINT_W, 'ShowHint');
    RegisterPropertyHelper(@TAPPLICATIONTERMINATED_R, nil, 'Terminated');
    RegisterPropertyHelper(@TAPPLICATIONTITLE_R, @TAPPLICATIONTITLE_W, 'Title');
    //RegisterMethod(@TAPPLICATION.CANCELHINT, 'CancelHint');

    RegisterMethod(@TAPPLICATION.HIDEHINT, 'HideHint');
    RegisterPropertyHelper(@TAPPLICATIONHINTCOLOR_R, @TAPPLICATIONHINTCOLOR_W, 'HintColor');
    RegisterPropertyHelper(@TAPPLICATIONHINTPAUSE_R, @TAPPLICATIONHINTPAUSE_W, 'HintPause');
    RegisterPropertyHelper(@TAPPLICATIONHINTSHORTPAUSE_R, @TAPPLICATIONHINTSHORTPAUSE_W, 'HintShortPause');
    RegisterPropertyHelper(@TAPPLICATIONHINTHIDEPAUSE_R, @TAPPLICATIONHINTHIDEPAUSE_W, 'HintHidePause');
  end;
end;

procedure RIRegister_Forms(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTCONTROLSCROLLBAR(cl);
  RIRegisterScrollingWinControl(cl);
  RIRegisterTSCROLLBOX(cl);
  RIRegisterTForm(Cl);
  RIRegisterTApplication(Cl);
end;


// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)
// FPC changes by Boguslaw brandys (brandys at o2 _dot_ pl)

end.





