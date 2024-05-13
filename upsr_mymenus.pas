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

    Based on the original uPSR_Menus.pas file of the RemObject Pascal Script
    component.
-------------------------------------------------------------------------------}

Unit uPSR_MyMenus;
Interface
Uses uPSRuntime;

procedure RIRegister_Menus_Routines(S: TPSExec);
procedure RIRegisterTPOPUPMENU(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMAINMENU(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMENU(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMENUITEM(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Menus(CL: TPSRuntimeClassImporter);

implementation

Uses LCLType, SYSUTILS, CLASSES, GRAPHICS, IMGLIST, ACTNLIST, Menus;


procedure TPOPUPMENUONPOPUP_W(Self: TPOPUPMENU; const T: TNOTIFYEVENT);
begin Self.ONPOPUP := T; end;

procedure TPOPUPMENUONPOPUP_R(Self: TPOPUPMENU; var T: TNOTIFYEVENT);
begin T := Self.ONPOPUP; end;

procedure TPOPUPMENUTRACKBUTTON_W(Self: TPOPUPMENU; const T: TTRACKBUTTON);
begin Self.TRACKBUTTON := T; end;

procedure TPOPUPMENUTRACKBUTTON_R(Self: TPOPUPMENU; var T: TTRACKBUTTON);
begin T := Self.TRACKBUTTON; end;


procedure TPOPUPMENUAUTOPOPUP_W(Self: TPOPUPMENU; const T: BOOLEAN);
begin Self.AUTOPOPUP := T; end;

procedure TPOPUPMENUAUTOPOPUP_R(Self: TPOPUPMENU; var T: BOOLEAN);
begin T := Self.AUTOPOPUP; end;

procedure TPOPUPMENUPOPUPCOMPONENT_W(Self: TPOPUPMENU; const T: TCOMPONENT);
begin Self.POPUPCOMPONENT := T; end;

procedure TPOPUPMENUPOPUPCOMPONENT_R(Self: TPOPUPMENU; var T: TCOMPONENT);
begin T := Self.POPUPCOMPONENT; end;

procedure TMENUHANDLE_R(Self: TMENU; var T: HMENU);
begin T := Self.HANDLE; end;




procedure TMENUITEMONCLICK_W(Self: TMENUITEM; const T: TNOTIFYEVENT);
begin Self.ONCLICK := T; end;

procedure TMENUITEMONCLICK_R(Self: TMENUITEM; var T: TNOTIFYEVENT);
begin T := Self.ONCLICK; end;

procedure TMENUITEMVISIBLE_W(Self: TMENUITEM; const T: BOOLEAN);
begin
  Self.VISIBLE := T;
end;

procedure TMENUITEMVISIBLE_R(Self: TMENUITEM; var T: BOOLEAN);
begin T := Self.VISIBLE; end;

procedure TMENUITEMSHORTCUT_W(Self: TMENUITEM; const T: TSHORTCUT);
begin Self.SHORTCUT := T; end;

procedure TMENUITEMSHORTCUT_R(Self: TMENUITEM; var T: TSHORTCUT);
begin T := Self.SHORTCUT; end;

procedure TMENUITEMRADIOITEM_W(Self: TMENUITEM; const T: BOOLEAN);
begin Self.RADIOITEM := T; end;

procedure TMENUITEMRADIOITEM_R(Self: TMENUITEM; var T: BOOLEAN);
begin T := Self.RADIOITEM; end;

procedure TMENUITEMIMAGEINDEX_W(Self: TMENUITEM; const T: TIMAGEINDEX);
begin Self.IMAGEINDEX := T; end;

procedure TMENUITEMIMAGEINDEX_R(Self: TMENUITEM; var T: TIMAGEINDEX);
begin T := Self.IMAGEINDEX; end;

procedure TMENUITEMHINT_W(Self: TMENUITEM; const T: STRING);
begin Self.HINT := T; end;

procedure TMENUITEMHINT_R(Self: TMENUITEM; var T: STRING);
begin T := Self.HINT; end;

procedure TMENUITEMGROUPINDEX_W(Self: TMENUITEM; const T: BYTE);
begin Self.GROUPINDEX := T; end;

procedure TMENUITEMGROUPINDEX_R(Self: TMENUITEM; var T: BYTE);
begin T := Self.GROUPINDEX; end;

procedure TMENUITEMENABLED_W(Self: TMENUITEM; const T: BOOLEAN);
begin Self.ENABLED := T; end;

procedure TMENUITEMENABLED_R(Self: TMENUITEM; var T: BOOLEAN);
begin T := Self.ENABLED; end;

procedure TMENUITEMDEFAULT_W(Self: TMENUITEM; const T: BOOLEAN);
begin Self.DEFAULT := T; end;

procedure TMENUITEMDEFAULT_R(Self: TMENUITEM; var T: BOOLEAN);
begin T := Self.DEFAULT; end;

procedure TMENUITEMSUBMENUIMAGES_W(Self: TMENUITEM; const T: TCUSTOMIMAGELIST);
begin Self.SUBMENUIMAGES := T; end;

procedure TMENUITEMSUBMENUIMAGES_R(Self: TMENUITEM; var T: TCUSTOMIMAGELIST);
begin T := Self.SUBMENUIMAGES; end;

procedure TMENUITEMCHECKED_W(Self: TMENUITEM; const T: BOOLEAN);
begin Self.CHECKED := T; end;

procedure TMENUITEMCHECKED_R(Self: TMENUITEM; var T: BOOLEAN);
begin T := Self.CHECKED; end;

procedure TMENUITEMCAPTION_W(Self: TMENUITEM; const T: STRING);
begin Self.CAPTION := T; end;

procedure TMENUITEMCAPTION_R(Self: TMENUITEM; var T: STRING);
begin T := Self.CAPTION; end;

procedure TMENUITEMBITMAP_W(Self: TMENUITEM; const T: TBITMAP);
begin Self.BITMAP := T; end;

procedure TMENUITEMBITMAP_R(Self: TMENUITEM; var T: TBITMAP);
begin T := Self.BITMAP; end;

{procedure TMENUITEMACTION_W(Self: TMENUITEM; const T: TBASICACTION);
begin Self.ACTION := T; end;  }

procedure TMENUITEMPARENT_R(Self: TMENUITEM; var T: TMENUITEM);
begin T := Self.PARENT; end;

procedure TMENUITEMMENUINDEX_W(Self: TMENUITEM; const T: INTEGER);
begin Self.MENUINDEX := T; end;

procedure TMENUITEMMENUINDEX_R(Self: TMENUITEM; var T: INTEGER);
begin T := Self.MENUINDEX; end;

procedure TMENUITEMITEMS_R(Self: TMENUITEM; var T: TMENUITEM; const t1: INTEGER);
begin T := Self.ITEMS[t1]; end;

procedure TMENUITEMCOUNT_R(Self: TMENUITEM; var T: INTEGER);
begin T := Self.COUNT; end;

procedure TMENUITEMHANDLE_R(Self: TMENUITEM; var T: HMENU);
begin T := Self.HANDLE; end;

procedure RIRegister_Menus_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@SHORTCUT, 'ShortCut', cdRegister);
	S.RegisterDelphiFunction(@SHORTCUTTOKEY, 'ShortCutToKey', cdRegister);
end;

procedure TPopupMenuPopupMenu(Sender: TPopupMenu);
begin
  Sender.PopUp;
end;

procedure TPopupMenuPopupMenuXY(Sender: TPopupMenu; X, Y: Integer);
begin
  Sender.PopUp(X, Y);
end;

procedure RIRegisterTPOPUPMENU(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(TPOPUPMENU) do
  begin
		RegisterConstructor(@TPOPUPMENU.CREATE, 'Create');
		RegisterMethod(@TPOPUPMENUPOPUPMENUXY, 'PopupXY');
    RegisterMethod(@TPOPUPMENUPOPUPMENU, 'Popup');
		RegisterPropertyHelper(@TPOPUPMENUPOPUPCOMPONENT_R,@TPOPUPMENUPOPUPCOMPONENT_W,'PopupComponent');
		RegisterEventPropertyHelper(@TPOPUPMENUONPOPUP_R,@TPOPUPMENUONPOPUP_W,'OnPopup');
	end;
end;

procedure RIRegisterTMAINMENU(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(TMAINMENU) do
	begin
	end;
end;


procedure RIRegisterTMENU(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(TMENU) do
	begin
		RegisterConstructor(@TMENU.CREATE, 'Create');
		RegisterPropertyHelper(@TMENUHANDLE_R,nil,'Handle');
	end;
end;

function TMENUITEMGETIMAGELIST(Self: TMenuItem): TCustomImageList;
begin
  Result := Self.GetImageList;
end;

procedure RIRegisterTMENUITEM(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(TMENUITEM) do
	begin
		RegisterConstructor(@TMENUITEM.CREATE, 'Create');
		RegisterMethod(@TMENUITEM.INSERT, 'Insert');
		RegisterMethod(@TMENUITEM.DELETE, 'Delete');
		RegisterMethod(@TMENUITEM.CLEAR, 'Clear');
		RegisterVirtualMethod(@TMENUITEM.CLICK, 'Click');

    RegisterMethod(@TMENUITEM.FIND, 'Find');
		RegisterMethod(@TMENUITEM.INDEXOF, 'IndexOf');
		RegisterMethod(@TMENUITEMGETIMAGELIST, 'GetImageList');
		RegisterMethod(@TMENUITEM.GETPARENTCOMPONENT, 'GetParentComponent');
		RegisterMethod(@TMENUITEM.GETPARENTMENU, 'GetParentMenu');
		RegisterMethod(@TMENUITEM.HASPARENT, 'HasParent');
		RegisterMethod(@TMENUITEM.ADD, 'Add');
		RegisterMethod(@TMENUITEM.REMOVE, 'Remove');

		RegisterPropertyHelper(@TMENUITEMHANDLE_R,nil,'Handle');
		RegisterPropertyHelper(@TMENUITEMCOUNT_R,nil,'Count');
		RegisterPropertyHelper(@TMENUITEMITEMS_R,nil,'Items');
		RegisterPropertyHelper(@TMENUITEMMENUINDEX_R,@TMENUITEMMENUINDEX_W,'MenuIndex');
		RegisterPropertyHelper(@TMENUITEMPARENT_R,nil,'Parent');
		RegisterPropertyHelper(@TMENUITEMBITMAP_R,@TMENUITEMBITMAP_W,'Bitmap');
		RegisterPropertyHelper(@TMENUITEMCAPTION_R,@TMENUITEMCAPTION_W,'Caption');
		RegisterPropertyHelper(@TMENUITEMCHECKED_R,@TMENUITEMCHECKED_W,'Checked');
		RegisterPropertyHelper(@TMENUITEMSUBMENUIMAGES_R,@TMENUITEMSUBMENUIMAGES_W,'SubMenuImages');
		RegisterPropertyHelper(@TMENUITEMDEFAULT_R,@TMENUITEMDEFAULT_W,'Default');
		RegisterPropertyHelper(@TMENUITEMENABLED_R,@TMENUITEMENABLED_W,'Enabled');
		RegisterPropertyHelper(@TMENUITEMGROUPINDEX_R,@TMENUITEMGROUPINDEX_W,'GroupIndex');
		RegisterPropertyHelper(@TMENUITEMHINT_R,@TMENUITEMHINT_W,'Hint');
		RegisterPropertyHelper(@TMENUITEMIMAGEINDEX_R,@TMENUITEMIMAGEINDEX_W,'ImageIndex');
		RegisterPropertyHelper(@TMENUITEMRADIOITEM_R,@TMENUITEMRADIOITEM_W,'RadioItem');
		RegisterPropertyHelper(@TMENUITEMSHORTCUT_R,@TMENUITEMSHORTCUT_W,'ShortCut');
		RegisterPropertyHelper(@TMENUITEMVISIBLE_R,@TMENUITEMVISIBLE_W,'Visible');
		RegisterEventPropertyHelper(@TMENUITEMONCLICK_R,@TMENUITEMONCLICK_W,'OnClick');
	end;
end;

procedure RIRegister_Menus(CL: TPSRuntimeClassImporter);
begin
	RIRegisterTMENUITEM(Cl);
	RIRegisterTMENU(Cl);
	RIRegisterTPOPUPMENU(Cl);
	RIRegisterTMAINMENU(Cl);
end;

end.
