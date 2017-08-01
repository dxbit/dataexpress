{
This file is a modified version of the file included in the PascalScript package.
Modified the file by Pavel Duborkin (e-mail: 7bit@list.ru, mydataexpress@mail.ru).
The changes made are marked with comments that begin with "// 7bit" and end with "//"
}
{ STDCtrls import unit }
unit uPSR_MyStdCtrls;

interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegisterTCUSTOMGROUPBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTGROUPBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCUSTOMLABEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTLABEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCUSTOMEDIT(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTEDIT(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCUSTOMMEMO(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMEMO(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCUSTOMCOMBOBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCOMBOBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBUTTONCONTROL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBUTTON(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCUSTOMCHECKBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCHECKBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTRADIOBUTTON(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCUSTOMLISTBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTLISTBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTSCROLLBAR(Cl: TPSRuntimeClassImporter);

procedure RIRegister_stdctrls(cl: TPSRuntimeClassImporter);

implementation
uses
  sysutils, classes, controls, stdctrls, Graphics, buttons;

procedure RIRegisterTCUSTOMGROUPBOX(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCUSTOMGROUPBOX);
end;


procedure RIRegisterTGROUPBOX(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TGROUPBOX);
end;

procedure TCUSTOMLABELCANVAS_R(Self: TCUSTOMLABEL; var T: TCanvas); begin T := Self.CANVAS; end;

procedure RIRegisterTCUSTOMLABEL(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCUSTOMLABEL) do
  begin
    RegisterPropertyHelper(@TCUSTOMLABELCANVAS_R, nil, 'Canvas');
  end;
end;

procedure RIRegisterTLABEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TLABEL);
end;
procedure TCUSTOMEDITMODIFIED_R(Self: TCUSTOMEDIT; var T: BOOLEAN); begin T := Self.MODIFIED; end;
procedure TCUSTOMEDITMODIFIED_W(Self: TCUSTOMEDIT; T: BOOLEAN); begin Self.MODIFIED := T; end;
procedure TCUSTOMEDITSELLENGTH_R(Self: TCUSTOMEDIT; var T: INTEGER); begin T := Self.SELLENGTH; end;
procedure TCUSTOMEDITSELLENGTH_W(Self: TCUSTOMEDIT; T: INTEGER); begin Self.SELLENGTH := T; end;
procedure TCUSTOMEDITSELSTART_R(Self: TCUSTOMEDIT; var T: INTEGER); begin T := Self.SELSTART; end;
procedure TCUSTOMEDITSELSTART_W(Self: TCUSTOMEDIT; T: INTEGER); begin Self.SELSTART := T; end;
procedure TCUSTOMEDITSELTEXT_R(Self: TCUSTOMEDIT; var T: STRING); begin T := Self.SELTEXT; end;
procedure TCUSTOMEDITSELTEXT_W(Self: TCUSTOMEDIT; T: STRING); begin Self.SELTEXT := T; end;
procedure TCUSTOMEDITTEXT_R(Self: TCUSTOMEDIT; var T: string); begin T := Self.TEXT; end;
procedure TCUSTOMEDITTEXT_W(Self: TCUSTOMEDIT; T: string); begin Self.TEXT := T; end;
procedure TCUSTOMEDITTEXTHINT_R(Self: TCUSTOMEDIT; var T: string); begin T := Self.TextHint; end;
procedure TCUSTOMEDITTEXTHINT_W(Self: TCUSTOMEDIT; T: string); begin Self.TEXTHINT := T; end;
procedure TCUSTOMEDITTEXTHINTFONTCOLOR_R(Self: TCUSTOMEDIT; var T: TColor); begin T := Self.TextHintFontColor; end;
procedure TCUSTOMEDITTEXTHINTFONTCOLOR_W(Self: TCUSTOMEDIT; T: TColor); begin Self.TextHintFontColor := T; end;
procedure TCUSTOMEDITTEXTHINTFONTSTYLE_R(Self: TCUSTOMEDIT; var T: TFontStyles); begin T := Self.TextHintFontStyle; end;
procedure TCUSTOMEDITTEXTHINTFONTSTYLE_W(Self: TCUSTOMEDIT; T: TFontStyles); begin Self.TextHintFontStyle := T; end;


procedure RIRegisterTCUSTOMEDIT(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCUSTOMEDIT) do
  begin
    RegisterMethod(@TCUSTOMEDIT.CLEAR, 'Clear');
    RegisterMethod(@TCUSTOMEDIT.CLEARSELECTION, 'ClearSelection');
    RegisterMethod(@TCUSTOMEDIT.SELECTALL, 'SelectAll');
    RegisterPropertyHelper(@TCUSTOMEDITMODIFIED_R, @TCUSTOMEDITMODIFIED_W, 'Modified');
    RegisterPropertyHelper(@TCUSTOMEDITSELLENGTH_R, @TCUSTOMEDITSELLENGTH_W, 'SelLength');
    RegisterPropertyHelper(@TCUSTOMEDITSELSTART_R, @TCUSTOMEDITSELSTART_W, 'SelStart');
    RegisterPropertyHelper(@TCUSTOMEDITSELTEXT_R, @TCUSTOMEDITSELTEXT_W, 'SelText');
    RegisterPropertyHelper(@TCUSTOMEDITTEXT_R, @TCUSTOMEDITTEXT_W, 'Text');
    RegisterPropertyHelper(@TCUSTOMEDITTEXTHINT_R, @TCUSTOMEDITTEXTHINT_W, 'TextHint');
    RegisterPropertyHelper(@TCUSTOMEDITTEXTHINTFONTCOLOR_R, @TCUSTOMEDITTEXTHINTFONTCOLOR_W, 'TextHintFontColor');
    RegisterPropertyHelper(@TCUSTOMEDITTEXTHINTFONTSTYLE_R, @TCUSTOMEDITTEXTHINTFONTSTYLE_W, 'TextHintFontStyle');

    RegisterMethod(@TCUSTOMEDIT.COPYTOCLIPBOARD, 'CopyToClipboard');
    RegisterMethod(@TCUSTOMEDIT.CUTTOCLIPBOARD, 'CutToClipboard');
		RegisterMethod(@TCUSTOMEDIT.PASTEFROMCLIPBOARD, 'PasteFromClipboard');
  end;
end;

procedure RIRegisterTEDIT(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TEDIT);
end;


procedure TCUSTOMMEMOLINES_R(Self: TCUSTOMMEMO; var T: TSTRINGS); begin T := Self.LINES; end;
procedure TCUSTOMMEMOLINES_W(Self: TCUSTOMMEMO; T: TSTRINGS); begin Self.LINES := T; end;


procedure RIRegisterTCUSTOMMEMO(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCUSTOMMEMO) do
  begin
    RegisterPropertyHelper(@TCUSTOMMEMOLINES_R, @TCUSTOMMEMOLINES_W, 'Lines');
  end;
end;


procedure RIRegisterTMEMO(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TMEMO) do
  begin
  end;
end;


procedure TCUSTOMCOMBOBOXCANVAS_R(Self: TCUSTOMCOMBOBOX; var T: TCANVAS); begin T := Self.CANVAS; end;
procedure TCUSTOMCOMBOBOXDROPPEDDOWN_R(Self: TCUSTOMCOMBOBOX; var T: BOOLEAN); begin T := Self.DROPPEDDOWN; end;
procedure TCUSTOMCOMBOBOXDROPPEDDOWN_W(Self: TCUSTOMCOMBOBOX; T: BOOLEAN); begin Self.DROPPEDDOWN := T; end;
procedure TCUSTOMCOMBOBOXITEMS_R(Self: TCUSTOMCOMBOBOX; var T: TSTRINGS); begin T := Self.ITEMS; end;
procedure TCUSTOMCOMBOBOXITEMS_W(Self: TCUSTOMCOMBOBOX; T: TSTRINGS); begin Self.ITEMS := T; end;
procedure TCUSTOMCOMBOBOXITEMINDEX_R(Self: TCUSTOMCOMBOBOX; var T: INTEGER); begin T := Self.ITEMINDEX; end;
procedure TCUSTOMCOMBOBOXITEMINDEX_W(Self: TCUSTOMCOMBOBOX; T: INTEGER); begin Self.ITEMINDEX := T; end;
procedure TCUSTOMCOMBOBOXSELLENGTH_R(Self: TCUSTOMCOMBOBOX; var T: INTEGER); begin T := Self.SELLENGTH; end;
procedure TCUSTOMCOMBOBOXSELLENGTH_W(Self: TCUSTOMCOMBOBOX; T: INTEGER); begin Self.SELLENGTH := T; end;
procedure TCUSTOMCOMBOBOXSELSTART_R(Self: TCUSTOMCOMBOBOX; var T: INTEGER); begin T := Self.SELSTART; end;
procedure TCUSTOMCOMBOBOXSELSTART_W(Self: TCUSTOMCOMBOBOX; T: INTEGER); begin Self.SELSTART := T; end;
procedure TCUSTOMCOMBOBOXSELTEXT_R(Self: TCUSTOMCOMBOBOX; var T: STRING); begin T := Self.SELTEXT; end;
procedure TCUSTOMCOMBOBOXSELTEXT_W(Self: TCUSTOMCOMBOBOX; T: STRING); begin Self.SELTEXT := T; end;


procedure RIRegisterTCUSTOMCOMBOBOX(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCUSTOMCOMBOBOX) do
  begin
    RegisterPropertyHelper(@TCUSTOMCOMBOBOXDROPPEDDOWN_R, @TCUSTOMCOMBOBOXDROPPEDDOWN_W, 'DroppedDown');
    RegisterPropertyHelper(@TCUSTOMCOMBOBOXITEMS_R, @TCUSTOMCOMBOBOXITEMS_W, 'Items');
    RegisterPropertyHelper(@TCUSTOMCOMBOBOXITEMINDEX_R, @TCUSTOMCOMBOBOXITEMINDEX_W, 'ItemIndex');

    RegisterMethod(@TCUSTOMCOMBOBOX.CLEAR, 'Clear');
    RegisterMethod(@TCUSTOMCOMBOBOX.SELECTALL, 'SelectAll');
    RegisterPropertyHelper(@TCUSTOMCOMBOBOXCANVAS_R, nil, 'Canvas');
    RegisterPropertyHelper(@TCUSTOMCOMBOBOXSELLENGTH_R, @TCUSTOMCOMBOBOXSELLENGTH_W, 'SelLength');
    RegisterPropertyHelper(@TCUSTOMCOMBOBOXSELSTART_R, @TCUSTOMCOMBOBOXSELSTART_W, 'SelStart');
    RegisterPropertyHelper(@TCUSTOMCOMBOBOXSELTEXT_R, @TCUSTOMCOMBOBOXSELTEXT_W, 'SelText');
  end;
end;




procedure RIRegisterTCOMBOBOX(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCOMBOBOX);
end;



procedure RIRegisterTBUTTONCONTROL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TBUTTONCONTROL);
end;


procedure TCustomButtonCancel_R(Self: TCustomButton; var T: Boolean); begin T := Self.Cancel; end;
procedure TCustomButtonCancel_W(Self: TCustomButton; T: Boolean); begin Self.Cancel := T; end;
procedure TCustomButtonDefault_R(Self: TCustomButton; var T: Boolean); begin T := Self.Default; end;
procedure TCustomButtonDefault_W(Self: TCustomButton; T: Boolean); begin Self.Default := T; end;
procedure TCustomButtonModalResult_R(Self: TCustomButton; var T: LongInt); begin T := Self.ModalResult; end;
procedure TCustomButtonModalResult_W(Self: TCustomButton; T: LongInt); begin Self.ModalResult := T; end;

procedure RIRegisterTBUTTON(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomButton) do
  begin
    RegisterPropertyHelper(@TCustomButtonCancel_R, @TCustomButtonCancel_W, 'Cancel');
    RegisterPropertyHelper(@TCustomButtonDefault_R, @TCustomButtonDefault_W, 'Default');
    RegisterPropertyHelper(@TCustomButtonModalResult_R, @TCustomButtonModalResult_W, 'ModalResult');
  end;
  Cl.Add(TBUTTON);
end;




procedure RIRegisterTCUSTOMCHECKBOX(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCUSTOMCHECKBOX);
end;


procedure RIRegisterTCHECKBOX(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCHECKBOX);
end;


procedure RIRegisterTRADIOBUTTON(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TRADIOBUTTON);
end;

procedure TCUSTOMLISTBOXCANVAS_R(Self: TCUSTOMLISTBOX; var T: TCANVAS); begin T := Self.CANVAS; end;
procedure TCUSTOMLISTBOXITEMS_R(Self: TCUSTOMLISTBOX; var T: TSTRINGS); begin T := Self.ITEMS; end;
procedure TCUSTOMLISTBOXITEMS_W(Self: TCUSTOMLISTBOX; T: TSTRINGS); begin Self.ITEMS := T; end;
procedure TCUSTOMLISTBOXITEMINDEX_R(Self: TCUSTOMLISTBOX; var T: INTEGER); begin T := Self.ITEMINDEX; end;
procedure TCUSTOMLISTBOXITEMINDEX_W(Self: TCUSTOMLISTBOX; T: INTEGER); begin Self.ITEMINDEX := T; end;
procedure TCUSTOMLISTBOXSELCOUNT_R(Self: TCUSTOMLISTBOX; var T: INTEGER); begin T := Self.SELCOUNT; end;
procedure TCUSTOMLISTBOXSELECTED_R(Self: TCUSTOMLISTBOX; var T: BOOLEAN; t1: INTEGER); begin T := Self.SELECTED[t1]; end;
procedure TCUSTOMLISTBOXSELECTED_W(Self: TCUSTOMLISTBOX; T: BOOLEAN; t1: INTEGER); begin Self.SELECTED[t1] := T; end;
procedure TCUSTOMLISTBOXTOPINDEX_R(Self: TCUSTOMLISTBOX; var T: INTEGER); begin T := Self.TOPINDEX; end;
procedure TCUSTOMLISTBOXTOPINDEX_W(Self: TCUSTOMLISTBOX; T: INTEGER); begin Self.TOPINDEX := T; end;


procedure RIRegisterTCUSTOMLISTBOX(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCUSTOMLISTBOX) do
  begin
    RegisterPropertyHelper(@TCUSTOMLISTBOXITEMS_R, @TCUSTOMLISTBOXITEMS_W, 'Items');
    RegisterPropertyHelper(@TCUSTOMLISTBOXITEMINDEX_R, @TCUSTOMLISTBOXITEMINDEX_W, 'ItemIndex');
    RegisterPropertyHelper(@TCUSTOMLISTBOXSELCOUNT_R, nil, 'SelCount');
    RegisterPropertyHelper(@TCUSTOMLISTBOXSELECTED_R, @TCUSTOMLISTBOXSELECTED_W, 'Selected');

    RegisterMethod(@TCUSTOMLISTBOX.CLEAR, 'Clear');
    RegisterMethod(@TCUSTOMLISTBOX.ITEMATPOS, 'ItemAtPos');
    RegisterMethod(@TCUSTOMLISTBOX.ITEMRECT, 'ItemRect');
    RegisterPropertyHelper(@TCUSTOMLISTBOXCANVAS_R, nil, 'Canvas');
    RegisterPropertyHelper(@TCUSTOMLISTBOXTOPINDEX_R, @TCUSTOMLISTBOXTOPINDEX_W, 'TopIndex');
  end;
end;


procedure RIRegisterTLISTBOX(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TLISTBOX);
end;


procedure RIRegisterTSCROLLBAR(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSCROLLBAR) do
  begin
  end;
end;


procedure RIRegister_stdctrls(cl: TPSRuntimeClassImporter);
begin
  RIRegisterTCUSTOMGROUPBOX(Cl);
  RIRegisterTGROUPBOX(Cl);
  RIRegisterTCUSTOMLABEL(Cl);
  RIRegisterTLABEL(Cl);
  RIRegisterTCUSTOMEDIT(Cl);
  RIRegisterTEDIT(Cl);
  RIRegisterTCUSTOMMEMO(Cl);
  RIRegisterTMEMO(Cl);
  RIRegisterTCUSTOMCOMBOBOX(Cl);
  RIRegisterTCOMBOBOX(Cl);
  RIRegisterTBUTTONCONTROL(Cl);
  RIRegisterTBUTTON(Cl);
  RIRegisterTCUSTOMCHECKBOX(Cl);
  RIRegisterTCHECKBOX(Cl);
  RIRegisterTRADIOBUTTON(Cl);
  RIRegisterTCUSTOMLISTBOX(Cl);
  RIRegisterTLISTBOX(Cl);
  RIRegisterTSCROLLBAR(Cl);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)

end.


