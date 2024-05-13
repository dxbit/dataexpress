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

    Based on the original uPSR_StdCtrls.pas file of the RemObject Pascal Script
    component.
-------------------------------------------------------------------------------}

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
procedure TCustomEditAlignmentR(Self: TCustomEdit; var T: TAlignment); begin T := Self.Alignment; end;
procedure TCustomEditAlignmentW(Self: TCustomEdit; T: TAlignment); begin Self.Alignment := T; end;
procedure TCustomEditBorderStyleR(Self: TCustomEdit; var T: TBorderStyle); begin T := Self.BorderStyle; end;
procedure TCustomEditBorderStyleW(Self: TCustomEdit; T: TBorderStyle); begin Self.BorderStyle := T; end;
procedure TCustomEditCanUndoR(Self: TCustomEdit; var T: Boolean); begin T := Self.CanUndo; end;
procedure TCustomEditCaretPosR(Self: TCustomEdit; var T: TPoint); begin T := Self.CaretPos; end;
procedure TCustomEditCaretPosW(Self: TCustomEdit; T: TPoint); begin Self.CaretPos := T; end;
procedure TCustomEditCharCaseR(Self: TCustomEdit; var T: TEditCharCase); begin T := Self.CharCase; end;
procedure TCustomEditCharCaseW(Self: TCustomEdit; T: TEditCharCase); begin Self.CharCase := T; end;
procedure TCustomEditEchoModeR(Self: TCustomEdit; var T: TEchoMode); begin T := Self.EchoMode; end;
procedure TCustomEditEchoModeW(Self: TCustomEdit; T: TEchoMode); begin Self.EchoMode := T; end;
procedure TCustomEditHideSelectionR(Self: TCustomEdit; var T: Boolean); begin T := Self.HideSelection; end;
procedure TCustomEditHideSelectionW(Self: TCustomEdit; T: Boolean); begin Self.HideSelection := T; end;
procedure TCustomEditMaxLengthR(Self: TCustomEdit; var T: Integer); begin T := Self.MaxLength; end;
procedure TCustomEditMaxLengthW(Self: TCustomEdit; T: Integer); begin Self.MaxLength := T; end;
procedure TCustomEditNumbersOnlyR(Self: TCustomEdit; var T: Boolean); begin T := Self.NumbersOnly; end;
procedure TCustomEditNumbersOnlyW(Self: TCustomEdit; T: Boolean); begin Self.NumbersOnly := T; end;
procedure TCustomEditOnChangeR(Self: TCustomEdit; var T: TNotifyEvent); begin T := Self.OnChange; end;
procedure TCustomEditOnChangeW(Self: TCustomEdit; T: TNotifyEvent); begin Self.OnChange := T; end;
procedure TCustomEditPasswordCharR(Self: TCustomEdit; var T: Char); begin T := Self.PasswordChar; end;
procedure TCustomEditPasswordCharW(Self: TCustomEdit; T: Char); begin Self.PasswordChar := T; end;
procedure TCustomEditReadOnlyR(Self: TCustomEdit; var T: Boolean); begin T := Self.ReadOnly; end;
procedure TCustomEditReadOnlyW(Self: TCustomEdit; T: Boolean); begin Self.ReadOnly := T; end;
procedure TCustomEditTextR(Self: TCustomEdit; var T: String); begin T := Self.Text; end;
procedure TCustomEditTextW(Self: TCustomEdit; T: String); begin Self.Text := T; end;

{procedure TCUSTOMEDITTEXTHINTFONTCOLOR_R(Self: TCUSTOMEDIT; var T: TColor); begin T := Self.TextHintFontColor; end;
procedure TCUSTOMEDITTEXTHINTFONTCOLOR_W(Self: TCUSTOMEDIT; T: TColor); begin Self.TextHintFontColor := T; end;
procedure TCUSTOMEDITTEXTHINTFONTSTYLE_R(Self: TCUSTOMEDIT; var T: TFontStyles); begin T := Self.TextHintFontStyle; end;
procedure TCUSTOMEDITTEXTHINTFONTSTYLE_W(Self: TCUSTOMEDIT; T: TFontStyles); begin Self.TextHintFontStyle := T; end; }


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
    {RegisterPropertyHelper(@TCUSTOMEDITTEXTHINTFONTCOLOR_R, @TCUSTOMEDITTEXTHINTFONTCOLOR_W, 'TextHintFontColor');
    RegisterPropertyHelper(@TCUSTOMEDITTEXTHINTFONTSTYLE_R, @TCUSTOMEDITTEXTHINTFONTSTYLE_W, 'TextHintFontStyle'); }

    RegisterPropertyHelper(@TCustomEditAlignmentR, @TCustomEditAlignmentW, 'Alignment');
    RegisterPropertyHelper(@TCustomEditBorderStyleR, @TCustomEditBorderStyleW, 'BorderStyle');
    RegisterPropertyHelper(@TCustomEditCanUndoR, nil, 'CanUndo');
    RegisterPropertyHelper(@TCustomEditCaretPosR, @TCustomEditCaretPosW, 'CaretPos');
    RegisterPropertyHelper(@TCustomEditCharCaseR, @TCustomEditCharCaseW, 'CharCase');
    RegisterPropertyHelper(@TCustomEditEchoModeR, @TCustomEditEchoModeW, 'EchoMode');
    RegisterPropertyHelper(@TCustomEditHideSelectionR, @TCustomEditHideSelectionW, 'HideSelection');
    RegisterPropertyHelper(@TCustomEditMaxLengthR, @TCustomEditMaxLengthW, 'MaxLength');
    RegisterPropertyHelper(@TCustomEditNumbersOnlyR, @TCustomEditNumbersOnlyW, 'NumbersOnly');
    RegisterEventPropertyHelper(@TCustomEditOnChangeR, @TCustomEditOnChangeW, 'OnChange');
    RegisterPropertyHelper(@TCustomEditPasswordCharR, @TCustomEditPasswordCharW, 'PasswordChar');
    RegisterPropertyHelper(@TCustomEditReadOnlyR, @TCustomEditReadOnlyW, 'ReadOnly');
    RegisterPropertyHelper(@TCustomEditTextR, @TCustomEditTextW, 'Text');

    RegisterMethod(@TCUSTOMEDIT.COPYTOCLIPBOARD, 'CopyToClipboard');
    RegisterMethod(@TCUSTOMEDIT.CUTTOCLIPBOARD, 'CutToClipboard');
		RegisterMethod(@TCUSTOMEDIT.PASTEFROMCLIPBOARD, 'PasteFromClipboard');
    RegisterMethod(@TCUSTOMEDIT.UNDO, 'Undo');
  end;
end;

procedure RIRegisterTEDIT(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TEDIT);
end;


procedure TCUSTOMMEMOLINES_R(Self: TCUSTOMMEMO; var T: TSTRINGS); begin T := Self.LINES; end;
procedure TCUSTOMMEMOLINES_W(Self: TCUSTOMMEMO; T: TSTRINGS); begin Self.LINES := T; end;
procedure TCustomMemoHorzScrollBarR(Self: TCustomMemo; var T: TMemoScrollBar); begin T := Self.HorzScrollBar; end;
procedure TCustomMemoHorzScrollBarW(Self: TCustomMemo; T: TMemoScrollBar); begin Self.HorzScrollBar := T; end;
procedure TCustomMemoVertScrollBarR(Self: TCustomMemo; var T: TMemoScrollBar); begin T := Self.VertScrollBar; end;
procedure TCustomMemoVertScrollBarW(Self: TCustomMemo; T: TMemoScrollBar); begin Self.VertScrollBar := T; end;
procedure TCustomMemoScrollBarsR(Self: TCustomMemo; var T: TScrollStyle); begin T := Self.ScrollBars; end;
procedure TCustomMemoScrollBarsW(Self: TCustomMemo; T: TScrollStyle); begin Self.ScrollBars := T; end;
procedure TCustomMemoWantReturnsR(Self: TCustomMemo; var T: Boolean); begin T := Self.WantReturns; end;
procedure TCustomMemoWantReturnsW(Self: TCustomMemo; T: Boolean); begin Self.WantReturns := T; end;
procedure TCustomMemoWantTabsR(Self: TCustomMemo; var T: Boolean); begin T := Self.WantTabs; end;
procedure TCustomMemoWantTabsW(Self: TCustomMemo; T: Boolean); begin Self.WantTabs := T; end;
procedure TCustomMemoWordWrapR(Self: TCustomMemo; var T: Boolean); begin T := Self.WordWrap; end;
procedure TCustomMemoWordWrapW(Self: TCustomMemo; T: Boolean); begin Self.WordWrap := T; end;

procedure RIRegisterTCUSTOMMEMO(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCUSTOMMEMO) do
  begin
    RegisterPropertyHelper(@TCUSTOMMEMOLINES_R, @TCUSTOMMEMOLINES_W, 'Lines');

    RegisterPropertyHelper(@TCustomMemoHorzScrollBarR, @TCustomMemoHorzScrollBarW, 'HorzScrollBar');
    RegisterPropertyHelper(@TCustomMemoVertScrollBarR, @TCustomMemoVertScrollBarW, 'VertScrollBar');
    RegisterPropertyHelper(@TCustomMemoScrollBarsR, @TCustomMemoScrollBarsW, 'ScrollBars');
    RegisterPropertyHelper(@TCustomMemoWantReturnsR, @TCustomMemoWantReturnsW, 'WantReturns');
    RegisterPropertyHelper(@TCustomMemoWantTabsR, @TCustomMemoWantTabsW, 'WantTabs');
    RegisterPropertyHelper(@TCustomMemoWordWrapR, @TCustomMemoWordWrapW, 'WordWrap');
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

procedure TCustomComboBoxAutoCompleteR(Self: TCustomComboBox; var T: Boolean); begin T := Self.AutoComplete; end;
procedure TCustomComboBoxAutoCompleteW(Self: TCustomComboBox; T: Boolean); begin Self.AutoComplete := T; end;
procedure TCustomComboBoxAutoCompleteTextR(Self: TCustomComboBox; var T: TComboBoxAutoCompleteText); begin T := Self.AutoCompleteText; end;
procedure TCustomComboBoxAutoCompleteTextW(Self: TCustomComboBox; T: TComboBoxAutoCompleteText); begin Self.AutoCompleteText := T; end;
procedure TCustomComboBoxAutoDropDownR(Self: TCustomComboBox; var T: Boolean); begin T := Self.AutoDropDown; end;
procedure TCustomComboBoxAutoDropDownW(Self: TCustomComboBox; T: Boolean); begin Self.AutoDropDown := T; end;
procedure TCustomComboBoxAutoSelectR(Self: TCustomComboBox; var T: Boolean); begin T := Self.AutoSelect; end;
procedure TCustomComboBoxAutoSelectW(Self: TCustomComboBox; T: Boolean); begin Self.AutoSelect := T; end;
procedure TCustomComboBoxAutoSelectedR(Self: TCustomComboBox; var T: Boolean); begin T := Self.AutoSelected; end;
procedure TCustomComboBoxAutoSelectedW(Self: TCustomComboBox; T: Boolean); begin Self.AutoSelected := T; end;
procedure TCustomComboBoxArrowKeysTraverseListR(Self: TCustomComboBox; var T: Boolean); begin T := Self.ArrowKeysTraverseList; end;
procedure TCustomComboBoxArrowKeysTraverseListW(Self: TCustomComboBox; T: Boolean); begin Self.ArrowKeysTraverseList := T; end;
procedure TCustomComboBoxDropDownCountR(Self: TCustomComboBox; var T: Integer); begin T := Self.DropDownCount; end;
procedure TCustomComboBoxDropDownCountW(Self: TCustomComboBox; T: Integer); begin Self.DropDownCount := T; end;
procedure TCustomComboBoxReadOnlyR(Self: TCustomComboBox; var T: Boolean); begin T := Self.Style = csDropDownList; end;
procedure TCustomComboBoxReadOnlyW(Self: TCustomComboBox; T: Boolean); begin if T then Self.Style := csDropDownList else Self.Style := csDropDown; end;
procedure TCustomComboBoxStyleR(Self: TCustomComboBox; var T: TComboBoxStyle); begin T := Self.Style; end;
procedure TCustomComboBoxStyleW(Self: TCustomComboBox; T: TComboBoxStyle); begin Self.Style := T; end;
procedure TCustomComboBoxTextR(Self: TCustomComboBox; var T: String); begin T := Self.Text; end;
procedure TCustomComboBoxTextW(Self: TCustomComboBox; T: String); begin Self.Text := T; end;

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

    RegisterPropertyHelper(@TCustomComboBoxAutoCompleteR, @TCustomComboBoxAutoCompleteW, 'AutoComplete');
    RegisterPropertyHelper(@TCustomComboBoxAutoCompleteTextR, @TCustomComboBoxAutoCompleteTextW, 'AutoCompleteText');
    RegisterPropertyHelper(@TCustomComboBoxAutoDropDownR, @TCustomComboBoxAutoDropDownW, 'AutoDropDown');
    RegisterPropertyHelper(@TCustomComboBoxAutoSelectR, @TCustomComboBoxAutoSelectW, 'AutoSelect');
    RegisterPropertyHelper(@TCustomComboBoxAutoSelectedR, @TCustomComboBoxAutoSelectedW, 'AutoSelected');
    RegisterPropertyHelper(@TCustomComboBoxArrowKeysTraverseListR, @TCustomComboBoxArrowKeysTraverseListW, 'ArrowKeysTraverseList');
    RegisterPropertyHelper(@TCustomComboBoxDropDownCountR, @TCustomComboBoxDropDownCountW, 'DropDownCount');
    RegisterPropertyHelper(@TCustomComboBoxReadOnlyR, @TCustomComboBoxReadOnlyW, 'ReadOnly');
    RegisterPropertyHelper(@TCustomComboBoxStyleR, @TCustomComboBoxStyleW, 'Style');
    RegisterPropertyHelper(@TCustomComboBoxTextR, @TCustomComboBoxTextW, 'Text');
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
    RegisterMethod(@TCustomButton.Click, 'Click');
    RegisterPropertyHelper(@TCustomButtonCancel_R, @TCustomButtonCancel_W, 'Cancel');
    RegisterPropertyHelper(@TCustomButtonDefault_R, @TCustomButtonDefault_W, 'Default');
    RegisterPropertyHelper(@TCustomButtonModalResult_R, @TCustomButtonModalResult_W, 'ModalResult');
  end;
  Cl.Add(TBUTTON);
end;

procedure TCustomCheckBoxAllowGrayedR(Self: TCustomCheckBox; var T: Boolean); begin T := Self.AllowGrayed; end;
procedure TCustomCheckBoxAllowGrayedW(Self: TCustomCheckBox; T: Boolean); begin Self.AllowGrayed := T; end;
procedure TCustomCheckBoxStateR(Self: TCustomCheckBox; var T: TCheckBoxState); begin T := Self.State; end;
procedure TCustomCheckBoxStateW(Self: TCustomCheckBox; T: TCheckBoxState); begin Self.State := T; end;
procedure TCustomCheckBoxOnChangeR(Self: TCustomCheckBox; var T: TNotifyEvent); begin T := Self.OnChange; end;
procedure TCustomCheckBoxOnChangeW(Self: TCustomCheckBox; T: TNotifyEvent); begin Self.OnChange := T; end;

procedure RIRegisterTCUSTOMCHECKBOX(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCUSTOMCHECKBOX) do
  begin
    RegisterPropertyHelper(@TCustomCheckBoxAllowGrayedR, @TCustomCheckBoxAllowGrayedW, 'AllowGrayed');
    RegisterPropertyHelper(@TCustomCheckBoxStateR, @TCustomCheckBoxStateW, 'State');
    RegisterEventPropertyHelper(@TCustomCheckBoxOnChangeR, @TCustomCheckBoxOnChangeW, 'OnChange');
  end;
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

procedure TCustomListBoxBorderStyleR(Self: TCustomListBox; var T: TBorderStyle); begin T := Self.BorderStyle; end;
procedure TCustomListBoxBorderStyleW(Self: TCustomListBox; T: TBorderStyle); begin Self.BorderStyle := T; end;
procedure TCustomListBoxColumnsR(Self: TCustomListBox; var T: Integer); begin T := Self.Columns; end;
procedure TCustomListBoxColumnsW(Self: TCustomListBox; T: Integer); begin Self.Columns := T; end;
procedure TCustomListBoxExtendedSelectR(Self: TCustomListBox; var T: Boolean); begin T := Self.ExtendedSelect; end;
procedure TCustomListBoxExtendedSelectW(Self: TCustomListBox; T: Boolean); begin Self.ExtendedSelect := T; end;
//procedure TCustomListBoxIntegralHeightR(Self: TCustomListBox; var T: Boolean); begin T := Self.IntegralHeight; end;
//procedure TCustomListBoxIntegralHeightW(Self: TCustomListBox; T: Boolean); begin Self.IntegralHeight := T; end;
procedure TCustomListBoxItemHeightR(Self: TCustomListBox; var T: Integer); begin T := Self.ItemHeight; end;
procedure TCustomListBoxItemHeightW(Self: TCustomListBox; T: Integer); begin Self.ItemHeight := T; end;
procedure TCustomListBoxMultiSelectR(Self: TCustomListBox; var T: Boolean); begin T := Self.MultiSelect; end;
procedure TCustomListBoxMultiSelectW(Self: TCustomListBox; T: Boolean); begin Self.MultiSelect := T; end;
procedure TCustomListBoxSortedR(Self: TCustomListBox; var T: Boolean); begin T := Self.Sorted; end;
procedure TCustomListBoxSortedW(Self: TCustomListBox; T: Boolean); begin Self.Sorted := T; end;
procedure TCustomListBoxStyleR(Self: TCustomListBox; var T: TListBoxStyle); begin T := Self.Style; end;
procedure TCustomListBoxStyleW(Self: TCustomListBox; T: TListBoxStyle); begin Self.Style := T; end;

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

    RegisterPropertyHelper(@TCustomListBoxBorderStyleR, @TCustomListBoxBorderStyleW, 'BorderStyle');
    RegisterPropertyHelper(@TCustomListBoxColumnsR, @TCustomListBoxColumnsW, 'Columns');
    RegisterPropertyHelper(@TCustomListBoxExtendedSelectR, @TCustomListBoxExtendedSelectW, 'ExtendedSelect');
    //RegisterPropertyHelper(@TCustomListBoxIntegralHeightR, @TCustomListBoxIntegralHeightW, 'IntegralHeight');
    RegisterPropertyHelper(@TCustomListBoxItemHeightR, @TCustomListBoxItemHeightW, 'ItemHeight');
    RegisterPropertyHelper(@TCustomListBoxMultiSelectR, @TCustomListBoxMultiSelectW, 'MultiSelect');
    RegisterPropertyHelper(@TCustomListBoxSortedR, @TCustomListBoxSortedW, 'Sorted');
    RegisterPropertyHelper(@TCustomListBoxStyleR, @TCustomListBoxStyleW, 'Style');
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


