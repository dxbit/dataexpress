{
Copyright © 2015-2017 Pavel Duborkin
Author: Pavel Duborkin
E-Mail: 7bit@list.ru, mydataexpress@mail.ru

This file is part of DataExpress.

DataExpress is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

DataExpress is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with DataExpress.  If not, see <http://www.gnu.org/licenses/>.
}
unit RunDecls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSRuntime, DB, DBCtrls, dxctrls, DBCtrlsEx, Buttons,
  Grids, DBGrids, ComCtrls, ImgList, Controls, timeedit, dximages, dxfiles,
  dxreports, Menus, Graphics, StdCtrls, LazUtf8, LazFileUtils, listform,
  ButtonPanel, formview, editform, Forms, GraphType, Dialogs, PrintersDlgs,
  KGrids, myclasses, mytypes;


procedure RIRegister_Clipboard(Cl: TPSRuntimeClassImporter);
procedure RIRegister_IniFiles(Cl: TPSRuntimeClassImporter);
procedure RIRegister_ReportWindow(Cl: TPSRuntimeClassImporter);
procedure RIRegister_More(Cl: TPSRuntimeClassImporter);
procedure RIRegister_KGrid(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxSQLQuery(Cl: TPSRuntimeClassImporter);
//procedure RIRegister_dxQuery(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxQueryGrid(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxFile(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxDBImage(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxButton(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxCounter(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxTimeEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBTimeEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxObjectField(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxShape(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxPageControl(Cl: TPSRuntimeClassImporter);
procedure RIRegister_PageControl(Cl: TPSRuntimeClassImporter);
procedure RIRegister_ImageList(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxGroupBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxGrid(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBGrid(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxImage(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxLookupComboBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxComboBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBComboBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_CustomDBComboBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxCheckBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBCheckBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxMemo(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBMemo(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxDateEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBDateEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxCalcEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBCalcEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_CustomDBEditButton(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_MaskEdit(Cl: TPSRuntimeClassImporter);
//procedure RIRegister_CustomMaskEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxLabel(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxForm(Cl: TPSRuntimeClassImporter);
procedure RIRegister_StdCtrlsEx(Cl: TPSRuntimeClassImporter);
procedure RIRegister_ControlsEx(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Dialogs(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxCtrls(Cl: TPSRuntimeClassImporter);
procedure RIRegister_TreeView(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Functions(Exec: TPSExec);

implementation

uses
  ScriptFuncs, maskedit, mainform, ExtCtrls, Math, exprfuncs, DateUtils,
  apputils, pivotgrid, reportwindow, IniFiles, BGRABitmap, Clipbrd, MyCtrls,
  DxSQLQuery, HMAC, Variants, Registry;


procedure TIniFileFileName_R(Self: TIniFile; var T: String); begin T := Self.FileName; end;
procedure TIniFileEscapeLineFeeds_R(Self: TIniFile; var T: Boolean); begin T := Self.EscapeLineFeeds; end;
procedure TIniFileCaseSensitive_R(Self: TIniFile; var T: Boolean); begin T := Self.CaseSensitive; end;
procedure TIniFileCaseSensitive_W(Self: TIniFile; T: Boolean); begin Self.CaseSensitive := T; end;
procedure TIniFileStripQuotes_R(Self: TIniFile; var T: Boolean); begin T := Self.StripQuotes; end;
procedure TIniFileStripQuotes_W(Self: TIniFile; T: Boolean); begin Self.StripQuotes := T; end;
procedure TIniFileCacheUpdates_R(Self: TIniFile; var T: Boolean); begin T := Self.CacheUpdates; end;
procedure TIniFileCacheUpdates_W(Self: TIniFile; T: Boolean); begin Self.CacheUpdates := T; end;

function TIniFileCreate(Self: TClass; CreateNewInstance: Boolean; FileName: string; AEscapeLineFeeds: Boolean): TObject;
begin
  Result := TIniFileEx.Create(FileName, AEscapeLineFeeds);
end;

function TClipboardHasText(Self: TClipboard): Boolean; begin Result := Self.HasFormat(CF_Text); end;
function TClipboardHasBitmap(Self: TClipboard): Boolean; begin Result := Self.HasFormat(CF_Bitmap); end;
procedure TClipboardAsText_R(Self: TClipboard; var T: String); begin T := Self.AsText; end;
procedure TClipboardAsText_W(Self: TClipboard; T: String); begin Self.AsText := T; end;

procedure RIRegister_Clipboard(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TClipboard) do
  begin
    RegisterConstructor(@TClipboard.Create, 'Create');
    RegisterMethod(@TClipboard.Close, 'Close');
    RegisterMethod(@TClipboard.Clear, 'Clear');
    RegisterMethod(@TClipboardHasText, 'HasText');
    RegisterMethod(@TClipboardHasBitmap, 'HasBitmap');
    RegisterMethod(@TClipboard.Open, 'Open');
    RegisterPropertyHelper(@TClipboardAsText_R, @TClipboardAsText_W, 'AsText');
  end;
end;

procedure RIRegister_IniFiles(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TIniFile) do
  begin
    RegisterConstructor(@TIniFileCreate, 'Create');
    RegisterMethod(@TIniFile.SectionExists, 'SectionExists');
    RegisterMethod(@TIniFile.ReadString, 'ReadString');
    RegisterMethod(@TIniFile.WriteString, 'WriteString');
    RegisterMethod(@TIniFile.ReadInteger, 'ReadInteger');
    RegisterMethod(@TIniFile.WriteInteger, 'WriteInteger');
    RegisterMethod(@TIniFile.ReadInt64, 'ReadInt64');
    RegisterMethod(@TIniFile.WriteInt64, 'WriteInt64');
    RegisterMethod(@TIniFile.ReadBool, 'ReadBool');
    RegisterMethod(@TIniFile.WriteBool, 'WriteBool');
    RegisterMethod(@TIniFile.ReadDate, 'ReadDate');
    RegisterMethod(@TIniFile.ReadDateTime, 'ReadDateTime');
    RegisterMethod(@TIniFile.ReadFloat, 'ReadFloat');
    RegisterMethod(@TIniFile.ReadTime, 'ReadTime');
    RegisterMethod(@TIniFile.ReadBinaryStream, 'ReadBinaryStream');
    RegisterMethod(@TIniFile.WriteDate, 'WriteDate');
    RegisterMethod(@TIniFile.WriteDateTime, 'WriteDateTime');
    RegisterMethod(@TIniFile.WriteFloat, 'WriteFloat');
    RegisterMethod(@TIniFile.WriteTime, 'WriteTime');
    RegisterMethod(@TIniFile.WriteBinaryStream, 'WriteBinaryStream');
    RegisterMethod(@TIniFile.ReadSection, 'ReadSection');
    RegisterMethod(@TIniFile.ReadSections, 'ReadSections');
    RegisterMethod(@TIniFile.ReadSectionValues, 'ReadSectionValues');
    RegisterMethod(@TIniFile.EraseSection, 'EraseSection');
    RegisterMethod(@TIniFile.DeleteKey, 'DeleteKey');
    RegisterMethod(@TIniFile.UpdateFile, 'UpdateFile');
    RegisterMethod(@TIniFile.ValueExists, 'ValueExists');
    RegisterPropertyHelper(@TIniFileFileName_R, nil, 'FileName');
    RegisterPropertyHelper(@TIniFileEscapeLineFeeds_R, nil, 'EscapeLineFeeds');
    RegisterPropertyHelper(@TIniFileCaseSensitive_R, @TIniFileCaseSensitive_W, 'CaseSensitive');
    RegisterPropertyHelper(@TIniFileStripQuotes_R, @TIniFileStripQuotes_W, 'StripQuotes');

    RegisterMethod(@TIniFile.ReadSectionRaw, 'ReadSectionRaw');
    RegisterPropertyHelper(@TIniFileCacheUpdates_R, @TIniFileCacheUpdates_W, 'CacheUpdates');
  end;
end;

procedure TReportWindowToolbar_R(Self: TReportWindow; var T: TToolBar); begin T := Self.ToolBar; end;
procedure TReportWindowQGrid_R(Self: TReportWindow; var T: TdxQueryGrid); begin T := Self.QGrid; end;
procedure TReportWindowStatusBar_R(Self: TReportWindow; var T: TStatusBar); begin T := Self.StatusBar; end;
procedure TReportWindowFilterSplitter_R(Self: TReportWindow; var T: TSplitter); begin T := Self.FilterSplitter; end;
procedure TReportWindowFilter_R(Self: TReportWindow; var T: TCustomControl); begin T := Self.Filter; end;

procedure RIRegister_ReportWindow(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TReportWindow) do
  begin
    RegisterConstructor(@TReportWindow.CreateWindow, 'CreateWindow');
    RegisterMethod(@TReportWindow.ShowReport, 'ShowReport');
    RegisterPropertyHelper(@TReportWindowToolbar_R, nil, 'ToolBar');
    RegisterPropertyHelper(@TReportWindowQGrid_R, nil, 'QGrid');
    RegisterPropertyHelper(@TReportWindowStatusBar_R, nil, 'StatusBar');
    RegisterPropertyHelper(@TReportWindowFilterSplitter_R, nil, 'FilterSplitter');
    RegisterPropertyHelper(@TReportWindowFilter_R, nil, 'Filter');
  end;
end;

procedure TStatusBarCanvas_R(Self: TStatusBar; var T: TCanvas); begin T := Self.Canvas; end;

procedure TStatusPanelsItems_R(Self: TStatusPanels; var T: TStatusPanel; I: Integer); begin T := Self.Items[I]; end;
procedure TStatusPanelsItems_W(Self: TStatusPanels; T: TStatusPanel; I: Integer); begin Self.Items[I] := T; end;
procedure TStatusPanelsStatusBar_R(Self: TStatusPanels; var T: TStatusBar); begin T := Self.StatusBar; end;

procedure TFormViewGrid_R(Self: TFormView; var T: TdxGrid); begin T := Self.Grid; end;
procedure TFormViewForm_R(Self: TFormView; var T: TdxForm); begin T := Self.Form; end;
procedure TFormViewTree_R(Self: TFormView; var T: TTreeView); begin T := Self.Tree; end;
procedure TFormViewScrollBox_R(Self: TFormView; var T: TScrollBox); begin T := Self.ScrollBox; end;
procedure TFormViewTreeSplitter_R(Self: TFormView; var T: TSplitter); begin T := Self.TreeSplitter; end;
procedure TFormViewFormSplitter_R(Self: TFormView; var T: TSplitter); begin T := Self.FormSplitter; end;

procedure TToolBarButtonCount_R(Self: TToolBar; var T: Integer); begin T := Self.ButtonCount; end;
procedure TToolBarButtons_R(Self: TToolBar; var T: TToolButton; I: Integer); begin T := Self.Buttons[I]; end;
procedure TToolBarRowCount_R(Self: TToolBar; var T: Integer); begin T := Self.RowCount; end;

procedure TParamListValues_R(Self: TParamList; var T: Variant; I: String); begin T := Self.Values[I]; end;
procedure TParamListValues_W(Self: TParamList; T: Variant; I: String); begin Self.Values[I] := T; end;
procedure TParamListObjects_R(Self: TParamList; var T: TObject; I: String); begin T := Self.Objects[I]; end;
procedure TParamListObjects_W(Self: TParamList; T: TObject; I: String); begin Self.Objects[I] := T; end;
procedure TParamListNames_R(Self: TParamList; var T: String; I: Integer); begin T := Self.Names[I]; end;
procedure TParamListValueFromIndex_R(Self: TParamList; var T: Variant; I: Integer); begin T := Self.ValueFromIndex[I]; end;
procedure TParamListValueFromIndex_W(Self: TParamList; T: Variant; I: Integer); begin Self.ValueFromIndex[I] := T; end;
procedure TParamListObjectFromIndex_R(Self: TParamList; var T: TObject; I: Integer); begin T := Self.ObjectFromIndex[I]; end;
procedure TParamListObjectFromIndex_W(Self: TParamList; T: TObject; I: Integer); begin Self.ObjectFromIndex[I] := T; end;
procedure TParamListCount_R(Self: TParamList; var T: Integer); begin T := Self.Count; end;
procedure TParamListOnGetParam_R(Self: TParamList; var T: TParamNotifyEvent); begin T := Self.OnGetParam; end;
procedure TParamListOnGetParam_W(Self: TParamList; T: TParamNotifyEvent); begin Self.OnGetParam := T; end;
procedure TParamListOnSetParam_R(Self: TParamList; var T: TParamNotifyEvent); begin T := Self.OnSetParam; end;
procedure TParamListOnSetParam_W(Self: TParamList; T: TParamNotifyEvent); begin Self.OnSetParam := T; end;

procedure TWindowParams(Self: TWindow; var T: TParamList); begin T := Self.Params; end;

procedure TListWindowButtons_R(Self: TListWindow; var T: TButtonPanel); begin T := Self.Buttons; end;
procedure TListWindowFormView_R(Self: TListWindow; var T: TFormView); begin T := TFormView(Self.FormView); end;
procedure TListWindowToolbar_R(Self: TListWindow; var T: TToolbar); begin T := Self.Toolbar; end;

procedure TEditWindowForm_R(Self: TEditWindow; var T: TdxForm); begin T := Self.Form; end;
procedure TEditWindowScrollBox_R(Self: TEditWindow; var T: TScrollBox); begin T := Self.ScrollBox; end;
procedure TEditWindowButtons_R(Self: TEditWindow; var T: TButtonPanel); begin T := Self.Buttons; end;

procedure TMainFmFormViews_R(Self: TMainFm; var T: TFormView; I: Integer); begin T := Self.FormViews[I]; end;
procedure TMainFmPages_R(Self: TMainFm; var T: TPageControl); begin T := Self.Pages; end;
procedure TMainFmToolbar_R(Self: TMainFm; var T: TToolbar); begin T := Self.Toolbar; end;
procedure TMainFmStatusBar_R(Self: TMainFm; var T: TStatusBar); begin T := Self.StatusBar; end;
procedure TMainFmParams_R(Self: TMainFm; var T: TParamList); begin T := Self.Params; end;
procedure TMainFmOnCreateForm_R(Self: TMainFm; var T: TCreateFormEvent); begin T := Self.OnCreateForm; end;
procedure TMainFmOnCreateForm_W(Self: TMainFm; T: TCreateFormEvent); begin Self.OnCreateForm := T; end;
procedure TMainFmOnDestroyForm_R(Self: TMainFm; var T: TCreateFormEvent); begin T := Self.OnDestroyForm; end;
procedure TMainFmOnDestroyForm_W(Self: TMainFm; T: TCreateFormEvent); begin Self.OnDestroyForm := T; end;
procedure TMainFmOnCreateListWindow_R(Self: TMainFm; var T: TCreateListWindowEvent); begin T := Self.OnCreateListWindow; end;
procedure TMainFmOnCreateListWindow_W(Self: TMainFm; T: TCreateListWindowEvent); begin Self.OnCreateListWindow := T; end;
procedure TMainFmOnCreateReportWindow_R(Self: TMainFm; var T: TCreateReportWindowEvent); begin T := Self.OnCreateReportWindow; end;
procedure TMainFmOnCreateReportWindow_W(Self: TMainFm; T: TCreateReportWindowEvent); begin Self.OnCreateReportWindow := T; end;

procedure RIRegister_More(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSplitter) do
  begin

  end;

  with Cl.Add(TPanelBitBtn) do
  begin

  end;

  with Cl.Add(TButtonPanel) do
  begin

  end;

  with Cl.Add(TStatusPanel) do
  begin
    RegisterMethod(@TStatusPanel.StatusBar, 'StatusBar');
  end;

  with Cl.Add(TStatusPanels) do
  begin
    RegisterConstructor(@TStatusBar.Create, 'Create');
    RegisterMethod(@TStatusPanels.Add, 'Add');
    RegisterPropertyHelper(@TStatusPanelsItems_R, @TStatusPanelsItems_W, 'Items');
    RegisterPropertyHelper(@TStatusPanelsStatusBar_R, nil, 'StatusBar');
  end;

  with Cl.Add(TStatusBar) do
  begin
    RegisterMethod(@TStatusBar.BeginUpdate, 'BeginUpdate');
    RegisterMethod(@TStatusBar.EndUpdate, 'EndUpdate');
    RegisterMethod(@TStatusBar.GetPanelIndexAt, 'GetPanelIndexAt');
    RegisterMethod(@TStatusBar.SizeGripEnabled, 'SizeGripEnabled');
    RegisterMethod(@TStatusBar.UpdatingStatusBar, 'UpdatingStatusBar');
    RegisterPropertyHelper(@TStatusBarCanvas_R, nil, 'Canvas');
  end;

  with Cl.Add(TFormView) do
  begin
    RegisterConstructor(@TFormView.CreateView, 'CreateView');
    RegisterPropertyHelper(@TFormViewGrid_R, nil, 'Grid');
    RegisterPropertyHelper(@TFormViewForm_R, nil, 'Form');
    RegisterPropertyHelper(@TFormViewTree_R, nil, 'Tree');
    RegisterPropertyHelper(@TFormViewScrollBox_R, nil, 'ScrollBox');
    RegisterPropertyHelper(@TFormViewTreeSplitter_R, nil, 'TreeSplitter');
    RegisterPropertyHelper(@TFormViewFormSplitter_R, nil, 'FormSplitter');
  end;

  with Cl.Add(TToolButton) do
  begin

  end;

  with Cl.Add(TToolbar) do
  begin
    RegisterPropertyHelper(@TToolBarButtonCount_R, nil, 'ButtonCount');
    RegisterPropertyHelper(@TToolBarButtons_R, nil, 'Buttons');
    RegisterPropertyHelper(@TToolBarRowCount_R, nil, 'RowCount');
  end;

  with Cl.Add(TParamList) do
  begin
    RegisterConstructor(@TParamList.Create, 'Create');
    RegisterMethod(@TParamList.Clear, 'Clear');
    RegisterMethod(@TParamList.ParamExists, 'ParamExists');
    RegisterPropertyHelper(@TParamListValues_R, @TParamListValues_W, 'Values');
    RegisterPropertyHelper(@TParamListObjects_R, @TParamListObjects_W, 'Objects');
    RegisterPropertyHelper(@TParamListNames_R, nil, 'Names');
    RegisterPropertyHelper(@TParamListValueFromIndex_R, @TParamListValueFromIndex_W, 'ValueFromIndex');
    RegisterPropertyHelper(@TParamListObjectFromIndex_R, @TParamListObjectFromIndex_W, 'ObjectFromIndex');
    RegisterPropertyHelper(@TParamListCount_R, nil, 'Count');
    RegisterEventPropertyHelper(@TParamListOnGetParam_R, @TParamListOnGetParam_W, 'OnGetParam');
    RegisterEventPropertyHelper(@TParamListOnSetParam_R, @TParamListOnSetParam_W, 'OnSetParam');
  end;

  with Cl.Add(TWindow) do
  begin
    RegisterConstructor(@TWindow.CreateWindow, 'CreateWindow');
    RegisterPropertyHelper(@TWindowParams, nil, 'Params');
  end;

  with Cl.Add(TListWindow) do
  begin
    RegisterConstructor(@TListWindow.CreateWindow, 'CreateWindow');
    RegisterPropertyHelper(@TListWindowButtons_R, nil, 'Buttons');
    RegisterPropertyHelper(@TListWindowFormView_R, nil, 'FormView');
    RegisterPropertyHelper(@TListWindowToolbar_R, nil, 'Toolbar');
  end;

  with Cl.Add(TEditWindow) do
  begin
    RegisterPropertyHelper(@TEditWindowForm_R, nil, 'Form');
    RegisterPropertyHelper(@TEditWindowScrollBox_R, nil, 'ScrollBox');
    RegisterPropertyHelper(@TEditWindowButtons_R, nil, 'Buttons');
  end;

  with Cl.Add(TMainFm) do
  begin
    RegisterMethod(@TMainFm.CreatePage, 'CreatePage');
    RegisterMethod(@TMainFm.DestroyPage, 'DestroyPage');
    RegisterPropertyHelper(@TMainFmFormViews_R, nil, 'FormViews');
    RegisterPropertyHelper(@TMainFmPages_R, nil, 'Pages');
    RegisterPropertyHelper(@TMainFmToolbar_R, nil, 'Toolbar');
    RegisterPropertyHelper(@TMainFmStatusBar_R, nil, 'StatusBar');
    RegisterPropertyHelper(@TMainFmParams_R, nil, 'Params');
    RegisterEventPropertyHelper(@TMainFmOnCreateForm_R, @TMainFmOnCreateForm_W, 'OnCreateForm');
    RegisterEventPropertyHelper(@TMainFmOnDestroyForm_R, @TMainFmOnDestroyForm_W, 'OnDestroyForm');
    RegisterEventPropertyHelper(@TMainFmOnCreateListWindow_R, @TMainFmOnCreateListWindow_W, 'OnCreateListWindow');
    RegisterEventPropertyHelper(@TMainFmOnCreateReportWindow_R, @TMainFmOnCreateReportWindow_W, 'OnCreateReportWindow');
  end;
end;

procedure TKGridCells_R(Self: TKGrid; var T: String; C, R: Integer); begin T := Self.Cells[C, R]; end;
procedure TKGridCells_W(Self: TKGrid; T: String; C, R: Integer); begin Self.Cells[C, R] := T; end;
procedure TKGridCellSpan_R(Self: TKGrid; var T: TKGridCellSpan; C, R: Integer); begin T := Self.CellSpan[C, R]; end;
procedure TKGridCellSpan_W(Self: TKGrid; T: TKGridCellSpan; C, R: Integer); begin Self.CellSpan[C, R] := T; end;
procedure TKGridCol_R(Self: TKGrid; var T: Integer); begin T := Self.Col; end;
procedure TKGridCol_W(Self: TKGrid; T: Integer); begin Self.Col := T; end;
procedure TKGridColWidths_R(Self: TKGrid; var T: Integer; I: Integer); begin T := Self.ColWidths[I]; end;
procedure TKGridColWidths_W(Self: TKGrid; T, I: Integer); begin Self.ColWidths[I] := T; end;
procedure TKGridLeftCol_R(Self: TKGrid; var T: Integer); begin T := Self.LeftCol; end;
procedure TKGridLeftCol_W(Self: TKGrid; T: Integer); begin Self.LeftCol := T; end;
procedure TKGridObjects_R(Self: TKGrid; var T: TObject; C, R: Integer); begin T := Self.Objects[C, R]; end;
procedure TKGridObjects_W(Self: TKGrid; T: TObject; C, R: Integer); begin Self.Objects[C, R] := T; end;
procedure TKGridRow_R(Self: TKGrid; var T: Integer); begin T := Self.Row; end;
procedure TKGridRow_W(Self: TKGrid; T: Integer); begin Self.Row := T; end;
procedure TKGridRowHeights_R(Self: TKGrid; var T: Integer; I: Integer); begin T := Self.RowHeights[I]; end;
procedure TKGridRowHeights_W(Self: TKGrid; T, I: Integer); begin Self.RowHeights[I] := T; end;
procedure TKGridSelection_R(Self: TKGrid; var T: TKGridRect); begin T := Self.Selection; end;
procedure TKGridSelection_W(Self: TKGrid; T: TKGridRect); begin Self.Selection := T; end;
procedure TKGridSelectionCount_R(Self: TKGrid; var T: Integer); begin T := Self.SelectionCount; end;
procedure TKGridSelectionRect_R(Self: TKGrid; var T: TRect); begin T := Self.SelectionRect; end;
procedure TKGridSelections_R(Self: TKGrid; var T: TKGridRect; I: Integer); begin T := Self.Selections[I]; end;
procedure TKGridSelections_W(Self: TKGrid; T: TKGridRect; I: Integer); begin Self.Selections[I] := T; end;
procedure TKGridTopRow_R(Self: TKGrid; var T: Integer); begin T := Self.TopRow; end;
procedure TKGridTopRow_W(Self: TKGrid; T: Integer); begin Self.TopRow := T; end;
procedure TKGridVisibleColCount_R(Self: TKGrid; var T: Integer); begin T := Self.VisibleColCount; end;
procedure TKGridVisibleRowCount_R(Self: TKGrid; var T: Integer); begin T := Self.VisibleRowCount; end;

procedure RIRegister_KGrid(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TKGridColors) do
  begin

  end;

  with Cl.Add(TKGrid) do
  begin
    RegisterVirtualMethod(@TKGrid.CellSelected, 'CellSelected');
    RegisterMethod(@TKGrid.DeleteCol, 'DeleteCol');
    RegisterMethod(@TKGrid.DeleteRow, 'DeleteRow');
    RegisterMethod(@TKGrid.InsertCol, 'InsertCol');
    RegisterMethod(@TKGrid.InsertRow, 'InsertRow');
    RegisterMethod(@TKGrid.MouseToCell, 'MouseToCell');
    RegisterPropertyHelper(@TKGridCells_R, @TKGridCells_W, 'Cells');
    RegisterPropertyHelper(@TKGridCellSpan_R, @TKGridCellSpan_W, 'CellSpan');
    RegisterPropertyHelper(@TKGridCol_R, @TKGridCol_W, 'Col');
    RegisterPropertyHelper(@TKGridColWidths_R, @TKGridColWidths_W, 'ColWidths');
    RegisterPropertyHelper(@TKGridLeftCol_R, @TKGridLeftCol_W, 'LeftCol');
    RegisterPropertyHelper(@TKGridObjects_R, @TKGridObjects_W, 'Objects');
    RegisterPropertyHelper(@TKGridRow_R, @TKGridRow_W, 'Row');
    RegisterPropertyHelper(@TKGridRowHeights_R, @TKGridRowHeights_W, 'RowHeights');
    RegisterPropertyHelper(@TKGridSelection_R, @TKGridSelection_W, 'Selection');
    RegisterPropertyHelper(@TKGridSelectionCount_R, nil, 'SelectionCount');
    RegisterPropertyHelper(@TKGridSelectionRect_R, nil, 'SelectionRect');
    RegisterPropertyHelper(@TKGridSelections_R, @TKGridSelections_W, 'Selections');
    RegisterPropertyHelper(@TKGridTopRow_R, @TKGridTopRow_W, 'TopRow');
    RegisterPropertyHelper(@TKGridVisibleColCount_R, nil, 'VisibleColCount');
    RegisterPropertyHelper(@TKGridVisibleRowCount_R, nil, 'VisibleRowCount');
  end;

  with Cl.Add(TdxPivotGrid) do
  begin

  end;
end;

procedure TdxSQLQueryFields_R(Self: TdxSQLQuery; var T: Variant; I: String); begin T := Self.Fields[I]; end;
procedure TdxSQLQueryField_R(Self: TdxSQLQuery; var T: TField; I: Integer); begin T := Self.Field[I]; end;
procedure TdxSQLQueryAsI_R(Self: TdxSQLQuery; var T: Integer; I: String); begin T := Self.AsI[I]; end;
procedure TdxSQLQueryAsF_R(Self: TdxSQLQuery; var T: Extended; I: String); begin T := Self.AsF[I]; end;
procedure TdxSQLQueryAsDT_R(Self: TdxSQLQuery; var T: TDateTime; I: String); begin T := Self.AsDT[I]; end;
procedure TdxSQLQueryAsS_R(Self: TdxSQLQuery; var T: String; I: String); begin T := Self.AsS[I]; end;

procedure RIRegister_dxSQLQuery(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxSQLQuery) do
  begin
    RegisterConstructor(@TdxSQLQuery.Create, 'Create');
    RegisterMethod(@TdxSQLQuery.Open, 'Open');
    RegisterMethod(@TdxSQLQuery.Close, 'Close');
    RegisterMethod(@TdxSQLQuery.MoveFirst, 'MoveFirst');
    RegisterMethod(@TdxSQLQuery.MovePrior, 'MovePrior');
    RegisterMethod(@TdxSQLQuery.MoveNext, 'MoveNext');
    RegisterMethod(@TdxSQLQuery.MoveLast, 'MoveLast');
    RegisterMethod(@TdxSQLQuery.MoveBy, 'MoveBy');
    RegisterMethod(@TdxSQLQuery.MoveTo, 'MoveTo');
    RegisterMethod(@TdxSQLQuery.BOF, 'BOF');
    RegisterMethod(@TdxSQLQuery.EOF, 'EOF');
    RegisterMethod(@TdxSQLQuery.RecNo, 'RecNo');
    RegisterMethod(@TdxSQLQuery.RecordCount, 'RecordCount');
    RegisterMethod(@TdxSQLQuery.FieldCount, 'FieldCount');
    RegisterPropertyHelper(@TdxSQLQueryFields_R, nil, 'Fields');
    RegisterPropertyHelper(@TdxSQLQueryField_R, nil, 'Field');
    RegisterPropertyHelper(@TdxSQLQueryAsI_R, nil, 'AsI');
    RegisterPropertyHelper(@TdxSQLQueryAsF_R, nil, 'AsF');
    RegisterPropertyHelper(@TdxSQLQueryAsDT_R, nil, 'AsDT');
    RegisterPropertyHelper(@TdxSQLQueryAsS_R, nil, 'AsS');
  end;
end;

{procedure TdxQFieldFieldName_R(Self: TdxQField; var T: String); begin T := Self.FieldName; end;
procedure TdxQFieldName_R(Self: TdxQField; var T: String); begin T := Self.Name; end;
procedure TdxQFieldFunc_R(Self: TdxQField; var T: TRpTotalFunc); begin T := Self.Func; end;
procedure TdxQFieldFieldType_R(Self: TdxQField; var T: TRpFieldType); begin T := Self.FieldType; end;

procedure TdxQueryFields_R(Self: TdxQuery; var T: Variant; I: String); begin T := Self.Fields[I]; end;
procedure TdxQueryAsI_R(Self: TdxQuery; var T: Integer; I: String); begin T := Self.AsI[I]; end;
procedure TdxQueryAsF_R(Self: TdxQuery; var T: Extended; I: String); begin T := Self.AsF[I]; end;
procedure TdxQueryAsDT_R(Self: TdxQuery; var T: TDateTime; I: String); begin T := Self.AsDT[I]; end;
procedure TdxQueryAsS_R(Self: TdxQuery; var T: String; I: String); begin T := Self.AsS[I]; end;

procedure RIRegister_dxQuery(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxQField) do
  begin
    RegisterPropertyHelper(@TdxQFieldFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxQFieldName_R, nil, 'Name');
    RegisterPropertyHelper(@TdxQFieldFunc_R, nil, 'Func');
    RegisterPropertyHelper(@TdxQFieldFieldType_R, nil, 'FieldType');
  end;
  with Cl.Add(TdxQSource) do
  begin
    RegisterConstructor(@TdxQSource.Create, 'Create');
    RegisterMethod(@TdxQSource.AddField, 'AddField');
  end;
  with Cl.Add(TdxQuery) do
  begin
    RegisterConstructor(@TdxQuery.Create, 'Create');
    RegisterMethod(@TdxQuery.AddSource, 'AddSource');
    RegisterMethod(@TdxQuery.AddSorting, 'AddSorting');
    RegisterMethod(@TdxQuery.GroupByDate, 'GroupByDate');
    RegisterMethod(@TdxQuery.Open, 'Open');
    RegisterMethod(@TdxQuery.Close, 'Close');
    RegisterMethod(@TdxQuery.MoveFirst, 'MoveFirst');
    RegisterMethod(@TdxQuery.MovePrior, 'MovePrior');
    RegisterMethod(@TdxQuery.MoveNext, 'MoveNext');
    RegisterMethod(@TdxQuery.MoveLast, 'MoveLast');
    RegisterMethod(@TdxQuery.MoveBy, 'MoveBy');
    RegisterMethod(@TdxQuery.MoveTo, 'MoveTo');
    RegisterMethod(@TdxQuery.BOF, 'BOF');
    RegisterMethod(@TdxQuery.EOF, 'EOF');
    RegisterMethod(@TdxQuery.RecNo, 'RecNo');
    RegisterMethod(@TdxQuery.RecId, 'RecId');
    RegisterMethod(@TdxQuery.RecordCount, 'RecordCount');
    RegisterPropertyHelper(@TdxQueryFields_R, nil, 'Fields');
    RegisterPropertyHelper(@TdxQueryAsI_R, nil, 'AsI');
    RegisterPropertyHelper(@TdxQueryAsF_R, nil, 'AsF');
    RegisterPropertyHelper(@TdxQueryAsDT_R, nil, 'AsDT');
    RegisterPropertyHelper(@TdxQueryAsS_R, nil, 'AsS');
  end;
end;  }

procedure TdxQueryGridQueryName_R(Self: TdxQueryGrid; var T: String); begin T := Self.QueryName; end;
procedure TdxQueryGridFields_R(Self: TdxQueryGrid; var T: Variant; I: String); begin T := Self.Fields[I]; end;
procedure TdxQueryGridAsI_R(Self: TdxQueryGrid; var T: Integer; I: String); begin T := Self.AsI[I]; end;
procedure TdxQueryGridAsF_R(Self: TdxQueryGrid; var T: Extended; I: String); begin T := Self.AsF[I]; end;
procedure TdxQueryGridAsDT_R(Self: TdxQueryGrid; var T: TDateTime; I: String); begin T := Self.AsDT[I]; end;
procedure TdxQueryGridAsS_R(Self: TdxQueryGrid; var T: String; I: String); begin T := Self.AsS[I]; end;

procedure TdxQueryGridOnCreateForm_R(Self: TdxQueryGrid; var T: TCreateFormEvent); begin T := Self.OnCreateForm; end;
procedure TdxQueryGridOnCreateForm_W(Self: TdxQueryGrid; T: TCreateFormEvent); begin Self.OnCreateForm := T; end;

procedure RIRegister_dxQueryGrid(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxQueryGrid) do
  begin
    RegisterMethod(@TdxQueryGrid.MoveFirst, 'MoveFirst');
    RegisterMethod(@TdxQueryGrid.MovePrior, 'MovePrior');
    RegisterMethod(@TdxQueryGrid.MoveNext, 'MoveNext');
    RegisterMethod(@TdxQueryGrid.MoveLast, 'MoveLast');
    RegisterMethod(@TdxQueryGrid.MoveBy, 'MoveBy');
    RegisterMethod(@TdxQueryGrid.MoveTo, 'MoveTo');
    RegisterMethod(@TdxQueryGrid.EOF, 'EOF');
    RegisterMethod(@TdxQueryGrid.BOF, 'BOF');
    RegisterMethod(@TdxQueryGrid.RecNo, 'RecNo');
    RegisterMethod(@TdxQueryGrid.RecId, 'RecId');
    RegisterMethod(@TdxQueryGrid.EnableControls, 'EnableControls');
    RegisterMethod(@TdxQueryGrid.DisableControls, 'DisableControls');
    RegisterMethod(@TdxQueryGrid.ControlsDisabled, 'ControlsDisabled');
    RegisterMethod(@TdxQueryGrid.RecordCount, 'RecordCount');
    RegisterMethod(@TdxQueryGrid.Locate, 'Locate');
    RegisterMethod(@TdxQueryGrid.GotoRecord, 'GotoRecord');
    RegisterMethod(@TdxQueryGrid.Refresh, 'Refresh');
    RegisterPropertyHelper(@TdxQueryGridQueryName_R, nil, 'QueryName');
    RegisterPropertyHelper(@TdxQueryGridFields_R, nil, 'Fields');
    RegisterPropertyHelper(@TdxQueryGridAsI_R, nil, 'AsI');
    RegisterPropertyHelper(@TdxQueryGridAsF_R, nil, 'AsF');
    RegisterPropertyHelper(@TdxQueryGridAsDT_R, nil, 'AsDT');
    RegisterPropertyHelper(@TdxQueryGridAsS_R, nil, 'AsS');
    RegisterEventPropertyHelper(@TdxQueryGridOnCreateForm_R, @TdxQueryGridOnCreateForm_W,
      'OnCreateForm');
  end;
end;

procedure TdxFileSourceFileName_R(Self: TdxFile; var T: String); begin T := Self.SourceFileName; end;
procedure TdxFileStoredFileName_R(Self: TdxFile; var T: String); begin T := Self.StoredFileName; end;
procedure TdxFileDescription_R(Self: TdxFile; var T: String); begin T := Self.Description; end;
procedure TdxFileDescription_W(Self: TdxFile; T: String); begin Self.Description := T; end;

procedure RIRegister_dxFile(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxFile) do
  begin
    RegisterMethod(@TdxFile.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TdxFile.SaveToFile, 'SaveToFile');
    RegisterMethod(@TdxFile.Clear, 'Clear');
    RegisterPropertyHelper(@TdxFileSourceFileName_R, nil, 'SourceFileName');
    RegisterPropertyHelper(@TdxFileStoredFileName_R, nil, 'StoredFileName');
    RegisterPropertyHelper(@TdxFileDescription_R, @TdxFileDescription_W, 'Description');
  end;
end;

procedure TdxDBImageReadOnly_R(Self: TdxDBImage; var T: Boolean); begin T := Self.ReadOnly; end;
procedure TdxDBImageReadOnly_W(Self: TdxDBImage; T: Boolean); begin Self.ReadOnly := T; end;
procedure TdxDBImageSourceFileName_R(Self: TdxDBImage; var T: String); begin T := Self.SourceFileName; end;
procedure TdxDBImageStoredFileName_R(Self: TdxDBImage; var T: String); begin T := Self.StoredFileName; end;

procedure RIRegister_dxDBImage(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxDBImage) do
  begin
    RegisterMethod(@TdxDBImage.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TdxDBImage.SaveToFile, 'SaveToFile');
    RegisterPropertyHelper(@TdxDBImageReadOnly_R, @TdxDBImageReadOnly_W, 'ReadOnly');
    RegisterPropertyHelper(@TdxDBImageSourceFileName_R, nil, 'SourceFileName');
    RegisterPropertyHelper(@TdxDBImageStoredFileName_R, nil, 'StoredFileName');
  end;
end;

procedure TdxButtonOnButtonClick_R(Self: TdxButton; var T: TNotifyEvent); begin T := Self.OnButtonClick; end;
procedure TdxButtonOnButtonClick_W(Self: TdxButton; T: TNotifyEvent); begin Self.OnButtonClick := T; end;

procedure RIRegister_dxButton(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxButton) do
  begin
    RegisterEventPropertyHelper(@TdxButtonOnButtonClick_R, @TdxButtonOnButtonClick_W, 'OnClick');
  end;
end;

procedure RIRegister_dxCounter(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxCounter) do
  begin

  end;
end;

procedure RIRegister_dxTimeEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxTimeEdit) do
  begin

  end;
end;

procedure RIRegister_DBTimeEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBTimeEdit) do
  begin

  end;
end;

procedure RIRegister_dxObjectField(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxObjectField) do
  begin

  end;
end;

procedure RIRegister_dxShape(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxShape) do
  begin

  end;
end;

procedure RIRegister_dxPageControl(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxTabSheet) do
  begin

  end;

  with Cl.Add(TdxPageControl) do
  begin

  end;
end;

procedure TPageControlPages_R(Self: TPageControl; var T: TTabSheet; I: Integer); begin T := Self.Pages[I]; end;
procedure TPageControlPageCount_R(Self: TPageControl; var T: Integer); begin T := Self.PageCount; end;
procedure TPageControlActivePageIndex_R(Self: TPageControl; var T: Integer); begin T := Self.ActivePageIndex; end;
procedure TPageControlActivePageIndex_W(Self: TPageControl; T: Integer); begin Self.ActivePageIndex := T; end;

procedure TTabSheetTabIndex_R(Self: TTabSheet; var T: Integer); begin T := Self.TabIndex; end;
procedure TTabSheetPageControl_R(Self: TTabSheet; var T: TPageControl); begin T := Self.PageControl; end;
procedure TTabSheetPageControl_W(Self: TTabSheet; T: TPageControl); begin Self.PageControl := T; end;

procedure RIRegister_PageControl(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomTabControl) do
  begin

  end;

  with Cl.Add(TPageControl) do
  begin
    RegisterPropertyHelper(@TPageControlPages_R, nil, 'Pages');
    RegisterPropertyHelper(@TPageControlPageCount_R, nil, 'PageCount');
    RegisterPropertyHelper(@TPageControlActivePageIndex_R, @TPageControlActivePageIndex_W, 'ActivePageIndex');
  end;

  with Cl.Add(TCustomPage) do
  begin

  end;

  with Cl.Add(TTabSheet) do
  begin
    RegisterPropertyHelper(@TTabSheetTabIndex_R, nil, 'TabIndex');
    RegisterPropertyHelper(@TTabSheetPageControl_R, @TTabSheetPageControl_W, 'PageControl');
  end;
end;

function TCustomImageListAddFromFile(Self: TCustomImageList; const FileName: String): Integer;
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(FileName, True);
  try
    Result := Self.Add(Bmp.Bitmap, nil);
  finally
    Bmp.Free;
  end;
end;

procedure TCustomImageListCount_R(Self: TCustomImageList; var T: Integer); begin T := Self.Count; end;

procedure RIRegister_ImageList(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomImageList) do
  begin
    RegisterMethod(@TCustomImageList.BeginUpdate, 'BeginUpdate');
    RegisterMethod(@TCustomImageList.EndUpdate, 'EndUpdate');
    RegisterMethod(@TCustomImageList.Add, 'Add');
    RegisterMethod(@TCustomImageListAddFromFile, 'AddFromFile');
    RegisterMethod(@TCustomImageList.Clear, 'Clear');
    RegisterMethod(@TCustomImageList.Delete, 'Delete');
    RegisterMethod(@TCustomImageList.GetBitmap, 'GetBitmap');
    RegisterPropertyHelper(@TCustomImageListCount_R, nil, 'Count');
  end;

  with Cl.Add(TImageList) do
  begin

  end;
end;

procedure RIRegister_dxGroupBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxGroupBox) do
  begin

  end;
end;

procedure TdxGridForm_R(Self: TdxGrid; var T: TdxForm); begin T := Self.Form; end;

procedure RIRegister_dxGrid(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TdxForm);

  with Cl.Add(TdxGrid) do
  begin
    RegisterMethod(@TdxGrid.GetFieldNameByColumn, 'GetFieldName');
    RegisterPropertyHelper(@TdxGridForm_R, nil, 'Form');
  end;
end;

//procedure TGridColumnWidthChanged_R(Self: TGridColumn; var T: Boolean); begin T := Self.WidthChanged; end;
procedure TGridColumnGrid_R(Self: TGridColumn; var T: TDBGrid); begin T := TDBGrid(Self.Grid); end;

procedure TGridColumnTitleColumn_R(Self: TGridColumnTitle; var T: TColumn); begin T := TColumn(Self.Column); end;

procedure TColumnField_R(Self: TColumn; var T: TField); begin T := Self.Field; end;
procedure TColumnDesignIndex_R(Self: TColumn; var T: Integer); begin T := Self.DesignIndex; end;

procedure TDBGridColumnsItems_R(Self: TDBGridColumns; var T: TColumn; t1: Integer); begin T := Self.Items[t1]; end;
procedure TDBGridColumnsItems_W(Self: TDBGridColumns; T: TColumn; t1: Integer); begin Self.Items[t1] := T; end;


procedure TDBGridBorderColor_R(Self: TDBGrid; var T: TColor); begin T := Self.BorderColor; end;
procedure TDBGridBorderColor_W(Self: TDBGrid; T: TColor); begin Self.BorderColor := T; end;
procedure TDBGridDefaultTextStyle_R(Self: TDBGrid; var T: TTextStyle); begin T := Self.DefaultTextStyle; end;
procedure TDBGridDefaultTextStyle_W(Self: TDBGrid; T: TTextStyle); begin Self.DefaultTextStyle := T; end;
//procedure TDBGridEditorBorderStyle_R(Self: TDBGrid; var T: TBorderStyle); begin T := Self.EditorBorderStyle; end;
//procedure TDBGridEditorBorderStyle_W(Self: TDBGrid; T: TBorderStyle); begin Self.EditorBorderStyle := T; end;
//procedure TDBGridEditorMode_R(Self: TDBGrid; var T: Boolean); begin T := Self.EditorMode; end;
//procedure TDBGridEditorMode_W(Self: TDBGrid; T: Boolean); begin Self.EditorMode := T; end;
procedure TDBGridExtendedColSizing_R(Self: TDBGrid; var T: Boolean); begin T := Self.ExtendedColSizing; end;
procedure TDBGridExtendedColSizing_W(Self: TDBGrid; T: Boolean); begin Self.ExtendedColSizing := T; end;
//procedure TDBGridFastEditing_R(Self: TDBGrid; var T: Boolean); begin T := Self.FastEditing; end;
//procedure TDBGridFastEditing_W(Self: TDBGrid; T: Boolean); begin Self.FastEditing := T; end;
procedure TDBGridFocusColor_R(Self: TDBGrid; var T: TColor); begin T := Self.FocusColor; end;
procedure TDBGridFocusColor_W(Self: TDBGrid; T: TColor); begin Self.FocusColor := T; end;
procedure TDBGridFocusRectVisible_R(Self: TDBGrid; var T: Boolean); begin T := Self.FocusRectVisible; end;
procedure TDBGridFocusRectVisible_W(Self: TDBGrid; T: Boolean); begin Self.FocusRectVisible := T; end;
procedure TDBGridGridLineColor_R(Self: TDBGrid; var T: TColor); begin T := Self.GridLineColor; end;
procedure TDBGridGridLineColor_W(Self: TDBGrid; T: TColor); begin Self.GridLineColor := T; end;
procedure TDBGridGridLineStyle_R(Self: TDBGrid; var T: TPenStyle); begin T := Self.GridLineStyle; end;
procedure TDBGridGridLineStyle_W(Self: TDBGrid; T: TPenStyle); begin Self.GridLineStyle := T; end;
//procedure TDBGridInplaceEditor_R(Self: TDBGrid; var T: TWinControl); begin T := Self.InplaceEditor; end;
procedure TDBGridSelectedColor_R(Self: TDBGrid; var T: TColor); begin T := Self.SelectedColor; end;
procedure TDBGridSelectedColor_W(Self: TDBGrid; T: TColor); begin Self.SelectedColor := T; end;
procedure TDBGridSelectedColumn_R(Self: TDBGrid; var T: TColumn); begin T := Self.SelectedColumn; end;
procedure TDBGridSelectedField_R(Self: TDBGrid; var T: TField); begin T := Self.SelectedField; end;
procedure TDBGridSelectedIndex_R(Self: TDBGrid; var T: Integer); begin T := Self.SelectedIndex; end;

procedure TMyDBGridSelectedRowCount_R(Self: TMyDBGrid; var T: Integer); begin T := Self.SelectedRowCount; end;

procedure RIRegister_DBGrid(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCUSTOMGRID);

  with Cl.Add(TGRIDCOLUMN) do
  begin
    //RegisterMethod(@TGridColumn.IsDefault, 'ISDEFAULT');
    //RegisterMethod(@TGridColumn.FillDefaultFont, 'FILLDEFAULTFONT');
    RegisterPropertyHelper(@TGridColumnGrid_R, nil, 'GRID');
    //RegisterPropertyHelper(@TGridColumnWidthChanged_R, nil, 'WIDTHCHANGED');
  end;

  with Cl.Add(TGRIDCOLUMNTITLE) do
  begin
    //RegisterMethod(@TGridColumnTitle.FillTitleDefaultFont, 'FILLTITLEDEFAULTFONT');
    //RegisterMethod(@TGridColumnTitle.IsDefault, 'ISDEFAULT');
    RegisterPropertyHelper(@TGridColumnTitleColumn_R, nil, 'COLUMN');
  end;

  with Cl.Add(TCOLUMN) do
  begin
    RegisterPropertyHelper(@TColumnField_R, nil, 'FIELD');
    RegisterPropertyHelper(@TColumnDesignIndex_R, nil, 'DESIGNINDEX');
  end;

  with Cl.Add(TDBGridColumns) do
  begin
    RegisterPropertyHelper(@TDBGridColumnsItems_R, @TDBGridColumnsItems_W, 'ITEMS');
  end;

  with Cl.Add(TDBGrid) do
  begin
    RegisterPropertyHelper(@TDBGridBorderColor_R, @TDBGridBorderColor_W, 'BorderColor');
    RegisterPropertyHelper(@TDBGridDefaultTextStyle_R, @TDBGridDefaultTextStyle_W, 'DEFAULTTEXTSTYLE');
    //RegisterPropertyHelper(@TDBGridEditorBorderStyle_R, @TDBGridEditorBorderStyle_W, 'EditorBorderStyle');
    //RegisterPropertyHelper(@TDBGridEditorMode_R, @TDBGridEditorMode_W, 'EditorMode');
    RegisterPropertyHelper(@TDBGridExtendedColSizing_R, @TDBGridExtendedColSizing_W, 'ExtendedColSizing');
    //RegisterPropertyHelper(@TDBGridFastEditing_R, @TDBGridFastEditing_W, 'FastEditing');
    RegisterPropertyHelper(@TDBGridFocusColor_R, @TDBGridFocusColor_W, 'FocusColor');
    RegisterPropertyHelper(@TDBGridFocusRectVisible_R, @TDBGridFocusRectVisible_W, 'FocusRectVisible');
    RegisterPropertyHelper(@TDBGridGridLineColor_R, @TDBGridGridLineColor_W, 'GridLineColor');
    RegisterPropertyHelper(@TDBGridGridLineStyle_R, @TDBGridGridLineStyle_W, 'GridLineStyle');
    //RegisterPropertyHelper(@TDBGridInplaceEditor_R, nil, 'InplaceEditor');
    RegisterPropertyHelper(@TDBGridSelectedColor_R, @TDBGridSelectedColor_W, 'SelectedColor');
    RegisterPropertyHelper(@TDBGridSelectedColumn_R, nil, 'SelectedColumn');
    RegisterPropertyHelper(@TDBGridSelectedField_R, nil, 'SelectedField');
    RegisterPropertyHelper(@TDBGridSelectedIndex_R, nil, 'SelectedIndex');
  end;
  with Cl.Add(TMyDBGrid) do
  begin
    RegisterMethod(@TMyDBGrid.MoveToSelectedRow, 'MoveToSelectedRow');
    RegisterMethod(@TMyDBGrid.ClearRowsSelection, 'ClearRowsSelection');
    RegisterMethod(@TMyDBGrid.CurrentRowSelected, 'CurrentRowSelected');
    RegisterPropertyHelper(@TMyDBGridSelectedRowCount_R, nil, 'SelectedRowCount');
  end;
end;

procedure TdxImageBitmap_R(Self: TdxImage; var T: TBitmap); begin T := Self.Bitmap; end;
procedure TdxImageBitmap_W(Self: TdxImage; T: TBitmap); begin Self.Bitmap := T; end;

procedure RIRegister_dxImage(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxImage) do
  begin
    RegisterMethod(@TdxImage.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TdxImage.SaveToFile, 'SaveToFile');
    RegisterVirtualMethod(@TdxImage.Clear, 'Clear');
    RegisterPropertyHelper(@TdxImageBitmap_R, @TdxImageBitmap_W, 'Bitmap');
  end;
end;

procedure TdxLookupComboBoxButton_R(Self: TdxLookupComboBox; var T: TSpeedButton); begin T := Self.Button; end;
procedure TdxLookupComboBoxKeyValue_R(Self: TdxLookupComboBox; var T: Variant); begin T := Self.KeyValue; end;
procedure TdxLookupComboBoxKeyValue_W(Self: TdxLookupComboBox; T: Variant); begin Self.KeyValue := T; end;
procedure TdxLookupComboBoxOnCreateListWindow_R(Self: TdxLookupComboBox; var T: TCreateListWindowEvent); begin T := Self.OnCreateListWindow; end;
procedure TdxLookupComboBoxOnCreateListWindow_W(Self: TdxLookupComboBox; T: TCreateListWindowEvent); begin Self.OnCreateListWindow := T; end;
procedure TdxLookupComboBoxOnCreateForm_R(Self: TdxLookupComboBox; var T: TCreateFormEvent); begin T := Self.OnCreateForm; end;
procedure TdxLookupComboBoxOnCreateForm_W(Self: TdxLookupComboBox; T: TCreateFormEvent); begin Self.OnCreateForm := T; end;
procedure TdxLookupComboBoxOnUtf8KeyPress_R(Self: TdxLookupComboBox; var T: TMyUtf8KeyPressEvent); begin T := Self.OnMyUTF8KeyPress; end;
procedure TdxLookupComboBoxOnUtf8KeyPress_W(Self: TdxLookupComboBox; T: TMyUtf8KeyPressEvent); begin Self.OnMyUTF8KeyPress := T; end;

procedure RIRegister_dxLookupComboBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxLookupComboBox) do
  begin
    RegisterPropertyHelper(@TdxLookupComboBoxButton_R, nil, 'Button');
    RegisterPropertyHelper(@TdxLookupComboBoxKeyValue_R, @TdxLookupComboBoxKeyValue_W, 'KeyValue');
    RegisterEventPropertyHelper(@TdxLookupComboBoxOnCreateListWindow_R, @TdxLookupComboBoxOnCreateListWindow_W,
      'OnCreateListWindow');
    RegisterEventPropertyHelper(@TdxLookupComboBoxOnCreateForm_R, @TdxLookupComboBoxOnCreateForm_W,
      'OnCreateForm');
    RegisterPropertyHelper(@TdxLookupComboBoxOnUtf8KeyPress_R, @TdxLookupComboBoxOnUtf8KeyPress_W, 'OnUtf8KeyPress');
  end;
end;

procedure TdxComboBoxOnUtf8KeyPress_R(Self: TdxComboBox; var T: TMyUtf8KeyPressEvent); begin T := Self.OnMyUTF8KeyPress; end;
procedure TdxComboBoxOnUtf8KeyPress_W(Self: TdxComboBox; T: TMyUtf8KeyPressEvent); begin Self.OnMyUTF8KeyPress := T; end;

procedure RIRegister_dxComboBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxComboBox) do
  begin
    RegisterPropertyHelper(@TdxComboBoxOnUtf8KeyPress_R, @TdxComboBoxOnUtf8KeyPress_W, 'OnUtf8KeyPress');
  end;
end;

procedure RIRegister_DBComboBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBComboBox) do
  begin

  end;
end;

procedure TCustomDBComboBoxFieldR(Self: TCustomDBComboBox; var T: TField); begin T := Self.Field; end;

procedure RIRegister_CustomDBComboBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomDBComboBox) do
  begin
    RegisterPropertyHelper(@TCustomDBComboBoxFieldR, nil, 'Field');
  end;
end;

procedure RIRegister_dxCheckBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxCheckBox) do
  begin
  end;
end;

procedure TDBCheckBoxFieldR(Self: TDBCheckBox; var T: TField); begin T := Self.Field; end;
procedure TDBCheckBoxCheckedR(Self: TDBCheckBox; var T: Boolean); begin T := Self.Checked; end;
procedure TDBCheckBoxCheckedW(Self: TDBCheckBox; T: Boolean); begin Self.Checked := T; end;

procedure RIRegister_DBCheckBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBCheckBox) do
  begin
    RegisterPropertyHelper(@TDBCheckBoxFieldR, nil, 'Field');
    RegisterPropertyHelper(@TDBCheckBoxCheckedR, @TDBCheckBoxCheckedW, 'Checked');
  end;
end;

procedure TdxMemoButtonR(Self: TdxMemo; var T: TSpeedButton); begin T := Self.Button; end;
procedure TdxMemoOnCreateListWindow_R(Self: TdxMemo; var T: TCreateListWindowEvent); begin T := Self.OnCreateListWindow; end;
procedure TdxMemoOnCreateListWindow_W(Self: TdxMemo; T: TCreateListWindowEvent); begin Self.OnCreateListWindow := T; end;
procedure TdxMemoOnUtf8KeyPress_R(Self: TdxMemo; var T: TMyUtf8KeyPressEvent); begin T := Self.OnMyUTF8KeyPress; end;
procedure TdxMemoOnUtf8KeyPress_W(Self: TdxMemo; T: TMyUtf8KeyPressEvent); begin Self.OnMyUTF8KeyPress := T; end;


procedure RIRegister_dxMemo(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxMemo) do
  begin
    RegisterPropertyHelper(@TdxMemoButtonR, nil, 'Button');
    RegisterEventPropertyHelper(@TdxMemoOnCreateListWindow_R, @TdxMemoOnCreateListWindow_W,
      'OnCreateListWindow');
    RegisterPropertyHelper(@TdxMemoOnUtf8KeyPress_R, @TdxMemoOnUtf8KeyPress_W, 'OnUtf8KeyPress');
  end;
end;

procedure TDBMemoFieldR(Self: TDBMemo; var T: TField); begin T := Self.Field; end;

procedure RIRegister_DBMemo(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBMemo) do
  begin
    RegisterPropertyHelper(@TDBMemoFieldR, nil, 'Field');
  end;
end;

procedure RIRegister_dxDateEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxDateEdit) do
  begin

  end;
end;

procedure RIRegister_DBDateEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBDateEditEx) do
  begin

  end;
end;

procedure TdxCalcEditPrecision_R(Self: TdxCalcEdit; var T: Integer); begin T := Self.Precission; end;
procedure TdxCalcEditPrecision_W(Self: TdxCalcEdit; T: Integer); begin Self.Precission := T; end;

procedure RIRegister_dxCalcEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxCalcEdit) do
  begin
    RegisterPropertyHelper(@TdxCalcEditPrecision_R, @TdxCalcEditPrecision_W, 'Precision');
  end;
end;

procedure RIRegister_DBCalcEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBCalcEdit) do
  begin

  end;
end;

procedure TCustomDBEditButtonButtonR(Self: TCustomDBEditButton; var T: TSpeedButton); begin T := Self.Button; end;
procedure TCustomDBEditButtonOnlyWhenFocusedR(Self: TCustomDBEditButton; var T: Boolean); begin T := Self.ButtonOnlyWhenFocused; end;
procedure TCustomDBEditButtonOnlyWhenFocusedW(Self: TCustomDBEditButton; T: Boolean); begin Self.ButtonOnlyWhenFocused := T; end;
procedure TCustomDBEditHideButtonR(Self: TCustomDBEditButton; var T: Boolean); begin T := Self.HideButton; end;
procedure TCustomDBEditHideButtonW(Self: TCustomDBEditButton; T: Boolean); begin Self.HideButton := T; end;

procedure RIRegister_CustomDBEditButton(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomDBEditButton) do
  begin
    RegisterPropertyHelper(@TCustomDBEditButtonButtonR, nil, 'Button');
    RegisterPropertyHelper(@TCustomDBEditButtonOnlyWhenFocusedR,
      @TCustomDBEditButtonOnlyWhenFocusedW, 'ButtonOnlyWhenFocused');
    RegisterPropertyHelper(@TCustomDBEditHideButtonR,
      @TCustomDBEditHideButtonW, 'HideButton');
  end;
end;

procedure TdxEditOnUtf8KeyPress_R(Self: TdxEdit; var T: TMyUtf8KeyPressEvent); begin T := Self.OnMyUTF8KeyPress; end;
procedure TdxEditOnUtf8KeyPress_W(Self: TdxEdit; T: TMyUtf8KeyPressEvent); begin Self.OnMyUTF8KeyPress := T; end;

procedure RIRegister_dxEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxEdit) do
  begin
    RegisterMethod(@TdxEdit.ValidateText, 'ValidateText');
    RegisterMethod(@TdxEdit.MaskTextEmpty, 'MaskTextEmpty');
    RegisterPropertyHelper(@TdxEditOnUtf8KeyPress_R, @TdxEditOnUtf8KeyPress_W, 'OnUtf8KeyPress');
  end;
end;

procedure TDBEditFieldR(Self: TDBEdit; var T: TField); begin T := Self.Field; end;

procedure RIRegister_DBEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBEdit) do
  begin
    RegisterPropertyHelper(@TDBEditFieldR, nil, 'Field');
  end;
end;

procedure TMaskEditIsMaskedR(Self: TMaskEdit; var T: Boolean); begin T := Self.IsMasked; end;
procedure TMaskEditEditTextR(Self: TMaskEdit; var T: String); begin T := Self.EditText; end;
procedure TMaskEditEditTextW(Self: TMaskEdit; const T: String); begin Self.EditText := T; end;

procedure RIRegister_MaskEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TMaskEditEx) do
  begin
    RegisterMethod(@TdxEdit.ValidateText, 'ValidateText');
    RegisterMethod(@TdxEdit.MaskTextEmpty, 'MaskTextEmpty');
    RegisterPropertyHelper(@TMaskEditIsMaskedR, nil, 'IsMasked');
    RegisterPropertyHelper(@TMaskEditEditTextR, @TMaskEditEditTextW, 'EditText');
  end;
end;

//procedure TCustomMaskEditModifiedR(Self: TCustomMaskEdit; var T: Boolean); begin T := Self.Modified; end;
//procedure TCustomMaskEditModifiedW(Self: TCustomMaskEdit; T: Boolean); begin Self.Modified:= T; end;

{procedure RIRegister_CustomMaskEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomMaskEdit) do
  begin
    //RegisterMethod(@TCustomMaskEdit.Clear, 'Clear');
    RegisterVirtualMethod(@TCustomMaskEdit.ValidateEdit, 'ValidateEdit');
    //RegisterPropertyHelper(@TCustomMaskEditModifiedR, @TCustomMaskEditModifiedW, 'Modified');
  end;
end; }

procedure TdxLabelFieldNameR(Self: TdxLabel; var T: String); begin T := Self.FieldName; end;
procedure TdxLabelFieldNameW(Self: TdxLabel; const T: String); begin Self.FieldName:= T; end;

procedure RIRegister_dxLabel(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxLabel) do
  begin
    RegisterPropertyHelper(@TdxLabelFieldNameR, @TdxLabelFieldNameW, 'FieldName');
  end;
end;

procedure TFilterFieldFieldName_R(Self: TFilterField; var T: String); begin T := Self.FieldName; end;
procedure TFilterFieldIsNot_R(Self: TFilterField; var T: Boolean); begin T := Self.IsNot; end;
procedure TFilterFieldIsNot_W(Self: TFilterField; T: Boolean); begin Self.IsNot := T; end;
procedure TFilterFieldIsNull_R(Self: TFilterField; var T: Boolean); begin T := Self.IsNull; end;
procedure TFilterFieldIsNull_W(Self: TFilterField; T: Boolean); begin Self.IsNull := T; end;
procedure TFilterFieldValues_R(Self: TFilterField; var T: TStringList); begin T := Self.Values; end;
procedure TFilterFieldValue_R(Self: TFilterField; var T: String; I: Integer); begin T := Self.Value[I]; end;
procedure TFilterFieldEndValue_R(Self: TFilterField; var T: String; I: Integer); begin T := Self.EndValue[I]; end;

procedure TFilterObjectFields_R(Self: TFilterObject; var T: TFilterField; I: Integer); begin T := Self.Fields[I]; end;
procedure TFilterObjectCount_R(Self: TFilterObject; var T: Integer); begin T := Self.Count; end;

procedure TdxFormFields_R(Self: TdxForm; var T: Variant; I: String); begin T := Self.Fields[I]; end;
procedure TdxFormFields_W(Self: TdxForm; const T: Variant; I: String); begin Self.Fields[I]:= T; end;
procedure TdxFormAsI_R(Self: TdxForm; var T: Integer; I: String); begin T := Self.AsI[I]; end;
procedure TdxFormAsF_R(Self: TdxForm; var T: Extended; I: String); begin T := Self.AsF[I]; end;
procedure TdxFormAsDT_R(Self: TdxForm; var T: TDateTime; I: String); begin T := Self.AsDT[I]; end;
procedure TdxFormAsS_R(Self: TdxForm; var T: String; I: String); begin T := Self.AsS[I]; end;
procedure TdxFormOldValues_R(Self: TdxForm; var T: Variant; I: String); begin T := Self.OldValues[I]; end;

procedure TdxFormForms_R(Self: TdxForm; var T: TdxForm; I: String); begin T := Self.Forms[I]; end;
procedure TdxFormFormByIndex_R(Self: TdxForm; var T: TdxForm; I: Integer); begin T := Self.FormByIndex[I]; end;
procedure TdxFormFormCount_R(Self: TdxForm; var T: Integer); begin T := Self.FormCount; end;

procedure TdxFormQueries_R(Self: TdxForm; var T: TdxQueryGrid; I: String); begin T := TdxQueryGrid(Self.Queries[I]); end;
procedure TdxFormQueryByIndex_R(Self: TdxForm; var T: TdxQueryGrid; I: Integer); begin T := TdxQueryGrid(Self.QueryByIndex[I]); end;
procedure TdxFormQueryCount_R(Self: TdxForm; var T: Integer); begin T := Self.QueryCount; end;

procedure TdxFormFormGrid_R(Self: TdxForm; var T: TdxGrid); begin T := Self.FormGrid; end;
procedure TdxFormEditWindow_R(Self: TdxForm; var T: TEditWindow); begin T := TEditWindow(Self.EditWindow); end;
procedure TdxFormParams_R(Self: TdxForm; var T: TParamList); begin T := Self.Params; end;
procedure TdxFormState_R(Self: TdxForm; var T: TDataSetState); begin T := Self.State; end;
procedure TdxFormFilter_R(Self: TdxForm; var T: TFilterObject); begin T := Self.Filter; end;
procedure TdxFormParentForm_R(Self: TdxForm; var T: TdxForm); begin T := Self.ParentForm; end;
procedure TdxFormModified_R(Self: TdxForm; var T: Boolean); begin T := Self.Modified; end;

procedure RIRegister_dxForm(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFilterField) do
  begin
    RegisterPropertyHelper(@TFilterFieldFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TFilterFieldIsNot_R, @TFilterFieldIsNot_W, 'IsNot');
    RegisterPropertyHelper(@TFilterFieldIsNull_R, @TFilterFieldIsNull_W, 'IsNull');
    RegisterPropertyHelper(@TFilterFieldValues_R, nil, 'Values');
    RegisterPropertyHelper(@TFilterFieldValue_R, nil, 'Value');
    RegisterPropertyHelper(@TFilterFieldEndValue_R, nil, 'EndValue');
  end;

  with Cl.Add(TFilterObject) do
  begin
    RegisterMethod(@TFilterObject.AddFieldByName, 'AddField');
    RegisterMethod(@TFilterObject.FindFieldByName, 'FindField');
    RegisterMethod(@TFilterObject.DeleteField, 'DeleteField');
    RegisterMethod(@TFilterObject.Clear, 'Clear');
    RegisterPropertyHelper(@TFilterObjectFields_R, nil, 'Fields');
		RegisterPropertyHelper(@TFilterObjectCount_R, nil, 'Count');
  end;

  with Cl.FindClass('TdxForm') do
  begin
    RegisterMethod(@TdxForm.Append, 'Append');
    RegisterMethod(@TdxForm.Edit, 'Edit');
    RegisterMethod(@TdxForm.Delete, 'Delete');
    RegisterMethod(@TdxForm.Post, 'Post');
    RegisterMethod(@TdxForm.Cancel, 'Cancel');
    RegisterMethod(@TdxForm.Refresh, 'Refresh');
    RegisterMethod(@TdxForm.MoveFirst, 'MoveFirst');
    RegisterMethod(@TdxForm.MovePrior, 'MovePrior');
    RegisterMethod(@TdxForm.MoveNext, 'MoveNext');
    RegisterMethod(@TdxForm.MoveLast, 'MoveLast');
    RegisterMethod(@TdxForm.MoveBy, 'MoveBy');
    RegisterMethod(@TdxForm.MoveTo, 'MoveTo');
    RegisterMethod(@TdxForm.Bof, 'BOF');
    RegisterMethod(@TdxForm.Eof, 'EOF');
    RegisterMethod(@TdxForm.RecNo, 'RecNo');
    RegisterMethod(@TdxForm.RecId, 'RecId');
    RegisterMethod(@TdxForm.RecordCount, 'RecordCount');
    RegisterMethod(@TdxForm.Print, 'Print');
    {RegisterMethod(@TdxForm.FindFirst, 'FindFirst');
    RegisterMethod(@TdxForm.FindNext, 'FindNext');
    RegisterMethod(@TdxForm.FindPrior, 'FindPrior');  }
    RegisterMethod(@TdxForm.Locate, 'Locate');
    RegisterMethod(@TdxForm.GotoRecord, 'GotoRecord');
    RegisterMethod(@TdxForm.DisableControls, 'DisableControls');
    RegisterMethod(@TdxForm.EnableControls, 'EnableControls');
    RegisterMethod(@TdxForm.ControlsDisabled, 'ControlsDisabled');
    RegisterMethod(@TdxForm.CanAppend, 'CanAppend');
    RegisterMethod(@TdxForm.CanEdit, 'CanEdit');
    RegisterMethod(@TdxForm.CanDelete, 'CanDelete');
    //RegisterMethod(@TdxForm.AddUserFilter, 'AddUserFilter');
    //RegisterMethod(@TdxForm.ClearUserFilter, 'ClearUserFilter');
    RegisterMethod(@TdxForm.Open, 'Open');
    RegisterMethod(@TdxForm.OpenRecords, 'OpenRecords');
    RegisterMethod(@TdxForm.OpenRecord, 'OpenRecord');
    RegisterMethod(@TdxForm.Opened, 'Opened');
    RegisterMethod(@TdxForm.Close, 'Close');
    RegisterMethod(@TdxForm.Validate, 'Validate');
    RegisterPropertyHelper(@TdxFormFields_R, @TdxFormFields_W, 'Fields');
    RegisterPropertyHelper(@TdxFormAsI_R, nil, 'AsI');
    RegisterPropertyHelper(@TdxFormAsF_R, nil, 'AsF');
    RegisterPropertyHelper(@TdxFormAsDT_R, nil, 'AsDT');
    RegisterPropertyHelper(@TdxFormAsS_R, nil, 'AsS');
    RegisterPropertyHelper(@TdxFormOldValues_R, nil, 'OldValues');
    RegisterPropertyHelper(@TdxFormForms_R, nil, 'Forms');
    RegisterPropertyHelper(@TdxFormFormByIndex_R, nil, 'FormByIndex');
    RegisterPropertyHelper(@TdxFormFormCount_R, nil, 'FormCount');
    RegisterPropertyHelper(@TdxFormQueries_R, nil, 'Queries');
    RegisterPropertyHelper(@TdxFormQueryByIndex_R, nil, 'QueryByIndex');
    RegisterPropertyHelper(@TdxFormQueryCount_R, nil, 'QueryCount');
    RegisterPropertyHelper(@TdxFormFormGrid_R, nil, 'Grid');
    RegisterPropertyHelper(@TdxFormEditWindow_R, nil, 'EditWindow');
    RegisterPropertyHelper(@TdxFormParams_R, nil, 'Params');
    RegisterPropertyHelper(@TdxFormState_R, nil, 'State');
    RegisterPropertyHelper(@TdxFormFilter_R, nil, 'Filter');
    RegisterPropertyHelper(@TdxFormParentForm_R, nil, 'ParentForm');
    RegisterPropertyHelper(@TdxFormModified_R, nil, 'Modified');
  end;
end;

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
procedure TCustomComboBoxReadOnlyR(Self: TCustomComboBox; var T: Boolean); begin T := Self.ReadOnly; end;
procedure TCustomComboBoxReadOnlyW(Self: TCustomComboBox; T: Boolean); begin Self.ReadOnly := T; end;
procedure TCustomComboBoxStyleR(Self: TCustomComboBox; var T: TComboBoxStyle); begin T := Self.Style; end;
procedure TCustomComboBoxStyleW(Self: TCustomComboBox; T: TComboBoxStyle); begin Self.Style := T; end;
procedure TCustomComboBoxTextR(Self: TCustomComboBox; var T: String); begin T := Self.Text; end;
procedure TCustomComboBoxTextW(Self: TCustomComboBox; T: String); begin Self.Text := T; end;

procedure TCustomCheckBoxAllowGrayedR(Self: TCustomCheckBox; var T: Boolean); begin T := Self.AllowGrayed; end;
procedure TCustomCheckBoxAllowGrayedW(Self: TCustomCheckBox; T: Boolean); begin Self.AllowGrayed := T; end;
procedure TCustomCheckBoxStateR(Self: TCustomCheckBox; var T: TCheckBoxState); begin T := Self.State; end;
procedure TCustomCheckBoxStateW(Self: TCustomCheckBox; T: TCheckBoxState); begin Self.State := T; end;
procedure TCustomCheckBoxOnChangeR(Self: TCustomCheckBox; var T: TNotifyEvent); begin T := Self.OnChange; end;
procedure TCustomCheckBoxOnChangeW(Self: TCustomCheckBox; T: TNotifyEvent); begin Self.OnChange := T; end;

procedure TCustomListBoxBorderStyleR(Self: TCustomListBox; var T: TBorderStyle); begin T := Self.BorderStyle; end;
procedure TCustomListBoxBorderStyleW(Self: TCustomListBox; T: TBorderStyle); begin Self.BorderStyle := T; end;
procedure TCustomListBoxColumnsR(Self: TCustomListBox; var T: Integer); begin T := Self.Columns; end;
procedure TCustomListBoxColumnsW(Self: TCustomListBox; T: Integer); begin Self.Columns := T; end;
procedure TCustomListBoxExtendedSelectR(Self: TCustomListBox; var T: Boolean); begin T := Self.ExtendedSelect; end;
procedure TCustomListBoxExtendedSelectW(Self: TCustomListBox; T: Boolean); begin Self.ExtendedSelect := T; end;
procedure TCustomListBoxIntegralHeightR(Self: TCustomListBox; var T: Boolean); begin T := Self.IntegralHeight; end;
procedure TCustomListBoxIntegralHeightW(Self: TCustomListBox; T: Boolean); begin Self.IntegralHeight := T; end;
procedure TCustomListBoxItemHeightR(Self: TCustomListBox; var T: Integer); begin T := Self.ItemHeight; end;
procedure TCustomListBoxItemHeightW(Self: TCustomListBox; T: Integer); begin Self.ItemHeight := T; end;
procedure TCustomListBoxMultiSelectR(Self: TCustomListBox; var T: Boolean); begin T := Self.MultiSelect; end;
procedure TCustomListBoxMultiSelectW(Self: TCustomListBox; T: Boolean); begin Self.MultiSelect := T; end;
procedure TCustomListBoxSortedR(Self: TCustomListBox; var T: Boolean); begin T := Self.Sorted; end;
procedure TCustomListBoxSortedW(Self: TCustomListBox; T: Boolean); begin Self.Sorted := T; end;
procedure TCustomListBoxStyleR(Self: TCustomListBox; var T: TListBoxStyle); begin T := Self.Style; end;
procedure TCustomListBoxStyleW(Self: TCustomListBox; T: TListBoxStyle); begin Self.Style := T; end;


procedure RIRegister_StdCtrlsEx(Cl: TPSRuntimeClassImporter);
begin
  with Cl.FindClass('TCustomEdit') do
  begin
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
  end;
  with Cl.FindClass('TCustomMemo') do
  begin
    RegisterPropertyHelper(@TCustomMemoHorzScrollBarR, @TCustomMemoHorzScrollBarW, 'HorzScrollBar');
    RegisterPropertyHelper(@TCustomMemoVertScrollBarR, @TCustomMemoVertScrollBarW, 'VertScrollBar');
    RegisterPropertyHelper(@TCustomMemoScrollBarsR, @TCustomMemoScrollBarsW, 'ScrollBars');
    RegisterPropertyHelper(@TCustomMemoWantReturnsR, @TCustomMemoWantReturnsW, 'WantReturns');
    RegisterPropertyHelper(@TCustomMemoWantTabsR, @TCustomMemoWantTabsW, 'WantTabs');
    RegisterPropertyHelper(@TCustomMemoWordWrapR, @TCustomMemoWordWrapW, 'WordWrap');
  end;
  with Cl.FindClass('TCustomComboBox') do
  begin
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
  with Cl.FindClass('TCustomCheckBox') do
  begin
    RegisterPropertyHelper(@TCustomCheckBoxAllowGrayedR, @TCustomCheckBoxAllowGrayedW, 'AllowGrayed');
    RegisterPropertyHelper(@TCustomCheckBoxStateR, @TCustomCheckBoxStateW, 'State');
    RegisterEventPropertyHelper(@TCustomCheckBoxOnChangeR, @TCustomCheckBoxOnChangeW, 'OnChange');
  end;
  with Cl.FindClass('TCustomListBox') do
  begin
    RegisterPropertyHelper(@TCustomListBoxBorderStyleR, @TCustomListBoxBorderStyleW, 'BorderStyle');
    RegisterPropertyHelper(@TCustomListBoxColumnsR, @TCustomListBoxColumnsW, 'Columns');
    RegisterPropertyHelper(@TCustomListBoxExtendedSelectR, @TCustomListBoxExtendedSelectW, 'ExtendedSelect');
    RegisterPropertyHelper(@TCustomListBoxIntegralHeightR, @TCustomListBoxIntegralHeightW, 'IntegralHeight');
    RegisterPropertyHelper(@TCustomListBoxItemHeightR, @TCustomListBoxItemHeightW, 'ItemHeight');
    RegisterPropertyHelper(@TCustomListBoxMultiSelectR, @TCustomListBoxMultiSelectW, 'MultiSelect');
    RegisterPropertyHelper(@TCustomListBoxSortedR, @TCustomListBoxSortedW, 'Sorted');
    RegisterPropertyHelper(@TCustomListBoxStyleR, @TCustomListBoxStyleW, 'Style');
  end;
end;


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

procedure TWinControlBorderWidthR(Self: TWinControl; var T: Integer); begin T := Self.BorderWidth; end;
procedure TWinControlBorderWidthW(Self: TWinControl; T: Integer); begin Self.BorderWidth := T; end;
{procedure TWinControlOnUtf8KeyPressR(Self: TWinControl; var T: TUtf8KeyPressEvent); begin T := Self.OnUtf8KeyPress; end;
procedure TWinControlOnUtf8KeyPressW(Self: TWinControl; T: TUtf8KeyPressEvent); begin Self.OnUtf8KeyPress := T; end;}
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

procedure TGraphicControlCanvasR(Self: TGraphicControl; var T: TCanvas); begin T := Self.Canvas; end;

procedure TCustomControlOnPaintR(Self: TCustomControl; var T: TNotifyEvent); begin T := Self.OnPaint; end;
procedure TCustomControlOnPaintW(Self: TCustomControl; T: TNotifyEvent); begin Self.OnPaint := T; end;
procedure TCustomControlCanvasR(Self: TCustomControl; var T: TCanvas); begin T := Self.Canvas; end;
procedure TCustomControlBorderStyleR(Self: TCustomControl; var T: TBorderStyle); begin T := Self.BorderStyle; end;
procedure TCustomControlBorderStyleW(Self: TCustomControl; T: TBorderStyle); begin Self.BorderStyle := T; end;

procedure RIRegister_ControlsEx(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TControlBorderSpacing) do;

  with Cl.FindClass('TControl') do
  begin
    RegisterPropertyHelper(@TControlAnchorsR, @TControlAnchorsW, 'Anchors');
    RegisterPropertyHelper(@TControlAutoSizeR, @TControlAutoSizeW, 'AutoSize');
    RegisterPropertyHelper(@TControlBorderSpacingR, @TControlBorderSpacingW, 'BorderSpacing');
    RegisterPropertyHelper(@TControlCaptionR, @TControlCaptionW, 'Caption');
    RegisterPropertyHelper(@TControlColorR, @TControlColorW, 'Color');
    RegisterPropertyHelper(@TControlFontR, @TControlFontW, 'Font');
    RegisterPropertyHelper(@TControlPopupMenuR, @TControlPopupMenuW, 'PopupMenu');
    RegisterEventPropertyHelper(@TControlOnChangeBoundsR, @TControlOnChangeBoundsW, 'OnChangeBounds');
    RegisterEventPropertyHelper(@TControlOnClickR, @TControlOnClickW, 'OnClick');
    RegisterEventPropertyHelper(@TControlOnResizeR, @TControlOnResizeW, 'OnResize');
    RegisterEventPropertyHelper(@TControlOnShowHintR, @TControlOnShowHintW, 'OnShowHint');
  end;

  with Cl.FindClass('TWinControl') do
  begin
    RegisterPropertyHelper(@TWinControlBorderWidthR, @TWinControlBorderWidthW, 'BorderWidth');
    RegisterEventPropertyHelper(@TWinControlOnEnterR, @TWinControlOnEnterW, 'OnEnter');
    RegisterEventPropertyHelper(@TWinControlOnExitR, @TWinControlOnExitW, 'OnExit');
    RegisterEventPropertyHelper(@TWinControlOnKeyDownR, @TWinControlOnKeyDownW, 'OnKeyDown');
    RegisterEventPropertyHelper(@TWinControlOnKeyPressR, @TWinControlOnKeyPressW, 'OnKeyPress');
    RegisterEventPropertyHelper(@TWinControlOnKeyUpR, @TWinControlOnKeyUpW, 'OnKeyUp');
    //RegisterEventPropertyHelper(@TWinControlOnUtf8KeyPressR, @TWinControlOnUtf8KeyPressW, 'OnUtf8KeyPress');
  end;

  with Cl.FindClass('TGraphicControl') do
  begin
    RegisterPropertyHelper(@TGraphicControlCanvasR, nil, 'Canvas');
  end;

  with Cl.FindClass('TCustomControl') do
  begin
    RegisterPropertyHelper(@TCustomControlBorderStyleR, @TCustomControlBorderStyleW, 'BorderStyle');
    RegisterPropertyHelper(@TCustomControlCanvasR, nil, 'Canvas');
    RegisterEventPropertyHelper(@TCustomControlOnPaintR, @TCustomControlOnPaintW, 'OnPaint');
  end;
end;

procedure TFIELDREADONLY_W(Self: TFIELD; const T: BOOLEAN); begin Self.READONLY := T; end;
procedure TFIELDREADONLY_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.READONLY; end;
procedure TFIELDALIGNMENT_W(Self: TFIELD; const T: TALIGNMENT); begin Self.ALIGNMENT := T; end;
procedure TFIELDALIGNMENT_R(Self: TFIELD; var T: TALIGNMENT); begin T := Self.ALIGNMENT; end;
procedure TFIELDVALUE_W(Self: TFIELD; const T: VARIANT); begin Self.VALUE := T; end;
procedure TFIELDVALUE_R(Self: TFIELD; var T: VARIANT); begin T := Self.VALUE; end;
//procedure TFIELDTEXT_W(Self: TFIELD; const T: String); begin Self.TEXT := T; end;
//procedure TFIELDTEXT_R(Self: TFIELD; var T: String); begin T := Self.TEXT; end;
procedure TFIELDOLDVALUE_R(Self: TFIELD; var T: VARIANT); begin T := Self.OLDVALUE; end;
procedure TFIELDISNULL_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.ISNULL; end;
procedure TFIELDDATATYPE_R(Self: TFIELD; var T: TFIELDTYPE); begin T := Self.DATATYPE; end;
procedure TFIELDCANMODIFY_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.CANMODIFY; end;
procedure TFIELDASVARIANT_W(Self: TFIELD; const T: VARIANT); begin Self.ASVARIANT := T; end;
procedure TFIELDASVARIANT_R(Self: TFIELD; var T: VARIANT); begin T := Self.ASVARIANT; end;
procedure TFIELDASSTRING_W(Self: TFIELD; const T: String); begin Self.ASSTRING := T; end;
procedure TFIELDASSTRING_R(Self: TFIELD; var T: String); begin T := Self.ASSTRING; end;
procedure TFIELDASINTEGER_W(Self: TFIELD; const T: LONGINT); begin Self.ASINTEGER := T; end;
procedure TFIELDASINTEGER_R(Self: TFIELD; var T: LONGINT); begin T := Self.ASINTEGER; end;
procedure TFIELDASFLOAT_W(Self: TFIELD; const T: DOUBLE); begin Self.ASFLOAT := T; end;
procedure TFIELDASFLOAT_R(Self: TFIELD; var T: DOUBLE); begin T := Self.ASFLOAT; end;
procedure TFIELDASDATETIME_W(Self: TFIELD; const T: TDATETIME); begin Self.ASDATETIME := T; end;
procedure TFIELDASDATETIME_R(Self: TFIELD; var T: TDATETIME); begin T := Self.ASDATETIME; end;
procedure TFIELDASCURRENCY_W(Self: TFIELD; const T: CURRENCY); begin Self.ASCURRENCY := T; end;
procedure TFIELDASCURRENCY_R(Self: TFIELD; var T: CURRENCY); begin T := Self.ASCURRENCY; end;
procedure TFIELDASBOOLEAN_W(Self: TFIELD; const T: BOOLEAN); begin Self.ASBOOLEAN := T; end;
procedure TFIELDASBOOLEAN_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.ASBOOLEAN; end;
{procedure TFIELDDISPLAYFORMAT_R(Self: TFIELD; var T: String);
begin
  if Self is TNumericField then T := TNumericField(Self).DisplayFormat
  else if Self is TDateTimeField then T := TDateTimeField(Self).DisplayFormat
  else T := '';
end;
procedure TFIELDDISPLAYFORMAT_W(Self: TFIELD; T: String);
begin
  if Self is TNumericField then TNumericField(Self).DisplayFormat := T
  else if Self is TDateTimeField then TDateTimeField(Self).DisplayFormat := T;
end;
procedure TFIELDEDITFORMAT_R(Self: TFIELD; var T: String);
begin
  if Self is TNumericField then T := TNumericField(Self).EditFormat
  else T := '';
end;
procedure TFIELDEDITFORMAT_W(Self: TFIELD; T: String);
begin
  if Self is TNumericField then TNumericField(Self).EditFormat := T
end;  }
//procedure TFIELDINSERTSTATE_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.DataSet.State = dsInsert; end;
//procedure TFIELDEDITSTATE_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.DataSet.State = dsEdit; end;
procedure TFIELDSTATE_R(Self: TFIELD; var T: TDataSetState); begin T := Self.DataSet.State; end;
procedure TFIELDFIELDNAME_R(Self: TFIELD; var T: String); begin T := Self.FieldName; end;

procedure RIRegister_TFIELD(Cl: TPSRuntimeClassImporter);
Begin
  with Cl.Add(TFIELD) do
  begin
    RegisterVirtualMethod(@TFIELD.CLEAR, 'Clear');
    RegisterMethod(@TFIELD.FOCUSCONTROL, 'FocusControl');
    //RegisterVirtualMethod(@TFIELD.ISVALIDCHAR, 'IsValidChar');
    RegisterPropertyHelper(@TFIELDASBOOLEAN_R,@TFIELDASBOOLEAN_W,'AsBoolean');
    RegisterPropertyHelper(@TFIELDASCURRENCY_R,@TFIELDASCURRENCY_W,'AsCurrency');
    RegisterPropertyHelper(@TFIELDASDATETIME_R,@TFIELDASDATETIME_W,'AsDateTime');
    RegisterPropertyHelper(@TFIELDASFLOAT_R,@TFIELDASFLOAT_W,'AsFloat');
    RegisterPropertyHelper(@TFIELDASINTEGER_R,@TFIELDASINTEGER_W,'AsInteger');
    RegisterPropertyHelper(@TFIELDASSTRING_R,@TFIELDASSTRING_W,'AsString');
    RegisterPropertyHelper(@TFIELDASVARIANT_R,@TFIELDASVARIANT_W,'AsVariant');
    RegisterPropertyHelper(@TFIELDCANMODIFY_R,nil,'CanModify');
    RegisterPropertyHelper(@TFIELDDATATYPE_R,nil,'DataType');
    RegisterPropertyHelper(@TFIELDISNULL_R,nil,'IsNull');
    RegisterPropertyHelper(@TFIELDOLDVALUE_R,nil,'OldValue');
    //RegisterPropertyHelper(@TFIELDTEXT_R,@TFIELDTEXT_W,'Text');
    RegisterPropertyHelper(@TFIELDVALUE_R,@TFIELDVALUE_W,'Value');
    RegisterPropertyHelper(@TFIELDALIGNMENT_R,@TFIELDALIGNMENT_W,'Alignment');
    RegisterPropertyHelper(@TFIELDREADONLY_R,@TFIELDREADONLY_W,'ReadOnly');
    //RegisterPropertyHelper(@TFIELDDISPLAYFORMAT_R, @TFIELDDISPLAYFORMAT_W, 'DisplayFormat');
    //RegisterPropertyHelper(@TFIELDEDITFORMAT_R, @TFIELDEDITFORMAT_W, 'EditFormat');
    RegisterPropertyHelper(@TFIELDSTATE_R, nil, 'State');
    RegisterPropertyHelper(@TFIELDFIELDNAME_R, nil, 'FieldName');
    {RegisterPropertyHelper(@TFIELDINSERTSTATE_R, nil, 'InsertState');
    RegisterPropertyHelper(@TFIELDEDITSTATE_R, nil, 'EditState'); }
  end;
end;

procedure RIRegister_Dialogs(Cl: TPSRuntimeClassImporter);
begin
  with CL.Add(TCommonDialog) do
  begin
    RegisterVirtualMethod(@TCommonDialog.Execute, 'Execute');
  end;

  with Cl.Add(TFileDialog) do
  begin

  end;

  with Cl.Add(TOpenDialog) do
  begin

  end;

  with Cl.Add(TSaveDialog) do
  begin

  end;

  with Cl.Add(TSelectDirectoryDialog) do
  begin

  end;

  with Cl.Add(TPrintDialog) do
  begin

  end;
end;

procedure RIRegister_dxCtrls(Cl: TPSRuntimeClassImporter);
begin
  RIRegister_ControlsEx(Cl);
  RIRegister_StdCtrlsEx(Cl);
  RIRegister_TreeView(Cl);
  RIRegister_TField(Cl);
  RIRegister_dxLabel(Cl);
  //RIRegister_CustomMaskEdit(Cl);
  RIRegister_MaskEdit(Cl);
  RIRegister_DBEdit(Cl);
  RIRegister_dxEdit(Cl);
  RIRegister_CustomDBEditButton(Cl);
  RIRegister_DBCalcEdit(Cl);
  RIRegister_dxCalcEdit(Cl);
  RIRegister_DBDateEdit(Cl);
  RIRegister_dxDateEdit(Cl);
  RIRegister_DBMemo(Cl);
  RIRegister_dxMemo(Cl);
  RIRegister_DBCheckBox(Cl);
  RIRegister_dxCheckBox(Cl);
  RIRegister_CustomDBComboBox(Cl);
  RIRegister_DBComboBox(Cl);
  RIRegister_dxComboBox(Cl);
  RIRegister_dxLookupComboBox(Cl);
  RIRegister_dxImage(Cl);
  RIRegister_DBGrid(Cl);
  RIRegister_dxGrid(Cl);
  RIRegister_dxGroupBox(Cl);
  RIRegister_ImageList(Cl);
  RIRegister_PageControl(Cl);
  RIRegister_dxPageControl(Cl);
  RIRegister_dxShape(Cl);
  RIRegister_dxObjectField(Cl);
  RIRegister_DBTimeEdit(Cl);
  RIRegister_dxTimeEdit(Cl);
  RIRegister_dxCounter(Cl);
  RIRegister_dxButton(Cl);
  RIRegister_dxDBImage(Cl);
  RIRegister_dxFile(Cl);
  RIRegister_More(Cl);
  RIRegister_dxForm(Cl);
  RIRegister_dxQueryGrid(Cl);
  RIRegister_dxSQLQuery(Cl);
  RIRegister_KGrid(Cl);
  RIRegister_ReportWindow(Cl);
  RIRegister_Dialogs(Cl);
  RIRegister_IniFiles(Cl);
  RIRegister_Clipboard(Cl);
end;

procedure TTreeNodeAbsoluteIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.AbsoluteIndex; end;
procedure TTreeNodeCount_R(Self: TTreeNode; var T: Integer); begin T := Self.Count; end;
procedure TTreeNodeCut_R(Self: TTreeNode; var T: Boolean); begin T := Self.Cut; end;
procedure TTreeNodeCut_W(Self: TTreeNode; T: Boolean); begin Self.Cut := T; end;
procedure TTreeNodeData_R(Self: TTreeNode; var T: TObject); begin T := TObject(Self.Data); end;
procedure TTreeNodeData_W(Self: TTreeNode; T: TObject); begin Self.Data := T; end;
procedure TTreeNodeDeleting_R(Self: TTreeNode; var T: Boolean); begin T := Self.Deleting; end;
procedure TTreeNodeExpanded_R(Self: TTreeNode; var T: Boolean); begin T := Self.Expanded; end;
procedure TTreeNodeExpanded_W(Self: TTreeNode; T: Boolean); begin Self.Expanded := T; end;
procedure TTreeNodeFocused_R(Self: TTreeNode; var T: Boolean); begin T := Self.Focused; end;
procedure TTreeNodeFocused_W(Self: TTreeNode; T: Boolean); begin Self.Focused := T; end;
procedure TTreeNodeHandle_R(Self: TTreeNode; var T: THandle); begin T := Self.Handle; end;
procedure TTreeNodeHasChildren_R(Self: TTreeNode; var T: Boolean); begin T := Self.HasChildren; end;
procedure TTreeNodeHasChildren_W(Self: TTreeNode; T: Boolean); begin Self.HasChildren := T; end;
procedure TTreeNodeHeight_R(Self: TTreeNode; var T: Integer); begin T := Self.Height; end;
procedure TTreeNodeHeight_W(Self: TTreeNode; T: Integer); begin Self.Height := T; end;
procedure TTreeNodeImageIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.ImageIndex; end;
procedure TTreeNodeImageIndex_W(Self: TTreeNode; T: Integer); begin Self.ImageIndex := T; end;
procedure TTreeNodeIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.Index; end;
procedure TTreeNodeIndex_W(Self: TTreeNode; T: Integer); begin Self.Index := T; end;
procedure TTreeNodeIsFullHeightVisible_R(Self: TTreeNode; var T: Boolean); begin T := Self.IsFullHeightVisible; end;
procedure TTreeNodeIsVisible_R(Self: TTreeNode; var T: Boolean); begin T := Self.IsVisible; end;
procedure TTreeNodeItems_R(Self: TTreeNode; var T: TTreeNode; I: Integer); begin T := Self.Items[I]; end;
procedure TTreeNodeItems_W(Self: TTreeNode; T: TTreeNode; I: Integer); begin Self.Items[I] := T; end;
procedure TTreeNodeLevel_R(Self: TTreeNode; var T: Integer); begin T := Self.Level; end;
procedure TTreeNodeMultiSelected_R(Self: TTreeNode; var T: Boolean); begin T := Self.MultiSelected; end;
procedure TTreeNodeMultiSelected_W(Self: TTreeNode; T: Boolean); begin Self.MultiSelected := T; end;
procedure TTreeNodeNodeEffect_R(Self: TTreeNode; var T: TGraphicsDrawEffect); begin T := Self.NodeEffect; end;
procedure TTreeNodeNodeEffect_W(Self: TTreeNode; T: TGraphicsDrawEffect); begin Self.NodeEffect := T; end;
procedure TTreeNodeOverlayIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.OverlayIndex; end;
procedure TTreeNodeOverlayIndex_W(Self: TTreeNode; T: Integer); begin Self.OverlayIndex := T; end;
procedure TTreeNodeOwner_R(Self: TTreeNode; var T: TTreeNodes); begin T := Self.Owner; end;
procedure TTreeNodeParent_R(Self: TTreeNode; var T: TTreeNode); begin T := Self.Parent; end;
procedure TTreeNodeSelected_R(Self: TTreeNode; var T: Boolean); begin T := Self.Selected; end;
procedure TTreeNodeSelected_W(Self: TTreeNode; T: Boolean); begin Self.Selected := T; end;
procedure TTreeNodeSelectedIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.SelectedIndex; end;
procedure TTreeNodeSelectedIndex_W(Self: TTreeNode; T: Integer); begin Self.SelectedIndex := T; end;
procedure TTreeNodeStateIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.StateIndex; end;
procedure TTreeNodeStateIndex_W(Self: TTreeNode; T: Integer); begin Self.StateIndex := T; end;
procedure TTreeNodeStates_R(Self: TTreeNode; var T: TNodeStates); begin T := Self.States; end;
procedure TTreeNodeSubTreeCount_R(Self: TTreeNode; var T: Integer); begin T := Self.SubTreeCount; end;
procedure TTreeNodeText_R(Self: TTreeNode; var T: String); begin T := Self.Text; end;
procedure TTreeNodeText_W(Self: TTreeNode; T: String); begin Self.Text := T; end;
procedure TTreeNodeTop_R(Self: TTreeNode; var T: Integer); begin T := Self.Top; end;
procedure TTreeNodeTreeNodes_R(Self: TTreeNode; var T: TTreeNodes); begin T := Self.TreeNodes; end;
procedure TTreeNodeTreeView_R(Self: TTreeNode; var T: TTreeView); begin T := TTreeView(Self.TreeView); end;
procedure TTreeNodeVisible_R(Self: TTreeNode; var T: Boolean); begin T := Self.Visible; end;
procedure TTreeNodeVisible_W(Self: TTreeNode; T: Boolean); begin Self.Visible := T; end;

procedure TTreeNodesCount_R(Self: TTreeNodes; var T: Integer); begin T := Self.Count; end;
procedure TTreeNodesItem_R(Self: TTreeNodes; var T: TTreeNode; I: Integer); begin T := Self.Item[I]; end;
procedure TTreeNodesKeepCollapsedNodes_R(Self: TTreeNodes; var T: Boolean); begin T := Self.KeepCollapsedNodes; end;
procedure TTreeNodesKeepCollapsedNodes_W(Self: TTreeNodes; T: Boolean); begin Self.KeepCollapsedNodes := T; end;
procedure TTreeNodesOwner_R(Self: TTreeNodes; var T: TTreeView); begin T := TTreeView(Self.Owner); end;
procedure TTreeNodesSelectionCount_R(Self: TTreeNodes; var T: Integer); begin T := Self.SelectionCount; end;
procedure TTreeNodesTopLvlCount_R(Self: TTreeNodes; var T: Integer); begin T := Self.TopLvlCount; end;
procedure TTreeNodesTopLvlItems_R(Self: TTreeNodes; var T: TTreeNode; I: Integer); begin T := Self.TopLvlItems[I]; end;

procedure RIRegister_TreeView(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TTreeNode) do
  begin
    RegisterVirtualConstructor(@TTreeNode.Create, 'Create');
    RegisterMethod(@TTreeNode.AlphaSort, 'AlphaSort');
    RegisterMethod(@TTreeNode.Bottom, 'Bottom');
    RegisterMethod(@TTreeNode.BottomExpanded, 'BottomExpanded');
    RegisterMethod(@TTreeNode.CustomSort, 'CustomSort');
    RegisterMethod(@TTreeNode.DefaultTreeViewSort, 'DefaultTreeViewSort');
    RegisterMethod(@TTreeNode.DisplayExpandSignLeft, 'DisplayExpandSignLeft');
    RegisterMethod(@TTreeNode.DisplayExpandSignRect, 'DisplayExpandSignRect');
    RegisterMethod(@TTreeNode.DisplayExpandSignRight, 'DisplayExpandSignRight');
    RegisterMethod(@TTreeNode.DisplayIconLeft, 'DisplayIconLeft');
    RegisterMethod(@TTreeNode.DisplayRect, 'DisplayRect');
    RegisterMethod(@TTreeNode.DisplayStateIconLeft, 'DisplayStateIconLeft');
    RegisterMethod(@TTreeNode.DisplayTextLeft, 'DisplayTextLeft');
    RegisterMethod(@TTreeNode.DisplayTextRight, 'DisplayTextRight');
    RegisterMethod(@TTreeNode.EditText, 'EditText');
    RegisterMethod(@TTreeNode.FindNode, 'FindNode');
    RegisterMethod(@TTreeNode.GetFirstChild, 'GetFirstChild');
    RegisterMethod(@TTreeNode.GetFirstVisibleChild, 'GetFirstVisibleChild');
    RegisterMethod(@TTreeNode.GetHandle, 'GetHandle');
    RegisterMethod(@TTreeNode.GetLastChild, 'GetLastChild');
    RegisterMethod(@TTreeNode.GetLastSibling, 'GetLastSibling');
    RegisterMethod(@TTreeNode.GetLastSubChild, 'GetLastSubChild');
    RegisterMethod(@TTreeNode.GetLastVisibleChild, 'GetLastVisibleChild');
    RegisterMethod(@TTreeNode.GetNext, 'GetNext');
    RegisterMethod(@TTreeNode.GetNextChild, 'GetNextChild');
    RegisterMethod(@TTreeNode.GetNextExpanded, 'GetNextExpanded');
    RegisterMethod(@TTreeNode.GetNextMultiSelected, 'GetNextMultiSelected');
    RegisterMethod(@TTreeNode.GetNextSibling, 'GetNextSibling');
    RegisterMethod(@TTreeNode.GetNextSkipChildren, 'GetNextSkipChildren');
    RegisterMethod(@TTreeNode.GetNextVisible, 'GetNextVisible');
    RegisterMethod(@TTreeNode.GetNextVisibleSibling, 'GetNextVisibleSibling');
    RegisterMethod(@TTreeNode.GetParentNodeOfAbsoluteLevel, 'GetParentNodeOfAbsoluteLevel');
    RegisterMethod(@TTreeNode.GetPrev, 'GetPrev');
    RegisterMethod(@TTreeNode.GetPrevChild, 'GetPrevChild');
    RegisterMethod(@TTreeNode.GetPrevExpanded, 'GetPrevExpanded');
    RegisterMethod(@TTreeNode.GetPrevMultiSelected, 'GetPrevMultiSelected');
    RegisterMethod(@TTreeNode.GetPrevSibling, 'GetPrevSibling');
    RegisterMethod(@TTreeNode.GetPrevVisible, 'GetPrevVisible');
    RegisterMethod(@TTreeNode.GetPrevVisibleSibling, 'GetPrevVisibleSibling');
    RegisterMethod(@TTreeNode.GetTextPath, 'GetTextPath');
    RegisterMethod(@TTreeNode.HasAsParent, 'HasAsParent');
    RegisterMethod(@TTreeNode.IndexOf, 'IndexOf');
    RegisterMethod(@TTreeNode.IndexOfText, 'IndexOfText');
    RegisterMethod(@TTreeNode.Collapse, 'Collapse');
    RegisterMethod(@TTreeNode.Delete, 'Delete');
    RegisterMethod(@TTreeNode.DeleteChildren, 'DeleteChildren');
    RegisterMethod(@TTreeNode.EndEdit, 'EndEdit');
    RegisterMethod(@TTreeNode.Expand, 'Expand');
    RegisterMethod(@TTreeNode.ExpandParents, 'ExpandParents');
    RegisterMethod(@TTreeNode.FreeAllNodeData, 'FreeAllNodeData');
    RegisterMethod(@TTreeNode.MakeVisible, 'MakeVisible');
    RegisterVirtualMethod(@TTreeNode.MoveTo, 'MoveTo');
    RegisterMethod(@TTreeNode.MultiSelectGroup, 'MultiSelectGroup');
    RegisterMethod(@TTreeNode.Update, 'Update');

    RegisterPropertyHelper(@TTreeNodeAbsoluteIndex_R, nil, 'AbsoluteIndex');
    RegisterPropertyHelper(@TTreeNodeCount_R, nil, 'Count');
    RegisterPropertyHelper(@TTreeNodeCut_R, @TTreeNodeCut_W, 'Cut');
    RegisterPropertyHelper(@TTreeNodeData_R, @TTreeNodeData_W, 'Data');
    RegisterPropertyHelper(@TTreeNodeDeleting_R, nil, 'Deleting');
    RegisterPropertyHelper(@TTreeNodeExpanded_R, @TTreeNodeExpanded_W, 'Expanded');
    RegisterPropertyHelper(@TTreeNodeFocused_R, @TTreeNodeFocused_W, 'Focused');
    RegisterPropertyHelper(@TTreeNodeHandle_R, nil, 'Handle');
    RegisterPropertyHelper(@TTreeNodeHasChildren_R, @TTreeNodeHasChildren_W, 'HasChildren');
    RegisterPropertyHelper(@TTreeNodeHeight_R, @TTreeNodeHeight_W, 'Height');
    RegisterPropertyHelper(@TTreeNodeImageIndex_R, @TTreeNodeImageIndex_W, 'ImageIndex');
    RegisterPropertyHelper(@TTreeNodeIndex_R, @TTreeNodeIndex_W, 'Index');
    RegisterPropertyHelper(@TTreeNodeIsFullHeightVisible_R, nil, 'IsFullHeightVisible');
    RegisterPropertyHelper(@TTreeNodeIsVisible_R, nil, 'IsVisible');
    RegisterPropertyHelper(@TTreeNodeItems_R, @TTreeNodeItems_W, 'Items');
    RegisterPropertyHelper(@TTreeNodeLevel_R, nil, 'Level');
    RegisterPropertyHelper(@TTreeNodeMultiSelected_R, @TTreeNodeMultiSelected_W, 'MultiSelected');
    RegisterPropertyHelper(@TTreeNodeNodeEffect_R, @TTreeNodeNodeEffect_W, 'NodeEffect');
    RegisterPropertyHelper(@TTreeNodeOverlayIndex_R, @TTreeNodeOverlayIndex_W, 'OverlayIndex');
    RegisterPropertyHelper(@TTreeNodeOwner_R, nil, 'Owner');
    RegisterPropertyHelper(@TTreeNodeParent_R, nil, 'Parent');
    RegisterPropertyHelper(@TTreeNodeSelected_R, @TTreeNodeSelected_W, 'Selected');
    RegisterPropertyHelper(@TTreeNodeSelectedIndex_R, @TTreeNodeSelectedIndex_W, 'SelectedIndex');
    RegisterPropertyHelper(@TTreeNodeStateIndex_R, @TTreeNodeStateIndex_W, 'StateIndex');
    RegisterPropertyHelper(@TTreeNodeStates_R, nil, 'States');
    RegisterPropertyHelper(@TTreeNodeSubTreeCount_R, nil, 'SubTreeCount');
    RegisterPropertyHelper(@TTreeNodeText_R, @TTreeNodeText_W, 'Text');
    RegisterPropertyHelper(@TTreeNodeTop_R, nil, 'Top');
    RegisterPropertyHelper(@TTreeNodeTreeNodes_R, nil, 'TreeNodes');
    RegisterPropertyHelper(@TTreeNodeTreeView_R, nil, 'TreeView');
    RegisterPropertyHelper(@TTreeNodeVisible_R, @TTreeNodeVisible_W, 'Visible');
  end;

  with Cl.Add(TTreeNodes) do
  begin
    RegisterConstructor(@TTreeNodes.Create, 'Create');
    RegisterMethod(@TTreeNodes.Add, 'Add');
    RegisterMethod(@TTreeNodes.AddChild, 'AddChild');
    RegisterMethod(@TTreeNodes.AddChildFirst, 'AddChildFirst');
    RegisterMethod(@TTreeNodes.AddChildObject, 'AddChildObject');
    RegisterMethod(@TTreeNodes.AddChildObjectFirst, 'AddChildObjectFirst');
    RegisterMethod(@TTreeNodes.AddFirst, 'AddFirst');
    RegisterMethod(@TTreeNodes.AddObject, 'AddObject');
    RegisterMethod(@TTreeNodes.AddObjectFirst, 'AddObjectFirst');
    RegisterMethod(@TTreeNodes.FindNodeWithData, 'FindNodeWithData');
    RegisterMethod(@TTreeNodes.FindNodeWithText, 'FindNodeWithText');
    RegisterMethod(@TTreeNodes.FindNodeWithTextPath, 'FindNodeWithTextPath');
    RegisterMethod(@TTreeNodes.FindTopLvlNode, 'FindTopLvlNode');
    RegisterMethod(@TTreeNodes.GetFirstNode, 'GetFirstNode');
    RegisterMethod(@TTreeNodes.GetFirstVisibleNode, 'GetFirstVisibleNode');
    RegisterMethod(@TTreeNodes.GetLastExpandedSubNode, 'GetLastExpandedSubNode');
    RegisterMethod(@TTreeNodes.GetLastNode, 'GetLastNode');
    RegisterMethod(@TTreeNodes.GetLastSubNode, 'GetLastSubNode');
    RegisterMethod(@TTreeNodes.GetLastVisibleNode, 'GetLastVisibleNode');
    RegisterMethod(@TTreeNodes.GetSelections, 'GetSelections');
    RegisterMethod(@TTreeNodes.Insert, 'Insert');
    RegisterMethod(@TTreeNodes.InsertBehind, 'InsertBehind');
    RegisterMethod(@TTreeNodes.InsertObject, 'InsertObject');
    RegisterMethod(@TTreeNodes.InsertObjectBehind, 'InsertObjectBehind');
    RegisterMethod(@TTreeNodes.IsMultiSelection, 'IsMultiSelection');
    RegisterMethod(@TTreeNodes.BeginUpdate, 'BeginUpdate');
    RegisterMethod(@TTreeNodes.Clear, 'Clear');
    RegisterMethod(@TTreeNodes.ClearMultiSelection, 'ClearMultiSelection');
    RegisterMethod(@TTreeNodes.Delete, 'Delete');
    RegisterMethod(@TTreeNodes.EndUpdate, 'EndUpdate');
    RegisterMethod(@TTreeNodes.SelectionsChanged, 'SelectionsChanged');
    RegisterMethod(@TTreeNodes.SelectOnlyThis, 'SelectOnlyThis');
    RegisterMethod(@TTreeNodes.SortTopLevelNodes, 'SortTopLevelNodes');
    RegisterPropertyHelper(@TTreeNodesCount_R, nil, 'Count');
    RegisterPropertyHelper(@TTreeNodesItem_R, nil, 'Item');
    RegisterPropertyHelper(@TTreeNodesKeepCollapsedNodes_R, @TTreeNodesKeepCollapsedNodes_W, 'KeepCollapsedNodes');
    RegisterPropertyHelper(@TTreeNodesOwner_R, nil, 'Owner');
    RegisterPropertyHelper(@TTreeNodesSelectionCount_R, nil, 'SelectionCount');
    RegisterPropertyHelper(@TTreeNodesTopLvlCount_R, nil, 'TopLvlCount');
    RegisterPropertyHelper(@TTreeNodesTopLvlItems_R, nil, 'TopLvlItems');
  end;

  with Cl.Add(TTreeView) do
  begin
    RegisterMethod(@TTreeView.AlphaSort, 'AlphaSort');
    RegisterVirtualMethod(@TTreeView.ClearSelection, 'ClearSelection');
    RegisterMethod(@TTreeView.CustomSort, 'CustomSort');
    RegisterMethod(@TTreeView.DefaultTreeViewSort, 'DefaultTreeViewSort');
    RegisterMethod(@TTreeView.GetHitTestInfoAt, 'GetHitTestInfoAt');
    RegisterMethod(@TTreeView.GetNodeAt, 'GetNodeAt');
    RegisterMethod(@TTreeView.GetInsertMarkAt, 'GetInsertMarkAt');
    RegisterMethod(@TTreeView.SetInsertMark, 'SetInsertMark');
    RegisterVirtualMethod(@TTreeView.SetInsertMarkAt, 'SetInsertMarkAt');
    RegisterMethod(@TTreeView.IsEditing, 'IsEditing');
    RegisterMethod(@TTreeView.BeginUpdate, 'BeginUpdate');
    RegisterMethod(@TTreeView.EndUpdate, 'EndUpdate');
    RegisterMethod(@TTreeView.FullCollapse, 'FullCollapse');
    RegisterMethod(@TTreeView.FullExpand, 'FullExpand');
    RegisterMethod(@TTreeView.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TTreeView.LoadFromStream, 'LoadFromStream');
    RegisterMethod(@TTreeView.SavetoFile, 'SaveToFile');
    RegisterMethod(@TTreeView.SavetoStream, 'SaveToStream');
    RegisterMethod(@TTreeView.LockSelectionChangeEvent, 'LockSelectionChangeEvent');
    RegisterMethod(@TTreeView.UnLockSelectionChangeEvent, 'UnLockSelectionChangeEvent');
    RegisterMethod(@TTreeView.GetFirstMultiSelected, 'GetFirstMultiSelected');
    RegisterMethod(@TTreeView.GetLastMultiSelected, 'GetLastMultiSelected');
    RegisterMethod(@TTreeView.Select, 'Select');
    RegisterMethod(@TTreeView.SelectionVisible, 'SelectionVisible');
    RegisterMethod(@TTreeView.MakeSelectionVisible, 'MakeSelectionVisible');
    RegisterMethod(@TTreeView.ClearInvisibleSelection, 'ClearInvisibleSelection');
    RegisterMethod(@TTreeView.StoreCurrentSelection, 'StoreCurrentSelection');
    RegisterMethod(@TTreeView.ApplyStoredSelection, 'ApplyStoredSelection');
    RegisterMethod(@TTreeView.MoveToNextNode, 'MoveToNextNode');
    RegisterMethod(@TTreeView.MoveToPrevNode, 'MoveToPrevNode');
  end;
end;

procedure RIRegister_Functions(Exec: TPSExec);
begin
  Exec.RegisterDelphiFunction(@MessageBox, 'MsgBox', cdRegister);
  Exec.RegisterDelphiFunction(@Debug, 'Debug', cdRegister);

  Exec.RegisterDelphiFunction(@UTF8Length, 'Utf8Length', cdRegister);
  Exec.RegisterDelphiFunction(@UTF8Pos, 'Utf8Pos', cdRegister);
  Exec.RegisterDelphiFunction(@UTF8Copy, 'Utf8Copy', cdRegister);
  Exec.RegisterDelphiFunction(@UTF8Delete, 'Utf8Delete', cdRegister);
  Exec.RegisterDelphiFunction(@UTF8Insert, 'Utf8Insert', cdRegister);
  Exec.RegisterDelphiFunction(@UTF8StringReplace, 'Utf8StringReplace', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8LowerCase, 'Utf8LowerCase', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8UpperCase, 'Utf8UpperCase', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8CompareStr, 'Utf8CompareStr', cdRegister);
  Exec.RegisterDelphiFunction(@UTF8CompareText, 'Utf8CompareText', cdRegister);
  Exec.RegisterDelphiFunction(@Utf8ToWinCP, 'Utf8ToWinCP', cdRegister);
  Exec.RegisterDelphiFunction(@WinCPToUtf8, 'WinCPToUtf8', cdRegister);
  Exec.RegisterDelphiFunction(@UTF8ToUTF16, 'Utf8ToUtf16', cdRegister);
  Exec.RegisterDelphiFunction(@UTF16ToUTF8, 'Utf16ToUtf8', cdRegister);

  Exec.RegisterDelphiFunction(@FileExistsUtf8, 'FileExists', cdRegister);
  Exec.RegisterDelphiFunction(@FileAgeUtf8, 'FileAge', cdRegister);
  Exec.RegisterDelphiFunction(@DirectoryExistsUtf8, 'DirectoryExists', cdRegister);
  Exec.RegisterDelphiFunction(@ExpandFileNameUtf8, 'ExpandFileName', cdRegister);
  Exec.RegisterDelphiFunction(@FileSetDateUtf8, 'FileSetDate', cdRegister);
  Exec.RegisterDelphiFunction(@FileGetAttrUtf8, 'FileGetAttr', cdRegister);
  Exec.RegisterDelphiFunction(@FileSetAttrUtf8, 'FileSetAttr', cdRegister);
  Exec.RegisterDelphiFunction(@DeleteFileUtf8, 'DeleteFile', cdRegister);
  Exec.RegisterDelphiFunction(@RenameFileUtf8, 'RenameFile', cdRegister);
  Exec.RegisterDelphiFunction(@GetCurrentDirUtf8, 'GetCurrentDir', cdRegister);
  Exec.RegisterDelphiFunction(@CreateDirUtf8, 'CreateDir', cdRegister);
  Exec.RegisterDelphiFunction(@RemoveDirUtf8, 'RemoveDir', cdRegister);
  Exec.RegisterDelphiFunction(@ForceDirectoriesUtf8, 'ForceDirectories', cdRegister);
  Exec.RegisterDelphiFunction(@MyCopyFile, 'CopyFile', cdRegister);
  Exec.RegisterDelphiFunction(@MyFindAllFiles, 'FindAllFiles', cdRegister);
  Exec.RegisterDelphiFunction(@MyFindAllDirectories, 'FindAllDirectories', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileName, 'ExtractFileName', cdRegister);
  Exec.RegisterDelphiFunction(@ExtractFileNameOnly, 'ExtractFileNameOnly', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileExt, 'ExtractFileExt', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFilePath, 'ExtractFilePath', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileDrive, 'ExtractFileDrive', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileDir, 'ExtractFileDir', cdRegister);
  Exec.RegisterDelphiFunction(@MyChangeFileExt, 'ChangeFileExt', cdRegister);
  Exec.RegisterDelphiFunction(@MyIncludeTrailingPathDelimiter, 'IncludeTrailingPathDelimiter', cdRegister);
  Exec.RegisterDelphiFunction(@MyExcludeLeadingPathDelimiter, 'ExcludeLeadingPathDelimiter', cdRegister);
  Exec.RegisterDelphiFunction(@MyGetTempFilename, 'GetTempFileName', cdRegister);
  Exec.RegisterDelphiFunction(@MyGetTempDir, 'GetTempDir', cdRegister);
  Exec.RegisterDelphiFunction(@ShellExec, 'ShellExecute', cdRegister);
  Exec.RegisterDelphiFunction(@FileDateToDateTime, 'FileDateToDateTime', cdRegister);
  Exec.RegisterDelphiFunction(@DateTimeToFileDate, 'DateTimeToFileDate', cdRegister);

  Exec.RegisterDelphiFunction(@CreateForm, 'CreateForm', cdRegister);
  Exec.RegisterDelphiFunction(@DestroyForm, 'DestroyForm', cdRegister);
  Exec.RegisterDelphiFunction(@GetForms, 'GetForms', cdRegister);

  Exec.RegisterDelphiFunction(@FreeAndNil, 'FreeAndNil', cdRegister);
  Exec.RegisterDelphiFunction(@Random, 'Random', cdRegister);

  Exec.RegisterDelphiFunction(@MsgDlg, 'MessageDlg', cdRegister);
  Exec.RegisterDelphiFunction(@InputBox, 'InputBox', cdRegister);
  Exec.RegisterDelphiFunction(@EvalExpr, 'EvalExpr', cdRegister);

  Exec.RegisterDelphiFunction(@DCount, 'DCount', cdRegister);
  Exec.RegisterDelphiFunction(@DSum, 'DSum', cdRegister);
  Exec.RegisterDelphiFunction(@DAvg, 'DAvg', cdRegister);
  Exec.RegisterDelphiFunction(@DMax, 'DMax', cdRegister);
  Exec.RegisterDelphiFunction(@DMin, 'DMin', cdRegister);
  Exec.RegisterDelphiFunction(@DMerge, 'DMerge', cdRegister);
  Exec.RegisterDelphiFunction(@ToWordsRu, 'ToWords', cdRegister);
  Exec.RegisterDelphiFunction(@RurToWords, 'RurToWords', cdRegister);
  Exec.RegisterDelphiFunction(@Nz, 'Nz', cdRegister);
  Exec.RegisterDelphiFunction(@MathRound, 'RoundTo', cdRegister);
  Exec.RegisterDelphiFunction(@_MyFrac, 'Frac', cdRegister);
  Exec.RegisterDelphiFunction(@Power, 'Power', cdRegister);
  Exec.RegisterDelphiFunction(@YearsBetweenEx, 'YearsBetween', cdRegister);
  Exec.RegisterDelphiFunction(@MonthsBetweenEx, 'MonthsBetween', cdRegister);
  Exec.RegisterDelphiFunction(@WeeksBetweenEx, 'WeeksBetween', cdRegister);
  Exec.RegisterDelphiFunction(@DaysBetweenEx, 'DaysBetween', cdRegister);
  Exec.RegisterDelphiFunction(@HoursBetweenEx, 'HoursBetween', cdRegister);
  Exec.RegisterDelphiFunction(@MinutesBetweenEx, 'MinutesBetween', cdRegister);
  Exec.RegisterDelphiFunction(@SecondsBetweenEx, 'SecondsBetween', cdRegister);
  Exec.RegisterDelphiFunction(@IncYear, 'AddYear', cdRegister);
  Exec.RegisterDelphiFunction(@IncMonth, 'AddMonth', cdRegister);
  Exec.RegisterDelphiFunction(@IncWeek, 'AddWeek', cdRegister);
  Exec.RegisterDelphiFunction(@IncDay, 'AddDay', cdRegister);
  Exec.RegisterDelphiFunction(@IncHour, 'AddHour', cdRegister);
  Exec.RegisterDelphiFunction(@IncMinute, 'AddMinute', cdRegister);
  Exec.RegisterDelphiFunction(@IncSecond, 'AddSecond', cdRegister);
  Exec.RegisterDelphiFunction(@YearOf, 'YearOf', cdRegister);
  Exec.RegisterDelphiFunction(@MonthOf, 'MonthOf', cdRegister);
  Exec.RegisterDelphiFunction(@WeekOf, 'WeekOf', cdRegister);
  Exec.RegisterDelphiFunction(@DayOf, 'DayOf', cdRegister);
  Exec.RegisterDelphiFunction(@HourOf, 'HourOf', cdRegister);
  Exec.RegisterDelphiFunction(@MinuteOf, 'MinuteOf', cdRegister);
  Exec.RegisterDelphiFunction(@SecondOf, 'SecondOf', cdRegister);
  Exec.RegisterDelphiFunction(@MyStrToTime, 'StrToTime', cdRegister);
  Exec.RegisterDelphiFunction(@MyTimeToStr, 'TimeToStr', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToDate, 'TryStrToDate', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToTime, 'TryStrToTime', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToDateTime, 'TryStrToDateTime', cdRegister);
  Exec.RegisterDelphiFunction(@TryStrToInt, 'TryStrToInt', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToFloat, 'TryStrToFloat', cdRegister);
  Exec.RegisterDelphiFunction(@MyStrToDateTime, 'StrToDateTime', cdRegister);
  Exec.RegisterDelphiFunction(@GetMonthName, 'GetMonthName', cdRegister);
  Exec.RegisterDelphiFunction(@GetWeekName, 'GetWeekName', cdRegister);
  Exec.RegisterDelphiFunction(@DayOfTheWeek, 'DayOfTheWeek', cdRegister);
  Exec.RegisterDelphiFunction(@FmtDate, 'FmtDate', cdRegister);
  Exec.RegisterDelphiFunction(@SetZeros, 'FillZeros', cdRegister);
  Exec.RegisterDelphiFunction(@CalcPeriodRu, 'CalcPeriod', cdRegister);
  Exec.RegisterDelphiFunction(@BeginYear, 'BeginYear', cdRegister);
  Exec.RegisterDelphiFunction(@BeginQuarter, 'BeginQuarter', cdRegister);
  Exec.RegisterDelphiFunction(@BeginMonth, 'BeginMonth', cdRegister);
  Exec.RegisterDelphiFunction(@BeginWeek, 'BeginWeek', cdRegister);
  Exec.RegisterDelphiFunction(@EndYear, 'EndYear', cdRegister);
  Exec.RegisterDelphiFunction(@EndQuarter, 'EndQuarter', cdRegister);
  Exec.RegisterDelphiFunction(@EndMonth, 'EndMonth', cdRegister);
  Exec.RegisterDelphiFunction(@EndWeek, 'EndWeek', cdRegister);
  Exec.RegisterDelphiFunction(@QuarterOf, 'QuarterOf', cdRegister);
  Exec.RegisterDelphiFunction(@GetUser, 'GetCurrentUser', cdRegister);
  Exec.RegisterDelphiFunction(@GetRole, 'GetCurrentRole', cdRegister);
  Exec.RegisterDelphiFunction(@GetCurrentDatabase, 'GetCurrentDatabase', cdRegister);
  Exec.RegisterDelphiFunction(@GetTemplatesDir, 'GetTemplatesDir', cdRegister);
  Exec.RegisterDelphiFunction(@GetOutDir, 'GetOutputDir', cdRegister);
  Exec.RegisterDelphiFunction(@Format, 'Format', cdRegister);
  Exec.RegisterDelphiFunction(@StringReplace, 'StringReplace', cdRegister);
  Exec.RegisterDelphiFunction(@SplitStr, 'SplitStr', cdRegister);

  Exec.RegisterDelphiFunction(@Clipboard, 'Clipboard', cdRegister);

	Exec.RegisterDelphiFunction(@SQLSelect, 'SQLSelect', cdRegister);

  Exec.RegisterDelphiFunction(@ColorToString, 'ColorToString', cdRegister);
  Exec.RegisterDelphiFunction(@MyStringToColor, 'StringToColor', cdRegister);
  Exec.RegisterDelphiFunction(@RGBToColor, 'RGBToColor', cdRegister);
  Exec.RegisterDelphiFunction(@ColorToRGB, 'ColorToRGB', cdRegister);
  Exec.RegisterDelphiFunction(@RedGreenBlue, 'RedGreenBlue', cdRegister);
  Exec.RegisterDelphiFunction(@MyFormatFloat, 'FormatFloat', cdRegister);

  Exec.RegisterDelphiFunction(@EncodeMD5, 'EncodeMD5', cdRegister);
  Exec.RegisterDelphiFunction(@EncodeSHA1, 'EncodeSHA1', cdRegister);
  Exec.RegisterDelphiFunction(@EncodeBase64, 'EncodeBase64', cdRegister);
  Exec.RegisterDelphiFunction(@DecodeBase64, 'DecodeBase64', cdRegister);
  Exec.RegisterDelphiFunction(@HMACMD5, 'HMACMD5', cdRegister);
  Exec.RegisterDelphiFunction(@HMACSHA1, 'HMACSHA1', cdRegister);

  Exec.RegisterDelphiFunction(@VarToStr, 'VarToStr', cdRegister);
  Exec.RegisterDelphiFunction(@ShowPrintErrors, 'ShowPrintErrors', cdRegister);
  Exec.RegisterDelphiFunction(@MySameValue, 'SameValue', cdRegister);
end;

end.

