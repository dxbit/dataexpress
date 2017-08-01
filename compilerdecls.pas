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
unit CompilerDecls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSCompiler;

procedure SIRegister_Clipboard(Cl: TPSPascalCompiler);
procedure SIRegister_IniFiles(Cl: TPSPascalCompiler);
procedure SIRegister_ReportWindow(Cl: TPSPascalCompiler);
procedure SIRegister_More(Cl: TPSPascalCompiler);
procedure SIRegister_MenuItemConsts(Cl: TPSPascalCompiler);
procedure SIRegister_KGrid(Cl: TPSPascalCompiler);
procedure SIRegister_dxSQLQuery(Cl: TPSPascalCompiler);
//procedure SIRegister_dxQuery(Cl: TPSPascalCompiler);
procedure SIRegister_dxQueryGrid(Cl: TPSPascalCompiler);
procedure SIRegister_dxFile(Cl: TPSPascalCompiler);
procedure SIRegister_dxDBImage(Cl: TPSPascalCompiler);
procedure SIRegister_dxButton(Cl: TPSPascalCompiler);
procedure SIRegister_dxCounter(Cl: TPSPascalCompiler);
procedure SIRegister_dxTimeEdit(Cl: TPSPascalCompiler);
procedure SIRegister_DBTimeEdit(Cl: TPSPascalCompiler);
procedure SIRegister_dxObjectField(Cl: TPSPascalCompiler);
procedure SIRegister_dxShape(Cl: TPSPascalCompiler);
procedure SIRegister_dxPageControl(Cl: TPSPascalCompiler);
procedure SIRegister_PageControl(Cl: TPSPascalCompiler);
procedure SIRegister_ImageList(Cl: TPSPascalCompiler);
procedure SIRegister_dxGroupBox(Cl: TPSPascalCompiler);
procedure SIRegister_dxGrid(Cl: TPSPascalCompiler);
procedure SIRegister_DBGrid(Cl: TPSPascalCompiler);
procedure SIRegister_dxImage(Cl: TPSPascalCompiler);
procedure SIRegister_dxLookupComboBox(Cl: TPSPascalCompiler);
procedure SIRegister_dxComboBox(Cl: TPSPascalCompiler);
procedure SIRegister_DBComboBox(Cl: TPSPascalCompiler);
procedure SIRegister_CustomDBComboBox(Cl: TPSPascalCompiler);
procedure SIRegister_dxCheckBox(Cl: TPSPascalCompiler);
procedure SIRegister_DBCheckBox(Cl: TPSPascalCompiler);
procedure SIRegister_dxMemo(Cl: TPSPascalCompiler);
procedure SIRegister_DBMemo(Cl: TPSPascalCompiler);
procedure SIRegister_dxDateEdit(Cl: TPSPascalCompiler);
procedure SIRegister_DBDateEdit(Cl: TPSPascalCompiler);
procedure SIRegister_dxCalcEdit(Cl: TPSPascalCompiler);
procedure SIRegister_DBCalcEdit(Cl: TPSPascalCompiler);
procedure SIRegister_CustomDBEditButton(Cl: TPSPascalCompiler);
procedure SIRegister_dxEdit(Cl: TPSPascalCompiler);
procedure SIRegister_DBEdit(Cl: TPSPascalCompiler);
procedure SIRegister_MaskEditEx(Cl: TPSPascalCompiler);
//procedure SIRegister_CustomMaskEdit(Cl: TPSPascalCompiler);
procedure SIRegister_dxLabel(Cl: TPSPascalCompiler);
procedure SIRegister_dxTypes(Cl: TPSPascalCompiler);
procedure SIRegister_dxForm(Cl: TPSPascalCompiler);
procedure SIRegister_TFIELD(CL: TPSPascalCompiler);
procedure SIRegister_StdCtrlsEx(Cl: TPSPascalCompiler);
procedure SIRegister_ControlsEx(Cl: TPSPascalCompiler);
procedure SIRegister_Dialogs(Cl: TPSPascalCompiler);
procedure SIRegister_dxCtrls(Cl: TPSPascalCompiler);
procedure SIRegister_TreeView(Cl: TPSPascalCompiler);
procedure SIRegister_Consts(Cl: TPSPascalCompiler);
procedure SIRegister_Functions(Cl: TPSPascalCompiler);

implementation

procedure SIRegister_Clipboard(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TPersistent'), 'TClipboard') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure Close');
    RegisterMethod('function HasText: Boolean');
    RegisterMethod('function HasBitmap: Boolean');
    RegisterMethod('procedure Open');
    RegisterProperty('AsText', 'String', iptRW);
  end;
end;

procedure SIRegister_IniFiles(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TObject'), 'TIniFile') do
  begin
    RegisterMethod('constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean)');
    RegisterMethod('function SectionExists(const Section: string): Boolean');
    RegisterMethod('function ReadString(const Section, Ident, Default: string): string');
    RegisterMethod('procedure WriteString(const Section, Ident, Value: String)');
    RegisterMethod('function ReadInteger(const Section, Ident: string; Default: Longint): Longint');
    RegisterMethod('procedure WriteInteger(const Section, Ident: string; Value: Longint)');
    RegisterMethod('function ReadInt64(const Section, Ident: string; Default: Int64): Longint');
    RegisterMethod('procedure WriteInt64(const Section, Ident: string; Value: Int64)');
    RegisterMethod('function ReadBool(const Section, Ident: string; Default: Boolean): Boolean');
    RegisterMethod('procedure WriteBool(const Section, Ident: string; Value: Boolean)');
    RegisterMethod('function ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime');
    RegisterMethod('function ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime');
    RegisterMethod('function ReadFloat(const Section, Ident: string; Default: Double): Double');
    RegisterMethod('function ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime');
    RegisterMethod('function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer');
    RegisterMethod('procedure WriteDate(const Section, Ident: string; Value: TDateTime)');
    RegisterMethod('procedure WriteDateTime(const Section, Ident: string; Value: TDateTime)');
    RegisterMethod('procedure WriteFloat(const Section, Ident: string; Value: Double)');
    RegisterMethod('procedure WriteTime(const Section, Ident: string; Value: TDateTime)');
    RegisterMethod('procedure WriteBinaryStream(const Section, Name: string; Value: TStream)');
    RegisterMethod('procedure ReadSection(const Section: string; Strings: TStrings)');
    RegisterMethod('procedure ReadSections(Strings: TStrings)');
    RegisterMethod('procedure ReadSectionValues(const Section: string; Strings: TStrings)');
    RegisterMethod('procedure EraseSection(const Section: string)');
    RegisterMethod('procedure DeleteKey(const Section, Ident: String)');
    RegisterMethod('procedure UpdateFile');
    RegisterMethod('function ValueExists(const Section, Ident: string): Boolean');
    RegisterProperty('FileName', 'string', iptR);
    RegisterProperty('EscapeLineFeeds', 'boolean', iptR);
    RegisterProperty('CaseSensitive', 'Boolean', iptRW);
    RegisterProperty('StripQuotes', 'Boolean', iptRW);

    RegisterMethod('procedure ReadSectionRaw(const Section: string; Strings: TStrings)');
    RegisterProperty('CacheUpdates', 'Boolean', iptRW);
  end;
end;

procedure SIRegister_ReportWindow(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TWindow'), 'TReportWindow') do
  begin
    RegisterMethod('constructor CreateWindow');
    RegisterMethod('function ShowReport(const aName: String): Integer');
    RegisterProperty('ToolBar', 'TToolBar', iptR);
    RegisterProperty('QGrid', 'TdxQueryGrid', iptR);
    RegisterProperty('StatusBar', 'TStatusBar', iptR);
    RegisterProperty('FilterSplitter', 'TSplitter', iptR);
    RegisterProperty('Filter', 'TCustomControl', iptR);
  end;
end;

procedure SIRegister_More(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TResizeStyle', '(rsLine, rsNone, rsPattern, rsUpdate)');
  with Cl.AddClassN(Cl.FindClass('TCustomControl'), 'TSplitter') do
  begin
    RegisterProperty('AutoSnap', 'Boolean', iptRW);
    RegisterProperty('Beveled', 'Boolean', iptRW);
    RegisterProperty('MinSize', 'Integer', iptRW);
    RegisterProperty('ParentColor', 'Boolean', iptRW);
    RegisterProperty('ResizeStyle', 'TResizeStyle', iptRW);
  end;

  Cl.AddTypeS('TPanelButton', '(pbOK, pbCancel, pbClose, pbHelp)');
  Cl.AddTypeS('TPanelButtons', 'set of TPanelButton');

  with Cl.AddClassN(Cl.FindClass('TCustomButton'), 'TPanelBitBtn') do
  begin
    RegisterProperty('Glyph', 'TBitmap', iptRW);
  end;

  with Cl.AddClassN(Cl.FindClass('TCustomPanel'), 'TButtonPanel') do
  begin
    RegisterProperty('OKButton', 'TPanelBitBtn', iptR);
    RegisterProperty('HelpButton', 'TPanelBitBtn', iptR);
    RegisterProperty('CloseButton', 'TPanelBitBtn', iptR);
    RegisterProperty('CancelButton', 'TPanelBitBtn', iptR);
    RegisterProperty('Spacing', 'Integer', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptRW);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptRW);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptRW);
    RegisterProperty('ShowButtons', 'TPanelButtons', iptRW);
    RegisterProperty('ShowGlyphs', 'TPanelButtons', iptRW);
    RegisterProperty('ShowBevel', 'Boolean', iptRW);
  end;

  with Cl.AddClassN(Cl.FindClass('TCollectionItem'), 'TStatusPanel') do
  begin

  end;

  Cl.AddClassN(Cl.FindClass('TWinControl'), 'TStatusBar');

  Cl.AddTypeS('TDrawPanelEvent', 'procedure(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect)');
  Cl.AddTypeS('TStatusPanelStyle', '(psText, psOwnerDraw)');
  Cl.AddTypeS('TStatusPanelBevel', '(pbNone, pbLowered, pbRaised)');

  with Cl.AddClassN(Cl.FindClass('TCollectionItem'), 'TStatusPanel') do
  begin
    RegisterMethod('function StatusBar: TStatusBar');
    RegisterProperty('Alignment', 'TAlignment', iptRW);
    RegisterProperty('Bevel', 'TStatusPanelBevel', iptRW);
    RegisterProperty('Style', 'TStatusPanelStyle', iptRW);
    RegisterProperty('Text', 'String', iptRW);
    RegisterProperty('Width', 'Integer', iptRW);
  end;

  with Cl.AddClassN(Cl.FindClass('TCollection'), 'TStatusPanels') do
  begin
    RegisterMethod('constructor Create(AStatusBar: TStatusBar)');
    RegisterMethod('function Add: TStatusPanel');
    RegisterProperty('Items', 'TStatusPanel Integer', iptRW);
    SetDefaultPropery('Items');
    RegisterProperty('StatusBar', 'TStatusBar', iptR);
  end;

  with Cl.FindClass('TStatusBar') do
  begin
    RegisterMethod('procedure BeginUpdate');
    RegisterMethod('procedure EndUpdate');
    RegisterMethod('function GetPanelIndexAt(X, Y: Integer): Integer');
    RegisterMethod('function SizeGripEnabled: Boolean');
    RegisterMethod('function UpdatingStatusBar: boolean');
    RegisterProperty('Canvas', 'TCanvas', iptR);
    RegisterProperty('AutoHint', 'Boolean', iptRW);
    RegisterProperty('Panels', 'TStatusPanels', iptRW);
    RegisterProperty('ParentColor', 'Boolean', iptRW);
    RegisterProperty('ParentFont', 'Boolean', iptRW);
    RegisterProperty('SimpleText', 'String', iptRW);
    RegisterProperty('SimplePanel', 'Boolean', iptRW);
    RegisterProperty('SizeGrip', 'Boolean', iptRW);
    RegisterProperty('UseSystemFont', 'Boolean', iptRW);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptRW);
    RegisterProperty('OnDrawPanel', 'TDrawPanelEvent', iptRW);
    RegisterProperty('OnHint', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptRW);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptRW);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptRW);
  end;

  with Cl.AddClassN(Cl.FindClass('TCustomPanel'), 'TFormView') do
  begin
    RegisterMethod('constructor CreateView(AOwner: TComponent; const FormName: String; ViewType: TViewType)');
    RegisterProperty('Grid', 'TdxGrid', iptR);
    RegisterProperty('Form', 'TdxForm', iptR);
    RegisterProperty('Tree', 'TTreeView', iptR);
    RegisterProperty('ScrollBox', 'TScrollBox', iptR);
    RegisterProperty('TreeSplitter', 'TSplitter', iptR);
    RegisterProperty('FormSplitter', 'TSplitter', iptR);
  end;

  Cl.AddTypeS('TToolButtonStyle', '(tbsButton, tbsCheck, tbsDropDown, tbsSeparator, tbsDivider)');

  with Cl.AddClassN(Cl.FindClass('TGraphicControl'), 'TToolButton') do
  begin
    RegisterProperty('AllowAllUp', 'Boolean', iptRW);
    RegisterProperty('Down', 'Boolean', iptRW);
    RegisterProperty('DropdownMenu', 'TPopupMenu', iptRW);
    RegisterProperty('Grouped', 'Boolean', iptRW);
    RegisterProperty('ImageIndex', 'Integer', iptRW);
    RegisterProperty('Indeterminate', 'Boolean', iptRW);
    RegisterProperty('Marked', 'Boolean', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptRW);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptRW);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptRW);
    RegisterProperty('ShowCaption', 'boolean', iptRW);
    RegisterProperty('Style', 'TToolButtonStyle', iptRW);
    RegisterProperty('Wrap', 'Boolean', iptRW);
  end;

  Cl.AddTypeS('TEdgeBorder', '(ebLeft, ebTop, ebRight, ebBottom)');
  Cl.AddTypeS('TEdgeBorders', 'set of TEdgeBorder');
  Cl.AddTypeS('TEdgeStyle', '(esNone, esRaised, esLowered)');

  with Cl.AddClassN(Cl.FindClass('TCustomControl'), 'TToolbar') do
  begin
    RegisterProperty('ButtonCount', 'Integer', iptR);
    RegisterProperty('Buttons', 'TToolButton Integer', iptR);
    RegisterProperty('RowCount', 'Integer', iptR);

    RegisterProperty('ButtonHeight', 'Integer', iptRW);
    RegisterProperty('ButtonWidth', 'Integer', iptRW);
    RegisterProperty('EdgeBorders', 'TEdgeBorders', iptRW);
    RegisterProperty('EdgeInner', 'TEdgeStyle', iptRW);
    RegisterProperty('EdgeOuter', 'TEdgeStyle', iptRW);
    RegisterProperty('Flat', 'Boolean', iptRW);
    RegisterProperty('Images', 'TCustomImageList', iptRW);
    RegisterProperty('List', 'Boolean', iptRW);
    RegisterProperty('ParentColor', 'Boolean', iptRW);
    RegisterProperty('ParentFont', 'Boolean', iptRW);
    RegisterProperty('ShowCaptions', 'Boolean', iptRW);
    RegisterProperty('Transparent', 'Boolean', iptRW);
    RegisterProperty('Wrapable', 'Boolean', iptRW);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptRW);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptRW);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptRW);
  end;

  with Cl.AddClassN(Cl.FindClass('TObject'), 'TParamList') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure Clear');
    RegisterMethod('function ParamExists(const aName: String): Boolean');
    RegisterProperty('Values', 'Variant String', iptRW);
    RegisterProperty('Objects', 'TObject String', iptRW);
    RegisterProperty('Names', 'String Integer', iptR);
    RegisterProperty('ValueFromIndex', 'Variant Integer', iptRW);
    RegisterProperty('ObjectFromIndex', 'TObject Integer', iptRW);
    RegisterProperty('Count', 'Integer', iptR);
    RegisterProperty('OnGetParam', 'TParamNotifyEvent', iptRW);
    RegisterProperty('OnSetParam', 'TParamNotifyEvent', iptRW);
  end;

  with Cl.AddClassN(Cl.FindClass('TForm'), 'TWindow') do
  begin
    RegisterMethod('constructor CreateWindow');
    RegisterProperty('Params', 'TParamList', iptR);
  end;

  with Cl.AddClassN(Cl.FindClass('TWindow'), 'TListWindow') do
  begin
    RegisterMethod('constructor CreateWindow(const aFormName: String; aViewType: TViewType)');
    RegisterProperty('Buttons', 'TButtonPanel', iptR);
    RegisterProperty('Toolbar', 'TToolbar', iptR);
    RegisterProperty('FormView', 'TFormView', iptR);
  end;

  with Cl.AddClassN(Cl.FindClass('TWindow'), 'TEditWindow') do
  begin
    RegisterProperty('Form', 'TdxForm', iptR);
    RegisterProperty('ScrollBox', 'TScrollBox', iptR);
    RegisterProperty('Buttons', 'TButtonPanel', iptR);
  end;

  with Cl.AddClassN(Cl.FindClass('TForm'), 'TMainFm') do
  begin
    RegisterMethod('function CreatePage(const FormName: String; ViewType: TViewType): TTabSheet');
    RegisterMethod('procedure DestroyPage(Pg: TTabSheet)');
    RegisterProperty('FormViews', 'TFormView Integer', iptR);
    RegisterProperty('Pages', 'TPageControl', iptR);
    RegisterProperty('Toolbar', 'TToolBar', iptR);
    RegisterProperty('StatusBar', 'TStatusBar', iptR);
    RegisterProperty('Params', 'TParamList', iptR);
    RegisterProperty('OnCreateForm', 'TCreateFormEvent', iptRW);
    RegisterProperty('OnDestroyForm', 'TCreateFormEvent', iptRW);
    RegisterProperty('OnCreateListWindow', 'TCreateListWindowEvent', iptRW);
    RegisterProperty('OnCreateReportWindow', 'TCreateReportWindowEvent', iptRW);
  end;
end;

procedure SIRegister_MenuItemConsts(Cl: TPSPascalCompiler);
begin
  Cl.AddConstantN('MI_FORM_APPEND', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MI_FORM_EDIT', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MI_FORM_DELETE', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MI_FORM_DUPLICATE', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('MI_FORM_DUPLICATEALL', 'Integer').Value^.ts16:=5;
  Cl.AddConstantN('MI_FORM_FILTER', 'Integer').Value^.ts16:=7;

  Cl.AddConstantN('MI_TABLE_APPEND', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MI_TABLE_EDIT', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MI_TABLE_DELETE', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MI_TABLE_DUPLICATE', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('MI_TABLE_SHOPPING', 'Integer').Value^.ts16:=6;
  Cl.AddConstantN('MI_TABLE_MOVEUP', 'Integer').Value^.ts16:=8;
  Cl.AddConstantN('MI_TABLE_MOVEDOWN', 'Integer').Value^.ts16:=9;

  Cl.AddConstantN('MI_QUERY_APPEND', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MI_QUERY_EDIT', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MI_QUERY_DELETE', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MI_QUERY_GOTO', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('MI_QUERY_REFRESH', 'Integer').Value^.ts16:=6;

  Cl.AddConstantN('MI_LCBX_CUT', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MI_LCBX_COPY', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MI_LCBX_PASTE', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MI_LCBX_CLEAR', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('MI_LCBX_APPEND', 'Integer').Value^.ts16:=6;
  Cl.AddConstantN('MI_LCBX_EDIT', 'Integer').Value^.ts16:=7;
  Cl.AddConstantN('MI_LCBX_GOTO', 'Integer').Value^.ts16:=9;

  Cl.AddConstantN('MI_EDIT_CUT', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MI_EDIT_COPY', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MI_EDIT_PASTE', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MI_EDIT_CLEAR', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('MI_EDIT_GOTO', 'Integer').Value^.ts16:=6;

  Cl.AddConstantN('MI_TIMEEDIT_CUT', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MI_TIMEEDIT_COPY', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MI_TIMEEDIT_PASTE', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MI_TIMEEDIT_CLEAR', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('MI_TIMEEDIT_8', 'Integer').Value^.ts16:=6;
  Cl.AddConstantN('MI_TIMEEDIT_9', 'Integer').Value^.ts16:=7;
  Cl.AddConstantN('MI_TIMEEDIT_10', 'Integer').Value^.ts16:=8;
  Cl.AddConstantN('MI_TIMEEDIT_11', 'Integer').Value^.ts16:=9;
  Cl.AddConstantN('MI_TIMEEDIT_12', 'Integer').Value^.ts16:=10;
  Cl.AddConstantN('MI_TIMEEDIT_13', 'Integer').Value^.ts16:=11;
  Cl.AddConstantN('MI_TIMEEDIT_14', 'Integer').Value^.ts16:=12;
  Cl.AddConstantN('MI_TIMEEDIT_15', 'Integer').Value^.ts16:=13;
  Cl.AddConstantN('MI_TIMEEDIT_16', 'Integer').Value^.ts16:=14;
  Cl.AddConstantN('MI_TIMEEDIT_17', 'Integer').Value^.ts16:=15;

  Cl.AddConstantN('MI_OBJECTFIELD_COPY', 'Integer').Value^.ts16:=0;

  Cl.AddConstantN('MI_DATEEDIT_CUT', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MI_DATEEDIT_COPY', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MI_DATEEDIT_PASTE', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MI_DATEEDIT_CLEAR', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('MI_DATEEDIT_TODAY', 'Integer').Value^.ts16:=6;
  Cl.AddConstantN('MI_DATEEDIT_BEGINWEEK', 'Integer').Value^.ts16:=7;
  Cl.AddConstantN('MI_DATEEDIT_BEGINMONTH', 'Integer').Value^.ts16:=8;
  Cl.AddConstantN('MI_DATEEDIT_BEGINYEAR', 'Integer').Value^.ts16:=9;

  Cl.AddConstantN('MI_CALCEDIT_CUT', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MI_CALCEDIT_COPY', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MI_CALCEDIT_PASTE', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MI_CALCEDIT_CLEAR', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('MI_CALCEDIT_SETZERO', 'Integer').Value^.ts16:=6;

  Cl.AddConstantN('MI_MEMO_CUT', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MI_MEMO_COPY', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MI_MEMO_PASTE', 'Integer').Value^.ts16:=2;

  Cl.AddConstantN('MI_DBIMAGE_LOAD', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MI_DBIMAGE_SAVE', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MI_DBIMAGE_CLEAR', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MI_DBIMAGE_OPEN', 'Integer').Value^.ts16:=3;

  Cl.AddConstantN('MI_FILE_OPEN', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MI_FILE_LOAD', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MI_FILE_SAVE', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MI_FILE_CLEAR', 'Integer').Value^.ts16:=3;

  {Cl.AddConstantN('MNU_FILE', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MNU_DATA', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MNU_REPORTS', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MNU_SERVICE', 'Integer').Value^.ts16:=3;
  Cl.AddConstantN('MNU_HELP', 'Integer').Value^.ts16:=4;

  Cl.AddConstantN('MNU_FILE_NEW', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MNU_FILE_OPEN', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MNU_FILE_CONNECT', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MNU_FILE_RECENTS', 'Integer').Value^.ts16:=3;
  Cl.AddConstantN('MNU_FILE_DESIGNER', 'Integer').Value^.ts16:=5;
  Cl.AddConstantN('MNU_FILE_REPORTS', 'Integer').Value^.ts16:=6;
  Cl.AddConstantN('MNU_FILE_TEMPLATES', 'Integer').Value^.ts16:=7;
  Cl.AddConstantN('MNU_FILE_USERS', 'Integer').Value^.ts16:=8;
  Cl.AddConstantN('MNU_FILE_EXPERTMODE', 'Integer').Value^.ts16:=10;
  Cl.AddConstantN('MNU_FILE_OPTIONS', 'Integer').Value^.ts16:=12;
  Cl.AddConstantN('MNU_FILE_PLUGINS', 'Integer').Value^.ts16:=13;
  Cl.AddConstantN('MNU_FILE_EXIT', 'Integer').Value^.ts16:=15;

  Cl.AddConstantN('MNU_SERVICE_RECALC', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MNU_SERVICE_SETVALUE', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MNU_SERVICE_DELETERECORDS', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MNU_SERVICE_EXPORTDATA', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('MNU_SERVICE_IMPORTDATA', 'Integer').Value^.ts16:=5;
  Cl.AddConstantN('MNU_SERVICE_EXPORTPROJECT', 'Integer').Value^.ts16:=6;
  Cl.AddConstantN('MNU_SERVICE_IMPORTPROJECT', 'Integer').Value^.ts16:=7;
  Cl.AddConstantN('MNU_SERVICE_MERGEPROJECTS', 'Integer').Value^.ts16:=8;
  Cl.AddConstantN('MNU_SERVICE_MONITOR', 'Integer').Value^.ts16:=10;

  Cl.AddConstantN('MNU_HELP_REFERENCE', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('MNU_HELP_ARTICLES', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('MNU_HELP_PROGRAMMING', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('MNU_HELP_VIDEOLESSONS', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('MNU_HELP_WEBSITE', 'Integer').Value^.ts16:=6;
  Cl.AddConstantN('MNU_HELP_FORUM', 'Integer').Value^.ts16:=7;
  Cl.AddConstantN('MNU_HELP_ABOUT', 'Integer').Value^.ts16:=9; }

  Cl.AddConstantN('TBN_MOVEFIRST', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('TBN_MOVEFRIOR', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('TBN_MOVENEXT', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('TBN_MOVELAST', 'Integer').Value^.ts16:=3;
  Cl.AddConstantN('TBN_APPEND', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('TBN_EDIT', 'Integer').Value^.ts16:=5;
  Cl.AddConstantN('TBN_CANCEL', 'Integer').Value^.ts16:=6;
  Cl.AddConstantN('TBN_POST', 'Integer').Value^.ts16:=7;
  Cl.AddConstantN('TBN_DELETE', 'Integer').Value^.ts16:=8;
  Cl.AddConstantN('TBN_REFRESH', 'Integer').Value^.ts16:=9;
  Cl.AddConstantN('TBN_PRINT', 'Integer').Value^.ts16:=10;
  Cl.AddConstantN('TBN_FILTER', 'Integer').Value^.ts16:=11;
  Cl.AddConstantN('TBN_SEARCH', 'Integer').Value^.ts16:=12;
  Cl.AddConstantN('TBN_HELP', 'Integer').Value^.ts16:=13;

  Cl.AddConstantN('LWBN_APPEND', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('LWBN_EDIT', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('LWBN_DELETE', 'Integer').Value^.ts16:=2;

  Cl.AddConstantN('RWBN_REPORTS', 'Integer').Value^.ts16:=0;
  Cl.AddConstantN('RWBN_DATEDETAIL', 'Integer').Value^.ts16:=1;
  Cl.AddConstantN('RWBN_MOVEFIRST', 'Integer').Value^.ts16:=2;
  Cl.AddConstantN('RWBN_MOVEFRIOR', 'Integer').Value^.ts16:=3;
  Cl.AddConstantN('RWBN_MOVENEXT', 'Integer').Value^.ts16:=4;
  Cl.AddConstantN('RWBN_MOVELAST', 'Integer').Value^.ts16:=5;
  Cl.AddConstantN('RWBN_APPEND', 'Integer').Value^.ts16:=6;
  Cl.AddConstantN('RWBN_EDIT', 'Integer').Value^.ts16:=7;
  Cl.AddConstantN('RWBN_DELETE', 'Integer').Value^.ts16:=8;
  Cl.AddConstantN('RWBN_REFRESH', 'Integer').Value^.ts16:=9;
  Cl.AddConstantN('RWBN_EXPORT', 'Integer').Value^.ts16:=10;
  Cl.AddConstantN('RWBN_HELP', 'Integer').Value^.ts16:=11;
end;

procedure SIRegister_KGrid(Cl: TPSPascalCompiler);
begin
  {Cl.AddTypeS('TKGridMeasureCellPriority', '(mpColWidth, mpRowHeight, mpCellExtent)');
  Cl.AddTypeS('TKGridAxisInfoMaskMembers', '(aiFixedParams, aiGridExtent, aiGridBoundary, aiFullVisBoundary)');
  Cl.AddTypeS('TKGridAxisInfoMask', 'set of TKGridAxisInfoMaskMembers');
  Cl.AddTypeS('TKGridAxisInfoBoth', 'record Horz, Vert: TKGridAxisInfo; end');
  Cl.AddTypeS('TKGridCanResizeFunc', 'function(var Index, Pos: Integer): Boolean');
  Cl.AddTypeS('TKGridAxisInfo', 'record InfoMask: TKGridAxisInfoMask; AlignLastCell: Boolean; FixedSelectable: Boolean; CanResize: TKGridCanResizeFunc; CellExtent: TKGridGetExtentFunc; EffectiveSpacing: TKGridGetExtentFunc; FixedCellCount: Integer; FirstGridCell: Integer; FirstGridCellExtent: Integer; ClientExtent: Integer; MinCellExtent: TKGridGetExtentFunc; MaxCellExtent: TKGridGetExtentFunc; TotalCellCount: Integer; ScrollOffset: Integer; FixedBoundary: Integer; GridBoundary: Integer; GridCells: Integer; FullVisBoundary: Integer; FullVisCells: Integer; GridExtent: Int64; end');}

  Cl.AddTypeS('TKGridCellSpan', 'record ColSpan: Integer; RowSpan: Integer; end');
  Cl.AddTypeS('TKGridDisabledDrawStyle', '(ddBright, ddGrayed, ddNormal)');
  Cl.AddTypeS('TKGridOption', '(goAlignLastCol, goAlignLastRow, goAlwaysShowEditor, goEraseBackground, goClippedCells, goColMoving, goColSizing, goColSorting, goDoubleBufferedCells, goDrawFocusSelected, goEditing, goEnterMoves, goFixedHorzLine, goFixedVertLine, goHeader, goHeaderAlignment, goHorzLine, goIndicateHiddenCells, goIndicateSelection, goMouseCanHideCells, goMouseOverCells, goNoSelEditText, goRangeSelect, goRowMoving, goRowSelect, goRowSizing, goRowSorting, goTabs, goThemes, goThemedCells, goVertLine, goVirtualGrid)');
  Cl.AddTypeS('TKGridOptions', 'set of TKGridOption');
  Cl.AddTypeS('TKGridOptionEx', '(gxEditorHCenter, gxEditorVCenter, gxEnterAppendsRow, gxEnterWraps, gxFixedCellClickSelect, gxFixedCellClickToggle, gxFixedThemedCells, gxTabAppendsRow, gxTabWraps, gxEditFixedRows, gxEditFixedCols, gxMouseWheelScroll, gxMouseWheelKey)');
  Cl.AddTypeS('TKGridOptionsEx', 'set of TKGridOptionEx');
  Cl.AddTypeS('TKGridRangeSelectStyle', '(rsDefault, rsMS_Excel)');
  Cl.AddTypeS('TKGridScrollMode', '(smSmooth, smCell)');
  Cl.AddTypeS('TKGridRect', 'record Col1, Row1, Col2, Row2: Integer; end');
  Cl.AddTypeS('TKGridSizingStyle', '(ssLine, ssUpdate, ssXORLine)');
  Cl.AddTypeS('TKGridDrawStateMembers', '(gdEdited, gdFixed, gdFocused, gdMouseDown, gdMouseOver, gdSelected, gdSorted, gdColsSortedUp, gdColsSortedDown, gdRowsSortedUp, gdRowsSortedDown)');
  Cl.AddTypeS('TKGridDrawState', 'set of TKGridDrawStateMembers');
  Cl.AddTypeS('TKGridDrawCellEvent', 'procedure(Sender: TObject; ACol, ARow: Integer; R: TRect; State: TKGridDrawState)');
  Cl.AddTypeS('TKGridMeasureCellPriority', '(mpColWidth, mpRowHeight, mpCellExtent)');
  Cl.AddTypeS('TKGridMeasureCellEvent', 'procedure(Sender: TObject; ACol, ARow: Integer; R: TRect; State: TKGridDrawState; Priority: TKGridMeasureCellPriority; var Extent: TPoint)');
  Cl.AddTypeS('TKGridCellHintEvent', 'procedure(Sender: TObject; ACol, ARow: Integer; AShow: Boolean)');
  Cl.AddTypeS('TKGridCellEvent', 'procedure(Sender: TObject; ACol, ARow: Integer)');
  Cl.AddTypeS('TKGridSelectCellEvent', 'procedure(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean)');

  with Cl.AddClassN(Cl.FindClass('TPersistent'), 'TKGridColors') do
  begin
    registerProperty('BrightRangeBkGnd', 'Boolean', iptRW);
    registerProperty('CellBkGnd', 'TColor', iptRW);
    registerProperty('CellLines', 'TColor', iptRW);
    registerProperty('CellText', 'TColor', iptRW);
    registerProperty('DragSuggestionBkGnd', 'TColor', iptRW);
    registerProperty('DragSuggestionLine', 'TColor', iptRW);
    registerProperty('FixedCellBkGnd', 'TColor', iptRW);
    registerProperty('FixedCellIndication', 'TColor', iptRW);
    registerProperty('FixedCellLines', 'TColor', iptRW);
    registerProperty('FixedCellText', 'TColor', iptRW);
    registerProperty('FixedThemedCellLines', 'TColor', iptRW);
    registerProperty('FixedThemedCellHighlight', 'TColor', iptRW);
    registerProperty('FixedThemedCellShadow', 'TColor', iptRW);
    registerProperty('FocusedCellBkGnd', 'TColor', iptRW);
    registerProperty('FocusedCellText', 'TColor', iptRW);
    registerProperty('FocusedRangeBkGnd', 'TColor', iptRW);
    registerProperty('FocusedRangeText', 'TColor', iptRW);
    registerProperty('SelectedCellBkGnd', 'TColor', iptRW);
    registerProperty('SelectedCellText', 'TColor', iptRW);
    registerProperty('SelectedRangeBkGnd', 'TColor', iptRW);
    registerProperty('SelectedRangeText', 'TColor', iptRW);
    registerProperty('SelectedFixedCellBkGnd', 'TColor', iptRW);
  end;

  with Cl.AddClassN(Cl.FindClass('TCustomControl'), 'TKGrid') do
  begin
    {RegisterMethod('procedure AutoSizeCol(ACol: Integer; FixedCells: Boolean);');
    RegisterMethod('procedure AutoSizeGrid(Priority: TKGridMeasureCellPriority; FixedCells: Boolean);');
    RegisterMethod('procedure AutoSizeRow(ARow: Integer; FixedCells: Boolean);');}
    RegisterMethod('function CellSelected(ACol, ARow: Integer): Boolean; virtual');
    {RegisterMethod('function CellRect(ACol, ARow: Integer; out R: TRect; VisibleOnly: Boolean): Boolean;');
    RegisterMethod('function CellToPoint(ACol, ARow: Integer; var Point: TPoint; VisibleOnly: Boolean): Boolean; virtual;');
    RegisterMethod('function CellVisible(ACol, ARow: Integer): Boolean; virtual;');
    RegisterMethod('procedure ClearCol(ACol: Integer); virtual;');
    RegisterMethod('procedure ClearGrid; virtual;');
    RegisterMethod('procedure ClearRow(ARow: Integer); virtual;');
    RegisterMethod('procedure ClearSortMode;');
    RegisterMethod('procedure ClearSortModeHorz; virtual;');
    RegisterMethod('procedure ClearSortModeVert; virtual;');
    RegisterMethod('function ColSelectable(ACol: Integer): Boolean; virtual;');
    RegisterMethod('function ColSelected(ACol: Integer): Boolean; virtual;');
    RegisterMethod('function ColValid(ACol: Integer): Boolean; virtual;');
    RegisterMethod('procedure DefaultComboKeyPreview(AEditor: TComboBox; ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean); virtual;');
    RegisterMethod('procedure DefaultComboSelect(AEditor: TComboBox; SelectAll, CaretToLeft: Boolean); virtual;');
    RegisterMethod('function DefaultCompareCells(ACell1, ACell2: TKGridCell): Integer; virtual;');
    RegisterMethod('procedure DefaultEditKeyPreview(AEditor: TCustomEdit; ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean); virtual;');
    RegisterMethod('procedure DefaultEditorCreate(ACol, ARow: Integer; var AEditor: TWinControl); virtual;');
    RegisterMethod('procedure DefaultEditorDataFromGrid(AEditor: TWinControl; ACol, ARow: Integer; var AssignText: Boolean); virtual;');
    RegisterMethod('procedure DefaultEditorDataToGrid(AEditor: TWinControl; ACol, ARow: Integer; var AssignText: Boolean); virtual;');
    RegisterMethod('procedure DefaultEditorDestroy(AEditor: TWinControl; ACol, ARow: Integer); virtual;');
    RegisterMethod('procedure DefaultEditorKeyPreview(AEditor: TWinControl; ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean); virtual;');
    RegisterMethod('procedure DefaultEditorResize(AEditor: TWinControl; ACol, ARow: Integer; var ARect: TRect); virtual;');
    RegisterMethod('procedure DefaultEditorSelect(AEditor: TWinControl; ACol, ARow: Integer; SelectAll, CaretToLeft, SelectedByMouse: Boolean); virtual;');
    RegisterMethod('procedure DefaultEditSelect(AEditor: TCustomEdit; SelectAll, CaretToLeft: Boolean); virtual;');
    RegisterMethod('procedure DefaultMouseCellHint(ACol, ARow: Integer; AShow: Boolean); virtual;');
    RegisterMethod('procedure DefaultScrollBarKeyPreview(AEditor: TScrollBar; ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);');}
    //RegisterMethod('procedure DeleteCol(At: Integer); virtual;');
    //RegisterMethod('procedure DeleteCols(At, Count: Integer); virtual;');
    //RegisterMethod('procedure DeleteRow(At: Integer); virtual;');
    {RegisterMethod('procedure DeleteRows(At, Count: Integer); virtual;');
    RegisterMethod('procedure FindBaseCell(ACol, ARow: Integer; out BaseCol, BaseRow: Integer); virtual;');
    RegisterMethod('function FindCol(ACol: TKGridCol): Integer;');
    RegisterMethod('function FindRow(ARow: TKGridRow): Integer;');
    RegisterMethod('procedure FocusCell(ACol, ARow: Integer);');
    RegisterMethod('function GetAxisInfoBoth(Mask: TKGridAxisInfoMask): TKGridAxisInfoBoth;');
    RegisterMethod('function GetAxisInfoHorz(Mask: TKGridAxisInfoMask): TKGridAxisInfo; virtual;');
    RegisterMethod('function GetAxisInfoVert(Mask: TKGridAxisInfoMask): TKGridAxisInfo; virtual;');
    RegisterMethod('function GetDrawState(ACol, ARow: Integer; AFocused: Boolean): TKGridDrawState; virtual;');
    RegisterMethod('function GridRectSelectable(const GridRect: TKGridRect): Boolean; virtual;');
    RegisterMethod('function GridRectToRect(GridRect: TKGridRect; var R: TRect; VisibleOnly: Boolean; Merged: Boolean): Boolean; virtual;');
    RegisterMethod('function GridRectValid(const GridRect: TKGridRect): Boolean; virtual;');
    RegisterMethod('function HasFocus: Boolean; virtual;');
    RegisterMethod('procedure HideCellHint;');
    RegisterMethod('function InitialCol(ACol: Integer): Integer; virtual;');
    RegisterMethod('function InitialColInv(ACol: Integer): Integer; virtual;');
    RegisterMethod('function InitialRow(ARow: Integer): Integer; virtual;');
    RegisterMethod('function InitialRowInv(ARow: Integer): Integer; virtual;');  }
    //RegisterMethod('function InsertCol(At: Integer): TKGridCol; virtual;');
    //RegisterMethod('procedure InsertCols(At, Count: Integer); virtual;');
    //RegisterMethod('function InsertRow(At: Integer): TKGridRow; virtual;');
    {RegisterMethod('procedure InsertRows(At, Count: Integer); virtual;');
    RegisterMethod('function InsertSortedCol(out ByRow, ACol: Integer): Boolean; virtual;');
    RegisterMethod('function InsertSortedRow(out ByCol, ARow: Integer): Boolean; virtual;');
    RegisterMethod('procedure InvalidateCell(ACol, ARow: Integer);');
    RegisterMethod('procedure InvalidateCol(ACol: Integer); virtual;');
    RegisterMethod('procedure InvalidateCols(FirstCol: Integer); virtual;');
    RegisterMethod('procedure InvalidateCurrentSelection; virtual;');
    RegisterMethod('procedure InvalidateGridRect(const GR: TKGridRect; Merged: Boolean); virtual;');
    RegisterMethod('procedure InvalidateRow(ARow: Integer); virtual;');
    RegisterMethod('procedure InvalidateRows(FirstRow: Integer); virtual;');
    RegisterMethod('procedure InvalidateSelection(ASelection: TKGridRect); virtual;');
    RegisterMethod('function IsDoubleBuffered: Boolean; virtual;');
    RegisterMethod('procedure LockSortMode; virtual;');     }
    RegisterMethod('function MouseToCell(X, Y: Integer; var ACol, ARow: Integer): Boolean;');
    {RegisterMethod('procedure MoveCol(FromIndex, ToIndex: Integer); virtual;');
    RegisterMethod('procedure MoveRow(FromIndex, ToIndex: Integer); virtual;');
    RegisterMethod('procedure MoveToNextCell; virtual;');
    RegisterMethod('procedure PaintCell(ACanvas: TCanvas; ACol, ARow: Integer; AX: Integer; AY: Integer; APrinting: Boolean; ABlockRect: PRect); virtual;');
    RegisterMethod('procedure PaintToCanvas(ACanvas: TCanvas); override;');
    RegisterMethod('procedure PostRecreateEditor; virtual;');
    RegisterMethod('procedure RealizeCellClass;');
    RegisterMethod('procedure RealizeColClass;');
    RegisterMethod('procedure RealizeRowClass;');
    RegisterMethod('function RowSelectable(ARow: Integer): Boolean; virtual;');
    RegisterMethod('function RowSelected(ARow: Integer): Boolean; virtual;');
    RegisterMethod('function RowValid(ARow: Integer): Boolean; virtual;');
    RegisterMethod('procedure ScrollBy(AColCount, ARowCount: Integer);');
    RegisterMethod('function ScrollDeltaFromDelta(const Info: TKGridAxisInfo; ADelta: Integer): Integer; virtual;');
    RegisterMethod('function ScrollNeeded(ACol, ARow: Integer; out DeltaHorz, DeltaVert: Integer): Boolean; virtual;');
    RegisterMethod('procedure SelectAll;');
    RegisterMethod('procedure SelectCol(ACol: Integer);');
    RegisterMethod('procedure SelectCols(FirstCol, Count: Integer);');
    RegisterMethod('procedure SelectionNormalize;');
    RegisterMethod('procedure SelectRow(ARow: Integer);');
    RegisterMethod('procedure SelectRows(FirstRow, Count: Integer);');
    RegisterMethod('procedure ShowCellHint;');
    RegisterMethod('procedure SortCols(ByRow: Integer; SortMode: TKGridSortMode); virtual;');
    RegisterMethod('function SortModeUnlocked: Boolean; virtual;');
    RegisterMethod('procedure SortRows(ByCol: Integer; SortMode: TKGridSortMode); virtual;');
    RegisterMethod('procedure UnlockSortMode; virtual;');
    RegisterMethod('procedure UnselectRange;');
    RegisterMethod('procedure UpdateSortMode(ACol, ARow: Integer); virtual;');}

    RegisterProperty('Flat', 'Boolean', iptRW);
    RegisterProperty('Cells', 'String Integer Integer', iptRW);
    RegisterProperty('CellSpan', 'TKGridCellSpan Integer Integer', iptRW);
    RegisterProperty('Col', 'Integer', iptRW);
    RegisterProperty('ColCount', 'Integer', iptRW);
    RegisterProperty('Colors', 'TKGridColors', iptRW);
    RegisterProperty('ColWidths', 'Integer Integer', iptRW);
    RegisterProperty('DefaultColWidth', 'Integer', iptRW);
    RegisterProperty('DefaultDrawing', 'Boolean', iptRW);
    RegisterProperty('DefaultRowHeight', 'Integer', iptRW);
    RegisterProperty('DisabledDrawStyle', 'TKGridDisabledDrawStyle', iptRW);
    RegisterProperty('FixedCols', 'Integer', iptRW);
    RegisterProperty('FixedRows', 'Integer', iptRW);
    RegisterProperty('GridLineWidth', 'Integer', iptRW);
    RegisterProperty('LeftCol', 'Integer', iptRW);
    RegisterProperty('MinColWidth', 'Integer', iptRW);
    RegisterProperty('MinRowHeight', 'Integer', iptRW);
    RegisterProperty('MouseCellHintTime', 'Cardinal', iptRW);
    RegisterProperty('Objects', 'TObject Integer Integer', iptRW);
    RegisterProperty('Options', 'TKGridOptions', iptRW);
    RegisterProperty('OptionsEx', 'TKGridOptionsEx', iptRW);
    RegisterProperty('ParentColor', 'Boolean', iptRW);
    RegisterProperty('RangeSelectStyle', 'TKGridRangeSelectStyle', iptRW);
    RegisterProperty('Row', 'Integer', iptRW);
    RegisterProperty('RowCount', 'Integer', iptRW);
    RegisterProperty('RowHeights', 'Integer, Integer', iptRW);
    RegisterProperty('ScrollBars', 'TScrollStyle', iptRW);
    RegisterProperty('ScrollModeHorz', 'TKGridScrollMode', iptRW);
    RegisterProperty('ScrollModeVert', 'TKGridScrollMode', iptRW);
    RegisterProperty('ScrollSpeed', 'Cardinal', iptRW);
    RegisterProperty('Selection', 'TKGridRect', iptRW);
    RegisterProperty('SelectionCount', 'Integer', iptR);
    RegisterProperty('SelectionRect', 'TRect', iptR);
    RegisterProperty('Selections', 'TKGridRect, Integer', iptRW);
    RegisterProperty('SizingStyle', 'TKGridSizingStyle', iptRW);
    RegisterProperty('TopRow', 'Integer', iptRW);
    RegisterProperty('VisibleColCount', 'Integer', iptR);
    RegisterProperty('VisibleRowCount', 'Integer', iptR);
    RegisterProperty('OnDrawCell', 'TKGridDrawCellEvent', iptRW);
    RegisterProperty('OnMeasureCell', 'TKGridMeasureCellEvent', iptRW);
    RegisterProperty('OnMouseCellHint', 'TKGridCellHintEvent', iptRW);
    RegisterProperty('OnMouseClickCell', 'TKGridCellEvent', iptRW);
    RegisterProperty('OnMouseDblClickCell', 'TKGridCellEvent', iptRW);
    RegisterProperty('OnMouseEnterCell', 'TKGridCellEvent', iptRW);
    RegisterProperty('OnMouseLeaveCell', 'TKGridCellEvent', iptRW);
    RegisterProperty('OnSelectCell', 'TKGridSelectCellEvent', iptRW);
    RegisterProperty('OnSelectionChanged', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptRW);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptRW);
  end;

  with Cl.AddClassN(Cl.FindClass('TKGrid'), 'TdxPivotGrid') do
  begin

  end;
end;

procedure SIRegister_dxSQLQuery(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TObject'), 'TdxSQLQuery') do
  begin
    RegisterMethod('constructor Create(const SQL: String)');
    RegisterMethod('procedure Open');
    RegisterMethod('procedure Close');
    RegisterMethod('function Opened: Boolean');
    RegisterMethod('procedure MoveFirst');
    RegisterMethod('procedure MovePrior');
    RegisterMethod('procedure MoveNext');
    RegisterMethod('procedure MoveLast');
    RegisterMethod('procedure MoveBy(Distance: Integer)');
    RegisterMethod('procedure MoveTo(aRecNo: Integer)');
    RegisterMethod('function BOF: Boolean');
    RegisterMethod('function EOF: Boolean');
    RegisterMethod('function RecNo: Integer');
    RegisterMethod('function RecordCount: Integer');
    RegisterMethod('function FieldCount: Integer');
    RegisterProperty('Fields', 'Variant String', iptR);
    SetDefaultPropery('Fields');
    RegisterProperty('Field', 'TField Integer', iptR);
    RegisterProperty('AsI', 'Integer String', iptR);
    RegisterProperty('AsF', 'Extended String', iptR);
    RegisterProperty('AsDT', 'TDateTime String', iptR);
    RegisterProperty('AsS', 'String String', iptR);
  end;
end;

{procedure SIRegister_dxQuery(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TRpTotalFunc', '(tfNone, tfSum, tfAvg, tfMax, tfMin, tfCount, tfProfit, tfDistCount, tfMergeAll, tfMerge)');
  Cl.AddTypeS('TRpSourceKind', '(skNone, skIncome, skOutcome)');
  Cl.AddTypeS('TRpFieldType', '(flNone, flText, flNumber, flDate, flBool, flObject, flTime, flCounter)');
  Cl.AddTypeS('TRpDateDetail', '(ddDay, ddWeek, ddMonth, ddQuart, ddHalfYear, ddYear)');

  with Cl.AddClassN(Cl.FindClass('TObject'), 'TdxQField') do
  begin
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('Name', 'String', iptR);
    RegisterProperty('Func', 'TRpTotalFunc', iptR);
    RegisterProperty('FieldType', 'TRpFieldType', iptR);
  end;
  with Cl.AddClassN(Cl.FindClass('TObject'), 'TdxQSource') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('function AddField(const aFieldName, aName: String; aFunc: TRpTotalFunc): TdxQField');
  end;
  with Cl.AddClassN(Cl.FindClass('TObject'), 'TdxQuery') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('function AddSource(const aFormName, aSubFormName, aFilter: String; aKind: TRpSourceKind): TdxQSource');
    RegisterMethod('procedure AddSorting(const aName: String; Desc: Boolean)');
    RegisterMethod('procedure GroupByDate(const DateField: String; DateDetail: TRpDateDetail)');
    RegisterMethod('procedure Open');
    RegisterMethod('procedure Close');
    RegisterMethod('function Opened: Boolean');
    RegisterMethod('procedure MoveFirst');
    RegisterMethod('procedure MovePrior');
    RegisterMethod('procedure MoveNext');
    RegisterMethod('procedure MoveLast');
    RegisterMethod('procedure MoveBy(Distance: Integer)');
    RegisterMethod('procedure MoveTo(aRecNo: Integer)');
    RegisterMethod('function BOF: Boolean');
    RegisterMethod('function EOF: Boolean');
    RegisterMethod('function RecNo: Integer');
    RegisterMethod('function RecId: Integer');
    RegisterMethod('function RecordCount: Integer');
    RegisterProperty('Fields', 'Variant String', iptR);
    SetDefaultPropery('Fields');
    RegisterProperty('AsI', 'Integer String', iptR);
    RegisterProperty('AsF', 'Extended String', iptR);
    RegisterProperty('AsDT', 'TDateTime String', iptR);
    RegisterProperty('AsS', 'String String', iptR);
  end;
end; }

procedure SIRegister_dxQueryGrid(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TMyDBGrid'), 'TdxQueryGrid') do
  begin
    RegisterMethod('procedure MoveFirst');
    RegisterMethod('procedure MovePrior');
    RegisterMethod('procedure MoveNext');
    RegisterMethod('procedure MoveLast');
    RegisterMethod('procedure MoveBy(Distance: Integer)');
    RegisterMethod('procedure MoveTo(aRecNo: Integer)');
    RegisterMethod('function EOF: Boolean');
    RegisterMethod('function BOF: Boolean');
    RegisterMethod('function RecNo: Integer');
    RegisterMethod('function RecId: Integer');
    RegisterMethod('procedure EnableControls');
    RegisterMethod('procedure DisableControls');
    RegisterMethod('function ControlsDisabled: Boolean');
    RegisterMethod('function RecordCount: Integer');
    RegisterMethod('function Locate(const FieldName: String; FieldValue: Variant; aOptions: TLocateOptions): Boolean');
    RegisterMethod('function GotoRecord(aRecId: Integer): Boolean');
    RegisterMethod('procedure Refresh');
    RegisterProperty('QueryName', 'String', iptR);
    RegisterProperty('Fields', 'Variant String', iptR);
    SetDefaultPropery('Fields');
    RegisterProperty('AsI', 'Integer String', iptR);
    RegisterProperty('AsF', 'Extended String', iptR);
    RegisterProperty('AsDT', 'TDateTime String', iptR);
    RegisterProperty('AsS', 'String String', iptR);

    RegisterProperty('OnAfterClose', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterOpen', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterScroll', 'TNotifyEvent', iptRW);

    RegisterProperty('OnBeforeClose', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeOpen', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeScroll', 'TNotifyEvent', iptRW);

    RegisterProperty('OnPrintField', 'TPrintFieldEvent', iptRW);
    RegisterProperty('OnCreateForm', 'TCreateFormEvent', iptRW);
  end;
end;

procedure SIRegister_dxFile(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomDBEditButton'), 'TdxFile') do
  begin
    RegisterMethod('procedure LoadFromFile(const FileName: String)');
    RegisterMethod('procedure SaveToFile(const FileName: String)');
    RegisterMethod('procedure Clear');

    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('StorageType', 'Integer', iptR);
    RegisterProperty('StorageFolder', 'String', iptR);
    RegisterProperty('FieldSize', 'Integer', iptR);
    RegisterProperty('SourceFileName', 'String' ,iptR);
    RegisterProperty('StoredFileName', 'String' ,iptR);
    RegisterProperty('Description', 'String' ,iptRW);
  end;
end;

procedure SIRegister_dxDBImage(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TdxImage'), 'TdxDBImage') do
  begin
    RegisterMethod('procedure LoadFromFile(const FileName: String)');
    RegisterMethod('procedure SaveToFile(const FileName: String)');
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('StorageType', 'Integer', iptR);
    RegisterProperty('StorageFolder', 'String', iptR);
    RegisterProperty('ThumbSize', 'Integer', iptR);
    RegisterProperty('ReadOnly', 'Boolean' ,iptRW);
    RegisterProperty('SourceFileName', 'String' ,iptR);
    RegisterProperty('StoredFileName', 'String' ,iptR);
  end;
end;

procedure SIRegister_dxButton(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TBitBtn'), 'TdxButton') do
  begin
    RegisterProperty('OnClick', 'TNotifyEvent', iptRW);
  end;
end;

procedure SIRegister_dxCounter(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDBEdit'), 'TdxCounter') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('CheckExpression', 'String', iptR);
  end;
end;

procedure SIRegister_dxTimeEdit(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDBTimeEdit'), 'TdxTimeEdit') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('CurTime', 'Boolean', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
  end;
end;

procedure SIRegister_DBTimeEdit(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomDBEditButton'), 'TDBTimeEdit') do
  begin

  end;
end;

procedure SIRegister_dxObjectField(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDBEdit'), 'TdxObjectField') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('ObjId', 'Integer', iptR);
    RegisterProperty('FieldId', 'Integer', iptR);
  end;
end;

procedure SIRegister_dxShape(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TShape'), 'TdxShape') do
  begin

  end;
end;

procedure SIRegister_dxPageControl(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TTabSheet'), 'TdxTabSheet') do
  begin

  end;

  with Cl.AddClassN(Cl.FindClass('TPageControl'), 'TdxPageControl') do
  begin

  end;
end;

procedure SIRegister_PageControl(Cl: TPSPascalCompiler);
var
  PG: TPSCompileTimeClass;
begin
  Cl.AddTypeS('TTabPosition', '(tpTop, tpBottom, tpLeft, tpRight)');
  Cl.AddTypeS('TCTabControlOption', '(nboShowCloseButtons, nboMultiLine, nboHidePageListPopup, ' +
    'nboKeyboardTabSwitch, nboShowAddTabButton)');
  Cl.AddTypeS('TCTabControlOptions', 'set of TCTabControlOption');
  Cl.AddTypeS('TTabChangingEvent', 'procedure(Sender: TObject; var AllowChange: Boolean)');

  with Cl.AddClassN(Cl.FindClass('TWinControl'), 'TCustomTabControl') do
  begin

  end;

  PG := Cl.AddClassN(Cl.FindClass('TCustomTabControl'), 'TPageControl');


  with Cl.AddClassN(Cl.FindClass('TWinControl'), 'TCustomPage') do
  begin

  end;

  with Cl.AddClassN(Cl.FindClass('TCustomPage'), 'TTabSheet') do
  begin
    RegisterProperty('PageControl', 'TPageControl', iptRW);
    RegisterProperty('TabIndex', 'Integer', iptR);
    RegisterProperty('Caption', 'String', iptRW);
    RegisterProperty('ImageIndex', 'Integer', iptRW);
    RegisterProperty('PageIndex', 'Integer', iptRW);
    RegisterProperty('TabVisible', 'Boolean', iptRW);

    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
  end;

  with PG do
  begin
    RegisterProperty('ActivePage', 'TTabSheet', iptRW);
    RegisterProperty('ActivePageIndex', 'Integer', iptRW);
    RegisterProperty('Images', 'TCustomImageList', iptRW);
    RegisterProperty('MultiLine', 'Boolean', iptRW);
    RegisterProperty('ShowTabs', 'Boolean', iptRW);
    RegisterProperty('TabIndex', 'Integer', iptRW);
    RegisterProperty('TabPosition', 'TTabPosition', iptRW);
    RegisterProperty('Options', 'TCTabControlOptions', iptRW);
    RegisterProperty('Pages', 'TTabSheet Integer', iptR);
    RegisterProperty('PageCount', 'Integer', iptR);

    RegisterProperty('OnChange', 'TNotifyEvent', iptRW);
    RegisterProperty('OnChanging', 'TTabChangingEvent', iptRW);
    RegisterProperty('OnCloseTabClicked', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    //RegisterProperty('OnResize', 'TNotifyEvent', iptRW);
  end;
end;

procedure SIRegister_ImageList(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TComponent'), 'TCustomImageList') do
  begin
    RegisterMethod('procedure BeginUpdate');
    RegisterMethod('procedure EndUpdate');
    RegisterMethod('function Add(Image, Mask: TBitmap): Integer');
    RegisterMethod('function AddFromFile(const FileName: String): Integer');
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure Delete(AIndex: Integer)');
    RegisterMethod('procedure GetBitmap(Index: Integer; Image: TBitmap)');
    RegisterProperty('Count', 'Integer', iptR);
  end;

  with Cl.AddClassN(Cl.FindClass('TCustomImageList'), 'TImageList') do
  begin
    RegisterProperty('Height', 'Integer', iptRW);
    RegisterProperty('Width', 'Integer', iptRW);
  end;
end;

procedure SIRegister_dxGroupBox(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TGroupBox'), 'TdxGroupBox') do
  begin

  end;
end;

procedure SIRegister_dxGrid(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TMyDBGrid'), 'TdxGrid') do
  begin
    RegisterMethod('function GetFieldName(Column: TColumn): String');
    RegisterProperty('Form', 'TdxForm', iptR);
  end;
end;

procedure SIRegister_DBGrid(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TTEXTSTYLE', 'record Alignment: TAlignment; Layout: TTextLayout; SingleLine: Boolean; Clipping: Boolean; ExpandTabs: Boolean; ShowPrefix: Boolean; WordBreak: Boolean; Opaque: Boolean; SystemFont: Boolean; RightToLeft: Boolean; EndEllipsis: Boolean; end;');
  Cl.AddTypeS('TAutoAdvance', '(aaNone,aaDown,aaRight,aaLeft, aaRightDown, aaLeftDown, aaRightUp, aaLeftUp)');
  Cl.AddTypeS('TCellHintPriority', '(chpAll, chpAllNoDefault, chpTruncOnly)');
  //Cl.AddTypeS('TColumnButtonStyle', '(cbsAuto, cbsEllipsis, cbsNone, cbsPickList, cbsCheckboxColumn, cbsButton, cbsButtonColumn)');
  //Cl.AddTypeS('TPrefixOption', '(poNone, poHeaderClick)');
  //Cl.AddTypeS('TGridZone', '(gzNormal, gzFixedCols, gzFixedRows, gzFixedCells, gzInvalid)');
  //Cl.AddTypeS('TGridZoneSet', 'set of TGridZone');
  Cl.AddTypeS('TDBGridOption', '(dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowEditor, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgMultiselect, dgHeaderHotTracking, dgHeaderPushedLook, dgPersistentMultiSelect, dgAutoSizeColumns, dgAnyButtonCanSelect, dgDisableDelete, dgDisableInsert, dgCellHints, dgTruncCellHints, dgCellEllipsis, dgRowHighlight, dgThumbTracking)');
  Cl.AddTypeS('TDBGridOptions', 'set of TDBGridOption');
  Cl.AddTypeS('TDbGridExtraOption', '(dgeAutoColumns, dgeCheckboxColumn)');
  Cl.AddTypeS('TDBGridExtraOptions', 'set of TDBGridExtraOption');
  Cl.AddTypeS('TTitleStyle', '(tsLazarus, tsStandard, tsNative)');

  Cl.AddClassN(cl.FindClass('TCOLLECTIONITEM'), 'TGRIDCOLUMN');
  Cl.AddClassN(cl.FindClass('TGRIDCOLUMN'), 'TCOLUMN');

  with Cl.AddClassN(cl.FindClass('TPERSISTENT'), 'TGRIDCOLUMNTITLE') do
  begin
    //RegisterMethod('procedure FillTitleDefaultFont');
    //RegisterMethod('function IsDefault: Boolean');
    //RegisterProperty('Column', 'TColumn', iptr);

    RegisterProperty('Alignment', 'TAlignment', iptrw);
    RegisterProperty('Caption', 'String', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    //RegisterProperty('ImageIndex', 'Integer', iptrw);
    //RegisterProperty('ImageLayout', 'TButtonLayout', iptrw);
    RegisterProperty('Layout', 'TTextLayout', iptrw);
    //RegisterProperty('PrefixOption', 'TPrefixOption', iptrw);
  end;

  Cl.AddClassN(cl.FindClass('TCUSTOMCONTROL'), 'TCUSTOMGRID');
  Cl.AddClassN(cl.FindClass('TCUSTOMGRID'), 'TGRIDCONTROL');

  with Cl.FindClass('TGRIDCOLUMN') do
  begin
    //RegisterMethod('function IsDefault: Boolean');
    //RegisterMethod('procedure FillDefaultFont');
    //RegisterProperty('WidthChanged', 'Boolean', iptr);
    RegisterProperty('Grid', 'TDBGrid', iptr);

    RegisterProperty('Alignment', 'TAlignment', iptrw);
    //RegisterProperty('ButtonStyle', 'TColumnButtonStyle', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('DropDownRows', 'Longint', iptrw);
    //RegisterProperty('Expanded', 'Boolean', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('Layout', 'TTextLayout', iptrw);
    RegisterProperty('MinSize', 'Integer', iptrw);
    RegisterProperty('MaxSize', 'Integer', iptrw);
    //RegisterProperty('PickList', 'TStrings', iptrw);
    RegisterProperty('ReadOnly', 'Boolean', iptrw);
    RegisterProperty('SizePriority', 'Integer', iptrw);
    RegisterProperty('Title', 'TGridColumnTitle', iptrw);
    RegisterProperty('Width', 'Integer', iptrw);
  end;

  with cl.FindClass('TCOLUMN') do
  begin
    RegisterProperty('Field', 'TField', iptr);
    RegisterProperty('DesignIndex', 'Integer', iptr);

    //RegisterProperty('FieldName', 'String', iptrw);
    //RegisterProperty('DisplayFormat', 'String', iptrw);
  end;

  with Cl.AddClassN(cl.FindClass('TCOLLECTION'), 'TDBGRIDCOLUMNS') do
  begin
    RegisterProperty('Items', 'TColumn integer', iptrw);
    SetDefaultPropery('Items');
  end;

  Cl.AddTypeS('TDBGridClickEvent', 'procedure(Column: TColumn)');
  Cl.AddTypeS('TMovedEvent', 'procedure(Sender: TObject; FromIndex, ToIndex: Integer)');
  Cl.AddTypeS('TGridDrawState_', '(gdSelected, gdFocused, gdFixed, gdHot, gdPushed, gdRowHighlight)');
  Cl.AddTypeS('TGridDrawState', 'set of TGridDrawState_');
  Cl.AddTypeS('TDrawColumnCellEvent', 'procedure(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState)');
  Cl.AddTypeS('TDbGridCellHintEvent', 'procedure(Sender: TObject; Column: TColumn; var AText: String)');

  with Cl.AddClassN(cl.FindClass('TCUSTOMGRID'), 'TCUSTOMDBGRID') do
  begin

  end;

  with Cl.AddClassN(cl.FindClass('TCUSTOMDBGRID'), 'TDBGRID') do
  begin
    RegisterProperty('Columns', 'TDBGridColumns', iptR);
    RegisterProperty('BorderColor', 'TColor', iptrw);
    RegisterProperty('DefaultTextStyle', 'TTextStyle', iptrw);
    //RegisterProperty('EditorBorderStyle', 'TBorderStyle', iptrw);
    //RegisterProperty('EditorMode', 'Boolean', iptrw);
    RegisterProperty('ExtendedColSizing', 'Boolean', iptrw);
    //RegisterProperty('FastEditing', 'Boolean', iptrw);
    RegisterProperty('FocusColor', 'TColor', iptrw);
    RegisterProperty('FocusRectVisible', 'Boolean', iptrw);
    RegisterProperty('GridLineColor', 'TColor', iptrw);
    RegisterProperty('GridLineStyle', 'TPenStyle', iptrw);
    //RegisterProperty('InplaceEditor', 'TWinControl', iptr);
    RegisterProperty('SelectedColor', 'TColor', iptrw);
    //property SelectedRows;
    RegisterProperty('AlternateColor', 'TColor', iptrw);
    RegisterProperty('AutoAdvance', 'TAutoAdvance', iptrw);
    RegisterProperty('AutoEdit', 'Boolean', iptrw);
    RegisterProperty('AutoFillColumns', 'Boolean', iptrw);
    RegisterProperty('BorderStyle', 'TBorderStyle', iptrw);
    RegisterProperty('CellHintPriority', 'TCellHintPriority', iptrw);
    //RegisterProperty('DefaultDrawing', 'Boolean', iptrw);
    RegisterProperty('DefaultRowHeight', 'Integer', iptrw);
    RegisterProperty('FixedColor', 'TColor', iptrw);
    RegisterProperty('FixedCols', 'Integer', iptrw);
    RegisterProperty('FixedHotColor', 'TColor', iptrw);
    RegisterProperty('Flat', 'Boolean', iptrw);
    //RegisterProperty('HeaderHotZones', 'TGridZoneSet', iptrw);
    //RegisterProperty('HeaderPushZones', 'TGridZoneSet', iptrw);
    RegisterProperty('Options', 'TDBGridOptions', iptrw);
    RegisterProperty('OptionsExtra', 'TDBGridExtraOptions', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('ReadOnly', 'Boolean', iptrw);
    RegisterProperty('Scrollbars', 'TScrollStyle', iptrw);
    RegisterProperty('SelectedField', 'TField', iptR);
    RegisterProperty('SelectedColumn', 'TColumn', iptR);
    RegisterProperty('SelectedIndex', 'Integer', iptR);
    RegisterProperty('TabAdvance', 'TAutoAdvance', iptrw);
    RegisterProperty('TitleFont', 'TFont', iptrw);
    //RegisterProperty('TitleImageList', 'TImageList', iptrw);
    RegisterProperty('TitleStyle', 'TTitleStyle', iptrw);
    RegisterProperty('UseXORFeatures', 'Boolean', iptrw);
    RegisterProperty('OnCellClick', 'TDBGridClickEvent', iptRW);
    RegisterProperty('OnColEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnColExit', 'TNotifyEvent', iptRW);
    RegisterProperty('OnColumnMoved', 'TMovedEvent', iptRW);
    RegisterProperty('OnColumnSized', 'TNotifyEvent', iptRW);
    RegisterProperty('OnDrawColumnCell', 'TDrawColumnCellEvent', iptRW);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptRW);
    RegisterProperty('OnGetCellHint', 'TDbGridCellHintEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptRW);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptRW);
    RegisterProperty('OnMouseUp', 'TNotifyEvent', iptRW);
  end;

  with Cl.AddClassN(cl.FindClass('TDBGRID'), 'TMYDBGRID') do
  begin
    RegisterMethod('procedure MoveToSelectedRow(i: Integer)');
    RegisterMethod('procedure ClearRowsSelection');
    RegisterMethod('function CurrentRowSelected: Boolean');
    RegisterProperty('SelectedRowCount', 'Integer', iptR);
  end;
end;

procedure SIRegister_dxImage(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TGraphicControl'), 'TdxImage') do
  begin
    RegisterMethod('procedure LoadFromFile(const FileName: String)');
    RegisterMethod('procedure SaveToFile(const FileName: String)');
    RegisterMethod('procedure Clear; virtual');

    RegisterProperty('Bitmap', 'TBitmap', iptRW);
    RegisterProperty('Center', 'Boolean', iptRW);
    RegisterProperty('Proportional', 'Boolean', iptRW);
    RegisterProperty('Stretch', 'Boolean', iptRW);
    RegisterProperty('Quality', 'Integer', iptRW);
    RegisterProperty('KeepSize', 'Boolean', iptRW);

    RegisterProperty('OnDblClick', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptRW);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptRW);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptRW);
    RegisterProperty('OnPaint', 'TNotifyEvent', iptRW);
  end;
end;

procedure SIRegister_dxLookupComboBox(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDBComboBox'), 'TdxLookupComboBox') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('FieldSize', 'Integer', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
    RegisterProperty('Filter', 'String', iptR);
    RegisterProperty('KeyValue', 'Variant', iptRW);
    RegisterProperty('Button', 'TSpeedButton', iptR);

    //RegisterProperty('OnDropDown', 'TNotifyEvent', iptRW);
    //RegisterProperty('OnChange', 'TNotifyEvent', iptRW);
    RegisterProperty('OnCreateListWindow', 'TCreateListWindowEvent', iptRW);
    RegisterProperty('OnCreateForm', 'TCreateFormEvent', iptRW);
    RegisterProperty('OnUTF8KeyPress', 'TUTF8KeyPressEvent', iptRW);
  end;
end;

procedure SIRegister_dxComboBox(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDBComboBox'), 'TdxComboBox') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('FieldSize', 'Integer', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
    RegisterProperty('Filter', 'String', iptR);
    RegisterProperty('OnUTF8KeyPress', 'TUTF8KeyPressEvent', iptRW);

    //RegisterProperty('OnDropDown', 'TNotifyEvent', iptRW);
    //RegisterProperty('OnChange', 'TNotifyEvent', iptRW);
  end;
end;

procedure SIRegister_DBComboBox(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomDBComboBox'), 'TDBComboBox') do
  begin
    RegisterProperty('ItemWidth', 'Integer', iptrw);
    RegisterProperty('ItemHeight', 'Integer', iptrw);
    //RegisterProperty('MaxLength', 'Integer', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    //RegisterProperty('Sorted', 'Boolean', iptRW);
    //RegisterProperty('OnChange', 'TNotifyEvent', iptRW);
    //RegisterProperty('OnCloseUp', 'TNotifyEvent', iptRW);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptRW);
    //RegisterProperty('OnEditingDone', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    //RegisterProperty('OnSelect', 'TNotifyEvent', iptRW);
    //RegisterProperty('OnDropDown', 'TNotifyEvent', iptRW);
    RegisterProperty('OnDrawItem', 'TDrawItemEvent', iptRW);
  end;
end;

procedure SIRegister_CustomDBComboBox(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomComboBox'), 'TCustomDBComboBox') do
  begin
    RegisterProperty('Field', 'TField', iptR);
  end;
end;

procedure SIRegister_dxCheckBox(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDBCheckBox'), 'TdxCheckBox') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('CheckedText', 'String', iptRW);
    RegisterProperty('UnCheckedText', 'String', iptRW);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
  end;
end;

procedure SIRegister_DBCheckBox(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomCheckBox'), 'TDBCheckBox') do
  begin
    RegisterProperty('Checked', 'Boolean', iptRW);
    RegisterProperty('Field', 'TField', iptR);
    RegisterProperty('ReadOnly', 'Boolean', iptRW);
    RegisterProperty('OnChange', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
  end;
end;

procedure SIRegister_dxMemo(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDBMemo'), 'TdxMemo') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('FieldSize', 'Integer', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
    RegisterProperty('Delimiter', 'String', iptR);
    RegisterProperty('Filter', 'String', iptR);
    RegisterProperty('Button', 'TSpeedButton', iptR);
    RegisterProperty('OnCreateListWindow', 'TCreateListWindowEvent', iptRW);
    RegisterProperty('OnUTF8KeyPress', 'TUTF8KeyPressEvent', iptRW);
  end;
end;

procedure SIRegister_DBMemo(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomMemo'), 'TDBMemo') do
  begin
    RegisterProperty('Field', 'TField', iptR);
    //RegisterProperty('OnChange', 'TNotifyEvent', iptRW);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptRW);
    //RegisterProperty('OnEditingDone', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
  end;
end;

procedure SIRegister_dxDateEdit(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDBDateEditEx'), 'TdxDateEdit') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('DateNow', 'Boolean', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
  end;
end;

procedure SIRegister_DBDateEdit(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomDBEditButton'), 'TDBDateEditEx') do
  begin

  end;
end;

procedure SIRegister_dxCalcEdit(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDBCalcEdit'), 'TdxCalcEdit') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('Precision', 'Integer', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
    RegisterProperty('MinValue', 'Double', iptR);
    RegisterProperty('MaxValue', 'Double', iptR);
  end;
end;

procedure SIRegister_DBCalcEdit(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomDBEditButton'), 'TDBCalcEdit') do
  begin

  end;
end;

procedure SIRegister_CustomDBEditButton(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDBEdit'), 'TCustomDBEditButton') do
  begin
    RegisterProperty('Button', 'TSpeedButton', iptR);
    RegisterProperty('ButtonOnlyWhenFocused', 'Boolean', iptRW);
    RegisterProperty('HideButton', 'Boolean', iptRW);
  end;
end;

procedure SIRegister_dxEdit(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDBEdit'), 'TdxEdit') do
  begin
  	RegisterMethod('function ValidateText: Boolean');
    RegisterMethod('function MaskTextEmpty: Boolean');
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('FieldSize', 'Integer', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
    RegisterProperty('EditMask', 'String', iptRW);
    RegisterProperty('OnUTF8KeyPress', 'TUTF8KeyPressEvent', iptRW);
  end;
end;

procedure SIRegister_DBEdit(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomEdit'), 'TDBEdit') do
  begin
    RegisterProperty('Field', 'TField', iptR);
    //RegisterProperty('CustomEditMask', 'Boolean', iptRW);
    //RegisterProperty('OnChange', 'TNotifyEvent', iptRW);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptRW);
    //RegisterProperty('OnEditingDone', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
  end;
end;

procedure SIRegister_MaskEditEx(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomEdit'), 'TMaskEditEx') do
  begin
    RegisterMethod('function ValidateText: Boolean');
    RegisterMethod('function MaskTextEmpty: Boolean');
    RegisterProperty('IsMasked', 'Boolean', iptR);
    RegisterProperty('EditText', 'String', iptRW);
    RegisterProperty('EditMask', 'String', iptRW);
    RegisterProperty('SpaceChar', 'Char', iptRW);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnEditingDone', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptrw);
  end;
end;

{procedure SIRegister_CustomMaskEdit(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TCustomEdit'), 'TCustomMaskEdit') do
  begin
    RegisterMethod('procedure ValidateEdit; virtual;');
  end;
end;   }

procedure SIRegister_dxLabel(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TLabel'), 'TdxLabel') do
  begin
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('Expression', 'String', iptR);
  end;
end;

procedure SIRegister_dxTypes(Cl: TPSPascalCompiler);
begin
  Cl.AddClassN(Cl.FindClass('TCustomPanel'), 'TdxForm');
  Cl.AddTypeS('TValidateEvent', 'procedure (Sender: TObject; var Ok: Boolean)');
  Cl.AddTypeS('TPrintFieldEvent', 'procedure (Sender: TObject; const FieldName: String; var FieldValue: String; var Ok: Boolean)');
  Cl.AddTypeS('TFieldChangeEvent', 'procedure (Sender, Control: TObject; const FieldName: String)');
  Cl.AddTypeS('TdxFormState', '(fsLoad, fsBrowse, fsFind, fsInsert, fsEdit, fsDuplicate, fsPrint, fsRecalc)');
  Cl.AddTypeS('TAccessStatus', '(asOk, asCantAppend, asCantEdit, asCantDelete, asModified, asDeleted, asLocked)');
  Cl.AddTypeS('TViewType', '(vtGridTop, vtGridBottom, vtGridLeft, vtGridRight, vtGridOnly, vtWithoutGrid, vtSimpleForm, vtDefault)');
  Cl.AddTypeS('TCreateFormEvent', 'procedure (Sender: TObject; Form: TdxForm)');
  Cl.AddTypeS('TCreateListWindowEvent', 'procedure (Sender: TObject; aWindow: TForm)');
  Cl.AddTypeS('TCreateReportWindowEvent', 'procedure (Sender: TObject; aWindow: TForm)');
  Cl.AddTypeS('TParamNotifyEvent', 'procedure (Sender: TObject; const ParamName: String)');
end;

procedure SIRegister_dxForm(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TObject'), 'TFilterField') do
  begin
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('IsNot', 'Boolean', iptRW);
    RegisterProperty('IsNull', 'Boolean', iptRW);
    RegisterProperty('Values', 'TStringList', iptRW);
    RegisterProperty('Value', 'String Integer', iptR);
		RegisterProperty('EndValue', 'String Integer', iptR);
  end;

  with Cl.AddClassN(Cl.FindClass('TObject'), 'TFilterObject') do
  begin
  	RegisterMethod('function AddField(const FieldName: String): TFilterField');
    RegisterMethod('function FindField(const FieldName: String): TFilterField');
    RegisterMethod('procedure DeleteField(F: TFilterField)');
    RegisterMethod('procedure Clear');
    RegisterProperty('Fields', 'TFilterField Integer', iptR);
    SetDefaultPropery('Fields');
    RegisterProperty('Count', 'Integer', iptR);
  end;

  with Cl.FindClass('TdxForm') do
  begin
    RegisterMethod('procedure Append');
    RegisterMethod('procedure Edit');
    RegisterMethod('function Delete: Boolean');
    RegisterMethod('procedure Post');
    RegisterMethod('procedure Cancel');
    RegisterMethod('procedure Refresh');
    RegisterMethod('procedure MoveFirst');
    RegisterMethod('procedure MovePrior');
    RegisterMethod('procedure MoveNext');
    RegisterMethod('procedure MoveLast');
    RegisterMethod('procedure MoveBy(Distance: Integer)');
    RegisterMethod('procedure MoveTo(aRecNo: Integer)');
    RegisterMethod('function Bof: Boolean');
    RegisterMethod('function Eof: Boolean');
    RegisterMethod('function RecNo: Integer');
    RegisterMethod('function RecId: Integer');
    RegisterMethod('function RecordCount: Integer');
    RegisterMethod('procedure Print(const TemplateName, OutFileName: String; var Errs: String; aOpenFile: Boolean)');
    {RegisterMethod('function FindFirst(const Expression: String): Boolean');
    RegisterMethod('function FindNext: Boolean');
    RegisterMethod('function FindPrior: Boolean'); }
    RegisterMethod('function Locate(const FieldName: String; FieldValue: Variant; Options: TLocateOptions): Boolean');
    RegisterMethod('function GotoRecord(aRecId: Integer): Boolean');
    RegisterMethod('procedure DisableControls');
    RegisterMethod('procedure EnableControls');
    RegisterMethod('function ControlsDisabled: Boolean');
    RegisterMethod('function CanAppend: TAccessStatus');
    RegisterMethod('function CanEdit: TAccessStatus');
    RegisterMethod('function CanDelete: TAccessStatus');
    //RegisterMethod('procedure AddUserFilter(const FieldName: String; aNot, aNull: Boolean; Values: array of Variant)');
    //RegisterMethod('procedure ClearUserFilter');
    RegisterMethod('procedure Open');
    RegisterMethod('procedure OpenRecord(RecId: Integer)');
    RegisterMethod('procedure OpenRecords(const Filter: String; SelCond: Boolean)');
    RegisterMethod('function Opened: Boolean');
    RegisterMethod('procedure Close');
    RegisterMethod('function Validate: Boolean');

    RegisterProperty('Fields', 'Variant String', iptRW);
    SetDefaultPropery('Fields');
    RegisterProperty('AsI', 'Integer String', iptR);
    RegisterProperty('AsF', 'Extended String', iptR);
    RegisterProperty('AsDT', 'TDateTime String', iptR);
    RegisterProperty('AsS', 'String String', iptR);
    RegisterProperty('OldValues', 'Variant String', iptR);

    RegisterProperty('State', 'TDataSetState', iptR);
    RegisterProperty('Forms', 'TdxForm String', iptR);
    RegisterProperty('FormByIndex', 'TdxForm Integer', iptR);
    RegisterProperty('FormCount', 'Integer', iptR);
    RegisterProperty('Queries', 'TdxQueryGrid String', iptR);
    RegisterProperty('QueryByIndex', 'TdxQueryGrid Integer', iptR);
    RegisterProperty('QueryCount', 'Integer', iptR);
    RegisterProperty('Grid', 'TdxGrid', iptR);
    RegisterProperty('EditWindow', 'TEditWindow', iptR);
    RegisterProperty('Params', 'TParamList', iptR);
    RegisterProperty('ParentForm', 'TdxForm', iptR);

    RegisterProperty('OnAfterCancel', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterClose', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterDelete', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterEdit', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterInsert', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterOpen', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterPost', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterScroll', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterDuplicate', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterPrint', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterRecalculate', 'TNotifyEvent', iptRW);

    RegisterProperty('OnBeforeCancel', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeClose', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeDelete', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeEdit', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeInsert', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeOpen', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforePost', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeScroll', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeDuplicate', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforePrint', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeRecalculate', 'TNotifyEvent', iptRW);

    RegisterProperty('OnValidate', 'TValidateEvent', iptRW);
    RegisterProperty('OnPrintField', 'TPrintFieldEvent', iptRW);
    RegisterProperty('OnFieldChange', 'TFieldChangeEvent', iptRW);

    RegisterProperty('OnDblClick', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptRW);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptRW);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptRW);

    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('PId', 'Integer', iptR);
    RegisterProperty('FormCaption', 'String', iptR);
    RegisterProperty('Filter', 'TFilterObject', iptR);
    RegisterProperty('Modified', 'Boolean', iptR);
    RegisterProperty('ViewType', 'TViewType', iptR);
  end;
end;

procedure SIRegister_Dialogs(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TMsgDlgType', '( mtWarning, mtError, mtInformation, mtConfirmation, mtCustom )');
  Cl.AddTypeS('TMsgDlgBtn', '( mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp )');
  Cl.AddTypeS('TMsgDlgButtons', 'set of TMsgDlgBtn');

  with CL.AddClassN(CL.FindClass('TComponent'),'TCommonDialog') do
  begin
    RegisterMethod('function Execute: boolean; virtual');
    RegisterProperty('Width', 'integer', iptRW);
    RegisterProperty('Height', 'integer', iptRW);
    RegisterProperty('Title', 'String', iptRW);
  end;

  with CL.AddClassN(CL.FindClass('TCommonDialog'), 'TFileDialog') do
  begin
    RegisterProperty('DefaultExt', 'string', iptRW);
    RegisterProperty('FileName', 'String', iptRW);
    RegisterProperty('Filter', 'String', iptRW);
    RegisterProperty('FilterIndex', 'Integer', iptRW);
    RegisterProperty('InitialDir', 'string', iptRW);
  end;

  Cl.AddTypeS('TOpenOption', '(ofReadOnly, ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofShowHelp, ofNoValidate, ofAllowMultiSelect, ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofCreatePrompt, ofShareAware, ofNoReadOnlyReturn, ofNoTestFileCreate, ofNoNetworkButton, ofNoLongNames, ofOldStyleDialog, ofNoDereferenceLinks, ofEnableIncludeNotify, ofEnableSizing, ofDontAddToRecent, ofForceShowHidden, ofViewDetail, ofAutoPreview)');
  Cl.AddTypeS('TOpenOptions', 'set of TOpenOption');

  with CL.AddClassN(CL.FindClass('TFileDialog'), 'TOpenDialog') do
  begin
    RegisterProperty('Options', 'TOpenOptions', iptRW);
  end;

  with CL.AddClassN(CL.FindClass('TOpenDialog'), 'TSaveDialog') do
  begin

  end;

  with CL.AddClassN(CL.FindClass('TOpenDialog'), 'TSelectDirectoryDialog') do
  begin

  end;

  Cl.AddTypeS('TPrintRange', '(prAllPages, prSelection, prPageNums, prCurrentPage)');
  Cl.AddTypeS('TPrintDialogOption', '(poPrintToFile, poPageNums, poSelection, poWarning, poHelp, poDisablePrintToFile)');
  Cl.AddTypeS('TPrintDialogOptions', 'set of TPrintDialogOption');

  with CL.AddClassN(CL.FindClass('TCommonDialog'), 'TPrintDialog') do
  begin
    RegisterProperty('Collate', 'Boolean', iptRW);
    RegisterProperty('Copies', 'Integer', iptRW);
    RegisterProperty('FromPage', 'Integer', iptRW);
    RegisterProperty('MinPage', 'Integer', iptRW);
    RegisterProperty('MaxPage', 'Integer', iptRW);
    RegisterProperty('Options', 'TPrintDialogOptions', iptRW);
    RegisterProperty('PrintToFile', 'Boolean', iptRW);
    RegisterProperty('PrintRange', 'TPrintRange', iptRW);
    RegisterProperty('ToPage', 'Integer', iptRW);
  end;
end;

procedure SIRegister_dxCtrls(Cl: TPSPascalCompiler);
begin
  SIRegister_ControlsEx(Cl);
  SIRegister_StdCtrlsEx(Cl);
  SIRegister_TreeView(Cl);
  SIRegister_TField(Cl);
  SIRegister_dxTypes(Cl);
  SIRegister_dxLabel(Cl);
  //SIRegister_CustomMaskEdit(Cl);
  SIRegister_MaskEditEx(Cl);
  SIRegister_DBEdit(Cl);
  SIRegister_dxEdit(Cl);
  SIRegister_CustomDBEditButton(Cl);
  SIRegister_DBCalcEdit(Cl);
  SIRegister_dxCalcEdit(Cl);
  SIRegister_DBDateEdit(Cl);
  SIRegister_dxDateEdit(Cl);
  SIRegister_DBMemo(Cl);
  SIRegister_dxMemo(Cl);
  SIRegister_DBCheckBox(Cl);
  SIRegister_dxCheckBox(Cl);
  SIRegister_CustomDBComboBox(Cl);
  SIRegister_DBComboBox(Cl);
  SIRegister_dxComboBox(Cl);
  SIRegister_dxLookupComboBox(Cl);
  SIRegister_dxImage(Cl);
  SIRegister_DBGrid(Cl);
  SIRegister_dxGrid(Cl);
  SIRegister_dxGroupBox(Cl);
  SIRegister_ImageList(Cl);
  SIRegister_PageControl(Cl);
  SIRegister_dxPageControl(Cl);
  SIRegister_dxShape(Cl);
  SIRegister_dxObjectField(Cl);
  SIRegister_DBTimeEdit(Cl);
  SIRegister_dxTimeEdit(Cl);
  SIRegister_dxCounter(Cl);
  SIRegister_dxButton(Cl);
  SIRegister_dxDBImage(Cl);
  SIRegister_dxFile(Cl);
  SIRegister_More(Cl);
  SIRegister_dxQueryGrid(Cl);
  SIRegister_dxForm(Cl);
  SIRegister_dxSQLQuery(Cl);
  SIRegister_KGrid(Cl);
  SIRegister_ReportWindow(Cl);
  SIRegister_MenuItemConsts(Cl);
  SIRegister_Dialogs(Cl);
  SIRegister_IniFiles(Cl);
  SIRegister_Clipboard(Cl);
end;

procedure SIRegister_TFIELD(CL: TPSPascalCompiler);
Begin
  cl.AddTypeS('TFieldType', '(ftUnknown, ftString, ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,'+
    'ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,'+
    'ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd)');
  cl.AddTypeS('TFieldChars', 'set of Char');
  cl.addTypeS('TLocateOption','(loCaseInsensitive, loPartialKey)');
  cl.addtypes('TLocateOptions','set of TLocateOption');
  Cl.AddTypeS('TDataSetState', '(dsInactive, dsBrowse, dsEdit, dsInsert, dsSetKey, dsCalcFields, dsFilter, dsNewValue, dsOldValue, dsCurValue, dsBlockRead, dsInternalCalc, dsOpening)');

  With Cl.AddClassN(Cl.FindClass('TComponent'), 'TField') do
  begin
    RegisterMethod('procedure Clear; virtual');
    RegisterMethod('procedure FocusControl');
    //RegisterMethod('function IsValidChar(InputChar: Char): Boolean');
    RegisterProperty('AsBoolean', 'Boolean', iptrw);
    RegisterProperty('AsCurrency', 'Currency', iptrw);
    RegisterProperty('AsDateTime', 'TDateTime', iptrw);
    RegisterProperty('AsFloat', 'Double', iptrw);
    RegisterProperty('AsInteger', 'LongInt', iptrw);
    RegisterProperty('AsString', 'string', iptrw);
    RegisterProperty('AsVariant', 'Variant', iptrw);
    RegisterProperty('CanModify', 'Boolean', iptr);
    RegisterProperty('DataType', 'TFieldType', iptr);
    //RegisterProperty('EditMask', 'String', iptrw);
    RegisterProperty('IsNull', 'Boolean', iptr);
    RegisterProperty('OldValue', 'Variant', iptr);
    //RegisterProperty('Text', 'string', iptrw);
    //RegisterProperty('ValidChars', 'TFieldChars', iptrw);
    RegisterProperty('Value', 'Variant', iptrw);
    RegisterProperty('Alignment', 'TAlignment', iptrw);
    RegisterProperty('ReadOnly', 'Boolean', iptrw);
    //RegisterProperty('DisplayFormat', 'String', iptrw);
    //RegisterProperty('EditFormat', 'String', iptrw);
    //RegisterProperty('InsertState', 'Boolean', iptr);
    //RegisterProperty('EditState', 'Boolean', iptr);
    RegisterProperty('State', 'TDataSetState', iptr);
    RegisterProperty('FieldName', 'String', iptr);
  end;
end;

procedure SIRegister_StdCtrlsEx(Cl: TPSPascalCompiler);
begin
  with Cl.FindClass('TCustomEdit') do
  begin
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
    RegisterProperty('Text', 'String', iptRW);
  end;
  with Cl.FindClass('TCustomMemo') do
  begin
    RegisterProperty('HorzScrollBar', 'TControlScrollBar', iptRW);
    RegisterProperty('VertScrollBar', 'TControlScrollBar', iptRW);
    RegisterProperty('ScrollBars', 'TScrollStyle', iptRW);
    RegisterProperty('WantReturns', 'Boolean', iptRW);
    RegisterProperty('WantTabs', 'Boolean', iptRW);
    RegisterProperty('WordWrap', 'Boolean', iptRW);
  end;
  Cl.AddTypeS('TComboBoxAutoCompleteTextOption', '(cbactEnabled, cbactEndOfLineComplete, cbactRetainPrefixCase, cbactSearchCaseSensitive, cbactSearchAscending)');
  Cl.AddTypeS('TComboBoxAutoCompleteText', 'set of TComboBoxAutoCompleteTextOption');
  with Cl.FindClass('TCustomComboBox') do
  begin
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
  with Cl.FindClass('TCustomCheckBox') do
  begin
    RegisterProperty('AllowGrayed', 'Boolean', iptRW);
    RegisterProperty('State', 'TCheckBoxState', iptRW);
    RegisterProperty('OnChange', 'TNotifyEvent', iptRW);
  end;
  with Cl.FindClass('TCustomListBox') do
  begin
    RegisterProperty('BorderStyle', 'TBorderStyle', iptRW);
    RegisterProperty('Columns', 'Integer', iptRW);
    RegisterProperty('ExtendedSelect', 'Boolean', iptRW);
    RegisterProperty('IntegralHeight', 'Boolean', iptRW);
    RegisterProperty('ItemHeight', 'Integer', iptRW);
    RegisterProperty('MultiSelect', 'Boolean', iptRW);
    RegisterProperty('Sorted', 'Boolean', iptRW);
    RegisterProperty('Style', 'TListBoxStyle', iptRW);
  end;
end;

procedure SIRegister_ControlsEx(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TUtf8Char', 'String');
  Cl.AddTypeS('TUtf8KeyPressEvent', 'procedure(Sender: TObject; var UTF8Key: TUTF8Char)');

  with Cl.AddClassN(Cl.FindClass('TPersistent'), 'TControlBorderSpacing') do
  begin
    RegisterProperty('Left', 'Integer', iptRW);
    RegisterProperty('Top', 'Integer', iptRW);
    RegisterProperty('Right', 'Integer', iptRW);
    RegisterProperty('Bottom', 'Integer', iptRW);
    RegisterProperty('Around', 'Integer', iptRW);
    RegisterProperty('InnerBorder', 'Integer', iptRW);
  end;

  with Cl.FindClass('TControl') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptRW);
    RegisterProperty('AutoSize', 'Boolean', iptRW);
    RegisterProperty('BorderSpacing', 'TControlBorderSpacing', iptRW);
    RegisterProperty('Caption', 'String', iptRW);
    RegisterProperty('Color', 'TColor', iptRW);
    RegisterProperty('Font', 'TFont', iptRW);
    RegisterProperty('PopupMenu', 'TPopupMenu', iptRW);
    RegisterProperty('OnChangeBounds', 'TNotifyEvent', iptRW);
    RegisterProperty('OnClick', 'TNotifyEvent', iptRW);
    RegisterProperty('OnResize', 'TNotifyEvent', iptRW);
    RegisterProperty('OnShowHint', 'TControlShowHintEvent', iptRW);
  end;

  with Cl.FindClass('TWinControl') do
  begin
    RegisterProperty('BorderWidth', 'Integer', iptRW);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnExit', 'TNotifyEvent', iptRW);
    RegisterProperty('OnKeyDown', 'TKeyEvent', iptRW);
    RegisterProperty('OnKeyPress', 'TKeyPressEvent', iptRW);
    RegisterProperty('OnKeyUp', 'TKeyEvent', iptRW);
    //RegisterProperty('OnUTF8KeyPress', 'TUTF8KeyPressEvent', iptRW);
  end;

  with Cl.FindClass('TGraphicControl') do
  begin
    RegisterProperty('Canvas', 'TCanvas', iptR);
  end;

  with Cl.FindClass('TCustomControl') do
  begin
    RegisterProperty('BorderStyle', 'TBorderStyle', iptRW);
    RegisterProperty('Canvas', 'TCanvas', iptR);
    RegisterProperty('OnPaint', 'TNotifyEvent', iptRW);
  end;
end;

procedure SIRegister_TreeView(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TGraphicsDrawEffect', '(gdeNormal, gdeDisabled, gdeHighlighted, gdeShadowed, gde1Bit)');
  Cl.AddTypeS('TNodeState', '(nsCut, nsDropHilited, nsFocused, nsSelected, nsMultiSelected, nsExpanded, nsHasChildren, nsInTree, nsDeleting, nsBound)');
  Cl.AddTypeS('TNodeStates', 'set of TNodeState');

  with Cl.AddClassN(Cl.FindClass('TCustomControl'), 'TCustomTreeView') do
  begin

  end;

  Cl.AddClassN(Cl.FindClass('TCustomTreeView'), 'TTreeView');

  Cl.AddClassN(Cl.FindClass('TPersistent'), 'TTreeNodes');
  Cl.AddClassN(Cl.FindClass('TPersistent'), 'TTreeNode');

  Cl.AddTypeS('TNodeAttachMode', '(naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert, naInsertBehind)');
  Cl.AddTypeS('TTreeNodeCompare', 'function(Node1, Node2: TTreeNode): integer');

  with Cl.FindClass('TTreeNode') do
  begin
    RegisterMethod('constructor Create(AnOwner: TTreeNodes)');
    RegisterMethod('function AlphaSort: Boolean');
    RegisterMethod('function Bottom: integer');
    RegisterMethod('function BottomExpanded: integer');
    RegisterMethod('function CustomSort(SortProc: TTreeNodeCompare): Boolean');
    RegisterMethod('function DefaultTreeViewSort(Node1, Node2: TTreeNode): Integer');
    RegisterMethod('function DisplayExpandSignLeft: integer');
    RegisterMethod('function DisplayExpandSignRect: TRect');
    RegisterMethod('function DisplayExpandSignRight: integer');
    RegisterMethod('function DisplayIconLeft: integer');
    RegisterMethod('function DisplayRect(TextOnly: Boolean): TRect');
    RegisterMethod('function DisplayStateIconLeft: integer');
    RegisterMethod('function DisplayTextLeft: integer');
    RegisterMethod('function DisplayTextRight: integer');
    RegisterMethod('function EditText: Boolean');
    RegisterMethod('function FindNode(const NodeText: string): TTreeNode');
    RegisterMethod('function GetFirstChild: TTreeNode');
    RegisterMethod('function GetFirstVisibleChild: TTreeNode');
    RegisterMethod('function GetHandle: THandle');
    RegisterMethod('function GetLastChild: TTreeNode');
    RegisterMethod('function GetLastSibling: TTreeNode');
    RegisterMethod('function GetLastSubChild: TTreeNode');
    RegisterMethod('function GetLastVisibleChild: TTreeNode');
    RegisterMethod('function GetNext: TTreeNode');
    RegisterMethod('function GetNextChild(AValue: TTreeNode): TTreeNode');
    RegisterMethod('function GetNextExpanded: TTreeNode');
    RegisterMethod('function GetNextMultiSelected: TTreeNode');
    RegisterMethod('function GetNextSibling: TTreeNode');
    RegisterMethod('function GetNextSkipChildren: TTreeNode');
    RegisterMethod('function GetNextVisible: TTreeNode');
    RegisterMethod('function GetNextVisibleSibling: TTreeNode');
    RegisterMethod('function GetParentNodeOfAbsoluteLevel(TheAbsoluteLevel: integer): TTreeNode');
    RegisterMethod('function GetPrev: TTreeNode');
    RegisterMethod('function GetPrevChild(AValue: TTreeNode): TTreeNode');
    RegisterMethod('function GetPrevExpanded: TTreeNode');
    RegisterMethod('function GetPrevMultiSelected: TTreeNode');
    RegisterMethod('function GetPrevSibling: TTreeNode');
    RegisterMethod('function GetPrevVisible: TTreeNode');
    RegisterMethod('function GetPrevVisibleSibling: TTreeNode');
    RegisterMethod('function GetTextPath: string');
    RegisterMethod('function HasAsParent(AValue: TTreeNode): Boolean');
    RegisterMethod('function IndexOf(AValue: TTreeNode): Integer');
    RegisterMethod('function IndexOfText(const NodeText: string): Integer');
    RegisterMethod('procedure Collapse(Recurse: Boolean)');
    RegisterMethod('procedure ConsistencyCheck');
    RegisterMethod('procedure Delete');
    RegisterMethod('procedure DeleteChildren');
    RegisterMethod('procedure EndEdit(Cancel: Boolean)');
    RegisterMethod('procedure Expand(Recurse: Boolean)');
    RegisterMethod('procedure ExpandParents');
    RegisterMethod('procedure FreeAllNodeData');
    RegisterMethod('procedure MakeVisible');
    RegisterMethod('procedure MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode); virtual');
    RegisterMethod('procedure MultiSelectGroup');
    RegisterMethod('procedure Update');
    RegisterProperty('AbsoluteIndex', 'Integer', iptR);
    RegisterProperty('Count', 'Integer', iptR);
    RegisterProperty('Cut', 'Boolean', iptRW);
    RegisterProperty('Data', 'TObject', iptRW);
    RegisterProperty('Deleting', 'Boolean', iptR);
    RegisterProperty('Expanded', 'Boolean', iptRW);
    RegisterProperty('Focused', 'Boolean', iptRW);
    RegisterProperty('Handle', 'THandle', iptR);
    RegisterProperty('HasChildren', 'Boolean', iptRW);
    RegisterProperty('Height', 'integer', iptRW);
    RegisterProperty('ImageIndex', 'Integer', iptRW);
    RegisterProperty('Index', 'Integer', iptRW);
    RegisterProperty('IsFullHeightVisible', 'Boolean', iptR);
    RegisterProperty('IsVisible', 'Boolean', iptR);
    RegisterProperty('Items', 'TTreeNode Integer', iptRW);
    RegisterProperty('Level', 'Integer', iptR);
    RegisterProperty('MultiSelected', 'Boolean', iptRW);
    RegisterProperty('NodeEffect', 'TGraphicsDrawEffect', iptRW);
    RegisterProperty('OverlayIndex', 'Integer', iptRW);
    RegisterProperty('Owner', 'TTreeNodes', iptR);
    RegisterProperty('Parent', 'TTreeNode', iptR);
    RegisterProperty('Selected', 'Boolean', iptRW);
    RegisterProperty('SelectedIndex', 'Integer', iptRW);
    RegisterProperty('StateIndex', 'Integer', iptRW);
    RegisterProperty('States', 'TNodeStates', iptR);
    RegisterProperty('SubTreeCount', 'integer', iptR);
    RegisterProperty('Text', 'string', iptRW);
    RegisterProperty('Top', 'integer', iptR);
    RegisterProperty('TreeNodes', 'TTreeNodes', iptR);
    RegisterProperty('TreeView', 'TTreeView', iptR);
    RegisterProperty('Visible', 'Boolean', iptRW);
  end;

  with Cl.FindClass('TTreeNodes') do
  begin
    RegisterMethod('constructor Create(AnOwner: TCustomTreeView)');
    RegisterMethod('function Add(SiblingNode: TTreeNode; const S: string): TTreeNode');
    RegisterMethod('function AddChild(ParentNode: TTreeNode; const S: string): TTreeNode');
    RegisterMethod('function AddChildFirst(ParentNode: TTreeNode; const S: string): TTreeNode');
    RegisterMethod('function AddChildObject(ParentNode: TTreeNode; const S: string; Data: TObject): TTreeNode');
    RegisterMethod('function AddChildObjectFirst(ParentNode: TTreeNode; const S: string; Data: TObject): TTreeNode');
    RegisterMethod('function AddFirst(SiblingNode: TTreeNode; const S: string): TTreeNode');
    RegisterMethod('function AddObject(SiblingNode: TTreeNode; const S: string; Data: TObject): TTreeNode');
    RegisterMethod('function AddObjectFirst(SiblingNode: TTreeNode; const S: string; Data: TObject): TTreeNode');
    RegisterMethod('function FindNodeWithData(const NodeData: TObject): TTreeNode');
    RegisterMethod('function FindNodeWithText(const NodeText: string): TTreeNode');
    RegisterMethod('function FindNodeWithTextPath(TextPath: string): TTreeNode');
    RegisterMethod('function FindTopLvlNode(const NodeText: string): TTreeNode');
    RegisterMethod('function GetFirstNode: TTreeNode');
    RegisterMethod('function GetFirstVisibleNode: TTreeNode');
    RegisterMethod('function GetLastExpandedSubNode: TTreeNode');
    RegisterMethod('function GetLastNode: TTreeNode');
    RegisterMethod('function GetLastSubNode: TTreeNode');
    RegisterMethod('function GetLastVisibleNode: TTreeNode');
    RegisterMethod('function GetSelections(const AIndex: Integer): TTreeNode');
    RegisterMethod('function Insert(NextNode: TTreeNode; const S: string): TTreeNode');
    RegisterMethod('function InsertBehind(PrevNode: TTreeNode; const S: string): TTreeNode');
    RegisterMethod('function InsertObject(NextNode: TTreeNode; const S: string; Data: TObject): TTreeNode');
    RegisterMethod('function InsertObjectBehind(PrevNode: TTreeNode; const S: string; Data: TObject): TTreeNode');
    RegisterMethod('function IsMultiSelection: boolean');
    RegisterMethod('procedure BeginUpdate');
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure ClearMultiSelection(ClearSelected: boolean)');
    RegisterMethod('procedure Delete(Node: TTreeNode)');
    RegisterMethod('procedure EndUpdate');
    RegisterMethod('procedure SelectionsChanged(ANode: TTreeNode; const AIsSelected: Boolean)');
    RegisterMethod('procedure SelectOnlyThis(Node: TTreeNode)');
    RegisterMethod('procedure SortTopLevelNodes(SortProc: TTreeNodeCompare)');
    RegisterProperty('Count', 'Integer', iptR);
    RegisterProperty('Item', 'TTreeNode Integer', iptR);
    RegisterProperty('KeepCollapsedNodes', 'boolean', iptRW);
    RegisterProperty('Owner', 'TTreeView', iptR);
    RegisterProperty('SelectionCount', 'Cardinal', iptR);
    RegisterProperty('TopLvlCount', 'integer', iptR);
    RegisterProperty('TopLvlItems', 'TTreeNode Integer', iptRW);
  end;

  Cl.AddTypeS('TCustomDrawStage', '(cdPrePaint, cdPostPaint, cdPreErase, cdPostErase)');
  Cl.AddTypeS('TCustomDrawStateFlag', '(cdsSelected, cdsGrayed, cdsDisabled, cdsChecked, cdsFocused, cdsDefault, cdsHot, cdsMarked, cdsIndeterminate)');
  Cl.AddTypeS('TCustomDrawState', 'set of TCustomDrawStateFlag');
  Cl.AddTypeS('TTreeViewExpandSignType', '(tvestTheme, tvestPlusMinus, tvestArrow)');
  Cl.AddTypeS('TMultiSelectStyles', '(msControlSelect, msShiftSelect, msVisibleOnly, msSiblingOnly)');
  Cl.AddTypeS('TMultiSelectStyle', 'set of TMultiSelectStyles');
  Cl.AddTypeS('TSortType', '(stNone, stData, stText, stBoth)');
  Cl.AddTypeS('TTreeNodeChangeReason', '(ncTextChanged, ncDataChanged, ncHeightChanged, ncImageEffect, ncImageIndex, ncParentChanged, ncVisibility, ncOverlayIndex, ncStateIndex, ncSelectedIndex)');

  Cl.AddTypeS('TTVExpandedEvent', 'procedure(Sender: TObject; Node: TTreeNode)');
  Cl.AddTypeS('TTVAdvancedCustomDrawEvent', 'procedure(Sender: TCustomTreeView; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean)');
  Cl.AddTypeS('TTVAdvancedCustomDrawItemEvent', 'procedure(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean)');
  Cl.AddTypeS('TTVChangedEvent', 'procedure(Sender: TObject; Node: TTreeNode)');
  Cl.AddTypeS('TTVChangingEvent', 'procedure(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean)');
  Cl.AddTypeS('TTVCollapsingEvent', 'procedure(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean)');
  Cl.AddTypeS('TTVCompareEvent', 'procedure(Sender: TObject; Node1, Node2: TTreeNode; var Compare: Integer)');
  Cl.AddTypeS('TTVCustomDrawEvent', 'procedure(Sender: TCustomTreeView; const ARect: TRect; var DefaultDraw: Boolean)');
  Cl.AddTypeS('TTVCustomDrawItemEvent', 'procedure(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean)');
  Cl.AddtypeS('TTVEditedEvent', 'procedure(Sender: TObject; Node: TTreeNode; var S: string)');
  Cl.AddTypeS('TTVEditingEvent', 'procedure(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean)');
  Cl.AddTypeS('TTVEditingEndEvent', 'procedure(Sender: TObject; Node: TTreeNode; Cancel: Boolean)');
  Cl.AddTypeS('TTVExpandingEvent', 'procedure(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean)');
  Cl.AddTypeS('TTVNodeChangedEvent', 'procedure(Sender: TObject; Node: TTreeNode; ChangeReason: TTreeNodeChangeReason)');
  Cl.AddTypeS('TTreeViewOption', '(tvoAllowMultiselect, tvoAutoExpand, tvoAutoInsertMark, tvoAutoItemHeight, tvoHideSelection, tvoHotTrack, tvoKeepCollapsedNodes, tvoReadOnly, tvoRightClickSelect, tvoRowSelect, tvoShowButtons, tvoShowLines, tvoShowRoot, tvoShowSeparators, tvoToolTips, tvoNoDoubleClickExpand, tvoThemedDraw)');
  Cl.AddTypeS('TTreeViewOptions', 'set of TTreeViewOption');

  Cl.AddTypeS('THitTest', '(htAbove, htBelow, htNowhere, htOnItem, htOnButton, htOnIcon, htOnIndent, htOnLabel, htOnRight, htOnStateIcon, htToLeft, htToRight)');
  Cl.AddtypeS('THitTests', 'set of THitTest');
  Cl.AddTypeS('TTreeViewInsertMarkType', '(tvimNone, tvimAsFirstChild, tvimAsNextSibling, tvimAsPrevSibling)');

  with Cl.FindClass('TTreeView') do
  begin
    RegisterMethod('function AlphaSort: Boolean');
    RegisterMethod('procedure ClearSelection(KeepPrimary: Boolean); virtual');
    RegisterMethod('function CustomSort(SortProc: TTreeNodeCompare): Boolean');
    RegisterMethod('function DefaultTreeViewSort(Node1, Node2: TTreeNode): Integer');
    RegisterMethod('function GetHitTestInfoAt(X, Y: Integer): THitTests');
    RegisterMethod('function GetNodeAt(X, Y: Integer): TTreeNode');
    RegisterMethod('procedure GetInsertMarkAt(X, Y: Integer; out AnInsertMarkNode: TTreeNode; out AnInsertMarkType: TTreeViewInsertMarkType)');
    RegisterMethod('procedure SetInsertMark(AnInsertMarkNode: TTreeNode; AnInsertMarkType: TTreeViewInsertMarkType)');
    RegisterMethod('procedure SetInsertMarkAt(X,Y: integer); virtual');
    RegisterMethod('function IsEditing: Boolean');
    RegisterMethod('procedure BeginUpdate');
    RegisterMethod('procedure EndUpdate');
    RegisterMethod('procedure FullCollapse');
    RegisterMethod('procedure FullExpand');
    RegisterMethod('procedure LoadFromFile(const FileName: string)');
    RegisterMethod('procedure LoadFromStream(Stream: TStream)');
    RegisterMethod('procedure SaveToFile(const FileName: string)');
    RegisterMethod('procedure SaveToStream(Stream: TStream)');
    RegisterMethod('procedure LockSelectionChangeEvent');
    RegisterMethod('procedure UnlockSelectionChangeEvent');
    RegisterMethod('function GetFirstMultiSelected: TTreeNode');
    RegisterMethod('function GetLastMultiSelected: TTreeNode');
    RegisterMethod('procedure Select(Node: TTreeNode; ShiftState: TShiftState)');
    RegisterMethod('function SelectionVisible: boolean');
    RegisterMethod('procedure MakeSelectionVisible');
    RegisterMethod('procedure ClearInvisibleSelection');
    RegisterMethod('function StoreCurrentSelection: TStringList');
    RegisterMethod('procedure ApplyStoredSelection(ASelection: TStringList; FreeList: boolean)');
    RegisterMethod('procedure MoveToNextNode');
    RegisterMethod('procedure MoveToPrevNode');
    RegisterProperty('AutoExpand', 'Boolean', iptRW);
    RegisterProperty('BackgroundColor', 'TColor', iptRW);
    RegisterProperty('BorderWidth', 'Integer', iptRW);
    RegisterProperty('DefaultItemHeight', 'Integer', iptRW);
    RegisterProperty('ExpandSignColor', 'TColor', iptRW);
    RegisterProperty('ExpandSignType', 'TTreeViewExpandSignType', iptRW);
    RegisterProperty('HideSelection', 'Boolean', iptRW);
    RegisterProperty('HotTrack', 'Boolean', iptRW);
    RegisterProperty('Images', 'TCustomImageList', iptRW);
    RegisterProperty('Indent', 'Integer', iptRW);
    RegisterProperty('MultiSelect', 'Boolean', iptRW);
    RegisterProperty('MultiSelectStyle', 'TMultiSelectStyle', iptRW);
    RegisterProperty('ParentColor', 'Boolean', iptRW);
    RegisterProperty('ParentFont', 'Boolean', iptRW);
    RegisterProperty('ReadOnly', 'Boolean', iptRW);
    RegisterProperty('RightClickSelect', 'Boolean', iptRW);
    RegisterProperty('RowSelect', 'Boolean', iptRW);
    RegisterProperty('ScrollBars', 'TScrollStyle', iptRW);
    RegisterProperty('SelectionColor', 'TColor', iptRW);
    RegisterProperty('ShowButtons', 'Boolean', iptRW);
    RegisterProperty('ShowLines', 'Boolean', iptRW);
    RegisterProperty('ShowRoot', 'Boolean', iptRW);
    RegisterProperty('SortType', 'TSortType', iptRW);
    RegisterProperty('StateImages', 'TCustomImageList', iptRW);
    RegisterProperty('OnAddition', 'TTVExpandedEvent', iptRW);
    RegisterProperty('OnAdvancedCustomDraw', 'TTVAdvancedCustomDrawEvent', iptRW);
    RegisterProperty('OnAdvancedCustomDrawItem', 'TTVAdvancedCustomDrawItemEvent', iptRW);
    RegisterProperty('OnChange', 'TTVChangedEvent', iptRW);
    RegisterProperty('OnChanging', 'TTVChangingEvent', iptRW);
    RegisterProperty('OnCollapsed', 'TTVExpandedEvent', iptRW);
    RegisterProperty('OnCollapsing', 'TTVCollapsingEvent', iptRW);
    RegisterProperty('OnCompare', 'TTVCompareEvent', iptRW);
    RegisterProperty('OnCustomDraw', 'TTVCustomDrawEvent', iptRW);
    RegisterProperty('OnCustomDrawItem', 'TTVCustomDrawItemEvent', iptRW);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptRW);
    RegisterProperty('OnDeletion', 'TTVExpandedEvent', iptRW);
    RegisterProperty('OnEdited', 'TTVEditedEvent', iptRW);
    RegisterProperty('OnEditing', 'TTVEditingEvent', iptRW);
    RegisterProperty('OnEditingEnd', 'TTVEditingEndEvent', iptRW);
    RegisterProperty('OnExpanded', 'TTVExpandedEvent', iptRW);
    RegisterProperty('OnExpanding', 'TTVExpandingEvent', iptRW);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptRW);
    RegisterProperty('OnMouseEnter', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseLeave', 'TNotifyEvent', iptRW);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptRW);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptRW);
    RegisterProperty('OnNodeChanged', 'TTVNodeChangedEvent', iptRW);
    RegisterProperty('OnSelectionChanged', 'TNotifyEvent', iptRW);
    RegisterProperty('Options', 'TTreeViewOptions', iptRW);
    RegisterProperty('Items', 'TTreeNodes', iptRW);
    RegisterProperty('TreeLineColor', 'TColor', iptRW);
    RegisterProperty('TreeLinePenStyle', 'TPenStyle', iptRW);
  end;
end;

procedure SIRegister_Consts(Cl: TPSPascalCompiler);
begin
  Cl.AddConstantN('LineEnding', 'String').SetString(LineEnding);
end;

procedure SIRegister_Functions(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TReplaceFlag', '(rfReplaceAll, rfIgnoreCase)');
  Cl.AddTypeS('TReplaceFlags', 'set of TReplaceFlag');
  Cl.AddDelphiFunction('procedure MsgBox(const Title, Msg: String);');
  Cl.AddDelphiFunction('procedure Debug(Value: Variant);');

  Cl.AddDelphiFunction('function UTF8Length(const s: string): LongInt;');
  Cl.AddDelphiFunction('function UTF8Pos(const SearchForText, SearchInText: string; StartPos: LongInt): LongInt;');
  Cl.AddDelphiFunction('function UTF8Copy(const s: string; StartCharIndex, CharCount: LongInt): string');
  Cl.AddDelphiFunction('procedure UTF8Delete(var s: String; StartCharIndex, CharCount: LongInt)');
  Cl.AddDelphiFunction('procedure UTF8Insert(const source: String; var s: string; StartCharIndex: LongInt);');
  Cl.AddDelphiFunction('function UTF8StringReplace(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;');
  Cl.AddDelphiFunction('function UTF8LowerCase(const AInStr: string): string;');
  Cl.AddDelphiFunction('function UTF8UpperCase(const AInStr: string): string;');
  Cl.AddDelphiFunction('function UTF8CompareStr(const S1, S2: string): LongInt;');
  Cl.AddDelphiFunction('function UTF8CompareText(const S1, S2: string): LongInt');
  Cl.AddDelphiFunction('function WinCPToUtf8(const s: String): String');
  Cl.AddDelphiFunction('function Utf8ToWinCP(const s: String) : String');
  Cl.AddDelphiFunction('function UTF8ToUTF16(const S: AnsiString): UnicodeString');
  Cl.AddDelphiFunction('function UTF16ToUTF8(const S: UnicodeString): AnsiString');

  Cl.AddTypeS('TCopyFileFlag', '(cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime)');
  Cl.AddTypeS('TCopyFileFlags', 'set of TCopyFileFlag');
  Cl.AddDelphiFunction('function FileExists(const Filename: string): boolean;');
  Cl.AddDelphiFunction('function FileAge(const FileName: string): Longint;');
  Cl.AddDelphiFunction('function DirectoryExists(const Directory: string): Boolean;');
  Cl.AddDelphiFunction('function ExpandFileName(const FileName, BaseDir: string): string;');
  Cl.AddDelphiFunction('function FileSetDate(const FileName: String; Age: Longint): Longint;');
  Cl.AddDelphiFunction('function FileGetAttr(const FileName: String): Longint;');
  Cl.AddDelphiFunction('function FileSetAttr(const Filename: String; Attr: longint): Longint;');
  Cl.AddDelphiFunction('function DeleteFile(const FileName: String): Boolean;');
  Cl.AddDelphiFunction('function RenameFile(const OldName, NewName: String): Boolean;');
  Cl.AddDelphiFunction('function GetCurrentDir: String;');
  Cl.AddDelphiFunction('function CreateDir(const NewDir: String): Boolean;');
  Cl.AddDelphiFunction('function RemoveDir(const Dir: String): Boolean;');
  Cl.AddDelphiFunction('function ForceDirectories(const Dir: string): Boolean;');
  Cl.AddDelphiFunction('function CopyFile(const SrcFilename, DestFilename: string; Flags: TCopyFileFlags): boolean;');
  Cl.AddDelphiFunction('procedure FindAllFiles(AList: TStrings; const SearchPath: String; SearchMask: String; SearchSubDirs: Boolean; DirAttr: Word);');
  Cl.AddDelphiFunction('procedure FindAllDirectories(AList: TStrings; const SearchPath: String; SearchSubDirs: Boolean);');
  Cl.AddDelphiFunction('function ExtractFileName(const FileName: string): string;');
  Cl.AddDelphiFunction('function ExtractFileNameOnly(const AFilename: string): string;');
  Cl.AddDelphiFunction('function ExtractFileExt(const FileName: string): string;');
  Cl.AddDelphiFunction('function ExtractFilePath(const FileName: string): string;');
  Cl.AddDelphiFunction('function ExtractFileDrive(const FileName: string): string;');
  Cl.AddDelphiFunction('function ExtractFileDir(Const FileName : string): string;');
  Cl.AddDelphiFunction('function ChangeFileExt(const FileName, Extension: string): string;');
  Cl.AddDelphiFunction('function IncludeTrailingPathDelimiter(Const Path : String) : String;');
  Cl.AddDelphiFunction('function ExcludeLeadingPathDelimiter(Const Path: string): string;');
  Cl.AddDelphiFunction('function GetTempFileName: String');
  Cl.AddDelphiFunction('function GetTempDir: String');
  Cl.AddDelphiFunction('function ShellExecute(const Operation, FileName, Params, WorkDir: String; ShowCmd: LongInt): Boolean');
  Cl.AddDelphiFunction('Function DateTimeToFileDate(DateTime : TDateTime) : Longint');
	Cl.AddDelphiFunction('Function FileDateToDateTime (Filedate : Longint) :TDateTime');

  Cl.AddDelphiFunction('function CreateForm(const FormName: String): TdxForm');
  Cl.AddDelphiFunction('procedure DestroyForm(var Fm: TdxForm)');
  Cl.AddDelphiFunction('procedure GetForms(SL: TStrings);');

  Cl.AddDelphiFunction('procedure FreeAndNil(var Obj: TObject);');
  Cl.AddDelphiFunction('function Random(n: LongInt): LongInt');

  Cl.AddDelphiFunction('function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): LongInt');
  Cl.AddDelphiFunction('function InputBox(const ACaption, APrompt, ADefault : String) : String');
  Cl.AddDelphiFunction('function EvalExpr(const Expr: String; Fm: TdxForm): Variant');

  Cl.AddDelphiFunction('function DCount(DataSet: TObject): Integer');
  Cl.AddDelphiFunction('function DSum(DataSet: TObject; const FieldName: String): Extended');
  Cl.AddDelphiFunction('function DAvg(DataSet: TObject; const FieldName: String): Extended');
  Cl.AddDelphiFunction('function DMax(DataSet: TObject; const FieldName: String): Variant');
  Cl.AddDelphiFunction('function DMin(DataSet: TObject; const FieldName: String): Variant');
  Cl.AddDelphiFunction('function DMerge(DataSet: TObject; const FieldName, Delimiter: String): String');
  Cl.AddDelphiFunction('function ToWords(Money: Currency): String');
  Cl.AddDelphiFunction('function RurToWords(Money: Currency): String');
  Cl.AddDelphiFunction('function Nz(V1, V2: Variant): Variant');
  {Cl.AddDelphiFunction('function DBCount(const FormName, Filter: String): Integer');
  Cl.AddDelphiFunction('function DBCountD(const FormName, FieldName, Filter: String): Integer');
  Cl.AddDelphiFunction('function DBSum(const FormName, FieldName, Filter: String): Extended');
  Cl.AddDelphiFunction('function DBAvg(const FormName, FieldName, Filter: String): Extended');
  Cl.AddDelphiFunction('function DBMax(const FormName, FieldName, Filter: String): Variant');
  Cl.AddDelphiFunction('function DBMin(const FormName, FieldName, Filter: String): Variant');
  Cl.AddDelphiFunction('function DBMerge(const FormName, FieldName, Filter: String): String');
  Cl.AddDelphiFunction('function DBMergeAll(const FormName, FieldName, Filter: String): String');
  Cl.AddDelphiFunction('function DBGet(const FormName, FieldName, Filter: String): Variant');
  Cl.AddDelphiFunction('function DBGetId(const FormName, Filter: String): Variant');
  Cl.AddDelphiFunction('function DBGetById(const FormName, FieldName: String; Id: Integer): Variant');}
  Cl.AddDelphiFunction('function RoundTo(AValue: Extended; Digits: Integer): Extended');
  Cl.AddDelphiFunction('function Frac(E: Extended): Extended');
  Cl.AddDelphiFunction('function Power(base, exponent: Extended): Extended');
  Cl.AddDelphiFunction('Function YearsBetween(const ANow, AThen: TDateTime): Integer');
  Cl.AddDelphiFunction('Function MonthsBetween(const ANow, AThen: TDateTime): Integer');
  Cl.AddDelphiFunction('Function WeeksBetween(const ANow, AThen: TDateTime): Integer');
  Cl.AddDelphiFunction('Function DaysBetween(const ANow, AThen: TDateTime): Integer');
  Cl.AddDelphiFunction('Function HoursBetween(const ANow, AThen: TDateTime): Int64');
  Cl.AddDelphiFunction('Function MinutesBetween(const ANow, AThen: TDateTime): Int64');
  Cl.AddDelphiFunction('Function SecondsBetween(const ANow, AThen: TDateTime): Int64');
  Cl.AddDelphiFunction('Function AddYear(const AValue: TDateTime; const ANumberOfYears: Integer ): TDateTime');
  Cl.AddDelphiFunction('function AddMonth(const DateTime: TDateTime; NumberOfMonths: integer ): TDateTime');
  Cl.AddDelphiFunction('Function AddWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer): TDateTime');
  Cl.AddDelphiFunction('Function AddDay(const AValue: TDateTime; const ANumberOfDays: Integer): TDateTime');
  Cl.AddDelphiFunction('Function AddHour(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime');
  Cl.AddDelphiFunction('Function AddMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime');
  Cl.AddDelphiFunction('Function AddSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime');
  Cl.AddDelphiFunction('Function YearOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function MonthOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function WeekOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function DayOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function HourOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function MinuteOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function SecondOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('function StrToTime(const S: String): TDateTime');
  Cl.AddDelphiFunction('function TimeToStr(Time: TDateTime): string');
  Cl.AddDelphiFunction('function TryStrToDate(const S: string; out Value: TDateTime): Boolean');
  Cl.AddDelphiFunction('function TryStrToTime(const S: string; out Value: TDateTime): Boolean');
  Cl.AddDelphiFunction('function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean');
  Cl.AddDelphiFunction('function TryStrToInt(const s: string; Out i : integer) : boolean');
  Cl.AddDelphiFunction('Function TryStrToFloat(Const S : String; Out Value: Extended): Boolean');
  Cl.AddDelphiFunction('function StrToDateTime(const S: String): TDateTime');

  Cl.AddDelphiFunction('function GetWeekName(D: TDateTime; Brief: Boolean): String');
  Cl.AddDelphiFunction('function GetMonthName(D: TDateTime; Brief: Boolean): String');
  Cl.AddDelphiFunction('Function DayOfTheWeek(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('function FmtDate(DT: TDateTime): String');
  Cl.AddDelphiFunction('function FillZeros(E: Extended; N: Integer): String');
  Cl.AddDelphiFunction('function CalcPeriod(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String');
  Cl.AddDelphiFunction('function BeginYear(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function BeginMonth(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function BeginWeek(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function EndYear(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function EndMonth(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function EndWeek(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function BeginQuarter(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function EndQuarter(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function QuarterOf(D: TDateTime): Word');
  //Cl.AddDelphiFunction('function DBUnique(Fm: TdxForm; const Fields: String): Boolean');
  Cl.AddDelphiFunction('function GetCurrentUser: String');
  Cl.AddDelphiFunction('function GetCurrentRole: String');
  Cl.AddDelphiFunction('function GetCurrentDatabase: String');
  Cl.AddDelphiFunction('function GetTemplatesDir: String');
  Cl.AddDelphiFunction('function GetOutputDir: String');
  Cl.AddDelphiFunction('function Format(const Fmt: String; Args: array of const): String');
  Cl.AddDelphiFunction('function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;');
  Cl.AddDelphiFunction('procedure SplitStr(const S: String; Delim: Char; SL: TStrings)');

  Cl.AddDelphiFunction('function Clipboard: TClipboard');

  Cl.AddDelphiFunction('function SQLSelect(const SQL: String): TdxSQLQuery');

  Cl.AddDelphiFunction('function ColorToString(Color: TColor): String');
	Cl.AddDelphiFunction('function StringToColor(const S: string): TColor');
  Cl.AddDelphiFunction('function RGBToColor(R, G, B: Byte): TColor');
  Cl.AddDelphiFunction('function ColorToRGB(Color: TColor): Longint');
	Cl.AddDelphiFunction('procedure RedGreenBlue(rgb: Cardinal; out Red, Green, Blue: Byte)');

  Cl.AddDelphiFunction('function FormatFloat(Const Format : String; Value : Extended) : String');

  Cl.AddDelphiFunction('function EncodeMD5(const S: String): String');
  Cl.AddDelphiFunction('function EncodeSHA1(const S: String): String');
  Cl.AddDelphiFunction('function EncodeBase64(const S: String): String');
  Cl.AddDelphiFunction('function DecodeBase64(const S: String; Strict: Boolean): String');
  Cl.AddDelphiFunction('function HMACMD5(const AKey, AMessage: string): string');
	Cl.AddDelphiFunction('function HMACSHA1(const AKey, AMessage: string): string');

  Cl.AddDelphiFunction('function VarToStr(const V: Variant): String');
  Cl.AddDelphiFunction('procedure ShowPrintErrors(const S: String)');

  Cl.AddDelphiFunction('function SameValue(const A, B: Extended; Epsilon: Extended): Boolean');
end;

end.

