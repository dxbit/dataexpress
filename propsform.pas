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

-------------------------------------------------------------------------------}

unit PropsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, Buttons, Controls, Variants, types,
  ExtCtrls, Dialogs, DBCtrls, strconsts, StdCtrls;

type

  TIntArray = array of Integer;

  { TPropsForm }

  TPropsForm = class(TPanel)
  private
    FControl: TControl;
    procedure ButtonClick(Sender: TObject);
    procedure SetControl(AValue: TControl);
    function CreateButton(aKind: Integer): TSpeedButton;
    procedure DeleteControls;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Control: TControl read FControl write SetControl;
  end;

  procedure ShowPropsForm(aControl: TControl);
  procedure HidePropsForm;
  procedure UpdatePropsForm;
  function IsPropsFormVisible: Boolean;

implementation

uses
  dxctrls, designerframe, formdesigner, listsourceform, stringsform, colorform,
  apputils, shapeform,
  storagetypeform, propdialogs, dximages, dxfiles, CheckPrintOutForm,
  Graphics, calcfieldsform, newgridform,
  DXReports, formfiltersform, HierarchyForm, treeform,
  ObjectFieldsForm, propsmenus, coloringform, counterform,
  labeltextform, rangeform, fixedlistform, shoppingform, hintform, HelpTextForm,
  gridbuttonsform, myctrls, pivotgrid, pivotgridform, scriptform,
  appsettings, EditMaskForm, actionseditform, formmanager, lcbxlistsourceform,
  precisionform, scriptmanager, imageprocessprintform, thumbform,
  templatefieldsform, dxcharts, chartform, printchartform, imagesform,
  memosourceform, imagepropsform;

const
  C_FIELDNAME = 1;
  C_COLOR = 2;
  C_FONT = 3;
  C_FORM = 4;
  C_LIST = 5;
  C_LISTSOURCE = 6;
  C_TEMPLATE = 7;
  C_FORMCAPTION = 8;
  C_PRECISSION = 9;
  C_DATENOW = 10;
  C_GRID = 11;
  C_GROUPCAPTION = 12;
  C_TABADD = 13;
  C_TABDEL = 14;
  C_TABLEFT = 15;
  C_TABRIGHT = 16;
  C_TABCAPTION = 17;
  C_LABELCAPTION = 18;
  C_SHAPE = 19;
  C_STORETYPE = 20;
  C_THUMBSIZE = 21;
  C_LOADIMAGE = 22;
  C_SAVEIMAGE = 23;
  C_CLEARIMAGE = 24;
  C_VIEWTYPE = 25;       // 21.05.15
  C_CHECKTEXT = 26;
  C_EXPR = 27;
  C_CALCFIELDS = 28;
  C_LISTFILTER = 29;
  C_COUNTER = 30;
  C_QUERYNAME = 31;
  C_QUERYSOURCE = 32;
  C_QUERYFILTER = 33;
  C_PICKLIST = 34;
  C_HELPTEXT = 35;
  C_QUERYGRID = 36;
  C_FORMFILTERS = 37;
  C_OBJFIELDS = 38;
  C_CURTIME = 39;
  C_TIMEFMT = 40;
  C_COLORING = 41;
  C_FIELDSIZE = 42;
  C_INSERTVALUES = 43;
  C_QUERYSTYLE = 44;
  C_CHECKVALUE = 45;
  C_READONLY = 46;
  C_LBLTEXT = 47;
  C_RANGE = 48;
  C_SHOP = 49;
  C_QCALCFIELDS = 50;
  C_DEFAULTVALUE = 51;
  C_HINT = 52;
  C_HIERARCHY = 53;
  C_TREE = 54;
  C_GRIDBUTTONS = 55;
  C_QUERYCOLORING = 56;
  C_ACTION = 57;
  C_BUTTONGLYPH = 58;
  //C_BUTTONNAME = 59;
  C_PIVOTGRID = 60;
  C_SELECTQUERY = 61;
  C_COMPONENTNAME = 62;
  //C_SCROLLBARS = 63;
  C_EDITMASK = 64;
  C_FORMMORE = 65;
  C_QGRIDMORE = 66;
  C_FORMACTIONS = 67;
  C_CALCEDITMORE = 68;
  C_LINKFORM = 69;
  C_IMGPROCESSPRINT = 70;
  C_CHART = 71;
  C_PRINTCHART = 72;
  C_IMAGES = 73;
  C_IMAGEPROPS = 74;

var
  PropsFrm: TPropsForm;
  OldControl: TControl;

procedure ShowPropsForm(aControl: TControl);
var
  P: TPoint;
  X, Y: Integer;
  SBox: TScrollBox;
begin
  if OldControl = aControl then
  begin
    HidePropsForm;
    Exit;
  end;

  if aControl is TdxForm then
    P := DesignFr.ScreenToClient(Mouse.CursorPos)
  else
    P := aControl.ClientToParent(Point(0, 0), DesignFr);
  X := P.X; Y := P.Y;

  SBox := DesignFr.ScrollBox1;
  HidePropsForm;
  PropsFrm := TPropsForm.Create(nil);
  PropsFrm.Parent := DesignFr;
  PropsFrm.Control := aControl;

  if (aControl is TdxPageControl) and (TdxPageControl(aControl).PageCount > 0) then
    Y := Y - 60
  else if aControl is TdxGroupBox then
    Y := Y - 50
  else
    Y := Y - 40;
  if Y < 0 then
    Y := 0;

  if X + PropsFrm.Width > SBox.Left + SBox.ClientWidth then
    X := SBox.Left + SBox.ClientWidth - PropsFrm.Width;
  if X < SBox.Left then X := SBox.Left;

  PropsFrm.Left := X;
  PropsFrm.Top := Y;
  PropsFrm.BringToFront;
  PropsFrm.Visible:=True;
  OldControl := aControl;
end;

procedure HidePropsForm;
begin
  FreeAndNil(PropsFrm);
  OldControl := nil;
end;

procedure UpdatePropsForm;
var
  C: TControl;
begin
  HidePropsForm;
  if (not FormDesign.Active) or (Length(FormDesign.Selected) <> 1) then Exit;
  C := FormDesign.Selection[0];
  ShowPropsForm(C);
end;

function IsPropsFormVisible: Boolean;
begin
  Result := PropsFrm <> nil;
end;

function GetControl: TControl;
begin
  Result := FormDesign.Control;
  if Result = nil then Result := FormDesign.Container;
end;

procedure DeleteComponent(c: TComponent);
begin
  if (c is TWinControl) then
  begin
    while TWinControl(c).ControlCount>0 do
      deleteComponent((c as TWinControl).controls[0]);
  end;

  c.Free;
end;

procedure AddTab;
var
  F: TdxForm;
  Pg: TdxPageControl;
  T: TdxTabSheet;
begin
  Pg := TdxPageControl(GetControl);
  F := FormDesign.Form;
  T := TdxTabSheet.Create(F);
  //T.Name := DesignUniqueName(F, T.ClassName);
  MakeUniqueName(F, T);
  T.PageControl := Pg;
  FormDesign.Messenger.DesignComponent(T, True);
  FormDesign.Change;
  DesignFr.CompTree.UpdateTree;
end;

procedure DeleteTab;
var
  Pg: TdxPageControl;
begin
  Pg := TdxPageControl(GetControl);
  if Pg.ActivePage <> nil then
  begin
    //if (not ConfirmDelete) or (not FormDesign.c then Exit;
    FormDesign.Select(Pg.ActivePage);
    FormDesign.UserDeleteComponents;
    FormDesign.Select(Pg);

    //DeleteComponent(Pg.ActivePage);
  end;
end;

procedure MoveLeftTab;
var
  Pg: TdxPageControl;
begin
  Pg := TdxPageControl(GetControl);
  if Pg.ActivePage <> nil then
    with Pg.ActivePage do
      if PageIndex > 0 then
        PageIndex:=PageIndex - 1;
  DesignFr.CompTree.UpdateTree;
end;

procedure MoveRightTab;
var
  Pg: TdxPageControl;
begin
  Pg := TdxPageControl(GetControl);
  if Pg.ActivePage <> nil then
    with Pg.ActivePage do
      if PageIndex < Pg.PageCount - 1 then
        PageIndex:=PageIndex + 1;
  DesignFr.CompTree.UpdateTree;
end;

{ TPropsForm }

procedure TPropsForm.ButtonClick(Sender: TObject);
var
  C: TControl;
  S: String;
  PropId: PtrInt;
  OldSize, mr: Integer;
begin
  C := GetControl;
  PropId := TToolButton(Sender).Tag;

  mr := mrNone;
  case PropId of
    C_FORM:
      DesignFr.FormsTreeView.SelectForm(FormMan.FindForm(GetId(C)));
    C_LISTSOURCE:
			ShowListSourceForm(C);
    C_LINKFORM:
    	ShowLCbxListSourceForm(TdxLookupComboBox(C));
    C_TEMPLATE:
      mr := ShowStringsForm(rsTemplates, 'formtemplates', FormDesign.Form.Templates);
    C_FORMCAPTION:
    begin
      mr := FormNameDlg(FormDesign.Form);
      if mr = mrOk then
      begin
        DesignFr.FormsTreeView.RenameSelectedForm(FormDesign.Form.FormCaption);
        if ScriptFm <> nil then
          ScriptFm.RenameForm(FormDesign.Form.Id);
      end;
    end;
    C_FIELDNAME: mr := FieldNameDlg(C);
    C_COLOR:
      ShowColorForm(C);
    C_FONT: if FontDlg(C) = mrOk then FormDesign.UpdateDesigner;
    C_LIST:
      ShowFixedListForm(TdxComboBox(C));
    C_PRECISSION:
      ShowPrecForm(TdxCalcEdit(C));
    C_DATENOW:
      DateEditMnu.PopupMenu(TdxDateEdit(C));
    C_GRID:
      ShowGridForm(FormDesign.Form);
    C_GROUPCAPTION:
      begin
        mr := ControlCaptionDlg(TWinControl(C));
      	if mr = mrOk then C.Refresh;
      end;
    C_TABADD: AddTab;
    C_TABDEL: DeleteTab;
    C_TABLEFT: MoveLeftTab;
    C_TABRIGHT: MoveRightTab;
    C_TABCAPTION:
      mr := ControlCaptionDlg(TWinControl(C));
    C_LABELCAPTION:
      begin
        mr := ControlCaptionDlg(TWinControl(C));
        if mr = mrOk then
          FormDesign.UpdateDesigner;
      end;
    C_SHAPE: ShowShapeForm(TdxShape(C));
    C_STORETYPE: ShowStorageTypeForm(C);
    C_THUMBSIZE: ShowThumbForm(TdxDBImage(C));  //ThumbSizeDlg(TdxDBImage(C));
    C_LOADIMAGE:
      begin
        S := OpenPictureDialog;
        if S <> '' then
          try
            SetImageName(C, '');
            TdxImage(C).LoadFromFile(S);
            C.Repaint;
          except
            on E: Exception do
              ErrMsg(rsFailedToLoadImage + Spaces + E.Message);
          end;
      end;
    C_SAVEIMAGE:
      begin
        S := SavePictureDialog('');
        if S <> '' then
          try
            TdxImage(C).SaveToFile(S);
            C.Repaint;
          except
            on E: Exception do
              ErrMsg(rsFailedToSaveImage + Spaces + E.Message);
          end;
      end;
    C_CLEARIMAGE:
      begin
        TdxImage(C).Clear;
        C.Repaint;
      end;
    C_CHECKTEXT: ShowCheckPrintOutForm(TdxCheckBox(C));
    C_CALCFIELDS:
      if ShowCalcFieldsForm(TdxForm(GetControl)) = mrOk then
        UpdateTemplateFieldsForm;
    C_VIEWTYPE: FormMnu.PopupMenu(FormDesign.Form);
    C_EXPR:
      begin
        if C is TdxLabel then mr := ShowExprDlg(C)
        else ExprMnu.PopupMenu(C);
      end;
    C_LISTFILTER: LookupFilterDlg(C);
    C_COUNTER: ShowCounterForm(TdxCounter(C));
    C_QUERYNAME: mr := QueryNameDlg(C);
    C_QUERYSOURCE: mr := QuerySourceDlg(C);
    C_QUERYGRID: QueryGridDlg(TdxQueryGrid(C));
    C_QUERYFILTER: QueryFilterDlg(C);
    C_FORMFILTERS: ShowFormFiltersForm(TdxForm(C));
    C_OBJFIELDS: ShowObjFieldsForm(TdxObjectField(C));
    C_CURTIME: TimeEditMnu.PopupMenu(TdxTimeEdit(C));
    C_TIMEFMT: TimeEditMnu.PopupMenu2(TdxTimeEdit(C));
    C_COLORING: ShowColoringForm(FormDesign.Form);
    C_FIELDSIZE:
      begin
        OldSize := GetFieldSize(C);
        if FieldSizeDlg(C) then
        begin
          Cache.SetFieldSize(C, OldSize);
          // Будет считать размер записей с учетом объектов и полей объектов.
          if FormDesign.Form.PId = 0 then DesignFr.NeedAllCalcRecordSize:=True;
          //FormDesign.StructChanged:=True;
        end;
      end;
    C_INSERTVALUES: InsValMnu.PopupMenu(TdxLookupComboBox(C));
    C_CHECKVALUE:
      if C is TdxCheckBox then ShowCheckExprDlg(C)
      else RequiredMnu.PopupMenu(C);
    C_READONLY: CounterMnu.PopupMenu(TdxCounter(C));
    C_LBLTEXT:
      begin
        mr := ShowLabelTextForm(TdxLabel(C));
        if mr = mrOk then FormDesign.UpdateDesigner;
      end;
    C_RANGE: ShowRangeForm(TdxCalcEdit(C));
    C_SHOP: ShowShoppingForm(FormDesign.Form);
    //C_DELIM: DelimDlg(TdxMemo(C));
    C_QUERYSTYLE: QueryStyleDlg(TdxQueryGrid(C));
    C_QCALCFIELDS: QueryCalcDlg(C);
    C_DEFAULTVALUE: DefaultValueDlg(C);
    C_HINT: ShowHintForm(TdxLabel(C));
    C_HELPTEXT: FormDesign.Form.HelpText.Text := ShowHelpTextForm(FormDesign.Form.HelpText.Text);
    C_HIERARCHY: ShowHierarchyForm(FormDesign.Form);
    C_TREE: ShowTreeForm(FormDesign.Form);
    C_GRIDBUTTONS: ShowGridButtonsForm(TMyDBGrid(C));
    C_QUERYCOLORING: QueryColoringDlg(C);
    C_ACTION: TdxButton(C).ActionOnClick := ShowActionsEditForm(rsButtonActions,
      TdxButton(C).ActionOnClick, FormDesign.Form, [atAll, atButton], mr);
    C_BUTTONGLYPH: BtnGlyphMnu.PopupMenu(TdxButton(C));
    C_PIVOTGRID: ShowPivotGridForm(TdxPivotGrid(C));
    C_SELECTQUERY: SelectQueryDlg(TdxPivotGrid(C));
    C_COMPONENTNAME: mr := ComponentNameDlg(C);
    C_EDITMASK: ShowEditMaskForm(TdxEdit(C));
    C_FORMMORE: FormMoreMnu.PopupMenu(FormDesign.Form);
    C_QGRIDMORE: QGridMoreMenu.PopupMenu(TdxQueryGrid(C));
    C_FORMACTIONS:
      with FormDesign do
	    	Form.ActionOnCreate := ShowActionsEditForm(rsFormActions,
          Form.ActionOnCreate, Form, [atAll, atForm], mr);
    C_CALCEDITMORE: CalcEditMoreMnu.PopupMenu(TdxCalcEdit(C));
    C_IMGPROCESSPRINT: ShowImgProcessPrintForm(TdxDBImage(C));
    C_CHART: ShowChartForm(TdxChart(C));
    C_PRINTCHART: ShowPrintChartForm(TdxChart(C));
    C_IMAGES:
      if ShowImagesForm(True, TdxImage(C).ImageName, False) = mrOk then
        TdxImage(C).ImageName := ImagesFm.SelectedImageName;
    C_PICKLIST:
      ShowMemoSourceForm(TdxMemo(C));
    C_IMAGEPROPS:
      ShowImagePropsForm(TdxImage(C));
  end;
  DesignFr.UpdateStatusBar;

  if (PropId in [C_FORMCAPTION, C_FIELDNAME, C_COMPONENTNAME, C_LABELCAPTION,
    C_LBLTEXT, C_GROUPCAPTION, C_QUERYNAME, C_TABCAPTION]) and (mr = mrOk) then
    DesignFr.CompTree.RenameComponent(C);
  if (PropId in [C_FIELDNAME, C_LABELCAPTION, C_LBLTEXT]) and (mr = mrOk) then
    DesignFr.SummaryTree.RenameComponent(C);
  if (PropId in [C_FORMCAPTION, C_FIELDNAME, C_LBLTEXT, C_LABELCAPTION, C_QUERYNAME,
    C_QUERYSOURCE, C_EXPR, C_TEMPLATE, C_ACTION, C_FORMACTIONS]) and (mr = mrOk) then
    UpdateTemplateFieldsForm
end;

procedure SetArray(var Arr: TIntArray; Values: array of Integer);
var
  i: Integer;
begin
  SetLength(Arr, Length(Values));
  for i := 0 to High(Values) do
    Arr[i] := Values[i];
end;

procedure TPropsForm.SetControl(AValue: TControl);
var
  X, Y: Integer;
  Props: array of Integer;
  B: TSpeedButton;
  i: Integer;
begin
  if FControl=AValue then Exit;
  FControl:=AValue;

  if AValue is TdxForm then
  begin
    if TdxForm(AValue).PId = 0 then
      SetArray(Props, [C_FORMCAPTION, C_COLOR, C_FONT, C_GRID,
        C_VIEWTYPE, C_TEMPLATE, C_CALCFIELDS, C_FORMFILTERS, C_COLORING, C_HELPTEXT, C_HIERARCHY, C_TREE, C_FORMACTIONS, C_FORMMORE])
    else
      SetArray(Props, [C_FORMCAPTION, C_COLOR, C_FONT, C_GRID, C_CALCFIELDS, C_COLORING, C_SHOP, C_HELPTEXT, C_FORMACTIONS, C_FORMMORE])
  end
  else if AValue is TdxLabel then
    SetArray(Props, [C_COMPONENTNAME, C_LABELCAPTION, C_LBLTEXT, C_COLOR, C_FONT, C_EXPR, C_HINT])
  else if AValue is TdxEdit then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_FIELDSIZE, C_EDITMASK, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE])
  else if AValue is TdxCalcEdit then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_PRECISSION, C_EXPR, C_CHECKVALUE, C_RANGE, C_DEFAULTVALUE, C_CALCEDITMORE])
  else if AValue is TdxDateEdit then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_DATENOW, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE])
  else if AValue is TdxMemo then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_FIELDSIZE, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE, C_PICKLIST, C_LISTFILTER])
  else if AValue is TdxCheckBox then
    SetArray(Props, [C_FIELDNAME, C_LABELCAPTION, C_FONT, C_CHECKTEXT, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE])
  else if AValue is TdxComboBox then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_FIELDSIZE, C_LIST, C_LISTSOURCE, C_LISTFILTER, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE])
  else if AValue is TdxLookupComboBox then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_LINKFORM, C_LISTFILTER, C_INSERTVALUES, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE])
  else if AValue is TdxGrid then
    SetArray(Props, [C_COMPONENTNAME, C_FORM, C_GRIDBUTTONS])
  else if AValue is TdxGroupBox then
    SetArray(Props, [C_COMPONENTNAME, C_GROUPCAPTION, C_FONT])
  else if AValue is TdxPageControl then
    SetArray(Props, [C_COMPONENTNAME, C_FONT, C_TABADD, C_TABDEL, C_TABLEFT, C_TABRIGHT])
  else if AValue is TdxTabSheet then
    SetArray(Props, [C_COMPONENTNAME, C_TABCAPTION, C_FONT])
  else if AValue is TdxShape then
    SetArray(Props, [C_COMPONENTNAME, C_SHAPE])
  else if AValue is TdxDBImage then
    SetArray(Props, [C_FIELDNAME, C_STORETYPE, C_THUMBSIZE, C_IMGPROCESSPRINT, C_CHECKVALUE])
  else if AValue is TdxImage then
    SetArray(Props, [C_COMPONENTNAME, C_IMAGES, C_LOADIMAGE, C_SAVEIMAGE, C_CLEARIMAGE, C_IMAGEPROPS])
  else if AValue is TdxFile then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_FIELDSIZE, C_STORETYPE, C_CHECKVALUE])
  else if AValue is TdxQueryGrid then
    SetArray(Props, [C_QUERYNAME, C_QUERYSOURCE, C_QUERYGRID, C_QCALCFIELDS,
    C_QUERYFILTER, C_QUERYCOLORING, C_QUERYSTYLE, C_GRIDBUTTONS, C_QGRIDMORE])
  else if AValue is TdxObjectField then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_OBJFIELDS])
  else if AValue is TdxTimeEdit then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_CURTIME, C_TIMEFMT, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE])
  else if AValue is TdxCounter then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_COUNTER, C_CHECKVALUE, C_READONLY])
  else if AValue is TdxButton then
    SetArray(Props, [C_COMPONENTNAME, C_LABELCAPTION, C_FONT, C_BUTTONGLYPH, C_ACTION])
  else if AValue is TdxPivotGrid then
    SetArray(Props, [C_COMPONENTNAME, C_SELECTQUERY, C_QUERYSOURCE, C_QCALCFIELDS, C_QUERYFILTER,
      C_PIVOTGRID])
  else if AValue is TdxChart then
    SetArray(Props, [C_COMPONENTNAME, C_CHART, C_PRINTCHART])
  else if AValue is TdxRecordId then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT])
  ;
  DeleteControls;
  X := 0; Y := 0;
  for i := 0 to High(Props) do
  begin
    if (not AppConfig.ExpertMode) and (Props[I] = C_COMPONENTNAME) then Continue;
    B := CreateButton(Props[i]);
    B.Left := X; B.Top := Y;
    X := X + B.Width;
  end;
  ClientWidth := X;
  ClientHeight := ScaleToScreen(32);
end;

function TPropsForm.CreateButton(aKind: Integer): TSpeedButton;
const
  Glyphs: array [1..74] of String = ('id24', 'color24', 'font24', 'form24',
    'list24', 'listsource24', 'print24', 'id24', 'prec24', 'date24', 'grid24',
    'label24', 'tab_add24', 'tab_remove24', 'left24', 'right24', 'label24',
    'label24', 'shape24', 'box24', 'thumbsize24', 'open24', 'save24', 'delete24',
    'layout24', 'checkbox24', 'sum24', 'sum24', 'filter24', 'counter24',
    'id24', 'source24', 'filter24', 'listsource24', 'help24', 'grid24', 'filter24',
    'object24', 'clock24', 'view24', 'colors24', 'size24', 'equal', 'style24',
    'required24', 'lock24', 'multiline24', 'range24', 'shopping24', 'sum24',
    'default24', 'help24', 'parent24', 'tree24', 'gridbuttons24', 'colors24',
    'action24', 'image24', 'id24', 'pivottable24', 'query24', 'id24',
    'scrollbars24', 'editmask24', 'more24', 'more24', 'action24', 'more24',
    'object24', 'printimage24', 'chart24', 'print24', 'image24', 'more24');
var
  Hints: array [1..74] of String;
begin
  Hints[1] := rsFieldName;
  Hints[2] := rsColor;
  Hints[3] := rsFont;
  Hints[4] := rsForm;
  Hints[5] := rsList;
  Hints[6] := rsListSource;
  Hints[7] := rsTemplates;
  Hints[8] := rsFormName;
  Hints[9] := rsPrecission;
  Hints[10] := rsNow;
  Hints[11] := rsTable;
  Hints[12] := rsCaption;
  Hints[13] := rsAdd;
  Hints[14] := rsDelete;
  Hints[15] := rsMoveLeft;
  Hints[16] := rsMoveRight;
  Hints[17] := rsCaption;
  Hints[18] := rsCaption;
  Hints[19] := rsShapeType;
  Hints[20] := rsStorageType;
  Hints[21] := rsThumb;
  Hints[22] := rsLoadImage;
  Hints[23] := rsSaveImage;
  Hints[24] := rsClearImage;
  Hints[25] := rsViewType;
  Hints[26] := rsPrintOut;
  Hints[27] := rsExpression;
  Hints[28] := rsCalcFields;
  Hints[29] := rsListFilter;
  HInts[30] := rsStartWith;
  Hints[31] := rsName;
  Hints[32] := rsSelectionSet;
  Hints[33] := rsOutputFilter;
  Hints[34] := rsPickFromListWindow;
  Hints[35] := rsHelpText;
  Hints[36] := rsQueryGrid;
  Hints[37] := rsFilterPresets;
  Hints[38] := rsLinkedObject;
  Hints[39] := rsCurrentTime;
  Hints[40] := rsTimeFormat;
  Hints[41] := rsColoring;
  Hints[42] := rsFieldSize;
  Hints[43] := rsInsertValues;
  Hints[44] := rsCopyReportStyle;
  Hints[45] := rsCheckValue;
  Hints[46] := rsReadOnly;
  Hints[47] := rsLblText;
  Hints[48] := rsRange;
  Hints[49] := rsShopping;
  Hints[50] := rsCalcFields;
  Hints[51] := rsDefaultValue;
  Hints[52] := rsHint;
  Hints[53] := rsHierarchy;
  Hints[54] := rsTree;
  Hints[55] := rsButtons;
  Hints[56] := rsColoring;
  Hints[57] := rsButtonActions;
  Hints[58] := rsGlyph;
  //Hints[59] := rsButtonName;
  Hints[60] := rsPivotTable;
  Hints[61] := rsQuery;
  Hints[62] := rsComponentName;
  //Hints[63] := rsShowScrollbars;
  Hints[64] := rsEditMask;
  Hints[65] := rsMoreProps;
  Hints[66] := rsMoreProps;
  Hints[67] := rsFormActions;
  Hints[68] := rsMoreProps;
  Hints[69] := rsLinkToForm;
  Hints[70] := rsImgProcessPrint;
  Hints[71] := rsChart;
  Hints[72] := rsPrintChart;
  Hints[73] := rsSelectFromGallery;
  Hints[74] := rsImageProps;

  Result := TSpeedButton.Create(Self);
  Result.Width:=ScaleToScreen(32);
  Result.Height:=ScaleToScreen(32);
  Result.Parent := Self;
  Result.Tag := aKind;
  Result.ShowHint:=True;
  Result.Hint:=Hints[aKind];
  SetupSpeedButton(Result, Glyphs[aKind]);
  Result.OnClick:=@ButtonClick;
end;

procedure TPropsForm.DeleteControls;
var
  i: Integer;
begin
  for i := ControlCount - 1 downto 0 do
    Controls[i].Free;
end;

constructor TPropsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  BevelOuter := bvNone;
end;

destructor TPropsForm.Destroy;
begin
  DeleteControls;
  inherited Destroy;
end;

end.

