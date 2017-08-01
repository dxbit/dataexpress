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

  procedure ShowPropsForm(aParent: TWinControl; aControl: TControl);
  procedure HidePropsForm;
  procedure UpdatePropsForm;
  function IsPropsFormVisible: Boolean;

implementation

uses
  dxctrls, designerframe, formdesigner, listsourceform, stringsform, colorform,
  fontform, JvDesignUtils, apputils, shapeform,
  storagetypeform, propdialogs, dximages, dxfiles, CheckPrintOutForm,
  Graphics, calcfieldsform, newgridform,
  DXReports, formfiltersform, parentfieldform, treeform,
  ObjectFieldsForm, propsmenus, coloringform, counterform,
  labeltextform, rangeform, fixedlistform, shoppingform, hintform, HelpTextForm,
  gridbuttonsform, myctrls, actionform, pivotgrid, pivotgridform, scriptform,
  appsettings, EditMaskForm;

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
  C_DELIM = 34;
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
  C_PARENTFIELD = 53;
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

var
  PropsFrm: TPropsForm;
  OldControl: TControl;

procedure ShowPropsForm(aParent: TWinControl; aControl: TControl);
var
  P: TPoint;
  Offset: Integer;
begin
  if OldControl = aControl then
  begin
    HidePropsForm;
    Exit;
  end;

  if aControl is TdxForm then
    P := aParent.ScreenToClient(Mouse.CursorPos)
  else
    P := aControl.ClientToParent(Point(0, 0), aParent);
  Offset := TScrollBox(aParent).HorzScrollBar.Position;
  HidePropsForm;
  PropsFrm := TPropsForm.Create(nil);
  PropsFrm.Parent := aParent;
  PropsFrm.Control := aControl;
  if P.X + PropsFrm.Width > aParent.ClientWidth + Offset  then
    P.X := aParent.ClientWidth - PropsFrm.Width + Offset;
  with TScrollBox(aParent).HorzScrollBar do
    if P.X < Position then
      P.X := Position;
  PropsFrm.Left := P.X;
  if (aControl is TdxPageControl) and (TdxPageControl(aControl).PageCount > 0) then
    P.Y:= P.Y - 60
  else if aControl is TdxGroupBox then
    P.Y:=p.Y - 50
  else
    P.Y := P.Y - 40;
  if P.Y < -20 then
    P.Y := -20;
  PropsFrm.Top := P.Y;
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
begin
  ShowPropsForm(PropsFrm.Parent, PropsFrm.Control);
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
  T.Name := DesignUniqueName(F, T.ClassName);
  T.PageControl := Pg;
  FormDesign.Messenger.DesignComponent(T, True);
  FormDesign.Change;
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
end;

{ TPropsForm }

procedure TPropsForm.ButtonClick(Sender: TObject);
var
  C: TControl;
  S: String;
begin
  C := GetControl;
  case TToolButton(Sender).Tag of
    C_FORM:
      DesignFr.SelectForm(GetId(C));
    C_LISTSOURCE:
      ListSourceFrm.ShowForm(C);
    C_TEMPLATE:
      StringsFm.ShowForm(rsTemplates, 'formtemplates', FormDesign.Form.Templates);
    C_FORMCAPTION:
    begin
      if FormNameDlg(FormDesign.Form) then
      begin
        with DesignFr.FormList do
        begin
          S := FormDesign.Form.FormCaption;
          if FormDesign.Form.PId > 0 then S := '    ' + S;
          Items[ItemIndex] := S;
        end;
        if ScriptFm <> nil then
          ScriptFm.RenameForm(FormDesign.Form.Id);
      end;
    end;
    C_FIELDNAME: FieldNameDlg(C);
    C_COLOR:
      ColorFm.ShowForm(C);
    C_FONT:
      begin
        if FontFm.ShowForm(C.Font) = mrOk then
        begin
          if C <> FormDesign.Form then
            SetParentFont(C, FontFm.IsDefault or C.Font.IsEqual(C.Parent.Font));
          FormDesign.UpdateDesigner;
        end;
      end;
    C_LIST:
      FixedListFm.ShowForm(TdxComboBox(C));
    C_PRECISSION:
      PrecissionDlg(C);
    C_DATENOW:
      DateEditMnu.PopupMenu(TdxDateEdit(C));
      //DateNowFm.ShowForm(TdxDateEdit(C));
    C_GRID:
      GrdFm.ShowForm(FormDesign.Form);
    C_GROUPCAPTION:
      ControlCaptionDlg(TWinControl(C));
    C_TABADD: AddTab;
    C_TABDEL: DeleteTab;
    C_TABLEFT: MoveLeftTab;
    C_TABRIGHT: MoveRightTab;
    C_TABCAPTION:
      ControlCaptionDlg(TWinControl(C));
    C_LABELCAPTION:
      if ControlCaptionDlg(TWinControl(C)) then
        FormDesign.UpdateDesigner;
    C_SHAPE: ShapeFm.ShowForm(TdxShape(C));
    C_STORETYPE: StorageTypeFm.ShowForm(C);
    C_THUMBSIZE: ThumbSizeDlg(TdxDBImage(C));
    C_LOADIMAGE:
      begin
        S := OpenPictureDialog;
        if S <> '' then
        begin
          TdxImage(C).LoadFromFile(S);
          C.Repaint;
        end;
      end;
    C_SAVEIMAGE:
      begin
        S := SavePictureDialog('');
        if S <> '' then
        begin
          TdxImage(C).SaveToFile(S);
          C.Repaint;
        end;
      end;
    C_CLEARIMAGE:
      begin
        TdxImage(C).Clear;
        C.Repaint;
      end;
    C_CHECKTEXT: CheckPrintOutFm.ShowForm(TdxCheckBox(C));
    C_CALCFIELDS: CalcFieldsFm.ShowForm(TdxForm(GetControl));
    C_VIEWTYPE: FormMnu.PopupMenu(FormDesign.Form);// FormViewFm.ShowForm(FormDesign.Form);
    C_EXPR:
      begin
        if C is TdxLabel then ShowExprDlg(C)
        else ExprMnu.PopupMenu(C);
      end;
    C_LISTFILTER: LookupFilterDlg(C); //LookupFilterFm.ShowForm(C);
    C_COUNTER: CounterFm.ShowForm(TdxCounter(C));
    C_QUERYNAME: QueryNameDlg(C);
    C_QUERYSOURCE: QuerySourceDlg(C);
    C_QUERYGRID: QueryGridDlg(C);
    C_QUERYFILTER: QueryFilterDlg(C);
    C_FORMFILTERS: FormFiltersFm.ShowForm(TdxForm(C));
    C_OBJFIELDS: ObjFieldsFm.ShowForm(TdxObjectField(C));
    C_CURTIME: TimeEditMnu.PopupMenu(TdxTimeEdit(C));
    C_TIMEFMT: TimeEditMnu.PopupMenu2(TdxTimeEdit(C));
    C_COLORING: ColoringFm.ShowForm(FormDesign.Form);
    C_FIELDSIZE: FieldSizeDlg(C);
    C_INSERTVALUES: InsValMnu.PopupMenu(TdxLookupComboBox(C)); //InsertValuesFm.ShowForm(TdxLookupComboBox(C));
    //C_FILTERPARAMS: FltParamsFm.ShowForm(TdxLookupComboBox(C));
    C_CHECKVALUE:
      if C is TdxCheckBox then
        CheckExprDlg(TdxCheckBox(C))
      else
        RequiredMnu.PopupMenu(C);
    C_READONLY: CounterMnu.PopupMenu(TdxCounter(C));
    //C_EDITCOMBO: CbxMnu.PopupMenu(TCustomComboBox(C));
    C_LBLTEXT: if LabelTextFm.ShowForm(TdxLabel(C)) = mrOk then FormDesign.UpdateDesigner;
    C_RANGE: RangeFm.ShowForm(TdxCalcEdit(C));
    C_SHOP: ShoppingFm.ShowForm(FormDesign.Form);
    C_DELIM: DelimDlg(TdxMemo(C));
    C_QUERYSTYLE: QueryStyleDlg(TdxQueryGrid(C));
    C_QCALCFIELDS: QueryCalcDlg(C);
    C_DEFAULTVALUE: DefaultValueDlg(C);
    C_HINT: HintFm.ShowForm(TdxLabel(C));
    C_HELPTEXT: FormDesign.Form.HelpText.Text := HelpTextFm.ShowForm(FormDesign.Form.HelpText.Text);
    C_PARENTFIELD: ParentFieldFm.ShowForm(FormDesign.Form);
    C_TREE: TreeFm.ShowForm(FormDesign.Form);
    C_GRIDBUTTONS: GridButtonsFm.ShowForm(TMyDBGrid(C));
    C_QUERYCOLORING: QueryColoringDlg(C);
    C_ACTION: ActionFm.ShowForm(TdxButton(C));
    C_BUTTONGLYPH: BtnGlyphMnu.PopupMenu(TdxButton(C));
    //C_BUTTONNAME: ButtonNameDlg(TdxButton(C));
    C_PIVOTGRID: PivotGridFm.ShowForm(TdxPivotGrid(C));
    C_SELECTQUERY: SelectQueryDlg(TdxPivotGrid(C));
    C_COMPONENTNAME: ComponentNameDlg(C);
    //C_SCROLLBARS: ShowSBMnu.PopupMenu(FormDesign.Form);
    C_EDITMASK: ShowInputMaskForm(TdxEdit(C));
    C_FORMMORE: FormMoreMnu.PopupMenu(FormDesign.Form);
  end;
  DesignFr.UpdateStatusBar;
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
        C_VIEWTYPE, C_TEMPLATE, C_CALCFIELDS, C_FORMFILTERS, C_COLORING, C_HELPTEXT, C_PARENTFIELD, C_TREE, C_FORMMORE])
    else
      SetArray(Props, [C_FORMCAPTION, C_COLOR, C_FONT, C_GRID, C_CALCFIELDS, C_COLORING, C_SHOP, C_HELPTEXT, C_FORMMORE])
  end
  else if AValue is TdxLabel then
    SetArray(Props, [C_COMPONENTNAME, C_LABELCAPTION, C_LBLTEXT, C_COLOR, C_FONT, C_EXPR, C_HINT])
  else if AValue is TdxEdit then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_FIELDSIZE, C_EDITMASK, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE])
  else if AValue is TdxCalcEdit then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_PRECISSION, C_EXPR, C_CHECKVALUE, C_RANGE, C_DEFAULTVALUE])
  else if AValue is TdxDateEdit then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_DATENOW, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE])
  else if AValue is TdxMemo then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_FIELDSIZE, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE, C_LISTSOURCE, C_LISTFILTER, C_DELIM])
  else if AValue is TdxCheckBox then
    SetArray(Props, [C_FIELDNAME, C_LABELCAPTION, C_COLOR, C_FONT, C_CHECKTEXT, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE])
  else if AValue is TdxComboBox then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_FIELDSIZE, C_LIST, C_LISTSOURCE, C_LISTFILTER, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE])
  else if AValue is TdxLookupComboBox then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_LISTSOURCE, C_LISTFILTER, C_INSERTVALUES, C_EXPR, C_CHECKVALUE, C_DEFAULTVALUE])
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
    SetArray(Props, [C_FIELDNAME, C_STORETYPE, C_THUMBSIZE])
  else if AValue is TdxImage then
    SetArray(Props, [C_COMPONENTNAME, C_LOADIMAGE, C_SAVEIMAGE, C_CLEARIMAGE])
  else if AValue is TdxFile then
    SetArray(Props, [C_FIELDNAME, C_COLOR, C_FONT, C_FIELDSIZE, C_STORETYPE])
  else if AValue is TdxQueryGrid then
    SetArray(Props, [C_QUERYNAME, C_QUERYSOURCE, C_QUERYGRID, C_QCALCFIELDS,
    C_QUERYFILTER, C_QUERYCOLORING, C_QUERYSTYLE, C_GRIDBUTTONS])
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
      C_QUERYCOLORING, C_PIVOTGRID])
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
  ClientHeight := 32;
end;

function TPropsForm.CreateButton(aKind: Integer): TSpeedButton;
const
  Glyphs: array [1..65] of String = ('id24', 'color24', 'font24', 'form24',
    'list24', 'listsource24', 'print24', 'id24', 'prec24', 'date24', 'grid24',
    'label24', 'tab_add24', 'tab_remove24', 'left24', 'right24', 'label24',
    'label24', 'shape24', 'box24', 'rect24', 'open24', 'save24', 'delete24',
    'layout24', 'checkbox24', 'sum24', 'sum24', 'filter24', 'counter24',
    'id24', 'source24', 'filter24', 'delim24', 'help24', 'grid24', 'filter24',
    'link24', 'clock24', 'view24', 'colors24', 'size24', 'equal', 'style24',
    'required24', 'readonly24', 'text24', 'range24', 'shopping24', 'sum24',
    'default24', 'help24', 'parent24', 'tree24', 'gridbuttons24', 'colors24',
    'action24', 'image24', 'id24', 'pivotgrid24', 'query24', 'id24',
    'scrollbars24', 'editmask24', 'more24');
var
  Hints: array [1..65] of String;
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
  Hints[21] := rsThumbSize;
  Hints[22] := rsLoadImage;
  Hints[23] := rsSaveImage;
  Hints[24] := rsClearImage;
  Hints[25] := rsViewType;
  Hints[26] := rsPrintOut;
  Hints[27] := rsExpression;
  Hints[28] := rsCalcFields;
  Hints[29] := rsFilter;
  HInts[30] := rsStartWith;
  Hints[31] := rsName;
  Hints[32] := rsSelectionSet;
  Hints[33] := rsOutputFilter;
  Hints[34] := rsDelimiter;
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
  Hints[53] := rsParentField;
  Hints[54] := rsTree;
  Hints[55] := rsButtons;
  Hints[56] := rsColoring;
  Hints[57] := rsAction;
  Hints[58] := rsGlyph;
  //Hints[59] := rsButtonName;
  Hints[60] := rsPivotTable;
  Hints[61] := rsQuery;
  Hints[62] := rsComponentName;
  //Hints[63] := rsShowScrollbars;
  Hints[64] := rsEditMask;
  Hints[65] := rsMoreProps;

  Result := TSpeedButton.Create(Self);
  Result.Width:=32;
  Result.Height:=32;
  Result.Parent := Self;
  Result.Tag := aKind;
  Result.ShowHint:=True;
  Result.Hint:=Hints[aKind];
  Result.LoadGlyphFromLazarusResource(Glyphs[aKind]);
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

