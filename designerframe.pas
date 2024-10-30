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

unit DesignerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls,
  Menus, Graphics, strconsts, dxctrls, formresizer, LclType,
  summarytree, componenttree, Dialogs, SQLDb, formmanager, db,
  formstree;

type
  { TDesignFr }

  TDesignFr = class(TFrame)
    ToolbarImages: TImageList;
    MenuImages: TImageList;
    DummyX: TLabel;
    DummyY: TLabel;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    HideMnu: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ToolBar1: TToolBar;
    DBImageBn: TToolButton;
    CheckBoxBn: TToolButton;
    ListBn: TToolButton;
    ObjectBn: TToolButton;
    TableBn: TToolButton;
    GroupBn: TToolButton;
    PagesBn: TToolButton;
    ShapeBn: TToolButton;
    SaveBn: TToolButton;
    AddFieldsBn: TToolButton;
    ToolButton17: TToolButton;
    AddFmBn: TToolButton;
    DelFmBn: TToolButton;
    StyleBn: TToolButton;
    QueryBn: TToolButton;
    ObjectFieldBn: TToolButton;
    ImageBn: TToolButton;
    TimeBn: TToolButton;
    CounterBn: TToolButton;
    ButtonBn: TToolButton;
    PivotBn: TToolButton;
    ScriptBn: TToolButton;
    TestBn: TToolButton;
    MainActBn: TToolButton;
    ChartBn: TToolButton;
    KeyBn: TToolButton;
    VsblFmBn: TToolButton;
    ToolButton3: TToolButton;
    CursorBn: TToolButton;
    FileBn: TToolButton;
    LabelBn: TToolButton;
    TextBn: TToolButton;
    NumBn: TToolButton;
    DateBn: TToolButton;
    MemoBn: TToolButton;
    ScrollBox1: TScrollBox;
    procedure AddFieldsBnClick(Sender: TObject);
    procedure AddFmBnClick(Sender: TObject);
    procedure DelFmBnClick(Sender: TObject);
    procedure DesignMnuClick(Sender: TObject);
    procedure FormDesignKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HideMnuClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem30Click(Sender: TObject);
    procedure MenuItem37Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ReplaceMnuClick(Sender: TObject);
    procedure SaveBnClick(Sender: TObject);
    procedure StyleBnClick(Sender: TObject);
    procedure TestBnClick(Sender: TObject);
    procedure ScriptBnClick(Sender: TObject);
    procedure MainActBnClick(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure VsblFmBnClick(Sender: TObject);
  private
    { private declarations }
    FCurForm: TdxForm;
    FResizer: TFormResizer;
    FFormsTree: TFormsTree;
    FSummaryTree: TSummaryTree;
    FCompTree: TComponentTree;
    //FFormChanges: TFormChangesList;
    //FCloneFormMan: TFormManager;
    procedure ComponentTreeSelectComponent(Sender: TObject; Cmp: array of TObject);
    procedure FormResize(Sender: TObject);
    procedure FormsTreeCommand(Sender: TObject; Command: TFormsTreeCommand);
    procedure FormsTreeSelectionChanged(Sender: TObject);
    procedure SummaryTreeEditComponent(Sender: TObject; Cmp: TComponent;
      EditProp: TEditProp);
    procedure UpdateDesignMenuState;
    procedure UpdateToolbarState;
    procedure UpdateTreeStates;
    //function CompareForms: String;
    function CheckBeforeChangeStruct: Boolean;
    procedure ChangeStruct;
    procedure InitFields;
    procedure ChangeFieldsSize;
    //procedure CheckAllComponentNames;
    procedure SummaryTreeSelectComponent(Sender: TObject; Cmp: TComponent);
    procedure AdjustScrollPos(C: TControl);

    procedure AddForm;
    procedure DeleteForm;
    procedure ResetDesigner;
  public
    NeedAllCalcRecordSize: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure Done;
    procedure ShowForm(Id: Integer);
    procedure SaveFormsAgain;
    function Save(ConfirmEmptyGroups: Boolean): Boolean;
    procedure UpdateAnchoredComponents(Fm: TdxForm);
    procedure UpdateStatusBar;
    procedure ExportProject(const aFileName: String);
    procedure ImportProject(const aFileName: String);
    procedure MergeProjects(const aFileName: String);
    procedure SetExpertMode(Value: Boolean);
    procedure TestForm(const FormName: String);
    procedure ShowScriptForm(Fm: TdxForm = nil);
    function CheckDeleteForm(Fm: TdxForm): Boolean;
    procedure SelectControl(C: TControl; AddToSelection: Boolean);
    procedure UpdateDesigner;
    //procedure ReCreateScriptForm;
    property FormsTreeView: TFormsTree read FFormsTree;
    property CurForm: TdxForm read FCurForm;
    property CompTree: TComponentTree read FCompTree;
    property SummaryTree: TSummaryTree read FSummaryTree;
  end;

function CalcRecordSize(Fm: TdxForm; OldSizes, WithObjects: Boolean): Integer;

var
  DesignFr: TDesignFr;
  MyTestForm: TdxForm;

implementation

uses
  apputils, sqlgen, dbengine, formdesigner, propsform, imagemanager,
  taborderform, addfieldsform, reportmanager, Visibleformsform, fontform,
  colorform, styleform, propsmenus, dxusers,
  mainform, mergeprojectsform, scriptform, scriptmanager, appsettings,
  scriptfuncs, exprfuncs, anchorsform, propdialogs, debugscriptform,
  mytypes, LConvEncoding, actionseditform, dxmains, dbctrlsex, dxfiles,
  dximages, templatefieldsform, appimagelists, dxreports, findactionsform,
  findexprform, findscriptform, zipper, myzipper, LazFileUtils, LazUtf8;

{$R *.lfm}

function VarCharLen(Len: Integer): Integer;
begin
  if Len > 0 then
    Result := Len * 4 + 2
  // Заметка может иметь неограниченный размер (Len = 0), 8 байт - указатель на blob
  else
    Result := 8;
end;

function CalcRecordSize(Fm: TdxForm; OldSizes, WithObjects: Boolean): Integer;

  function CalcObjectSize(Cbx: TComponent): Integer;
  var
    SrcFm: TdxForm;
    C: TComponent;
  begin
    Result := 0;
    SrcFm := FormMan.FindForm(GetSourceTId(Cbx));
    if SrcFm <> nil then
    begin

      C := FindById(SrcFm, GetSourceFId(Cbx));
      if C <> nil then
      begin

        if (C is TdxEdit) or (C is TdxMemo) or (C is TdxComboBox) then
          Result := VarCharLen(GetFieldSize(C))
        else if C is TdxCounter then
          Result := 4
        else if C is TdxCalcEdit then
          Result := 8;
        if SrcFm.ParentField > 0 then
        begin
          if GetFormParentFieldFieldId(SrcFm) = GetId(C) then
          begin
            Result := Result * SrcFm.LevelCount + (SrcFm.LevelCount - 1);
            // Размер иерархического поля не может превышать 8191 символ
            // (лишнее отсекается), см. функцию SqlSelectGroups
            if Result > 32764 then Result := 32764;
          end;
        end;

      end;

    end;
    Result := Result + 4; // + id
  end;

var
  i: Integer;
  C: TComponent;
  DCI: TDesignCacheItem;
begin
  // Резервирую, т. к. реальный размер записи чуть больше, а как узнать точный
  // размер не знаю.
  Result := 50;
  Result := Result + 4;    // id
  if Fm.PId > 0 then Result := Result + 4 + 4;  // pid, oldid
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if WithObjects and (C is TdxObjectField) then
      C := LookupObjectField(TdxObjectField(C), False);

    if (C is TdxEdit) or (C is TdxMemo) or (C is TdxComboBox) then
      Result := Result + VarCharLen(GetFieldSize(C))
    else if WithObjects and (C is TdxLookupComboBox) then
      Result := Result + CalcObjectSize(C)
    else if (C is TdxCounter) or (C is TdxLookupComboBox) or (C is TdxDateEdit)
      or (C is TdxTimeEdit) or (C is TdxRecordId) then
      Result := Result + 4
    else if C is TdxCalcEdit then
      Result := Result + 8
    else if C is TdxCheckBox then
      Result := Result + 2
    else if C is TdxDBImage then
      Result := Result + VarCharLen(255) + VarCharLen(150) + 8 + 8 + 4 // 4 - флаг изменений
    else if C is TdxFile then
      Result := Result + VarCharLen(255) + VarCharLen(150) + VarCharLen(GetFieldSize(C)) + 8 + 4 // 4 - флаг изменений
    else Continue;

    if OldSizes then
    begin
      DCI := Cache.FindFieldSize(Fm.Id, GetId(C));
      if DCI <> nil then
        Result := Result - VarCharLen(DCI.FieldSize) + VarCharLen(DCI.OldSize);
    end;
  end;
end;

function GetResizedFields(Fm: TdxForm; StartIndex: Integer): String;
var
  i: Integer;
  DCI: TDesignCacheItem;
  C: TComponent;
  SL: TStringListUtf8;
begin
  Result := '';
  SL := TStringListUtf8.Create;

  for i := StartIndex to Cache.Count - 1 do
  begin
    DCI := Cache[i];
    if (DCI.FmId = Fm.Id) and (DCI.Status = dstFieldSize) then
    begin
      C := FindById(Fm, DCI.Id);
      SL.Add(Fm.FormCaption + '->' + GetFieldName(C) +
        ' (' + IntToStr(DCI.OldSize) + '->' + IntToStr(DCI.FieldSize) + ')');
    end;
  end;
  SL.Sort;
  Result := Trim(SL.Text);
  SL.Free;
end;

function IsBackColorChangeable(C: TComponent): Boolean;
const
  Cls = ' tdxcheckbox tdximage tdxdbimage tdxshape tdxgrid tdxquerygrid tdxpivotgrid' +
        ' tdxgroupbox tdxpagecontrol tdxtabsheet tdxbutton ';
begin
  Result := Pos(' ' + LowerCase(C.ClassName) + ' ', Cls) = 0;
end;

function CheckForChangeBackColor(out FirstCtrl: TControl): Boolean;
var
  i: Integer;
  C: TControl;
begin
  Result := False;
  for i := 0 to Length(FormDesign.Selected) - 1 do
  begin
    C := FormDesign.Selection[i];
    if IsBackColorChangeable(C) then
    begin
      FirstCtrl := C;
      Exit(True);
    end;
  end;
end;

function IsFontChangeable(C: TComponent): Boolean;
const
  Cls = ' tdximage tdxdbimage tdxshape tdxgrid tdxquerygrid tdxpivotgrid ';
begin
  Result := Pos(' ' + LowerCase(C.ClassName) + ' ', Cls) = 0;
end;

function CheckForChangingFont(out FirstCtrl: TControl): Boolean;
var
  i: Integer;
  C: TControl;
begin
  Result := False;
  for i := 0 to Length(FormDesign.Selected) - 1 do
  begin
    C := FormDesign.Selection[i];
    if IsFontChangeable(C) then
    begin
      FirstCtrl := C;
      Exit(True);
    end;
  end;
end;

function CheckFirstEditButtonVisible: Integer;
var
  i: Integer;
  C: TControl;
begin
  Result := -1;
  for i := 0 to High(FormDesign.Selected) do
  begin
    C := FormDesign.Selection[i];
    if C is TCustomDBEditButton then
    begin
      if TCustomDBEditButton(C).HideButton then Exit(0)
      else Exit(1);
    end
    else if C is TdxLookupComboBox then
    begin
      if TdxLookupComboBox(C).HideButton then Exit(0)
      else Exit(1);
    end;
  end;
end;

{ TDesignFr }

procedure TDesignFr.SummaryTreeSelectComponent(Sender: TObject; Cmp: TComponent);
begin
  if not (Cmp is TControl) then Exit;
  SelectControl(TControl(Cmp), False);
end;

procedure TDesignFr.AdjustScrollPos(C: TControl);
var
  DX, DY, CW, CH: Integer;
  P: TPoint;
begin
  DX := ScrollBox1.HorzScrollBar.Position;
  DY := ScrollBox1.VertScrollBar.Position;
  CW := ScrollBox1.ClientWidth;
  CH := ScrollBox1.ClientHeight;
  P := C.ClientToParent(Point(0, 0), ScrollBox1);
  P.X := P.X + DX; P.Y := P.Y + DY;

  if P.X > DX + CW - 8 then
  begin
    if C.Width < CW then
      DX := P.X - CW + C.Width + 8
    else
      DX := P.X - 8;
  end
  else if P.X < DX then
  begin
    if P.X + C.Width < CW then
      DX := 0
    else
      DX := P.X - 8;
  end;

  if P.Y > DY + CH - 8 then
  begin
    if C.Height < CH then
      DY := P.Y - CH + C.Height + 8
    else
      DY := P.Y - 8;
  end
  else if P.Y < DY then
  begin
    if P.Y + C.Height < CH then
      DY := 0
    else
      DY := P.Y - 8;
  end;

  ScrollBox1.HorzScrollBar.Position := DX;
  ScrollBox1.VertScrollBar.Position := DY;
end;

procedure TDesignFr.SelectControl(C: TControl; AddToSelection: Boolean);
var
  PC: TWinControl;
begin
  PC := C.Parent;
  while PC <> FCurForm do
  begin
    if PC is TTabSheet then
    	TPageControl(PC.Parent).ActivePage := TTabSheet(PC);
    PC := PC.Parent;
  end;

  if C is TTabSheet then
	 	TPageControl(C.Parent).ActivePage := TTabSheet(C);

  HidePropsForm;
  if AddToSelection then FormDesign.Selector.AddToSelection(C)
  else FormDesign.Select(C);
  AdjustScrollPos(C);

  UpdateStatusBar;
end;

procedure TDesignFr.UpdateDesigner;
begin
  FormDesign.GridSizeX := ScaleToScreen(AppConfig.GridSizeX);
  FormDesign.GridSizeY := ScaleToScreen(AppConfig.GridSizeY);
  FormDesign.GridColor := AppConfig.GridColor;
  FormDesign.ShowGrid := AppConfig.ShowGrid;
end;

procedure TDesignFr.AddForm;
var
  Fm: TdxForm;
begin
  Fm := FormMan.CreateNewForm;
  FFormsTree.AddForm(Fm, False);
  Cache.AddForm(Fm);
  FormChanges.AddForm(Fm.Id, 0);
  DXMain.AddForm(Fm.Id);
  UpdateTemplateFieldsForm;
end;

procedure TDesignFr.DeleteForm;
var
  Fm: TdxForm;
  G: TdxGrid;
  //FmId: Integer;
begin
  if (not ConfirmDelete) or (not CheckDeleteForm(FCurForm)) then Exit;

  if FCurForm.PId > 0 then
  begin
    Fm := FormMan.FindForm(FCurForm.PId);
    G := FindGridById(Fm, FCurForm.Id);
    G.Free;

    Fm.SetFormChanged;
  end;

  Fm := FCurForm;
  FFormsTree.DeleteSelected;

  if ScriptFm <> nil then ScriptFm.DeleteForm(Fm.Id)
  else
  begin
    ScriptMan.DeleteFormModule(Fm.Id, skForm);
    ScriptMan.DeleteFormModule(Fm.Id, skWebForm);
  end;
  if FindActionsFm <> nil then FindActionsFm.DeleteForm(Fm);
  if FindExprFm <> nil then FindExprFm.DeleteForm(Fm);

  DeleteQueries(Fm);
  DeleteReferences(Fm);
  Cache.DeleteForm(Fm);
  UndoMan.DeleteCache(Fm);
  FormChanges.DeleteForm(Fm.Id);
  DXMain.DeleteForm(Fm.Id);
  FormMan.DeleteForm(Fm.Id);
  UpdateTemplateFieldsForm;
end;

procedure TDesignFr.ResetDesigner;
begin
  HidePropsForm;
  FormDesign.DesignForm(nil);
  if FCurForm <> nil then
  begin
    UpdateAnchoredComponents(FCurForm);
    FCurForm.Parent := nil;
    FCurForm.Visible:=False;
    FCurForm.PopupMenu := nil;
    FResizer.UnBind;
  end;
  FCurForm := nil;
  FCompTree.ClearAll;
  FSummaryTree.ClearAll;
  UpdateTreeStates;
  UpdateToolbarState;
  UpdateStatusBar;
end;

procedure TDesignFr.UpdateAnchoredComponents(Fm: TdxForm);
var
  i: Integer;
  C: TComponent;
begin
  if Fm = nil then Exit;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxPageControl then
      TdxPageControl(C).UpdateAnchoredControls;
  end;
end;

{procedure TDesignFr.CheckAllComponentNames;
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
begin
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C.Name = '' then C.Name := DesignUniqueName(Fm, C.ClassName);
    end;
  end;
end;        }

procedure TDesignFr.DesignMnuClick(Sender: TObject);

  function GetFirstSelectionWinControl: TControl;
  var
    i, L: Integer;
    C: TControl;
  begin
    Result := nil;
    L := Length(FormDesign.Selected);
    for i := 0 to L - 1 do
    begin
      C := FormDesign.Selection[i];
      if C is TWinControl then Exit(C);
    end;
    if (Result = nil) and (L > 0) then
      Result := FormDesign.Selection[0].Parent;
  end;

begin
  case TMenuItem(Sender).Tag of
    0: FormDesign.MoveComponents;
    1: FormDesign.CopyComponents;
    2: FormDesign.PasteComponents;
    3: FormDesign.UserDeleteComponents;
    4: FormDesign.SetLeftMostEdge;
    5: FormDesign.SetRightMostEdge;
    6: FormDesign.SetTopMostEdge;
    7: FormDesign.SetBottomMostEdge;
    8: FormDesign.SetHorzCenter;
    9: FormDesign.SetVertCenter;
    10: FormDesign.SetMaxWidth;
    11: FormDesign.SetMinWidth;
    12: FormDesign.SetMaxHeight;
    13: FormDesign.SetMinHeight;
    14: FormDesign.BringToFront;
    15: FormDesign.SendToBack;
    16:
      begin
        //FormDesign.ClearSelection;
        ShowTabOrderForm(FormDesign.Form, GetFirstSelectionWinControl);
      end;
  end;
end;

procedure TDesignFr.FormDesignKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F4: if (Shift = []) and AppConfig.ExpertMode then ShowScriptForm;
    VK_F5: if Shift = [] then TestBn.Click;
    VK_F11: MainFm.Timer1.Enabled := True;
    VK_F12: MainFm.ReportsMnu.Click;
    VK_Z:
      if Shift = [ssCtrl] then UndoMan.CurrentCache.Undo
      else if Shift = [ssCtrl, ssShift] then UndoMan.CurrentCache.Redo
  end;
end;

procedure TDesignFr.HideMnuClick(Sender: TObject);
var
  i: Integer;
  C: TControl;
  V: Boolean;
begin
  V := not HideMnu.Checked;
  for i := 0 to High(FormDesign.Selected) do
  begin
    C := FormDesign.Selection[i];
    SetHidden(C, V);
  end;
  FSummaryTree.UpdateTree;
end;

procedure TDesignFr.MenuItem1Click(Sender: TObject);
var
  i: Integer;
  C: TControl;
  V: Boolean;
begin
  V := MenuItem1.Caption = rsHideButton;
  for i := 0 to High(FormDesign.Selected) do
  begin
    C := FormDesign.Selection[i];
    if C is TCustomDBEditButton then
      TCustomDBEditButton(C).HideButton := V
    else if C is TdxLookupComboBox then
      TdxLookupComboBox(C).HideButton := V;
  end;
end;

procedure TDesignFr.AddFieldsBnClick(Sender: TObject);
begin
  if FormDesign.Active then
    if ShowAddFieldsForm(FormDesign.Form) = mrOk then
    begin
      HidePropsForm;
      CursorBn.Down:=True;
      FormDesign.ControlClass := '';
    end;
end;

procedure TDesignFr.AddFmBnClick(Sender: TObject);
begin
  AddForm;
end;

procedure TDesignFr.DelFmBnClick(Sender: TObject);
begin
  DeleteForm;
end;

procedure TDesignFr.MenuItem27Click(Sender: TObject);
var
  C, FirstC: TControl;
  i: Integer;
  Fnt: TFont;
begin
  if not CheckForChangingFont(FirstC) then Exit;
  Fnt := FirstC.Font;
  if ShowFontForm(Fnt, FirstC.Parent.Font) <> mrOk then Exit;
  for i := 0 to Length(FormDesign.Selected) - 1 do
  begin
    C := FormDesign.Selection[i];
    if IsFontChangeable(C) then
    begin
      C.Font := Fnt;
      SetParentFont(C, C.Font.IsEqual(C.Parent.Font));
    end;
  end;
end;

procedure TDesignFr.MenuItem28Click(Sender: TObject);
var
  i: Integer;
  C, CC: TControl;
begin
  if not CheckForChangeBackColor(CC) then Exit;
  if ShowColorForm(CC) <> mrOk then Exit;
  for i := 0 to Length(FormDesign.Selected) - 1 do
  begin
    C := FormDesign.Selection[i];
    if IsBackColorChangeable(C) then
      C.Color := CC.Color;
  end;
end;

{procedure DeleteAllDetailForms(Fm: TdxForm);
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxGrid then
      with TdxGrid(C) do
          FormMan.DeleteForm(Id);
  end;
end;    }

procedure TDesignFr.MenuItem30Click(Sender: TObject);
begin
  FormDesign.FindLost;
  if Length(FormDesign.Selected) = 0 then
    CompTree.SelectComponents([FCurForm])
  else
    CompTree.SelectComponents(FormDesign.Selected);
end;

procedure TDesignFr.MenuItem37Click(Sender: TObject);
var
  CL: TList;
  i: Integer;
begin
  CL := TList.Create;
  for i := 0 to Length(FormDesign.Selected) - 1 do
    CL.Add(FormDesign.Selection[i]);
  ShowAnchorsForm(CL);
  CL.Free;
end;

procedure TDesignFr.PageControl1Change(Sender: TObject);
begin
  // По каким-то причинам смена закладки устанавливает фокус на каком-либо компоненте.
  PageControl1.SetFocus;
end;

procedure TDesignFr.PopupMenu1Popup(Sender: TObject);
begin
  HidePropsForm;
  UpdateDesignMenuState;
end;

procedure TDesignFr.ReplaceMnuClick(Sender: TObject);
var
  i: PtrInt;
  C, NewC: TControl;
  Fm: TdxForm;
  CName: TComponentName;
  CTabOrder: TTabOrder;
begin
  i := TMenuItem(Sender).Tag;
  C := FormDesign.Control;
  if (C is TdxMemo) and (GetFieldSize(C) = 0) then
  begin
    Info(rsCantReplaceMemo);
    Exit;
  end;
  CName := C.Name;
  CTabOrder := TWinControl(C).TabOrder;
  FormDesign.ClearSelection;
  Fm := FormDesign.Form;
  case i of
    1: NewC := TdxEdit.Create(Fm);
    2: NewC := TdxMemo.Create(Fm);
    3: NewC := TdxComboBox.Create(Fm);
  end;
  NewC.Parent := C.Parent;
  NewC.BoundsRect := C.BoundsRect;
  SetId(NewC, GetId(C));
  SetFieldName(NewC, GetFieldName(C));
  NewC.Font := C.Font;
  NewC.Color := C.Color;
  SetExpression(NewC, GetExpression(C));
  SetEditable(NewC, GetEditable(C));
  SetDefaultValue(NewC, GetDefaultValue(C));
  SetCheckExpression(NewC, GetCheckExpression(C));
  SetRequired(NewC, GetRequired(C));
  SetFieldSize(NewC, GetFieldSize(C));
  TWinControl(NewC).TabStop := TWinControl(C).TabStop;

  C.Free;
  NewC.Name := CName;//DesignUniqueName(Fm, NewC.ClassName);
  TWinControl(NewC).TabOrder := CTabOrder;

  FormDesign.Messenger.DesignComponent(NewC, True);
  FormDesign.Selector.AddToSelection(NewC);
  CompTree.UpdateTree;
  CompTree.SelectComponents(FormDesign.Selected);
  SummaryTree.UpdateTree;
  UpdateTemplateFieldsForm;
end;

procedure TDesignFr.SaveBnClick(Sender: TObject);
begin
  Save(False);
end;

procedure TDesignFr.StyleBnClick(Sender: TObject);
begin
  ShowStyleForm(FCurForm);
end;

procedure TDesignFr.TestBnClick(Sender: TObject);
begin
  TestForm(FCurForm.FormCaption);
end;

procedure TDesignFr.ScriptBnClick(Sender: TObject);
begin
  if (ScriptFm <> nil) and ScriptFm.Visible and (ScriptFm.WindowState <> wsMinimized) then
    ScriptFm.Close
  else
    ShowScriptForm;
end;

procedure TDesignFr.MainActBnClick(Sender: TObject);
var
  mr: Integer;
begin
  DXMain.Actions := ShowActionsEditForm(rsStartupActions, DXMain.Actions, nil,
    [atMain, atAll], mr);
end;

procedure TDesignFr.ToolButtonClick(Sender: TObject);
const
  Comps: array [0..23] of String = ('', 'TdxLabel', 'TdxEdit', 'TdxCalcEdit',
    'TdxDateEdit', 'TdxMemo', 'TdxCheckBox', 'TdxComboBox', 'TdxLookupComboBox',
    'TdxGrid', 'TdxGroupBox', 'TdxPageControl', 'TdxShape', 'TdxDBImage',
    'TdxImage', 'TdxFile', 'TdxQueryGrid', 'TdxObjectField', 'TdxTimeEdit',
    'TdxCounter', 'TdxButton', 'TdxPivotGrid', 'TdxChart', 'TdxRecordId');
begin
  FormDesign.ControlClass := Comps[ TComponent(Sender).Tag ];
  HidePropsForm;
end;

procedure TDesignFr.VsblFmBnClick(Sender: TObject);
begin
  ShowVisibleFormsForm;
end;

procedure TDesignFr.SummaryTreeEditComponent(Sender: TObject; Cmp: TComponent;
  EditProp: TEditProp);
begin
 	case EditProp of
    epDefaultValue: DefaultValueDlg(Cmp);
    epExpression: ShowExprDlg(Cmp);
    epCheckExpression: ShowCheckExprDlg(Cmp);
    epListFilter: LookupFilterDlg(Cmp);
  end;
end;

function ControlExists(C: TObject; Controls: array of TObject): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(Controls) - 1 do
    if C = Controls[i] then Exit(True);
end;

procedure TDesignFr.ComponentTreeSelectComponent(Sender: TObject;
  Cmp: array of TObject);
var
  i: Integer;
begin
  for i := Length(FormDesign.Selected) - 1 downto 0 do
    if not ControlExists(FormDesign.Selection[i], Cmp) then
      FormDesign.Selector.RemoveFromSelection(FormDesign.Selection[i]);
  for i := 0 to Length(Cmp) - 1 do
    if (Cmp[i] <> FCurForm) and not ControlExists(Cmp[i], FormDesign.Selected) then
      SelectControl(TControl(Cmp[i]), True);
  FormDesign.OnSelectionChange(FormDesign);
  HidePropsForm;
end;

procedure TDesignFr.FormResize(Sender: TObject);
begin
  if FormDesign.Control = nil then;
    UpdateStatusBar;
end;

procedure TDesignFr.FormsTreeCommand(Sender: TObject; Command: TFormsTreeCommand
  );
begin
  case Command of
    ftcAddForm: AddForm;
    ftcDeleteForm: DeleteForm;
    ftcScriptEditor: ShowScriptForm(FCurForm);
  end;
end;

procedure TDesignFr.FormsTreeSelectionChanged(Sender: TObject);
begin
	if FFormsTree.SelectedForm <> nil then
	  ShowForm(FFormsTree.SelectedForm.Id)
  else
  	ResetDesigner;
end;

function GetFirstComponentHiddenState: Integer;
begin
  if FormDesign.Count = 0 then Exit(-1);
  if GetHidden(FormDesign.Selection[0]) then Result := 1
  else Result := 0;
end;

procedure TDesignFr.UpdateDesignMenuState;
var
  C, DummyCtrl: TControl;
  MI: TMenuItem;
  n: Integer;
begin
  MenuItem3.Enabled:=FormDesign.Count > 0;
  MenuItem4.Enabled := FormDesign.Count > 0;
  MenuItem5.Enabled := FormDesign.CanPasteComponents or FormDesign.CanMoveComponents;
  MenuItem7.Enabled := FormDesign.Count > 0;
  MenuItem11.Enabled := FormDesign.Count > 1;
  MenuItem12.Enabled := FormDesign.Count > 1;
  MenuItem13.Enabled := FormDesign.Count > 1;
  MenuItem14.Enabled := FormDesign.Count > 1;
  MenuItem15.Enabled := FormDesign.Count > 1;
  MenuItem16.Enabled := FormDesign.Count > 1;
  MenuItem17.Enabled := FormDesign.Count > 1;
  MenuItem18.Enabled := FormDesign.Count > 1;
  MenuItem19.Enabled := FormDesign.Count > 1;
  MenuItem20.Enabled := FormDesign.Count > 1;
  MenuItem22.Enabled := FormDesign.Count >= 1;
  MenuItem23.Enabled := FormDesign.Count >= 1;
  MenuItem27.Enabled := CheckForChangingFont(DummyCtrl);
  MenuItem28.Enabled := CheckForChangeBackColor(DummyCtrl);
  C := FormDesign.Control;
  MenuItem32.Enabled:=(FormDesign.Count = 1) and ((C is TdxMemo) or (C is TdxComboBox));
  MenuItem33.Enabled:=(FormDesign.Count = 1) and ((C is TdxEdit) or (C is TdxComboBox));
  MenuItem34.Enabled:=(FormDesign.Count = 1) and ((C is TdxMemo) or (C is TdxEdit));
  MenuItem37.Enabled := (FormDesign.Count > 0) and HasAnchors(FormDesign.Control);

  n := GetFirstComponentHiddenState;
  HideMnu.Enabled := n >= 0;
  HideMnu.Checked := n = 1;

  // "Скрыть кнопку"
  MI := MenuItem1;
  case CheckFirstEditButtonVisible of
    -1:
      begin
        MI.Enabled := False;
        MI.Caption := rsHideButton;
      end;
    0:
      begin
        MI.Enabled := True;
        MI.Caption := rsShowButton;
      end;
    1:
      begin
        MI.Enabled := True;
        MI.Caption := rsHideButton;
      end;
  end;
end;

procedure TDesignFr.UpdateToolbarState;
begin
  if FormDesign.Active then
  begin
    TableBn.Enabled := FormDesign.Form.PId = 0;
    //QueryBn.Enabled := FormDesign.Form.PId = 0;
  end;
  DelFmBn.Enabled := FCurForm <> nil;
  TestBn.Enabled := FCurForm <> nil;
  //VsblFmBn.Enabled := FormList.Count > 0;
  StyleBn.Enabled := FCurForm <> nil;
end;

procedure TDesignFr.UpdateTreeStates;
begin
  FCompTree.Enabled:=FCurForm <> nil;
  FSummaryTree.Enabled:=FCurForm <> nil;
end;

procedure TDesignFr.UpdateStatusBar;
var
  C: TControl;
begin
  if FormDesign.Active = False then
  begin
    StatusBar.Panels[1].Text:='';
    StatusBar.Panels[2].Text := '';
    StatusBar.Panels[3].Text := '';
  end
  else if Length(FormDesign.Selected) > 1 then
  begin
    StatusBar.Panels[1].Text:=Format(rsSelectedComponentCount, [FormDesign.Count]);
    StatusBar.Panels[2].Text := '';
    StatusBar.Panels[3].Text := '';
  end
  else
  begin
    if Length(FormDesign.Selected) = 1 then
      C := FormDesign.Control
    else
      C := FormDesign.Form;
    StatusBar.Panels[1].Text := GetComponentType(C);
    StatusBar.Panels[2].Text := GetComponentName(C);
    StatusBar.Panels[3].Text := Format('X: %d Y: %d W: %d H: %d',
      [C.Left, C.Top, C.Width, C.Height]);
  end;
end;

procedure CreateVersionFile(const aDir: String);
var
  S: String;
begin
  with TFileStream.Create(aDir + 'version', fmCreate + fmOpenWrite) do
  try
  	S := IntToStr(DX_VERSION);
    Write(Pointer(S)^, Length(S));
  finally
    Free;
  end;
end;

procedure TDesignFr.ExportProject(const aFileName: String);
var
  TempDir, S: String;
  SL: TStringList;
  i: Integer;
begin
  try

  if ScriptFm <> nil then ScriptFm.SaveAll;
  FFormsTree.SaveTree;

  TempDir := GetTempDir + 'dx' + IntToStr(Random(1000000)) + DirectorySeparator;
  CreateDir(TempDir);
  FormMan.SaveToDir(TempDir);
  ReportMan.SaveToDir(TempDir);
  UserMan.SaveToDir(TempDir);
  ScriptMan.SaveToDir(TempDir);
  DXMain.SaveToDir(TempDir);
  ImageMan.SaveToDir(TempDir);
  CreateVersionFile(TempDir);
  SL := FindAllFiles(TempDir, '*');
  with TZipper.Create do
  try
    FileName:=aFileName;
    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      S := Copy(S, Length(TempDir) + 1, 1024);
      Entries.AddFileEntry(SL[i], Utf8ToCP866(S));
    end;
    ZipAllFiles;
    MessageDlg(rsExportProject, rsExportPrjOk, mtInformation, [mbOk], 0);
  finally
    Free;
    SL.Free;
  end;

  except
    on E: Exception do
      ErrMsg(rsExportProjectError + ExceptionToString(E, True, False));
  end;
end;

procedure TDesignFr.ImportProject(const aFileName: String);

  procedure CompareForms(Fm, OldFm: TdxForm);
  var
    i: Integer;
    OldC, C: TComponent;
  begin
    for i := 0 to OldFm.ComponentCount - 1 do
    begin
      OldC := OldFm.Components[i];
      if not IsField(OldC) then Continue;
      C := FindById(Fm, GetId(OldC));
      if C = nil then Cache.DeleteComponent(OldC)
      else if IsTextComponent(C) and IsTextComponent(OldC) then
      else if C.ClassName <> OldC.ClassName then Cache.DeleteComponent(OldC);
    end;
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if not IsField(C) then Continue;
      OldC := FindById(OldFm, GetId(C));
      if OldC = nil then Cache.AddComponent(C)
      else if IsTextComponent(C) and IsTextComponent(OldC) then
      begin
        if GetFieldSize(C) <> GetFieldSize(OldC) then
          Cache.SetFieldSize(C, GetFieldSize(OldC));
      end
      else if C.ClassName <> OldC.ClassName then Cache.AddComponent(C);
    end;
  end;

  procedure ImportForms(const TempDir: String);
  var
    SL: TStringList;
    IL: TIntegerList;
    i, n: Integer;
    Fm, ImportFm: TdxForm;
    FileName: String;
  begin
    SL := TStringList.Create;
    IL := TIntegerList.Create;

    try

    FindAllFiles(SL, TempDir, '*.frm', False);
    for i := 0 to SL.Count - 1 do
      if TryStrToInt(ChangeFileExt(ExtractFileName(SL[i]), ''), n) then
        IL.AddValue(n);

    // Удаляем формы, которых нет в списке
    for i := FormMan.FormCount - 1 downto 0 do
    begin
      Fm := FormMan.Forms[i];
      if IL.FindValue(Fm.Id) < 0 then
      begin
        Cache.DeleteForm(Fm);
        FormChanges.DeleteForm(Fm.Id);
        FormMan.DeleteForm(Fm.Id);
      end;
    end;

    // Сравниваем формы
    for i := FormMan.FormCount - 1 downto 0 do
    begin
      Fm := FormMan.Forms[i];
      FileName := TempDir + IntToStr(Fm.Id) + '.frm';
      if SameFileDateTime(FileName, Fm.LastModified) then Continue;

      ImportFm := FormMan.LoadFromFile(FileName);
      ImportFm.SetFormChanged;
      ImportFm.LastModified := GetFileDateTime(FileName);
      ScaleForm(ImportFm, DXMain.DesignTimePPI);

      if ImportFm.PId <> Fm.PId then
      begin
        Cache.DeleteForm(Fm);
        FormChanges.DeleteForm(Fm.Id);
        FormMan.DeleteForm(Fm.Id);
        Cache.AddFormWithComponents(ImportFm);
        FormChanges.AddForm(ImportFm.Id, 0);
        FormMan.AddForm(ImportFm);
      end
      else
      begin
        CompareForms(ImportFm, Fm);
        FormMan.ReplaceForm(Fm, ImportFm);
      end;
    end;

    // Добавление новых форм
    for i := 0 to IL.Count - 1 do
    begin
      n := IL[i];
      if FormMan.FindForm(n) <> nil then Continue;

      FileName := TempDir + IntToStr(n) + '.frm';
      ImportFm := FormMan.LoadFromFile(FileName);
      ImportFm.LastModified := GetFileDateTime(FileName);
      ScaleForm(ImportFm, DXMain.DesignTimePPI);

      Cache.AddFormWithComponents(ImportFm);
      FormChanges.AddForm(ImportFm.Id, 0);
      FormMan.AddForm(ImportFm);
    end;

    FormMan.CorrectMaxTId;
    FormMan.CorrectMaxFId;

    finally
      SL.Free;
      IL.Free;
    end;
  end;

  procedure ImportReports(const TempDir: String);
  var
    SL: TStringList;
    IL: TIntegerList;
    i, n: Integer;
    RD, ImportRD: TReportData;
    FileName: String;
  begin
    SL := TStringList.Create;
    IL := TIntegerList.Create;

    try

    FindAllFiles(SL, TempDir, '*.rpt', False);
    for i := 0 to SL.Count - 1 do
      if TryStrToInt(ChangeFileExt(ExtractFileName(SL[i]), ''), n) then
        IL.AddValue(n);

    // Удаляем старые отчеты
    for i := ReportMan.ReportCount - 1 downto 0 do
    begin
      RD := ReportMan.Reports[i];
      if IL.FindValue(RD.Id) < 0 then
        ReportMan.DeleteReport(RD);
    end;

    // Сравниваем отчеты
    for i := 0 to ReportMan.ReportCount - 1 do
    begin
      RD := ReportMan.Reports[i];
      FileName := TempDir + IntToStr(RD.Id) + '.rpt';
      if SameFileDateTime(FileName, RD.LastModified) then Continue;

      ImportRD := ReportMan.LoadFromFile(FileName);
      ImportRD.SetReportChanged;
      ImportRD.LastModified := GetFileDateTime(FileName);
      ScaleReport(ImportRD, DXMain.DesignTimePPI, Screen.PixelsPerInch);
      ReportMan.ReplaceReport(RD, ImportRD);
    end;

    // Добавляем новые
    for i := 0 to IL.Count - 1 do
    begin
      n := IL[i];
      if ReportMan.FindReport(n) <> nil then Continue;

      FileName := TempDir + IntToStr(n) + '.rpt';
      ImportRD := ReportMan.LoadFromFile(FileName);
      ImportRD.LastModified := GetFileDateTime(FileName);
      ScaleReport(ImportRD, DXMain.DesignTimePPI, Screen.PixelsPerInch);
      ReportMan.AddReport(ImportRD);
    end;

    ReportMan.CorrectMaxId;

    finally
      SL.Free;
      IL.Free;
    end;
  end;

  procedure ImportScripts(const TempDir: String);
  var
    SL: TStringList;
    Names: TStringListUtf8;
    i: Integer;
    SD, ImportSD: TScriptData;
    FileName, Nm, Ext: String;
    FmId: LongInt;
    SDKind: TScriptKind;
  begin
    SL := TStringList.Create;
    Names := TStringListUtf8.Create;
    Names.CaseSensitive:=False;
    FindAllFiles(SL, TempDir, '*.pas;*.fpas;*.epas;*.wfpas;*.wepas', False);

    try

      for i := 0 to SL.Count - 1 do
        Names.Add(ExtractFileName(SL[i]));

      // Удаляем старое
      for i := ScriptMan.ScriptCount - 1 downto 0 do
      begin
        SD := ScriptMan.Scripts[i];

        if (SD.Kind = skForm) and (Names.IndexOf(IntToStr(SD.FmId) + '.fpas') < 0) or
          (SD.Kind = skWebForm) and (Names.IndexOf(IntToStr(SD.FmId) + '.wfpas') < 0) or
          (SD.Kind = skExpr) and (Names.IndexOf(SD.Name + '.epas') < 0) or
          (SD.Kind = skWebExpr) and (Names.IndexOf(SD.Name + '.wepas') < 0) or
          (SD.Kind in [skUser, skWebMain]) and (Names.IndexOf(SD.Name + '.pas') < 0) then
        begin
          ScriptMan.DeleteScript(SD);
        end;
      end;

      // Сравниваем
      for i := 0 to ScriptMan.ScriptCount - 1 do
      begin
        SD := ScriptMan.Scripts[i];
        FileName := TempDir + SD.GetFileName;
        if SameFileDateTime(FileName, SD.LastModified) then Continue;

        ImportSD := TScriptData.Create;
        ImportSD.Source := LoadString(FileName);
        ImportSD.SourceData.LoadFromFile(FileName + '.cfg');
        ImportSD.Name := SD.Name;
        ImportSD.FmId := SD.FmId;
        ImportSD.Kind := SD.Kind;
        ImportSD.Id := SD.Id;
        ImportSD.PartChanges := [spcName, spcScript, spcExtra];
        ImportSD.LastModified := GetFileDateTime(FileName);
        ScriptMan.ReplaceScript(SD, ImportSD);
      end;

      // Добавляем новые
      for i := 0 to Names.Count - 1 do
      begin
        FileName := TempDir + Names[i];
        Nm := ExtractFileNameOnly(Names[i]);
        Ext := LowerCase(ExtractFileExt(Names[i]));

        FmId := 0;
        if (Ext = '.fpas') and (ScriptMan.FindScript(StrToInt(Nm), skForm) = nil) then
        begin
          FmId := StrToInt(Nm);
          SDKind := skForm;
        end
        else if (Ext = '.wfpas') and (ScriptMan.FindScript(StrToInt(Nm), skWebForm) = nil) then
        begin
          FmId := StrToInt(Nm);
          SDKind := skWebForm;
        end
        else if (Ext = '.epas') and (ScriptMan.FindScriptByName(Nm) = nil) then
        begin
          SDKind := skExpr;
        end
        else if (Ext = '.wepas') and (ScriptMan.FindScriptByName(Nm) = nil) then
        begin
          SDKind := skWebExpr;
        end
        else if (Ext = '.pas') and (ScriptMan.FindScriptByName(Nm) = nil) then
        begin
          if Nm = 'WebMain' then SDKind := skWebMain
          else SDKind := skUser;
        end
        else Continue;

        if FmId > 0 then Nm := '';
        ImportSD := ScriptMan.AddNewScript(FmId, Nm, LoadString(FileName));
        ImportSD.SourceData.LoadFromFile(FileName + '.cfg');
        ImportSD.Kind := SDKind;
        ImportSD.LastModified := GetFileDateTime(FileName);

      end;
    finally
      SL.Free;
      Names.Free;
    end;
  end;

  procedure ImportUsers(const TempDir: String);
  var
    UsersModified: TDateTime;
    ConnId, UId, i: Integer;
    TmpSt: TMemoryStream;
    OldUsers: TdxUserList;
    OldU, U: TdxUser;
    FileName: String;
  begin
    FileName := TempDir + 'users';
    if SameFileDateTime(FileName, UserMan.LastModified) then Exit;

    TmpSt := TMemoryStream.Create;
    OldUsers := TdxUserList.Create;

    try

      UserMan.Users.SaveToStream(TmpSt);
      OldUsers.LoadFromStream(TmpSt);
      ConnId := UserMan.ConnId;
      UId := UserMan.CurrentUserId;

      UserMan.LoadFromDir(TempDir);
      UserMan.LastModified := GetFileDateTime(FileName);
      UserMan.UsersChanged := True;
      UserMan.RolesChanged := True;
      UserMan.IntfsChanged := True;

      UserMan.ConnId := ConnId;
      if UserMan.Users.FindUser(UId) <> nil then UserMan.CurrentUserId:=UId;
      if (UserMan.CurrentUser <> nil) and (UserMan.CurrentUser.RoleId >= 0) then
        UserMan.CurrentUser.WasDeveloper := True;

      // Восстанавливаем пароли старых пользователей
      for i := 0 to OldUsers.Count - 1 do
      begin
        OldU := OldUsers[i];
        U := UserMan.Users.FindUserByName(OldU.Name);
        if U <> nil then U.Password := OldU.Password;
      end;

    finally
      OldUsers.Free;
      TmpSt.Free;
    end;
  end;

  procedure UpdateImages;
  var
    i: Integer;
  begin
    for i := 0 to FormMan.FormCount - 1 do
      UpdateImagesInForm(FormMan.Forms[i]);
  end;

  procedure GetImageFiles(SL: TStrings; const ImgName: String; out Img100, Img150, Img200, Img: String);
  var
    i: Integer;
    Tmp, S: String;
  begin
    Img100 := '';
    Img150 := '';
    Img200 := '';
    Img := '';
    for i := 0 to SL.Count - 1 do
    begin
      if Utf8CompareText(SL.Names[i], ImgName) = 0 then
      begin
        S := SL.ValueFromIndex[i];
        Tmp := RightStr(ExcludeTrailingPathDelimiter(ExtractFilePath(S)), 3);
        if Tmp = '100' then Img100 := S
        else if Tmp = '150' then Img150 := S
        else if Tmp = '200' then Img200 := S;
      end;
    end;
    if Img100 <> '' then Img := Img100
    else if Img150 <> '' then Img := Img150
    else if Img200 <> '' then Img := Img200;
  end;

  procedure ImportImages(const TempDir: String);
  var
    SL: TStringListUtf8;
    DS: TSQLQuery;
    i: Integer;
    Img100, Img150, Img200, ImgFile, Nm: String;
  begin
    SL := TStringListUtf8.Create;

    try
      FindAllFiles(SL, TempDir, '*', True);

      for i := 0 to SL.Count - 1 do
        SL[i] := ExtractFileNameOnly(SL[i]) + '=' + SL[i];

      DS := ImageMan.DataSet;

      // Удаляем старое
      DS.First;
      while not DS.Eof do
      begin
        if SL.IndexOfName(DS.Fields[1].AsString) < 0 then
          DS.Delete
        else
          DS.Next;
      end;

      // Сравниваем
      DS.First;
      while not DS.Eof do
      begin
        GetImageFiles(SL, DS.Fields[1].AsString, Img100, Img150, Img200, ImgFile);
        if SameFileDateTime(ImgFile, DS.Fields[5].AsDateTime) then Continue;

        DS.Edit;

        // img_100
        if Img100 <> '' then
          TBlobField(DS.Fields[2]).LoadFromFile(Img100)
        else
          DS.Fields[2].SetData(nil);

        // img_150
        if Img150 <> '' then
          TBlobField(DS.Fields[3]).LoadFromFile(Img150)
        else
          DS.Fields[3].SetData(nil);

        // img_200
        if Img200 <> '' then
          TBlobField(DS.Fields[4]).LoadFromFile(Img200)
        else
          DS.Fields[4].SetData(nil);

        // lastmodified
        DS.Fields[5].AsDateTime := GetFileDateTime(ImgFile);

        DS.Post;
        DS.Next;
      end;

      // Добавляем новые
      for i := 0 to SL.Count - 1 do
      begin
        Nm := SL.Names[i];
        if DS.Locate('name', Nm, []) then Continue;
        GetImageFiles(SL, Nm, Img100, Img150, Img200, ImgFile);

        ImageMan.AddImage(Nm);
        DS.Edit;
        if Img100 <> '' then TBlobField(DS.Fields[2]).LoadFromFile(Img100);
        if Img150 <> '' then TBlobField(DS.Fields[3]).LoadFromFile(Img150);
        if Img200 <> '' then TBlobField(DS.Fields[4]).LoadFromFile(Img200);
        DS.Fields[5].AsDateTime := GetFileDateTime(ImgFile);
        DS.Post;
      end;

      UpdateImages;
    finally
      SL.Free;
    end;
  end;

var
  TempDir: String;
  Ver: Integer;
begin
  if Cache.StructChanged then
  begin
    Info(rsSaveChangesBeforeImportMsg);
    Exit;
  end;

  if ScriptFm <> nil then ScriptFm.SaveAll;

  TempDir := GetTempDir + 'dx' + IntToStr(Random(1000000)) + DirectorySeparator;

  try

  with TMyUnZipper.Create do
  try
    FileName:=aFileName;
    OutputPath:=TempDir;
    ForceDirectories(OutputPath);
    UnZipAllFiles;
  finally
    Free;
  end;

  Ver := GetVersionFromFile(TempDir);

  if Ver < 31 then
  begin
    ErrMsg(rsDBNotSupport);
    DeleteDirectory(TempDir, False);
    Exit;
  end;

  FFormsTree.ClearAll;
  ResetDesigner;
  ResetTemplateFieldsForm;
  UndoMan.Clear;
  Cache.Clear;

  DXMain.LoadFromDir(TempDir);
  ImportForms(TempDir);
  ImportReports(TempDir);
  ImportScripts(TempDir);
  ImportUsers(TempDir);
  ImportImages(TempDir + 'images');
  ConvertToDXMainVersion2(DXMain, FormMan, False);

  FFormsTree.BuildTree;
  CursorBn.Down := True;

  DeleteDirectory(TempDir, False);
  if AppConfig.ExpertMode and (ScriptFm <> nil) then
    ScriptFm.Reset(False);
  UpdateTemplateFieldsForm;
  if FindActionsFm <> nil then FindActionsFm.Reset;
  if FindExprFm <> nil then FindExprFm.Reset;
  if FindScriptFm <> nil then FindScriptFm.Reset;
  MessageDlg(rsImportProject, rsImportPrjOk, mtInformation, [mbOk], 0);

  except
    on E: Exception do
      ErrMsg(rsImportProjectError + ExceptionToString(E, True, False));
  end;
end;

(*procedure TDesignFr.ImportProject(const aFileName: String);

  procedure _CompareForms(Fm, OldFm: TdxForm);
  var
    i: Integer;
    OldC, C: TComponent;
  begin
    for i := 0 to OldFm.ComponentCount - 1 do
    begin
      OldC := OldFm.Components[i];
      if not IsField(OldC) then Continue;
      C := FindById(Fm, GetId(OldC));
      if C = nil then Cache.DeleteComponent(OldC)
      else if IsTextComponent(C) and IsTextComponent(OldC) then
      else if C.ClassName <> OldC.ClassName then Cache.DeleteComponent(OldC);
    end;
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if not IsField(C) then Continue;
      OldC := FindById(OldFm, GetId(C));
      if OldC = nil then Cache.AddComponent(C)
      else if IsTextComponent(C) and IsTextComponent(OldC) then
      begin
        if GetFieldSize(C) <> GetFieldSize(OldC) then
          Cache.SetFieldSize(C, GetFieldSize(OldC));
      end
      else if C.ClassName <> OldC.ClassName then Cache.AddComponent(C);
    end;
  end;

  procedure CompareFormMans(const TempDir: String);
  var
    i, n: Integer;
    Fm, OldFm: TdxForm;
    SL: TStringList;
    IL: TIntegerList;
  begin
    SL := TStringList.Create;
    IL := TIntegerList.Create;

    try

    FindAllFiles(SL, TempDir, '*.frm', False);
    for i := 0 to SL.Count - 1 do
      if TryStrToInt(ChangeFileExt(ExtractFileName(SL[i]), ''), n) then
        IL.AddValue(n);

    Cache.Clear;
    for i := 0 to FormMan.FormCount - 1 do
    begin
      OldFm := FormMan.Forms[i];
      if IL.FindValue(OldFm.Id) < 0 then
      begin
        Cache.DeleteForm(OldFm);
        FormChanges.DeleteForm(OldFm.Id);
      end
    end;
    for i := 0 to IL.Count - 1 do
    begin
      n := IL[i];
      Fm := FormMan.FindForm(n);
      if Fm = nil then
      begin
        Fm := FormMan.LoadFromFile(TempDir + IntToStr(n) + '.frm');
        Cache.AddFormWithComponents(Fm);
        FormChanges.AddForm(Fm.Id, 0);
        Fm.Free;
      end
      else
      begin
        OldFm := Fm;
        Fm := FormMan.LoadFromFile(TempDir + IntToStr(n) + '.frm');
        if Fm.PId <> OldFm.PId then
        begin
          Cache.DeleteForm(OldFm);
          FormChanges.DeleteForm(OldFm.Id);
          Cache.AddFormWithComponents(Fm);
          FormChanges.AddForm(Fm.Id, 0);
        end
        else _CompareForms(Fm, OldFm);
        Fm.Free;
      end;
    end;

    finally
      SL.Free;
      IL.Free;
    end;
  end;

  procedure UpdateImages;
  var
    i: Integer;
  begin
    for i := 0 to FormMan.FormCount - 1 do
      UpdateImagesInForm(FormMan.Forms[i]);
  end;

var
  TempDir: String;
  Ver: Integer;
  ConnId, UId, i: Integer;
  TmpSt: TMemoryStream;
  OldUsers: TdxUserList;
  U, OldU: TdxUser;
begin
  if Cache.StructChanged then
  begin
    Info(rsSaveChangesBeforeImportMsg);
    Exit;
  end;
  TempDir := GetTempDir + 'dx' + IntToStr(Random(1000000)) + DirectorySeparator;
  TmpSt := TMemoryStream.Create;
  OldUsers := TdxUserList.Create;

  try try

  with TMyUnZipper.Create do
  try
    FileName:=aFileName;
    OutputPath:=TempDir;
    ForceDirectories(OutputPath);
    UnZipAllFiles;
    //RenameFileNamesCP866ToUtf8(Entries, OutputPath);
  finally
    Free;
  end;

  Ver := GetVersionFromFile(TempDir);

  if Ver < 31 then
  begin
    ErrMsg(rsDBNotSupport);
    DeleteDirectory(TempDir, False);
    Exit;
  end;

  FFormsTree.ClearAll;
  ResetDesigner;
  ResetTemplateFieldsForm;
  UndoMan.Clear;

  ConnId := UserMan.ConnId;
  UId := UserMan.CurrentUserId;

  UserMan.Users.SaveToStream(TmpSt);
  OldUsers.LoadFromStream(TmpSt);

  CompareFormMans(TempDir);
  FormMan.LoadFromDir(TempDir);
  FormMan.CorrectMaxTId;
  FormMan.CorrectMaxFId;
  ReportMan.LoadFromDir(TempDir);
  ReportMan.CorrectMaxId;
  UserMan.LoadFromDir(TempDir);
  ScriptMan.LoadFromDir(TempDir);
  ScriptMan.CompileAll;
  DXMain.LoadFromDir(TempDir);
  ImageMan.LoadFromDir(TempDir);
  ScaleForms(FormMan, DXMain.DesignTimePPI);
  ScaleReports(ReportMan, DXMain.DesignTimePPI, Screen.PixelsPerInch);

  UserMan.ConnId := ConnId;
  if UserMan.Users.FindUser(UId) <> nil then UserMan.CurrentUserId:=UId;
  if (UserMan.CurrentUser <> nil) and (UserMan.CurrentUser.RoleId >= 0) then
    UserMan.CurrentUser.WasDeveloper := True;
  // Восстанавливаем пароли старых пользователей
  for i := 0 to OldUsers.Count - 1 do
  begin
    OldU := OldUsers[i];
    U := UserMan.Users.FindUserByName(OldU.Name);
    if U <> nil then U.Password := OldU.Password;
  end;

  FFormsTree.BuildTree;
  CursorBn.Down := True;

  DeleteDirectory(TempDir, False);
  if AppConfig.ExpertMode and (ScriptFm <> nil) then
    ScriptFm.Reset(False);
  UpdateTemplateFieldsForm;
  UpdateImages;
  if FindActionsFm <> nil then FindActionsFm.Reset;
  if FindExprFm <> nil then FindExprFm.Reset;
  if FindScriptFm <> nil then FindScriptFm.Reset;
  MessageDlg(rsImportProject, rsImportPrjOk, mtInformation, [mbOk], 0);

  except
    on E: Exception do
      ErrMsg(rsImportProjectError + ExceptionToString(E, True, False));
  end;

  finally
    OldUsers.Free;
    TmpSt.Free;
  end;
end; *)

procedure TDesignFr.MergeProjects(const aFileName: String);
var
  S: String;
begin
  if ScriptFm <> nil then ScriptFm.SaveAll;
  FFormsTree.SaveTree;

  if ShowMergeProjectsForm(aFileName) = mrOk then
  begin
    FFormsTree.BuildTree;
    CursorBn.Down := True;
    if AppConfig.ExpertMode and (ScriptFm <> nil) then
      ScriptFm.Reset(True);
    UpdateTemplateFieldsForm;
    if not MergeProjectsFm.Failed then
    begin
      S := rsMergePrjOk;
      if MergeProjectsFm.OutputMsg <> '' then
        S := S + Spaces + rsMergePrjRenameMsg + Spaces + MergeProjectsFm.OutputMsg;
      MessageDlg(rsMergeProjects, S, mtInformation, [mbOk], 0);
      if MergeProjectsFm.ParseErrors <> '' then
        MessageDlg(rsImportProject, Format(rsParseExtModulesErrors, [Spaces +
          MergeProjectsFm.ParseErrors + LineEnding]), mtWarning, [mbOk], 0);
    end;
  end;
end;

procedure TDesignFr.SetExpertMode(Value: Boolean);
begin
  MainFm.ExpertMnu.Checked := Value;
  if Value then
  begin
    ScriptBn.Visible:=True;
    ScriptBn.Left := 32;
    //ReCreateScriptForm;
  end
  else
  begin
    if ScriptFm <> nil then
    begin
      ScriptFm.SaveAll;
      if ScriptFm.Visible then ScriptFm.Close;
      FreeAndNil(ScriptFm);
    end;
    ScriptBn.Visible := False;
  end;
  FFormsTree.ExpertMode:=Value;
end;

procedure TDesignFr.TestForm(const FormName: String);
var
  Fm: TdxForm;
begin
  //if FormDesign.StructChanged then
  if Cache.StructChanged then
  begin
    if Confirm(rsWarning, rsSaveChangesBeforeTestMsg) = mrYes then
    begin
      if not Save(False) then Exit;
    end
    else Exit;
  end;

	Fm := FormMan.FindFormByName(FormName);
  UpdateAnchoredComponents(Fm);
  if Fm.PId > 0 then
  	Fm := FormMan.FindForm(Fm.PId);

  if ScriptFm <> nil then ScriptFm.SaveAll;

  ScriptMan.CompileAll;

  if ScriptFm <> nil then ScriptFm.FillCompilerMessages;

  if ScriptMan.HasErrors then
  begin
    if AppConfig.ExpertMode then ShowScriptForm(nil)
    else ErrMsg(rsTestFormErrorFounds);
  	Exit;
  end;

  ExtRunMan := TExtRunManager.Create;
  ExtRunMan.Init;

  VarList := TVarList.Create;

  try
    Fm := CreateForm(Fm.FormCaption);
    MyTestForm := Fm;
    try try
      Fm.OpenRecord(0);
  	  Fm.Append;
	    TForm(Fm.EditWindow).ShowModal;
  	  Fm.Cancel;
    except
      on E: Exception do
        ErrMsg(ExceptionToString(E, False, False));
        {if ScriptLastError.ExObj = E then
          ErrMsg(ScriptLastErrorToString)
        else
      	  ErrMsg(E.Message); }
    end;
    finally
	    DestroyForm(Fm);
      FreeAndNil(VarList);
      FreeAndNil(ExtRunMan);
      FreeAndNil(DebugScriptFm);
      MyTestForm := nil;
      MainFm.ClearEventHandlers;
      MainFm.RestoreFormatSettings;
      DBase.ReadCommit;
      DBase.StartReadTrans;
    end;
  except
    on E: Exception do
    begin
      if ScriptLastError.ExObj = E then
        ErrMsg(ScriptLastErrorToString)
      else
      	ErrMsg(E.Message);
    end;
  end;
  // Если показывается окно отладки в Form_Create, то при закрытии окна формы
  // главное окно может потерять фокус и будет активно окно другого приложения.
  // Принудительно устанавливаем фокус на главном окне.
  Application.MainForm.SetFocus;
end;

procedure TDesignFr.ShowScriptForm(Fm: TdxForm);
begin
  if ScriptFm = nil then
  begin
    ScriptFm := TScriptFm.Create(Application);
    ScriptFm.Init;
    ScriptFm.RestoreSelection;
    if ScriptMan.HasErrors then ScriptFm.FillCompilerMessages;
  end;
  if (FCurForm = nil) or not TMyController(FormDesign.Controller).LeftButtonPressed then
    ScriptFm.ShowForm(Fm);
end;

{function GetMaxOldSize(Fm: TdxForm; StartIndex: Integer): Integer;
var
  i: Integer;
  DCI: TDesignCacheItem;
begin
  Result := 0;
  for i := StartIndex to Cache.Count - 1 do
  begin
    DCI := Cache[i];
    if (DCI.FmId = Fm.Id) and (DCI.Status = dstFieldSize) then
      if DCI.OldSize > Result then Result := DCI.OldSize;
  end;
end;  }

function TDesignFr.CheckBeforeChangeStruct: Boolean;
var
  i, RecSz, j: Integer;
  DCI: TDesignCacheItem;
  IL: TIntegerList;
  CL: TFormChangesList;
  FCI: TFormChangesItem;
  Fm: TdxForm;
  S: String;
begin
  Result := True;
  CL := FormChanges.Clone;
  IL := TIntegerList.Create;

  try

  // Собираем формы, которые были изменены.
  for i := 0 to Cache.Count - 1 do
  begin
    DCI := Cache[i];
    if not DCI.IsForm and (DCI.Status in [dstNew, dstDelete]) and
      not Cache.FormExists(DCI.FmId) and (IL.FindValue(DCI.FmId) < 0) then
      IL.AddValue(DCI.FmId)
    else if DCI.IsForm and (DCI.Status = dstNew) and
      (IL.FindValue(DCI.Id) < 0) then IL.AddValue(DCI.Id);
  end;

  // Проверка возможности добавить/удалить поля формы.
  for i := 0 to IL.Count - 1 do
  begin
    FCI := CL.FindForm(IL[i]);
    if FCI <> nil then
    begin
      Inc(FCI.Cnt);
      if FCI.Cnt > 255 then
      begin
        Fm := FormMan.FindForm(FCI.Id);
        ErrMsg(rsSaveProjectError + Spaces + Format(rsTableChangesLimitMsg,
          [Fm.FormCaption]) + Spaces + rsNotLoseChanges);
        Exit(False);
      end;
    end;
  end;

  // Добавляем к списку формы, где менялся размер поля
  for i := 0 to Cache.Count - 1 do
  begin
    DCI := Cache[i];
    if (DCI.Status = dstFieldSize) and (IL.FindValue(DCI.FmId) < 0) then
      IL.AddValue(DCI.FmId);
  end;

  // Проверяем возможность изменить размер поля и общий размер записи.
  for i := 0 to IL.Count - 1 do
  begin
    Fm := FormMan.FindForm(IL[i]);

    // Считаем размер записи до изменений размеров полей.
    RecSz := CalcRecordSize(Fm, True, False);
    if RecSz > 65535 then
    begin
      ErrMsg(rsSaveProjectError + Spaces + Format(rsLimit64KOldMsg, [Fm.FormCaption,
        RecSz, RecSz - 65535, (RecSz - 65535) / 4]) + Spaces +
        rsNotLoseChanges);
      Exit(False);
    end;

    // Считаем размер записи после изменений размеров полей.
    RecSz := CalcRecordSize(Fm, False, False);
    if RecSz > 65535 then
    begin
      ErrMsg(rsSaveProjectError + Spaces + Format(rsLimit64KMsg, [Fm.FormCaption,
        RecSz, RecSz - 65535, (RecSz - 65535) / 4]) + Spaces +
        rsNotLoseChanges);
      Exit(False);
    end;

    for j := 0 to Cache.Count - 1 do
    begin
      DCI := Cache[j];
      if (DCI.Status = dstFieldSize) and (DCI.FmId = Fm.Id) then
      begin
        FCI := CL.FindForm(Fm.Id);
        if FCI <> nil then
        begin
          Inc(FCI.Cnt, 2);
          // Достигнут предел изменений
          if FCI.Cnt > 255 then
          begin
            S := GetResizedFields(Fm, j);
            ErrMsg(rsCheckResizeFieldsError + AroundSpaces(S) + Format(rsTableChangesLimitMsg,
              [Fm.FormCaption]));
            Exit(False);
          end;
        end;
      end;
    end;
  end;

  if Cache.StructChanged then NeedAllCalcRecordSize := True;

  if NeedAllCalcRecordSize then
  begin
    for i := 0 to FormMan.FormCount - 1 do
    begin
      Fm := FormMan.Forms[i];
      // Считаем размер записи после изменений размеров полей с учетом объектов и полей объектов.
      RecSz := CalcRecordSize(Fm, False, True);
      if RecSz > 65535 then
      begin
        ErrMsg(rsSaveProjectError + Spaces + Format(rsLimit64KObjMsg, [Fm.FormCaption,
         RecSz, RecSz - 65535, (RecSz - 65535) / 4]) + Spaces +
         rsNotLoseChanges);
        Exit(False);
      end;
    end;
    // Сбрасываем флаг только после успешной проверки.
    NeedAllCalcRecordSize := False;
  end;

  finally
    IL.Free;
    CL.Free;
  end;
end;

procedure TDesignFr.ChangeStruct;

  procedure IncFormChangeCounts;
  var
    i: Integer;
    DCI: TDesignCacheItem;
    IL: TIntegerList;
    FCI: TFormChangesItem;
  begin
    IL := TIntegerList.Create;
    for i := 0 to Cache.Count - 1 do
    begin
      DCI := Cache[i];
      if (not DCI.IsForm) and (DCI.Status in [dstNew, dstDelete]) and
        not Cache.FormExists(DCI.FmId) then
      begin
        if IL.FindValue(DCI.FmId) < 0 then IL.AddValue(DCI.FmId);
      end
    end;
    for i := 0 to IL.Count - 1 do
    begin
      FCI := FormChanges.FindForm(IL[i]);
      Inc(FCI.Cnt);
    end;
    IL.Free;
  end;

var
  i: Integer;
  DCI: TDesignCacheItem;
  DelStr, AddStr: String;
begin
  DelStr := ''; AddStr := '';
  for i := 0 to Cache.Count - 1 do
  begin
    DCI := Cache[i];
    case DCI.Status of
      dstNew:
        if DCI.IsForm then AddStr := AddStr + SQLCreateTable(DCI.Id, DCI.PId)
        else AddStr := AddStr + SQLCreateField(DCI.Id, DCI.FmId, DCI.FieldSize, DCI.ClsName);
      dstDelete:
        if DCI.IsForm then DelStr := DelStr + SQLDeleteTable(DCI.Id, DCI.PId)
        else DelStr := DelStr + SQLDeleteField(DCI.Id, DCI.FmId, DCI.ClsName);
    end;
  end;
  if (AddStr <> '') or (DelStr <> '') then
  begin
    DBase.Execute(DelStr + AddStr);
    IncFormChangeCounts;
  end;
end;

procedure TDesignFr.InitFields;
var
  i: Integer;
  DCI: TDesignCacheItem;
  InitStr: String;
begin
  InitStr := '';
  for i := 0 to Cache.Count - 1 do
  begin
    DCI := Cache[i];
    case DCI.Status of
      dstNew:
        if not DCI.IsForm and DCI.InitField then
          InitStr := InitStr + SQLInitField(DCI.Id, DCI.FmId, DCI.ClsName);
      dstCounter:
        InitStr := InitStr + SQLSetCounter(DCI.Id, DCI.StartWith);
    end;
  end;
  if InitStr <> '' then
    DBase.Execute(InitStr);
end;

procedure TDesignFr.ChangeFieldsSize;
var
  FieldSizeExists: Boolean;

  procedure _Change(ReduceFirst: Boolean);
  var
    i: Integer;
    DCI: TDesignCacheItem;
    FCI: TFormChangesItem;
  begin
    i := 0;
    while i < Cache.Count do
    begin
      DCI := Cache[i];
      if (DCI.Status = dstFieldSize) and ( (ReduceFirst and (DCI.FieldSize < DCI.OldSize))
        or (not ReduceFirst and (DCI.FieldSize > DCI.OldSize)) ) then
      begin
        if not FieldSizeExists then
        begin
          DBase.Execute(CreateTempTable);
          FieldSizeExists := True;
        end;
        FCI := FormChanges.FindForm(DCI.FmId);
        DBase.Execute(SQLSetFieldSize(DCI.Id, DCI.FmId, DCI.FieldSize, DCI.ClsName));
        Inc(FCI.Cnt, 2);
        // Удаляем элемент из списка, чтобы проще было собрать список полей,
        // размер которых не удалось изменить в случае сбоя.
        DCI.Free;
        Cache.Remove(DCI);
      end
      else Inc(i);
    end;
  end;

begin
  FieldSizeExists := False;
  try
    _Change(True);
    _Change(False);
  finally
    if FieldSizeExists then DBase.Execute(DeleteTempTable);
  end;
end;

procedure CancelResizeFields(out Fields: String);
var
  i: Integer;
  DCI: TDesignCacheItem;
  C: TComponent;
  Fm: TdxForm;
  SL: TStringListUtf8;
begin
  SL := TStringListUtf8.Create;
  Fm := nil;
  for i := Cache.Count - 1 downto 0 do
  begin
    DCI := Cache[i];
    if DCI.Status = dstFieldSize then
    begin
      if (Fm = nil) or (Fm.Id <> DCI.FmId) then
        Fm := FormMan.FindForm(DCI.FmId);
      C := FindById(Fm, DCI.Id);
      SetFieldSize(C, DCI.OldSize);
      Fm.SetFormChanged;
      // Т. к. мы вернули исходный размер полей, удаляем изменения из кэша.
      {DCI.Free;
      Cache.Remove(DCI); }
      SL.Add(Fm.FormCaption + '->' + GetFieldName(C));
    end;
  end;
  SL.Sort;
  Fields := Trim(SL.Text);
  SL.Free;
end;

function TDesignFr.Save(ConfirmEmptyGroups: Boolean): Boolean;
var
  Gr, Fields: String;
begin
  Result := False;

  if not DXMain.CanProjectSave then Exit;
  if ConfirmEmptyGroups and (FormsTreeView.HasEmptyGroups(Gr)) then
    begin
			if Confirm(rsWarning, Format(rsEmptyGroupsNotSaved,
      	[LineEnding + LineEnding + Gr + LineEnding])) = mrNo then Exit;
    end;

  if not CheckBeforeChangeStruct then Exit;

  FFormsTree.SaveTree;

  if ScriptFm <> nil then ScriptFm.SaveAll;
  // В случае ошибок bin-ы не будут обновлены.
  {if AppConfig.Caching and ScriptMan.NeedCompile then
    ScriptMan.CompileAll;    }

  try
    ImageMan.SaveToDb;
    FormMan.SaveToDb;
    ReportMan.SaveToDB;
    UserMan.SaveToDb;
    //if ScriptFm <> nil then ScriptFm.SaveAll;
    ScriptMan.SaveToDB;
    DXMain.SaveToDb;
    ChangeStruct;
    DBase.Commit;

    // Т. к. флаг ставится при выборе формы, а при сохранении флаг сбрасывается,
    // ставим флаг на текущей выбранной форме
    if FCurForm <> nil then FCurForm.SetFormChanged;
  except
    on E: Exception do
    begin
      ErrMsg(rsSaveProjectError + ExceptionToString(E, True, True) + rsNotLoseChanges);
      Exit;
    end;
  end;

  try

    try
      InitFields;
    except
      on E: Exception do
      begin
        ErrMsg(rsInitFieldsError + ExceptionToString(E, True, False));
        CancelResizeFields(Fields);
        if Fields <> '' then
        begin
          ErrMsgFmt(rsResizeFieldsError, [ExceptionToString(E, True, True), Fields]);
          Info(rsSaveFormsAgain);
          SaveFormsAgain;
        end;
        Exit;
      end;
    end;

    try
      ChangeFieldsSize;
    except
      on E: Exception do
      begin
        CancelResizeFields(Fields);
        ErrMsgFmt(rsResizeFieldsError, [ExceptionToString(E, True, True), Fields]);
        Info(rsSaveFormsAgain);
        SaveFormsAgain;
        Exit;
      end;
    end;

  finally
    Cache.Clear;
    DXMain.SetLastModified;
    DBase.ReadCommit;
    DBase.StartReadTrans;
  end;

  Result := True;
end;

constructor TDesignFr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DesignFr := Self;
  ScrollBox1 := TDesignerBox.Create(Self);
  ScrollBox1.Parent := Self;
  ScrollBox1.Align := alClient;
  ScrollBox1.TabOrder := 2;
  DummyX.Parent := ScrollBox1;
  DummyY.Parent := ScrollBox1;
  FResizer := TFormResizer.Create;
  FormDesign := TFormDesigner.Create(Self);
  FormDesign.Parent := ScrollBox1;
  FormDesign.OnKeyDown:=@FormDesignKeyDown;
  Cache := TDesignCache.Create;
  FormChanges := TFormChangesList.Create;
  UndoMan := TUndoManager.Create;

  SaveBn.Hint := rsSaveProject;
  AddFmBn.Hint := rsAddForm;
  DelFmBn.Hint := rsDeleteForm;
  VsblFmBn.Hint := rsOrderVisibleForms;
  StyleBn.Hint := rsCopyFormStyle;
  CursorBn.Hint:=rsSelectComponents;
  LabelBn.Hint := rsLabel;
  TextBn.Hint := rsText;
  NumBn.Hint := rsNumber;
  DateBn.Hint := rsDate;
  MemoBn.Hint := rsMemo;
  CheckBoxBn.Hint := rsCheckBox;
  ListBn.Hint := rsList;
  ObjectBn.Hint := rsObject;
  TableBn.Hint := rsTable;
  GroupBn.Hint := rsGroup;
  PagesBn.Hint := rsPages;
  ShapeBn.Hint := rsShape;
  DBImageBn.Hint := rsImage;
  ImageBn.Hint := rsDsgnBackImage;
  FileBn.Hint := rsDsgnFile;
  QueryBn.Hint := rsQuery;
  ObjectFieldBn.Hint := rsObjField;
  TimeBn.Hint := rsTime;
  CounterBn.Hint := rsCounter;
  ButtonBn.Hint := rsButton;
  PivotBn.Hint := rsPivotTable;
  AddFieldsBn.Hint := rsAddFields;
  ScriptBn.Hint := rsScriptEditorBn;
  TestBn.Hint := rsTestForm;
  MainActBn.Hint := rsStartupActions;
  ChartBn.Hint := rsChart;
  KeyBn.Hint := rsRecordId;

  MenuItem3.Caption := rsMoveComponents;
  MenuItem4.Caption := rsCopy;
  MenuItem5.Caption := rsPaste;
  MenuItem7.Caption := rsDeleteComponents;
  MenuItem9.Caption := rsAlignment;
  MenuItem11.Caption := rsAlignmentLeft;
  MenuItem12.Caption := rsAlignmentRight;
  MenuItem13.Caption := rsAlignmentTop;
  MenuItem14.Caption := rsAlignmentBottom;
  MenuItem15.Caption := rsHorzCenter;
  MenuItem16.Caption := rsVertCenter;
  MenuItem10.Caption := rsSize;
  MenuItem17.Caption := rsMaxWidth;
  MenuItem18.Caption := rsMinWidth;
  MenuItem19.Caption := rsMaxHeight;
  MenuItem20.Caption := rsMinHeight;
  MenuItem22.Caption := rsBringToFront;
  MenuItem23.Caption := rsSendToBack;
  MenuItem25.Caption := rsTabOrder;
  MenuItem27.Caption := rsFont;
  MenuItem28.Caption := rsColor;
  MenuItem30.Caption := rsFindLost;
  MenuItem31.Caption := rsReplacedBy;
  MenuItem32.Caption := rsText;
  MenuItem33.Caption := rsMemo;
  MenuItem34.Caption := rsList;
  MenuItem37.Caption := rsAnchors;
  HideMnu.Caption := rsHideComponent;

  DateEditMnu := TDateEditMenu.Create(nil);
  //CalcEditMnu := TCalcEditMenu.Create(nil);
  FormMnu := TFormMenu.Create(nil);
  FormMoreMnu := TFormMoreMenu.Create(nil);
  TimeEditMnu := TTimeEditMenu.Create(nil);
  RequiredMnu := TRequiredMenu.Create(nil);
  CounterMnu := TCounterMenu.Create(nil);
  InsValMnu := TInsertValuesMenu.Create(nil);
  ExprMnu := TExprMenu.Create(nil);
  BtnGlyphMnu := TButtonGlyphMenu.Create(nil);
  QGridMoreMenu := TQueryGridMoreMenu.Create(nil);
  CalcEditMoreMnu := TCalcEditMoreMenu.Create(nil);

  TabSheet2.Caption := rsSummary;
  TabSheet3.Caption := rsComponents;

  FFormsTree := TFormsTree.Create(Self);
  with FFormsTree do
  begin
    Parent := Panel1;
    Align := alClient;
    OnSelectionChanged:=@FormsTreeSelectionChanged;
    OnCommand:=@FormsTreeCommand;
  end;

  FSummaryTree := TSummaryTree.Create(Self);
  with FSummaryTree do
  begin
    Parent := TabSheet2;
    Align := alClient;
    OnSelectComponent:=@SummaryTreeSelectComponent;
    OnEditComponent:=@SummaryTreeEditComponent;
  end;

  FCompTree := TComponentTree.Create(Self);
  with FCompTree do
  begin
    Parent := TabSheet3;
    Align := alClient;
    OnSelectComponent:=@ComponentTreeSelectComponent;
  end;

  Panel1.Width := AppConfig.LeftPanelWidth;
  PageControl1.Width := AppConfig.RightPanelWidth;

  SetupImageList(ToolbarImages, ['save24', 'script24', 'mainactions24', 'addform24',
    'deleteform24', 'test24', 'view24', 'style24', 'magic24', 'cursor24', 'label24',
    'text24', 'calc24', 'date24', 'clock24', 'memo24', 'checkbox24', 'combobox24',
    'object24', 'objectfield24', 'dbimage24', 'file24', 'key24', 'counter24',
    'grid24', 'query24', 'pivottable24', 'chart24', 'image24', 'shape24',
    'button24', 'groupbox24', 'tabs24']);
  SetupImageList(MenuImages, ['align_bottom16', 'align_vcenter16', 'align_left16',
    'align_hcenter16', 'align_right16', 'align_top16', 'copy16', 'toback16',
    'tofront16', 'paste16', 'delete16', 'anchors16', 'editbn16']);
end;

destructor TDesignFr.Destroy;
begin
  MainFm.SelectedFormId:=0;
  MainFm.DesignPageIndex:=0;
  if FCurForm <> nil then
  begin
    if FCurForm.PId > 0 then
      MainFm.SelectedFormId:=FCurForm.PId
    else
      MainFm.SelectedFormId:=FCurForm.Id;
    MainFm.DesignPageIndex:=PageControl1.ActivePageIndex;
  end;
  ResetDesigner;
  AppConfig.LeftPanelWidth:=ScaleTo96(Panel1.Width);
  AppConfig.RightPanelWidth:=ScaleTo96(PageControl1.Width);

  if FindActionsFm <> nil then FindActionsFm.Close;
  if FindExprFm <> nil then FindExprFm.Close;
  if ScriptFm <> nil then ScriptFm.Close;
  FreeAndNil(ScriptFm);

  HidePropsForm;
  CalcEditMoreMnu.Free;
  QGridMoreMenu.Free;
  BtnGlyphMnu.Free;
  ExprMnu.Free;
  InsValMnu.Free;
  CounterMnu.Free;
  RequiredMnu.Free;
  TimeEditMnu.Free;
  FormMoreMnu.Free;
  FormMnu.Free;
  DateEditMnu.Free;
  FResizer.Free;
  FreeAndNil(UndoMan);
  FreeAndNil(FormDesign);
  FreeAndNil(FormChanges);
  FreeAndNil(Cache);
  DesignFr := nil;
  inherited Destroy;
end;

procedure TDesignFr.Init;
var
  Fm: TdxForm;
begin
  FFormsTree.BuildTree;

  CursorBn.Down := True;
  FCurForm := nil;
  UpdateToolbarState;
  StatusBar.Panels[0].Text:=DBase.Database;
  SetExpertMode(AppConfig.ExpertMode);
  PageControl1.ActivePageIndex:=0;

  // Устанавливаем фокус на компоненте, иначе не работает перемещение по деревьям с
  // клавиатуры (связано с дизайнером)
  FFormsTree.SetFocus;
  //

  if MainFm.SelectedFormId > 0 then
  begin
    Fm := FormMan.FindForm(MainFm.SelectedFormId);
    FFormsTree.SelectForm(Fm);
    PageControl1.ActivePageIndex:=MainFm.DesignPageIndex;
  end;

  FormChanges.GetFormChanges;
  UpdateDesigner;
end;

procedure TDesignFr.Done;
begin
  (*// Обсолете
  if (ScriptFm <> nil) and (ScriptFm.Visible) then ScriptFm.Close;
  //
  MainFm.SelectedFormId:=0;
  MainFm.DesignPageIndex:=0;
  if FCurForm <> nil then
  begin
    if FCurForm.PId > 0 then
    	MainFm.SelectedFormId:=FCurForm.PId
    else
      MainFm.SelectedFormId:=FCurForm.Id;
    MainFm.DesignPageIndex:=PageControl1.ActivePageIndex;
  end;
  ResetDesigner;
  AppConfig.LeftPanelWidth:=ScaleTo96(Panel1.Width);
  AppConfig.RightPanelWidth:=ScaleTo96(PageControl1.Width); *)
end;

procedure TDesignFr.ShowForm(Id: Integer);
var
  Fm: TdxForm;
begin
  ResetDesigner;

  Fm := FormMan.FindForm(Id);
  Fm.Parent := ScrollBox1;
  Fm.Left := 10; Fm.Top := 10;
  Fm.Visible:=True;
  Fm.PopupMenu := PopupMenu1;
  FResizer.Bind(ScrollBox1, Fm);
  FResizer.OnFormResize:=@FormResize;
  FormDesign.DesignForm(Fm);
  FCurForm := Fm;
  HidePropsForm;
  //DummyY.AnchorToCompanion(akTop, 32, FCurForm);
  //DummyX.AnchorToCompanion(akLeft, 32, FCurForm);
  FSummaryTree.LoadTree(FCurForm);
  FCompTree.LoadTree(FCurForm);
  UpdateTreeStates;
  UpdateToolbarState;
  UpdateStatusBar;
  UndoMan.SelectCache(Fm);
  // Чтобы сильно не погружаться в дебри кода решил ставить флаг изменения формы,
  // когда пользователь выбирает ее в дизайнере. При большом количестве форм
  // вряд ли пользователь просматривает все формы. Даже в таком случае скорость
  // сохранения будет существенно выше, чем сохранение всех форм. Если же
  // ставить флаг, только при каком-либо изменении в форме или его компонентах,
  // это потребует вызывать метод SetFormChanged в сотне-другой местах кода.
  Fm.SetFormChanged;
end;

procedure TDesignFr.SaveFormsAgain;
begin
  try
    FormMan.SaveToDb;
    DBase.Commit;
    Info(rsSaveFormsOk);
  except
    on E: Exception do
      ErrMsg(rsSaveFormsError + ExceptionToString(E, True, True) + rsNotLoseChanges);
  end;
end;

function TDesignFr.CheckDeleteForm(Fm: TdxForm): Boolean;
var
  RptMsg, QryMsg: String;

  function CheckField(const Fl: TRpField): Boolean;
  begin
    Result := True;
    if Fl.TId = 0 then Exit;
    if Fl.TId = Fm.Id then
      Exit(False);
    if Fl.Src <> nil then
      Result := CheckField(Fl.Src^);
  end;

  function CheckFields(RD: TReportData; L: TRpFieldList): Boolean;
  var
    m: Integer;
    Fl: TRpField;
    Frm: TdxForm;
  begin
    Result := True;
    for m := 0 to L.Count - 1 do
    begin
      Fl := L[m]^;
      if not CheckField(Fl) then
      begin
        if RD.Kind = rkReport then
          RptMsg := RptMsg + '    ' + RD.Name + LineEnding
        else if RD.Kind = rkQuery then
        begin
          Frm := FindFormByRDId(RD.Id);
          QryMsg := QryMsg + Format('    %s (%s)', [RD.Name, Frm.FormCaption]) + LineEnding;
        end;
        Exit(False);
      end;
    end;
  end;

var
  j, z: Integer;
  RD: TReportData;
  Sc: TRpSource;
  Frm: TdxForm;
begin
  for j := 0 to FormMan.FormCount - 1 do
  begin
    Frm := FormMan.Forms[j];
    if CheckExistsInActions(Frm, renForm, Fm.FormCaption) then Exit(False);
  end;

  RptMsg := ''; QryMsg := '';
  for j := 0 to ReportMan.ReportCount - 1 do
  begin
    RD := ReportMan.Reports[j];
    for z := 0 to RD.Sources.Count - 1 do
    begin
      Sc := RD.Sources[z]^;

      if (Sc.Id = Fm.Id) or (Sc.TId = Fm.Id) then
      begin
        if RD.Kind = rkReport then
          RptMsg := RptMsg + '    ' + RD.Name + LineEnding
        else
        begin
          Frm := FindFormByRDId(RD.Id);
          QryMsg := QryMsg + Format('    %s (%s)', [RD.Name, Frm.FormCaption]) + LineEnding;
        end;
        Break;
      end;

      if not CheckFields(RD, Sc.Fields) then Break;
    end;
  end;
  Result := (RptMsg = '') and (QryMsg = '');

  if not Result then
  begin
    if RptMsg <> '' then RptMsg := Format(rsCantDelFormMsgRpt, [LineEnding + SortStr(RptMsg)]);
    if QryMsg <> '' then QryMsg := Format(rsCantDelFormMsgQry, [LineEnding + SortStr(QryMsg)]);
    ErrMsgFmt(rsCantDelFormMsg, [Fm.FormCaption, RptMsg + QryMsg]);
  end;
end;

end.

