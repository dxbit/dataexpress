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
unit DesignerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls,
  Menus, Graphics, strconsts, dxctrls, formresizer, DXReports, LclType,
  summarytree, componenttree, Dialogs, IBConnection;

type

  { TDesignFr }

  TDesignFr = class(TFrame)
    DummyX: TLabel;
    FormList: TListBox;
    ImageList1: TImageList;
    ImageList2: TImageList;
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
    MenuItem2: TMenuItem;
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
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
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
    FormListMenu: TPopupMenu;
    PopupMenu1: TPopupMenu;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    SaveBn: TToolButton;
    AddFieldsBn: TToolButton;
    ToolButton17: TToolButton;
    AddFmBn: TToolButton;
    DelFmBn: TToolButton;
    StyleBn: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    TestBn: TToolButton;
    VsblFmBn: TToolButton;
    ToolButton3: TToolButton;
    CursorBn: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure AddFieldsBnClick(Sender: TObject);
    procedure AddFmBnClick(Sender: TObject);
    procedure DelFmBnClick(Sender: TObject);
    procedure DesignMnuClick(Sender: TObject);
    procedure FormDesignKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormListSelectionChange(Sender: TObject; User: boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem30Click(Sender: TObject);
    procedure MenuItem36Click(Sender: TObject);
    procedure MenuItem37Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ReplaceMnuClick(Sender: TObject);
    procedure SaveBnClick(Sender: TObject);
    procedure ScrollBox1Click(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure StyleBnClick(Sender: TObject);
    procedure TestBnClick(Sender: TObject);
    procedure ToolButton24Click(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure VsblFmBnClick(Sender: TObject);
  private
    { private declarations }
    FCurForm: TdxForm;
    FResizer: TFormResizer;
    FSummaryTree: TSummaryTree;
    FCompTree: TComponentTree;
    procedure ComponentTreeSelectComponent(Sender: TObject; Cmp: TControl);
    procedure SummaryTreeEditComponent(Sender: TObject; Cmp: TComponent;
      EditProp: TEditProp);
    procedure UpdateFormListMenuState;
    procedure UpdateDesignMenuState;
    procedure UpdateToolbarState;
    procedure UpdateTreeStates;
    //procedure ShowForm;
    //procedure HideForm;
    function CompareForms: String;
    //function CheckDeleteComponents: Boolean;
    //function CheckDeleteForm(Fm: TdxForm): Boolean;
    procedure CheckAllComponentNames;
    procedure SummaryTreeSelectComponent(Sender: TObject; Cmp: TComponent);
    procedure SelectControl(C: TControl);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure Done;
    procedure ShowForm(Id: Integer);
    procedure SelectForm(Id: Integer);
    procedure Save;
    procedure UpdateStatusBar;
    procedure UpdateSelectComponent;
    procedure UpdateSummaryTree;
    procedure UpdateComponentTree;
    procedure AddChildForm(PFm, Fm: TdxForm);
    procedure ExportProject(const aFileName: String);
    procedure ImportProject(const aFileName: String);
    procedure MergeProjects(const aFileName: String);
    procedure SetExpertMode(Value: Boolean);
    procedure TestForm(const FormName: String);
    procedure ShowScriptForm;
    procedure ReCreateScriptForm;
  end;

var
  DesignFr: TDesignFr;

implementation

uses
  formmanager, apputils, sqlgen, dbengine, formdesigner, propsform,
  taborderform, addfieldsform, reportmanager, Visibleformsform, fontform,
  dximages, colorform, styleform, propsmenus, jvdesignutils, dxusers,
  zipper, mainform, mergeprojectsform, scriptform, scriptmanager, appsettings,
  scriptfuncs, exprfuncs, anchorsform, propdialogs, debugscriptform,
  breakpointsform;

{$R *.lfm}

{ TDesignFr }

procedure TDesignFr.SummaryTreeSelectComponent(Sender: TObject; Cmp: TComponent);
begin
  if not (Cmp is TControl) then Exit;
  SelectControl(TControl(Cmp));
end;

procedure TDesignFr.UpdateSummaryTree;
begin
  FSummaryTree.UpdateTree;
end;

procedure TDesignFr.UpdateComponentTree;
begin
  FCompTree.UpdateTree;
end;

procedure TDesignFr.SelectControl(C: TControl);
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
  FormDesign.Select(C);
  UpdateStatusBar;
end;

procedure TDesignFr.FormListSelectionChange(Sender: TObject; User: boolean);
begin
  if FormList.ItemIndex < 0 then Exit;
  ShowForm(TdxForm(FormList.Items.Objects[FormList.ItemIndex]).Id);
  UpdateFormListMenuState;
  UpdateToolbarState;
  UpdateStatusBar;
end;

procedure TDesignFr.CheckAllComponentNames;
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
end;

procedure TDesignFr.ReCreateScriptForm;
begin
  FreeAndNil(ScriptFm);
  ScriptFm := TScriptFm.Create(nil);
  ScriptFm.Init;
end;

procedure TDesignFr.DesignMnuClick(Sender: TObject);
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
        FormDesign.ClearSelection;
        TabOrderFm.ShowForm(FormDesign.Form);
      end;
  end;
end;

procedure TDesignFr.FormDesignKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F4: if Shift = [] then ShowScriptForm;
    VK_F5: MenuItem36.Click;
    VK_F11: MainFm.Timer1.Enabled := True;
    VK_F12: MainFm.MenuItem12.Click;
  end;
end;

procedure TDesignFr.AddFieldsBnClick(Sender: TObject);
begin
  if FormDesign.Active then
    if AddFieldsFm.ShowForm(FormDesign.Form) = mrOk then
      HidePropsForm;
end;

procedure TDesignFr.AddFmBnClick(Sender: TObject);
begin
  MenuItem1.Click;
end;

procedure TDesignFr.DelFmBnClick(Sender: TObject);
begin
  MenuItem2.Click;
end;

procedure TDesignFr.MenuItem1Click(Sender: TObject);
var
  Fm: TdxForm;
begin
  Fm := FormMan.CreateNewForm;
  with FormList do
  begin
    Items.AddObject(Fm.FormCaption, Fm);
    ItemIndex := Count - 1;
  end;
  UpdateFormListMenuState;
end;

procedure TDesignFr.MenuItem27Click(Sender: TObject);
var
  C: TControl;
  Fnt: TFont;
  i: Integer;
begin
  Fnt := nil;
  for i := 0 to Length(FormDesign.Selected) - 1 do
  begin
    C := FormDesign.Selection[i];
    if (C is TdxImage) or (C is TdxDBImage) or (C is TdxShape) or (C is TdxGrid) then
      Continue;
    Fnt := C.Font;
    Break;
  end;
  if Fnt = nil then Exit;
  if FontFm.ShowForm(Fnt) <> mrOk then Exit;
  for i := 0 to Length(FormDesign.Selected) - 1 do
  begin
    C := FormDesign.Selection[i];
    if (C is TdxImage) or (C is TdxDBImage) or (C is TdxShape) or (C is TdxGrid) then
      Continue;
    C.Font := Fnt;
    SetParentFont(C, FontFm.IsDefault or C.Font.IsEqual(C.Parent.Font));
  end;
end;

procedure TDesignFr.MenuItem28Click(Sender: TObject);
var
  i: Integer;
  C, CC: TControl;
begin
  CC := nil;
  for i := 0 to Length(FormDesign.Selected) - 1 do
  begin
    C := FormDesign.Selection[i];
    if (C is TdxImage) or (C is TdxDBImage) or (C is TdxShape) or (C is TdxGrid)
      or (C is TdxPageControl) or (C is TdxTabSheet) or (C is TdxCheckBox) then Continue;
    CC := C;
    Break;
  end;
  if CC = nil then Exit;
  if ColorFm.ShowForm(CC) <> mrOk then Exit;
  for i := 0 to Length(FormDesign.Selected) - 1 do
  begin
    C := FormDesign.Selection[i];
    if (C is TdxImage) or (C is TdxDBImage) or (C is TdxShape) or (C is TdxGrid)
      or (C is TdxPageControl) or (C is TdxTabSheet) or (C is TdxCheckBox) then Continue;
    C.Color := CC.Color;
  end;
end;

procedure DeleteAllDetailForms(Fm: TdxForm);
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxGrid then
      with TdxGrid(C) do
        //if Id > 0 then
          FormMan.DeleteForm(Id);
  end;
end;

procedure TDesignFr.MenuItem2Click(Sender: TObject);
var
  i, FmId: Integer;
  Fm: TdxForm;
  G: TdxGrid;
begin
  if (not ConfirmDelete) or (not CheckDeleteForm(FCurForm)) then Exit;
  //FSummaryTree.Form := nil;
  FmId := FCurForm.Id;
  if FCurForm.PId = 0 then
  begin
    // удаление дочерних форм из списка
    i := FormList.ItemIndex + 1;
    while i < FormList.Count do
      with TdxForm(FormList.Items.Objects[i]) do
        if PId = FCurForm.Id then FormList.Items.Delete(i)
        else Break;
  end
  else
  begin
    Fm := FormMan.FindForm(FCurForm.PId);
    G := FindGridById(Fm, FCurForm.Id);
    G.Free;
  end;
  FormList.Items.Delete(FormList.ItemIndex);
  FormDesign.DesignForm(nil);
  FResizer.UnBind;
  DeleteQueries(FCurForm);
  DeleteReferences(FCurForm);
  DeleteAllDetailForms(FCurForm);
  FormMan.DeleteForm(FCurForm.Id);
  FormDesign.DesignForm(nil);
  FResizer.UnBind;
  FCurForm := nil;
  UpdateFormListMenuState;
  UpdateToolbarState;
  UpdateStatusBar;
  FCompTree.ClearAll;
  FSummaryTree.ClearAll;
  UpdateTreeStates;
  HidePropsForm;
  if ScriptFm <> nil then ScriptFm.DeleteForm(FmId);
end;

procedure TDesignFr.MenuItem30Click(Sender: TObject);
begin
  FormDesign.FindLost;
end;

procedure TDesignFr.MenuItem36Click(Sender: TObject);
var
  Fm: TdxForm;
begin
  Fm := FCurForm;
  if Fm = nil then Exit;
  if Fm.PId > 0 then Fm := FormMan.FindForm(Fm.PId);
	TestForm(Fm.FormCaption);
end;

procedure TDesignFr.MenuItem37Click(Sender: TObject);
var
  CL: TList;
  i: Integer;
begin
  CL := TList.Create;
  for i := 0 to Length(FormDesign.Selected) - 1 do
    CL.Add(FormDesign.Selection[i]);
  AnchorsFm.ShowForm(CL);
  CL.Free;
end;

procedure TDesignFr.PageControl1Change(Sender: TObject);
begin
  UpdateTreeStates;
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
begin
  i := TMenuItem(Sender).Tag;
  C := FormDesign.Control;
  FormDesign.ClearSelection;
  Fm := FormDesign.Form;
  case i of
    1: NewC := TdxEdit.Create(Fm);
    2: NewC := TdxMemo.Create(Fm);
    3: NewC := TdxComboBox.Create(Fm);
  end;
  NewC.Name := DesignUniqueName(Fm, NewC.ClassName);
  NewC.Parent := C.Parent;
  NewC.BoundsRect := C.BoundsRect;
  SetId(NewC, GetId(C));
  SetFieldName(NewC, GetFieldName(C));
  NewC.Font := C.Font;
  NewC.Color := C.Color;

  C.Free;
  FormDesign.Messenger.DesignComponent(NewC, True);
  FormDesign.Selector.AddToSelection(NewC);
end;

procedure TDesignFr.SaveBnClick(Sender: TObject);
begin
  Save;
end;

procedure TDesignFr.ScrollBox1Click(Sender: TObject);
begin
  HidePropsForm;
end;

procedure TDesignFr.ScrollBox1Resize(Sender: TObject);
begin
  if FormDesign.Active then
    FormDesign.UpdateDesigner;
end;

procedure TDesignFr.StyleBnClick(Sender: TObject);
begin
  StyleFm.ShowForm(FCurForm, FormList.Items);
end;

procedure TDesignFr.TestBnClick(Sender: TObject);
begin
  TestForm(FCurForm.FormCaption);
end;

procedure TDesignFr.ToolButton24Click(Sender: TObject);
begin
  ShowScriptForm;
end;

procedure TDesignFr.ToolButtonClick(Sender: TObject);
const
  Comps: array [0..21] of String = ('', 'TdxLabel', 'TdxEdit', 'TdxCalcEdit',
    'TdxDateEdit', 'TdxMemo', 'TdxCheckBox', 'TdxComboBox', 'TdxLookupComboBox',
    'TdxGrid', 'TdxGroupBox', 'TdxPageControl', 'TdxShape', 'TdxDBImage',
    'TdxImage', 'TdxFile', 'TdxQueryGrid', 'TdxObjectField', 'TdxTimeEdit',
    'TdxCounter', 'TdxButton', 'TdxPivotGrid');
begin
  FormDesign.ControlClass := Comps[ TComponent(Sender).Tag ];
  HidePropsForm;
end;

procedure TDesignFr.VsblFmBnClick(Sender: TObject);
begin
  VisibleFormsFm.ShowForm;
end;

procedure TDesignFr.UpdateFormListMenuState;
begin
  MenuItem2.Enabled := FCurForm <> nil;
  MenuItem36.Enabled := FCurForm <> nil;
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

procedure TDesignFr.ComponentTreeSelectComponent(Sender: TObject; Cmp: TControl
  );
begin
  if Cmp <> FCurForm then
	  SelectControl(Cmp);
end;

procedure TDesignFr.UpdateDesignMenuState;
var
  C: TControl;
begin
  MenuItem3.Enabled:=FormDesign.Count > 0;
  MenuItem4.Enabled := FormDesign.Count > 0;
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
  MenuItem27.Enabled := FormDesign.Count > 0;
  MenuItem28.Enabled := FormDesign.Count > 0;
  C := FormDesign.Control;
  MenuItem32.Enabled:=(FormDesign.Count = 1) and ((C is TdxMemo) or (C is TdxComboBox));
  MenuItem33.Enabled:=(FormDesign.Count = 1) and ((C is TdxEdit) or (C is TdxComboBox));
  MenuItem34.Enabled:=(FormDesign.Count = 1) and ((C is TdxMemo) or (C is TdxEdit));
  MenuItem37.Enabled := (FormDesign.Count > 0) and HasAnchors(FormDesign.Control);
end;

procedure TDesignFr.UpdateToolbarState;
begin
  if FormDesign.Active then
  begin
    ToolButton13.Enabled := FormDesign.Form.PId = 0;
    //ToolButton18.Enabled := FormDesign.Form.PId = 0;
  end;
  DelFmBn.Enabled := FCurForm <> nil;
  TestBn.Enabled := FCurForm <> nil;
  VsblFmBn.Enabled := FormList.Count > 0;
  StyleBn.Enabled := FCurForm <> nil;
end;

procedure TDesignFr.UpdateTreeStates;
begin
  case PageControl1.ActivePageIndex of
    1: FCompTree.Enabled:=FCurForm <> nil;
    2: FSummaryTree.Enabled:=FCurForm <> nil;
  end;
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

procedure TDesignFr.UpdateSelectComponent;
var
  C: TControl;
  L: Integer;
begin
  L := Length(FormDesign.Selected);
  if L = 0 then
  	C := FormDesign.Form
  else if L = 1 then
  	C := FormDesign.Control
  else Exit;

  FCompTree.SelectComponent(C);
end;

procedure TDesignFr.AddChildForm(PFm, Fm: TdxForm);
var
  n, i: Integer;
  F: TdxForm;
begin
  n := FormList.Items.IndexOfObject(PFm);
  for i := n + 1 to FormList.Count - 1 do
  begin
    F := TdxForm(FormList.Items.Objects[i]);
    if F.PId <> Fm.PId then
    begin
      FormList.Items.InsertObject(i, '    ' + Fm.FormCaption, Fm);
      Exit;
    end;
  end;
  FormList.Items.AddObject('    ' + Fm.FormCaption, Fm);
end;

procedure TDesignFr.ExportProject(const aFileName: String);
var
  TempDir: String;
  SL: TStringList;
  i: Integer;
begin
  TempDir := GetTempDir + 'dx' + IntToStr(Random(1000000)) + DirectorySeparator;
  CreateDir(TempDir);
  FormMan.SaveToDir(TempDir);
  ReportMan.SaveToDir(TempDir);
  UserMan.SaveToDir(TempDir);
  ScriptMan.SaveToDir(TempDir);
  SL := FindAllFiles(TempDir, '*');
  with TZipper.Create do
  try
    FileName:=aFileName;
    for i := 0 to SL.Count - 1 do
      Entries.AddFileEntry(SL[i], ExtractFileName(SL[i]));
    ZipAllFiles;
  finally
    Free;
    SL.Free;
  end;
end;

procedure TDesignFr.ImportProject(const aFileName: String);
var
  TempDir: String;
begin
  FormDesign.DesignForm(nil);
  FResizer.UnBind;
  HidePropsForm;
  FCurForm := nil;
  UpdateFormListMenuState;
  UpdateToolbarState;
  UpdateStatusBar;
  FormList.Clear;

  TempDir := GetTempDir + 'dx' + IntToStr(Random(1000000)) + DirectorySeparator;

  with TUnZipper.Create do
  try
    FileName:=aFileName;
    OutputPath:=TempDir;
    UnZipAllFiles;
  finally
    Free;
  end;

  FormMan.LoadFromDir(TempDir);
  FormMan.CorrectMaxTId;
  FormMan.CorrectMaxFId;
  ReportMan.LoadFromDir(TempDir);
  ReportMan.CurrentMaxId;
  UserMan.LoadFromDir(TempDir);
  ScriptMan.LoadFromDir(TempDir);

  FormMan.AllFormsToList(FormList.Items);
  CursorBn.Down := True;

  DeleteDirectory(TempDir, False);
  if AppConfig.ExpertMode then
    ReCreateScriptForm;
  FCompTree.ClearAll;
  FSummaryTree.ClearAll;
  UpdateTreeStates;
end;

procedure TDesignFr.MergeProjects(const aFileName: String);
begin
  if ScriptFm <> nil then ScriptFm.SaveAll;
  if MergeProjectsFm.ShowForm(aFileName) = mrOk then
  begin
    FormMan.AllFormsToList(FormList.Items);
    CursorBn.Down := True;
    if AppConfig.ExpertMode then
      ReCreateScriptForm;
    with FormList do
    	ItemIndex := Items.IndexOfObject(FCurForm);
  end;
end;

procedure TDesignFr.SetExpertMode(Value: Boolean);
begin
  MainFm.MenuItem29.Checked := Value;
  if Value then
  begin
    ToolButton24.Visible:=True;
    ToolButton24.Left := 32;
    ReCreateScriptForm;
  end
  else
  begin
    if ScriptFm <> nil then
    begin
      ScriptFm.SaveAll;
      if ScriptFm.Visible then ScriptFm.Close;
      FreeAndNil(ScriptFm);
    end;
    ToolButton24.Visible := False;
  end;
end;

procedure TDesignFr.TestForm(const FormName: String);
var
  Fm: TdxForm;
begin
  Fm := FormMan.FindFormByName(FormName);
  if Fm.PId > 0 then
  	Fm := FormMan.FindForm(Fm.PId);

  if ScriptFm <> nil then ScriptFm.SaveAll;

  ScriptMan.CompileAll;
  ExprModule := TRunScript.Create;
  ExprModule.Exec.OnException:=nil;
  ExprModule.SD := ScriptMan.ExprModule;
  ExprModule.LoadBin;
  ExprModule.BindVars;
  VarList := TVarList.Create;

  try
    Fm := CreateForm(Fm.FormCaption);
    try try
      Fm.OpenRecord(0);
  	  Fm.Append;
	    TForm(Fm.EditWindow).ShowModal;
  	  Fm.Cancel;
    except
      on E: Exception do
    	  ErrMsg(E.Message);
    end;
    finally
	    DestroyForm(Fm);
      FreeAndNil(VarList);
      FreeAndNil(ExprModule);
      DBase.ReadCommit;
      FreeAndNil(DebugScriptFm);
    end;
  except
    on E: Exception do
    	ErrMsg(E.Message);
  end;
end;

procedure TDesignFr.ShowScriptForm;
begin
  if ScriptFm <> nil then ScriptFm.ShowForm;
end;

function TDesignFr.CompareForms: String;
var
  FMan: TFormManager;
  i: Integer;
  Fm: TdxForm;
begin
  Result := '';
  FMan := TFormManager.Create;
  try

  FMan.LoadFromDb;
  for i := FMan.FormCount - 1 downto 0 do
    if FormMan.FindForm(FMan.Forms[i].Id) = nil then
      Result := Result + SQLDeleteTable(FMan.Forms[i]);
  for i := 0 to FormMan.FormCount - 1 do
    if FMan.FindForm(FormMan.Forms[i].Id) = nil then
      Result := Result + SQLCreateTable(FormMan.Forms[i]);
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FMan.FindForm(FormMan.Forms[i].Id);
    if Fm = nil then Continue;
    Result := Result + SQLCompareTables(FormMan.Forms[i], Fm);
  end;

  finally
    FMan.Free;
  end;
end;

procedure TDesignFr.Save;
var
  SQL: String;
begin
  DBase.ReadCommit;
  CheckAllComponentNames;

  SQL := CompareForms;
  try
    if SQL <> '' then
       DBase.Execute(SQL);
  except
    on E: EIBDatabaseError do
      if E.ErrorCode = 335544351 then
      begin
        ErrMsg(rsLimit64KMsg);
        Exit;
      end;
  end;
  FormMan.SaveToDb;
  ReportMan.SaveToDB;
  UserMan.SaveToDb;
  if ScriptFm <> nil then ScriptFm.SaveAll;
  ScriptMan.SaveToDB;
end;

constructor TDesignFr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DesignFr := Self;
  FResizer := TFormResizer.Create;
  FormDesign := TFormDesigner.Create(Self);
  FormDesign.Parent := ScrollBox1;
  FormDesign.OnKeyDown:=@FormDesignKeyDown;
  FormMan := TFormManager.Create;

  SaveBn.Hint := rsSave;
  AddFmBn.Hint := rsNewForm;
  DelFmBn.Hint := rsDelete;
  VsblFmBn.Hint := rsVisibleForms;
  StyleBn.Hint := rsCopyStyle;
  CursorBn.Hint:=rsSelectComponents;
  ToolButton5.Hint := rsLabel;
  ToolButton6.Hint := rsText;
  ToolButton7.Hint := rsNumber;
  ToolButton8.Hint := rsDate;
  ToolButton9.Hint := rsMemo;
  ToolButton10.Hint := rsCheckBox;
  ToolButton11.Hint := rsList;
  ToolButton12.Hint := rsObject;
  ToolButton13.Hint := rsTable;
  ToolButton14.Hint := rsGroup;
  ToolButton15.Hint := rsPages;
  ToolButton16.Hint := rsShape;
  ToolButton1.Hint := rsImage;
  ToolButton2.Hint := rsDsgnBackImage;
  Toolbutton4.Hint := rsDsgnFile;
  ToolButton18.Hint := rsQuery;
  ToolButton19.Hint := rsObjField;
  ToolButton20.Hint := rsTime;
  ToolButton21.Hint := rsCounter;
  ToolButton22.Hint := rsButton;
  ToolButton23.Hint := rsPivotTable;
  AddFieldsBn.Hint := rsAddFields;
  ToolButton24.Hint := rsScriptEditorBn;
  TestBn.Hint := rsTestForm;

  MenuItem1.Caption := rsNewForm;
  SetMenuItemImage(MenuItem1, 'add16');
  MenuItem2.Caption := rsDelete;
  SetMenuItemImage(MenuItem2, 'delete16');
  MenuItem36.Caption := rsTest;
  SetMenuItemImage(MenuItem36, 'test16');

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

  DateEditMnu := TDateEditMenu.Create(nil);
  CalcEditMnu := TCalcEditMenu.Create(nil);
  FormMnu := TFormMenu.Create(nil);
  FormMoreMnu := TFormMoreMenu.Create(nil);
  TimeEditMnu := TTimeEditMenu.Create(nil);
  RequiredMnu := TRequiredMenu.Create(nil);
  CounterMnu := TCounterMenu.Create(nil);
  InsValMnu := TInsertValuesMenu.Create(nil);
  ExprMnu := TExprMenu.Create(nil);
  BtnGlyphMnu := TButtonGlyphMenu.Create(nil);
  //ShowSBMnu := TShowScrollBarsMenu.Create(nil);

  TabSheet1.Caption := rsForms;
  TabSheet2.Caption := rsSummary;
  TabSheet3.Caption := rsComponents;

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
end;

destructor TDesignFr.Destroy;
begin
  FreeAndNil(ScriptFm);
  HidePropsForm;
  //ShowSBMnu.Free;
  BtnGlyphMnu.Free;
  ExprMnu.Free;
  InsValMnu.Free;
  CounterMnu.Free;
  RequiredMnu.Free;
  TimeEditMnu.Free;
  FormMoreMnu.Free;
  FormMnu.Free;
  CalcEditMnu.Free;
  DateEditMnu.Free;
  FResizer.Free;
  FreeAndNil(FormDesign);
  FreeAndNil(FormMan);
  DesignFr := nil;
  inherited Destroy;
end;

procedure TDesignFr.Init;
begin
  ReportMan.LoadFromDB;
  FormMan.LoadFromDb;
  FormMan.AllFormsToList(FormList.Items);
  ScriptMan.LoadFromDB;
  //AddScripts;
  CursorBn.Down := True;
  FormList.SetFocus;
  FCurForm := nil;
  UpdateFormListMenuState;
  UpdateToolbarState;
  StatusBar.Panels[0].Text:=DBase.Database;
  SetExpertMode(AppConfig.ExpertMode);
  PageControl1.ActivePageIndex:=0;
  {if AppConfig.ExpertMode then
    ReCreateScriptForm;  }
end;

procedure TDesignFr.Done;
begin
  if (ScriptFm <> nil) and (ScriptFm.Visible) then ScriptFm.Close;
	FreeAndNil(BreakpointsFm);
end;

procedure TDesignFr.ShowForm(Id: Integer);
var
  Fm: TdxForm;
begin
  FormDesign.DesignForm(nil);
  if FCurForm <> nil then
  begin
    FCurForm.Parent := nil;
    FCurForm.Visible:=False;
    FCurForm.PopupMenu := nil;
    FResizer.UnBind;
  end;
  Fm := FormMan.FindForm(Id);
  Fm.Parent := ScrollBox1;
  Fm.Left := 10; Fm.Top := 10;
  Fm.Visible:=True;
  Fm.PopupMenu := PopupMenu1;
  FResizer.Bind(ScrollBox1, Fm);
  FormDesign.DesignForm(Fm);
  UpdateFormListMenuState;
  UpdateToolbarState;
  FCurForm := Fm;
  HidePropsForm;
  DummyY.AnchorToCompanion(akTop, 32, FCurForm);
  DummyX.AnchorToCompanion(akLeft, 32, FCurForm);
  FSummaryTree.LoadTree(FCurForm);
  FCompTree.LoadTree(FCurForm);
end;

// Для грида
procedure TDesignFr.SelectForm(Id: Integer);
begin
  with FormList do
    ItemIndex :=
      Items.IndexOfObject(FormMan.FindForm(Id));
end;

end.

