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

unit TemplateFieldsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls, ExtCtrls, ButtonPanel, Menus, TreeFilterEdit, dxctrls, dxreports,
  strconsts, TreeViewEx, {$IFDEF WINDOWS}Windows, ActiveX, ComObj, {$ENDIF}LCLType;

type

  { TOleDropSource }

  {$IFDEF WINDOWS}
  TOleDropSource = class(TInterfacedObject, IDropSource)
  public
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: DWORD):HResult;StdCall;
    function GiveFeedback(dwEffect: DWORD): HResult;StdCall;
  end;
  {$ELSE}
  // Заглушка
  TOleDropSource = class

  end;
  {$ENDIF}

  { TTemplateFieldsFm }

  TTemplateFieldsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Fields: TTreeViewEx;
    Msg: TLabel;
    MsgPan: TPanel;
    RpFields: TTreeViewEx;
    RpFilter: TTreeFilterEdit;
    FormCbx: TComboBox;
    FieldsImages: TImageList;
    RpCbx: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MenuItem5: TMenuItem;
    Pg: TPageControl;
    Panel3: TPanel;
    Panel4: TPanel;
    RpSources: TTreeViewEx;
    Sources: TTreeViewEx;
    Splitter2: TSplitter;
    SrcImages: TImageList;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Timer1: TTimer;
    ToolButton3: TToolButton;
    ToolImages: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    Filter: TTreeFilterEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FieldsCompare(Sender: TObject; Node1, Node2: TTreeNode;
      var Compare: Integer);
    procedure FieldsDblClick(Sender: TObject);
    procedure FieldsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FieldsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure FieldsSelectionChanged(Sender: TObject);
    procedure FieldsUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormCbxSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure PgChange(Sender: TObject);
    procedure RpCbxSelect(Sender: TObject);
    procedure RpFieldsUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure RpSourcesSelectionChanged(Sender: TObject);
    procedure SourcesCompare(Sender: TObject; Node1, Node2: TTreeNode;
      var Compare: Integer);
    procedure SourcesSelectionChanged(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    FOleDropSource: TOleDropSource;
    FOldX, FOldY: Integer;
    FCurFormId, FCurRpId: Integer;
    FPop: TPopupMenu;
    FRpForm: TdxForm;
    FShowAgain: Boolean;
    procedure UpdateSelTemplateBn;
    function GetFieldNameFromTree(Tree: TTreeView): String;
    function GetSourceNameFromTree(Tree: TTreeView): String;
    procedure FillForms;
    procedure FillSources(Tree: TTreeView; AForm: TdxForm);
    procedure FillFormFields(Tree: TTreeView; AForm: TdxForm; const Pfx: String);
    procedure FillQueryFields(Tree: TTreeView; RD: TReportData);
    procedure FillFields(ParentTree, Tree: TTreeView);
    procedure FillReports;
    procedure MenuClick(Sender: TObject);
    procedure SetControlState;
    function GetCurrentForm: TdxForm;
    procedure SetForm(Fm: TdxForm);
    procedure SetReport(RD: TReportData);
    procedure SaveSettings;
    procedure MakeFmTemplatesMenu(Fm: TdxForm);
    procedure MakeRpTemplatesMenu(RD: TReportData);
    procedure MakeTemplatesMenu;
    procedure SaveSelection(Tree: TTreeView; out Path: String; out NodeIdx: Integer);
    procedure RestoreSelection(Tree: TTreeView; const Path: String; NodeIdx: Integer);
    function GetSelectedForm: TdxForm;
    function GetSelectedReport: TReportData;
    function CreateRpForm(RD: TReportData): TdxForm;
    function ActiveFieldsTree: TTreeView;
  public
    procedure ShowForm;
    procedure UpdateAll;
    procedure Reset;
    //function IsCurrentForm: Boolean;
    //property CurFormId: Integer read FCurFormId write FCurFormId;
  end;

var
  TemplateFieldsFm: TTemplateFieldsFm;

procedure ShowTemplateFieldsForm;
procedure CloseTemplateFieldsForm;
procedure UpdateTemplateFieldsForm;
procedure ResetTemplateFieldsForm;

implementation

uses
  formmanager, reportmanager, Clipbrd, mainframe, designerframe, mainform,
  LazUtf8, apputils, dxfiles, dximages, pivotgrid, dxactions, appsettings,
  helpmanager, scriptmanager, mytypes, dxcharts;

const
  FORM_IDX = 0;
  QRY_IDX = 12;
  GRID_IDX = 13;
  SUM_IDX = 17;
  CF_IDX = 18;

procedure ShowTemplateFieldsForm;
begin
  if TemplateFieldsFm = nil then
    TemplateFieldsFm := TTemplateFieldsFm.Create(Application);
  TemplateFieldsFm.ShowForm;
  if TemplateFieldsFm.WindowState = wsMinimized then
    TemplateFieldsFm.WindowState := wsNormal;
end;

procedure CloseTemplateFieldsForm;
begin
  if (TemplateFieldsFm <> nil) and TemplateFieldsFm.Visible then
    TemplateFieldsFm.Close;
end;

procedure UpdateTemplateFieldsForm;
begin
  if (TemplateFieldsFm <> nil) and TemplateFieldsFm.Visible then
    TemplateFieldsFm.UpdateAll;
end;

procedure ResetTemplateFieldsForm;
begin
  if TemplateFieldsFm <> nil then TemplateFieldsFm.Reset;
end;

function GetImageIdx(C: TComponent): Integer;
var
  n: Integer;
begin
  n := -1;
  if C is TdxForm then n := 0
  else if C is TdxEdit then n := 1
  else if C is TdxCalcEdit then n := 2
  else if C is TdxDateEdit then n := 3
  else if C is TdxTimeEdit then n := 4
  else if C is TdxMemo then n := 5
  else if C is TdxCheckBox then n := 6
  else if C is TdxComboBox then n := 7
  else if C is TdxLookupComboBox then n := 8
  else if C is TdxLabel then n := 9
  else if C is TdxCounter then n := 10
  else if C is TdxObjectField then n := 11
	else if C is TdxQueryGrid then n := 12
  else if C is TdxGrid then n := 13
  else if C is TdxDBImage then n := 14
  else if C is TdxPivotGrid then n := 15
  else if C is TdxFile then n := 16
  else if C is TdxChart then n := 19
  else if C is TdxRecordId then n := 20;
  Result := n;
end;

function GetImageIdxRp(Tp: TRpFieldType): Integer;
var
  n: Integer;
begin
  n := -1;
  case Tp of
    flText: n := 1;
    flNumber: n := 2;
    flDate: n := 3;
    flBool: n := 6;
    flObject: n := 8;
    flTime: n := 4;
    flCounter: n := 10;
    flFile: n := 16;
    flRecId: n := 20;
  end;
  Result := n;
end;

procedure ExtractTemplatesFromForm(Fm: TdxForm; SL: TStringListUtf8);

  procedure _ExtractFromActionGrid(Controls: TEAControls; const GridPropValue: String);
  var
    Rows, Titles, Cols: TStringList;
    i, j, n: Integer;
    EAC: TEAControl;
  begin
    Rows := TStringList.Create;
    Titles := TStringList.Create;
    Cols := TStringList.Create;

    try

    SplitStr(GridPropValue, '|', Rows);
    SplitStr(Rows[0], ';', Titles);

    for i := 1 to Rows.Count - 1 do
    begin
      SplitStr(Rows[i], ';', Cols);
      for j := 0 to Controls.Count - 1 do
      begin
        EAC := Controls[j];
        n := Titles.IndexOf(EAC.Name);
        if (n < 0) or (n > Cols.Count - 1) then Continue;
        if EAC.ControlType = eacTemplate then SL.AddStr(Cols[n]);
      end;
    end;

    finally
      Rows.Free;
      Titles.Free;
      Cols.Free;
    end;
  end;

  procedure _ExtractFromAction(A: TBaseAction);
  var
    AC: TActionCustom;
    EA: TExprAction;
    i: Integer;
    EAC: TEAControl;
    P: TActionProp;
  begin
    if A is TPrintAction then
      with TPrintAction(A) do
      begin
        if TemplateFile <> '' then SL.AddStr(TemplateFile);
      end
    else if A is TActionCustom then
    begin
      AC := TActionCustom(A);
      EA := ScriptMan.Actions.FindAction(AC.ActionId);
      if EA = nil then Exit;   // Если расширение удалено
      for i := 0 to EA.Controls.Count - 1 do
      begin
        EAC := EA.Controls[i];
        if EAC.ControlType = eacTemplate then
        begin
          P := AC.Props.Find(EAC.Name);
          if P <> nil then SL.AddStr(P.Value);
        end
        else if EAC.ControlType = eacGrid then
        begin
          P := AC.Props.Find(EAC.Name);
          if P <> nil then _ExtractFromActionGrid(EAC.Controls, P.Value);
        end;
      end;
    end;
  end;

  procedure _ExtractFromLines(L: TActionLines);
  var
    i: Integer;
    Line: TActionLine;
  begin
    for i := 0 to L.Count - 1 do
    begin
      Line := L[i];
      if Line.Kind in [alkIf, alkElseIf, alkElse] then
        _ExtractFromLines(Line.Lines)
      else if Line.Kind = alkAction then
        _ExtractFromAction(Line.Action);
    end;
  end;

  procedure _Extract(AR: TActionRunner; const Xml: String);
  begin
    AR.Load(Xml);
    _ExtractFromLines(AR.Lines);
  end;

var
  AR: TActionRunner;
  i: Integer;
  C: TComponent;
begin
  SL.Clear;
  SL.AddStrings(Fm.Templates);
  AR := TActionRunner.Create;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxButton then _Extract(AR, TdxButton(C).ActionOnClick);
  end;
  _Extract(AR, Fm.ActionOnCreate);
  SL.Sort;
  AR.Free;
end;

{$R *.lfm}

{ TOleDropSource }

{$IFDEF WINDOWS}
function TOleDropSource.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: DWORD): HResult; StdCall;
begin
  if fEscapePressed then Result:=DRAGDROP_S_CANCEL
  else if (grfKeyState and MK_LBUTTON)=0 then Result:=DRAGDROP_S_DROP
  else Result:=S_OK;
end;

function TOleDropSource.GiveFeedback(dwEffect: DWORD): HResult; StdCall;
begin
  Result:=DRAGDROP_S_USEDEFAULTCURSORS;
end;
{$ENDIF}

{ TTemplateFieldsFm }

procedure TTemplateFieldsFm.FormCbxSelect(Sender: TObject);
var
  Fm: TdxForm;
begin
  Fm := GetSelectedForm;
  SetForm(Fm);
  MakeFmTemplatesMenu(Fm);
end;

procedure TTemplateFieldsFm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveSettings;
end;

procedure TTemplateFieldsFm.FieldsSelectionChanged(Sender: TObject);
begin
  SetControlState;
end;

procedure TTemplateFieldsFm.FieldsUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if Utf8Key < #32 then Exit;

  Filter.Text := UTF8Key;
  Filter.SetFocus;
  Filter.SelStart := Length(UTF8Key);
  Filter.SelLength := 0;
end;

procedure TTemplateFieldsFm.FieldsDblClick(Sender: TObject);
begin
  if MenuItem1.Enabled then MenuItem1.Click
  else if MenuItem2.Enabled then MenuItem2.Click;
end;

procedure TTemplateFieldsFm.FieldsCompare(Sender: TObject; Node1,
  Node2: TTreeNode; var Compare: Integer);
begin
  // Корневые узлы остаются на месте
  if Node1.Parent = nil then Compare := 0
  // Узел "Вычисляемые поля" должен оставаться на месте
  else if ((Node1.Count > 0) and (Node1.Data = nil)) or
    ((Node2.Count > 0) and (Node2.Data = nil)) then Compare := 0
  else Compare := MyUtf8CompareText(Node1.Text, Node2.Text);
end;

procedure TTemplateFieldsFm.FieldsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FOldX := X; FOldY := Y;
  end;
end;

{$IFDEF WINDOWS}
procedure TTemplateFieldsFm.FieldsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Effect: Integer;
  pEffect: LPDWORD;
  OleData: IDataObject;
  N: TTreeNode;
  S: String;
  Tree: TTreeView;
begin
  if (Shift = [ssLeft]) and ((Abs(X - FOldX) > 4) or (Abs(Y - FOldY) > 4)) then
  begin
    Tree := TTreeView(Sender);
    N := Tree.Selected;
    if N = nil then Exit;
    if N.Parent = nil then S := GetSourceNameFromTree(Tree)
    else if (N.Count = 0) or (N.Data <> nil) then S := GetFieldNameFromTree(Tree)
    else Exit;

    Effect := DROPEFFECT_NONE;
    pEffect := LPDWORD(@Effect);
    Clipboard.AsText := S;
    OleGetClipboard(OleData);
    DoDragDrop(OleData, FOleDropSource, DROPEFFECT_COPY, pEffect);
  end;
end;
{$ELSE}
procedure TTemplateFieldsFm.FieldsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
end;
{$ENDIF}

procedure TTemplateFieldsFm.FormCreate(Sender: TObject);
begin
  Caption := rsTemplateWizard;
  ToolButton1.Hint := rsSetCurrentForm;
  ToolButton2.Hint := rsOpenTemplatesFolder;
  ToolButton3.Hint := rsOpenTemplateFile;
  TabSheet1.Caption := rsForms;
  TabSheet2.Caption := rsReports;
  Label1.Caption := rsCurrentForm;
  Label2.Caption := rsDataSources;
  Label3.Caption := rsFields;
  Filter.TextHint := rsFindField;
  Label4.Caption := rsCurrentReport;
  Label5.Caption := rsDataSources;
  Label6.Caption := rsFields;
  RpFilter.TextHint := rsFindField;
  Msg.Caption := rsCouldNotFindFormRp;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  ButtonPanel1.CloseButton.Caption := rsClose;
  MenuItem1.Caption := rsCopy;
  MenuItem2.Caption := rsCopyTagGrid;
  MenuItem3.Caption := rsCopyTagForm;
  MenuItem5.Caption := rsCopyTagGroup;
  MenuItem4.Caption := rsCopyTagEnd;

  ToolImages.AddLazarusResource('form16');
  ToolImages.AddLazarusResource('folder16');
  ToolImages.AddLazarusResource('doc16');
  SrcImages.AddLazarusResource('form16');
  SrcImages.AddLazarusResource('grid16');
  SrcImages.AddLazarusResource('query16');
  with FieldsImages do
	begin
    AddLazarusResource('form16');
    AddLazarusResource('text16');
    AddLazarusResource('calc16');
    AddLazarusResource('date16');
    AddLazarusResource('clock16');
    AddLazarusResource('memo16');
    AddLazarusResource('checkbox16');
    AddLazarusResource('combobox16');
    AddLazarusResource('object16');
    AddLazarusResource('label16');
    AddLazarusResource('counter16');
    AddLazarusResource('objectfield16');
    AddLazarusResource('query16');
    AddLazarusResource('grid16');
    AddLazarusResource('dbimage16');
    AddLazarusResource('pivottable16');
    AddLazarusResource('file16');
    AddLazarusResource('sum16');
    AddLazarusResource('calcfield16');
    AddLazarusResource('chart16');
    AddLazarusResource('key16')
  end;

  AppConfig.LoadTemplateWizard;
  Left := AppConfig.TWLeft;
  Top := AppConfig.TWTop;
  Width := AppConfig.TWWidth;
  Height := AppConfig.TWHeight;
  Panel1.Height := AppConfig.TWTopPanHeight;
  Panel3.Height := AppConfig.TWTopPan2Height;

  {$IFDEF WINDOWS}OleInitialize(nil);{$ENDIF}
  FOleDropSource := TOleDropSource.Create;
  Sources.IsWine := AppConfig.IsWine;
  Fields.IsWine := Sources.IsWine;
  RpSources.IsWine := Sources.IsWine;
  RpFields.IsWine := Sources.IsWine;
end;

procedure TTemplateFieldsFm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRpForm);
  {$IFDEF WINDOWS}OleUninitialize;{$ENDIF}
  FOleDropSource.Free;
  SaveSettings;
end;

procedure TTemplateFieldsFm.FormShow(Sender: TObject);
begin
  if not AppConfig.TemplatesFormPosCorrected then
  begin
    CorrectFormPos(Self, Self);
    AppConfig.TemplatesFormPosCorrected := True;
  end;
  if Pg.PageIndex = 0 then
  begin
    Fields.SetFocus;
    if Fields.Items.Count > 0 then Fields.Items[0].Selected := True;
  end
  else if Pg.PageIndex = 1 then
  begin
    RpFields.SetFocus;
    if RpFields.Items.Count > 0 then RpFields.Items[0].Selected := True;
  end;
  SetControlState;
end;

procedure TTemplateFieldsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('templatewizard');
end;

procedure TTemplateFieldsFm.MenuItem1Click(Sender: TObject);
begin
  Clipboard.AsText := GetFieldNameFromTree(ActiveFieldsTree);
end;

procedure TTemplateFieldsFm.MenuItem2Click(Sender: TObject);
begin
  Clipboard.AsText := GetSourceNameFromTree(ActiveFieldsTree);
end;

procedure TTemplateFieldsFm.MenuItem3Click(Sender: TObject);
begin
  Clipboard.AsText := '{form|' + ActiveFieldsTree.Selected.Text + '}';
end;

procedure TTemplateFieldsFm.MenuItem4Click(Sender: TObject);
begin
  Clipboard.AsText := '{end}';
end;

procedure TTemplateFieldsFm.MenuItem5Click(Sender: TObject);
var
  S: String;
begin
  S := GetFieldNameFromTree(ActiveFieldsTree);
  Delete(S, 1, 1);
  Delete(S, Length(S), 1);
  Clipboard.AsText := '{group|' + S + '}';
end;

procedure TTemplateFieldsFm.PgChange(Sender: TObject);
begin
  MakeTemplatesMenu;
  SetControlState;
end;

procedure TTemplateFieldsFm.RpCbxSelect(Sender: TObject);
var
  RD: TReportData;
begin
  RD := GetSelectedReport;
  SetReport(RD);
  MakeRpTemplatesMenu(RD);
end;

procedure TTemplateFieldsFm.RpFieldsUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if Utf8Key < #32 then Exit;

  RpFilter.Text := UTF8Key;
  RpFilter.SetFocus;
  RpFilter.SelStart := Length(UTF8Key);
  RpFilter.SelLength := 0;
end;

procedure TTemplateFieldsFm.RpSourcesSelectionChanged(Sender: TObject);
begin
  RpFields.Items.Clear;
  RpFilter.Text := '';
  FillFields(RpSources, RpFields);
end;

procedure TTemplateFieldsFm.SourcesCompare(Sender: TObject; Node1,
  Node2: TTreeNode; var Compare: Integer);
var
  Obj1, Obj2: TObject;
begin
  Obj1 := TObject(Node1.Data);
  Obj2 := TObject(Node2.Data);
  if (Obj1 is TdxForm) and (Obj2 is TReportData) then Compare := 1
  else if (Obj1 is TReportData) and (Obj2 is TdxForm) then Compare := -1
  else Compare := MyUtf8CompareText(Node1.Text, Node2.Text);
end;

procedure TTemplateFieldsFm.SourcesSelectionChanged(Sender: TObject);
begin
  Fields.Items.Clear;
  Filter.Text := '';
  FillFields(Sources, Fields);
end;

procedure TTemplateFieldsFm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  MsgPan.Hide;
end;

procedure TTemplateFieldsFm.ToolButton1Click(Sender: TObject);
begin
  FillForms;
  SetForm(GetCurrentForm);
  if Pg.ActivePageIndex > 0 then
    Pg.ActivePageIndex := 0
  else
    MakeTemplatesMenu;
end;

procedure TTemplateFieldsFm.ToolButton2Click(Sender: TObject);
begin
  MainFm.OpenTemplateFolder;
end;

function FindFormByTemplate(const S: String): TdxForm;

  function _Exists(AR: TActionRunner; const Xml: String): Boolean;
  var
    z: Integer;
    Line: TActionLine;
  begin
    Result := False;
    AR.Load(Xml);
    for z := 0 to AR.Lines.Count - 1 do
    begin
      Line := AR.Lines[z];
      if Line.Kind <> alkAction then Continue;
      if Line.Action.TemplateExists(S) then Exit(True);
    end;
  end;

var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
  AR: TActionRunner;
begin
  Result := nil;
  AR := TActionRunner.Create;
  try

  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if Fm.PId > 0 then Continue;
    for j := 0 to Fm.Templates.Count - 1 do
    begin
      if MyUtf8CompareText(S, Fm.Templates[j]) = 0 then Exit(Fm);
    end;
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if (C is TdxButton) and _Exists(AR, TdxButton(C).ActionOnClick) then Exit(Fm);
    end;
    if _Exists(AR, Fm.ActionOnCreate) then Exit(Fm);
  end;

  finally
    AR.Free;
  end;
end;

function FindReportByTemplate(const S: String): TReportData;
var
  i, j: Integer;
  RD: TReportData;
begin
  Result := nil;
  for i := 0 to ReportMan.ReportCount - 1 do
  begin
    RD := ReportMan.Reports[i];
    if RD.Kind = rkQuery then Continue;
    for j := 0 to RD.Templates.Count - 1 do
    begin
      if MyUtf8CompareText(S, RD.Templates[j]) = 0 then Exit(RD);
    end;
  end;
end;

procedure TTemplateFieldsFm.ToolButton3Click(Sender: TObject);
var
  FNm, RelFNm: String;
  Fm: TdxForm;
  RD: TReportData;
begin
  FNm := '';
  with TOpenDialog.Create(nil) do
  begin
    Title := rsTemplates;
    InitialDir := GetTemplatesDir;
    Filter := rsTemplatesFilesFilter;
    Options := Options + [ofFileMustExist];
    if Execute then FNm := FileName;
    Free;
  end;
  if FNm <> '' then
  begin
    {$ifdef windows}
      RelFNm := ExtractRelativepath(Utf8LowerCase(GetTemplatesDir), Utf8LowerCase(FNm));
    {$else}
      RelFNm := ExtractRelativepath(GetTemplatesDir, FNm);
    {$endif}
    Fm := FindFormByTemplate(RelFNm);
    if Fm = nil then
    begin
      RD := FindReportByTemplate(RelFNm);
      if RD = nil then
      begin
        //SetForm(nil);
        //SetReport(nil);
        MsgPan.Show;
        Timer1.Enabled := True;
      end
      else
      begin
        SetReport(RD);
        if Pg.ActivePageIndex = 1 then MakeRpTemplatesMenu(RD)
        else Pg.ActivePageIndex := 1;
      end;
    end
    else
    begin
      SetForm(Fm);
      if Pg.ActivePageIndex = 0 then MakeFmTemplatesMenu(Fm)
      else Pg.ActivePageIndex := 0;
    end;
    OpenFile(FNm);
  end;
end;

procedure TTemplateFieldsFm.UpdateSelTemplateBn;
begin
  if FPop <> nil then
  begin
    ToolButton3.Style := tbsDropDown;
    ToolButton3.DropdownMenu := FPop;
  end
  else
  begin
    ToolButton3.Style := tbsButton;
    ToolButton3.DropdownMenu := nil;
  end;
end;

function TTemplateFieldsFm.GetFieldNameFromTree(Tree: TTreeView): String;
var
  N: TTreeNode;
begin
  N := Tree.Selected;
  if Integer(N.Data) = 2 then Exit('{pivotgrid|' + N.Text + '}')
  else if Integer(N.Data) = 3 then Exit('{chart|' + N.Text + '}');
  Result := '';
  while True do
  begin
    if Result = '' then Result := N.Text
    else Result := N.Text + '|' + Result;
    N := N.Parent;
    if (N = nil) or (N.Data = nil) then Break;
  end;
  Result := '[' + Result + ']';
end;

function TTemplateFieldsFm.GetSourceNameFromTree(Tree: TTreeView): String;
var
  N: TTreeNode;
begin
  Result := '';
  N := Tree.Selected;
  if N = nil then Exit;
  Result := '{grid|' + N.Text + '}';
end;

procedure TTemplateFieldsFm.FillForms;
begin
  FormMan.FormsToList(FormCbx.Items);
end;

procedure TTemplateFieldsFm.FillSources(Tree: TTreeView; AForm: TdxForm);

  procedure _AddFormSources(PN: TTreeNode; Fm: TdxForm);
  var
    i: Integer;
    C: TComponent;
    RD: TReportData;
    N: TTreeNode;
    Tbl: TdxForm;
  begin
    N := Tree.Items.AddChildObject(PN, Fm.FormCaption, Fm);
    if Fm.PId = 0 then SetNodeImageIndex(N, 0)
    else SetNodeImageIndex(N, 1);
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if C is TdxQueryGrid then
      begin
        RD := ReportMan.FindReport(GetId(C));
        SetNodeImageIndex( Tree.Items.AddChildObject(N, RD.Name, RD), 2);
      end
      else if C is TdxGrid then
      begin
        Tbl := FormMan.FindForm(GetId(C));
        // Подчиненная форма может быть в процессе удаления
        if Tbl <> nil then
          _AddFormSources(N, Tbl);
      end;
    end;
  end;

begin
  Tree.BeginUpdate;
  Tree.Items.Clear;
  _AddFormSources(nil, AForm);
  if Tree.Items.Count > 0 then
  begin
    Tree.Items[0].Expand(True);
    Tree.Items[0].Selected := True;
  end;
  Tree.AlphaSort;
  Tree.EndUpdate;
end;

procedure TTemplateFieldsFm.FillFormFields(Tree: TTreeView; AForm: TdxForm;
  const Pfx: String);

  procedure FillObjectFields(PN: TTreeNode; Obj: TComponent; L: Integer);
  var
    Fm: TdxForm;
    i: Integer;
    C: TComponent;
    N: TTreeNode;
  begin
    if L = 4 then Exit;
    Fm := FormMan.FindForm(GetSourceTId(Obj));
    if Fm = nil then Exit;
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if not (C is TdxRecordId) and not IsField(C) then Continue;
      N := Tree.Items.AddChild(PN, GetFieldName(C));
      SetNodeImageIndex(N, GetImageIdx(C));
      if (C is TdxLookupComboBox) {and (L < 4)} then
      begin
        N.Data := TObject(1);
        FillObjectFields(N, C, L + 1);
      end;
    end;
  end;

var
  PN, N, CN: TTreeNode;
  i: Integer;
  C: TComponent;
  S: String;
begin
  N := Tree.Items.AddChild(nil, AForm.FormCaption);
  if AForm.PId = 0 then SetNodeImageIndex(N, FORM_IDX)
  else SetNodeImageIndex(N, GRID_IDX);
  PN := N;
  for i := 0 to AForm.ComponentCount - 1 do
  begin
    C := AForm.Components[i];

    if (C is TdxLabel) and (Trim(TdxLabel(C).Expression) <> '') then
    begin
      N := Tree.Items.AddChild(PN, Pfx + TdxLabel(C).FieldName);
      SetNodeImageIndex(N, GetImageIdx(C));
    end
    else if C is TdxPivotGrid then
    begin
      N := Tree.Items.AddChild(PN, C.Name);
      N.Data := TObject(2);
      SetNodeImageIndex(N, GetImageIdx(C));
    end
    else if C is TdxChart then
    begin
      N := Tree.Items.AddChild(PN, C.Name);
      N.Data := TObject(3);
      SetNodeImageIndex(N, GetImageIdx(C));
    end
    else if HasFId(C) then
    begin
      N := Tree.Items.AddChild(PN, Pfx + GetFieldName(C));
      SetNodeImageIndex(N, GetImageIdx(C));
      if C is TdxLookupComboBox then
      begin
        N.Data := TObject(1);
        FillObjectFields(N, C, 0);
      end;
    end;
  end;
  if AForm.CalcFields.Count > 0 then
  begin
    N := Tree.Items.AddChild(PN, rsCalcFields);
    SetNodeImageIndex(N, SUM_IDX);
    for i := 0 to AForm.CalcFields.Count - 1 do
    begin
      S := AForm.CalcFields.Names[i];
      CN := Tree.Items.AddChild(N, Pfx + S);
      SetNodeImageIndex(CN, CF_IDX);
    end;
    N.Expand(False);
  end;
  PN.Expand(False);
end;

procedure TTemplateFieldsFm.FillQueryFields(Tree: TTreeView; RD: TReportData);
var
  N, PN, CN: TTreeNode;
  i: Integer;
  //rF: TRpField;
  CF: TRpCalcField;
begin
  N := Tree.Items.AddChild(nil, RD.Name);
  SetNodeImageIndex(N, QRY_IDX);
  PN := N;

  for i := 0 to RD.GetRpSQLFieldCount - 1 do
  begin
    if not RD.GetFieldVisible(i) then Continue;
    CN := Tree.Items.AddChild(N, RD.GetFieldName(i));
    SetNodeImageIndex(CN, GetImageIdxRp( RD.GetFieldType(i) ));
  end;

  {if RD.Sources.Count > 0 then
    for i := 0 to RD.Sources[0]^.Fields.Count - 1 do
    begin
      rF := RD.Sources[0]^.Fields[i]^;
      if rF.Visible then
      begin
        CN := Tree.Items.AddChild(N, rF.Name);
        SetNodeImageIndex(CN, GetImageIdxRp(GetLowField(@rF)^.Tp));
      end;
    end; }
  if RD.CalcFields.Count > 0 then
  begin
    N := Tree.Items.AddChild(N, rsCalcFields);
    SetNodeImageIndex(N, SUM_IDX);
    for i := 0 to RD.CalcFields.Count - 1 do
    begin
      CF := RD.CalcFields[i]^;
      CN := Tree.Items.AddChild(N, CF.Name);
      SetNodeImageIndex(CN, GetImageIdxRp(CF.Tp));
    end;
  end;
  PN.Expand(True);
end;

procedure TTemplateFieldsFm.FillFields(ParentTree, Tree: TTreeView);
var
  N: TTreeNode;
  Obj: TObject;
  PrevRD: TReportData;
  PrevFm: TdxForm;
  Pfx: String;
begin
  Tree.BeginUpdate;
  Tree.Items.Clear;
  PrevRD := nil;
  PrevFm := nil;
  N := ParentTree.Selected;
  while N <> nil do
  begin
    Obj := TObject(N.Data);
    if Obj is TReportData then
    begin
      PrevRD := TReportData(Obj);
      FillQueryFields(Tree, PrevRD)
    end
    else if Obj is TdxForm then
    begin
      if PrevFm <> nil then
        Pfx := '!'
      else if PrevRD <> nil then
        Pfx := ':'
      else
        Pfx := '';
      PrevFm := TdxForm(Obj);
      FillFormFields(Tree, PrevFm, Pfx);
    end;
    N := N.Parent;
  end;
  Tree.AlphaSort;
  Tree.EndUpdate;
end;

procedure TTemplateFieldsFm.FillReports;
begin
  ReportMan.GetReports(RpCbx.Items);
end;

procedure TTemplateFieldsFm.MenuClick(Sender: TObject);
begin
  OpenFile(GetTemplatesDir + TMenuItem(Sender).Caption);
end;

procedure TTemplateFieldsFm.SetControlState;
var
  N: TTreeNode;
begin
  N := ActiveFieldsTree.Selected;
  MenuItem1.Enabled := (N <> nil) and (N.Parent <> nil) and ((N.Count = 0) or (N.Data <> nil));
  MenuItem2.Enabled := (N <> nil) and (N.Parent = nil);
  MenuItem3.Enabled := MenuItem2.Enabled;
  MenuItem5.Enabled := (N <> nil) and (N.Parent <> nil) and (Integer(N.Data) <> 2) and
    ((N.Count = 0) or (N.Data <> nil));
  MenuItem5.Visible := Pg.ActivePageIndex = 0
end;

function TTemplateFieldsFm.GetCurrentForm: TdxForm;
var
  Fm: TdxForm;
begin
  Fm := nil;
  if MainFr <> nil then
  begin
    if MainFr.CurView <> nil then
      Fm := MainFr.CurView.Form;
  end
  else if DesignFr <> nil then
    Fm := DesignFr.CurForm;

  if Fm <> nil then
  begin
    if Fm.PId > 0 then
      Fm := FormMan.FindForm(Fm.PId)
    // Берем указатели из метаданных, а не реальных форм.
    else if MainFr <> nil then
      Fm := FormMan.FindForm(Fm.Id);
  end;
  Result := Fm;
end;

procedure TTemplateFieldsFm.SetForm(Fm: TdxForm);
begin
  if Fm <> nil then
    with FormCbx do
    begin
      ItemIndex := Items.IndexOfObject(Fm);
      FillSources(Sources, Fm);
      FCurFormId := Fm.Id;
    end
  else
  begin
    FormCbx.ItemIndex := -1;
    Sources.Items.Clear;
    Fields.Items.Clear;
    Filter.Text := '';
    FCurFormId := 0;
  end;
end;

procedure TTemplateFieldsFm.SetReport(RD: TReportData);
begin
  if (RD = nil) or RD.IsEmpty then
  begin
    if RD = nil then
    begin
      RpCbx.ItemIndex := -1;
      FCurRpId := 0;
    end;
    RpSources.Items.Clear;
    RpFields.Items.Clear;
    RpFilter.Text := '';
  end
  else
    with RpCbx do
    begin
      ItemIndex := Items.IndexOfObject(RD);
      FillSources(RpSources, CreateRpForm(RD));
      FCurRpId := RD.Id;
    end
end;

procedure TTemplateFieldsFm.SaveSettings;
begin
  with AppConfig do
  begin
    TWLeft := ScaleTo96(Left);
    TWTop := ScaleTo96(Top);
    TWWidth := ScaleTo96(Width);
    TWHeight := ScaleTo96(Height);
    TWTopPanHeight := ScaleTo96(Panel1.Height);
    TWTopPan2Height := ScaleTo96(Panel3.Height);
    SaveTemplateWizard;
  end;
end;

procedure TTemplateFieldsFm.MakeFmTemplatesMenu(Fm: TdxForm);
var
  SL: TStringListUtf8;
  i: Integer;
begin
  FreeAndNil(FPop);
  if Fm = nil then Exit;
  SL := TStringListUtf8.Create;
  ExtractTemplatesFromForm(Fm, SL);
  if SL.Count > 0 then
  begin
    FPop := TPopupMenu.Create(Self);
    for i := 0 to SL.Count - 1 do
      FPop.Items.Add( CreateMenuItem(FPop, SL[i], 0, 0, @MenuClick) );
  end;
  SL.Free;
  UpdateSelTemplateBn;
end;

procedure TTemplateFieldsFm.MakeRpTemplatesMenu(RD: TReportData);
var
  SL: TStringListUtf8;
  i: Integer;
begin
  FreeAndNil(FPop);
  if RD = nil then Exit;
  SL := TStringListUtf8.Create;
  SL.AddStrings(RD.Templates);
  SL.Sort;
  if SL.Count > 0 then
  begin
    FPop := TPopupMenu.Create(Self);
    for i := 0 to SL.Count - 1 do
      FPop.Items.Add( CreateMenuItem(FPop, SL[i], 0, 0, @MenuClick) );
  end;
  SL.Free;
  UpdateSelTemplateBn;
end;

procedure TTemplateFieldsFm.MakeTemplatesMenu;
begin
  if Pg.PageIndex = 0 then
    MakeFmTemplatesMenu(GetSelectedForm)
  else if Pg.PageIndex = 1 then
    MakeRpTemplatesMenu(GetSelectedReport);
end;

function GetNodePath(N: TTreeNode): String;
begin
  if N = nil then Exit('');
  Result := GetNodePath(N.Parent);
  if Result <> '' then Result := Result + '/';
  Result := Result + N.Text;
end;

procedure TTemplateFieldsFm.SaveSelection(Tree: TTreeView; out Path: String;
  out NodeIdx: Integer);
begin
  Path := ''; NodeIdx := -1;
  if Tree.Selected <> nil then
  begin
    Path := GetNodePath(Tree.Selected);
    if Path <> '' then NodeIdx := Tree.Selected.AbsoluteIndex;
  end;
end;

procedure TTemplateFieldsFm.RestoreSelection(Tree: TTreeView;
  const Path: String; NodeIdx: Integer);
var
  N: TTreeNode;
begin
  if Path <> '' then
  begin
    N := Tree.Items.FindNodeWithTextPath(Path);
    if N <> nil then N.Selected := True
    else if Tree.Items.Count > NodeIdx then Tree.Items[NodeIdx].Selected := True;
  end;
end;

function TTemplateFieldsFm.GetSelectedForm: TdxForm;
begin
  with FormCbx do
    if ItemIndex >= 0 then
      Result := TdxForm(Items.Objects[ItemIndex])
    else
      Result := nil;
end;

function TTemplateFieldsFm.GetSelectedReport: TReportData;
begin
  with RpCbx do
    if ItemIndex >= 0 then
      Result := TReportData(Items.Objects[ItemIndex])
    else
      Result := nil;
end;

procedure TTemplateFieldsFm.ShowForm;
begin
  if Visible then Exit;

  MsgPan.Visible := False;
  UpdateAll;
  if not FShowAgain then
  begin
    ToolButton1.Click;
    FShowAgain := True;
  end;
  Show;
end;

function TTemplateFieldsFm.CreateRpForm(RD: TReportData): TdxForm;
var
  SQL: String;
begin
  FreeAndNil(FRpForm);
  FRpForm := CreateReportForm(RD, SQL);
  Result := FRpForm;
end;

function TTemplateFieldsFm.ActiveFieldsTree: TTreeView;
begin
  if Pg.ActivePageIndex = 0 then Result := Fields
  else Result := RpFields;
end;

procedure TTemplateFieldsFm.UpdateAll;
var
  P1, P2: String;
  i1, i2: Integer;
  FilterText: TCaption;
begin
  SaveSelection(Sources, P1, i1);
  SaveSelection(Fields, P2, i2);
  FilterText := Filter.Text;
  FillForms;
  SetForm(FormMan.FindForm(FCurFormId));
  RestoreSelection(Sources, P1, i1);
  RestoreSelection(Fields, P2, i2);
  if FilterText <> '' then Filter.Text := FilterText;

  SaveSelection(RpSources, P1, i1);
  SaveSelection(RpFields, P2, i2);
  FilterText := RpFilter.Text;
  FillReports;
  SetReport(ReportMan.FindReport(FCurRpId));
  RestoreSelection(RpSources, P1, i1);
  RestoreSelection(RpFields, P2, i2);
  if FilterText <> '' then RpFilter.Text := FilterText;

  MakeTemplatesMenu;
end;

procedure TTemplateFieldsFm.Reset;
begin
  FCurFormId := 0;
  FCurRpId := 0;
  FormCbx.Clear;
  Sources.Items.Clear;
  Fields.Items.Clear;
  Filter.Text := '';
  RpSources.Items.Clear;
  RpFields.Items.Clear;
  RpFilter.Text := '';
  FreeAndNil(FPop);
  UpdateSelTemplateBn;
end;

{function TTemplateFieldsFm.IsCurrentForm: Boolean;
var
  Fm: TdxForm;
begin
  Fm := GetCurrentForm;
  Result := (Fm <> nil) and (FCurFormId = Fm.Id);
end; }

end.

