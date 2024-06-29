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

unit MainFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, ComCtrls, Menus, dxctrls,
  formview, sqldb, strconsts, DXReports, DatasetProcessor, Dialogs, ExtCtrls,
  dbgrids, myctrls, StdCtrls;

type

  { TMainFr }

  TMainFr = class(TFrame)
    ImageList1: TImageList;
    CloseTabMnu: TMenuItem;
    ImageList2: TImageList;
    PageControl1: TPageControl;
    EmptyMnu: TPopupMenu;
    FilterMnu: TPopupMenu;
    TabMnu: TPopupMenu;
    TemplatesMnu: TPopupMenu;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    MoveFirstBn: TToolButton;
    MovePriorBn: TToolButton;
    MoveNextBn: TToolButton;
    MoveLastBn: TToolButton;
    AppendBn: TToolButton;
    EditBn: TToolButton;
    CancelBn: TToolButton;
    PostBn: TToolButton;
    DeleteBn: TToolButton;
    RefreshBn: TToolButton;
    PrintBn: TToolButton;
    FilterBn: TToolButton;
    FindBn: TToolButton;
    HelpBn: TToolButton;
    procedure CloseTabMnuClick(Sender: TObject);
    procedure DataMenuClick(Sender: TObject);
    procedure FilterMnuHandler(Sender: TObject);
    procedure FilterMnuPopup(Sender: TObject);
    procedure HelpBnClick(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure PageControl1CloseTabClicked(Sender: TObject);
    procedure StateChange(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure TabMnuPopup(Sender: TObject);
    procedure TblFilterMnuHandler(Sender: TObject);
    procedure FindBnClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure ReportMenuClick(Sender: TObject);
    procedure TemplatesMnuHandler(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
  private
    { private declarations }
    FCurView: TFormView;
    FOldMousePos: TPoint;
    procedure OpenOrFindTab(FmId: Integer);
    procedure OpenTab(FmId: Integer);
    procedure OpenTabs;
    procedure UpdateToolButtonState;
    procedure UpdateStatusBar;
    procedure FillDataMenu;
    procedure BuildTemplatesMenu(Fm: TdxForm);
    procedure BuildFilterMenu(Fm: TdxForm);
    function FindTab(FmId: Integer): TTabSheet;
    procedure ClearMnu(Mnu: TPopupMenu);
    procedure DataSetStateChange;
  protected
    procedure DoOnResize; override;
  public
    procedure CloseAllTabs;
    procedure CloseTab(Pg: TTabSheet);
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure Done;
    procedure RefreshAllLookups(TId: Integer);
    procedure UnBindForms;
    procedure SaveCurrentDataSet;
    function GotoRec(FormId, RecId: Integer): Boolean;
    procedure GotoRec(FormId: Integer; const FieldName, FieldValue: String); overload;
    procedure FillReportMenu;
    procedure BuildMenu;
    procedure UpdateMenu;
    procedure UpdateDataMenu;
    function ValidateAndSave: Boolean;
    property CurView: TFormView read FCurView;
  public
    function OpenTab2(const FormName: String; ViewType: TViewType): TTabSheet;
  end;


var
  MainFr: TMainFr;

implementation

uses
  mainform, formmanager, Db, apputils, dbengine, findform, reportmanager, reportwindow,
  dxusers, ExprFuncs, helpviewform, scriptmanager, mydialogs,
  debugscriptform, mytypes, dxmains, imagemanager, appimagelists, appsettings;

{$R *.lfm}

{ TMainFr }

procedure TMainFr.PageControl1Change(Sender: TObject);
begin
  with PageControl1 do
    if ActivePage <> nil then
    begin
      FCurView := TFormView(ActivePage.Controls[0]);
      with FCurView.DataSetProc do
        if Opened then
        begin
          UpdateToolButtonState;
          UpdateStatusBar;
          if Form.ViewType = vtSimpleForm then Edit;
        end
        else
        begin
          Open;
          FCurView.Form.UpdateTree;
        end;
      BuildTemplatesMenu(FCurView.Form);
      BuildFilterMenu(FCurView.Form);
      // Снова обновляем кнопки, т. к. некоторые (Создать, переход, ...) не обновляются
      UpdateToolButtonState;
    end;
end;

procedure TMainFr.PageControl1Changing(Sender: TObject; var AllowChange: Boolean
  );
begin
  AllowChange := ValidateAndSave;
end;

procedure TMainFr.ReportMenuClick(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  ShowReportWindow(TMenuItem(Sender).Tag);
end;

procedure TMainFr.TemplatesMnuHandler(Sender: TObject);
var
  S: String;
begin
  S := GetTemplatesDir + FCurView.Form.Templates[TMenuItem(Sender).Tag];
  FCurView.DataSetProc.Print(S);
end;

procedure TMainFr.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  PageControl1.OnChange(PageControl1);
end;

procedure TMainFr.DataMenuClick(Sender: TObject);
var
  FmId: PtrInt;
begin
  if not ValidateAndSave then Exit;
  FmId := TMenuItem(Sender).Tag;
  OpenOrFindTab(FmId);
end;

{procedure TMainFr.CloseTabMnuClick(Sender: TObject);
var
  P: TPoint;
  i: Integer;
  Page: TTabSheet;
begin
  P := FOldMousePos;
  P := PageControl1.ScreenToClient(P);
  Page := nil;
  for i := 0 to PageControl1.PageCount - 1 do
  begin
    if PageControl1.TabRect(i).Contains(P) then
    begin
      Page := PageControl1.Pages[i];
      Break;
    end;
  end;
  if Page = nil then Exit;

  if (Page = PageControl1.ActivePage) and not ValidateAndSave then Exit;
  CloseTab(Page);
end;  }

procedure TMainFr.CloseTabMnuClick(Sender: TObject);
var
  P: TPoint;
  i: Integer;
begin
  P := PageControl1.ScreenToClient(FOldMousePos);
  i := PageControl1.IndexOfPageAt(P);
  if i < 0 then Exit;

  if (i = PageControl1.ActivePageIndex) and not ValidateAndSave then Exit;
  CloseTab(PageControl1.Pages[i]);
end;

procedure TMainFr.FilterMnuHandler(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  FCurView.DataSetProc.ApplyFilter(TMenuItem(Sender).Tag);
end;

procedure TMainFr.FilterMnuPopup(Sender: TObject);
var
  Mnu: TPopupMenu;
  i, n: Integer;
  Fm: TdxForm;
  DSP: TDataSetProcessor;
begin
  Mnu := TPopupMenu(Sender);
  Fm := FCurView.Form;
  DSP := FCurView.DataSetProc;
  for i := 0 to Mnu.Items.Count - 1 do
    Mnu.Items[i].Checked:=False;
  n := DSP.DataSets[0]^.FilterIndex;
  if n >= 0 then Mnu.Items[n].Checked:=True;

  n := 0;
  if Fm.Filters.Count > 0 then n := Fm.Filters.Count + 1;
  for i := n to Mnu.Items.Count - 1 do
    Mnu.Items[i].Checked := DSP.DataSets[Mnu.Items[i].Tag]^.Filter.ValuesExists;
end;

procedure TMainFr.HelpBnClick(Sender: TObject);
begin
  ShowHelpForm(FCurView.Form.HelpText.Text);
end;

procedure TMainFr.MenuItemClick(Sender: TObject);
var
  MI: TMenuItem;
  dxMI: TdxMenuItem;
begin
  MI := TMenuItem(Sender);
  dxMI := TdxMenuItem(MI.Tag);

  if dxMI.Kind in [miForm, miReport] then
    if not ValidateAndSave then Exit;

  if dxMI.Kind = miForm then
    OpenOrFindTab(dxMI.Id)
  else if dxMI.Kind = miReport then
  begin
    {$ifdef linux}
    // Почему-то пункт помечается флажком, нужно сделать ЭТО (!), чтобы убрать флажок.
    MI.Checked := True;
    MI.Checked := False;
    //
    {$endif}
    ShowReportWindow(dxMi.Id);
  end;
end;

procedure TMainFr.PageControl1CloseTabClicked(Sender: TObject);
begin
  if (PageControl1.ActivePage = Sender) and not ValidateAndSave then Exit;
  CloseTab(TTabSheet(Sender));
end;

procedure TMainFr.StateChange(Sender: TObject);
begin
  DataSetStateChange;
end;

procedure TMainFr.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  X, Y: LongInt;
begin
  if (Panel.Index = 3) and (Panel.Text = '1') then
  begin
	  Y := (Rect.Bottom - Rect.Top) div 2 - 8 + Rect.Top;
    X := Rect.Left + 2;
  	//X := (Rect.Right - Rect.Left) div 2 - 8 + Rect.Left;
 		ImageList2.Draw(StatusBar.Canvas, X, Y, 0);
  end;
end;

procedure TMainFr.TabMnuPopup(Sender: TObject);
begin
  FOldMousePos := Mouse.CursorPos;
end;

procedure TMainFr.TblFilterMnuHandler(Sender: TObject);
begin
  //if not FCurView.DataSetProc.Validate(0) then Exit;
  if not ValidateAndSave then Exit;
  FCurView.DataSetProc.OpenFilter(TMenuItem(Sender).Tag);
end;

procedure TMainFr.FindBnClick(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  ShowFindForm(FCurView.Form, FCurView.DataSetProc.MasterSet);
  FCurView.DataSetProc.RefreshCurRecord;
end;

procedure TMainFr.ToolButtonClick(Sender: TObject);
var
  i: PtrInt;
begin
  i := TComponent(Sender).Tag;
  if (i in [0..4, 7..9]) and (not ValidateAndSave) then Exit;
  if FCurView.Grid.CanFocus then FCurView.Grid.SetFocus;

  case i of
    0: FCurView.DataSetProc.First;
    1: FCurView.DataSetProc.Prior;
    2: FCurView.DataSetProc.Next;
    3: FCurView.DataSetProc.Last;
    4: FCurView.DataSetProc.Append;
    5: FCurView.DataSetProc.Edit;
    6: FCurView.DataSetProc.Delete;
    7: FCurView.DataSetProc.Refresh;
    8:
      if TemplatesMnu.Items.Count = 1 then
        TemplatesMnu.Items[0].Click
      else
        TemplatesMnu.Popup;
    9: FCurView.DataSetProc.OpenFilter(0);
    10:
      begin
        FCurView.DataSetProc.ForceChangeFields(0);
        if FCurView.Form.ConfirmCancelEditing and
          	FCurView.DataSetProc.AnyDataSetModified(0) {MasterSet.Modified} then
        begin
        	if MessageDlg(rsWarning, rsConfirmCancelEditMsg, mtConfirmation,
          	[mbYes, mbNo], 0) <> mrYes then Exit;
        end;
      	FCurView.DataSetProc.MasterSet.Cancel;
      end;
    11:
      begin
        FCurView.DataSetProc.ForceChangeFields(0);
        if FCurView.Form.ConfirmSaveRecord and
	        FCurView.DataSetProc.AnyDataSetModified(0) {MasterSet.Modified} then
        begin
        	if MessageDlg(rsWarning, rsConfirmSaveMsg, mtConfirmation,
          	[mbYes, mbNo], 0) <> mrYes then Exit;
        end;
        if FCurView.DataSetProc.Validate(0, False) then
	        SaveCurrentDataSet;
      end;
  end;
end;

procedure TMainFr.OpenOrFindTab(FmId: Integer);
var
  Tab: TTabSheet;
begin
  Tab := FindTab(FmId);
  if Tab = nil then
  begin
    OpenTab(FmId);
    Tab := PageControl1.Pages[PageControl1.PageCount - 1];
    UpdateMenu;
    UpdateDataMenu;
  end;

  PageControl1.ActivePage := Tab;
  if PageControl1.PageCount = 1 then
    PageControl1Change(PageControl1);
  UpdateMenu;
  UpdateDataMenu;
end;

procedure TMainFr.OpenTab(FmId: Integer);
var
  Tb: TTabSheet;
  Fm: TdxForm;
begin
  // !!! Доступ
  if UserMan.CheckFmVisible(FmId) = False then Exit;
  //
  Tb := PageControl1.AddTabSheet;
  try

  with TFormView.Create(Tb) do
  begin
    Parent := Tb;
    Align := alClient;
    Visible := True;
    DataSetProc.GotoEnable := True;
    DataSetProc.OnStateChange:=@StateChange;
    BindForm(FmId, False, vtDefault);
    Tb.Caption := Form.GetRecordsCaption;
  end;
  Tb.PopupMenu := EmptyMnu;    // Чтобы меню "Закрыть" открывалось только на вкладках

  except
    on E: Exception do
    begin
      Fm := FormMan.FindForm(FmId);
      ErrMsgFmt(rsErrorCreatingTab, [Fm.FormCaption, ExceptionToString(E, True, False)]);
      {if ScriptLastError.ExObj = E then
        ErrMsg(Format(rsErrorCreatingTab, [Fm.FormCaption]) +
          LineEnding + LineEnding + ScriptLastErrorToString)
      else
        ErrMsg(Format(rsErrorCreatingTab, [Fm.FormCaption]) +
          LineEnding + LineEnding + E.Message); }
      CloseTab(Tb);
      if MainFm.SelectedFormId = FmId then MainFm.SelectedFormId := 0;
    end;
  end;
end;

procedure TMainFr.OpenTabs;
var
  SL: TStringList;
  i: Integer;
  Fm: TdxForm;
  Intf: TdxIntf;
  L: TdxTabList;
begin
  Intf := UserMan.GetIntf;
  if Intf = nil then
  begin
    SL := TStringList.Create;
    FormMan.SortFormsByIndex(SL);
    for i := 0 to SL.Count - 1 do
    begin
      Fm := TdxForm(SL.Objects[i]);
      if Fm.AutoOpen then OpenTab(Fm.Id);
    end;
    SL.Free;

    {SL := TStringList.Create;
    FormMan.FormsToList(SL);
    for i := 0 to SL.Count - 1 do
    begin
      Fm := TdxForm(SL.Objects[i]);
      if Fm.AutoOpen then
        OpenTab(Fm.Id);
    end;
    SL.Free;     }
  end
  else
  begin
    L := Intf.Tabs;
    for i := 0 to L.Count - 1 do
    begin
      Fm := FormMan.FindForm(L[i]);
      if Fm <> nil then OpenTab(Fm.Id);
    end;
  end;
end;

procedure TMainFr.UpdateToolButtonState;
var
  DS: TSQLQuery;
  DSR: TDataSetRec;
  HasVw, bEdit, bNotEdit, bHasRec, nsf, bNoBOF, bNoEOF, hf: Boolean;
begin
  HasVw := FCurView <> nil;

  if HasVw then
  begin
    DSR := FCurView.DataSetProc.DataSets[0]^;
    DS := DSR.DataSet;

    bEdit := DS.State in [dsInsert, dsEdit];
    bNotEdit := not bEdit;
    bHasRec := DS.RecordCount > 0;
    nsf := DSR.Form.ViewType <> vtSimpleForm;
    bNoBOF := not DS.BOF;
    bNoEOF := not DS.EOF;
    hf := DSR.HasFields;

    if DSR.CanEdit then
    begin
      EditBn.ImageIndex := 0;
      EditBn.Hint := rsEdit;
    end
    else
    begin
      EditBn.ImageIndex := 17;
      EditBn.Hint := rsLook;
    end;
  end;

  MoveFirstBn.Enabled := HasVw and bNoBOF and nsf and hf;
  MovePriorBn.Enabled := HasVw and bNoBOF and nsf and hf;
  MoveNextBn.Enabled := HasVw and bNoEOF and nsf and hf;
  MoveLastBn.Enabled := HasVw and bNoEOF and nsf and hf;
  AppendBn.Enabled := HasVw and DSR.Adding and bNotEdit and nsf and hf;
  EditBn.Enabled := HasVw and bNotEdit and bHasRec and nsf and hf;
  CancelBn.Enabled := HasVw and bEdit and nsf and hf;
  PostBn.Enabled := HasVw and bEdit and nsf and hf;
  DeleteBn.Enabled := HasVw and DSR.CanDelete and DSR.Editing and bNotEdit and bHasRec and nsf and hf;
  RefreshBn.Enabled := HasVw and nsf and hf;
  PrintBn.Enabled := HasVw and (bHasRec or (not nsf)) and (DSR.Form.Templates.Count > 0);
  FilterBn.Enabled := HasVw and nsf and hf;
  FindBn.Enabled := HasVw and bHasRec and nsf and hf;
  HelpBn.Visible := HasVw and (DSR.Form.HelpText.Count > 0);
  HelpBn.Enabled := HelpBn.Visible;
end;

procedure TMainFr.UpdateStatusBar;
var
  S: String;
begin
  StatusBar1.Panels[0].Text := DBase.Database;
  if (FCurView <> nil) and (FCurView.Form.ViewType <> vtSimpleForm) then
  begin
    with  FCurView.DataSetProc.MasterSet do
    begin
      case State of
        dsInsert: S := rsStatusInsert;
        dsEdit: S := rsStatusEdit;
        dsBrowse: S := rsStatusBrowse;
      end;
      StatusBar1.Panels[1].Text := S;
      StatusBar1.Panels[2].Text := IntToStr(RecNo) + ' / ' + IntToStr(RecordCount);
      if FilterBn.Enabled and (FCurView.DataSetProc.IsFilterSet) then
      	StatusBar1.Panels[3].Text := '1'
      else
      	StatusBar1.Panels[3].Text := '';
    end;
  end
  else
  begin
    StatusBar1.Panels[1].Text := '-';
		StatusBar1.Panels[2].Text := '-';
    StatusBar1.Panels[3].Text := '-';
  end;
end;

procedure TMainFr.FillDataMenu;
var
  DMnu: TMenuItem;
  SL: TStringList;
  MI: TMenuItem;
  Fm: TdxForm;
  i: Integer;
begin
  DMnu := MainFm.DataMnu;
  SL := TStringList.Create;
  FormMan.FormsToList(SL);
  for i := 0 to SL.Count - 1 do
  begin
    Fm := TdxForm(SL.Objects[i]);
    // !!! Доступ
    if UserMan.CheckFmVisible(Fm.Id) = False then Continue;
    //
    MI := TMenuItem.Create(MainFm.MainMenu1);
    MI.Caption:=Fm.GetRecordsCaption;
    MI.Tag := Fm.Id;
    MI.OnClick:=@DataMenuClick;
    DMnu.Add(MI);
  end;
  SL.Free;
end;

procedure TMainFr.FillReportMenu;
var
  SL: TStringList;
  i: Integer;
  RD: TReportData;
  Mnu, MI: TMenuItem;
begin
  SL := TStringList.Create;
  ReportMan.GetReports(SL);
  Mnu := MainFm.ReportMnu;
  for i := 0 to SL.Count - 1 do
  begin
    RD := TReportData(SL.Objects[i]);
    // !!! Доступ
    if UserMan.CheckRpVisible(RD.Id) = False then Continue;
    //
    MI := TMenuItem.Create(MainFm.MainMenu1);
    MI.Caption:=RD.Name;
    MI.Tag := RD.Id;
    MI.OnClick:=@ReportMenuClick;
    Mnu.Add(MI);
  end;
  SL.Free;
end;

procedure _BuildMenu(PMI: TMenuItem; dxMI: TdxMenuItem; Handler: TNotifyEvent);
var
  MI: TMenuItem;
  Fm: TdxForm;
  S: String;
  RD: TReportData;
  i, j: Integer;
begin
  S := '';
  case dxMI.Kind of
    miDiv: S := '-';
    miMenu: S := dxMI.Caption;
    miForm:
      begin
        // !!! Доступ
        if UserMan.CheckFmVisible(dxMI.Id) = False then Exit;

        Fm := FormMan.FindForm(dxMI.Id);
        if Fm <> nil then
          S := Fm.GetRecordsCaption
        else Exit;
      end;
    miReport:
      begin
        // !!! Доступ
        if UserMan.CheckRpVisible(dxMI.Id) = False then Exit;

        RD := ReportMan.FindReport(dxMI.Id);
        if RD <> nil then
          S := RD.Name
        else Exit;
      end;
  end;
  MI := TMenuItem.Create(MainFm.MainMenu1);
  MI.Caption := S;
  if PMI = nil then
    MainFm.MainMenu1.Items.Insert(1, MI)
  else
    PMI.Add(MI);
  MI.Tag:=PtrInt(dxMI);
  if dxMI.Kind = miForm then
    SetMenuItemImage(MI, 'form16')
  else if dxMI.Kind = miReport then
    SetMenuItemImage(MI, 'grid16');
  MI.OnClick:=Handler;
  for i := 0 to dxMI.Items.Count - 1 do
    _BuildMenu(MI, dxMI.Items[i], Handler);
  // Корректировка меню
  for j := MI.Count - 1 downto 1 do
  begin
    if (MI.Items[j].Caption = '-') and (MI.Items[j - 1].Caption = '-') then
      MI.Items[j].Free;
  end;
  if (MI.Count > 0) and (MI.Items[MI.Count - 1].Caption = '-') then
    MI.Items[MI.Count - 1].Free;
  if (MI.Count > 0) and (MI.Items[0].Caption = '-') then
    MI.Items[0].Free;
  for j := MI.Count - 1 downto 0 do
  begin
    if (TdxMenuItem(MI.Items[j].Tag).Kind = miMenu) and (MI.Items[j].Count = 0) then
      Mi.Items[j].Free;
  end;
  if (dxMI.Kind = miMenu) and (MI.Count = 0) then MI.Free;
end;

procedure TMainFr.BuildMenu;
var
  Intf: TdxIntf;
  i: Integer;
begin
  Intf := UserMan.GetIntf;
  if Intf = nil then
  begin
    FillDataMenu;
    FillReportMenu;
  end
  else
    for i := Intf.Menu.Count - 1 downto 0 do
      _BuildMenu(nil, Intf.Menu[i], @MenuItemClick);
  MainFm.DataMnu.Visible := MainFm.DataMnu.Count > 0;
  MainFm.ReportMnu.Visible := MainFm.ReportMnu.Count > 0;
end;

function TMainFr.FindTab(FmId: Integer): TTabSheet;
var
  i: Integer;
  Tab: TTabSheet;
begin
  Result := nil;
  for i := 0 to PageControl1.PageCount - 1 do
  begin
    Tab := PageControl1.Pages[i];
    if TFormView(Tab.Controls[0]).Form.Id = FmId then
      Exit(Tab);
  end;
end;

procedure TMainFr.ClearMnu(Mnu: TPopupMenu);
begin
  while Mnu.Items.Count > 0 do Mnu.Items[0].Free;
end;

procedure TMainFr.UpdateDataMenu;
var
  i: Integer;
  MI: TMenuItem;
begin
  for i := 0 to MainFm.DataMnu.Count - 1 do
  begin
    MI := MainFm.DataMnu.Items[i];
    MI.Checked:=FindTab(MI.Tag) <> nil;
  end;
end;

function TMainFr.ValidateAndSave: Boolean;
var
  DS: TSQLQuery;
begin
  Result := True;
  if CurView = nil then Exit;
  if CurView.Form.ViewType = vtSimpleForm then Exit;

  CurView.DataSetProc.ForceChangeFields(0);
  DS := CurView.DataSetProc.MasterSet;
  if CurView.Form.ConfirmAutoSaveRecord and CurView.DataSetProc.AnyDataSetModified(0) {DS.Modified} then
  begin
    case MessageDlg(rsWarning, rsConfirmAutoSaveMsg,	mtConfirmation,
    	[mbYes, mbNo, mbCancel], 0) of
      mrYes: ;
      mrNo: begin
          		DS.Cancel;
          		Exit;
		        end;
      else Exit(False);
    end;
    {if MessageDlg(rsWarning, rsConfirmAutoSaveMsg,	mtConfirmation,
    	[mbYes, mbNo], 0) = mrNo then Exit(False);}
  end;
  Result := CurView.DataSetProc.Validate(0, False);
  if Result then SaveCurrentDataSet;
end;

function TMainFr.OpenTab2(const FormName: String; ViewType: TViewType
  ): TTabSheet;
var
  Tb: TTabSheet;
  Fm: TdxForm;
begin
  Fm := FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);
  if Fm.PId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormName]);

  Tb := PageControl1.AddTabSheet;
  Result := Tb;

  with TFormView.Create(Tb) do
  begin
    Parent := Tb;
    Align := alClient;
    Visible := True;
    DataSetProc.GotoEnable := True;
    DataSetProc.OnStateChange:=@StateChange;
    BindForm(Fm.Id, False, ViewType);
    Tb.Caption := Form.GetRecordsCaption;
  end;

  Tb.PopupMenu := EmptyMnu;    // Чтобы меню "Закрыть" открывалось только на вкладках
end;

procedure TMainFr.CloseAllTabs;
begin
  while PageControl1.PageCount > 0 do
    PageControl1.Pages[0].Free;
end;

procedure TMainFr.BuildTemplatesMenu(Fm: TdxForm);
var
  L: TStringList;
  i: Integer;
  S: String;
begin
  ClearMnu(TemplatesMnu);
  L := Fm.Templates;
  for i := 0 to L.Count - 1 do
  begin
    S := ExtractFileName(ChangeFileExt(L[i], ''));
    TemplatesMnu.Items.Add( CreateMenuItem(TemplatesMnu, S, i, 0, @TemplatesMnuHandler) );
  end;
end;

procedure TMainFr.BuildFilterMenu(Fm: TdxForm);
var
  SL: TStringListUtf8;
  i: Integer;
  DSP: TDataSetProcessor;
begin
  ClearMnu(FilterMnu);
  SL := TStringListUtf8.Create;
  DSP := FCurView.DataSetProc;

  // Здесь в Tag записывается индекс фильтра формы
  for i := 0 to Fm.Filters.Count - 1 do
    FilterMnu.Items.Add( CreateMenuItem(FilterMnu, Fm.Filters.Names[i], i, 0,
      @FilterMnuHandler) );

  for i := 1 to DSP.DataSetCount - 1 do
  	SL.AddObject(DSP.DataSets[i]^.Form.GetRecordsCaption, TObject(PtrInt(i)));
  SL.Sort;

  if (SL.Count > 0) and (FilterMnu.Items.Count > 0) then
    FilterMnu.Items.Add( CreateMenuItem(FilterMnu, '-', 0, 0, nil) );

  // Здесь в Tag записывается индекс DSR.
  for i := 0 to SL.Count - 1 do
    FilterMnu.Items.Add( CreateMenuItem(FilterMnu, SL[i],
      PtrInt(SL.Objects[i]), 0, @TblFilterMnuHandler) );

  if (SL.Count > 0) or (FilterMnu.Items.Count > 0) then
  begin
    FilterBn.Style:=tbsDropDown;
    FilterBn.DropdownMenu := FilterMnu;
  end
  else
  begin
    FilterBn.Style := tbsButton;
    FilterBn.DropdownMenu := nil;
  end;
  FilterMnu.OnPopup:=@FilterMnuPopup;

  SL.Free;
end;

procedure TMainFr.DoOnResize;
begin
  inherited DoOnResize;
  StatusBar1.Panels[0].Width:=ClientWidth - 280;
end;

procedure TMainFr.CloseTab(Pg: TTabSheet);
var
  IsCurrent: Boolean;
begin
  if Pg = nil then Exit;
  //IsCurrent := PageControl1.ActivePage = Pg;
  // Когда удаляется закладка, Pages.OnChange срабатывает в то время как FormView уже удален.
  // Поэтому вызываем OnChange до фактического удаления закладки.
  Pg.PageControl := nil;
  //
  Pg.Free;
  //if not IsCurrent then Exit;

  UpdateDataMenu;
  UpdateMenu;
  if PageControl1.PageCount = 0 then
  begin
    FCurView := nil;
    DataSetStateChange;
  end;
end;

procedure TMainFr.SaveCurrentDataSet;
begin
  if FCurView <> nil then
    FCurView.DataSetProc.Post;
end;

function TMainFr.GotoRec(FormId, RecId: Integer): Boolean;
var
  T: TTabSheet;
begin
  Result := False;
  // !!! Доступ
  if not UserMan.CheckFmVisible(FormId) then Exit;
  //
  T := FindTab(FormId);
  if T = nil then
  begin
    OpenTab(FormId);
    T := PageControl1.Pages[PageControl1.PageCount - 1];
    UpdateDataMenu;
    UpdateMenu;
  end;
  PageControl1.ActivePage := T;
  if RecId > 0 then
    FCurView.DataSetProc.MasterSet.Locate('id', RecId, []);
  Result := T = PageControl1.ActivePage;
end;

procedure TMainFr.GotoRec(FormId: Integer; const FieldName, FieldValue: String);
var
  T: TTabSheet;
begin
  // !!! Доступ
  if UserMan.CheckFmVisible(FormId) = False then Exit;
  //
  T := FindTab(FormId);
  if T = nil then
  begin
    OpenTab(FormId);
    T := PageControl1.Pages[PageControl1.PageCount - 1];
    UpdateDataMenu;
    UpdateMenu;
  end;
  PageControl1.ActivePage := T;
  FCurView.DataSetProc.MasterSet.Locate(FieldName, FieldValue, [loCaseInsensitive]);
end;

constructor TMainFr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MoveFirstBn.Hint := rsMoveFirst;
  MovePriorBn.Hint := rsMovePrevious;
  MoveNextBn.Hint := rsMoveNext;
  MoveLastBn.Hint := rsMoveLast;
  AppendBn.Hint := rsAppend;
  EditBn.Hint := rsEdit;
  DeleteBn.Hint := rsDelete;
  RefreshBn.Hint := rsRefresh;
  PrintBn.Hint := rsPrint;
  FilterBn.Hint := rsFilterBnHint;
  FindBn.Hint := rsSearchBn;
  CancelBn.Hint := rsCancelChange;
  PostBn.Hint := rsSaveChanges;
  HelpBn.Hint := rsHelp;
  CloseTabMnu.Caption:=rsClose;
  SetMenuItemImage(CloseTabMnu, 'delete16');
  ImageList2.AddLazarusResource('filter16');

  VarList := TVarList.Create;
  MainFr := Self;
  MainModule := TRunScript.Create;
  ExtRunMan := TExtRunManager.Create;
end;

destructor TMainFr.Destroy;
begin
  MainFm.SelectedFormId:=0;
  if FCurView <> nil then
  	MainFm.SelectedFormId:=FCurView.Form.Id;
  try
    CloseAllTabs;
    if MainFm.OnDatabaseClose <> nil then
      MainFm.OnDatabaseClose(MainFm);
    MainModule.TryRunProc('database_close', []);
  except
    on E: Exception do
      ErrMsg(rsErrorCloseDBMsg + ExceptionToString(E, True, False), True, 'CloseDatabase');
  end;
  MainFm.RestoreMainMenu;
  MainFm.ClearEventHandlers;
  MainFm.RestoreFormatSettings;
  MainFm.Params.Clear;

  //FreeAndNil(CompilerErrorsDlg);
  FreeAndNil(DebugScriptFm);
  FreeAndNil(ExtRunMan);
  FreeAndNil(MainModule);
  ClearMnu(TemplatesMnu);
  VarList.Free;
  MainFr := nil;
  inherited Destroy;
end;

procedure TMainFr.Init;
begin
  MainFm.EnableDropFiles(not UserMan.IsUser);
  BuildMenu;
  MainFm.ReportMnu.Visible := MainFm.ReportMnu.Count > 0;

  if AppConfig.CacheLoaded and UserMan.IsUser then
    ScriptMan.ParseExprModules
  else if DXMain.AllowDynamicForms then
  begin
    ScriptMan.CompileMain;
    ScriptMan.CompileExpr;
  end
  else
    ScriptMan.CompileAll;

  if ScriptMan.HasErrors then
    ShowCompilerErrors
  else if CanCache and not AppConfig.CacheLoaded then
    SaveMetaToCache;

  ExtRunMan.Init;
  MainModule.SD := ScriptMan.FindScriptByName('Main');
  MainModule.LoadBin;
  MainModule.BindVars;
  MainModule.TryRunProc('database_open', []);
  DXMain.RunActions;

  OpenTabs;
  UpdateDataMenu;
  UpdateMenu;

  if MainFm.SelectedFormId > 0 then
  begin
  	OpenOrFindTab(MainFm.SelectedFormId);
    if PageControl1.ActivePageIndex = 0 then
    	PageControl1.OnChange(PageControl1);
  end
  else
    with PageControl1 do
      if PageCount > 0 then
      begin
        if PageControl1.OnChange <> nil then
        begin
          // При первом запуске перед открытием набора нужна маленькая задержка,
          // чтобы не двоились записи в табличной части (баг от archs8 от 21.03.2017).
          if MainFm.IsLoadApp then
	          Timer1.Enabled := True
          else
      		  PageControl1.OnChange(PageControl1);
        end;
      end
      else
        DataSetStateChange;
end;

procedure TMainFr.Done;
begin
 { MainFm.SelectedFormId:=0;
  if FCurView <> nil then
  	MainFm.SelectedFormId:=FCurView.Form.Id;
  FreeAndNil(CompilerErrorsDlg); }
end;

procedure TMainFr.DataSetStateChange;
begin
  UpdateToolButtonState;
  UpdatestatusBar;
end;

procedure TMainFr.UpdateMenu;
var
  i: Integer;

  procedure _UpdateMenu(MI: TMenuItem);
  var
    dxMI: TdxMenuItem;
    j: Integer;
    b: Boolean;
  begin
    dxMI := TdxMenuItem(MI.Tag);
    if dxMI = nil then Exit;
    if dxMI.Kind = miForm then
    begin
      b := FindTab(dxMI.Id) <> nil;

      // В линукс нужно делать так, чтобы флажок устанавливался/убирался
      // правильно.
      MI.Checked := not b;
      MI.Checked := b;
      //
    end;
    if dxMI.Kind = miMenu then
      for j := 0 to MI.Count - 1 do
        _UpdateMenu(MI.Items[j]);
  end;

begin
  for i := 1 to MainFm.MainMenu1.Items.Count - 5 do
    _UpdateMenu(MainFm.MainMenu1.Items[i]);
end;

procedure TMainFr.RefreshAllLookups(TId: Integer);
var
  i: Integer;
  Vw: TFormView;
begin
  for i := 0 to PageControl1.PageCount - 1 do
  begin
    Vw := TFormView(PageControl1.Pages[i].Controls[0]);
    Vw.DataSetProc.RefreshLookups(TId);
  end;
end;

procedure TMainFr.UnBindForms;
begin
  {for i := 0 to PageControl1.PageCount - 1 do
    TFormView(PageControl1.Pages[i].Controls[0]).DataSetProc.UnBind;   }
end;

end.

