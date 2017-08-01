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
unit MainFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, ComCtrls, Menus, dxctrls,
  formview, sqldb, strconsts, DXReports, DatasetProcessor, Dialogs, ExtCtrls;

type

  { TMainFr }

  TMainFr = class(TFrame)
    ImageList1: TImageList;
    CloseTabMnu: TMenuItem;
    ImageList2: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PageControl1: TPageControl;
    EmptyMnu: TPopupMenu;
    FilterMnu: TPopupMenu;
    AppendMnu: TPopupMenu;
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
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure StateChange(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
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
    procedure CloseAllTabs;
    procedure DataSetStateChange;
  protected
    procedure DoOnResize; override;
  public
    procedure CloseTab(Pg: TTabSheet);
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure Done;
    procedure RefreshAllLookups(TId: Integer);
    procedure UnBindForms;
    procedure SaveCurrentDataSet;
    procedure GotoRec(FormId, RecId: Integer);
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
  dxusers, ExprFuncs, helpviewform, scriptmanager, outputform, mydialogs,
  debugscriptform, mytypes;

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
          Open;
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

procedure TMainFr.CloseTabMnuClick(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  CloseTab(PageControl1.ActivePage);
end;

procedure TMainFr.FilterMnuHandler(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  FCurView.DataSetProc.ApplyFilter(FCurView.Form.Id, TMenuItem(Sender).Tag);
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
    Mnu.Items[i].Checked := DSP.DataSets[Mnu.Items[i].Tag]^.TblFilterSet;
end;

procedure TMainFr.HelpBnClick(Sender: TObject);
begin
  HelpViewFm.ShowForm(FCurView.Form.HelpText.Text);
end;

procedure TMainFr.MenuItem1Click(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  FCurView.DataSetProc.Duplicate;
end;

procedure TMainFr.MenuItem2Click(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  FCurView.DataSetProc.DuplicateAll;
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
    ShowReportWindow(dxMi.Id);
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

procedure TMainFr.TblFilterMnuHandler(Sender: TObject);
begin
  //if not FCurView.DataSetProc.Validate(0) then Exit;
  if not ValidateAndSave then Exit;
  FCurView.DataSetProc.OpenFilter(TMenuItem(Sender).Tag);
end;

procedure TMainFr.FindBnClick(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  FindFrm.ShowForm(FCurView.Form, FCurView.DataSetProc.MasterSet);
  FCurView.DataSetProc.RefreshCurRecord;
end;

procedure TMainFr.ToolButtonClick(Sender: TObject);
var
  i: PtrInt;
begin
  i := TComponent(Sender).Tag;
  if (i in [0..4, 7..9]) and (not ValidateAndSave) then Exit;

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
          	FCurView.DataSetProc.MasterSet.Modified then
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
	        FCurView.DataSetProc.MasterSet.Modified then
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
begin
  // !!! Доступ
  if UserMan.CheckFmVisible(FmId) = False then Exit;
  //
  Tb := PageControl1.AddTabSheet;
  with TFormView.Create(Tb) do
  begin
    Parent := Tb;
    Align := alClient;
    Visible := True;
    DataSetProc.GotoEnable := True;
    DataSetProc.OnStateChange:=@StateChange;
    BindForm(FmId, False, vtDefault);
    Tb.Caption := Form.FormCaption;
  end;
  Tb.PopupMenu := EmptyMnu;    // Чтобы меню "Закрыть" открывалось только на вкладках
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
    FormMan.FormsToList(SL);
    for i := 0 to SL.Count - 1 do
    begin
      Fm := TdxForm(SL.Objects[i]);
      if Fm.AutoOpen then
        OpenTab(Fm.Id);
    end;
    SL.Free;
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
  DSP: TDataSetProcessor;
  nsf, bbrowse, bedit, bcount: Boolean;
begin
  DS := nil; DSP := nil;
  if FCurView <> nil then
  begin
    DSP := FCurView.DataSetProc;
    DS := DSP.MasterSet;
  end;
  nsf := (DSP <> nil) and (DSP.Form.ViewType <> vtSimpleForm);
  bbrowse := (DS <> nil) and (not (DS.State in [dsInsert, dsEdit]));
  bedit := (DS <> nil) and (DS.State in [dsInsert, dsEdit]);
  bcount := (DS <> nil) and (DS.RecordCount > 0);

  // !!! Доступ
  MoveFirstBn.Enabled:=(DS <> nil) and (not DS.BOF) and nsf;
  MovePriorBn.Enabled:=(DS <> nil) and (not DS.BOF) and nsf;
  MoveNextBn.Enabled := (DS <> nil) and (not DS.Eof) and nsf;
  MoveLastBn.Enabled := (DS <> nil) and (not DS.Eof) and nsf;
  AppendBn.Enabled:=(DS <> nil) and (DSP.CanAdd) and nsf and bbrowse;
  MenuItem1.Enabled := (DS <> nil) and DSP.CanAdd and DSP.CanEdit and bcount and bbrowse;
  MenuItem2.Enabled := MenuItem1.Enabled;
  EditBn.Enabled:=(DS <> nil) and nsf and bbrowse and bcount;
  if (DS = nil) or DSP.CanEdit then
  begin
    EditBn.Hint:=rsEdit;
    EditBn.ImageIndex:=0;
  end
  else
  begin
    EditBn.Hint := rsLook;
    EditBn.ImageIndex := 17;
  end;
  CancelBn.Enabled:=(DS <> nil) and bedit and nsf;
  PostBn.Enabled:=(DS <> nil) and bedit and nsf;
  DeleteBn.Enabled:=(DS <> nil) and bcount and (DSP.CanDelete) and (DSP.CanEdit) and nsf and bbrowse;
  RefreshBn.Enabled:=(DS <> nil) and nsf;
  PrintBn.Enabled:=(DS <> nil) and (bcount or bedit) and (FCurView.Form.Templates.Count > 0);
  FilterBn.Enabled := (DS <> nil)  and nsf;
  {if FilterBn.Enabled and (DS <> nil) and (DSP.IsFilterSet) then
  	FilterBn.ImageIndex:=19
  else
    FilterBn.ImageIndex:=3;   }
  FindBn.Enabled:=(DS <> nil) and nsf;
  HelpBn.Visible:=(DS <> nil) and (FCurView.Form.HelpText.Count > 0);
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
    MI := TMenuItem.Create(DMnu);
    MI.Caption:=Fm.FormCaption;
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
    MI := TMenuItem.Create(Mnu);
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
          S := Fm.FormCaption
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
  if CurView.Form.ConfirmAutoSaveRecord and DS.Modified then
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
begin
  Tb := PageControl1.AddTabSheet;
  Result := Tb;
  with TFormView.CreateView(Tb, FormName, ViewType) do
  begin
    Parent := Tb;
    Align := alClient;
    Visible := True;
    DataSetProc.GotoEnable := True;
    DataSetProc.OnStateChange:=@StateChange;
    Tb.Caption := Form.FormCaption;
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
      @FilterMnuHandler, '') );

  for i := 1 to DSP.DataSetCount - 1 do
  	SL.AddObject(DSP.DataSets[i]^.Form.FormCaption, TObject(i));
  SL.Sort;

  if (SL.Count > 0) and (FilterMnu.Items.Count > 0) then
    FilterMnu.Items.Add( CreateMenuItem(FilterMnu, '-', 0, 0, nil, '') );

  // Здесь в Tag записывается индекс DSR.
  for i := 0 to SL.Count - 1 do
    FilterMnu.Items.Add( CreateMenuItem(FilterMnu, SL[i],
      Integer(SL.Objects[i]), 0, @TblFilterMnuHandler, '') );

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
  IsCurrent := PageControl1.ActivePage = Pg;
  Pg.Free;
  if not IsCurrent then Exit;

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

procedure TMainFr.GotoRec(FormId, RecId: Integer);
var
  T: TTabSheet;
begin
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
  FilterBn.Hint := rsFilter;
  FindBn.Hint := rsSearchBn;
  CancelBn.Hint := rsCancelChange;
  PostBn.Hint := rsSaveChanges;
  HelpBn.Hint := rsHelp;
  MenuItem1.Caption := rsDuplicate;
  MenuItem2.Caption:=rsDuplicateAll;
  CloseTabMnu.Caption:=rsClose;
  SetMenuItemImage(CloseTabMnu, 'delete16');
  ImageList2.AddLazarusResource('filter16');

  FormMan := TFormManager.Create;
  VarList := TVarList.Create;
  MainFr := Self;
  MainModule := TRunScript.Create;
  ExprModule := TRunScript.Create;
  ExprModule.Exec.OnException:=nil;
end;

destructor TMainFr.Destroy;
begin
  CloseAllTabs;
  MainModule.TryRunProc('database_close', []);
  MainFm.OnCreateForm:=nil;
  MainFm.OnDestroyForm:=nil;
  FreeAndNil(ExprModule);
  FreeAndNil(MainModule);
  ClearMnu(TemplatesMnu);
  VarList.Free;
  FreeAndNil(FormMan);
  MainFr := nil;
  inherited Destroy;
end;

procedure TMainFr.Init;
begin
  FormMan.LoadFromDb;
  ScriptMan.LoadFromDB;
  BuildMenu;
  MainFm.ReportMnu.Visible := MainFm.ReportMnu.Count > 0;

  OutputFm.Clear;
  ScriptMan.CompileAll;

  ExprModule.SD := ScriptMan.ExprModule;
  ExprModule.LoadBin;
  ExprModule.BindVars;
  MainModule.SD := ScriptMan.FindScriptByName('Main');
  MainModule.LoadBin;
  MainModule.BindVars;
  MainModule.TryRunProc('database_open', []);

  OpenTabs;
  UpdateDataMenu;
  UpdateMenu;
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
      {FCurView := TFormView(Pages[0].Controls[0]);
      FCurView.DataSetProc.Open;
      BuildTemplatesMenu(FCurView.Form);
      BuildFilterMenu(FCurView.Form); }
    end
    else
      DataSetStateChange;

  if ScriptMan.HasErrors then
    ShowCompilerErrors;
end;

procedure TMainFr.Done;
begin
  FreeAndNil(DebugScriptFm);
  //SaveCurrentDataSet;
  //UnBindForms;
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
  begin
    dxMI := TdxMenuItem(MI.Tag);
    if dxMI = nil then Exit;
    if dxMI.Kind = miForm then
      MI.Checked:=FindTab(dxMI.Id) <> nil;
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

