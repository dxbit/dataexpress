unit ReportsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Menus, ExtCtrls, Buttons, strconsts, DXReports, reportmanager,
  DxCtrls, LclType;

type

  { TReportsFm }

  TReportsFm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    BitBtn14: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    ButtonPanel1: TButtonPanel;
    List: TListBox;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    procedure ButtonClick(Sender: TObject);
    //procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListSelectionChange(Sender: TObject; User: boolean);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
  private
    { private declarations }
    //FInDesigner: Boolean;
    FModified, FInDesigner: Boolean;
    FDelRps: TList;
    //FCloneReportMan: TReportManager;
    procedure FillReports;
    //procedure CheckQueries;
    procedure SetControlState;
    //procedure ShowSelectionForm(ARD: TReportData);
  public
    { public declarations }
    function ShowForm(InDesigner: Boolean; SelReport: TReportData = nil): Integer;
    property DelRps: TList read FDelRps;
  end;

var
  ReportsFm: TReportsFm;

function ShowReportsForm(InDesigner: Boolean; SelReport: TReportData = nil): Integer;

implementation

uses
  apputils, ReportForm, newgridform, rpstyleform,
  helpmanager, inputform, QueryCalcForm, exprform, totalsform, helptextform,
  querycoloringform, propdialogs, stringsform, calcfieldsform, dbengine,
  dxusers, dxmains, findexprform, sqlform;

function ShowReportsForm(InDesigner: Boolean; SelReport: TReportData): Integer;
begin
  if ReportsFm = nil then
  	ReportsFm := TReportsFm.Create(Application);
  Result := ReportsFm.ShowForm(InDesigner, SelReport);
end;

{$R *.lfm}

{ TReportsFm }

procedure TReportsFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
  SetControlState;
end;

procedure TReportsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('reports');
end;

procedure TReportsFm.ListDblClick(Sender: TObject);
begin
  if List.ItemIndex >= 0 then
    MenuItem5.Click;
end;

procedure TReportsFm.ListSelectionChange(Sender: TObject; User: boolean);
begin
  SetControlState;
end;

procedure TReportsFm.MenuItem10Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  if ShowCalcForm(RD, nil, nil, FInDesigner) = mrOk then FModified := True;
end;

procedure TReportsFm.MenuItem11Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  ShowTotalsForm(RD);
end;

procedure TReportsFm.MenuItem12Click(Sender: TObject);
var
  RD: TReportData;
  OldText: String;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  OldText := RD.HelpText;
  RD.HelpText:=Trim(ShowHelpTextForm(RD.HelpText));
  if OldText <> RD.HelpText then FModified := True;
end;

procedure TReportsFm.MenuItem13Click(Sender: TObject);
var
  SrcRD, RD: TReportData;
  MS: TMemoryStream;
  OldId: Integer;
  S: String;
begin
  SrcRD := TReportData(List.Items.Objects[List.ItemIndex]);
  S := SrcRD.Name;
  if InputStr(rsNewReport, rsReportName, 'queryname', S, True) then
  begin
    RD := ReportMan.CreateNewReport;
    OldId := RD.Id;

    MS := TMemoryStream.Create;
    SrcRD.SaveToStream(MS);
    MS.Position:=0;
    RD.LoadFromStream(MS);
    MS.Free;

    RD.Id := OldId;
    RD.Name := S;
    List.Items.AddObject(S, RD);
    List.ItemIndex := List.Count - 1;
    if ShowReportForm(RD, nil, nil, FInDesigner) = mrOk then FModified := True;
    //ShowSelectionForm(RD);
    FModified := True;
  end;
end;

procedure TReportsFm.MenuItem14Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  if ShowQueryColoringForm(RD, nil) = mrOk then FModified := True;
end;

procedure TReportsFm.MenuItem15Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  ShowStringsForm(rsTemplates, 'reporttemplates', RD.Templates);
end;

procedure TReportsFm.MenuItem16Click(Sender: TObject);
var
  RD: TReportData;
  S: String;
  Fm: TdxForm;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  if not RD.IsEmpty then
  begin
    Fm := CreateReportForm(RD, S);
    if ShowReportPrintFieldsForm(Fm) = mrOk then
      RD.PrintFields.Assign(Fm.CalcFields);
    Fm.Free;
  end
  else
    ErrMsg(rsSourceNotSel);
end;

procedure TReportsFm.MenuItem1Click(Sender: TObject);
var
  S: String;
  RD: TReportData;
begin
  S := '';
  //if InputStr(rsNewReport, rsReportName, 'queryname', S, True) then
  if ReportNameDlg(rsNewReport, S, nil) = mrOk then
  begin
    RD := ReportMan.CreateNewReport;
    RD.Name := S;
    List.Items.AddObject(S, RD);
    List.ItemIndex := List.Count - 1;
    if ShowReportForm(RD, nil, nil, FInDesigner) = mrOk then FModified := True;
    //ShowSelectionForm(RD);
    FModified := True;
  end;
end;

procedure TReportsFm.MenuItem2Click(Sender: TObject);
var
  RD: TReportData;
  i: Integer;
begin
  if not ConfirmDelete then Exit;
  i := List.ItemIndex;
  RD := TReportData(List.Items.Objects[i]);

  if CheckExistsInActions(nil, renReport, RD.Name) then Exit;

  FDelRps.Add(Pointer(RD.Id));
  ReportMan.DeleteReport(RD);
  List.Items.Delete(i);
  if i > List.Count - 1 then
    i := List.Count - 1;
  List.ItemIndex := i;
  SetControlState;
  FModified := True;
end;

procedure TReportsFm.MenuItem4Click(Sender: TObject);
var
  RD: TReportData;
  i: Integer;
  S, OldName: String;
begin
  i := List.ItemIndex;
  RD := TReportData(List.Items.Objects[i]);
  S := RD.Name;
  //if InputStr(rsRenameReport, rsReportName, 'queryname', S, True) then
  OldName := S;
  if ReportNameDlg(rsRenameReport, S, RD) = mrOk then
  begin
    if OldName <> S then
    begin
      if FInDesigner then RenameInActions(nil, renReport, OldName, S)
      else if CheckExistsInActions(nil, renReport, OldName, LineEnding +
        rsCantRenameReportMsg) then Exit;
    end;
    RD.Name := S;
    List.Items[i] := S;

    FModified := True;
  end;
end;

procedure TReportsFm.MenuItem5Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  if ShowReportForm(RD, nil, nil, FInDesigner) = mrOk then FModified := True;
  //ShowSelectionForm(RD);
end;

procedure TReportsFm.MenuItem6Click(Sender: TObject);
var
  S: String;
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  S := RD.Filter;
  if ShowExprForm(etOutputFilter, nil, S, nil, nil, nil, RD) = mrOk then
  begin
    RD.Filter := S;
    FModified := True;
  end;
end;

procedure TReportsFm.MenuItem7Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  if ShowRpGridForm(nil, RD) = mrOk then FModified := True;
end;

procedure TReportsFm.MenuItem9Click(Sender: TObject);
var
  RD: TReportData;
begin
  RD := TReportData(List.Items.Objects[List.ItemIndex]);
  if ShowRpStyleForm(RD, List.Items, rkReport) = mrOk then FModified := True;
end;

procedure TReportsFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not FInDesigner then
  begin
    if (ModalResult = mrOk) and not DXMain.CanProjectSave then
      CanClose := False
    else if (ModalResult = mrCancel) and FModified then
      CanClose := MessageDlg(rsWarning, rsCancelChangesMsg, mtWarning, [mbYes,
        mbNo], 0) = mrYes;
  end;
end;

procedure TReportsFm.ButtonClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: MenuItem1.Click;
    1: MenuItem2.Click;
    2: MenuItem4.Click;
    3: MenuItem5.Click;
    5: MenuItem7.Click;
    7: MenuItem10.Click;
    6: MenuItem9.Click;
    8: MenuItem6.Click;
    9: MenuItem11.Click;
    10: MenuItem12.Click;
    11: MenuItem13.Click;
    12: MenuItem14.Click;
    13: MenuItem15.Click;
    14: MenuItem16.Click;
  end;
end;

procedure TReportsFm.FormCreate(Sender: TObject);
begin
  Caption := rsReports;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.CloseButton.Caption:=rsClose;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  MenuItem1.Caption := rsAppend;
  SetMenuItemImage(MenuItem1, 'add16');
  MenuItem2.Caption := rsDelete;
  SetMenuItemImage(MenuItem2, 'delete16');
  MenuItem4.Caption := rsRename;
  MenuItem5.Caption := rsSelection;
  SetMenuItemImage(MenuItem5, 'query16');
  MenuItem7.Caption := rsTable;
  SetMenuItemImage(MenuItem7, 'grid16');
  MenuItem9.Caption := rsCopyReportStyle;
  SetMenuItemImage(MenuItem9, 'copy16');
  MenuItem10.Caption := rsCalcFields;
  SetMenuItemImage(MenuItem10, 'sum16');
  MenuItem6.Caption := rsOutputFilter;
  SetMenuItemImage(MenuItem6, 'filter16');
  MenuItem11.Caption := rsTotals;
  MenuItem12.Caption := rsHelpText;
  MenuItem13.Caption:=rsCopy;
  MenuItem14.Caption := rsColoring;
  MenuItem15.Caption := rsTemplates;
  MenuItem16.Caption := rsWhenPrinting;
  BitBtn1.Caption := rsAppend;
  BitBtn6.Caption := rsDelete;
  BitBtn2.Caption := rsRename;
  BitBtn3.Caption := rsSelection;
  BitBtn5.Caption := rsTable;
  BitBtn7.Caption := rsCopyReportStyle;
  BitBtn8.Caption:= rsCalcFields;
  BitBtn9.Caption := rsOutputFilter;
  BitBtn10.Caption := rsTotals;
  BitBtn11.Caption := rsHelpText;
  BitBtn4.Caption := rsCopy;
  BitBtn12.Caption := rsColoring;
  BitBtn13.Caption := rsTemplates;
  BitBtn14.Caption := rsWhenPrinting;
  FDelRps := TList.Create;
end;

procedure TReportsFm.FormDestroy(Sender: TObject);
begin
  FDelRps.Free;
end;

procedure TReportsFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrClose;
end;

procedure TReportsFm.FillReports;
begin
  ReportMan.GetReports(List.Items);
end;

procedure TReportsFm.SetControlState;
var
  b: Boolean;
begin
  b := List.ItemIndex >= 0;
  MenuItem2.Enabled := b;
  MenuItem4.Enabled := b;
  MenuItem5.Enabled := b;
  MenuItem7.Enabled := b;
  MenuItem9.Enabled := b;
  MenuItem10.Enabled := b;
  MenuItem6.Enabled := b;
  MenuItem11.Enabled := b;
  MenuItem12.Enabled := b;
  MenuItem13.Enabled := b;
  MenuItem14.Enabled := b;
  MenuItem15.Enabled := b;
  MenuItem16.Enabled := b;

  BitBtn2.Enabled := b;
  BitBtn3.Enabled := b;
  BitBtn5.Enabled := b;
  BitBtn6.Enabled := b;
  BitBtn7.Enabled := b;
  BitBtn8.Enabled := b;
  BitBtn9.Enabled := b;
  BitBtn10.Enabled := b;
  BitBtn11.Enabled := b;
  BitBtn4.Enabled := b;
  BitBtn12.Enabled := b;
  BitBtn13.Enabled := b;
  BitBtn14.Enabled := b;
end;

{procedure TReportsFm.ShowSelectionForm(ARD: TReportData);
var
  OldSqlMode: Boolean;
begin
  while True do
  begin
    OldSqlMode := ARD.SqlMode;
    if ARD.SqlMode then
    begin
      if ShowSqlQueryForm(ARD, nil) = mrOk then FModified := True;
    end
    else
    begin
      if ShowReportForm(ARD, nil, nil, FInDesigner) = mrOk then FModified := True;
    end;
    if ARD.SqlMode = OldSqlMode then Break;
  end;
end;   }

function TReportsFm.ShowForm(InDesigner: Boolean; SelReport: TReportData
  ): Integer;
var
  i: Integer;
  CloneReportMan: TReportManager;
begin
  if not InDesigner and not DXMain.CanProjectChange then Exit(mrCancel);

  FModified := False;
  FInDesigner := InDesigner;
  FDelRps.Clear;
  FillReports;

  if InDesigner then
  begin
    KeyPreview := True;
    ButtonPanel1.ShowButtons:=[pbClose, pbHelp];
    if SelReport <> nil then
      with List do
        ItemIndex := Items.IndexOfObject(SelReport);
    Result := ShowModal;
    for i := 0 to FDelRps.Count - 1 do
    begin
  	  DeleteRefFromIntfs(PtrInt(FDelRps[i]), False);
      if FindExprFm <> nil then FindExprFm.DeleteReport(PtrInt(FDelRps[i]));
    end;
  end
  else
  begin
    KeyPreview := False;
    ButtonPanel1.ShowButtons := [pbOk, pbCancel, pbHelp];
    CloneReportMan := ReportMan.CloneManager;
    Result := ShowModal;
    if Result = mrOk then
      try
        // PPI экрана может отличаться от сохраненного в базе, поэтому
        // масштабируем отчеты и запросы до сохраненного PPI...
        ScaleReports(ReportMan, Screen.PixelsPerInch, DXMain.DesignTimePPI);
        ReportMan.SaveToDB;
        // ... а затем восстаналиваем исходное масштабирование
        ScaleReports(ReportMan, DXMain.DesignTimePPI, Screen.PixelsPerInch);
        // Если были удалены ссылки в меню, то также сохраняем пользователей
        if FDelRps.Count > 0 then
        begin
      	  for i := 0 to FDelRps.Count - 1 do
        	  DeleteRefFromIntfs(PtrInt(FDelRps[i]), False);
          UserMan.SaveToDb;
        end;
        DXMain.SetLastModified;
        DBase.Commit;
      	FreeAndNil(CloneReportMan);
      except
        on E: Exception do
        begin
          ErrMsg(rsSaveReportsError + ExceptionToString(E, True, False));
          ReportMan.Free;
          ReportMan := CloneReportMan;
          Result := mrCancel;
        end;
      end
    else
    begin
      ReportMan.Free;
      ReportMan := CloneReportMan;
    end;
  end;
  {
	if InDesigner or (Result = mrOk) then
	  for i := 0 to FDelRps.Count - 1 do
  	  DeleteRefFromIntfs(PtrInt(FDelRps[i]), False);}
end;

end.

