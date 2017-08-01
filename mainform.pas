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
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  strconsts, helpform, LclType, LclProc, StdCtrls, ExtCtrls, IBConnection,
  dxctrls, ComCtrls, formview, reportwindow;

type

  { TMainFm }

  TMainFm = class(TForm)
    MainMenu1: TMainMenu;
    FileMnu: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    HelpMnu: TMenuItem;
    DataMnu: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    RecentsMnu: TMenuItem;
    MenuItem28: TMenuItem;
    Timer1: TTimer;
    UsersMnu: TMenuItem;
    ToolsMnu: TMenuItem;
    MenuItem18: TMenuItem;
    ReportMnu: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    procedure ApplicationException(Sender: TObject; E: Exception);
    procedure ClearHistoryMnuHandler(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure MenuItem32Click(Sender: TObject);
    procedure MenuItem33Click(Sender: TObject);
    procedure MenuItem34Click(Sender: TObject);
    procedure MenuItem35Click(Sender: TObject);
    procedure RecentsMnuHandler(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure UsersMnuClick(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
  private
    { private declarations }
    FFr: TFrame;
    FLocked: Boolean;
    FOnCreateForm: TCreateFormEvent;
    FOnCreateListWindow: TCreateListWindowEvent;
    FOnCreateReportWindow: TCreateReportWindowEvent;
    FOnDestroyForm: TCreateFormEvent;
    FIsLoadApp: Boolean;
    FParams: TParamList;
    function GetFormViews(Index: Integer): TFormView;
    function GetPages: TPageControl;
    function GetStatusBar: TStatusBar;
    function GetToolBar: TToolbar;
    procedure OpenDatabase(DBName: String; aCreateDB: Boolean; const User, Pwd: String);
    procedure CloseDatabase;
    procedure ClearDataMenu;
    procedure ClearReportMenu;
    procedure ClearMenu;
    function ValidateAndSave: Boolean;
    procedure AddRecent;
    procedure BuildRecentsMenu;
    procedure SaveConfig;
  public
    { public declarations }
    procedure ShowDesigner;
    procedure ShowMain;
    procedure UpdateMenuState;
    procedure Lock(Value: Boolean);
    function CreatePage(const FormName: String; ViewType: TViewType): TTabSheet;
    procedure DestroyPage(Pg: TTabSheet);
    function IsLoadApp: Boolean;
    property FormViews[Index: Integer]: TFormView read GetFormViews;
    property Pages: TPageControl read GetPages;
    property ToolBar: TToolbar read GetToolBar;
    property StatusBar: TStatusBar read GetStatusBar;
    property Params: TParamList read FParams;
    property OnCreateForm: TCreateFormEvent read FOnCreateForm write FOnCreateForm;
    property OnDestroyForm: TCreateFormEvent read FOnDestroyForm write FOnDestroyForm;
    property OnCreateListWindow: TCreateListWindowEvent read FOnCreateListWindow
			write FOnCreateListWindow;
    property OnCreateReportWindow: TCreateReportWindowEvent read FOnCreateReportWindow
      write FOnCreateReportWindow;
  end;

var
  MainFm: TMainFm;

implementation

uses
  dbengine, appsettings, LazUtf8, designerframe, mainframe, LResources,
  apputils, settingsform, Translations, reportsform, reportmanager, recalcform,
  usersform, loginform, importform, exportform,
  deleterecsform, setvalueform, md5, dxusers, monitorform, connectionform,
  scriptmanager, outputform, modulesform, debugscriptform, langmanager;

{$R *.lfm}


function Translate(Name,Value : AnsiString; Hash : Longint; arg:pointer) : AnsiString;
begin
  case StringCase(Value,['&Yes','&No','Cancel']) of
   0: Result:=rsYesMsg;
   1: Result:=rsNoMsg;
   2: Result:=rsCancelMsg;
   else Result:='';
  end;
end;

procedure DoTranslate;
begin
  {TranslateUnitResourceStrings('strconsts', AppPath + 'languages' +
    DirectorySeparator + 'dataexpress.%s.po', LangId, '');}
  TranslateUnitResourceStrings('strconsts', LangMan.DataExpressFile);
  SetResourceStrings(@Translate,nil);
end;

function GetCmdParam(const aName: String): String;
var
  i, n: Integer;
  S, Key, Vl: String;
begin
  Result := '';
  n := Length(aName);
  for i := 2 to ParamCount do
  begin
    S := ParamStrUTF8(i);
    Key := Copy(S, 1, n);
    Vl := Copy(S, n + 1, 255);
    if CompareText(Key, aName) = 0 then Exit(Vl);
  end;
end;

function CmdParamExists(const aName: String): Boolean;
var
  i, n: Integer;
  S, Key: String;
begin
  Result := False;
  n := Length(aName);
  for i := 2 to ParamCount do
  begin
    S := ParamStrUTF8(i);
    Key := Copy(S, 1, n);
    if CompareText(Key, aName) = 0 then Exit(True);
  end;
end;

{ TMainFm }

procedure TMainFm.FormCreate(Sender: TObject);
begin
  FIsLoadApp := True;
  Randomize;
  Application.OnException:=@ApplicationException;

  FParams := TParamList.Create;

  AppConfig := TAppSettings.Create;
  AppConfig.Load;

  LangMan := TLangManager.Create;
  LangMan.Load;
  LangMan.SetCurLang(Appconfig.Language);

  DoTranslate;

  Caption := rsAppName;

  FileMnu.Caption := rsFile;
  MenuItem3.Caption := rsNewDB;
  MenuItem4.Caption := rsOpenDB;
  MenuItem6.Caption := rsDesigner;
  MenuItem8.Caption := rsSettings;
  MenuItem10.Caption := rsExit;
  MenuItem5.Caption := rsTemplatesFolder;
  DataMnu.Caption := rsDataMenu;
  HelpMnu.Caption := rsHelp;
  MenuItem2.Caption := rsAbout;
  MenuItem12.Caption := rsReports;
  ReportMnu.Caption := rsReports;
  MenuItem14.Caption := rsReference;
  MenuItem16.Caption := rsDXSite;
  ToolsMnu.Caption := rsTools;
  MenuItem18.Caption := rsRecalculate;
  UsersMnu.Caption:=rsUsers;
  MenuItem17.Caption := rsImportData;
  MenuItem19.Caption:=rsConnect;
  MenuItem20.Caption := rsExportProject;
  MenuItem21.Caption := rsImportProject;
  MenuItem22.Caption := rsExportData;
  MenuItem23.Caption := rsDeleteRecords;
  MenuItem24.Caption := rsSetValue;
  MenuItem11.Caption := rsUserMonitor;
  RecentsMnu.Caption:=rsRecents;
  MenuItem29.Caption := rsExpertMode;
  MenuItem32.Caption := rsProgInDX;
  MenuItem33.Caption := rsExpressionModules;
  MenuItem26.Caption := rsArticles;
  MenuItem34.Caption := rsForum;
  MenuItem35.Caption := rsVideoLessons;

  DBase := TDBEngine.Create;
  DBase.Remote:=AppConfig.Remote;
  DBase.Pwd:=AppConfig.Pwd;
  ReportMan := TReportManager.Create;
  UserMan := TdxUserManager.Create;
  ScriptMan := TScriptManager.Create;

  HelpDB := THelpDB.Create;
  HelpDB.Load;

  Width := AppConfig.FormWidth;
  Height := AppConfig.FormHeight;

  OutputFm := TOutputFm.Create(nil);
end;

procedure TMainFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FFr is TMainFr then
    MainFr.Done
  else if FFr is TDesignFr then
    DesignFr.Done;
  SaveConfig;
  if (UserMan.CurrentUser <> nil) and (DBase.Connected) then
    UserMan.UnRegisterUser;
end;

procedure TMainFm.ApplicationException(Sender: TObject; E: Exception);
var
  S: String;
begin
  S := E.Message;
  if E is EIBDatabaseError then
    S := WinCPToUtf8(S);

  if MessageDlg(rsFatalError, Format(rsFatalErrorMsg,
    [LineEnding, LineEnding, E.ClassName, LineEnding, S, LineEnding, LineEnding]),
    mtError, [mbYes, mbNo], 0) = mrNo then
  begin
    AppConfig.WasError:=True;
    SaveConfig;
    Halt;
  end;
end;

procedure TMainFm.ClearHistoryMnuHandler(Sender: TObject);
begin
  if MessageDlg(rsWarning, rsClearHistoryMsg, mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    AppConfig.ClearRecents;
    RecentsMnu.Clear;
    BuildRecentsMenu;
  end;
end;

procedure TMainFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if FLocked then
    CanClose := False
  else if DesignFr <> nil then
    CanClose := MessageDlg(rsWarning, rsNoSaveChangesExit, mtWarning, [mbYes,
      mbNo], 0) = mrYes
  else if MainFr <> nil then
  begin
    if AppConfig.ConfirmExit then
      CanClose := MessageDlg(rsWarning, rsExitMsg, mtWarning, [mbYes, mbNo], 0) = mrYes;
    if CanClose then
      CanClose := ValidateAndSave;
  end;
end;

procedure TMainFm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFr);
  HelpDB.Free;
  FreeAndNil(ScriptMan);
  FreeAndNil(UserMan);
  FreeAndNil(ReportMan);
  FreeAndNil(DBase);
  FreeAndNil(AppConfig);
  FreeAndNil(OutputFm);
  FreeAndNil(LangMan);
  FParams.Free;
end;

procedure TMainFm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if MainFr <> nil then
  begin
    case Key of
      VK_F3:
        begin
          MainFr.FindBn.Click;
          Key := 0;
        end;
    end;
  end
  else if DesignFr <> nil then
  begin
    if (Key = VK_F4) and (Shift = []) then DesignFr.ShowScriptForm;
  end;
  if (UserMan.CurrentUser = nil) or (UserMan.CurrentUser.RoleId < 0) then
    case Key of
      VK_F11: begin Key := 0; Timer1.Enabled := True; end;
      VK_F12: begin Key := 0; MenuItem12.Click; end;
    end;
end;

{function GetDatabaseName: String;
begin
  Result := ParamStrUTF8(1);
  if Result = '' then
    Result := AppConfig.Database;
end;    }

procedure TMainFm.FormShow(Sender: TObject);
var
  DBName: String;
  ContinueOpen: Boolean;
begin
	ContinueOpen := True;

  if AppConfig.WasError then
  begin
    AppConfig.WasError:=False;
    if MessageDlg(rsWarning, rsLastErrorMsg, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    	ContinueOpen := False;
    {begin
      UpdateMenuState;
      Exit;
    end;}
  end;

  if ContinueOpen then
  begin
    DBName := ParamStrUtf8(1);
    if DBName <> '' then
    begin
      if ParamCount > 0 then
        DBase.Remote:=CmdParamExists('r');
      if not DBase.Remote then DBase.SetLocalDB;
      if CmdParamExists('t:') then
        AppConfig.TemplateDir:=GetCmdParam('t:');
      with AppConfig do
        if Database <> '' then
    	    AddRecent(Database, Pwd, Remote);
	    OpenDatabase(DBName, False, GetCmdParam('u:'), GetCmdParam('p:'));
    end
    else
	    OpenDatabase(AppConfig.Database, False, '', '');
  end;

  BuildRecentsMenu;
  UpdateMenuState;

  WindowState := AppConfig.FormState;
  FIsLoadApp := False;
  //ShowMessage(DxSQLToNativeSQL('select t.[номер], t.[дата], (select sum([продажа товара].[сумма]) as sm from [продажа товара] where [продажа товара].[дата] = t.[дата]) as pole, tt.[название], sum(t.[сумма]) from [поступление товара] t left join [поставщики] tt on t.[поставщик]=tt.id'));
end;

procedure TMainFm.MenuItem10Click(Sender: TObject);
begin
  Close;
end;

procedure TMainFm.MenuItem11Click(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  MonitorFm.ShowForm;
end;

procedure TMainFm.MenuItem12Click(Sender: TObject);
var
  mr: Integer;
begin
  if not ValidateAndSave then Exit;

  mr := ReportsFm.ShowForm(DesignFr <> nil);
  if (MainFr <> nil) and (mr = mrOk) then
  begin
    if UserMan.GetIntf = nil then
    begin
      ClearReportMenu;
      MainFr.FillReportMenu;
      ReportMnu.Visible := ReportMnu.Count > 0;
    end
    // Если удалялись отчеты, то пункты меню тоже были удалены. Надо сохранить
    // изменения и перестроить меню.
    else if ReportsFm.DelRps.Count > 0 then
    begin
      UserMan.SaveToDb;
      ClearMenu;
      MainFr.BuildMenu;
    end;
  end;
end;

procedure TMainFm.MenuItem14Click(Sender: TObject);
begin
  OpenHelp('');
end;

procedure TMainFm.MenuItem16Click(Sender: TObject);
begin
  OpenUrl('http://mydataexpress.ru');
end;

procedure TMainFm.MenuItem17Click(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
	if MainFr.CurView = nil then
	  ErrMsg(rsNoFormsAvailable)
  else if MainFr.CurView.Form.ViewType = vtSimpleForm then
  	ErrMsg(rsThisCommandNotApplySimpleForm)
  else
	  ImportFm.ShowForm(MainFr.CurView.Form);
end;

procedure TMainFm.MenuItem19Click(Sender: TObject);
begin
  if not ValidateAndSave then Exit;

  if ConnectionFm.ShowForm <> mrOk then Exit;
  //AddRecent;
  CloseDatabase;
  UpdateMenuState;
  try
    DBase.Pwd := ConnectionFm.Pwd.Text;
    DBase.Remote := True;
    OpenDatabase(ConnectionFm.Conn.Text, False, '', '');
  except
    on E: Exception do
    begin
      ErrMsg(rsErrConnect + LineEnding + LineEnding + WinCPToUtf8(E.Message));
    end;
  end;
  UpdateMenuState;
end;

procedure TMainFm.MenuItem20Click(Sender: TObject);
var
  FlNm: String;
begin
  with TSaveDialog.Create(nil) do
  try
    Title := rsExportProject;
    Filter:=rsImportExportPrjFilter;
    DefaultExt:='dxp';
    Options:=Options + [ofPathMustExist, ofOverwritePrompt];
    if not Execute then Exit;
    FlNm := FileName;
  finally
    Free;
  end;
  DesignFr.ExportProject(FlNm);
  MessageDlg(rsExportProject, rsExportPrjOk, mtInformation, [mbOk], 0);
end;

procedure TMainFm.MenuItem21Click(Sender: TObject);
var
  FlNm: String;
begin
  with TOpenDialog.Create(nil) do
  try
    Title := rsImportProject;
    Filter:=rsImportExportPrjFilter;
    DefaultExt:='dxp';
    Options:=Options + [ofFileMustExist];
    if not Execute then Exit;
    FlNm := FileName;
  finally
    Free;
  end;
  DesignFr.ImportProject(FlNm);
end;

procedure TMainFm.MenuItem22Click(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  if MainFr.CurView = nil then
	  ErrMsg(rsNoFormsAvailable)
  else if MainFr.CurView.Form.ViewType = vtSimpleForm then
  	ErrMsg(rsThisCommandNotApplySimpleForm)
  else
    ExportFm.ShowForm(MainFr.CurView.Form);
end;

procedure TMainFm.MenuItem23Click(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  if MainFr.CurView = nil then
    ErrMsg(rsNoFormsAvailable)
  else if MainFr.CurView.Form.ViewType = vtSimpleForm then
  	ErrMsg(rsThisCommandNotApplySimpleForm)
  else if DeleteRecsFm.ShowForm = mrOk then
    MainFr.CurView.DataSetProc.DeleteAllRecords;
end;

procedure TMainFm.MenuItem24Click(Sender: TObject);
var
  TId, FId: Integer;
  Expr: String;
begin
  if not ValidateAndSave then Exit;
  if MainFr.CurView = nil then
    ErrMsg(rsNoFormsAvailable)
  else if MainFr.CurView.Form.ViewType = vtSimpleForm then
  	ErrMsg(rsThisCommandNotApplySimpleForm)
  else if SetValueFm.ShowForm(MainFr.CurView.Form, TId, FId, Expr) then
    MainFr.CurView.DataSetProc.Recalculate(TId, FId, Expr);
end;

procedure TMainFm.MenuItem26Click(Sender: TObject);
begin
  OpenUrl('http://mydataexpress.ru/articles.html');
end;

procedure TMainFm.MenuItem27Click(Sender: TObject);
var
  FlNm: String;
begin
  with TOpenDialog.Create(nil) do
  try
    Title := rsMergeProjects;
    Filter:=rsImportExportPrjFilter;
    DefaultExt:='dxp';
    Options:=Options + [ofFileMustExist];
    if not Execute then Exit;
    FlNm := FileName;
  finally
    Free;
  end;
  DesignFr.MergeProjects(FlNm);
end;

procedure TMainFm.MenuItem29Click(Sender: TObject);
begin
  AppConfig.ExpertMode:=not AppConfig.ExpertMode;
  DesignFr.SetExpertMode(AppConfig.ExpertMode);
end;

procedure TMainFm.MenuItem32Click(Sender: TObject);
begin
  OpenUrl('http://mydataexpress.ru/programming_in_dx3.pdf');
end;

procedure TMainFm.MenuItem33Click(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  ModulesFm.ShowForm;
end;

procedure TMainFm.MenuItem34Click(Sender: TObject);
begin
  OpenUrl('http://forum.mydataexpress.ru');
end;

procedure TMainFm.MenuItem35Click(Sender: TObject);
begin
  OpenUrl('http://forum.mydataexpress.ru/viewforum.php?f=31');
end;

procedure TMainFm.RecentsMnuHandler(Sender: TObject);
var
  R: TRecentData;
  i: PtrInt;
  DBName, Pwd: String;
  Rmt: Boolean;
begin
  if not ValidateAndSave then Exit;

  i := TMenuItem(Sender).Tag;
  R := AppConfig.Recents[i];
  DBName := R.DBName;
  Pwd := R.Pwd;
  Rmt := R.Remote;
  //AddRecent;
  CloseDatabase;
  if Rmt = False then
    DBase.SetLocalDB
  else
  begin
    DBase.Remote:=True;
    DBase.Pwd:=Pwd;
  end;
  //UpdateMenuState;
  OpenDatabase(DBName, False, '', '');
  UpdateMenuState;
end;

procedure TMainFm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  MenuItem6.Click;
end;

procedure TMainFm.UsersMnuClick(Sender: TObject);
begin
  if not ValidateAndSave then Exit;

  UsersFm.ShowForm(DesignFr <> nil);
  if MainFr <> nil then
  begin
    ClearDataMenu;
    ClearReportMenu;
    ClearMenu;
    MainFr.BuildMenu;
    MainFr.UpdateMenu;
    MainFr.UpdateDataMenu;
  end;
end;

procedure TMainFm.MenuItem18Click(Sender: TObject);
var
  TId, FId: Integer;
begin
  if not ValidateAndSave then Exit;

  if MainFr.CurView = nil then
    ErrMsg(rsNoFormsAvailable)
  else if MainFr.CurView.Form.ViewType = vtSimpleForm then
  	ErrMsg(rsThisCommandNotApplySimpleForm)
  else
  begin
    if RecalcFm.ShowForm(MainFr.CurView.Form, TId, FId) then
      MainFr.CurView.DataSetProc.Recalculate(TId, FId, '');
  end;
end;

procedure TMainFm.MenuItem2Click(Sender: TObject);
begin
  OpenHelp('about');
end;

procedure TMainFm.MenuItem3Click(Sender: TObject);
var
  FName: String;
begin
  if not ValidateAndSave then Exit;

  with TSaveDialog.Create(nil) do
  try
    title:=rsCreateDialogTitle;
    Filter := rsDialogFilter;
    DefaultExt:='fdb';
    Options:=Options + [ofPathMustExist, ofOverwritePrompt];
    if not Execute then Exit;
    FName := FileName;
  finally
    Free;
  end;

  if FileExists(FName) then
    if not DeleteFile(FName) then
    begin
      ErrMsg(rsCanNotDeleteFile);
      Exit;
    end;

  //AddRecent;
  CloseDatabase;
  DBase.SetLocalDB;
  OpenDatabase(FName, True, '', '');
  UpdateMenustate;
end;

procedure TMainFm.MenuItem4Click(Sender: TObject);
var
  FName: String;
begin
  if not ValidateAndSave then Exit;

  with TOpenDialog.Create(nil) do
  try
    Title:=rsOpenDatabase;
    Filter := rsDialogFilter;
    DefaultExt:='fdb';
    Options:=Options + [ofFileMustExist];
    if not Execute then Exit;
    FName := FileName;
  finally
    Free;
  end;

  //AddRecent;
  CloseDatabase;
  UpdateMenuState;
  DBase.SetLocalDB;
  OpenDatabase(FName, False, '', '');
  UpdateMenuState;
end;

function GetUnixFileManager: String;
begin
  if FileExists('/usr/bin/nautilus') then
    Result := 'nautilus'
  else if FileExists('/usr/bin/thunar') then
    Result := 'thunar'
  else if FileExists('/usr/bin/nemo') then
    Result := 'nemo'
  else if FileExists('/usr/bin/dolphin') then
    Result := 'dolphin'
  else if FileExists('/usr/bin/pcmanfm') then
    Result := 'pcmanfm'
  else
    Result := '';
end;

procedure TMainFm.MenuItem5Click(Sender: TObject);
var
  S, FM: String;
begin
  if not ValidateAndSave then Exit;

  S := '"' + GetTemplatesDir + '"';
  {$ifdef windows}
  ExecuteProcess('explorer.exe', [Utf8ToWinCP(S)]);
  {$else}
  FM := GetUnixFileManager;
  if S <> '' then
    ShellExec('', FM, S, '', 0)
  else
    ErrMsg(rsUnableFindFileManager);
  {$endif}
end;

procedure TMainFm.MenuItem6Click(Sender: TObject);
begin
  if not ValidateAndSave then
  begin
    MenuItem6.Checked:=False;
    Exit;
  end;

  if TmenuItem(Sender).Checked then
  begin
    MainFr.SaveCurrentDataSet;
    ShowDesigner;
    ClearDataMenu;
    ClearReportMenu;
    ClearMenu;
  end
  else
    ShowMain;
  UpdateMenuState;
end;

procedure TMainFm.MenuItem8Click(Sender: TObject);
begin
  SettingsFm.ShowForm;
end;

procedure TMainFm.OpenDatabase(DBName: String; aCreateDB: Boolean; const User,
  Pwd: String);
var
  U: TdxUser;
  code, Ver: Integer;
begin
  if DBName = '' then Exit;
  DBase.Database:=DBName;
  if (not DBase.Remote) and (not DBase.DatabaseExists) then
  begin
    if aCreateDB then
    begin
      DBase.CreateDatabase;
      {$ifndef windows}
      DBase.Disconnect;
      ExecuteProcess('/bin/chmod', ['666', DBase.Database]);
      {$endif}
    end
    else
    begin
      ErrMsg(Format(rsDBNotFound, [DBase.Database]));
      DBase.Database:='';
      DBase.Remote := False;
      Exit;
    end;
  end;
  DBase.Connect;

  Ver := DBase.CheckVersion;
  if Ver = 0 then
  begin
    if not aCreateDB then
    begin
      ReportMan.CheckReports;
      UserMan.CheckUsers;
      ScriptMan.CheckScripts;
      DBase.AddVersion;
    end;
  end;
  // На будущее
  {if Ver = 30 then
  begin
		UpdateVersion;
  end;}

  ReportMan.LoadFromDB;
  UserMan.LoadFromDb;

  // Вход

  if UserMan.Users.Count > 0 then
  begin
    U := nil;
    if User <> '' then
      U := UserMan.Users.FindUserByName(User);
    if (Pwd <> '') and (U <> nil) and (md5print(md5string(Pwd)) = U.Password) then
      UserMan.CurrentUserId:=U.Id
    else if LoginFm.ShowForm(U) <> mrOk then
    begin
      CloseDatabase;
      Exit;
    end;
  end;

  if UserMan.CurrentUser <> nil then
  begin
    code := UserMan.RegisterUser;
    if code = 10 then
      MessageDlg(rsWarning, rsSingleModeActiveUsers, mtWarning, [mbOk], 0)
    else if code > 0 then
    begin
      case code of
        1: ErrMsg(rsDBInSingleMode);
        2: ErrMsg(Format(rsUserAlreadyLogged, [UserMan.CurrentUser.Name]));
        3: ErrMsg(rsActiveUsersInDB);
      end;
      UserMan.CurrentUserId:=-1;
      CloseDatabase;
      Exit;
    end;
    Caption := 'DataExpress (' + UserMan.CurrentUser.Name + ')';
  end;
  if (ssShift in GetKeyShiftState) and ((UserMan.CurrentUser = nil) or
    (UserMan.CurrentUser.RoleId < 0)) then
  begin
    MenuItem6.Checked := True;
    ShowDesigner;
  end
  else
    ShowMain;
end;

function TMainFm.GetFormViews(Index: Integer): TFormView;
begin
  Result := TFormView(MainFr.PageControl1.Pages[Index].Controls[0]);
end;

function TMainFm.GetPages: TPageControl;
begin
  Result := MainFr.PageControl1;
end;

function TMainFm.GetStatusBar: TStatusBar;
begin
  Result := MainFr.StatusBar1;
end;

function TMainFm.GetToolBar: TToolbar;
begin
  Result := MainFr.ToolBar1;
end;

procedure TMainFm.ClearDataMenu;
begin
  while DataMnu.Count > 0 do
    DataMnu.Items[0].Free;
end;

procedure TMainFm.ClearReportMenu;
begin
  while ReportMnu.Count > 0 do
    ReportMnu.Items[0].Free;
end;

procedure TMainFm.ClearMenu;
var
  i: Integer;
  MI: TMenuItem;
begin
  for i := MainMenu1.Items.Count - 1 downto 0 do
	begin
    MI := MainMenu1.Items[i];
  	if (MI <> FileMnu) and (MI <> DataMnu) and (MI <> ReportMnu) and
    	(MI <> ToolsMnu) and (MI <> HelpMnu) then MI.Free;
  end;
  {for i := MainMenu1.Items.Count - 5 downto 1 do
    MainMenu1.Items[i].Free;}
end;

function TMainFm.ValidateAndSave: Boolean;
begin
  Result := True;
  if MainFr <> nil then
    Result := MainFr.ValidateAndSave;
end;

procedure TMainFm.AddRecent;
begin
  if DBase.Database = '' then Exit;
  AppConfig.AddRecent(DBase.Database, DBase.Pwd, DBase.Remote);
  BuildRecentsMenu;
end;

procedure TMainFm.BuildRecentsMenu;
var
  i: Integer;
  R: TRecentData;
begin
  RecentsMnu.Clear;
  for i := 0 to AppConfig.RecentCount - 1 do
  begin
    R := AppConfig.Recents[i];
    RecentsMnu.Add( CreateMenuItem(MainMenu1, R.DBName, i, 0, @RecentsMnuHandler, '') );
  end;
  if RecentsMnu.Count = 0 then
  begin
    RecentsMnu.Add( CreateMenuItem(MainMenu1, rsEmpty, 0, 0, nil, '') );
    RecentsMnu.Items[0].Enabled:=False;
  end
  else
  begin
    RecentsMnu.Add(CreateMenuItem(MainMenu1, '-', 0, 0, nil, '' ));
    RecentsMnu.Add(CreateMenuItem(MainMenu1, rsClearHistory, 0, 0,
      @ClearHistoryMnuHandler, '' ));
  end;
end;

procedure TMainFm.SaveConfig;
begin
  if WindowState in [wsNormal, wsMaximized] then
    AppConfig.FormState := WindowState;
  if WindowState = wsNormal then
  begin
	  AppConfig.FormWidth := Width;
  	AppConfig.FormHeight := Height;
  end;
  if DBase.Database <> '' then
  begin
    // Если БД в папке с программой
    if ExtractFilePath(DBase.Database) = AppPath then
      AppConfig.Database:=ExtractFileName(DBase.Database)
    else
      AppConfig.DataBase := DBase.Database;
    AppConfig.Remote:=DBase.Remote;
    AppConfig.Pwd := DBase.Pwd;
  end;
  AppConfig.Save;
end;

procedure TMainFm.CloseDatabase;
begin
  if UserMan.CurrentUser <> nil then
    UserMan.UnRegisterUser;
  if FFr = MainFr then
  	MainFr.Done;
  FreeAndNil(FFr);
  DBase.Disconnect;

  AddRecent;

  DBase.Database := '';
  ReportMan.Clear;
  UserMan.Clear;
  ClearMenu;
  ClearDataMenu;
  ClearReportMenu;
  Caption := 'DataExpress';
end;

procedure TMainFm.ShowDesigner;
begin
  if FFr is TMainFr then
    MainFr.Done;
  FreeAndNil(FFr);
  DBase.ReadCommit;

  FFr := TDesignFr.Create(Self);
  with TDesignFr(FFr) do
  begin
    Parent := Self;
    Align := alClient;
    Visible := True;
    Init;
  end;
end;

procedure TMainFm.ShowMain;
begin
  if DesignFr <> nil then
  begin
    DesignFr.Save;
    DesignFr.Done;
  end;
  FreeAndNil(FFr);
  FFr := TMainFr.Create(Self);
  with TMainFr(FFr) do
  begin
    Parent := Self;
    Align := alClient;
    Visible := True;
    Init;
  end;
end;

procedure TMainFm.UpdateMenuState;
var
  IsDevel: Boolean;
begin
  IsDevel := (UserMan.CurrentUser = nil) or (UserMan.CurrentUser.RoleId < 0);
  MenuItem3.Visible := (DesignFr = nil) and IsDevel;
  MenuItem4.Visible := DesignFr = nil;
  MenuItem19.Visible := DesignFr = nil;
  MenuItem7.Visible := DesignFr = nil;
  MenuItem6.Enabled := (FFr <> nil);
  MenuItem6.Visible:=IsDevel;
  MenuItem12.Enabled := (FFr <> nil);
  MenuItem12.Visible:=IsDevel;
  MenuItem5.Visible := IsDevel;
  UsersMnu.Enabled:= FFr <> nil;
  UsersMnu.Visible := IsDevel;
  MenuItem13.Visible:=IsDevel;
  MenuItem8.Visible := IsDevel;
  MenuItem9.Visible:=IsDevel;
  DataMnu.Visible := (DesignFr = nil) and (DataMnu.Count > 0);
  ReportMnu.Visible := (DesignFr = nil) and (ReportMnu.Count > 0);
  ToolsMnu.Visible := IsDevel;
  MenuItem18.Visible := MainFr <> nil;
  MenuItem17.Visible := MainFr <> nil;
  MenuItem22.Visible := MainFr <> nil;
  MenuItem20.Visible := DesignFr <> nil;
  MenuItem21.Visible := DesignFr <> nil;
  MenuItem23.Visible:=MainFr <> nil;
  MenuItem24.Visible:=MainFr <> nil;
  MenuItem25.Visible:=MainFr <> nil;
  MenuItem1.Visible := FFr <> nil;
  MenuItem11.Visible := FFr <> nil;
  //MenuItem11.Enabled:=DBase.Remote and (UserMan.CurrentUser <> nil);
  MenuItem27.Visible := DesignFr <> nil;
  RecentsMnu.Visible:=DesignFr = nil;
  MenuItem29.Visible := DesignFr <> nil;
  MenuItem30.Visible := DesignFr <> nil;
  MenuItem33.Enabled := FFr <> nil;
  MenuItem33.Visible := IsDevel;
end;

procedure TMainFm.Lock(Value: Boolean);
var
  i, j: Integer;
  Form: TForm;
begin
  FLocked := Value;
  FFr.Enabled:=not Value;
  for i := 0 to MainMenu1.Items.Count - 1 do
  	MainMenu1.Items[i].Enabled := not Value;

  for i := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[i];
    if (Form = MainFm) or (Form = DebugScriptFm) then Continue;
    for j := 0 to Form.ControlCount - 1 do
      Form.Controls[j].Enabled := not Value;
  end;

  {FileMnu.Enabled := not Value;
  DataMnu.Enabled:=not Value;
  HelpMnu.Enabled:=not Value;
  ToolsMnu.Enabled:=not Value;
  ReportMnu.Enabled := not Value;  }
end;

function TMainFm.CreatePage(const FormName: String; ViewType: TViewType
  ): TTabSheet;
begin
  Result := MainFr.OpenTab2(FormName, ViewType);
end;

procedure TMainFm.DestroyPage(Pg: TTabSheet);
begin
  MainFr.CloseTab(Pg);
end;

function TMainFm.IsLoadApp: Boolean;
begin
  Result := FIsLoadApp;
end;

initialization
  {$i images.lrs}

end.

