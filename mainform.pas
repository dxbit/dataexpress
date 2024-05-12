unit MainForm;

{$mode objfpc}{$H+}
{$DEFINE DXFull}

interface

uses
  Classes, {$IFDEF WINDOWS}windows,{$ENDIF} SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, strconsts, LclType, LclProc, ExtCtrls, IBConnection, dxctrls,
  ComCtrls, formview, LclIntf, CrossApi, LazStringUtils;

type

  TFatalErrorEvent = procedure (Sender: TObject; const Msg: String; var Accept: Boolean) of object;
  TDatabaseCloseQueryEvent = TCloseQueryEvent;

  { TMainFm }

  TMainFm = class(TForm)
    MainMenu1: TMainMenu;
    FileMnu: TMenuItem;
    FindActionsDivMnu: TMenuItem;
    FindActionsMnu: TMenuItem;
    FindExprMnu: TMenuItem;
    UpdatesMnu: TMenuItem;
    MergePrjDivMnu: TMenuItem;
    ExitMnu: TMenuItem;
    HelpMnu: TMenuItem;
    DataMnu: TMenuItem;
    MonitorMnu: TMenuItem;
    ReportsMnu: TMenuItem;
    ExpertDivMnu: TMenuItem;
    ApiReferenceMnu: TMenuItem;
    MenuItem15: TMenuItem;
    SiteMnu: TMenuItem;
    ImportDataMnu: TMenuItem;
    ConnectMnu: TMenuItem;
    ExportPrjMnu: TMenuItem;
    ImportPrjMnu: TMenuItem;
    ExportDataMnu: TMenuItem;
    DeleteRecsMnu: TMenuItem;
    SetValueMnu: TMenuItem;
    DeleteRecsDivMnu: TMenuItem;
    ArticlesMnu: TMenuItem;
    MergePrjMnu: TMenuItem;
    ExpertMnu: TMenuItem;
    ExtDivMnu: TMenuItem;
    MenuItem31: TMenuItem;
    ProgMnu: TMenuItem;
    ExtMnu: TMenuItem;
    ForumMnu: TMenuItem;
    VideoMnu: TMenuItem;
    ArticlesDivMnu: TMenuItem;
    OpenDemoMnu: TMenuItem;
    UserReferenceMnu: TMenuItem;
    PascalScriptMnu: TMenuItem;
    GalleryMnu: TMenuItem;
    TemplatesFolderMnu: TMenuItem;
    TemplatesHelperMnu: TMenuItem;
    RecentsMnu: TMenuItem;
    MenuItem28: TMenuItem;
    Timer1: TTimer;
    Timer2: TTimer;
    UsersMnu: TMenuItem;
    ToolsMnu: TMenuItem;
    RecalcMnu: TMenuItem;
    ReportMnu: TMenuItem;
    AboutMnu: TMenuItem;
    CreateMnu: TMenuItem;
    OpenMnu: TMenuItem;
    TemplatesMnu: TMenuItem;
    DesignMnu: TMenuItem;
    RecentsDivMnu: TMenuItem;
    OptionsMnu: TMenuItem;
    OptionsDivMnu: TMenuItem;
    procedure ApplicationException(Sender: TObject; E: Exception);
    procedure ClearHistoryMnuHandler(Sender: TObject);
    procedure FindActionsMnuClick(Sender: TObject);
    procedure FindExprMnuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ExitMnuClick(Sender: TObject);
    procedure MonitorMnuClick(Sender: TObject);
    procedure ReportsMnuClick(Sender: TObject);
    procedure ApiReferenceMnuClick(Sender: TObject);
    procedure SiteMnuClick(Sender: TObject);
    procedure ImportDataMnuClick(Sender: TObject);
    procedure ConnectMnuClick(Sender: TObject);
    procedure ExportPrjMnuClick(Sender: TObject);
    procedure ImportPrjMnuClick(Sender: TObject);
    procedure ExportDataMnuClick(Sender: TObject);
    procedure DeleteRecsMnuClick(Sender: TObject);
    procedure SetValueMnuClick(Sender: TObject);
    procedure ArticlesMnuClick(Sender: TObject);
    procedure MergePrjMnuClick(Sender: TObject);
    procedure ExpertMnuClick(Sender: TObject);
    procedure ProgMnuClick(Sender: TObject);
    procedure ExtMnuClick(Sender: TObject);
    procedure ForumMnuClick(Sender: TObject);
    procedure UpdatesMnuClick(Sender: TObject);
    procedure VideoMnuClick(Sender: TObject);
    procedure OpenDemoMnuClick(Sender: TObject);
    procedure UserReferenceMnuClick(Sender: TObject);
    procedure PascalScriptMnuClick(Sender: TObject);
    procedure GalleryMnuClick(Sender: TObject);
    procedure TemplatesFolderMnuClick(Sender: TObject);
    procedure TemplatesHelperMnuClick(Sender: TObject);
    procedure RecentsMnuHandler(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure UsersMnuClick(Sender: TObject);
    procedure RecalcMnuClick(Sender: TObject);
    procedure AboutMnuClick(Sender: TObject);
    procedure CreateMnuClick(Sender: TObject);
    procedure OpenMnuClick(Sender: TObject);
    procedure DesignMnuClick(Sender: TObject);
    procedure OptionsMnuClick(Sender: TObject);
  private
    FDesignPageIndex: Integer;
    { private declarations }
    FFr: TFrame;
    FLocked: Boolean;
    FOnDatabaseCloseQuery: TDatabaseCloseQueryEvent;
    FOnCreateForm: TCreateFormEvent;
    FOnCreateListWindow: TCreateListWindowEvent;
    FOnCreateReportWindow: TCreateReportWindowEvent;
    FOnDatabaseClose: TNotifyEvent;
    FOnDestroyForm: TCreateFormEvent;
    FIsLoadApp: Boolean;
    FOnFatalError: TFatalErrorEvent;
    FParams: TParamList;
    FSelectedFormId: Integer;
    FOldFormatSettings: TFormatSettings;
    function GetFormViews(Index: Integer): TFormView;
    function GetPages: TPageControl;
    function GetStatusBar: TStatusBar;
    function GetToolBar: TToolbar;
    //procedure RestoreConnection;
    procedure OpenDatabase(DBName: String; aCreateDB, aDesignDB: Boolean; DBPwd, User, Pwd: String);
    procedure CloseDatabase(ClearConnectInfo: Boolean = True);
    procedure ClearDataMenu;
    procedure ClearReportMenu;
    procedure ClearMenu;
    function ValidateAndSave: Boolean;
    procedure AddRecent;
    procedure SaveConfig;
    procedure SetAppCaption(const aCaption: String);
  public
    { public declarations }
    procedure ShowDesigner;
    procedure ShowMain;
    procedure UpdateMenuState;
    procedure Lock(Value: Boolean);
    function CreatePage(const FormName: String; ViewType: TViewType): TTabSheet;
    procedure DestroyPage(Pg: TTabSheet);
    function IsLoadApp: Boolean;
    procedure ClearEventHandlers;
    procedure BuildRecentsMenu;
    procedure RestoreFormatSettings;
    procedure RestoreMainMenu;
    procedure OpenTemplateFolder;
    function CanCloseDatabase: Boolean;
    procedure EnableDropFiles(AEnabled: Boolean);
    property SelectedFormId: Integer read FSelectedFormId write FSelectedFormId;
    property DesignPageIndex: Integer read FDesignPageIndex write FDesignPageIndex;
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
    property OnDatabaseClose: TNotifyEvent read FOnDatabaseClose
    	write FOnDatabaseClose;
    property OnDatabaseCloseQuery: TDatabaseCloseQueryEvent read FOnDatabaseCloseQuery
      write FOnDatabaseCloseQuery;
    property OnFatalError: TFatalErrorEvent read FOnFatalError write FOnFatalError;
  end;

var
  MainFm: TMainFm;

implementation

uses
  dbengine, appsettings, LazUtf8, mainframe, LResources, apputils, settingsform,
  Translations, recalcform, loginform, importform, exportform, deleterecsform,
  setvalueform, md5, dxusers, monitorform, scriptmanager, outputform,
  langmanager, aboutform, formmanager, ConnectionsForm, progressform,
  helpmanager, dxmains, LazFileUtils, imagemanager,
  appimagelists, reportmanager, mydialogs, crypt, mylogger{$ifdef DXFull},
  reportsform, usersform, modulesform, debugscriptform, scriptform,
  templatefieldsform, imagesform, designerframe, findactionsform,
  findexprform, findscriptform, updatemanform, updatemanager{$endif}
  {$ifdef linux},
  gtkthememanager
  {$endif};

const
  WIKI_URL = 'https://wiki.mydataexpress.ru/';

{$R *.lfm}


function Translate(Name,Value : AnsiString; Hash : Longint; arg:pointer) : AnsiString;
begin
  case StringCase(Value,['&Yes','&No','Cancel', '&Retry', '&Ignore', 'Abort',
  	'&All', 'No to all', 'Yes to &All', '&Close', 'Append', 'Edit', 'Delete',
    'Move up', 'Move down', '&Help', '&OK', 'Cut', 'Copy', 'Paste', 'Copy cells',
    'Paste cells']) of
   0: Result:=rsYes;
   1: Result:=rsNo;
   2: Result:=rsCancel;
   3: Result:=rsRetry;
   4: Result:=rsIgnore;
   5: Result:=rsAbort;
   6: Result:=rsAll;
   7: Result:=rsNoToAll;
   8: Result:=rsYesToAll;
   9: Result:=rsClose;
   10: Result:=rsAppend;
   11: Result:=rsEdit;
   12: Result:=rsDelete;
   13: Result:=rsMoveUp;
   14: Result:=rsMoveDown;
   15: Result:=rsHelp;
   16: Result:=rsOK;
   17: Result:=rsCut;
   18: Result:=rsCopy;
   19: Result:=rsPaste;
   20: Result:=rsCopyCells;
   21: Result:=rsPasteCells;
   else Result:='';
  end;
end;

procedure DoTranslate;
begin
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

function IsConnectName(const DBPath: String): Boolean;
begin
  Result := Copy(DBPath, 1, 1) = '$';
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
  //AppConfig.LoadConnections;

  LangMan := TLangManager.Create;
  LangMan.Load;
  LangMan.SetCurLang(Appconfig.Language);

  DoTranslate;

  Caption := rsAppName;

  FileMnu.Caption := rsFile;
  CreateMnu.Caption := rsNewDB;
  OpenMnu.Caption := rsOpenDB;
  DesignMnu.Caption := rsDesigner;
  OptionsMnu.Caption := rsSettings;
  ExitMnu.Caption := rsExit;
  TemplatesMnu.Caption := rsTemplates;
  TemplatesFolderMnu.Caption := rsTemplatesFolder;
  TemplatesHelperMnu.Caption := rsTemplateWizard;
  DataMnu.Caption := rsDataMenu;
  HelpMnu.Caption := rsHelp;
  AboutMnu.Caption := rsAbout;
  ReportsMnu.Caption := rsReports;
  ReportMnu.Caption := rsReports;
  ApiReferenceMnu.Caption := rsAPIReference;
  SiteMnu.Caption := rsDXSite;
  ToolsMnu.Caption := rsTools;
  RecalcMnu.Caption := rsRecalculate;
  UsersMnu.Caption:=rsUsers;
  ImportDataMnu.Caption := rsImportData;
  ConnectMnu.Caption:=rsConnect;
  ExportPrjMnu.Caption := rsExportProject;
  ImportPrjMnu.Caption := rsImportProject;
  ExportDataMnu.Caption := rsExportData;
  DeleteRecsMnu.Caption := rsDeleteRecords;
  SetValueMnu.Caption := rsSetValue;
  MonitorMnu.Caption := rsUserMonitor;
  RecentsMnu.Caption:=rsRecents;
  ExpertMnu.Caption := rsExpertMode;
  ProgMnu.Caption := rsProgInDX;
  ExtMnu.Caption := rsExtensions;
  ArticlesMnu.Caption := rsArticles;
  MergePrjMnu.Caption := rsMergeProjects;
  ForumMnu.Caption := rsForum;
  VideoMnu.Caption := rsVideoLessons;
  OpenDemoMnu.Caption := rsOpenDemoDB;
  UserReferenceMnu.Caption := rsUserManual;
  PascalScriptMnu.Caption := rsPascalScriptReference;
  GalleryMnu.Caption := rsGallery;
  FindActionsMnu.Caption := rsFindActions;
  FindExprMnu.Caption := rsFindExpressions;
  UpdatesMnu.Caption := rsUpdatesDB;

  {$IFNDEF DXFull}
  FreeAndNil(CreateMnu);
  FreeAndNil(DesignMnu);
  FreeAndNil(ReportsMnu);
  FreeAndNil(TemplatesFolderMnu);
  FreeAndNil(TemplatesHelperMnu);
  FreeAndNil(TemplatesMnu);
  FreeAndNil(GalleryMnu);
  FreeAndNil(UsersMnu);
  FreeAndNil(ExtMnu);
  FreeAndNil(ExtDivMnu);
  FreeAndNil(ExpertMnu);
  FreeAndNil(ExpertDivMnu);
  FreeAndNil(ExportPrjMnu);
  FreeAndNil(ImportPrjMnu);
  FreeAndNil(MergePrjMnu);
  FreeAndNil(HelpMnu);
  {$ENDIF}

  CreateImageLists;
  ImageCache := TImageCache.Create;

  FBLoader := TFBLoader.Create(nil);
  DBase := TDBEngine.Create;
  ImageMan := TImageManager.Create;
  FormMan := TFormManager.Create;
  ReportMan := TReportManager.Create;
  UserMan := TdxUserManager.Create;
  ScriptMan := TScriptManager.Create;
  DXMain := TDXMain.Create;

  HelpMan := THelpMan.Create;

  Left := AppConfig.FormLeft;
  Top := AppConfig.FormTop;
  Width := AppConfig.FormWidth;
  Height := AppConfig.FormHeight;

  SetFormPropPosition(Self);
  CorrectFormPos(Self, Self);

  BuildRecentsMenu;
  {$ifdef linux}
  // Неправильно может определяться разделитель тысяч и в числах будет
  // отображаться вопросительный знак. В результате печать шаблонов и прочее
  // будет работать с ошибками.
  if not (DefaultFormatSettings.ThousandSeparator in [' ', ',', '.', '-', '''']) then
    DefaultFormatSettings.ThousandSeparator := ' ';
  //
  CreateThemeFile;
  {$endif}
  FOldFormatSettings := DefaultFormatSettings;
end;

procedure TMainFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  (*if FFr is TMainFr then
    MainFr.Done
  {$ifdef DXFull}else if FFr is TDesignFr then
    DesignFr.Done{$endif};
  SaveConfig;
  try
    if (UserMan.CurrentUser <> nil) and (DBase.Connected) then
      UserMan.UnRegisterUser;
  except
    on E: Exception do
      ErrMsg(rsErrorCloseApp + ExceptionToString(E, True, False));
  end;*)
end;

procedure ShowScriptErrorMsg(const S: String);
begin
  MessageDlg(rsScriptError, Format(rsScriptErrorOccurred, [LineEnding + S]),
    mtError, [mbOk], 0);
end;

procedure TMainFm.ApplicationException(Sender: TObject; E: Exception);
var
  Msg: String;
  Btns: TMsgDlgButtons;
  Accept: Boolean;
begin
  if MainFr <> nil then
  begin
    Btns := [mbYes, mbNo];
    Msg := Format(rsFatalErrorMsg, [ExceptionToString(E, True, True)]);
  end
  else
  begin
    Btns := [mbOk];
    Msg := Format(rsFatalErrorMsgDesign, [ExceptionToString(E, True, False)]);
  end;

  if FOnFatalError <> nil then
  begin
    Accept := False;
    try
      FOnFatalError(Self, E.Message, Accept);
    except
      on E: Exception do
        Msg := Format(rsFatalErrorMsg, [ExceptionToString(E, True, True)]);
    end;
    if Accept then Exit;
  end;

  LogString(Msg, 'FatalError');
  if MessageDlg(rsFatalError, Msg, mtError, Btns, 0) = mrNo then
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
    BuildRecentsMenu;
    //AppConfig.Save;
  end;
end;

procedure TMainFm.FindActionsMnuClick(Sender: TObject);
begin
  ShowFindActionsForm;
end;

procedure TMainFm.FindExprMnuClick(Sender: TObject);
begin
  ShowFindExprForm;
end;

procedure TMainFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if FLocked then
    CanClose := False
  {$ifdef DXFull}
  else if DesignFr <> nil then
  begin
    case MessageDlg(rsWarning, rsSaveYourChanges, mtConfirmation,
    	[mbYes, mbNo, mbCancel], 0) of
    	mrYes:
        begin
          DesignFr.UpdateAnchoredComponents(DesignFr.CurForm);
          CanClose := DesignFr.Save(True);
        end;
      mrCancel: CanClose := False;
    end;
  end
  {$endif}
  else if MainFr <> nil then
  begin
    {$ifdef DXFull}
    if (DebugScriptFm <> nil) and (DebugScriptFm.Paused) then
    	CanClose := False
    else{$endif} if AppConfig.ConfirmExit then
      CanClose := MessageDlg(rsWarning, rsExitMsg, mtWarning, [mbYes, mbNo], 0) = mrYes;
    if CanClose then
      CanClose := ValidateAndSave and CanCloseDatabase;
  end;
end;

procedure TMainFm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFr);
  try
    if (UserMan <> nil) and (UserMan.CurrentUser <> nil) and (DBase.Connected) then
      UserMan.UnRegisterUser;
  except
    on E: Exception do
      ErrMsg(rsErrorCloseApp + ExceptionToString(E, True, False), True, 'CloseApp');
  end;
  SaveConfig;
  HelpMan.Free;
  LogString('', 'CloseApp');
  FreeAndNil(DXMain);
  FreeAndNil(ScriptMan);
  FreeAndNil(UserMan);
  FreeAndNil(ReportMan);
  FreeAndNil(FormMan);
  FreeAndNil(ImageMan);
  FreeAndNil(DBase);
  FreeAndNil(FBLoader);
  FreeAndNil(AppConfig);
  FreeAndNil(OutputFm);
  FreeAndNil(LangMan);
  FreeAndNil(FParams);
  FreeAndNil(ImageCache);
  FreeImageLists;
end;

procedure TMainFm.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
var
  i: Integer;
  FlNm, Ext, Imported, Ignored: String;
  SD: TScriptData;
begin
  Imported := '';
  Ignored := '';

  for i := 0 to High(FileNames) do
  begin
    FlNm := FileNames[i];
    Ext := LowerCase(ExtractFileExt(FlNm));
    if (Ext = '.epas') or (Ext = '.wepas') then
      Imported := Imported + FlNm + LineEnding
    else
      Ignored := Ignored + FlNm + LineEnding;
  end;
  if Imported <> '' then
  begin
    TrimLineEnding(Imported);
    if Confirm(rsImportModules, Format(rsDragDropExtensionsMsg,
      [Spaces + Imported + Spaces])) <> mrYes then Exit;
  end
  else
  begin
    if Ignored <> '' then
    begin
      TrimLineEnding(Ignored);
      Info(Format(rsDropFilesNotSupport, [Spaces + Ignored + Spaces]));
    end;
    Exit;
  end;

  Imported := '';
  try

  for i := 0 to High(FileNames) do
  begin
    FlNm := FileNames[i];
    Ext := LowerCase(ExtractFileExt(FlNm));
    if (Ext = '.epas') or (Ext = '.wepas') then
    begin
      if ScriptMan.AddExtModule(FlNm, SD) then
        Imported := Imported + FlNm + LineEnding;
    end;
  end;

  if Imported <> '' then
  begin
    if MainFr <> nil then
      try
        if DXMain.CanProjectChange then
        begin
  		    ScriptMan.SaveToDB;
          DXMain.SetLastModified;
          DBase.Commit;
        end;
      except
        on E: Exception do
        begin
          ErrMsg(rsSaveModulesError + ExceptionToString(E, True, False));
          Exit;
        end;
      end
    else if (DesignFr <> nil) and (ScriptFm <> nil) then
      ScriptFm.Reset(True);
      //ReCreateScriptForm;

    TrimLineEnding(Imported);
    Info(rsDragDropExtensionsAfterMsg + Spaces + Imported);
    if ScriptMan.HasErrors then
      ShowCompilerErrors;
  end;

  except
    on E: Exception do
      ErrMsg(rsErrorWhileDragDropFiles + ExceptionToString(E, True, False));
  end;
end;

procedure TMainFm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if MainFr <> nil then
  begin
    case Key of
      VK_F3:
        begin
          if (MainFr.CurView <> nil) and (MainFr.CurView.Form.ViewType <> vtSimpleForm) then
	          MainFr.FindBn.Click;
          Key := 0;
        end;
      VK_F7:
        begin
          if (MainFr.CurView <> nil) and (MainFr.CurView.Form.ViewType <> vtSimpleForm) then
	          MainFr.FilterBn.Click;
          Key := 0;
        end;
    end;
  end
  {$ifdef DXFull}
  else if DesignFr <> nil then
  begin
    if (Key = VK_F4) and (Shift = []) and AppConfig.ExpertMode then DesignFr.ShowScriptForm
    else if (Key = VK_F5) and (Shift = []) and (DesignFr.CurForm <> nil) then
      DesignFr.TestForm(DesignFr.CurForm.FormCaption);
  end{$endif};
  if (UserMan.CurrentUser = nil) or (UserMan.CurrentUser.RoleId < 0) then
    case Key of
      VK_F6: begin Key := 0; Timer2.Enabled := True; end;
      VK_F11: begin Key := 0; Timer1.Enabled := True; end;
      VK_F12: begin Key := 0;  ReportsMnu.Click; end;
    end;
end;

{function GetDatabaseName: String;
begin
  Result := ParamStrUTF8(1);
  if Result = '' then
    Result := AppConfig.Database;
end;    }

function EscapeStringToString(const Str: String): String;
var
  S: String;
  Len, i: Integer;
  Code: Longint;
begin
  S := '';
  Len := Length(Str);
  i := 1;
  while i <= Len do
  begin
    if Str[i] = '%' then
    begin
      if TryStrToInt('$' + Copy(Str, i+1, 2), Code) then
      begin
        S := S + Chr(Code);
        Inc(i, 3);
        Continue;
      end;
    end;
    S := S + Str[i];
    Inc(i);
  end;
  Result := S;
end;

function ParseDXDB(S: String; out DBPath, User, Pwd, DBPwd: String): Boolean;
var
  p, Len: Integer;
  Param: String;
  Ch: Char;
  Flag: (fPath, fParam, fUser, fPwd, fDBPwd);
begin
  DBPath := ''; User := ''; Pwd := ''; DBPwd := '';
  if AnsiLowerCase(Copy(S, 1, 7)) <> 'dxdb://' then Exit(False);
  Result := True;

  Delete(S, 1, 7);
  S := EscapeStringToString(ExcludeTrailingPathDelimiter(S));
  Flag := fPath;
  p := 1; Len := Length(S);
  while p <= Len do
  begin
    Ch := S[p];
    if Ch = '&' then
    begin
      Flag := fParam;
      Param := '';
    end
    else
      case Flag of
        fPath: DBPath := DBPath + Ch;
        fUser: User := User + Ch;
        fPwd: Pwd := Pwd + Ch;
        fDBPwd: DBPwd := DBPwd + Ch;
        fParam:
          if Ch <> ':' then Param := Param + Ch
          else
          begin
            if Param = 'u' then Flag := fUser
            else if Param = 'p' then Flag := fPwd
            else if Param = 'dbpwd' then Flag := fDBPwd;
          end;
      end;
    Inc(p);
  end;
  if Pwd <> '' then Pwd := Decrypt(Pwd, StartKey, MultKey, AddKey);
  if DBPwd <> '' then DBPwd := Decrypt(DBPwd, StartKey, MultKey, AddKey);
end;

procedure TMainFm.FormShow(Sender: TObject);
var
  DBName, DBPath, User, Pwd, DBPwd, DXUpdateFile, DXUpdateFileTmp: String;
  ContinueOpen: Boolean;
  i: Integer;
  R: TRecentData;
  CI: TConnectInfo;
begin
  // На случай, если окно скроют, а потом покажут.
  if not FIsLoadApp then Exit;

  if not AppConfig.MainFormPosCorrected then
  begin
    CorrectFormPos(Self, Self);
    WindowState := AppConfig.FormState;
    AppConfig.MainFormPosCorrected := True;
  end;


  // Проверка обновлений...
  DXUpdateFile := AppPath + 'dxupdate.exe';
  if AppConfig.CheckUpdates and (AppConfig.UpdatesDBPath <> '') and FileExists(DXUpdateFile) then
  begin
    with TUpdateMan.Create do
    try try
      if Connect and CheckUpdates and (Confirm(rsWarning, IIF(UserMsg = '', rsDXUpdateAvailableMsg, UserMsg)) = mrYes) then
      begin
        ProgressFm.ShowForm(rsDownloadUpdatesMsg, False);
        DownloadFiles;

        DXUpdateFileTmp := GetTempDir + 'dxupdates.tmp' + PathDelim + 'dxupdate.exe';
        if FileExists(DXUpdateFileTmp) then
        begin
          CopyFile(DXUpdateFileTmp, DXUpdateFile, [cffOverwriteFile, cffPreserveTime], False);
          DeleteFile(DXUpdateFileTmp);
        end;
        ShellExec('open', DXUpdateFile, '', '', 5);
        Close;
        Exit;
      end;
    except
      on E: Exception do
      begin
        LogString(E.Message, 'UpdateApp');
        MessageDlg(rsUpdateError, E.Message, mtError, [mbOk], 0);
      end;
    end;
    finally
      ProgressFm.Close;
      Free;
    end;
  end;
  //

  ContinueOpen := True;

  if AppConfig.WasError and (AppConfig.StartupAction = saOpenLastDB) then
  begin
    if MessageDlg(rsWarning, rsLastErrorMsg, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    	ContinueOpen := False
  end;

  AppConfig.WasError:=False;

  if ContinueOpen then
  begin
    DBName := ParamStrUtf8(1);
    if DBName <> '' then
    begin
      AddRecent;
      AppConfig.ClearConnectInfo;
      if ParseDXDB(DBName, DBPath, User, Pwd, DBPwd) then
        OpenDatabase(DBPath, False, False, DBPwd, User, Pwd)
      else
      begin
        if not IsConnectName(DBName) then
        begin
          // Если в параметрах только путь к базе (например при двойном клике по
          // базе в проводнике), ...
          if ParamCount = 1 then
          begin
            // Эта проверка позволит нам открыть из командной строки или проводника
            // последнюю базу не сбивая настройки папок шаблонов и вывода.
            if AppConfig.Database <> DBName then
            begin
              //... то пытаемся найти в ее недавних.
              i := AppConfig.FindRecent(DBName);
              if i >= 0 then
              begin
                R := AppConfig.Recents[i];
                AppConfig.TemplateDir := R.TemplateDir;
                AppConfig.OutputDir := R.OutputDir;
              end
              // или в настроенных соединениях.
              else
              begin
                CI := AppConfig.Connects.FindConnectionByDBPath(DBName);
                if CI <> nil then
                begin
                  AppConfig.TemplateDir := CI.TemplateDir;
                  AppConfig.OutputDir := CI.OutputDir;
                end;
              end;
            end;
          end
          else
          begin
  	        AppConfig.TemplateDir:=GetCmdParam('t:');
    	      AppConfig.OutputDir:=GetCmdParam('o:');
          end
        end;

	      OpenDatabase(DBName, False, False, GetCmdParam('pwd:'), GetCmdParam('u:'), GetCmdParam('p:'));
      end;
    end
    else
    begin
      if AppConfig.StartupAction = saOpenLastDB then
      begin
        if AppConfig.ConnectName <> '' then
    		  DBName := '$' + AppConfig.ConnectName
    	  else
	        DBName := AppConfig.Database;
	      OpenDatabase(DBName, False, False, AppConfig.Pwd, '', '');
      end
      else
      	ConnectMnu.Click;
    end;
  end;

  UpdateMenuState;

  FIsLoadApp := False;
end;

procedure TMainFm.ExitMnuClick(Sender: TObject);
begin
  Close;
end;

procedure TMainFm.MonitorMnuClick(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  ShowMonitorForm;
end;

procedure TMainFm.ReportsMnuClick(Sender: TObject);
var
  mr: Integer;
begin
  {$ifdef DXFull}
  if not ValidateAndSave then Exit;

  mr := ShowReportsForm(DesignFr <> nil);
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
      ClearMenu;
      MainFr.BuildMenu;
    end;
  end;

  if (mr = mrOk) or (DesignFr <> nil) then
    UpdateTemplateFieldsForm;
  {$endif}
end;

procedure TMainFm.ApiReferenceMnuClick(Sender: TObject);
begin
  OpenUrl(WIKI_URL + 'api:');
end;

procedure TMainFm.SiteMnuClick(Sender: TObject);
begin
  OpenUrl('https://mydataexpress.ru');
end;

procedure TMainFm.ImportDataMnuClick(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
	if MainFr.CurView = nil then
	  ErrMsg(rsNoFormsAvailable)
  else if MainFr.CurView.Form.ViewType = vtSimpleForm then
  	ErrMsg(rsThisCommandNotApplySimpleForm)
  else
	  ShowImportForm(MainFr.CurView.Form);
end;

procedure TMainFm.ConnectMnuClick(Sender: TObject);
var
	CI: TConnectInfo;
	cna: TConnectAction;
begin
  if not ValidateAndSave then Exit;
  if ShowConnectsForm <> mrOk then Exit;
  CI := ConnectsFm.ConnectInfo;
  cna := ConnectsFm.ConnectAction;
  if cna in [cnaConnect, cnaDesign] then
  begin
    if not CanCloseDatabase then Exit;
    CloseDatabase;
	  UpdateMenuState;
    OpenDatabase('$'+CI.Name, False, cna = cnaDesign, '', '', '');
	  UpdateMenuState;
  end;
end;

procedure TMainFm.ExportPrjMnuClick(Sender: TObject);
var
  FlNm: String;
begin
  {$ifdef DXFull}
  with TSaveDialog.Create(nil) do
  try
    Title := rsExportProject;
    Filter:=rsImportExportPrjFilter;
    DefaultExt:='dxp';
    Options:=Options + [ofPathMustExist, ofOverwritePrompt];
    FileName := ExtractFileNameOnly(DBase.Database);
    if not Execute then Exit;
    FlNm := FileName;
  finally
    Free;
  end;
  DesignFr.ExportProject(FlNm);
  {$endif}
end;

procedure TMainFm.ImportPrjMnuClick(Sender: TObject);
var
  FlNm: String;
begin
  {$ifdef DXFull}
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
  {$endif}
end;

procedure TMainFm.ExportDataMnuClick(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  if MainFr.CurView = nil then
	  ErrMsg(rsNoFormsAvailable)
  else if MainFr.CurView.Form.ViewType = vtSimpleForm then
  	ErrMsg(rsThisCommandNotApplySimpleForm)
  else
    ShowExportForm(MainFr.CurView.Form);
end;

procedure TMainFm.DeleteRecsMnuClick(Sender: TObject);
begin
  if not ValidateAndSave then Exit;
  if MainFr.CurView = nil then
    ErrMsg(rsNoFormsAvailable)
  else if MainFr.CurView.Form.ViewType = vtSimpleForm then
  	ErrMsg(rsThisCommandNotApplySimpleForm)
  else if ShowDeleteRecsForm = mrOk then
    MainFr.CurView.DataSetProc.DeleteAllRecords;
end;

procedure TMainFm.SetValueMnuClick(Sender: TObject);
var
  TId, FId: Integer;
  Expr: String;
begin
  if not ValidateAndSave then Exit;
  if MainFr.CurView = nil then
    ErrMsg(rsNoFormsAvailable)
  else if MainFr.CurView.Form.ViewType = vtSimpleForm then
  	ErrMsg(rsThisCommandNotApplySimpleForm)
  else if ShowSetValueForm(MainFr.CurView.Form, TId, FId, Expr) then
  	try
	    MainFr.CurView.DataSetProc.Recalculate(TId, FId, Expr, True);
    except
      on E: Exception do
      	ErrMsg(rsRecalculateError + ExceptionToString(E, True, False));
    end;
end;

procedure TMainFm.ArticlesMnuClick(Sender: TObject);
begin
  OpenUrl(WIKI_URL + 'stati:');
end;

procedure TMainFm.MergePrjMnuClick(Sender: TObject);
var
  FlNm: String;
begin
  {$ifdef DXFull}
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
  {$endif}
end;

procedure TMainFm.ExpertMnuClick(Sender: TObject);
begin
  {$ifdef DXFull}
  AppConfig.ExpertMode:=not AppConfig.ExpertMode;
  DesignFr.SetExpertMode(AppConfig.ExpertMode);
  {$endif}
end;

procedure TMainFm.ProgMnuClick(Sender: TObject);
begin
  OpenUrl(WIKI_URL + 'programmirovanie_v_dx:');
end;

procedure TMainFm.ExtMnuClick(Sender: TObject);
begin
  if UserMan.IsUser then Exit;
  {$ifdef DXFull}
  if not ValidateAndSave then Exit;
  ShowModulesForm;
  {$endif}
end;

procedure TMainFm.ForumMnuClick(Sender: TObject);
begin
  OpenUrl('https://forum.mydataexpress.ru');
end;

procedure TMainFm.UpdatesMnuClick(Sender: TObject);
begin
  if UserMan.IsUser then Exit;
  ShowUpdateManForm;
end;

procedure TMainFm.VideoMnuClick(Sender: TObject);
begin
  OpenUrl(WIKI_URL + 'videouroki:');
end;

procedure TMainFm.OpenDemoMnuClick(Sender: TObject);
begin
  if not ValidateAndSave or not CanCloseDatabase then Exit;

  CloseDatabase;
  UpdateMenuState;
  OpenDatabase('DEMO_DB.FDB', False, False, '', '', '');
  UpdateMenuState;
end;

procedure TMainFm.UserReferenceMnuClick(Sender: TObject);
begin
  OpenUrl(WIKI_URL + 'rukovodstvo_po_polzovaniu_dx:');
end;

procedure TMainFm.PascalScriptMnuClick(Sender: TObject);
begin
  OpenUrl(WIKI_URL + 'remobject_pascal_script:');
end;

procedure TMainFm.GalleryMnuClick(Sender: TObject);
begin
  {$ifdef DXFull}
  ShowImagesForm(False, '', False);
  {$endif}
end;

procedure TMainFm.TemplatesFolderMnuClick(Sender: TObject);
begin
  {$ifdef DXFull}
  if not ValidateAndSave then Exit;
  OpenTemplateFolder;
  {$endif}
end;

procedure TMainFm.TemplatesHelperMnuClick(Sender: TObject);
begin
  {$ifdef DXFull}
  ShowTemplateFieldsForm;
  {$endif}
end;

procedure TMainFm.RecentsMnuHandler(Sender: TObject);
var
  R: TRecentData;
  i: PtrInt;
  DBName, Pwd, TemplDir, OutDir: String;
begin
  if not ValidateAndSave or not CanCloseDatabase then Exit;

  i := TMenuItem(Sender).Tag;
  R := AppConfig.Recents[i];
  if R.ConnectName <> '' then DBName := '$' + R.ConnectName
  else DBName := R.DBName;
  Pwd := R.Pwd;
  TemplDir := R.TemplateDir;
  OutDir := R.OutputDir;
  CloseDatabase;
  AppConfig.TemplateDir:=TemplDir;
  AppConfig.OutputDir:=OutDir;
  OpenDatabase(DBName, False, False, Pwd, '', '');
  UpdateMenuState;
end;

procedure TMainFm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  DesignMnu.Click;
end;

procedure TMainFm.Timer2Timer(Sender: TObject);
begin
  Timer2.Enabled := False;
  ConnectMnu.Click;
end;

procedure TMainFm.UsersMnuClick(Sender: TObject);
begin
  {$ifdef DXFull}
  if not ValidateAndSave then Exit;

  ShowUsersForm(DesignFr <> nil);
  if MainFr <> nil then
  begin
    ClearDataMenu;
    ClearReportMenu;
    ClearMenu;
    MainFr.BuildMenu;
    MainFr.UpdateMenu;
    MainFr.UpdateDataMenu;
  end;
  {$endif}
end;

procedure TMainFm.RecalcMnuClick(Sender: TObject);
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
    if ShowRecalcForm(MainFr.CurView.Form, TId, FId) then
    	try
	      MainFr.CurView.DataSetProc.Recalculate(TId, FId, '', True);
      except
        on E: Exception do
        	ErrMsg(rsRecalculateError + ExceptionToString(E, True, False));
      end;
  end;
end;

procedure TMainFm.AboutMnuClick(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TMainFm.CreateMnuClick(Sender: TObject);
var
  FName: String;
begin
  if not ValidateAndSave then Exit;

  with TSaveDialog.Create(nil) do
  try
    title:=rsCreateDialogTitle;
    Filter := ConstructDBOpenFilter(False);
    DefaultExt := GetDefaultDBExt;
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

  if not CanCloseDatabase then Exit;

  CloseDatabase;
  OpenDatabase(FName, True, False, '', '', '');
  UpdateMenustate;
end;

procedure TMainFm.OpenMnuClick(Sender: TObject);
var
  FName: String;
  i: Integer;
  RD: TRecentData;
begin
  if not ValidateAndSave then Exit;

  with TOpenDialog.Create(nil) do
  try
    Title:=rsOpenDatabase;
    Filter := ConstructDBOpenFilter(True);
    DefaultExt := GetDefaultDBExt;
    Options:=Options + [ofFileMustExist];
    if not Execute then Exit;
    FName := FileName;
  finally
    Free;
  end;

  if not CanCloseDatabase then Exit;
  CloseDatabase;
  UpdateMenuState;
  OpenDatabase(FName, False, False, '', '', '');
  i := AppConfig.FindRecent(AppConfig.Database);
  if i >= 0 then
  begin
  	RD := AppConfig.Recents[i];
    AppConfig.TemplateDir := RD.TemplateDir;
    Appconfig.OutputDir := RD.OutputDir;
  end;
  UpdateMenuState;
end;

procedure TMainFm.DesignMnuClick(Sender: TObject);
begin
  if UserMan.IsUser then Exit;

  if {not TMenuItem(Sender).Checked}MainFr <> nil then
  begin
    if not ValidateAndSave or not CanCloseDatabase then Exit;
    if DXMain.CanProjectChange then
    begin
      MainFr.SaveCurrentDataSet;
      ClearDataMenu;
      ClearReportMenu;
      ClearMenu;
      ShowDesigner;
    end;
  end
  else
  begin
    {if (ScriptFm <> nil) and ScriptFm.Visible then
    begin
      if (FindScriptFm <> nil) and FindScriptFm.Visible then
      begin
        if not FindScriptFm.Active then
        begin
          Beep;
          FindScriptFm.SetFocus;
        end;
      end
      else if not ScriptFm.Active then
      begin
        Beep;
        ScriptFm.SetFocus;
      end;
    end
    else }
      ShowMain;
  end;
  UpdateMenuState;
end;

procedure TMainFm.OptionsMnuClick(Sender: TObject);
begin
  if ShowSettingsForm = mrOk then
  begin
    if DesignFr <> nil then DesignFr.UpdateDesigner;
  end;
end;

procedure TMainFm.OpenDatabase(DBName: String; aCreateDB, aDesignDB: Boolean;
  DBPwd, User, Pwd: String);
var
  U: TdxUser;
  code, Ver: Integer;
  CI: TConnectInfo;
  Title: String;
begin
  if DBName = '' then Exit;
	CI := nil;
  // Это именованное соединение?
  if IsConnectName(DBName) then
  begin
    Delete(DBName, 1, 1);
    CI := AppConfig.Connects.FindConnection(DBName);
    if CI = nil then
    begin
      ErrMsgFmt(rsConnectNotFound, [DBName]);
      Exit;
    end;
    // Сначала remote, чтобы потом правильно присвоить путь к БД
    DBase.Database := CI.DBPath;
    DBase.Pwd := CI.DBPwd;
    User := CI.User;
    Pwd := CI.Pwd;
    Title := CI.Name;
    AppConfig.ConnectName := CI.Name;
    AppConfig.TemplateDir := CI.TemplateDir;
    AppConfig.OutputDir := CI.OutputDir;
  end
  else
  begin
    DBase.Database := DBName;
    DBase.Pwd := DBPwd;
    Title := Utf8UpperCase(ChangeFileExt(ExtractFileName(DBase.Database), ''));
    if ExtractFilePath(DBName) = AppPath then
      AppConfig.Database := ExtractFileName(DBase.Database)
    else
	    AppConfig.Database := DBName;
    AppConfig.Pwd := DBPwd;
  end;
  AppConfig.Save;

  if not DBase.IsRemote and not DBase.DatabaseExists then
  begin
    if aCreateDB then
    begin
      try
        DBase.CreateDatabase;
        {$ifndef windows}
        DBase.Disconnect;
        ExecuteProcess('/bin/chmod', ['666', DBase.Database]);
        {$endif}
      except
      	on E: Exception do
        begin
          ErrMsgFmt(rsFailedToCreateDB,	[ExceptionToString(E, True, False)]);
          Exit;
        end;
      end;
    end
    else
    begin
      ErrMsg(Format(rsDBNotFound, [DBase.Database]));
      Exit;
    end;
  end;

  MyLog.FileName := AppPath + 'logs' + PathDelim + ExtractFileNameOnly(DBName) + '.log';

  // Подключение

  try
    ShowProgress(rsConnectToDB2, False);
	  DBase.Connect(aCreateDB = False);
    LogString(DBase.Database, 'Connect');
  except
    on E: Exception do
    begin
      CloseProgress;
      ErrMsgFmt(rsErrConnect, [DBase.Database, ExceptionToString(E, True, False)], True, 'Connect');
      Exit;
    end;
  end;

  // Проверка метаданных

  try

    Ver := DBase.CheckVersion;
    if Ver < 31 then
      raise Exception.Create(rsDBNotSupport);

    if Ver < 32 then DXMain.CreateMain;
    if Ver < 33 then DXMain.UpdateMain;
    if Ver < 34 then ImageMan.CreateTable;
    if Ver < DX_VERSION then
    begin
      DXMain.UpdateMain2;
  	  DBase.UpdateVersion(DX_VERSION);
    end;
    if Ver > DX_VERSION then
      Info(rsNewDBDetectMsg);

  except
    on E: Exception do
    begin
      CloseProgress;
      ErrMsgFmt(rsErrorCheckDB, [ExceptionToString(E, True, False)], True, 'CheckMetaData');
      Exit;
    end;
  end;

  // Загрузка пользователей

  try
    UserMan.LoadFromDb;
  except
    on E: Exception do
    begin
      CloseProgress;
      ErrMsgFmt(rsErrorLoadMeta, [ExceptionToString(E, True, False)], True, 'LoadMetaData');
      Exit;
    end;
  end;

  // Вход

  if UserMan.Users.Count > 0 then
  begin
    U := nil;
    if User <> '' then
      U := UserMan.Users.FindUserByName(User);
    if (Pwd <> '') and (U <> nil) and (md5print(md5string(Pwd)) = U.Password) then
      UserMan.CurrentUserId:=U.Id
    else
    begin
      CloseProgress;
      if ShowLoginForm(Title, U) <> mrOk then
      begin
        CloseDatabase(False);
        Exit;
      end;
    end;
  end;

  if UserMan.CurrentUser <> nil then
  begin
    if IsNeedUserControl then
    begin

      code := UserMan.RegisterUser;
      if code = 10 then
        MessageDlg(rsWarning, rsSingleModeActiveUsers, mtWarning, [mbOk], 0)
      else if code > 0 then
      begin
        CloseProgress;
        case code of
          1: ErrMsg(rsDBInSingleMode);
          2: ErrMsg(Format(rsUserAlreadyLogged, [UserMan.CurrentUser.Name]));
          3: ErrMsg(rsActiveUsersInDB);
        end;
        UserMan.CurrentUserId:=-1;
        CloseDatabase(False);
        Exit;
      end;

    end;
  end;

  {$ifdef DXFull}
  // Мастер шаблонов недоступен пользователю
  if UserMan.IsUser then CloseTemplateFieldsForm;
  {$endif}

  // Загрузка медатанных

  try

    DXMain.LoadFromDB;
    if not CanCache or not IsFreshCache then
    begin
      ShowProgress(rsPreparingToWork, True);
      ImageMan.LoadFromDB;
      FormMan.LoadFromDB;
      ReportMan.LoadFromDB;
      ScriptMan.LoadFromDB;
      AppConfig.CacheLoaded := False;
    end
    else
    begin
      ShowProgress(rsPreparingToWork, False);
      LoadMetaFromCache;
      AppConfig.CacheLoaded := True;
    end;
    ScaleForms(FormMan, DXMain.DesignTimePPI);
    ScaleReports(ReportMan, DXMain.DesignTimePPI, Screen.PixelsPerInch);

  except
    // Возможно это никогда не сработает, потому что в FormMan ошибки чтения
    // обрабатываются и отображается другое сообщение.
    {on E: EReadError do
    begin
      CloseProgress;
      ErrMsgFmt(rsErrorReadingFormMetadata,
      	[LineEnding + LineEnding + E.Message + LineEnding + LineEnding]);
      Exit;
    end;  }
    on E: Exception do
    begin
      CloseProgress;
      ErrMsgFmt(rsErrorLoadMeta, [ExceptionToString(E, True, False)], True, 'OpenDatabase');
      Exit;
    end;
  end;

  CloseProgress;

  if UserMan.CurrentUser <> nil then
    SetAppCaption(rsAppName + ': ' + Title + ' - ' + UserMan.CurrentUser.Name)
  else
    SetAppCaption(rsAppName + ': ' + Title);

  if ((ssShift in GetKeyShiftState) or aDesignDB) and
  	((UserMan.CurrentUser = nil) or (UserMan.CurrentUser.RoleId < 0)) then
  begin
    // Компилируем иначе не будут определятся действия в редакторе действий
    ScriptMan.CompileExpr;
    ShowDesigner;
  end
  else if aDesignDB then
  begin
    ErrMsg(rsOnlyDevelCanEnterDesigner);
    CloseDatabase(False);
    Exit;
  end
  else
  begin
    LogString(UserMan.CurrentUserName, 'Login');
    ShowMain;
  end;

  {$ifdef DXFull}
  UpdateTemplateFieldsForm;
  {$endif}
end;

function TMainFm.GetFormViews(Index: Integer): TFormView;
begin
  if MainFr = nil then raise Exception.Create('The property is not available in designer mode.');
  Result := TFormView(MainFr.PageControl1.Pages[Index].Controls[0]);
end;

function TMainFm.GetPages: TPageControl;
begin
  if MainFr = nil then raise Exception.Create('The property is not available in designer mode.');
  Result := MainFr.PageControl1;
end;

function TMainFm.GetStatusBar: TStatusBar;
begin
  if MainFr = nil then raise Exception.Create('The property is not available in designer mode.');
  Result := MainFr.StatusBar1;
end;

function TMainFm.GetToolBar: TToolbar;
begin
  if MainFr = nil then raise Exception.Create('The property is not available in designer mode.');
  Result := MainFr.ToolBar1;
end;

{procedure TMainFm.RestoreConnection;
type
  TDSInfo = record
    Active, Opening: Boolean;
    RecNo: Integer;
    State: TDataSetState;
  end;
var
  Conn: TIBConnection;
  i: Integer;
  DS: TdxDataSet;
  DSInfo: array of TDSInfo;
begin
  Conn := DBase.Conn;
  SetLength(DSInfo, Conn.DataSetCount);
  for i := 0 to Conn.DataSetCount - 1 do
  begin
    DS := TdxDataSet(Conn.DataSets[i]);
    with DSInfo[i] do
    begin
      Opening := DS.Opening;
      Active := DS.Active;
      RecNo := DS.RecNo;
      State := DS.State;
    end;
  end;
  DBase.Disconnect;
  while True do
    try
      DBase.Connect;
      Break;
    except
      on E: Exception do
      begin
        if MessageDlg(rsError, 'Unable to reconnect' +
          ExceptionToString(E, True, True) + 'Try again?', mtError,
          [mbYes, mbNo], 0) = mrNo then Halt;
      end;
    end;

  for i := 0 to Conn.DataSetCount - 1 do
  begin
    DS := TdxDataSet(Conn.DataSets[i]);
    with DSInfo[i] do
      if Active or Opening then
      begin
        DS.Open;
        if RecNo > 1 then
          DS.MoveBy(RecNo - 1);
        if State = dsEdit then
          DS.Edit;
      end;
  end;
  //FormMan.LoadFromDb;
  SetLength(DSInfo, 0);
end;  }

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
end;

function TMainFm.ValidateAndSave: Boolean;
begin
  Result := True;
  if MainFr <> nil then
    Result := MainFr.ValidateAndSave;
end;

procedure TMainFm.AddRecent;
var
  RD: TRecentData;
begin
  RD := AppConfig.AddRecent(AppConfig.ConnectName, AppConfig.Database);
  if RD = nil then Exit;

  if RD.DBName <> '' then
  begin
    RD.Pwd := AppConfig.Pwd;
    RD.TemplateDir := AppConfig.TemplateDir;
    RD.OutputDir := AppConfig.OutputDir;
  end;

  BuildRecentsMenu;
end;

procedure TMainFm.BuildRecentsMenu;
var
  i: Integer;
  R: TRecentData;
  DBNm: String;
begin
  RecentsMnu.Clear;
  for i := 0 to AppConfig.RecentCount - 1 do
  begin
    R := AppConfig.Recents[i];
    if R.ConnectName <> '' then DBNm := R.ConnectName
    else DBNm := R.DBName;
    RecentsMnu.Add( CreateMenuItem(MainMenu1, DBNm, i, 0, @RecentsMnuHandler) );
  end;
  if RecentsMnu.Count = 0 then
  begin
    RecentsMnu.Add( CreateMenuItem(MainMenu1, rsEmpty, 0, 0, nil) );
    RecentsMnu.Items[0].Enabled:=False;
  end
  else
  begin
    RecentsMnu.Add(CreateMenuItem(MainMenu1, '-', 0, 0, nil));
    RecentsMnu.Add(CreateMenuItem(MainMenu1, rsClearHistory, 0, 0,
      @ClearHistoryMnuHandler));
  end;
end;

procedure TMainFm.RestoreFormatSettings;
begin
  DefaultFormatSettings := FOldFormatSettings;
end;

procedure TMainFm.RestoreMainMenu;

  procedure SetVisibleItem(MI: TMenuItem);
  var
    i: Integer;
  begin
    MI.Visible := True;
    for i := 0 to MI.Count - 1 do
      SetVisibleItem(MI.Items[i]);
  end;

begin
  Menu := MainMenu1;
  // При закрытии программы Menu=nil
  if Menu <> nil then
    SetVisibleItem(Menu.Items);
end;

procedure TMainFm.OpenTemplateFolder;
var
  S, FM: String;
begin
  S := '"' + GetTemplatesDir + '"';
  {$ifdef windows}
  ExecuteProcess('explorer.exe', [{Utf8ToWinCP}(S)]);
  {$else}
  FM := GetUnixFileManager;
  if S <> '' then
    ShellExec('', FM, S, '', 0)
  else
    ErrMsg(rsUnableFindFileManager);
  {$endif}
end;

function TMainFm.CanCloseDatabase: Boolean;
begin
  Result := True;
  if FOnDatabaseCloseQuery <> nil then FOnDatabaseCloseQuery(Self, Result);
end;

procedure TMainFm.EnableDropFiles(AEnabled: Boolean);
begin
  if AEnabled then
  begin
    AllowDropFiles := True;
    OnDropFiles := @FormDropFiles;
  end
  else
  begin
    AllowDropFiles := False;
    OnDropFiles := nil;
  end;
end;

procedure TMainFm.SaveConfig;
var
  R: TRect;
begin
  if WindowState in [wsNormal, wsMaximized] then
    AppConfig.FormState := WindowState;
  R := ScaleRectTo96(GetFormRealBounds(Self));
  AppConfig.FormLeft := R.Left;
  Appconfig.FormTop := R.Top;
	AppConfig.FormWidth := R.Width;
  AppConfig.FormHeight := R.Height;
  if OutputFm <> nil then OutputFm.Close;
  AppConfig.Save;
end;

procedure TMainFm.SetAppCaption(const aCaption: String);
begin
  Caption := aCaption;
  Application.Title := aCaption;
end;

procedure TMainFm.CloseDatabase(ClearConnectInfo: Boolean);
begin
  try
    if UserMan.CurrentUser <> nil then
      UserMan.UnRegisterUser;
  except
    on E: Exception do
      ErrMsg(rsErrorWhileDisconnect + ExceptionToString(E, True, False), True, 'Disconnect');
  end;
  //if (FFr <> nil) and (FFr = MainFr) then
  //	MainFr.Done;
  FreeAndNil(FFr);

  if FindActionsFm <> nil then FindActionsFm.Reset;
  if FindExprFm <> nil then FindExprFm.Reset;
  if FindScriptFm <> nil then FindScriptFm.Reset;

  SelectedFormId:=0;
  DesignPageIndex:=0;

  DBase.Disconnect;

  AddRecent;

  LogString('', 'Logout');

  DBase.Database := '';
  FormMan.Clear;
  ReportMan.Clear;
  UserMan.Clear;
  ClearMenu;
  ClearDataMenu;
  ClearReportMenu;
  ClearImageLists;
  ImageCache.Clear;
  if ClearConnectInfo then AppConfig.ClearConnectInfo;
  SetAppCaption(rsAppName);
  {$ifdef DXFull}
  ResetTemplateFieldsForm;
  {$endif}
end;

procedure TMainFm.ShowDesigner;
begin
  {$ifdef DXFull}
  //if FFr is TMainFr then
  //  MainFr.Done;
  FreeAndNil(FFr);

  // На случай обрыва соединения, блокируем пункты меню
  UpdateMenuState;

  DBase.ReadCommit;

  FFr := TDesignFr.Create(Self);
  with TDesignFr(FFr) do
  begin
    Parent := Self;
    Align := alClient;
    Visible := True;
    Init;
  end;
  DesignMnu.Checked := True;
  {$endif}
end;

procedure TMainFm.ShowMain;
begin
  {$IFDEF DXFull}
  if DesignFr <> nil then
  begin
    if not DesignFr.Save(True) then Exit;
    //DesignFr.Done;
  end;
  FreeAndNil(FFr);
  DesignMnu.Checked := False;
  {$ENDIF}
  FFr := TMainFr.Create(Self);
  with TMainFr(FFr) do
  begin
    Parent := Self;
    Align := alClient;
    Visible := True;

    try
	    Init;
    except
      on E: Exception do
        ErrMsgFmt(rsErrorRunDB, [ExceptionToString(E, True, True)], True, 'Init');
    end;
  end;
end;

procedure TMainFm.UpdateMenuState;
var
  IsDevel: Boolean;
begin
  IsDevel := IsDeveloper; //(UserMan.CurrentUser = nil) or (UserMan.CurrentUser.RoleId < 0);

  {$IFDEF DXFull}
  CreateMnu.Visible := (DesignFr = nil) and IsDevel;
  OpenMnu.Visible := DesignFr = nil;
  ConnectMnu.Visible := DesignFr = nil;
  ConnectMnu.Enabled := ConnectMnu.Visible;
  RecentsMnu.Visible := DesignFr = nil;
  RecentsDivMnu.Visible := DesignFr = nil;
  DesignMnu.Enabled := (FFr <> nil) and IsDevel;
  DesignMnu.Visible:=IsDevel;
  ReportsMnu.Enabled := (FFr <> nil) and IsDevel;
  ReportsMnu.Visible:=IsDevel;
  TemplatesMnu.Visible := IsDevel;
  TemplatesMnu.Enabled := FFr <> nil;
  GalleryMnu.Visible := DesignFr <> nil;
  UsersMnu.Enabled:= (FFr <> nil) and IsDevel;
  UsersMnu.Visible := IsDevel;
  ExtMnu.Enabled := (FFr <> nil) and IsDevel;
  ExtMnu.Visible := IsDevel;
  ExtDivMnu.Visible := DesignFr <> nil;
  ExpertDivMnu.Visible := IsDevel;
  ExpertMnu.Visible := DesignFr <> nil;
  UpdatesMnu.Enabled := (FFr <> nil) and IsDevel;
  UpdatesMnu.Visible := IsDevel;
  FindActionsDivMnu.Visible := DesignFr <> nil;
  FindActionsMnu.Visible := DesignFr <> nil;
  FindExprMnu.Visible := DesignFr <> nil;
  {$ENDIF}

  DataMnu.Visible := {$ifdef DXFull}(DesignFr = nil) and {$endif}(DataMnu.Count > 0);
  ReportMnu.Visible := {$ifdef DXFull}(DesignFr = nil) and {$endif}(ReportMnu.Count > 0);
  ToolsMnu.Visible := IsDevel;
  RecalcMnu.Visible := MainFr <> nil;
  SetValueMnu.Visible:=MainFr <> nil;
  DeleteRecsMnu.Visible:=MainFr <> nil;
  DeleteRecsDivMnu.Visible:=MainFr <> nil;
  ImportDataMnu.Visible := MainFr <> nil;
  ExportDataMnu.Visible := MainFr <> nil;
  {$IFDEF DXFull}
  ExportPrjMnu.Visible := DesignFr <> nil;
  ImportPrjMnu.Visible := DesignFr <> nil;
  MergePrjMnu.Visible := DesignFr <> nil;
  MergePrjDivMnu.Visible := FFr <> nil;
  {$ENDIF}
  MonitorMnu.Visible := FFr <> nil;
  {$IFDEF DXFull}
  ArticlesDivMnu.Visible := DesignFr = nil;
  OpenDemoMnu.Visible := DesignFr = nil;
  {$ENDIF}
end;

procedure TMainFm.Lock(Value: Boolean);
var
  i: Integer;
begin
  FLocked := Value;
  FFr.Enabled:=not Value;
  for i := 0 to MainMenu1.Items.Count - 1 do
  	MainMenu1.Items[i].Enabled := not Value;
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

procedure TMainFm.ClearEventHandlers;
begin
  OnChangeBounds := nil;
  OnClick := nil;
  OnShowHint := nil;

  OnEnter := nil;
  OnExit := nil;
  OnKeyDown := @FormKeyDown;
  OnKeyPress := nil;
  OnKeyUp := nil;

  OnPaint := nil;

  OnMouseDown := nil;
  OnMouseMove := nil;
  OnMouseUp := nil;
  OnMouseEnter := nil;
  OnMouseLeave := nil;

  OnActivate := nil;
  OnDblClick := nil;
  OnClose := @FormClose;
  OnCloseQuery := @FormCloseQuery;
  OnDestroy := @FormDestroy;
  OnDeactivate := nil;
  OnHide := nil;
  OnResize := nil;
  OnShow := @FormShow;
  OnWindowStateChange := nil;
  OnDropFiles := @FormDropFiles;

  OnCreateListWindow := nil;
  OnCreateReportWindow := nil;
  OnCreateForm := nil;
  OnDestroyForm := nil;
  OnDatabaseClose := nil;
  OnDatabaseCloseQuery := nil;
  OnFatalError := nil;

  FParams.OnSetParam := nil;
  FParams.OnGetParam := nil;
end;

initialization
  {$i images.lrs}

end.

