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

unit AppSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, IniFiles, Graphics, strconsts;

type

  { TRecentData }

  TRecentData = class
  public
    DBName: String;
    //Remote: Boolean;
    Pwd: String;
    TemplateDir: String;
    OutputDir: String;
    ConnectName: String;
    function IsRemote: Boolean;
  end;

  { TConnectInfo }

  TConnectInfo = class
  public
    Name, Group, DBPath, DBPwd, TemplateDir, OutputDir, User, Pwd: String;
    function IsRemote: Boolean;
  end;

  { TConnections }

  TConnections = class(TList)
  private
    function GetConnections(Index: Integer): TConnectInfo;
  public
    function AddConnection: TConnectInfo;
    function FindConnection(const aName: String): TConnectInfo;
    function FindConnectionByDBPath(const DBPath: String): TConnectInfo;
    procedure DeleteConnection(CI: TConnectInfo);
    procedure Clear; override;
    property Connections[Index: Integer]: TConnectInfo read GetConnections; default;
  end;

  TStartupAction = (saOpenLastDB, saOpenConnectList);

  { TAppSettings }

  TAppSettings = class
  private
    FAEHeight: Integer;
    FAELeftPanWidth: Integer;
    FAERightPanWidth: Integer;
    FAEWidth: Integer;
    FAppDocXFile: String;
    FAppHtmlFile: String;
    FAppOdsFile: String;
    FAppOdtFile: String;
    FAppXmlFile: String;
    FCacheLoaded: Boolean;
    FCaching: Boolean;
    FCheckUpdates: Boolean;
    FConfirmExit: Boolean;
    FConnectName: String;
    FDatabase: String;
    FDebugFormHeight: Integer;
    FDebugFormLeft: Integer;
    FDebugFormPosCorrected: Boolean;
    FDebugFormTop: Integer;
    FDebugFormWidth: Integer;
    FExpertMode: Boolean;
    FExprFormHeight: Integer;
    FExprFormWidth: Integer;
    FFormHeight: Integer;
    FFormLeft: Integer;
    FFormState: TWindowState;
    FFormTop: Integer;
    FFormWidth: Integer;
    FGridColor: TColor;
    FGridSizeX: Integer;
    FGridSizeY: Integer;
    FIsWine: Boolean;
    //FHideParents: Boolean;
    FLanguage: String;
    FLeftPanelWidth: Integer;
    FLogErrors: Boolean;
    FMainFormPosCorrected: Boolean;
    FOutputBounds: TRect;
    FOutputDir: String;
    FOutputFormPosCorrected: Boolean;
    FPPI: Integer;
    //FRemote: Boolean;
    FRightPanelWidth: Integer;
    FScriptFormPosCorrected: Boolean;
    FSEClassesHelpHeight: Integer;
    FSEClassesHideAncestors: Boolean;
    FSEClassesHideBaseClasses: Boolean;
    FSEClassesHideHelp: Boolean;
    //FSearchUrl: String;
    FSEClassesWidth: Integer;
    FSEFmId, FSEWebFmId: Integer;
    FSEFormLeft: Integer;
    FSEFormTop: Integer;
    FSEFormHeight: Integer;
    FSEFormState: TWindowState;
    FSEFormWidth: Integer;
    FSEModulesWidth: Integer;
    FSEMsgHeight: Integer;
    FSESDName: String;
    FShowGrid: Boolean;
    FSQLEditorHeight: Integer;
    FSQLEditorWidth: Integer;
    FSQLModeEditorRightPanelWidth: Integer;
    FSQLModeEditorHeight: Integer;
    FSQLModeEditorWidth: Integer;
    FStartupAction: TStartupAction;
    FSupportDXDB: Boolean;
    FTemplateDir: String;
    FPwd: String;
    FTemplatesFormPosCorrected: Boolean;
    FTWHeight: Integer;
    FTWLeft: Integer;
    FTWTop: Integer;
    FTWTopPan2Height: Integer;
    FTWTopPanHeight: Integer;
    FTWWidth: Integer;
    FUpdatesDBPath: String;
    FUpdatesDBPwd: String;
    FWasError: Boolean;
    FRecents: TList;
    FConnects: TConnections;
    function GetPwd: String;
    function GetRecentCount: Integer;
    function GetRecents(Index: Integer): TRecentData;
    procedure SetGridSizeX(AValue: Integer);
    procedure SetGridSizeY(AValue: Integer);
    procedure SetPwd(AValue: String);
    procedure ReadConnections(Ini: TIniFile);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Save;
    procedure Load;
    procedure SaveConnections;
    procedure LoadConnections;
    procedure LoadTemplateWizard;
    procedure SaveTemplateWizard;
    function NewRecent: TRecentData;
    function AddRecent(aConnectName, aDBName: String): TRecentData;
    function FindRecent(const aDBName: String): Integer;
    function DeleteRecent(const aDBName: String): Boolean;
    procedure ClearRecents;
    procedure ClearConnectInfo;
    property Database: String read FDatabase write FDatabase;
    //property Remote: Boolean read FRemote write FRemote;
    property TemplateDir: String read FTemplateDir write FTemplateDir;
    property OutputDir: String read FOutputDir write FOutputDir;
    property AppXmlFile: String read FAppXmlFile write FAppXmlFile;
    property AppDocXFile: String read FAppDocXFile write FAppDocXFile;
    property AppOdtFile: String read FAppOdtFile write FAppOdtFile;
    property AppOdsFile: String read FAppOdsFile write FAppOdsFile;
    property AppHtmlFile: String read FAppHtmlFile write FAppHtmlFile;
    property Language: String read FLanguage write FLanguage;
    property FormLeft: Integer read FFormLeft write FFormLeft;
    property FormTop: Integer read FFormTop write FFormTop;
    property FormWidth: Integer read FFormWidth write FFormWidth;
    property FormHeight: Integer read FFormHeight write FFormHeight;
    property FormState: TWindowState read FFormState write FFormState;
    property ConfirmExit: Boolean read FConfirmExit write FConfirmExit;
    property Caching: Boolean read FCaching write FCaching;
    property CacheLoaded: Boolean read FCacheLoaded write FCacheLoaded;
    property CheckUpdates: Boolean read FCheckUpdates write FCheckUpdates;
    property UpdatesDBPath: String read FUpdatesDBPath write FUpdatesDBPath;
    property UpdatesDBPwd: String read FUpdatesDBPwd write FUpdatesDBPwd;
    property Pwd: String read GetPwd write SetPwd;
    property SupportDXDB: Boolean read FSupportDXDB write FSupportDXDB;
    property LogErrors: Boolean read FLogErrors write FLogErrors;
    property WasError: Boolean read FWasError write FWasError;
    property Recents[Index: Integer]: TRecentData read GetRecents;
    property RecentCount: Integer read GetRecentCount;
    // Редактор скриптов
    //property HideParents: Boolean read FHideParents write FHideParents;
    //property SearchUrl: String read FSearchUrl write FSearchUrl;
    property SEFormLeft: Integer read FSEFormLeft write FSEFormLeft;
    property SEFormTop: Integer read FSEFormTop write FSEFormTop;
    property SEFormWidth: Integer read FSEFormWidth write FSEFormWidth;
    property SEFormHeight: Integer read FSEFormHeight write FSEFormHeight;
    property SEFormState: TWindowState read FSEFormState write FSEFormState;
    property SEModulesWidth: Integer read FSEModulesWidth write FSEModulesWidth;
    property SEClassesWidth: Integer read FSEClassesWidth write FSEClassesWidth;
    property SEClassesHelpHeight: Integer read FSEClassesHelpHeight write FSEClassesHelpHeight;
    property SEClassesHideBaseClasses: Boolean read FSEClassesHideBaseClasses write FSEClassesHideBaseClasses;
    property SEClassesHideAncestors: Boolean read FSEClassesHideAncestors write FSEClassesHideAncestors;
    property SEClassesHideHelp: Boolean read FSEClassesHideHelp write FSEClassesHideHelp;
    property SEMsgHeight: Integer read FSEMsgHeight write FSEMsgHeight;
    // Не сохраняется
    property SESDName: String read FSESDName write FSESDName;
    property SEFmId: Integer read FSEFmId write FSEFmId;
    property SEWebFmId: Integer read FSEWebFmId write FSEWebFmId;
    // Мастер шаблонов
    property TWLeft: Integer read FTWLeft write FTWLeft;
    property TWTop: Integer read FTWTop write FTWTop;
    property TWWidth: Integer read FTWWidth write FTWWidth;
    property TWHeight: Integer read FTWHeight write FTWHeight;
    property TWTopPanHeight: Integer read FTWTopPanHeight write FTWTopPanHeight;
    property TWTopPan2Height: Integer read FTWTopPan2Height write FTWTopPan2Height;
    // Редактор действий
    property AEWidth: Integer read FAEWidth write FAEWidth;
    property AEHeight: Integer read FAEHeight write FAEHeight;
    property AELeftPanWidth: Integer read FAELeftPanWidth write FAELeftPanWidth;
    property AERightPanWidth: Integer read FAERightPanWidth write FAERightPanWidth;
    //
    property ExpertMode: Boolean read FExpertMode write FExpertMode;
    property DebugFormLeft: Integer read FDebugFormLeft write FDebugFormLeft;
    property DebugFormTop: Integer read FDebugFormTop write FDebugFormTop;
    property DebugFormWidth: Integer read FDebugFormWidth write FDebugFormWidth;
    property DebugFormHeight: Integer read FDebugFormHeight write FDebugFormHeight;
    property ExprFormWidth: Integer read FExprFormWidth write FExprFormWidth;
    property ExprFormHeight: Integer read FExprFormHeight write FExprFormHeight;
    property Connects: TConnections read FConnects;
    property ConnectName: String read FConnectName write FConnectName;
    property StartupAction: TStartupAction read FStartupAction write FStartupAction;
    property OutputBounds: TRect read FOutputBounds write FOutputBounds;
    property LeftPanelWidth: Integer read FLeftPanelWidth write FLeftPanelWidth;
    property RightPanelWidth: Integer read FRightPanelWidth write FRightPanelWidth;
    property PPI: Integer read FPPI write FPPI;
    property IsWine: Boolean read FIsWine;
    // Дизайнер
    property GridSizeX: Integer read FGridSizeX write SetGridSizeX;
    property GridSizeY: Integer read FGridSizeY write SetGridSizeY;
    property GridColor: TColor read FGridColor write FGridColor;
    property ShowGrid: Boolean read FShowGrid write FShowGrid;
    // Корректировка положения окон
    property MainFormPosCorrected: Boolean read FMainFormPosCorrected write FMainFormPosCorrected;
    property ScriptFormPosCorrected: Boolean read FScriptFormPosCorrected write FScriptFormPosCorrected;
    property DebugFormPosCorrected: Boolean read FDebugFormPosCorrected write FDebugFormPosCorrected;
    property OutputFormPosCorrected: Boolean read FOutputFormPosCorrected write FOutputFormPosCorrected;
    property TemplatesFormPosCorrected: Boolean read FTemplatesFormPosCorrected write FTemplatesFormPosCorrected;
    // Редактор SQL (скрипты)
    property SQLEditorWidth: Integer read FSQLEditorWidth write FSQLEditorWidth;
    property SQLEditorHeight: Integer read FSQLEditorHeight write FSQLEditorHeight;
    // Редактор SQL (отбор)
    property SQLModeEditorWidth: Integer read FSQLModeEditorWidth write FSQLModeEditorWidth;
    property SQLModeEditorHeight: Integer read FSQLModeEditorHeight write FSQLModeEditorHeight;
    property SQLModeEditorRightPanelWidth: Integer read FSQLModeEditorRightPanelWidth
      write FSQLModeEditorRightPanelWidth;
  end;

var
  AppConfig: TAppSettings;

implementation

uses
  LazUtf8, apputils, crypt, dynlibs;

type

  { TIniFileEx }

  TIniFileEx = class(TIniFile)
  public
    procedure WriteString(const Section, Ident, Value: String); override;
    function ReadString(const Section, Ident, Default: string): string; override;
  end;

function UnderWine: Boolean;
var
  H: cardinal;
begin
  Result := False;
  H := LoadLibrary('ntdll.dll');
  if H <> NilHandle then
  begin
    Result := Assigned(GetProcAddress(H, 'wine_get_version'));
    FreeLibrary(H);
  end;
end;

function DeleteQuotesIfNeed(const S: String): String;
var
  L: Integer;
begin
  Result := S;
  L := Length(Result);
  if (L > 1) and (Result[1] = '"') and (Result[L] = '"') and
    ((Result[L - 1] = ' ') or (Result[2] = ' ')) then
    Result := Copy(Result, 2, L - 2);
end;

{ TIniFileEx }

procedure TIniFileEx.WriteString(const Section, Ident, Value: String);
var
  L: Integer;
  S: String;
begin
  S := Value;
  L := Length(S);
  if (L > 0) and ((S[1] = ' ') or (S[L] = ' ')) then S := '"' + S + '"';
  inherited WriteString(Section, Ident, S);
end;

function TIniFileEx.ReadString(const Section, Ident, Default: string): string;
begin
  Result:=inherited ReadString(Section, Ident, Default);
  Result := DeleteQuotesIfNeed(Result);
end;

{ TRecentData }

function TRecentData.IsRemote: Boolean;
begin
  Result := IsRemoteDatabase(DBName);
end;

{ TConnectInfo }

function TConnectInfo.IsRemote: Boolean;
begin
  Result := IsRemoteDatabase(DBPath);
end;

{ TConnections }

function TConnections.GetConnections(Index: Integer): TConnectInfo;
begin
  Result := TConnectInfo(Items[Index]);
end;

function TConnections.AddConnection: TConnectInfo;
begin
	Result := TConnectInfo.Create;
  Add(Result);
end;

function TConnections.FindConnection(const aName: String): TConnectInfo;
var
  i: Integer;
  CI: TConnectInfo;
begin
  Result := nil;
	for i := 0 to Count - 1 do
  begin
    CI := Connections[i];
    if MyUtf8CompareText(CI.Name, aName) = 0 then Exit(CI);
  end;
end;

function TConnections.FindConnectionByDBPath(const DBPath: String
  ): TConnectInfo;
var
  i: Integer;
  CI: TConnectInfo;
begin
  Result := nil;
	for i := 0 to Count - 1 do
  begin
    CI := Connections[i];
    if MyUtf8CompareText(CI.DBPath, DBPath) = 0 then Exit(CI);
  end;
end;

procedure TConnections.DeleteConnection(CI: TConnectInfo);
begin
  Remove(CI);
  CI.Free;
end;

procedure TConnections.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Connections[i].Free;
  inherited Clear;
end;

{ TAppSettings }

function TAppSettings.GetPwd: String;
begin
  Result := Decrypt(FPwd, StartKey, MultKey, AddKey);
end;

function TAppSettings.GetRecentCount: Integer;
begin
  Result := FRecents.Count;
end;

function TAppSettings.GetRecents(Index: Integer): TRecentData;
begin
  Result := TRecentData(FRecents[Index]);
end;

procedure TAppSettings.SetGridSizeX(AValue: Integer);
begin
  if FGridSizeX=AValue then Exit;
  if AValue < 5 then AValue := 5
  else if AValue > 32 then AValue := 32;
  FGridSizeX:=AValue;
end;

procedure TAppSettings.SetGridSizeY(AValue: Integer);
begin
  if FGridSizeY=AValue then Exit;
  if AValue < 5 then AValue := 5
  else if AValue > 32 then AValue := 32;
  FGridSizeY:=AValue;
end;

procedure TAppSettings.SetPwd(AValue: String);
begin
  FPwd := Encrypt(AValue, StartKey, MultKey, AddKey);
end;

procedure TAppSettings.ReadConnections(Ini: TIniFile);
var
  SL: TStringList;
  i: Integer;
  S: String;
  CI: TConnectInfo;
begin
  ClearList(FConnects);
  SL := TStringList.Create;
  Ini.ReadSectionValues('Connections', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    CutStr(S, '=');
    S := DeleteQuotesIfNeed(S);
    CI := FConnects.AddConnection;
    CI.Name := CutStr(S, '|');
    CI.Group := CutStr(S, '|');
    CI.DBPath := CutStr(S, '|');
    {CI.Remote := Str2Bool}(CutStr(S, '|'));
    CI.DBPwd := Decrypt(CutStr(S, '|'), StartKey, MultKey, AddKey);
    CI.TemplateDir := CutStr(S, '|');
    CI.OutputDir := CutStr(S, '|');
    CI.User := CutStr(S, '|');
    CI.Pwd := Decrypt(CutStr(S, '|'), StartKey, MultKey, AddKey);
  end;
  SL.Free;
end;

constructor TAppSettings.Create;
begin
  FRecents := TList.Create;
  FConnects := TConnections.Create;

  FLanguage := '';
  FFormWidth := 770;
  FFormHeight := 590;
  FFormState := wsNormal;
  FConfirmExit := False;
  FStartupAction := saOpenLastDB;
  FDatabase := '';
  FPwd := Encrypt('masterkey', StartKey, MultKey, AddKey);
	FConnectName := '';
  FTemplateDir := '';
  FOutputDir := '';
  FAppXmlFile := '';
  FAppDocXFile := '';
  FAppODtFile := '';
  FAppODSFile := '';
  FAppHtmlFile := '';
  FWasError := False;
  FExpertMode := False;
  FDebugFormWidth := 750;
  FDebugFormHeight := 480;
  FExprFormWidth := 700;
  FExprFormHeight := 273;
  FOutputBounds := TRect.Empty;
  FLeftPanelWidth := 220;
  FRightPanelWidth := 250;
  FSEFormWidth := 1000;
  FSEFormHeight := 600;
  FSEFormState := wsNormal;
  FSEClassesWidth := 217;
  FSEModulesWidth := 231;
  FSEMsgHeight := 81;
  FIsWine := UnderWine;
end;

destructor TAppSettings.Destroy;
begin
  FConnects.Free;
  ClearList(FRecents);
  FRecents.Free;
  inherited Destroy;
end;

procedure TAppSettings.Save;

  function GetOutputBoundsStr: String;
  var
    R: TRect;
  begin
    R := FOutputBounds;
    Result := Format('%d;%d;%d;%d', [R.Left, R.Top, R.Right, R.Bottom]);
  end;

var
  i: Integer;
  S: String;
  R: TRecentData;
  Ini: TIniFileEx;
begin
  try
    Ini := TIniFileEx.Create(AppPath + 'dataexpress.cfg');
    with Ini do
    try
      CacheUpdates:=True;
      DeleteKey('Script Editor', 'Classes_SearchUrl');
      DeleteKey('Script Editor', 'Classes_HideParents');

      WriteString('UI', 'Language', FLanguage);
      WriteInteger('UI', 'FormLeft', FFormLeft);
      WriteInteger('UI', 'FormTop', FFormTop);
      WriteInteger('UI', 'FormWidth', FFormWidth);
      WriteInteger('UI', 'FormHeight', FFormHeight);
      WriteInteger('UI', 'FormState', Ord(FFormState));
      WriteBool('UI', 'ConfirmExit', FConfirmExit);
      WriteBool('UI', 'CacheMetadata', FCaching);
      WriteBool('UI', 'SupportDXDB', FSupportDXDB);
      WriteBool('UI', 'LogErrors', FLogErrors);
      WriteInteger('UI', 'StartupAction', Ord(FStartupAction));
      WriteBool('UI', 'WasError', FWasError);
      WriteBool('UI', 'ExpertMode', FExpertMode);
      WriteInteger('UI', 'DebugFormLeft', FDebugFormLeft);
      WriteInteger('UI', 'DebugFormTop', FDebugFormTop);
      WriteInteger('UI', 'DebugFormWidth', FDebugFormWidth);
      WriteInteger('UI', 'DebugFormHeight', FDebugFormHeight);
      WriteInteger('UI', 'ExprFormWidth', FExprFormWidth);
      WriteInteger('UI', 'ExprFormHeight', FExprFormHeight);
      if not FOutputBounds.IsEmpty then
        WriteString('UI', 'OutputFormBounds', GetOutputBoundsStr);
      WriteInteger('UI', 'LeftPanelWidth', FLeftPanelWidth);
      WriteInteger('UI', 'RightPanelWidth', FRightPanelWidth);

      WriteString('Folders', 'Templates', FTemplateDir);
      WriteString('Folders', 'Output', FOutputDir);
      WriteString('Apps', 'XML', FAppXmlFile);
      WriteString('Apps', 'DOCX', FAppDocXFile);
      WriteString('Apps', 'ODT', FAppOdtFile);
      WriteString('Apps', 'ODS', FAppOdsFile);
      WriteString('Apps', 'HTML', FAppHtmlFile);

      if FConnectName <> '' then
      begin
        DeleteKey('Connection', 'Database');
        DeleteKey('Connection', 'Password');
        WriteString('Connection', 'ConnectionName', FConnectName);
      end
      else //if FDatabase <> '' then
      begin
        WriteString('Connection', 'Database', FDatabase);
        WriteString('Connection', 'Password', FPwd);
        DeleteKey('Connection', 'ConnectionName');
      end;

      for i := 1 to 7 do
        DeleteKey('Recents', IntToStr(i));

      for i := 0 to FRecents.Count - 1 do
      begin
        R := Recents[i];
        if R.ConnectName = '' then
	        S := R.DBName + '|' +  Bool2Str(R.IsRemote) + '|' +
  	        Encrypt(R.Pwd, StartKey, MultKey, AddKey) + '|' +
            R.TemplateDir + '|' + R.OutputDir
        else
          S := '$' + R.ConnectName;
        WriteString('Recents', IntToStr(i + 1), S);
      end;

      WriteInteger('Script Editor', 'Left', FSEFormLeft);
      WriteInteger('Script Editor', 'Top', FSEFormTop);
      WriteInteger('Script Editor', 'Width', FSEFormWidth);
      WriteInteger('Script Editor', 'Height', FSEFormHeight);
      WriteInteger('Script Editor', 'State', Ord(FSEFormState));
      //WriteString('Script Editor', 'Classes_SearchUrl', FSearchUrl);
      WriteInteger('Script Editor', 'Classes_Width', FSEClassesWidth);
      WriteInteger('Script Editor', 'Classes_HelpHeight', FSEClassesHelpHeight);
      WriteBool('Script Editor', 'Classes_HideBaseClasses', FSEClassesHideBaseClasses);
      WriteBool('Script Editor', 'Classes_HideAncestors', FSEClassesHideAncestors);
      WriteBool('Script Editor', 'Classes_HideHelp', FSEClassesHideHelp);
      WriteInteger('Script Editor', 'Modules_Width', FSEModulesWidth);
      WriteInteger('Script Editor', 'Messages_Height', FSEMsgHeight);

      WriteInteger('Actions Editor', 'Width', FAEWidth);
      WriteInteger('Actions Editor', 'Height', FAEHeight);
      WriteInteger('Actions Editor', 'LeftPanelWidth', FAELeftPanWidth);
      WriteInteger('Actions Editor', 'RightPanelWidth', FAERightPanWidth);

      WriteBool('Updates', 'CheckUpdates', FCheckUpdates);
      WriteString('Updates', 'UpdatesDBPath', FUpdatesDBPath);
      WriteString('Updates', 'DBPwd', Encrypt(FUpdatesDBPwd, StartKey, MultKey, AddKey));

      WriteBool('Designer', 'ShowGrid', FShowGrid);
      WriteInteger('Designer', 'GridSizeX', FGridSizeX);
      WriteInteger('Designer', 'GridSizeY', FGridSizeY);
      WriteInteger('Designer', 'GridColor', FGridColor);

      WriteInteger('SQL Editor', 'Width', FSQLEditorWidth);
      WriteInteger('SQL Editor', 'Height', FSQLEditorHeight);

      WriteInteger('SQL mode Editor', 'Width', FSQLModeEditorWidth);
      WriteInteger('SQL mode Editor', 'Height', FSQLModeEditorHeight);
      WriteInteger('SQL mode Editor', 'RightPanelWidth', FSQLModeEditorRightPanelWidth);

    finally
      Free;
    end;
  except
    on E: Exception do
      ErrMsgFmt(rsErrorWritingCfg, [LineEnding + LineEnding + E.Message]);
  end;
end;

procedure TAppSettings.Load;

  function StrToOutputBounds(S: String): TRect;
  begin
    TryStrToInt(CutStr(S, ';'), Result.Left);
    TryStrToInt(CutStr(S, ';'), Result.Top);
    TryStrToInt(CutStr(S, ';'), Result.Right);
    TryStrToInt(CutStr(S, ';'), Result.Bottom);
  end;

var
  SL: TStringList;
  i: Integer;
  S, DBNm: String;
  RD: TRecentData;
  Ini: TIniFileEx;
begin
  try
    Ini := TIniFileEx.Create(AppPath + 'dataexpress.cfg');
    SL := TStringList.Create;
    with Ini do
    try
      FLanguage := ReadString('UI', 'Language', 'ru');
      FFormLeft := ReadInteger('UI', 'FormLeft', -1);
      FFormTop := ReadInteger('UI', 'FormTop', -1);
      FFormWidth := ReadInteger('UI', 'FormWidth', 770);
      FFormHeight := ReadInteger('UI', 'FormHeight', 590);
      FFormState := TWindowState(ReadInteger('UI', 'FormState', 0));
      FConfirmExit := ReadBool('UI', 'ConfirmExit', False);
      FCaching := ReadBool('UI', 'CacheMetadata', False);
      FSupportDXDB := ReadBool('UI', 'SupportDXDB', True);
      FLogErrors := ReadBool('UI', 'LogErrors', False);
      FStartupAction := TStartupAction(ReadInteger('UI', 'StartupAction', 0));
      FWasError := ReadBool('UI', 'WasError', False);
      FExpertMode := ReadBool('UI', 'ExpertMode', False);
      FDebugFormLeft := ReadInteger('UI', 'DebugFormLeft', -1);
      FDebugFormTop := ReadInteger('UI', 'DebugFormTop', -1);
      FDebugFormWidth := ReadInteger('UI', 'DebugFormWidth', 750);
      FDebugFormHeight := ReadInteger('UI', 'DebugFormHeight', 480);
      FExprFormWidth := ReadInteger('UI', 'ExprFormWidth', 950);
      FExprFormHeight := ReadInteger('UI', 'ExprFormHeight', 273);
      FOutputBounds := StrToOutputBounds(ReadString('UI', 'OutputFormBounds', ''));
      FLeftPanelWidth := ReadInteger('UI', 'LeftPanelWidth', 220);
      FRightPanelWidth := ReadInteger('UI', 'RightPanelWidth', 250);

      FTemplateDir := ReadString('Folders', 'Templates', '');
      FOutputDir := ReadString('Folders', 'Output', '');
      FAppXmlFile := ReadString('Apps', 'XML', '');
      FAppDocXFile := ReadString('Apps', 'DOCX', '');
      FAppODtFile := ReadString('Apps', 'ODT', '');
      FAppODSFile := ReadString('Apps', 'ODS', '');
      FAppHtmlFile := ReadString('Apps', 'HTML', '');

      FDatabase := ReadString('Connection', 'Database', '');
      //FRemote := ReadBool('Connection', 'Remote', False);
      Pwd := ReadString('Connection', 'Password', 'masterkey');
		  FConnectName := ReadString('Connection', 'ConnectionName', '');

      ClearList(FRecents);
      ReadSectionValues('Recents', SL);
      for i := 0 to SL.Count - 1 do
      begin
        S := SL[i];
        RD := NewRecent;

        CutStr(S, '=');
        S := DeleteQuotesIfNeed(S);
        DBNm := CutStr(S, '|');
        if Copy(DBNm, 1, 1) = '$' then
        begin
          Delete(DBNm, 1, 1);
          RD.ConnectName:=DBNm;
        end
        else
        begin
          RD.DBName := DBNm;
          {RD.Remote := Str2Bool}(CutStr(S, '|'));
          RD.Pwd := Decrypt(CutStr(S, '|'), StartKey, MultKey, AddKey);
          RD.TemplateDir := CutStr(S, '|');
          RD.OutputDir := CutStr(S, '|');
        end;
        //NewRecent(DBNm, Pass, TemplDir, Rmt);
      end;

      FSEFormLeft := ReadInteger('Script Editor', 'Left', -1);
      FSEFormTop := ReadInteger('Script Editor', 'Top', -1);
      FSEFormWidth := ReadInteger('Script Editor', 'Width', 1000);
      FSEFormHeight := ReadInteger('Script Editor', 'Height', 600);
      FSEFormState := TWindowState(ReadInteger('Script Editor', 'State', 0));
      //FSearchUrl := ReadString('Script Editor', 'Classes_SearchUrl',
      //  'https://www.google.ru/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=VCL+');
      FSEClassesWidth := ReadInteger('Script Editor', 'Classes_Width', 217);
      FSEClassesHelpHeight := ReadInteger('Script Editor', 'Classes_HelpHeight', 200);
      FSEClassesHideBaseClasses := ReadBool('Script Editor', 'Classes_HideBaseClasses', False);
      FSEClassesHideAncestors := ReadBool('Script Editor', 'Classes_HideAncestors', False);
      FSEClassesHideHelp := ReadBool('Script Editor', 'Classes_HideHelp', False);
      FSEModulesWidth := ReadInteger('Script Editor', 'Modules_Width', 231);
      FSEMsgHeight := ReadInteger('Script Editor', 'Messages_Height', 81);

      FAEWidth := ReadInteger('Actions Editor', 'Width', 880);
      FAEHeight := ReadInteger('Actions Editor', 'Height', 580);
      FAELeftPanWidth := ReadInteger('Actions Editor', 'LeftPanelWidth', 260);
      FAERightPanWidth := ReadInteger('Actions Editor', 'RightPanelWidth', 260);

      FCheckUpdates := ReadBool('Updates', 'CheckUpdates', False);
      FUpdatesDBPath := ReadString('Updates', 'UpdatesDBPath', '');
      FUpdatesDBPwd := Decrypt(ReadString('Updates', 'DBPwd', ''), StartKey, MultKey, AddKey);

      FShowGrid := ReadBool('Designer', 'ShowGrid', True);
      GridSizeX := ReadInteger('Designer', 'GridSizeX', 8);
      GridSizeY := ReadInteger('Designer', 'GridSizeY', 8);
      FGridColor := ReadInteger('Designer', 'GridColor', 0);

      FSQLEditorWidth := ReadInteger('SQL Editor', 'Width', 950);
      FSQLEditorHeight := ReadInteger('SQL Editor', 'Height', 550);

      FSQLModeEditorWidth := ReadInteger('SQL mode Editor', 'Width', 950);
      FSQLModeEditorHeight := ReadInteger('SQL mode Editor', 'Height', 550);
      FSQLModeEditorRightPanelWidth := ReadInteger('SQL mode Editor', 'RightPanelWidth', 250);

      ReadConnections(Ini);

      FSESDName := '';
      FSEFmId := 0;
      FSEWebFmId := 0;
    finally
      Free;
      SL.Free;
    end;
  except
    on E: Exception do
      ErrMsgFmt(rsErrorReadingCfg, [LineEnding + LineEnding + E.Message]);
  end;
end;

procedure TAppSettings.SaveConnections;
var
  SL: TStringList;
  i: Integer;
  CI: TConnectInfo;
  S: String;
  Ini: TIniFileEx;
begin
  try
    Ini := TIniFileEx.Create(AppPath + 'dataexpress.cfg');
    SL := TStringList.Create;
    with Ini do
    try
      CacheUpdates:=True;
      ReadSection('Connections', SL);
      for i := 0 to SL.Count - 1 do
        DeleteKey('Connections', SL[i]);

      for i := 0 to FConnects.Count - 1 do
      begin
        CI := FConnects[i];
        S := CI.Name + '|' + CI.Group + '|' + CI.DBPath + '|' + Bool2Str(CI.IsRemote) + '|' +
          Encrypt(CI.DBPwd, StartKey, MultKey, AddKey) + '|' + CI.TemplateDir + '|' +
  				CI.OutputDir + '|' + CI.User + '|' + Encrypt(CI.Pwd, StartKey, MultKey, AddKey);
        WriteString('Connections', IntToStr(i + 1), S);
      end;
    finally
      Free;
      SL.Free;
    end;
  except
    on E: Exception do
      ErrMsgFmt(rsErrorWritingCfg, [LineEnding + LineEnding + E.Message]);
  end;
end;

procedure TAppSettings.LoadConnections;
var
  Ini: TIniFileEx;
begin
  try
    Ini := TIniFileEx.Create(AppPath + 'dataexpress.cfg');
    try
      ReadConnections(Ini);
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
      ErrMsgFmt(rsErrorReadingCfg, [LineEnding + LineEnding + E.Message]);
  end;
end;

procedure TAppSettings.LoadTemplateWizard;
var
  Ini: TIniFileEx;
begin
  try
    Ini := TIniFileEx.Create(AppPath + 'dataexpress.cfg');
    FTWLeft := Ini.ReadInteger('Template Wizard', 'Left', 200);
    FTWTop := Ini.ReadInteger('Template Wizard', 'Top', 200);
    FTWWidth := Ini.ReadInteger('Template Wizard', 'Width', 320);
    FTWHeight := Ini.ReadInteger('Template Wizard', 'Height', 520);
    FTWTopPanHeight := Ini.ReadInteger('Template Wizard', 'Top Panel Height', 150);
    FTWTopPan2Height := Ini.ReadInteger('Template Wizard', 'Top Panel 2 Height', 150);
  except
    on E: Exception do
      ErrMsgFmt(rsErrorReadingCfg, [LineEnding + LineEnding + E.Message]);
  end;
end;

procedure TAppSettings.SaveTemplateWizard;
var
  Ini: TIniFileEx;
begin
  try
    Ini := TIniFileEx.Create(AppPath + 'dataexpress.cfg');
    Ini.WriteInteger('Template Wizard', 'Left', FTWLeft);
    Ini.WriteInteger('Template Wizard', 'Top', FTWTop);
    Ini.WriteInteger('Template Wizard', 'Width', FTWWidth);
    Ini.WriteInteger('Template Wizard', 'Height', FTWHeight);
    Ini.WriteInteger('Template Wizard', 'Top Panel Height', FTWTopPanHeight);
    Ini.WriteInteger('Template Wizard', 'Top Panel 2 Height', FTWTopPan2Height);
  except
    on E: Exception do
      ErrMsgFmt(rsErrorWritingCfg, [LineEnding + LineEnding + E.Message]);
  end;
end;

function TAppSettings.NewRecent: TRecentData;
begin
  Result := TRecentData.Create;
  FRecents.Add(Result);
end;

function TAppSettings.AddRecent(aConnectName, aDBName: String): TRecentData;
var
  i: Integer;
  R: TRecentData;
begin
  if (aConnectName = '') and (aDBName = '') then Exit(nil);

  if aConnectName <> '' then
  begin
    aDBName := '';
  	i := FindRecent(aConnectName);
  end
  else
  begin
    // Сохраняем только имя файла, если файл в папке с программой.
    if ExtractFilePath(aDBName) = AppPath then
    	aDBName := ExtractFileName(aDBName);
	  i := FindRecent(aDBName);
  end;

  if i >= 0 then
  begin
    R := Recents[i];
    FRecents.Delete(i);
    FRecents.Insert(0, R);
  end
  else
  begin
    if FRecents.Count = 7 then
    begin
      Recents[6].Free;
      FRecents.Delete(6);
    end;
    R := TRecentData.Create;
    FRecents.Insert(0, R);

    R.ConnectName := aConnectName;
 		R.DBName := aDBName;
  end;
  {R.Pwd := aPwd;
  R.TemplateDir := aTemplateDir;
  R.Remote := aRemote;   }
  Result := R;
end;

function TAppSettings.FindRecent(const aDBName: String): Integer;
var
  i: Integer;
  R: TRecentData;
begin
  Result := -1;
  for i := 0 to FRecents.Count - 1 do
  begin
    R := Recents[i];
    if ((R.DBName <> '') and (MyUtf8CompareText(R.DBName, aDBName) = 0)) or
    	((R.ConnectName <> '') and (MyUtf8CompareText(R.ConnectName, aDBName) = 0)) then Exit(i);
  end;
end;

function TAppSettings.DeleteRecent(const aDBName: String): Boolean;
var
  i: Integer;
begin
  i := FindRecent(aDBName);
  Result := i >= 0;
  if Result then
  begin
    Recents[i].Free;
    FRecents.Delete(i);
  end;
end;

procedure TAppSettings.ClearRecents;
begin
  ClearList(FRecents);
end;

procedure TAppSettings.ClearConnectInfo;
begin
  ConnectName:='';
  Database := '';
  Pwd := '';
  TemplateDir := '';
  OutputDir := '';
end;

end.


