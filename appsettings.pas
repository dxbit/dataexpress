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
unit AppSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  { TRecentData }

  TRecentData = class
  public
    DBName: String;
    Remote: Boolean;
    Pwd: String;
  end;

  { TAppSettings }

  TAppSettings = class
  private
    FAppDocXFile: String;
    FAppHtmlFile: String;
    FAppOdsFile: String;
    FAppOdtFile: String;
    FAppXmlFile: String;
    FConfirmExit: Boolean;
    FDatabase: String;
    FDebugFormHeight: Integer;
    FDebugFormWidth: Integer;
    FExpertMode: Boolean;
    FExprFormHeight: Integer;
    FExprFormWidth: Integer;
    FFormHeight: Integer;
    FFormState: TWindowState;
    FFormWidth: Integer;
    FHideParents: Boolean;
    FLanguage: String;
    FOutputDir: String;
    FRemote: Boolean;
    FSearchUrl: String;
    FSEClassesWidth: Integer;
    FSEFormHeight: Integer;
    FSEFormWidth: Integer;
    FSEModulesWidth: Integer;
    FSEMsgHeight: Integer;
    FTemplateDir: String;
    FPwd: String;
    FWasError: Boolean;
    FRecents: TList;
    function GetPwd: String;
    function GetRecentCount: Integer;
    function GetRecents(Index: Integer): TRecentData;
    procedure SetPwd(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Save;
    procedure Load;
    function NewRecent(aDBName, aPwd: String; aRemote: Boolean): TRecentData;
    function AddRecent(aDBName, aPwd: String; aRemote: Boolean): TRecentData;
    function FindRecent(const aDBName: String): Integer;
    procedure ClearRecents;
    property Database: String read FDatabase write FDatabase;
    property Remote: Boolean read FRemote write FRemote;
    property TemplateDir: String read FTemplateDir write FTemplateDir;
    property OutputDir: String read FOutputDir write FOutputDir;
    property AppXmlFile: String read FAppXmlFile write FAppXmlFile;
    property AppDocXFile: String read FAppDocXFile write FAppDocXFile;
    property AppOdtFile: String read FAppOdtFile write FAppOdtFile;
    property AppOdsFile: String read FAppOdsFile write FAppOdsFile;
    property AppHtmlFile: String read FAppHtmlFile write FAppHtmlFile;
    property Language: String read FLanguage write FLanguage;
    property FormWidth: Integer read FFormWidth write FFormWidth;
    property FormHeight: Integer read FFormHeight write FFormHeight;
    property FormState: TWindowState read FFormState write FFormState;
    property ConfirmExit: Boolean read FConfirmExit write FConfirmExit;
    property Pwd: String read GetPwd write SetPwd;
    property WasError: Boolean read FWasError write FWasError;
    property Recents[Index: Integer]: TRecentData read GetRecents;
    property RecentCount: Integer read GetRecentCount;
    property HideParents: Boolean read FHideParents write FHideParents;
    property SearchUrl: String read FSearchUrl write FSearchUrl;
    property SEFormWidth: Integer read FSEFormWidth write FSEFormWidth;
    property SEFormHeight: Integer read FSEFormHeight write FSEFormHeight;
    property SEModulesWidth: Integer read FSEModulesWidth write FSEModulesWidth;
    property SEClassesWidth: Integer read FSEClassesWidth write FSEClassesWidth;
    property SEMsgHeight: Integer read FSEMsgHeight write FSEMsgHeight;
    property ExpertMode: Boolean read FExpertMode write FExpertMode;
    property DebugFormWidth: Integer read FDebugFormWidth write FDebugFormWidth;
    property DebugFormHeight: Integer read FDebugFormHeight write FDebugFormHeight;
    property ExprFormWidth: Integer read FExprFormWidth write FExprFormWidth;
    property ExprFormHeight: Integer read FExprFormHeight write FExprFormHeight;
  end;

var
  AppConfig: TAppSettings;

implementation

uses
  IniFiles, LazUtf8, apputils, crypt, mytypes;

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

procedure TAppSettings.SetPwd(AValue: String);
begin
  FPwd := Encrypt(AValue, StartKey, MultKey, AddKey);
end;

constructor TAppSettings.Create;
begin
  FRecents := TList.Create;
end;

destructor TAppSettings.Destroy;
begin
  ClearList(FRecents);
  FRecents.Free;
  inherited Destroy;
end;

procedure TAppSettings.Save;
var
  i: Integer;
  S: String;
  R: TRecentData;
begin
  with TIniFileEx.Create(AppPath + 'dataexpress.cfg') do
    try
      CacheUpdates:=True;
      WriteString('UI', 'Language', FLanguage);
      WriteInteger('UI', 'FormWidth', FFormWidth);
      WriteInteger('UI', 'FormHeight', FFormHeight);
      WriteInteger('UI', 'FormState', Ord(FFormState));
      WriteBool('UI', 'ConfirmExit', FConfirmExit);
      WriteString('Connection', 'Database', FDatabase);
      WriteBool('Connection', 'Remote', FRemote);
      WriteString('Connection', 'Password', FPwd);
      WriteString('Folders', 'Templates', FTemplateDir);
      WriteString('Folders', 'Output', FOutputDir);
      WriteString('Apps', 'XML', FAppXmlFile);
      WriteString('Apps', 'DOCX', FAppDocXFile);
      WriteString('Apps', 'ODT', FAppOdtFile);
      WriteString('Apps', 'ODS', FAppOdsFile);
      WriteString('Apps', 'HTML', FAppHtmlFile);
      WriteBool('UI', 'WasError', FWasError);
      WriteBool('UI', 'ExpertMode', FExpertMode);
      WriteInteger('UI', 'DebugFormWidth', FDebugFormWidth);
      WriteInteger('UI', 'DebugFormHeight', FDebugFormHeight);
      WriteInteger('UI', 'ExprFormWidth', FExprFormWidth);
      WriteInteger('UI', 'ExprFormHeight', FExprFormHeight);

      for i := 0 to FRecents.Count - 1 do
      begin
        S := '';
        R := Recents[i];
        S := S + R.DBName + '|' +  Bool2Str(R.Remote) + '|' +
          Encrypt(R.Pwd, StartKey, MultKey, AddKey);
        WriteString('Recents', IntToStr(i + 1), S);
      end;
      if FRecents.Count = 0 then
        for i := 1 to 7 do
          DeleteKey('Recents', IntToStr(i));

      WriteInteger('Script Editor', 'Width', FSEFormWidth);
      WriteInteger('Script Editor', 'Height', FSEFormHeight);
      WriteBool('Script Editor', 'Classes_HideParents', FHideParents);
      WriteString('Script Editor', 'Classes_SearchUrl', FSearchUrl);
      WriteInteger('Script Editor', 'Classes_Width', FSEClassesWidth);
      WriteInteger('Script Editor', 'Modules_Width', FSEModulesWidth);
      WriteInteger('Script Editor', 'Messages_Height', FSEMsgHeight);
    finally
      Free;
    end;
end;

function CutStr(var S: String; D: Char): String;
var
  p: SizeInt;
begin
  p := Pos(D, S);
  if p = 0 then
  begin
    Result := S;
    S := '';
  end
  else
  begin
    Result := Copy(S, 1, p - 1);
    Delete(S, 1, p);
  end;
end;

procedure TAppSettings.Load;
var
  SL: TStringList;
  i: Integer;
  S, DBNm, Pass: String;
  Rmt: Boolean;
begin
  with TIniFileEx.Create(AppPath + 'dataexpress.cfg') do
    try
      FLanguage := ReadString('UI', 'Language', '');
      FFormWidth := ReadInteger('UI', 'FormWidth', 770);
      FFormHeight := ReadInteger('UI', 'FormHeight', 590);
      FFormState := TWindowState(ReadInteger('UI', 'FormState', 0));
      FConfirmExit := ReadBool('UI', 'ConfirmExit', False);
      FDatabase := ReadString('Connection', 'Database', '');
      FRemote := ReadBool('Connection', 'Remote', False);
      FPwd := readString('Connection', 'Password', Encrypt('masterkey', StartKey, MultKey, AddKey));
      FTemplateDir := ReadString('Folders', 'Templates', '');
      FOutputDir := ReadString('Folders', 'Output', '');
      FAppXmlFile := ReadString('Apps', 'XML', '');
      FAppDocXFile := ReadString('Apps', 'DOCX', '');
      FAppODtFile := ReadString('Apps', 'ODT', '');
      FAppODSFile := ReadString('Apps', 'ODS', '');
      FAppHtmlFile := ReadString('Apps', 'HTML', '');
      FWasError := ReadBool('UI', 'WasError', False);
      FExpertMode := ReadBool('UI', 'ExpertMode', False);
      FDebugFormWidth := ReadInteger('UI', 'DebugFormWidth', 750);
      FDebugFormHeight := ReadInteger('UI', 'DebugFormHeight', 480);
      FExprFormWidth := ReadInteger('UI', 'ExprFormWidth', 700);
      FExprFormHeight := ReadInteger('UI', 'ExprFormHeight', 273);

      ClearList(FRecents);
      SL := TStringList.Create;
      ReadSectionValues('Recents', SL);
      for i := 0 to SL.Count - 1 do
      begin
        S := SL[i];
        CutStr(S, '=');
        DBNm := CutStr(S, '|');
        Rmt := Str2Bool(CutStr(S, '|'));
        Pass := Decrypt(CutStr(S, '|'), StartKey, MultKey, AddKey);

        NewRecent(DBNm, Pass, Rmt);
      end;
      SL.Free;

      FSEFormWidth := ReadInteger('Script Editor', 'Width', 1000);
      FSEFormHeight := ReadInteger('Script Editor', 'Height', 600);
      FHideParents := ReadBool('Script Editor', 'Classes_HideParents', True);
      FSearchUrl := ReadString('Script Editor', 'Classes_SearchUrl',
        'https://www.google.ru/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=VCL+');
      FSEClassesWidth := ReadInteger('Script Editor', 'Classes_Width', 217);
      FSEModulesWidth := ReadInteger('Script Editor', 'Modules_Width', 231);
      FSEMsgHeight := ReadInteger('Script Editor', 'Messages_Height', 81);
    finally
      Free;
    end;
end;

function TAppSettings.NewRecent(aDBName, aPwd: String; aRemote: Boolean
  ): TRecentData;
begin
  Result := TRecentData.Create;
  FRecents.Add(Result);
  Result.DBName := aDBName;
  Result.Pwd := aPwd;
  Result.Remote := aRemote;
end;

function TAppSettings.AddRecent(aDBName, aPwd: String; aRemote: Boolean
  ): TRecentData;
var
  i: Integer;
  R: TRecentData;
begin
  if aDBName = '' then Exit(nil);

  // Сохраняем только имя файла, если файл в папке с программой.
  if ExtractFilePath(aDBName) = AppPath then
  	aDBName := ExtractFileName(aDBName);

  i := FindRecent(aDBName);
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
  end;
  R.DBName := aDBName;
  R.Pwd := aPwd;
  R.Remote := aRemote;
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
    if Utf8CompareText(R.DBName, aDBName) = 0 then Exit(i);
  end;
end;

procedure TAppSettings.ClearRecents;
begin
  ClearList(FRecents);
end;

end.

