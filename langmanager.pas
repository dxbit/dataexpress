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
unit LangManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TLaguageInfo }

  TLanguageInfo = class
  public
    Name, Author, Version, Description, Suffix: String;
  end;

  { TLanguageInfoList }

  TLanguageInfoList = class(TList)
  private
    function GetLanguages(Index: Integer): TLanguageInfo;
  public
    function AddLanguage: TLanguageInfo;
    procedure Clear; override;
    function FindLang(const Suffix: String): TLanguageInfo;
    property Languages[Index: Integer]: TLanguageInfo read GetLanguages; default;
  end;

  { TLangManager }

  TLangManager = class
  private
    FActionsFile: String;
    FClassesFile: String;
    FCurLang: TLanguageInfo;
    FCurLangDir: String;
    FDataExpressFile: String;
    FFuncsFile: String;
    FHelpDir: String;
    FHelpIndexFile: String;
    FLanguages: TLanguageInfoList;
    function GetLangDir: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure SetCurLang(const Suffix: String);
    procedure LangsToList(L: TStrings);
    property LangDir: String read GetLangDir;
    property CurLang: TLanguageInfo read FCurLang;
    property CurLangDir: String read FCurLangDir;
    property HelpDir: String read FHelpDir;
    property HelpIndexFile: String read FHelpIndexFile;
    property FuncsFile: String read FFuncsFile;
    property ActionsFile: String read FActionsFile;
    property DataExpressFile: String read FDataExpressFile;
    property ClassesFile: String read FClassesFile;
  end;

var
  LangMan: TLangManager;

implementation

uses
  apputils, FileUtil, mytypes, appsettings;

{ TLangManager }

function TLangManager.GetLangDir: String;
begin
  Result := AppPath + 'languages' + DirectorySeparator;
end;

constructor TLangManager.Create;
begin
  FLanguages := TLanguageInfoList.Create;
end;

destructor TLangManager.Destroy;
begin
  FLanguages.Free;
  inherited Destroy;
end;

procedure TLangManager.Load;
var
  Langs: TStringList;
  i: Integer;
  LI: TLanguageInfo;
  LngFile, LngName, Author, Version, Descript: String;
begin
	Langs := FindAllDirectories(LangDir, False);
  for i := 0 to Langs.Count - 1 do
  begin
    LngFile := Langs[i] + DirectorySeparator + 'language.info';
    if not FileExists(LngFile) then Continue;
    with TIniFileEx.Create(LngFile) do
    try
      LngName := ReadString('Info', 'Name', '');
      Author := ReadString('Info', 'Author', '');
      Version := ReadString('Info', 'Version', '');
      Descript := ReadString('Info', 'Description', '');
    finally
      Free;
    end;

    LI := FLanguages.AddLanguage;
    LI.Suffix:=ExtractFileName(Langs[i]);
    LI.Name:=LngName;
    LI.Author := Author;
    LI.Version := Version;
    LI.Description := Descript;
  end;
  Langs.Free;
end;

procedure TLangManager.SetCurLang(const Suffix: String);
begin
  FCurLang := FLanguages.FindLang(AppConfig.Language);
  if FCurLang = nil then FCurLang := FLanguages.FindLang('ru');
  if FCurLang = nil then FCurLang := FLanguages.FindLang('en');
  if FCurLang = nil then
  begin
  	FCurLang := FLanguages.AddLanguage;
    FCurLang.Name := 'Default';
    //raise Exception.Create('Russian language not found.');
  end;
  FCurLangDir := GetLangDir + CurLang.Suffix + DirectorySeparator;

  FActionsFile := CurLangDir + 'actions.' + CurLang.Suffix + '.dat';
  if not FileExists(FActionsFile) then
  	FActionsFile := GetLangDir + 'ru' + DirectorySeparator + 'actions.ru.dat';

  FClassesFile := GetLangDir + 'en' + DirectorySeparator + 'classes.dat';
  FDataExpressFile := CurLangDir + 'dataexpress.' + CurLang.Suffix + '.po';

  FFuncsFile := CurLangDir + 'funcs.' + CurLang.Suffix + '.dat';
  if not FileExists(FFuncsFile) then
  	FFuncsFile := GetLangDir + 'ru' + DirectorySeparator + 'funcs.ru.dat';

  FHelpDir := CurLangDir + 'help' + DirectorySeparator;
  if not DirectoryExists(FHelpDir) then
    FHelpDir := GetLangDir + 'ru' + DirectorySeparator + 'help' + DirectorySeparator;

  FHelpIndexFile := HelpDir + 'index';
end;

procedure TLangManager.LangsToList(L: TStrings);
var
  SL: TStringListUtf8;
  i: Integer;
begin
  SL := TStringListUtf8.Create;
  for i := 0 to FLanguages.Count - 1 do
    SL.AddObject(FLanguages[i].Name, FLanguages[i]);
  SL.Sort;
  L.Assign(SL);
  SL.Free;
end;

{ TLanguageInfoList }

function TLanguageInfoList.GetLanguages(Index: Integer): TLanguageInfo;
begin
  Result := TLanguageInfo(Items[Index]);
end;

function TLanguageInfoList.AddLanguage: TLanguageInfo;
begin
  Result := TLanguageInfo.Create;
  Add(Result);
end;

procedure TLanguageInfoList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Languages[i].Free;
  inherited Clear;
end;

function TLanguageInfoList.FindLang(const Suffix: String): TLanguageInfo;
var
  i: Integer;
  L: TLanguageInfo;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    L := Languages[i];
  	if CompareText(L.Suffix, Suffix) = 0 then Exit(L);
  end;
end;

end.

