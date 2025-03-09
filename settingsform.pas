{-------------------------------------------------------------------------------

    Copyright 2015-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ButtonPanel, Menus, Buttons, Spin, ColorBox, ExtCtrls,
  strconsts;

type

  { TSettingsFm }

  TSettingsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Caching: TCheckBox;
    LogErrorsChk: TCheckBox;
    SupportDXDB: TCheckBox;
    ShowGridChk: TCheckBox;
    CheckUpdates: TCheckBox;
    GridColorBox: TColorBox;
    ComboBox1: TComboBox;
    ConfirmExit: TCheckBox;
    DesignTab: TTabSheet;
    Label10: TLabel;
    Label11: TLabel;
    Label9: TLabel;
    GridXSpin: TSpinEdit;
    GridYSpin: TSpinEdit;
    UpdatesDBPath: TEdit;
    UpdatesDBPwd: TEdit;
    InfoLangBn: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OutputDir: TDirectoryEdit;
    Pages: TPageControl;
    PopupMenu1: TPopupMenu;
    StartupAction: TComboBox;
    IntfTab: TTabSheet;
    TplTab: TTabSheet;
    UpdTab: TTabSheet;
    OtherTab: TTabSheet;
    TemplateDir: TDirectoryEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure InfoLangBnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm: Integer;
  end;

var
  SettingsFm: TSettingsFm;

function ShowSettingsForm: Integer;

implementation

uses
  appsettings, helpmanager, langmanager, apputils;

function ShowSettingsForm: Integer;
begin
  if SettingsFm = nil then
    SettingsFm := TSettingsFm.Create(Application);
  Result := SettingsFm.ShowForm;
end;

{$R *.lfm}

{ TSettingsFm }

procedure TSettingsFm.FormCreate(Sender: TObject);
var
  LI: TListItem;
begin
  Caption := rsSettings;

  IntfTab.Caption := rsInterface;
  TplTab.Caption := rsTemplates;
  UpdTab.Caption := rsUpdates;
  DesignTab.Caption := rsDesigner;
  OtherTab.Caption := rsOther;
  // Интерфейс
  Label1.Caption := rsLanguage;
  InfoLangBn.Hint := rsAboutLang;
  Label5.Caption := rsStartupAction;
  StartupAction.Items.AddStrings([rsOpenLastDB, rsOpenConnectList]);
  ConfirmExit.Caption := rsConfirmExit;
  // Шаблоны
  Label2.Caption := rsTemplatesFolder;
  TemplateDir.TextHint := rsHintTemplateDefaultFolder;
  Label3.Caption := rsOpenTemplate;
  Label4.Caption := rsOutputFolder;
  OutputDir.TextHint := rsHintOutputDefaultFolder;
  ListView1.Column[0].Caption:=rsAppType;
  ListView1.Column[1].Caption:=rsAppDescription;
  ListView1.Column[2].Caption:=rsAppPath;
  LI := ListView1.Items.Add;
  LI.Caption := 'XML';
  LI.SubItems.Add(rsWordXML);
  LI.SubItems.Add('');
  LI := ListView1.Items.Add;
  LI.Caption := 'DOCX';
  LI.SubItems.Add(rsWordDocument);
  LI.SubItems.Add('');
  LI := ListView1.Items.Add;
  LI.Caption := 'ODT';
  LI.SubItems.Add(rsOODocument);
  LI.SubItems.Add('');
  LI := ListView1.Items.Add;
  LI.Caption := 'ODS';
  LI.SubItems.Add(rsOOSpreadSheet);
  LI.SubItems.Add('');
  LI := ListView1.Items.Add;
  LI.Caption := 'HTML';
  LI.SubItems.Add(rsWebPage);
  LI.SubItems.Add('');
  MenuItem1.Caption := rsSelect;
  MenuItem2.Caption := rsClear;
  // Обновление
  CheckUpdates.Caption := rsCheckUpdates;
  Label7.Caption := rsUpdatesDBPath;
  UpdatesDBPath.TextHint := rsExampleDBLocation;
  Label8.Caption := rsPwdSYSDBA;
  UpdatesDBPwd.TextHint := rsDefaultPwd;
  //Дизайнер
  ShowGridChk.Caption := rsShowGrid;
  Label9.Caption := rsGridSizeX;
  Label10.Caption := rsGridSizeY;
  Label11.Caption := rsGridColor;
  // Прочее
  Caching.Caption := rsCacheMetadata;
  SupportDXDB.Caption := rsSupportDXDB;
  LogErrorsChk.Caption := rsErrorLogging;

  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;

  SetupSpeedButton(InfoLangBn, 'info16');

  //AddFormHeight(Self);
end;

procedure TSettingsFm.InfoLangBnClick(Sender: TObject);
var
  S: String;
  LI: TLanguageInfo;
begin
  S := StringReplace(rsAboutLangMsg, '/', LineEnding,	[rfReplaceAll]);
  with ComboBox1 do
    if ItemIndex < 0 then Exit
    else LI := TLanguageInfo(Items.Objects[ItemIndex]);
  S := Format(S, [LI.Author, LI.Version, LI.Description]);
  MessageDlg(rsAboutLang, S, mtInformation, [mbOk], 0);
end;

procedure TSettingsFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then Exit;
  if ComboBox1.ItemIndex < 0 then
  begin
    ErrMsg(rsSelectLang);
    ComboBox1.SetFocus;
    CanClose := False;
  end;
end;

procedure TSettingsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('settings');
end;

procedure TSettingsFm.MenuItem1Click(Sender: TObject);
var
  FlNm: String;
begin
  FlNm := ListView1.Selected.SubItems[1];
  with TOpenDialog.Create(nil) do
  try
    Title := rsSelectApplication;
    InitialDir := ExtractFilePath(FlNm);
    {$ifdef windows}
    if not AppConfig.IsWine then
      Filter := rsApplicationsFilter
    else
      Filter := rsApplicationsFilterWine;
    {$else}
    Filter := rsApplicationsFilterUnix;
    {$endif}
    Options := Options + [ofFileMustExist];
    if Execute then
      ListView1.Selected.SubItems[1] := FileName;
  finally
    Free;
  end
end;

procedure TSettingsFm.MenuItem2Click(Sender: TObject);
begin
  ListView1.Selected.SubItems[1] := '';
end;

procedure TSettingsFm.PopupMenu1Popup(Sender: TObject);
begin
  MenuItem1.Enabled := ListView1.Selected <> nil;
  MenuItem2.Enabled := ListView1.Selected <> nil;
end;

function TSettingsFm.ShowForm: Integer;
var
  Theme, S: String;
  OldItemIndex: Integer;
  LI: TLanguageInfo;
begin
  with ComboBox1 do
  begin
	  LangMan.LangsToList(Items);
    LI := LangMan.Languages.FindLang(AppConfig.Language);
    ItemIndex := Items.IndexOfObject(LI);
    OldItemIndex := ItemIndex;
	end;

  StartupAction.ItemIndex := Ord(AppConfig.StartupAction);
  ConfirmExit.Checked := AppConfig.ConfirmExit;

  TemplateDir.Text:=AppConfig.TemplateDir;
  OutputDir.Text := AppConfig.OutputDir;
  ListView1.Items[0].SubItems[1] := AppConfig.AppXmlFile;
  ListView1.Items[1].SubItems[1] := AppConfig.AppDocXFile;
  ListView1.Items[2].SubItems[1] := AppConfig.AppOdtFile;
  ListView1.Items[3].SubItems[1] := AppConfig.AppOdsFile;
  ListView1.Items[4].SubItems[1] := AppConfig.AppHtmlFile;

  CheckUpdates.Checked := AppConfig.CheckUpdates;
  UpdatesDBPath.Text := AppConfig.UpdatesDBPath;
  UpdatesDBPwd.Text := AppConfig.UpdatesDBPwd;

  ShowGridChk.Checked := AppConfig.ShowGrid;
  GridXSpin.Value := AppConfig.GridSizeX;
  GridYSpin.Value := AppConfig.GridSizeY;
  GridColorBox.Selected := AppConfig.GridColor;

  Caching.Checked := AppConfig.Caching;
  SupportDXDB.Checked := AppConfig.SupportDXDB;
  LogErrorsChk.Checked := AppConfig.LogErrors;

  DesignTab.TabVisible := IsDeveloper and not IsEmptyApp;
  if Pages.ActivePage = DesignTab then Pages.ActivePage := IntfTab;

  Result := ShowModal;
  if Result <> mrOk then Exit;

  with ComboBox1 do
    if ItemIndex >= 0 then
  	  AppConfig.Language:=TLanguageInfo(Items.Objects[ItemIndex]).Suffix;
  AppConfig.StartupAction := TStartupAction(StartupAction.ItemIndex);

  AppConfig.ConfirmExit := ConfirmExit.Checked;
  AppConfig.TemplateDir:=TemplateDir.Text;
  AppConfig.OutputDir:=OutputDir.Text;
  if AppConfig.OutputDir = DirectorySeparator then
    AppConfig.OutputDir := '';
  AppConfig.AppXmlFile := ListView1.Items[0].SubItems[1];
  AppConfig.AppDocXFile := ListView1.Items[1].SubItems[1];
  AppConfig.AppOdtFile := ListView1.Items[2].SubItems[1];
  AppConfig.AppOdsFile := ListView1.Items[3].SubItems[1];
  AppConfig.AppHtmlFile := ListView1.Items[4].SubItems[1];

  AppConfig.CheckUpdates := CheckUpdates.Checked;
  AppConfig.UpdatesDBPath := UpdatesDBPath.Text;
  AppConfig.UpdatesDBPwd := UpdatesDBPwd.Text;

  AppConfig.ShowGrid := ShowGridChk.Checked;
  AppConfig.GridSizeX := GridXSpin.Value;
  AppConfig.GridSizeY := GridYSpin.Value;
  AppConfig.GridColor := GridColorBox.Selected;

  AppConfig.Caching := Caching.Checked;
  AppConfig.SupportDXDB := SupportDXDB.Checked;
  Appconfig.LogErrors := LogErrorsChk.Checked;

  if ComboBox1.ItemIndex <> OldItemIndex then
    Info(rsNewLangApply);
end;

end.

