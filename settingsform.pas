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
unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ButtonPanel, Menus, Buttons, strconsts;

type

  { TSettingsFm }

  TSettingsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ConfirmExit: TCheckBox;
    ComboBox1: TComboBox;
    DirectoryEdit1: TDirectoryEdit;
    DirectoryEdit2: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    InfoLangBn: TSpeedButton;
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

implementation

uses
  appsettings, helpform, zipper, apputils, mytypes, langmanager;

{$R *.lfm}

{ TSettingsFm }

procedure TSettingsFm.FormCreate(Sender: TObject);
var
  LI: TListItem;
begin
  Caption := rsSettings;
  Label1.Caption := rsLanguage;
  Label2.Caption := rsTemplatesFolder;
  Label3.Caption := rsOpenTemplate;
  Label4.Caption := rsOutputFolder;
  MenuItem1.Caption := rsSelect;
  MenuItem2.Caption := rsClear;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  {ComboBox1.Items.Add('English');
  ComboBox1.Items.Add('Russian');}
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
  ConfirmExit.Caption := rsConfirmExit;
end;

procedure TSettingsFm.InfoLangBnClick(Sender: TObject);
var
  S: String;
  LI: TLanguageInfo;
begin
  S := StringReplace(rsAboutLangMsg, '/', LineEnding,	[rfReplaceAll]);
  with ComboBox1 do
	  LI := TLanguageInfo(Items.Objects[ItemIndex]);
  S := Format(S, [LI.Author, LI.Version, LI.Description]);
  MessageDlg(rsAboutLang, S, mtInformation, [mbOk], 0);
end;

procedure TSettingsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('settings');
end;

procedure TSettingsFm.MenuItem1Click(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Title:=rsSelectApplication;
    {$ifdef windows}
    Filter := rsApplicationsFilter;
    {$else}
    Filter := rsApplicationsFilterUnix;
    {$endif}
    Options := Options + [ofFileMustExist];
    if Execute then
      ListView1.Selected.SubItems[1] := FileName;
  finally
    Free;
  end;
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
//const
  //LangIds: array [0..1] of String = ('', 'ru');
var
  //LangId: Integer;
  i: Integer;
begin
  {LangId := 0;
  for i := Low(LangIds) to High(LangIds) do
    if AppConfig.Language = LangIds[i] then
    begin
      LangId := i; Break;
    end;
  ComboBox1.ItemIndex := LangId; }
  with ComboBox1 do
  begin
	  LangMan.LangsToList(Items);
    ItemIndex := Items.IndexOfObject(LangMan.CurLang);
	end;

  DirectoryEdit1.Text:=AppConfig.TemplateDir;
  DirectoryEdit2.Text := AppConfig.OutputDir;
  ListView1.Items[0].SubItems[1] := AppConfig.AppXmlFile;
  ListView1.Items[1].SubItems[1] := AppConfig.AppDocXFile;
  ListView1.Items[2].SubItems[1] := AppConfig.AppOdtFile;
  ListView1.Items[3].SubItems[1] := AppConfig.AppOdsFile;
  ListView1.Items[4].SubItems[1] := AppConfig.AppHtmlFile;
  ConfirmExit.Checked := AppConfig.ConfirmExit;

  Result := ShowModal;
  if Result <> mrOk then Exit;

  with ComboBox1 do
	  AppConfig.Language:=TLanguageInfo(Items.Objects[ItemIndex]).Suffix;  //LangIds[ComboBox1.ItemIndex];
  AppConfig.TemplateDir:=IncludeTrailingPathDelimiter(DirectoryEdit1.Text);
  if AppConfig.TemplateDir = DirectorySeparator then
    AppConfig.TemplateDir := '';
  AppConfig.OutputDir:=IncludeTrailingPathDelimiter(DirectoryEdit2.Text);
  if AppConfig.OutputDir = DirectorySeparator then
    AppConfig.OutputDir := '';
  AppConfig.AppXmlFile := ListView1.Items[0].SubItems[1];
  AppConfig.AppDocXFile := ListView1.Items[1].SubItems[1];
  AppConfig.AppOdtFile := ListView1.Items[2].SubItems[1];
  AppConfig.AppOdsFile := ListView1.Items[3].SubItems[1];
  AppConfig.AppHtmlFile := ListView1.Items[4].SubItems[1];
  AppConfig.ConfirmExit:=ConfirmExit.Checked;
  //AppConfig.Save;
end;

end.

