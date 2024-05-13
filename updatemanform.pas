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

unit UpdateManForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ButtonPanel, ComCtrls, ExtCtrls, EditBtn, Menus, CheckTreeView, TreeViewEx,
  strconsts, updatemanager;

type
  { TUpdateManFm }

  TUpdateManFm = class(TForm)
    CheckAllMnu: TMenuItem;
    CheckDbMnu: TMenuItem;
    DBSta: TStaticText;
    MnuImages: TImageList;
    FilesSta: TStaticText;
    UnCheckAllMnu: TMenuItem;
    MoreBn: TBitBtn;
    DBTree: TTreeViewEx;
    FilesTree: TCheckTreeView;
    FilesGrp: TGroupBox;
    AppVer: TLabel;
    DBGrp: TGroupBox;
    DBVer: TLabel;
    MsgPan: TPanel;
    FilesMnu: TPopupMenu;
    UploadBn: TBitBtn;
    ButtonPanel1: TButtonPanel;
    FilesImages: TImageList;
    procedure CheckAllMnuClick(Sender: TObject);
    procedure CheckDbMnuClick(Sender: TObject);
    procedure DBTreeSelectionChanged(Sender: TObject);
    procedure FilesTreeCheckChange(Sender: TObject; ANode: TTreeNode;
      AValue: Boolean);
    procedure FilesTreeCompare(Sender: TObject; Node1, Node2: TTreeNode;
      var Compare: Integer);
    procedure FilesTreeSelectionChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MoreBnClick(Sender: TObject);
    procedure UnCheckAllMnuClick(Sender: TObject);
    procedure UploadBnClick(Sender: TObject);
    procedure UpdateDbVer;
  private
    FUpdateMan: TUpdateMan;
    procedure FillFilesTree;
    procedure FillDBTree;
    procedure ShowMsg(const Msg: String);
    procedure HideMsg;
    procedure UpdateControlState;
  public
    procedure ShowForm;
  end;

var
  UpdateManFm: TUpdateManFm;

procedure ShowUpdateManForm;

implementation

uses
  IBConnection, LazUtf8, LazFileUtils, FileUtil, apputils, appsettings,
  updateoptionsform, helpmanager, dbengine;

procedure ShowUpdateManForm;
begin
  if UpdateManFm = nil then
    UpdateManFm := TUpdateManFm.Create(Application);
  UpdateManFm.ShowForm;
end;

function FormatFileSize(FlSz: Extended): String;
var
  SzU: String;
begin
  if FlSz < 1024 then SzU := ' B'
  else if FlSz < 1024 * 1024 then
  begin
    FlSz := FlSz / 1024;
    SzU := ' KB'
  end
  else
  begin
    FlSz := FlSz / 1024 / 1024;
    SzU := ' MB';
  end;
  Result := FormatFloat('0.##', FlSz) + SzU;
end;

{$R *.lfm}

{ TUpdateManFm }

procedure TUpdateManFm.FilesTreeCompare(Sender: TObject; Node1,
  Node2: TTreeNode; var Compare: Integer);
begin
  if (Node1.ImageIndex = 0) or (Node2.ImageIndex = 0) then
  begin
    Compare := Node1.ImageIndex - Node2.ImageIndex;
    if Compare <> 0 then Exit;
  end;
  Compare := MyUtf8CompareText(Node1.Text, Node2.Text);
end;

procedure TUpdateManFm.FilesTreeSelectionChanged(Sender: TObject);
var
  N: TTreeNode;
  S, FlNm: String;
  FlDT: TDateTime;
begin
  N := FilesTree.Selected;
  S := '';
  if (N <> nil) and (N.Count = 0) then
  begin
    FlNm := AppPath + StringReplace(N.GetTextPath, '/', PathDelim, [rfReplaceAll]);
    FlDT := FileDateToDateTime(FileAge(FlNm));
    S := Format(rsUpdateDBFileInfo, [FormatFileSize(FileSize(FlNm)), DateTimeToStr(FlDT)]);
  end;

  FilesSta.Caption := S;
end;

procedure TUpdateManFm.CheckAllMnuClick(Sender: TObject);
begin
  FilesTree.CheckAll;
  UpdateControlState;
end;

procedure TUpdateManFm.CheckDbMnuClick(Sender: TObject);
var
  i: Integer;
  F: TUpdateFileInfo;
  S: String;
  N: TTreeNode;
begin
  FilesTree.UnCheckAll;
  for i := 0 to FUpdateMan.Files.Count - 1 do
  begin
    F := FUpdateMan.Files[i];
    S := StringReplace(F.FileName, '\', '/', [rfReplaceAll]);
    N := FilesTree.Items.FindNodeWithTextPath(S);
    if N <> nil then FilesTree.SetChecked(N, True);
  end;
  FilesTree.UpdateNodesState;
  UpdateControlState;
end;

procedure TUpdateManFm.DBTreeSelectionChanged(Sender: TObject);
var
  N: TTreeNode;
  S: String;
  Inf: TUpdateFileInfo;
begin
  N := DBTree.Selected;
  S := '';
  if (N <> nil) and (N.Data <> nil) then
  begin
    Inf := PUpdateFileInfo(N.Data)^;
    S := Format(rsUpdateDBFileInfo, [FormatFileSize(Inf.Size), DateTimeToStr(FileDateToDateTime(Inf.Time))]);
  end;
  DBSta.Caption := S;
end;

procedure TUpdateManFm.FilesTreeCheckChange(Sender: TObject; ANode: TTreeNode;
  AValue: Boolean);
begin
  UpdateControlState;
end;

function GetImageIndexByFileExt(const FlNm: String): Integer;
var
  Ext: String;
begin
  Ext := LowerCase(ExtractFileExt(FlNm));
  if FlNm = 'dataexpress.exe' then Result := 1
  else if Ext = '.exe' then Result := 2
  else if Ext = '.dll' then Result := 3
  else if (Ext = '.txt') or (Ext = '.dat') or (Ext = '.po') or (Ext = '.info') or
    (Ext = '.msg') or (Ext = '.dic') or (Ext = '.cfg') then Result := 4
  else if (Ext = '.docx') or (Ext = '.docm') or (Ext = '.odt') then
    Result := 5
  else if (Ext = '.xlsx') or (Ext = '.xlsm') or (Ext = '.ods') then
    Result := 6
  else if (Ext = '.fdb') then Result := 7
  else Result := 8;
end;

procedure TUpdateManFm.FormCreate(Sender: TObject);
begin
  FilesGrp.Caption := rsAppFolder;
  DBGrp.Caption := rsUpdateDBContent;
  UploadBn.Hint := rsUploadSelectedFilesIntoDB;
  UploadBn.LoadGlyphFromLazarusResource('right24');
  MoreBn.Hint := rsUpdateSettings;
  MoreBn.LoadGlyphFromLazarusResource('more24');
  CheckAllMnu.Caption := rsCheckAll;
  UnCheckAllMnu.Caption := rsUnCheckAll;
  CheckDbMnu.Caption := rsCheckBoxDBFiles;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  ButtonPanel1.CloseButton.Caption := rsClose;
  with FilesImages do
  begin
    AddLazarusResource('folder16');
    AddLazarusResource('dx16');
    AddLazarusResource('app16');
    AddLazarusResource('dll16');
    AddLazarusResource('textfile16');
    AddLazarusResource('word16');
    AddLazarusResource('excel16');
    AddLazarusResource('db16');
    AddLazarusResource('anyfile16');
  end;
  with MnuImages do
  begin
    AddLazarusResource('checkall16');
    AddLazarusResource('uncheckall16');
    AddLazarusResource('magic16');
  end;
end;

procedure TUpdateManFm.FormShow(Sender: TObject);
begin
  FilesTree.SetFocus;
end;

procedure TUpdateManFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('updatedb');
end;

procedure TUpdateManFm.MoreBnClick(Sender: TObject);
begin
  if ShowUpdateOptionsForm(FUpdateMan) = mrOk then UpdateDbVer;
end;

procedure TUpdateManFm.UnCheckAllMnuClick(Sender: TObject);
begin
  FilesTree.UnCheckAll;
  UpdateControlState;
end;

procedure TUpdateManFm.UploadBnClick(Sender: TObject);
var
  SL: TStringList;
  i: Integer;
  N: TTreeNode;
begin
  if Confirm(rsWarning, rsUploadFilesToUpdatesDBMsg) <> mrYes then Exit;
  SL := TStringList.Create;
  for i := 0 to FilesTree.Items.Count - 1 do
  begin
    N := FilesTree.Items[i];
    if FilesTree.NodeChecked(N) and (N.Count = 0) then
      SL.Add(AppPath + StringReplace(N.GetTextPath, '/', PathDelim, [rfReplaceAll]));
  end;

  Screen.Cursor := crHourGlass;
  ShowMsg(rsProcessing);
  Application.ProcessMessages;
  try try
    FUpdateMan.Version := BuildDateToStr;
    FUpdateMan.UploadFiles(AppPath, SL);
    UpdateDbVer;
  finally
    SL.Free;
    HideMsg;
  end;
  except
    on E: Exception do
      MessageDlg(rsUploadFilesError, E.Message, mtError, [mbOk], 0);
  end;
  FillDBTree;
end;

procedure TUpdateManFm.UpdateDbVer;
begin
  DBVer.Caption := rsVersion + ': ' + IIF(FUpdateMan.Version = '', '-', FUpdateMan.Version);
end;

procedure TUpdateManFm.FillFilesTree;
var
  SL: TStringList;
  i: Integer;
  S: String;
  N: TTreeNode;
begin
  SL := FindAllFiles(AppPath, AllFilesMask, True);
  FilesTree.BeginUpdate;
  FilesTree.Items.Clear;
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    Delete(S, 1, Length(AppPath));
    N := FilesTree.AddNodePath(S, PathDelim, 0);
    N.ImageIndex := GetImageIndexByFileExt(S);
    N.SelectedIndex := N.ImageIndex;
  end;
  FilesTree.AlphaSort;
  FilesTree.EndUpdate;
  FilesSta.Caption := Format(rsUpdateDBFileInfo, ['-', '-']);
  SL.Free;
end;

procedure TUpdateManFm.FillDBTree;
var
  i: Integer;
  N: TTreeNode;
  F: TUpdateFileInfo;
begin
  DBTree.BeginUpdate;
  DBTree.Items.Clear;
  for i := 0 to FUpdateMan.Files.Count - 1 do
  begin
    F := FUpdateMan.Files[i];
    N := DBTree.AddNodePath(F.FileName, PathDelim, 0);
    N.ImageIndex := GetImageIndexByFileExt(F.FileName);
    N.SelectedIndex := N.ImageIndex;
    N.Data := FUpdateMan.Files.Items[i];
  end;
  DBTree.EndUpdate;
  DBSta.Caption := Format(rsUpdateDBFileInfo, ['-', '-']);

  if DBTree.Items.Count = 0 then
    ShowMsg(rsUpdatesDBIsEmpty);
end;

procedure TUpdateManFm.ShowMsg(const Msg: String);
begin
  MsgPan.Caption := Msg;
  MsgPan.Show;
end;

procedure TUpdateManFm.HideMsg;
begin
  Screen.Cursor := crDefault;
  MsgPan.Hide;
end;

function HasChecked(T: TCheckTreeView): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to T.Items.Count - 1 do
    if T.NodeChecked(T.Items[i]) then Exit(True);
end;

procedure TUpdateManFm.UpdateControlState;
begin
  UploadBn.Enabled := FUpdateMan.DB.Connected and HasChecked(FilesTree);
end;

procedure TUpdateManFm.ShowForm;
begin
  FUpdateMan := TUpdateMan.Create;
  if AppConfig.UpdatesDBPath = '' then
    Caption := rsUpdatesDB
  else
    Caption := rsUpdatesDB + ' (' + AppConfig.UpdatesDBPath + ')';

  AppVer.Caption := rsVersion + ': ' + BuildDateToStr;
  HideMsg;

  if AppConfig.UpdatesDBPath = '' then
  begin
    Info(rsUpdatesDBPathNotSet);
  end;

  UploadBn.Enabled := False;
  FillFilesTree;
  if (AppConfig.UpdatesDBPath <> '') and FUpdateMan.ConnectOrCreate then
  begin
    //UploadBn.Enabled := True;
    MoreBn.Enabled := True;
    FUpdateMan.LoadFromDb;
    FillDBTree;
  end
  else
  begin
    DBSta.Caption := Format(rsUpdateDBFileInfo, ['-', '-']);
    DBTree.Items.Clear;
    MoreBn.Enabled := False;
    ShowMsg(rsUpdatesDBUnavailable);
  end;
  UpdateDbVer;
  ShowModal;
  FUpdateMan.Free;
end;

end.

