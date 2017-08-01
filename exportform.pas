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
unit ExportForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CheckLst, EditBtn, ButtonPanel, ExtCtrls, DBGrids, strconsts, dxctrls, db;

type

  { TExportFm }

  TExportFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    CheckListBox1: TCheckListBox;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    procedure GetCheckedItems(SL: TStrings);
    function GetCheckFieldsCount: Integer;
    procedure FillFields(Fm: TdxForm);
  public
    { public declarations }
    function ShowForm(Fm: TdxForm): Integer;
  end;

var
  ExportFm: TExportFm;

implementation

uses
  mainform, LazUtf8, sqlgen, apputils, dximages, dxfiles, processform, helpform;

{$R *.lfm}

{ TExportFm }


procedure TExportFm.FormCreate(Sender: TObject);
begin
  Caption := rsExportData;
  Label1.Caption := rsSelectFields;
  Label2.Caption := rsFileName;
  RadioGroup1.Caption := rsEncoding;
  checkBox1.Caption := rsOpenFile;
  FileNameEdit1.DialogTitle:=rsExportData;
  FileNameEdit1.Filter := rsImportExportDataFilter;
  FileNameEdit1.DefaultExt:='csv';
  FileNameEdit1.DialogOptions:=FileNameEdit1.DialogOptions + [ofOverwritePrompt, ofPathMustExist];
  FileNameEdit1.DialogKind:=dkSave;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TExportFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult <> mrOk then Exit;
  CanClose := False;
  if GetCheckFieldsCount = 0 then
    ErrMsg(rsFieldsNotSelected)
  else if FileNameEdit1.Text = '' then
    ErrMsg(rsFileNameEmpty)
  else
    CanClose := True;
end;

procedure TExportFm.FormShow(Sender: TObject);
begin
  FileNameEdit1.SetFocus;
end;

procedure TExportFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('export');
end;

procedure TExportFm.GetCheckedItems(SL: TStrings);
var
  i: Integer;
begin
  SL.Clear;
  with CheckListBox1 do
    for i := 0 to Items.Count - 1 do
      if Checked[i] then SL.AddObject(Items[i], Items.Objects[i]);
end;

function TExportFm.GetCheckFieldsCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to CheckListBox1.Items.count - 1 do
    if CheckListBox1.Checked[i] then Inc(Result);
end;

{procedure TExportFm.FillFields(Fm: TdxForm);
var
  i: Integer;
  C: TComponent;
  L: TUtf8StringList;
begin
  L := TUtf8StringList.Create;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (not HasFId(C)) or (C is TdxGrid) then Continue;
    L.AddObject(GetFieldName(C), C);
  end;
  L.Sort;
  CheckListBox1.Items := L;
  L.Free;
end; }

procedure TExportFm.FillFields(Fm: TdxForm);
var
  i: Integer;
  C: TComponent;
  L: TStrings;
  Col: TColumn;
  S: String;
begin
  L := CheckListBox1.Items;
  L.Clear;
  for i := 0 to Fm.Grid.Columns.Count - 1 do
  begin
    Col := Fm.Grid.Columns[i];
    C := FindById(Fm, Col.Tag);
    S := GetFieldName(C);
    if S <> Col.Title.Caption then
      S := S + ' (' + Col.Title.Caption + ')';
    L.AddObject(S, C);
  end;
end;

function SortItems(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Utf8CompareText(List[Index1], List[Index2]);
end;

procedure GetDataSetFields(Fm: TdxForm; Fields: TStringList; DSFields: TList);
var
  i: Integer;
  FNm: String;
  F: TField;
begin
  for i := 0 to Fields.Count - 1 do
  begin
    FNm := FieldStr(TComponent(Fields.Objects[i]));
    F := Fm.Grid.DataSource.DataSet.FindField(FNm);
    DSFields.Add(F);
  end;
end;

function CheckFileExists(const FileName: String): String;
var
  S, Ext: String;
  No: Integer;
begin
  Result := FileName;
  No := 0;
  if FileExists(FileName) then
  begin
    Ext := ExtractFileExt(FileName);
    S := ChangeFileExt(FileName, '');
    repeat
      Inc(No);
      Result := S + '(' + IntToStr(No) + ')' + Ext;
    until not FileExists(Result);
  end;
end;

function TryCreateDir(const Folder: String): Boolean;
begin
  Result := DirectoryExists(Folder);
  if Result then Exit;
  Result := CreateDir(Folder);
  if not Result then
    ErrMsgFmt(rsExportCantCreateFolder, [Folder]);
end;

procedure ExportData(const FileName: String; DS: TDataSet; Fields: TStringList; DSFields: TList;
  ANSI: Boolean; aOpenFile: Boolean);
var
  AfterScroll, BeforeScroll: TDataSetNotifyEvent;
  B: TBookmark;
  FS: TFileStream;
  i, ProcessCounter, RecCounter: Integer;
  F: TField;
  C: TComponent;
  FileNm, BlobDir, S, ErrStr: String;

  function CreateBlobSubDirs: Boolean;
  var
    j: Integer;
    BlobDirCreated: Boolean;
    C: TComponent;
  begin
    Result := False;
    BlobDirCreated := False;
  	for j := 0 to Fields.Count - 1 do
    begin
      C := TComponent(Fields.Objects[j]);
      if (C is TdxDBImage) or (C is TdxFile) then
      begin
        if not BlobDirCreated then
        begin
          if not TryCreateDir(BlobDir) then Exit;
          BlobDirCreated := True;
        end;
        if not TryCreateDir(BlobDir + GetFieldName(C)) then Exit;
      end;
    end;
    Result := True;
  end;

begin
  BlobDir := ExtractFilePath(FileName) + 'blobdata' + DirectorySeparator;
  if not CreateBlobSubDirs then Exit;

  try
	  FS := TFileStream.Create(FileName, fmCreate);
  except
    on E: EFCreateError do
    begin
    	ErrMsgFmt(rsExportCantCreateFile, [FileName]);
      Exit;
    end;
  end;

  MainFm.Lock(True);
  DS.DisableControls;
  BeforeScroll := DS.BeforeScroll;
  AfterScroll := DS.AfterScroll;
  DS.BeforeScroll := nil;
  DS.AfterScroll:=nil;
  B := DS.GetBookmark;

  DS.Last; // Чтобы определить кол-во записей.
  try

    S := '';
    for i := 0 to Fields.Count - 1 do
      S := S + QuoteStr(GetFieldName( TComponent(Fields.Objects[i]) )) + ';';
    S := Copy(S, 1, Length(S) - 1) + LineEnding;
    if ANSI then S := Utf8ToWinCP(S);
    FS.WriteBuffer(Pointer(S)^, Length(S));

    ProcessCounter := 0;
    RecCounter := 0;
    ProcessFm.ShowForm;
    DS.First;
    while not DS.Eof do
    begin
      S := '';
      for i := 0 to DSFields.Count - 1 do
      begin
        F := TField(DSFields[i]);
        C := TComponent(Fields.Objects[i]);
        if C is TdxDBImage then
        begin
          FileNm := GetImageFileName(TdxDBImage(C), DS);
          if FileNm <> '' then
          begin
            FileNm := BlobDir + GetFieldName(C) + DirectorySeparator + ExtractFileName(FileNm);
            FileNm := CheckFileExists(FileNm);
            ErrStr := SaveImageToFile(FileNm, TdxDBImage(C), DS);
            if ErrStr = '' then
		          S := S + QuoteStr(FileNm)
            else
              S := S + QuoteStr(EscapeQuotes(ErrStr));
          end;
        end
        else if C is TdxFile then
        begin
          FileNm := GetFileFileName(TdxFile(C), DS);
          if FileNm <> '' then
          begin
            FileNm := BlobDir + GetFieldName(C) + DirectorySeparator + ExtractFileName(FileNm);
            FileNm := CheckFileExists(FileNm);
            ErrStr := SaveFileToFile(FileNm, TdxFile(C), DS);
            if ErrStr = '' then
		          S := S + QuoteStr(FileNm)
            else
              S := S + QuoteStr(EscapeQuotes(ErrStr));
          end;
        end
        else if C is TdxLookupComboBox then
          S := S + QuoteStr(EscapeQuotes(DS.FieldByName(F.FieldName + 'l').AsString))
        else
        begin
          if F.DataType in [ftString, ftMemo] then
	          S := S + QuoteStr(EscapeQuotes(F.AsString))
          else
          	S := S + F.AsString;
        end;
        if i < DSFields.Count - 1 then S := S + ';';
      end;
      S := S + LineEnding;
      if ANSI then S := Utf8ToWinCP(S);
      FS.WriteBuffer(Pointer(S)^, Length(S));

      if ProcessFm.Canceled then Break;
      Inc(RecCounter);
      ProcessFm.Msg.Caption := IntToStr(RecCounter) + ' / ' + IntToStr(DS.RecordCount);
      ProcessFm.Progress.Position:=Round(RecCounter / DS.RecordCount * 100);
      Inc(ProcessCounter);
      if ProcessCounter = 100 then
      begin
	      Application.ProcessMessages;
        ProcessCounter := 0;
      end;

      DS.Next;
    end;
  finally
    ProcessFm.Close;
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    DS.BeforeScroll:=BeforeScroll;
    DS.AfterScroll:=AfterScroll;
    DS.EnableControls;
    FS.Free;
    MainFm.Lock(False);
  end;
  if ProcessFm.Canceled then
    MessageDlg(rsExportData, rsCanceledByUser, mtInformation, [mbOk], 0)
  else if aOpenFile then
    OpenFile(FileName)
  else // Export ok
    MessageDlg(rsExportData, rsExportSuccess, mtInformation, [mbOk], 0);
end;

procedure CreateBlobFolders(const BasePath: String; Fields: TStringList);
var
  i: Integer;
  C: TComponent;
  S: String;
begin
  for i := 0 to Fields.Count - 1 do
  begin
    C := TComponent(Fields.Objects[i]);
    if (C is TdxDBImage) or (C is TdxFile) then
    begin
      S := GetFieldName(C);
      ForceDirectories(BasePath + S);
    end;
  end;
end;

function TExportFm.ShowForm(Fm: TdxForm): Integer;
var
  Fields: TList;
  SL: TStringList;
begin
  Caption := rsExportData + ': ' + Fm.FormCaption;
  Fields := TList.Create;
  SL := TStringList.Create;
  try

  FillFields(Fm);
  CheckListBox1.CheckAll(cbChecked);
  FileNameEdit1.Text := '';
  RadioGroup1.ItemIndex:=0;
  checkBox1.Checked := False;
  Result := ShowModal;
  if Result <> mrOk then Exit;
  GetCheckedItems(SL);
  GetDataSetFields(Fm, SL, Fields);
  CreateBlobFolders(ExtractFilePath(FileNameEdit1.Text) + 'blobdata' + DirectorySeparator, SL);
  ExportData(FileNameEdit1.Text, Fm.Grid.DataSource.DataSet, SL, Fields,
  	RadioGroup1.ItemIndex = 1, CheckBox1.Checked);
  finally
    SL.Free;
    Fields.Free;
  end;
end;

end.

