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

unit ReportExportForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CheckLst, EditBtn, ButtonPanel, ExtCtrls, strconsts, DXReports, db, DBGrids,
  Buttons;

type

  { TReportExportFm }

  TReportExportFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    OpenFlag: TCheckBox;
    List: TCheckListBox;
    FlName: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    ExpEncode: TRadioGroup;
    CheckAll: TSpeedButton;
    UncheckAll: TSpeedButton;
    procedure CheckAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure UncheckAllClick(Sender: TObject);
  private
    FRD: TReportData;
    FGrid: TDBGrid;
    FExportFlName: String;
    function GetCheckFieldsCount: Integer;
    procedure FillFields;
    procedure DoExport;
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(RD: TReportData; Grid: TDBGrid);
  end;

var
  ReportExportFm: TReportExportFm;

procedure ShowReportExportForm(RD: TReportData; Grid: TDBGrid);

implementation

uses
  apputils, helpmanager, LazFileUtils, LazUtf8;

procedure ShowReportExportForm(RD: TReportData; Grid: TDBGrid);
begin
  if ReportExportFm = nil then
    ReportExportFm := TReportExportFm.Create(Application);
  ReportExportFm.ShowForm(RD, Grid);
end;

{$R *.lfm}

{ TReportExportFm }

function TReportExportFm.GetCheckFieldsCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to List.Items.count - 1 do
    if List.Checked[i] then Inc(Result);
end;

procedure TReportExportFm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  if ModalResult <> mrOk then Exit;
  CanClose := False;
  if GetCheckFieldsCount = 0 then
    ErrMsg(rsFieldsNotSelected)
  //else if FlName.Text = '' then
  //  ErrMsg(rsFileNameEmpty)
  else
    CanClose := True;
end;

procedure TReportExportFm.CheckAllClick(Sender: TObject);
begin
  List.CheckAll(cbChecked);
end;

procedure TReportExportFm.FormCreate(Sender: TObject);
begin
  Caption := rsReportExport;
  Label1.Caption := rsSelectFields;
  CheckAll.LoadGlyphFromLazarusResource('checkall16');
  CheckAll.Hint := rsCheckAll;
  UncheckAll.LoadGlyphFromLazarusResource('uncheckall16');
  UncheckAll.Hint := rsUncheckAll;
  Label2.Caption := rsReportExportFilename;
  ExpEncode.Caption := rsEncoding;
  OpenFlag.Caption := rsOpenFile;
  FlName.DialogTitle:=rsExportData;
  FlName.Filter := rsImportExportDataFilter;
  FlName.DefaultExt:='csv';
  FlName.DialogOptions:=FlName.DialogOptions + [ofOverwritePrompt, ofPathMustExist];
  FlName.DialogKind := dkSave;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TReportExportFm.FormShow(Sender: TObject);
begin
  FlName.SetFocus;
end;

procedure TReportExportFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('exportreport');
end;

procedure TReportExportFm.UncheckAllClick(Sender: TObject);
begin
  List.CheckAll(cbUnchecked);
end;

procedure TReportExportFm.FillFields;
var
  i: Integer;
  C: TColumn;
begin
  List.Clear;
  for i := 0 to FGrid.Columns.Count - 1 do
  begin
    C := FGrid.Columns[i];
    if C.Visible then List.Items.AddObject(C.Title.Caption, C);
  end;
end;

procedure TReportExportFm.DoExport;
var
  FS: TFileStream;
  DS: TDataSet;
  B: TBookmark;
  i: Integer;
  S, Prefix: String;
  F: TField;
begin
  if Trim(FlName.Text) = '' then
  begin
    S := GetOutputFileName(GetTempDir + FRD.Name + '.csv');
  end
  else
  begin
    S := FlName.Text;
    if ExtractFileExt(S) = '' then
    	S := S + '.csv';
  end;
  FExportFlName := S;
  try
	  FS := TFileStream.Create(S, fmCreate);
  except
    on E: EFCreateError do
    begin
      ErrMsgFmt(rsExportCantCreateFile, [S], True, 'ReportExport');
      Exit;
    end;
  end;

  S := '';
  for i := 0 to List.Items.Count - 1 do
  begin
    if not List.Checked[i] then Continue;
    S := S + QuoteStr(List.Items[i]);
    if i < List.Items.Count - 1 then
      S := S + ';';
  end;
  S := S + LineEnding;
  if ExpEncode.ItemIndex = 1 then
    S := Utf8ToWinCP(S);
  FS.Write(Pointer(S)^, Length(S));

  DS := FGrid.DataSource.DataSet;
  B := DS.GetBookmark;
  DS.DisableControls;
  DS.First;
  try
    while not DS.Eof do
    begin
      S := '';
      for i := 0 to List.Items.Count - 1 do
      begin
        if not List.Checked[i] then Continue;
        F := TColumn(List.Items.Objects[i]).Field;
        Prefix := UpperCase(Copy(F.FieldName, 1, 1));
        if (F.DataType in [ftString, ftMemo]) and	(Prefix = 'F') then
	        S := S + QuoteStr(EscapeQuotes(F.AsString))
        // Вычисляемые поля
        else if Prefix = 'C' then
        	S := S + StrToCsv(F.AsString)
        else
        	S := S + F.AsString;
        if i < List.Items.Count - 1 then
          S := S + ';';
      end;
      if ExpEncode.ItemIndex = 1 then
        S := Utf8ToWinCP(S);
      S := S + LineEnding;
      FS.Write(Pointer(S)^, Length(S));
      DS.Next;
    end;
  finally
    FS.Free;
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    DS.EnableControls;
  end;

  if OpenFlag.Checked then
    OpenFile(FExportFlName)
  else
    MessageDlg(rsExportData, Format(rsExportSuccess, [FExportFlName]), mtInformation, [mbOk], 0);
end;

procedure TReportExportFm.ShowForm(RD: TReportData; Grid: TDBGrid);
begin
  FRD := RD;
  FGrid := Grid;
  FillFields;
  List.CheckAll(cbChecked);
  FlName.Text:='';
  ExpEncode.ItemIndex:=1;
  OpenFlag.Checked := True;
  if ShowModal <> mrOk then Exit;
  try
    DoExport;
  except
    on E: Exception do
      ErrMsg(rsExportDataError + ExceptionToString(E, True, False), True, 'ReportExport');
  end;
end;

end.

