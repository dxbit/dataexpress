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
unit ReportExportForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CheckLst, EditBtn, ButtonPanel, ExtCtrls, strconsts, DXReports, db, DBGrids;

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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
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

implementation

uses
  apputils, helpform, LazFileUtils, LazUtf8;

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

procedure TReportExportFm.FormCreate(Sender: TObject);
begin
  Caption := rsReportExport;
  Label1.Caption := rsSelectFields;
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

procedure TReportExportFm.FillFields;
var
  i: Integer;
  C: TColumn;
begin
  List.Clear;
  for i := 0 to FGrid.Columns.Count - 1 do
  begin
    C := FGrid.Columns[i];
    List.Items.AddObject(C.Title.Caption, C);
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
    S := GetTempFilenameUtf8(GetTempDir, 'export')
  else
    S := FlName.Text;
  S := ChangeFileExt(S, '.csv');
  FExportFlName := S;
  try
	  FS := TFileStream.Create(S, fmCreate);
  except
    on E: EFCreateError do
    begin
      ErrMsgFmt(rsExportCantCreateFile, [S]);
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
    MessageDlg(rsExportData, rsExportSuccess, mtInformation, [mbOk], 0);
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
  DoExport;
end;

end.

