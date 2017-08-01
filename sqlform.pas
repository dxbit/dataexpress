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
unit SqlForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterSQL, Forms, Controls,
  Graphics, Dialogs, DBGrids, ExtCtrls, ComCtrls, strconsts, db, SqlDb,
  dxsqlquery, SynEditTypes, LclType, StdCtrls;

{ TSqlFm }

type
  TSqlFm = class(TForm)
    DataSource1: TDataSource;
    Errors: TMemo;
    Grid: TDBGrid;
    ImageList1: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    Edit: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    { private declarations }
    FQry: TdxSQLQuery;
    procedure SetColWidths;
    procedure FreeQry;
  public
    { public declarations }
    function ShowForm: Integer;
  end;

var
  SqlFm: TSqlFm;

implementation

uses
  Clipbrd;

{$R *.lfm}

{ TSqlFm }

procedure TSqlFm.FormCreate(Sender: TObject);
begin
  Caption := rsSQLEditor;
  ToolButton1.Hint := rsExecuteSQL;
  ToolButton2.Hint := rsCopyToPasteScript;
  ToolButton3.Hint := rsPasteFromScript;
  with ImageList1 do
	begin
    AddLazarusResource('play16');
    AddLazarusResource('copy16');
    AddLazarusResource('paste16');
  end;
end;

procedure TSqlFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeQry;
end;

procedure TSqlFm.EditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  with StatusBar1 do
  begin
    Panels[0].Text := IntToStr(Edit.CaretY) + ': ' + IntToStr(Edit.CaretX);
    if Edit.InsertMode then
      Panels[1].Text := rsInserting
    else
      Panels[1].Text := rsReplacing;
  end;
end;

procedure TSqlFm.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = VK_F9 then
  begin
    Key := 0;
    ToolButton1.Click;
  end;
end;

procedure TSqlFm.ToolButton1Click(Sender: TObject);
begin
  FreeQry;
  if Trim(Edit.Text) = '' then Exit;
  try
	  FQry := TdxSQLQuery.Create(Edit.Text);
  	DataSource1.DataSet := FQry.DataSet;
    FQry.Open;
    SetColWidths;
    Errors.Visible := False;
    Grid.Visible := True;
  except
    on E: Exception do
    begin
      Grid.Visible := False;
      Errors.Visible := True;
    	Errors.Text := E.Message;
      if FQry <> nil then
      	Errors.Lines.AddStrings(['', '', FQry.DataSet.SQL.Text]);
    end;
  end;
end;

procedure TSqlFm.ToolButton2Click(Sender: TObject);
var
  i: Integer;
  S: String;
begin
  S := 'SQL := ';
  for i := 0 to Edit.Lines.Count - 1 do
  begin
  	S := S + '''' + StringReplace(Edit.Lines[i], #39, #39#39, [rfReplaceAll]) + '''';
    if i < Edit.Lines.Count - 1 then
    	S := S + ' + ' + LineEnding + '  ';
  end;
  Clipboard.AsText := S + ';';
  MessageDlg(rsSQLEditor, rsSQLTextCopyToClip,	mtInformation, [mbOk], 0);
end;

function DeleteQuotes(const S: String): String;
var
  i, Len: Integer;
  Ch: String;
begin
  Result := '';
  i := 1; Len := Length(S);
  if Len = 0 then Exit;
  while i <= Len do
	begin
    if S[i] = #39 then
    begin
      if Copy(S, i + 1, 1) = #39 then
      begin
      	Result := Result + S[i];
        Inc(i);
      end;
    end
    else
    	Result := Result + S[i];
    Inc(i);
  end;
  Ch := Copy(Result, Length(Result), 1);
  if (Ch = '+') or (Ch = ';') then
  	Result := Copy(Result, 1, Length(Result) - 1);
end;

procedure TSqlFm.ToolButton3Click(Sender: TObject);
var
  SL: TStringList;
  i: Integer;
  S: String;
begin
  if not Clipboard.HasFormat(CF_Text) then Exit;
  SL := TStringList.Create;
  SL.Text := Trim(Clipboard.AsText);
  for i := 0 to SL.Count - 1 do
  begin
    S := Trim(SL[i]);
    SL[i] := DeleteQuotes(S);
  end;
  Edit.SelText := SL.Text;
  SL.Free;
end;

procedure TSqlFm.SetColWidths;
var
  i: Integer;
begin
  for i := 0 to Grid.Columns.Count - 1 do
    Grid.Columns[i].Width := 100;
end;

procedure TSqlFm.FreeQry;
begin
  if FQry <> nil then
  begin
    FQry.Close;
    FreeAndNil(FQry);
  end;
end;

function TSqlFm.ShowForm: Integer;
begin
  Result := ShowModal;
end;

end.

