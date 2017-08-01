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
unit QuickSearchForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, db, LCLType, LazUtf8;

type

  { TQuickSearchFrm }

  TQuickSearchFrm = class(TForm)
    Edit1: TEdit;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FGrid: TDBGrid;
    FField: TField;
  public
    { public declarations }
    procedure ShowForm(const Key: String; Grid: TDBGrid);
  end;

var
  QuickSearchFrm: TQuickSearchFrm;

implementation

uses
  mainform;

{$R *.lfm}

type
  TFindDirection = (fdFirst, fdNext, fdPrev);

function DataSetFindText(ADataSet: TDataSet; const TextField, Text: String;
  Direction: TFindDirection): Boolean;
var
  S1, S2: String;
  B: TBookmark;
  AftScroll: TDataSetNotifyEvent;
begin
  Result := False;
  if (ADataSet = nil) or (ADataSet.Active = False) or (Text = '') then Exit;
  S1 := Utf8LowerCase(Text);
  with ADataSet do
    try
      AftScroll := AfterScroll;
      AfterScroll := nil;
      DisableControls;
      B := GetBookmark;
      case Direction of
        fdFirst: First;
        fdNext: Next;
        fdPrev: Prior;
      end;
      if Direction in [fdFirst, fdNext] then
        while not Eof do
        begin
          S2 := Utf8LowerCase(FieldByName(TextField).AsString);
          if Utf8Pos(S1, S2) > 0 then
          begin
            Result := True;
            Break;
          end;
          Next;
        end
      else
        while not Bof do
        begin
          S2 := Utf8LowerCase(FieldByName(TextField).AsString);
          if Utf8Pos(S1, S2) > 0 then
          begin
            Result := True;
            Break;
          end;
          Prior;
        end
    finally
      if not Result then
        GotoBookmark(B);
      FreeBookmark(B);
      EnableControls;
      AfterScroll := AftScroll;
      if AfterScroll <> nil then
        AfterScroll(ADataSet);
    end;
end;

{ TQuickSearchFrm }

procedure TQuickSearchFrm.FormShow(Sender: TObject);
begin
  Height := Edit1.Height;
  Edit1.Width := Width;
end;

procedure TQuickSearchFrm.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TQuickSearchFrm.Edit1Change(Sender: TObject);
begin
  if FField <> nil then
    DataSetFindText(FField.DataSet, FField.FieldName, Edit1.Text, fdFirst);
end;

procedure TQuickSearchFrm.Edit1Enter(Sender: TObject);
begin
  Edit1.SelStart := Utf8Length(Edit1.Text);
end;

procedure TQuickSearchFrm.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key in [VK_ESCAPE, VK_RETURN] then
    Close
  else if Key = VK_DOWN then
    DataSetFindText(FField.DataSet, FField.FieldName, Edit1.Text, fdNext)
  else if Key = VK_UP then
    DataSetFindText(FField.DataSet, FField.FieldName, Edit1.Text, fdPrev)
  else
    Exit;
  Key := 0;
end;

procedure TQuickSearchFrm.ShowForm(const Key: String; Grid: TDBGrid);
var
  P: TPoint;
  x, w: Integer;
begin
  FGrid := Grid;
  FField := Grid.SelectedField;
  x := Grid.SelectedFieldRect.Left;
  w := Grid.SelectedFieldRect.Right - x;
  P := Grid.ClientToScreen(Point(0, Grid.Height));
  Left := P.x + x; Top := P.y - Edit1.Height;
  Width := w;
  Edit1.Text := Key;
  Edit1.SelStart := Utf8Length(Edit1.Text);
  Show;
end;

end.

