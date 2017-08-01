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
unit SearchReplaceForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel, strconsts;

type

  { TSearchReplaceFm }

  TSearchReplaceFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    Options: TCheckGroup;
    SearchText: TComboBox;
    ReplaceText: TComboBox;
    Label1: TLabel;
    Start: TRadioGroup;
    Where: TRadioGroup;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm: Integer;
    procedure SetFind;
    procedure SetReplace;
  end;

var
  SearchReplaceFm: TSearchReplaceFm;

implementation

{$R *.lfm}

{ TSearchReplaceFm }

procedure TSearchReplaceFm.CheckBox1Change(Sender: TObject);
begin
  ReplaceText.Enabled := CheckBox1.Checked;
  if CheckBox1.Checked then
  begin
    ButtonPanel1.ShowButtons:=[pbOk, pbCancel, pbClose];
    ButtonPanel1.OkButton.Caption := rsReplace;
    ButtonPanel1.CloseButton.Caption := rsReplaceAll;
  end
  else
  begin
    ButtonPanel1.OkButton.Caption := rsFind;
    ButtonPanel1.ShowButtons:=[pbOk, pbCancel];
  end;
end;

procedure TSearchReplaceFm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  S: String;
begin
  if ModalResult in [mrOk, mrYesToAll] then
  begin
    with SearchText do
    begin
      S := Trim(Text);
      if (S <> '') and (Items.IndexOf(S) < 0) then
        Items.Add(S);
    end;
    with ReplaceText do
    begin
      S := Trim(Text);
      if (S <> '') and (Items.IndexOf(S) < 0) then
        Items.Add(S);
    end;
  end;
end;

procedure TSearchReplaceFm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  if ModalResult = mrYesToAll then
  begin
    if MessageDlg(rsWarning, rsReplaceAllMsg, mtWarning,
      [mbYes, mbNo], 0) = mrNo then CanClose := False;
  end;
end;

procedure TSearchReplaceFm.FormCreate(Sender: TObject);
begin
  Caption := rsFindReplace;
  Label1.Caption := rsSearchedText;
  CheckBox1.Caption := rsReplace;
  Options.Caption := rsSearchOptions;
  Options.Items[0] := rsWholeWords;
  Options.Items[1] := rsCaseSensitive;
  Start.Caption:=rsStart;
  Start.Items[0] := rsFromCursor;
  Start.Items[1] := rsFromBegin;
  Where.Caption := rsSearchArea;
  Where.Items[0] := rsEntireScope;
  Where.Items[1] := rsInSelected;
  ButtonPanel1.OkButton.Caption := rsFind;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.ShowGlyphs:=[];
  ButtonPanel1.CloseButton.ModalResult:=mrYesToAll;
end;

procedure TSearchReplaceFm.FormShow(Sender: TObject);
begin
  SearchText.SetFocus;
end;

function TSearchReplaceFm.ShowForm: Integer;
begin
  Result := ShowModal;
end;

procedure TSearchReplaceFm.SetFind;
begin
  CheckBox1.Checked := False;
end;

procedure TSearchReplaceFm.SetReplace;
begin
  CheckBox1.Checked := True;
end;

end.

