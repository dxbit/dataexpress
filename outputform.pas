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
unit OutputForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, strconsts;

type
  { TOutputFm }

  TOutputFm = class(TForm)
    Memo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure Clear;
    procedure AddMsg(const S: String);
    procedure AddFmtMsg(const S: String; Params: array of const);
  end;

var
  OutputFm: TOutputFm;

implementation

{$R *.lfm}

{ TOutputFm }

procedure TOutputFm.FormCreate(Sender: TObject);
begin
  Caption := rsOutput;
  MenuItem1.Caption:=rsClear;
  MenuItem2.Caption := rsCopy;
end;

procedure TOutputFm.MenuItem1Click(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TOutputFm.MenuItem2Click(Sender: TObject);
begin
  Memo.CopyToClipboard;
end;

procedure TOutputFm.Clear;
begin
  Memo.Clear;
end;

procedure TOutputFm.AddMsg(const S: String);
begin
  Memo.Lines.Add(S);
  if not Visible then
  begin
    Left := Screen.Width - Width - 20;
    Top := 0;
    Show;
  end;
end;

procedure TOutputFm.AddFmtMsg(const S: String; Params: array of const);
begin
  AddMsg(Format(S, Params));
end;

end.

