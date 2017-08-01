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
unit FieldNameForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, dxctrls, strconsts;

type

  { TFieldNameFm }

  TFieldNameFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
    FCmp: TComponent;
  public
    { public declarations }
    function ShowForm(C: TComponent): Integer;
  end;

var
  FieldNameFm: TFieldNameFm;

implementation

uses
  apputils, dxusers;

{$R *.lfm}

{ TFieldNameFm }

procedure RenameComponentInRights(Fm: TdxForm; const OldName, NewName: String);
var
  i: Integer;
  R: TdxRole;
  FR: TdxFormRight;
  CR: TdxControlRight;
begin
  for i := 0 to UserMan.Users.Count - 1 do
  begin
    R := UserMan.Roles[i];
    FR := R.FormRights.FindRight(Fm.Id);
    if FR <> nil then
    begin
      CR := FR.Controls.FindRight(OldName);
      if CR <> nil then CR.Name:=NewName;
    end;
  end;
end;

procedure TFieldNameFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  S: String;
begin
  if ModalResult <> mrOk then Exit;

  if Trim(Edit2.Text) = '' then
  begin
    ErrMsg(Format(rsInvalidCmpName, [Edit2.Text]));
    CanClose := False;
    Exit;
  end;

  try
    FCmp.Name := Edit2.Text;
    SetFieldName(FCmp, Edit1.Text);
  except
    on E: EComponentError do
    begin
      CanClose := False;
      S := E.Message;
      if Pos('Duplicate', S) > 0 then
        ErrMsg(Format(rsDuplicateComponentName, [Edit2.Text]))
      else if Pos('is not a valid component name', S) > 0 then
        ErrMsg(Format(rsInvalidCmpName, [Edit2.Text]))
      else
        ErrMsg(S);
    end;
  end;
end;

function TFieldNameFm.ShowForm(C: TComponent): Integer;
begin
  FCmp := C;
  Edit1.Text := GetFieldName(C);
  Edit2.Text := C.Name;
  Result := ShowModal;
end;

end.

