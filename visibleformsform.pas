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
unit VisibleFormsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  ButtonPanel, strconsts, DxCtrls;

type

  { TVisibleFormsFm }

  TVisibleFormsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    List: TCheckListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm: Integer;
  end;

var
  VisibleFormsFm: TVisibleFormsFm;

implementation

uses
  designerframe, helpform;

{$R *.lfm}

{ TVisibleFormsFm }

procedure TVisibleFormsFm.FormCreate(Sender: TObject);
begin
  Caption := rsVisibleForms;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TVisibleFormsFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
end;

procedure TVisibleFormsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('visibleforms');
end;

function TVisibleFormsFm.ShowForm: Integer;
var
  i: Integer;
  Fm: TdxForm;
begin
  List.Clear;
  for i := 0 to DesignFr.FormList.Count - 1 do
  begin
    Fm := TdxForm(DesignFr.FormList.Items.Objects[i]);
    if Fm.PId = 0 then
    begin
      List.Items.AddObject(Fm.FormCaption, Fm);
      List.Checked[List.Count - 1] := Fm.AutoOpen;
    end;
  end;
  Result := ShowModal;
  if Result <> mrOk then Exit;
  for i := 0 to List.Count - 1 do
  begin
    Fm := TdxForm(List.Items.Objects[i]);
    Fm.AutoOpen := List.Checked[i];
  end;
end;

end.

