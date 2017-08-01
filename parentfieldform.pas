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
unit ParentFieldForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts, dxctrls;

type

  { TParentFieldFm }

  TParentFieldFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    List: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    procedure FillFields;
  public
    { public declarations }
    procedure ShowForm(aFm: TdxForm);
  end;

var
  ParentFieldFm: TParentFieldFm;

implementation

uses
  helpform, mytypes;

{$R *.lfm}

{ TParentFieldFm }

procedure TParentFieldFm.FormCreate(Sender: TObject);
begin
  Caption := rsParentField;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TParentFieldFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
end;

procedure TParentFieldFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('parentfield');
end;

procedure TParentFieldFm.FillFields;
var
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  SL := TStringListUtf8.Create;
  SL.Add('');
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TdxLookupComboBox then
      with TdxLookupComboBox(C) do
        if SourceTId = FFm.Id then
          SL.AddObject(GetFieldName(C), TObject(PtrInt(GetId(C))));
  end;
  SL.Sort;
  List.Items := SL;
  SL.Free;
end;

procedure TParentFieldFm.ShowForm(aFm: TdxForm);
begin
  FFm := aFm;
  FillFields;
  with List do
    ItemIndex := Items.IndexOfObject(TObject(PtrInt(FFm.ParentField)));
  if ShowModal = mrOk then
  begin
    with List do
      if ItemIndex >= 0 then
        FFm.ParentField := PtrInt(Items.Objects[ItemIndex])
      else
        FFm.ParentField := 0;
  end;
end;

end.

