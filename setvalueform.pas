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
unit SetValueForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, EditBtn, dxctrls, strconsts;

type

  { TSetValueFm }

  TSetValueFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ExprEd: TEditButton;
    Fields: TComboBox;
    Forms: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ExprEdButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    procedure FillForms;
    procedure FillFields;
  public
    { public declarations }
    function ShowForm(aFm: TdxForm; var TId, FId: Integer; var Expr: String): Boolean;
  end;

var
  SetValueFm: TSetValueFm;

implementation

uses
  formmanager, apputils, helpform, exprform, mytypes;

{$R *.lfm}

{ TSetValueFm }

procedure TSetValueFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
  begin
    CanClose := False;
    if Fields.ItemIndex < 0 then ErrMsg(rsFieldNotSel)
    else if Trim(ExprEd.Text) = '' then ErrMsg(rsEnterExpr)
    else CanClose := True;
  end;
end;

procedure TSetValueFm.ExprEdButtonClick(Sender: TObject);
var
  S: TCaption;
  Fm: TdxForm;
begin
  S := ExprEd.Text;
  Fm := nil;
  if Forms.ItemIndex >= 0 then
    Fm := TdxForm(Forms.Items.Objects[Forms.ItemIndex]);
  if ExprFm.ShowForm(rsExpression, nil, S, Fm, nil, nil, nil, 'expressions') then
    ExprEd.Text := S;
end;

procedure TSetValueFm.FormCreate(Sender: TObject);
begin
  Caption := rsSetValue;
  Label1.Caption := rsForm;
  Label2.Caption := rsField;
  Label3.Caption := rsExpression;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TSetValueFm.FormsChange(Sender: TObject);
begin
  FillFields;
end;

procedure TSetValueFm.FormShow(Sender: TObject);
begin
  Forms.SetFocus;
end;

procedure TSetValueFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('setvalue');
end;

procedure TSetValueFm.FillForms;
var
  SL: TStringListUtf8;
  i: Integer;
  Fm: TdxForm;
begin
  Forms.Clear;
  SL := TStringListUtf8.Create;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if Fm.PId = FFm.Id then
      SL.AddObject(Fm.FormCaption, Fm);
  end;
  SL.Sort;
  SL.InsertObject(0, FFm.FormCaption, FFm);
  Forms.Items := SL;
  SL.Free;
end;

procedure TSetValueFm.FillFields;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  Expr: String;
  SL: TStringListUtf8;
begin
  Fields.Clear;
  if Forms.ItemIndex < 0 then Exit;
  SL := TStringListUtf8.Create;
  Fm := TdxForm(Forms.Items.Objects[Forms.ItemIndex]);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not IsField(C) then Continue;
    Expr := Trim(GetExpression(C));
    if Expr = '' then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Fields.Items := SL;
  SL.Free;
end;

function TSetValueFm.ShowForm(aFm: TdxForm; var TId, FId: Integer;
  var Expr: String): Boolean;
begin
  FFm := aFm;
  FillForms;
  Fields.Clear;
  ExprEd.Text := '';
  Result := ShowModal = mrOk;
  if Result then
  begin
    TId := TdxForm(Forms.Items.Objects[Forms.ItemIndex]).Id;
    FId := GetId(TComponent(Fields.Items.Objects[Fields.ItemIndex]));
    Expr := Trim(ExprEd.Text);
  end;
end;

end.

