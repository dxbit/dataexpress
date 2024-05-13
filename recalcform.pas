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

unit RecalcForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts, dxctrls;

type

  { TRecalcFm }

  TRecalcFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Forms: TComboBox;
    Fields: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
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
    function ShowForm(aFm: TdxForm; var TId, FId: Integer): Boolean;
  end;

var
  RecalcFm: TRecalcFm;

function ShowRecalcForm(aFm: TdxForm; var TId, FId: Integer): Boolean;

implementation

uses
  formmanager, apputils, helpmanager, mytypes;

function ShowRecalcForm(aFm: TdxForm; var TId, FId: Integer): Boolean;
begin
  if RecalcFm = nil then
  	RecalcFm := TRecalcFm.Create(Application);
  Result := RecalcFm.ShowForm(aFm, TId, FId);
end;

{$R *.lfm}

{ TRecalcFm }

procedure TRecalcFm.FormsChange(Sender: TObject);
begin
  FillFields;
end;

procedure TRecalcFm.FormShow(Sender: TObject);
begin
  Forms.SetFocus;
end;

procedure TRecalcFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('recalc');
end;

procedure TRecalcFm.FormCreate(Sender: TObject);
begin
  Caption := rsRecalculate;
  Label1.Caption := rsForm;
  Label2.Caption := rsField;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TRecalcFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
  begin
    if Fields.ItemIndex < 0 then
    begin
      ErrMsg(rsFieldNotSel);
      CanClose := False;
    end;
  end;
end;

procedure TRecalcFm.FillForms;
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
      SL.AddObject(Fm.GetRecordsCaption, Fm);
  end;
  SL.Sort;
  SL.InsertObject(0, FFm.GetRecordsCaption, FFm);
  Forms.Items := SL;
  SL.Free;
end;

procedure TRecalcFm.FillFields;
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
    Expr := GetExpression(C);
    if Expr <> '' then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Fields.Items := SL;
  SL.Free;
end;

function TRecalcFm.ShowForm(aFm: TdxForm; var TId, FId: Integer): Boolean;
begin
  FFm := aFm;
  FillForms;
  Fields.Clear;
  Result := ShowModal = mrOk;
  if Result then
  begin
    TId := TdxForm(Forms.Items.Objects[Forms.ItemIndex]).Id;
    FId := GetId(TComponent(Fields.Items.Objects[Fields.ItemIndex]));
  end;
end;

end.

