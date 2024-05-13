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

unit FormFieldValueForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts, dxctrls;

{ TFormFieldValueFm }

type
  TFormFieldValueFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Form: TListBox;
    Field: TListBox;
    Value: TListBox;
    procedure FieldSelectionChange(Sender: TObject; User: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormSelectionChange(Sender: TObject; User: boolean);
    procedure FormShow(Sender: TObject);
    procedure ValueDblClick(Sender: TObject);
    procedure ValueSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    procedure FillFields;
    procedure FillValues;
    procedure SetControlState;
  public
    { public declarations }
    function ShowForm(var AValue: String): Integer;
  end;

var
  FormFieldValueFm: TFormFieldValueFm;

function ShowFormFieldValueForm(var AValue: String): Integer;

implementation

uses
  apputils, formmanager, dximages, dxfiles, dbengine, sqlgen;

function ShowFormFieldValueForm(var AValue: String): Integer;
begin
  if FormFieldValueFm = nil then
  	FormFieldValueFm := TFormFieldValueFm.Create(Application);
  Result := FormFieldValueFm.ShowForm(AValue);
end;

{$R *.lfm}

{ TFormFieldValueFm }

procedure TFormFieldValueFm.FormSelectionChange(Sender: TObject; User: boolean);
begin
  FillFields;
  Value.Clear;
  SetControlState;
end;

procedure TFormFieldValueFm.FormShow(Sender: TObject);
begin
  Form.SetFocus;
  SetControlState;
end;

procedure TFormFieldValueFm.ValueDblClick(Sender: TObject);
begin
  if Value.ItemIndex >= 0 then ModalResult := mrOk;
end;

procedure TFormFieldValueFm.ValueSelectionChange(Sender: TObject; User: boolean
  );
begin
  SetControlState;
end;

procedure TFormFieldValueFm.FieldSelectionChange(Sender: TObject; User: boolean
  );
begin
  FillValues;
  SetControlState;
end;

procedure TFormFieldValueFm.FormCreate(Sender: TObject);
begin
  Caption := rsValueFromForm;
  Label1.Caption := rsForm;
  Label2.Caption := rsField;
  Label3.Caption := rsValue;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
end;

procedure TFormFieldValueFm.FillFields;
var
  Fm: TdxForm;
begin
  Field.Items.Clear;
  with Form do
		if ItemIndex >= 0 then
      Fm := TdxForm(Items.Objects[ItemIndex])
    else Exit;
  FieldsToList(Fm, Field.Items, [TdxObjectField, TdxFile, TdxDBImage]);
end;

procedure TFormFieldValueFm.FillValues;
var
  Fm: TdxForm;
  C: TComponent;
  SQL: String;
begin
  with Form do
		if ItemIndex >= 0 then
      Fm := TdxForm(Items.Objects[ItemIndex])
    else Exit;

  with Field do
		if ItemIndex >= 0 then
      C := TComponent(Items.Objects[ItemIndex])
    else Exit;

  SQL := 'select distinct ' + FieldStr(C) + ' from ' + TableStr(Fm.Id) +
  	' where ' + FieldStr(C) + ' is not null order by 1';

  Value.Clear;
  with DBase.OpenDataSet(SQL) do
  try
    while not EOF do
		begin
      Value.Items.Add(Fields[0].AsString);
      Next;
    end;
  finally
    Free;
  end;
end;

procedure TFormFieldValueFm.SetControlState;
begin
  ButtonPanel1.OkButton.Enabled := Value.ItemIndex >= 0;
end;

function TFormFieldValueFm.ShowForm(var AValue: String): Integer;
var
  C: TComponent;
begin
  FormMan.SourceFormsToList(Form.Items);
  Form.ItemIndex := -1;
  Field.Clear;
  Value.Clear;
  Result := ShowModal;
  if Result = mrOk then
  begin
    AValue := Value.Items[Value.ItemIndex];

    with Field do
    	C := TComponent(Items.Objects[ItemIndex]);
    if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) or
    	(C is TdxDateEdit) or (C is TdxTimeEdit) then
    	AValue := '''' + AValue + '''';
  end;
end;

end.

