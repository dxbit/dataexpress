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

unit QueryFieldValueForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ButtonPanel, strconsts, dxctrls;

{ TQueryFieldValueFm }

type
  TQueryFieldValueFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    QryCbx: TComboBox;
    Field: TEditButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FieldButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure QryCbxChange(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    procedure FillTablesQueries;
    procedure SetControlState;
  public
    { public declarations }
    function ShowForm(Fm: TdxForm; var Res: String): Integer;
  end;

var
  QueryFieldValueFm: TQueryFieldValueFm;

function ShowQueryFieldValueForm(Fm: TdxForm; var Res: String): Integer;

implementation

uses
  dxreports, reportmanager, formmanager, mydialogs;

function ShowQueryFieldValueForm(Fm: TdxForm; var Res: String): Integer;
begin
  if QueryFieldValueFm = nil then
  	QueryFieldValueFm := TQueryFieldValueFm.Create(Application);
  Result := QueryFieldValueFm.ShowForm(Fm, Res);
end;

{$R *.lfm}

{ TQueryFieldValueFm }

procedure TQueryFieldValueFm.FormShow(Sender: TObject);
begin
  QryCbx.SetFocus;
  SetControlState;
end;

procedure TQueryFieldValueFm.FieldButtonClick(Sender: TObject);
var
  RD: TReportData;
  i: Integer;
  Fm: TdxForm;
begin
  i := QryCbx.ItemIndex;

  if QryCbx.Items.Objects[i] is TReportData then
  begin
 	  RD := TReportData(QryCbx.Items.Objects[i]);

    with TSelectQFieldForm.CreateNew(nil) do
	  try
      if ShowForm(RD) = mrOk then
    	  Field.Text := FieldName;
    finally
      Free;
    end;
  end
  else if QryCbx.Items.Objects[i] is TdxForm then
  begin
    Fm := TdxForm(QryCbx.Items.Objects[i]);

    with TSelectFieldForm.CreateNew(nil) do
    try
      if ShowForm(Fm, nil) = mrOk then
        Field.Text := FieldName;
    finally
      Free;
    end;
  end;
end;

procedure TQueryFieldValueFm.FormCreate(Sender: TObject);
begin
  Caption := rsFunctionGET;
  Label1.Caption := rsTableOrQuery;
  Label2.Caption := rsField;
  Field.Button.LoadGlyphFromLazarusResource('grid24');
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
end;

procedure TQueryFieldValueFm.QryCbxChange(Sender: TObject);
begin
  SetControlState;
end;

procedure TQueryFieldValueFm.FillTablesQueries;
var
  i: Integer;
  C: TComponent;
  RD: TReportData;
  Fm: TdxForm;
begin
  QryCbx.Clear;
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TdxQueryGrid then
    begin
      RD := ReportMan.FindReport(TdxQueryGrid(C).Id);
	    QryCbx.Items.AddObject(RD.Name, RD);
    end
    else if C is TdxGrid then
    begin
      Fm := FormMan.FindForm(GetId(C));
      QryCbx.Items.AddObject(Fm.FormCaption, Fm);
    end;
  end;
end;

procedure TQueryFieldValueFm.SetControlState;
begin
  Field.Enabled := QryCbx.ItemIndex >= 0;
end;

function TQueryFieldValueFm.ShowForm(Fm: TdxForm; var Res: String): Integer;
begin
  FFm := Fm;
  FillTablesQueries;
  Field.Text := '';
	Result := ShowModal;
  if Result <> mrOk then Exit;
  Res := 'GET(''' + QryCbx.Text + ''', ''' + Field.Text + ''')';
end;

end.

