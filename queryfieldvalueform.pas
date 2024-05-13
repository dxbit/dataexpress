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
    Qry: TComboBox;
    Field: TEditButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FieldButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure QryChange(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    procedure FillQueries;
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
  dxreports, reportmanager, mydialogs;

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
  Qry.SetFocus;
  SetControlState;
end;

procedure TQueryFieldValueFm.FieldButtonClick(Sender: TObject);
var
  RD: TReportData;
begin
  with Qry do
  	RD := TReportData(Items.Objects[ItemIndex]);

  with TSelectQFieldForm.CreateNew(nil) do
	try
    if ShowForm(RD) = mrOk then
    	Field.Text := FieldName;
  finally
    Free;
  end;
end;

procedure TQueryFieldValueFm.FormCreate(Sender: TObject);
begin
  Caption := rsFunctionGET;
  Label1.Caption := rsQuery;
  Label2.Caption := rsField;
  Field.Button.LoadGlyphFromLazarusResource('grid24');
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
end;

procedure TQueryFieldValueFm.QryChange(Sender: TObject);
begin
  SetControlState;
end;

procedure TQueryFieldValueFm.FillQueries;
var
  i: Integer;
  C: TComponent;
  RD: TReportData;
begin
  Qry.Clear;
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TdxQueryGrid then
    begin
      RD := ReportMan.FindReport(TdxQueryGrid(C).Id);
	    Qry.Items.AddObject(RD.Name, RD);
    end;
  end;
end;

procedure TQueryFieldValueFm.SetControlState;
begin
  Field.Enabled := Qry.ItemIndex >= 0;
end;

function TQueryFieldValueFm.ShowForm(Fm: TdxForm; var Res: String): Integer;
begin
  FFm := Fm;
  FillQueries;
  Field.Text := '';
	Result := ShowModal;
  if Result <> mrOk then Exit;
  Res := 'GET(''' + Qry.Text + ''', ''' + Field.Text + ''')';
end;

end.

