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

unit DBFuncForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ButtonPanel, dxctrls, strconsts;

{ TDBFuncFm }

type
  TDBFuncFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Field: TEditButton;
    Filter: TEditButton;
    Func: TComboBox;
    Form: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FieldButtonClick(Sender: TObject);
    procedure FilterButtonClick(Sender: TObject);
    procedure FormChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FuncChange(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    procedure SetControlState;
  public
    { public declarations }
    function ShowForm(Fm: TdxForm; var Res: String): Integer;
  end;

var
  DBFuncFm: TDBFuncFm;

//function ShowDBFuncForm(Fm: TdxForm): String;

implementation

uses
  formmanager, mydialogs, exprform, helpmanager, apputils;

{function ShowDBFuncForm(Fm: TdxForm): String;
begin
  if DBFuncFm = nil then
  	DBFuncFm := TDBFuncFm.Create(Application);
  Result := DBFuncFm.ShowForm(Fm);
end;}

{$R *.lfm}

{ TDBFuncFm }

procedure TDBFuncFm.FormCreate(Sender: TObject);
begin
  Caption := rsDBFunction;
  Label1.Caption := rsFunction;
  Label2.Caption := rsForm;
  Label3.Caption := rsField;
  Label4.Caption := rsFilter;
  Field.Button.LoadGlyphFromLazarusResource('formfields16');
  Filter.Button.LoadGlyphFromLazarusResource('filter16');
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;

  Func.Items.AddStrings([rsDBAVGFunc,
  	rsDBCOUNTFunc,
    rsDBCOUNTDFunc,
  	rsDBGETFunc,
    rsDBGETIDFunc,
    rsDBGETBYIDFunc,
    rsDBMAXFunc,
    rsDBMERGEFunc,
    rsDBMERGEALLFunc,
    rsDBMINFunc,
    rsDBSUMFunc]);

  AddFormHeight(Self);
end;

procedure TDBFuncFm.FormShow(Sender: TObject);
begin
  Func.SetFocus;
  SetControlState;
end;

procedure TDBFuncFm.FuncChange(Sender: TObject);
begin
	SetControlState;
end;

procedure TDBFuncFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('dbfunc');
end;

procedure TDBFuncFm.SetControlState;
begin
  Form.Enabled := Func.ItemIndex >= 0;
  Field.Enabled := (not (Func.ItemIndex in [1, 4])) and (Form.ItemIndex >= 0);
  Filter.Enabled := (Func.ItemIndex <> 5) and (Form.ItemIndex >= 0);
  ButtonPanel1.OKButton.Enabled := Func.ItemIndex >= 0;

  if Func.ItemIndex in [1, 4] then Field.Text := ''
  else if Func.ItemIndex = 5 then Filter.Text := '';
end;

procedure TDBFuncFm.FieldButtonClick(Sender: TObject);
var
  Fm: TdxForm;
begin
  with Form do
  	if ItemIndex >= 0 then
      Fm := TdxForm(Items.Objects[ItemIndex])
    else
      Exit;

  with TSelectFieldForm.CreateNew(nil) do
  try
    Caption := rsSelectFormField;
    ShowFieldsOfObject:=True;
    if ShowForm(Fm, nil) = mrOk then
    	Field.Text := FieldName;
  finally
    Free;
  end;
end;

procedure TDBFuncFm.FilterButtonClick(Sender: TObject);
var
  Fm, PFm: TdxForm;
  S: String;
begin
  with Form do
  	if ItemIndex >= 0 then
      Fm := TdxForm(Items.Objects[ItemIndex])
    else
      Exit;
  if Fm.PId = 0 then
  begin
    PFm := Fm;
    Fm := nil;
  end
  else
	  PFm := FormMan.FindForm(Fm.PId);

  S := Filter.Text;
  with TExprFm.Create(nil) do
  try
  	if ShowForm(etDBFuncFilter, nil, S, FFm, Fm, PFm, nil) = mrOk then
			Filter.Text := S;
  finally
    Free;
  end;
end;

procedure TDBFuncFm.FormChange(Sender: TObject);
begin
  SetControlState;
end;

function TDBFuncFm.ShowForm(Fm: TdxForm; var Res: String): Integer;
var
  S: String;
begin
  Res := '';
  FFm := Fm;
  FormMan.AllFormsToList(Form.Items);
  Result := ShowModal;
  if Result = mrOk then
  begin
    S := Copy(Func.Text, 1, Pos(' - ', Func.Text) - 1);
    S := S + '(''' + Trim(Form.Text) + ''', ';
    if not (Func.ItemIndex in [1, 4]) then
    	S := S + '''' + Field.Text + ''', ';
    if Func.ItemIndex <> 5 then
    	S := S + '''' + StringReplace(Filter.Text, '''', '"', [rfReplaceAll]) + '''';
    S := S + ')';
    Res := S;
  end;
end;

end.

