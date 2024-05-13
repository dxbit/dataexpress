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

unit TotalForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, EditBtn, dxctrls, strconsts, DXReports;

type

  { TTotalFm }

  TTotalFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Field: TEditButton;
    Filter: TEditButton;
    FormList: TComboBox;
    Func: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FieldButtonClick(Sender: TObject);
    procedure FilterButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormListChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FuncChange(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FForm: TdxForm;
    procedure SetControlState;
    procedure FillForms(aForm: TdxForm);
    //procedure FillFields;
  public
    { public declarations }
    function ShowForm(aForm: TdxForm; var Res: String): Integer;
  end;

var
  TotalFm: TTotalFm;

function ShowTotalForm(aForm: TdxForm; var Res: String): Integer;

implementation

uses
  formmanager, mydialogs, helpmanager, reportmanager, mytypes, exprform,
  dximages, dxfiles, apputils;

function ShowTotalForm(aForm: TdxForm; var Res: String): Integer;
begin
  with TTotalFm.Create(nil) do
  try
    Result := ShowForm(aForm, Res);
  finally
    Free;
  end;
  {if TotalFm = nil then
  	TotalFm := TTotalFm.Create(Application);
  Result := TotalFm.ShowForm(aForm); }
end;

{$R *.lfm}

{ TTotalFm }

procedure TTotalFm.FormCreate(Sender: TObject);
begin
  Caption := rsInsertTotalFunc;
  Label1.Caption := rsTableOrQuery;
  Label2.Caption := rsField;
  Label3.Caption := rsFunction;
  Label4.Caption := rsFilter;
  Field.Button.LoadGlyphFromLazarusResource('formfields16');
  Filter.Button.LoadGlyphFromLazarusResource('filter16');
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  Func.Items.AddStrings([rsSum, rsAverage, rsCount, rsMaximum, rsMinimum,
    rsMerge, rsTakeValue]);
  AddFormHeight(Self);
end;

procedure TTotalFm.FormListChange(Sender: TObject);
begin
  SetControlState;
end;

procedure TTotalFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  {if ModalResult <> mrOk then Exit;
  CanClose := False;
  if FormList.ItemIndex < 0 then
    ErrMsg(rsTableNotSelect)
  else if (Field.ItemIndex < 0) and (Func.ItemIndex <> 2) then
    ErrMsg(rsFieldNotSel)
  else if Func.ItemIndex < 0 then
    ErrMsg(rsFuncNotSelect)
  else
    CanClose := True;   }
end;

procedure TTotalFm.FilterButtonClick(Sender: TObject);
var
  S: TCaption;
  Obj: TObject;
  Fm: TdxForm;
  RD: TReportData;
begin
  with FormList do
  	if ItemIndex >= 0 then
		  Obj := Items.Objects[ItemIndex]
    else Exit;

  Fm := nil; RD := nil;
  if Obj is TdxForm then Fm := TdxForm(Obj)
  else
  begin
    RD := TReportData(Obj);
    Fm := FForm;
  end;

  with TExprFm.Create(nil) do
  try
    S := Filter.Text;
    if ShowForm(etLogicalExpr, nil, S, Fm, nil, nil, RD) = mrOk then
      Filter.Text := S;
  finally
    Free;
  end;
end;

procedure TTotalFm.FieldButtonClick(Sender: TObject);
var
  Obj: TObject;
begin
  with FormList do
	  Obj := Items.Objects[ItemIndex];

  if Obj is TdxForm then
    with TSelectFieldForm.CreateNew(nil) do
    try
      ShowObjectFields:=True;
    	if ShowForm(TdxForm(Obj), nil) = mrOk then
        Field.Text := FieldName;
    finally
      Free;
    end
  else if Obj is TReportData then
    with TSelectQFieldForm.CreateNew(nil) do
    try
      if ShowForm(TReportData(Obj)) = mrOk then
        Field.Text := FieldName;
    finally
      Free;
    end;
end;

procedure TTotalFm.FormShow(Sender: TObject);
begin
  FormList.SetFocus;
  SetControlState;
end;

procedure TTotalFm.FuncChange(Sender: TObject);
begin
  SetControlState;
end;

procedure TTotalFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('total');
end;

procedure TTotalFm.SetControlState;
begin
  FormList.Enabled := Func.ItemIndex >= 0;
  Field.Enabled := (Func.ItemIndex <> 2) and (FormList.ItemIndex >= 0);
  Filter.Enabled := (Func.ItemIndex <> 5) and (FormList.ItemIndex >= 0);
  ButtonPanel1.OkButton.Enabled := Func.ItemIndex >= 0;

  if Func.ItemIndex = 5 then Filter.Text := ''
  else if Func.ItemIndex = 2 then Field.Text := '';
end;

procedure TTotalFm.FillForms(aForm: TdxForm);
var
  i: Integer;
  Fm: TdxForm;
  SL: TStringListUtf8;
  C: TComponent;
  RD: TReportData;
begin
  FormList.Clear;
  SL := TStringListUtf8.Create;
  for i := 0 to aForm.ComponentCount - 1 do
  begin
    C := aForm.Components[i];
    if C is TdxGrid then
    begin
    	Fm := FormMan.FindForm(TdxGrid(C).Id);
      SL.AddObject(Fm.FormCaption, Fm);
    end
    else if C is TdxQueryGrid then
    begin
      RD := ReportMan.FindReport(TdxQueryGrid(C).Id);
      if not RD.IsEmpty then
	      SL.AddObject(RD.Name, RD);
    end;
  end;
  SL.Sort;
  FormList.Items := SL;
  SL.Free;
end;

procedure FillFormFields(Fm: TdxForm; L: TStrings);
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (not HasFId(C)) or (C is TdxDBImage) or (C is TdxFile) then Continue;
    L.Add(GetFieldName(C));
  end;
end;

(*procedure FillQueryFields(RD: TReportData; L: TStrings);
var
  //Src: TRpSource;
  i: Integer;
  {F: TRpField;
  CF: TRpCalcField;}
begin
  for i := 0 to RD.GetFieldCount - 1 do
    if RD.GetFieldVisible(i) then
      L.Add(RD.GetFieldName(i));
  {Src := RD.Sources[0]^;
	for i := 0 to Src.Fields.Count - 1 do
  begin
    F := Src.Fields[i]^;
    if F.Visible then
    	L.Add(F.Name);
  end;
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i]^;
    L.Add(CF.Name);
  end; }
end;   *)

{procedure TTotalFm.FillFields;
var
  Obj: TObject;
  SL: TStringListUtf8;
begin
  Field.Clear;
  with FormList do
	  Obj := Items.Objects[ItemIndex];
  SL := TStringListUtf8.Create;
  if Obj is TdxForm then
  	FillFormFields(TdxForm(Obj), SL)
  else if Obj is TReportData then
  	FillQueryFields(TReportData(Obj), SL);
  SL.Sort;
	Field.Items := SL;
  SL.Free;
end; }

function TTotalFm.ShowForm(aForm: TdxForm; var Res: String): Integer;
const
  Fn: array [0..6] of String = ('SUM', 'AVG', 'COUNT', 'MAX', 'MIN', 'MERGE', 'TAKE');
var
  S: String;
begin
  FForm := aForm;
  FillForms(aForm);
  Result := ShowModal;

  if Result <> mrOk then Exit;

  S := Fn[Func.ItemIndex];
  if (Filter.Text <> '') and (Func.ItemIndex < 6) then S := S + 'IF';
  S := S + '(' + '''' + FormList.Text + '''';
  if Func.ItemIndex <> 2 then S := S + ', ''' + Field.Text + '''';
  if Filter.Text <> '' then S := S + ', ''' +
  	StringReplace(Filter.Text, '''', '"', [rfReplaceall]) + ''''
  else if Func.ItemIndex = 5 then S := S + ', ''; '''
  else if Func.ItemIndex = 6 then S := S + ', ''''';
  S := S + ')';
  Res := S;
end;

end.

