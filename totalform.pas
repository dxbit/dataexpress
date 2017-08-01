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
unit TotalForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, dxctrls, strconsts, DXReports;

type

  { TTotalFm }

  TTotalFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    FormList: TComboBox;
    Fields: TComboBox;
    Func: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormListSelect(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    procedure FillForms(aForm: TdxForm);
    procedure FillFields;
  public
    { public declarations }
    function ShowForm(aForm: TdxForm): String;
  end;

var
  TotalFm: TTotalFm;

implementation

uses
  formmanager, apputils, helpform, reportmanager, mytypes;

{$R *.lfm}

{ TTotalFm }

procedure TTotalFm.FormCreate(Sender: TObject);
begin
  Caption := rsInsertTotalFunc;
  Label1.Caption := rsTableOrQuery;
  Label2.Caption := rsField;
  Label3.Caption := rsFunction;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  Func.ITems.AddStrings([rsSum, rsAverage, rsCount, rsMaximum, rsMinimum]);
end;

procedure TTotalFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult <> mrOk then Exit;
  CanClose := False;
  if FormList.ItemIndex < 0 then
    ErrMsg(rsTableNotSelect)
  else if (Fields.ItemIndex < 0) and (Func.ItemIndex <> 2) then
    ErrMsg(rsFieldNotSel)
  else if Func.ItemIndex < 0 then
    ErrMsg(rsFuncNotSelect)
  else
    CanClose := True;
end;

procedure TTotalFm.FormListSelect(Sender: TObject);
begin
  FillFields;
end;

procedure TTotalFm.FormShow(Sender: TObject);
begin
  FormList.SetFocus;
end;

procedure TTotalFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('total');
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
      if RD.Sources.Count > 0 then
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
    if C is TdxCalcEdit then
      L.Add(GetFieldName(C));
  end;
end;

procedure FillQueryFields(RD: TReportData; L: TStrings);
var
  Src: TRpSource;
  i: Integer;
  F: TRpField;
  CF: TRpCalcField;
begin
  Src := RD.Sources[0]^;
	for i := 0 to Src.Fields.Count - 1 do
  begin
    F := Src.Fields[i]^;
    if F.Visible and (F.Tp = flNumber) then
    	L.Add(F.Name);
  end;
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i]^;
    L.Add(CF.Name);
  end;
end;

procedure TTotalFm.FillFields;
var
  Obj: TObject;
  SL: TStringListUtf8;
begin
  Fields.Clear;
  with FormList do
	  Obj := Items.Objects[ItemIndex];
  SL := TStringListUtf8.Create;
  if Obj is TdxForm then
  	FillFormFields(TdxForm(Obj), SL)
  else if Obj is TReportData then
  	FillQueryFields(TReportData(Obj), SL);
  SL.Sort;
	Fields.Items := SL;
  SL.Free;
end;

function TTotalFm.ShowForm(aForm: TdxForm): String;
const
  Fn: array [0..4] of String = ('SUM', 'AVG', 'COUNT', 'MAX', 'MIN');
begin
  Result := '';
  FillForms(aForm);
  Fields.Clear;
  Func.ItemIndex := -1;
  if ShowModal <> mrOk then Exit;
  if Func.ItemIndex <> 2 then
    Result := Fn[Func.ItemIndex] + '(''' + FormList.Items[FormList.ItemIndex] + ''', ''' +
      Fields.Items[Fields.ItemIndex] + ''')'
  else
    Result := Fn[Func.ItemIndex] + '(''' + FormList.Items[FormList.ItemIndex] + ''')';
end;

end.

