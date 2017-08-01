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
unit MergeProjectsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, StdCtrls, CheckLst, ExtCtrls, DBGrids, formmanager, DxCtrls,
  reportmanager, dxreports, strconsts, scriptmanager;

type

  { TMergeProjectsFm }

  TMergeProjectsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    FormList: TCheckListBox;
    DepForms: TCheckListBox;
    DepReps: TCheckListBox;
    SrcForms: TCheckListBox;
    RepList: TCheckListBox;
    DepSources: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure DepFormsClickCheck(Sender: TObject);
    procedure DepRepsClickCheck(Sender: TObject);
    procedure DepSourcesClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormListClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure RepListClick(Sender: TObject);
    procedure RepListClickCheck(Sender: TObject);
    procedure SrcFormsClickCheck(Sender: TObject);
  private
    { private declarations }
    FFmMan: TFormManager;
    FRpMan: TReportManager;
    FScrMan: TScriptManager;
    procedure FillSources(aRD: TReportData);
    procedure FillDependsReports(aFm: TdxForm);
    procedure FillDependsForms(aFm: TdxForm);
    procedure FillSourceForms(aFm: TdxForm);
    procedure ClearBrokenLinksInReports(aForms, aReports: TList);
    procedure ClearBrokenLinks(aForms, aReports: TList);
    procedure ChangeQueryId(aForms: TList; OldId, NewId: Integer);
    procedure ChangeFieldIdInReports(aReports: TList; OldId, NewId: Integer);
    procedure ChangeFieldId(aForm: TdxForm; OldId, NewId: Integer);
    procedure ChangeAllFieldId(aForms: TList; OldId, NewId: Integer);
    procedure ChangeFormIdInReports(aReports: TList; OldId, NewId: Integer);
    procedure ChangeFormId(aForms: TList; OldId, NewId: Integer);
    procedure SelReportsToList(aForms, L: TList);
    procedure SelFormsToList(L: TList);
    procedure ImportForms;
  public
    { public declarations }
    function ShowForm(const aFileName: String): Integer;
  end;

var
  MergeProjectsFm: TMergeProjectsFm;

implementation

uses
  Zipper, dbengine, apputils, dxactions, sqlgen, helpform;

{$R *.lfm}

function BnGetFormId(Bn: TdxButton): Integer;
var
  Act: TBasicAction;
begin
  Result := 0;
  Act := CreateAction(Bn.ActionType);
  if Act = nil then Exit;
  Act.Load(Bn.ActionProps);
  if Act is TGotoFormAction then
    Result := TGotoFormAction(Act).FormId
  else if Act is TMassCalcAction then
    Result := TMassCalcAction(Act).FormId;
  Act.Free;
end;

function BnGetReportId(Bn: TdxButton): Integer;
var
  Act: TBasicAction;
begin
  Result := 0;
  Act := CreateAction(Bn.ActionType);
  if Act = nil then Exit;
  Act.Load(Bn.ActionProps);
  if Act is TOpenReportAction then
    Result := TOpenReportAction(Act).RpId;
  Act.Free;
end;

procedure BnChangeFormId(Bn: TdxButton; OldId, NewId: Integer);
begin
  Bn.ActionProps := StringReplace(Bn.ActionProps, ' formid="' + IntToStr(OldId) + '"',
    ' formid="' + IntToStr(NewId) + '"', []);
end;

procedure BnChangeFieldId(Bn: TdxButton; OldId, NewId: Integer);
begin
  Bn.ActionProps := StringReplace(Bn.ActionProps, ' fieldid="' + IntToStr(OldId) + '"',
    ' fieldid="' + IntToStr(NewId) + '"', []);
end;

procedure BnChangeRpId(Bn: TdxButton; OldId, NewId: Integer);
begin
  Bn.ActionProps := StringReplace(Bn.ActionProps, ' rpid="' + IntToStr(OldId) + '"',
    ' rpid="' + IntToStr(NewId) + '"', []);
end;

function QryIdExists(RD: TReportData; aId: Integer; IsField: Boolean): Boolean;
var
  i, j, N: Integer;
  pSr: PRpSource;

  function _ProcessField(pF: PRpField): Boolean;
  var
    N: Integer;
  begin
    Result := False;
    if not IsField then
    begin
      if TryStrToInt(pF^.TId, N) and (N = aId) then Exit(True)
    end
    else if TryStrToInt(pF^.FId, N) and (N = aId) then Exit(True);
    if pF^.Src <> nil then
      if _ProcessField(pF^.Src) then Exit(True);
  end;

begin
  Result := False;
  for i := 0 to RD.Sources.Count - 1 do
  begin
    pSr := RD.Sources[i];
    if not IsField then
      if TryStrToInt(pSr^.Id, N) and (N = aId) then Exit(True)
      else if TryStrToInt(pSr^.TId, N) and (N = aId) then Exit(True);

    for j := 0 to pSr^.Fields.Count - 1 do
      if _ProcessField(PRpField(pSr^.Fields[j])) then Exit(True);
  end;
end;

procedure QryIdReplace(RD: TReportData; aId, aNewId: Integer; IsField: Boolean);
var
  i, j, N: Integer;
  pSr: PRpSource;

  procedure _ProcessField(pF: PRpField);
  var
    N: Integer;
  begin
    if not IsField then
    begin
      if TryStrToInt(pF^.TId, N) and (N = aId) then pF^.TId := IntToStr(aNewId);
    end
    else if TryStrToInt(pF^.FId, N) and (N = aId) then pF^.FId := IntToStr(aNewId);
    if pF^.Src <> nil then
      _ProcessField(pF^.Src);
  end;

begin
  for i := 0 to RD.Sources.Count - 1 do
  begin
    pSr := RD.Sources[i];
    if not IsField then
      if TryStrToInt(pSr^.Id, N) and (N = aId) then pSr^.Id := IntToStr(aNewId)
      else if TryStrToInt(pSr^.TId, N) and (N = aId) then pSr^.TId := IntToStr(aNewId);

    for j := 0 to pSr^.Fields.Count - 1 do
      _ProcessField(PRpField(pSr^.Fields[j]));
  end;
end;

{ TMergeProjectsFm }

procedure TMergeProjectsFm.FormCreate(Sender: TObject);
begin
  FFmMan := TFormManager.Create;
  FRpMan := TReportManager.Create;
  FScrMan := TScriptManager.Create;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Caption := rsMergeProjects;
  TabSheet1.Caption := rsForms;
  TabSheet2.Caption := rsReports;
  Label3.Caption := rsFormLinkTo;
  Label1.Caption := rsFormsLinkToSelFm;
  Label2.Caption := rsReportsLinkToFm;
  Label4.Caption := rsReportRefersTo;
end;

procedure TMergeProjectsFm.DepFormsClickCheck(Sender: TObject);
var
  Fm: TdxForm;
  i: Integer;
begin
  with DepForms do
  begin
    Fm := TdxForm(Items.Objects[ItemIndex]);
    i := FormList.Items.IndexOfObject(Fm);
    FormList.Checked[i] := Checked[ItemIndex];
  end;
end;

procedure TMergeProjectsFm.DepRepsClickCheck(Sender: TObject);
var
  i: Integer;
  RD: TdxForm;
begin
  with DepReps do
  begin
    RD := TdxForm(Items.Objects[ItemIndex]);
    i := RepList.Items.IndexOfObject(RD);
    RepList.Checked[i] := Checked[ItemIndex];
  end;
end;

procedure TMergeProjectsFm.DepSourcesClickCheck(Sender: TObject);
var
  Fm: TdxForm;
  i: Integer;
begin
  with DepSources do
  begin
    Fm := TdxForm(Items.Objects[ItemIndex]);
    i := FormList.Items.IndexOfObject(Fm);
    FormList.Checked[i] := Checked[ItemIndex];
  end;
end;

procedure TMergeProjectsFm.FormDestroy(Sender: TObject);
begin
  FScrMan.Free;
  FRpMan.Free;
  FFmMan.Free;
end;

procedure TMergeProjectsFm.FormListClick(Sender: TObject);
var
  Fm: TdxForm;
begin
  with FormList do
  begin
    if ItemIndex >= 0 then
    begin
      Fm := TdxForm(Items.Objects[ItemIndex]);
      FillSourceForms(Fm);
      FillDependsForms(Fm);
      FillDependsReports(Fm);
    end;
  end;
end;

procedure TMergeProjectsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('mergeprojects');
end;

procedure TMergeProjectsFm.RepListClick(Sender: TObject);
var
  RD: TReportData;
begin
  with RepList do
    if ItemIndex >= 0 then
    begin
      RD := TReportData(Items.Objects[ItemIndex]);
      FillSources(RD);
    end;
end;

procedure TMergeProjectsFm.RepListClickCheck(Sender: TObject);
begin
  FormListClick(FormList);
end;

procedure TMergeProjectsFm.SrcFormsClickCheck(Sender: TObject);
var
  Fm: TdxForm;
  i: Integer;
begin
  with SrcForms do
  begin
    Fm := TdxForm(Items.Objects[ItemIndex]);
    i := FormList.Items.IndexOfObject(Fm);
    FormList.Checked[i] := Checked[ItemIndex];
  end;
end;

procedure TMergeProjectsFm.FillSources(aRD: TReportData);
var
  i: Integer;
  Fm: TdxForm;

  procedure _AddSource(aFm: TdxForm);
  var
    idx, idx2: Integer;
  begin
    if aFm.PId > 0 then aFm := FFmMan.FindForm(aFm.PId);
    if DepSources.Items.IndexOfObject(aFm) >= 0 then Exit;
    idx := DepSources.Items.AddObject(aFm.FormCaption, aFm);
    idx2 := FormList.Items.IndexOfObject(aFm);
    DepSources.Checked[idx] := FormList.Checked[idx2];
  end;

begin
  DepSources.Items.Clear;
  for i := 0 to FFmMan.FormCount - 1 do
  begin
    Fm := FFmMan.Forms[i];
    if QryIdExists(aRD, Fm.Id, False) then
      _AddSource(Fm);
  end;
end;

procedure TMergeProjectsFm.FillDependsReports(aFm: TdxForm);
var
  i: Integer;
  RD: TReportData;

  procedure _AddRep(aRD: TReportData);
  var
    idx, idx2: Integer;
  begin
    idx := DepReps.Items.AddObject(aRD.Name, aRD);
    idx2 := RepList.Items.IndexOfObject(aRD);
    DepReps.Checked[idx] := RepList.Checked[idx2];
  end;

begin
  DepReps.Items.Clear;
  for i := 0 to FRpMan.ReportCount - 1 do
  begin
    RD := FRpMan.Reports[i];
    if RD.Kind <> rkReport then Continue;
    if QryIdExists(RD, aFm.Id, False) then
      _AddRep(RD);
  end;
end;

procedure TMergeProjectsFm.FillDependsForms(aFm: TdxForm);
var
  L: TStrings;
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
  RD: TReportData;

  procedure _AddForm(aForm: TdxForm);
  var
    idx, idx2: Integer;
  begin
    if aForm.PId > 0 then
      aForm := FFmMan.FindForm(aForm.PId);
    if (aForm = aFm) or (L.IndexOfObject(aForm) >= 0) then Exit;
    idx := L.AddObject(aForm.FormCaption, Fm);
    idx2 := FormList.Items.IndexOfObject(aForm);
    DepForms.Checked[idx] := FormList.Checked[idx2];
  end;

begin
  L := DepForms.Items;
  L.Clear;
  for i := 0 to FFmMan.FormCount - 1 do
  begin
    Fm := FFmMan.Forms[i];
    if Fm = aFm then Continue;
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if (C is TCustomComboBox) or (C is TdxMemo) then
      begin
        if GetSourceTId(C) = aFm.Id then
        begin
          _AddForm(Fm);
          Break;
        end;
      end
      else if C is TdxQueryGrid then
      begin
        RD := FRpMan.FindReport(TdxQueryGrid(C).Id);
        if QryIdExists(RD, aFm.Id, False) then
        begin
          _AddForm(Fm);
          Break;
        end;
      end
      else if C is TdxButton then
      begin
        if BnGetFormId(TdxButton(C)) = aFm.Id then
        begin
          _AddForm(Fm);
          Break;
        end;
      end;
    end;
  end;
end;

procedure TMergeProjectsFm.FillSourceForms(aFm: TdxForm);
var
  L: TStrings;

  procedure _AddForm(FmId: Integer);
  var
    idx, idx2: Integer;
    Fm: TdxForm;
  begin
    Fm := FFmMan.FindForm(FmId);
    if (Fm = nil) or (Fm.PId > 0) or (L.IndexOfObject(Fm) >= 0) or (Fm = aFm) then Exit;

    idx := L.AddObject(Fm.FormCaption, Fm);
    idx2 := FormList.Items.IndexOfObject(Fm);
    SrcForms.Checked[idx] := FormList.Checked[idx2];
  end;

  procedure _ProcessField(pF: PRpField);
  var
    N: integer;
  begin
    if TryStrToInt(pF^.TId, N) then _AddForm(N);
    if pF^.Src <> nil then _ProcessField(pF^.Src);
  end;

  procedure _Fill(aForm: TdxForm);
  var
    i, j, z, N: Integer;
    C: TComponent;
    RD: TReportData;
    pSr: PRpSource;
  begin
    for i := 0 to aForm.ComponentCount - 1 do
    begin
      C := aForm.Components[i];
      if (C is TCustomComboBox) or (C is TdxMemo) then
      begin
        _AddForm(GetSourceTId(C));
      end
      else if C is TdxQueryGrid then
      begin
        RD := FRpMan.FindReport(TdxQueryGrid(C).Id);
        for j := 0 to RD.Sources.Count - 1  do
        begin
          pSr := RD.Sources[j];
          if TryStrToInt(pSr^.Id, N) then
            _AddForm(N);
          for z := 0 to pSr^.Fields.Count - 1 do
            _ProcessField(pSr^.Fields[z]);
        end;
      end
      else if C is TdxButton then
        _AddForm(BnGetFormId(TdxButton(C)))
      else if C is TdxGrid then
      begin
        _Fill(FFmMan.FindForm(TdxGrid(C).Id));
      end;
    end;
  end;

begin
  L := SrcForms.Items;
  L.Clear;
  _Fill(aFm);
end;

procedure TMergeProjectsFm.ClearBrokenLinksInReports(aForms, aReports: TList);
var
  i, j: Integer;
  RD: TReportData;
  pSr: PRpSource;

  function IsExistsForm(const FmId: String): Boolean;
  var
    Fm: TdxForm;
    N: integer;
  begin
    if not TryStrToInt(FmId, N) then Exit(True);
    Fm := FFmMan.FindForm(N);
    Result := aForms.IndexOf(Fm) >= 0;
  end;

  function _ProcessField(pFl: PRpField): Boolean;
  begin
    Result := True;
    if not IsExistsForm(pFl^.TId) then Exit(False);
    if pFl^.Src <> nil then
      Result := _ProcessField(pFl^.Src);
  end;

  function ProcessFields(FL: TRpFieldList): Boolean;
  var
    z: Integer;
    pF: PRpField;
  begin
    Result := True;
    for z := 0 to pSr^.Fields.Count - 1 do
    begin
      pF := pSr^.Fields[z];
      if not _ProcessField(pF) then Exit(False);
    end;
  end;

begin
  for i := 0 to aReports.Count - 1 do
  begin
    RD := TReportData(aReports[i]);
    for j := 0 to RD.Sources.Count - 1 do
    begin
      pSr := RD.Sources[j];
      if (not IsExistsForm(pSr^.Id)) or (not ProcessFields(pSr^.Fields)) then
      begin
        RD.Clear;
        Break;
      end
    end;
  end;
end;

procedure TMergeProjectsFm.ClearBrokenLinks(aForms, aReports: TList);
var
  i, j, z: Integer;
  Fm, SrcFm: TdxForm;
  C, CC: TComponent;
  SrcRD: TReportData;
begin
  for i := 0 to aForms.Count - 1 do
  begin
    Fm := TdxForm(aForms[i]);
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if (C is TCustomComboBox) or (C is TdxMemo) then
      begin
        SrcFm := FFmMan.FindForm(GetSourceTId(C));
        if aForms.IndexOf(SrcFm) < 0 then
        begin
          SetSourceTId(C, 0);
          SetSourceFId(C, 0);
          if C is TdxLookupComboBox then
          begin
            with TdxLookupComboBox(C) do
            begin
              if Fm.ShopData.ObjId = Id then Fm.ShopData.Clear;
              InsertedValues:='';
              SourceTable:=0;
              DestTable:=0;
              FieldsTables.Clear;
              FillFilter:='';
              PromptFillTable:=False;
              ClearTableBeforeFill:=False;
            end;
            // Поля объекта
            for z := 0 to Fm.ComponentCount - 1 do
            begin
              CC := Fm.Components[z];
              if CC is TdxObjectField then
                with TdxObjectField(CC) do
                  if ObjId = GetId(C) then
                  begin
                    ObjId := 0;
                    FieldId := 0;
                  end;
            end;
          end;
        end;
      end
      else if C is TdxButton then
      begin
        SrcFm := FFmMan.FindForm(BnGetFormId(TdxButton(C)));
        if SrcFm <> nil then
        begin
          if aForms.IndexOf(SrcFm) < 0 then
          begin
            TdxButton(C).ActionType:=actNone;
            TdxButton(C).ActionProps := '';
          end;
        end;
        SrcRD := FRpMan.FindReport(BnGetReportId(TdxButton(C)));
        if SrcRD <> nil then
        begin
          if aReports.IndexOf(SrcRD) < 0 then
          begin
            TdxButton(C).ActionType:=actNone;
            TdxButton(C).ActionProps := '';
          end;
        end;
      end;
    end;
  end;
end;

procedure TMergeProjectsFm.ChangeQueryId(aForms: TList; OldId, NewId: Integer);
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
begin
  for i := 0 to aForms.Count - 1 do
  begin
    Fm := TdxForm(aForms[i]);
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxQueryGrid then
      begin
        with TdxQueryGrid(C) do
          if Id = OldId then Id := NewId;
      end
      else if C is TdxButton then
      begin
        if BnGetReportId(TdxButton(C)) = OldId then
          BnChangeRpId(TdxButton(C), OldId, NewId);
      end;
    end;
  end;
end;

procedure TMergeProjectsFm.ChangeFieldIdInReports(aReports: TList; OldId,
  NewId: Integer);
var
  i: Integer;
  RD: TReportData;
begin
  for i := 0 to aReports.Count - 1 do
  begin
    RD := TReportData(aReports[i]);
    QryIdReplace(RD, OldId, NewId, True);
  end;
end;

procedure TMergeProjectsFm.ChangeFieldId(aForm: TdxForm; OldId, NewId: Integer);
var
  i, j, p, N: Integer;
  C: TComponent;
  SL: TStringList;
  S, S2, S3: String;
  Col: TColumn;
begin
  SL := TStringList.Create;
  if aForm.ParentField = OldId then aForm.ParentField:=NewId;
  if aForm.GroupField = OldId then aForm.GroupField := NewId;
  // Ссылки на поля в колонках
  for i := 0 to aForm.Grid.Columns.Count - 1 do
  begin
    Col := aForm.Grid.Columns[i];
    if Col.Tag = OldId then
    begin
      Col.Tag := NewId;
      C := FindById(aForm, OldId);
      Col.FieldName := FieldStr(NewId);
      if C is TdxLookupComboBox then
        Col.FieldName := Col.FieldName + 'l';
    end;
  end;
  // Шопинг
  with aForm.ShopData do
  begin
    if ObjId = OldId then ObjId := NewId;
    if PriceFId = OldId then PriceFId := NewId;
    if PriceObjFId = OldId then PriceObjFId := NewId;
    if QttyFId = OldId then QttyFId := NewId;
  end;

  for i := 0 to aForm.ComponentCount - 1 do
  begin
    C := aForm.Components[i];
    if C is TdxLookupComboBox then
      with TdxLookupComboBox(C) do
      begin
        // источник списка
        if SourceFId = OldId then SourceFId := NewId;
        // Заполнить таблицу
        for j := 0 to FieldsTables.Count - 1 do
        begin
          if TryStrToInt(FieldsTables.Names[j], N) and (N = OldId) then
            FieldsTables[j] := IntToStr(NewId) + '=' + FieldsTables.ValueFromIndex[j]
          else if TryStrToInt(FieldsTables.ValueFromIndex[j], N) and (N = OldId) then
            FieldsTables.ValueFromIndex[j] := IntToStr(NewId);
        end;
        // Вставить значения
        SplitStr(InsertedValues, '|', SL);
        InsertedValues := '';
        for j := 0 to SL.Count - 1 do
        begin
          S := SL[j];
          p := Pos(';', S);
          S2 := Copy(S, 1, p - 1);
          S3 := Copy(S, p + 1, 255);
          if TryStrToInt(S2, N) and (N = OldId) then S2 := IntToStr(NewId)
          else if TryStrToInt(S3, N) and (N = OldId) then S3 := IntToStr(NewId);
          S := S2 + ';' + S3;
          InsertedValues := InsertedValues + S;
          if j < SL.Count - 1 then
            InsertedValues := InsertedValues + '|';
        end;
      end
    // Источник списка
    else if (C is TdxComboBox) or (C is TdxMemo) then
    begin
      if GetSourceFId(C) = OldId then SetSourceFId(C, NewId);
    end
    else if C is TdxObjectField then
      // Ссылки на объект и поля объекта
      with TdxObjectField(C) do
      begin
        if ObjId = OldId then ObjId := NewId
        else if FieldId = OldId then FieldId := NewId;
      end
    // Команды кнопки
    else if C is TdxButton then
      BnChangeFieldId(TdxButton(C), OldId, NewId);
  end;
  SL.Free;
end;

procedure TMergeProjectsFm.ChangeAllFieldId(aForms: TList; OldId, NewId: Integer
  );
var
  i: Integer;
begin
  for i := 0 to aForms.Count - 1 do
    ChangeFieldId(TdxForm(aForms[i]), OldId, NewId);
end;

procedure TMergeProjectsFm.ChangeFormIdInReports(aReports: TList; OldId,
  NewId: Integer);
var
  i: Integer;
  RD: TReportData;
begin
  for i := 0 to aReports.Count - 1 do
  begin
    RD := TReportData(aReports[i]);
    QryIdReplace(RD, OldId, NewId, False);
  end;
end;

procedure TMergeProjectsFm.ChangeFormId(aForms: TList; OldId, NewId: Integer);
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
begin
  for i := 0 to aForms.Count - 1 do
  begin
    Fm := TdxForm(aForms[i]);
    //if Fm.Id = OldId then Continue;
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      // Заполнить таблицу, источник списка
      if C is TdxLookupComboBox then
        with TdxLookupComboBox(C) do
        begin
          if SourceTable = OldId then SourceTable := NewId
          else if DestTable = OldId then DestTable := NewId;
          if SourceTId = OldId then SourceTId := NewId;
        end
      // Источник списка
      else if (C is TdxComboBox) or (C is TdxMemo) then
      begin
        if GetSourceTId(C) = OldId then SetSourceTId(C, NewId);
      end
      // Таблица ссылается на подчиненную форму
      else if C is TdxGrid then
      begin
        if GetId(C) = OldId then SetId(C, NewId);
      end
      // Команды кнопки
      else if C is TdxButton then
        with TdxButton(C) do
          BnChangeFormId(TdxButton(C), OldId, NewId)
    end;
    // Подчиненная форма ссылается на родительскую
    if Fm.PId = OldId then Fm.PId := NewId;
  end;
end;

// В том числе и запросы.
procedure TMergeProjectsFm.SelReportsToList(aForms, L: TList);
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
  RD: TReportData;
begin
  for i := 0 to aForms.Count - 1 do
  begin
    Fm := TdxForm(aForms[i]);
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxQueryGrid then
      begin
        RD := FRpMan.FindReport(TdxQueryGrid(C).Id);
        L.Add(RD);
      end;
    end;
  end;
  for i := 0 to RepList.Count - 1 do
  begin
    if RepList.Checked[i] then
      L.Add(RepList.Items.Objects[i]);
  end;
end;

procedure TMergeProjectsFm.SelFormsToList(L: TList);
var
  i, n: Integer;
  Fm, PFm: TdxForm;
begin
  for i := 0 to FFmMan.FormCount - 1 do
  begin
    Fm := FFmMan.Forms[i];
    PFm := Fm;
    // Для подчиненных форм смотрим выбрана ли родительская форма
    if Fm.PId > 0 then
      PFm := FFmMan.FindForm(Fm.PId);
    n := FormList.Items.IndexOfObject(PFm);
    if FormList.Checked[n] then
      L.Add(Fm);
  end;
end;

function FormSortProc(Item1, Item2: Pointer): Integer;
var
  id1, id2: Integer;
begin
  id1 := TdxForm(Item1).Id;
  id2 := TdxForm(Item2).Id;
  Result := id1 - id2;
end;

function ReportSortProc(Item1, Item2: Pointer): Integer;
var
  id1, id2: Integer;
begin
  id1 := TReportData(Item1).Id;
  id2 := TReportData(Item2).Id;
  Result := id1 - id2;
end;

function CompSortProc(Item1, Item2: Pointer): Integer;
var
  id1, id2: Integer;
begin
  id1 := GetId(TComponent(Item1));
  id2 := GetId(TComponent(Item2));
  Result := id1 - id2;
end;

// Для правильной замены ID сортируем все по ID и обрабатываем, начиная с большего.
// Это сделано для того, чтобы ID не пересекались. Может получится так, что
// старый и новый ID будут совпадать и тогда новый ID снова будет изменен, что
// приведет к сбою.
procedure TMergeProjectsFm.ImportForms;
var
  L, RL, CL: TList;
  CurTId, MaxTId, NewTId, CurFId, MaxFId, NewFId, CurRId, MaxRId, NewRId: Integer;
  i, j, OldTId: Integer;
  Fm: TdxForm;
  C: TComponent;
  RD: TReportData;
  RenForms, RenReps: String;
  SD, DestSD: TScriptData;
begin
  L := TList.Create;
  RL := TList.Create;
  CL := TList.Create;
  SelFormsToList(L);
  L.Sort(@FormSortProc);
  SelReportsToList(L, RL);
  RL.Sort(@ReportSortProc);
  CurTId := DBase.GetCurrentId('gen_tid');
  MaxTId := CurTId;
  CurFId := DBase.GetCurrentId('gen_fid');
  MaxFId := CurFId;
  CurRId := DBase.GetCurrentId('gen_rid');
  MaxRId := CurRId;

  for i := L.Count - 1 downto 0 do
  begin
    Fm := TdxForm(L[i]);
    OldTId := Fm.Id;
    NewTId := CurTId + Fm.Id;
    if NewTId > MaxTId then MaxTId := NewTId;
    ChangeFormId(L, Fm.Id, NewTId);
    ChangeFormIdInReports(RL, Fm.Id, NewTId);
    Fm.Id := NewTId;
    if FormMan.FindFormByComponentName(Fm.Name) <> nil then
      Fm.Name := Fm.Name + IntToStr(Fm.Id);

    SD := FScrMan.FindScript(OldTId);
    if SD <> nil then
    begin
      DestSD := ScriptMan.AddScript(NewTId, '', SD.Source);
      DestSD.Kind := skForm;
      DestSD.SourceData.LoadFromString(SD.SourceData.SaveToString);
    end;
  end;

  for i := L.Count - 1 downto 0 do
  begin
    Fm := TdxForm(L[i]);
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if not HasFId(C) then Continue;
      CL.Add(C);
    end;
  end;

  CL.Sort(@CompSortProc);
  for j := CL.Count - 1 downto 0 do
  begin
    C := TComponent(CL[j]);
    NewFId := CurFId + GetId(C);
    if NewFId > MaxFId then MaxFId := NewFId;
    ChangeAllFieldId(L, GetId(C), NewFId);
    ChangeFieldIdInReports(RL, GetId(C), NewFId);
    SetId(C, NewFId);
  end;

  for i := 0 to RL.Count - 1 do
  begin
    RD := TReportData(RL[i]);
    NewRId := CurRId + RD.Id;
    ChangeQueryId(L, RD.Id, NewRId);
    RD.Id:=NewRId;
    if NewRId > MaxRId then MaxRId := NewRId;
  end;

  ClearBrokenLinks(L, RL);
  ClearBrokenLinksInReports(L, RL);

  DBase.ChangeId('gen_tid', MaxTId);
  DBase.ChangeId('gen_fid', MaxFId);
  DBase.ChangeId('gen_rid', MaxRId);

  RenForms := '';
  for i := 0 to L.Count - 1 do
  begin
    Fm := TdxForm(L[i]);
    if FormMan.FindFormByName(Fm.FormCaption) <> nil then
    begin
      Fm.FormCaption := Fm.FormCaption + IntToStr(Fm.Id);
      RenForms := RenForms + Fm.FormCaption + LineEnding;
    end;
    FormMan.AddCopyForm(Fm);
  end;
  RenReps := '';
  for i := 0 to RL.Count - 1 do
  begin
    RD := TReportData(RL[i]);
    if ReportMan.FindByName(RD.Name) <> nil then
    begin
      RD.Name := RD.Name + IntToStr(RD.Id);
      RenReps := RenReps + RD.Name + LineEnding;
    end;
    ReportMan.AddCopyReport(RD);
  end;

  CL.Free;
  RL.Free;
  L.Free;

  if RenForms <> '' then RenForms := rsForms + ':' + LineEnding + RenForms;
  if RenReps <> '' then RenReps := rsReports + ':' + LineEnding + RenReps;
  if (RenForms <> '') and (RenReps <> '') then RenForms := RenForms + LineEnding;

  if (RenForms <> '') or (RenReps <> '') then
    MessageDlg(rsWarning, rsMergePrjRenameMsg + LineEnding + LineEnding + RenForms + RenReps,
      mtInformation, [mbOk], 0);
end;

function TMergeProjectsFm.ShowForm(const aFileName: String): Integer;
var
  TempDir: String;
begin
  TempDir := GetTempDir + 'dx' + IntToStr(Random(1000000)) + DirectorySeparator;

  with TUnZipper.Create do
  try
    FileName:=aFileName;
    OutputPath:=TempDir;
    UnZipAllFiles;
  finally
    Free;
  end;

  FFmMan.LoadFromDir(TempDir);
  FFmMan.FormsToList(FormList.Items);
  FRpMan.LoadFromDir(TempDir);
  FRpMan.GetReports(RepList.Items);
  FScrMan.LoadFromDir(TempDir);

  Result := ShowModal;
  if Result = mrOk then
    ImportForms;

  DeleteDirectory(TempDir, False);
end;

end.

