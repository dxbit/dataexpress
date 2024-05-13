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

unit ObjectFieldsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts, dxctrls;

type

  { TObjFieldsFm }

  TObjFieldsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Fields: TListBox;
    ObjList: TListBox;
    Label1: TLabel;
    procedure FieldsDblClick(Sender: TObject);
    procedure FieldsSelectionChange(Sender: TObject; User: boolean);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ObjListSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    FFm: TdxForm;
    procedure FillObjects;
    procedure FillFields;
    procedure SetControlState;
  public
    { public declarations }
    procedure ShowForm(Ctrl: TdxObjectField);
  end;

var
  ObjFieldsFm: TObjFieldsFm;

procedure ShowObjFieldsForm(Ctrl: TdxObjectField);

implementation

uses
  formmanager, helpmanager, mytypes, designerframe;

procedure ShowObjFieldsForm(Ctrl: TdxObjectField);
begin
  if ObjFieldsFm = nil then
  	ObjFieldsFm := TObjFieldsFm.Create(Application);
  ObjFieldsFm.ShowForm(Ctrl);
end;

{$R *.lfm}

{ TObjFieldsFm }

procedure TObjFieldsFm.FormCreate(Sender: TObject);
begin
  Caption := rsLinkedObject;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TObjFieldsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('linkobject');
end;

procedure TObjFieldsFm.FieldsDblClick(Sender: TObject);
begin
  if Fields.ItemIndex >= 0 then
    ModalResult := mrOk;
end;

procedure TObjFieldsFm.FieldsSelectionChange(Sender: TObject; User: boolean);
begin
  SetControlState;
end;

procedure TObjFieldsFm.ObjListSelectionChange(Sender: TObject; User: boolean
  );
begin
  FillFields;
  SetControlState;
end;

procedure TObjFieldsFm.FillObjects;
var
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  ObjList.Clear;
  SL := TStringListUtf8.Create;
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TdxLookupComboBox then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  ObjList.Items := SL;
  SL.Free;
end;

procedure TObjFieldsFm.FillFields;
var
  Obj: TdxLookupComboBox;
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  Fields.Clear;
  if ObjList.ItemIndex < 0 then Exit;
  Obj := TdxLookupComboBox(ObjList.Items.Objects[ObjList.ItemIndex]);
  if Obj.SourceTId = 0 then Exit;
  SL := TStringListUtf8.Create;
  Fm := FormMan.FindForm(Obj.SourceTId);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxDateEdit) or
      (C is TdxCalcEdit) or (C is TdxCheckBox) or
      ((C is TdxMemo) and (GetFieldSize(C) > 0)) or
      (C is TdxTimeEdit) or (C is TdxCounter) or (C is TdxLookupComboBox) or
      (C is TdxRecordId) then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Fields.Items := SL;
  SL.Free;
end;

procedure TObjFieldsFm.SetControlState;
begin
  ButtonPanel1.OkButton.Enabled:=Fields.ItemIndex >= 0;
end;

procedure TObjFieldsFm.ShowForm(Ctrl: TdxObjectField);
var
  ObjId, FId, TId: Integer;
  C: TComponent;
  Fm: TdxForm;
begin
  ObjId := Ctrl.ObjId;
  FId := Ctrl.FieldId;
  FFm := TdxForm(Ctrl.Owner);
  FillObjects;
  Fields.Clear;
  SetControlState;
  if ObjId > 0 then
  begin
    C := FindById(FFm, ObjId);
    if C <> nil then
    begin
      ObjList.ItemIndex := ObjList.Items.IndexOfObject(C);
      FillFields;
      if FId > 0 then
      begin
        TId := GetSourceTId(C);
        if TId > 0 then
        begin
          Fm := FormMan.FindForm(TId);
          C := FindById(Fm, FId);
          Fields.ItemIndex := Fields.Items.IndexOfObject(C);
        end;
      end;
    end;
  end;

  if ShowModal <> mrOk then Exit;

  with ObjList do
    ObjId := GetId(TComponent(Items.Objects[ItemIndex]));
  with Fields do
    FId := GetId(TComponent(Items.Objects[ItemIndex]));

  if Ctrl.FieldId <> FId then DesignFr.NeedAllCalcRecordSize := True;

  Ctrl.ObjId := ObjId;
  Ctrl.FieldId := FId;
end;

end.

