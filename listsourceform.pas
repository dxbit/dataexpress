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
unit ListSourceForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Buttons, Menus, ExtCtrls, Spin, strconsts;

type

  { TListSourceFrm }

  TListSourceFrm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label2: TLabel;
    Obj: TListBox;
    Fields: TListBox;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    RowCnt: TSpinEdit;
    procedure FieldsDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ObjSelectionChange(Sender: TObject; User: boolean);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    { private declarations }
    FCbx: TComponent;
    procedure FillFieldList;
    procedure SelectForm(aId: Integer);
    procedure SelectField(aId: Integer);
  public
    { public declarations }
    function ShowForm(C: TComponent): Integer;
  end;

var
  ListSourceFrm: TListSourceFrm;

implementation

uses
  formmanager, dxctrls, LazUtf8, helpform, apputils;

{$R *.lfm}

{ TListSourceFrm }

procedure TListSourceFrm.FormCreate(Sender: TObject);
begin
  Caption := rsListSource;
  Label2.Caption := rsRowCountInList;
  MenuItem1.Caption := rsClear;
  SetMenuItemImage(MenuItem1, 'delete16');
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TListSourceFrm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('listsource');
end;

procedure TListSourceFrm.ObjSelectionChange(Sender: TObject; User: boolean
  );
begin
  FillFieldList;
end;

procedure TListSourceFrm.FieldsDblClick(Sender: TObject);
begin
  if Fields.ItemIndex >= 0 then
    ModalResult := mrOk;
end;

procedure TListSourceFrm.FormShow(Sender: TObject);
begin
  Obj.SetFocus;
end;

procedure TListSourceFrm.MenuItem1Click(Sender: TObject);
begin
  Obj.ItemIndex:=-1;
  Fields.Clear;
end;

procedure TListSourceFrm.FillFieldList;
var
  i: Integer;
  C: TComponent;

  procedure AddToList;
  var
    j: Integer;
    S: String;
  begin
    S := GetFieldName(C);
    for j := 0 to Fields.Count - 1 do
      if Utf8CompareText(Fields.Items[j], S) > 0 then
      begin
        Fields.Items.InsertObject(j, S, C);
        Exit;
      end;
    Fields.Items.AddObject(S, C);
  end;

begin
  Fields.Clear;
  i := Obj.ItemIndex;
  if i < 0 then Exit;
  with TdxForm(Obj.Items.Objects[i]) do
    for i := 0 to ComponentCount - 1 do
    begin
      C := Components[i];
      if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) or
        (C is TdxCounter) or (C is TdxCalcEdit) then
        AddToList;
    end;
end;

procedure TListSourceFrm.SelectForm(aId: Integer);
var
  i: Integer;
begin
  for i := 0 to Obj.Count - 1 do
    with TdxForm(Obj.Items.Objects[i]) do
      if Id = aId then
      begin
        Obj.ItemIndex := i;
        Break;
      end;
end;

procedure TListSourceFrm.SelectField(aId: Integer);
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to Fields.Count - 1 do
  begin
    C := TComponent(Fields.Items.Objects[i]);
    if GetId(C) = aId then
    begin
      Fields.ItemIndex := i;
      Break;
    end;
  end;
end;

function TListSourceFrm.ShowForm(C: TComponent): Integer;
var
  TId: Integer;
  i: Integer;
begin
  FCbx := C;
  Obj.Clear;
  FormMan.FormsToList(Obj.Items);
  Fields.Clear;
  TId := GetSourceTId(C);
  SelectForm(TId);
  SelectField(GetSourceFId(C));

  if C is TCustomComboBox then
  begin
	  RowCnt.Value:=TCustomComboBox(C).DropDownCount;
    Panel1.Height := 32;
  end
  else
  	Panel1.Height := 0;

  if ShowModal = mrOk then
  begin
    if Fields.ItemIndex >= 0 then
    begin
      SetSourceTId(C, TdxForm(Obj.Items.Objects[Obj.ItemIndex]).Id);
      SetSourceFId(C, GetId( TComponent(Fields.Items.Objects[Fields.ItemIndex]) ));
      if C is TdxComboBox then TdxComboBox(C).Style := csDropDown;
    end
    else
    begin
      SetSourceTId(C, 0);
      SetSourceFId(C, 0);
    end;
    if C is TCustomComboBox then
    	TCustomComboBox(C).DropDownCount:=RowCnt.Value;
  end;
  if (C is TdxLookupComboBox) and (GetSourceTId(C) <> TId) then
    with TdxLookupComboBox(C) do
    begin
      Filter := '';
      InsertedValues:='';
      ClearObjectFieldId(TdxForm(C.Owner), GetId(C), 0);
      SourceTable := 0;
      FillFilter := '';
      for i := 0 to FieldsTables.Count - 1 do
        FieldsTables[i] := '=' +FieldsTables.ValueFromIndex[i];
    end;
  Result := ModalResult;
end;

end.

