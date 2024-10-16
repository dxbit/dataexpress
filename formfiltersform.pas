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

unit FormFiltersForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Menus, ExtCtrls, Buttons, strconsts, dxctrls;

type

  { TFormFiltersFm }

  TFormFiltersFm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    Label1: TLabel;
    List: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    procedure ButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListSelectionChange(Sender: TObject; User: boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
  private
    FFm: TdxForm;
    FFilters: TStringList;
    FModified: Boolean;
    procedure LoadList;
    procedure SaveList;
    procedure EditFilter;
    procedure SetControlState;
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(Fm: TdxForm);
  end;

var
  FormFiltersFm: TFormFiltersFm;

procedure ShowFormFiltersForm(Fm: TdxForm);

implementation

uses
  filterform, apputils, helpmanager, myclasses;

procedure ShowFormFiltersForm(Fm: TdxForm);
begin
  if FormFiltersFm = nil then
  	FormFiltersFm := TFormFiltersFm.Create(Application);
  FormFiltersFm.ShowForm(Fm);
end;

{$R *.lfm}

{ TFormFiltersFm }

procedure TFormFiltersFm.MenuItem1Click(Sender: TObject);
var
  S: String;
begin
  S := InputBox(rsFilterName, rsEnterFilterName, '');
  if S <> '' then
  begin
    List.Items.Add(S);
    FFilters.Add('');
    List.ItemIndex := List.Count - 1;
    EditFilter;
    FModified := True;
  end;
end;

procedure TFormFiltersFm.MenuItem2Click(Sender: TObject);
var
  S: String;
begin
  S := InputBox(rsFilterName, rsEnterFilterName, List.Items[List.ItemIndex]);
  if S <> '' then
  begin
    List.Items[List.ItemIndex] := S;
    FModified := True;
  end;
end;

procedure TFormFiltersFm.MenuItem3Click(Sender: TObject);
var
  i: Integer;
begin
  if ConfirmDelete then
  begin
    i := List.ItemIndex;
    List.Items.Delete(i);
    FFilters.Delete(i);
    if i > List.Count - 1 then
      i := List.Count - 1;
    List.ItemIndex := i;
    SetControlState;
    FModified := True;
  end;
end;

procedure TFormFiltersFm.MenuItem4Click(Sender: TObject);
begin
  EditFilter;
end;

procedure TFormFiltersFm.MenuItem7Click(Sender: TObject);
begin
  with List do
  begin
    Items.Exchange(ItemIndex, ItemIndex - 1);
    FFilters.Exchange(ItemIndex, ItemIndex - 1);
    ItemIndex := ItemIndex - 1;
  end;
  SetControlState;
  FModified := True;
end;

procedure TFormFiltersFm.MenuItem8Click(Sender: TObject);
begin
  with List do
  begin
    Items.Exchange(ItemIndex, ItemIndex + 1);
    FFilters.Exchange(ItemIndex, ItemIndex + 1);
    ItemIndex := ItemIndex + 1;
  end;
  SetControlState;
  FModified := True;
end;

procedure TFormFiltersFm.MenuItem9Click(Sender: TObject);
var
  S, SS: String;
  i: Integer;
begin
  i := List.ItemIndex;
  S := List.Items[i];
  SS := FFilters[i];
  List.Items.Add(S);
  FFilters.Add(SS);
  List.ItemIndex := List.Count - 1;
  FModified := True;
end;

procedure TFormFiltersFm.ListDblClick(Sender: TObject);
begin
  if List.ItemIndex >= 0 then EditFilter;
end;

procedure TFormFiltersFm.ListSelectionChange(Sender: TObject; User: boolean);
begin
  SetControlState;
end;

procedure TFormFiltersFm.ButtonClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: MenuItem1.Click;
    1: MenuItem2.Click;
    2: MenuItem3.Click;
    3: MenuItem4.Click;
    4: MenuItem7.Click;
    5: MenuItem8.Click;
    6: MenuItem9.Click;
  end;
end;

procedure TFormFiltersFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
  begin
    if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TFormFiltersFm.FormCreate(Sender: TObject);
begin
  Caption := rsFilterPresets;
  MenuItem1.Caption := rsAppend;
  MenuItem2.Caption := rsRename;
  MenuItem3.Caption := rsDelete;
  MenuItem4.Caption := rsEdit;
  MenuItem7.Caption := rsMoveUp;
  MenuItem8.Caption := rsMoveDown;
  MenuItem9.Caption := rsCopy;
  SetupImageList(ImageList1, ['add16', 'delete16', 'edit16', 'copy16', 'up16', 'down16']);
  BitBtn1.Caption := rsAppend;
  BitBtn2.Caption := rsRename;
  BitBtn3.Caption := rsDelete;
  BitBtn4.Caption := rsEdit;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Label1.Caption := rsFiltersHint;
end;

procedure TFormFiltersFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('formfilters');
end;

procedure TFormFiltersFm.LoadList;
var
  i: Integer;
begin
  List.Clear;
  for i := 0 to FFm.Filters.Count - 1 do
  begin
    List.Items.Add(FFm.Filters.Names[i]);
    FFilters.Add(FFm.Filters.ValueFromIndex[i]);
  end;
end;

procedure TFormFiltersFm.SaveList;
var
  L: TStrings;
  i: Integer;
begin
  L := FFm.Filters;
  L.Clear;
  for i := 0 to List.Count - 1 do
    L.Add(List.Items[i] + '=' + FFilters[i]);
end;

procedure TFormFiltersFm.EditFilter;
var
  S: String;
  Flt: TFilterObject;
  FilterFm: TFilterFm;
begin
  FilterFm := TFilterFm.Create(nil);
  FilterFm.Form := FFm;
  S := FFilters[List.ItemIndex];
  Flt := TFilterObject.Create(FFm);
  Flt.Load(S);
  if FilterFm.ShowForm(Flt) = mrOk then
  begin
    Flt.Save(S);
    FFilters[List.ItemIndex] := S;
    FModified := True;
  end;
  Flt.Free;
  FilterFm.Free;
end;

procedure TFormFiltersFm.SetControlState;
var
  b: Boolean;
begin
  b := List.ItemIndex >= 0;
  MenuItem2.Enabled := b;
  MenuItem3.Enabled := b;
  MenuItem4.Enabled := b;
  MenuItem7.Enabled:=List.ItemIndex > 0;
  MenuItem8.Enabled := b and (List.ItemIndex < List.Count - 1);
  MenuItem9.Enabled := b;
  BitBtn2.Enabled := b;
  BitBtn3.Enabled := b;
  BitBtn4.Enabled := b;
  BitBtn5.Enabled := MenuItem7.Enabled;
  BitBtn6.Enabled := MenuItem8.Enabled;
  BitBtn7.Enabled := b;
end;

procedure TFormFiltersFm.ShowForm(Fm: TdxForm);
begin
  FFm := Fm;
  FFilters := TStringList.Create;
  LoadList;
  SetControlState;
  FModified := False;
  if ShowModal = mrOk then
    SaveList;
  FFilters.Free;
end;

end.

