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

unit GridColsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  ButtonPanel, strconsts, Grids, Menus, StdCtrls, ExtCtrls, Buttons, ComCtrls,
  dxctrls;

type

  { TGridColsFm }

  TGridColsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    List: TCheckListBox;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
  private
    { private declarations }
    procedure SetControlState;
  public
    { public declarations }
    function ShowForm(aCols: TGridColumns; aForm: TdxForm): Integer;
  end;

var
  GridColsFm: TGridColsFm;

function ShowGridColsForm(aCols: TGridColumns; aForm: TdxForm): Integer;

implementation

function ShowGridColsForm(aCols: TGridColumns; aForm: TdxForm): Integer;
begin
  if GridColsFm = nil then
  	GridColsFm := TGridColsFm.Create(Application);
  Result := GridColsFm.ShowForm(aCols, aForm);
end;

{$R *.lfm}

{ TGridColsFm }

procedure TGridColsFm.FormCreate(Sender: TObject);
begin
  Caption := rsOrderVisibleColumns;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ImageList1.AddLazarusResource('checkall16');
  ImageList1.AddLazarusResource('uncheckall16');
  ImageList1.AddLazarusResource('up16');
  ImageList1.AddLazarusResource('down16');
  ToolButton1.Caption := rsCheckAll;
  ToolButton2.Caption := rsUncheckAll;
  ToolButton3.Caption := rsMoveUp;
  ToolButton4.Caption := rsMoveDown;
end;

procedure TGridColsFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
  if List.Count > 0 then List.ItemIndex := 0;
  SetControlState;
end;

procedure TGridColsFm.ListClick(Sender: TObject);
begin
  SetControlState;
end;

procedure TGridColsFm.ToolButton1Click(Sender: TObject);
begin
  List.CheckAll(cbChecked);
end;

procedure TGridColsFm.ToolButton2Click(Sender: TObject);
begin
  List.CheckAll(cbUnchecked);
end;

procedure TGridColsFm.ToolButton3Click(Sender: TObject);
var
  Tmp: Boolean;
begin
  with List do
	begin
    Tmp := Checked[ItemIndex];
    Checked[ItemIndex] := Checked[ItemIndex - 1];
    Checked[ItemIndex - 1] := Tmp;

    Items.Exchange(ItemIndex, ItemIndex - 1);
    ItemIndex := ItemIndex - 1;
  end;
  SetControlState;
end;

procedure TGridColsFm.ToolButton4Click(Sender: TObject);
var
  Tmp: Boolean;
begin
  with List do
	begin
    Tmp := Checked[ItemIndex];
    Checked[ItemIndex] := Checked[ItemIndex + 1];
    Checked[ItemIndex + 1] := Tmp;

    Items.Exchange(ItemIndex, ItemIndex + 1);
    ItemIndex := ItemIndex + 1;
  end;
  SetControlState;
end;

procedure TGridColsFm.SetControlState;
begin
  ToolButton3.Enabled := List.ItemIndex > 0;
  ToolButton4.Enabled := (List.ItemIndex >=0) and (List.ItemIndex < List.Items.Count - 1);
end;

function TGridColsFm.ShowForm(aCols: TGridColumns; aForm: TdxForm): Integer;
var
  i: Integer;
  C: TGridColumn;
  Tmp: Boolean;
  S, FlNm: String;
  Cmp: TComponent;
begin
  List.Clear;
  for i := 0 to aCols.Count - 1 do
  begin
    C := aCols[i];
    S := C.Title.Caption;
    if aForm <> nil then
    begin
      Cmp := FindById(aForm, C.Tag);
      FlNm := GetFieldName(Cmp);
      if (FlNm <> S) then S := S + ' (' + FlNm + ')';
    end;
    List.Items.AddObject(S, C);
    List.Checked[i] := C.Visible;
  end;
  Result := ShowModal;
  if Result <> mrOk then Exit;
  for i := 0 to List.Items.Count - 1 do
  begin
  	C := TGridColumn(List.Items.Objects[i]);
    Tmp := C.Visible;
    C.Visible := List.Checked[i];
    C.Index := i;
    if not Tmp and C.Visible and (C.Width = 0) then C.Width := 100;
  end;
end;

end.

