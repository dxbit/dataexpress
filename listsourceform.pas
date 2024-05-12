unit ListSourceForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Buttons, Menus, ExtCtrls, Spin, strconsts;

type

  { TListSourceFrm }

  TListSourceFrm = class(TForm)
    BitBtn1: TBitBtn;
    ButtonPanel1: TButtonPanel;
    Label2: TLabel;
    Obj: TListBox;
    Fields: TListBox;
    Label1: TLabel;
    Panel1: TPanel;
    RowCnt: TSpinEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure FieldsDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ObjSelectionChange(Sender: TObject; User: boolean);
    procedure FormShow(Sender: TObject);
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

function ShowListSourceForm(C: TComponent): Integer;

implementation

uses
  formmanager, dxctrls, LazUtf8, helpmanager, apputils;

function ShowListSourceForm(C: TComponent): Integer;
begin
  if ListSourceFrm = nil then
  	ListSourceFrm := TListSourceFrm.Create(Application);
  Result := ListSourceFrm.ShowForm(C);
end;

{$R *.lfm}

{ TListSourceFrm }

procedure TListSourceFrm.FormCreate(Sender: TObject);
begin
  Caption := rsListSource;
  Label2.Caption := rsRowCountInList;
  BitBtn1.Caption := rsClear;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TListSourceFrm.HelpButtonClick(Sender: TObject);
begin
  if FCbx is TdxComboBox then
    OpenHelp('listsource')
  else
    OpenHelp('memosource');
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

procedure TListSourceFrm.BitBtn1Click(Sender: TObject);
begin
  Obj.ItemIndex:=-1;
  Fields.Clear;
end;

procedure TListSourceFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult <> mrOk then Exit;

  CanClose := False;
  if (Obj.ItemIndex >= 0) and (Fields.ItemIndex < 0) then
  begin
  	ErrMsg(rsFieldNotSel);
    Fields.SetFocus;
  end
  else
  	CanClose := True;
end;

procedure TListSourceFrm.FormShow(Sender: TObject);
begin
  Obj.SetFocus;
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
      if MyUtf8CompareText(Fields.Items[j], S) > 0 then
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
        (C is TdxCounter) or (C is TdxCalcEdit) or (C is TdxRecordId) then
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
        //Obj.ItemIndex := -1;
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
      //Fields.ItemIndex := -1;
      Fields.ItemIndex := i;
      Break;
    end;
  end;
end;

function TListSourceFrm.ShowForm(C: TComponent): Integer;
var
  TId, NewTId, NewFId: Integer;
begin
  FCbx := C;
  Obj.Clear;
  FormMan.SourceFormsToList(Obj.Items);
  Fields.Clear;
  TId := GetSourceTId(C);
  SelectForm(TId);
  SelectField(GetSourceFId(C));

  Label2.Visible := C is TdxComboBox;
  RowCnt.Visible := Label2.Visible;

  if C is TdxComboBox then
	  RowCnt.Value:=TdxComboBox(C).DropDownCount;

  if ShowModal = mrOk then
  begin
    if Fields.ItemIndex >= 0 then
    begin
      NewTId := TdxForm(Obj.Items.Objects[Obj.ItemIndex]).Id;
      NewFId := GetId( TComponent(Fields.Items.Objects[Fields.ItemIndex]) );
      SetSourceTId(C, NewTId);
      SetSourceFId(C, NewFId);
      if C is TdxComboBox then TdxComboBox(C).Style := csDropDown;
    end
    else
    begin
      SetSourceTId(C, 0);
      SetSourceFId(C, 0);
    end;
    if C is TdxComboBox then
    	TdxComboBox(C).DropDownCount:=RowCnt.Value;
  end;
  Result := ModalResult;
end;

end.

