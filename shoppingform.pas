unit ShoppingForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Buttons, strconsts, dxctrls, Lists;

type

  { TShoppingFm }

  TShoppingFm = class(TForm)
    BitBtn1: TBitBtn;
    ButtonPanel1: TButtonPanel;
    QttyInp: TCheckBox;
    PriceInp: TCheckBox;
    AddExist: TCheckBox;
    Obj: TComboBox;
    QttyFld: TComboBox;
    PriceObjF: TComboBox;
    PriceFld: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ObjChange(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    procedure FillObjects;
    procedure FillFormFields(L: TStrings);
    procedure FillObjFields;
    procedure ClearData;
    function Validate: Boolean;
  public
    { public declarations }
    procedure ShowForm(Fm: TdxForm);
  end;

var
  ShoppingFm: TShoppingFm;

procedure ShowShoppingForm(Fm: TdxForm);

implementation

uses
  apputils, formmanager, helpmanager, mytypes;

procedure ShowShoppingForm(Fm: TdxForm);
begin
  if ShoppingFm = nil then
  	ShoppingFm := TShoppingFm.Create(Application);
  ShoppingFm.ShowForm(Fm);
end;

{$R *.lfm}

{ TShoppingFm }

procedure TShoppingFm.ObjChange(Sender: TObject);
begin
  FillObjFields;
end;

procedure TShoppingFm.BitBtn1Click(Sender: TObject);
begin
  ClearData;
end;

procedure TShoppingFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
    CanClose := Validate;
end;

procedure TShoppingFm.FormCreate(Sender: TObject);
begin
  Caption := rsShopping;
  Label1.Caption := rsObject;
  GroupBox1.Caption := rsQuantity;
  QttyInp.Caption := rsInput;
  Label2.Caption := rsFormField;
  GroupBox2.Caption := rsPrice;
  PriceInp.Caption := rsInput;
  Label3.Caption := rsObjField;
  Label4.Caption := rsFormField;
  AddExist.Caption := rsAddToExisting;
  BitBtn1.Caption := rsClear;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TShoppingFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('shopping');
end;

procedure TShoppingFm.FillObjects;
var
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  SL := TStringListUtf8.Create;
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TdxLookupComboBox then
      with TdxLookupComboBox(C) do
        if (SourceTId > 0) and (SourceFId > 0) then
          SL.AddObject(FieldName, TObject(PtrInt(Id)));
  end;
  SL.Sort;
  Obj.Items := SL;
  Obj.Items.Insert(0, '');
  SL.Free;
end;

procedure TShoppingFm.FillFormFields(L: TStrings);
var
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  SL := TStringListUtf8.Create;
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TdxCalcEdit then
      SL.AddObject(GetFieldName(C), TObject(PtrInt(GetId(C))));
  end;
  SL.Sort;
  L.Assign(SL);
  L.Insert(0, '');
  SL.Free;
end;

procedure TShoppingFm.FillObjFields;
var
  SL: TStringListUtf8;
  i: Integer;
  C: TComponent;
  OFm: TdxForm;
begin
  PriceObjF.Clear;
  i := Obj.ItemIndex;
  if i <= 0 then Exit;
  C := FindById(FFm, PtrInt(Obj.Items.Objects[i]));
  OFm := FormMan.FindForm(GetSourceTId(C));

  SL := TStringListUtf8.Create;
  for i := 0 to OFm.ComponentCount - 1 do
  begin
    C := OFm.Components[i];
    if C is TdxCalcEdit then
      SL.AddObject(GetFieldName(C), TObject(PtrInt(GetId(C))));
  end;
  SL.Sort;
  PriceObjF.Items := SL;
  PriceObjF.Items.Insert(0, '');
  SL.Free;
end;

procedure TShoppingFm.ClearData;
begin
  Obj.ItemIndex := -1;
  QttyInp.Checked := False;
  QttyFld.ItemIndex := -1;
  PriceInp.Checked := False;
  PriceObjF.ItemIndex := -1;
  PriceFld.ItemIndex := -1;
  AddExist.Checked := False;
end;

function TShoppingFm.Validate: Boolean;
begin
  Result := False;
  if (QttyInp.Checked or AddExist.Checked) and (QttyFld.ItemIndex <= 0) then
  begin
    ErrMsg(rsQttyFldNotSel);
    QttyFld.SetFocus;
    Exit;
  end
  else if PriceInp.Checked then
  begin
    if PriceFld.ItemIndex <= 0 then
    begin
      ErrMsg(rsPriceFldNotSel);
      PriceFld.SetFocus;
      Exit;
    end;
  end
  else if (Obj.ItemIndex <= 0) and ((QttyInp.Checked) or (QttyFld.ItemIndex > 0) or
    (PriceInp.Checked) or (PriceFld.ItemIndex > 0) or (AddExist.Checked)) then
  begin
    ErrMsg(rsObjectNotSel);
    Obj.SetFocus;
    Exit;
  end
  else if not PriceInp.Checked then
  begin
    if (PriceObjF.ItemIndex <= 0) and (PriceFld.ItemIndex > 0) then
    begin
      ErrMsg(rsObjectFieldNotSel);
      PriceObjF.SetFocus;
      Exit;
    end
    else if (PriceFld.ItemIndex <= 0) and (PriceObjF.ItemIndex > 0) then
    begin
      ErrMsg(rsPriceFldNotSel);
      PriceFld.SetFocus;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TShoppingFm.ShowForm(Fm: TdxForm);
var
  SD: TShopData;
begin
  FFm := Fm;
  SD := FFm.ShopData;
  FillObjects;
  with Obj do
    ItemIndex := Items.IndexOfObject(TObject(PtrInt(SD.ObjId)));
  ObjChange(Obj);
  with PriceObjF do
    ItemIndex := Items.IndexOfObject(TObject(PtrInt(SD.PriceObjFId)));
  FillFormFields(QttyFld.Items);
  with QttyFld do
    ItemIndex := Items.IndexOfObject(TObject(PtrInt(SD.QttyFId)));
  FillFormFields(PriceFld.Items);
  with PriceFld do
    ItemIndex := Items.IndexOfObject(TObject(PtrInt(SD.PriceFId)));
  QttyInp.Checked:=SD.QttyInput;
  PriceInp.Checked := SD.PriceInput;
  AddExist.Checked := SD.AddToExisting;

  if ShowModal = mrOk then
  begin
    SD.ObjId:=0;
    with Obj do
      if ItemIndex > 0 then
        SD.ObjId := PtrInt(Items.Objects[ItemIndex]);
    SD.QttyInput:=QttyInp.Checked;
    SD.QttyFId:=0;
    with QttyFld do
      if ItemIndex > 0 then
        SD.QttyFId := PtrInt(Items.Objects[ItemIndex]);
    SD.PriceInput:=PriceInp.Checked;
    SD.PriceObjFId:=0;
    with PriceObjF do
      if ItemIndex > 0 then
        SD.PriceObjFId := PtrInt(Items.Objects[ItemIndex]);
    SD.PriceFId:=0;
    with PriceFld do
      if ItemIndex > 0 then
        SD.PriceFId := PtrInt(Items.Objects[ItemIndex]);
    SD.AddToExisting:=AddExist.Checked;
    if SD.ObjId = 0 then ClearData;
  end;
end;

end.

