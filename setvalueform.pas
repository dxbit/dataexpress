unit SetValueForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, EditBtn, dxctrls, strconsts;

type

  { TSetValueFm }

  TSetValueFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ExprEd: TEditButton;
    Fields: TComboBox;
    Forms: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ExprEdButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    procedure FillForms;
    procedure FillFields;
  public
    { public declarations }
    function ShowForm(aFm: TdxForm; var TId, FId: Integer; var Expr: String): Boolean;
  end;

var
  SetValueFm: TSetValueFm;

function ShowSetValueForm(aFm: TdxForm; var TId, FId: Integer; var Expr: String): Boolean;

implementation

uses
  formmanager, apputils, helpmanager, exprform, mytypes, dximages, dxfiles;

function ShowSetValueForm(aFm: TdxForm; var TId, FId: Integer; var Expr: String
  ): Boolean;
begin
  if SetValueFm = nil then
  	SetValueFm := TSetValueFm.Create(Application);
  Result := SetValueFm.ShowForm(aFm, TId, FId, Expr);
end;

{$R *.lfm}

{ TSetValueFm }

procedure TSetValueFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
  begin
    CanClose := False;
    if Fields.ItemIndex < 0 then ErrMsg(rsFieldNotSel)
    else if Trim(ExprEd.Text) = '' then ErrMsg(rsEnterExpr)
    else CanClose := True;
  end;
end;

procedure TSetValueFm.ExprEdButtonClick(Sender: TObject);
var
  S: TCaption;
  Fm: TdxForm;
begin
  S := ExprEd.Text;
  Fm := nil;
  if Forms.ItemIndex >= 0 then
    Fm := TdxForm(Forms.Items.Objects[Forms.ItemIndex]);
  if ShowExprForm(etSetValue, nil, S, Fm, nil, nil, nil) = mrOk then
    ExprEd.Text := S;
end;

procedure TSetValueFm.FormCreate(Sender: TObject);
begin
  Caption := rsSetValue;
  Label1.Caption := rsForm;
  Label2.Caption := rsField;
  Label3.Caption := rsExpression;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TSetValueFm.FormsChange(Sender: TObject);
begin
  FillFields;
end;

procedure TSetValueFm.FormShow(Sender: TObject);
begin
  Forms.SetFocus;
end;

procedure TSetValueFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('setvalue');
end;

procedure TSetValueFm.FillForms;
var
  SL: TStringListUtf8;
  i: Integer;
  Fm: TdxForm;
begin
  Forms.Clear;
  SL := TStringListUtf8.Create;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if Fm.PId = FFm.Id then
      SL.AddObject(Fm.GetRecordsCaption, Fm);
  end;
  SL.Sort;
  SL.InsertObject(0, FFm.GetRecordsCaption, FFm);
  Forms.Items := SL;
  SL.Free;
end;

procedure TSetValueFm.FillFields;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  //Expr: String;
  SL: TStringListUtf8;
begin
  Fields.Clear;
  if Forms.ItemIndex < 0 then Exit;
  SL := TStringListUtf8.Create;
  Fm := TdxForm(Forms.Items.Objects[Forms.ItemIndex]);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not IsField(C) or (C is TdxDBImage) or (C is TdxFile) then Continue;
    //Expr := Trim(GetExpression(C));
    //if Expr = '' then
      SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  Fields.Items := SL;
  SL.Free;
end;

function TSetValueFm.ShowForm(aFm: TdxForm; var TId, FId: Integer;
  var Expr: String): Boolean;
begin
  FFm := aFm;
  FillForms;
  Fields.Clear;
  ExprEd.Text := '';
  Result := ShowModal = mrOk;
  if Result then
  begin
    TId := TdxForm(Forms.Items.Objects[Forms.ItemIndex]).Id;
    FId := GetId(TComponent(Fields.Items.Objects[Fields.ItemIndex]));
    Expr := Trim(ExprEd.Text);
  end;
end;

end.

