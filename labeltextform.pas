unit LabelTextForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ButtonPanel, strconsts, dxctrls;

type

  { TLabelTextFm }

  TLabelTextFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    Memo1: TMemo;
    ToolBar1: TToolBar;
    Bn1: TToolButton;
    Bn2: TToolButton;
    Bn3: TToolButton;
    procedure Bn1Click(Sender: TObject);
    procedure Bn2Click(Sender: TObject);
    procedure Bn3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(C: TdxLabel): Integer;
  end;

var
  LabelTextFm: TLabelTextFm;

function ShowLabelTextForm(C: TdxLabel): Integer;

implementation

uses
  apputils;

function ShowLabelTextForm(C: TdxLabel): Integer;
begin
  if LabelTextFm = nil then
  	LabelTextFm := TLabelTextFm.Create(Application);
  Result := LabelTextFm.ShowForm(C);
end;

{$R *.lfm}

{ TLabelTextFm }

procedure TLabelTextFm.FormCreate(Sender: TObject);
begin
  Caption := rsLblText;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
end;

procedure TLabelTextFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
    if Trim(Memo1.Text) = '' then
    begin
      ErrMsg(rsLabelEmpty);
      CanClose := False;
    end;
end;

procedure TLabelTextFm.Bn1Click(Sender: TObject);
begin
  Memo1.Alignment:=taLeftJustify;
end;

procedure TLabelTextFm.Bn2Click(Sender: TObject);
begin
  Memo1.Alignment:=taCenter;
end;

procedure TLabelTextFm.Bn3Click(Sender: TObject);
begin
  Memo1.Alignment:=taRightJustify;
end;

procedure TLabelTextFm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

function TLabelTextFm.ShowForm(C: TdxLabel): Integer;
begin
  if Trim(C.Expression) <> '' then
  begin
    Info(rsTextPropNotAvail);
    Exit;
  end;
  Bn1.Down:=C.Alignment = taLeftJustify;
  Bn2.Down:=C.Alignment = taCenter;
  Bn3.Down:=C.Alignment = taRightJustify;
  Memo1.Alignment:=C.Alignment;
  Memo1.Text := C.Caption;
  Result := ShowModal;
  if Result = mrOk then
  begin
    if Bn1.Down then C.Alignment:=taLeftJustify
    else if Bn2.Down then C.Alignment:=taCenter
    else if Bn3.Down then C.Alignment:=taRightJustify;
    C.Caption := Memo1.Text;
    C.FieldName := Memo1.Text;
  end;
end;

end.

