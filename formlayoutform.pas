unit FormLayoutForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ButtonPanel, ExtCtrls, dxctrls, formlayouts, strconsts;

type

  { TFormLayoutFm }

  TFormLayoutFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    AppTypeGrp: TCheckGroup;
    DisabledChk: TCheckBox;
    FixedHeightChk: TCheckBox;
    FormSizeLbl: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    NameEdt: TEdit;
    Label1: TLabel;
    MinWidthSpn: TSpinEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    FpFm: PFormLayoutForm;
    FpLay: PFormLayout;
  public
    function ShowForm(AForm: TdxForm; var ALayoutName: String; pFm: PFormLayoutForm): Integer;
  end;

var
  FormLayoutFm: TFormLayoutFm;

function ShowFormLayoutForm(AForm: TdxForm; var ALayoutName: String; pFm: PFormLayoutForm): Integer;

implementation

uses
  apputils, helpmanager;

function ShowFormLayoutForm(AForm: TdxForm; var ALayoutName: String;
  pFm: PFormLayoutForm): Integer;
begin
  if FormLayoutFm = nil then
    FormLayoutFm := TFormLayoutFm.Create(Application);
  Result := FormLayoutFm.ShowForm(AForm, ALayoutName, pFm);
end;

{$R *.lfm}

{ TFormLayoutFm }

procedure TFormLayoutFm.FormShow(Sender: TObject);
begin
  NameEdt.SetFocus;
end;

procedure TFormLayoutFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('layouts');
end;

procedure TFormLayoutFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  pLay: PFormLayout;
begin
  if ModalResult <> mrOk then Exit;

  CanClose := False;
  if Trim(NameEdt.Text) = '' then
  begin
    ErrMsg(rsEnterLayoutName);
    NameEdt.SetFocus;
    Exit;
  end;

  pLay := FpFm^.Layouts.FindLayout(NameEdt.Text);
  if (pLay <> nil) and (pLay <> FpLay) then
  begin
    ErrMsg(rsLayoutExists);
    NameEdt.SetFocus;
  end
  else if not AppTypeGrp.Checked[0] and not AppTypeGrp.Checked[1] then
  begin
    ErrMsg(rsAppTypeNotSelected);
    AppTypeGrp.SetFocus;
  end
  else CanClose := True;
end;

procedure TFormLayoutFm.FormCreate(Sender: TObject);
begin
  Label1.Caption := rsLayoutName;
  AppTypeGrp.Caption := rsWhereUse;
  AppTypeGrp.Items[0] := rsWeb;
  AppTypeGrp.Items[1] := rsDesktop;
  Label2.Caption := rsFormSize + ':';
  Label3.Caption := rsBreakingPoint;
  FixedHeightChk.Caption := rsFixedHeightForm;
  DisabledChk.Caption := rsDisabled;
  ButtonPanel1.OkButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

function TFormLayoutFm.ShowForm(AForm: TdxForm; var ALayoutName: String;
  pFm: PFormLayoutForm): Integer;
var
  IsNew: Boolean;
begin
  FpFm := pFm;
  FpLay := pFm^.Layouts.FindLayout(ALayoutName);
  IsNew := ALayoutName = '';
  NameEdt.Text := ALayoutName;
  if IsNew then
  begin
    Caption := rsNewLayout;
    AppTypeGrp.Checked[0] := False;
    AppTypeGrp.Checked[1] := False;
    MinWidthSpn.Value := AForm.Width;
    FixedHeightChk.Checked := False;
    DisabledChk.Checked := False;
    FormSizeLbl.Caption := Format('%d x %d', [AForm.Width, AForm.Height]);
  end
  else
  begin
    Caption := rsEditLayout;
    AppTypeGrp.Checked[0] := FpLay^.Web;
    AppTypeGrp.Checked[1] := FpLay^.Desktop;
    MinWidthSpn.Value := FpLay^.MinWidth;
    FixedHeightChk.Checked := FpLay^.FixedHeight;
    DisabledChk.Checked := FpLay^.Disabled;
    FormSizeLbl.Caption := Format('%d x %d', [FpLay^.Width, FpLay^.Height]);
  end;

  Result := ShowModal;

  if Result = mrOk then
  begin
    ALayoutName := NameEdt.Text;
    if IsNew then
      FpLay := pFm^.Layouts.AddLayout(ALayoutName, AForm);
    FpLay^.Name := ALayoutName;
    FpLay^.Web := AppTypeGrp.Checked[0];
    FpLay^.Desktop := AppTypeGrp.Checked[1];
    FpLay^.MinWidth := MinWidthSpn.Value;
    FpLay^.FixedHeight := FixedHeightChk.Checked;
    FpLay^.Disabled := DisabledChk.Checked;
  end;
end;

end.

