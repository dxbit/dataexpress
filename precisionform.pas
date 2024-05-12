unit PrecisionForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ButtonPanel, strconsts, dxctrls;

type

  { TPrecFm }

  TPrecFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    PadZeros: TCheckBox;
    GroupDigits: TCheckBox;
    Label1: TLabel;
    Prec: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private

  public
    function ShowForm(C: TdxCalcEdit): Integer;
  end;

var
  PrecFm: TPrecFm;

function ShowPrecForm(C: TdxCalcEdit): Integer;

implementation

uses
  helpmanager;

function ShowPrecForm(C: TdxCalcEdit): Integer;
begin
  if PrecFm = nil then
    PrecFm := TPrecFm.Create(Application);
  Result := PrecFm.ShowForm(C);
end;

{$R *.lfm}

{ TPrecFm }

procedure TPrecFm.FormCreate(Sender: TObject);
begin
  Caption := rsPrecission;
  Label1.Caption := rsNumberOfDigitsAfterPoint;
  GroupDigits.Caption := rsGroupDigits;
  PadZeros.Caption := rsPadZeros;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TPrecFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('precission');
end;

function TPrecFm.ShowForm(C: TdxCalcEdit): Integer;
begin
  Prec.Value := C.Precission;
  GroupDigits.Checked := C.GroupDigits;
  PadZeros.Checked := C.PadZeros;
  Result := ShowModal;
  if Result = mrOk then
  begin
    C.Precission := Prec.Value;
    C.GroupDigits := GroupDigits.Checked;
    C.PadZeros := PadZeros.Checked;
  end;
end;

end.

