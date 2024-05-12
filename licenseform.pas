unit LicenseForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LclType,
  strconsts;

type

  { TLicenseFm }

  TLicenseFm = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public
    function ShowForm: Integer;
  end;

var
  LicenseFm: TLicenseFm;

function ShowLicenseForm: Integer;

implementation

uses
  apputils;

function ShowLicenseForm: Integer;
begin
  if LicenseFm = nil then
    LicenseFm := TLicenseFm.Create(Application);
  Result := LicenseFm.ShowForm;
end;

{$R *.lfm}

{ TLicenseFm }

procedure TLicenseFm.FormCreate(Sender: TObject);
begin
  Caption := rsLicenseAgreement;
end;

procedure TLicenseFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrClose;
end;

function TLicenseFm.ShowForm: Integer;
var
  FlNm: String;
begin
  FlNm := AppPath + 'license.txt';

  if FileExists(FlNm) then
  begin
    Memo1.Lines.LoadFromFile(FlNm, True);
    Result := ShowModal;
  end
  else
    ErrMsgFmt(rsFileNotExists, [FlNm]);
end;

end.

