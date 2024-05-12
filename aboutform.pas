unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, strconsts, LclType;

{ TAboutFm }

type
  TAboutFm = class(TForm)
    Licensed: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    AppVersion: TLabel;
    Copyright: TLabel;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    AppDescript: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutFm: TAboutFm;

procedure ShowAboutForm;

implementation

uses
  apputils, licenseform;

procedure ShowAboutForm;
begin
  if AboutFm = nil then
  	AboutFm := TAboutFm.Create(Application);
  AboutFm.ShowModal;
end;

{$R *.lfm}

{ TAboutFm }

procedure TAboutFm.FormCreate(Sender: TObject);
begin
  Caption := rsAbout;
  AppDescript.Caption := rsAppDescript;
  Label2.Caption := rsVersion + ':';
  AppVersion.Caption := BuildDateToStr;
  Copyright.Caption := rsCopyrightText;
  Licensed.Caption := rsLicensedApache;
  BitBtn1.Caption := rsAgreement;
  BitBtn2.Caption := rsDonate;
  BitBtn3.Caption := rsClose;
  BitBtn1.LoadGlyphFromLazarusResource('hands16');
  BitBtn2.LoadGlyphFromLazarusResource('money16');
  BitBtn3.LoadGlyphFromLazarusResource('delete16');
end;

procedure TAboutFm.BitBtn2Click(Sender: TObject);
begin
  OpenUrl('http://mydataexpress.ru/donate/');
end;

procedure TAboutFm.BitBtn3Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutFm.BitBtn1Click(Sender: TObject);
begin
  ShowLicenseForm;
end;

end.

