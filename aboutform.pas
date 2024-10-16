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

unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, strconsts, LclType;

{ TAboutFm }

type
  TAboutFm = class(TForm)
    MailLbl: TLabel;
    WebSiteLbl: TLabel;
    MailImg: TImage;
    WebImg: TImage;
    Licensed: TLabel;
    Label2: TLabel;
    LicenseBn: TBitBtn;
    DonateBn: TBitBtn;
    CloseBn: TBitBtn;
    AppVersion: TLabel;
    Copyright: TLabel;
    GroupBox1: TGroupBox;
    LogoImg: TImage;
    Label1: TLabel;
    AppDescript: TLabel;
    procedure LicenseBnClick(Sender: TObject);
    procedure DonateBnClick(Sender: TObject);
    procedure CloseBnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MailLblClick(Sender: TObject);
    procedure WebSiteLblClick(Sender: TObject);
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
  LicenseBn.Caption := rsAgreement;
  DonateBn.Caption := rsDonate;
  CloseBn.Caption := rsClose;
  SetupBitBtn(LicenseBn, 'hands16');
  SetupBitBtn(DonateBn, 'money16');
  SetupBitBtn(CloseBn, 'delete16');
  SetupPicture(LogoImg.Picture, 'logo64');
  SetupPicture(MailImg.Picture, 'mail16');
  SetupPicture(WebImg.Picture, 'web16');
end;

procedure TAboutFm.MailLblClick(Sender: TObject);
begin
  OpenUrl('mailto:mydataexpress@mail.ru');
end;

procedure TAboutFm.WebSiteLblClick(Sender: TObject);
begin
  OpenUrl('https://mydataexpress.ru');
end;

procedure TAboutFm.DonateBnClick(Sender: TObject);
begin
  OpenUrl('http://mydataexpress.ru/donate/');
end;

procedure TAboutFm.CloseBnClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutFm.LicenseBnClick(Sender: TObject);
begin
  ShowLicenseForm;
end;

end.

