unit HelpViewForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, IpHtml, Ipfilebroker, Forms, Controls, Graphics,
  Dialogs, Menus, IpMsg, strconsts, LclType;

type

  { THelpViewFm }

  THelpViewFm = class(TForm)
    HtmlPan: TIpHtmlPanel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(const aText: String);
  end;

var
  HelpViewFm: THelpViewFm;

procedure ShowHelpForm(const aText: String);

implementation

uses
  apputils, myctrls;

procedure ShowHelpForm(const aText: String);
begin
  if HelpViewFm = nil then
  	HelpViewFm := THelpViewFm.Create(Application);
  HelpViewFm.ShowForm(aText);
end;

{$R *.lfm}

{ THelpViewFm }

procedure THelpViewFm.FormCreate(Sender: TObject);
begin
  Caption := rsHelp;
  HtmlPan.DataProvider := THtmlProvider.Create(Self);
  MenuItem1.Caption:=rsCopy;
  MenuItem2.Caption := rsSelectAll;
end;

procedure THelpViewFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrClose;
end;

procedure THelpViewFm.MenuItem1Click(Sender: TObject);
begin
  HtmlPan.CopyToClipboard;
end;

procedure THelpViewFm.MenuItem2Click(Sender: TObject);
begin
  HtmlPan.SelectAll;
end;

procedure THelpViewFm.ShowForm(const aText: String);
begin
  HtmlPan.SetHtmlFromStr('<html><head><meta content="text/html;charset=UTF-8" ' +
    'http-equiv="Content-Type"></head>' + aText + '</html>');
  PositionActiveFormCenter(Self);
  ShowModal;
end;

end.

