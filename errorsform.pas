unit ErrorsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TErrorsFm }

  TErrorsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(const Err: String);
  end;

var
  ErrorsFm: TErrorsFm;

procedure ShowErrorsForm(const Err: String);

implementation

uses
  helpmanager;

procedure ShowErrorsForm(const Err: String);
begin
  if ErrorsFm = nil then
  	ErrorsFm := TErrorsFm.Create(Application);
  ErrorsFm.ShowForm(Err);
end;

{$R *.lfm}

{ TErrorsFm }

procedure TErrorsFm.FormCreate(Sender: TObject);
begin
  Caption := rsPrintErrors;
  ButtonPanel1.CloseButton.Caption:=rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TErrorsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('errors');
end;

procedure TErrorsFm.ShowForm(const Err: String);
begin
  Memo1.Lines.Text := Err;
  ShowModal;
end;

end.

