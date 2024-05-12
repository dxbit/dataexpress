unit HintForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { THintFm }

  THintFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(C: TControl);
  end;

var
  HintFm: THintFm;

procedure ShowHintForm(C: TControl);

implementation

uses
  helpmanager;

procedure ShowHintForm(C: TControl);
begin
  if HintFm = nil then
  	HintFm := THintFm.Create(Application);
  HintFm.ShowForm(C);
end;

{$R *.lfm}

{ THintFm }

procedure THintFm.FormCreate(Sender: TObject);
begin
  Caption := rsHint;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure THintFm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure THintFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('Hints');
end;

procedure THintFm.ShowForm(C: TControl);
begin
  Memo1.Text := C.Hint;
  if ShowModal = mrOk then
  begin
    C.Hint := Trim(Memo1.Text);
    C.ShowHint:=C.Hint <> '';
  end;
end;

end.

