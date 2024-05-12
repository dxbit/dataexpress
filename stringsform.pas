unit StringsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TStringsFm }

  TStringsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FUrl: String;
  public
    { public declarations }
    function ShowForm(const Title, Url: String; SL: TStrings): Integer;
  end;

var
  StringsFm: TStringsFm;

function ShowStringsForm(const Title, Url: String; SL: TStrings): Integer;

implementation

uses
  helpmanager;

function ShowStringsForm(const Title, Url: String; SL: TStrings): Integer;
begin
  if StringsFm = nil then
  	StringsFm := TStringsFm.Create(Application);
  Result := StringsFm.ShowForm(Title, Url, SL);
end;

{$R *.lfm}

{ TStringsFm }

procedure TStringsFm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure TStringsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp(FUrl);
end;

procedure TStringsFm.FormCreate(Sender: TObject);
begin
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

function TStringsFm.ShowForm(const Title, Url: String; SL: TStrings): Integer;
begin
  FUrl := Url;
  ButtonPanel1.HelpButton.Visible:=Url > '';
  Caption := Title;
  Memo1.Clear;
  Memo1.Lines.AddStrings(SL);
  if ShowModal = mrOk then
  begin
    SL.Clear;
    SL.AddStrings(Memo1.Lines);
  end;
  Result := ModalResult;
end;

end.

