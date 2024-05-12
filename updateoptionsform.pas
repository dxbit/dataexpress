unit UpdateOptionsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ButtonPanel, SQLDB, updatemanager, strconsts;

type

  { TUpdateOptionsFm }

  TUpdateOptionsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FUpdMan: TUpdateMan;
  public
    function ShowForm(AUpdMan: TUpdateMan): Integer;
  end;

var
  UpdateOptionsFm: TUpdateOptionsFm;

function ShowUpdateOptionsForm(AUpdMan: TUpdateMan): Integer;

implementation

uses
  apputils;

function ShowUpdateOptionsForm(AUpdMan: TUpdateMan): Integer;
begin
  if UpdateOptionsFm = nil then
    UpdateOptionsFm := TUpdateOptionsFm.Create(Application);
  Result := UpdateOptionsFm.ShowForm(AUpdMan);
end;

{$R *.lfm}

{ TUpdateOptionsFm }

procedure TUpdateOptionsFm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure TUpdateOptionsFm.FormCreate(Sender: TObject);
begin
  Caption := rsUpdateSettings;
  GroupBox1.Caption := rsMsgWhenUpdateFound;
  ButtonPanel1.OkButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
end;

function TUpdateOptionsFm.ShowForm(AUpdMan: TUpdateMan): Integer;
begin
  FUpdMan := AUpdMan;
  Memo1.Text := AUpdMan.UserMsg;
  Result := ShowModal;
  if Result = mrOk then
  begin
    AUpdMan.UserMsg := Trim(Memo1.Text);
    AUpdMan.Version := BuildDateToStr;
    AUpdMan.SaveToDb;
  end;
end;

end.

