unit ProcessForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, strconsts;

type

  { TProcessFm }

  TProcessFm = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Msg: TLabel;
    Progress: TProgressBar;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCanceled: Boolean;
    { private declarations }
  public
    { public declarations }
    procedure ShowForm;
    property Canceled: Boolean read FCanceled;
  end;

var
  ProcessFm: TProcessFm;

procedure ShowProcessForm;
procedure FreeProcessForm;

implementation

uses
  apputils;

procedure ShowProcessForm;
begin
  ProcessFm := TProcessFm.Create(nil);
  ProcessFm.ShowForm;
end;

procedure FreeProcessForm;
begin
  FreeAndNil(ProcessFm);
end;

{$R *.lfm}

{ TProcessFm }

procedure TProcessFm.BitBtn1Click(Sender: TObject);
begin
  if Confirm(rsWarning, rsAbortOperation) = mrYes then
	  FCanceled := True;
end;

procedure TProcessFm.FormCreate(Sender: TObject);
begin
  Caption := '';
  Label1.Caption := rsProcessing;
  BitBtn1.Caption := rsAbort;
end;

procedure TProcessFm.ShowForm;
begin
  Msg.Caption := '';
  Progress.Position:=0;
  FCanceled := False;
  Show;
end;

end.

