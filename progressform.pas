unit ProgressForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls;

type

  { TProgressFm }

  TProgressFm = class(TForm)
    Image1: TImage;
    Msg: TLabel;
    Progress: TProgressBar;
    Shape1: TShape;
  private

  public
    procedure ShowForm(const aMsg: String; Marquee: Boolean);
  end;

var
  ProgressFm: TProgressFm;

procedure ShowProgress(const aMsg: String; Marquee: Boolean);
procedure CloseProgress;

implementation

uses
  mainform, dbengine;

procedure ShowProgress(const aMsg: String; Marquee: Boolean);
begin
  if not DBase.IsRemote then Exit;

  MainFm.Enabled:=False;
  ProgressFm.ShowForm(aMsg, Marquee);
  //Application.ProcessMessages;
end;

procedure CloseProgress;
begin
  if not DBase.IsRemote then Exit;

  ProgressFm.Close;
  MainFm.Enabled:=True;
end;

{$R *.lfm}

{ TProgressFm }

procedure TProgressFm.ShowForm(const aMsg: String; Marquee: Boolean);
begin
  if Marquee then
    Progress.Style := pbstMarquee
  else
    Progress.Style := pbstNormal;
  Show;
  Msg.Caption := aMsg;
  Application.ProcessMessages;
end;

end.

