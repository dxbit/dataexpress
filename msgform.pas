unit MsgForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, {Windows, }SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TMsgFm }

  TMsgFm = class(TForm)
    Image1: TImage;
    MsgLbl: TLabel;
    Timer: TTimer;
    procedure MsgLblClick(Sender: TObject);
    procedure MsgLblMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerTimer(Sender: TObject);
  private
    FDetails: String;
  public
    procedure ShowForm(const AMsg, ADetails: String);
  end;

var
  MsgFm: TMsgFm;

implementation

uses
  apputils;

{$R *.lfm}

{ TMsgFm }

procedure TMsgFm.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  Close;
end;

procedure TMsgFm.MsgLblClick(Sender: TObject);
begin
  //Timer.Enabled := False;
  Close;
  ErrMsg(FDetails);
end;

procedure TMsgFm.MsgLblMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Timer.Enabled := False;
end;

procedure TMsgFm.ShowForm(const AMsg, ADetails: String);
var
  R: TRect;
  M: TMonitor;
begin
  FDetails := ADetails;
  GetWindowBounds(Application.MainForm, R);
  M := Screen.MonitorFromRect(R);
  R := M.WorkareaRect;
  Left := R.Right - Width - ScaleToScreen(5);
  Top := R.Bottom - Height - ScaleToScreen(5);

  MsgLbl.Caption := AMsg;
  Timer.Enabled := True;
  Show;
end;

end.

