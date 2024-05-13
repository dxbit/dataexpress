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

