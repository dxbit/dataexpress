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

