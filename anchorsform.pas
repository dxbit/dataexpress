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

unit AnchorsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel, strconsts, dxctrls;

{ TAnchorsFm }

type
  TAnchorsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grp: TCheckGroup;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(CL: TList): Integer;
  end;

var
  AnchorsFm: TAnchorsFm;

function ShowAnchorsForm(CL: TList): Integer;

implementation

uses
  helpmanager;

function ShowAnchorsForm(CL: TList): Integer;
begin
  if AnchorsFm = nil then
  	AnchorsFm := TAnchorsFm.Create(Application);
  Result := AnchorsFm.ShowForm(CL);
end;

{$R *.lfm}

{ TAnchorsFm }

procedure TAnchorsFm.FormCreate(Sender: TObject);
begin
  Caption := rsAnchors;
  Grp.Caption := rsAnchors;
  Grp.Items.AddStrings([rsLeftSide, rsTopSide, rsRightSide, rsBottomSide]);
  ButtonPanel1.Okbutton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TAnchorsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('anchors');
end;

function TAnchorsFm.ShowForm(CL: TList): Integer;
var
  A: TAnchors;
  i: Integer;
begin
  A := GetCtrlAnchors(TComponent(CL[0]));
  Grp.Checked[0] := akLeft in A;
  Grp.Checked[1] := akTop in A;
  Grp.Checked[2] := akRight in A;
  Grp.Checked[3] := akBottom in A;
  Result := ShowModal;
  if Result = mrOk then
  begin
    A := [];
    if Grp.Checked[0] then Include(A, akLeft);
    if Grp.Checked[1] then Include(A, akTop);
    if Grp.Checked[2] then Include(A, akRight);
    if Grp.Checked[3] then Include(A, akBottom);
    for i := 0 to CL.Count - 1 do
	  	SetCtrlAnchors(TComponent(CL[i]), A);
  end;
end;

end.

