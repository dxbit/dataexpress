{
Copyright © 2015-2017 Pavel Duborkin
Author: Pavel Duborkin
E-Mail: 7bit@list.ru, mydataexpress@mail.ru

This file is part of DataExpress.

DataExpress is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

DataExpress is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with DataExpress.  If not, see <http://www.gnu.org/licenses/>.
}
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

implementation

uses
  helpform;

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

