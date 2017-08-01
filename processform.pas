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

implementation

{$R *.lfm}

{ TProcessFm }

procedure TProcessFm.BitBtn1Click(Sender: TObject);
begin
  FCanceled := True;
end;

procedure TProcessFm.FormCreate(Sender: TObject);
begin
  Caption := '';
  Label1.Caption := rsProcessing;
  BitBtn1.Caption := rsCancel;
end;

procedure TProcessFm.ShowForm;
begin
  Msg.Caption := '';
  Progress.Position:=0;
  FCanceled := False;
  Show;
end;

end.

