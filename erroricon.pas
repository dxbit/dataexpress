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
unit ErrorIcon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, strconsts;

type
  { TCalcError }

  TCalcError = class(TCustomControl)
  private
    FImage: TImage;
    FErrs: TStringList;
    procedure ImageClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddError(aName: String; const Msg: String);
  end;

implementation

uses
  calcerrsform;

{ TCalcError }

procedure TCalcError.ImageClick(Sender: TObject);
begin
  Visible := False;
  CalcErrsFm.ShowForm(FErrs);
end;

constructor TCalcError.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 16; Height := 16;
  FImage := TImage.Create(Self);
  FImage.Align := alClient;
  FImage.Parent := Self;
  FImage.Picture.LoadFromLazarusResource('error16');
  FImage.ShowHint:=True;
  FImage.Hint := rsErrorsOccured;
  FImage.OnClick := @ImageClick;
  FErrs := TStringList.Create;
  Visible := False;
end;

destructor TCalcError.Destroy;
begin
  FImage.Free;
  FErrs.Free;
  inherited Destroy;
end;

procedure TCalcError.AddError(aName: String; const Msg: String);
begin
  FErrs.Insert(0, '[' + TimeToStr(Time) + '] ' + aName + ': ' + Msg);
  Visible := True;
end;

end.

