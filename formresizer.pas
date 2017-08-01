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
unit FormResizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, dxctrls;

type

  { TFormResizer }

  TFormResizer = class
    procedure ControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FControl: TScrollBox;
    FForm: TdxForm;
    FOldX, FOldY: Integer;
    FXPress, FYPress: Boolean;
  public
    procedure Bind(aControl: TScrollBox; aForm: TdxForm);
    procedure UnBind;
  end;

implementation

uses
  apputils;

{ TFormResizer }

procedure TFormResizer.Bind(aControl: TScrollBox; aForm: TdxForm);
begin
  FControl := aControl;
  FForm := aForm;
  FControl.OnMouseDown:=@ControlMouseDown;
  FControl.OnMouseMove:=@ControlMouseMove;
  FControl.OnMouseUp:=@ControlMouseUp;
end;

procedure TFormResizer.UnBind;
begin
  if FControl = nil then Exit;
  FControl.OnMouseUp:=nil;
  FControl.OnMouseMove:=nil;
  FControl.OnMouseDown:=nil;
  FControl := nil;
  FForm := nil;
end;

procedure TFormResizer.ControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FOldX := X; FOldY := Y;
  end;
end;

procedure TFormResizer.ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  R: TRect;
  delta: Integer;
begin
  R := FForm.BoundsRect;
  X := FControl.HorzScrollBar.Position + X;
  Y := FControl.VertScrollBar.Position + Y;
  {$ifdef windows}
  delta := 0;
  {$else}
  delta := 4;
  {$endif}
  if (X >= R.Right+delta) and (X <= R.Right + 8) and (Y >= R.Top) and
    (Y <= R.Bottom) then
  begin
    FControl.Cursor := crSizeWE;
    FXPress := ssLeft in Shift;
  end
  else if (X >= R.Left) and (X <= R.Right) and (Y >= R.Bottom+delta) and
    (Y <= R.Bottom + 8) then
  begin
    FControl.Cursor := crSizeNS;
    FYPress := ssLeft in Shift;
  end
  else
    FControl.Cursor := crDefault;
  if ssLeft in Shift then
  begin
    if FXPress then
    begin
      R.Right := R.Right + (X - FOldX);
        R.Right := X - 1;
      FForm.BoundsRect := R;
      X := FOldX;
    end
    else if FYPress then
    begin
      R.Bottom := R.Bottom + (Y - FOldY);
        R.Bottom := Y - 1;
      FForm.BoundsRect := R;
      Y := FOldY;
    end;
  end;
end;

procedure TFormResizer.ControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FXPress := False;
    FYPress := False;
  end;
end;

end.

