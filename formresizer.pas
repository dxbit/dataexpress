{-------------------------------------------------------------------------------

    Copyright 2015-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit FormResizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, Graphics, dxctrls, formdesigner;

type

  { TFormResizer }

  TFormResizer = class
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FOffset, FSpace: Integer;
    FControl: TDesignerBox;
    FForm: TdxForm;
    FOnFormResize: TNotifyEvent;
    FXPress, FYPress: Boolean;
    FDummyX, FDummyY: TLabel;
  public
    procedure Bind(aControl: TDesignerBox; aForm: TdxForm; AOffset: Integer);
    procedure UnBind;
    property OnFormResize: TNotifyEvent read FOnFormResize write FOnFormResize;
  end;

implementation

uses
  apputils;

{ TFormResizer }

procedure TFormResizer.Bind(aControl: TDesignerBox; aForm: TdxForm;
  AOffset: Integer);
begin
  FControl := aControl;
  FForm := aForm;
  FOffset := AOffset;
  FSpace := ScaleToScreen(8);
  FControl.OnMouseMove:=@ControlMouseMove;
  FControl.OnMouseUp:=@ControlMouseUp;
  FDummyX := TLabel(FControl.Owner.FindComponent('DummyX'));
  FDummyY := TLabel(FControl.Owner.FindComponent('DummyY'));
  FDummyX.Left := FOffset + FForm.Width + FSpace;
  FDummyY.Top := FOffset + FForm.Height + FSpace;
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

procedure TFormResizer.ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  R: TRect;
begin
  R := FForm.BoundsRect;
  R.Offset(FOffset, FOffset);
  X := FControl.HorzScrollBar.Position + X;
  Y := FControl.VertScrollBar.Position + Y;
  if (X >= R.Right) and (X <= R.Right + 8) and (Y >= R.Top) and
    (Y <= R.Bottom) then
  begin
    FControl.Cursor := crSizeWE;
    FXPress := ssLeft in Shift;
  end
  else if (X >= R.Left) and (X <= R.Right) and (Y >= R.Bottom) and
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
      R.Right := X - 1;
      FForm.Width := R.Width;
      {$ifdef linux}
      FControl.DesignFm.Width := R.Width;
      {$endif}
    end
    else if FYPress then
    begin
      R.Bottom := Y - 1;
      FForm.Height := R.Height;
      {$ifdef linux}
      FControl.DesignFm.Height := R.Height;
      {$endif}
    end;
    if FOnFormResize <> nil then FOnFormResize(Self);
  end;
end;

procedure TFormResizer.ControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  if Button = mbLeft then
  begin
    FXPress := False;
    FYPress := False;
    R := FForm.BoundsRect;
    R.Offset(FOffset, FOffset);
    FDummyX.Left := R.Right + FSpace;
    FDummyY.Top := R.Bottom + FSpace;
  end;
end;

end.

