unit FormResizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, dxctrls;

type

  { TFormResizer }

  TFormResizer = class
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FControl: TScrollBox;
    FForm: TdxForm;
    FOnFormResize: TNotifyEvent;
    FXPress, FYPress: Boolean;
    FDummyX, FDummyY: TLabel;
  public
    procedure Bind(aControl: TScrollBox; aForm: TdxForm);
    procedure UnBind;
    property OnFormResize: TNotifyEvent read FOnFormResize write FOnFormResize;
  end;

implementation

{ TFormResizer }

procedure TFormResizer.Bind(aControl: TScrollBox; aForm: TdxForm);
begin
  FControl := aControl;
  FForm := aForm;
  FControl.OnMouseMove:=@ControlMouseMove;
  FControl.OnMouseUp:=@ControlMouseUp;
  FDummyX := TLabel(FControl.Owner.FindComponent('DummyX'));
  FDummyY := TLabel(FControl.Owner.FindComponent('DummyY'));
  FDummyX.Left := FForm.Left + FForm.Width + 32;
  FDummyY.Top := FForm.Top + FForm.Height + 32;
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
      R.Right := X - 1;
      FForm.BoundsRect := R;
    end
    else if FYPress then
    begin
      R.Bottom := Y - 1;
      FForm.BoundsRect := R;
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
    FDummyX.Left := R.Right + 32;
    FDummyY.Top := R.Bottom + 32;
  end;
end;

end.

