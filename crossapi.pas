unit CrossApi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils {$ifdef windows}, Windows{$endif}, Controls, Forms, LclIntf;

procedure GetWindowNormalPosition(Frm: TCustomForm; out R: TRect);

implementation

{$ifdef windows}
// Функция вычисляет размер окна в нормальном состоянии с учетом того, что
// Form.Width и Form.Height не включают в себя рамку и заголовок окна.
procedure GetWindowNormalPosition(Frm: TCustomForm; out R: TRect);
var
  Pl: WINDOWPLACEMENT;
  x, y: Integer;
begin
  GetWindowPlacement(Frm.Handle, Pl);
  R := Pl.rcNormalPosition;

  x := 0; y := 0;

  case Frm.BorderStyle of
    bsSizeable, bsSizeToolWin:
      begin
        x := x + GetSystemMetrics(SM_CXSIZEFRAME) * 2;
        y := y + GetSystemMetrics(SM_CYSIZEFRAME) * 2;
      end;
    bsDialog, bsSingle, bsToolWindow:
      begin
        x := x + GetSystemMetrics(SM_CXFIXEDFRAME) * 2;
        y := y + GetSystemMetrics(SM_CYFIXEDFRAME) * 2;
      end;
  end;

  //if Frm.Menu <> nil then y := y + GetSystemMetrics(SM_CYMENU);
  if Frm.BorderStyle <> bsNone then y := y + GetSystemMetrics(SM_CYCAPTION);

  R.Width := R.Width - x;
  R.Height := R.Height - y;
end;

{$else}
procedure GetWindowNormalPosition(Frm: TCustomForm; out R: TRect);
begin
  R := Frm.BoundsRect;
end;
{$endif}

end.

