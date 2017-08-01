unit Regs;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, dbctrlsex, listbar, gridbar;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Controls Ex',[TDBEditButton, TDBDateEditEx, TDBCalcEdit,
    TComboBoxDS, TListBar, TGridBar]);
end;

initialization
{$i DataControlsEx.lrs}

end.
