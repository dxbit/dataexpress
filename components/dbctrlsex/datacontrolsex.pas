{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DataControlsEx;

interface

uses
  DBCtrlsEx, Regs, ListBar, GridBar, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Regs', @Regs.Register);
end;

initialization
  RegisterPackage('DataControlsEx', @Register);
end.
