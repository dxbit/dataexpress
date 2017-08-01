{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dxcomponents;

interface

uses
  CtrlUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CtrlUtils', @CtrlUtils.Register);
end;

initialization
  RegisterPackage('dxcomponents', @Register);
end.
