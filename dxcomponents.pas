{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dxcomponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  CtrlUtils, DialogGrid, CheckTreeView, TreeViewEx, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CtrlUtils', @CtrlUtils.Register);
  RegisterUnit('TreeViewEx', @TreeViewEx.Register);
end;

initialization
  RegisterPackage('dxcomponents', @Register);
end.
