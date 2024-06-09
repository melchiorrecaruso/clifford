{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Clifford;

{$warn 5023 off : no warning about unused units}
interface

uses
  Cl2, Cl3, Cl4, Cl13, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('Clifford', @Register);
end.
