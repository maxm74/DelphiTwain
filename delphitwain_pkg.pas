{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DelphiTwain_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  Twain, DelphiTwain, DelphiTwain_VCL, uFormSelectSource_VCL, DelphiTwainLang, DelphiTwainUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DelphiTwain_pkg', @Register);
end.
