# delphi-twain
Twain features for Delphi and Lazarus (Free Pascal)

# Installation

DelphiTwain is a runtime library. There are no visual components that have to be installed. 
Just add the package to dependancy/required of your Lazarus/Delphi Application and start using it.

The library is initially created by © Gustavo Daud and modified by Nemeth Peter and vcldeveloper.
In the year 2023 it was taken up by Massimo Magnano to update and modify it.

# Library design

- Full html help for the component classes. The library is able to fully access Twain capabilities.
- VCL, LCL and FireMonkey support (Windows-only).
- DelphiTwain is not a TComponent descendand. 
  You have to use it from code only and free it by yourself (see the examples).
  For Lazarus use delphitwain_pkg.lpk package, 
  For Delphi use delphitwain_dpkg.dpk package,
  For FireMonkey add DelphiTwain and DelphiTwain_FMX units to the uses clause.
- Acquiring images is easy as a few line codes.
- Direct access to various twain features.
- Showcases making it easy to learn.
- Supported Compilers:
  Lazarus / Free Pascal
  VCL: Delphi 6 and newer
  FireMonkey: Delphi XE2 and newer.

See the changelog.txt file for Change Log

Original code: delphitwain.sourceforge.net
Unicode modifications: stackoverflow.com/questions/2059343/twain-scanning-components-for-delphi
Additional modifications: kluug.net/delphitwain.php

Last modification: Massimo Magnano form year 2023
