# delphi-twain
Twain Scanner features for Delphi and Lazarus (Free Pascal)

# Installation

DelphiTwain is a runtime library. There are no visual components that have to be installed. 
Just add the package to dependancy/required of your Lazarus/Delphi Application and start using it.

The library is initially created by Gustavo Daud and modified by Nemeth Peter and Others.
In the year 2023 it was taken up by Massimo Magnano to update and modify it.

# Library design

- The library is able to fully access Twain capabilities (except for the known limitations described below).
- VCL, LCL and FireMonkey support (Windows-only). (FireMonkey is not tested by Massimo Magnano)
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

# Known limitations

Most Twain Scanners have a 32-bit driver, so if you compile your project as a 64-bit project you will not be able to use this Package to acquire an image. 
In fact, Windows cannot load a 32-bit DLL inside a 64-bit application. 
If you want to use a 32-bit scanner you MUST compile your project as a 32-bit project, 
if you want to use a 64-bit scanner you MUST compile it as a 64-bit project.

This is a limitation of the Operating System, not of the Package.

The demo project has both 32-bit and 64-bit Build Modes.

Currently the Package is not tested for Front/Back acquisition from a Feeder, but it will be done in future releases.


See the changelog.txt file for Change Log

Original code: delphitwain.sourceforge.net
Unicode modifications: stackoverflow.com/questions/2059343/twain-scanning-components-for-delphi
Additional modifications: kluug.net/delphitwain.php

Last modification: Massimo Magnano form year 2023
