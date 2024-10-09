(*******************************************************************************
**                                  Delphi Twain                              **
**                                                                            **
**          (c) 2024 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Twain Source Data Types and Consts                                       **
*******************************************************************************)
unit DelphiTwainTypes;

{$H+}

interface

uses
  Classes, SysUtils, Twain, DelphiTwain;

type
{$ifndef FPC}
{$ifdef CPUX64}
  PtrInt = Int64;
  PtrUInt = UInt64;
{$else}
  PtrInt = longint;
  PtrUInt = Longword;
{$endif}
{$endif}

  TTwainDeviceInfo = record
    FromAddList: Boolean;
    Manufacturer,          { Manufacturer name, e.g. "Hewlett-Packard" }
    ProductFamily,         { Product family name, e.g. "ScanJet" }
    ProductName: String;   { Product name, e.g. "ScanJet Plus" }
  end;

  TTwainParams = packed record
    PaperFeed: TTwainPaperFeeding;
    PaperSize: TTwainPaperSize;
    Resolution,
    Contrast,
    Brightness: Single;
    BitDepth: Integer;
    PixelType:TTwainPixelType;
  end;

  TTwainParamsCapabilities = packed record
    PaperFeedingSet: TTwainPaperFeedingSet;
    //PaperFeedDefault: TTwainPaperFeeding;
    PaperSizeSet: TTwainPaperSizeSet;
    PixelType:TTwainPixelTypeSet;
    PixelTypeDefault:TTwainPixelType;
    PaperSizeDefault: TTwainPaperSize;
    ResolutionDefault: Single;
    BitDepthDefault,
    ResolutionArraySize,
    BitDepthArraySize: Integer;

    //Array MUST be at the end so then 32bit server can write up to BitDepthArraySize with a single write
    ResolutionArray: TTwainResolution;
    BitDepthArray: TArrayInteger;
  end;

//Comparing by "Unique Id" given by Twain It's useless because it's not unique and it always changes
function DeviceInfoDifferent(A, B: TTwainDeviceInfo): Boolean; overload;
function DeviceInfoDifferent(A, B: TW_IDENTITY): Boolean; overload;
function DeviceInfoDifferent(A: TTwainDeviceInfo; B: TW_IDENTITY): Boolean; overload;

implementation

function DeviceInfoDifferent(A, B: TTwainDeviceInfo): Boolean;
begin
  Result:= (A.FromAddList <> B.FromAddList) or (A.ProductName <> B.ProductName) or
           (A.ProductFamily <> B.ProductFamily) or (A.Manufacturer <> B.Manufacturer);
end;

function DeviceInfoDifferent(A, B: TW_IDENTITY): Boolean;
begin
  Result:= (A.ProductName <> B.ProductName) or
           (A.ProductFamily <> B.ProductFamily) or (A.Manufacturer <> B.Manufacturer);
end;

function DeviceInfoDifferent(A: TTwainDeviceInfo; B: TW_IDENTITY): Boolean;
begin
  Result:= (A.ProductName <> B.ProductName) or
           (A.ProductFamily <> B.ProductFamily) or (A.Manufacturer <> B.Manufacturer);
end;

end.

