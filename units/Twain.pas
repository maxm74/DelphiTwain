(******************************************************************************
*                FreePascal \ Delphi Twain Implementation                     *
*                                                                             *
*  FILE: Twain.pas                                                            *
*                                                                             *
*  VERSION:     2.3.1                                                         *
*                                                                             *
*  DESCRIPTION:                                                               *
*    Twain include file for applications and data sources written to          *
*    the TWAIN specification. It defines constants, data structures,          *
*    messages etc. for the public interface to TWAIN                          *
*                                                                             *
*******************************************************************************
*                                                                             *
*  (c) 2025 Massimo Magnano                                                   *
*      see Other Copyrights below                                             *                                                                             *
*                                                                             *
*  See changelog.txt and changelog_twain.txt for Change Log                   *
*                                                                             *
*******************************************************************************

 Portions created by TWAIN Working Group,

 The original file is: twain.h, released March 15, 2000.
 The original Pascal code is: twain.pas, released 20. Dez 1999.

 The initial developer of the Pascal code is:
   Uli Tessel (UT)(UliTessel@swol.de) with help of Matthias Thoma(MT)(ma.thoma@gmx.de)

 Translation cleaned up and updated to twain 1.9 by:
   Martin Olsson (MO), mnemo@home.se

 Updated to twain 2.0 (Dec 2008), 2.1 (Mar 2009) by:
   Lorenzo Monti (LM) lomo74@gmail.com  www.lorenzomonti.it

 Obtained through:
   Joint Endeavour of Delphi Innovators (Project JEDI) delphi-jedi.org

 The contents of this file are used with permission, subject to the
 Mozilla Public License Version 1.1 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1.1.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

*******************************************************************************

  Copyright (C) 2007 TWAIN Working Group: Adobe Systems Incorporated,
  AnyDoc Software Inc., Eastman Kodak Company, Fujitsu Computer Products
  of America, JFL Peripheral Solutions Inc., Ricoh Corporation, and
  Xerox Corporation.  All rights reserved.

  Copyright (C) 1991, 1992 TWAIN Working Group: Aldus, Caere, Eastman-Kodak,
  Hewlett-Packard and Logitech Corporations.  All rights reserved.

  Copyright (C) 1997 TWAIN Working Group: Bell+Howell, Canon, DocuMagix,
  Fujitsu, Genoa Technology, Hewlett-Packard, Kofax Imaging Products, and
  Ricoh Corporation.  All rights reserved.

  Copyright � 1998 TWAIN Working Group: Adobe Systems Incorporated,
  Canon Information Systems, Eastman Kodak Company,
  Fujitsu Computer Products of America, Genoa Technology,
  Hewlett-Packard Company, Intel Corporation, Kofax Image Products,
  JFL Peripheral Solutions Inc., Ricoh Corporation, and Xerox Corporation.
  All rights reserved.

  Copyright � 2000 TWAIN Working Group: Adobe Systems Incorporated,
  Canon Information Systems, Digimarc Corporation, Eastman Kodak Company,
  Fujitsu Computer Products of America, Hewlett-Packard Company,
  JFL Peripheral Solutions Inc., Ricoh Corporation, and Xerox Corporation.
  All rights reserved.
*******************************************************************************)
unit Twain;

{$IFDEF VER80}{$DEFINE DELPHI_6_PRIOR}{$ENDIF}
{$IFDEF VER90}{$DEFINE DELPHI_6_PRIOR}{$ENDIF}
{$IFDEF VER93}{$DEFINE DELPHI_6_PRIOR}{$ENDIF}
{$IFDEF VER100}{$DEFINE DELPHI_6_PRIOR}{$ENDIF}
{$IFDEF VER110}{$DEFINE DELPHI_6_PRIOR}{$ENDIF}
{$IFDEF VER120}{$DEFINE DELPHI_6_PRIOR}{$ENDIF}
{$IFDEF VER125}{$DEFINE DELPHI_6_PRIOR}{$ENDIF}
{$IFDEF VER130}{$DEFINE DELPHI_6_PRIOR}{$ENDIF}

{$IFDEF DELPHI_6_PRIOR}
  // LM: Delphi < 6 does not support alignment
  {$ALIGN OFF}
{$ELSE}
  // LM: set 2 byte alignment the correct way
  {$ALIGN 2}
{$ENDIF}

interface

{$HPPEMIT '#include <twain.h>'}

{***************************************************************************
 * TWAIN Version                                                           *
 ***************************************************************************}

const
  TWON_PROTOCOLMINOR = 2;    { Changed for Version 2.2 }
  {$EXTERNALSYM TWON_PROTOCOLMINOR}
  TWON_PROTOCOLMAJOR = 2;
  {$EXTERNALSYM TWON_PROTOCOLMAJOR}

{***************************************************************************
 * Platform Dependent Definitions and Typedefs                             *
 ***************************************************************************}

type
  THandle = LongWord;
  {$EXTERNALSYM THandle}

  TW_HANDLE = THandle;
  {$EXTERNALSYM TW_HANDLE}
  TTWHandle = TW_HANDLE;

  TW_MEMREF = Pointer;
  {$EXTERNALSYM TW_MEMREF}
  TTWMemRef = TW_MEMREF;

  TW_UINTPTR = LongWord;
  {$EXTERNALSYM TW_UINTPTR}
  TTWUintPtr = TW_UINTPTR;

{***************************************************************************
 * Type Definitions                                                        *
 ***************************************************************************}

{* String types. These include room for the strings and a NULL char,     *
 * or, on the Mac, a length byte followed by the string.                 *
 * TW_STR255 must hold less than 256 chars so length fits in first byte. *}

  TW_STR32 = array[0..33] of AnsiChar;   // char    TW_STR32[34]
  {$EXTERNALSYM TW_STR32}
  pTW_STR32 = ^TW_STR32;
  {$EXTERNALSYM pTW_STR32}
  TTWStr32 = TW_STR32;
  PTWStr32 = pTW_STR32;

  TW_STR64 = array[0..65] of AnsiChar;   // char    TW_STR64[66]
  {$EXTERNALSYM TW_STR64}
  pTW_STR64 = ^TW_STR64;
  {$EXTERNALSYM pTW_STR64}
  TTWStr64 = TW_STR64;
  PTWStr64 = pTW_STR64;

  TW_STR128 = array[0..129] of AnsiChar; // char    TW_STR128[130]
  {$EXTERNALSYM TW_STR128}
  pTW_STR128 = ^TW_STR128;
  {$EXTERNALSYM pTW_STR128}
  TTWStr128 = TW_STR128;
  PTWStr128 = pTW_STR128;

  TW_STR255 = array[0..255] of AnsiChar; // char    TW_STR255[256]
  {$EXTERNALSYM TW_STR255}
  pTW_STR255 = ^TW_STR255;
  {$EXTERNALSYM pTW_STR255}
  TTWStr255 = TW_STR255;
  PTWStr255 = pTW_STR255;

{ Numeric types. }
  TW_INT8 = ShortInt;     // char TW_INT8
  {$EXTERNALSYM TW_INT8}
  pTW_INT8 = ^TW_INT8;
  {$EXTERNALSYM pTW_INT8}
  TTWInt8 = TW_INT8;
  PTWInt8 = pTW_INT8;

  TW_INT16 = SmallInt;    // short TW_INT16
  {$EXTERNALSYM TW_INT16}
  pTW_INT16 = ^TW_INT16;
  {$EXTERNALSYM pTW_INT16}
  TTWInt16 = TW_INT16;
  PTWInt16 = pTW_INT16;

  TW_INT32 = LongInt;     // long TW_INT32
  {$EXTERNALSYM TW_INT32}
  pTW_INT32 = ^TW_INT32;
  {$EXTERNALSYM pTW_INT32}
  TTWInt32 = TW_INT32;
  PTWInt32 = pTW_INT32;

  TW_UINT8 = Byte;        // unsigned char TW_UINT8
  {$EXTERNALSYM TW_UINT8}
  pTW_UINT8 = ^TW_UINT8;
  {$EXTERNALSYM pTW_UINT8}
  TTWUInt8 = TW_UINT8;
  PTWUInt8 = pTW_UINT8;

  TW_UINT16 = Word;       // unsigned short TW_UINT16
  {$EXTERNALSYM TW_UINT16}
  pTW_UINT16 = ^TW_UINT16;
  {$EXTERNALSYM pTW_UINT16}
  TTWUInt16 = TW_UINT16;
  PTWUInt16 = pTW_UINT16;

  TW_UINT32 = LongWord;   // unsigned long TW_UINT32
  {$EXTERNALSYM TW_UINT32}
  pTW_UINT32 = ^TW_UINT32;
  {$EXTERNALSYM pTW_UINT32}
  TTWUInt32 = TW_UINT32;
  PTWUInt32 = pTW_UINT32;

  TW_BOOL = WordBool;     // unsigned short TW_BOOL
  {$EXTERNALSYM TW_BOOL}
  pTW_BOOL = ^TW_BOOL;
  {$EXTERNALSYM pTW_BOOL}
  TTWBool = TW_BOOL;
  PTWBool = pTW_BOOL;

{***************************************************************************
 * Structure Definitions                                                   *
 ***************************************************************************}

{ Fixed point structure type. }
  TW_FIX32 = record
    Whole   : TW_INT16;
    Frac    : TW_UINT16;
  end;
  {$EXTERNALSYM TW_FIX32}
  pTW_FIX32 = ^TW_FIX32;
  {$EXTERNALSYM pTW_FIX32}
  TTWFix32 = TW_FIX32;
  PTWFix32 = pTW_FIX32;

{ Defines a frame rectangle in ICAP_UNITS coordinates. }
  TW_FRAME = record
    Left    : TW_FIX32;
    Top     : TW_FIX32;
    Right   : TW_FIX32;
    Bottom  : TW_FIX32;
  end;
  {$EXTERNALSYM TW_FRAME}
  pTW_FRAME = ^TW_FRAME;
  {$EXTERNALSYM pTW_FRAME}
  TTWFrame = TW_FRAME;
  PTWFrame = pTW_FRAME;

{ Defines the parameters used for channel-specific transformation. }
  TW_DECODEFUNCTION = record
    StartIn       : TW_FIX32;
    BreakIn       : TW_FIX32;
    EndIn         : TW_FIX32;
    StartOut      : TW_FIX32;
    BreakOut      : TW_FIX32;
    EndOut        : TW_FIX32;
    Gamma         : TW_FIX32;
    SampleCount   : TW_FIX32; { if =0 use the gamma }
  end;
  {$EXTERNALSYM TW_DECODEFUNCTION}
  pTW_DECODEFUNCTION = ^TW_DECODEFUNCTION;
  {$EXTERNALSYM pTW_DECODEFUNCTION}
  TTWDecodeFunction = TW_DECODEFUNCTION;
  PTWDecodeFunction = pTW_DECODEFUNCTION;

{ Stores a Fixed point number in two parts, a whole and a fractional part. }
  TW_TRANSFORMSTAGE = record
    Decode  : array[0..2] of TW_DECODEFUNCTION;
    Mix     : array[0..2, 0..2] of TW_FIX32;
  end;
  {$EXTERNALSYM TW_TRANSFORMSTAGE}
  pTW_TRANSFORMSTAGE = ^TW_TRANSFORMSTAGE;
  {$EXTERNALSYM pTW_TRANSFORMSTAGE}
  TTWTransformStage = TW_TRANSFORMSTAGE;
  PTWTransformStage = pTW_TRANSFORMSTAGE;

{ Container for array of values }
  TW_ARRAY = record
    ItemType  : TW_UINT16;
    NumItems  : TW_UINT32; { How many items in ItemList }
{$IFDEF DELPHI_6_PRIOR}
    ItemList  : array[0..1] of TW_UINT8; { Array of ItemType values starts here }
    // UT: ..1 for alignment to 2 Byte Packing, so sizeof is correct
{$ELSE}
    ItemList  : array[0..0] of TW_UINT8; { Array of ItemType values starts here }
    // LM: $ALIGN 2 specified
{$ENDIF}
  end;
  {$EXTERNALSYM TW_ARRAY}
  pTW_ARRAY = ^TW_ARRAY;
  {$EXTERNALSYM pTW_ARRAY}
  TTWArray = TW_ARRAY;
  PTWArray = pTW_ARRAY;

{ Information about audio data }
  TW_AUDIOINFO = record
    Name      : TW_STR255;  { name of audio data }
    Reserved  : TW_UINT32;  { reserved space }
  end;
  {$EXTERNALSYM TW_AUDIOINFO}
  pTW_AUDIOINFO = ^TW_AUDIOINFO;
  {$EXTERNALSYM pTW_AUDIOINFO}
  TTWAudioInfo = TW_AUDIOINFO;
  PTWAudioInfo = pTW_AUDIOINFO;

{ Used to register callbacks. }
  TW_CALLBACK = record
    CallBackProc  : TW_MEMREF;
    RefCon        : TW_UINT32;
    Message       : TW_INT16;
  end;
  {$EXTERNALSYM TW_CALLBACK}
  pTW_CALLBACK = ^TW_CALLBACK;
  {$EXTERNALSYM pTW_CALLBACK}
  TTWCallback = TW_CALLBACK;
  PTWCallback = pTW_CALLBACK;

{ Used to register callbacks. }
  TW_CALLBACK2 = record
      CallBackProc  : TW_MEMREF;
      RefCon        : TW_UINTPTR;
      Message       : TW_INT16;
  end;
  {$EXTERNALSYM TW_CALLBACK2}
  pTW_CALLBACK2 = ^TW_CALLBACK2;
  {$EXTERNALSYM pTW_CALLBACK2}
  TTWCallback2 = TW_CALLBACK2;
  PTTWCallback2 = pTW_CALLBACK2;

{ Used by application to get/set capability from/in a data source. }
  TW_CAPABILITY = record
    Cap         : TW_UINT16; { id of capability to set or get, e.g. CAP_BRIGHTNESS }
    ConType     : TW_UINT16; { TWON_ONEVALUE, _RANGE, _ENUMERATION or _ARRAY }
    hContainer  : TW_HANDLE; { Handle to container of type Dat }
  end;
  {$EXTERNALSYM TW_CAPABILITY}
  pTW_CAPABILITY = ^TW_CAPABILITY;
  {$EXTERNALSYM pTW_CAPABILITY}
  TTWCapability = TW_CAPABILITY;
  PTWCapability = pTW_CAPABILITY;

{ Defines a CIE XYZ space tri-stimulus value. }
  TW_CIEPOINT = record
    X   : TW_FIX32;
    Y   : TW_FIX32;
    Z   : TW_FIX32;
  end;
  {$EXTERNALSYM TW_CIEPOINT}
  pTW_CIEPOINT = ^TW_CIEPOINT;
  {$EXTERNALSYM pTW_CIEPOINT}
  TTWCiePoint = TW_CIEPOINT;
  PTWCiePoint = pTW_CIEPOINT;

{ Defines the mapping from an RGB color space device into CIE 1931 (XYZ) color space. }
  TW_CIECOLOR = record
    ColorSpace        : TW_UINT16;
    LowEndian         : TW_INT16;
    DeviceDependent   : TW_INT16;
    VersionNumber     : TW_INT32;
    StageABC          : TW_TRANSFORMSTAGE;
    StageLMN          : TW_TRANSFORMSTAGE;
    WhitePoint        : TW_CIEPOINT;
    BlackPoint        : TW_CIEPOINT;
    WhitePaper        : TW_CIEPOINT;
    BlackInk          : TW_CIEPOINT;
    Samples           : array[0..0] of TW_FIX32;
  end;
  {$EXTERNALSYM TW_CIECOLOR}
  pTW_CIECOLOR = ^TW_CIECOLOR;
  {$EXTERNALSYM pTW_CIECOLOR}
  TTWCieColor = TW_CIECOLOR;
  PTWCieColor = pTW_CIECOLOR;

{ Allows for a data source and application to pass custom data to each other. }
  TW_CUSTOMDSDATA = record
    InfoLength  : TW_UINT32; { Length of Information in bytes. }
    hData       : TW_HANDLE; { Place holder for data, DS Allocates }
  end;
  {$EXTERNALSYM TW_CUSTOMDSDATA}
  pTW_CUSTOMDSDATA = ^TW_CUSTOMDSDATA;
  {$EXTERNALSYM pTW_CUSTOMDSDATA}
  TTWCustomDSData = TW_CUSTOMDSDATA;
  PTWCustomDSData = pTW_CUSTOMDSDATA;

{ Provides information about the Event that was raised by the Source }
  TW_DEVICEEVENT = record
    Event                   : TW_UINT32; { One of the TWDE_xxxx values. }
    DeviceName              : TW_STR255; { The name of the device that generated the event }
    BatteryMinutes          : TW_UINT32; { Battery Minutes Remaining }
    BatteryPercentage       : TW_INT16;  { Battery Percentage Remaining }
    PowerSupply             : TW_INT32;  { Power Supply }
    XResolution             : TW_FIX32;  { Resolution }
    YResolution             : TW_FIX32;  { Resolution }
    FlashUsed2              : TW_UINT32; { Flash Used2 }
    AutomaticCapture        : TW_UINT32; { Automatic Capture }
    TimeBeforeFirstCapture  : TW_UINT32; { Automatic Capture }
    TimeBetweenCaptures     : TW_UINT32; { Automatic Capture }
  end;
  {$EXTERNALSYM TW_DEVICEEVENT}
  pTW_DEVICEEVENT = ^TW_DEVICEEVENT;
  {$EXTERNALSYM pTW_DEVICEEVENT}
  TTWDeviceEvent = TW_DEVICEEVENT;
  PTWDeviceEvent = pTW_DEVICEEVENT;

{ This structure holds the tri-stimulus color palette information for TW_PALETTE8 structures. }
{$IFDEF DELPHI_6_PRIOR}
  TW_ELEMENT8 = record
    Index     : TW_UINT8; { Value used to index into the color table. }
    Pad1      : Byte;     { LM: for alignment }
    Channel1  : TW_UINT8; { First tri-stimulus value (e.g Red) }
    Pad2      : Byte;     { LM: for alignment }
    Channel2  : TW_UINT8; { Second tri-stimulus value (e.g Green) }
    Pad3      : Byte;     { LM: for alignment }
    Channel3  : TW_UINT8; { Third tri-stimulus value (e.g Blue) }
    Pad4      : Byte;     { LM: for alignment }
  end;
{$ELSE}
  TW_ELEMENT8 = record
    Index     : TW_UINT8; { Value used to index into the color table. }
    Channel1  : TW_UINT8; { First tri-stimulus value (e.g Red) }
    Channel2  : TW_UINT8; { Second tri-stimulus value (e.g Green) }
    Channel3  : TW_UINT8; { Third tri-stimulus value (e.g Blue) }
  end;
{$ENDIF}
  {$EXTERNALSYM TW_ELEMENT8}
  pTW_ELEMENT8 = ^TW_ELEMENT8;
  {$EXTERNALSYM pTW_ELEMENT8}
  TTWElement8 = TW_ELEMENT8;
  PTWElement8 = pTW_ELEMENT8;

{ Stores a group of individual values describing a capability. }
  TW_ENUMERATION = record
    ItemType      : TW_UINT16;
    NumItems      : TW_UINT32; { How many items in ItemList }
    CurrentIndex  : TW_UINT32; { Current value is in ItemList[CurrentIndex] }
    DefaultIndex  : TW_UINT32; { Powerup value is in ItemList[DefaultIndex] }
{$IFDEF DELPHI_6_PRIOR}
    ItemList      : array[0..1] of TW_UINT8; { Array of ItemType values starts here }
    // UT: ..1 for alignment to 2 Byte Packing, so sizeof is correct
{$ELSE}
    ItemList      : array[0..0] of TW_UINT8; { Array of ItemType values starts here }
    // LM: $ALIGN 2 specified.
{$ENDIF}
  end;
  {$EXTERNALSYM TW_ENUMERATION}
  pTW_ENUMERATION = ^TW_ENUMERATION;
  {$EXTERNALSYM pTW_ENUMERATION}
  TTWEnumeration = TW_ENUMERATION;
  PTWEnumeration = pTW_ENUMERATION;

{ Used to pass application events/messages from the application to the Source. }
  TW_EVENT = record
    pEvent      : TW_MEMREF; { Windows pMSG or Mac pEvent. }
    TWMessage   : TW_UINT16; { TW msg from data source, e.g. MSG_XFERREADY }
  end;
  {$EXTERNALSYM TW_EVENT}
  pTW_EVENT = ^TW_EVENT;
  {$EXTERNALSYM pTW_EVENT}
  TTWEvent = TW_EVENT;
  PTWEvent = pTW_EVENT;

{ This structure is used to pass specific information between the data source and the application. }
  TW_INFO = record
    InfoID      : TW_UINT16;
    ItemType    : TW_UINT16;
    NumItems    : TW_UINT16;
    ReturnCode  : TW_UINT16;
    Item        : TW_UINTPTR;
  end;
  {$EXTERNALSYM TW_INFO}
  pTW_INFO = ^TW_INFO;
  {$EXTERNALSYM pTW_INFO}
  TTWInfo = TW_INFO;
  PTWInfo = pTW_INFO;

  TW_EXTIMAGEINFO = record
    NumInfos  : TW_UINT32;
    Info      : array[0..0] of TW_INFO;
  end;
  {$EXTERNALSYM TW_EXTIMAGEINFO}
  pTW_EXTIMAGEINFO = ^TW_EXTIMAGEINFO;
  {$EXTERNALSYM pTW_EXTIMAGEINFO}
  TTWExtImageInfo = TW_EXTIMAGEINFO;
  PTWExtImageInfo = pTW_EXTIMAGEINFO;

{ Provides information about the currently selected device }
  TW_FILESYSTEM = record
    InputName         : TW_STR255;        { The name of the input or source file }
    OutputName        : TW_STR255;        { The result of an operation or the name of a destination file }
    Context           : TW_MEMREF;        { Source specific data used to remember state information }
    case Integer of
      0: (
        Recursive         : Integer;      { recursively delete all sub-directories }
        FileSystemType    : TW_UINT32;
      );
      1: (
        Subdirectories    : TW_BOOL;
        FileType          : TW_INT32;     { One of the TWFY_xxxx values }
        Size              : TW_UINT32;    { Size of current FileType }
        CreateTimeDate    : TW_STR32;     { creation date of the file }
        ModifiedTimeDate  : TW_STR32;     { last date the file was modified }
        FreeSpace         : TW_UINT32;    { bytes of free space on the current device }
        NewImageSize      : TW_INT32;     { estimate of the amount of space a new image would take up }
        NumberOfFiles     : TW_UINT32;    { number of files, depends on FileType }
        NumberOfSnippets  : TW_UINT32;    { number of audio snippets }
        DeviceGroupMask   : TW_UINT32;    { used to group cameras (ex: front/rear bitonal, front/rear grayscale...) }
        Reserved          : array[0..507] of TW_INT8;
      );
  end;
  {$EXTERNALSYM TW_FILESYSTEM}
  pTW_FILESYSTEM = ^TW_FILESYSTEM;
  {$EXTERNALSYM pTW_FILESYSTEM}
  TTWFileSystem = TW_FILESYSTEM;
  PTWFileSystem = pTW_FILESYSTEM;

{ This structure is used by the application to specify a set of mapping values to be applied to grayscale data. }
  TW_GRAYRESPONSE = record
    Response  : array[0..0] of TW_ELEMENT8;
  end;
  {$EXTERNALSYM TW_GRAYRESPONSE}
  pTW_GRAYRESPONSE = ^TW_GRAYRESPONSE;
  {$EXTERNALSYM pTW_GRAYRESPONSE}
  TTWGrayResponse = TW_GRAYRESPONSE;
  PTWGrayResponse = pTW_GRAYRESPONSE;

{ A general way to describe the version of software that is running. }
  TW_VERSION = record
    MajorNum  : TW_UINT16; { Major revision number of the software. }
    MinorNum  : TW_UINT16; { Incremental revision number of the software. }
    Language  : TW_UINT16; { e.g. TWLG_SWISSFRENCH }
    Country   : TW_UINT16; { e.g. TWCY_SWITZERLAND }
    Info      : TW_STR32;  { e.g. "1.0b3 Beta release" }
  end;
  {$EXTERNALSYM TW_VERSION}
  pTW_VERSION = ^TW_VERSION;
  {$EXTERNALSYM pTW_VERSION}
  TTWVersion = TW_VERSION;
  PTWVersion = pTW_VERSION;

{ Provides identification information about a TWAIN entity. }
  TW_IDENTITY = record
    Id              : TW_UINT32;  { Unique number. In Windows, application hWnd }
    Version         : TW_VERSION; { Identifies the piece of code }
    ProtocolMajor   : TW_UINT16;  { Application and DS must set to TWON_PROTOCOLMAJOR }
    ProtocolMinor   : TW_UINT16;  { Application and DS must set to TWON_PROTOCOLMINOR }
    SupportedGroups : TW_UINT32;  { Bit field OR combination of DG_ constants }
    Manufacturer    : TW_STR32;   { Manufacturer name, e.g. "Hewlett-Packard" }
    ProductFamily   : TW_STR32;   { Product family name, e.g. "ScanJet" }
    ProductName     : TW_STR32;   { Product name, e.g. "ScanJet Plus" }
  end;
  {$EXTERNALSYM TW_IDENTITY}
  pTW_IDENTITY = ^TW_IDENTITY;
  {$EXTERNALSYM pTW_IDENTITY}
  TTWIdentity = TW_IDENTITY;
  PTWIdentity = pTW_IDENTITY;

{ Describes the �real� image data, that is, the complete image being transferred between the Source and application. }
  TW_IMAGEINFO = record
    XResolution     : TW_FIX32;  { Resolution in the horizontal }
    YResolution     : TW_FIX32;  { Resolution in the vertical }
    ImageWidth      : TW_INT32;  { Columns in the image, -1 if unknown by DS }
    ImageLength     : TW_INT32;  { Rows in the image, -1 if unknown by DS }
    SamplesPerPixel : TW_INT16;  { Number of samples per pixel, 3 for RGB }
    BitsPerSample   : array[0..7] of TW_INT16; { Number of bits for each sample }
    BitsPerPixel    : TW_INT16;  { Number of bits for each padded pixel }
    Planar          : TW_BOOL;   { True if Planar, False if chunky }
    PixelType       : TW_INT16;  { How to interp data: ; photo interp (TWPT_) }
    Compression     : TW_UINT16; { How the data is compressed (TWCP_xxxx) }
  end;
  {$EXTERNALSYM TW_IMAGEINFO}
  pTW_IMAGEINFO = ^TW_IMAGEINFO;
  {$EXTERNALSYM pTW_IMAGEINFO}
  TTWImageInfo = TW_IMAGEINFO;
  PTWImageInfo = pTW_IMAGEINFO;

{ Involves information about the original size of the acquired image. }
  TW_IMAGELAYOUT = record
    Frame           : TW_FRAME;   { Frame coords within larger document }
    DocumentNumber  : TW_UINT32;
    PageNumber      : TW_UINT32;  { Reset when you go to next document }
    FrameNumber     : TW_UINT32;  { Reset when you go to next page }
  end;
  {$EXTERNALSYM TW_IMAGELAYOUT}
  pTW_IMAGELAYOUT = ^TW_IMAGELAYOUT;
  {$EXTERNALSYM pTW_IMAGELAYOUT}
  TTWImageLayout = TW_IMAGELAYOUT;
  PTWImageLayout = pTW_IMAGELAYOUT;

{ Provides information for managing memory buffers. }
  TW_MEMORY = record
    Flags   : TW_UINT32; { Any combination of the TWMF_ constants. }
    Length  : TW_UINT32; { Number of bytes stored in buffer TheMem. }
    TheMem  : TW_MEMREF; { Pointer or handle to the allocated memory buffer. }
  end;
  {$EXTERNALSYM TW_MEMORY}
  pTW_MEMORY = ^TW_MEMORY;
  {$EXTERNALSYM pTW_MEMORY}
  TTWMemory = TW_MEMORY;
  PTWMemory = pTW_MEMORY;

{ Describes the form of the acquired data being passed from the Source to the application. }
  TW_IMAGEMEMXFER = record
    Compression   : TW_UINT16; { How the data is compressed }
    BytesPerRow   : TW_UINT32; { Number of bytes in a row of data }
    Columns       : TW_UINT32; { How many columns }
    Rows          : TW_UINT32; { How many rows }
    XOffset       : TW_UINT32; { How far from the side of the image }
    YOffset       : TW_UINT32; { How far from the top of the image }
    BytesWritten  : TW_UINT32; { How many bytes written in Memory }
    Memory        : TW_MEMORY; { Mem struct used to pass actual image data }
  end;
  {$EXTERNALSYM TW_IMAGEMEMXFER}
  pTW_IMAGEMEMXFER = ^TW_IMAGEMEMXFER;
  {$EXTERNALSYM pTW_IMAGEMEMXFER}
  TTWImageMemXFER = TW_IMAGEMEMXFER;
  PTWImageMemXFER = pTW_IMAGEMEMXFER;

{ Describes the information necessary to transfer a JPEG-compressed image. }
  TW_JPEGCOMPRESSION = record
    ColorSpace        : TW_UINT16; { One of the TWPT_xxxx values }
    SubSampling       : TW_UINT32; { Two word "array" for subsampling values }
    NumComponents     : TW_UINT16; { Number of color components in image }
    RestartFrequency  : TW_UINT16; { Frequency of restart marker codes in MDU's }
    QuantMap    : array[0..3] of TW_UINT16; { Mapping of components to QuantTables }
    QuantTable  : array[0..3] of TW_MEMORY; { Quantization tables }
    HuffmanMap  : array[0..3] of TW_UINT16; { Mapping of components to Huffman tables }
    HuffmanDC   : array[0..1] of TW_MEMORY; { DC Huffman tables }
    HuffmanAC   : array[0..1] of TW_MEMORY; { AC Huffman tables }
  end;
  {$EXTERNALSYM TW_JPEGCOMPRESSION}
  pTW_JPEGCOMPRESSION = ^TW_JPEGCOMPRESSION;
  {$EXTERNALSYM pTW_JPEGCOMPRESSION}
  TTWJPEGCompression = TW_JPEGCOMPRESSION;
  PTWJPEGCompression = pTW_JPEGCOMPRESSION;

{ Stores a single value (item) which describes a capability. }
  TW_ONEVALUE = record
    ItemType  : TW_UINT16;
    Item      : TW_UINT32;
  end;
  {$EXTERNALSYM TW_ONEVALUE}
  pTW_ONEVALUE = ^TW_ONEVALUE;
  {$EXTERNALSYM pTW_ONEVALUE}
  TTWOneValue = TW_ONEVALUE;
  PTWOneValue = pTW_ONEVALUE;

{ This structure holds the color palette information. }
  TW_PALETTE8 = record
     NumColors    : TW_UINT16; { Number of colors in the color table. }
     PaletteType  : TW_UINT16; { TWPA_xxxx, specifies type of palette. }
     Colors       : array[0..255] of TW_ELEMENT8; { Array of palette values starts here. }
  end;
  {$EXTERNALSYM TW_PALETTE8}
  pTW_PALETTE8 = ^TW_PALETTE8;
  {$EXTERNALSYM pTW_PALETTE8}
  TTWPalette8 = TW_PALETTE8;
  PTWPalette8 = pTW_PALETTE8;

{ Used to bypass the TWAIN protocol when communicating with a device }
  TW_PASSTHRU = record
    pCommand        : TW_MEMREF; { Pointer to Command buffer }
    CommandBytes    : TW_UINT32; { Number of bytes in Command buffer }
    Direction       : TW_INT32;  { One of the TWDR_xxxx values. Defines the direction of data flow }
    pData           : TW_MEMREF; { Pointer to Data buffer }
    DataBytes       : TW_UINT32; { Number of bytes in Data buffer }
    DataBytesXfered : TW_UINT32; { Number of bytes successfully transferred }
  end;
  {$EXTERNALSYM TW_PASSTHRU}
  pTW_PASSTHRU = ^TW_PASSTHRU;
  {$EXTERNALSYM pTW_PASSTHRU}
  TTWPassThru = TW_PASSTHRU;
  PTWPassThru = pTW_PASSTHRU;

{ This structure tells the application how many more complete transfers the Source currently has available. }
  TW_PENDINGXFERS = record
    Count      : TW_UINT16;
    case Integer of
      0: (EOJ       : TW_UINT32);
      1: (Reserved  : TW_UINT32);
  end;
  {$EXTERNALSYM TW_PENDINGXFERS}
  pTW_PENDINGXFERS = ^TW_PENDINGXFERS;
  {$EXTERNALSYM pTW_PENDINGXFERS}
  TTWPendingXFERS = TW_PENDINGXFERS;
  PTWPendingXFERS = pTW_PENDINGXFERS;

{ Stores a range of individual values describing a capability. }
  TW_RANGE = record
    ItemType      : TW_UINT16;
    MinValue      : TW_UINT32; { Starting value in the range. }
    MaxValue      : TW_UINT32; { Final value in the range. }
    StepSize      : TW_UINT32; { Increment from MinValue to MaxValue. }
    DefaultValue  : TW_UINT32; { Power-up value. }
    CurrentValue  : TW_UINT32; { The value that is currently in effect. }
  end;
  {$EXTERNALSYM TW_RANGE}
  pTW_RANGE = ^TW_RANGE;
  {$EXTERNALSYM pTW_RANGE}
  TTWRange = TW_RANGE;
  PTWRange = pTW_RANGE;

{ This structure is used by the application to specify a set of mapping values to be applied to RGB color data. }
  TW_RGBRESPONSE = record
    Response  : array[0..0] of TW_ELEMENT8;
  end;
  {$EXTERNALSYM TW_RGBRESPONSE}
  pTW_RGBRESPONSE = ^TW_RGBRESPONSE;
  {$EXTERNALSYM pTW_RGBRESPONSE}
  TTWRGBResponse = TW_RGBRESPONSE;
  PTWRGBResponse = pTW_RGBRESPONSE;

{ Describes the file format and file specification information for a transfer through a disk file. }
  TW_SETUPFILEXFER = record
    FileName  : TW_STR255;
    Format    : TW_UINT16; { Any TWFF_ constant }
    VRefNum   : TW_INT16;  { Used for Mac only }
  end;
  {$EXTERNALSYM TW_SETUPFILEXFER}
  pTW_SETUPFILEXFER = ^TW_SETUPFILEXFER;
  {$EXTERNALSYM pTW_SETUPFILEXFER}
  TTWSetupFileXFER = TW_SETUPFILEXFER;
  PTWSetupFileXFER = pTW_SETUPFILEXFER;

{ Provides the application information about the Source�s requirements and preferences regarding allocation of transfer buffer(s). }
  TW_SETUPMEMXFER = record
    MinBufSize  : TW_UINT32;
    MaxBufSize  : TW_UINT32;
    Preferred   : TW_UINT32;
  end;
  {$EXTERNALSYM TW_SETUPMEMXFER}
  pTW_SETUPMEMXFER = ^TW_SETUPMEMXFER;
  {$EXTERNALSYM pTW_SETUPMEMXFER}
  TTWSetupMemXFER = TW_SETUPMEMXFER;
  PTWSetupMemXFER = pTW_SETUPMEMXFER;

{ Describes the status of a source. }
  TW_STATUS = record
    ConditionCode : TW_UINT16;   { Any TWCC_ constant }
    case Integer of
      0: (Data          : TW_UINT16); { output (TWAIN 2.1 and newer) This field contains additional data. }
      1: (Reserved      : TW_UINT16); { output (TWAIN 2.0 and older) }
  end;
  {$EXTERNALSYM TW_STATUS}
  pTW_STATUS = ^TW_STATUS;
  {$EXTERNALSYM pTW_STATUS}
  TTWStatus = TW_STATUS;
  PTWStatus = pTW_STATUS;

{ Translates the contents of Status into a localized UTF8string. }
  TW_STATUSUTF8 = record
    Status     : TW_STATUS; { input  TW_STATUS data received from a previous call to DG_CONTROL / DAT_STATUS / MSG_GET. }
    Size       : TW_UINT32; { output Total number of bytes in the UTF8string, plus the terminating NUL byte.  This is not the same as the total number of characters in the string. }
    UTF8string : TW_HANDLE; { output TW_HANDLE to a UTF-8 encoded localized string (based on TW_IDENTITY.Language or CAP_LANGUAGE).  The Source allocates it, the Application frees it. }
  end;
  {$EXTERNALSYM TW_STATUSUTF8}
  pTW_STATUSUTF8 = ^TW_STATUSUTF8;
  {$EXTERNALSYM pTW_STATUSUTF8}
  TTWStatusUTF8 = TW_STATUSUTF8;
  PTWStatusUTF8 = pTW_STATUSUTF8;

{ This structure is used to handle the user interface coordination between an application and a Source. }
  TW_USERINTERFACE = record
    ShowUI    : TW_BOOL;   { TRUE if DS should bring up its UI }
    ModalUI   : TW_BOOL;   { For Mac only - true if the DS's UI is modal }
    hParent   : TW_HANDLE; { For windows only - Application window handle }
  end;
  {$EXTERNALSYM TW_USERINTERFACE}
  pTW_USERINTERFACE = ^TW_USERINTERFACE;
  {$EXTERNALSYM pTW_USERINTERFACE}
  TTWUserInterface = TW_USERINTERFACE;
  PTWUserInterface = pTW_USERINTERFACE;


{***************************************************************************
 * Generic Constants                                                       *
 ***************************************************************************}
const
  TWON_ARRAY        = 3; { indicates TW_ARRAY container }
  {$EXTERNALSYM TWON_ARRAY}
  TWON_ENUMERATION  = 4; { indicates TW_ENUMERATION container }
  {$EXTERNALSYM TWON_ENUMERATION}
  TWON_ONEVALUE     = 5; { indicates TW_ONEVALUE container }
  {$EXTERNALSYM TWON_ONEVALUE}
  TWON_RANGE        = 6; { indicates TW_RANGE container }
  {$EXTERNALSYM TWON_RANGE}

  TWON_ICONID       = 962; { res Id of icon used in USERSELECT lbox }
  {$EXTERNALSYM TWON_ICONID}
  TWON_DSMID        = 461; { res Id of the DSM version num resource }
  {$EXTERNALSYM TWON_DSMID}
  TWON_DSMCODEID    = 63;  { res Id of the Mac SM Code resource }
  {$EXTERNALSYM TWON_DSMCODEID}

  TWON_DONTCARE8    = $ff;
  {$EXTERNALSYM TWON_DONTCARE8}
  TWON_DONTCARE16   = $ffff;
  {$EXTERNALSYM TWON_DONTCARE16}
  TWON_DONTCARE32   = $ffffffff;
  {$EXTERNALSYM TWON_DONTCARE32}

{ Flags used in TW_MEMORY structure. }
  TWMF_APPOWNS      = $0001;
  {$EXTERNALSYM TWMF_APPOWNS}
  TWMF_DSMOWNS      = $0002;
  {$EXTERNALSYM TWMF_DSMOWNS}
  TWMF_DSOWNS       = $0004;
  {$EXTERNALSYM TWMF_DSOWNS}
  TWMF_POINTER      = $0008;
  {$EXTERNALSYM TWMF_POINTER}
  TWMF_HANDLE       = $0010;
  {$EXTERNALSYM TWMF_HANDLE}

{* There are four containers used for capabilities negotiation:
 *    TWON_ONEVALUE, TWON_RANGE, TWON_ENUMERATION, TWON_ARRAY
 * In each container structure ItemType can be TWTY_INT8, TWTY_INT16, etc.
 * The kind of data stored in the container can be determined by doing
 * DCItemSize[ItemType] where the following is defined in TWAIN glue code:
 *          DCItemSize[]=   sizeof(TW_INT8),
 *                          sizeof(TW_INT16),
 *                          etc.
 *                          sizeof(TW_UINT32) ;
 *
 *}

  TWTY_INT8     = $0000;  { Means Item is a TW_INT8 }
  {$EXTERNALSYM TWTY_INT8}
  TWTY_INT16    = $0001;  { Means Item is a TW_INT16 }
  {$EXTERNALSYM TWTY_INT16}
  TWTY_INT32    = $0002;  { Means Item is a TW_INT32 }
  {$EXTERNALSYM TWTY_INT32}

  TWTY_UINT8    = $0003;  { Means Item is a TW_UINT8 }
  {$EXTERNALSYM TWTY_UINT8}
  TWTY_UINT16   = $0004;  { Means Item is a TW_UINT16 }
  {$EXTERNALSYM TWTY_UINT16}
  TWTY_UINT32   = $0005;  { Means Item is a TW_UINT32 }
  {$EXTERNALSYM TWTY_UINT32}

  TWTY_BOOL     = $0006;  { Means Item is a TW_BOOL }
  {$EXTERNALSYM TWTY_BOOL}

  TWTY_FIX32    = $0007;  { Means Item is a TW_FIX32 }
  {$EXTERNALSYM TWTY_FIX32}

  TWTY_FRAME    = $0008;  { Means Item is a TW_FRAME }
  {$EXTERNALSYM TWTY_FRAME}

  TWTY_STR32    = $0009;  { Means Item is a TW_STR32 }
  {$EXTERNALSYM TWTY_STR32}
  TWTY_STR64    = $000a;  { Means Item is a TW_STR64 }
  {$EXTERNALSYM TWTY_STR64}
  TWTY_STR128   = $000b;  { Means Item is a TW_STR128 }
  {$EXTERNALSYM TWTY_STR128}
  TWTY_STR255   = $000c;  { Means Item is a TW_STR255 }
  {$EXTERNALSYM TWTY_STR255}
  TWTY_HANDLE   = $000f;  { Means Item is a TW_HANDLE }
  {$EXTERNALSYM TWTY_HANDLE}

{***************************************************************************
 * Capability Constants                                                    *
 ***************************************************************************}

{ CAP_ALARMS values }
  TWAL_ALARM          = 0;
  {$EXTERNALSYM TWAL_ALARM}
  TWAL_FEEDERERROR    = 1;
  {$EXTERNALSYM TWAL_FEEDERERROR}
  TWAL_FEEDERWARNING  = 2;
  {$EXTERNALSYM TWAL_FEEDERWARNING}
  TWAL_BARCODE        = 3;
  {$EXTERNALSYM TWAL_BARCODE}
  TWAL_DOUBLEFEED     = 4;
  {$EXTERNALSYM TWAL_DOUBLEFEED}
  TWAL_JAM            = 5;
  {$EXTERNALSYM TWAL_JAM}
  TWAL_PATCHCODE      = 6;
  {$EXTERNALSYM TWAL_PATCHCODE}
  TWAL_POWER          = 7;
  {$EXTERNALSYM TWAL_POWER}
  TWAL_SKEW           = 8;
  {$EXTERNALSYM TWAL_SKEW}

{ ICAP_AUTOSIZE values }
  TWAS_NONE           = 0;
  {$EXTERNALSYM TWAS_NONE}
  TWAS_AUTO           = 1;
  {$EXTERNALSYM TWAS_AUTO}
  TWAS_CURRENT        = 2;
  {$EXTERNALSYM TWAS_CURRENT}

{ TWEI_BARCODEROTATION values }
  TWBCOR_ROT0         = 0;
  {$EXTERNALSYM TWBCOR_ROT0}
  TWBCOR_ROT90        = 1;
  {$EXTERNALSYM TWBCOR_ROT90}
  TWBCOR_ROT180       = 2;
  {$EXTERNALSYM TWBCOR_ROT180}
  TWBCOR_ROT270       = 3;
  {$EXTERNALSYM TWBCOR_ROT270}
  TWBCOR_ROTX         = 4;
  {$EXTERNALSYM TWBCOR_ROTX}

{ ICAP_BARCODESEARCHMODE values }
  TWBD_HORZ           = 0;
  {$EXTERNALSYM TWBD_HORZ}
  TWBD_VERT           = 1;
  {$EXTERNALSYM TWBD_VERT}
  TWBD_HORZVERT       = 2;
  {$EXTERNALSYM TWBD_HORZVERT}
  TWBD_VERTHORZ       = 3;
  {$EXTERNALSYM TWBD_VERTHORZ}

{ ICAP_BITORDER values }
  TWBO_LSBFIRST       = 0;
  {$EXTERNALSYM TWBO_LSBFIRST}
  TWBO_MSBFIRST       = 1;
  {$EXTERNALSYM TWBO_MSBFIRST}

{ ICAP_AUTODISCARDBLANKPAGES values }
  TWBP_DISABLE        = -2;
  {$EXTERNALSYM TWBP_DISABLE}
  TWBP_AUTO           = -1;
  {$EXTERNALSYM TWBP_AUTO}

{ ICAP_BITDEPTHREDUCTION values }
  TWBR_THRESHOLD        = 0;
  {$EXTERNALSYM TWBR_THRESHOLD}
  TWBR_HALFTONE         = 1;
  {$EXTERNALSYM TWBR_HALFTONE}
  TWBR_CUSTHALFTONE     = 2;
  {$EXTERNALSYM TWBR_CUSTHALFTONE}
  TWBR_DIFFUSION        = 3;
  {$EXTERNALSYM TWBR_DIFFUSION}
  TWBR_DYNAMICTHRESHOLD = 4;
  {$EXTERNALSYM TWBR_DYNAMICTHRESHOLD}

{ ICAP_SUPPORTEDBARCODETYPES and TWEI_BARCODETYPE values }
  TWBT_3OF9                 = 0;
  {$EXTERNALSYM TWBT_3OF9}
  TWBT_2OF5INTERLEAVED      = 1;
  {$EXTERNALSYM TWBT_2OF5INTERLEAVED}
  TWBT_2OF5NONINTERLEAVED   = 2;
  {$EXTERNALSYM TWBT_2OF5NONINTERLEAVED}
  TWBT_CODE93               = 3;
  {$EXTERNALSYM TWBT_CODE93}
  TWBT_CODE128              = 4;
  {$EXTERNALSYM TWBT_CODE128}
  TWBT_UCC128               = 5;
  {$EXTERNALSYM TWBT_UCC128}
  TWBT_CODABAR              = 6;
  {$EXTERNALSYM TWBT_CODABAR}
  TWBT_UPCA                 = 7;
  {$EXTERNALSYM TWBT_UPCA}
  TWBT_UPCE                 = 8;
  {$EXTERNALSYM TWBT_UPCE}
  TWBT_EAN8                 = 9;
  {$EXTERNALSYM TWBT_EAN8}
  TWBT_EAN13                = 10;
  {$EXTERNALSYM TWBT_EAN13}
  TWBT_POSTNET              = 11;
  {$EXTERNALSYM TWBT_POSTNET}
  TWBT_PDF417               = 12;
  {$EXTERNALSYM TWBT_PDF417}
  TWBT_2OF5INDUSTRIAL       = 13;
  {$EXTERNALSYM TWBT_2OF5INDUSTRIAL}
  TWBT_2OF5MATRIX           = 14;
  {$EXTERNALSYM TWBT_2OF5MATRIX}
  TWBT_2OF5DATALOGIC        = 15;
  {$EXTERNALSYM TWBT_2OF5DATALOGIC}
  TWBT_2OF5IATA             = 16;
  {$EXTERNALSYM TWBT_2OF5IATA}
  TWBT_3OF9FULLASCII        = 17;
  {$EXTERNALSYM TWBT_3OF9FULLASCII}
  TWBT_CODABARWITHSTARTSTOP = 18;
  {$EXTERNALSYM TWBT_CODABARWITHSTARTSTOP}
  TWBT_MAXICODE             = 19;
  {$EXTERNALSYM TWBT_MAXICODE}
  TWBT_QRCODE               = 20;
  {$EXTERNALSYM TWBT_QRCODE}

{ ICAP_COMPRESSION values }
  TWCP_NONE         = 0;
  {$EXTERNALSYM TWCP_NONE}
  TWCP_PACKBITS     = 1;
  {$EXTERNALSYM TWCP_PACKBITS}
  TWCP_GROUP31D     = 2;   { Follows CCITT spec (no End Of Line) }
  {$EXTERNALSYM TWCP_GROUP31D}
  TWCP_GROUP31DEOL  = 3;   { Follows CCITT spec (has End Of Line) }
  {$EXTERNALSYM TWCP_GROUP31DEOL}
  TWCP_GROUP32D     = 4;   { Follows CCITT spec (use cap for K Factor) }
  {$EXTERNALSYM TWCP_GROUP32D}
  TWCP_GROUP4       = 5;   { Follows CCITT spec }
  {$EXTERNALSYM TWCP_GROUP4}
  TWCP_JPEG         = 6;   { Use capability for more info }
  {$EXTERNALSYM TWCP_JPEG}
  TWCP_LZW          = 7;   { Must license from Unisys and IBM to use }
  {$EXTERNALSYM TWCP_LZW}
  TWCP_JBIG         = 8;   { For Bitonal images -- Added 1.7 KHL }
  {$EXTERNALSYM TWCP_JBIG}
  TWCP_PNG          = 9;   { Added 1.8 }
  {$EXTERNALSYM TWCP_PNG}
  TWCP_RLE4         = 10;  { Added 1.8 }
  {$EXTERNALSYM TWCP_RLE4}
  TWCP_RLE8         = 11;  { Added 1.8 }
  {$EXTERNALSYM TWCP_RLE8}
  TWCP_BITFIELDS    = 12;  { Added 1.8 }
  {$EXTERNALSYM TWCP_BITFIELDS}
  TWCP_ZIP          = 13;
  {$EXTERNALSYM TWCP_ZIP}
  TWCP_JPEG2000     = 14;
  {$EXTERNALSYM TWCP_JPEG2000}

{ CAP_CAMERASIDE and TWEI_PAGESIDE values }
  TWCS_BOTH         = 0;
  {$EXTERNALSYM TWCS_BOTH}
  TWCS_TOP          = 1;
  {$EXTERNALSYM TWCS_TOP}
  TWCS_BOTTOM       = 2;
  {$EXTERNALSYM TWCS_BOTTOM}

{ CAP_CLEARBUFFERS values }
  TWCB_AUTO         = 0;
  {$EXTERNALSYM TWCB_AUTO}
  TWCB_CLEAR        = 1;
  {$EXTERNALSYM TWCB_CLEAR}
  TWCB_NOCLEAR      = 2;
  {$EXTERNALSYM TWCB_NOCLEAR}

{ CAP_DEVICEEVENT values }
  TWDE_CUSTOMEVENTS          = $8000;
  {$EXTERNALSYM TWDE_CUSTOMEVENTS}
  TWDE_CHECKAUTOMATICCAPTURE = 0;
  {$EXTERNALSYM TWDE_CHECKAUTOMATICCAPTURE}
  TWDE_CHECKBATTERY          = 1;
  {$EXTERNALSYM TWDE_CHECKBATTERY}
  TWDE_CHECKDEVICEONLINE     = 2;
  {$EXTERNALSYM TWDE_CHECKDEVICEONLINE}
  TWDE_CHECKFLASH            = 3;
  {$EXTERNALSYM TWDE_CHECKFLASH}
  TWDE_CHECKPOWERSUPPLY      = 4;
  {$EXTERNALSYM TWDE_CHECKPOWERSUPPLY}
  TWDE_CHECKRESOLUTION       = 5;
  {$EXTERNALSYM TWDE_CHECKRESOLUTION}
  TWDE_DEVICEADDED           = 6;
  {$EXTERNALSYM TWDE_DEVICEADDED}
  TWDE_DEVICEOFFLINE         = 7;
  {$EXTERNALSYM TWDE_DEVICEOFFLINE}
  TWDE_DEVICEREADY           = 8;
  {$EXTERNALSYM TWDE_DEVICEREADY}
  TWDE_DEVICEREMOVED         = 9;
  {$EXTERNALSYM TWDE_DEVICEREMOVED}
  TWDE_IMAGECAPTURED         = 10;
  {$EXTERNALSYM TWDE_IMAGECAPTURED}
  TWDE_IMAGEDELETED          = 11;
  {$EXTERNALSYM TWDE_IMAGEDELETED}
  TWDE_PAPERDOUBLEFEED       = 12;
  {$EXTERNALSYM TWDE_PAPERDOUBLEFEED}
  TWDE_PAPERJAM              = 13;
  {$EXTERNALSYM TWDE_PAPERJAM}
  TWDE_LAMPFAILURE           = 14;
  {$EXTERNALSYM TWDE_LAMPFAILURE}
  TWDE_POWERSAVE             = 15;
  {$EXTERNALSYM TWDE_POWERSAVE}
  TWDE_POWERSAVENOTIFY       = 16;
  {$EXTERNALSYM TWDE_POWERSAVENOTIFY}

  { TW_PASSTHRU.Direction values }
  TWDR_GET          = 1;
  {$EXTERNALSYM TWDR_GET}
  TWDR_SET          = 2;
  {$EXTERNALSYM TWDR_SET}

{ TWEI_DESKEWSTATUS values }
  TWDSK_SUCCESS     = 0;
  {$EXTERNALSYM TWDSK_SUCCESS}
  TWDSK_REPORTONLY  = 1;
  {$EXTERNALSYM TWDSK_REPORTONLY}
  TWDSK_FAIL        = 2;
  {$EXTERNALSYM TWDSK_FAIL}
  TWDSK_DISABLED    = 3;
  {$EXTERNALSYM TWDSK_DISABLED}

{ CAP_DUPLEX values }
  TWDX_NONE         = 0;
  {$EXTERNALSYM TWDX_NONE}
  TWDX_1PASSDUPLEX  = 1;
  {$EXTERNALSYM TWDX_1PASSDUPLEX}
  TWDX_2PASSDUPLEX  = 2;
  {$EXTERNALSYM TWDX_2PASSDUPLEX}

{ CAP_FEEDERALIGNMENT values }
  TWFA_NONE         = 0;
  {$EXTERNALSYM TWFA_NONE}
  TWFA_LEFT         = 1;
  {$EXTERNALSYM TWFA_LEFT}
  TWFA_CENTER       = 2;
  {$EXTERNALSYM TWFA_CENTER}
  TWFA_RIGHT        = 3;
  {$EXTERNALSYM TWFA_RIGHT}

{ ICAP_FEEDERTYPE values }
  TWFE_GENERAL      = 0;
  {$EXTERNALSYM TWFE_GENERAL}
  TWFE_PHOTO        = 1;
  {$EXTERNALSYM TWFE_PHOTO}

{ ICAP_IMAGEFILEFORMAT values }
  TWFF_TIFF         = 0;  { Tagged Image File Format }
  {$EXTERNALSYM TWFF_TIFF}
  TWFF_PICT         = 1;  { Macintosh PICT }
  {$EXTERNALSYM TWFF_PICT}
  TWFF_BMP          = 2;  { Windows Bitmap }
  {$EXTERNALSYM TWFF_BMP}
  TWFF_XBM          = 3;  { X-Windows Bitmap }
  {$EXTERNALSYM TWFF_XBM}
  TWFF_JFIF         = 4;  { JPEG File Interchange Format }
  {$EXTERNALSYM TWFF_JFIF}
  TWFF_FPX          = 5;  { Flash Pix }
  {$EXTERNALSYM TWFF_FPX}
  TWFF_TIFFMULTI    = 6;  { Multi-page tiff file }
  {$EXTERNALSYM TWFF_TIFFMULTI}
  TWFF_PNG          = 7;
  {$EXTERNALSYM TWFF_PNG}
  TWFF_SPIFF        = 8;
  {$EXTERNALSYM TWFF_SPIFF}
  TWFF_EXIF         = 9;
  {$EXTERNALSYM TWFF_EXIF}
  TWFF_PDF          = 10; { 1.91 NB: this is not PDF/A }
  {$EXTERNALSYM TWFF_PDF}
  TWFF_JP2          = 11; { 1.91 }
  {$EXTERNALSYM TWFF_JP2}
  TWFF_JPX          = 13; { 1.91 }
  {$EXTERNALSYM TWFF_JPX}
  TWFF_DEJAVU       = 14; { 1.91 }
  {$EXTERNALSYM TWFF_DEJAVU}
  TWFF_PDFA         = 15; { 2.0 }
  {$EXTERNALSYM TWFF_PDFA}
  TWFF_PDFA2        = 16; { 2.1 Adobe PDF/A, Version 2 }
  {$EXTERNALSYM TWFF_PDFA2}

{ ICAP_FLASHUSED2 values }
  TWFL_NONE         = 0;
  {$EXTERNALSYM TWFL_NONE}
  TWFL_OFF          = 1;
  {$EXTERNALSYM TWFL_OFF}
  TWFL_ON           = 2;
  {$EXTERNALSYM TWFL_ON}
  TWFL_AUTO         = 3;
  {$EXTERNALSYM TWFL_AUTO}
  TWFL_REDEYE       = 4;
  {$EXTERNALSYM TWFL_REDEYE}

{ CAP_FEEDERORDER values }
  TWFO_FIRSTPAGEFIRST = 0;
  {$EXTERNALSYM TWFO_FIRSTPAGEFIRST}
  TWFO_LASTPAGEFIRST  = 1;
  {$EXTERNALSYM TWFO_LASTPAGEFIRST}

{ CAP_FEEDERPOCKET values }
  TWFP_POCKETERROR    = 0;
  {$EXTERNALSYM TWFP_POCKETERROR}
  TWFP_POCKET1        = 1;
  {$EXTERNALSYM TWFP_POCKET1}
  TWFP_POCKET2        = 2;
  {$EXTERNALSYM TWFP_POCKET2}
  TWFP_POCKET3        = 3;
  {$EXTERNALSYM TWFP_POCKET3}
  TWFP_POCKET4        = 4;
  {$EXTERNALSYM TWFP_POCKET4}
  TWFP_POCKET5        = 5;
  {$EXTERNALSYM TWFP_POCKET5}
  TWFP_POCKET6        = 6;
  {$EXTERNALSYM TWFP_POCKET6}
  TWFP_POCKET7        = 7;
  {$EXTERNALSYM TWFP_POCKET7}
  TWFP_POCKET8        = 8;
  {$EXTERNALSYM TWFP_POCKET8}
  TWFP_POCKET9        = 9;
  {$EXTERNALSYM TWFP_POCKET9}
  TWFP_POCKET10       = 10;
  {$EXTERNALSYM TWFP_POCKET10}
  TWFP_POCKET11       = 11;
  {$EXTERNALSYM TWFP_POCKET11}
  TWFP_POCKET12       = 12;
  {$EXTERNALSYM TWFP_POCKET12}
  TWFP_POCKET13       = 13;
  {$EXTERNALSYM TWFP_POCKET13}
  TWFP_POCKET14       = 14;
  {$EXTERNALSYM TWFP_POCKET14}
  TWFP_POCKET15       = 15;
  {$EXTERNALSYM TWFP_POCKET15}
  TWFP_POCKET16       = 16;
  {$EXTERNALSYM TWFP_POCKET16}

{ ICAP_FLIPROTATION values }
  TWFR_BOOK           = 0;
  {$EXTERNALSYM TWFR_BOOK}
  TWFR_FANFOLD        = 1;
  {$EXTERNALSYM TWFR_FANFOLD}

{ ICAP_FILTER values }
  TWFT_RED            = 0;
  {$EXTERNALSYM TWFT_RED}
  TWFT_GREEN          = 1;
  {$EXTERNALSYM TWFT_GREEN}
  TWFT_BLUE           = 2;
  {$EXTERNALSYM TWFT_BLUE}
  TWFT_NONE           = 3;
  {$EXTERNALSYM TWFT_NONE}
  TWFT_WHITE          = 4;
  {$EXTERNALSYM TWFT_WHITE}
  TWFT_CYAN           = 5;
  {$EXTERNALSYM TWFT_CYAN}
  TWFT_MAGENTA        = 6;
  {$EXTERNALSYM TWFT_MAGENTA}
  TWFT_YELLOW         = 7;
  {$EXTERNALSYM TWFT_YELLOW}
  TWFT_BLACK          = 8;
  {$EXTERNALSYM TWFT_BLACK}

{ TW_FILESYSTEM.FileType values }
  TWFY_CAMERA         = 0;
  {$EXTERNALSYM TWFY_CAMERA}
  TWFY_CAMERATOP      = 1;
  {$EXTERNALSYM TWFY_CAMERATOP}
  TWFY_CAMERABOTTOM   = 2;
  {$EXTERNALSYM TWFY_CAMERABOTTOM}
  TWFY_CAMERAPREVIEW  = 3;
  {$EXTERNALSYM TWFY_CAMERAPREVIEW}
  TWFY_DOMAIN         = 4;
  {$EXTERNALSYM TWFY_DOMAIN}
  TWFY_HOST           = 5;
  {$EXTERNALSYM TWFY_HOST}
  TWFY_DIRECTORY      = 6;
  {$EXTERNALSYM TWFY_DIRECTORY}
  TWFY_IMAGE          = 7;
  {$EXTERNALSYM TWFY_IMAGE}
  TWFY_UNKNOWN        = 8;
  {$EXTERNALSYM TWFY_UNKNOWN}

{ ICAP_ICCPROFILE values }
  TWIC_NONE           = 0;
  {$EXTERNALSYM TWIC_NONE}
  TWIC_LINK           = 1;
  {$EXTERNALSYM TWIC_LINK}
  TWIC_EMBED          = 2;
  {$EXTERNALSYM TWIC_EMBED}

{ ICAP_IMAGEFILTER values }
  TWIF_NONE           = 0;
  {$EXTERNALSYM TWIF_NONE}
  TWIF_AUTO           = 1;
  {$EXTERNALSYM TWIF_AUTO}
  TWIF_LOWPASS        = 2;
  {$EXTERNALSYM TWIF_LOWPASS}
  TWIF_BANDPASS       = 3;
  {$EXTERNALSYM TWIF_BANDPASS}
  TWIF_HIGHPASS       = 4;
  {$EXTERNALSYM TWIF_HIGHPASS}
  TWIF_TEXT           = TWIF_BANDPASS;
  {$EXTERNALSYM TWIF_TEXT}
  TWIF_FINELINE       = TWIF_HIGHPASS;
  {$EXTERNALSYM TWIF_FINELINE}

{ ICAP_IMAGEMERGE values }
  TWIM_NONE           = 0;
  {$EXTERNALSYM TWIM_NONE}
  TWIM_FRONTONTOP     = 1;
  {$EXTERNALSYM TWIM_FRONTONTOP}
  TWIM_FRONTONBOTTOM  = 2;
  {$EXTERNALSYM TWIM_FRONTONBOTTOM}
  TWIM_FRONTONLEFT    = 3;
  {$EXTERNALSYM TWIM_FRONTONLEFT}
  TWIM_FRONTONRIGHT   = 4;
  {$EXTERNALSYM TWIM_FRONTONRIGHT}

{ CAP_JOBCONTROL values }
  TWJC_NONE           = 0;
  {$EXTERNALSYM TWJC_NONE}
  TWJC_JSIC           = 1;
  {$EXTERNALSYM TWJC_JSIC}
  TWJC_JSIS           = 2;
  {$EXTERNALSYM TWJC_JSIS}
  TWJC_JSXC           = 3;
  {$EXTERNALSYM TWJC_JSXC}
  TWJC_JSXS           = 4;
  {$EXTERNALSYM TWJC_JSXS}

{ ICAP_JPEGQUALITY values }
  TWJQ_UNKNOWN        = -4;
  {$EXTERNALSYM TWJQ_UNKNOWN}
  TWJQ_LOW            = -3;
  {$EXTERNALSYM TWJQ_LOW}
  TWJQ_MEDIUM         = -2;
  {$EXTERNALSYM TWJQ_MEDIUM}
  TWJQ_HIGH           = -1;
  {$EXTERNALSYM TWJQ_HIGH}

{ ICAP_LIGHTPATH values }
  TWLP_REFLECTIVE     = 0;
  {$EXTERNALSYM TWLP_REFLECTIVE}
  TWLP_TRANSMISSIVE   = 1;
  {$EXTERNALSYM TWLP_TRANSMISSIVE}

{ ICAP_LIGHTSOURCE values }
  TWLS_RED            = 0;
  {$EXTERNALSYM TWLS_RED}
  TWLS_GREEN          = 1;
  {$EXTERNALSYM TWLS_GREEN}
  TWLS_BLUE           = 2;
  {$EXTERNALSYM TWLS_BLUE}
  TWLS_NONE           = 3;
  {$EXTERNALSYM TWLS_NONE}
  TWLS_WHITE          = 4;
  {$EXTERNALSYM TWLS_WHITE}
  TWLS_UV             = 5;
  {$EXTERNALSYM TWLS_UV}
  TWLS_IR             = 6;
  {$EXTERNALSYM TWLS_IR}

{ TWEI_MAGTYPE values }
  TWMD_MICR           = 0; { Added 2.0 }
  {$EXTERNALSYM TWMD_MICR}
  TWMD_RAW            = 1; { added 2.1 }
  {$EXTERNALSYM TWMD_RAW}
  TWMD_INVALID        = 2; { added 2.1 }
  {$EXTERNALSYM TWMD_INVALID}

{ ICAP_NOISEFILTER values }
  TWNF_NONE           = 0;
  {$EXTERNALSYM TWNF_NONE}
  TWNF_AUTO           = 1;
  {$EXTERNALSYM TWNF_AUTO}
  TWNF_LONEPIXEL      = 2;
  {$EXTERNALSYM TWNF_LONEPIXEL}
  TWNF_MAJORITYRULE   = 3;
  {$EXTERNALSYM TWNF_MAJORITYRULE}

{ ICAP_ORIENTATION values }
  TWOR_ROT0           = 0;
  {$EXTERNALSYM TWOR_ROT0}
  TWOR_ROT90          = 1;
  {$EXTERNALSYM TWOR_ROT90}
  TWOR_ROT180         = 2;
  {$EXTERNALSYM TWOR_ROT180}
  TWOR_ROT270         = 3;
  {$EXTERNALSYM TWOR_ROT270}
  TWOR_PORTRAIT       = TWOR_ROT0;
  {$EXTERNALSYM TWOR_PORTRAIT}
  TWOR_LANDSCAPE      = TWOR_ROT270;
  {$EXTERNALSYM TWOR_LANDSCAPE}
  TWOR_AUTO           = 4;           { 2.0 }
  {$EXTERNALSYM TWOR_AUTO}
  TWOR_AUTOTEXT       = 5;           { 2.0 }
  {$EXTERNALSYM TWOR_AUTOTEXT}
  TWOR_AUTOPICTURE    = 6;           { 2.0 }
  {$EXTERNALSYM TWOR_AUTOPICTURE}

{ ICAP_OVERSCAN values (OV_ means overscan) }
  TWOV_NONE           = 0;
  {$EXTERNALSYM TWOV_NONE}
  TWOV_AUTO           = 1;
  {$EXTERNALSYM TWOV_AUTO}
  TWOV_TOPBOTTOM      = 2;
  {$EXTERNALSYM TWOV_TOPBOTTOM}
  TWOV_LEFTRIGHT      = 3;
  {$EXTERNALSYM TWOV_LEFTRIGHT}
  TWOV_ALL            = 4;
  {$EXTERNALSYM TWOV_ALL}

{ Palette types for TW_PALETTE8 }
  TWPA_RGB            = 0;
  {$EXTERNALSYM TWPA_RGB}
  TWPA_GRAY           = 1;
  {$EXTERNALSYM TWPA_GRAY}
  TWPA_CMY            = 2;
  {$EXTERNALSYM TWPA_CMY}

{ ICAP_PLANARCHUNKY values }
  TWPC_CHUNKY         = 0;
  {$EXTERNALSYM TWPC_CHUNKY}
  TWPC_PLANAR         = 1;
  {$EXTERNALSYM TWPC_PLANAR}

{ TWEI_PATCHCODE values }
  TWPCH_PATCH1        = 0;
  {$EXTERNALSYM TWPCH_PATCH1}
  TWPCH_PATCH2        = 1;
  {$EXTERNALSYM TWPCH_PATCH2}
  TWPCH_PATCH3        = 2;
  {$EXTERNALSYM TWPCH_PATCH3}
  TWPCH_PATCH4        = 3;
  {$EXTERNALSYM TWPCH_PATCH4}
  TWPCH_PATCH6        = 4;
  {$EXTERNALSYM TWPCH_PATCH6}
  TWPCH_PATCHT        = 5;
  {$EXTERNALSYM TWPCH_PATCHT}

{ ICAP_PIXELFLAVOR values }
  TWPF_CHOCOLATE      = 0; { zero pixel represents darkest shade }
  {$EXTERNALSYM TWPF_CHOCOLATE}
  TWPF_VANILLA        = 1; { zero pixel represents lightest shade }
  {$EXTERNALSYM TWPF_VANILLA}

{ CAP_PRINTERMODE values }
  TWPM_SINGLESTRING   = 0;
  {$EXTERNALSYM TWPM_SINGLESTRING}
  TWPM_MULTISTRING    = 1;
  {$EXTERNALSYM TWPM_MULTISTRING}
  TWPM_COMPOUNDSTRING = 2;
  {$EXTERNALSYM TWPM_COMPOUNDSTRING}

{ CAP_PRINTER values }
  TWPR_IMPRINTERTOPBEFORE    = 0;
  {$EXTERNALSYM TWPR_IMPRINTERTOPBEFORE}
  TWPR_IMPRINTERTOPAFTER     = 1;
  {$EXTERNALSYM TWPR_IMPRINTERTOPAFTER}
  TWPR_IMPRINTERBOTTOMBEFORE = 2;
  {$EXTERNALSYM TWPR_IMPRINTERBOTTOMBEFORE}
  TWPR_IMPRINTERBOTTOMAFTER  = 3;
  {$EXTERNALSYM TWPR_IMPRINTERBOTTOMAFTER}
  TWPR_ENDORSERTOPBEFORE     = 4;
  {$EXTERNALSYM TWPR_ENDORSERTOPBEFORE}
  TWPR_ENDORSERTOPAFTER      = 5;
  {$EXTERNALSYM TWPR_ENDORSERTOPAFTER}
  TWPR_ENDORSERBOTTOMBEFORE  = 6;
  {$EXTERNALSYM TWPR_ENDORSERBOTTOMBEFORE}
  TWPR_ENDORSERBOTTOMAFTER   = 7;
  {$EXTERNALSYM TWPR_ENDORSERBOTTOMAFTER}

{ CAP_POWERSUPPLY values }
  TWPS_EXTERNAL       = 0;
  {$EXTERNALSYM TWPS_EXTERNAL}
  TWPS_BATTERY        = 1;
  {$EXTERNALSYM TWPS_BATTERY}

{ ICAP_PIXELTYPE values }
  TWPT_BW             = 0; { Black and White }
  {$EXTERNALSYM TWPT_BW}
  TWPT_GRAY           = 1;
  {$EXTERNALSYM TWPT_GRAY}
  TWPT_RGB            = 2;
  {$EXTERNALSYM TWPT_RGB}
  TWPT_PALETTE        = 3;
  {$EXTERNALSYM TWPT_PALETTE}
  TWPT_CMY            = 4;
  {$EXTERNALSYM TWPT_CMY}
  TWPT_CMYK           = 5;
  {$EXTERNALSYM TWPT_CMYK}
  TWPT_YUV            = 6;
  {$EXTERNALSYM TWPT_YUV}
  TWPT_YUVK           = 7;
  {$EXTERNALSYM TWPT_YUVK}
  TWPT_CIEXYZ         = 8;
  {$EXTERNALSYM TWPT_CIEXYZ}
  TWPT_LAB            = 9;
  {$EXTERNALSYM TWPT_LAB}
  TWPT_SRGB           = 10; { 1.91 }
  {$EXTERNALSYM TWPT_SRGB}
  TWPT_SCRGB          = 11; { 1.91 }
  {$EXTERNALSYM TWPT_SCRGB}
  TWPT_INFRARED       = 16; { 2.0 }
  {$EXTERNALSYM TWPT_INFRARED}

{ CAP_SEGMENTED values (SG_ means segmented) Added 1.91 }
  TWSG_NONE           = 0;
  {$EXTERNALSYM TWSG_NONE}
  TWSG_AUTO           = 1;
  {$EXTERNALSYM TWSG_AUTO}
  TWSG_MANUAL         = 2;
  {$EXTERNALSYM TWSG_MANUAL}

{ ICAP_FILMTYPE values }
  TWFM_POSITIVE     = 0;
  {$EXTERNALSYM TWFM_POSITIVE}
  TWFM_NEGATIVE     = 1;
  {$EXTERNALSYM TWFM_NEGATIVE}

{ CAP_DOUBLEFEEDDETECTION }
  TWDF_ULTRASONIC   = 0;
  {$EXTERNALSYM TWDF_ULTRASONIC}
  TWDF_BYLENGTH     = 1;
  {$EXTERNALSYM TWDF_BYLENGTH}
  TWDF_INFRARED     = 2;
  {$EXTERNALSYM TWDF_INFRARED}

{ CAP_DOUBLEFEEDDETECTIONSENSITIVITY }
  TWUS_LOW      = 0;
  {$EXTERNALSYM TWUS_LOW}
  TWUS_MEDIUM   = 1;
  {$EXTERNALSYM TWUS_MEDIUM}
  TWUS_HIGH     = 2;
  {$EXTERNALSYM TWUS_HIGH}

{ CAP_DOUBLEFEEDDETECTIONRESPONSE }
  TWDP_STOP         = 0;
  {$EXTERNALSYM TWDP_STOP}
  TWDP_STOPANDWAIT  = 1;
  {$EXTERNALSYM TWDP_STOPANDWAIT}
  TWDP_SOUND        = 2;
  {$EXTERNALSYM TWDP_SOUND}
  TWDP_DONOTIMPRINT = 3;
  {$EXTERNALSYM TWDP_DONOTIMPRINT}

{ ICAP_MIRROR values }
  TWMR_NONE         = 0;
  {$EXTERNALSYM TWMR_NONE}
  TWMR_VERTICAL     = 1;
  {$EXTERNALSYM TWMR_VERTICAL}
  TWMR_HORIZONTAL   = 2;
  {$EXTERNALSYM TWMR_HORIZONTAL}

{ ICAP_JPEGSUBSAMPLING values }
  TWJS_444YCBCR   = 0;
  {$EXTERNALSYM TWJS_444YCBCR}
  TWJS_444RGB     = 1;
  {$EXTERNALSYM TWJS_444RGB}
  TWJS_422        = 2;
  {$EXTERNALSYM TWJS_422}
  TWJS_421        = 3;
  {$EXTERNALSYM TWJS_421}
  TWJS_411        = 4;
  {$EXTERNALSYM TWJS_411}
  TWJS_420        = 5;
  {$EXTERNALSYM TWJS_420}
  TWJS_410        = 6;
  {$EXTERNALSYM TWJS_410}
  TWJS_311        = 7;
  {$EXTERNALSYM TWJS_311}

{ CAP_PAPERHANDLING values }
  TWPH_NORMAL       = 0;
  {$EXTERNALSYM TWPH_NORMAL}
  TWPH_FRAGILE      = 1;
  {$EXTERNALSYM TWPH_FRAGILE}
  TWPH_THICK        = 2;
  {$EXTERNALSYM TWPH_THICK}
  TWPH_TRIFOLD      = 3;
  {$EXTERNALSYM TWPH_TRIFOLD}
  TWPH_PHOTOGRAPH   = 4;
  {$EXTERNALSYM TWPH_PHOTOGRAPH}

{ CAP_INDICATORSMODE values }
  TWCI_INFO     = 0;
  {$EXTERNALSYM TWCI_INFO}
  TWCI_WARNING  = 1;
  {$EXTERNALSYM TWCI_WARNING}
  TWCI_ERROR    = 2;
  {$EXTERNALSYM TWCI_ERROR}
  TWCI_WARMUP   = 3;
  {$EXTERNALSYM TWCI_WARMUP}

{ ICAP_SUPPORTEDSIZES values }
  TWSS_NONE           = 0;
  {$EXTERNALSYM TWSS_NONE}
  TWSS_A4             = 1;
  {$EXTERNALSYM TWSS_A4}
  TWSS_JISB5          = 2;
  {$EXTERNALSYM TWSS_JISB5}
  TWSS_USLETTER       = 3;
  {$EXTERNALSYM TWSS_USLETTER}
  TWSS_USLEGAL        = 4;
  {$EXTERNALSYM TWSS_USLEGAL}
{ Added 1.5 }
  TWSS_A5             = 5;
  {$EXTERNALSYM TWSS_A5}
  TWSS_ISOB4          = 6;
  {$EXTERNALSYM TWSS_ISOB4}
  TWSS_ISOB6          = 7;
  {$EXTERNALSYM TWSS_ISOB6}
{ Added 1.7 }
  TWSS_USLEDGER       = 9;
  {$EXTERNALSYM TWSS_USLEDGER}
  TWSS_USEXECUTIVE    = 10;
  {$EXTERNALSYM TWSS_USEXECUTIVE}
  TWSS_A3             = 11;
  {$EXTERNALSYM TWSS_A3}
  TWSS_ISOB3          = 12;
  {$EXTERNALSYM TWSS_ISOB3}
  TWSS_A6             = 13;
  {$EXTERNALSYM TWSS_A6}
  TWSS_C4             = 14;
  {$EXTERNALSYM TWSS_C4}
  TWSS_C5             = 15;
  {$EXTERNALSYM TWSS_C5}
  TWSS_C6             = 16;
  {$EXTERNALSYM TWSS_C6}
{ Added 1.8 }
  TWSS_4A0            = 17;
  {$EXTERNALSYM TWSS_4A0}
  TWSS_2A0            = 18;
  {$EXTERNALSYM TWSS_2A0}
  TWSS_A0             = 19;
  {$EXTERNALSYM TWSS_A0}
  TWSS_A1             = 20;
  {$EXTERNALSYM TWSS_A1}
  TWSS_A2             = 21;
  {$EXTERNALSYM TWSS_A2}
  TWSS_A7             = 22;
  {$EXTERNALSYM TWSS_A7}
  TWSS_A8             = 23;
  {$EXTERNALSYM TWSS_A8}
  TWSS_A9             = 24;
  {$EXTERNALSYM TWSS_A9}
  TWSS_A10            = 25;
  {$EXTERNALSYM TWSS_A10}
  TWSS_ISOB0          = 26;
  {$EXTERNALSYM TWSS_ISOB0}
  TWSS_ISOB1          = 27;
  {$EXTERNALSYM TWSS_ISOB1}
  TWSS_ISOB2          = 28;
  {$EXTERNALSYM TWSS_ISOB2}
  TWSS_ISOB5          = 29;
  {$EXTERNALSYM TWSS_ISOB5}
  TWSS_ISOB7          = 30;
  {$EXTERNALSYM TWSS_ISOB7}
  TWSS_ISOB8          = 31;
  {$EXTERNALSYM TWSS_ISOB8}
  TWSS_ISOB9          = 32;
  {$EXTERNALSYM TWSS_ISOB9}
  TWSS_ISOB10         = 33;
  {$EXTERNALSYM TWSS_ISOB10}
  TWSS_JISB0          = 34;
  {$EXTERNALSYM TWSS_JISB0}
  TWSS_JISB1          = 35;
  {$EXTERNALSYM TWSS_JISB1}
  TWSS_JISB2          = 36;
  {$EXTERNALSYM TWSS_JISB2}
  TWSS_JISB3          = 37;
  {$EXTERNALSYM TWSS_JISB3}
  TWSS_JISB4          = 38;
  {$EXTERNALSYM TWSS_JISB4}
  TWSS_JISB6          = 39;
  {$EXTERNALSYM TWSS_JISB6}
  TWSS_JISB7          = 40;
  {$EXTERNALSYM TWSS_JISB7}
  TWSS_JISB8          = 41;
  {$EXTERNALSYM TWSS_JISB8}
  TWSS_JISB9          = 42;
  {$EXTERNALSYM TWSS_JISB9}
  TWSS_JISB10         = 43;
  {$EXTERNALSYM TWSS_JISB10}
  TWSS_C0             = 44;
  {$EXTERNALSYM TWSS_C0}
  TWSS_C1             = 45;
  {$EXTERNALSYM TWSS_C1}
  TWSS_C2             = 46;
  {$EXTERNALSYM TWSS_C2}
  TWSS_C3             = 47;
  {$EXTERNALSYM TWSS_C3}
  TWSS_C7             = 48;
  {$EXTERNALSYM TWSS_C7}
  TWSS_C8             = 49;
  {$EXTERNALSYM TWSS_C8}
  TWSS_C9             = 50;
  {$EXTERNALSYM TWSS_C9}
  TWSS_C10            = 51;
  {$EXTERNALSYM TWSS_C10}
  TWSS_USSTATEMENT    = 52;
  {$EXTERNALSYM TWSS_USSTATEMENT}
  TWSS_BUSINESSCARD   = 53;
  {$EXTERNALSYM TWSS_BUSINESSCARD}
  TWSS_MAXSIZE        = 54;  { Added 2.1 }
  {$EXTERNALSYM TWSS_MAXSIZE}

{ ICAP_XFERMECH values (SX_ means Setup XFer) }
  TWSX_NATIVE         = 0;
  {$EXTERNALSYM TWSX_NATIVE}
  TWSX_FILE           = 1;
  {$EXTERNALSYM TWSX_FILE}
  TWSX_MEMORY         = 2;
  {$EXTERNALSYM TWSX_MEMORY}
  TWSX_MEMFILE        = 4;  { added 1.91 }
  {$EXTERNALSYM TWSX_MEMFILE}

{ ICAP_UNITS values (UN_ means UNits) }
  TWUN_INCHES         = 0;
  {$EXTERNALSYM TWUN_INCHES}
  TWUN_CENTIMETERS    = 1;
  {$EXTERNALSYM TWUN_CENTIMETERS}
  TWUN_PICAS          = 2;
  {$EXTERNALSYM TWUN_PICAS}
  TWUN_POINTS         = 3;
  {$EXTERNALSYM TWUN_POINTS}
  TWUN_TWIPS          = 4;
  {$EXTERNALSYM TWUN_TWIPS}
  TWUN_PIXELS         = 5;
  {$EXTERNALSYM TWUN_PIXELS}
  TWUN_MILLIMETERS    = 6;    { added 1.91 }
  {$EXTERNALSYM TWUN_MILLIMETERS}

{***************************************************************************
 * Country Constants                                                       *
 ***************************************************************************}

  TWCY_AFGHANISTAN    =  1001;
  {$EXTERNALSYM TWCY_AFGHANISTAN}
  TWCY_ALGERIA        =   213;
  {$EXTERNALSYM TWCY_ALGERIA}
  TWCY_AMERICANSAMOA  =   684;
  {$EXTERNALSYM TWCY_AMERICANSAMOA}
  TWCY_ANDORRA        =   033;
  {$EXTERNALSYM TWCY_ANDORRA}
  TWCY_ANGOLA         =  1002;
  {$EXTERNALSYM TWCY_ANGOLA}
  TWCY_ANGUILLA       =  8090;
  {$EXTERNALSYM TWCY_ANGUILLA}
  TWCY_ANTIGUA        =  8091;
  {$EXTERNALSYM TWCY_ANTIGUA}
  TWCY_ARGENTINA      =    54;
  {$EXTERNALSYM TWCY_ARGENTINA}
  TWCY_ARUBA          =   297;
  {$EXTERNALSYM TWCY_ARUBA}
  TWCY_ASCENSIONI     =   247;
  {$EXTERNALSYM TWCY_ASCENSIONI}
  TWCY_AUSTRALIA      =    61;
  {$EXTERNALSYM TWCY_AUSTRALIA}
  TWCY_AUSTRIA        =    43;
  {$EXTERNALSYM TWCY_AUSTRIA}
  TWCY_BAHAMAS        =  8092;
  {$EXTERNALSYM TWCY_BAHAMAS}
  TWCY_BAHRAIN        =   973;
  {$EXTERNALSYM TWCY_BAHRAIN}
  TWCY_BANGLADESH     =   880;
  {$EXTERNALSYM TWCY_BANGLADESH}
  TWCY_BARBADOS       =  8093;
  {$EXTERNALSYM TWCY_BARBADOS}
  TWCY_BELGIUM        =    32;
  {$EXTERNALSYM TWCY_BELGIUM}
  TWCY_BELIZE         =   501;
  {$EXTERNALSYM TWCY_BELIZE}
  TWCY_BENIN          =   229;
  {$EXTERNALSYM TWCY_BENIN}
  TWCY_BERMUDA        =  8094;
  {$EXTERNALSYM TWCY_BERMUDA}
  TWCY_BHUTAN         =  1003;
  {$EXTERNALSYM TWCY_BHUTAN}
  TWCY_BOLIVIA        =   591;
  {$EXTERNALSYM TWCY_BOLIVIA}
  TWCY_BOTSWANA       =   267;
  {$EXTERNALSYM TWCY_BOTSWANA}
  TWCY_BRITAIN        =     6;
  {$EXTERNALSYM TWCY_BRITAIN}
  TWCY_BRITVIRGINIS   =  8095;
  {$EXTERNALSYM TWCY_BRITVIRGINIS}
  TWCY_BRAZIL         =    55;
  {$EXTERNALSYM TWCY_BRAZIL}
  TWCY_BRUNEI         =   673;
  {$EXTERNALSYM TWCY_BRUNEI}
  TWCY_BULGARIA       =   359;
  {$EXTERNALSYM TWCY_BULGARIA}
  TWCY_BURKINAFASO    =  1004;
  {$EXTERNALSYM TWCY_BURKINAFASO}
  TWCY_BURMA          =  1005;
  {$EXTERNALSYM TWCY_BURMA}
  TWCY_BURUNDI        =  1006;
  {$EXTERNALSYM TWCY_BURUNDI}
  TWCY_CAMAROON       =   237;
  {$EXTERNALSYM TWCY_CAMAROON}
  TWCY_CANADA         =     2;
  {$EXTERNALSYM TWCY_CANADA}
  TWCY_CAPEVERDEIS    =   238;
  {$EXTERNALSYM TWCY_CAPEVERDEIS}
  TWCY_CAYMANIS       =  8096;
  {$EXTERNALSYM TWCY_CAYMANIS}
  TWCY_CENTRALAFREP   =  1007;
  {$EXTERNALSYM TWCY_CENTRALAFREP}
  TWCY_CHAD           =  1008;
  {$EXTERNALSYM TWCY_CHAD}
  TWCY_CHILE          =    56;
  {$EXTERNALSYM TWCY_CHILE}
  TWCY_CHINA          =    86;
  {$EXTERNALSYM TWCY_CHINA}
  TWCY_CHRISTMASIS    =  1009;
  {$EXTERNALSYM TWCY_CHRISTMASIS}
  TWCY_COCOSIS        =  1009;
  {$EXTERNALSYM TWCY_COCOSIS}
  TWCY_COLOMBIA       =    57;
  {$EXTERNALSYM TWCY_COLOMBIA}
  TWCY_COMOROS        =  1010;
  {$EXTERNALSYM TWCY_COMOROS}
  TWCY_CONGO          =  1011;
  {$EXTERNALSYM TWCY_CONGO}
  TWCY_COOKIS         =  1012;
  {$EXTERNALSYM TWCY_COOKIS}
  TWCY_COSTARICA      =   506;
  {$EXTERNALSYM TWCY_COSTARICA}
  TWCY_CUBA           =   005;
  {$EXTERNALSYM TWCY_CUBA}
  TWCY_CYPRUS         =   357;
  {$EXTERNALSYM TWCY_CYPRUS}
  TWCY_CZECHOSLOVAKIA =    42;
  {$EXTERNALSYM TWCY_CZECHOSLOVAKIA}
  TWCY_DENMARK        =    45;
  {$EXTERNALSYM TWCY_DENMARK}
  TWCY_DJIBOUTI       =  1013;
  {$EXTERNALSYM TWCY_DJIBOUTI}
  TWCY_DOMINICA       =  8097;
  {$EXTERNALSYM TWCY_DOMINICA}
  TWCY_DOMINCANREP    =  8098;
  {$EXTERNALSYM TWCY_DOMINCANREP}
  TWCY_EASTERIS       =  1014;
  {$EXTERNALSYM TWCY_EASTERIS}
  TWCY_ECUADOR        =   593;
  {$EXTERNALSYM TWCY_ECUADOR}
  TWCY_EGYPT          =    20;
  {$EXTERNALSYM TWCY_EGYPT}
  TWCY_ELSALVADOR     =   503;
  {$EXTERNALSYM TWCY_ELSALVADOR}
  TWCY_EQGUINEA       =  1015;
  {$EXTERNALSYM TWCY_EQGUINEA}
  TWCY_ETHIOPIA       =   251;
  {$EXTERNALSYM TWCY_ETHIOPIA}
  TWCY_FALKLANDIS     =  1016;
  {$EXTERNALSYM TWCY_FALKLANDIS}
  TWCY_FAEROEIS       =   298;
  {$EXTERNALSYM TWCY_FAEROEIS}
  TWCY_FIJIISLANDS    =   679;
  {$EXTERNALSYM TWCY_FIJIISLANDS}
  TWCY_FINLAND        =   358;
  {$EXTERNALSYM TWCY_FINLAND}
  TWCY_FRANCE         =    33;
  {$EXTERNALSYM TWCY_FRANCE}
  TWCY_FRANTILLES     =   596;
  {$EXTERNALSYM TWCY_FRANTILLES}
  TWCY_FRGUIANA       =   594;
  {$EXTERNALSYM TWCY_FRGUIANA}
  TWCY_FRPOLYNEISA    =   689;
  {$EXTERNALSYM TWCY_FRPOLYNEISA}
  TWCY_FUTANAIS       =  1043;
  {$EXTERNALSYM TWCY_FUTANAIS}
  TWCY_GABON          =   241;
  {$EXTERNALSYM TWCY_GABON}
  TWCY_GAMBIA         =   220;
  {$EXTERNALSYM TWCY_GAMBIA}
  TWCY_GERMANY        =    49;
  {$EXTERNALSYM TWCY_GERMANY}
  TWCY_GHANA          =   233;
  {$EXTERNALSYM TWCY_GHANA}
  TWCY_GIBRALTER      =   350;
  {$EXTERNALSYM TWCY_GIBRALTER}
  TWCY_GREECE         =    30;
  {$EXTERNALSYM TWCY_GREECE}
  TWCY_GREENLAND      =   299;
  {$EXTERNALSYM TWCY_GREENLAND}
  TWCY_GRENADA        =  8099;
  {$EXTERNALSYM TWCY_GRENADA}
  TWCY_GRENEDINES     =  8015;
  {$EXTERNALSYM TWCY_GRENEDINES}
  TWCY_GUADELOUPE     =   590;
  {$EXTERNALSYM TWCY_GUADELOUPE}
  TWCY_GUAM           =   671;
  {$EXTERNALSYM TWCY_GUAM}
  TWCY_GUANTANAMOBAY  =  5399;
  {$EXTERNALSYM TWCY_GUANTANAMOBAY}
  TWCY_GUATEMALA      =   502;
  {$EXTERNALSYM TWCY_GUATEMALA}
  TWCY_GUINEA         =   224;
  {$EXTERNALSYM TWCY_GUINEA}
  TWCY_GUINEABISSAU   =  1017;
  {$EXTERNALSYM TWCY_GUINEABISSAU}
  TWCY_GUYANA         =   592;
  {$EXTERNALSYM TWCY_GUYANA}
  TWCY_HAITI          =   509;
  {$EXTERNALSYM TWCY_HAITI}
  TWCY_HONDURAS       =   504;
  {$EXTERNALSYM TWCY_HONDURAS}
  TWCY_HONGKONG       =   852;
  {$EXTERNALSYM TWCY_HONGKONG}
  TWCY_HUNGARY        =    36;
  {$EXTERNALSYM TWCY_HUNGARY}
  TWCY_ICELAND        =   354;
  {$EXTERNALSYM TWCY_ICELAND}
  TWCY_INDIA          =    91;
  {$EXTERNALSYM TWCY_INDIA}
  TWCY_INDONESIA      =    62;
  {$EXTERNALSYM TWCY_INDONESIA}
  TWCY_IRAN           =    98;
  {$EXTERNALSYM TWCY_IRAN}
  TWCY_IRAQ           =   964;
  {$EXTERNALSYM TWCY_IRAQ}
  TWCY_IRELAND        =   353;
  {$EXTERNALSYM TWCY_IRELAND}
  TWCY_ISRAEL         =   972;
  {$EXTERNALSYM TWCY_ISRAEL}
  TWCY_ITALY          =    39;
  {$EXTERNALSYM TWCY_ITALY}
  TWCY_IVORYCOAST     =   225;
  {$EXTERNALSYM TWCY_IVORYCOAST}
  TWCY_JAMAICA        =  8010;
  {$EXTERNALSYM TWCY_JAMAICA}
  TWCY_JAPAN          =    81;
  {$EXTERNALSYM TWCY_JAPAN}
  TWCY_JORDAN         =   962;
  {$EXTERNALSYM TWCY_JORDAN}
  TWCY_KENYA          =   254;
  {$EXTERNALSYM TWCY_KENYA}
  TWCY_KIRIBATI       =  1018;
  {$EXTERNALSYM TWCY_KIRIBATI}
  TWCY_KOREA          =    82;
  {$EXTERNALSYM TWCY_KOREA}
  TWCY_KUWAIT         =   965;
  {$EXTERNALSYM TWCY_KUWAIT}
  TWCY_LAOS           =  1019;
  {$EXTERNALSYM TWCY_LAOS}
  TWCY_LEBANON        =  1020;
  {$EXTERNALSYM TWCY_LEBANON}
  TWCY_LIBERIA        =   231;
  {$EXTERNALSYM TWCY_LIBERIA}
  TWCY_LIBYA          =   218;
  {$EXTERNALSYM TWCY_LIBYA}
  TWCY_LIECHTENSTEIN  =    41;
  {$EXTERNALSYM TWCY_LIECHTENSTEIN}
  TWCY_LUXENBOURG     =   352;
  {$EXTERNALSYM TWCY_LUXENBOURG}
  TWCY_MACAO          =   853;
  {$EXTERNALSYM TWCY_MACAO}
  TWCY_MADAGASCAR     =  1021;
  {$EXTERNALSYM TWCY_MADAGASCAR}
  TWCY_MALAWI         =   265;
  {$EXTERNALSYM TWCY_MALAWI}
  TWCY_MALAYSIA       =    60;
  {$EXTERNALSYM TWCY_MALAYSIA}
  TWCY_MALDIVES       =   960;
  {$EXTERNALSYM TWCY_MALDIVES}
  TWCY_MALI           =  1022;
  {$EXTERNALSYM TWCY_MALI}
  TWCY_MALTA          =   356;
  {$EXTERNALSYM TWCY_MALTA}
  TWCY_MARSHALLIS     =   692;
  {$EXTERNALSYM TWCY_MARSHALLIS}
  TWCY_MAURITANIA     =  1023;
  {$EXTERNALSYM TWCY_MAURITANIA}
  TWCY_MAURITIUS      =   230;
  {$EXTERNALSYM TWCY_MAURITIUS}
  TWCY_MEXICO         =     3;
  {$EXTERNALSYM TWCY_MEXICO}
  TWCY_MICRONESIA     =   691;
  {$EXTERNALSYM TWCY_MICRONESIA}
  TWCY_MIQUELON       =   508;
  {$EXTERNALSYM TWCY_MIQUELON}
  TWCY_MONACO         =    33;
  {$EXTERNALSYM TWCY_MONACO}
  TWCY_MONGOLIA       =  1024;
  {$EXTERNALSYM TWCY_MONGOLIA}
  TWCY_MONTSERRAT     =  8011;
  {$EXTERNALSYM TWCY_MONTSERRAT}
  TWCY_MOROCCO        =   212;
  {$EXTERNALSYM TWCY_MOROCCO}
  TWCY_MOZAMBIQUE     =  1025;
  {$EXTERNALSYM TWCY_MOZAMBIQUE}
  TWCY_NAMIBIA        =   264;
  {$EXTERNALSYM TWCY_NAMIBIA}
  TWCY_NAURU          =  1026;
  {$EXTERNALSYM TWCY_NAURU}
  TWCY_NEPAL          =   977;
  {$EXTERNALSYM TWCY_NEPAL}
  TWCY_NETHERLANDS    =    31;
  {$EXTERNALSYM TWCY_NETHERLANDS}
  TWCY_NETHANTILLES   =   599;
  {$EXTERNALSYM TWCY_NETHANTILLES}
  TWCY_NEVIS          =  8012;
  {$EXTERNALSYM TWCY_NEVIS}
  TWCY_NEWCALEDONIA   =   687;
  {$EXTERNALSYM TWCY_NEWCALEDONIA}
  TWCY_NEWZEALAND     =    64;
  {$EXTERNALSYM TWCY_NEWZEALAND}
  TWCY_NICARAGUA      =   505;
  {$EXTERNALSYM TWCY_NICARAGUA}
  TWCY_NIGER          =   227;
  {$EXTERNALSYM TWCY_NIGER}
  TWCY_NIGERIA        =   234;
  {$EXTERNALSYM TWCY_NIGERIA}
  TWCY_NIUE           =  1027;
  {$EXTERNALSYM TWCY_NIUE}
  TWCY_NORFOLKI       =  1028;
  {$EXTERNALSYM TWCY_NORFOLKI}
  TWCY_NORWAY         =    47;
  {$EXTERNALSYM TWCY_NORWAY}
  TWCY_OMAN           =   968;
  {$EXTERNALSYM TWCY_OMAN}
  TWCY_PAKISTAN       =    92;
  {$EXTERNALSYM TWCY_PAKISTAN}
  TWCY_PALAU          =  1029;
  {$EXTERNALSYM TWCY_PALAU}
  TWCY_PANAMA         =   507;
  {$EXTERNALSYM TWCY_PANAMA}
  TWCY_PARAGUAY       =   595;
  {$EXTERNALSYM TWCY_PARAGUAY}
  TWCY_PERU           =    51;
  {$EXTERNALSYM TWCY_PERU}
  TWCY_PHILLIPPINES   =    63;
  {$EXTERNALSYM TWCY_PHILLIPPINES}
  TWCY_PITCAIRNIS     =  1030;
  {$EXTERNALSYM TWCY_PITCAIRNIS}
  TWCY_PNEWGUINEA     =   675;
  {$EXTERNALSYM TWCY_PNEWGUINEA}
  TWCY_POLAND         =    48;
  {$EXTERNALSYM TWCY_POLAND}
  TWCY_PORTUGAL       =   351;
  {$EXTERNALSYM TWCY_PORTUGAL}
  TWCY_QATAR          =   974;
  {$EXTERNALSYM TWCY_QATAR}
  TWCY_REUNIONI       =  1031;
  {$EXTERNALSYM TWCY_REUNIONI}
  TWCY_ROMANIA        =    40;
  {$EXTERNALSYM TWCY_ROMANIA}
  TWCY_RWANDA         =   250;
  {$EXTERNALSYM TWCY_RWANDA}
  TWCY_SAIPAN         =   670;
  {$EXTERNALSYM TWCY_SAIPAN}
  TWCY_SANMARINO      =    39;
  {$EXTERNALSYM TWCY_SANMARINO}
  TWCY_SAOTOME        =  1033;
  {$EXTERNALSYM TWCY_SAOTOME}
  TWCY_SAUDIARABIA    =   966;
  {$EXTERNALSYM TWCY_SAUDIARABIA}
  TWCY_SENEGAL        =   221;
  {$EXTERNALSYM TWCY_SENEGAL}
  TWCY_SEYCHELLESIS   =  1034;
  {$EXTERNALSYM TWCY_SEYCHELLESIS}
  TWCY_SIERRALEONE    =  1035;
  {$EXTERNALSYM TWCY_SIERRALEONE}
  TWCY_SINGAPORE      =    65;
  {$EXTERNALSYM TWCY_SINGAPORE}
  TWCY_SOLOMONIS      =  1036;
  {$EXTERNALSYM TWCY_SOLOMONIS}
  TWCY_SOMALI         =  1037;
  {$EXTERNALSYM TWCY_SOMALI}
  TWCY_SOUTHAFRICA    =    27;
  {$EXTERNALSYM TWCY_SOUTHAFRICA}
  TWCY_SPAIN          =    34;
  {$EXTERNALSYM TWCY_SPAIN}
  TWCY_SRILANKA       =    94;
  {$EXTERNALSYM TWCY_SRILANKA}
  TWCY_STHELENA       =  1032;
  {$EXTERNALSYM TWCY_STHELENA}
  TWCY_STKITTS        =  8013;
  {$EXTERNALSYM TWCY_STKITTS}
  TWCY_STLUCIA        =  8014;
  {$EXTERNALSYM TWCY_STLUCIA}
  TWCY_STPIERRE       =   508;
  {$EXTERNALSYM TWCY_STPIERRE}
  TWCY_STVINCENT      =  8015;
  {$EXTERNALSYM TWCY_STVINCENT}
  TWCY_SUDAN          =  1038;
  {$EXTERNALSYM TWCY_SUDAN}
  TWCY_SURINAME       =   597;
  {$EXTERNALSYM TWCY_SURINAME}
  TWCY_SWAZILAND      =   268;
  {$EXTERNALSYM TWCY_SWAZILAND}
  TWCY_SWEDEN         =    46;
  {$EXTERNALSYM TWCY_SWEDEN}
  TWCY_SWITZERLAND    =    41;
  {$EXTERNALSYM TWCY_SWITZERLAND}
  TWCY_SYRIA          =  1039;
  {$EXTERNALSYM TWCY_SYRIA}
  TWCY_TAIWAN         =   886;
  {$EXTERNALSYM TWCY_TAIWAN}
  TWCY_TANZANIA       =   255;
  {$EXTERNALSYM TWCY_TANZANIA}
  TWCY_THAILAND       =    66;
  {$EXTERNALSYM TWCY_THAILAND}
  TWCY_TOBAGO         =  8016;
  {$EXTERNALSYM TWCY_TOBAGO}
  TWCY_TOGO           =   228;
  {$EXTERNALSYM TWCY_TOGO}
  TWCY_TONGAIS        =   676;
  {$EXTERNALSYM TWCY_TONGAIS}
  TWCY_TRINIDAD       =  8016;
  {$EXTERNALSYM TWCY_TRINIDAD}
  TWCY_TUNISIA        =   216;
  {$EXTERNALSYM TWCY_TUNISIA}
  TWCY_TURKEY         =    90;
  {$EXTERNALSYM TWCY_TURKEY}
  TWCY_TURKSCAICOS    =  8017;
  {$EXTERNALSYM TWCY_TURKSCAICOS}
  TWCY_TUVALU         =  1040;
  {$EXTERNALSYM TWCY_TUVALU}
  TWCY_UGANDA         =   256;
  {$EXTERNALSYM TWCY_UGANDA}
  TWCY_USSR           =     7;
  {$EXTERNALSYM TWCY_USSR}
  TWCY_UAEMIRATES     =   971;
  {$EXTERNALSYM TWCY_UAEMIRATES}
  TWCY_UNITEDKINGDOM  =    44;
  {$EXTERNALSYM TWCY_UNITEDKINGDOM}
  TWCY_USA            =     1;
  {$EXTERNALSYM TWCY_USA}
  TWCY_URUGUAY        =   598;
  {$EXTERNALSYM TWCY_URUGUAY}
  TWCY_VANUATU        =  1041;
  {$EXTERNALSYM TWCY_VANUATU}
  TWCY_VATICANCITY    =    39;
  {$EXTERNALSYM TWCY_VATICANCITY}
  TWCY_VENEZUELA      =    58;
  {$EXTERNALSYM TWCY_VENEZUELA}
  TWCY_WAKE           =  1042;
  {$EXTERNALSYM TWCY_WAKE}
  TWCY_WALLISIS       =  1043;
  {$EXTERNALSYM TWCY_WALLISIS}
  TWCY_WESTERNSAHARA  =  1044;
  {$EXTERNALSYM TWCY_WESTERNSAHARA}
  TWCY_WESTERNSAMOA   =  1045;
  {$EXTERNALSYM TWCY_WESTERNSAMOA}
  TWCY_YEMEN          =  1046;
  {$EXTERNALSYM TWCY_YEMEN}
  TWCY_YUGOSLAVIA     =    38;
  {$EXTERNALSYM TWCY_YUGOSLAVIA}
  TWCY_ZAIRE          =   243;
  {$EXTERNALSYM TWCY_ZAIRE}
  TWCY_ZAMBIA         =   260;
  {$EXTERNALSYM TWCY_ZAMBIA}
  TWCY_ZIMBABWE       =   263;
  {$EXTERNALSYM TWCY_ZIMBABWE}
{  Added for 1.8  }
  TWCY_ALBANIA        =   355;
  {$EXTERNALSYM TWCY_ALBANIA}
  TWCY_ARMENIA        =   374;
  {$EXTERNALSYM TWCY_ARMENIA}
  TWCY_AZERBAIJAN     =   994;
  {$EXTERNALSYM TWCY_AZERBAIJAN}
  TWCY_BELARUS        =   375;
  {$EXTERNALSYM TWCY_BELARUS}
  TWCY_BOSNIAHERZGO   =   387;
  {$EXTERNALSYM TWCY_BOSNIAHERZGO}
  TWCY_CAMBODIA       =   855;
  {$EXTERNALSYM TWCY_CAMBODIA}
  TWCY_CROATIA        =   385;
  {$EXTERNALSYM TWCY_CROATIA}
  TWCY_CZECHREPUBLIC  =   420;
  {$EXTERNALSYM TWCY_CZECHREPUBLIC}
  TWCY_DIEGOGARCIA    =   246;
  {$EXTERNALSYM TWCY_DIEGOGARCIA}
  TWCY_ERITREA        =   291;
  {$EXTERNALSYM TWCY_ERITREA}
  TWCY_ESTONIA        =   372;
  {$EXTERNALSYM TWCY_ESTONIA}
  TWCY_GEORGIA        =   995;
  {$EXTERNALSYM TWCY_GEORGIA}
  TWCY_LATVIA         =   371;
  {$EXTERNALSYM TWCY_LATVIA}
  TWCY_LESOTHO        =   266;
  {$EXTERNALSYM TWCY_LESOTHO}
  TWCY_LITHUANIA      =   370;
  {$EXTERNALSYM TWCY_LITHUANIA}
  TWCY_MACEDONIA      =   389;
  {$EXTERNALSYM TWCY_MACEDONIA}
  TWCY_MAYOTTEIS      =   269;
  {$EXTERNALSYM TWCY_MAYOTTEIS}
  TWCY_MOLDOVA        =   373;
  {$EXTERNALSYM TWCY_MOLDOVA}
  TWCY_MYANMAR        =    95;
  {$EXTERNALSYM TWCY_MYANMAR}
  TWCY_NORTHKOREA     =   850;
  {$EXTERNALSYM TWCY_NORTHKOREA}
  TWCY_PUERTORICO     =   787;
  {$EXTERNALSYM TWCY_PUERTORICO}
  TWCY_RUSSIA         =     7;
  {$EXTERNALSYM TWCY_RUSSIA}
  TWCY_SERBIA         =   381;
  {$EXTERNALSYM TWCY_SERBIA}
  TWCY_SLOVAKIA       =   421;
  {$EXTERNALSYM TWCY_SLOVAKIA}
  TWCY_SLOVENIA       =   386;
  {$EXTERNALSYM TWCY_SLOVENIA}
  TWCY_SOUTHKOREA     =    82;
  {$EXTERNALSYM TWCY_SOUTHKOREA}
  TWCY_UKRAINE        =   380;
  {$EXTERNALSYM TWCY_UKRAINE}
  TWCY_USVIRGINIS     =   340;
  {$EXTERNALSYM TWCY_USVIRGINIS}
  TWCY_VIETNAM        =    84;
  {$EXTERNALSYM TWCY_VIETNAM}

{***************************************************************************
 * Language Constants                                                      *
 ***************************************************************************}
{  Added for 1.8  }
  TWLG_USERLOCALE               =    -1;
  {$EXTERNALSYM TWLG_USERLOCALE}
  TWLG_DAN                      =     0;  { Danish                 }
  {$EXTERNALSYM TWLG_DAN}
  TWLG_DUT                      =     1;  { Dutch                  }
  {$EXTERNALSYM TWLG_DUT}
  TWLG_ENG                      =     2;  { International English  }
  {$EXTERNALSYM TWLG_ENG}
  TWLG_FCF                      =     3;  { French Canadian        }
  {$EXTERNALSYM TWLG_FCF}
  TWLG_FIN                      =     4;  { Finnish                }
  {$EXTERNALSYM TWLG_FIN}
  TWLG_FRN                      =     5;  { French                 }
  {$EXTERNALSYM TWLG_FRN}
  TWLG_GER                      =     6;  { German                 }
  {$EXTERNALSYM TWLG_GER}
  TWLG_ICE                      =     7;  { Icelandic              }
  {$EXTERNALSYM TWLG_ICE}
  TWLG_ITN                      =     8;  { Italian                }
  {$EXTERNALSYM TWLG_ITN}
  TWLG_NOR                      =     9;  { Norwegian              }
  {$EXTERNALSYM TWLG_NOR}
  TWLG_POR                      =    10;  { Portuguese             }
  {$EXTERNALSYM TWLG_POR}
  TWLG_SPA                      =    11;  { Spanish                }
  {$EXTERNALSYM TWLG_SPA}
  TWLG_SWE                      =    12;  { Swedish                }
  {$EXTERNALSYM TWLG_SWE}
  TWLG_USA                      =    13;  { U.S. English           }
  {$EXTERNALSYM TWLG_USA}
  TWLG_AFRIKAANS                =    14;
  {$EXTERNALSYM TWLG_AFRIKAANS}
  TWLG_ALBANIA                  =    15;
  {$EXTERNALSYM TWLG_ALBANIA}
  TWLG_ARABIC                   =    16;
  {$EXTERNALSYM TWLG_ARABIC}
  TWLG_ARABIC_ALGERIA           =    17;
  {$EXTERNALSYM TWLG_ARABIC_ALGERIA}
  TWLG_ARABIC_BAHRAIN           =    18;
  {$EXTERNALSYM TWLG_ARABIC_BAHRAIN}
  TWLG_ARABIC_EGYPT             =    19;
  {$EXTERNALSYM TWLG_ARABIC_EGYPT}
  TWLG_ARABIC_IRAQ              =    20;
  {$EXTERNALSYM TWLG_ARABIC_IRAQ}
  TWLG_ARABIC_JORDAN            =    21;
  {$EXTERNALSYM TWLG_ARABIC_JORDAN}
  TWLG_ARABIC_KUWAIT            =    22;
  {$EXTERNALSYM TWLG_ARABIC_KUWAIT}
  TWLG_ARABIC_LEBANON           =    23;
  {$EXTERNALSYM TWLG_ARABIC_LEBANON}
  TWLG_ARABIC_LIBYA             =    24;
  {$EXTERNALSYM TWLG_ARABIC_LIBYA}
  TWLG_ARABIC_MOROCCO           =    25;
  {$EXTERNALSYM TWLG_ARABIC_MOROCCO}
  TWLG_ARABIC_OMAN              =    26;
  {$EXTERNALSYM TWLG_ARABIC_OMAN}
  TWLG_ARABIC_QATAR             =    27;
  {$EXTERNALSYM TWLG_ARABIC_QATAR}
  TWLG_ARABIC_SAUDIARABIA       =    28;
  {$EXTERNALSYM TWLG_ARABIC_SAUDIARABIA}
  TWLG_ARABIC_SYRIA             =    29;
  {$EXTERNALSYM TWLG_ARABIC_SYRIA}
  TWLG_ARABIC_TUNISIA           =    30;
  {$EXTERNALSYM TWLG_ARABIC_TUNISIA}
  TWLG_ARABIC_UAE               =    31;  { United Arabic Emirates }
  {$EXTERNALSYM TWLG_ARABIC_UAE}
  TWLG_ARABIC_YEMEN             =    32;
  {$EXTERNALSYM TWLG_ARABIC_YEMEN}
  TWLG_BASQUE                   =    33;
  {$EXTERNALSYM TWLG_BASQUE}
  TWLG_BYELORUSSIAN             =    34;
  {$EXTERNALSYM TWLG_BYELORUSSIAN}
  TWLG_BULGARIAN                =    35;
  {$EXTERNALSYM TWLG_BULGARIAN}
  TWLG_CATALAN                  =    36;
  {$EXTERNALSYM TWLG_CATALAN}
  TWLG_CHINESE                  =    37;
  {$EXTERNALSYM TWLG_CHINESE}
  TWLG_CHINESE_HONGKONG         =    38;
  {$EXTERNALSYM TWLG_CHINESE_HONGKONG}
  TWLG_CHINESE_PRC              =    39;  { People's Republic of China }
  {$EXTERNALSYM TWLG_CHINESE_PRC}
  TWLG_CHINESE_SINGAPORE        =    40;
  {$EXTERNALSYM TWLG_CHINESE_SINGAPORE}
  TWLG_CHINESE_SIMPLIFIED       =    41;
  {$EXTERNALSYM TWLG_CHINESE_SIMPLIFIED}
  TWLG_CHINESE_TAIWAN           =    42;
  {$EXTERNALSYM TWLG_CHINESE_TAIWAN}
  TWLG_CHINESE_TRADITIONAL      =    43;
  {$EXTERNALSYM TWLG_CHINESE_TRADITIONAL}
  TWLG_CROATIA                  =    44;
  {$EXTERNALSYM TWLG_CROATIA}
  TWLG_CZECH                    =    45;
  {$EXTERNALSYM TWLG_CZECH}
  TWLG_DANISH                   =    TWLG_DAN;
  {$EXTERNALSYM TWLG_DANISH}
  TWLG_DUTCH                    =    TWLG_DUT;
  {$EXTERNALSYM TWLG_DUTCH}
  TWLG_DUTCH_BELGIAN            =    46;
  {$EXTERNALSYM TWLG_DUTCH_BELGIAN}
  TWLG_ENGLISH                  =    TWLG_ENG;
  {$EXTERNALSYM TWLG_ENGLISH}
  TWLG_ENGLISH_AUSTRALIAN       =    47;
  {$EXTERNALSYM TWLG_ENGLISH_AUSTRALIAN}
  TWLG_ENGLISH_CANADIAN         =    48;
  {$EXTERNALSYM TWLG_ENGLISH_CANADIAN}
  TWLG_ENGLISH_IRELAND          =    49;
  {$EXTERNALSYM TWLG_ENGLISH_IRELAND}
  TWLG_ENGLISH_NEWZEALAND       =    50;
  {$EXTERNALSYM TWLG_ENGLISH_NEWZEALAND}
  TWLG_ENGLISH_SOUTHAFRICA      =    51;
  {$EXTERNALSYM TWLG_ENGLISH_SOUTHAFRICA}
  TWLG_ENGLISH_UK               =    52;
  {$EXTERNALSYM TWLG_ENGLISH_UK}
  TWLG_ENGLISH_USA              =    TWLG_USA;
  {$EXTERNALSYM TWLG_ENGLISH_USA}
  TWLG_ESTONIAN                 =    53;
  {$EXTERNALSYM TWLG_ESTONIAN}
  TWLG_FAEROESE                 =    54;
  {$EXTERNALSYM TWLG_FAEROESE}
  TWLG_FARSI                    =    55;
  {$EXTERNALSYM TWLG_FARSI}
  TWLG_FINNISH                  =    TWLG_FIN;
  {$EXTERNALSYM TWLG_FINNISH}
  TWLG_FRENCH                   =    TWLG_FRN;
  {$EXTERNALSYM TWLG_FRENCH}
  TWLG_FRENCH_BELGIAN           =    56;
  {$EXTERNALSYM TWLG_FRENCH_BELGIAN}
  TWLG_FRENCH_CANADIAN          =    TWLG_FCF;
  {$EXTERNALSYM TWLG_FRENCH_CANADIAN}
  TWLG_FRENCH_LUXEMBOURG        =    57;
  {$EXTERNALSYM TWLG_FRENCH_LUXEMBOURG}
  TWLG_FRENCH_SWISS             =    58;
  {$EXTERNALSYM TWLG_FRENCH_SWISS}
  TWLG_GERMAN                   =    TWLG_GER;
  {$EXTERNALSYM TWLG_GERMAN}
  TWLG_GERMAN_AUSTRIAN          =    59;
  {$EXTERNALSYM TWLG_GERMAN_AUSTRIAN}
  TWLG_GERMAN_LUXEMBOURG        =    60;
  {$EXTERNALSYM TWLG_GERMAN_LUXEMBOURG}
  TWLG_GERMAN_LIECHTENSTEIN     =    61;
  {$EXTERNALSYM TWLG_GERMAN_LIECHTENSTEIN}
  TWLG_GERMAN_SWISS             =    62;
  {$EXTERNALSYM TWLG_GERMAN_SWISS}
  TWLG_GREEK                    =    63;
  {$EXTERNALSYM TWLG_GREEK}
  TWLG_HEBREW                   =    64;
  {$EXTERNALSYM TWLG_HEBREW}
  TWLG_HUNGARIAN                =    65;
  {$EXTERNALSYM TWLG_HUNGARIAN}
  TWLG_ICELANDIC                =    TWLG_ICE;
  {$EXTERNALSYM TWLG_ICELANDIC}
  TWLG_INDONESIAN               =    66;
  {$EXTERNALSYM TWLG_INDONESIAN}
  TWLG_ITALIAN                  =    TWLG_ITN;
  {$EXTERNALSYM TWLG_ITALIAN}
  TWLG_ITALIAN_SWISS            =    67;
  {$EXTERNALSYM TWLG_ITALIAN_SWISS}
  TWLG_JAPANESE                 =    68;
  {$EXTERNALSYM TWLG_JAPANESE}
  TWLG_KOREAN                   =    69;
  {$EXTERNALSYM TWLG_KOREAN}
  TWLG_KOREAN_JOHAB             =    70;
  {$EXTERNALSYM TWLG_KOREAN_JOHAB}
  TWLG_LATVIAN                  =    71;
  {$EXTERNALSYM TWLG_LATVIAN}
  TWLG_LITHUANIAN               =    72;
  {$EXTERNALSYM TWLG_LITHUANIAN}
  TWLG_NORWEGIAN                =    TWLG_NOR;
  {$EXTERNALSYM TWLG_NORWEGIAN}
  TWLG_NORWEGIAN_BOKMAL         =    73;
  {$EXTERNALSYM TWLG_NORWEGIAN_BOKMAL}
  TWLG_NORWEGIAN_NYNORSK        =    74;
  {$EXTERNALSYM TWLG_NORWEGIAN_NYNORSK}
  TWLG_POLISH                   =    75;
  {$EXTERNALSYM TWLG_POLISH}
  TWLG_PORTUGUESE               =    TWLG_POR;
  {$EXTERNALSYM TWLG_PORTUGUESE}
  TWLG_PORTUGUESE_BRAZIL        =    76;
  {$EXTERNALSYM TWLG_PORTUGUESE_BRAZIL}
  TWLG_ROMANIAN                 =    77;
  {$EXTERNALSYM TWLG_ROMANIAN}
  TWLG_RUSSIAN                  =    78;
  {$EXTERNALSYM TWLG_RUSSIAN}
  TWLG_SERBIAN_LATIN            =    79;
  {$EXTERNALSYM TWLG_SERBIAN_LATIN}
  TWLG_SLOVAK                   =    80;
  {$EXTERNALSYM TWLG_SLOVAK}
  TWLG_SLOVENIAN                =    81;
  {$EXTERNALSYM TWLG_SLOVENIAN}
  TWLG_SPANISH                  =    TWLG_SPA;
  {$EXTERNALSYM TWLG_SPANISH}
  TWLG_SPANISH_MEXICAN          =    82;
  {$EXTERNALSYM TWLG_SPANISH_MEXICAN}
  TWLG_SPANISH_MODERN           =    83;
  {$EXTERNALSYM TWLG_SPANISH_MODERN}
  TWLG_SWEDISH                  =    TWLG_SWE;
  {$EXTERNALSYM TWLG_SWEDISH}
  TWLG_THAI                     =    84;
  {$EXTERNALSYM TWLG_THAI}
  TWLG_TURKISH                  =    85;
  {$EXTERNALSYM TWLG_TURKISH}
  TWLG_UKRANIAN                 =    86;
  {$EXTERNALSYM TWLG_UKRANIAN}
{  More stuff added for 1.8  }
  TWLG_ASSAMESE                 =    87;
  {$EXTERNALSYM TWLG_ASSAMESE}
  TWLG_BENGALI                  =    88;
  {$EXTERNALSYM TWLG_BENGALI}
  TWLG_BIHARI                   =    89;
  {$EXTERNALSYM TWLG_BIHARI}
  TWLG_BODO                     =    90;
  {$EXTERNALSYM TWLG_BODO}
  TWLG_DOGRI                    =    91;
  {$EXTERNALSYM TWLG_DOGRI}
  TWLG_GUJARATI                 =    92;
  {$EXTERNALSYM TWLG_GUJARATI}
  TWLG_HARYANVI                 =    93;
  {$EXTERNALSYM TWLG_HARYANVI}
  TWLG_HINDI                    =    94;
  {$EXTERNALSYM TWLG_HINDI}
  TWLG_KANNADA                  =    95;
  {$EXTERNALSYM TWLG_KANNADA}
  TWLG_KASHMIRI                 =    96;
  {$EXTERNALSYM TWLG_KASHMIRI}
  TWLG_MALAYALAM                =    97;
  {$EXTERNALSYM TWLG_MALAYALAM}
  TWLG_MARATHI                  =    98;
  {$EXTERNALSYM TWLG_MARATHI}
  TWLG_MARWARI                  =    99;
  {$EXTERNALSYM TWLG_MARWARI}
  TWLG_MEGHALAYAN               =   100;
  {$EXTERNALSYM TWLG_MEGHALAYAN}
  TWLG_MIZO                     =   101;
  {$EXTERNALSYM TWLG_MIZO}
  TWLG_NAGA                     =   102;
  {$EXTERNALSYM TWLG_NAGA}
  TWLG_ORISSI                   =   103;
  {$EXTERNALSYM TWLG_ORISSI}
  TWLG_PUNJABI                  =   104;
  {$EXTERNALSYM TWLG_PUNJABI}
  TWLG_PUSHTU                   =   105;
  {$EXTERNALSYM TWLG_PUSHTU}
  TWLG_SERBIAN_CYRILLIC         =   106;
  {$EXTERNALSYM TWLG_SERBIAN_CYRILLIC}
  TWLG_SIKKIMI                  =   107;
  {$EXTERNALSYM TWLG_SIKKIMI}
  TWLG_SWEDISH_FINLAND          =   108;
  {$EXTERNALSYM TWLG_SWEDISH_FINLAND}
  TWLG_TAMIL                    =   109;
  {$EXTERNALSYM TWLG_TAMIL}
  TWLG_TELUGU                   =   110;
  {$EXTERNALSYM TWLG_TELUGU}
  TWLG_TRIPURI                  =   111;
  {$EXTERNALSYM TWLG_TRIPURI}
  TWLG_URDU                     =   112;
  {$EXTERNALSYM TWLG_URDU}
  TWLG_VIETNAMESE               =   113;
  {$EXTERNALSYM TWLG_VIETNAMESE}

{***************************************************************************
 * Data Groups                                                             *
 ***************************************************************************}

{* More Data Groups may be added in the future.
 * Possible candidates include text, vector graphics, sound, etc.
 * NOTE: Data Group constants must be powers of 2 as they are used
 *       as bitflags when Application asks DSM to present a list of DSs.
 *}

  DG_CONTROL    = LongWord($0001); { data pertaining to control }
  {$EXTERNALSYM DG_CONTROL}
  DG_IMAGE      = LongWord($0002); { data pertaining to raster images }
  {$EXTERNALSYM DG_IMAGE}
{ Added 1.8 }
  DG_AUDIO      = LongWord($0004); { data pertaining to audio }
  {$EXTERNALSYM DG_AUDIO}

{* More Data Functionality may be added in the future.
 * These are for items that need to be determined before DS is opened.
 * NOTE: Supported Functionality constants must be powers of 2 as they are
 *       used as bitflags when Application asks DSM to present a list of DSs.
 *       to support backward capability the App and DS will not use the fields
 *}
  DF_DSM2        = $10000000;  { added to the identity by the DSM }
  {$EXTERNALSYM DF_DSM2}
  DF_APP2        = $20000000;  { Set by the App to indicate it would }
                               { prefer to use DSM2 }
  {$EXTERNALSYM DF_APP2}
  DF_DS2         = $40000000;  { Set by the DS to indicate it would }
                               { prefer to use DSM2 }
  {$EXTERNALSYM DF_DS2}
  DG_MASK        = LongWord($FFFF); { all Data Groups limited to 16 bit. Added for 2.1 }
  {$EXTERNALSYM DG_MASK}

{***************************************************************************
 * Data Argument Types                                                     *
 ***************************************************************************}

  DAT_NULL            = $0000; { No data or structure. }
  {$EXTERNALSYM DAT_NULL}
  DAT_CUSTOMBASE      = $8000; { Base of custom DATs. }
  {$EXTERNALSYM DAT_CUSTOMBASE}

{ Data Argument Types for the DG_CONTROL Data Group. }
  DAT_CAPABILITY      = $0001; { TW_CAPABILITY }
  {$EXTERNALSYM DAT_CAPABILITY}
  DAT_EVENT           = $0002; { TW_EVENT }
  {$EXTERNALSYM DAT_EVENT}
  DAT_IDENTITY        = $0003; { TW_IDENTITY }
  {$EXTERNALSYM DAT_IDENTITY}
  DAT_PARENT          = $0004; { TW_HANDLE, application win handle in Windows }
  {$EXTERNALSYM DAT_PARENT}
  DAT_PENDINGXFERS    = $0005; { TW_PENDINGXFERS }
  {$EXTERNALSYM DAT_PENDINGXFERS}
  DAT_SETUPMEMXFER    = $0006; { TW_SETUPMEMXFER }
  {$EXTERNALSYM DAT_SETUPMEMXFER}
  DAT_SETUPFILEXFER   = $0007; { TW_SETUPFILEXFER }
  {$EXTERNALSYM DAT_SETUPFILEXFER}
  DAT_STATUS          = $0008; { TW_STATUS }
  {$EXTERNALSYM DAT_STATUS}
  DAT_USERINTERFACE   = $0009; { TW_USERINTERFACE }
  {$EXTERNALSYM DAT_USERINTERFACE}
  DAT_XFERGROUP       = $000a; { TW_UINT32 }
  {$EXTERNALSYM DAT_XFERGROUP}
  DAT_CUSTOMDSDATA    = $000c; { TW_CUSTOMDSDATA. }
  {$EXTERNALSYM DAT_CUSTOMDSDATA}
  DAT_DEVICEEVENT     = $000d; { TW_DEVICEEVENT    Added 1.8 }
  {$EXTERNALSYM DAT_DEVICEEVENT}
  DAT_FILESYSTEM      = $000e; { TW_FILESYSTEM     Added 1.8 }
  {$EXTERNALSYM DAT_FILESYSTEM}
  DAT_PASSTHRU        = $000f; { TW_PASSTHRU       Added 1.8 }
  {$EXTERNALSYM DAT_PASSTHRU}
  DAT_CALLBACK        = $0010; { TW_CALLBACK       Added 2.0 }
  {$EXTERNALSYM DAT_CALLBACK}
  DAT_STATUSUTF8      = $0011; { TW_STATUSUTF8     Added 2.1 }
  {$EXTERNALSYM DAT_STATUSUTF8}
  DAT_CALLBACK2       = $0012;
  {$EXTERNALSYM DAT_CALLBACK2}

{ Data Argument Types for the DG_IMAGE Data Group. }
  DAT_IMAGEINFO       = $0101; { TW_IMAGEINFO }
  {$EXTERNALSYM DAT_IMAGEINFO}
  DAT_IMAGELAYOUT     = $0102; { TW_IMAGELAYOUT }
  {$EXTERNALSYM DAT_IMAGELAYOUT}
  DAT_IMAGEMEMXFER    = $0103; { TW_IMAGEMEMXFER }
  {$EXTERNALSYM DAT_IMAGEMEMXFER}
  DAT_IMAGENATIVEXFER = $0104; { TW_UINT32 loword is hDIB, PICHandle }
  {$EXTERNALSYM DAT_IMAGENATIVEXFER}
  DAT_IMAGEFILEXFER   = $0105; { Null data }
  {$EXTERNALSYM DAT_IMAGEFILEXFER}
  DAT_CIECOLOR        = $0106; { TW_CIECOLOR }
  {$EXTERNALSYM DAT_CIECOLOR}
  DAT_GRAYRESPONSE    = $0107; { TW_GRAYRESPONSE }
  {$EXTERNALSYM DAT_GRAYRESPONSE}
  DAT_RGBRESPONSE     = $0108; { TW_RGBRESPONSE }
  {$EXTERNALSYM DAT_RGBRESPONSE}
  DAT_JPEGCOMPRESSION = $0109; { TW_JPEGCOMPRESSION }
  {$EXTERNALSYM DAT_JPEGCOMPRESSION}
  DAT_PALETTE8        = $010a; { TW_PALETTE8 }
  {$EXTERNALSYM DAT_PALETTE8}
  DAT_EXTIMAGEINFO    = $010b; { TW_EXTIMAGEINFO -- for 1.7 Spec. }
  {$EXTERNALSYM DAT_EXTIMAGEINFO}
  DAT_FILTER          = $010c;
  {$EXTERNALSYM DAT_FILTER}

{ Data Argument Types for the DG_AUDIO Data Group. }
  DAT_AUDIOFILEXFER   = $0201; { Null data         Added 1.8 }
  {$EXTERNALSYM DAT_AUDIOFILEXFER}
  DAT_AUDIOINFO       = $0202; { TW_AUDIOINFO      Added 1.8 }
  {$EXTERNALSYM DAT_AUDIOINFO}
  DAT_AUDIONATIVEXFER = $0203; { TW_UINT32 handle to WAV, (AIFF Mac) Added 1.8 }
  {$EXTERNALSYM DAT_AUDIONATIVEXFER}

{ misplaced }
  DAT_ICCPROFILE        = $0401; { TW_MEMORY        Added 1.91  This Data Argument is misplaced but belongs to the DG_IMAGE Data Group }
  {$EXTERNALSYM DAT_ICCPROFILE}
  DAT_IMAGEMEMFILEXFER  = $0402; { TW_IMAGEMEMXFER  Added 1.91  This Data Argument is misplaced but belongs to the DG_IMAGE Data Group }
  {$EXTERNALSYM DAT_IMAGEMEMFILEXFER}
  DAT_ENTRYPOINT        = $0403; { TW_ENTRYPOINT    Added 2.0   This Data Argument is misplaced but belongs to the DG_CONTROL Data Group }
  {$EXTERNALSYM DAT_ENTRYPOINT}

{***************************************************************************
 * Messages                                                                *
 ***************************************************************************}

{* All message constants are unique.
 * Messages are grouped according to which DATs they are used with.*}

  MSG_NULL              = $0000; { Used in TW_EVENT structure }
  {$EXTERNALSYM MSG_NULL}
  MSG_CUSTOMBASE        = $8000; { Base of custom messages }
  {$EXTERNALSYM MSG_CUSTOMBASE}

{ Generic messages may be used with any of several DATs. }
  MSG_GET               = $0001; { Get one or more values }
  {$EXTERNALSYM MSG_GET}
  MSG_GETCURRENT        = $0002; { Get current value }
  {$EXTERNALSYM MSG_GETCURRENT}
  MSG_GETDEFAULT        = $0003; { Get default (e.g. power up) value }
  {$EXTERNALSYM MSG_GETDEFAULT}
  MSG_GETFIRST          = $0004; { Get first of a series of items, e.g. DSs }
  {$EXTERNALSYM MSG_GETFIRST}
  MSG_GETNEXT           = $0005; { Iterate through a series of items. }
  {$EXTERNALSYM MSG_GETNEXT}
  MSG_SET               = $0006; { Set one or more values }
  {$EXTERNALSYM MSG_SET}
  MSG_RESET             = $0007; { Set current value to default value }
  {$EXTERNALSYM MSG_RESET}
  MSG_QUERYSUPPORT      = $0008; { Get supported operations on the cap. }
  {$EXTERNALSYM MSG_QUERYSUPPORT}
  MSG_GETHELP           = $0009; { Returns help text suitable for use in a GUI        Added 2.1 }
  {$EXTERNALSYM MSG_GETHELP}
  MSG_GETLABEL          = $000a; { Returns a label suitable for use in a GUI          Added 2.1 }
  {$EXTERNALSYM MSG_GETLABEL}
  MSG_GETLABELENUM      = $000b; { Return all of the labels for a capability of type  Added 2.1 }
  {$EXTERNALSYM MSG_GETLABELENUM}
  MSG_SETCONSTRAINT     = $000c;
  {$EXTERNALSYM MSG_SETCONSTRAINT}

{ Messages used with DAT_NULL }
  MSG_XFERREADY         = $0101; { The data source has data ready }
  {$EXTERNALSYM MSG_XFERREADY}
  MSG_CLOSEDSREQ        = $0102; { Request for Application. to close DS }
  {$EXTERNALSYM MSG_CLOSEDSREQ}
  MSG_CLOSEDSOK         = $0103; { Tell the Application. to save the state. }
  {$EXTERNALSYM MSG_CLOSEDSOK}
  MSG_DEVICEEVENT       = $0104; { Some event has taken place  Added 1.8 }
  {$EXTERNALSYM MSG_DEVICEEVENT}

{ Messages used with a pointer to DAT_PARENT data }
  MSG_OPENDSM          = $0301; { Open the DSM }
  {$EXTERNALSYM MSG_OPENDSM}
  MSG_CLOSEDSM         = $0302; { Close the DSM }
  {$EXTERNALSYM MSG_CLOSEDSM}

{ Messages used with a pointer to a DAT_IDENTITY structure }
  MSG_OPENDS           = $0401; { Open a data source }
  {$EXTERNALSYM MSG_OPENDS}
  MSG_CLOSEDS          = $0402; { Close a data source }
  {$EXTERNALSYM MSG_CLOSEDS}
  MSG_USERSELECT       = $0403; { Put up a dialog of all DS }
  {$EXTERNALSYM MSG_USERSELECT}

{ Messages used with a pointer to a DAT_USERINTERFACE structure }
  MSG_DISABLEDS        = $0501; { Disable data transfer in the DS }
  {$EXTERNALSYM MSG_DISABLEDS}
  MSG_ENABLEDS         = $0502; { Enable data transfer in the DS }
  {$EXTERNALSYM MSG_ENABLEDS}
  MSG_ENABLEDSUIONLY   = $0503; { Enable for saving DS state only. }
  {$EXTERNALSYM MSG_ENABLEDSUIONLY}

{ Messages used with a pointer to a DAT_EVENT structure }
  MSG_PROCESSEVENT     = $0601;
  {$EXTERNALSYM MSG_PROCESSEVENT}

{ Messages used with a pointer to a DAT_PENDINGXFERS structure }
  MSG_ENDXFER          = $0701;
  {$EXTERNALSYM MSG_ENDXFER}
  MSG_STOPFEEDER       = $0702;
  {$EXTERNALSYM MSG_STOPFEEDER}

{ Messages used with a pointer to a DAT_FILESYSTEM structure }
  MSG_CHANGEDIRECTORY  = $0801;  { Added 1.8 }
  {$EXTERNALSYM MSG_CHANGEDIRECTORY}
  MSG_CREATEDIRECTORY  = $0802;  { Added 1.8 }
  {$EXTERNALSYM MSG_CREATEDIRECTORY}
  MSG_DELETE           = $0803;  { Added 1.8 }
  {$EXTERNALSYM MSG_DELETE}
  MSG_FORMATMEDIA      = $0804;  { Added 1.8 }
  {$EXTERNALSYM MSG_FORMATMEDIA}
  MSG_GETCLOSE         = $0805;  { Added 1.8 }
  {$EXTERNALSYM MSG_GETCLOSE}
  MSG_GETFIRSTFILE     = $0806;  { Added 1.8 }
  {$EXTERNALSYM MSG_GETFIRSTFILE}
  MSG_GETINFO          = $0807;  { Added 1.8 }
  {$EXTERNALSYM MSG_GETINFO}
  MSG_GETNEXTFILE      = $0808;  { Added 1.8 }
  {$EXTERNALSYM MSG_GETNEXTFILE}
  MSG_RENAME           = $0809;  { Added 1.8 }
  {$EXTERNALSYM MSG_RENAME}
  MSG_COPY             = $080A;  { Added 1.8 }
  {$EXTERNALSYM MSG_COPY}
  MSG_AUTOMATICCAPTUREDIRECTORY = $080B;  { Added 1.8 }
  {$EXTERNALSYM MSG_AUTOMATICCAPTUREDIRECTORY}

{ Messages used with a pointer to a DAT_PASSTHRU structure }
  MSG_PASSTHRU         = $0901;
  {$EXTERNALSYM MSG_PASSTHRU}

{ used with DAT_CALLBACK }
  MSG_REGISTER_CALLBACK  = $0902;
  {$EXTERNALSYM MSG_REGISTER_CALLBACK}

{ used with DAT_CAPABILITY }
  MSG_RESETALL         = $0A01; {  Added 1.91 }
  {$EXTERNALSYM MSG_RESETALL}

{***************************************************************************
 * Capabilities                                                            *
 ***************************************************************************}

  CAP_CUSTOMBASE                   = $8000; { Base of custom capabilities }
  {$EXTERNALSYM CAP_CUSTOMBASE}

{ all data sources are REQUIRED to support these caps }
  CAP_XFERCOUNT                    = $0001;
  {$EXTERNALSYM CAP_XFERCOUNT}

{ image data sources are REQUIRED to support these caps }
  ICAP_COMPRESSION                 = $0100;
  {$EXTERNALSYM ICAP_COMPRESSION}
  ICAP_PIXELTYPE                   = $0101;
  {$EXTERNALSYM ICAP_PIXELTYPE}
  ICAP_UNITS                       = $0102; { default is TWUN_INCHES }
  {$EXTERNALSYM ICAP_UNITS}
  ICAP_XFERMECH                    = $0103;
  {$EXTERNALSYM ICAP_XFERMECH}

{  all data sources MAY support these caps  }
  CAP_AUTHOR                    = $1000;
  {$EXTERNALSYM CAP_AUTHOR}
  CAP_CAPTION                   = $1001;
  {$EXTERNALSYM CAP_CAPTION}
  CAP_FEEDERENABLED             = $1002;
  {$EXTERNALSYM CAP_FEEDERENABLED}
  CAP_FEEDERLOADED              = $1003;
  {$EXTERNALSYM CAP_FEEDERLOADED}
  CAP_TIMEDATE                  = $1004;
  {$EXTERNALSYM CAP_TIMEDATE}
  CAP_SUPPORTEDCAPS             = $1005;
  {$EXTERNALSYM CAP_SUPPORTEDCAPS}
  CAP_EXTENDEDCAPS              = $1006;
  {$EXTERNALSYM CAP_EXTENDEDCAPS}
  CAP_AUTOFEED                  = $1007;
  {$EXTERNALSYM CAP_AUTOFEED}
  CAP_CLEARPAGE                 = $1008;
  {$EXTERNALSYM CAP_CLEARPAGE}
  CAP_FEEDPAGE                  = $1009;
  {$EXTERNALSYM CAP_FEEDPAGE}
  CAP_REWINDPAGE                = $100a;
  {$EXTERNALSYM CAP_REWINDPAGE}
  CAP_INDICATORS                = $100b;  { Added 1.1 }
  {$EXTERNALSYM CAP_INDICATORS}
  CAP_PAPERDETECTABLE           = $100d;  { Added 1.6 }
  {$EXTERNALSYM CAP_PAPERDETECTABLE}
  CAP_UICONTROLLABLE            = $100e;  { Added 1.6 }
  {$EXTERNALSYM CAP_UICONTROLLABLE}
  CAP_DEVICEONLINE              = $100f;  { Added 1.6 }
  {$EXTERNALSYM CAP_DEVICEONLINE}
  CAP_AUTOSCAN                  = $1010;  { Added 1.6 }
  {$EXTERNALSYM CAP_AUTOSCAN}
  CAP_THUMBNAILSENABLED         = $1011;  { Added 1.7 }
  {$EXTERNALSYM CAP_THUMBNAILSENABLED}
  CAP_DUPLEX                    = $1012;  { Added 1.7 }
  {$EXTERNALSYM CAP_DUPLEX}
  CAP_DUPLEXENABLED             = $1013;  { Added 1.7 }
  {$EXTERNALSYM CAP_DUPLEXENABLED}
  CAP_ENABLEDSUIONLY            = $1014;  { Added 1.7 }
  {$EXTERNALSYM CAP_ENABLEDSUIONLY}
  CAP_CUSTOMDSDATA              = $1015;  { Added 1.7 }
  {$EXTERNALSYM CAP_CUSTOMDSDATA}
  CAP_ENDORSER                  = $1016;  { Added 1.7 }
  {$EXTERNALSYM CAP_ENDORSER}
  CAP_JOBCONTROL                = $1017;  { Added 1.7 }
  {$EXTERNALSYM CAP_JOBCONTROL}
  CAP_ALARMS                    = $1018;  { Added 1.8 }
  {$EXTERNALSYM CAP_ALARMS}
  CAP_ALARMVOLUME               = $1019;  { Added 1.8 }
  {$EXTERNALSYM CAP_ALARMVOLUME}
  CAP_AUTOMATICCAPTURE          = $101a;  { Added 1.8 }
  {$EXTERNALSYM CAP_AUTOMATICCAPTURE}
  CAP_TIMEBEFOREFIRSTCAPTURE    = $101b;  { Added 1.8 }
  {$EXTERNALSYM CAP_TIMEBEFOREFIRSTCAPTURE}
  CAP_TIMEBETWEENCAPTURES       = $101c;  { Added 1.8 }
  {$EXTERNALSYM CAP_TIMEBETWEENCAPTURES}
  CAP_CLEARBUFFERS              = $101d;  { Added 1.8 }
  {$EXTERNALSYM CAP_CLEARBUFFERS}
  CAP_MAXBATCHBUFFERS           = $101e;  { Added 1.8 }
  {$EXTERNALSYM CAP_MAXBATCHBUFFERS}
  CAP_DEVICETIMEDATE            = $101f;  { Added 1.8 }
  {$EXTERNALSYM CAP_DEVICETIMEDATE}
  CAP_POWERSUPPLY               = $1020;  { Added 1.8 }
  {$EXTERNALSYM CAP_POWERSUPPLY}
  CAP_CAMERAPREVIEWUI           = $1021;  { Added 1.8 }
  {$EXTERNALSYM CAP_CAMERAPREVIEWUI}
  CAP_DEVICEEVENT               = $1022;  { Added 1.8 }
  {$EXTERNALSYM CAP_DEVICEEVENT}
  CAP_SERIALNUMBER              = $1024;  { Added 1.8 }
  {$EXTERNALSYM CAP_SERIALNUMBER}
  CAP_PRINTER                   = $1026;  { Added 1.8 }
  {$EXTERNALSYM CAP_PRINTER}
  CAP_PRINTERENABLED            = $1027;  { Added 1.8 }
  {$EXTERNALSYM CAP_PRINTERENABLED}
  CAP_PRINTERINDEX              = $1028;  { Added 1.8 }
  {$EXTERNALSYM CAP_PRINTERINDEX}
  CAP_PRINTERMODE               = $1029;  { Added 1.8 }
  {$EXTERNALSYM CAP_PRINTERMODE}
  CAP_PRINTERSTRING             = $102a;  { Added 1.8 }
  {$EXTERNALSYM CAP_PRINTERSTRING}
  CAP_PRINTERSUFFIX             = $102b;  { Added 1.8 }
  {$EXTERNALSYM CAP_PRINTERSUFFIX}
  CAP_LANGUAGE                  = $102c;  { Added 1.8 }
  {$EXTERNALSYM CAP_LANGUAGE}
  CAP_FEEDERALIGNMENT           = $102d;  { Added 1.8 }
  {$EXTERNALSYM CAP_FEEDERALIGNMENT}
  CAP_FEEDERORDER               = $102e;  { Added 1.8 }
  {$EXTERNALSYM CAP_FEEDERORDER}
  CAP_REACQUIREALLOWED          = $1030;  { Added 1.8 }
  {$EXTERNALSYM CAP_REACQUIREALLOWED}
  CAP_BATTERYMINUTES            = $1032;  { Added 1.8 }
  {$EXTERNALSYM CAP_BATTERYMINUTES}
  CAP_BATTERYPERCENTAGE         = $1033;  { Added 1.8 }
  {$EXTERNALSYM CAP_BATTERYPERCENTAGE}
  CAP_CAMERASIDE                = $1034;  { Added 1.91 }
  {$EXTERNALSYM CAP_CAMERASIDE}
  CAP_SEGMENTED                 = $1035;  { Added 1.91 }
  {$EXTERNALSYM CAP_SEGMENTED}
  CAP_CAMERAENABLED             = $1036;  { Added 2.0 }
  {$EXTERNALSYM CAP_CAMERAENABLED}
  CAP_CAMERAORDER               = $1037;  { Added 2.0 }
  {$EXTERNALSYM CAP_CAMERAORDER}
  CAP_MICRENABLED               = $1038;  { Added 2.0 }
  {$EXTERNALSYM CAP_MICRENABLED}
  CAP_FEEDERPREP                = $1039;  { Added 2.0 }
  {$EXTERNALSYM CAP_FEEDERPREP}
  CAP_FEEDERPOCKET              = $103a;  { Added 2.0 }
  {$EXTERNALSYM CAP_FEEDERPOCKET}
  CAP_AUTOMATICSENSEMEDIUM      = $103b;  { Added 2.1 }
  {$EXTERNALSYM CAP_AUTOMATICSENSEMEDIUM}
  CAP_CUSTOMINTERFACEGUID       = $103c;  { Added 2.1 }
  {$EXTERNALSYM CAP_CUSTOMINTERFACEGUID}
  CAP_SUPPORTEDCAPSSEGMENTUNIQUE = $103d;
  {$EXTERNALSYM CAP_SUPPORTEDCAPSSEGMENTUNIQUE}
  CAP_SUPPORTEDDATS             = $103e;
  {$EXTERNALSYM CAP_SUPPORTEDDATS}
  CAP_DOUBLEFEEDDETECTION       = $103f;
  {$EXTERNALSYM CAP_DOUBLEFEEDDETECTION}
  CAP_DOUBLEFEEDDETECTIONLENGTH = $1040;
  {$EXTERNALSYM CAP_DOUBLEFEEDDETECTIONLENGTH}
  CAP_DOUBLEFEEDDETECTIONSENSITIVITY = $1041;
  {$EXTERNALSYM CAP_DOUBLEFEEDDETECTIONSENSITIVITY}
  CAP_DOUBLEFEEDDETECTIONRESPONSE = $1042;
  {$EXTERNALSYM CAP_DOUBLEFEEDDETECTIONRESPONSE}
  CAP_PAPERHANDLING             = $1043;
  {$EXTERNALSYM CAP_PAPERHANDLING}
  CAP_INDICATORSMODE            = $1044;
  {$EXTERNALSYM CAP_INDICATORSMODE}
  CAP_PRINTERVERTICALOFFSET     = $1045;
  {$EXTERNALSYM CAP_PRINTERVERTICALOFFSET}
  CAP_POWERSAVETIME             = $1046;
  {$EXTERNALSYM CAP_POWERSAVETIME}

{  image data sources MAY support these caps  }
  ICAP_AUTOBRIGHT                         = $1100;
  {$EXTERNALSYM ICAP_AUTOBRIGHT}
  ICAP_BRIGHTNESS                         = $1101;
  {$EXTERNALSYM ICAP_BRIGHTNESS}
  ICAP_CONTRAST                           = $1103;
  {$EXTERNALSYM ICAP_CONTRAST}
  ICAP_CUSTHALFTONE                       = $1104;
  {$EXTERNALSYM ICAP_CUSTHALFTONE}
  ICAP_EXPOSURETIME                       = $1105;
  {$EXTERNALSYM ICAP_EXPOSURETIME}
  ICAP_FILTER                             = $1106;
  {$EXTERNALSYM ICAP_FILTER}
  ICAP_FLASHUSED                          = $1107;
  {$EXTERNALSYM ICAP_FLASHUSED}
  ICAP_GAMMA                              = $1108;
  {$EXTERNALSYM ICAP_GAMMA}
  ICAP_HALFTONES                          = $1109;
  {$EXTERNALSYM ICAP_HALFTONES}
  ICAP_HIGHLIGHT                          = $110a;
  {$EXTERNALSYM ICAP_HIGHLIGHT}
  ICAP_IMAGEFILEFORMAT                    = $110c;
  {$EXTERNALSYM ICAP_IMAGEFILEFORMAT}
  ICAP_LAMPSTATE                          = $110d;
  {$EXTERNALSYM ICAP_LAMPSTATE}
  ICAP_LIGHTSOURCE                        = $110e;
  {$EXTERNALSYM ICAP_LIGHTSOURCE}
  ICAP_ORIENTATION                        = $1110;
  {$EXTERNALSYM ICAP_ORIENTATION}
  ICAP_PHYSICALWIDTH                      = $1111;
  {$EXTERNALSYM ICAP_PHYSICALWIDTH}
  ICAP_PHYSICALHEIGHT                     = $1112;
  {$EXTERNALSYM ICAP_PHYSICALHEIGHT}
  ICAP_SHADOW                             = $1113;
  {$EXTERNALSYM ICAP_SHADOW}
  ICAP_FRAMES                             = $1114;
  {$EXTERNALSYM ICAP_FRAMES}
  ICAP_XNATIVERESOLUTION                  = $1116;
  {$EXTERNALSYM ICAP_XNATIVERESOLUTION}
  ICAP_YNATIVERESOLUTION                  = $1117;
  {$EXTERNALSYM ICAP_YNATIVERESOLUTION}
  ICAP_XRESOLUTION                        = $1118;
  {$EXTERNALSYM ICAP_XRESOLUTION}
  ICAP_YRESOLUTION                        = $1119;
  {$EXTERNALSYM ICAP_YRESOLUTION}
  ICAP_MAXFRAMES                          = $111a;
  {$EXTERNALSYM ICAP_MAXFRAMES}
  ICAP_TILES                              = $111b;
  {$EXTERNALSYM ICAP_TILES}
  ICAP_BITORDER                           = $111c;
  {$EXTERNALSYM ICAP_BITORDER}
  ICAP_CCITTKFACTOR                       = $111d;
  {$EXTERNALSYM ICAP_CCITTKFACTOR}
  ICAP_LIGHTPATH                          = $111e;
  {$EXTERNALSYM ICAP_LIGHTPATH}
  ICAP_PIXELFLAVOR                        = $111f;
  {$EXTERNALSYM ICAP_PIXELFLAVOR}
  ICAP_PLANARCHUNKY                       = $1120;
  {$EXTERNALSYM ICAP_PLANARCHUNKY}
  ICAP_ROTATION                           = $1121;
  {$EXTERNALSYM ICAP_ROTATION}
  ICAP_SUPPORTEDSIZES                     = $1122;
  {$EXTERNALSYM ICAP_SUPPORTEDSIZES}
  ICAP_THRESHOLD                          = $1123;
  {$EXTERNALSYM ICAP_THRESHOLD}
  ICAP_XSCALING                           = $1124;
  {$EXTERNALSYM ICAP_XSCALING}
  ICAP_YSCALING                           = $1125;
  {$EXTERNALSYM ICAP_YSCALING}
  ICAP_BITORDERCODES                      = $1126;
  {$EXTERNALSYM ICAP_BITORDERCODES}
  ICAP_PIXELFLAVORCODES                   = $1127;
  {$EXTERNALSYM ICAP_PIXELFLAVORCODES}
  ICAP_JPEGPIXELTYPE                      = $1128;
  {$EXTERNALSYM ICAP_JPEGPIXELTYPE}
  ICAP_TIMEFILL                           = $112a;
  {$EXTERNALSYM ICAP_TIMEFILL}
  ICAP_BITDEPTH                           = $112b;
  {$EXTERNALSYM ICAP_BITDEPTH}
  ICAP_BITDEPTHREDUCTION                  = $112c;  { Added 1.5 }
  {$EXTERNALSYM ICAP_BITDEPTHREDUCTION}
  ICAP_UNDEFINEDIMAGESIZE                 = $112d;  { Added 1.6 }
  {$EXTERNALSYM ICAP_UNDEFINEDIMAGESIZE}
  ICAP_IMAGEDATASET                       = $112e;  { Added 1.7 }
  {$EXTERNALSYM ICAP_IMAGEDATASET}
  ICAP_EXTIMAGEINFO                       = $112f;  { Added 1.7 }
  {$EXTERNALSYM ICAP_EXTIMAGEINFO}
  ICAP_MINIMUMHEIGHT                      = $1130;  { Added 1.7 }
  {$EXTERNALSYM ICAP_MINIMUMHEIGHT}
  ICAP_MINIMUMWIDTH                       = $1131;  { Added 1.7 }
  {$EXTERNALSYM ICAP_MINIMUMWIDTH}
  ICAP_AUTODISCARDBLANKPAGES              = $1134;  { Added 2.0 }
  {$EXTERNALSYM ICAP_AUTODISCARDBLANKPAGES}
  ICAP_FLIPROTATION                       = $1136;  { Added 1.8 }
  {$EXTERNALSYM ICAP_FLIPROTATION}
  ICAP_BARCODEDETECTIONENABLED            = $1137;  { Added 1.8 }
  {$EXTERNALSYM ICAP_BARCODEDETECTIONENABLED}
  ICAP_SUPPORTEDBARCODETYPES              = $1138;  { Added 1.8 }
  {$EXTERNALSYM ICAP_SUPPORTEDBARCODETYPES}
  ICAP_BARCODEMAXSEARCHPRIORITIES         = $1139;  { Added 1.8 }
  {$EXTERNALSYM ICAP_BARCODEMAXSEARCHPRIORITIES}
  ICAP_BARCODESEARCHPRIORITIES            = $113a;  { Added 1.8 }
  {$EXTERNALSYM ICAP_BARCODESEARCHPRIORITIES}
  ICAP_BARCODESEARCHMODE                  = $113b;  { Added 1.8 }
  {$EXTERNALSYM ICAP_BARCODESEARCHMODE}
  ICAP_BARCODEMAXRETRIES                  = $113c;  { Added 1.8 }
  {$EXTERNALSYM ICAP_BARCODEMAXRETRIES}
  ICAP_BARCODETIMEOUT                     = $113d;  { Added 1.8 }
  {$EXTERNALSYM ICAP_BARCODETIMEOUT}
  ICAP_ZOOMFACTOR                         = $113e;  { Added 1.8 }
  {$EXTERNALSYM ICAP_ZOOMFACTOR}
  ICAP_PATCHCODEDETECTIONENABLED          = $113f;  { Added 1.8 }
  {$EXTERNALSYM ICAP_PATCHCODEDETECTIONENABLED}
  ICAP_SUPPORTEDPATCHCODETYPES            = $1140;  { Added 1.8 }
  {$EXTERNALSYM ICAP_SUPPORTEDPATCHCODETYPES}
  ICAP_PATCHCODEMAXSEARCHPRIORITIES       = $1141;  { Added 1.8 }
  {$EXTERNALSYM ICAP_PATCHCODEMAXSEARCHPRIORITIES}
  ICAP_PATCHCODESEARCHPRIORITIES          = $1142;  { Added 1.8 }
  {$EXTERNALSYM ICAP_PATCHCODESEARCHPRIORITIES}
  ICAP_PATCHCODESEARCHMODE                = $1143;  { Added 1.8 }
  {$EXTERNALSYM ICAP_PATCHCODESEARCHMODE}
  ICAP_PATCHCODEMAXRETRIES                = $1144;  { Added 1.8 }
  {$EXTERNALSYM ICAP_PATCHCODEMAXRETRIES}
  ICAP_PATCHCODETIMEOUT                   = $1145;  { Added 1.8 }
  {$EXTERNALSYM ICAP_PATCHCODETIMEOUT}
  ICAP_FLASHUSED2                         = $1146;  { Added 1.8 }
  {$EXTERNALSYM ICAP_FLASHUSED2}
  ICAP_IMAGEFILTER                        = $1147;  { Added 1.8 }
  {$EXTERNALSYM ICAP_IMAGEFILTER}
  ICAP_NOISEFILTER                        = $1148;  { Added 1.8 }
  {$EXTERNALSYM ICAP_NOISEFILTER}
  ICAP_OVERSCAN                           = $1149;  { Added 1.8 }
  {$EXTERNALSYM ICAP_OVERSCAN}
  ICAP_AUTOMATICBORDERDETECTION           = $1150;  { Added 1.8 }
  {$EXTERNALSYM ICAP_AUTOMATICBORDERDETECTION}
  ICAP_AUTOMATICDESKEW                    = $1151;  { Added 1.8 }
  {$EXTERNALSYM ICAP_AUTOMATICDESKEW}
  ICAP_AUTOMATICROTATE                    = $1152;  { Added 1.8 }
  {$EXTERNALSYM ICAP_AUTOMATICROTATE}
  ICAP_JPEGQUALITY                        = $1153;  { Added 1.9 }
  {$EXTERNALSYM ICAP_JPEGQUALITY}
  ICAP_FEEDERTYPE                         = $1154;  { Added 1.91 }
  {$EXTERNALSYM ICAP_FEEDERTYPE}
  ICAP_ICCPROFILE                         = $1155;  { Added 1.91 }
  {$EXTERNALSYM ICAP_ICCPROFILE}
  ICAP_AUTOSIZE                           = $1156;  { Added 2.0 }
  {$EXTERNALSYM ICAP_AUTOSIZE}
  ICAP_AUTOMATICCROPUSESFRAME             = $1157;  { Added 2.1 }
  {$EXTERNALSYM ICAP_AUTOMATICCROPUSESFRAME}
  ICAP_AUTOMATICLENGTHDETECTION           = $1158;  { Added 2.1 }
  {$EXTERNALSYM ICAP_AUTOMATICLENGTHDETECTION}
  ICAP_AUTOMATICCOLORENABLED              = $1159;  { Added 2.1 }
  {$EXTERNALSYM ICAP_AUTOMATICCOLORENABLED}
  ICAP_AUTOMATICCOLORNONCOLORPIXELTYPE    = $115a;  { Added 2.1 }
  {$EXTERNALSYM ICAP_AUTOMATICCOLORNONCOLORPIXELTYPE}
  ICAP_COLORMANAGEMENTENABLED             = $115b;  { Added 2.1 }
  {$EXTERNALSYM ICAP_COLORMANAGEMENTENABLED}
  ICAP_IMAGEMERGE                         = $115c;  { Added 2.1 }
  {$EXTERNALSYM ICAP_IMAGEMERGE}
  ICAP_IMAGEMERGEHEIGHTTHRESHOLD          = $115d;  { Added 2.1 }
  {$EXTERNALSYM ICAP_IMAGEMERGEHEIGHTTHRESHOLD}
  ICAP_SUPPORTEDEXTIMAGEINFO              = $115e;  { Added 2.1 }
  {$EXTERNALSYM ICAP_SUPPORTEDEXTIMAGEINFO}
  ICAP_FILMTYPE                           = $115f;
  {$EXTERNALSYM ICAP_FILMTYPE}
  ICAP_MIRROR                             = $1160;
  {$EXTERNALSYM ICAP_MIRROR}
  ICAP_JPEGSUBSAMPLING                    = $1161;
  {$EXTERNALSYM ICAP_JPEGSUBSAMPLING}

{ image data sources MAY support these audio caps }
  ACAP_XFERMECH                    = $1202; { Added 1.8 }
  {$EXTERNALSYM ACAP_XFERMECH}

{***************************************************************************
 *            Extended Image Info Attributes section  Added 1.7            *
 ***************************************************************************}

  TWEI_BARCODEX                 = $1200;
  {$EXTERNALSYM TWEI_BARCODEX}
  TWEI_BARCODEY                 = $1201;
  {$EXTERNALSYM TWEI_BARCODEY}
  TWEI_BARCODETEXT              = $1202;
  {$EXTERNALSYM TWEI_BARCODETEXT}
  TWEI_BARCODETYPE              = $1203;
  {$EXTERNALSYM TWEI_BARCODETYPE}
  TWEI_DESHADETOP               = $1204;
  {$EXTERNALSYM TWEI_DESHADETOP}
  TWEI_DESHADELEFT              = $1205;
  {$EXTERNALSYM TWEI_DESHADELEFT}
  TWEI_DESHADEHEIGHT            = $1206;
  {$EXTERNALSYM TWEI_DESHADEHEIGHT}
  TWEI_DESHADEWIDTH             = $1207;
  {$EXTERNALSYM TWEI_DESHADEWIDTH}
  TWEI_DESHADESIZE              = $1208;
  {$EXTERNALSYM TWEI_DESHADESIZE}
  TWEI_SPECKLESREMOVED          = $1209;
  {$EXTERNALSYM TWEI_SPECKLESREMOVED}
  TWEI_HORZLINEXCOORD           = $120A;
  {$EXTERNALSYM TWEI_HORZLINEXCOORD}
  TWEI_HORZLINEYCOORD           = $120B;
  {$EXTERNALSYM TWEI_HORZLINEYCOORD}
  TWEI_HORZLINELENGTH           = $120C;
  {$EXTERNALSYM TWEI_HORZLINELENGTH}
  TWEI_HORZLINETHICKNESS        = $120D;
  {$EXTERNALSYM TWEI_HORZLINETHICKNESS}
  TWEI_VERTLINEXCOORD           = $120E;
  {$EXTERNALSYM TWEI_VERTLINEXCOORD}
  TWEI_VERTLINEYCOORD           = $120F;
  {$EXTERNALSYM TWEI_VERTLINEYCOORD}
  TWEI_VERTLINELENGTH           = $1210;
  {$EXTERNALSYM TWEI_VERTLINELENGTH}
  TWEI_VERTLINETHICKNESS        = $1211;
  {$EXTERNALSYM TWEI_VERTLINETHICKNESS}
  TWEI_PATCHCODE                = $1212;
  {$EXTERNALSYM TWEI_PATCHCODE}
  TWEI_ENDORSEDTEXT             = $1213;
  {$EXTERNALSYM TWEI_ENDORSEDTEXT}
  TWEI_FORMCONFIDENCE           = $1214;
  {$EXTERNALSYM TWEI_FORMCONFIDENCE}
  TWEI_FORMTEMPLATEMATCH        = $1215;
  {$EXTERNALSYM TWEI_FORMTEMPLATEMATCH}
  TWEI_FORMTEMPLATEPAGEMATCH    = $1216;
  {$EXTERNALSYM TWEI_FORMTEMPLATEPAGEMATCH}
  TWEI_FORMHORZDOCOFFSET        = $1217;
  {$EXTERNALSYM TWEI_FORMHORZDOCOFFSET}
  TWEI_FORMVERTDOCOFFSET        = $1218;
  {$EXTERNALSYM TWEI_FORMVERTDOCOFFSET}
  TWEI_BARCODECOUNT             = $1219;
  {$EXTERNALSYM TWEI_BARCODECOUNT}
  TWEI_BARCODECONFIDENCE        = $121A;
  {$EXTERNALSYM TWEI_BARCODECONFIDENCE}
  TWEI_BARCODEROTATION          = $121B;
  {$EXTERNALSYM TWEI_BARCODEROTATION}
  TWEI_BARCODETEXTLENGTH        = $121C;
  {$EXTERNALSYM TWEI_BARCODETEXTLENGTH}
  TWEI_DESHADECOUNT             = $121D;
  {$EXTERNALSYM TWEI_DESHADECOUNT}
  TWEI_DESHADEBLACKCOUNTOLD     = $121E;
  {$EXTERNALSYM TWEI_DESHADEBLACKCOUNTOLD}
  TWEI_DESHADEBLACKCOUNTNEW     = $121F;
  {$EXTERNALSYM TWEI_DESHADEBLACKCOUNTNEW}
  TWEI_DESHADEBLACKRLMIN        = $1220;
  {$EXTERNALSYM TWEI_DESHADEBLACKRLMIN}
  TWEI_DESHADEBLACKRLMAX        = $1221;
  {$EXTERNALSYM TWEI_DESHADEBLACKRLMAX}
  TWEI_DESHADEWHITECOUNTOLD     = $1222;
  {$EXTERNALSYM TWEI_DESHADEWHITECOUNTOLD}
  TWEI_DESHADEWHITECOUNTNEW     = $1223;
  {$EXTERNALSYM TWEI_DESHADEWHITECOUNTNEW}
  TWEI_DESHADEWHITERLMIN        = $1224;
  {$EXTERNALSYM TWEI_DESHADEWHITERLMIN}
  TWEI_DESHADEWHITERLAVE        = $1225;
  {$EXTERNALSYM TWEI_DESHADEWHITERLAVE}
  TWEI_DESHADEWHITERLMAX        = $1226;
  {$EXTERNALSYM TWEI_DESHADEWHITERLMAX}
  TWEI_BLACKSPECKLESREMOVED     = $1227;
  {$EXTERNALSYM TWEI_BLACKSPECKLESREMOVED}
  TWEI_WHITESPECKLESREMOVED     = $1228;
  {$EXTERNALSYM TWEI_WHITESPECKLESREMOVED}
  TWEI_HORZLINECOUNT            = $1229;
  {$EXTERNALSYM TWEI_HORZLINECOUNT}
  TWEI_VERTLINECOUNT            = $122A;
  {$EXTERNALSYM TWEI_VERTLINECOUNT}
  TWEI_DESKEWSTATUS             = $122B;
  {$EXTERNALSYM TWEI_DESKEWSTATUS}
  TWEI_SKEWORIGINALANGLE        = $122C;
  {$EXTERNALSYM TWEI_SKEWORIGINALANGLE}
  TWEI_SKEWFINALANGLE           = $122D;
  {$EXTERNALSYM TWEI_SKEWFINALANGLE}
  TWEI_SKEWCONFIDENCE           = $122E;
  {$EXTERNALSYM TWEI_SKEWCONFIDENCE}
  TWEI_SKEWWINDOWX1             = $122F;
  {$EXTERNALSYM TWEI_SKEWWINDOWX1}
  TWEI_SKEWWINDOWY1             = $1230;
  {$EXTERNALSYM TWEI_SKEWWINDOWY1}
  TWEI_SKEWWINDOWX2             = $1231;
  {$EXTERNALSYM TWEI_SKEWWINDOWX2}
  TWEI_SKEWWINDOWY2             = $1232;
  {$EXTERNALSYM TWEI_SKEWWINDOWY2}
  TWEI_SKEWWINDOWX3             = $1233;
  {$EXTERNALSYM TWEI_SKEWWINDOWX3}
  TWEI_SKEWWINDOWY3             = $1234;
  {$EXTERNALSYM TWEI_SKEWWINDOWY3}
  TWEI_SKEWWINDOWX4             = $1235;
  {$EXTERNALSYM TWEI_SKEWWINDOWX4}
  TWEI_SKEWWINDOWY4             = $1236;
  {$EXTERNALSYM TWEI_SKEWWINDOWY4}
  TWEI_BOOKNAME                 = $1238;  { added 1.9 }
  {$EXTERNALSYM TWEI_BOOKNAME}
  TWEI_CHAPTERNUMBER            = $1239;  { added 1.9 }
  {$EXTERNALSYM TWEI_CHAPTERNUMBER}
  TWEI_DOCUMENTNUMBER           = $123A;  { added 1.9 }
  {$EXTERNALSYM TWEI_DOCUMENTNUMBER}
  TWEI_PAGENUMBER               = $123B;  { added 1.9 }
  {$EXTERNALSYM TWEI_PAGENUMBER}
  TWEI_CAMERA                   = $123C;  { added 1.9 }
  {$EXTERNALSYM TWEI_CAMERA}
  TWEI_FRAMENUMBER              = $123D;  { added 1.9 }
  {$EXTERNALSYM TWEI_FRAMENUMBER}
  TWEI_FRAME                    = $123E;  { added 1.9 }
  {$EXTERNALSYM TWEI_FRAME}
  TWEI_PIXELFLAVOR              = $123F;  { added 1.9 }
  {$EXTERNALSYM TWEI_PIXELFLAVOR}
  TWEI_ICCPROFILE               = $1240;  { added 1.91 }
  {$EXTERNALSYM TWEI_ICCPROFILE}
  TWEI_LASTSEGMENT              = $1241;  { added 1.91 }
  {$EXTERNALSYM TWEI_LASTSEGMENT}
  TWEI_SEGMENTNUMBER            = $1242;  { added 1.91 }
  {$EXTERNALSYM TWEI_SEGMENTNUMBER}
  TWEI_MAGDATA                  = $1243;  { added 2.0 }
  {$EXTERNALSYM TWEI_MAGDATA}
  TWEI_MAGTYPE                  = $1244;  { added 2.0 }
  {$EXTERNALSYM TWEI_MAGTYPE}
  TWEI_PAGESIDE                 = $1245;  { added 2.0 }
  {$EXTERNALSYM TWEI_PAGESIDE}
  TWEI_FILESYSTEMSOURCE         = $1246;  { added 2.0 }
  {$EXTERNALSYM TWEI_FILESYSTEMSOURCE}
  TWEI_IMAGEMERGED              = $1247;  { added 2.1 }
  {$EXTERNALSYM TWEI_IMAGEMERGED}
  TWEI_MAGDATALENGTH            = $1248;  { added 2.1 }
  {$EXTERNALSYM TWEI_MAGDATALENGTH}
  TWEI_PAPERCOUNT               = $1249;
  {$EXTERNALSYM TWEI_PAPERCOUNT}

  TWEJ_NONE           = $0000;
  {$EXTERNALSYM TWEJ_NONE}
  TWEJ_MIDSEPARATOR   = $0001;
  {$EXTERNALSYM TWEJ_MIDSEPARATOR}
  TWEJ_PATCH1         = $0002;
  {$EXTERNALSYM TWEJ_PATCH1}
  TWEJ_PATCH2         = $0003;
  {$EXTERNALSYM TWEJ_PATCH2}
  TWEJ_PATCH3         = $0004;
  {$EXTERNALSYM TWEJ_PATCH3}
  TWEJ_PATCH4         = $0005;
  {$EXTERNALSYM TWEJ_PATCH4}
  TWEJ_PATCH6         = $0006;
  {$EXTERNALSYM TWEJ_PATCH6}
  TWEJ_PATCHT         = $0007;
  {$EXTERNALSYM TWEJ_PATCHT}

{**************************************************************************
 *      Return Codes and Condition Codes section                          *
 **************************************************************************}

{ Return Codes: DSM_Entry and DS_Entry may return any one of these values. }
  TWRC_CUSTOMBASE       = $8000;
  {$EXTERNALSYM TWRC_CUSTOMBASE}

  TWRC_SUCCESS          =  0;
  {$EXTERNALSYM TWRC_SUCCESS}
  TWRC_FAILURE          =  1; { Application may get TW_STATUS for info on failure }
  {$EXTERNALSYM TWRC_FAILURE}
  TWRC_CHECKSTATUS      =  2; { "tried hard": ; get status }
  {$EXTERNALSYM TWRC_CHECKSTATUS}
  TWRC_CANCEL           =  3;
  {$EXTERNALSYM TWRC_CANCEL}
  TWRC_DSEVENT          =  4;
  {$EXTERNALSYM TWRC_DSEVENT}
  TWRC_NOTDSEVENT       =  5;
  {$EXTERNALSYM TWRC_NOTDSEVENT}
  TWRC_XFERDONE         =  6;
  {$EXTERNALSYM TWRC_XFERDONE}
  TWRC_ENDOFLIST        =  7; { After MSG_GETNEXT if nothing left }
  {$EXTERNALSYM TWRC_ENDOFLIST}
  TWRC_INFONOTSUPPORTED =  8;
  {$EXTERNALSYM TWRC_INFONOTSUPPORTED}
  TWRC_DATANOTAVAILABLE =  9;
  {$EXTERNALSYM TWRC_DATANOTAVAILABLE}
  TWRC_BUSY             =  10;
  {$EXTERNALSYM TWRC_BUSY}
  TWRC_SCANNERLOCKED    =  11;
  {$EXTERNALSYM TWRC_SCANNERLOCKED}

{ Condition Codes: Application gets these by doing DG_CONTROL DAT_STATUS MSG_GET. }
  TWCC_CUSTOMBASE       = $8000;
  {$EXTERNALSYM TWCC_CUSTOMBASE}

  TWCC_SUCCESS           =  0; { It worked! }
  {$EXTERNALSYM TWCC_SUCCESS}
  TWCC_BUMMER            =  1; { Failure due to unknown causes }
  {$EXTERNALSYM TWCC_BUMMER}
  TWCC_LOWMEMORY         =  2; { Not enough memory to perform operation }
  {$EXTERNALSYM TWCC_LOWMEMORY}
  TWCC_NODS              =  3; { No Data Source }
  {$EXTERNALSYM TWCC_NODS}
  TWCC_MAXCONNECTIONS    =  4; { DS is connected to max possible applications }
  {$EXTERNALSYM TWCC_MAXCONNECTIONS}
  TWCC_OPERATIONERROR    =  5; { DS or DSM reported error, application shouldn't }
  {$EXTERNALSYM TWCC_OPERATIONERROR}
  TWCC_BADCAP            =  6; { Unknown capability }
  {$EXTERNALSYM TWCC_BADCAP}
  TWCC_BADPROTOCOL       =  9; { Unrecognized MSG DG DAT combination }
  {$EXTERNALSYM TWCC_BADPROTOCOL}
  TWCC_BADVALUE          =  10; { Data parameter out of range }
  {$EXTERNALSYM TWCC_BADVALUE}
  TWCC_SEQERROR          =  11; { DG DAT MSG out of expected sequence }
  {$EXTERNALSYM TWCC_SEQERROR}
  TWCC_BADDEST           =  12; { Unknown destination Application/Source in DSM_Entry }
  {$EXTERNALSYM TWCC_BADDEST}
  TWCC_CAPUNSUPPORTED    =  13; { Capability not supported by source }
  {$EXTERNALSYM TWCC_CAPUNSUPPORTED}
  TWCC_CAPBADOPERATION   =  14; { Operation not supported by capability }
  {$EXTERNALSYM TWCC_CAPBADOPERATION}
  TWCC_CAPSEQERROR       =  15; { Capability has dependancy on other capability }
  {$EXTERNALSYM TWCC_CAPSEQERROR}
  TWCC_DENIED            =  16; { File System operation is denied (file is protected) Added 1.8 }
  {$EXTERNALSYM TWCC_DENIED}
  TWCC_FILEEXISTS        =  17; { Operation failed because file already exists.       Added 1.8 }
  {$EXTERNALSYM TWCC_FILEEXISTS}
  TWCC_FILENOTFOUND      =  18; { File not found                                      Added 1.8 }
  {$EXTERNALSYM TWCC_FILENOTFOUND}
  TWCC_NOTEMPTY          =  19; { Operation failed because directory is not empty     Added 1.8 }
  {$EXTERNALSYM TWCC_NOTEMPTY}
  TWCC_PAPERJAM          =  20; { The feeder is jammed                                Added 1.8 }
  {$EXTERNALSYM TWCC_PAPERJAM}
  TWCC_PAPERDOUBLEFEED   =  21; { The feeder detected multiple pages                  Added 1.8 }
  {$EXTERNALSYM TWCC_PAPERDOUBLEFEED}
  TWCC_FILEWRITEERROR    =  22; { Error writing the file (meant for things like disk full conditions) Added 1.8 }
  {$EXTERNALSYM TWCC_FILEWRITEERROR}
  TWCC_CHECKDEVICEONLINE =  23; { The device went offline prior to or during this operation Added 1.8 }
  {$EXTERNALSYM TWCC_CHECKDEVICEONLINE}
  TWCC_INTERLOCK          = 24; { Added 2.0 }
  {$EXTERNALSYM TWCC_INTERLOCK}
  TWCC_DAMAGEDCORNER      = 25; { Added 2.0 }
  {$EXTERNALSYM TWCC_DAMAGEDCORNER}
  TWCC_FOCUSERROR         = 26; { Added 2.0 }
  {$EXTERNALSYM TWCC_FOCUSERROR}
  TWCC_DOCTOOLIGHT        = 27; { Added 2.0 }
  {$EXTERNALSYM TWCC_DOCTOOLIGHT}
  TWCC_DOCTOODARK         = 28; { Added 2.0 }
  {$EXTERNALSYM TWCC_DOCTOODARK}
  TWCC_NOMEDIA            = 29; { Added 2.1 }
  {$EXTERNALSYM TWCC_NOMEDIA}

{ bit patterns: for query the operation that are supported by the data source on a capability }
{ Application gets these through DG_CONTROL/DAT_CAPABILITY/MSG_QUERYSUPPORT }
{ Added 1.6 }
  TWQC_GET        = $0001;
  {$EXTERNALSYM TWQC_GET}
  TWQC_SET        = $0002;
  {$EXTERNALSYM TWQC_SET}
  TWQC_GETDEFAULT = $0004;
  {$EXTERNALSYM TWQC_GETDEFAULT}
  TWQC_GETCURRENT = $0008;
  {$EXTERNALSYM TWQC_GETCURRENT}
  TWQC_RESET      = $0010;
  {$EXTERNALSYM TWQC_RESET}
  TWQC_SETCONSTRAINT = $0020;
  {$EXTERNALSYM TWQC_SETCONSTRAINT}
  TWQC_CONSTRAINABLE = $0040;
  {$EXTERNALSYM TWQC_CONSTRAINABLE}

{***************************************************************************
 * Depreciated Items                                                       *
 ***************************************************************************}
type
  HPBYTE = ^BYTE;
  {$EXTERNALSYM HPBYTE}
  THPByte = HPBYTE;

  HPVOID = Pointer;
  {$EXTERNALSYM HPVOID}
  THPVoid = HPVOID;

  TW_STR1024 = array[0..1025] of Byte;   // unsigned char  TW_STR1024[1026]
  {$EXTERNALSYM TW_STR1024}
  pTW_STR1024 = ^TW_STR1024;
  {$EXTERNALSYM pTW_STR1024}
  TTWStr1024 = TW_STR1024;               // added 1.9
  PTWStr1024 = pTW_STR1024;

  TW_UNI512 = array[0..511] of WideChar; // wchar_t TW_UNI512[512]
  {$EXTERNALSYM TW_UNI512}
  pTW_UNI512 = ^TW_UNI512;
  {$EXTERNALSYM pTW_UNI512}
  TTWUni512 = TW_UNI512;                 // added 1.9
  PTWUni512 = pTW_UNI512;

const
  TWTY_STR1024          = $000d;  { Means Item is a TW_STR1024...added 1.9 }
  {$EXTERNALSYM TWTY_STR1024}
  TWTY_UNI512           = $000e;  { Means Item is a TW_UNI512...added 1.9 }
  {$EXTERNALSYM TWTY_UNI512}

  TWFF_JPN              = 12;    { 1.91 }
  {$EXTERNALSYM TWFF_JPN}

  DAT_TWUNKIDENTITY     = $000b; { Additional message required for thunker to request the special identity information. }
  {$EXTERNALSYM DAT_TWUNKIDENTITY}
  DAT_SETUPFILEXFER2    = $0301;  { New file xfer operation }
  {$EXTERNALSYM DAT_SETUPFILEXFER2}

{ LM: declared twice in twain.h. Is this depreciated or not? }
{  CAP_SUPPORTEDCAPSEXT = $100c;  } { Added 1.6 }
  {--$EXTERNALSYM CAP_SUPPORTEDCAPSEXT}
{ LM: ???? }
{  CAP_FILESYSTEM       = $????;  }
  {--$EXTERNALSYM CAP_FILESYSTEM}
  CAP_PAGEMULTIPLEACQUIRE       = $1023;  { Added 1.8 }
  {$EXTERNALSYM CAP_PAGEMULTIPLEACQUIRE}
  CAP_PAPERBINDING              = $102f;  { Added 1.8 }
  {$EXTERNALSYM CAP_PAPERBINDING}
  CAP_PASSTHRU                  = $1031;  { Added 1.8 }
  {$EXTERNALSYM CAP_PASSTHRU}
  CAP_POWERDOWNTIME             = $1034;  { Added 1.8 } //0x1034 is reused by CAP_CAMERASIDE
  {$EXTERNALSYM CAP_POWERDOWNTIME}
  ACAP_AUDIOFILEFORMAT          = $1201;  { Added 1.8 }
  {$EXTERNALSYM ACAP_AUDIOFILEFORMAT}

//  MSG_CHECKSTATUS       = $0201;  { Get status information - use MSG_GET instead }
//  {$EXTERNALSYM MSG_CHECKSTATUS}

  MSG_INVOKE_CALLBACK   = $0903;  { Mac Only, deprecated - use DAT_NULL and MSG_xxx instead }
  {$EXTERNALSYM MSG_INVOKE_CALLBACK}

  TWSX_FILE2    = 3;               { added 1.9 }
  {$EXTERNALSYM TWSX_FILE2}

{ CAP_FILESYSTEM values (FS_ means file system) }
  TWFS_FILESYSTEM      = 0;
  {$EXTERNALSYM TWFS_FILESYSTEM}
  TWFS_RECURSIVEDELETE = 1;
  {$EXTERNALSYM TWFS_RECURSIVEDELETE}

{ ICAP_PIXELTYPE values (PT_ means Pixel Type) }
  TWPT_SRGB64     = 11; { 1.91 }
  {$EXTERNALSYM TWPT_SRGB64}
  TWPT_BGR        = 12; { 1.91 }
  {$EXTERNALSYM TWPT_BGR}
  TWPT_CIELAB     = 13; { 1.91 }
  {$EXTERNALSYM TWPT_CIELAB}
  TWPT_CIELUV     = 14; { 1.91 }
  {$EXTERNALSYM TWPT_CIELUV}
  TWPT_YCBCR      = 15; { 1.91 }
  {$EXTERNALSYM TWPT_YCBCR}

{ ICAP_SUPPORTEDSIZES values (SS_ means Supported Sizes) }
  TWSS_B           = 8;
  {$EXTERNALSYM TWSS_B}
  TWSS_A4LETTER    = TWSS_A4;      { use TWSS_A4 instead }
  {$EXTERNALSYM TWSS_A4LETTER}
  TWSS_B3          = TWSS_ISOB3;   { use TWSS_ISOB3 instead }
  {$EXTERNALSYM TWSS_B3}
  TWSS_B4          = TWSS_ISOB4;   { use TWSS_ISOB4 instead }
  {$EXTERNALSYM TWSS_B4}
  TWSS_B6          = TWSS_ISOB6;   { use TWSS_ISOB6 instead }
  {$EXTERNALSYM TWSS_B6}
  TWSS_B5LETTER    = TWSS_JISB5;   { use TWSS_JISB5 instead }
  {$EXTERNALSYM TWSS_B5LETTER}

{ ACAP_AUDIOFILEFORMAT values (AF_ means audio format) Added 1.8 }
  TWAF_WAV   = 0;
  {$EXTERNALSYM TWAF_WAV}
  TWAF_AIFF  = 1;
  {$EXTERNALSYM TWAF_AIFF}
  TWAF_AU    = 3;
  {$EXTERNALSYM TWAF_AU}
  TWAF_SND   = 4;
  {$EXTERNALSYM TWAF_SND}

type
{ DAT_SETUPFILEXFER2. Sets up DS to application data transfer via a file. Added 1.9 }
  TW_SETUPFILEXFER2 = record
    FileName      : TW_MEMREF; { Pointer to file name text }
    FileNameType  : TW_UINT16; { TWTY_STR1024 or TWTY_UNI512 }
    Format        : TW_UINT16; { Any TWFF_ constant }
    VRefNum       : TW_INT16;  { Used for Mac only  }
    parID         : TW_UINT32; { Used for Mac only }
  end;
  {$EXTERNALSYM TW_SETUPFILEXFER2}
  pTW_SETUPFILEXFER2 = ^TW_SETUPFILEXFER2;
  {$EXTERNALSYM pTW_SETUPFILEXFER2}
  TTWSetupFileXFER2 = TW_SETUPFILEXFER2;
  PTWSetupFileXFER2 = pTW_SETUPFILEXFER2;

{* SDH - 03/21/95 - TWUNK *
 * DAT_TWUNKIDENTITY. Provides DS identity and 'other' information necessary *
 *                    across thunk link. *}
  TW_TWUNKIDENTITY = record
    identity  : TW_IDENTITY; { Identity of data source. }
    dsPath    : TW_STR255;   { Full path and file name of data source. }
  end;
  {$EXTERNALSYM TW_TWUNKIDENTITY}
  pTW_TWUNKIDENTITY = ^TW_TWUNKIDENTITY;
  {$EXTERNALSYM pTW_TWUNKIDENTITY}
  TTWTwunkIdentity = TW_TWUNKIDENTITY;
  PTWTwunkIdentity = pTW_TWUNKIDENTITY;

{* SDH - 03/21/95 - TWUNK */
 * Provides DS_Entry parameters over thunk link. *
 *  SDH - 03/23/95 - WATCH                                                  *
 *  The thunker requires knowledge about size of data being passed in the   *
 *  lpData parameter to DS_Entry (which is not readily available due to     *
 *  type LPVOID.  Thus, we key off the DAT_ argument to determine the size. *
 *  This has a couple implications:                                         *
 *  1) Any additional DAT_ features require modifications to the thunk code *
 *     for thunker support.                                                 *
 *  2) Any applications which use the custom capabailites are not supported *
 *     under thunking since we have no way of knowing what size data (if    *
 *     any) is being passed.                                                *}
  TW_TWUNKDSENTRYPARAMS = record
    destFlag    : TW_INT8;     { TRUE if dest is not NULL }
 {$IFDEF DELPHI_6_PRIOR}
    pad         : Byte;        { LM: for alignment }
 {$ENDIF}
    dest        : TW_IDENTITY; { Identity of data source (if used) }
    dataGroup   : TW_INT32;    { DSM_Entry dataGroup parameter }
    dataArgType : TW_INT16;    { DSM_Entry dataArgType parameter }
    message     : TW_INT16;    { DSM_Entry message parameter }
    pDataSize   : TW_INT32;    { Size of pData (0 if NULL) }
    //pData     : TW_MEMREF;   { Based on implementation specifics, a }
                               { pData parameter makes no sense in this }
                               { structure, but data (if provided) will be }
                               { appended in the data block. }
  end;
  {$EXTERNALSYM TW_TWUNKDSENTRYPARAMS}
  pTW_TWUNKDSENTRYPARAMS = ^TW_TWUNKDSENTRYPARAMS;
  {$EXTERNALSYM pTW_TWUNKDSENTRYPARAMS}
  TTWTwunkDSEntryParams = TW_TWUNKDSENTRYPARAMS;
  PTWTwunkDSEntryParams = pTW_TWUNKDSENTRYPARAMS;

{ SDH - 03/21/95 - TWUNK }
{ Provides DS_Entry results over thunk link. }
  TW_TWUNKDSENTRYRETURN = record
    returnCode    : TW_UINT16; { Thunker DsEntry return code. }
    conditionCode : TW_UINT16; { Thunker DsEntry condition code. }
    pDataSize     : TW_INT32;  { Size of pData (0 if NULL) }
    //pData       : TW_MEMREF; { Based on implementation specifics, a }
                               { pData parameter makes no sense in this }
                               { structure, but data (if provided) will be }
                               { appended in the data block. }
  end;
  {$EXTERNALSYM TW_TWUNKDSENTRYRETURN}
  pTW_TWUNKDSENTRYRETURN = ^TW_TWUNKDSENTRYRETURN;
  {$EXTERNALSYM pTW_TWUNKDSENTRYRETURN}
  TTWTwunkDSEntryReturn = TW_TWUNKDSENTRYRETURN;
  PTWTwunkDSEntryReturn = pTW_TWUNKDSENTRYRETURN;

{ WJD - 950818 }
{ Added for 1.6 Specification }
{ TWAIN 1.6 CAP_SUPPORTEDCAPSEXT structure }
  TW_CAPEXT = record
    Cap         : TW_UINT16;  { Which CAP/ICAP info is relevant to }
    Properties  : TW_UINT16;  { Messages this CAP/ICAP supports }
  end;
  {$EXTERNALSYM TW_CAPEXT}
  pTW_CAPEXT = ^TW_CAPEXT;
  {$EXTERNALSYM pTW_CAPEXT}
  TTWCapExt = TW_CAPEXT;
  PTWCapExt = pTW_CAPEXT;

{ DAT_SETUPAUDIOFILEXFER, information required to setup an audio file transfer }
  TW_SETUPAUDIOFILEXFER = record
    FileName  : TW_STR255; { full path target file }
    Format    : TW_UINT16; { one of TWAF_xxxx }
    VRefNum   : TW_INT16;
  end;
  {$EXTERNALSYM TW_SETUPAUDIOFILEXFER}
  pTW_SETUPAUDIOFILEXFER = ^TW_SETUPAUDIOFILEXFER;
  {$EXTERNALSYM pTW_SETUPAUDIOFILEXFER}
  TTWSetupAudioFileXFER = TW_SETUPAUDIOFILEXFER;
  PTWSetupAudioFileXFER = pTW_SETUPAUDIOFILEXFER;

{***************************************************************************
 * Entry Points                                                            *
 ***************************************************************************}

{**********************************************************************
 * Function: DSM_Entry, the only entry point into the Data Source Manager.
 *
 * Parameters:
 *  pOrigin Identifies the source module of the message. This could
 *          identify an Application, a Source, or the Source Manager.
 *
 *  pDest   Identifies the destination module for the message.
 *          This could identify an application or a data source.
 *          If this is NULL, the message goes to the Source Manager.
 *
 *  DG      The Data Group.
 *          Example: DG_IMAGE.
 *
 *  DAT     The Data Attribute Type.
 *          Example: DAT_IMAGEMEMXFER.
 *
 *  MSG     The message.  Messages are interpreted by the destination module
 *          with respect to the Data Group and the Data Attribute Type.
 *          Example: MSG_GET.
 *
 *  pData   A pointer to the data structure or variable identified
 *          by the Data Attribute Type.
 *          Example: (TW_MEMREF)&ImageMemXfer
 *                   where ImageMemXfer is a TW_IMAGEMEMXFER structure.
 *
 * Returns:
 *  ReturnCode
 *         Example: TWRC_SUCCESS.
 *
 ********************************************************************}
type
  DSMENTRYPROC = function(pOrigin: pTW_IDENTITY;
                          pDest: pTW_IDENTITY;
                          DG: TW_UINT32;
                          DAT: TW_UINT16;
                          MSG: TW_UINT16;
                          pData: TW_MEMREF): TW_UINT16; stdcall;
  {$EXTERNALSYM DSMENTRYPROC}
  TDSMEntryProc = DSMENTRYPROC;

var
  DSM_Entry: TDSMEntryProc = nil;

{**********************************************************************
 * Function: DS_Entry, the entry point provided by a Data Source.
 *
 * Parameters:
 *  pOrigin Identifies the source module of the message. This could
 *          identify an application or the Data Source Manager.
 *
 *  DG      The Data Group.
 *          Example: DG_IMAGE.
 *
 *  DAT     The Data Attribute Type.
 *          Example: DAT_IMAGEMEMXFER.
 *
 *  MSG     The message.  Messages are interpreted by the data source
 *          with respect to the Data Group and the Data Attribute Type.
 *          Example: MSG_GET.
 *
 *  pData   A pointer to the data structure or variable identified
 *          by the Data Attribute Type.
 *          Example: (TW_MEMREF)&ImageMemXfer
 *                   where ImageMemXfer is a TW_IMAGEMEMXFER structure.
 *
 * Returns:
 *  ReturnCode
 *          Example: TWRC_SUCCESS.
 *
 * Note:
 *  The DSPROC type is only used by an application when it calls
 *  a Data Source directly, bypassing the Data Source Manager.
 *
 ********************************************************************}
type
  DSENTRYPROC = function(pOrigin: pTW_IDENTITY;
                         DG: TW_UINT32;
                         DAT: TW_UINT16;
                         MSG: TW_UINT16;
                         pData: TW_MEMREF): TW_UINT16; stdcall;
  {$EXTERNALSYM DSENTRYPROC}
  TDSEntryProc = DSENTRYPROC;

var
  DS_Entry: TDSEntryProc = nil;

{**********************************************************************
 * Function: TWAIN_Callback
 ********************************************************************}
type
  TWAINCALLBACKPROC = function(pOrigin: pTW_IDENTITY;
                               pDest: pTW_IDENTITY;
                               DG: TW_UINT32;
                               DAT: TW_UINT16;
                               MSG: TW_UINT16;
                               pData: TW_MEMREF): TW_UINT16; stdcall;
  {$EXTERNALSYM TWAINCALLBACKPROC}
  TTwainCallbackProc = TWAINCALLBACKPROC;

var
  TWAIN_Callback: TTwainCallbackProc = nil;

type
  DSM_MEMALLOCATE = function(_size: TW_UINT32): TW_HANDLE; stdcall;
  {$EXTERNALSYM DSM_MEMALLOCATE}
  TDSM_MemAllocate = DSM_MEMALLOCATE;

  DSM_MEMFREE = procedure(_handle: TW_HANDLE); stdcall;
  {$EXTERNALSYM DSM_MEMFREE}
  TDSM_MemFree = DSM_MEMFREE;

  DSM_MEMLOCK = function(_handle: TW_HANDLE): TW_MEMREF; stdcall;
  {$EXTERNALSYM DSM_MEMLOCK}
  TDSM_MemLock = DSM_MEMLOCK;

  DSM_MEMUNLOCK = procedure(_handle: TW_HANDLE); stdcall;
  {$EXTERNALSYM DSM_MEMUNLOCK}
  TDSM_MemUnlock = DSM_MEMUNLOCK;

{ DAT_ENTRYPOINT. returns essential entry points. }
  TW_ENTRYPOINT = record
     Size             : TW_UINT32;
     DSM_Entry        : DSMENTRYPROC;
     DSM_MemAllocate  : DSM_MEMALLOCATE;
     DSM_MemFree      : DSM_MEMFREE;
     DSM_MemLock      : DSM_MEMLOCK;
     DSM_MemUnlock    : DSM_MEMUNLOCK;
  end;
  {$EXTERNALSYM TW_ENTRYPOINT}
  pTW_ENTRYPOINT = ^TW_ENTRYPOINT;
  {$EXTERNALSYM pTW_ENTRYPOINT}
  TTWEntryPoint = TW_ENTRYPOINT;
  PTWEntryPoint = pTW_ENTRYPOINT;

{ DAT_FILTER }
  TW_FILTER_DESCRIPTOR = record
    Size        : TW_UINT32;
    HueStart    : TW_UINT32;
    HueEnd      : TW_UINT32;
    SaturationStart : TW_UINT32;
    SaturationEnd   : TW_UINT32;
    ValueStart  : TW_UINT32;
    ValueEnd    : TW_UINT32;
    Replacement : TW_UINT32;
  end;
  {$EXTERNALSYM TW_FILTER_DESCRIPTOR}
  pTW_FILTER_DESCRIPTOR = ^TW_FILTER_DESCRIPTOR;
  {$EXTERNALSYM pTW_FILTER_DESCRIPTOR}
  TTWFilterDescriptor = TW_FILTER_DESCRIPTOR;
  PTWFilterDescriptor = pTW_FILTER_DESCRIPTOR;

{ DAT_FILTER }
  TW_FILTER = record
    Size            : TW_UINT32;
    DescriptorCount : TW_UINT32;
    MaxDescriptorCount : TW_UINT32;
    Condition       : TW_UINT32;
    hDescriptors    : TW_HANDLE;
  end;
  {$EXTERNALSYM TW_FILTER}
  pTW_FILTER = ^TW_FILTER;
  {$EXTERNALSYM pTW_FILTER}
  TTWFilter = TW_FILTER;
  PTWFilter = pTW_FILTER;

implementation

end.

