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
  Classes, SysUtils, Twain;

const
  {Name of the Twain library for 32 bits enviroment}
  TWAINLIBRARY_64 = 'TWAINDSM.DLL';
  TWAINLIBRARY_32 = 'TWAIN_32.DLL';

  {$IFDEF WIN64}
  TWAINLIBRARY = TWAINLIBRARY_64;
  {$ELSE}
  TWAINLIBRARY = TWAINLIBRARY_32;
  {$ENDIF}

  {Error codes}
  ERROR_BASE              = 300;
  ERROR_INT16: TW_INT16   = HIGH(TW_INT16);

  VirtualWinClassName: array[0..15] of WideChar =
  ('D', 'e', 'l', 'p', 'h', 'i','T', 'w', 'a', 'i', 'n', 'P', 'a', 'c', 'k', #0);


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

  //Dinamic Array types
  TArraySingle = array of Single;
  TArrayInteger = array of Integer;
  TStringArray = array of String;
  TArrayTW_IDENTITY = array of TW_IDENTITY;

  {From twain}
  TW_STR255 = Twain.TW_STR255;

  {File formats}
  TTwainFormat = (tfTIFF, tfPict, tfBMP, tfXBM, tfJPEG, tfFPX,
    tfTIFFMulti, tfPNG, tfSPIFF, tfEXIF, tfUnknown);

  {Twain units}
  TTwainUnit = (tuInches, tuCentimeters, tuPicas, tuPoints, tuTwips,
    tuPixels, tuUnknown);
  TTwainUnitSet = set of TTwainUnit;

  {Twain pixel flavor}
  TTwainPixelFlavor = (tpfChocolate, tpfVanilla, tpfUnknown);
  TTwainPixelFlavorSet = set of TTwainPixelFlavor;

  {Orientation}
  TTwainOrientation = (torPortrait, torLandscape);

  {Paper size}
  TPaperSize = packed record
    name:String[16];
    w, h:Single;
  end;

  TTwainPaperSize = (tpsNONE, tpsA4, tpsJISB5, tpsUSLETTER, tpsUSLEGAL, tpsA5, tpsISOB4, tpsISOB6,
   tpsUSLEDGER, tpsUSEXECUTIVE, tpsA3, tpsISOB3, tpsA6, tpsC4, tpsC5, tpsC6, tps4A0,
   tps2A0, tpsA0, tpsA1, tpsA2, tpsA7, tpsA8, tpsA9, tpsA10, tpsISOB0, tpsISOB1,
   tpsISOB2, tpsISOB5, tpsISOB7, tpsISOB8, tpsISOB9, tpsISOB10, tpsJISB0, tpsJISB1,
   tpsJISB2, tpsJISB3, tpsJISB4, tpsJISB6, tpsJISB7, tpsJISB8, tpsJISB9, tpsJISB10,
   tpsC0, tpsC1, tpsC2, tpsC3, tpsC7, tpsC8, tpsC9, tpsC10, tpsUSSTATEMENT, tpsBUSINESSCARD,
   tpsMAXSIZE);
  TTwainPaperSizeSet = set of TTwainPaperSize;

  {Auto size}
  TTwainAutoSize = (tasNone, tasAuto, tasCurrent);

  {Twain pixel type}
  TTwainPixelType = (tbdBw, tbdGray, tbdRgb, tbdPalette, tbdCmy, tbdCmyk,
    tbdYuv, tbdYuvk, tbdCieXYZ, tbdUnknown, tbdUnknown1, tbdUnknown2, tbdBgr);
  TTwainPixelTypeSet = set of TTwainPixelType;

  {Twain bit depth}
  TTwainBitDepth = array of TW_UINT16;

  {Twain resolutions}
  TTwainResolution = TArraySingle;

  {Available twain languages}
  TTwainLanguage = ({-1}tlUserLocale = -1, tlDanish, tlDutch, tlInternationalEnglish,
    tlFrenchCanadian, tlFinnish, tlFrench, tlGerman, tlIcelandic, tlItalian,
    tlNorwegian, tlPortuguese, tlSpanish, tlSwedish, tlUsEnglish,
    tlAfrikaans, tlAlbania, tlArabic, tlArabicAlgeria, tlArabicBahrain, {18}
    tlArabicEgypt, tlArabicIraq, tlArabJordan, tlArabicKuwait,
    tlArabicLebanon, tlArabicLibya, tlArabicMorocco, tlArabicOman,
    tlArabicQatar, tlArabicSaudiarabia, tlArabicSyria, tlArabicTunisia,
    tlArabicUae, tlArabicYemen, tlBasque, tlByelorussian, tlBulgarian,  {35}
    tlCatalan, tlChinese, tlChineseHongkong, tlChinesePeoplesRepublic,
    tlChineseSingapore, tlChineseSimplified, tlChineseTwain, {42}
    tlChineseTraditional, tlCroatia, tlCzech, tlDutchBelgian, {46}
    tlEnglishAustralian, tlEnglishCanadian, tlEnglishIreland,
    tlEnglishNewZealand, tlEnglishSouthAfrica, tlEnglishUk, {52}
    tlEstonian, tlFaeroese, tlFarsi, tlFrenchBelgian, tlFrenchLuxembourg, {57}
    tlFrenchSwiss, tlGermanAustrian, tlGermanLuxembourg, tlGermanLiechtenstein,
    tlGermanSwiss, tlGreek, tlHebrew, tlHungarian, tlIndonesian, {66}
    tlItalianSwiss, tlJapanese, tlKorean, tlKoreanJohab, tlLatvian, {71}
    tlLithuanian, tlNorewgianBokmal, tlNorwegianNynorsk, tlPolish, {75}
    tlPortugueseBrazil, tlRomanian, tlRussian, tlSerbianLatin,
    tlSlovak, tlSlovenian, tlSpanishMexican, tlSpanishModern, tlThai,
    tlTurkish, tlUkranian, tlAssamese, tlBengali, tlBihari, tlBodo,
    tlDogri, tlGujarati {92}, tlHarayanvi, tlHindi, tlKannada, tlKashmiri,
    tlMalayalam, tlMarathi, tlMarwari, tlMeghalayan, tlMizo, tlNaga {102},
    tlOrissi, tlPunjabi, tlPushtu, tlSerbianCyrillic, tlSikkimi,
    tlSwedishFinland, tlTamil, tlTelugu, tlTripuri, tlUrdu, tlVietnamese);
  {Twain supported groups}
  TTwainGroups = set of (tgControl, tgImage, tgAudio, tgDSM2, tgAPP2, tgDS2);

  {Transfer mode for twain}
  TTwainTransferMode = (ttmFile, ttmNative, ttmMemory);

  {rect for LAYOUT; npeter 2004.01.12.}
  TTwainRect =
   record
    Left:   double;
    Top:    double;
    Right:  double;
    Bottom: double;
   end;

  {Return set for capability retrieving/setting}
  TCapabilityRet = (crSuccess, crUnsupported, crBadOperation, crDependencyError,
    crLowMemory, crInvalidState, crInvalidContainer);

  {Kinds of capability retrieving}
  TRetrieveCap = (rcGet, rcGetCurrent, rcGetDefault);

  {Kinds of capability operation}
  TCapabilityOperation = (capGet, capGetCurrent, capGetDefault, capReset, capResetAll, capSet, capSetConstraint);
  TCapabilityOperationSet = set of TCapabilityOperation;

  {Capability list type}
  TSetCapabilityList = array of pointer;

  TTwainPaperFeeding = (pfFlatbed, pfFeeder);
  TTwainPaperFeedingSet = set of TTwainPaperFeeding;

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
    PixelTypeCurrent,
    PixelTypeDefault:TTwainPixelType;
    PaperSizeCurrent,
    PaperSizeDefault: TTwainPaperSize;
    ResolutionCurrent,
    ResolutionDefault,
    ResolutionMin,
    ResolutionMax: Single;
    BitDepthCurrent,
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

