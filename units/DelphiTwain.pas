(******************************************************************************
*                FreePascal \ Delphi Twain Implementation                     *
*                                                                             *
*  FILE: DelphiTwain.pas                                                      *
*                                                                             *
*  VERSION:     2.3.1                                                         *
*                                                                             *
*  DESCRIPTION:                                                               *
*    Twain implementation base Classes                                        *
*                                                                             *
*******************************************************************************
*                                                                             *
*  (c) 2003 Gustavo Daud, 2025 Massimo Magnano                                *
*                                                                             *
*  See changelog.txt for Change Log                                           *
*                                                                             *
*******************************************************************************)
unit DelphiTwain;

{$I DelphiTwain.inc}

interface

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

{Used units}
uses
  SysUtils, Windows, Messages, Classes,
  {$IFDEF FPC}fptimer,{$ENDIF}
  Twain, DelphiTwainTypes, DelphiTwainUtils;

type
  {Forward declaration}
  TCustomDelphiTwain = class;

  {Component kinds}
  TTwainComponent = TObject;

  {Events}
  TOnTwainError = procedure(Sender: TObject; const Index: Integer; ErrorCode,
    Additional: Integer) of object;
  TOnSourceNotify = procedure(Sender: TObject; const Index: Integer) of object;
  TOnSourceBoolNotify = function(Sender: TObject; const Index: Integer): Boolean of object;
  TOnTransferComplete = procedure(Sender: TObject; const Index: Integer; const Canceled: Boolean) of object;
  TOnSourceFileTransfer = procedure(Sender: TObject; const Index: Integer;
    Filename: TW_STR255; Format: TTwainFormat; var Cancel: Boolean) of object;

  { TTwainIdentity }
  {Object to handle TW_IDENTITY}
  TTwainIdentity = class(TObject)
  private
    {Sets application language property}
    procedure SetLanguage(const Value: TTwainLanguage);
    {Sets text values}
    procedure SetString(const Index: Integer; const Value: String);
    {Sets avaliable groups}
    procedure SetGroups(const Value: TTwainGroups);

  protected
    {Structure which should be filled}
    Structure: TW_IDENTITY;

    function GetId: TW_UINT32;
    function GetCountryCode: Word;

    function GetMajorVersion: TW_UINT16;
    function GetMinorVersion: TW_UINT16;
    procedure SetCountryCode(const aCountryCode: Word);
    procedure SetMajorVersion(const aMajorVersion: TW_UINT16);
    procedure SetMinorVersion(const aMinorVersion: TW_UINT16);

    {Returns application language property}
    function GetLanguage: TTwainLanguage;
    {Returns text values}
    function GetString(const Index: integer): String;
    {Returns avaliable groups}
    function GetGroups: TTwainGroups;

  public
    {Object being created}
    constructor Create;
    {Copy properties from another TTwainIdentity}
    procedure Assign(Source: TObject);

    property ID: TW_UINT32 read GetId;
    {Application major version}
    property MajorVersion: TW_UINT16 read GetMajorVersion write SetMajorVersion;
    {Application minor version}
    property MinorVersion: TW_UINT16 read GetMinorVersion write SetMinorVersion;
    {Language}
    property Language: TTwainLanguage read GetLanguage write SetLanguage;
    {Country code}
    property CountryCode: Word read GetCountryCode write SetCountryCode;
    {Supported groups}
    property Groups: TTwainGroups read GetGroups write SetGroups;
    {Text values}
    property VersionInfo: String index 0 read GetString write SetString;
    {Scanner manufacturer}
    property Manufacturer: String index 1 read GetString write SetString;
    {Scanner product family}
    property ProductFamily: String index 2 read GetString write SetString;
    {Scanner product name}
    property ProductName: String index 3 read GetString write SetString;
  end;

  { TTwainSource }
  {Source object}
  TTwainSource = class(TTwainIdentity)
  private
    {Holds the item index}
    fIndex: Integer;
    {Transfer mode for the images}
    fTransferMode: TTwainTransferMode;
    {Stores if user interface should be shown}
    fShowUI: Boolean;
    {Stores if the source window is modal}
    fModal: Boolean;
    {Stores if the source is enabled}
    fEnabled: Boolean;
    {Stores if the source is loaded}
    fLoaded: Boolean;
    {Stores the owner}
    fOwner: TCustomDelphiTwain;

    {Used with property SourceManagerLoaded to test if the source manager}
    {is loaded or not.}
    function GetSourceManagerLoaded: Boolean;
    {Returns a pointer to the application}
    function GetAppInfo: pTW_IDENTITY;
    {Sets if the source is loaded}
    procedure SetLoaded(const Value: Boolean);
    {Sets if the source is enabled}
    procedure SetEnabled(const Value: Boolean);
    {Returns a pointer to the source pTW_IDENTITY}
    function GetStructure: pTW_IDENTITY;
    {Returns a resolution}
    function GetResolution(Capability: TW_UINT16; var Current, Default: Single;
      var Values: TTwainResolution): TCapabilityRet;

  protected
    rDownloaded,
    rDownload_Done,
    rDownload_Cancelled: Boolean;
    rDownload_Count: Integer;
    rDownload_Path,
    rDownload_Ext,
    rDownload_FileName: String;

    {Reads a native image}
    procedure ReadNative(nativeHandle: TW_UINT32; var Cancel: Boolean);
    {Reads the file image}
    procedure ReadFile(Name: TW_STR255; Format: TW_UINT16; var Cancel: Boolean);
    {Call event for memory image}
    procedure ReadMemory(imageHandle: HBitmap; var Cancel: Boolean);

    {Prepare image memory transference}
    function PrepareMemXfer(var BitmapHandle: HBitmap;
      var PixelType: TW_INT16): TW_UINT16;
    {Transfer image memory}
    function TransferImageMemory(var ImageHandle: HBitmap;
      {%H-}PixelType: TW_INT16): TW_UINT16;
    {Returns a pointer to the TW_IDENTITY for the application}
    property AppInfo: pTW_IDENTITY read GetAppInfo;
    {Method to transfer the images}
    procedure TransferImages;
    {Returns if the source manager is loaded}
    property SourceManagerLoaded: Boolean read GetSourceManagerLoaded;
    {Source configuration methods}
    {************************}

    {Gets an item and returns it in a string}
    procedure GetItem(var Return: String; ItemType: TW_UINT16; Data: Pointer);
    {Converts from a result to a TCapabilityRec}
    function ResultToCapabilityRec(const Value: TW_UINT16): TCapabilityRet;
    {Sets a capability}
    function SetCapabilityRec(const Capability, ConType: TW_UINT16;
      Data: HGLOBAL): TCapabilityRet;

    {Used with property PendingXfers}
    function GetPendingXfers: TW_INT16;

  public
    {Message received in the event loop}
    function ProcessMessage(const Msg: TMsg): Boolean;

    {Returns supported capability Operations}
    function GetCapabilitySupportedOp(const Capability: TW_UINT16):TCapabilityOperationSet;
    function CapabilityCanGet(const Capability: TW_UINT16):Boolean;
    function CapabilityCanSet(const Capability: TW_UINT16):Boolean;

    {Returns a capability structure}
    function GetCapabilityRec(const Capability: TW_UINT16;
      var Handle: HGLOBAL; Mode: TRetrieveCap;
      var Container: TW_UINT16): TCapabilityRet;
    {************************}

    {Returns an one value Capability}
    function GetOneValue(Capability: TW_UINT16; var Value:Integer;
      Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet; overload;
    function GetOneValue(Capability: TW_UINT16; var Value:Single;
      Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet; overload;
    function GetOneValue(Capability: TW_UINT16; var Value:Boolean;
      Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet; overload;
    function GetOneValue(Capability: TW_UINT16; var Value:String;
      Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet; overload;

    function GetOneValue(Capability: TW_UINT16;
      var ItemType: TW_UINT16; var Value: string;
      Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF};
      MemHandle: HGLOBAL{$IFDEF DEFAULTPARAM}=0{$ENDIF}): TCapabilityRet; overload;

    {Returns an range capability}
    function GetRangeValue(Capability: TW_UINT16; var Min, Max, Step, Default, Current:Integer): TCapabilityRet; overload;
    function GetRangeValue(Capability: TW_UINT16; var Min, Max, Step, Default, Current:Single): TCapabilityRet; overload;
    function GetRangeValue(Capability: TW_UINT16; var Min, Max, Step, Default, Current:String): TCapabilityRet; overload;

    function GetRangeValue(Capability: TW_UINT16; var ItemType: TW_UINT16;
      var Min, Max, Step, Default, Current: String;
      MemHandle: HGLOBAL{$IFDEF DEFAULTPARAM}=0{$ENDIF}): TCapabilityRet; overload;

    {Returns an enumeration capability}
    function GetEnumerationValue(Capability: TW_UINT16;
      var List:TArrayInteger; var Current, Default: Integer;
      Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet; overload;
    function GetEnumerationValue(Capability: TW_UINT16;
      var List:TArraySingle; var Current, Default: Single;
      Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet; overload;
    function GetEnumerationValue(Capability: TW_UINT16;
      var List:TStringArray; var Current, Default: String;
      Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet; overload;

    function GetEnumerationValue(Capability: TW_UINT16;
      var ItemType: TW_UINT16; var List: TStringArray; var Current,
      Default: Integer; Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF};
      MemHandle: HGLOBAL{$IFDEF DEFAULTPARAM}=0{$ENDIF}): TCapabilityRet; overload;

    {Returns an array capability}
    function GetArrayValue(Capability: TW_UINT16; var List:TArrayInteger): TCapabilityRet; overload;
    function GetArrayValue(Capability: TW_UINT16; var List:TArraySingle): TCapabilityRet; overload;
    function GetArrayValue(Capability: TW_UINT16; var List:TStringArray): TCapabilityRet; overload;

    function GetArrayValue(Capability: TW_UINT16; var ItemType: TW_UINT16;
      var List: TStringArray; MemHandle: HGLOBAL{$IFDEF DEFAULTPARAM}=0{$ENDIF}): TCapabilityRet; overload;

    {************************}
    {Sets an one value capability}
    function SetOneValue(Capability: TW_UINT16; Value: Single): TCapabilityRet; overload;
    function SetOneValue(Capability: TW_UINT16; Value: Boolean): TCapabilityRet; overload;

    function SetOneValue(Capability: TW_UINT16; ItemType: TW_UINT16;
      Value: Pointer): TCapabilityRet; overload;

    {Sets a range capability}
    function SetRangeValue(Capability, ItemType: TW_UINT16; Min, Max, Step,
      Current: TW_UINT32): TCapabilityRet;
    {Sets an enumeration capability}
    function SetEnumerationValue(Capability, ItemType: TW_UINT16;
      CurrentIndex: TW_UINT32; List: TSetCapabilityList): TCapabilityRet;
    {Sets an array capability}
    function SetArrayValue(Capability, ItemType: TW_UINT16;
      List: TSetCapabilityList): TCapabilityRet;

    {Setup file transfer}
    function SetupFileTransfer(APath, AFileName, AExt: String; Format: TTwainFormat): Boolean;

    //Enable Source and Start the Images Transfer in APath, if multiple pages is downloaded then
    // the file names are APath\AFileName-n.AExt where n is then Index (n is not present when 0)
    function Download(UserInterface: TW_USERINTERFACE; APath, AFileName, AExt: String;
                      Format: TTwainFormat): Integer; overload;
    function Download(UserInterface: TW_USERINTERFACE; APath, AFileName, AExt: String;
                      Format: TTwainFormat;
                      var DownloadedFiles: TStringArray; UseRelativePath: Boolean=False): Integer; overload;

    {Set source transfer mode}
    //function ChangeTransferMode(NewMode: TTwainTransferMode): TCapabilityRet;
    {Transfer mode for transfering images from the source to}
    {the component and finally to the application}
    property TransferMode: TTwainTransferMode read fTransferMode write fTransferMode;

    {Returns return status information}
    function GetReturnStatus: TW_UINT16;

    {Capability setting}
    {Set the number of images that the application wants to receive}
    function SetCapXferCount(Value: SmallInt): TCapabilityRet;
    {Returns the number of images that the source will return}
    function GetCapXferCount(var Return: SmallInt;
      Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;

    {Retrieve the unit measure for all quantities}
    function GetICapUnits(var Return: TTwainUnit;
      var Supported: TTwainUnitSet; Mode: TRetrieveCap
      {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    {Set the unit measure}
    function SetICapUnits(Value: TTwainUnit): TCapabilityRet;

    function SetImagelayoutFrame(const fLeft,fTop,fRight,
      fBottom: double): TCapabilityRet;

    function GetIndicators(var Value: Boolean): TCapabilityRet;
    function SetIndicators(Value: Boolean): TCapabilityRet;

    {Retrieve the pixel flavor values}
    function GetIPixelFlavor(var Return: TTwainPixelFlavor;
      var Supported: TTwainPixelFlavorSet; Mode: TRetrieveCap
      {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    {Set the pixel flavor values}
    function SetIPixelFlavor(Value: TTwainPixelFlavor): TCapabilityRet;

    {Get orientation}
    function GetOrientation(var Value: TTwainOrientation): TCapabilityRet;
    {Set orientation}
    function SetOrientation(Value: TTwainOrientation): TCapabilityRet;

    //Get Paper Feeding Set
    function GetPaperFeeding: TTwainPaperFeedingSet;
    //Set Paper Feeding
    function SetPaperFeeding(Value: TTwainPaperFeeding): TCapabilityRet;

    {Get Available Paper Sizes}
    function GetPaperSizeSet(var Current, Default:TTwainPaperSize; var Values:TTwainPaperSizeSet): TCapabilityRet;
    {Set paper size}
    function SetPaperSize(Value: TTwainPaperSize): TCapabilityRet;

    {Get Duplex}
    function GetDuplexEnabled(var Value: Boolean): TCapabilityRet;
    {Set Duplex}
    function SetDuplexEnabled(Value: Boolean): TCapabilityRet;

    {Set auto size}
    function SetAutoSize(Value: TTwainAutoSize): TCapabilityRet;

    {Set auto border detection}
    function SetAutoBorderDetection(Value: Boolean): TCapabilityRet;

    {Set auto rotate}
    function SetAutoRotate(Value: Boolean): TCapabilityRet;

    {Set auto Deskew}
    function SetAutoDeskew(Value: Boolean): TCapabilityRet;

    {Set undefined image size}
    function SetUndefinedImageSize(Value: Boolean): TCapabilityRet;

    {Returns bitdepth values}
    function GetIBitDepth(var Current, Default: Integer; var Values: TArrayInteger): TCapabilityRet;
    {Set current bitdepth value}
    function SetIBitDepth(Value: Word): TCapabilityRet;

    {Returns pixel type values}
    function GetIPixelType(var Current, Default: TTwainPixelType; var Values: TTwainPixelTypeSet): TCapabilityRet;
    {Set the pixel type value}
    function SetIPixelType(Value: TTwainPixelType): TCapabilityRet;

    {Returns X and Y resolutions}
    function GetIXResolution(var Current, Default: Single; var Values: TTwainResolution): TCapabilityRet;
    function GetIYResolution(var Current, Default: Single; var Values: TTwainResolution): TCapabilityRet;
    {Sets X and X resolutions}
    function SetIXResolution(Value: Single): TCapabilityRet;
    function SetIYResolution(Value: Single): TCapabilityRet;

    {Returns physical width and height}
    function GetIPhysicalWidth(var Return: Single; Mode: TRetrieveCap
      {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    function GetIPhysicalHeight(var Return: Single; Mode: TRetrieveCap
      {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;

    {Returns if user interface is controllable}
    function GetUIControllable(var Return: Boolean): TCapabilityRet;

    {Returns feeder is loaded or not}
    function GetFeederLoaded(var Return: Boolean): TCapabilityRet;
    {Returns/sets if feeder is enabled}
    function GetFeederEnabled(var Return: Boolean): TCapabilityRet;
    function SetFeederEnabled(Value: Boolean): TCapabilityRet;

    {Returns/sets if auto feed is enabled}
    function GetAutoFeed(var Return: Boolean): TCapabilityRet;
    function SetAutoFeed(Value: Boolean): TCapabilityRet;

    {Returns/sets if auto Scan is enabled}
    function GetAutoScan(var Return: Boolean): TCapabilityRet;
    function SetAutoScan(Value: Boolean): TCapabilityRet;

    //Get/set Contrast
    function GetContrast(var Return: Single): TCapabilityRet;
    function SetContrast(Value: Single): TCapabilityRet;

    //Get/set Contrast
    function GetBrightness(var Return: Single): TCapabilityRet;
    function SetBrightness(Value: Single): TCapabilityRet;

    //Get Capabilities for Current Selected Item
    function GetParamsCapabilities(var Value: TTwainParamsCapabilities): Boolean;

    {Object being created/destroyed}
    constructor Create(AOwner: TCustomDelphiTwain);
    destructor Destroy; override;

    {Enables the source}
    function EnableSource(ShowUI, Modal: Boolean; ParentWindow:TW_HANDLE=0): Boolean; overload;
    function EnableSource(UserInterface: TW_USERINTERFACE): Boolean; overload;

    {Disables the source}
    function DisableSource: Boolean;
    {Loads the source}
    function LoadSource: Boolean;
    {Unloads the source}
    function UnloadSource: Boolean;

    {Returns number of pending transfer}
    property PendingXfers: TW_INT16 read GetPendingXfers;

    property Downloaded: Boolean read rDownloaded;
    property Download_Cancelled: Boolean read rDownload_Cancelled;
    property Download_Count: Integer read rDownload_Count;
    property Download_Path: String read rDownload_Path;
    property Download_Ext: String read rDownload_Ext;
    property Download_FileName: String read rDownload_FileName;

    {Returns a pointer to the source identity}
    property SourceIdentity: pTW_IDENTITY read GetStructure;
    {Returns/sets if the source is enabled}
    property Enabled: Boolean read fEnabled write SetEnabled;
    {Returns/sets if this source is loaded}
    property Loaded: Boolean read fLoaded write SetLoaded;
    {Returns owner}
    property Owner: TCustomDelphiTwain read fOwner;
    {Source window is modal}
    property Modal: Boolean read fModal write fModal;
    {Sets if user interface should be shown}
    property ShowUI: Boolean read fShowUI write fShowUI;
    {Returns the item index}
    property Index: Integer read fIndex;
    {Convert properties from write/read to read only}
    {(read description on TTwainIdentity source)}
    property MajorVersion: TW_UINT16 read GetMajorVersion;
    property MinorVersion: TW_UINT16 read GetMinorVersion;
    property Language: TTwainLanguage read GetLanguage;
    property CountryCode: Word read GetCountryCode;
    property Groups: TTwainGroups read GetGroups;
    property VersionInfo: String index 0 read GetString;
    property Manufacturer: String index 1 read GetString;
    property ProductFamily: String index 2 read GetString;
    property ProductName: String index 3 read GetString;
  end;

  { TCustomDelphiTwain }
  {Component part}

  TTwainAcquireNativeEvent = procedure (Sender: TObject; const Index: Integer;
                                        nativeHandle: TW_UINT32; var Cancel: Boolean) of object;
  TTwainAcquireBitmapEvent = procedure (Sender: TObject; const Index: Integer;
                                        imageHandle:HBitmap; var Cancel: Boolean) of object;
  TAcquireProgressEvent = procedure (Sender: TObject; const Index: Integer;
                                     const imageHandle: HBitmap; const Current, Total: Integer) of object;

  TCustomDelphiTwain = class(TTwainComponent)
  private
    {Should contain the number of Twain sources loaded}
    fSourcesLoaded: Integer;

    {Event pointer holders}
    fOnSourceDisable: TOnSourceNotify;
    fOnAcquireCancel: TOnSourceNotify;
    fOnSourceSetupFileXfer: TOnSourceNotify;
    fOnSourceFileTransfer: TOnSourceFileTransfer;
    fOnAcquireError: TOnTwainError;
    fOnTransferComplete: TOnTransferComplete;

    fSelectedSourceIndex: Integer;

    {Contains list of source devices}
    DeviceList: TPointerList;
    {Contains a pointer to the structure with the application}
    {information}
    AppInfo: pTW_IDENTITY;
    {Holds the object to allow the user to set the application information}
    fInfo: TTwainIdentity;
    {Holds the handle for the virtual window which will receive}
    {twain message notifications}
    {Will hold Twain library handle}
    fHandle: HInst;
    {Holds if the component has enumerated the devices}
    fHasEnumerated: Boolean;
    {Holds twain dll procedure handle}
    fTwainProc: TDSMEntryProc;
    {Holds the transfer mode to be used}
    fTransferMode: TTwainTransferMode;
    {Contains if the library is loaded}
    fLibraryLoaded: Boolean;
    {Contains if the source manager was loaded}
    fSourceManagerLoaded: Boolean;
    rOnAcquireProgress: TAcquireProgressEvent;
    rOnTwainAcquireNative: TTwainAcquireNativeEvent;
    rOnTwainAcquireBitmap: TTwainAcquireBitmapEvent;
    rOnProcessMessages: TOnSourceBoolNotify;

    {Procedure to load and unload twain library and update property}
    procedure SetLibraryLoaded(const Value: Boolean);
    {Procedure to load or unloaded the twain source manager}
    procedure SetSourceManagerLoaded(const Value: Boolean);
    {Updates the application information object}
    procedure SetInfo(const Value: TTwainIdentity);
    {Returns the number of sources}
    function GetSourceCount: Integer;
    {Returns a source from the list}
    function GetSource(Index: Integer): TTwainSource;
    //Gets selected source
    function GetSelectedSource: TTwainSource;
    //Gets selected source index
    function GetSelectedSourceIndex: Integer;
    //Sets selected source index
    procedure SetSelectedSourceIndex(const Value: Integer);
    //Gets selected source ID
    function GetSelectedSourceID: TW_UINT32;
    //Sets selected source searching by ID if not the current selected
    procedure SetSelectedSourceID(AValue: TW_UINT32);

    //Refresh the VirtualWindow - usually needed when transfer was completed
    procedure RefreshVirtualWindow;

  protected
    fVirtualWindow: THandle;
    {$IFDEF FPC}
    fMessagesTimer: TFPTimer;
    {$ENDIF}

    procedure WndProc(var Message: TMessage);

    {Returns the default source}
    function GetDefaultSource: Integer;

    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;

    procedure DoCreateVirtualWindow; virtual;
    procedure DoDestroyVirtualWindow; virtual;

    procedure DoCreateTimer; virtual;
    procedure DoDestroyTimer; virtual;
    procedure DoMessagesTimer(Sender: TObject); virtual;
    procedure MessageTimer_Enable; virtual;
    procedure MessageTimer_Disable; virtual;

    function CustomSelectSource: Integer; virtual; abstract;
    function CustomGetParentWindow: TW_HANDLE; virtual;

    procedure DoTwainAcquireNative(Sender: TObject; const Index: Integer; nativeHandle: TW_UINT32;
                                   var NeedBitmap, Cancel: Boolean); virtual;
    procedure DoTwainAcquire(Sender: TObject; const Index: Integer; imageHandle:HBitmap;
                             var Cancel: Boolean); virtual;
    procedure DoAcquireProgress(Sender: TObject; const Index: Integer; const imageHandle: HBitmap;
                                const Current, Total: Integer); virtual;

  public
    {Clears the list of sources}
    procedure ClearDeviceList;

    {Allows Twain to display a dialog to let the user choose any source}
    {and returns the source index in the list}
    function SelectSource: Integer; overload;
    {Returns the number of loaded sources}
    property SourcesLoaded: Integer read fSourcesLoaded;
    {Enumerate the avaliable devices after Source Manager is loaded}
    function EnumerateDevices: Boolean;
    {Object being created}
    constructor Create; virtual;
    {Object being destroyed}
    destructor Destroy; override;
    {Loads twain library and returns if it loaded sucessfully}
    function LoadLibrary: Boolean;
    {Unloads twain and returns if it unloaded sucessfully}
    function UnloadLibrary: Boolean;
    {Loads twain source manager}
    function LoadSourceManager: Boolean;
    {Unloads the source manager}
    function UnloadSourceManager(forced: boolean): Boolean;
    {Returns the application TW_IDENTITY}

    {Finds a matching source and return Index}
    function FindSource(Value: pTW_IDENTITY): Integer; overload;
    function FindSource(AID: TW_UINT32): Integer; overload;
    function FindSource(Value: TTwainSource): Integer; overload;
    function FindSource(AManufacturer, AProductFamily, AProductName: String): Integer; overload;
    function FindSource(AProductName: String): Integer; overload;

    {Finds a matching source and return ID}
    function FindSourceID(Value: pTW_IDENTITY): TW_UINT32; overload;
    function FindSourceID(AID:TW_UINT32): Boolean; overload;
    function FindSourceID(Value: TTwainSource): TW_UINT32; overload;
    function FindSourceID(AManufacturer, AProductFamily, AProductName: String): TW_UINT32; overload;
    function FindSourceID(AProductName: String): TW_UINT32; overload;

    function SelectSource(Value: pTW_IDENTITY; Load: Boolean = False): TTwainSource; overload;
    function SelectSource(AID: TW_UINT32; Load: Boolean = False): TTwainSource; overload;
    function SelectSource(AManufacturer, AProductFamily, AProductName: String; Load: Boolean = False): TTwainSource; overload;
    function SelectSource(AProductName: String; Load: Boolean = False): TTwainSource; overload;

    property AppIdentity: pTW_IDENTITY read AppInfo;
    {Returns Twain library handle}
    property Handle: HInst read fHandle;
    {Returns virtual window that receives messages}
    property VirtualWindow: THandle read fVirtualWindow;
    {Returns a pointer to Twain only procedure}
    property TwainProc: TDSMEntryProc read fTwainProc;
    {Holds if the component has enumerated the devices}
    property HasEnumerated: Boolean read fHasEnumerated;
    {Returns a source}
    property Source[Index: Integer]: TTwainSource read GetSource;

    {Events}
    {Source being disabled}
    property OnSourceDisable: TOnSourceNotify read fOnSourceDisable
      write fOnSourceDisable;
    {Acquire cancelled}
    property OnAcquireCancel: TOnSourceNotify read fOnAcquireCancel
      write fOnAcquireCancel;
    {User should set information to prepare for the file transfer}
    property OnSourceSetupFileXfer: TOnSourceNotify read fOnSourceSetupFileXfer
      write fOnSourceSetupFileXfer;
    {File transfered}
    property OnSourceFileTransfer: TOnSourceFileTransfer read
      fOnSourceFileTransfer write fOnSourceFileTransfer;
    {Acquire error}
    property OnAcquireError: TOnTwainError read fOnAcquireError
      write fOnAcquireError;
    {All images transfered}
    property OnTransferComplete: TOnTransferComplete read fOnTransferComplete
      write fOnTransferComplete;

    property OnTwainAcquireNative: TTwainAcquireNativeEvent read rOnTwainAcquireNative write rOnTwainAcquireNative;
    property OnTwainAcquireBitmap: TTwainAcquireBitmapEvent read rOnTwainAcquireBitmap write rOnTwainAcquireBitmap;
    property OnAcquireProgress: TAcquireProgressEvent read rOnAcquireProgress write rOnAcquireProgress;
    property OnProcessMessages: TOnSourceBoolNotify read rOnProcessMessages write rOnProcessMessages;

    {Default transfer mode to be used with sources}
    property TransferMode: TTwainTransferMode read fTransferMode write fTransferMode;
    {Returns the number of sources, after Library and Source Manager}
    {has being loaded}
    property SourceCount: Integer read GetSourceCount;
    //Selected source by Index
    property SelectedSourceIndex: Integer read GetSelectedSourceIndex write SetSelectedSourceIndex;
    //Selected source by ID
    property SelectedSourceID: TW_UINT32 read GetSelectedSourceID write SetSelectedSourceID;
    //Selected source
    property SelectedSource: TTwainSource read GetSelectedSource;
    {User should fill the application information}
    property Info: TTwainIdentity read fInfo write SetInfo;
    {Loads or unload Twain library}
    property LibraryLoaded: Boolean read fLibraryLoaded write SetLibraryLoaded;
    {Loads or unloads the source manager}
    property SourceManagerLoaded: Boolean read fSourceManagerLoaded write SetSourceManagerLoaded;
  end;

const
  CapabilityRetrieveModeToTwain: array [TRetrieveCap] of TW_UINT16 =
    (MSG_GET, MSG_GETCURRENT, MSG_GETDEFAULT);

  CapabilityModeToTwain: array [TCapabilityOperation] of TW_UINT16 =
    (MSG_GET, MSG_GETCURRENT, MSG_GETDEFAULT, MSG_RESET, MSG_RESETALL, MSG_SET, MSG_SETCONSTRAINT);

  FormatToTwain: Array[TTwainFormat] of TW_UINT16 =
    (TWFF_TIFF, TWFF_PICT, TWFF_BMP, TWFF_XBM, TWFF_JFIF, TWFF_FPX,
     TWFF_TIFFMULTI, TWFF_PNG, TWFF_SPIFF, TWFF_EXIF, 0);

  //Sizes of Papers in cm (fuck inch)
  PaperSizesTwain: array [TTwainPaperSize] of TPaperSize =
   ((name:''; w:0; h:0), (name:'A4'; w:21.0; h:29.7), (name:'JIS B5'; w:18.2; h:25.7), (name:'US Letter'; w:21.6; h:27.9),
    (name:'US Legal'; w:21.6; h:35.6), (name:'A5'; w:14.8; h:21.0), (name:'ISO B4'; w:25.0; h:35.3), (name:'ISO B6'; w:12.5; h:17.6),
    (name:'US Ledger'; w:43.2; h:27.9), (name:'US Executive'; w:18.4; h:26.7), (name:'A3'; w:29.7; h:42.0), (name:'ISO B3'; w:35.3; h:50.0),
    (name:'A6'; w:4.1; h:5.8), (name:'C4'; w:22.9; h:32.4), (name:'C5'; w:16.2; h:22.9), (name:'C6'; w:11.4; h:16.2),
    (name:'4A0'; w:168.2; h:237.8), (name:'2A0'; w:118.9; h:168.2), (name:'A0'; w:84.1; h:118.9),  (name:'A1'; w:59.4; h:84.1),
    (name:'A2'; w:42.0; h:59.4), (name:'A7'; w:7.4; h:10.5), (name:'A8'; w:5.2; h:7.4), (name:'A9'; w:3.7; h:5.2), (name:'A10'; w:2.6; h:3.7),
    (name:'ISO B0'; w:100.0; h:141.4), (name:'ISO B1'; w:70.7; h:100.0), (name:'ISO B2'; w:50.0; h:70.7), (name:'ISO B5'; w:17.6; h:25.0),
    (name:'ISO B7'; w:8.8; h:12.5), (name:'ISO B8'; w:6.2; h:8.8), (name:'ISO B9'; w:4.4; h:6.2), (name:'ISO B10'; w:3.1; h:4.4),
    (name:'JIS B0'; w:103.0; h:145.6), (name:'JIS B1'; w:72.8; h:103.0), (name:'JIS B2'; w:51.5; h:72.8), (name:'JIS B3'; w:36.4; h:51.5),
    (name:'JIS B4'; w:25.7; h:36.4), (name:'JIS B6'; w:12.8; h:18.2), (name:'JIS B7'; w:9.1; h:12.8), (name:'JIS B8'; w:6.4; h:9.1),
    (name:'JIS B9'; w:4.5; h:6.4), (name:'JIS B10'; w:3.2; h:4.5),
    (name:'C0'; w:91.7; h:129.7), (name:'C1'; w:64.8; h:91.7), (name:'C2'; w:45.8; h:64.8), (name:'C3'; w:32.4; h:45.8),
    (name:'C7'; w:8.1; h:11.4), (name:'C8'; w:5.7; h:8.1), (name:'C9'; w:4.0; h:5.7), (name:'C10'; w:2.8; h:4.0),
    (name:'US Statement'; w:14.0; h:21.6), (name:'Business card'; w:9.0; h:5.5),
    (name:''; w:0; h:0)
   );

  TwainPixelTypes: array [TTWainPixelType] of String =
  ('Black & White', 'Gray scale', 'Color (RGB)', 'Color (Palette)', 'Color (CMY)', 'Color (CMYK)',
   'Color (YUV)', 'Color (YUVK)', 'Color (Cie XYZ)', 'Unknown', 'Unknown1', 'Unknown2', 'Color (BGR)');

{Puts a string inside a TW_STR255}
{$IFDEF UNICODE}
function StrToStr255(Value: RawByteString): TW_STR255;
{$ELSE}
function StrToStr255(Value: String): TW_STR255;
{$ENDIF}

{Returns full Twain directory (usually in Windows directory)}
function GetTwainDirectory(ALib: String = TWAINLIBRARY): String;

{This method returns if Twain is installed in the current machine}
function IsTwainInstalled: Boolean;

{Returns the size of a twain type}
function TWTypeSize(TypeName: TW_UINT16): Integer;

function MakeMsg(const Handle: THandle; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): TMsg;

//Convert TWSS Value to TTwainPaperSize and vice versa (we can't do the cast because of obsolet TWSS_B=8)
function PaperSizeToTwain(AValue:TTwainPaperSize):TW_UINT16;
function TWSS_ToTwainPaperSize(AValue:TW_UINT16):TTwainPaperSize;

//Returns the smallest TwainPaper that can contain the specified dimensions (in cm)
function GetTwainPaperSize(AWidth, AHeight:Single):TTwainPaperSize; overload;
function GetTwainPaperSize(AWidth, AHeight:Single; APaperSizeSet:TTwainPaperSizeSet):TTwainPaperSize; overload;

implementation

{Returns the size of a twain type}
function TWTypeSize(TypeName: TW_UINT16): Integer;
begin
  {Test the type to return the size}
  case TypeName of
    TWTY_INT8  :  Result := sizeof(TW_INT8);
    TWTY_UINT8 :  Result := sizeof(TW_UINT8);
    TWTY_INT16 :  Result := sizeof(TW_INT16);
    TWTY_UINT16:  Result := sizeof(TW_UINT16);
    TWTY_INT32 :  Result := sizeof(TW_INT32);
    TWTY_UINT32:  Result := sizeof(TW_UINT32);
    TWTY_FIX32 :  Result := sizeof(TW_FIX32);
    TWTY_FRAME :  Result := sizeof(TW_FRAME);
    TWTY_STR32 :  Result := sizeof(TW_STR32);
    TWTY_STR64 :  Result := sizeof(TW_STR64);
    TWTY_STR128:  Result := sizeof(TW_STR128);
    TWTY_STR255:  Result := sizeof(TW_STR255);
    //npeter: the following types were not implemented
    //especially the bool caused problems
    TWTY_BOOL:    Result := sizeof(TW_BOOL);
    TWTY_UNI512:  Result := sizeof(TW_UNI512);
    TWTY_STR1024:  Result := sizeof(TW_STR1024);
    else          Result := 0;
  end {case}
end;

{Puts a string inside a TW_STR255}
{$IFDEF UNICODE}
function StrToStr255(Value: RawByteString): TW_STR255;
{$ELSE}
function StrToStr255(Value: String): TW_STR255;
{$ENDIF}
begin
  {Clean result}
  Fillchar({%H-}Result, sizeof(TW_STR255), #0);
  {If value fits inside the TW_STR255, copy memory}
  if Length(Value) <= sizeof(TW_STR255) then
    CopyMemory(@Result[0], @Value[1], Length(Value))
  else CopyMemory(@Result[0], @Value[1], sizeof(TW_STR255));
end;

{Returns full Twain directory (usually in Windows directory)}
function GetTwainDirectory(ALib: String = TWAINLIBRARY): String;
var
  i: TDirectoryKind;
  Dir: String;
begin
  {Searches in all the directories}
  for i :=Low(TDirectoryKind) to High(TDirectoryKind) do
  begin
    try
       {Directory to search}
       Dir := GetCustomDirectory(i);
       {Tests if the file exists in this directory}
       if FileExists(Dir + ALib) then
       begin
         {In case it exists, returns this directory and exit}
         {the for loop}
         Result := Dir;
         Break;
       end;
    except
      //FileExists got an ERangeError Exception when File does not exists
    end;
  end;
end;

{This method returns if Twain is installed in the current machine}
function IsTwainInstalled: Boolean;
begin
  {If GetTwainDirectory function returns an empty string, it means}
  {that Twain was not found}
  Result := (GetTwainDirectory <> '');
end;

function PaperSizeToTwain(AValue: TTwainPaperSize): TW_UINT16;
begin
  if (AValue<tpsUSLEDGER)
  then Result :=TW_UINT16(AValue)
  else Result :=TW_UINT16(AValue)+1;
end;

function TWSS_ToTwainPaperSize(AValue:TW_UINT16):TTwainPaperSize;
begin
  Result :=tpsNONE;

  if (AValue<TWSS_B)
  then Result :=TTwainPaperSize(AValue)
  else if (AValue>TWSS_B) then Result :=TTwainPaperSize(AValue-1);
end;

function GetTwainPaperSize(AWidth, AHeight:Single):TTwainPaperSize;
var
   i:TTwainPaperSize;
   curW, curH:Single;

begin
  curW :=MAXINT; curH :=MAXINT;
  Result :=tpsMAXSIZE;
  for i:=Low(PaperSizesTwain) to High(PaperSizesTwain) do
  begin
    //Current paper can contain AWidth x AHeight ?
    if (PaperSizesTwain[i].w>=AWidth) and (PaperSizesTwain[i].h>=AHeight) then
    begin
      //Current paper is smallest then Result ?
      if (PaperSizesTwain[i].w<=curW) and (PaperSizesTwain[i].h<=curH) then
      begin
        curW :=PaperSizesTwain[i].w;
        curH :=PaperSizesTwain[i].h;
        Result :=i;
      end;
    end;
  end;
end;

function GetTwainPaperSize(AWidth, AHeight: Single; APaperSizeSet: TTwainPaperSizeSet): TTwainPaperSize;
var
   i:TTwainPaperSize;
   curW, curH:Single;

begin
  curW :=MAXINT; curH :=MAXINT;
  Result :=tpsMAXSIZE;
  for i in APaperSizeSet do
  begin
    //Current paper can contain AWidth x AHeight ?
    if (PaperSizesTwain[i].w>=AWidth) and (PaperSizesTwain[i].h>=AHeight) then
    begin
      //Current paper is smallest then Result ?
      if (PaperSizesTwain[i].w<=curW) and (PaperSizesTwain[i].h<=curH) then
      begin
        curW :=PaperSizesTwain[i].w;
        curH :=PaperSizesTwain[i].h;
        Result :=i;
      end;
    end;
  end;
end;

{ TTwainIdentity object implementation }

{Object being created}
constructor TTwainIdentity.Create;
begin
  {Allows ancestor to work}
  inherited Create;

  {Set initial properties}
  FillChar(Structure, sizeof(Structure), #0);
  Language := tlUserLocale;
  CountryCode := 1;
  MajorVersion := 1;
  VersionInfo := 'Application name';
  Structure.ProtocolMajor := TWON_PROTOCOLMAJOR;
  Structure.ProtocolMinor := TWON_PROTOCOLMINOR;
  Groups := [tgImage, tgControl];
  Manufacturer := 'Application manufacturer';
  ProductFamily := 'App product family';
  ProductName := 'App product name';
end;

{Sets a text value}
procedure TTwainIdentity.SetString(const Index: Integer; const Value: String);
var
  PropStr: PAnsiChar;
begin
  {Select and copy pointer}
  case Index of
    0: PropStr := @Structure.Version.Info[0];
    1: PropStr := @Structure.Manufacturer[0];
    2: PropStr := @Structure.ProductFamily[0];
  else PropStr := @Structure.ProductName[0];
  end {case};

  {Set value}
  Fillchar(PropStr^, sizeof(TW_STR32), #0);
  if Length(Value) > sizeof(TW_STR32) then
    CopyMemory(PropStr, @Value[1], sizeof(TW_STR32))
  else
    CopyMemory(PropStr, @Value[1], Length(Value));
end;

{Returns a text value}
function TTwainIdentity.GetString(const Index: integer): String;
begin
  {Test for the required property}
  case Index of
    0: Result := string(Structure.Version.Info);
    1: Result := string(Structure.Manufacturer);
    2: Result := string(Structure.ProductFamily);
  else Result := string(Structure.ProductName);
  end {case}
end;

{Returns application language property}
function TTwainIdentity.GetLanguage: TTwainLanguage;
begin
  if Structure.Version.Language = High(TW_UINT16) then
    Result := tlUserLocale
  else
    Result := TTwainLanguage(Structure.Version.Language);
end;

function TTwainIdentity.GetMajorVersion: TW_UINT16;
begin
  Result := Structure.Version.MajorNum;
end;

function TTwainIdentity.GetMinorVersion: TW_UINT16;
begin
  Result := Structure.Version.MinorNum;
end;

function TTwainIdentity.GetId: TW_UINT32;
begin
  Result := Structure.Id;
end;

{Sets application language property}
procedure TTwainIdentity.SetLanguage(const Value: TTwainLanguage);
begin
  if Ord(Value) >= 0 then
    Structure.Version.Language := Ord(Value)
  else
    Structure.Version.Language := High(TW_UINT16);
end;

procedure TTwainIdentity.SetMajorVersion(const aMajorVersion: TW_UINT16);
begin
  Structure.Version.MajorNum := aMajorVersion;
end;

procedure TTwainIdentity.SetMinorVersion(const aMinorVersion: TW_UINT16);
begin
  Structure.Version.MinorNum := aMinorVersion;
end;

{Copy properties from another TTwainIdentity}
procedure TTwainIdentity.Assign(Source: TObject);
begin
  {The source should also be a TTwainIdentity}
  if Source is TTwainIdentity then begin
    {Copy properties}
    Structure := TTwainIdentity(Source).Structure
  end;
end;

{Returns avaliable groups}
function TTwainIdentity.GetCountryCode: Word;
begin
  Result := Structure.Version.Country;
end;

function TTwainIdentity.GetGroups: TTwainGroups;
begin
  {Convert from Structure.SupportedGroups to TTwainGroups}
 Result := [];
 Include(Result, tgControl);
  if DG_IMAGE AND Structure.SupportedGroups <> 0 then
    Include(Result, tgImage);
  if DG_AUDIO AND Structure.SupportedGroups <> 0 then
    Include(Result, tgAudio);
  if DF_DSM2 AND Structure.SupportedGroups <> 0 then
    Include(Result, tgDSM2);
  if DF_APP2 AND Structure.SupportedGroups <> 0 then
    Include(Result, tgAPP2);
  if DF_DS2 AND Structure.SupportedGroups <> 0 then
    Include(Result, tgDS2);
end;

{Sets avaliable groups}
procedure TTwainIdentity.SetCountryCode(const aCountryCode: Word);
begin
  Structure.Version.Country := aCountryCode;
end;

procedure TTwainIdentity.SetGroups(const Value: TTwainGroups);
begin
  {Convert from TTwainGroups to Structure.SupportedGroups}
  Structure.SupportedGroups := DG_CONTROL;
  if tgImage in Value then
    Structure.SupportedGroups := Structure.SupportedGroups or DG_IMAGE;
  if tgAudio in Value then
    Structure.SupportedGroups := Structure.SupportedGroups or DG_AUDIO;
  if tgDSM2 in Value then
    Structure.SupportedGroups := Structure.SupportedGroups or DF_DSM2;
  if tgAPP2 in Value then
    Structure.SupportedGroups := Structure.SupportedGroups or DF_APP2;
  if tgDS2 in Value then
    Structure.SupportedGroups := Structure.SupportedGroups or DF_DS2;
end;

{ TCustomDelphiTwain component implementation }

{Loads twain library and returns if it loaded sucessfully}
function TCustomDelphiTwain.LoadLibrary: Boolean;
var
  TwainDirectory: String;
begin
  {The library must not be already loaded}
  if (not LibraryLoaded) then
  begin
    Result := FALSE; {Initially returns FALSE}
    {Searches for Twain directory}
    TwainDirectory := GetTwainDirectory;
    {Continue only if twain is installed in an known directory}
    if TwainDirectory <> '' then
    begin

      fHandle := Windows.LoadLibrary(PChar(TwainDirectory + TWAINLIBRARY));
      {If the library was sucessfully loaded}
      if (fHandle <> INVALID_HANDLE_VALUE) then
      begin

        {Obtains method handle}
        @fTwainProc := GetProcAddress(fHandle, MAKEINTRESOURCE(1));
        {Returns TRUE/FALSE if the method was obtained}
        Result := (@fTwainProc <> nil);

        {If the method was not obtained, also free the library}
        if not Result then
        begin
          {Free the handle and clears the variable}
          Windows.FreeLibrary(fHandle);
          fHandle := 0;
        end {if not Result}
      end
      else
        {If it was not loaded, clears handle value}
        fHandle := 0;

    end {if TwainDirectory <> ''};

  end
  else
    {If it was already loaded, returns true, since that is}
    {what was supposed to happen}
    Result := TRUE;

  {In case the method was sucessful, updates property}
  if Result then fLibraryLoaded := TRUE;
end;


{Unloads twain and returns if it unloaded sucessfully}
function TCustomDelphiTwain.UnloadLibrary: Boolean;
begin
  {The library must not be already unloaded}
  if (LibraryLoaded) then
  begin
    {Unloads the source manager}
    SourceManagerLoaded := FALSE;
    {Just call windows method to unload}
    Result := Windows.FreeLibrary(Handle);
    {If it was sucessfull, also clears handle value}
    if Result then fHandle := 0;
    {Updates property}
    fLibraryLoaded := not Result;
  end
  else
    {If it was already unloaded, returns true, since that is}
    {what was supposed to happen}
    Result := TRUE;

  {In case the method was sucessful, updates property}
  {if Result then }fLibraryLoaded := FALSE;
  MessageTimer_Disable;
end;

{Enumerate the avaliable devices after Source Manager is loaded}
function TCustomDelphiTwain.EnumerateDevices: Boolean;
var
  NewSource: TTwainSource;
  CallRes  : TW_UINT16;
begin
  {Booth library and source manager must be loaded}
  if (LibraryLoaded and SourceManagerLoaded) then
  begin
    {Clears the preview list of sources}
    ClearDeviceList;

    {Allocate new identity and tries to enumerate}
    NewSource := TTwainSource.Create(Self);
    NewSource.TransferMode := Self.TransferMode;
    CallRes := fTwainProc(AppInfo, nil, DG_CONTROL, DAT_IDENTITY, MSG_GETFIRST, @NewSource.Structure);
    if CallRes = TWRC_SUCCESS then
      repeat

        {Add this item to the list}
        DeviceList.Add(NewSource);
        {Allocate memory for the next}
        NewSource := TTwainSource.Create(Self);
        NewSource.TransferMode := Self.TransferMode;
        NewSource.fIndex := DeviceList.Count;

        {Try to get the next item}
        CallRes :=fTwainProc(AppInfo, nil, DG_CONTROL, DAT_IDENTITY, MSG_GETNEXT, @NewSource.Structure);
      until (CallRes <> TWRC_SUCCESS);

    {Set that the component has enumerated the devices}
    {if everything went correctly}
    Result := TRUE;
    fHasEnumerated := Result;

    {Dispose un-needed source object}
    NewSource.Free;

  end
  else Result := FALSE; {If library and source manager aren't loaded}
end;

procedure TCustomDelphiTwain.WndProc(var Message: TMessage);
var
  i    : Integer;
  xMsg  : TMsg;
begin
  //WndProc := False;
  with Message do begin
  {Tests for the message}
      {Try to obtain the current object pointer}
      if Assigned(Self) then
        {If there are sources loaded, we need to verify}
        {this message}
       if (Self.SourcesLoaded > 0) then
        begin
          {Convert parameters to a TMsg}
          xMsg := MakeMsg(Handle, Msg, wParam, lParam);
          {Tell about this message}
          FOR i := 0 TO Self.SourceCount - 1 DO
            if ((Self.Source[i].Loaded) and (Self.Source[i].Enabled)) then
              if Self.Source[i].ProcessMessage(xMsg) then
              begin
                {Case this was a message from the source, there is}
                {no need for the default procedure to process}
                //Result := 0;
                //WndProc := True;
                Exit;
              end;

        end; {if (Twain.SourcesLoaded > 0)}
  end;
end;

{Procedure to load and unload twain library and update property}
procedure TCustomDelphiTwain.SetLibraryLoaded(const Value: Boolean);
begin
  {The value must be changing to activate}
  if (Value <> fLibraryLoaded) then
  begin
    {Depending on the parameter load/unload the library and updates}
    {property whenever it loaded or unloaded sucessfully}
    if Value           then  LoadLibrary
    else {if not Value then} UnloadLibrary;

  end {if (Value <> fLibraryLoaded)}
end;

{Loads twain source manager}
function TCustomDelphiTwain.LoadSourceManager: Boolean;
begin
  {The library must be loaded}
  if LibraryLoaded and not SourceManagerLoaded then begin
    {Loads source manager}
    Result := (fTwainProc(AppInfo, nil, DG_CONTROL, DAT_PARENT,
      MSG_OPENDSM, @VirtualWindow) = TWRC_SUCCESS);
  end else begin
    {The library is not loaded, thus the source manager could}
    {not be loaded}
    Result := FALSE or SourceManagerLoaded;
  end;

  {In case the method was sucessful, updates property}
  if Result then fSourceManagerLoaded := TRUE;
end;

procedure TCustomDelphiTwain.RefreshVirtualWindow;
begin
  (*  { #note 10 -oMaxM : But why? Try to understand for what obscure reason it is done }
  //BUG WORKAROUND
  DoDestroyVirtualWindow;
  DoDestroy;
  DoCreate;
  DoCreateVirtualWindow;

  if LoadLibrary then
    SourceManagerLoaded := True;
    *)
end;

{UnLoads twain source manager}
function TCustomDelphiTwain.UnloadSourceManager(forced: boolean): Boolean;
begin
  {The library must be loaded}
  if LibraryLoaded and SourceManagerLoaded then
  begin
    {Clears the list of sources}
    ClearDeviceList;
    {Unload source manager}
    if not forced then
     Result := (TwainProc(AppInfo, nil, DG_CONTROL, DAT_PARENT, MSG_CLOSEDSM, @VirtualWindow) = TWRC_SUCCESS)
    else result:=true;
  end
  else
    {The library is not loaded, meaning that the Source Manager isn't either}
    Result := TRUE;

  {In case the method was sucessful, updates property}
  if Result then fSourceManagerLoaded := FALSE;
end;

procedure TCustomDelphiTwain.DoCreate;
begin
  {Create source list}
  DeviceList := TPointerList.Create;
  {Clear variables}
  fSourcesLoaded := 0;
  fHandle := 0;
  @fTwainProc := nil;
  fSourceManagerLoaded := FALSE;
  fHasEnumerated := FALSE;
  fTransferMode := ttmNative;

  {Creates the object to allow the user to set the application}
  {information to inform twain source manager and sources}
  fInfo := TTwainIdentity.Create;
  AppInfo := @fInfo.Structure;
end;

procedure TCustomDelphiTwain.DoDestroy;
begin
  {Completely unload the library}
  LibraryLoaded := FALSE;

  {Free the object}
  fInfo.Free;
  {Clears and free source list}
  ClearDeviceList;
  DeviceList.Free;
end;

{Virtual window procedure handler}
function VirtualWinProc(Handle: THandle; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LResult; stdcall;

  {Returns the TCustomDelphiTwain object}
  function Obj: TCustomDelphiTwain;
  begin
    DTNativeUInt(Result) := GetWindowLong(Handle, GWL_USERDATA);
  end {function};

var
  Twain: TCustomDelphiTwain;
  i    : Integer;
  Msg  : TMsg;
begin
  {Tests for the message}
  case uMsg of
    {Creation of the window}
    WM_CREATE:
      {Stores the TCustomDelphiTwain object handle}
      with {%H-}pCreateStruct(lParam)^ do
        SetWindowLong(Handle, GWL_USERDATA, {%H-}Longint(lpCreateParams));
    {case} else
    begin
      {Try to obtain the current object pointer}
      Twain := Obj;

      if Assigned(Twain) then
        {If there are sources loaded, we need to verify}
        {this message}
       if (Twain.SourcesLoaded > 0) then
        begin
          {Convert parameters to a TMsg}
          Msg := MakeMsg(Handle, uMsg, wParam, lParam);
          {Tell about this message}
          FOR i := 0 TO Twain.SourceCount - 1 DO
            if ((Twain.Source[i].Loaded) and (Twain.Source[i].Enabled)) then
              if Twain.Source[i].ProcessMessage(Msg) then
              begin
                {Case this was a message from the source, there is}
                {no need for the default procedure to process}
                Result := 0;
                Exit;
              end;

        end {if (Twain.SourcesLoaded > 0)}


    end {case Else}
  end {case uMsg of};

  {Calls method to handle}
  Result := DefWindowProc(Handle, uMsg, wParam, lParam);
end;

procedure TCustomDelphiTwain.DoCreateVirtualWindow;

  //fVirtualWindow := Classes.AllocateHWnd(WndProc);
var
  WindowClassW: WndClassW;

begin
  if (Windows.GetClassInfoW(HInstance, @VirtualWinClassName, {$IFDEF FPC}@{$ENDIF}WindowClassW)=False) then
  begin
    with WindowClassW do
    begin
      Style :=0;
      LPFnWndProc := @VirtualWinProc;
      CbClsExtra := 0;
      CbWndExtra := 0;
      hIcon := 0;
      hCursor := 0;
      hbrBackground := 0;
      LPSzMenuName := nil;
      LPSzClassName := @VirtualWinClassName;
    end;
    WindowClassW.hInstance :=HInstance;
    Windows.RegisterClassW(WindowClassW);
  end;

  fVirtualWindow :=CreateWindowExW(0, @VirtualWinClassName, @VirtualWinClassName,
    WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, Self);
end;

procedure TCustomDelphiTwain.DoDestroyVirtualWindow;
begin
  DestroyWindow(fVirtualWindow);
end;

procedure TCustomDelphiTwain.DoCreateTimer;
begin
 {$IFDEF FPC}
 fMessagesTimer := TFPTimer.Create(nil);
 fMessagesTimer.Enabled := False;
 fMessagesTimer.Interval := 100;
 fMessagesTimer.OnTimer := DoMessagesTimer;
 {$ENDIF}
end;

procedure TCustomDelphiTwain.DoDestroyTimer;
begin
 {$IFDEF FPC}
  FreeAndNil(fMessagesTimer);
 {$ENDIF}
end;

procedure TCustomDelphiTwain.DoMessagesTimer(Sender: TObject);
begin
  //MUST BE HERE SO THAT TWAIN RECEIVES MESSAGES
  if (VirtualWindow > 0) then SendMessage(VirtualWindow, WM_USER, 0, 0);
end;

procedure TCustomDelphiTwain.MessageTimer_Enable;
begin
  {$IFDEF FPC}
  if Assigned(fMessagesTimer)
  then fMessagesTimer.Enabled := True;
  {$ENDIF}
end;

procedure TCustomDelphiTwain.MessageTimer_Disable;
begin
  {$IFDEF FPC}
  if Assigned(fMessagesTimer)
  then fMessagesTimer.Enabled := False;
  {$ENDIF}
end;

function TCustomDelphiTwain.CustomGetParentWindow: TW_HANDLE;
begin
  Result := VirtualWindow;
end;

procedure TCustomDelphiTwain.DoTwainAcquireNative(Sender: TObject; const Index: Integer;
                                                 nativeHandle: TW_UINT32; var NeedBitmap, Cancel: Boolean);
begin
  //MaxM: No need to create HBitmap if unused
  NeedBitmap :=NeedBitmap or Assigned(rOnTwainAcquireBitmap);
  if Assigned(rOnTwainAcquireNative) then rOnTwainAcquireNative(Sender, Index, nativeHandle, Cancel);
end;

procedure TCustomDelphiTwain.DoTwainAcquire(Sender: TObject; const Index: Integer;
                                            imageHandle: HBitmap; var Cancel: Boolean);
begin
  if Assigned(rOnTwainAcquireBitmap) then rOnTwainAcquireBitmap(Sender, Index, imageHandle, Cancel);
end;

procedure TCustomDelphiTwain.DoAcquireProgress(Sender: TObject; const Index: Integer;
                                               const imageHandle: HBitmap; const Current, Total: Integer);
begin
  if Assigned(rOnAcquireProgress) then rOnAcquireProgress(Sender, Index, imageHandle, Current, Total);
end;

{Returns a TMsg structure}
function MakeMsg(const Handle: THandle; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): TMsg;
begin
  {Fill structure with the parameters}
  Result.hwnd := Handle;
  Result.message := uMsg;
  Result.wParam := wParam;
  Result.lParam := lParam;
  GetCursorPos(Result.pt);
end;

{Procedure to load or unloaded the twain source manager}
procedure TCustomDelphiTwain.SetSelectedSourceIndex(const Value: Integer);
begin
  fSelectedSourceIndex := Value;
end;

procedure TCustomDelphiTwain.SetSelectedSourceID(AValue: TW_UINT32);
begin
  fSelectedSourceIndex:= FindSource(AValue);
end;

procedure TCustomDelphiTwain.SetSourceManagerLoaded(const Value: Boolean);
begin
  {The library must be loaded to have access to the method}
  if LibraryLoaded and (Value <> fSourceManagerLoaded) then
  begin
    {Load/unload the source manager}
    if Value           then  LoadSourceManager
    else {if not Value then} UnloadSourceManager(false);
  end {if LibraryLoaded}
end;

{Clears the list of sources}
procedure TCustomDelphiTwain.ClearDeviceList;
var
  i: Integer;
begin
  {Deallocate pTW_IDENTITY}
  FOR i := 0 TO DeviceList.Count - 1 DO
    TTwainSource(DeviceList.Item[i]).Free;
  {Clears the list}
  DeviceList.Clear;
  {Set trigger to tell that it has not enumerated again yet}
  fHasEnumerated := FALSE;

end;

{Finds a matching source index}
function TCustomDelphiTwain.FindSource(Value: pTW_IDENTITY): Integer;
var
  i: Integer;

begin
  Result := -1;
  if (LibraryLoaded and SourceManagerLoaded) then
  begin
    {Search for this source in the list}
    for i :=0 to SourceCount-1 do
    if CompareMem(@TTwainSource(DeviceList.Item[i]).Structure, PAnsiChar(Value), SizeOf(TW_IDENTITY)) then
    begin
      {Return index and exit}
      Result := i;
      break;
    end; {if CompareMem, for i}
  end;
end;

function TCustomDelphiTwain.FindSource(AID: TW_UINT32): Integer;
var
  i: Integer;

begin
 Result := -1;
 if (LibraryLoaded and SourceManagerLoaded) then
 for i:=0 to SourceCount-1 do
 begin
   if (TTwainSource(DeviceList.Item[i]).ID = AID)
   then begin Result:=i; break; end;
 end;
end;

function TCustomDelphiTwain.FindSource(Value: TTwainSource): Integer;
begin
  Result :=FindSource(Value.Manufacturer, Value.ProductFamily, Value.ProductName);
end;

function TCustomDelphiTwain.FindSource(AManufacturer, AProductFamily, AProductName: String): Integer;
var
  i: Integer;
  curStr: TTwainSource;

begin
  Result := -1;
  if (LibraryLoaded and SourceManagerLoaded) then
  for i:=0 to SourceCount-1 do
  begin
    curStr:= TTwainSource(DeviceList.Item[i]);
    { #todo -oMaxM : if there is more identical scanner? }
    if (curStr.Manufacturer=AManufacturer) and
       (curStr.ProductFamily=AProductFamily) and (curStr.ProductName=AProductName)
    then begin Result:=i; break; end;
  end;
end;

function TCustomDelphiTwain.FindSource(AProductName: String): Integer;
var
  i: Integer;

begin
  Result := -1;
  if (LibraryLoaded and SourceManagerLoaded) then
  for i:=0 to SourceCount-1 do
  begin
    { #todo -oMaxM : if there is more identical scanner? }
    if (TTwainSource(DeviceList.Item[i]).ProductName=AProductName)
    then begin Result:=i; break; end;
  end;
end;

function TCustomDelphiTwain.FindSourceID(Value: pTW_IDENTITY): TW_UINT32;
var
  i: Integer;

begin
 Result := 0;
 if (LibraryLoaded and SourceManagerLoaded) then
 for i :=0 to SourceCount-1 do
 if CompareMem(@TTwainSource(DeviceList.Item[i]).Structure, PAnsiChar(Value), SizeOf(TW_IDENTITY)) then
 begin
   Result:= Value^.Id;
   break;
 end;
end;

function TCustomDelphiTwain.FindSourceID(AID: TW_UINT32): Boolean;
var
  i: Integer;

begin
  Result:= False;
  if (LibraryLoaded and SourceManagerLoaded) then
  for i:=0 to SourceCount-1 do
  begin
    if (TTwainSource(DeviceList.Item[i]).ID = AID)
    then begin Result:= True; break; end;
  end;
end;

function TCustomDelphiTwain.FindSourceID(Value: TTwainSource): TW_UINT32;
begin
  Result :=FindSourceID(Value.Manufacturer, Value.ProductFamily, Value.ProductName);
end;

function TCustomDelphiTwain.FindSourceID(AManufacturer, AProductFamily, AProductName: String): TW_UINT32;
var
  i: Integer;
  curStr: TTwainSource;

begin
  Result :=0;
  if (LibraryLoaded and SourceManagerLoaded) then
  for i:=0 to SourceCount-1 do
  begin
    curStr:= TTwainSource(DeviceList.Item[i]);
    { #todo -oMaxM : if there is more identical scanner? }
    if (curStr.Manufacturer=AManufacturer) and
       (curStr.ProductFamily=AProductFamily) and (curStr.ProductName=AProductName)
    then begin Result:=curStr.ID; break; end;
  end;
end;

function TCustomDelphiTwain.FindSourceID(AProductName: String): TW_UINT32;
var
  i: Integer;
  curStr: TTwainSource;

begin
  Result :=0;
  if (LibraryLoaded and SourceManagerLoaded) then
  for i:=0 to SourceCount-1 do
  begin
    curStr:= TTwainSource(DeviceList.Item[i]);
    { #todo -oMaxM : if there is more identical scanner? }
    if (curStr.ProductName=AProductName)
    then begin Result:=curStr.ID; break; end;
  end;
end;

function TCustomDelphiTwain.SelectSource(Value: pTW_IDENTITY; Load: Boolean): TTwainSource;
begin
  fSelectedSourceIndex:= FindSource(Value);
  if (fSelectedSourceIndex > -1)
  then Result:= TTwainSource(DeviceList.Item[fSelectedSourceIndex])
  else Result:= nil;

  if Load and (Result <> nil) then Result.Loaded:= True;
end;

function TCustomDelphiTwain.SelectSource(AID: TW_UINT32; Load: Boolean): TTwainSource;
begin
  fSelectedSourceIndex:= FindSource(AID);
  if (fSelectedSourceIndex > -1)
  then Result:= TTwainSource(DeviceList.Item[fSelectedSourceIndex])
  else Result:= nil;

  if Load and (Result <> nil) then Result.Loaded:= True;
end;

function TCustomDelphiTwain.SelectSource(AManufacturer, AProductFamily, AProductName: String; Load: Boolean): TTwainSource;
begin
  fSelectedSourceIndex:= FindSource(AManufacturer, AProductFamily, AProductName);
  if (fSelectedSourceIndex > -1)
  then Result:= TTwainSource(DeviceList.Item[fSelectedSourceIndex])
  else Result:= nil;

  if Load and (Result <> nil) then Result.Loaded:= True;
end;

function TCustomDelphiTwain.SelectSource(AProductName: String; Load: Boolean): TTwainSource;
begin
  fSelectedSourceIndex:= FindSource(AProductName);
  if (fSelectedSourceIndex > -1)
  then Result:= TTwainSource(DeviceList.Item[fSelectedSourceIndex])
  else Result:= nil;

  if Load and (Result <> nil) then Result.Loaded:= True;
end;

{Allows Twain to display a dialog to let the user choose any source}
{and returns the source index in the list}
function TCustomDelphiTwain.SelectSource: Integer;
begin
  Result := -1;     {Default result}
  {Booth library and source manager must be loaded}
  if (LibraryLoaded and SourceManagerLoaded) then
  begin
    Result := CustomSelectSource;

    SelectedSourceIndex := Result;
  end {(LibraryLoaded and SourceManagerLoaded)}
end;

{Returns the number of sources}
function TCustomDelphiTwain.GetSourceCount: Integer;
begin
  {Library and source manager must be loaded}
  if (LibraryLoaded and SourceManagerLoaded) then
  begin
    {Enumerate devices, if needed}
    if not HasEnumerated then EnumerateDevices;
    {Returns}
    Result := DeviceList.Count;
  end
  {In case library and source manager aren't loaded, returns 0}
  else Result := 0
end;

{Returns the default source}
function TCustomDelphiTwain.GetDefaultSource: Integer;
var
  Identity: TW_IDENTITY;
begin
  {Call twain to display the dialog}
  if SourceManagerLoaded and (TwainProc(AppInfo, nil, DG_CONTROL, DAT_IDENTITY,
    MSG_GETDEFAULT, @Identity) = TWRC_SUCCESS) then
    Result := FindSource(@Identity)
  else Result := 0 {Returns}
end;

{Returns a source from the list}
function TCustomDelphiTwain.GetSelectedSource: TTwainSource;
begin
  { #note 10 -oMaxM : Must be optimized, SourceCount and Source do the same tests and re-enumerate devices }
  if SourceCount = 0 then begin
    Result := nil;
  end else begin
    if (fSelectedSourceIndex >= 0) and (fSelectedSourceIndex < SourceCount) then
      Result := Source[fSelectedSourceIndex]
    else
      Result := nil;
  end;
end;

function TCustomDelphiTwain.GetSelectedSourceIndex: Integer;
begin
  Result := fSelectedSourceIndex;
end;

function TCustomDelphiTwain.GetSelectedSourceID: TW_UINT32;
var
   rSelectedSource: TTwainSource;

begin
  rSelectedSource:= SelectedSource;
  if (rSelectedSource <> nil)
  then Result:= rSelectedSource.ID
  else Result:= 0;
end;

function TCustomDelphiTwain.GetSource(Index: Integer): TTwainSource;
begin
  {Both library and source manager must be loaded}
  if (LibraryLoaded and SourceManagerLoaded) then
  begin
    {If index is in range, returns}
    {(Call to SourceCount property enumerates the devices, if needed)}
    if Index in [0..SourceCount - 1] then
      Result := DeviceList.Item[Index]
    else if (Index = -1) and (SourceCount > 0) then
      Result := DeviceList.Item[GetDefaultSource]
    {Unknown object, returns nil}
    else Result := nil;

  end
  {In case either the library or the source manager aren't}
  {loaded, it returns nil}
  else Result := nil
end;

{Object being created}
constructor TCustomDelphiTwain.Create;
begin
  inherited Create;

  fSelectedSourceIndex := -1;

  DoCreate;
  DoCreateVirtualWindow;
  DoCreateTimer;
end;

{Object being destroyed}
destructor TCustomDelphiTwain.Destroy;
begin
  DoDestroyTimer;
  DoDestroyVirtualWindow;
  DoDestroy;

  {Let ancestor class handle}
  inherited Destroy;
end;

{Updates the application information object}
procedure TCustomDelphiTwain.SetInfo(const Value: TTwainIdentity);
begin
  {Assign one object to another}
  fInfo.Assign(Value);
end;

{ TTwainSource object implementation }

{Used with property SourceManagerLoaded to test if the source manager}
{is loaded or not.}
function TTwainSource.GetSourceManagerLoaded: Boolean;
begin
  {Obtain information from owner TCustomDelphiTwain}
  Result := Owner.SourceManagerLoaded;
end;

{Sets if the source is loaded}
procedure TTwainSource.SetLoaded(const Value: Boolean);
begin
  {Value should be changing}
  if (Value <> fLoaded) then
  begin
    {Loads or unloads the source}
    if Value           then  LoadSource
    else {if not Value then} UnloadSource;
  end {if (Value <> fLoaded)}
end;

{Sets if the source is enabled}
procedure TTwainSource.SetEnabled(const Value: Boolean);
begin
  {Source must be already enabled and value changing}
  if (Loaded) and (Value <> fEnabled) then
  begin
    {Enables/disables}
    if Value           then  EnableSource(ShowUI, Modal)
    else {if not Value then} DisableSource;
  end {if (Loaded) and (Value <> fEnabled)}
end;

{Enables the source}
function TTwainSource.EnableSource(ShowUI, Modal: Boolean; ParentWindow: TW_HANDLE): Boolean;
var
  twUserInterface: TW_USERINTERFACE;

begin
  {Builds UserInterface structure}
  twUserInterface.ShowUI := ShowUI;
  twUserInterface.ModalUI := Modal;

  if (ParentWindow=0)
  then twUserInterface.hParent := Owner.CustomGetParentWindow
  else twUserInterface.hParent := ParentWindow;

  Result :=EnableSource(twUserInterface);
end;

function TTwainSource.EnableSource(UserInterface: TW_USERINTERFACE): Boolean;
begin
  {Source must be loaded and the value changing}
  if (Loaded) and (not Enabled) then
  begin
    if (UserInterface.hParent=0) then UserInterface.hParent := Owner.CustomGetParentWindow;

    //npeter may be it is better to send messages to VirtualWindow
    //I am not sure, but it seems more stable with a HP TWAIN driver
    //it was: := GetActiveWindow;
    //fEnabled := TRUE;
    Owner.MessageTimer_Enable;
    {Call method}
    Result := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL,
      DAT_USERINTERFACE, MSG_ENABLEDS, @UserInterface) in
      [TWRC_SUCCESS, TWRC_CHECKSTATUS]);
  end
  else {If it's either not loaded or already enabled}
    {If it is not loaded}
    Result := FALSE or Enabled;

  {Updates property}
  if (Result = TRUE) then fEnabled := TRUE;
end;

{Disables the source}
function TTwainSource.DisableSource: Boolean;
var
  twUserInterface: TW_USERINTERFACE;
begin
  {Source must be loaded and the value changing}
  if Loaded and Enabled then
  begin
    {Call method}
    Result := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL,
      DAT_USERINTERFACE, MSG_DISABLEDS, @twUserInterface) = TWRC_SUCCESS);
    {Call notification event if being used}
    if (Result) and (Assigned(Owner.OnSourceDisable)) then
      Owner.OnSourceDisable(Owner, Index);

  end
  else {If it's either not loaded or already disabled}
    {If it is not loaded}
    Result := TRUE;

  {Updates property}
  //if (Result = TRUE) then fEnabled := FALSE;
  fEnabled := False;
  Owner.MessageTimer_Disable;
end;

{Loads the source}
function TTwainSource.LoadSource: Boolean;
begin
  {Only loads if it is not already loaded}
  if Not Loaded then
  begin
    Result := (Owner.TwainProc(AppInfo, nil, DG_CONTROL, DAT_IDENTITY,
      MSG_OPENDS, @Structure) = TWRC_SUCCESS);
    {Increase the loaded sources count variable}
    if Result then inc(Owner.fSourcesLoaded);
  end
  else
    {If it was already loaded, returns true}
    Result := TRUE;

  {In case the method was sucessful, updates property}
  if Result then
    fLoaded := TRUE;

end;

{Unloads the source}
function TTwainSource.UnloadSource: Boolean;
begin
  {Only unloads if it is loaded}
  if Loaded then
  begin
    {If the source was enabled, disable it}
    DisableSource;
    {Call method to load}
    Result := (Owner.TwainProc(AppInfo, nil, DG_CONTROL, DAT_IDENTITY,
      MSG_CLOSEDS, @Structure) = TWRC_SUCCESS);
    {Decrease the loaded sources count variable}
    if Result then dec(Owner.fSourcesLoaded);
  end
  else
    {If it was already unloaded, returns true}
    Result := TRUE;

  {In case the method was sucessful, updates property}
    fLoaded := FALSE;
end;

{Object being destroyed}
destructor TTwainSource.Destroy;
begin
  {If loaded, unloads source}
  UnloadSource;
  {Let ancestor class process}
  inherited Destroy;
end;

{Returns a pointer to the application}
function TTwainSource.GetAppInfo: pTW_IDENTITY;
begin
  Result := Owner.AppInfo;
end;

{Returns a pointer to the source identity}
function TTwainSource.GetStructure: pTW_IDENTITY;
begin
  Result := @Structure;
end;

{Object being created}
constructor TTwainSource.Create(AOwner: TCustomDelphiTwain);
begin
  {Allows ancestor class to process}
  inherited Create;

  {Initial values}
  fTransferMode := AOwner.TransferMode;
  fLoaded := FALSE;
  fShowUI := TRUE;
  fEnabled := FALSE;
  fModal := TRUE;
  {Stores owner}
  fOwner := AOwner;
end;

{Set source transfer mode}
{function TTwainSource.ChangeTransferMode(
  NewMode: TTwainTransferMode): TCapabilityRet;
const
  TransferModeToTwain: Array[TTwainTransferMode] of TW_UINT16 =
    (TWSX_FILE, TWSX_NATIVE, TWSX_MEMORY);
var
  Value: TW_UINT16;
begin
  //Set transfer mode method
  Value := TransferModeToTwain[NewMode];
  Result := SetOneValue(ICAP_XFERMECH, TWTY_UINT16, @Value);
  TransferMode := NewMode;
end;}

{Message received in the event loop}
function TTwainSource.ProcessMessage(const Msg: TMsg): Boolean;
var
  twEvent: TW_EVENT;
begin
  {Make twEvent structure}
  twEvent.TWMessage := MSG_NULL;
  twEvent.pEvent := TW_MEMREF(@Msg);
  {Call Twain procedure to handle message}
  Result := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_EVENT,
    MSG_PROCESSEVENT, @twEvent) = TWRC_DSEVENT);

  {If it is a message from the source, process}
  if Result then
    case twEvent.TWMessage of
      {No message from the source}
      MSG_NULL: exit;
      {Requested to close the source}
      MSG_CLOSEDSREQ:
      begin
        {Call notification event}
        if (Assigned(Owner.OnAcquireCancel)) then
          Owner.OnAcquireCancel(Owner, Index);
        if Assigned(Owner.OnTransferComplete) then
          Owner.OnTransferComplete(Owner, Index, True);
        {Disable the source}
        DisableSource;
        Owner.RefreshVirtualWindow;
      end;
      {Ready to transfer the images}
      MSG_XFERREADY:
        {Call method to transfer}
        TransferImages;

      MSG_CLOSEDSOK:
       result:=true;

      MSG_DEVICEEVENT:
       result:=true;

    end {case twEvent.TWMessage}
end;

{Returns return status information}
function TTwainSource.GetReturnStatus: TW_UINT16;
var
  StatusInfo: TW_STATUS;
begin
  {The source must be loaded in order to get the status}
  if Loaded then
  begin
    {Call method to get the information}
    Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_STATUS, MSG_GET,
      @StatusInfo);
    Result := StatusInfo.ConditionCode;
  end else Result := 0 {In case it was called while the source was not loaded}
end;

{Converts from a result to a TCapabilityRec}
function TTwainSource.ResultToCapabilityRec(
  const Value: TW_UINT16): TCapabilityRet;
begin

  {Test result code to return}
  case Value of
    {Successull, copy handle and return a success value}
    TWRC_SUCCESS: Result := crSuccess;
    {Error, get more on the error, and return result}
    {case} else
      case GetReturnStatus of
         TWCC_CAPUNSUPPORTED: Result := crUnsupported;
         TWCC_CAPBADOPERATION: Result := crBadOperation;
         TWCC_CAPSEQERROR: Result := crDependencyError;
         TWCC_LOWMEMORY: Result := crLowMemory;
         TWCC_SEQERROR: Result := crInvalidState;
         else Result := crBadOperation;
      end {case GetReturnStatus of}
  end {case};

end;

{Sets a capability}
function TTwainSource.SetCapabilityRec(const Capability, ConType: TW_UINT16;
  Data: HGLOBAL): TCapabilityRet;
var
  CapabilityInfo: TW_CAPABILITY;
begin
  {Source must be loaded to set}
  if Loaded then
  begin
    {Fill structure}
    CapabilityInfo.Cap := Capability;
    CapabilityInfo.ConType := ConType;
    CapabilityInfo.hContainer := Data;

    {Call method and store return}
    Result := ResultToCapabilityRec(Owner.TwainProc(AppInfo, @Structure,
      DG_CONTROL, DAT_CAPABILITY, MSG_SET, @CapabilityInfo));

  end
  else Result := crInvalidState  {In case the source is not loaded}
end;

function TTwainSource.GetCapabilitySupportedOp(const Capability: TW_UINT16): TCapabilityOperationSet;
var
  capRet:TCapabilityRet;
  CapabilityInfo: TW_CAPABILITY;
  queryRes:pTW_ONEVALUE;

begin
  Result :=[];

  {Source must be loaded}
  if Loaded then
  begin
    {Fill structure}
    CapabilityInfo.Cap := Capability;
    CapabilityInfo.ConType := TWON_ONEVALUE;
    CapabilityInfo.hContainer := 0;

    {Call method and store return}
    capRet := ResultToCapabilityRec(Owner.TwainProc(AppInfo, @Structure,
      DG_CONTROL, DAT_CAPABILITY, MSG_QUERYSUPPORT, @CapabilityInfo));

    if (capRet = crSuccess) then
    begin
      try
         queryRes := GlobalLock(CapabilityInfo.hContainer);
         if (queryRes<>nil) then
         begin
           if (queryRes^.Item and TWQC_GET)=TWQC_GET then Result :=[capGet];
           if (queryRes^.Item and TWQC_SET)=TWQC_SET then Result :=Result+[capSet];
           if (queryRes^.Item and TWQC_GETDEFAULT)=TWQC_GETDEFAULT then Result :=Result+[capGetDefault];
           if (queryRes^.Item and TWQC_GETCURRENT)=TWQC_GETCURRENT then Result :=Result+[capGetCurrent];
           if (queryRes^.Item and TWQC_RESET)=TWQC_RESET then Result :=Result+[capReset];
           if (queryRes^.Item and TWQC_SETCONSTRAINT)=TWQC_SETCONSTRAINT then Result :=Result+[capSetConstraint];
         end;
      finally
        GlobalUnlock(CapabilityInfo.hContainer);
        GlobalFree(CapabilityInfo.hContainer);
      end;
    end
  end;
end;

function TTwainSource.CapabilityCanGet(const Capability: TW_UINT16): Boolean;
begin
  Result := (capGet in GetCapabilitySupportedOp(Capability));
end;

function TTwainSource.CapabilityCanSet(const Capability: TW_UINT16): Boolean;
begin
  Result := (capSet in GetCapabilitySupportedOp(Capability));
end;

{Returns a capability strucutre}
function TTwainSource.GetCapabilityRec( const Capability: TW_UINT16;
  var Handle: HGLOBAL; Mode: TRetrieveCap;
  var Container: TW_UINT16): TCapabilityRet;
var
  CapabilityInfo: TW_CAPABILITY;
begin
  {Source must be loaded}
  if Loaded then
  begin
    {Fill structure}
    CapabilityInfo.Cap := Capability;
    CapabilityInfo.ConType := TWON_DONTCARE16;
    CapabilityInfo.hContainer := 0;

    {Call method and store return}
    Result := ResultToCapabilityRec(Owner.TwainProc(AppInfo, @Structure,
      DG_CONTROL, DAT_CAPABILITY, CapabilityRetrieveModeToTwain[Mode], @CapabilityInfo));

    if Result = crSuccess then
    begin
      Handle := CapabilityInfo.hContainer;
      Container := CapabilityInfo.ConType;
    end
  end {if not Loaded}
  else Result := crInvalidState  {In case the source is not loaded}
end;

{Gets an item and returns it in a string}
procedure TTwainSource.GetItem(var Return: String; ItemType: TW_UINT16;
  Data: Pointer);
begin
  {Test the item type}
  case ItemType of
    TWTY_INT8   :         Return := IntToStr(pTW_INT8(Data)^);
    TWTY_UINT8  :         Return := IntToStr(pTW_UINT8(Data)^);
    TWTY_INT16,
    44 {TWTY_HANDLE} :    Return := IntToStr(pTW_INT16(Data)^);
    TWTY_UINT16,
    TWTY_BOOL   :         Return := IntToStr(pTW_UINT16(Data)^);
    TWTY_INT32  :         Return := IntToStr(pTW_INT32(Data)^);
    TWTY_UINT32,
    43 {TWTY_MEMREF} :    Return := IntToStr(pTW_UINT32(Data)^);
    {Floating integer type}
    TWTY_FIX32:
      with pTW_FIX32(Data)^ do
        //npeter bugfix:
        //it is better to use the actual decimal separator
        //and not a wired in value!
        //If not, you may get error on strtofloat
        //original: Return := IntToStr(Whole) + ',' + IntToStr(Frac);
        Return := IntToStr(Whole) + {%H-}{$IFDEF DELPHI_XE2_UP}FormatSettings.{$ENDIF}DecimalSeparator + IntToStr(Frac);
    {String types, which are all ended by a null char (#0)}
    TWTY_STR32,
    TWTY_STR64,
    TWTY_STR128,
    TWTY_STR255 :         Return := String(PAnsiChar(Data));

  end {case ItemType}
end;

{Returns an array capability}
function TTwainSource.GetArrayValue(Capability: TW_UINT16; var List: TArrayInteger): TCapabilityRet;
var
  ArrayV   : pTW_ARRAY;
  ItemSize : Integer;
  Data     : PAnsiChar;
  CurItem  : Integer;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, rcGet, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_ARRAY)
    then begin
           ArrayV := GlobalLock(MemHandle);

           if (ArrayV^.ItemType in [TWTY_INT8,TWTY_UINT8,TWTY_INT16,44 {TWTY_HANDLE},
                                    TWTY_UINT16,TWTY_INT32,TWTY_UINT32,43 {TWTY_MEMREF}]) then
           begin
             {Prepare to list items}
             ItemSize := TWTypeSize(ArrayV^.ItemType);
             Data := @ArrayV^.ItemList[0];
             SetLength(List, ArrayV^.NumItems);

             {Copy items}
             for CurItem := 0 to ArrayV^.NumItems-1 do
             begin
               case ArrayV^.ItemType of
               TWTY_INT8   :      List[CurItem] := pTW_INT8(Data)^;
               TWTY_UINT8  :      List[CurItem] := pTW_UINT8(Data)^;
               TWTY_INT16,
               44 {TWTY_HANDLE} : List[CurItem] := pTW_INT16(Data)^;
               TWTY_UINT16 :      List[CurItem] := pTW_UINT16(Data)^;
               TWTY_INT32  :      List[CurItem] := pTW_INT32(Data)^;
               TWTY_UINT32,
               43 {TWTY_MEMREF} : List[CurItem] := pTW_UINT32(Data)^;
               end;

               {Move memory to the next}
               inc(Data, ItemSize);
             end;

             //Unlock memory
             GlobalUnlock(MemHandle);
          end
         else Result := crInvalidContainer;
      end
    else Result := crInvalidContainer;

    //Unallocate memory
    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetArrayValue(Capability: TW_UINT16; var List: TArraySingle): TCapabilityRet;
var
  ArrayV   : pTW_ARRAY;
  ItemSize : Integer;
  Data     : PChar;
  CurItem  : Integer;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, rcGet, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_ARRAY)
    then begin
           ArrayV := GlobalLock(MemHandle);

           if (ArrayV^.ItemType = TWTY_FIX32) then
           begin
             {Prepare to list items}
             ItemSize := TWTypeSize(ArrayV^.ItemType);
             Data := @ArrayV^.ItemList[0];
             SetLength(List, ArrayV^.NumItems);

             {Copy items}
             for CurItem := 0 to ArrayV^.NumItems-1 do
             begin
               List[CurItem] :=Fix32ToFloat(pTW_FIX32(Data)^);

               {Move memory to the next}
               inc(Data, ItemSize);
             end;

             //Unlock memory
             GlobalUnlock(MemHandle);
          end
         else Result := crInvalidContainer;
      end
    else Result := crInvalidContainer;

    //Unallocate memory
    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetArrayValue(Capability: TW_UINT16; var List: TStringArray): TCapabilityRet;
var
  ArrayV   : pTW_ARRAY;
  ItemSize : Integer;
  Data     : PAnsiChar;
  CurItem  : Integer;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, rcGet, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_ARRAY)
    then begin
           ArrayV := GlobalLock(MemHandle);

           if (ArrayV^.ItemType in [TWTY_STR32,TWTY_STR64,TWTY_STR128,TWTY_STR255]) then
           begin
             {Prepare to list items}
             ItemSize := TWTypeSize(ArrayV^.ItemType);
             Data := @ArrayV^.ItemList[0];
             SetLength(List, ArrayV^.NumItems);

             {Copy items}
             for CurItem := 0 to ArrayV^.NumItems-1 do
             begin
               List[CurItem] := String(Data);

               {Move memory to the next}
               inc(Data, ItemSize);
             end;

             //Unlock memory
             GlobalUnlock(MemHandle);
          end
         else Result := crInvalidContainer;
      end
    else Result := crInvalidContainer;

    //Unallocate memory
    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetArrayValue(Capability: TW_UINT16;
  var ItemType: TW_UINT16; var List: TStringArray;
  MemHandle: HGLOBAL): TCapabilityRet;
var
  ArrayV   : pTW_ARRAY;
  ItemSize : Integer;
  Data     : PAnsiChar;
  CurItem  : Integer;
  Value    : String;
  Container: TW_UINT16;
begin
  {Call method to get the memory to the return}
  if MemHandle = 0 then
    Result := GetCapabilityRec(Capability, MemHandle, rcGet, {%H-}Container)
  else
  begin
    Result := crSuccess;
    Container := TWON_ARRAY;
  end;

  if (Result = crSuccess) and (Container <> TWON_ARRAY) then
  begin
    Result := crInvalidContainer;
    GlobalFree(MemHandle);
    Exit;
  end;

  {If result was sucessfull and memory was allocated}
  if (Result = crSuccess) then
  begin
    {Obtain structure pointer}
    ArrayV := GlobalLock(MemHandle);

    {Fill return properties}
    ItemType := ArrayV^.ItemType;

    {Prepare to list items}
    ItemSize := TWTypeSize(ItemType);
    Data := @ArrayV^.ItemList[0];
    SetLength(List, ArrayV^.NumItems);

    {Copy items}
    for CurItem := 0 TO ArrayV^.NumItems - 1 do
    begin
      {Obtain this item}
      GetItem({%H-}Value, ItemType, Data);
      List[CurItem] := Value;
      {Move memory to the next}
      inc(Data, ItemSize);
    end;

    {Unlock memory and unallocate}
    GlobalUnlock(MemHandle);
    GlobalFree(MemHandle);
  end {if (Result = crSuccess)}
end;

{Returns an enumeration capability}
function TTwainSource.GetEnumerationValue(Capability: TW_UINT16;
  var List: TArrayInteger; var Current, Default: Integer; Mode: TRetrieveCap): TCapabilityRet;
var
  EnumV    : pTW_ENUMERATION;
  ItemSize : Integer;
  Data     : PAnsiChar;
  CurItem  : Integer;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, Mode, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_ENUMERATION)
    then begin
           EnumV := GlobalLock(MemHandle);

           if (EnumV^.ItemType in [TWTY_INT8,TWTY_UINT8,TWTY_INT16,44 {TWTY_HANDLE},
                                   TWTY_UINT16,TWTY_INT32,TWTY_UINT32,43 {TWTY_MEMREF}]) then
           begin
             {Prepare to list items}
             ItemSize := TWTypeSize(EnumV^.ItemType);
             Data := @EnumV^.ItemList[0];
             SetLength(List, EnumV^.NumItems);

             {Copy items}
             for CurItem := 0 to EnumV^.NumItems-1 do
             begin
               case EnumV^.ItemType of
               TWTY_INT8   :      List[CurItem] := pTW_INT8(Data)^;
               TWTY_UINT8  :      List[CurItem] := pTW_UINT8(Data)^;
               TWTY_INT16,
               44 {TWTY_HANDLE} : List[CurItem] := pTW_INT16(Data)^;
               TWTY_UINT16 :      List[CurItem] := pTW_UINT16(Data)^;
               TWTY_INT32  :      List[CurItem] := pTW_INT32(Data)^;
               TWTY_UINT32,
               43 {TWTY_MEMREF} : List[CurItem] := pTW_UINT32(Data)^;
               end;

               {Move memory to the next}
               inc(Data, ItemSize);
             end;
             Current :=List[EnumV^.CurrentIndex];
             Default :=List[EnumV^.DefaultIndex];

             //Unlock memory
             GlobalUnlock(MemHandle);
          end
         else Result := crInvalidContainer;
      end
    else Result := crInvalidContainer;

    //Unallocate memory
    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetEnumerationValue(Capability: TW_UINT16;
  var List: TArraySingle; var Current, Default: Single; Mode: TRetrieveCap): TCapabilityRet;
var
  EnumV    : pTW_ENUMERATION;
  ItemSize : Integer;
  Data     : PChar;
  CurItem  : Integer;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, Mode, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_ENUMERATION)
    then begin
           EnumV := GlobalLock(MemHandle);

           if (EnumV^.ItemType = TWTY_FIX32) then
           begin
             {Prepare to list items}
             ItemSize := TWTypeSize(EnumV^.ItemType);
             Data := @EnumV^.ItemList[0];
             SetLength(List, EnumV^.NumItems);

             {Copy items}
             for CurItem := 0 to EnumV^.NumItems-1 do
             begin
               List[CurItem] :=Fix32ToFloat(pTW_FIX32(Data)^);

               {Move memory to the next}
               inc(Data, ItemSize);
             end;
             Current :=List[EnumV^.CurrentIndex];
             Default :=List[EnumV^.DefaultIndex];

             //Unlock memory
             GlobalUnlock(MemHandle);
          end
         else Result := crInvalidContainer;
      end
    else Result := crInvalidContainer;

    //Unallocate memory
    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetEnumerationValue(Capability: TW_UINT16;
  var List: TStringArray; var Current, Default: String; Mode: TRetrieveCap): TCapabilityRet;
var
  EnumV    : pTW_ENUMERATION;
  ItemSize : Integer;
  Data     : PAnsiChar;
  CurItem  : Integer;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, Mode, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_ENUMERATION)
    then begin
           EnumV := GlobalLock(MemHandle);

           if (EnumV^.ItemType in [TWTY_STR32,TWTY_STR64,TWTY_STR128,TWTY_STR255]) then
           begin
             {Prepare to list items}
             ItemSize := TWTypeSize(EnumV^.ItemType);
             Data := @EnumV^.ItemList[0];
             SetLength(List, EnumV^.NumItems);

             {Copy items}
             for CurItem := 0 to EnumV^.NumItems-1 do
             begin
               List[CurItem] := String(Data);

               {Move memory to the next}
               inc(Data, ItemSize);
             end;
             Current :=List[EnumV^.CurrentIndex];
             Default :=List[EnumV^.DefaultIndex];

             //Unlock memory
             GlobalUnlock(MemHandle);
          end
         else Result := crInvalidContainer;
      end
    else Result := crInvalidContainer;

    //Unallocate memory
    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetEnumerationValue(Capability: TW_UINT16;
  var ItemType: TW_UINT16; var List: TStringArray;
  var Current, Default: Integer; Mode: TRetrieveCap;
  MemHandle: HGLOBAL): TCapabilityRet;
var
  EnumV    : pTW_ENUMERATION;
  ItemSize : Integer;
  Data     : PAnsiChar;
  CurItem  : Integer;
  Value    : String;
  Container: TW_UINT16;
begin
  {Call method to get the memory to the return}
  if MemHandle = 0 then
    Result := GetCapabilityRec(Capability, MemHandle, Mode, {%H-}Container)
  else
  begin
    Result := crSuccess;
    Container := TWON_ENUMERATION;
  end;

  if (Result = crSuccess) and (Container <> TWON_ENUMERATION) then
  begin
    Result := crInvalidContainer;
    GlobalFree(MemHandle);
    Exit;
  end;

  {If result was sucessfull and memory was allocated}
  if (Result = crSuccess) then
  begin
    {Obtain structure pointer}
    EnumV := GlobalLock(MemHandle);

    {Fill return properties}
    Current := EnumV^.CurrentIndex;
    Default := EnumV^.DefaultIndex;
    ItemType := EnumV^.ItemType;

    {Prepare to list items}
    ItemSize := TWTypeSize(ItemType);
    Data := @EnumV^.ItemList[0];
    SetLength(List, EnumV^.NumItems);

    {Copy items}
    for CurItem := 0 to EnumV^.NumItems - 1 do
    begin
      {Obtain this item}
      GetItem({%H-}Value, ItemType, Data);
      List[CurItem] := Value;
      {Move memory to the next}
      inc(Data, ItemSize);
    end;

    {Unlock memory and unallocate}
    GlobalUnlock(MemHandle);
    GlobalFree(MemHandle);
  end {if (Result = crSuccess)}
end;

{Returns a range capability}
function TTwainSource.GetRangeValue(Capability: TW_UINT16; var Min, Max, Step, Default, Current: Integer): TCapabilityRet;
var
  RangeV   : pTW_RANGE;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, rcGet, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_RANGE)
    then begin
           //Obtain structure pointer
           RangeV := GlobalLock(MemHandle);

           case RangeV^.ItemType of
           TWTY_INT8   : begin
                           Min := pTW_INT8(@RangeV^.MinValue)^;
                           Max := pTW_INT8(@RangeV^.MaxValue)^;
                           Step := pTW_INT8(@RangeV^.StepSize)^;
                           Default := pTW_INT8(@RangeV^.DefaultValue)^;
                           Current := pTW_INT8(@RangeV^.CurrentValue)^;
                         end;
           TWTY_UINT8  : begin
                           Min := pTW_UINT8(@RangeV^.MinValue)^;
                           Max := pTW_UINT8(@RangeV^.MaxValue)^;
                           Step := pTW_UINT8(@RangeV^.StepSize)^;
                           Default := pTW_UINT8(@RangeV^.DefaultValue)^;
                           Current := pTW_UINT8(@RangeV^.CurrentValue)^;
                         end;
           TWTY_INT16, {TWTY_HANDLE}
           44          : begin
                           Min := pTW_INT16(@RangeV^.MinValue)^;
                           Max := pTW_INT16(@RangeV^.MaxValue)^;
                           Step := pTW_INT16(@RangeV^.StepSize)^;
                           Default := pTW_INT16(@RangeV^.DefaultValue)^;
                           Current := pTW_INT16(@RangeV^.CurrentValue)^;
                         end;
           TWTY_UINT16 : begin
                           Min := pTW_UINT16(@RangeV^.MinValue)^;
                           Max := pTW_UINT16(@RangeV^.MaxValue)^;
                           Step := pTW_UINT16(@RangeV^.StepSize)^;
                           Default := pTW_UINT16(@RangeV^.DefaultValue)^;
                           Current := pTW_UINT16(@RangeV^.CurrentValue)^;
                         end;
           TWTY_INT32  : begin
                           Min := pTW_INT32(@RangeV^.MinValue)^;
                           Max := pTW_INT32(@RangeV^.MaxValue)^;
                           Step := pTW_INT32(@RangeV^.StepSize)^;
                           Default := pTW_INT32(@RangeV^.DefaultValue)^;
                           Current := pTW_INT32(@RangeV^.CurrentValue)^;
                         end;
           TWTY_UINT32, {TWTY_MEMREF}
           43           : begin
                           Min := pTW_UINT32(@RangeV^.MinValue)^;
                           Max := pTW_UINT32(@RangeV^.MaxValue)^;
                           Step := pTW_UINT32(@RangeV^.StepSize)^;
                           Default := pTW_UINT32(@RangeV^.DefaultValue)^;
                           Current := pTW_UINT32(@RangeV^.CurrentValue)^;
                         end;
           else Result := crInvalidContainer;
           end;

           //Unlock memory
           GlobalUnlock(MemHandle);
         end
    else Result := crInvalidContainer;

    //Unallocate memory
    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetRangeValue(Capability: TW_UINT16; var Min, Max, Step, Default, Current: Single): TCapabilityRet;
var
  RangeV   : pTW_RANGE;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, rcGet, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_RANGE)
    then begin
           RangeV := GlobalLock(MemHandle);

           if (RangeV^.ItemType = TWTY_FIX32)
           then begin
                  Min :=Fix32ToFloat(pTW_FIX32(@RangeV^.MinValue)^);
                  Max :=Fix32ToFloat(pTW_FIX32(@RangeV^.MaxValue)^);
                  Step :=Fix32ToFloat(pTW_FIX32(@RangeV^.StepSize)^);
                  Default :=Fix32ToFloat(pTW_FIX32(@RangeV^.DefaultValue)^);
                  Current :=Fix32ToFloat(pTW_FIX32(@RangeV^.CurrentValue)^);
                end
           else Result := crInvalidContainer;

           GlobalUnlock(MemHandle);
         end
    else Result := crInvalidContainer;

    GlobalFree(MemHandle);
  end;
end;

//MaxM: not to be confused with the following method, this one works on a string range (which might not make much sense)
function TTwainSource.GetRangeValue(Capability: TW_UINT16; var Min, Max, Step, Default, Current: String): TCapabilityRet;
var
  RangeV   : pTW_RANGE;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, rcGet, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_RANGE)
    then begin
           RangeV := GlobalLock(MemHandle);

           if (RangeV^.ItemType in [TWTY_STR32,TWTY_STR64,TWTY_STR128,TWTY_STR255])
           then begin
                  Min := String(PAnsiChar(@RangeV^.MinValue));
                  Max := String(PAnsiChar(@RangeV^.MaxValue));
                  Step := String(PAnsiChar(@RangeV^.StepSize));
                  Default := String(PAnsiChar(@RangeV^.DefaultValue));
                  Current := String(PAnsiChar(@RangeV^.CurrentValue));
                end
           else Result := crInvalidContainer;

           GlobalUnlock(MemHandle);
         end
    else Result := crInvalidContainer;

    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetRangeValue(Capability: TW_UINT16;
  var ItemType: TW_UINT16; var Min, Max, Step, Default,
  Current: String; MemHandle: HGLOBAL): TCapabilityRet;
var
  RangeV   : pTW_RANGE;
  Container: TW_UINT16;
begin
  {Call method to get the memory to the return}
  if MemHandle = 0 then
    Result := GetCapabilityRec(Capability, MemHandle, rcGet, {%H-}Container)
  else
  begin
    Result := crSuccess;
    Container := TWON_RANGE;
  end;

  if (Result = crSuccess) and (Container <> TWON_RANGE) then
  begin
    Result := crInvalidContainer;
    GlobalFree(MemHandle);
    Exit;
  end;

  {If result was sucessfull and memory was allocated}
  if (Result = crSuccess) then
  begin
    {Obtain structure pointer}
    RangeV := GlobalLock(MemHandle);
    {Fill return}
    ItemType := RangeV^.ItemType;
    GetItem(Min, ItemType, @RangeV^.MinValue);
    GetItem(Max, ItemType, @RangeV^.MaxValue);
    GetItem(Step, ItemType, @RangeV^.StepSize);
    GetItem(Default, ItemType, @RangeV^.DefaultValue);
    GetItem(Current, ItemType, @RangeV^.CurrentValue);

    {Unlock memory and unallocate}
    GlobalUnlock(MemHandle);
    GlobalFree(MemHandle);
  end {if (Result = crSuccess)}
end;

{Returns an one value capability}
function TTwainSource.GetOneValue(Capability: TW_UINT16; var Value: Integer; Mode: TRetrieveCap): TCapabilityRet;
var
  OneV     : pTW_ONEVALUE;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, Mode, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_ONEVALUE)
    then begin
           //Obtain structure pointer
           OneV := GlobalLock(MemHandle);

           case OneV^.ItemType of
           TWTY_INT8   :         Value := pTW_INT8(@OneV^.Item)^;
           TWTY_UINT8  :         Value := pTW_UINT8(@OneV^.Item)^;
           TWTY_INT16,
           44 {TWTY_HANDLE} :    Value := pTW_INT16(@OneV^.Item)^;
           TWTY_UINT16 :         Value := pTW_UINT16(@OneV^.Item)^;
           TWTY_INT32  :         Value := pTW_INT32(@OneV^.Item)^;
           TWTY_UINT32,
           43 {TWTY_MEMREF} :    Value := pTW_UINT32(@OneV^.Item)^;
           else Result := crInvalidContainer;
           end;

           //Unlock memory
           GlobalUnlock(MemHandle);
         end
    else Result := crInvalidContainer;

    //Unallocate memory
    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetOneValue(Capability: TW_UINT16; var Value: Single; Mode: TRetrieveCap): TCapabilityRet;
var
  OneV     : pTW_ONEVALUE;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, Mode, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_ONEVALUE)
    then begin
           OneV := GlobalLock(MemHandle);

           if (OneV^.ItemType = TWTY_FIX32)
           then Value := Fix32ToFloat(pTW_FIX32(@OneV^.Item)^)
           else Result := crInvalidContainer;

           GlobalUnlock(MemHandle);
         end
    else Result := crInvalidContainer;

    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetOneValue(Capability: TW_UINT16; var Value: Boolean; Mode: TRetrieveCap): TCapabilityRet;
var
  OneV     : pTW_ONEVALUE;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, Mode, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_ONEVALUE)
    then begin
           OneV := GlobalLock(MemHandle);

           if (OneV^.ItemType = TWTY_BOOL)
           then Value :=pTW_BOOL(@OneV^.Item)^
           else Result := crInvalidContainer;

           GlobalUnlock(MemHandle);
         end
    else Result := crInvalidContainer;

    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetOneValue(Capability: TW_UINT16; var Value: String; Mode: TRetrieveCap): TCapabilityRet;
var
  OneV     : pTW_ONEVALUE;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;

begin
  MemHandle :=0;
  Result := GetCapabilityRec(Capability, MemHandle, Mode, {%H-}Container);
  if (Result = crSuccess) then
  begin
    if (Container = TWON_ONEVALUE)
    then begin
           OneV := GlobalLock(MemHandle);

           if (OneV^.ItemType in [TWTY_STR32,TWTY_STR64,TWTY_STR128,TWTY_STR255])
           then Value := String(PAnsiChar(@OneV^.Item))
           else Result := crInvalidContainer;

           GlobalUnlock(MemHandle);
         end
    else Result := crInvalidContainer;

    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.GetOneValue(Capability: TW_UINT16;
  var ItemType: TW_UINT16; var Value: string; Mode: TRetrieveCap;
  MemHandle: HGLOBAL): TCapabilityRet;
var
  OneV     : pTW_ONEVALUE;
  Container: TW_UINT16;

begin
  if (MemHandle = 0)
  then Result := GetCapabilityRec(Capability, MemHandle, Mode, {%H-}Container)
  else begin
         Result := crSuccess;
         Container := TWON_ONEVALUE;
       end;

  if (Result = crSuccess) then
  begin
    if (Container = TWON_ONEVALUE)
    then begin
           //Obtain structure pointer
           OneV := GlobalLock(MemHandle);

           //Fill return Values
           ItemType := OneV^.ItemType;
           GetItem(Value, OneV^.ItemType, @OneV^.Item);

           //Unlock memory
           GlobalUnlock(MemHandle);
         end
    else Result := crInvalidContainer;

    //Unallocate memory;
    GlobalFree(MemHandle);
  end;
end;

{Sets an one value capability}
function TTwainSource.SetOneValue(Capability: TW_UINT16; Value: Single): TCapabilityRet;
var
  Fix32: TW_FIX32;

begin
  Fix32 := FloatToFix32(Value);
  Result := SetOneValue(Capability, TWTY_FIX32, @Fix32);
end;

function TTwainSource.SetOneValue(Capability: TW_UINT16; Value: Boolean): TCapabilityRet;
var
   iValue: TW_BOOL;

begin
  iValue := Value;
  Result := SetOneValue(Capability, TWTY_BOOL, @iValue);
end;

function TTwainSource.SetOneValue(Capability: TW_UINT16;
  ItemType: TW_UINT16; Value: Pointer): TCapabilityRet;
var
  Data: HGLOBAL;
  OneV: pTW_ONEVALUE;
  ItemSize,ItemSize2: Integer;
begin
  {Allocate enough memory for the TW_ONEVALUE and obtain pointer}
  ItemSize := TWTypeSize(ItemType);
  //npeter: TW_ONEVALUE minimal size !!!
  //I think to meet the specifications the
  //Item's size must be at least sizeof(TW_UINT32)!
  //when I did it, some mistic errors on some drivers went gone
  if ItemSize<TWTypeSize(TWTY_UINT32) then ItemSize2:=TWTypeSize(TWTY_UINT32) else ItemSize2:=ItemSize;
  Data := GlobalAlloc(GHND, sizeof({%H-}OneV^.ItemType) + ItemSize2);
  OneV := GlobalLock(Data);

  {Fill value}
  OneV^.ItemType := ItemType;
  CopyMemory(@OneV^.Item, Value, ItemSize);
  GlobalUnlock(Data);

  {Call method to set}
  Result := SetCapabilityRec(Capability, TWON_ONEVALUE, Data);

  {Unload memory}
  GlobalFree(Data);
end;

function TTwainSource.GetOrientation(var Value: TTwainOrientation): TCapabilityRet;
var
  iValue: Integer;

begin
  Result := GetOneValue(ICAP_ORIENTATION, iValue, rcGet);

  if (Result = crSuccess) then
  begin
    Case iValue of
    TWOR_PORTRAIT : Value :=torPortrait;
    TWOR_LANDSCAPE: Value :=torLandscape;
    else Result := crInvalidContainer;
    end;
  end;
end;

function TTwainSource.SetOrientation(Value: TTwainOrientation): TCapabilityRet;
var
  iValue: TW_UINT16;

begin
  if (Value=torPortrait)
  then iValue:=TWOR_PORTRAIT
  else iValue:=TWOR_LANDSCAPE;

  Result := SetOneValue(ICAP_ORIENTATION, TWTY_UINT16, @iValue);
end;

function TTwainSource.GetPaperFeeding: TTwainPaperFeedingSet;
var
   capRet: TCapabilityRet;
   feedEnabled: Boolean;

begin
  Result :=[];

  //Get Initial Feeder State
  capRet :=GetFeederEnabled(feedEnabled);

  //if we can disable the Feeder then the scanner have a Plane
  capRet :=SetFeederEnabled(False);
  if (capRet=crSuccess) then Result :=[pfFlatbed];

  //if we can enable the Feeder then the scanner have it
  capRet :=SetFeederEnabled(True);
  if (capRet=crSuccess) then Result :=Result+[pfFeeder];

  //Set Feeder State to original State
  capRet :=SetFeederEnabled(feedEnabled);
end;

function TTwainSource.SetPaperFeeding(Value: TTwainPaperFeeding): TCapabilityRet;
begin
  Result :=SetFeederEnabled((Value=pfFeeder));
end;

function TTwainSource.GetPaperSizeSet(var Current, Default:TTwainPaperSize; var Values:TTwainPaperSizeSet):TCapabilityRet;
var
  EnumV: pTW_ENUMERATION;
  Item: pTW_UINT16;
  CurItem: Integer;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;
  curVal: TTwainPaperSize;

begin
  Values:=[];
  Current:=tpsNONE;
  Default:=tpsNONE;
  MemHandle:=0;
  Result := GetCapabilityRec(ICAP_SUPPORTEDSIZES, MemHandle, rcGet, {%H-}Container);

  if (Result = crSuccess) and (Container <> TWON_ENUMERATION) then
  begin
    GlobalFree(MemHandle);
    Result := crUnsupported;
    Exit;
  end;

  {If result was sucessfull and memory was allocated}
  if (Result = crSuccess) then
  begin
    {Obtain structure pointer}
    EnumV := GlobalLock(MemHandle);

    //Paper Sizes must be TWTY_UINT16
    if (EnumV^.ItemType=TWTY_UINT16) then
    begin
      {Prepare to list items}
      Item := @EnumV^.ItemList[0];

      {Fill the Set with items}
      for CurItem:=0 to (EnumV^.NumItems-1) do
      begin
        curVal:=TWSS_ToTwainPaperSize(Item^);

        if (CurItem=EnumV^.CurrentIndex) then Current:=curVal;
        if (CurItem=EnumV^.DefaultIndex) then Default:=curVal;

        Values :=Values+[curVal];

        inc(Item);
      end;
    end;

    {Unlock memory and unallocate}
    GlobalUnlock(MemHandle);
    GlobalFree(MemHandle);
  end;
end;

function TTwainSource.SetPaperSize(Value: TTwainPaperSize): TCapabilityRet;
var iValue: TW_UINT16;
begin
  iValue:=PaperSizeToTwain(Value);
  Result := SetOneValue(ICAP_SUPPORTEDSIZES, TWTY_UINT16, @iValue);
end;

function TTwainSource.GetDuplexEnabled(var Value: Boolean): TCapabilityRet;
begin
  Result :=GetOneValue(CAP_DUPLEXENABLED, Value, rcGet);
end;

function TTwainSource.SetDuplexEnabled(Value: Boolean): TCapabilityRet;
begin
 Result :=SetOneValue(CAP_DUPLEXENABLED, Value);
end;

{Sets a range capability}
function TTwainSource.SetRangeValue(Capability, ItemType: TW_UINT16; Min, Max,
  Step, Current: TW_UINT32): TCapabilityRet;
var
  Data: HGLOBAL;
  RangeV: pTW_RANGE;
begin
  {Allocate enough memory for the TW_RANGE and obtain pointer}
  Data := GlobalAlloc(GHND, sizeof(TW_RANGE));
  RangeV := GlobalLock(Data);

  {Fill value}
  RangeV^.ItemType := ItemType;
  RangeV^.MinValue := Min;
  RangeV^.MaxValue := Max;
  RangeV^.StepSize := Step;
  RangeV^.CurrentValue := Current;
  GlobalUnlock(Data);

  {Call method to set}
  Result := SetCapabilityRec(Capability, TWON_RANGE, Data);

  {Unload memory}
  GlobalFree(Data);
end;

function TTwainSource.SetUndefinedImageSize(Value: Boolean): TCapabilityRet;
var iValue: TW_BOOL;
begin
  iValue := Value;
  Result := SetOneValue(ICAP_UNDEFINEDIMAGESIZE, TWTY_BOOL, @iValue);
end;

{Sets an array capability}
function TTwainSource.SetArrayValue(Capability, ItemType: TW_UINT16;
  List: TSetCapabilityList): TCapabilityRet;
var
  Data: HGLOBAL;
  EnumV: pTW_ENUMERATION;
  i, ItemSize: Integer;
  DataPt: PAnsiChar;//ccc
begin
  {Allocate enough memory for the TW_ARRAY and obtain pointer}
  ItemSize := TWTypeSize(ItemType);
  Data := GlobalAlloc(GHND, sizeof(TW_ARRAY) + ItemSize * Length(List));
  EnumV := GlobalLock(Data);

  {Fill values}
  EnumV^.ItemType := ItemType;
  EnumV^.NumItems := Length(List);

  {Copy item values}
  DataPt := @EnumV^.ItemList[0];
  for i := Low(List) TO High(List) do
  begin
    {Copy item}
    CopyMemory(DataPt, List[i], ItemSize);
    {Move to next item}
    inc(DataPt, ItemSize);
  end;
  GlobalUnlock(Data);

  {Call method to set}
  Result := SetCapabilityRec(Capability, TWON_ARRAY, Data);

  {Unload memory}
  GlobalFree(Data);
end;

{Sets an enumeration capability}
function TTwainSource.SetEnumerationValue(Capability, ItemType: TW_UINT16;
  CurrentIndex: TW_UINT32; List: TSetCapabilityList): TCapabilityRet;
var
  Data: HGLOBAL;
  EnumV: pTW_ENUMERATION;
  i, ItemSize: Integer;
  DataPt: PAnsiChar;//ccc
begin
  {Allocate enough memory for the TW_ENUMERATION and obtain pointer}
  ItemSize := TWTypeSize(ItemType);
  Data := GlobalAlloc(GHND, sizeof(TW_ENUMERATION) + ItemSize * Length(List));
  EnumV := GlobalLock(Data);

  {Fill values}
  EnumV^.ItemType := ItemType;
  EnumV^.NumItems := Length(List);
  EnumV^.CurrentIndex := CurrentIndex;

  {Copy item values}
  DataPt := @EnumV^.ItemList[0];
  for i := Low(List) TO High(List) do
  begin
    {Copy item}
    CopyMemory(DataPt, List[i], ItemSize);
    {Move to next item}
    inc(DataPt, ItemSize);
  end;
  GlobalUnlock(Data);

  {Call method to set}
  Result := SetCapabilityRec(Capability, TWON_ENUMERATION, Data);

  {Unload memory}
  GlobalFree(Data);
end;

{Transfer image memory}
function TTwainSource.TransferImageMemory(var ImageHandle: HBitmap;
  PixelType: TW_INT16): TW_UINT16;
var
  {Memory buffer information from the source}
  Setup  : TW_SETUPMEMXFER;
  {Memory information from the image}
  Xfer   : TW_IMAGEMEMXFER;
  {Image processing variables}
  ImageInfo : Windows.TBitmap;
  Ptr       : PAnsiChar;//ccc
  LineLength,
  CurLine: Cardinal;
  LinePtr,
  AllocPtr  : pointer;
  DataSize,
  Readed: Cardinal;

  Index    : Cardinal;
  ItemPtr  : pRGBTriple;
  Temp     : Byte;

begin
  {Obtain information on the transference buffers}
  Result := Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_SETUPMEMXFER,
    MSG_GET, @Setup);

  {Get information on the bitmap}
  GetObject(ImageHandle, sizeof(Windows.TBitmap), @ImageInfo);
  LineLength := (((ImageInfo.bmWidth * ImageInfo.bmBitsPixel + 31) div 32) * 4);
  {Get pointer for the last line}
  CurLine := ImageInfo.bmHeight - 1;
  {%H-}DTNativeUInt(LinePtr) := DTNativeUInt(ImageInfo.bmBits) + LineLength * CurLine;
  Ptr := LinePtr;
  DataSize := 0;

  {Prepare buffer record to transfer}
  Fillchar({%H-}Xfer, SizeOf(TW_IMAGEMEMXFER), $FF);
  Xfer.Memory.Flags := TWMF_APPOWNS or TWMF_POINTER;
  Xfer.Memory.Length := Setup.Preferred;
  GetMem(AllocPtr, Setup.Preferred);
  Xfer.Memory.TheMem := AllocPtr;

  {Transfer data until done or cancelled}
  if Result = TWRC_SUCCESS then begin
    repeat
      {Retrieve another piece of memory to the pointer}
      Xfer.BytesWritten := 0;
      Result := Owner.TwainProc(AppInfo, @Structure, DG_IMAGE,
        DAT_IMAGEMEMXFER, MSG_GET, @Xfer);
      {Test the result}
      {Piece sucessfully transfer, move to next}
      if (Result = TWRC_SUCCESS) or (Result = TWRC_XFERDONE) then
      begin
        {While we have data}
        while Xfer.BytesWritten > 0 do
        begin
          {In case the total bytes received now have more than we}
          {need to complete the line}
          if Xfer.BytesWritten + DataSize > LineLength then
          begin
            Readed := LineLength - DataSize;
            CopyMemory(Ptr, Xfer.Memory.TheMem, LineLength - DataSize);
          end
          else
          {Otherwise, continue completing the line}
          begin
            Readed := Xfer.BytesWritten;
            CopyMemory(Ptr, Xfer.Memory.TheMem, Readed);
          end;

          {Adjust}
          inc(DataSize, Readed); inc(Ptr, Readed);
          dec(Xfer.BytesWritten, Readed);
          {%H-}DTNativeUInt(Xfer.Memory.TheMem) :=
            {%H-}DTNativeUInt(Xfer.Memory.TheMem) + Readed;

          {Reached end of line}
          if (DataSize >= LineLength) then
          begin
            {Fix RGB to BGR}
            if PixelType = TWPT_RGB then
            begin
              ItemPtr := LinePtr;
              FOR Index := 1 TO ImageInfo.bmWidth DO
              begin
                Temp := ItemPtr^.rgbtRed;
                ItemPtr^.rgbtRed := ItemPtr^.rgbtBlue;
                ItemPtr^.rgbtBlue := Temp;
                inc(ItemPtr);
              end {FOR Index};
            end {if PixelType = TWPT_RGB};

            {Adjust pointers}
            {%H-}DTNativeUInt(LinePtr) := {%H-}DTNativeUInt(LinePtr) - LineLength;
            Ptr := LinePtr; dec(CurLine); DataSize := 0;

            {Call event}
            Owner.DoAcquireProgress(Self, Self.Index, ImageHandle,
              Cardinal(ImageInfo.bmHeight) - CurLine - 1,
              ImageInfo.bmHeight - 1);

          end {if DataSize >= LineLength}

        end {while Xfer.BytesWritten > 0};


        {Set again pointer to write to}
        Xfer.Memory.TheMem := AllocPtr;
      end {TWRC_SUCCESS};

    until Result <> TWRC_SUCCESS;
  end;
  {Free allocated memory}
  FreeMem(AllocPtr, Setup.Preferred);

  {Some error ocurred, free memory and returns}
  if Result <> TWRC_XFERDONE then
    DeleteObject(ImageHandle);
end;

{Prepare image memory transference}
function TTwainSource.PrepareMemXfer(var BitmapHandle: HBitmap;
  var PixelType: TW_INT16): TW_UINT16;
const
  PixelColor: Array[TTwainPixelFlavor] of Array[0..1] of Byte =
   ((0, $FF), ($FF, 00), (0, $FF));
var
  Handle: HGlobal;
  Info: TW_IMAGEINFO;
  Setup: TW_SETUPMEMXFER;
  structsize, index, Size, Blocks: Integer;
  XRes, YRes: Single;
  Pal   : TW_PALETTE8;
  vUnit : TTwainUnit;
  vUnits: TTwainUnitSet;
  Dib   : pBitmapInfo;
  PixelFlavor: TTwainPixelFlavor;
  PixelFlavors: TTwainPixelFlavorSet;
  DC: HDC;
  Data  : Pointer;
begin
  {First of all, get information on the image being acquired}
  Result := Owner.TwainProc(AppInfo, @Structure, DG_IMAGE, DAT_IMAGEINFO,
    MSG_GET, @Info);
  if Result <> TWRC_SUCCESS then exit;

  {Calculate image size}
  with Info do
    size := ((((ImageWidth * BitsPerPixel + 31) div 32)*4) * info.ImageLength);

  {Obtain image buffer transference sizes}
  Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_SETUPMEMXFER,
    MSG_GET, @Setup);
  blocks := (size div Integer(setup.Preferred));
  size := (blocks + 1) * Integer(setup.Preferred);

  {Prepare new bitmap}
  structsize := size + sizeof(BITMAPINFOHEADER) + 256 * sizeof(RGBQUAD);

  Handle := GlobalAlloc(GHND, StructSize);
  Dib := GlobalLock(Handle);
  Fillchar(Dib^, structsize, #0);
  {Fill image information}
  Dib^.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
  Dib^.bmiHeader.biWidth := info.ImageWidth;
  Dib^.bmiHeader.biHeight := info.ImageLength;
  {Only 1 plane supported}
  Dib^.bmiHeader.biPlanes := 1;
  Dib^.bmiHeader.biBitCount := info.BitsPerPixel;
  {No compression}
  Dib^.bmiHeader.biCompression := BI_RGB;
  Dib^.bmiHeader.biSizeImage := Size;

  {Adjust units}
  XRes := Fix32ToFloat(Info.XResolution);
  YRes := Fix32ToFloat(Info.YResolution);
  GetICapUnits({%H-}vUnit, {%H-}vUnits);
  case vUnit of
    tuInches: begin
      Dib^.bmiHeader.biXPelsPerMeter := Trunc((XRes*2.54)*100);
      Dib^.bmiHeader.biYPelsPerMeter := Trunc((YRes*2.54)*100);
      end;
    tuCentimeters: begin
      Dib^.bmiHeader.biXPelsPerMeter := Trunc(XRes*100);
      Dib^.bmiHeader.biYPelsPerMeter := Trunc(YRes*100);
      end
    else begin
      Dib^.bmiHeader.biXPelsPerMeter := 0;
      Dib^.bmiHeader.biYPelsPerMeter := 0;
    end
  end {case vUnits of};

  {Now it should setup the palette to be used by the image}
  {by either building a definied palette or retrieving the}
  {image's one}
  case (Info.PixelType) of
    TWPT_BW:
    begin
      {Only two colors are used}
      Dib^.bmiHeader.biClrUsed := 2;
      Dib^.bmiHeader.biClrImportant := 0;
      {Try obtaining the pixel flavor}
      if GetIPixelFlavor({%H-}PixelFlavor, {%H-}PixelFlavors) <> crSuccess then
        PixelFlavor := tpfChocolate;
      {Set palette colors}
      for Index := 0 to 1 do
      begin
        Dib^.bmiColors[Index].rgbRed := PixelColor[PixelFlavor][Index];
        Dib^.bmiColors[Index].rgbGreen := PixelColor[PixelFlavor][Index];
        Dib^.bmiColors[Index].rgbBlue := PixelColor[PixelFlavor][Index];
        Dib^.bmiColors[Index].rgbReserved := 0;
      end;

    end;
    TWPT_GRAY:
    begin
      {Creates a 256 shades of gray palette}
      Dib^.bmiHeader.biClrUsed := 256;
      for index := 0 to 255 do
      begin
        Dib^.bmiColors[index].rgbRed := index;
        Dib^.bmiColors[index].rgbGreen := index;
        Dib^.bmiColors[index].rgbBlue := index;
        Dib^.bmiColors[index].rgbReserved := 0;
      end {for i}
    end;
    TWPT_RGB: Dib^.bmiHeader.biClrUsed := 0;
    else
    begin
      {Try obtaining the palette}
      if Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_PALETTE8,
        MSG_GET, @Pal) <> TWRC_SUCCESS then
      begin
        {If the source did not provide a palette, uses shades of gray here}
        Dib^.bmiHeader.biClrUsed := 256;
        for index := 0 to 255 do
        begin
          Dib^.bmiColors[index].rgbRed := index;
          Dib^.bmiColors[index].rgbGreen := index;
          Dib^.bmiColors[index].rgbBlue := index;
          Dib^.bmiColors[index].rgbReserved := 0;
        end {for i}
      end
      else
      begin
        {Uses source palette here}
        Dib^.bmiHeader.biClrUsed := Pal.NumColors;
        for Index := 0 TO Pal.NumColors - 1 do
        begin
          Dib^.bmiColors[index].rgbRed := pal.Colors[index].Channel1;
          Dib^.bmiColors[index].rgbGreen := pal.Colors[index].Channel2;
          Dib^.bmiColors[index].rgbBlue := pal.Colors[index].Channel3;
          Dib^.bmiColors[index].rgbReserved := 0;
        end {for Index}
      end {if Owner.TwainProc(AppInfo...}

    end {case else};
  end {case Info.PixelType};

  {Creates the bitmap}
  DC := GetDC(Owner.VirtualWindow);
  {%H-}DTNativeUInt({%H-}Data) := DTNativeUInt(Dib) + Dib^.bmiHeader.biSize +
    (Dib^.bmiHeader.biClrUsed * sizeof(RGBQUAD));
  BitmapHandle := CreateDIBSection(DC, Dib^, DIB_RGB_COLORS, Data, 0, 0);
  ReleaseDC(Owner.VirtualWindow, DC);
  PixelType := Info.PixelType;

  {Unlock and free data}
  GlobalUnlock(Handle);
  GlobalFree(Handle);
end;

{Method to transfer the images}
procedure TTwainSource.TransferImages;
var
  {Return code from Twain method}
  rc : TW_UINT16;
  {Handle to the native Device independent Image (DIB)}
  hNative: TW_UINT32;
  {Pending transfers structure}
  PendingXfers: TW_PENDINGXFERS;
  {File transfer info}
  Info: TW_SETUPFILEXFER;
  {Image handle and pointer}
  ImageHandle: HBitmap;
  PixelType  : TW_INT16;

begin
  {Set the transfer mode}
  //npeter:
  //on a HP driver I got error events
  //when it was set above state 5;
  //commented out
  // ChangeTransferMode(TransferMode);

  rDownload_Cancelled := False; {Testing if it was cancelled}
  rDownload_Done := False;  {Initialize done variable}

  {Obtain all the images from the source}
  rDownload_Count:= -1;
  repeat
    Inc(rDownload_Count);

    {Transfer depending on the transfer mode}
    case TransferMode of
      {Native transfer, the source creates the image thru a device}
      {dependent image}
      ttmNative:
      begin
        {Call method to obtain the image}
        hNative := 0;
        rc := Owner.TwainProc(AppInfo, @Structure, DG_IMAGE,
          DAT_IMAGENATIVEXFER, MSG_GET, @hNative);
      end {case ttmNative};
      {File transfering, the source should create a file with}
      {the acquired image}
      ttmFile:
      begin
        {Event to allow user to set the file transfer information}
        if Assigned(Owner.OnSourceSetupFileXfer) then
          Owner.OnSourceSetupFileXfer(Owner, Index);

        Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_SETUPFILEXFER,
          MSG_GET, @Info);

        //MaxM: Apparently i can't set the filename here, so if DownloadCount is greater than zero
        //      we are forced to rename the file correctly otherwise it will be overwritten
        if (rDownload_Count = 1)
        then RenameFile(Info.FileName, rDownload_Path+rDownload_FileName+'-0'+rDownload_Ext);

        {Call method to make source acquire and create file}
        rc := Owner.TwainProc(AppInfo, @Structure, DG_IMAGE,
          DAT_IMAGEFILEXFER, MSG_GET, nil);

        if (rDownload_Count > 0)
        then RenameFile(Info.FileName, rDownload_Path+rDownload_FileName+
                                    '-'+IntToStr(rDownload_Count)+rDownload_Ext);

        //Set correct Filename for Events
        Info.FileName:= StrToStr255(rDownload_Path+rDownload_FileName+
                                    '-'+IntToStr(rDownload_Count)+rDownload_Ext);
      end {case ttmFile};
      {Memory buffer transfers}
      ttmMemory:
      begin
        {Prepare for memory transference}
        rc := PrepareMemXfer({%H-}ImageHandle, {%H-}PixelType);
        {If the image was sucessfully prepared to be transfered, it's}
        {now time to transfer it}
        if rc = TWRC_SUCCESS then rc := TransferImageMemory(ImageHandle,
          PixelType);
      end;
    end;

    {Twain call to transfer image return}
    case rc of
      {Transfer sucessfully done}
      TWRC_XFERDONE:
        case TransferMode of
          {Native transfer sucessfull}
          ttmNative: ReadNative(hNative, rDownload_Cancelled);
          {File transfer sucessfull}
          ttmFile: ReadFile(Info.FileName, Info.Format, rDownload_Cancelled);
          {Memory transfer sucessfull}
          ttmMemory: ReadMemory(ImageHandle, rDownload_Cancelled);
        end {case TransferMode, TWRC_XFERDONE};
      {User cancelled the transfers}
      TWRC_CANCEL:
      begin
        {Acknowledge end of transfer}
        rDownload_Done := True;
        rDownload_Cancelled := True;
        {Call event, if avaliable}
        if Assigned(Owner.OnAcquireCancel) then
          Owner.OnAcquireCancel(Owner, Index)
      end
      else {Unknown return or error}
        if Assigned(Owner.OnAcquireError) then
          Owner.OnAcquireError(Owner, Index, Rc, GetReturnStatus)
    end;

    {Check if there are pending transfers}
    if not(rDownload_Done) then
      rDownload_Done := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL,
                        DAT_PENDINGXFERS, MSG_ENDXFER, @PendingXfers) <> TWRC_SUCCESS) or
                        (PendingXfers.Count = 0);

    {If user has cancelled}
    if not(rDownload_Done) and rDownload_Cancelled then
      rDownload_Done := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL,
                        DAT_PENDINGXFERS, MSG_RESET, @PendingXfers) = TWRC_SUCCESS);

  until rDownload_Done;

  {Disable source}
  Enabled := False;

  {All documents have been transfered}
  if Assigned(Owner.OnTransferComplete) then
    Owner.OnTransferComplete(Owner, Index, rDownload_Cancelled);

  Owner.RefreshVirtualWindow;
end;

{Converts from TWain TW_UINT16 to TTwainFormat}
function TwainToTTwainFormat(Value: TW_UINT16): TTwainFormat;
begin
  Case Value of
    TWFF_TIFF     : Result := tfTIFF;
    TWFF_PICT     : Result := tfPict;
    TWFF_BMP      : Result := tfBMP;
    TWFF_XBM      : Result := tfXBM;
    TWFF_JFIF     : Result := tfJPEG;
    TWFF_FPX      : Result := tfFPX;
    TWFF_TIFFMULTI: Result := tfTIFFMulti;
    TWFF_PNG      : Result := tfPNG;
    TWFF_SPIFF    : Result := tfSPIFF;
    TWFF_EXIF     : Result := tfEXIF;
    else            Result := tfUnknown;
  end {case Value of}
end;

{Reads the file image}
procedure TTwainSource.ReadFile(Name: TW_STR255; Format: TW_UINT16;
  var Cancel: Boolean);
begin
  {Call event, if set}
  if Assigned(Owner.OnSourceFileTransfer) then
    Owner.OnSourceFileTransfer(Self, Index, Name, TwainToTTwainFormat(Format),
      Cancel);
end;

{Call event for memory image}
procedure TTwainSource.ReadMemory(imageHandle: HBitmap; var Cancel: Boolean);
begin
  Owner.DoTwainAcquire(Owner, Index, imageHandle, Cancel);
end;

{Reads a native image}
procedure TTwainSource.ReadNative(nativeHandle: TW_UINT32; var Cancel: Boolean);
var
  DibInfo: PBITMAPINFO;
  ColorTableSize: Integer;
  lpBits: PAnsiChar;
  DC: HDC;
  BitmapHandle: HBitmap;
  NeedBitmap:Boolean;

begin
  NeedBitmap:=False;
  Owner.DoTwainAcquireNative(Owner, Index, nativeHandle, NeedBitmap, Cancel);
  if NeedBitmap then
  begin
    {Get image information pointer and size}
    DibInfo := GlobalLock(nativeHandle);
    ColorTableSize := (DibNumColors(DibInfo) * SizeOf(RGBQUAD));

    {Get data memory position}
    lpBits := PAnsiChar(DibInfo);
    //{$IFDEF FPC}
    Inc(lpBits, DibInfo.bmiHeader.biSize);
    //{$ELSE}
    //DELPHI BUG - due to wrong PChar definition
    //Inc(lpBits, DibInfo.bmiHeader.biSize div 2);
    //{$ENDIF}
    Inc(lpBits, ColorTableSize);
    //lpBits := PAnsiChar(DibInfo^.bmiColors);

    {Creates the bitmap}
    DC := GetDC(Owner.VirtualWindow);

    { #note -oMaxM : In this way an RGB bitmap is always created even when we have a grayscale or a palette }
    BitmapHandle := CreateDIBitmap(DC, DibInfo.bmiHeader, CBM_INIT, lpBits, DibInfo^, DIB_RGB_COLORS);

    ReleaseDC(Owner.VirtualWindow, DC);

    Owner.DoTwainAcquire(Owner, Index, BitmapHandle, Cancel);

    {Free bitmap}
    //DeleteObject(BitmapHandle); //MaxM: we can not Free the Bitmap Handle
    GlobalUnlock(nativeHandle);
    GlobalFree(nativeHandle);
  end;
end;

{Setup file transfer}
function TTwainSource.SetupFileTransfer(APath, AFileName, AExt: String;
  Format: TTwainFormat): Boolean;
var
  FileTransferInfo: TW_SETUPFILEXFER;
begin
  {Source must be loaded to set things}
  if (Loaded) then
  begin
    {Prepare structure}
    FileTransferInfo.FileName := StrToStr255({$IFDEF UNICODE}RawByteString{$ENDIF}(APath+AFileName+AExt));
    FileTransferInfo.Format := FormatToTwain[Format];

    rDownload_Path:= APath;
    rDownload_FileName:= AFileName;
    rDownload_Ext:= AExt;

    {Call method}
    Result := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL,
      DAT_SETUPFILEXFER, MSG_SET, @FileTransferInfo) = TWRC_SUCCESS);
  end
  else Result := FALSE;  {Could not set file transfer with source unloaded}
end;

function TTwainSource.Download(UserInterface: TW_USERINTERFACE; APath, AFileName, AExt: String;
                               Format: TTwainFormat): Integer;
begin
  if (APath = '') or (APath[Length(APath)]='\')
  then rDownload_Path:= APath
  else rDownload_Path:= APath+'\';

  Result:= 0;
  if not(ForceDirectories(rDownload_Path)) then exit;

  rDownload_FileName:= AFileName;
  rDownload_Ext:= AExt;
  rDownload_Count:= -1;
  rDownload_Done:= False;
  rDownload_Cancelled:= False;
  rDownloaded:= False;

  { #note -oMaxM : We absolutely must not set the file name when we are in ttmNative mode otherwise it will be deleted }
  if (TransferMode = ttmFile)
  then SetupFileTransfer(rDownload_Path, rDownload_FileName, rDownload_Ext, Format);

  EnableSource(UserInterface);

  //Everything is asynchronous and we have to wait here until it finishes
  repeat
    CheckSynchronize(10);
  until rDownload_Done or
       (Assigned(Owner.rOnProcessMessages) and not(Owner.rOnProcessMessages(Owner, Index)));

  //If not Cancelled return the number of files really Downloaded
  if not(rDownload_Cancelled) then Inc(rDownload_Count);

  rDownloaded:= (rDownload_Count > 0);

  if rDownloaded
  then begin
         //MaxM: We have changed the first filename adding -0 otherwise it will be overwritten
         //      See note on TransferImages method
         if (TransferMode = ttmFile)
         then RenameFile(rDownload_Path+rDownload_FileName+'-0'+rDownload_Ext,
                    rDownload_Path+rDownload_FileName+rDownload_Ext);
         Result:= rDownload_Count;
       end
  else Result:= 0;
end;

function TTwainSource.Download(UserInterface: TW_USERINTERFACE; APath, AFileName, AExt: String;
                               Format: TTwainFormat;
                               var DownloadedFiles: TStringArray; UseRelativePath: Boolean): Integer;
var
   i: Integer;

begin
  Result:= 0;

  Result:= Download(UserInterface, APath, AFileName, AExt, Format);
  if (Result > 0 ) then
  begin
    SetLength(DownloadedFiles, Result);

    if UseRelativePath
    then begin
           DownloadedFiles[0]:= rDownload_FileName+rDownload_Ext;
           for i:=1 to Result-1 do
             DownloadedFiles[i]:= rDownload_FileName+'-'+IntToStr(i)+rDownload_Ext;
         end
    else begin
           DownloadedFiles[0]:= rDownload_Path+rDownload_FileName+rDownload_Ext;
           for i:=1 to Result-1 do
             DownloadedFiles[i]:= rDownload_Path+rDownload_FileName+'-'+IntToStr(i)+rDownload_Ext;
         end;
  end;
end;

{Set the number of images that the application wants to receive}
function TTwainSource.SetCapXferCount(Value: SmallInt): TCapabilityRet;
begin
  {Call method to set the value}
  Result := SetOneValue(CAP_XFERCOUNT, TWTY_UINT16, @Value);
end;

{Returns the number of images that the source will return}
function TTwainSource.GetCapXferCount(var Return: SmallInt;
  Mode: TRetrieveCap): TCapabilityRet;
var
  {Will hold the capability information}
  ItemType: TW_UINT16;
  Value   : String;
begin
  {Call method to return information}
  Result := GetOneValue(CAP_XFERCOUNT, {%H-}ItemType, {%H-}Value, Mode);
  {Item type must be of TW_UINT16}
  if (Result = crSuccess) and (ItemType <> TWTY_INT16) then
    Result := crUnsupported;
  {If everything gone ok, fill result}
  if Result = crSuccess then Return := StrToIntDef(Value, -1);
end;

{Set the unit measure}
function TTwainSource.SetICapUnits(Value: TTwainUnit): TCapabilityRet;
//npeter
//the TTwainUnit is byte!!!
//so we have to convert it to TW_UINT16
//before this fix I was not able to set this capability
//on a HP driver
const Transfer: Array[TTwainUnit] of TW_UINT16 =
       (TWUN_INCHES, TWUN_CENTIMETERS, TWUN_PICAS, TWUN_POINTS, TWUN_TWIPS, TWUN_PIXELS, TWUN_INCHES);
var
  iValue: TW_UINT16;
begin
  ivalue:=Transfer[Value];
  Result := SetOneValue(ICAP_UNITS, TWTY_UINT16, @iValue);
end;

{Convert from Twain to TTwainPixelFlavor}
function TwainToTTwainPixelFlavor(Value: TW_UINT16): TTwainPixelFlavor;
begin
  {Test the value to make the convertion}
  case Value of
    TWPF_CHOCOLATE: Result := tpfChocolate;
    TWPF_VANILLA  : Result := tpfVanilla;
  else Result := tpfUnknown;
  end {case Value}
end;

{Convert from Twain to TTwainUnit}
function TwainToTTwainUnit(Value: TW_UINT16): TTwainUnit;
begin
  {Test the value to make the convertion}
  case Value of
    TWUN_INCHES     : Result := tuInches;
    TWUN_CENTIMETERS: Result := tuCentimeters;
    TWUN_PICAS      : Result := tuPicas;
    TWUN_POINTS     : Result := tuPoints;
    TWUN_TWIPS      : Result := tuTwips;
    TWUN_PIXELS     : Result := tuPixels;
  else Result := tuUnknown;
  end {case Value}
end;

{Retrieve the unit measure for all quantities}
function TTwainSource.GetICapUnits(var Return: TTwainUnit;
  var Supported: TTwainUnitSet; Mode: TRetrieveCap): TCapabilityRet;
var
  ItemType: TW_UINT16;
  List    : TStringArray;
  Current, i,
  Default : Integer;
begin
  {Call method to get result}
  Result := GetEnumerationValue(ICAP_UNITS, {%H-}ItemType, {%H-}List, {%H-}Current, {%H-}Default,
    Mode);
  if ItemType <> TWTY_UINT16 then Result := crUnsupported;

  {If it was sucessfull, return values}
  if Result = crSuccess then
  begin
    {Make list}
    for i := Low(List) to High(List) do
      Include(Supported, TwainToTTwainUnit(StrToIntDef(List[i], -1)));
    {Return values depending on the mode}
    if Mode = rcGetDefault then
      Return := TwainToTTwainUnit(StrToIntDef(List[Default], -1))
    else
      Return := TwainToTTwainUnit(StrToIntDef(List[Current], -1));
  end {if Result = crSuccess}

end;

{Retrieve the pixel flavor values}
function TTwainSource.GetIPixelFlavor(var Return: TTwainPixelFlavor;
  var Supported: TTwainPixelFlavorSet; Mode: TRetrieveCap): TCapabilityRet;
var
  ItemType: TW_UINT16;
  List    : TStringArray;
  Current, i,
  Default : Integer;
begin
  {Call method to get result}
  Result := GetEnumerationValue(ICAP_PIXELFLAVOR, {%H-}ItemType, {%H-}List, {%H-}Current,
    {%H-}Default, Mode);
  if ItemType <> TWTY_UINT16 then Result := crUnsupported;

  {If it was sucessfull, return values}
  if Result = crSuccess then
  begin
    {Make list}
    for i := Low(List) to High(List) do
      Include(Supported, TwainToTTwainPixelFlavor(StrToIntDef(List[i], -1)));
    {Return values depending on the mode}
    if Mode = rcGetDefault then
      Return := TwainToTTwainPixelFlavor(StrToIntDef(List[Default], -1))
    else
      Return := TwainToTTwainPixelFlavor(StrToIntDef(List[Current], -1));
  end {if Result = crSuccess}
end;

function TTwainSource.SetIPixelFlavor(Value: TTwainPixelFlavor): TCapabilityRet;
//npeter
//the TTwainPixelFlavor is byte!!!
//so we have to convert it to TW_UINT16
//before this fix I was not able to set this capability
//on a HP driver
const Transfer: array [TTwainPixelFlavor] of TW_UINT16 = (TWPF_CHOCOLATE,TWPF_VANILLA,TWPF_CHOCOLATE);
var iValue: TW_UINT16;
begin
  iValue:=Transfer[value];
  Result := SetOneValue(ICAP_PIXELFLAVOR, TWTY_UINT16, @iValue);
end;

{Convert from Twain to TTwainPixelType}
function TwainToTTwainPixelType(Value: TW_UINT16): TTwainPixelType;
begin
  {Test the value to make the convertion}
  case Value of
    TWPT_BW         : Result := tbdBw;
    TWPT_GRAY       : Result := tbdGray;
    TWPT_RGB        : Result := tbdRgb;
    TWPT_BGR        : Result := tbdBgr;
    TWPT_PALETTE    : Result := tbdPalette;
    TWPT_CMY        : Result := tbdCmy;
    TWPT_CMYK       : Result := tbdCmyk;
    TWPT_YUV        : Result := tbdYuv;
    TWPT_YUVK       : Result := tbdYuvk;
    TWPT_CIEXYZ     : Result := tbdCieXYZ;
  else Result := tbdUnknown;
  end {case Value}
end;

{Returns pixel type values}
function TTwainSource.GetIPixelType(var Current, Default: TTwainPixelType; var Values: TTwainPixelTypeSet): TCapabilityRet;
var
  EnumV: pTW_ENUMERATION;
  Item: pTW_UINT16;
  CurItem: Integer;
  Container: TW_UINT16;
  MemHandle: HGLOBAL;
  curVal: TTwainPixelType;

begin
  Values:=[];
  Current:=tbdUnknown;
  Default:=tbdUnknown;
  MemHandle:=0;
  Result := GetCapabilityRec(ICAP_PIXELTYPE, MemHandle, rcGet, {%H-}Container);

  if (Result = crSuccess) and (Container <> TWON_ENUMERATION) then
  begin
    GlobalFree(MemHandle);
    Result := crUnsupported;
    Exit;
  end;

  {If result was sucessfull and memory was allocated}
  if (Result = crSuccess) then
  begin
    {Obtain structure pointer}
    EnumV := GlobalLock(MemHandle);

    //Paper Sizes must be TWTY_UINT16
    if (EnumV^.ItemType=TWTY_UINT16) then
    begin
      {Prepare to list items}
      Item := @EnumV^.ItemList[0];

      {Fill the Set with items}
      for CurItem:=0 to (EnumV^.NumItems-1) do
      begin
        curVal:=TwainToTTwainPixelType(Item^);

        if (CurItem=EnumV^.CurrentIndex) then Current:=curVal;
        if (CurItem=EnumV^.DefaultIndex) then Default:=curVal;

        Values :=Values+[curVal];

        inc(Item);
      end;
    end;

    {Unlock memory and unallocate}
    GlobalUnlock(MemHandle);
    GlobalFree(MemHandle);
  end;
end;

{Set the pixel type value}
function TTwainSource.SetIPixelType(Value: TTwainPixelType): TCapabilityRet;
//npeter
//the TTwainPixelType is byte!!!
//so we have to convert it to TW_UINT16
//before this fix occasionally I was not able to set this capability
//on a HP driver
var ivalue: smallint;
begin
 ivalue:=ord(value);
 Result := SetOneValue(ICAP_PIXELTYPE, TWTY_UINT16, @iValue);
end;

{Returns bitdepth values}
function TTwainSource.GetIBitDepth(var Current, Default: Integer; var Values: TArrayInteger): TCapabilityRet;
begin
 Result := GetEnumerationValue(ICAP_BITDEPTH, Values, Current, Default, rcGet);
end;

{Set current bitdepth value}
function TTwainSource.SetIBitDepth(Value: Word): TCapabilityRet;
begin
  Result := SetOneValue(ICAP_BITDEPTH, TWTY_UINT16, @Value);
end;

{Returns physical width}
function TTwainSource.GetIPhysicalWidth(var Return: Single; Mode: TRetrieveCap): TCapabilityRet;
begin
  Result :=GetOneValue(ICAP_PHYSICALWIDTH, Return, Mode);
end;

{Returns physical height}
function TTwainSource.GetIPhysicalHeight(var Return: Single; Mode: TRetrieveCap): TCapabilityRet;
begin
  Result :=GetOneValue(ICAP_PHYSICALHEIGHT, Return, Mode);
end;

{Returns a resolution}
function TTwainSource.GetResolution(Capability: TW_UINT16; var Current, Default: Single;
  var Values: TTwainResolution): TCapabilityRet;
var
  Handle: HGlobal;
  EnumV:  pTW_ENUMERATION;
  ArrayV: pTW_ARRAY;
  Container: TW_UINT16;
  Item: pTW_FIX32;
  i   : Integer;

begin
  {Obtain handle to data from this capability}
  Result := GetCapabilityRec(Capability, {%H-}Handle, rcGet, {%H-}Container);
  if Result = crSuccess then
  begin
    {Obtain data}

    Case Container of
    TWON_ENUMERATION: begin
        EnumV := GlobalLock(Handle);
        if (EnumV^.ItemType = TWTY_FIX32) then
        begin
          {Set array size and pointer to the first item}
          Item := @EnumV^.ItemList[0];
          SetLength(Values, EnumV^.NumItems);

          {Fill array}
          for i := 0 to EnumV^.NumItems-1 do
          begin
           {Fill array with the item}
           Values[i] := Fix32ToFloat(Item^);
           {Move to next item}
           inc(Item);
          end;

          if (EnumV^.NumItems>0) then //MaxM: You can never have too much paranoia
          begin
            Default :=Values[EnumV^.DefaultIndex];
            Current :=Values[EnumV^.CurrentIndex];
          end;
        end
        else Result := crUnsupported;

        {Free data}
        GlobalUnlock(Handle);
        GlobalFree(Handle);
    end;
    TWON_ARRAY: begin
        //MaxM: The old code used indiscriminately between Enum and Array. :-o
        //      A very serious mistake given the different alignment of the two types.
        ArrayV := GlobalLock(Handle);
        if (ArrayV^.ItemType = TWTY_FIX32) then
        begin
          {Set array size and pointer to the first item}
          Item := @ArrayV^.ItemList[0];
          SetLength(Values, ArrayV^.NumItems);

          {Fill array}
          for i := 0 to ArrayV^.NumItems-1 do
          begin
            {Fill array with the item}
            Values[i] := Fix32ToFloat(Item^);
            {Move to next item}
            inc(Item);
          end;

          //npeter
          //I got nice AV with an old Mustek scanner which uses TWON_ARRAY
          //i return 0 [in Current,Default] in this case (may be not the best solution, but not AV at least :-)
          //MaxM: is better to return the first element
          if (EnumV^.NumItems>0) then //MaxM: You can never have too much paranoia
          begin
            Default :=Values[0];
            Current :=Values[0];
          end;
        end
        else Result := crUnsupported;

        {Free data}
        GlobalUnlock(Handle);
        GlobalFree(Handle);
    end;
    else Result:=crUnsupported;
    end;
  end;
end;

{Sets X resolution}
function TTwainSource.SetIXResolution(Value: Single): TCapabilityRet;
begin
  Result := SetOneValue(ICAP_XRESOLUTION, Value);
end;

{Sets Y resolution}
function TTwainSource.SetIYResolution(Value: Single): TCapabilityRet;
begin
  Result := SetOneValue(ICAP_YRESOLUTION, Value);
end;

{Returns X resolution}
function TTwainSource.GetIXResolution(var Current, Default: Single; var Values: TTwainResolution): TCapabilityRet;
begin
  Result := GetResolution(ICAP_XRESOLUTION, Current, Default, Values);
end;

{Returns Y resolution}
function TTwainSource.GetIYResolution(var Current, Default: Single; var Values: TTwainResolution): TCapabilityRet;
begin
  Result := GetResolution(ICAP_YRESOLUTION, Current, Default, Values);
end;

{Returns if user interface is controllable}
function TTwainSource.GetUIControllable(var Return: Boolean): TCapabilityRet;
begin
  Result :=GetOneValue(CAP_UICONTROLLABLE, Return, rcGet);
end;

{Returns if feeder is loaded}
function TTwainSource.GetFeederLoaded(var Return: Boolean): TCapabilityRet;
begin
  Result :=GetOneValue(CAP_FEEDERLOADED, Return, rcGet);
end;

{Returns if feeder is enabled}
function TTwainSource.GetFeederEnabled(var Return: Boolean): TCapabilityRet;
begin
  Result := GetOneValue(CAP_FEEDERENABLED, Return, rcGet);
end;

{Set if feeder is enabled}
function TTwainSource.SetFeederEnabled(Value: Boolean): TCapabilityRet;
begin
  Result := SetOneValue(CAP_FEEDERENABLED, Value);

  if Value then
  begin
    //MaxM: to really use feeder we must also set autofeed or autoscan, but only
    // for one of them since setting autoscan also sets autofeed
    if CapabilityCanSet(CAP_AUTOSCAN)
    then Result := SetOneValue(CAP_AUTOSCAN, Value)
    else if CapabilityCanSet(CAP_AUTOFEED)
         then Result := SetOneValue(CAP_AUTOFEED, Value);
  end;
end;

{Returns if autofeed is enabled}
function TTwainSource.GetAutoFeed(var Return: Boolean): TCapabilityRet;
begin
  Result :=GetOneValue(CAP_AUTOFEED, Return, rcGet);
end;

{Set if autofeed is enabled}
function TTwainSource.SetAutoFeed(Value: Boolean): TCapabilityRet;
begin
  Result := SetOneValue(CAP_AUTOFEED, Value);
end;

function TTwainSource.GetAutoScan(var Return: Boolean): TCapabilityRet;
begin
  Result :=GetOneValue(CAP_AUTOSCAN, Return, rcGet);
end;

function TTwainSource.SetAutoScan(Value: Boolean): TCapabilityRet;
begin
  Result := SetOneValue(CAP_AUTOSCAN, Value);
end;

function TTwainSource.GetContrast(var Return: Single): TCapabilityRet;
begin
  Result :=GetOneValue(ICAP_CONTRAST, Return, rcGet);
end;

function TTwainSource.SetContrast(Value: Single): TCapabilityRet;
begin
  Result := SetOneValue(ICAP_CONTRAST, Value);
end;

function TTwainSource.GetBrightness(var Return: Single): TCapabilityRet;
begin
  Result :=GetOneValue(ICAP_BRIGHTNESS, Return, rcGet);
end;

function TTwainSource.SetBrightness(Value: Single): TCapabilityRet;
begin
 Result := SetOneValue(ICAP_BRIGHTNESS, Value);
end;

function TTwainSource.GetParamsCapabilities(var Value: TTwainParamsCapabilities): Boolean;
var
   capRet:TCapabilityRet;
   i: Integer;

begin
 Result :=False;
 with Value do
 try
    ResolutionArray:= nil;
    BitDepthArray:= nil;

    PaperFeedingSet:= GetPaperFeeding;
    capRet :=GetPaperSizeSet(PaperSizeCurrent, PaperSizeDefault, PaperSizeSet);
    capRet :=GetIBitDepth(BitDepthCurrent, BitDepthDefault, BitDepthArray);
    BitDepthArraySize :=Length(BitDepthArray);
    capRet :=GetIPixelType(PixelTypeCurrent, PixelTypeDefault, PixelType);
    capRet :=GetIXResolution(ResolutionCurrent, ResolutionDefault, ResolutionArray);
    ResolutionArraySize :=Length(ResolutionArray);

    //In theory the minimum is the first value and the maximum is the last,
    //  but you never know a little paranoia doesn't hurt
    ResolutionMin:= MaxInt;
    ResolutionMax:= 0;
    for i:=0 to ResolutionArraySize-1 do
    begin
      if (ResolutionArray[i] < ResolutionMin) then ResolutionMin:= ResolutionArray[i];
      if (ResolutionArray[i] > ResolutionMax) then ResolutionMax:= ResolutionArray[i];
    end;

    Result :=True;

 except
    ResolutionArray:= nil;
    BitDepthArray:= nil;
    Result :=False;
 end;
end;

function TTwainSource.SetAutoBorderDetection(Value: Boolean): TCapabilityRet;
begin
  Result := SetOneValue(ICAP_AUTOMATICBORDERDETECTION, Value);
end;

function TTwainSource.SetAutoRotate(Value: Boolean): TCapabilityRet;
begin
  Result := SetOneValue(ICAP_AUTOMATICROTATE, Value);
end;

function TTwainSource.SetAutoDeskew(Value: Boolean): TCapabilityRet;
begin
  Result := SetOneValue(ICAP_AUTOMATICDESKEW, Value);
end;

function TTwainSource.SetAutoSize(Value: TTwainAutoSize): TCapabilityRet;
const Transfer: array [TTwainAutoSize] of TW_UINT16 = (TWAS_NONE, TWAS_AUTO, TWAS_CURRENT);
var iValue: TW_UINT16;
begin
  iValue:=Transfer[value];
  Result := SetOneValue(ICAP_AUTOSIZE, TWTY_UINT16, @iValue);
end;

{Used with property PendingXfers}
function TTwainSource.GetPendingXfers: TW_INT16;
var
  PendingXfers: TW_PENDINGXFERS;
begin
  if Loaded and Enabled then
  begin
    {Call method to retrieve}
    if Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_PENDINGXFERS,
      MSG_GET, @PendingXfers) = TWRC_SUCCESS then
      Result := PendingXfers.Count
    else Result := ERROR_INT16; {Some error ocurred while calling message}
  end
  else Result := ERROR_INT16;  {Source not loaded/enabled}
end;

//npeter: 2004.01.12
//sets the acquired area
function TTwainSource.SetImagelayoutFrame(const fLeft, fTop, fRight,
  fBottom: double): TCapabilityRet;
var ImageLayout: TW_IMAGELAYOUT;
begin
 if not Loaded then
  begin
   Result := crInvalidState;  {In case the source is not loaded}
   exit;
  end;

 fillchar({%H-}ImageLayout,sizeof(TW_IMAGELAYOUT),0);
 with ImageLayout.Frame do
  begin
   Left:=FloatToFIX32(fLeft);
   Top:=FloatToFIX32(fTop);
   Right:=FloatToFIX32(fRight);
   Bottom:=FloatToFIX32(fBottom);
  end;
 {Call method and store return}
 Result := ResultToCapabilityRec(Owner.TwainProc(AppInfo, @Structure,
      DG_IMAGE, DAT_IMAGELAYOUT, MSG_SET, @ImageLayout));
end;

function TTwainSource.GetIndicators(var Value: Boolean): TCapabilityRet;
begin
  Result :=GetOneValue(CAP_INDICATORS, Value, rcGet);
end;

//npeter: 2004.01.12
//enable/disable progress indicators
function TTwainSource.SetIndicators(Value: Boolean): TCapabilityRet;
begin
  Result := SetOneValue(CAP_INDICATORS, Value);
end;

end.

