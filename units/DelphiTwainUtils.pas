(******************************************************************************
*                FreePascal \ Delphi Twain Implementation                     *
*                                                                             *
*  FILE: DelphiTwainUtils.pas                                                 *
*                                                                             *
*  VERSION:     2.3.1                                                         *
*                                                                             *
*  DESCRIPTION:                                                               *
*    GENERAL METHODS USED BY TWAIN DELPHI                                     *
*    This unit contains general methods used by Delphi Twain component.       *
*    Some of the methods bellow aren't directly related to Twain, but         *
*    are pieces needed to implement the component.                            *
*                                                                             *
*******************************************************************************
*                                                                             *
*  (c) 2001 Gustavo Daud, 2025 Massimo Magnano                                *
*                                                                             *
*  See changelog.txt for Change Log                                           *
*                                                                             *
*******************************************************************************)
unit DelphiTwainUtils;

{$INCLUDE DELPHITWAIN.INC}

interface

uses
  Windows, Classes, Twain;

type
  {Kinds of directories to be obtained with GetCustomDirectory}
  TDirectoryKind = (dkWindows, dkSystem, dkCurrent, dkApplication, dkTemp);

{$IFNDEF FPC}
  {$IF (CompilerVersion >= 23)}
  DTNativeUInt = NativeUInt;
  {$ELSE}
  DTNativeUInt = Cardinal;
  {$IFEND}
{$ELSE}//FPC
  DTNativeUInt = NativeUInt;
{$ENDIF}
  PDTNativeUInt = ^DTNativeUInt;

  {Class to store a list of pointers}
  TPointerList = class
  private
    {Stores pointer to the allocated data}
    Data: Pointer;
    {Contains number of additional items allocated every time}
    {it needs more data to store}
    fAdditionalBlock: Integer;
    {Contains the number of items in the list}
    fCount: Integer;
    {Contains number of allocated items}
    fAllocated: Integer;
    {Allocate/deallocate memory to have enough memory}
    {to hold the new number of items}
    procedure SetAllocated(const Value: Integer);
    {Sets the AdditionalBlock property}
    procedure SetAdditionalBlock(const Value: Integer);
    {Set the number of items in the list}
    procedure SetCount(const Value: Integer);
    function GetItem(Index: Integer): Pointer;
    procedure PutItem(Index: Integer; const Value: Pointer);
  public
    {Add a new item}
    procedure Add(Value: Pointer);
    {Clear all the items in the list}
    procedure Clear;
    {Object being created or destroyed}
    constructor Create;
    destructor Destroy; override;
    {Returns/sets an item value}
    property Item[Index: Integer]: Pointer read GetItem write PutItem; default;
    {Returns the number of items}
    property Count: Integer read fCount write SetCount;
    {Number of allocated items}
    property Allocated: Integer read fAllocated write SetAllocated;
    {Additional items to alloc when it needs more memory}
    property AdditionalBlock: Integer read fAdditionalBlock write
      SetAdditionalBlock;
  end;

{Returns custom Microsoft Windows� directories}
function GetCustomDirectory(const DirectoryKind: TDirectoryKind): String;
{Returns the last error string from Microsoft Windows�}
function GetLastErrorText: String;
{Returns if the directory exists}
function DirectoryExists(const Directory: String): Boolean;
{Returns if the file exists}
function FileExists(const FilePath: String): Boolean;
{Extracts the file directory part}
function ExtractDirectory(const FilePath: String): String;
{Convert from integer to string}
{Convert from twain Fix32 to Single}
function Fix32ToFloat(Value: TW_FIX32): Single;
{Convert from Single to Fix32}
function FloatToFix32(floater: Single): TW_FIX32;

{Returns the number of colors in the DIB}
function DibNumColors(pv: Pointer): Word;

//Write a Windows Bitmap Native Handle to File
function WriteBitmapToFile(FileName:String; hDIB: TW_UINT32): Boolean; overload;
//Write a Windows HBitmap Handle to File (32bit RGB)
function WriteBitmapToFile(FileName:String; hBmp:HBITMAP; hDC:HDC): Boolean; overload;

implementation

{Convert from Single to Fix32}
function FloatToFix32 (floater: Single): TW_FIX32;
//Chad Berchek new code:
var
  i32: Cardinal;
begin
  {$IFOPT R+}{$DEFINE RANGEON}{$ELSE}{$UNDEF RANGEON}{$ENDIF} {save initial switch state}
  {$R-}
  i32 := Round(floater * 65536.0);
  Result.Whole := i32 shr 16;
  Result.Frac := i32 and $ffff;
  {$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}
end;
{function FloatToFix32 (floater: Single): TW_FIX32;
//old code
var
  fracpart : Single;
begin
  //Obtain numerical part by truncating the float number
  Result.Whole := trunc(floater);
  //Obtain fracional part by subtracting float number by
  //numerical part. Also we make sure the number is not
  //negative by multipling by -1 if it is negative
  fracpart := floater - result.Whole;
  if fracpart < 0 then fracpart := fracpart * -1;
  //Multiply by 10 until there is no fracional part any longer
  while FracPart - trunc(FracPart) <> 0 do fracpart := fracpart * 10;
  //Return fracional part
  Result.Frac := trunc(fracpart);
end;}

{Convert from twain Fix32 to Single}
function Fix32ToFloat(Value: TW_FIX32): Single;
begin
  {$IFOPT R+}{$DEFINE RANGEON}{$ELSE}{$UNDEF RANGEON}{$ENDIF} {save initial switch state}
  {$R-}
  Result := Value.Whole + (Value.Frac / 65536.0);
  {$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}
end;

{Returns the last position for any of the characters in the parameter}
function LastPosition(const Text, characters: String): Integer;
var
  x, y: Integer;  {For loop variables}
begin
  Result := Length(Text);  {Initial result}

  {Search each character in the text}
  FOR x := 1 TO Length(Text) DO
  begin
    {Test for each character}
    FOR y := 1 TO Length(characters) DO
      if Text[x] = characters[y] then
        Result := x;
  end {for x}
end;

{Extracts the file directory}
function ExtractDirectory(const FilePath: String): String;
begin
  {Searches for the last \ or : characters}
  {ex: c:\windows\system32\yfile.ext or c:autoexec.bat}
  Result := Copy(FilePath, 1, LastPosition(FilePath, '\:'));
end;

{Returns if the file exists}
function FileExists(const FilePath: String): Boolean;
var
  FindData  : TWin32FindData;
  FindHandle: THandle;
begin
  {Searches for the file}
  FindHandle := FindFirstFile(PChar(FilePath), {%H-}FindData);
  Result := (FindHandle <> INVALID_HANDLE_VALUE);
  {In case it found, closes the FindFirstFile handle}
  if Result then FindClose(FindHandle);
end;

{Returns if the directory exists}
function DirectoryExists(const Directory: String): Boolean;
var
  Attr: DWORD;
begin
  {Calls GetFileAttributes to verify}
  Attr := GetFileAttributes(PChar(Directory));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;

{Makes an language identifier using the two ids}
function MAKELANGID(p, s: WORD): DWORD;
begin
  Result := (s shl 10) or p;
end;

{Returns the last error string from Microsoft Windows�}
function GetLastErrorText: String;
var
  Buffer: Array[Byte] of WideChar;
  Len   : DWORD;
begin
  {Calls format message to translate from the error code ID to}
  {a text understandable error}
  Len := Windows.FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, GetLastError,
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), Buffer, sizeof(Buffer), nil);
  {Remove this chars from the ending of the result}
  {$WARNINGS OFF}
  while (Len > 0) and (Char(Buffer[Len - 1]) in [#0..#32, '.']) do Dec(Len);
  {$WARNINGS ON}
  {Fills result}
  SetString(Result, Buffer, Len);
end;

{Includes a trailing backslash in the end of the directory; if necessary}
procedure IncludeTrailingBackslash(var Directory: String);
begin
  {If there isn't already a backslash, add one}
  if Directory[Length(Directory)] <> '\' then
    Directory := Directory + '\'
end;

{Returns custom Microsoft Windows� directories}
function GetCustomDirectory(const DirectoryKind: TDirectoryKind): String;
const
  {Default maximum size for directories}
  DEF_DIRLEN = MAX_PATH;

  {Calls appropriate method and returns necessary size}
  function CallDirectoryMethod(Buffer: Pointer; Size: UINT): UINT;
  begin
    Result:=0;
    {Test the directory needed by the parameter}
    case DirectoryKind of
      {Windows directory}
      dkWindows: Result := Windows.GetWindowsDirectory(Buffer, Size);
      {System directory}
      dkSystem : Result := Windows.GetSystemDirectory(Buffer, Size);
      {Current directory}
      dkCurrent: Result := Windows.GetCurrentDirectory(Size, Buffer);
      {Application directory}
      dkApplication: Result := Windows.GetModuleFileName(0, Buffer, Size);
      {Temp directory}
      dkTemp   : Result := Windows.GetTempPath(Size, Buffer);
    end {case}
  end;

var
  DirectoryLen: UINT;
begin
  {Set length of the resulting buffer to MAX_PATH to try to hold}
  {windows directory}
  SetLength(Result, DEF_DIRLEN + 1);
  {Tries to obtain the windows directory and stores the size}
  DirectoryLen := CallDirectoryMethod(@Result[1], DEF_DIRLEN);

  {In case it was not enough to hold windows directory, enlarge}
  if DirectoryLen > DEF_DIRLEN then
  begin
    {Try again, now with the right size}
    SetLength(Result, DirectoryLen + 1);
    CallDirectoryMethod(@Result[1], DirectoryLen);
  end
  else {Otherwise, adjust the result to excluded unused data}
    SetLength(Result, DirectoryLen);

  {In case the user searched for the application directory}
  {extracts just the directory part}
  if DirectoryKind = dkApplication then
    Result := ExtractDirectory(Result);
  {Add a trailing backslash to end of the directory name}
  IncludeTrailingBackslash(Result);
end;

{Returns the number of colors in the DIB}
function DibNumColors (pv: Pointer): Word;
var
  Bits: Integer;
  lpbi: PBITMAPINFOHEADER absolute pv;
  lpbc: PBITMAPCOREHEADER absolute pv;
begin
  //With the BITMAPINFO format headers, the size of the palette
  //is in biClrUsed, whereas in the BITMAPCORE - style headers, it
  //is dependent on the bits per pixel ( = 2 raised to the power of
  //bits/pixel).
  if (lpbi^.biSize <> sizeof(BITMAPCOREHEADER)) then
  begin
    if (lpbi^.biClrUsed <> 0) then
    begin
      result := lpbi^.biClrUsed;
      exit;
    end;
    Bits := lpbi^.biBitCount;
  end
  else
     Bits := lpbc^.bcBitCount;

  {Test bits to return}
  case (Bits) of
    1: Result := 2;
    4: Result := 16;
    8: Result := 256;
    else Result := 0;
  end {case};

end;

function WriteBitmapToFile(FileName:String; hDIB: TW_UINT32): Boolean;
var
   DibInfo: PBITMAPINFO;
   hdr: BITMAPFILEHEADER;
   fd: TFileStream;

begin
  Result :=False;
  DibInfo :=GlobalLock(hDIB);
  hdr.bfType :=$4D42;
  hdr.bfSize :=(sizeof(BITMAPFILEHEADER) + DibInfo^.bmiHeader.biSize + DibInfo^.bmiHeader.biClrUsed * sizeof(RGBQUAD) + DibInfo^.bmiHeader.biSizeImage);
  hdr.bfReserved1 :=0;
  hdr.bfReserved2 :=0;
  hdr.bfOffBits :=(sizeof(BITMAPFILEHEADER) + DibInfo^.bmiHeader.biSize + DibInfo^.bmiHeader.biClrUsed * sizeof(RGBQUAD));

  fd :=TFileStream.Create(FileName, fmCreate);
  // Write the file header
  fd.Write(hdr, SizeOf(hdr));
  // Write the DIB header and the bits
  fd.Write(DibInfo^, GlobalSize(hDIB));
  fd.Free;
  GlobalUnLock(hDIB);
  Result :=True;
end;

function WriteBitmapToFile(FileName:String; hBmp:HBITMAP; hDC:HDC): Boolean;
var
   hdr: BITMAPFILEHEADER;
   fd: TFileStream;
   handleBits: HGlobal;
   lpBits: PChar;
   DibInfo: TBITMAPINFO;

begin
  fd:=nil;
  Result :=False;
  FillChar(DibInfo, SizeOf(TBITMAPINFO), 0);
  DibInfo.bmiHeader.biSize :=SizeOf(DibInfo.bmiHeader);
  if (GetDIBits(hDC, hBmp, 0, 0, nil, DibInfo, DIB_RGB_COLORS) > 0) then
  try
    handleBits :=GlobalAlloc(GMEM_FIXED, DibInfo.bmiHeader.biSizeImage);
    lpBits :=GlobalLock(handleBits);

    // requesting a 32 bit image means that no stride/padding will be necessary,
    // although it always contains an (possibly unused) alpha channel
    DibInfo.bmiHeader.biBitCount :=32;
    DibInfo.bmiHeader.biCompression :=BI_RGB;  // no compression -> easier to use
    DibInfo.bmiHeader.biClrUsed :=0;  //no palette

    // correct the bottom-up ordering of lines (abs is in cstdblib and stdlib.h)
    DibInfo.bmiHeader.biHeight :=Abs(DibInfo.bmiHeader.biHeight);

    // Call GetDIBits a second time, this time to (format and) store the actual
    // bitmap data (the "pixels") in the buffer lpBits
    if (GetDIBits(hDC, hBmp, 0, DibInfo.bmiHeader.biHeight, lpBits, &DibInfo, DIB_RGB_COLORS) > 0) then
    begin
      hdr.bfType :=$4D42;
      hdr.bfSize :=(sizeof(BITMAPFILEHEADER) + DibInfo.bmiHeader.biSize (*+ DibInfo.bmiHeader.biClrUsed*sizeof(RGBQUAD)*) + DibInfo.bmiHeader.biSizeImage);
      hdr.bfReserved1 :=0;
      hdr.bfReserved2 :=0;
      hdr.bfOffBits :=(sizeof(BITMAPFILEHEADER) + DibInfo.bmiHeader.biSize (*+ DibInfo.bmiHeader.biClrUsed * sizeof(RGBQUAD)*));

      fd :=TFileStream.Create(FileName, fmCreate);
      // Write the file header
      fd.Write(hdr, SizeOf(hdr));
      // Write the DIB header
      fd.Write(DibInfo, sizeof(BITMAPINFOHEADER)(*+DibInfo.bmiHeader.biClrUsed * sizeof(RGBQUAD)*));
      // Write the bits
      fd.Write(lpBits^, DibInfo.bmiHeader.biSizeImage);

      Result :=True;
    end;
  finally
    GlobalUnlock(handleBits);
    GlobalFree(handleBits);
    fd.Free;
  end;
end;


{ TPointerList object implementation }

{Add a new item}
procedure TPointerList.Add(Value: Pointer);
begin
  {Increase number of items and update new item}
  Count := Count + 1;
  Item[Count - 1] := Value;
end;

{Clear all the items in the list}
procedure TPointerList.Clear;
begin
  {Set number of items to 0 and initialize again allocated items}
  Count := 0;
  Allocated := AdditionalBlock;
end;

{TPointerList being created}
constructor TPointerList.Create;
begin
  {Let ancestor receive the call}
  inherited Create;

  {Allocate a number of items}
  fAdditionalBlock := 10;
  fAllocated := fAdditionalBlock;
  GetMem(Data, (fAllocated * sizeof(Pointer)));
end;

{TPointerList being destroyed}
destructor TPointerList.Destroy;
begin
  {Deallocate data}
  FreeMem(Data, (fAllocated * sizeof(Pointer)));

  {Let ancestor receive and finish}
  inherited Destroy;
end;

{Returns an item from the list}
function TPointerList.GetItem(Index: Integer): Pointer;
begin
  {Check item bounds and return item}
  {$WARNINGS OFF}
  if Index in [0..Count - 1] then
    {%H-}DTNativeUInt(Result) := pDTNativeUInt({%H-}DTNativeUInt(Data) + (Index * sizeof(Pointer)))^
  else Result := nil; {Otherwise returns nil}
  {$WARNINGS ON}
end;

{Sets an item from the list}
procedure TPointerList.PutItem(Index: Integer; const Value: Pointer);
begin
  {Check item bounds and sets item}
  {$WARNINGS OFF}
  if Index in [0..Count - 1] then
    {%H-}pDTNativeUInt({%H-}DTNativeUInt(Data) + (Index * sizeof(Pointer)))^ := {%H-}DTNativeUInt(Value);
  {$WARNINGS ON}
end;

{Sets the AdditionalBlock property}
procedure TPointerList.SetAdditionalBlock(const Value: Integer);
begin
  {Value must be a positive number greater than 0}
  if (Value > 0) then
    fAdditionalBlock := Value;
end;

{Allocate/deallocate memory to have enough memory to hold}
{the new number of items}
procedure TPointerList.SetAllocated(const Value: Integer);
begin
  {Must be always greater than 0 the number of allocated items}
  {And it also should not be smaller than count}
  if (Value > 0) and (Value >= Count) then
  begin
    {Just realloc memory and update property variable}
    ReallocMem(Data, (Value * sizeof(Pointer)));
    fAllocated := Value;
  end {if (Value <> 0)}
end;

{Set the number of items in the list}
procedure TPointerList.SetCount(const Value: Integer);
begin
  {Value must be 0 or greater}
  if (Value >= 0) then
  begin
    {If there is no more memory to hold data, allocate some more}
    while (Value > fAllocated) do
      Allocated := Allocated + fAdditionalBlock;
    {Update property}
    fCount := Value;
  end {if (Value >= 0)}
end;

end.
