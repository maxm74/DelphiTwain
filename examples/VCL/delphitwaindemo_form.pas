unit DelphiTwainDemo_Form;

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Twain, DelphiTwain, DelphiTwainUtils, DelphiTwainTypes, DelphiTwain_VCL,
  DelphiTwain_SettingsForm;

type

  { TFormTwainDemo }

  TFormTwainDemo = class(TForm)
    btAcquire: TButton;
    btSelect: TButton;
    cbNativeCapture: TCheckBox;
    cbModalCapture: TCheckBox;
    cbShowUI: TCheckBox;
    ImageHolder: TImage;
    Panel1: TPanel;
    procedure btSelectClick(Sender: TObject);
    procedure btAcquireClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    rTwain:TDelphiTwain;
    capRet: TCapabilityRet;
    TwainCap: TTwainParamsCapabilities;
    TwainParams: TTwainParams;
    TwainSource: TTwainSource;

    rDownloaded: Boolean;
    rDownload_Count: Integer;
    rDownload_Path,
    rDownload_Ext,
    rDownload_FileName: String;


    function getTwain: TDelphiTwain;

    property Twain: TDelphiTwain read getTwain;

    procedure TwainAcquireNative(Sender: TObject; const Index: Integer;
                                nativeHandle: TW_UINT32; var Cancel: Boolean);

    procedure TwainAcquire(Sender: TObject; const Index: Integer;
                           Image: TBitmap; var Cancel: Boolean);

    procedure TransferComplete(Sender: TObject; const Index: Integer; const Canceled: Boolean);

    function Download(APath, AFileName, AExt: String): Integer; overload;
    function Download(APath, AFileName, AExt: String; var DownloadedFiles: TStringArray): Integer; overload;

  end;

var
  FormTwainDemo: TFormTwainDemo;

implementation


{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}


{ TFormTwainDemo }

procedure TFormTwainDemo.btAcquireClick(Sender: TObject);
var
   aPath:String;
   res: Integer;

begin
    //Show Select Form and User Select a device
    Twain.SelectedSource;

    if Assigned(Twain.SelectedSource) then
    begin
     (* aPath:=ExtractFilePath(ParamStr(0))+'test_0.bmp';
      if FileExists(aPath)
      then DeleteFile(aPath);
      *)

      TwainSource :=Twain.SelectedSource;
      TwainSource.Loaded := True;

      try
        //Select Scanner Setting to use
        if TTwainSettingsSource.Execute(True, TwainCap, TwainParams) then
        begin
          with TwainParams do
          begin
            capRet :=TwainSource.SetPaperFeeding(PaperFeed);
            capRet :=TwainSource.SetDuplexEnabled(False);
            capRet :=TwainSource.SetPaperSize(PaperSize);
            capRet :=TwainSource.SetIPixelType(PixelType);

            capRet :=TwainSource.SetIXResolution(Resolution);
            capRet :=TwainSource.SetIYResolution(Resolution);
            capRet :=TwainSource.SetContrast(Contrast);
            capRet :=TwainSource.SetBrightness(Brightness);
          end;
          capRet :=TwainSource.SetIndicators(True);

          (*
          //Load source, select transference method and enable
          TwainSource.TransferMode:=ttmNative;

          if cbNativeCapture.Checked
          then begin
                 Twain.OnTwainAcquireNative:=TwainAcquireNative;
                 Twain.OnTwainAcquire:=nil;
               end
          else begin
                 Twain.OnTwainAcquireNative:=nil;
                 Twain.OnTwainAcquire:=TwainAcquire;
               end;

          TwainSource.EnableSource(cbShowUI.Checked, cbModalCapture.Checked, Application.ActiveFormHandle);
          *)
          res:= Download(ExtractFilePath(ParamStr(0)), 'test', '.bmp');

          if (res > 0)
          then begin
                 MessageDlg('Downloaded '+IntToStr(res)+' Files', mtInformation, [mbOk], 0);
                 ImageHolder.Picture.Bitmap.LoadFromFile('test.bmp')
               end
          else MessageDlg('NO Files Downloaded', mtError, [mbOk], 0);
        end;
      finally
        TwainSettingsSource.Free; TwainSettingsSource:= Nil;
      end;
    end;
end;

procedure TFormTwainDemo.FormDestroy(Sender: TObject);
begin
  if rTwain<>nil then rTwain.Free;
end;

procedure TFormTwainDemo.btSelectClick(Sender: TObject);
var
  bitCurrent: Integer;
  paperCurrent: TTwainPaperSize;
  pixelCurrent:TTwainPixelType;
  resolutionCurrent:Single;
  i, cbSelected: Integer;

begin
    btAcquire.Enabled :=False;

    //Load source manager
    Twain.SourceManagerLoaded :=True;

    //Allow user to select source
    Twain.SelectSource;
    if Assigned(Twain.SelectedSource) then
    begin
      TwainSource :=Twain.SelectedSource;

      TwainSource.Loaded:=True;

      TwainCap.PaperFeedingSet:=TwainSource.GetPaperFeeding;
      capRet :=TwainSource.GetPaperSizeSet(paperCurrent, TwainCap.PaperSizeDefault, TwainCap.PaperSizeSet);
      capRet :=TwainSource.GetIBitDepth(bitCurrent, TwainCap.BitDepthDefault, TwainCap.BitDepthArray);
      TwainCap.BitDepthArraySize :=Length(TwainCap.BitDepthArray);
      capRet :=TwainSource.GetIPixelType(pixelCurrent, TwainCap.PixelTypeDefault, TwainCap.PixelType);
      capRet :=TwainSource.GetIXResolution(resolutionCurrent, TwainCap.ResolutionDefault, TwainCap.ResolutionArray);
      TwainCap.ResolutionArraySize :=Length(TwainCap.ResolutionArray);

      btAcquire.Enabled :=True;
    end;
end;

function TFormTwainDemo.getTwain: TDelphiTwain;
begin
  //Create Twain
  if (rTwain = nil) then
  begin
    rTwain := TDelphiTwain.Create;
    //Load Twain Library dynamically
    if not(rTwain.LoadLibrary) then ShowMessage('Twain is not installed.');
  end;

  Result :=rTwain;
end;

procedure TFormTwainDemo.TwainAcquireNative(Sender: TObject; const Index: Integer;
  nativeHandle: TW_UINT32; var Cancel: Boolean);
begin
  try
    if (rDownload_Count = 0)
    then WriteBitmapToFile(rDownload_Path+rDownload_FileName+rDownload_Ext, nativeHandle)
    else WriteBitmapToFile(rDownload_Path+rDownload_FileName+
                           '-'+IntToStr(rDownload_Count)+rDownload_Ext, nativeHandle);
    inc(rDownload_Count);

    Cancel := False;
  except
  end;
end;

procedure TFormTwainDemo.TwainAcquire(Sender: TObject; const Index: Integer;
  Image: TBitmap; var Cancel: Boolean);
var
   Pict: TPicture;

begin
  try
    Pict:= TPicture.Create;
    Pict.Assign(Image);

    if (rDownload_Count = 0)
    then Pict.SaveToFile(rDownload_Path+rDownload_FileName+rDownload_Ext)
    else Pict.SaveToFile(rDownload_Path+rDownload_FileName+
                         '-'+IntToStr(rDownload_Count)+rDownload_Ext);
    inc(rDownload_Count);

    (* ImageHolder.Picture.Bitmap.Assign(Image);
     ImageHolder.Picture.Bitmap.SaveToFile('test_0.bmp'); *)

     Cancel := False;
  finally
    Pict.Free;
  end;
end;

procedure TFormTwainDemo.TransferComplete(Sender: TObject; const Index: Integer; const Canceled: Boolean);
begin
  rDownloaded:= True;
end;

function TFormTwainDemo.Download(APath, AFileName, AExt: String): Integer;
begin
  if (APath = '') or CharInSet(APath[Length(APath)], AllowDirectorySeparators)
  then rDownload_Path:= APath
  else rDownload_Path:= APath+DirectorySeparator;

  if not(ForceDirectories(rDownload_Path)) then exit;

  rDownload_FileName:= AFileName;
  rDownload_Ext:= AExt;
  rDownload_Count:= 0;
  rDownloaded:= False;

  //Load source, select transference method and enable


  if cbNativeCapture.Checked
  then begin
         TwainSource.TransferMode:= ttmNative;
         Twain.OnTwainAcquireNative:=TwainAcquireNative;
         Twain.OnTwainAcquire:=nil;
       end
  else begin
         TwainSource.TransferMode:= ttmFile;
         TwainSource.SetupFileTransfer('test.bmp', tfBMP);
         Twain.OnTwainAcquireNative:=nil;
         Twain.OnTwainAcquire:=TwainAcquire;
       end;
  Twain.OnTransferComplete:= TransferComplete;

  TwainSource.EnableSource(cbShowUI.Checked, cbModalCapture.Checked, Application.ActiveFormHandle);

  repeat
    CheckSynchronize(10);
    Application.ProcessMessages;
  until rDownloaded;

  rDownloaded:= (rDownload_Count > 0);

  if rDownloaded
  then Result:= rDownload_Count
  else Result:= 0;
end;

function TFormTwainDemo.Download(APath, AFileName, AExt: String;
  var DownloadedFiles: TStringArray): Integer;
begin

end;

end.

