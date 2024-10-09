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

    function getTwain: TDelphiTwain;

    property Twain: TDelphiTwain read getTwain;

    procedure TwainAcquireNative(Sender: TObject; const Index: Integer;
                                nativeHandle: TW_UINT32; var Cancel: Boolean);

    procedure TwainAcquire(Sender: TObject; const Index: Integer;
                           Image: TBitmap; var Cancel: Boolean);

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
   TwainSource:TTwainSource;

begin
    //Show Select Form and User Select a device
    Twain.SelectedSource;

    if Assigned(Twain.SelectedSource) then
    begin
      aPath:=ExtractFilePath(ParamStr(0))+'test_0.bmp';
      if FileExists(aPath)
      then DeleteFile(aPath);

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
  TwainSource:TTwainSource;
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
     WriteBitmapToFile('test_0.bmp', nativeHandle);
     ImageHolder.Picture.Bitmap.LoadFromFile('test_0.bmp');
     Cancel := True;//Only want one image
  except
  end;
end;

procedure TFormTwainDemo.TwainAcquire(Sender: TObject; const Index: Integer;
  Image: TBitmap; var Cancel: Boolean);
begin
  try
     ImageHolder.Picture.Bitmap.Assign(Image);
     ImageHolder.Picture.Bitmap.SaveToFile('test_0.bmp');
     Cancel := True;//Only want one image
  except
  end;
end;

end.

