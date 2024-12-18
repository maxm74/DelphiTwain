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

    function getTwain: TDelphiTwain;

    property Twain: TDelphiTwain read getTwain;

    function TwainProcessMessages(Sender: TObject; const Index: Integer): Boolean;

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
   res: Integer;
   rUserInterface: TW_USERINTERFACE;

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

          if cbNativeCapture.Checked
          then begin
                 TwainSource.TransferMode:=ttmNative;
                 Twain.OnTwainAcquireNative:=TwainAcquireNative;
                 Twain.OnTwainAcquire:=nil;
               end
          else begin
                 TwainSource.TransferMode:=ttmFile;
                 Twain.OnTwainAcquireNative:=nil;
                 Twain.OnTwainAcquire:=TwainAcquire;
               end;

          rUserInterface.hParent:= Application.ActiveFormHandle;
          rUserInterface.ModalUI:= cbModalCapture.Checked;
          rUserInterface.ShowUI:= cbShowUI.Checked;
          res:= TwainSource.Download(rUserInterface, ExtractFilePath(ParamStr(0)), 'test', '.bmp', tfBMP);

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

      btAcquire.Enabled:= TwainSource.GetParamsCapabilities(TwainCap);
    end;
end;

function TFormTwainDemo.getTwain: TDelphiTwain;
begin
  //Create Twain
  if (rTwain = nil) then
  begin
    rTwain := TDelphiTwain.Create;
    rTwain.OnProcessMessages:= TwainProcessMessages;
    //Load Twain Library dynamically
    if not(rTwain.LoadLibrary) then ShowMessage('Twain is not installed.');
  end;

  Result :=rTwain;
end;

function TFormTwainDemo.TwainProcessMessages(Sender: TObject; const Index: Integer): Boolean;
begin
  Application.ProcessMessages;
  Result:= True;
end;

procedure TFormTwainDemo.TwainAcquireNative(Sender: TObject; const Index: Integer;
  nativeHandle: TW_UINT32; var Cancel: Boolean);
begin
  with TCustomDelphiTwain(Sender).Source[Index] do
  try
    if (Download_Count = 0)
    then WriteBitmapToFile(Download_Path+Download_FileName+Download_Ext, nativeHandle)
    else WriteBitmapToFile(Download_Path+Download_FileName+
                           '-'+IntToStr(Download_Count)+Download_Ext, nativeHandle);

    Cancel := False;
  except
  end;
end;

procedure TFormTwainDemo.TwainAcquire(Sender: TObject; const Index: Integer;
  Image: TBitmap; var Cancel: Boolean);
begin
  with TCustomDelphiTwain(Sender).Source[Index] do
  try
   (* if (Download_Count = 0)
    then WriteBitmapToFile(Download_Path+Download_FileName+Download_Ext, nativeHandle)
    else WriteBitmapToFile(Download_Path+Download_FileName+
                           '-'+IntToStr(Download_Count)+Download_Ext, nativeHandle);
    *)
    Cancel := False;
  except
  end;
end;


end.

