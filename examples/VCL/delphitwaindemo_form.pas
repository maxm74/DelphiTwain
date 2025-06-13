(******************************************************************************
*                FreePascal \ Delphi Twain Implementation                     *
*                                                                             *
*  FILE: DelphiTwainDemo_Form.pas                                             *
*                                                                             *
*  VERSION:     2.3.1                                                         *
*                                                                             *
*  DESCRIPTION:                                                               *
*    Twain Demo Form.                                                         *
*    This is a Package demo not a full scanning application,                  *
*    see the DigIt project on my GitHub repository for that.                  *
*                                                                             *
*  Most Twain Scanners have a 32-bit driver, so if you compile this project   *
*  as a 64-bit project you will not be able to acquire an image.              *
*  In fact, Windows cannot load a 32-bit DLL inside a 64-bit application.     *
*  If you want to use a 32-bit scanner you MUST compile this project as a     *
*  32-bit project, if you want to use a 64-bit scanner you MUST compile it    *
*  as a 64-bit project.                                                       *
*                                                                             *
*  This is a limitation of the Operating System, not of the Package.          *
*                                                                             *
*  This demo project has both 32-bit and 64-bit Build Modes.                  *
*                                                                             *
*******************************************************************************
*                                                                             *
*  (c) 2025 Massimo Magnano                                                   *
*                                                                             *
*  See changelog.txt for Change Log                                           *
*                                                                             *
*******************************************************************************)
unit DelphiTwainDemo_Form;

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Twain, DelphiTwain, DelphiTwainUtils, DelphiTwainTypes, DelphiTwain_VCL,
  DelphiTwain_SettingsForm;

type

  { TFormTwainDemo }

  TFormTwainDemo = class(TForm)
    btAcquire: TButton;
    btBrowse: TButton;
    btSelect: TButton;
    cbNativeCapture: TCheckBox;
    cbModalCapture: TCheckBox;
    cbShowUI: TCheckBox;
    edPath: TEdit;
    ImageHolder: TImage;
    Label2: TLabel;
    lbSelected: TLabel;
    Panel1: TPanel;
    procedure btBrowseClick(Sender: TObject);
    procedure btSelectClick(Sender: TObject);
    procedure btAcquireClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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


{$ifdef fpc}
  {$R *.lfm}

function SelectPath(var ADir: String): Boolean;
var
   selDir: TSelectDirectoryDialog;

begin
  try
     selDir:= TSelectDirectoryDialog.Create(FormTwainDemo);
     Result:= selDir.Execute;
     if Result then ADir:= selDir.FileName;

  finally
    selDir.Free;
  end;
end;

{$else}
  {$R *.dfm}

uses FileCtrl;

function SelectPath(var ADir: String): Boolean;
begin
  Result:= SelectDirectory(ADir, [sdAllowCreate], 0);
end;
{$endif}


{ TFormTwainDemo }

procedure TFormTwainDemo.btAcquireClick(Sender: TObject);
var
   aPath:String;
   c: Integer;
   rUserInterface: TW_USERINTERFACE;
   test: Boolean;

begin
    //Show Select Form and User Select a device
    Twain.SelectedSource;

    if Assigned(Twain.SelectedSource) then
    begin
      aPath:= edPath.Text;

      TwainSource :=Twain.SelectedSource;
      TwainSource.Loaded := True;

      try
        //Select Scanner Setting to use
        if TTwainSettingsSource.Execute(True, TwainCap, TwainParams) then
        begin
          with TwainParams do
          begin
//            capRet :=TwainSource.SetOneValue(CAP_AUTOMATICSENSEMEDIUM, False);

            capRet :=TwainSource.SetPaperFeeding(PaperFeed);
            if (PaperFeed=pfFeeder) then
            begin
              capRet :=TwainSource.SetDuplexEnabled(False);
            end;
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
          c:= TwainSource.Download(rUserInterface, aPath, 'twain_demo', '.bmp', tfBMP);

          if (c > 0)
          then begin
                 MessageDlg('Downloaded '+IntToStr(c)+' Files on'#13#10+aPath, mtInformation, [mbOk], 0);
                 ImageHolder.Picture.Bitmap.LoadFromFile('twain_demo.bmp')
               end
          else MessageDlg('NO Files Downloaded', mtError, [mbOk], 0);
        end;
      finally
        TwainSettingsSource.Free; TwainSettingsSource:= Nil;
      end;
    end;
end;

procedure TFormTwainDemo.FormCreate(Sender: TObject);
begin
  edPath.Text:= ExtractFilePath(ParamStr(0));
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
  test: Boolean;

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

//      capRet :=TwainSource.GetAutoScan(test);
//      capRet :=TwainSource.GetAutoFeed(test);

    end;
end;

procedure TFormTwainDemo.btBrowseClick(Sender: TObject);
var
   ADir: String;

begin
  if SelectPath(ADir) then edPath.Text:= ADir;
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

