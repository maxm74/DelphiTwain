unit DelphiTwainDemo_Form;

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Twain, DelphiTwain, DelphiTwainUtils, DelphiTwain_VCL;

type

  { TFormTwainDemo }

  TFormTwainDemo = class(TForm)
    btAcquire: TButton;
    btSelect: TButton;
    cbNativeCapture: TCheckBox;
    cbModalCapture: TCheckBox;
    cbPaperSize: TComboBox;
    cbPixelType: TComboBox;
    cbResolution: TComboBox;
    cbShowUI: TCheckBox;
    ImageHolder: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    procedure btSelectClick(Sender: TObject);
    procedure btAcquireClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    rTwain:TDelphiTwain;
    capRet: TCapabilityRet;
    PaperSizeSet: TTwainPaperSizeSet;
    PaperSizeCurrent,
    PaperSizeDefault: TTwainPaperSize;
    PixelType:TTwainPixelTypeSet;
    PixelTypeCurrent,
    PixelTypeDefault:TTwainPixelType;
    ResolutionDefault,
    ResolutionCurrent:Single;
    ResolutionArray: TTwainResolution;

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
    if Assigned(Twain.SelectedSource) then
    begin
      TwainSource :=Twain.SelectedSource;

      aPath:=ExtractFilePath(ParamStr(0))+'test_0.bmp';
      if FileExists(aPath)
      then DeleteFile(aPath);

      TwainSource.Loaded := True;

      //Fill Params with new values
      if (cbPaperSize.ItemIndex>-1)
      then PaperSizeCurrent:=TTwainPaperSize(Integer(cbPaperSize.Items.Objects[cbPaperSize.ItemIndex]));

      if (cbPixelType.ItemIndex>-1)
      then PixelTypeCurrent:=TTwainPixelType(Integer(cbPixelType.Items.Objects[cbPixelType.ItemIndex]));

      if (cbResolution.ItemIndex>-1)
      then ResolutionCurrent:=ResolutionArray[Integer(cbResolution.Items.Objects[cbResolution.ItemIndex])];

      capRet :=TwainSource.SetPaperSize(PaperSizeCurrent);
      capRet :=TwainSource.SetIPixelType(PixelTypeCurrent);
      capRet :=TwainSource.SetIXResolution(ResolutionCurrent);
      capRet :=TwainSource.SetIYResolution(ResolutionCurrent);

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
end;

procedure TFormTwainDemo.FormDestroy(Sender: TObject);
begin
  if rTwain<>nil then rTwain.Free;
end;

procedure TFormTwainDemo.btSelectClick(Sender: TObject);
var
  TwainSource:TTwainSource;
  paperI: TTwainPaperSize;
  pixelI:TTwainPixelType;
  i, cbSelected: Integer;

begin
    btAcquire.Enabled :=False;
    cbPaperSize.Clear;
    cbPixelType.Clear;
    cbResolution.Clear;

    //Load source manager
    Twain.SourceManagerLoaded :=True;

    //Allow user to select source
    Twain.SelectSource;
    if Assigned(Twain.SelectedSource) then
    begin
      TwainSource :=Twain.SelectedSource;

      TwainSource.Loaded:=True;

      //Get Params
      capRet :=TwainSource.GetPaperSizeSet(PaperSizeCurrent, PaperSizeDefault, PaperSizeSet);
      capRet :=TwainSource.GetIPixelType(PixelTypeCurrent, PixelTypeDefault, PixelType);
      capRet :=TwainSource.GetIXResolution(ResolutionCurrent, ResolutionDefault, ResolutionArray);

      //Fill List of Papers
      cbSelected :=0;
      cbPaperSize.Items.AddObject('Full Scanner size', TObject(Integer(tpsNONE)));
      for paperI in PaperSizeSet do
      begin
        if (paperI<>tpsNONE) and (paperI<>tpsMAXSIZE)
        then cbPaperSize.Items.AddObject(PaperSizesTwain[paperI].name+
               ' ('+FloatToStrF(PaperSizesTwain[paperI].w, ffFixed, 15, 2)+' x '+
                    FloatToStrF(PaperSizesTwain[paperI].h, ffFixed, 15, 2)+')',
               TObject(Integer(paperI)));

        if (paperI=PaperSizeCurrent) then cbSelected :=cbPaperSize.Items.Count-1;
      end;
      cbPaperSize.ItemIndex:=cbSelected;

      //Fill List of Pixel Type
      cbSelected :=0;
      for pixelI in PixelType do
      begin
        cbPixelType.Items.AddObject(TwainPixelTypes[pixelI], TObject(Integer(pixelI)));

        if (pixelI=PixelTypeCurrent) then cbSelected :=cbPixelType.Items.Count-1;
      end;
      cbPixelType.ItemIndex:=cbSelected;

      //Fill List of Resolution (Y Resolution=X Resolution)
      cbSelected :=0;
      for i:=Low(ResolutionArray) to High(ResolutionArray) do
      begin
        cbResolution.Items.AddObject(FloatToStr(ResolutionArray[i]), TObject(Integer(i)));

        if (ResolutionArray[i] = ResolutionCurrent) then cbSelected :=cbResolution.Items.Count-1;
      end;
      cbResolution.ItemIndex:=cbSelected;

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

