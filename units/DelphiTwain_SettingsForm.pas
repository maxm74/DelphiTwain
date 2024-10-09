(*******************************************************************************
**                                  Delphi Twain                              **
**                                                                            **
**          (c) 2024 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Twain Settings Form                                                      **
*******************************************************************************)
unit DelphiTwain_SettingsForm;

{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, StdCtrls, Spin, Twain, DelphiTwain, DelphiTwainTypes,
  ImgList
  {$ifndef fpc}, ImageList{$endif};

type
  { TTwainSettingsSource }

  TTwainSettingsSource = class(TForm)
    btCancel: TBitBtn;
    btOrientation: TSpeedButton;
    btRefresh: TBitBtn;
    btOk: TBitBtn;
    cbPixelType: TComboBox;
    cbPaperFeeding: TComboBox;
    cbResolution: TComboBox;
    cbPaperSize: TComboBox;
    cbBitDepth: TComboBox;
    cbUseNativeUI: TCheckBox;
    edBrightness: TSpinEdit;
    edContrast: TSpinEdit;
    imgList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    panelUI: TPanel;
    panelButtons: TPanel;
    trBrightness: TTrackBar;
    trContrast: TTrackBar;
    procedure edBrightnessChange(Sender: TObject);
    procedure edContrastChange(Sender: TObject);
    procedure trBrightnessChange(Sender: TObject);
    procedure trContrastChange(Sender: TObject);
  private
    Twain: TCustomDelphiTwain;
    TwainSource: TTwainSource;
    SelectedSourceIndex:Integer;

  public
     class function Execute(useScannerDefault: Boolean; TwainCap:TTwainParamsCapabilities;
                            var AParams:TTwainParams): Boolean;
  end;

var
  TwainSettingsSource: TTwainSettingsSource=nil;

implementation

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

{ TTwainSettingsSource }

procedure TTwainSettingsSource.trBrightnessChange(Sender: TObject);
begin
  edBrightness.Value:=trBrightness.Position;
end;

procedure TTwainSettingsSource.edBrightnessChange(Sender: TObject);
begin
  trBrightness.Position:=edBrightness.Value;
end;

procedure TTwainSettingsSource.edContrastChange(Sender: TObject);
begin
  trContrast.Position:=edContrast.Value;
end;

procedure TTwainSettingsSource.trContrastChange(Sender: TObject);
begin
  edContrast.Value:=trContrast.Position;
end;

class function TTwainSettingsSource.Execute(useScannerDefault: Boolean; TwainCap: TTwainParamsCapabilities;
                                            var AParams: TTwainParams): Boolean;
var
  paperI: TTwainPaperSize;
  pixelI:TTwainPixelType;
  i, cbSelected: Integer;

begin
  if (TwainSettingsSource=nil)
  then TwainSettingsSource :=TTwainSettingsSource.Create(nil);

  with TwainSettingsSource do
  begin
    cbPaperFeeding.Clear;
    if (pfFlatbed in TwainCap.PaperFeedingSet) then cbPaperFeeding.Items.AddObject('Flatbed', TObject(PtrUInt(pfFlatbed)));
    if (pfFeeder in TwainCap.PaperFeedingSet) then cbPaperFeeding.Items.AddObject('Feeder', TObject(PtrUInt(pfFeeder)));
    cbPaperFeeding.ItemIndex:=cbPaperFeeding.Items.IndexOfObject(TObject(PtrUInt(AParams.PaperFeed)));

    //Fill List of Papers
    cbPaperSize.Clear;
    cbSelected :=0;
    cbPaperSize.Items.AddObject('Full Scanner size', TObject(PtrUInt(tpsNONE)));
    for paperI in TwainCap.PaperSizeSet do
    begin
      if (paperI<>tpsNONE) and (paperI<>tpsMAXSIZE)
      then cbPaperSize.Items.AddObject(PaperSizesTwain[paperI].name+
             ' ('+FloatToStrF(PaperSizesTwain[paperI].w, ffFixed, 15, 2)+' x '+
                  FloatToStrF(PaperSizesTwain[paperI].h, ffFixed, 15, 2)+')',
             TObject(PtrUInt(paperI)));

      if useScannerDefault
      then begin if (paperI = TwainCap.PaperSizeDefault) then cbSelected :=cbPaperSize.Items.Count-1; end
      else begin if (paperI = AParams.PaperSize) then cbSelected :=cbPaperSize.Items.Count-1; end;
    end;
    cbPaperSize.ItemIndex:=cbSelected;

    //Fill List of Bit Depth
    cbBitDepth.Clear;
    cbSelected :=0;
    for i:=0 to TwainCap.BitDepthArraySize-1 do
    begin
      cbBitDepth.Items.AddObject(IntToStr(TwainCap.BitDepthArray[i])+' Bit', TObject(PtrUInt(TwainCap.BitDepthArray[i])));

      if useScannerDefault
      then begin if (TwainCap.BitDepthArray[i] = TwainCap.BitDepthDefault) then cbSelected :=cbBitDepth.Items.Count-1; end
      else begin if (TwainCap.BitDepthArray[i] = AParams.BitDepth) then cbSelected :=cbBitDepth.Items.Count-1; end;
    end;
    cbBitDepth.ItemIndex:=cbSelected;

    //Fill List of Pixel Type
    cbPixelType.Clear;
    cbSelected :=0;
    for pixelI in TwainCap.PixelType do
    begin
      cbPixelType.Items.AddObject(TwainPixelTypes[pixelI], TObject(PtrUInt(pixelI)));

      if useScannerDefault
      then begin if (pixelI = TwainCap.PixelTypeDefault) then cbSelected :=cbPixelType.Items.Count-1; end
      else begin if (pixelI = AParams.PixelType) then cbSelected :=cbPixelType.Items.Count-1; end;
    end;
    cbPixelType.ItemIndex:=cbSelected;

    //Fill List of Resolution (Y Resolution=X Resolution)
    cbResolution.Clear;
    cbSelected :=0;
    for i:=0 to TwainCap.ResolutionArraySize-1 do
    begin
      cbResolution.Items.AddObject(FloatToStr(TwainCap.ResolutionArray[i]), TObject(PtrUInt(i)));

      if useScannerDefault
      then begin if (TwainCap.ResolutionArray[i] = TwainCap.ResolutionDefault) then cbSelected :=cbResolution.Items.Count-1; end
      else begin if (TwainCap.ResolutionArray[i] = AParams.Resolution) then cbSelected :=cbResolution.Items.Count-1; end;
    end;
    cbResolution.ItemIndex:=cbSelected;

    { #todo -oMaxM 2 : is an Single or an Integer? }
    trContrast.Position:=Trunc(AParams.Contrast);
    edContrast.Value:=Trunc(AParams.Contrast);
    trBrightness.Position:=Trunc(AParams.Brightness);
    edBrightness.Value:=Trunc(AParams.Brightness);

    Result := (ShowModal=mrOk);

    if Result then
    begin
      //Fill AParams with new values
      if (cbPaperFeeding.ItemIndex>-1)
      then AParams.PaperFeed:=TTwainPaperFeeding(PtrUInt(cbPaperFeeding.Items.Objects[cbPaperFeeding.ItemIndex]));
      { #todo -oMaxM : else Predefined Value }

      if (cbPaperSize.ItemIndex>-1)
      then AParams.PaperSize:=TTwainPaperSize(PtrUInt(cbPaperSize.Items.Objects[cbPaperSize.ItemIndex]));

      if (cbBitDepth.ItemIndex>-1)
      then AParams.BitDepth:=PtrUInt(cbBitDepth.Items.Objects[cbBitDepth.ItemIndex]);

      if (cbPixelType.ItemIndex>-1)
      then AParams.PixelType:=TTwainPixelType(PtrUInt(cbPixelType.Items.Objects[cbPixelType.ItemIndex]));

      if (cbResolution.ItemIndex>-1)
      then AParams.Resolution:=TwainCap.ResolutionArray[PtrUInt(cbResolution.Items.Objects[cbResolution.ItemIndex])];

      AParams.Contrast:=edContrast.Value;
      AParams.Brightness:=edBrightness.Value;
    end;
  end;
end;

end.

