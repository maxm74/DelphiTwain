(*******************************************************************************
**                                  Delphi Twain                              **
**                                                                            **
**          (c) 2024 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Twain Source Select Form                                                 **
*******************************************************************************)
unit DelphiTwain_SelectForm;

{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, Twain, DelphiTwain, DelphiTwainTypes, StdCtrls;

type

  { TTwainSelectSource }

  TTwainSelectSource = class;
  TRefreshNotify = procedure(ASender:TTwainSelectSource) of object;

  TTwainSelectSource = class(TForm)
    btCancel: TBitBtn;
    btRefresh: TBitBtn;
    btOk: TBitBtn;
    lvSources: TListView;
    Panel1: TPanel;
    panelButtons: TPanel;
    procedure btRefreshClick(Sender: TObject);
  private
    Twain: TCustomDelphiTwain;
    countTwain_Source:Integer;
    rRefreshClick:TRefreshNotify;
    rScannerInfo: TTwainDeviceInfo;
    rSelectedIndex: Integer;
    MsgApp,
    MsgAdditionalList: String;

  public
     class function Execute(AMsgApp, AMsgAdditionalList: String; ARefreshClick: TRefreshNotify;
                            ATwain: TCustomDelphiTwain;
                            const addList: TArrayTW_IDENTITY; var AScannerInfo: TTwainDeviceInfo): Boolean;
     procedure FillList(const addList: TArrayTW_IDENTITY);

     property SelectedIndex: Integer read rSelectedIndex;
  end;

var
  TwainSelectSource: TTwainSelectSource=nil;

implementation

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

{ TTwainSelectSource }

procedure TTwainSelectSource.btRefreshClick(Sender: TObject);
begin
  if Assigned(rRefreshClick) then rRefreshClick(Self);
end;

procedure TTwainSelectSource.FillList(const addList: TArrayTW_IDENTITY);
var
   i,
   txtW,
   selIndex:Integer;
   curItem:TListItem;
   curSource:TTwainSource;

begin
  selIndex:=-1;
  countTwain_Source:=Twain.SourceCount;

  lvSources.Clear;
  for i:=0 to countTwain_Source-1 do
  begin
    curSource :=Twain.Source[i];
    curItem :=lvSources.Items.Add;

    //Add Name Colums and increase width if necessary (since MinWidth/AutoSize don't work as expected)
    curItem.Caption:=curSource.ProductName;
    txtW:= lvSources.Canvas.TextWidth(curSource.ProductName)+16;
    if (lvSources.Columns[0].Width < txtW) then lvSources.Columns[0].Width:= txtW;

    //Add Manufacturer
    curItem.SubItems.Add(curSource.Manufacturer);
    txtW:= lvSources.Canvas.TextWidth(curSource.Manufacturer)+16;
    if (lvSources.Columns[1].Width < txtW) then lvSources.Columns[1].Width:= txtW;

    //if is Current Selected Scanner set selIndex
    if not(rScannerInfo.FromAddList) and not(DeviceInfoDifferent(rScannerInfo, curSource.SourceIdentity^))
    then selIndex :=curItem.Index;
  end;

  if (addList <> nil) then
  for i:=Low(addList) to High(addList) do
  begin
    curItem :=lvSources.Items.Add;

    curItem.Caption:=addList[i].ProductName;
    txtW:= lvSources.Canvas.TextWidth(addList[i].ProductName)+16;
    if (lvSources.Columns[0].Width < txtW) then lvSources.Columns[0].Width:= txtW;

    curItem.SubItems.Add(addList[i].Manufacturer);
    txtW:= lvSources.Canvas.TextWidth(addList[i].Manufacturer)+16;
    if (lvSources.Columns[1].Width < txtW) then lvSources.Columns[1].Width:= txtW;

    curItem.SubItems.Add(MsgAdditionalList);
    txtW:= lvSources.Canvas.TextWidth(MsgAdditionalList)+16;
    if (lvSources.Columns[2].Width < txtW) then lvSources.Columns[2].Width:= txtW;

    if (rScannerInfo.FromAddList) and not(DeviceInfoDifferent(rScannerInfo, addList[i]))
    then selIndex :=curItem.Index;
  end;

  //Select Current Scanner
  if (selIndex>-1)
  then lvSources.ItemIndex :=selIndex
  else lvSources.ItemIndex :=0;
end;

class function TTwainSelectSource.Execute(AMsgApp, AMsgAdditionalList: String; ARefreshClick: TRefreshNotify;
                       ATwain: TCustomDelphiTwain;
                       const addList: TArrayTW_IDENTITY; var AScannerInfo: TTwainDeviceInfo): Boolean;
begin
  Result :=False;

  if (TwainSelectSource=nil)
  then TwainSelectSource :=TTwainSelectSource.Create(nil);

  with TwainSelectSource do
  begin
    rSelectedIndex:= -1;
    MsgApp:= AMsgApp;
    MsgAdditionalList:= AMsgAdditionalList;
    Twain:= ATwain;
    rScannerInfo:= AScannerInfo;
    FillList(addList);

    if (lvSources.Items.Count=0)
    then MessageDlg({$ifdef fpc}MsgApp,{$endif} 'No Twain Devices present...', mtError, [mbOk], 0)
    else begin
           rRefreshClick:= ARefreshClick;
           btRefresh.Visible:= Assigned(rRefreshClick);

           Result:= (ShowModal=mrOk);
           if Result then
           begin
             AScannerInfo.FromAddList:= (lvSources.ItemIndex >= countTwain_Source);
             if AScannerInfo.FromAddList
             then begin
                    rSelectedIndex:= lvSources.ItemIndex-countTwain_Source;
                    AScannerInfo.Manufacturer:= addList[rSelectedIndex].Manufacturer;
                    AScannerInfo.ProductFamily:= addList[rSelectedIndex].ProductFamily;
                    AScannerInfo.ProductName:= addList[rSelectedIndex].ProductName;
                  end
             else begin
                    rSelectedIndex:= lvSources.ItemIndex;
                    AScannerInfo.Manufacturer:= Twain.Source[rSelectedIndex].Manufacturer;
                    AScannerInfo.ProductFamily:= Twain.Source[rSelectedIndex].ProductFamily;
                    AScannerInfo.ProductName:= Twain.Source[rSelectedIndex].ProductName;
                  end
           end;
         end;
  end;
end;

end.

