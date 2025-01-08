unit DelphiTwain_VCL;

{$I DelphiTwain.inc}

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

interface

//{$DEFINE USE_CENTRALIZED_WIN}

uses
  Windows, SysUtils, Classes, Forms, ExtCtrls, Graphics,
  {$IFDEF FPC}interfacebase,{$ENDIF}
  Twain, DelphiTwain;

type
  TOnTwainAcquire = procedure(Sender: TObject; const Index: Integer; Image:
    TBitmap; var Cancel: Boolean) of object;
  TOnAcquireProgress = procedure(Sender: TObject; const Index: Integer;
    const Image: HBitmap; const Current, Total: Integer) of object;

  { TDelphiTwain }

  TDelphiTwain = class(TCustomDelphiTwain)
  private
    fMessagesTimer: TTimer;
    fOnTwainAcquire: TOnTwainAcquire;
    fOnAcquireProgress: TOnAcquireProgress;

  protected
   {$IFDEF USE_CENTRALIZED_WIN}
    {$IFNDEF FPC}
    function WndFunc(var Message: TMessage): Boolean;
    {$ENDIF}
    procedure DoCreateVirtualWindow; override;
    procedure DoDestroyVirtualWindow; override;
   {$ENDIF}
    procedure DoCreateTimer; override;
    procedure DoDestroyTimer; override;
    procedure MessageTimer_Enable; override;
    procedure MessageTimer_Disable; override;
    function CustomSelectSource: Integer; override;
    function CustomGetParentWindow: TW_HANDLE; override;

    procedure DoTwainAcquireNative(Sender: TObject; const Index: Integer;
                                   nativeHandle: TW_UINT32; var NeedBitmap, Cancel: Boolean); override;
    procedure DoTwainAcquire(Sender: TObject; const Index: Integer;
                             imageHandle:HBitmap; var Cancel: Boolean); override;
    procedure DoAcquireProgress(Sender: TObject; const Index: Integer;
                                const imageHandle: HBitmap; const Current, Total: Integer); override;

  public
    {Image acquired}
    property OnTwainAcquire: TOnTwainAcquire read fOnTwainAcquire write fOnTwainAcquire;
    {Acquire progress, for memory transfers}
    property OnAcquireProgress: TOnAcquireProgress read fOnAcquireProgress write fOnAcquireProgress;
  end;

implementation

uses DelphiTwainTypes, DelphiTwain_SelectForm, Controls;

{$IFDEF USE_CENTRALIZED_WIN}

{$IFDEF FPC}
var
  xTwainList: TList = nil;
  xAppWndCallback: WNDPROC = nil;//WNDPROC = nil;

function AppWndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;
var
  I: Integer;
  xMess: TMessage;
begin
  if Assigned(xTwainList) and (xTwainList.Count > 0) then begin
    xMess.msg := uMsg;
    xMess.lParam := lParam;
    xMess.wParam := wParam;
    xMess.Result := 0;

    for I := 0 to xTwainList.Count-1 do
      TDelphiTwain(xTwainList[I]).WndProc(xMess);
  end;
  Result := CallWindowProc(xAppWndCallback,Ahwnd, uMsg, WParam, LParam);
end;
{$ENDIF}

{$ENDIF} //USE_CENTRALIZED_WIN

{ TDelphiTwain }

function TDelphiTwain.CustomGetParentWindow: TW_HANDLE;
begin
  if IsConsole then
    Result := 0
  else if not IsLibrary then
    Result := {$IF DEFINED(FPC) OR DEFINED(DELPHI_7_DOWN)}GetActiveWindow{$ELSE}Application.ActiveFormHandle{$IFEND}
  else
    Result := GetActiveWindow;//GetForegroundWindow;
end;

function TDelphiTwain.CustomSelectSource: Integer;
var
  newSelectedInfo: TTwainDeviceInfo;

begin
  Result := -1;
  if SourceCount = 0 then begin
    Exit;
  end;
  try
     if TTwainSelectSource.Execute('', '', (*@RefreshList*)nil, Self, nil, newSelectedInfo)
     then Result:= TwainSelectSource.SelectedIndex;

  finally
    TwainSelectSource.Free; TwainSelectSource:= Nil;
  end;
end;

procedure TDelphiTwain.DoAcquireProgress(Sender: TObject; const Index: Integer;
                                         const imageHandle: HBitmap; const Current, Total: Integer);
begin
  if Assigned(fOnAcquireProgress) then
    fOnAcquireProgress(Self, Index, imageHandle, Current, Total);
end;

procedure TDelphiTwain.DoTwainAcquireNative(Sender: TObject; const Index: Integer;
                                           nativeHandle: TW_UINT32; var NeedBitmap, Cancel: Boolean);
begin
  //MaxM: No need to create HBitmap if unused
  NeedBitmap:=Assigned(fOnTwainAcquire);
  inherited DoTwainAcquireNative(Sender, Index, nativeHandle, NeedBitmap, Cancel);
end;

procedure TDelphiTwain.DoTwainAcquire(Sender: TObject; const Index: Integer;
                                      imageHandle: HBitmap; var Cancel: Boolean);
var BitmapObj: TBitmap;
begin
  if Assigned(fOnTwainAcquire) then
  begin
    BitmapObj := TBitmap.Create;
    try
      BitmapObj.Handle := imageHandle;
      fOnTwainAcquire(Sender, Index, BitmapObj, Cancel);
    finally
      BitmapObj.Free;
    end;
  end;
end;

procedure TDelphiTwain.MessageTimer_Disable;
begin
  if Assigned(fMessagesTimer) then
    fMessagesTimer.Enabled := False;
end;

procedure TDelphiTwain.MessageTimer_Enable;
begin
  if Assigned(fMessagesTimer) then
    fMessagesTimer.Enabled := True;
end;

{$IFDEF USE_CENTRALIZED_WIN}

{$IFNDEF FPC}
function TDelphiTwain.WndFunc(var Message: TMessage): Boolean;
var
  i    : Integer;
  xMsg  : TMsg;
begin
  Result := False;
  with Message do begin
  {Tests for the message}
      {Try to obtain the current object pointer}
      if Assigned(Self) then
        {If there are sources loaded, we need to verify}
        {this message}
       if (Self.SourcesLoaded > 0) then
        begin
          {Convert parameters to a TMsg}
          xMsg := MakeMsg(Handle, Msg, wParam, lParam);//MakeMsg(Handle, Msg, wParam, lParam);
          {Tell about this message}
          FOR i := 0 TO Self.SourceCount - 1 DO
            if ((Self.Source[i].Loaded) and (Self.Source[i].Enabled)) then
              if Self.Source[i].ProcessMessage(xMsg) then
              begin
                {Case this was a message from the source, there is}
                {no need for the default procedure to process}
                Result := 0;
                WndFunc := True;
                Exit;
              end;

        end; {if (Twain.SourcesLoaded > 0)}
  end;
end;
{$ENDIF}

procedure TDelphiTwain.DoCreateVirtualWindow;
begin
  if IsLibrary or IsConsole
  then inherited DoCreateVirtualWindow
  else begin
    {$IFDEF FPC}
    if Assigned(Application.MainForm) and (Application.MainForm.Visible) then
      fVirtualWindow := Application.MainFormHandle
    else
      fVirtualWindow := WidgetSet.AppHandle;

    if not Assigned(xTwainList) then
      xTwainList := TList.Create;
    xTwainList.Add(Self);
    if not Assigned(xAppWndCallback) then begin
      xAppWndCallback := {%H-}{Windows.WNDPROC}Pointer(SetWindowLongPtr(fVirtualWindow,GWL_WNDPROC,{%H-}NativeInt(@AppWndCallback)));
    end;

    {$ELSE}
    fVirtualWindow := Application.Handle;//Application.Handle;
    Application.HookMainWindow(WndFunc);
    {$ENDIF}
  end;
end;

procedure TDelphiTwain.DoDestroyVirtualWindow;
begin
  if IsLibrary or IsConsole
  then inherited DoDestroyVirtualWindow
  else begin
    {$IFDEF FPC}
    xTwainList.Remove(Self);
    if xTwainList.Count = 0 then begin
      FreeAndNil(xTwainList);
    end;
    {$ELSE}
    Application.UnhookMainWindow(WndFunc);
    {$ENDIF}
  end;
end;

{$ENDIF} //USE_CENTRALIZED_WIN

procedure TDelphiTwain.DoCreateTimer;
begin
  fMessagesTimer := TTimer.Create(nil);
  fMessagesTimer.Enabled := False;
  fMessagesTimer.Interval := 100;
  fMessagesTimer.OnTimer := DoMessagesTimer;
end;

procedure TDelphiTwain.DoDestroyTimer;
begin
  FreeAndNil(fMessagesTimer);
end;


end.
