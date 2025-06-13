object FormTwainDemo: TFormTwainDemo
  Left = 234
  Top = 98
  Caption = 'FormTwainDemo'
  ClientHeight = 750
  ClientWidth = 707
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object ImageHolder: TImage
    Left = 0
    Top = 100
    Width = 707
    Height = 650
    Align = alClient
    Proportional = True
    Stretch = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 707
    Height = 100
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 705
    DesignSize = (
      707
      100)
    object Label2: TLabel
      Left = 128
      Top = 59
      Width = 27
      Height = 15
      Caption = 'Path:'
    end
    object lbSelected: TLabel
      Left = 128
      Top = 28
      Width = 3
      Height = 15
    end
    object btAcquire: TButton
      Left = 10
      Top = 48
      Width = 110
      Height = 40
      Caption = 'Acquire'
      Enabled = False
      TabOrder = 0
      OnClick = btAcquireClick
    end
    object btSelect: TButton
      Left = 10
      Top = 0
      Width = 110
      Height = 40
      Caption = 'Select Source'
      TabOrder = 1
      OnClick = btSelectClick
    end
    object cbNativeCapture: TCheckBox
      Left = 128
      Top = 0
      Width = 120
      Height = 24
      Caption = 'Native Capture'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object cbModalCapture: TCheckBox
      Left = 264
      Top = 0
      Width = 64
      Height = 24
      Caption = 'Modal'
      TabOrder = 3
    end
    object cbShowUI: TCheckBox
      Left = 352
      Top = 0
      Width = 71
      Height = 24
      Caption = 'ShowUI'
      TabOrder = 4
    end
    object edPath: TEdit
      Left = 169
      Top = 51
      Width = 433
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      ExplicitWidth = 431
    end
    object btBrowse: TButton
      Left = 611
      Top = 48
      Width = 86
      Height = 31
      Anchors = [akTop, akRight]
      Caption = 'Browse ...'
      TabOrder = 6
      OnClick = btBrowseClick
      ExplicitLeft = 609
    end
  end
end
