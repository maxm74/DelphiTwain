object FormTwainDemo: TFormTwainDemo
  Left = 234
  Top = 98
  Caption = 'FormTwainDemo'
  ClientHeight = 576
  ClientWidth = 769
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnDestroy = FormDestroy
  TextHeight = 15
  object ImageHolder: TImage
    Left = 0
    Top = 40
    Width = 769
    Height = 536
    Align = alClient
    Proportional = True
    Stretch = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 769
    Height = 40
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 765
    object btAcquire: TButton
      Left = 680
      Top = 0
      Width = 75
      Height = 32
      Caption = 'Acquire'
      Enabled = False
      TabOrder = 0
      OnClick = btAcquireClick
    end
    object btSelect: TButton
      Left = 8
      Top = 0
      Width = 88
      Height = 32
      Caption = 'Select Source'
      TabOrder = 1
      OnClick = btSelectClick
    end
    object cbNativeCapture: TCheckBox
      Left = 456
      Top = 0
      Width = 97
      Height = 19
      Caption = 'Native Capture'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object cbModalCapture: TCheckBox
      Left = 555
      Top = 0
      Width = 52
      Height = 19
      Caption = 'Modal'
      TabOrder = 3
    end
    object cbShowUI: TCheckBox
      Left = 611
      Top = 0
      Width = 58
      Height = 19
      Caption = 'ShowUI'
      TabOrder = 4
    end
  end
end
