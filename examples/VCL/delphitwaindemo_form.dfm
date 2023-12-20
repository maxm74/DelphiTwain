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
    Top = 64
    Width = 769
    Height = 512
    Align = alClient
    Proportional = True
    Stretch = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 769
    Height = 64
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 765
    object Label2: TLabel
      Left = 8
      Top = 34
      Width = 36
      Height = 15
      Caption = 'Paper :'
    end
    object Label7: TLabel
      Left = 240
      Top = 34
      Width = 66
      Height = 15
      Caption = 'Image Type :'
    end
    object Label3: TLabel
      Left = 504
      Top = 34
      Width = 62
      Height = 15
      Caption = 'Resolution :'
    end
    object btAcquire: TButton
      Left = 685
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
    object cbPaperSize: TComboBox
      Left = 46
      Top = 34
      Width = 192
      Height = 23
      Style = csDropDownList
      TabOrder = 2
    end
    object cbPixelType: TComboBox
      Left = 310
      Top = 34
      Width = 192
      Height = 23
      Style = csDropDownList
      TabOrder = 3
    end
    object cbResolution: TComboBox
      Left = 568
      Top = 34
      Width = 192
      Height = 23
      Style = csDropDownList
      TabOrder = 4
    end
    object cbNativeCapture: TCheckBox
      Left = 436
      Top = 0
      Width = 102
      Height = 19
      Caption = 'Native Capture'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object cbModalCapture: TCheckBox
      Left = 544
      Top = 0
      Width = 58
      Height = 19
      Caption = 'Modal'
      TabOrder = 6
    end
    object cbShowUI: TCheckBox
      Left = 614
      Top = 0
      Width = 65
      Height = 19
      Caption = 'ShowUI'
      TabOrder = 7
    end
  end
end
