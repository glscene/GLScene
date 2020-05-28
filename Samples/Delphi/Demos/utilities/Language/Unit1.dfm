object Form1: TForm1
  Left = 245
  Top = 173
  Caption = 'Language demo'
  ClientHeight = 283
  ClientWidth = 496
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object AboutScene: TMemo
    Left = 296
    Top = 4
    Width = 179
    Height = 274
    Lines.Strings = (
      'AboutGLScene')
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 4
    Width = 296
    Height = 274
    Caption = 'GroupBox1'
    TabOrder = 1
    object Label1: TLabel
      Left = 102
      Top = 100
      Width = 32
      Height = 13
      Caption = 'Label1'
      Color = clBtnFace
      ParentColor = False
    end
    object Button: TButton
      Left = 22
      Top = 28
      Width = 75
      Height = 25
      Caption = 'Button'
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 142
      Top = 172
      Width = 128
      Height = 50
      Caption = 'Panel1'
      TabOrder = 1
    end
  end
  object MainMenu1: TMainMenu
    Left = 168
    Top = 72
    object mOption: TMenuItem
      Caption = 'Option'
      object mLanguage: TMenuItem
        Caption = 'Language'
        object mEnglish: TMenuItem
          Caption = 'English'
          OnClick = mEnglishClick
        end
        object mRussian: TMenuItem
          Caption = 'Russian'
          OnClick = mRussianClick
        end
        object mDeutsch: TMenuItem
          Caption = 'Deutsch'
          OnClick = mDeutschClick
        end
      end
    end
    object mHelp: TMenuItem
      Caption = 'Help'
    end
  end
  object GLSLanguage1: TGLSLanguage
    Left = 168
    Top = 32
  end
end
