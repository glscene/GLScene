object Form1: TForm1
  Left = 245
  Top = 173
  Caption = 'Language demo'
  ClientHeight = 354
  ClientWidth = 620
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object AboutScene: TMemo
    Left = 370
    Top = 5
    Width = 224
    Height = 343
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Lines.Strings = (
      'AboutGLScene')
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 5
    Width = 370
    Height = 343
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'GroupBox1'
    TabOrder = 1
    object Label1: TLabel
      Left = 128
      Top = 125
      Width = 41
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Label1'
      Color = clBtnFace
      ParentColor = False
    end
    object Button: TButton
      Left = 28
      Top = 35
      Width = 93
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Button'
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 178
      Top = 215
      Width = 160
      Height = 63
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
