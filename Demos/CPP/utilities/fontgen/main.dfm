object frmMain: TfrmMain
  Left = 277
  Top = 178
  BorderStyle = bsSingle
  Caption = 'FontGen for GLScene'
  ClientHeight = 363
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 6
    Top = 8
    Width = 363
    Height = 109
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = '1. Select Windows font :'
    TabOrder = 0
    object Panel1: TPanel
      Left = 10
      Top = 20
      Width = 343
      Height = 41
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      BevelOuter = bvLowered
      Caption = 'Sample AaBbCcDd'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object Button1: TButton
      Left = 142
      Top = 70
      Width = 75
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Select...'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 6
    Top = 120
    Width = 363
    Height = 169
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = '2. Check glyphs'
    TabOrder = 1
    object Image1: TImage
      Left = 2
      Top = 15
      Width = 359
      Height = 152
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      Center = True
      ExplicitWidth = 360
    end
  end
  object GroupBox3: TGroupBox
    Left = 6
    Top = 290
    Width = 363
    Height = 65
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = '3. Save font:'
    TabOrder = 2
    object Button2: TButton
      Left = 150
      Top = 24
      Width = 75
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Save...'
      TabOrder = 0
      OnClick = Button2Click
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 264
    Top = 82
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'gsf'
    FileName = 'font'
    Filter = 'GlScene Font|*.gsf|All Files|*.*'
    Title = 'Save font'
    Left = 290
    Top = 310
  end
end
