object frmMain: TfrmMain
  Left = 277
  Top = 178
  BorderStyle = bsSingle
  Caption = 'FontGen'
  ClientHeight = 454
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object GroupBox1: TGroupBox
    Left = 8
    Top = 10
    Width = 453
    Height = 136
    Caption = '1. Select Windows font :'
    TabOrder = 0
    object Panel1: TPanel
      Left = 13
      Top = 25
      Width = 428
      Height = 51
      BevelOuter = bvLowered
      Caption = 'Sample AaBbCcDd'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object Button1: TButton
      Left = 178
      Top = 88
      Width = 93
      Height = 31
      Caption = 'Select...'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 150
    Width = 453
    Height = 211
    Caption = '2. Check glyphs'
    TabOrder = 1
    object Image1: TImage
      Left = 2
      Top = 18
      Width = 449
      Height = 191
      Align = alClient
      Center = True
      ExplicitLeft = 3
      ExplicitTop = 19
      ExplicitWidth = 448
      ExplicitHeight = 190
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 363
    Width = 453
    Height = 81
    Caption = '3. Save font:'
    TabOrder = 2
    object Button2: TButton
      Left = 188
      Top = 30
      Width = 93
      Height = 31
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
