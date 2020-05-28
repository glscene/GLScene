object Form1: TForm1
  Left = 192
  Top = 105
  BorderStyle = bsDialog
  Caption = 'Splitter'
  ClientHeight = 248
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 276
    Height = 32
    Caption = 
      'This utility will generate 16 1024x1024 BMP '#13#10'textures from the ' +
      #39'TextureMap.jpg'#39' files.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LAAction: TLabel
    Left = 48
    Top = 179
    Width = 43
    Height = 13
    Caption = 'LAAction'
    Visible = False
  end
  object EDFile: TEdit
    Left = 8
    Top = 46
    Width = 265
    Height = 21
    Enabled = False
    TabOrder = 0
    Text = 'TextureMap.jpg'
    Visible = False
  end
  object Button1: TButton
    Left = 26
    Top = 203
    Width = 65
    Height = 25
    Caption = 'Split'
    TabOrder = 1
    OnClick = Button1Click
  end
  object EDTileSize: TEdit
    Left = 112
    Top = 176
    Width = 57
    Height = 21
    Enabled = False
    TabOrder = 2
    Text = '1024'
    Visible = False
  end
  object EDMask: TEdit
    Left = 8
    Top = 73
    Width = 265
    Height = 21
    Enabled = False
    TabOrder = 3
    Text = 'Tex_%d_%d.bmp'
    Visible = False
  end
  object ProgressBar: TProgressBar
    Left = 136
    Top = 203
    Width = 185
    Height = 25
    Max = 16
    Smooth = True
    TabOrder = 4
  end
  object RBFull: TRadioButton
    Left = 48
    Top = 100
    Width = 225
    Height = 17
    Caption = 'Full Resolution (64 MB graphics memory)'
    TabOrder = 5
  end
  object RBHalf: TRadioButton
    Left = 48
    Top = 123
    Width = 225
    Height = 17
    Caption = 'Medium Resolution (16 MB)'
    Checked = True
    TabOrder = 6
    TabStop = True
  end
  object RBLow: TRadioButton
    Left = 48
    Top = 146
    Width = 225
    Height = 17
    Caption = 'Low Resolution (4 MB)'
    TabOrder = 7
  end
end
