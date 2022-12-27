object Form1: TForm1
  Left = 192
  Top = 105
  BorderStyle = bsDialog
  Caption = 'Splitter'
  ClientHeight = 310
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 10
    Top = 10
    Width = 329
    Height = 38
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'This utility will generate 16 1024x1024 BMP '#13#10'textures from the ' +
      #39'TextureMap.jpg'#39' files.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LAAction: TLabel
    Left = 60
    Top = 224
    Width = 53
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'LAAction'
    Visible = False
  end
  object EDFile: TEdit
    Left = 10
    Top = 58
    Width = 331
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Enabled = False
    TabOrder = 0
    Text = 'TextureMap.jpg'
    Visible = False
  end
  object Button1: TButton
    Left = 33
    Top = 254
    Width = 81
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Split'
    TabOrder = 1
    OnClick = Button1Click
  end
  object EDTileSize: TEdit
    Left = 140
    Top = 220
    Width = 71
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Enabled = False
    TabOrder = 2
    Text = '1024'
    Visible = False
  end
  object EDMask: TEdit
    Left = 10
    Top = 91
    Width = 331
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Enabled = False
    TabOrder = 3
    Text = 'Tex_%d_%d.bmp'
    Visible = False
  end
  object ProgressBar: TProgressBar
    Left = 170
    Top = 254
    Width = 231
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Max = 16
    Smooth = True
    TabOrder = 4
  end
  object RBFull: TRadioButton
    Left = 60
    Top = 125
    Width = 281
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Full Resolution (64 MB graphics memory)'
    TabOrder = 5
  end
  object RBHalf: TRadioButton
    Left = 60
    Top = 154
    Width = 281
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Medium Resolution (16 MB)'
    Checked = True
    TabOrder = 6
    TabStop = True
  end
  object RBLow: TRadioButton
    Left = 60
    Top = 183
    Width = 281
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Low Resolution (4 MB)'
    TabOrder = 7
  end
end
