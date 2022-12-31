object Form1: TForm1
  Left = 192
  Top = 126
  Caption = 'Splitter'
  ClientHeight = 319
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 451
    Height = 30
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alTop
    Caption = 
      'This utility will generate 16 1024x1024 BMP '#13#10'textures from the ' +
      #39'ArchipelagoMap.jpg'#39' files.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitWidth = 250
  end
  object LAAction: TLabel
    Left = 42
    Top = 223
    Width = 43
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'LAAction'
    Visible = False
  end
  object EDFile: TEdit
    Left = 16
    Top = 68
    Width = 265
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Enabled = False
    TabOrder = 0
    Text = 'ArchipelagoMap.jpg'
    Visible = False
  end
  object Button1: TButton
    Left = 42
    Top = 240
    Width = 65
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Split'
    TabOrder = 1
    OnClick = Button1Click
  end
  object EDTileSize: TEdit
    Left = 271
    Top = 132
    Width = 57
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Enabled = False
    TabOrder = 2
    Text = '1024'
    Visible = False
  end
  object EDMask: TEdit
    Left = 16
    Top = 101
    Width = 265
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Enabled = False
    TabOrder = 3
    Text = 'Tex_%d_%d.bmp'
    Visible = False
  end
  object ProgressBar: TProgressBar
    Left = 128
    Top = 240
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Max = 16
    Smooth = True
    TabOrder = 4
  end
  object RBFull: TRadioButton
    Left = 42
    Top = 134
    Width = 225
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Full Resolution (64 MB graphics memory)'
    TabOrder = 5
  end
  object RBHalf: TRadioButton
    Left = 42
    Top = 155
    Width = 225
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Medium Resolution (16 MB)'
    TabOrder = 6
  end
  object RBLow: TRadioButton
    Left = 42
    Top = 176
    Width = 225
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Low Resolution (4 MB)'
    Checked = True
    TabOrder = 7
    TabStop = True
  end
end
