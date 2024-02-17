object frmComputing: TfrmComputing
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Computing'
  ClientHeight = 814
  ClientWidth = 1138
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 168
  TextHeight = 30
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 226
    Height = 814
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 0
    object tvBench: TTreeView
      Left = 1
      Top = 1
      Width = 224
      Height = 812
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Indent = 33
      TabOrder = 0
      Items.NodeData = {
        070600000009540054007200650065004E006F00640065003500000000000000
        00000000FFFFFFFFFFFFFFFF00000000000000000000000000010B4600610073
        00740046006F007500720069006500720000003B0000000000000000000000FF
        FFFFFFFFFFFFFF00000000000000000000000000010E50006F00730074005000
        72006F00630065007300730069006E0067000000390000000000000000000000
        FFFFFFFFFFFFFFFF00000000000000000000000000010D5300630061006C0061
        007200500072006F0064007500630074000000330000000000000000000000FF
        FFFFFFFFFFFFFF00000000000000000000000000010A530069006D0070006C00
        650043005500440041000000370000000000000000000000FFFFFFFFFFFFFFFF
        00000000000000000000000000010C53007400610062006C00650046006C0075
        0069006400730000003F0000000000000000000000FFFFFFFFFFFFFFFF000000
        000000000000000000000110560065007200740065007800470065006E006500
        72006100740069006F006E00}
    end
  end
  object PageControl: TPageControl
    Left = 226
    Top = 0
    Width = 912
    Height = 814
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 1
    object tsOne: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'One'
      TabVisible = False
    end
    object tsTwo: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Two'
      ImageIndex = 1
      TabVisible = False
    end
    object tsThree: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Three'
      ImageIndex = 2
      TabVisible = False
    end
    object tsFour: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Four'
      ImageIndex = 3
      TabVisible = False
    end
    object tsFive: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Five'
      ImageIndex = 4
      TabVisible = False
    end
    object tsSix: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Six'
      ImageIndex = 5
      TabVisible = False
    end
    object tsSeven: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Seven'
      ImageIndex = 6
      TabVisible = False
    end
  end
end
