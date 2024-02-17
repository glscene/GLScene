object frmMaterials: TfrmMaterials
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Materials'
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
    object tvMaterials: TTreeView
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
        071000000009540054007200650065004E006F00640065002D00000000000000
        00000000FFFFFFFFFFFFFFFF0000000000000000000000000001074300750062
        0065004D00610070000000330000000100000000000000FFFFFFFFFFFFFFFF00
        000000000000000000000000010A43007500730074006F006D00510075006100
        640000003B0000000200000000000000FFFFFFFFFFFFFFFF0000000000000000
        0000000000010E440079006E0061006D00690063005400650078007400750072
        0065000000350000000300000000000000FFFFFFFFFFFFFFFF00000000000000
        000000000000010B4600690072006500320044005F0047005200330032000000
        3B0000000400000000000000FFFFFFFFFFFFFFFF000000000000000000000000
        00010E4D006100740065007200690061006C0053006300720069007000740000
        002B0000000500000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        000001064D006900720072006F0072000000390000000600000000000000FFFF
        FFFFFFFFFFFF00000000000000000000000000010D4D0075006C00740069004D
        006100740065007200690061006C000000310000000700000000000000FFFFFF
        FFFFFFFFFF0000000000000000000000000001094D0075006C00740069005000
        6100730073000000370000000800000000000000FFFFFFFFFFFFFFFF00000000
        000000000000000000010C4D0075006C00740069005400650078007400750072
        0065000000330000000900000000000000FFFFFFFFFFFFFFFF00000000000000
        000000000000010A4F0062006A006500630074004D0061007400730000003F00
        00000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000000001
        10500072006F006300650064007500720061006C0043006C006F007500640073
        0000002D0000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
        00000000010754006500780041006E0069006D00000033000000000000000000
        0000FFFFFFFFFFFFFFFF00000000000000000000000000010A54006500780043
        006F006D00620069006E0065000000310000000000000000000000FFFFFFFFFF
        FFFFFF00000000000000000000000000010954006500780046006F0072006D00
        610074000000370000000000000000000000FFFFFFFFFFFFFFFF000000000000
        00000000000000010C5400720061006E00730070006100720065006E00630079
        000000470000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
        0000000001145400720061006E00730070006100720065006E00630079004100
        6400760061006E00630065006400}
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
    ActivePage = tsSeven
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
