object frmSandbox: TfrmSandbox
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Interface'
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
        070F00000009540054007200650065004E006F00640065002D00000000000000
        00000000FFFFFFFFFFFFFFFF00000000000000000000000000010742006D0070
        0046006F006E00740000002B0000000000000000000000FFFFFFFFFFFFFFFF00
        0000000000000000000000000106430061006D0065007200610000003F000000
        0000000000000000FFFFFFFFFFFFFFFF00000000000000000000000000011043
        0061006D0065007200610043006F006E00740072006F006C006C006500720000
        002D0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        0000010743006F006E0073006F006C00650000002B0000000000000000000000
        FFFFFFFFFFFFFFFF00000000000000000000000000010643007500720073006F
        0072000000350000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        000000000000010B43007500720073006F007200430072006F00730073000000
        310000000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000
        00010946006F006E00740043006F006C006F00720000002F0000000000000000
        000000FFFFFFFFFFFFFFFF000000000000000000000000000108470061006D00
        65004D0065006E0075000000290000000000000000000000FFFFFFFFFFFFFFFF
        000000000000000000000000000105470069007A006D006F0000002D00000000
        00000000000000FFFFFFFFFFFFFFFF0000000000000000000000000001074700
        69007A006D006F004500780000002D0000000000000000000000FFFFFFFFFFFF
        FFFF000000000000000000000000000107470075006900440065006D006F0000
        002F0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        000001084700750069005000610069006E00740000002B000000000000000000
        0000FFFFFFFFFFFFFFFF00000000000000000000000000010648004600500069
        0063006B000000270000000000000000000000FFFFFFFFFFFFFFFF0000000000
        000000000000000001045000690063006B0000003F0000000000000000000000
        FFFFFFFFFFFFFFFF000000000000000000000000000110530069006D0070006C
        0065004E0061007600690067006100740069006F006E00}
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
