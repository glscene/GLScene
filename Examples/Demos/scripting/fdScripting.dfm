object FormScripting: TFormScripting
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Scripting'
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
        070400000009540054007200650065004E006F00640065003300000000000000
        00000000FFFFFFFFFFFFFFFF00000000000000000000000000010A4400570053
        00730068006100640065007200730000002F0000000000000000000000FFFFFF
        FFFFFFFFFF00000000000000000000000000010847004C005300630072006900
        700074000000370000000000000000000000FFFFFFFFFFFFFFFF000000000000
        00000000000000010C5300630072006900700074004200610073006900630073
        000000310000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
        00000000010950007900740068006F006E0047004C005300}
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
