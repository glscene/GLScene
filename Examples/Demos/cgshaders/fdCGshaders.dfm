object frmCGshaders: TfrmCGshaders
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'CGshaders'
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
        070900000009540054007200650065004E006F00640065003300000000000000
        00000000FFFFFFFFFFFFFFFF00000000000000000000000000010A42006C0069
        006E006E0053006800650065006E000000330000000000000000000000FFFFFF
        FFFFFFFFFF00000000000000000000000000010A42006F006D00620053006800
        61006400650072000000350000000000000000000000FFFFFFFFFFFFFFFF0000
        0000000000000000000000010B420075006D0070004D0061007000700069006E
        0067000000350000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        000000000000010B430065006C006C00530068006100640069006E0067000000
        2F0000000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000
        00010843006C006F007500640053006B00790000003300000000000000000000
        00FFFFFFFFFFFFFFFF00000000000000000000000000010A4400690073007400
        6F007200740069006F006E0000002D0000000000000000000000FFFFFFFFFFFF
        FFFF0000000000000000000000000001075200650066006C0065006300740000
        002F0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        00000108530069006D0070006C00650043004700000031000000000000000000
        0000FFFFFFFFFFFFFFFF00000000000000000000000000010954006500780074
        007500720069006E006700}
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
