object FormBench: TFormBench
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Bench'
  ClientHeight = 1036
  ClientWidth = 1558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 168
  TextHeight = 30
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 226
    Height = 1036
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
      Height = 1034
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Indent = 33
      TabOrder = 0
      OnClick = tvBenchClick
      Items.NodeData = {
        070600000009540054007200650065004E006F00640065002B00000000000000
        00000000FFFFFFFFFFFFFFFF000000000000000000000000000106430061006E
        0076006100730000002F0000000000000000000000FFFFFFFFFFFFFFFF000000
        0000000000000000000001084D00650067006100630075006200650000003900
        00000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000000001
        0D4D0065006700610067006C00610073007300630075006200650000002D0000
        000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000000107
        53006D006F006B0069006E00670000002D0000000000000000000000FFFFFFFF
        FFFFFFFF00000000000000000000000000010756006F006C00630061006E006F
        000000310000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
        00000000010957006800690072006C00770069006E006400}
    end
  end
  object PageControl: TPageControl
    Left = 226
    Top = 0
    Width = 1332
    Height = 1036
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tsCanvas
    Align = alClient
    TabOrder = 1
    object tsCanvas: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Canvas'
      TabVisible = False
    end
    object tsMegacube: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Megacube'
      ImageIndex = 1
      TabVisible = False
    end
    object tsMegaglasscube: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Megaglasscube'
      ImageIndex = 2
      TabVisible = False
    end
    object tsSmoking: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Smoking'
      ImageIndex = 3
      TabVisible = False
    end
    object tsVolcano: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Volcano'
      ImageIndex = 4
      TabVisible = False
    end
    object tsWhirlwind: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Whirlwind'
      ImageIndex = 5
      TabVisible = False
    end
  end
end
