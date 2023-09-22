object frmBench: TfrmBench
  Left = 0
  Top = 0
  Caption = 'Bench D'
  ClientHeight = 566
  ClientWidth = 890
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 129
    Height = 566
    Align = alLeft
    TabOrder = 0
    object tvBench: TTreeView
      Left = 1
      Top = 1
      Width = 127
      Height = 564
      Align = alClient
      Indent = 19
      TabOrder = 0
      OnClick = tvBenchClick
      Items.NodeData = {
        03060000002A0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        00000000000106430061006E007600610073002E0000000000000000000000FF
        FFFFFFFFFFFFFF00000000000000000000000001084D00650067006100630075
        0062006500380000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        0000000000010D4D0065006700610067006C0061007300730063007500620065
        002C0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        00010753006D006F006B0069006E0067002C0000000000000000000000FFFFFF
        FFFFFFFFFF000000000000000000000000010756006F006C00630061006E006F
        00300000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        00010957006800690072006C00770069006E006400}
    end
  end
  object PageControl: TPageControl
    Left = 129
    Top = 0
    Width = 761
    Height = 566
    Align = alClient
    TabOrder = 1
    object tsCanvas: TTabSheet
      Caption = 'Canvas'
      TabVisible = False
    end
    object tsMegacube: TTabSheet
      Caption = 'Megacube'
      ImageIndex = 1
      TabVisible = False
    end
    object tsMegaglasscube: TTabSheet
      Caption = 'Megaglasscube'
      ImageIndex = 2
      TabVisible = False
    end
    object tsSmoking: TTabSheet
      Caption = 'Smoking'
      ImageIndex = 3
      TabVisible = False
    end
    object tsVolcano: TTabSheet
      Caption = 'Volcano'
      ImageIndex = 4
      TabVisible = False
    end
    object tsWhirlwind: TTabSheet
      Caption = 'Whirlwind'
      ImageIndex = 5
      TabVisible = False
    end
  end
end
