object FormCudaD: TFormCudaD
  Left = 0
  Top = 0
  Caption = 'Cuda D'
  ClientHeight = 544
  ClientWidth = 902
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 129
    Height = 544
    Align = alLeft
    TabOrder = 0
    object tvCuda: TTreeView
      Left = 1
      Top = 1
      Width = 127
      Height = 542
      Align = alClient
      Indent = 19
      TabOrder = 0
      OnClick = tvCudaClick
      Items.NodeData = {
        03060000003E0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        00000000000110460061007300740046006F0075007200690065007200540072
        0061006E0073003A0000000000000000000000FFFFFFFFFFFFFFFF0000000000
        00000000000000010E50006F0073007400500072006F00630065007300730069
        006E006700380000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        0000000000010D5300630061006C0061007200500072006F0064007500630074
        00380000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        00010D530069006D0070006C0065005400650078007400750072006500360000
        000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000010C53
        007400610062006C00650046006C0075006900640073003A0000000000000000
        000000FFFFFFFFFFFFFFFF000000000000000000000000010E56006500720074
        006500780074004400610074006100470065006E00}
      ExplicitLeft = 0
      ExplicitTop = 0
    end
  end
  object PageControl: TPageControl
    Left = 129
    Top = 0
    Width = 773
    Height = 544
    ActivePage = tsVertexDataGen
    Align = alClient
    TabOrder = 1
    object tsFastFourierTrans: TTabSheet
      Caption = 'FastFourierTrans'
      TabVisible = False
    end
    object tsPostProcessing: TTabSheet
      Caption = 'PostProcessing'
      ImageIndex = 1
      TabVisible = False
    end
    object tsScalarProduct: TTabSheet
      Caption = 'ScalarProduct'
      ImageIndex = 2
      TabVisible = False
    end
    object tsSimpleTexture: TTabSheet
      Caption = 'SimpleTexture'
      ImageIndex = 3
      TabVisible = False
    end
    object tsStableFluids: TTabSheet
      Caption = 'StableFluids'
      ImageIndex = 5
      TabVisible = False
    end
    object tsVertexDataGen: TTabSheet
      Caption = 'VertexDataGen'
      ImageIndex = 4
      TabVisible = False
    end
  end
  object MainMenu: TMainMenu
    Left = 272
    Top = 64
  end
end
