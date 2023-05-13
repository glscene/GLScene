object frmExtrusionD: TfrmExtrusionD
  Left = 0
  Top = 0
  Caption = 'Extrusion D'
  ClientHeight = 558
  ClientWidth = 858
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
    Height = 558
    Align = alLeft
    TabOrder = 0
    object tvExtrusion: TTreeView
      Left = 1
      Top = 1
      Width = 127
      Height = 556
      Align = alClient
      Indent = 19
      TabOrder = 0
      OnClick = tvExtrusionClick
      Items.NodeData = {
        03050000002C0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        00000000000107420065006E00640069006E0067003200000000000000000000
        00FFFFFFFFFFFFFFFF000000000000000000000000010A4300750074006F0075
        0074005300740061007200320000000000000000000000FFFFFFFFFFFFFFFF00
        0000000000000000000000010A4E007500740073006E0042006F006C00740073
        00260000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        0001045000610077006E00300000000000000000000000FFFFFFFFFFFFFFFF00
        00000000000000000000000109540065006E007400610063006C0065007300}
    end
  end
  object PageControl: TPageControl
    Left = 129
    Top = 0
    Width = 729
    Height = 558
    ActivePage = tsTentacles
    Align = alClient
    TabOrder = 1
    object tsBending: TTabSheet
      Caption = 'Bending'
      TabVisible = False
    end
    object tsCutoutStar: TTabSheet
      Caption = 'CutoutStar'
      ImageIndex = 1
      TabVisible = False
    end
    object tsNutsnBolts: TTabSheet
      Caption = 'NutsnBolts'
      ImageIndex = 2
      TabVisible = False
    end
    object tsPawn: TTabSheet
      Caption = 'Pawn'
      ImageIndex = 3
      TabVisible = False
    end
    object tsTentacles: TTabSheet
      Caption = 'Tentacles'
      ImageIndex = 4
      TabVisible = False
    end
  end
  object MainMenu: TMainMenu
    Left = 425
    Top = 100
  end
end
