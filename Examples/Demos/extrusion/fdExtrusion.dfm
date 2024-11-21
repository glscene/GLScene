object FormExtrusion: TFormExtrusion
  Left = 0
  Top = 0
  Caption = 'Extrusion'
  ClientHeight = 558
  ClientWidth = 657
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
    ExplicitHeight = 533
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
        070500000009540054007200650065004E006F00640065002D00000000000000
        00000000FFFFFFFFFFFFFFFF000000000000000000000000000107420065006E
        00640069006E0067000000330000000000000000000000FFFFFFFFFFFFFFFF00
        000000000000000000000000010A4300750074006F0075007400530074006100
        72000000330000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
        0000000000010A4E007500740073006E0042006F006C00740073000000270000
        000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000000104
        5000610077006E000000310000000000000000000000FFFFFFFFFFFFFFFF0000
        00000000000000000000000109540065006E007400610063006C0065007300}
      ExplicitHeight = 531
    end
  end
  object PageControl: TPageControl
    Left = 129
    Top = 0
    Width = 528
    Height = 558
    ActivePage = tsTentacles
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 520
    ExplicitHeight = 533
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
