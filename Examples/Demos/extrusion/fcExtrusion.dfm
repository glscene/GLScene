object FormExtrusion: TFormExtrusion
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Extrusion'
  ClientHeight = 947
  ClientWidth = 1306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 168
  TextHeight = 30
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 226
    Height = 947
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 0
    object tvExtrusion: TTreeView
      Left = 1
      Top = 1
      Width = 224
      Height = 945
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Indent = 33
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
    end
  end
  object PageControl: TPageControl
    Left = 226
    Top = 0
    Width = 1080
    Height = 947
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tsTentacles
    Align = alClient
    TabOrder = 1
    object tsBending: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Bending'
      TabVisible = False
    end
    object tsCutoutStar: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'CutoutStar'
      ImageIndex = 1
      TabVisible = False
    end
    object tsNutsnBolts: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'NutsnBolts'
      ImageIndex = 2
      TabVisible = False
    end
    object tsPawn: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Pawn'
      ImageIndex = 3
      TabVisible = False
    end
    object tsTentacles: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Tentacles'
      ImageIndex = 4
      TabVisible = False
    end
  end
  object MainMenu: TMainMenu
    Left = 340
    Top = 80
  end
end
