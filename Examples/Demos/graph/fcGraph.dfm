object FormGraph: TFormGraph
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Graph'
  ClientHeight = 999
  ClientWidth = 1544
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
    Height = 999
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 0
    object tvGraph: TTreeView
      Left = 1
      Top = 1
      Width = 224
      Height = 997
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Indent = 33
      TabOrder = 0
      OnClick = tvGraphClick
      Items.NodeData = {
        070500000009540054007200650065004E006F00640065002500000000000000
        00000000FFFFFFFFFFFFFFFF0000000000000000000000000001034600780079
        000000330000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
        00000000010A480069006700680074004600690065006C00640000002B000000
        0000000000000000FFFFFFFFFFFFFFFF00000000000000000000000000010650
        006F0069006E00740073000000330000000000000000000000FFFFFFFFFFFFFF
        FF00000000000000000000000000010A500072006F006A006500630074006900
        6F006E0000002D0000000000000000000000FFFFFFFFFFFFFFFF000000000000
        000000000000000107530070006C0069006E0065007300}
    end
  end
  object PageControl: TPageControl
    Left = 226
    Top = 0
    Width = 1318
    Height = 999
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 1
    object tsFxy: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Fxy'
      TabVisible = False
    end
    object tsHeightField: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'HeightField'
      ImageIndex = 1
      TabVisible = False
    end
    object tsPoints: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Points'
      ImageIndex = 2
      TabVisible = False
    end
    object tsProjection: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Projection'
      ImageIndex = 3
      TabVisible = False
    end
    object tsSplines: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Splines'
      ImageIndex = 4
      TabVisible = False
    end
  end
  object MainMenu: TMainMenu
    Left = 340
    Top = 80
  end
end
