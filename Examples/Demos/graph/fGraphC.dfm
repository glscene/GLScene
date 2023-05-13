object FormGraph: TFormGraph
  Left = 0
  Top = 0
  Caption = 'Graph C'
  ClientHeight = 571
  ClientWidth = 882
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
    Height = 571
    Align = alLeft
    TabOrder = 0
    object tvGraph: TTreeView
      Left = 1
      Top = 1
      Width = 127
      Height = 569
      Align = alClient
      Indent = 19
      TabOrder = 0
      OnClick = tvGraphClick
      Items.NodeData = {
        0305000000240000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        00000000000103460078007900320000000000000000000000FFFFFFFFFFFFFF
        FF000000000000000000000000010A480069006700680074004600690065006C
        0064002A0000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
        000000010650006F0069006E0074007300320000000000000000000000FFFFFF
        FFFFFFFFFF000000000000000000000000010A500072006F006A006500630074
        0069006F006E002C0000000000000000000000FFFFFFFFFFFFFFFF0000000000
        000000000000000107530070006C0069006E0065007300}
    end
  end
  object PageControl: TPageControl
    Left = 129
    Top = 0
    Width = 753
    Height = 571
    Align = alClient
    TabOrder = 1
    object tsFxy: TTabSheet
      Caption = 'Fxy'
      TabVisible = False
    end
    object tsHeightField: TTabSheet
      Caption = 'HeightField'
      ImageIndex = 1
      TabVisible = False
    end
    object tsPoints: TTabSheet
      Caption = 'Points'
      ImageIndex = 2
      TabVisible = False
    end
    object tsProjection: TTabSheet
      Caption = 'Projection'
      ImageIndex = 3
      TabVisible = False
    end
    object tsSplines: TTabSheet
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
