object FormCollisions: TFormCollisions
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Collisions'
  ClientHeight = 1055
  ClientWidth = 1425
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
    Height = 1055
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 0
    object tvCollisions: TTreeView
      Left = 1
      Top = 1
      Width = 224
      Height = 1053
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Indent = 33
      TabOrder = 0
      Items.NodeData = {
        070A00000009540054007200650065004E006F00640065002D00000000000000
        00000000FFFFFFFFFFFFFFFF00000000000000000000000000010742006F0078
        006500640069006E000000310000000000000000000000FFFFFFFFFFFFFFFF00
        000000000000000000000000010942006F007800530070006800650072006500
        0000310000000000000000000000FFFFFFFFFFFFFFFF00000000000000000000
        000000010946006100630065007600660061006300650000002D000000000000
        0000000000FFFFFFFFFFFFFFFF0000000000000000000000000001074D006500
        7300680068006900740000003B0000000000000000000000FFFFFFFFFFFFFFFF
        00000000000000000000000000010E4F00630063006C007500730069006F006E
        005100750065007200790000002B0000000000000000000000FFFFFFFFFFFFFF
        FF0000000000000000000000000001064F006300740072006500650000002B00
        00000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000000001
        0652006100790062006F00780000002D0000000000000000000000FFFFFFFFFF
        FFFFFF0000000000000000000000000001075200610079006300610073007400
        00002B0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000000
        00000001065300700068006500720065000000350000000000000000000000FF
        FFFFFFFFFFFFFF00000000000000000000000000010B54007200690061006E00
        67006C00650062006F007800}
    end
  end
  object PageControl: TPageControl
    Left = 226
    Top = 0
    Width = 1199
    Height = 1055
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 1
    object tsBoxedin: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Boxedin'
      TabVisible = False
    end
    object tsBoxSphere: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'BoxSphere'
      ImageIndex = 1
      TabVisible = False
    end
    object tsFacevface: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Facevface'
      ImageIndex = 2
      TabVisible = False
    end
    object tsMeshHit: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'MeshHit'
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
    object tsOcclusionQuery: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'OcclusionQuery'
      ImageIndex = 5
      TabVisible = False
    end
    object tsOctree: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Octree'
      ImageIndex = 6
      TabVisible = False
    end
    object tsRaybox: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Raybox'
      ImageIndex = 7
      TabVisible = False
    end
    object tsRaycast: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Raycast'
      ImageIndex = 8
      TabVisible = False
    end
    object tsSphere: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Sphere'
      ImageIndex = 9
      TabVisible = False
    end
    object tsTriangleBox: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'TriangleBox'
      ImageIndex = 10
      TabVisible = False
    end
  end
end
