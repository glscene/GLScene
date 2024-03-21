object TTBMain: TTTBMain
  Left = 293
  Top = 216
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'Texture ToolBox'
  ClientHeight = 812
  ClientWidth = 1307
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 168
  TextHeight = 24
  object Splitter1: TSplitter
    Left = 575
    Top = 0
    Width = 0
    Height = 812
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    MinSize = 42
    ExplicitLeft = 673
  end
  object PAImages: TPanel
    Left = 0
    Top = 0
    Width = 575
    Height = 812
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alLeft
    BevelOuter = bvLowered
    BorderWidth = 4
    Caption = 'PAImages'
    TabOrder = 0
    DesignSize = (
      575
      812)
    object PageControl: TPageControl
      Left = 5
      Top = 54
      Width = 565
      Height = 753
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      ActivePage = TSAlpha
      Align = alClient
      Style = tsButtons
      TabOrder = 0
      ExplicitWidth = 663
      object TSRGB: TTabSheet
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'RGB'
        object ScrollBox1: TScrollBox
          Left = 0
          Top = 0
          Width = 557
          Height = 711
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          HorzScrollBar.Smooth = True
          HorzScrollBar.Style = ssFlat
          HorzScrollBar.Tracking = True
          VertScrollBar.Smooth = True
          VertScrollBar.Style = ssFlat
          VertScrollBar.Tracking = True
          Align = alClient
          Color = clSilver
          ParentColor = False
          TabOrder = 0
          ExplicitWidth = 655
          object IMRGB: TImage
            Left = 0
            Top = 0
            Width = 533
            Height = 505
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            AutoSize = True
          end
        end
      end
      object TSAlpha: TTabSheet
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'Alpha'
        ImageIndex = 1
        object ScrollBox2: TScrollBox
          Left = 0
          Top = 0
          Width = 557
          Height = 711
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          HorzScrollBar.Smooth = True
          HorzScrollBar.Style = ssFlat
          HorzScrollBar.Tracking = True
          VertScrollBar.Smooth = True
          VertScrollBar.Style = ssFlat
          VertScrollBar.Tracking = True
          Align = alClient
          Color = clSilver
          ParentColor = False
          TabOrder = 0
          ExplicitWidth = 655
          object IMAlpha: TImage
            Left = 0
            Top = 0
            Width = 504
            Height = 546
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            AutoSize = True
          end
        end
      end
    end
    object ToolBar: TToolBar
      Left = 308
      Top = 62
      Width = 267
      Height = 40
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alNone
      Anchors = [akTop, akRight]
      ButtonHeight = 32
      ButtonWidth = 126
      Caption = 'ToolBar'
      Color = clBtnFace
      ParentColor = False
      ShowCaptions = True
      TabOrder = 1
      ExplicitLeft = 406
      object tbImport: TToolButton
        Left = 0
        Top = 0
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Action = ACImport
      end
      object tbExport: TToolButton
        Left = 126
        Top = 0
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Action = ACExport
      end
    end
    object Panel1: TPanel
      Left = 5
      Top = 5
      Width = 565
      Height = 49
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitWidth = 663
      object Label1: TLabel
        Left = 7
        Top = 7
        Width = 103
        Height = 24
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'Texture size'
      end
      object Label2: TLabel
        Left = 231
        Top = 7
        Width = 20
        Height = 24
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = ' x '
      end
      object CBWidth: TComboBox
        Left = 126
        Top = 0
        Width = 99
        Height = 32
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Style = csDropDownList
        TabOrder = 0
        OnChange = CBWidthChange
        Items.Strings = (
          '1'
          '2'
          '4'
          '8'
          '16'
          '32'
          '64'
          '128'
          '256'
          '512'
          '1024'
          '2048'
          '4096')
      end
      object CBHeight: TComboBox
        Left = 259
        Top = 0
        Width = 99
        Height = 32
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Style = csDropDownList
        TabOrder = 1
        OnChange = CBWidthChange
        Items.Strings = (
          '1'
          '2'
          '4'
          '8'
          '16'
          '32'
          '64'
          '128'
          '256'
          '512'
          '1024'
          '2048'
          '4096')
      end
    end
  end
  object PAPreview: TPanel
    Left = 575
    Top = 0
    Width = 732
    Height = 812
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    BevelOuter = bvLowered
    BorderWidth = 4
    Caption = 'PAPreview'
    TabOrder = 1
    OnResize = PAPreviewResize
    ExplicitLeft = 680
    ExplicitWidth = 500
    object GLSceneViewer: TGLSceneViewer
      Left = 5
      Top = 5
      Width = 722
      Height = 802
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Camera = GLCamera
      Buffer.AntiAliasing = aa2x
      FieldOfView = 164.228927612304700000
      PenAsTouch = False
      Align = alClient
      OnMouseDown = GLSceneViewerMouseDown
      OnMouseMove = GLSceneViewerMouseMove
      TabOrder = 0
      ExplicitWidth = 596
    end
    object Panel2: TPanel
      Left = 14
      Top = 14
      Width = 449
      Height = 56
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      TabOrder = 1
      object CBTextureFiltering: TCheckBox
        Left = 196
        Top = 14
        Width = 169
        Height = 29
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'Texture filtering'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CBTextureFilteringClick
      end
      object CBBackground: TComboBox
        Left = 13
        Top = 11
        Width = 156
        Height = 32
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Style = csDropDownList
        TabOrder = 1
        OnChange = CBBackgroundChange
        Items.Strings = (
          'Checkered'
          'Black'
          'Gray'
          'White')
      end
    end
  end
  object MainMenu: TMainMenu
    Images = ImageList
    Left = 338
    Top = 86
    object File1: TMenuItem
      Caption = 'File'
      object Exit2: TMenuItem
        Action = ACOpenTexture
      end
      object SaveTexture1: TMenuItem
        Action = ACSaveTexture
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = ACExit
      end
    end
    object ools1: TMenuItem
      Caption = 'Tools'
      object Colormapdilatation1: TMenuItem
        Action = ACColorDilatation
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Alphamaperosion1: TMenuItem
        Action = ACAlphaErosion
      end
      object AlphamapDilatation1: TMenuItem
        Action = ACAlphaDilatation
      end
    end
    object Alpha1: TMenuItem
      Caption = 'Alpha'
      object GenerateAlpha1: TMenuItem
        Caption = 'Generate Alpha'
        object Opaque1: TMenuItem
          Action = ACOpaque
        end
        object SuperBlackTransparent1: TMenuItem
          Action = ACAlphaSuperBlack
        end
        object FromRGBIntensity1: TMenuItem
          Action = ACFromRGBIntensity
        end
        object FromRGBSqrtIntensity1: TMenuItem
          Action = ACFromRGBSqrtIntensity
        end
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Negate1: TMenuItem
        Action = ACAlphaNegate
      end
      object Offset1: TMenuItem
        Action = ACAlphaOffset
      end
      object Saturate1: TMenuItem
        Action = ACAlphaSaturate
      end
    end
  end
  object ImageList: TImageList
    Left = 48
    Top = 72
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000FFFF00000000000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000808000000000000000000000000000FFFFFF0000FFFF000000
      0000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000008080000080800000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000080800000808000008080000080800000808000008080000080
      8000008080000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      00000080800000808000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000808000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000C001FFFF00000000
      8031001F000000008031000F0000000080310007000000008001000300000000
      800100010000000080010000000000008FF1001F000000008FF1001F00000000
      8FF1001F000000008FF18FF1000000008FF1FFF9000000008FF5FF7500000000
      8001FF8F00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object ActionList: TActionList
    Images = ImageList
    Left = 16
    Top = 104
    object ACExit: TAction
      Category = 'File'
      Caption = 'Exit'
    end
    object ACImport: TAction
      Category = 'File'
      Caption = ' Import image '
      ImageIndex = 1
      OnExecute = ACImportExecute
    end
    object ACOpenTexture: TAction
      Category = 'File'
      Caption = 'Open Texture...'
      ImageIndex = 1
      OnExecute = ACOpenTextureExecute
    end
    object ACSaveTexture: TAction
      Category = 'File'
      Caption = 'Save Texture...'
      ImageIndex = 0
      OnExecute = ACSaveTextureExecute
    end
    object ACColorDilatation: TAction
      Category = 'Tools'
      Caption = 'Colormap Dilatation'
      OnExecute = ACColorDilatationExecute
    end
    object ACAlphaErosion: TAction
      Category = 'Tools'
      Caption = 'Alphamap Erosion'
      OnExecute = ACAlphaErosionExecute
    end
    object ACExport: TAction
      Category = 'File'
      Caption = ' Export image'
      OnExecute = ACExportExecute
    end
    object ACAlphaDilatation: TAction
      Category = 'Tools'
      Caption = 'Alphamap Dilatation'
      OnExecute = ACAlphaDilatationExecute
    end
    object ACOpaque: TAction
      Category = 'Alpha'
      Caption = 'Opaque'
      OnExecute = ACOpaqueExecute
    end
    object ACAlphaSuperBlack: TAction
      Category = 'Alpha'
      Caption = 'SuperBlack Transparent'
      OnExecute = ACAlphaSuperBlackExecute
    end
    object ACFromRGBIntensity: TAction
      Category = 'Alpha'
      Caption = 'From RGB Intensity'
      OnExecute = ACFromRGBIntensityExecute
    end
    object ACFromRGBSqrtIntensity: TAction
      Category = 'Alpha'
      Caption = 'From RGB Sqrt Intensity'
      OnExecute = ACFromRGBSqrtIntensityExecute
    end
    object ACAlphaOffset: TAction
      Category = 'Alpha'
      Caption = 'Offset'
      OnExecute = ACAlphaOffsetExecute
    end
    object ACAlphaSaturate: TAction
      Category = 'Alpha'
      Caption = 'Saturate'
      OnExecute = ACAlphaSaturateExecute
    end
    object ACAlphaNegate: TAction
      Category = 'Alpha'
      Caption = 'Negate'
      OnExecute = ACAlphaNegateExecute
    end
  end
  object GLScene: TGLScene
    Left = 168
    Top = 72
    object HSBkgnd: TGLHUDSprite
      Material.Texture.Image.Picture.Data = {
        07544269746D61704E000000424D4E000000000000003E000000280000000400
        000004000000010001000000000010000000130B0000130B0000020000000200
        000000000000FFFFFF003000000030000000C0000000C0000000}
      Material.Texture.MagFilter = maNearest
      Material.Texture.MinFilter = miNearest
      Material.Texture.TextureMode = tmReplace
      Material.Texture.Compression = tcNone
      Material.Texture.Disabled = False
      Position.Coordinates = {0000C8420000C842000000000000803F}
      Width = 100.000000000000000000
      Height = 100.000000000000000000
      Rotation = 0.000000000000000000
      XTiles = 2
      YTiles = 2
    end
    object GLDummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCube: TGLCube
        Material.BlendingMode = bmTransparency
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twNone
        Material.Texture.FilteringQuality = tfAnisotropic
      end
    end
    object GLLightSource: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00004842000020420000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube
      Position.Coordinates = {0000804000004040000000400000803F}
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 112
    Top = 72
  end
  object SaveDialog: TSaveDialog
    Filter = '32 bits BMP|*.bmp|32 bits TGA|*.tga'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 112
    Top = 104
  end
end
