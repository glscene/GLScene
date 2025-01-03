object FormViewer: TFormViewer
  Left = 0
  Top = 0
  Caption = 'GLSViewer'
  ClientHeight = 595
  ClientWidth = 785
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object StatusBar: TStatusBar
    Left = 0
    Top = 576
    Width = 785
    Height = 19
    Panels = <
      item
        Text = 'X'
        Width = 50
      end
      item
        Text = 'Y'
        Width = 50
      end
      item
        Text = 'Z'
        Width = 50
      end
      item
        Text = 'Model'
        Width = 40
      end>
    ExplicitTop = 424
    ExplicitWidth = 733
  end
  object snViewer: TGLSceneViewer
    Left = 153
    Top = 54
    Width = 632
    Height = 522
    Camera = Camera
    Buffer.BackgroundColor = clGray
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roTwoSideLighting]
    Buffer.FaceCulling = False
    FieldOfView = 158.310379028320300000
    PenAsTouch = False
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 581
    ExplicitHeight = 370
  end
  object ControlBar: TControlBar
    Left = 0
    Top = 25
    Width = 785
    Height = 29
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 733
    object atbTools: TActionToolBar
      Left = 333
      Top = 2
      Width = 65
      Height = 22
      ActionManager = ActionManager
      Caption = 'Tools'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 10461087
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Spacing = 0
    end
    object atbView: TActionToolBar
      Left = 102
      Top = 2
      Width = 218
      Height = 22
      ActionManager = ActionManager
      Caption = 'View'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 10461087
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Spacing = 0
    end
    object atbFile: TActionToolBar
      Left = 11
      Top = 2
      Width = 78
      Height = 22
      ActionManager = ActionManager
      Caption = 'File'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 10461087
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Spacing = 0
    end
  end
  object amMenuBar: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 785
    Height = 25
    UseSystemFont = False
    ActionManager = ActionManager
    Caption = 'ActionMainMenuBar'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 10461087
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Spacing = 0
    ExplicitWidth = 733
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 54
    Width = 153
    Height = 522
    Align = alLeft
    TabOrder = 4
    ExplicitHeight = 370
    object tvScene: TTreeView
      Left = 1
      Top = 1
      Width = 151
      Height = 520
      Align = alClient
      Images = ImageListObjects
      Indent = 19
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = tvSceneClick
      Items.NodeData = {
        071400000009540054007200650065004E006F00640065002B00000000000000
        00000000FFFFFFFFFFFFFFFF000000000000000000000000000106430061006D
        006500720061000000350000000100000001000000FFFFFFFFFFFFFFFF000000
        00000000000000000000010B4C00690067006800740053006F00750072006300
        65000000310000000200000002000000FFFFFFFFFFFFFFFF0000000000000000
        00000000000109440075006D006D007900430075006200650000003900000060
        00000060000000FFFFFFFFFFFFFFFF00000000000000000012000000010D4200
        6100730069006300470065006F006D00650074007200790000002B0000000300
        000003000000FFFFFFFFFFFFFFFF000000000000000000000000000106530070
        00720069007400650000002B0000000400000004000000FFFFFFFFFFFFFFFF00
        000000000000000000000000010650006F0069006E0074007300000029000000
        0500000005000000FFFFFFFFFFFFFFFF0000000000000000000000000001054C
        0069006E00650073000000290000000600000006000000FFFFFFFFFFFFFFFF00
        000000000000000000000000010550006C0061006E00650000002D0000000700
        000007000000FFFFFFFFFFFFFFFF00000000000000000000000000010750006F
        006C00790067006F006E000000270000000800000008000000FFFFFFFFFFFFFF
        FF000000000000000000000000000104430075006200650000002D0000000900
        000009000000FFFFFFFFFFFFFFFF000000000000000000000000000107460072
        0075007300740075006D0000002B0000000A0000000A000000FFFFFFFFFFFFFF
        FF00000000000000000000000000010653007000680065007200650000002700
        00000B0000000B000000FFFFFFFFFFFFFFFF0000000000000000000000000001
        044400690073006B000000270000000C0000000C000000FFFFFFFFFFFFFFFF00
        000000000000000000000000010443006F006E00650000002F0000000D000000
        0D000000FFFFFFFFFFFFFFFF000000000000000000000000000108430079006C
        0069006E0064006500720000002D0000000E0000000E000000FFFFFFFFFFFFFF
        FF000000000000000000000000000107430061007000730075006C0065000000
        370000000F0000000F000000FFFFFFFFFFFFFFFF000000000000000000000000
        00010C44006F00640065006300610068006500640072006F006E000000350000
        001000000010000000FFFFFFFFFFFFFFFF00000000000000000000000000010B
        490063006F007300610068006500640072006F006E0000003300000011000000
        11000000FFFFFFFFFFFFFFFF00000000000000000000000000010A4800650078
        00610068006500640072006F006E000000330000001200000012000000FFFFFF
        FFFFFFFFFF00000000000000000000000000010A4F0063007400610068006500
        640072006F006E000000350000001300000013000000FFFFFFFFFFFFFFFF0000
        0000000000000000000000010B5400650074007200610068006500640072006F
        006E0000003B0000001400000014000000FFFFFFFFFFFFFFFF00000000000000
        000000000000010E5300750070006500720065006C006C006900700073006F00
        6900640000003F0000006000000060000000FFFFFFFFFFFFFFFF000000000000
        00000009000000011041006400760061006E00630065006400470065006F006D
        00650074007200790000003B0000001500000015000000FFFFFFFFFFFFFFFF00
        000000000000000000000000010E41006E0069006D0061007400650064005300
        700072006900740065000000310000001600000016000000FFFFFFFFFFFFFFFF
        0000000000000000000000000001094100720072006F0077004C0069006E0065
        0000002F0000001700000017000000FFFFFFFFFFFFFFFF000000000000000000
        0000000001084100720072006F00770041007200630000002D00000018000000
        18000000FFFFFFFFFFFFFFFF00000000000000000000000000010741006E006E
        0075006C007500730000003B0000001900000019000000FFFFFFFFFFFFFFFF00
        000000000000000000000000010E45007800740072007500730069006F006E00
        53006F006C00690064000000370000001A0000001A000000FFFFFFFFFFFFFFFF
        00000000000000000000000000010C4D0075006C007400690050006F006C0079
        0067006F006E000000270000001B0000001B000000FFFFFFFFFFFFFFFF000000
        000000000000000000000104500069007000650000003D0000001C0000001C00
        0000FFFFFFFFFFFFFFFF00000000000000000000000000010F5200650076006F
        006C007500740069006F006E0053006F006C00690064000000290000001D0000
        001D000000FFFFFFFFFFFFFFFF00000000000000000000000000010554006F00
        7200750073000000350000006000000060000000FFFFFFFFFFFFFFFF00000000
        000000000006000000010B4D006500730068004F0062006A0065006300740073
        000000290000001E0000001E000000FFFFFFFFFFFFFFFF000000000000000000
        0000000001054100630074006F00720000002F0000001F0000001F000000FFFF
        FFFFFFFFFFFF000000000000000000000000000108460072006500650046006F
        0072006D000000270000002000000020000000FFFFFFFFFFFFFFFF0000000000
        000000000000000001044D006500730068000000310000002100000021000000
        FFFFFFFFFFFFFFFF000000000000000000000000000109540069006C00650050
        006C0061006E00650000002B0000002200000022000000FFFFFFFFFFFFFFFF00
        000000000000000000000000010650006F007200740061006C0000003D000000
        2300000023000000FFFFFFFFFFFFFFFF00000000000000000000000000010F54
        00650072007200610069006E00520065006E0064006500720065007200000037
        0000006000000060000000FFFFFFFFFFFFFFFF00000000000000000003000000
        010C470072006100700068004F0062006A00650063007400730000002F000000
        2400000024000000FFFFFFFFFFFFFFFF00000000000000000000000000010846
        006C006100740054006500780074000000350000002500000025000000FFFFFF
        FFFFFFFFFF00000000000000000000000000010B480065006900670068007400
        4600690065006C00640000002D0000002600000026000000FFFFFFFFFFFFFFFF
        000000000000000000000000000107580059005A00470072006900640000003D
        0000006000000060000000FFFFFFFFFFFFFFFF00000000000000000002000000
        010F5000610072007400690063006C006500530079007300740065006D007300
        0000310000002700000027000000FFFFFFFFFFFFFFFF00000000000000000000
        00000001095000610072007400690063006C0065007300000035000000280000
        0028000000FFFFFFFFFFFFFFFF00000000000000000000000000010B50004600
        5800720065006E00640065007200650072000000430000006000000060000000
        FFFFFFFFFFFFFFFF00000000000000000004000000011245006E007600690072
        006F006E006D0065006E0074004F0062006A0065006300740073000000370000
        002900000029000000FFFFFFFFFFFFFFFF00000000000000000000000000010C
        4500610072007400680053006B00790044006F006D00650000002D0000002A00
        00002A000000FFFFFFFFFFFFFFFF00000000000000000000000000010753006B
        00790044006F006D00650000002B0000002B0000002B000000FFFFFFFFFFFFFF
        FF00000000000000000000000000010653006B00790042006F00780000003300
        00002C0000002C000000FFFFFFFFFFFFFFFF0000000000000000000000000001
        0A410074006D006F007300700068006500720065000000330000006000000060
        000000FFFFFFFFFFFFFFFF00000000000000000006000000010A480055004400
        6F0062006A0065006300740073000000310000002D0000002D000000FFFFFFFF
        FFFFFFFF00000000000000000000000000010948005500440073007000720069
        007400650000002D0000002E0000002E000000FFFFFFFFFFFFFFFF0000000000
        0000000000000000010748005500440074006500780074000000430000002F00
        00002F000000FFFFFFFFFFFFFFFF000000000000000000000000000112480055
        004400740065007800740049006E0064006500700065006E00640061006E0074
        0000003F0000003000000030000000FFFFFFFFFFFFFFFF000000000000000000
        00000000011048005500440074006500780074004100620073006F006C007500
        74006500640000002F0000003100000031000000FFFFFFFFFFFFFFFF00000000
        0000000000000000000108470061006D0065004D0065006E00750000002D0000
        003200000032000000FFFFFFFFFFFFFFFF000000000000000000000000000107
        43006F006E0073006F006C0065000000330000006000000060000000FFFFFFFF
        FFFFFFFF0000000000000000000C000000010A4700550049006F0062006A0065
        006300740073000000350000003300000033000000FFFFFFFFFFFFFFFF000000
        00000000000000000000010B52006F006F00740043006F006E00740072006F00
        6C000000350000003400000034000000FFFFFFFFFFFFFFFF0000000000000000
        0000000000010B47004C0050006F007000750070004D0065006E00750000002B
        0000003500000035000000FFFFFFFFFFFFFFFF00000000000000000000000000
        010647004C0046006F0072006D0000002D0000003600000036000000FFFFFFFF
        FFFFFFFF00000000000000000000000000010747004C00500061006E0065006C
        0000002F0000003700000037000000FFFFFFFFFFFFFFFF000000000000000000
        00000000010847004C0042007500740074006F006E0000003300000038000000
        38000000FFFFFFFFFFFFFFFF00000000000000000000000000010A47004C0043
        006800650063006B0042006F00780000002B0000003900000039000000FFFFFF
        FFFFFFFFFF00000000000000000000000000010647004C004500640069007400
        00002D0000003A0000003A000000FFFFFFFFFFFFFFFF00000000000000000000
        000000010747004C004C006100620065006C0000003D0000003B0000003B0000
        00FFFFFFFFFFFFFFFF00000000000000000000000000010F47004C0041006400
        760061006E006300650064004C006100620065006C000000350000003C000000
        3C000000FFFFFFFFFFFFFFFF00000000000000000000000000010B47004C0053
        00630072006F006C006C006200610072000000370000003D0000003D000000FF
        FFFFFFFFFFFFFF00000000000000000000000000010C47004C00530074007200
        69006E006700470072006900640000003D0000003E0000003E000000FFFFFFFF
        FFFFFFFF00000000000000000000000000010F47004C004200690074006D0061
        00700043006F006E00740072006F006C0000003B0000006000000060000000FF
        FFFFFFFFFFFFFF0000000000000000000F000000010E53007000650063006900
        61006C004F0062006A0065006300740073000000310000003F0000003F000000
        FFFFFFFFFFFFFFFF0000000000000000000000000001094C0065006E00730046
        006C0061007200650000003F0000004000000040000000FFFFFFFFFFFFFFFF00
        000000000000000000000000011054006500780074007500720065004C006500
        6E00730046006C0061007200650000002B0000004100000041000000FFFFFFFF
        FFFFFFFF0000000000000000000000000001064D006900720072006F00720000
        00350000004200000042000000FFFFFFFFFFFFFFFF0000000000000000000000
        0000010B53006800610064006F00770050006C0061006E006500000037000000
        4300000043000000FFFFFFFFFFFFFFFF00000000000000000000000000010C53
        006800610064006F00770056006F006C0075006D00650000002F000000440000
        0044000000FFFFFFFFFFFFFFFF0000000000000000000000000001085A005300
        6800610064006F00770073000000430000004500000045000000FFFFFFFFFFFF
        FFFF00000000000000000000000000011267006C0073006C0054006500780074
        0075007200650045006D00690074007400650072000000490000004600000046
        000000FFFFFFFFFFFFFFFF00000000000000000000000000011567006C007300
        6C00500072006F006A0065006300740065006400540065007800740075007200
        6500730000003B0000004700000047000000FFFFFFFFFFFFFFFF000000000000
        00000000000000010E540065007800740075007200650045006D006900740074
        00650072000000410000004800000048000000FFFFFFFFFFFFFFFF0000000000
        00000000000000000111500072006F006A006500630074006500640054006500
        7800740075007200650073000000270000004900000049000000FFFFFFFFFFFF
        FFFF00000000000000000000000000010442006C00750072000000330000004A
        0000004A000000FFFFFFFFFFFFFFFF00000000000000000000000000010A4D00
        6F00740069006F006E0042006C007500720000002D0000004B0000004B000000
        FFFFFFFFFFFFFFFF00000000000000000000000000010747004C005400720061
        0069006C000000330000004C0000004C000000FFFFFFFFFFFFFFFF0000000000
        0000000000000000010A50006F00730074004500660066006500630074000000
        3F0000004D0000004D000000FFFFFFFFFFFFFFFF000000000000000000000000
        00011050006F007300740053006800610064006500720048006F006C00640065
        0072000000390000006000000060000000FFFFFFFFFFFFFFFF00000000000000
        000004000000010D44006F006F006400610064004F0062006A00650063007400
        73000000310000004E0000004E000000FFFFFFFFFFFFFFFF0000000000000000
        0000000000010953007000610063006500540065007800740000002B0000004F
        0000004F000000FFFFFFFFFFFFFFFF0000000000000000000000000001065400
        6500610070006F0074000000270000005000000050000000FFFFFFFFFFFFFFFF
        0000000000000000000000000001045400720065006500000033000000510000
        0051000000FFFFFFFFFFFFFFFF00000000000000000000000000010A57006100
        74006500720050006C0061006E00650000003700000060000000FFFFFFFFFFFF
        FFFFFFFFFFFF00000000000000000007000000010C500072006F00780079004F
        0062006A0065006300740073000000350000005200000052000000FFFFFFFFFF
        FFFFFF00000000000000000000000000010B500072006F00780079004F006200
        6A006500630074000000330000005300000053000000FFFFFFFFFFFFFFFF0000
        0000000000000000000000010A43006F006C006F007200500072006F00780079
        000000390000005400000054000000FFFFFFFFFFFFFFFF000000000000000000
        00000000010D460072006500650046006F0072006D00500072006F0078007900
        0000390000005500000055000000FFFFFFFFFFFFFFFF00000000000000000000
        000000010D4D006100740065007200690061006C00500072006F007800790000
        00330000005600000056000000FFFFFFFFFFFFFFFF0000000000000000000000
        0000010A4100630074006F007200500072006F00780079000000330000005700
        000057000000FFFFFFFFFFFFFFFF00000000000000000000000000010A4D0075
        006C0074006900500072006F00780079000000430000005800000058000000FF
        FFFFFFFFFFFFFF0000000000000000000000000001124E006100740065007200
        690061006C004D0075006C0074006900500072006F0078007900000037000000
        5900000059000000FFFFFFFFFFFFFFFF00000000000000000000000000010C44
        00690072006500630074004F00700065006E0047004C000000350000005A0000
        005A000000FFFFFFFFFFFFFFFF00000000000000000000000000010B52006500
        6E0064006500720050006F0069006E00740000003B0000005B0000005B000000
        FFFFFFFFFFFFFFFF00000000000000000000000000010E49006D0070006F0073
        007400650072005300700072006900740065000000350000005C0000005C0000
        00FFFFFFFFFFFFFFFF00000000000000000000000000010B4F0047004C004600
        6500650064006200610063006B0000003B0000005D0000005D000000FFFFFFFF
        FFFFFFFF00000000000000000000000000010E4F0047004C004600720061006D
        0065004200750066006600650072000000390000006000000060000000FFFFFF
        FFFFFFFFFF00000000000000000001000000010D430055004400410043006F00
        6D0070007500740069006E0067000000410000005E0000005E000000FFFFFFFF
        FFFFFFFF0000000000000000000000000001114300550044004100470065006E
        006500720061007400650064004D00650073006800}
      ExplicitHeight = 368
    end
  end
  object Scene: TGLScene
    Left = 24
    Top = 96
    object Camera: TGLCamera
      DepthOfView = 10000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = dcObject
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {0000000000000040000000400000803F}
      object LightSource: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsParallel
        Specular.Color = {9A99193F9A99193F9A99193F0000803F}
        SpotCutOff = 180.000000000000000000
        SpotDirection.Coordinates = {3ACD133F3ACD133F3ACD133F00000000}
      end
    end
    object XYZGrid: TGLXYZGrid
      XSamplingScale.Min = -10.000000000000000000
      XSamplingScale.Max = 10.000000000000000000
      XSamplingScale.Step = 1.000000000000000000
      YSamplingScale.Step = 0.100000001490116100
      ZSamplingScale.Min = -10.000000000000000000
      ZSamplingScale.Max = 10.000000000000000000
      ZSamplingScale.Step = 1.000000000000000000
      Parts = [gpX, gpZ]
    end
    object CubeLines: TGLCube
      Material.BackProperties.Ambient.Color = {0000000000000000000000000000803F}
      Material.BackProperties.Diffuse.Color = {0C026B3F0C026B3F0000803F0000803F}
      Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0C026B3F0C026B3F0000803F0000803F}
      Material.MaterialOptions = [moNoLighting]
      Material.FaceCulling = fcNoCull
      Material.PolygonMode = pmLines
    end
    object dcObject: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object ffObject: TGLFreeForm
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      AutoCentering = [macCenterX, macCenterY, macCenterZ, macUseBarycenter]
      MaterialLibrary = MaterialLib
    end
    object dcAxis: TGLDummyCube
      Direction.Coordinates = {000000000000803F0000000000000000}
      ShowAxes = True
      Up.Coordinates = {0000000000000000000080BF00000000}
      CubeSize = 1.000000000000000000
    end
    object dcWorld: TGLDummyCube
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
      object GLPoints: TGLPoints
        NoZWrite = False
        Static = False
      end
    end
  end
  object MaterialLib: TGLMaterialLibrary
    Left = 296
    Top = 544
  end
  object Cadencer: TGLCadencer
    Scene = Scene
    Enabled = False
    Mode = cmApplicationIdle
    Left = 32
    Top = 152
  end
  object Timer: TTimer
    Enabled = False
    Left = 1047
    Top = 110
  end
  object ActionManager: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = acFileOpen
                ImageIndex = 0
              end
              item
                Action = acFilePick
                Caption = '&Pick texture...'
                ImageIndex = 1
              end
              item
                Action = acFileOpenTexLib
                Caption = 'Op&en Texture Library...'
              end
              item
                Action = acFileSaveAs
                Caption = '&Save As...'
                ImageIndex = 4
              end
              item
                Action = acFileSaveTextures
                Caption = 'S&ave Textures...'
              end
              item
                Caption = '-'
              end
              item
                Action = acSaveTreeView
                Caption = 'Sa&veTreeView...'
              end
              item
                Action = acLoadTreeView
              end
              item
                Action = acFileExit
                Caption = 'E&xit'
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = acEditUndo
                ImageIndex = 18
                ShortCut = 16474
              end
              item
                Caption = '-'
              end
              item
                Action = acEditCut
                ImageIndex = 15
                ShortCut = 16472
              end
              item
                Action = acEditCopy
                ImageIndex = 16
                ShortCut = 16451
              end
              item
                Action = acEditPaste
                ImageIndex = 17
                ShortCut = 16470
              end
              item
                Action = acEditSelectAll
                ShortCut = 16449
              end
              item
                Action = acEditDelete
                ImageIndex = 19
                ShortCut = 46
              end>
            Caption = '&Edit'
          end
          item
            Items = <
              item
                Action = acPoints
                Caption = '&Points'
                ImageIndex = 21
              end
              item
                Action = acSpheres
                Caption = 'Sph&eres'
              end
              item
                Caption = '-'
              end
              item
                Action = acViewSmoothShading
                ImageIndex = 9
              end
              item
                Action = acViewFlatShading
                Caption = '&Flat Shading'
                ImageIndex = 7
              end
              item
                Action = acViewFlatLines
                Caption = 'F&lat Shading with Lines'
                ImageIndex = 11
              end
              item
                Action = acViewHiddenLines
                Caption = '&Hidden Lines'
                ImageIndex = 8
              end
              item
                Action = acViewWireFrame
                Caption = '&Wireframe'
                ImageIndex = 6
              end
              item
                Caption = '-'
              end
              item
                Action = acViewZoomIn
                Caption = '&Zoom In'
                ImageIndex = 2
              end
              item
                Action = acViewZoomOut
                Caption = 'Z&oom Out'
                ImageIndex = 3
              end
              item
                Action = acViewReset
                Caption = '&Reset View'
                ImageIndex = 5
              end
              item
                Caption = '-'
              end>
            Caption = '&View'
          end
          item
            Items = <
              item
                Action = acToolsOptions
                Caption = '&Options...'
              end
              item
                Action = acToolsFaceCulling
                Caption = '&Face Culling'
              end
              item
                Action = acToolsTexturing
                Caption = '&Texturing'
                ImageIndex = 10
              end
              item
                Action = acToolsLighting
                Caption = '&Lighting'
                ImageIndex = 12
              end
              item
                Action = acToolsNaviCube
                Caption = '&Navi Cube'
                ImageIndex = 20
              end
              item
                Action = acToolsCustomize
              end
              item
                Action = acToolsShowFPS
                Caption = '&Show FPS'
              end>
            Caption = '&Tools'
          end
          item
            Items = <
              item
                Action = acHelpContents
                ImageIndex = 13
              end
              item
                Action = acHelpTopicSearch
                ImageIndex = 14
              end
              item
                Action = acHelpGLSHomePage
                Caption = '&GLScene Home Page'
              end
              item
                Caption = '-'
              end
              item
                Action = acHelpAbout
                Caption = '&About...'
              end>
            Caption = '&Help'
          end>
        ActionBar = amMenuBar
      end
      item
      end
      item
      end
      item
      end
      item
        Items = <
          item
            Action = acFileOpen
            ImageIndex = 0
            ShowCaption = False
          end
          item
            Action = acFilePick
            Caption = '&Pick texture...'
            ImageIndex = 1
            ShowCaption = False
          end
          item
            Action = acFileSaveAs
            Caption = '&Save As...'
            ImageIndex = 4
            ShowCaption = False
          end>
      end
      item
      end
      item
        Items = <
          item
            Action = acViewSmoothShading
            ImageIndex = 9
            ShowCaption = False
          end
          item
            Action = acViewFlatShading
            Caption = '&Flat Shading'
            ImageIndex = 7
            ShowCaption = False
          end
          item
            Action = acViewFlatLines
            Caption = 'F&lat Shading with Lines'
            ImageIndex = 11
            ShowCaption = False
          end
          item
            Action = acViewHiddenLines
            Caption = '&Hidden Lines'
            ImageIndex = 8
            ShowCaption = False
          end
          item
            Action = acViewWireFrame
            Caption = '&Wireframe'
            ImageIndex = 6
            ShowCaption = False
          end
          item
            Caption = '-'
          end
          item
            Action = acViewZoomIn
            Caption = '&Zoom In'
            ImageIndex = 2
            ShowCaption = False
          end
          item
            Action = acViewZoomOut
            Caption = 'Z&oom Out'
            ImageIndex = 3
            ShowCaption = False
          end
          item
            Action = acViewReset
            Caption = '&Reset View'
            ImageIndex = 5
            ShowCaption = False
          end>
      end
      item
      end
      item
        Items = <
          item
            Action = acToolsTexturing
            Caption = '&Texturing'
            ImageIndex = 10
            ShowCaption = False
          end
          item
            Action = acToolsLighting
            Caption = '&Lighting'
            ImageIndex = 12
            ShowCaption = False
          end>
      end
      item
      end
      item
      end
      item
      end
      item
      end
      item
        Items = <
          item
            Items = <
              item
                Action = acFileOpen
                ImageIndex = 0
              end
              item
                Action = acFilePick
                Caption = '&Pick texture...'
                ImageIndex = 1
              end
              item
                Caption = '-'
              end
              item
                Action = acFileOpenTexLib
                Caption = 'Op&en Texture Library...'
              end
              item
                Action = acFileSaveAs
                Caption = '&Save As...'
                ImageIndex = 4
              end
              item
                Action = acFileSaveTextures
                Caption = 'S&ave Textures...'
              end
              item
                Caption = '-'
              end
              item
                Action = acFileExit
                Caption = 'E&xit'
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = acEditUndo
                ImageIndex = 18
                ShortCut = 16474
              end
              item
                Caption = '-'
              end
              item
                Action = acEditCut
                ImageIndex = 15
                ShortCut = 16472
              end
              item
                Action = acEditCopy
                ImageIndex = 16
                ShortCut = 16451
              end
              item
                Action = acEditPaste
                ImageIndex = 17
                ShortCut = 16470
              end
              item
                Action = acEditSelectAll
                ShortCut = 16449
              end
              item
                Action = acEditDelete
                ImageIndex = 19
                ShortCut = 46
              end>
            Caption = '&Edit'
          end
          item
            Items = <
              item
                Action = acViewSmoothShading
                ImageIndex = 9
              end
              item
                Action = acViewFlatShading
                Caption = '&Flat Shading'
                ImageIndex = 7
              end
              item
                Action = acViewFlatLines
                Caption = 'F&lat Shading with Lines'
                ImageIndex = 11
              end
              item
                Action = acViewHiddenLines
                Caption = '&Hidden Lines'
                ImageIndex = 8
              end
              item
                Action = acViewWireFrame
                Caption = '&Wireframe'
                ImageIndex = 6
              end
              item
                Caption = '-'
              end
              item
                Action = acViewZoomIn
                Caption = '&Zoom In'
                ImageIndex = 2
              end
              item
                Action = acViewZoomOut
                Caption = 'Z&oom Out'
                ImageIndex = 3
              end
              item
                Action = acViewReset
                Caption = '&Reset View'
                ImageIndex = 5
              end>
            Caption = '&View'
          end
          item
            Items = <
              item
                Items = <
                  item
                    Action = acAADefault
                    Caption = '&Default'
                  end
                  item
                    Action = acAA2X
                    Caption = '&MSAA 2X'
                  end
                  item
                    Action = acAA4X
                    Caption = 'M&SAA 4X'
                  end
                  item
                    Action = acAA8X
                    Caption = 'MS&AA 8X'
                  end
                  item
                    Action = acAA16X
                    Caption = 'MSAA &16X'
                  end
                  item
                    Action = acCSA8X
                    Caption = '&CSAA 8X'
                  end
                  item
                    Action = acCSA16X
                    Caption = 'CSAA 1&6X'
                  end>
                Caption = '&Antialiasing'
              end
              item
                Action = acOptimizeMesh
                Caption = '&Optimize Mesh'
              end
              item
                Action = acProcessInvertNormals
                Caption = '&Invert Normals'
              end
              item
                Action = acReverseRendering
                Caption = '&Reverse Rendering Order'
              end
              item
                Action = acConvertToTriangles
                Caption = '&Convert to Indexed Triangles'
              end
              item
                Action = acProcessStripify
                Caption = '&Stripify'
              end>
            Caption = '&Processing'
          end
          item
            Items = <
              item
                Action = acToolsOptions
                Caption = '&Options...'
              end
              item
                Action = acToolsFaceCulling
                Caption = '&Face Culling'
              end
              item
                Action = acToolsTexturing
                Caption = '&Texturing'
                ImageIndex = 10
              end
              item
                Action = acToolsLighting
                Caption = '&Lighting'
                ImageIndex = 12
              end
              item
                Action = acToolsCustomize
              end
              item
                Caption = '-'
              end
              item
                Action = acToolsShowFPS
                Caption = '&Show FPS'
              end>
            Caption = '&Tools'
          end
          item
            Items = <
              item
                Action = acHelpContents
                ImageIndex = 13
              end
              item
                Action = acHelpTopicSearch
                ImageIndex = 14
              end
              item
                Action = acHelpGLSHomePage
                Caption = '&GLScene Home Page'
              end
              item
                Caption = '-'
              end
              item
                Action = acHelpAbout
                Caption = '&About...'
              end>
            Caption = '&Help'
          end>
      end
      item
      end
      item
        Items = <
          item
            Action = acFileOpen
            ImageIndex = 0
          end
          item
            Action = acFileSaveAs
            Caption = '&Save As...'
            ImageIndex = 4
          end>
      end
      item
        Items = <
          item
            Action = acFileOpen
            ImageIndex = 0
          end>
      end
      item
        Items = <
          item
            Action = acFileExit
            Caption = 'E&xit'
          end
          item
            Action = acFileSaveTextures
            Caption = 'S&ave Textures...'
          end
          item
            Action = acFileSaveAs
            Caption = '&Save As...'
            ImageIndex = 4
          end
          item
            Action = acFileOpenTexLib
            Caption = 'Op&en Texture Library...'
          end
          item
            Action = acFilePick
            Caption = '&Pick texture...'
            ImageIndex = 1
          end
          item
            Action = acFileOpen
            ImageIndex = 0
          end>
      end
      item
      end
      item
        Items = <
          item
            Action = acToolsTexturing
            Caption = '&Texturing'
            ImageIndex = 10
            ShowCaption = False
          end
          item
            Action = acToolsLighting
            Caption = '&Lighting'
            ImageIndex = 12
            ShowCaption = False
          end
          item
            Action = acToolsInfo
            Caption = '&Info...'
            ImageIndex = 22
            ShowCaption = False
          end>
        ActionBar = atbTools
      end
      item
      end
      item
        Items = <
          item
            Action = acViewSmoothShading
            ImageIndex = 9
            ShowCaption = False
          end
          item
            Action = acViewFlatShading
            Caption = '&Flat Shading'
            ImageIndex = 7
            ShowCaption = False
          end
          item
            Action = acViewHiddenLines
            Caption = '&Hidden Lines'
            ImageIndex = 8
            ShowCaption = False
          end
          item
            Action = acViewWireFrame
            Caption = '&Wireframe'
            ImageIndex = 6
            ShowCaption = False
          end
          item
            Action = acViewFlatLines
            Caption = 'F&lat Shading with Lines'
            ImageIndex = 11
            ShowCaption = False
          end
          item
            Caption = '-'
          end
          item
            Action = acViewZoomIn
            Caption = '&Zoom In'
            ImageIndex = 2
            ShowCaption = False
          end
          item
            Action = acViewZoomOut
            Caption = 'Z&oom Out'
            ImageIndex = 3
            ShowCaption = False
          end
          item
            Action = acViewReset
            Caption = '&Reset View'
            ImageIndex = 5
            ShowCaption = False
          end
          item
            Action = acToolsNaviCube
            Caption = '&Navi Cube'
            ImageIndex = 20
            ShowCaption = False
          end>
        ActionBar = atbView
      end
      item
        Items = <
          item
            Action = acFileOpen
            ImageIndex = 0
            ShowCaption = False
          end
          item
            Action = acFilePick
            Caption = '&Pick texture...'
            ImageIndex = 1
            ShowCaption = False
          end
          item
            Action = acFileSaveAs
            Caption = '&Save As...'
            ImageIndex = 4
            ShowCaption = False
          end>
        ActionBar = atbFile
      end>
    Images = ImageListMenu
    Left = 297
    Top = 289
    StyleName = 'Standard'
    object acOptimizeMesh: TAction
      Category = 'Processing'
      Caption = 'Optimize Mesh'
      Hint = 'Optimize mesh'
    end
    object acProcessInvertNormals: TAction
      Category = 'Processing'
      Caption = 'Invert Normals'
      Hint = 'Invert normals'
    end
    object acReverseRendering: TAction
      Category = 'Processing'
      Caption = 'Reverse Rendering Order'
      Hint = 'Reverse rendering order'
    end
    object acConvertToTriangles: TAction
      Category = 'Processing'
      Caption = 'Convert to Indexed Triangles'
      Hint = 'Convert  to indexed triangles'
    end
    object acProcessStripify: TAction
      Category = 'Processing'
      Caption = 'Stripify'
      Hint = 'Stripify'
    end
    object acToolsOptions: TAction
      Category = 'Tools'
      Caption = 'Options...'
      Hint = 'Tools|Options...'
    end
    object acToolsFaceCulling: TAction
      Category = 'Tools'
      Caption = 'Face Culling'
    end
    object acToolsTexturing: TAction
      Category = 'Tools'
      Caption = 'Texturing'
      Checked = True
      Hint = 'Texturing'
      ImageIndex = 10
    end
    object acToolsLighting: TAction
      Category = 'Tools'
      Caption = 'Lighting'
      Checked = True
      Hint = 'Lighting'
      ImageIndex = 12
    end
    object acToolsNaviCube: TAction
      Category = 'Tools'
      Caption = 'Navi Cube'
      Hint = 'Navi Cube'
      ImageIndex = 20
    end
    object acToolsCustomize: TCustomizeActionBars
      Category = 'Tools'
      Caption = '&Customize...'
      CustomizeDlg.StayOnTop = False
    end
    object acToolsShowFPS: TAction
      Category = 'Tools'
      Caption = 'Show FPS'
    end
    object acViewSmoothShading: TAction
      Category = 'View'
      AutoCheck = True
      Caption = '&Smooth Shading'
      GroupIndex = 1
      HelpType = htContext
      Hint = 'Smooth shading|Smooth shading'
      ImageIndex = 9
    end
    object acViewFlatShading: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Flat Shading'
      GroupIndex = 1
      HelpType = htContext
      Hint = 'Flat Shading|Flat Shading'
      ImageIndex = 7
    end
    object acViewFlatLines: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Flat Shading with Lines'
      GroupIndex = 1
      HelpType = htContext
      Hint = 'Flat Shading with Lines'
      ImageIndex = 11
    end
    object acViewHiddenLines: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Hidden Lines'
      GroupIndex = 1
      HelpType = htContext
      Hint = 'Hidden Lines'
      ImageIndex = 8
    end
    object acViewWireFrame: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Wireframe'
      GroupIndex = 1
      HelpType = htContext
      Hint = 'Wireframe'
      ImageIndex = 6
    end
    object acViewZoomIn: TAction
      Category = 'View'
      Caption = 'Zoom In'
      Hint = 'Zoom In'
      ImageIndex = 2
    end
    object acViewZoomOut: TAction
      Category = 'View'
      Caption = 'Zoom Out'
      Hint = 'Zoom Out'
      ImageIndex = 3
    end
    object acViewReset: TAction
      Category = 'View'
      Caption = 'Reset View'
      Hint = 'Reset View'
      ImageIndex = 5
    end
    object acFileOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      HelpType = htContext
      Hint = 'Open'
      ImageIndex = 0
    end
    object acFilePick: TAction
      Category = 'File'
      Caption = 'Pick texture...'
      Hint = 'Pick texture'
      ImageIndex = 1
    end
    object acFileOpenTexLib: TAction
      Category = 'File'
      Caption = 'Open Texture Library...'
      Hint = 'Open texture library'
    end
    object acFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save As...'
      Hint = 'Save as'
      ImageIndex = 4
    end
    object acFileSaveTextures: TAction
      Category = 'File'
      Caption = 'Save Textures...'
      Enabled = False
      Hint = 'Save textures...'
    end
    object acSaveTreeView: TAction
      Category = 'File'
      Caption = '&SaveTreeView...'
    end
    object acLoadTreeView: TAction
      Category = 'File'
      Caption = '&LoadTreeView...'
    end
    object acFileExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit'
    end
    object acHelpContents: THelpContents
      Category = 'Help'
      Caption = '&Contents'
      Enabled = False
      Hint = 'Contents|Help Contents'
      ImageIndex = 13
    end
    object acHelpTopicSearch: THelpTopicSearch
      Category = 'Help'
      Caption = '&Topic Search'
      Enabled = False
      Hint = 'Help|Topic Search'
      ImageIndex = 14
    end
    object acHelpGLSHomePage: TAction
      Category = 'Help'
      Caption = 'GLScene Home Page'
      Hint = 'Help|Home page'
    end
    object acHelpAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      Hint = 'Help|About...'
    end
    object acAADefault: TAction
      Category = 'Antialiasing'
      AutoCheck = True
      Caption = 'Default'
      Checked = True
      GroupIndex = 2
      Hint = 'Default|Default antialiacing'
    end
    object acAA2X: TAction
      Category = 'Antialiasing'
      AutoCheck = True
      Caption = 'MSAA 2X'
      GroupIndex = 2
    end
    object acAA4X: TAction
      Category = 'Antialiasing'
      AutoCheck = True
      Caption = 'MSAA 4X'
      GroupIndex = 2
    end
    object acEditUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts'
      ImageIndex = 18
      ShortCut = 16474
    end
    object acEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cut the selection and puts it on the Clipboard'
      ImageIndex = 15
      ShortCut = 16472
    end
    object acEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 16
      ShortCut = 16451
    end
    object acEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 17
      ShortCut = 16470
    end
    object acEditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object acEditDelete: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 19
      ShortCut = 46
    end
    object acAA8X: TAction
      Category = 'Antialiasing'
      Caption = 'MSAA 8X'
      GroupIndex = 2
    end
    object acAA16X: TAction
      Category = 'Antialiasing'
      AutoCheck = True
      Caption = 'MSAA 16X'
      GroupIndex = 2
    end
    object acCSA8X: TAction
      Category = 'Antialiasing'
      Caption = 'CSAA 8X'
      GroupIndex = 2
    end
    object acCSA16X: TAction
      Category = 'Antialiasing'
      Caption = 'CSAA 16X'
      GroupIndex = 2
    end
    object acPoints: TAction
      Category = 'View'
      Caption = 'Points'
      Hint = 'Points'
      ImageIndex = 21
    end
    object acToolsInfo: TAction
      Category = 'Tools'
      Caption = 'Info...'
      Hint = 'Info of objects'
      ImageIndex = 22
    end
    object acSpheres: TAction
      Category = 'View'
      Caption = 'Spheres'
    end
  end
  object ImageListMenu: TImageList
    Left = 792
    Top = 106
    Bitmap = {
      494C01011700F801040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000006000000001002000000000000060
      000000000000000000000000000000000000000000007FC8F70089CDF70092D0
      F8009DD5F8002BF6EF002BF6EF002BF6EF002BF6EF002BF6EF002BF6EF00D8EE
      FD00E1F3FD00E6F5FE00EDF7FE00F3F9FE000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006DC1F60000880000008800000088
      00002BF6EF002BF6EF002BF6EF009CDFF900B4E0FA002BF6EF002BF6EF002BF6
      EF00000088000000880000008800EDF7FE000000000000000000C0C0C0008080
      000080800000C0C0C000C0C0C000C0C0C0008080000080800000000000000000
      0000C0C0C000C0C0C000C0C0C000C0C0C0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000064BDF50000880000008800002BF6
      EF002BF6EF008ECEF70098D3F800A2D7F900ACDCFA00ACE0F900BFE4FB002BF6
      EF002BF6EF000000880000008800E7F5FE0000000000C0C0C00000000000FFFF
      0000FF00000080800000C0C0C00080800000FFFF0000FF00000000000000C0C0
      C00000000000C0C0C000C0C0C000C0C0C0000000000000000000000000000000
      000000000000808000008080000080800000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005CB9F50000880000008800000088
      00007CC8F6006FD5F50090D0F70099D4F800A3D8F900ACDCFA00B7E0FA00BFE3
      FB00000088000000880000008800E1F3FD0000000000C0C0C000808000000000
      0000FFFF000080800000C0C0C00080800000FFFF0000FFFF000000000000C0C0
      C000C0C0C00000000000C0C0C000C0C0C0000000000000000000000000000000
      00000000000080800000808000000000000080808000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000052B5F400008800002BF6EF000088
      0000008800007AC6F70085CBF7008FCFF80099D4F800A2D7F900ACDCFA000000
      8800000088002BF6EF0000008800D9EFFD0000000000C0C0C000C0C0C000FFFF
      000000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C00000000000C0C0
      C000C0C0C000C0C0C00000000000C0C0C0000000000000000000000000000000
      000000000000FFFF000080800000808000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000049B2F3002BF6EF002BF6EF005EBA
      F50000880000008800007CC8F60086CBF70090D0F80099D4F800000088000000
      8800B7E0FA002BF6EF002BF6EF00D2ECFC0000000000C0C0C000C0C0C000C0C0
      C000C0C0C00000000000C0C0C0000000FF000000FF00C0C0C00000000000C0C0
      C000FFFF0000FF000000C0C0C000000000000000000000000000000000000000
      00000000000000000000FFFF0000808000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000041ADF3002BF6EF0048BBF20054B5
      F4005EBBF50000880000008800007BC6F70086CBF7000000880000008800A2D7
      F900ADDCFA0097E4F8002BF6EF00C9E7FC0000000000C0C0C000C0C0C000C0C0
      C000C0C0C000000000000000FF000000FF000000FF000000FF0000000000C0C0
      C000FFFF0000FFFF0000C0C0C000000000000000000000000000000000000000
      00000000000000000000FFFF0000808000008080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003AABF2002BF6EF0043AFF2004CB3
      F30055B7F3005EBAF5000088000000880000000088000000880091D0F8009AD4
      F900A4D9F9009FDFF9002BF6EF00C2E5FB0000000000C0C0C000808000008080
      0000C0C0C000000000000000FF000000FF000000FF000000FF00000000008080
      000080800000C0C0C000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000FFFF00008080000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000031A7F2002BF6EF003BABF10041AE
      F3004CB2F30054B5F4005EBBF50088000000880000007BC6F70086CBF7008FCF
      F8009AD2F500A3D7F9002BF6EF00B9E1FB000000000080800000FF000000FFFF
      00008080000000000000C0C0C0000000FF000000FF00C0C0C00000000000FFFF
      0000FFFF000080800000C0C0C000000000000000000000000000000000000000
      000000000000FFFFFF0080808000000000008080000080800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002BA5F1002BF6EF002BF6EF003DAB
      F00044B0F2004BB3F30056B7F400880000008800000072C3F6007DC8F60087CB
      F40091D0F8002BF6EF002BF6EF00B1DEFA000000000080800000FFFF0000FFFF
      00008080000000000000C0C0C000C0C0C000C0C0C000C0C0C00000000000FFFF
      0000FF00000080800000C0C0C000000000000000000000000000000000000000
      00000000000000000000FFFF0000808000008080000080800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000024A1F10020A0F1002BF6EF002BF6
      EF004FA9E10042AEF3004CB3F300880000008800000068BEF50078BFEC007CC6
      F7002BF6EF002BF6EF0084DBF700A6D9F9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      000080800000C0C0C000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000FFFF00008080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001E9FF0001A9EF00025B3F0002BF6
      EF002BF6EF0041AAED0045B0F200880000008800000068B8EA0069C0F5002BF6
      EF002BF6EF0087CCF70091D1F8009ED6F800C0C0C00000000000C0C0C000FFFF
      0000FF00000000000000C0C0C000FFFF0000FFFF0000C0C0C000C0C0C0000000
      0000C0C0C000C0C0C000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000189CF000149AF0001B9EF00024BE
      F0002BF6EF002BF6EF002BF6EF0088000000880000002BF6EF002BF6EF002DF4
      EF004FDCF3007CC7F70087CCF70094D1F800C0C0C000C0C0C00000000000C0C0
      C000FFFF000000000000C0C0C000FFFF0000FF00000080800000FFFF0000FF00
      000000000000C0C0C000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000129AF0000E98EF00159BEF001B9E
      F00023A1F0002BF6EF00880000008800000088000000880000002BF6EF0060BB
      F4006AC0F50073C3F6007EC8F6008CCEF700C0C0C000C0C0C000C0C0C0000000
      0000C0C0C00000000000C0C0C000C0C0C000C0C0C00080800000FFFF0000FFFF
      00008080000000000000C0C0C000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000C97EF000896EF000E98EF00149A
      F0001B9EF00021A0F100880000008800000088000000880000004DB3F30055B6
      F4005FBBF50069BFF60074C4F50081C9F700C0C0C000C0C0C000C0C0C000C0C0
      C0000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000808000008080
      0000C0C0C000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000149AF0000D98EF00129AF000189C
      F0001E9FF00024A1F1002CA5F100880000008800000042AEF3004CB3F40053B6
      F4005EBAF50066BEF60070C3F6007EC8F700C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000000000
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
      0000000000000000000080000000800000008000000080000000800000008000
      0000800000008000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000008000000080000000800000008000
      0000800000008000000080000000800000000000000000000000000000000000
      0000000000000000000080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00800000000000000086868600008080008686
      8600008080008686860080000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF00800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00800000000000000000808000868686000080
      8000868686000080800080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00800000000000000086868600008080008686
      8600008080008686860080000000FFFFFF00000000000000000000000000FFFF
      FF00800000008000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00800000000000000000808000868686000080
      8000868686000080800080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0080000000FFFFFF0080000000000000000000000000000000663300006633
      0000663300006633000066330000000000000000000000000000000000000000
      0000000000006666000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000080000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00800000000000000086868600008080008686
      8600008080008686860080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00800000008000000000000000000000000000000000000000996600006666
      0000666600009966000000000000000000000000000000000000000000000000
      0000000000006666000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF000000000000000000FFFF
      FF00800000008000000080000000800000000000000000808000868686000080
      8000868686000080800080000000800000008000000080000000800000008000
      0000800000000000000000000000000000000000000000000000996600006666
      000099660000CC99660000000000000000000000000000000000000000000000
      0000000000006666000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000080000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0080000000FFFFFF0080000000000000000000000086868600008080008686
      8600008080008686860000808000868686000080800086868600008080008686
      8600008080000000000000000000000000000000000000000000996600009966
      0000CC99660066660000CC996600000000000000000000000000000000000000
      0000CC9966006666000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00800000008000000000000000000000000000000000808000868686000000
      0000000000000000000000000000000000000000000000000000000000008686
      8600868686000000000000000000000000000000000000000000996600000000
      000000000000CC9966006666000066660000CC9966000000000000000000CC99
      660066660000CC99660000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF0000000000800000008000000080000000800000008000
      0000800000000000000000000000000000000000000086868600868686000000
      0000000000000000000000000000000000000000000000000000000000008686
      8600008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000CC9966006666000066660000666600006666
      0000CC9966000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000808000868686000080
      80000000000000FFFF00000000000000000000FFFF0000000000868686000080
      8000868686000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
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
      0000000000000000000000000000424242004242420042424200424242000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008C8C8C00C6C6C600FFFFFF0084848400848484004242
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008C8C8C00424242004242420042424200424242004242
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800080008000800086868600000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000000000000000000000800000000000000000000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008C8C8C00C6C6C600FFFFFF0084848400848484004242
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000008000800080008000FFFFFF00FFFFFF00C0C0C000868686000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000000000000000000000800000000000000080000000000000000000
      000080000000000000000000000000000000000000000000000000ADFF000000
      000000000000000000008C8C8C00848484008484840084848400848484004242
      420000000000000000000000000000ADFF000000000000000000000000008000
      800080008000FFFFFF00FFFFFF000000000000000000C0C0C000C0C0C0008686
      86000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000008686860000000000000000008686
      8600000000000000000000000000000000000000000000000000000000000000
      0000800000000000000000000000800000000000000080000000000000000000
      000080000000000000000000000000000000000000000000000000000000006B
      FF00000000000084FF000084840000FFFF0000FFFF0000CEFF0000FFFF000084
      8400426BB50000000000006BFF0000000000868686008000800080008000FFFF
      FF00FFFFFF000000000000000000800080008000800000000000C0C0C000C0C0
      C0008686860000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000868686000000000000000000FFFF00008686
      8600868686000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000000000000080000000000000000000
      0000800000000000000000000000000000000000000000000000000000000000
      000000ADFF000084FF0000FFFF0000E7F70000E7FF0000E7FF0000E7F70000FF
      FF00426BB50000ADFF0000000000000000008686860080008000FFFFFF000000
      000000000000800080008000800080008000800080008000800000000000C0C0
      C000C0C0C00086868600000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000008686
      8600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000080000000800000008000
      0000000000000000000000000000000000000000000000ADFF00000000000000
      00005ABDFF0000FFFF0000E7FF0000EFFF0000EFFF0000E7FF0000E7FF0000DE
      FF0000FFFF00426BB50000000000000000008686860000000000000000008000
      800080008000800080000080800000FFFF008000800080008000800080000000
      0000C0C0C000C0C0C000868686000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000000000FFFF000000000000000000008686
      8600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000080000000000000000000
      0000000000000000000000000000000000000000000000000000006BFF005ABD
      FF00C6FFFF0029EFFF0000EFFF0000F7FF0000FFFF0000FFFF0000E7FF0000E7
      FF0000DEFF0000FFFF00426BB500006BFF008686860080008000800080008000
      8000800080008000800080008000008080008000800080008000800080008000
      800000000000C0C0C000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000086868600FFFF0000FFFF0000000000008686
      8600868686000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005ABD
      FF00C6FFFF006BFFFF0021FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000EF
      FF0000E7FF0000CEFF00426BB500000000000000000080008000FFFFFF008000
      80008000800080008000800080008000800000FFFF0000FFFF00800080008000
      80008000800000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000008686860000000000000000008686
      8600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000006BFF005ABD
      FF00C6FFFF00E7FFFF0031FFFF0008FFFF0000FFFF0000FFFF0000FFFF0000F7
      FF0000E7FF0000FFFF00426BB500006BFF00000000000000000080008000FFFF
      FF0080008000800080008000800080008000800080000080800000FFFF0000FF
      FF008000800080008000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000ADFF00000000005ABD
      FF00C6FFFF00E7FFFF006BFFFF0031FFFF0000FFFF0000FFFF0000FFFF0000F7
      FF0000FFFF0000CEFF00426BB500000000000000000000000000000000008000
      8000FFFFFF00800080008000800080008000008080008000800000FFFF0000FF
      FF008000800080008000800080000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005ABDFF00C6FFFF00E7FFFF0073FFFF0021FFFF0000FFFF0000FFFF0000FF
      FF0000CEFF00426BB50000000000000000000000000000000000000000000000
      000080008000FFFFFF00800080008000800000FFFF0000FFFF0000FFFF008000
      80008000800080008000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000006BFF005ABDFF00C6FFFF00E7FFFF00E7FFFF0000FFFF0000FFFF0000CE
      FF00426BB500006BFF0000000000000000000000000000000000000000000000
      00000000000080008000FFFFFF00800080008000800080008000800080008000
      80000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000006BFF0000AD
      FF0000000000000000005ABDFF005ABDFF0010B5FF0010B5FF0010B5FF0010B5
      FF00000000000000000000ADFF00006BFF000000000000000000000000000000
      0000000000000000000080008000FFFFFF008000800080008000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800080008000800000000000000000000000
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
      000000000000000000008B8B8B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A5A5A004A4A4A004A4A
      4A004A4A4A000000000094949400948C94008C8C8C008C8C8C008C8C8C008C8C
      8C008C8C8C008C8C8C008C8C8C008C8C8C000000000000000000000000000000
      00000000000000000000AD7B6B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008B8B8B008B8B8B008B8B8B008B8B8B008B8B8B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008B8B3D008D8D3D0090903D007A7A3D0062623D000000
      000000000000000000000000000000000000000000005A5A5A00DE080800DE08
      08004A4A4A0000000000949494008CCE8C008CCE8C008CD69C0094DEA50094DE
      B5008CDEBD008CDEBD0094E7CE008C8484000000000000000000000000000000
      000000000000AD7B6B00AD7B6B00AD7B6B00AD7B6B00AD7B6B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008B8B8B00000000008B8B8B000000000000000000000000008B8B8B008B8B
      8B00000000000000000000000000000000000000000000000000000000000000
      000090903D00B2B23D00C1C13D00C0C03D00BFBF3D00ACAC3D0098983D007777
      3D0045453D00000000000000000000000000000000005A5A5A00CE082900FF08
      31004A4A4A00000000009C9C9C00085208000052080000520800005208000842
      080008520800A5FFFF009CFFFF008C8484000000000000000000000000000000
      0000AD7B6B00D1D1D100AD7B6B00A1A1A100A0A0A0009F9F9F00AD7B6B00AD7B
      6B00000000000000000000000000000000000000000000000000000000008B8B
      8B0000000000000000008B8B8B00000000000000000000000000000000000000
      00008B8B8B008B8B8B0000000000000000000000000000000000000000009595
      3D00C9C93D00DDDD3D00E4E43D00E2E23D00E0E03D00CECE3D00BBBB3D00A1A1
      3D0078783D0041413D00000000000000000000000000737B7B00C6181000FF18
      08004A4A4A0000000000A5A5A500004A0000003900000039000000310000004A
      0000186B100084E7B50073E7AD008C848C00000000000000000000000000AD7B
      6B00CFCFCF00D2D2D200AD7B6B00A2A2A200A1A1A100A0A0A0009E9E9E009D9D
      9D00AD7B6B00AD7B6B00000000000000000000000000000000008B8B8B000000
      000000000000000000008B8B8B00000000000000000000000000000000000000
      0000000000008B8B8B000000000000000000000000000000000000000000CDCD
      3D00E8E83D00F4F43D00F8F83D00F8F83D00F5F53D00E4E43D00D3D33D00BBBB
      3D0097973D006D6D3D000000000000000000000000008C8C8C00CE4A0800FF5A
      08004A4A4A0000000000A5A5A500186B1000004A0000003900000031000029A5
      42004AC66B0094B54A00B5AD29008C8C94000000000000000000AD7B6B00CDCD
      CD00D0D0D000D3D3D300AD7B6B00A3A3A300A2A2A200A0A0A0009F9F9F009E9E
      9E009C9C9C00AD7B6B00000000000000000000000000000000008B8B8B000000
      000000000000000000008B8B8B00000000000000000000000000000000000000
      0000000000008B8B8B0000000000000000000000000000000000B1B13D00E6E6
      3D00F8F83D00FEFE3D00FFFF3D00FEFE3D00FDFD3D00F2F23D00E2E23D00CBCB
      3D00A9A93D0083833D0043433D0000000000000000008C8C8C00C6B50800FFEF
      08004A4A4A0000000000ADADB5005ABD63000873100000520000085A0000C66B
      0000CE630000DE6B0000DE730000848C9C000000000000000000AD7B6B00CECE
      CE00D1D1D100D4D4D400AD7B6B00A3A3A300A2A2A200A1A1A100A0A0A0009E9E
      9E009D9D9D00AD7B6B00000000000000000000000000000000008B8B8B000000
      000000000000000000008B8B8B00000000000000000000000000000000000000
      0000000000008B8B8B0000000000000000000000000000000000CACA3D00F1F1
      3D00FDFD3D00FFFF3D00FFFF3D00FFFF3D00FFFF3D00F6F63D00EBEB3D00D6D6
      3D00B5B53D0090903D0051513D0000000000000000009C9C9C00ADCE0800CEFF
      08004A4A4A0000000000B5B5B50031AD3900218C1800529C390094944200DE7B
      2100C65A0000BD4A0000C65200008C949C000000000000000000AD7B6B00CFCF
      CF00D2D2D200D5D5D500AD7B6B00A4A4A400A3A3A300A2A2A200A0A0A0009F9F
      9F009E9E9E00AD7B6B00000000000000000000000000000000008B8B8B000000
      000000000000000000008B8B8B00000000000000000000000000000000000000
      0000000000008B8B8B0000000000000000000000000000000000D3D33D00F3F3
      3D00FFFF3D00FFFF3D00FFFF3D00FFFF3D00FFFF3D00F8F83D00F1F13D00D9D9
      3D00BBBB3D0095953D005B5B3D0000000000000000009C9C9C0039CE080042FF
      08004A4A4A0000000000BDBDBD008CBD4A00A5BD5A00E7D68400FFEF9C00F7CE
      7B00DE9C4200BD733100AD4A00008C949C000000000000000000AD7B6B00D0D0
      D000D3D3D300D6D6D600AD7B6B00A5A5A500A3A3A300A2A2A200A1A1A100A0A0
      A0009F9F9F00AD7B6B00000000000000000000000000000000008B8B8B000000
      0000000000008F8F8F008F8F8F008B8B8B000000000000000000000000000000
      0000000000008B8B8B0000000000000000000000000000000000C7C73D00F0F0
      3D00FEFE3D00FFFF3D00FFFF3D00FFFF3D00FFFF3D00F9F93D00F1F13D00D9D9
      3D00BCBC3D0091913D0059593D0000000000000000009C9C9C0010CE100010FF
      10004A4A4A0000000000C6C6C600F7CE8C00FFB54200FFAD2900FFBD5A00FFBD
      5A00FFDEA500F7EFC600FFFFCE00848484000000000000000000AD7B6B00D1D1
      D100D4D4D400AD7B6B00AD7B6B00A5A5A500A4A4A400A3A3A300A2A2A200A1A1
      A1009F9F9F00AD7B6B00000000000000000000000000000000008B8B8B000000
      00008E8E8E000000000000000000000000008D8D8D008C8C8C008C8C8C008B8B
      8B00000000008B8B8B0000000000000000000000000000000000B4B43D00F0F0
      3D00F9F93D00FFFF3D00FFFF3D00FFFF3D00FFFF3D00F6F63D00EDED3D00D4D4
      3D00B2B23D008A8A3D0049493D0000000000000000009C9C9C0008CE730008FF
      94004A4A4A0000000000C6C6C600EFCEA500F7CE9400F7CE8C00F7D6AD00F7D6
      AD00F7D69C00EFCE9C00F7D69C0084848C000000000000000000AD7B6B00D2D2
      D200AD7B6B008E8E8E008E8E8E00AD7B6B00AD7B6B00AD7B6B00AD7B6B00A1A1
      A100A0A0A000AD7B6B00000000000000000000000000000000008B8B8B008D8D
      8D00000000000000000000000000000000000000000000000000000000000000
      00008A8A8A00898989000000000000000000000000000000000000000000D7D7
      3D00F5F53D00FEFE3D00FFFF3D00FDFD3D00FCFC3D00F1F13D00E2E23D00C9C9
      3D00A4A43D0073733D000000000000000000000000009C9C9C0010C6AD0008FF
      DE004A4A4A0000000000C6C6C600EFCEA500EFCEA500EFCEA500E7C69C00E7C6
      9C00E7C69C00E7C69C00EFC694008C8C94000000000000000000AD7B6B00AD7B
      6B008D8D8D008C8C8C008C8C8C008C8C8C008B8B8B008B8B8B008B8B8B00AD7B
      6B00AD7B6B00AD7B6B00000000000000000000000000000000008C8C8C000000
      0000000000000000000000000000000000000000000000000000000000008888
      8800888888000000000000000000000000000000000000000000000000009D9D
      3D00D9D93D00EFEF3D00F6F63D00F8F83D00F0F03D00E3E33D00CECE3D00B1B1
      3D0088883D0044443D000000000000000000000000009C9C9C000884CE00089C
      FF004A4A4A0000000000C6C6C600B5B5B500ADB5B500ADADB500ADADAD00A5A5
      AD00A5A5AD00A5A5A5009C9C9C009C9C9C000000000000000000AD7B6B008B8B
      8B008B8B8B008B8B8B008B8B8B008A8A8A008A8A8A008989890089898900AD7B
      6B00AD7B6B000000000000000000000000000000000000000000000000008A8A
      8A00898989008989890000000000000000000000000000000000878787000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009A9A3D00C8C83D00D6D63D00DADA3D00D3D33D00C5C53D00AEAE3D008C8C
      3D004E4E3D00000000000000000000000000000000009C9C9C00084ACE00085A
      FF004A4A4A000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AD7B
      6B00AD7B6B00AD7B6B0089898900898989008888880088888800AD7B6B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000087878700878787008787870086868600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C3D00A9A93D00A4A43D0095953D0074743D000000
      000000000000000000000000000000000000000000009C9C9C000808DE000808
      DE004A4A4A000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AD7B6B00AD7B6B00AD7B6B00AD7B6B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5A5A500A5A5A500A5A5
      A500A5A5A5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000AD7B6B00AD7B6B00AD7B6B00AD7B6B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000094313100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009431310000000000000000000000000000000000AD7B
      6B00DEA57B00EFC69400E7C6A500DEB58C00AD7B6B00AD7B6B00AD7B6B00AD7B
      6B00000000000000000000000000000000000000000000000000000000000000
      000000000000000000008B8B8B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A1A1A100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B57373000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B573730000000000000000000000000000000000AD7B6B00DEB5
      8C00EFB57B00EFC69C00EFD6B500EFCEA500C69C73007B4A4A008C525200AD7B
      6B00AD7B6B00AD7B6B0000000000000000000000000000000000000000000000
      0000000000008B8B8B008B8B8B008B8B8B008B8B8B008B8B8B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D0D0D000A2A2A200A1A1A100A0A0A0009E9E9E00000000000000
      000000000000000000000000000000000000000000000000000000000000AD5A
      5A00AD5A5A00E7C6C600E7C6C600E7C6C600C6CEC600C6CEC600C6CEC600AD5A
      5A009431310000000000000000000000000000000000AD7B6B00E7BD9400EFC6
      8C00EFB57B00EFC6A500F7E7CE00F7D6AD00C69C84007B4A4A007B4A4A00CEA5
      7B00EFBD8C00D6A58400AD7B6B00000000000000000000000000000000000000
      00008B8B8B00000000008B8B8B000000000000000000000000008B8B8B008B8B
      8B00000000000000000000000000000000000000000000000000000000000000
      0000CECECE00D1D1D100A3A3A300A1A1A100A0A0A0009F9F9F009E9E9E009C9C
      9C0000000000000000000000000000000000000000000000000000000000BD7B
      7300C65A5A00E7C6C6009C393900B5737300C6CEC600F7F7F700F7F7F700C65A
      5A0094313100000000000000000000000000AD7B6B00F7CE9C00F7D69400EFC6
      8C00EFB57B00F7CEAD00FFEFDE00FFE7C600CEA58C007B4A4A007B4A4A00CEA5
      7B00EFBD8C00DEB58400AD7B6B00000000000000000000000000000000008B8B
      8B0000000000000000008B8B8B00000000000000000000000000000000000000
      00008B8B8B008B8B8B000000000000000000000000000000000000000000CCCC
      CC00CFCFCF00D2D2D200A3A3A300A2A2A200A1A1A100A0A0A0009E9E9E009D9D
      9D009C9C9C009B9B9B000000000000000000000000000000000000000000BD7B
      7300C65A5A00C6948C009C3939009C4A4A00E7C6C600C6C6C600F7F7F700C65A
      5A0094313100000000000000000000000000AD7B6B00F7D69C00F7D69400F7C6
      8C00F7B57300F7CEAD00FFF7EF00FFEFDE00CEAD9C00734242007B4A4A00CEA5
      7B00EFBD8C00DEB58400AD7B6B000000000000000000000000008B8B8B008B8B
      8B0000000000000000008B8B8B00000000000000000000000000000000008B8B
      8B008B8B8B008B8B8B0000000000000000000000000000000000CACACA00CDCD
      CD00D0D0D000D3D3D300A4A4A400A3A3A300A2A2A200A0A0A0009F9F9F009E9E
      9E009C9C9C009B9B9B000000000000000000000000000000000000000000BD7B
      7300C65A5A00C6948C00C6948C00BDA5A500BDA5A500E7C6C600C6CEC600C65A
      5A0094313100000000000000000000000000AD7B6B00F7D69C00FFD69400E7BD
      9400B5A59400F7CEAD00FFFFF700FFF7EF00DEC6B50094635A0084524A00CEA5
      8400EFBD8C00DEB58400AD7B6B000000000000000000000000008B8B8B000000
      00008B8B8B008B8B8B008B8B8B008B8B8B008B8B8B008B8B8B008B8B8B000000
      0000000000008B8B8B0000000000000000000000000000000000CBCBCB00CECE
      CE00D1D1D100D4D4D400A5A5A500A3A3A300A2A2A200A1A1A100A0A0A0009E9E
      9E009D9D9D009C9C9C000000000000000000000000000000000000000000BD7B
      7300AD524A00B55A5A00C65A5A00C65A5A00C65A5A00C65A5A00C65A5A00C65A
      5A0094313100000000000000000000000000AD7B6B00FFD69400D6CEA50052A5
      E7002184F70084ADDE00FFFFEF00FFF7EF00FFF7E700F7E7CE00E7C6A500E7C6
      9C00E7BD9400DEB58400AD7B6B000000000000000000000000008B8B8B000000
      000000000000000000008B8B8B00000000008B8B8B0000000000000000000000
      0000000000008B8B8B0000000000000000000000000000000000CCCCCC00CFCF
      CF00D2D2D200D5D5D500A5A5A500A4A4A400A3A3A300A2A2A200A0A0A0009F9F
      9F009E9E9E009D9D9D000000000000000000000000000000000000000000BD7B
      7300AD524A00FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A
      5A0094313100000000000000000000000000AD7B6B009CC6C60042B5FF0031AD
      FF00319CFF001884FF0084BDF700FFFFEF00FFF7EF00FFEFDE00F7E7CE00EFD6
      B500EFC69C00DEB58C00A57B7B000000000000000000000000008B8B8B000000
      000000000000000000008B8B8B00000000008B8B8B0000000000000000000000
      0000000000008B8B8B0000000000000000000000000000000000CDCDCD00D0D0
      D000D3D3D300D6D6D600A6A6A600A5A5A500A3A3A300A2A2A200A1A1A100A0A0
      A0009F9F9F009D9D9D000000000000000000000000000000000000000000BD7B
      7300AD524A00FFF7F700FFF7F700FFF7F700FFF7F700FFFFF700FFF7F700C65A
      5A0094313100000000000000000000000000429CF70042A5FF0042ADFF0042B5
      FF0039A5FF002994FF001884FF008CC6F700FFFFEF00FFF7EF00FFEFDE00FFE7
      C600DEC6B500948C94009C7B84000000000000000000000000008B8B8B000000
      0000000000008F8F8F008F8F8F008B8B8B008B8B8B0000000000000000000000
      0000000000008B8B8B0000000000000000000000000000000000CDCDCD00D1D1
      D100D4D4D4008F8F8F008F8F8F00A5A5A500A4A4A400A3A3A300A2A2A200A1A1
      A1009F9F9F009E9E9E000000000000000000000000000000000000000000BD7B
      7300AD524A00FFF7F700FFF7F700FFF7F700FFF7F700FFFFF700FFF7F700C65A
      5A009431310000000000000000000000000000000000429CFF0042A5FF0042AD
      FF0042B5FF0039A5FF002994FF001884FF008CC6F700FFFFEF00FFFFEF00D6D6
      D600737BAD007B739400000000000000000000000000000000008B8B8B000000
      00008E8E8E000000000000000000000000008D8D8D008C8C8C008C8C8C008B8B
      8B00000000008B8B8B0000000000000000000000000000000000CECECE00D2D2
      D2008E8E8E008E8E8E008E8E8E008D8D8D008D8D8D008C8C8C008C8C8C00A1A1
      A100A0A0A0009F9F9F000000000000000000000000000000000000000000BD7B
      7300AD524A00FFF7F700FFF7F700FFFFF700FFFFF700FFFFF700FFF7F700C65A
      5A00943131000000000000000000000000000000000000000000429CF70042A5
      FF0042ADFF0042ADFF0039A5FF002994FF001884FF0094C6FF00B5CEE7004A6B
      BD00526BA50000000000000000000000000000000000000000008B8B8B008D8D
      8D00000000000000000000000000000000008B8B8B0000000000000000000000
      00008A8A8A008989890000000000000000000000000000000000CFCFCF008D8D
      8D008D8D8D008C8C8C008C8C8C008C8C8C008B8B8B008B8B8B008B8B8B008A8A
      8A008A8A8A00898989000000000000000000000000000000000000000000BD7B
      7300AD524A00D6D6D600CEB5B500CEB5B500CEB5B500CEB5B500D6D6D600AD52
      4A009431310000000000000000000000000000000000000000000000000042A5
      FF0042ADFF0042B5FF0042ADFF00319CFF002994FF001884FF00316BE7003163
      C6000000000000000000000000000000000000000000000000008C8C8C000000
      0000000000000000000000000000000000008B8B8B0000000000000000008888
      88008888880000000000000000000000000000000000000000008C8C8C008B8B
      8B008B8B8B008B8B8B008B8B8B008A8A8A008A8A8A0089898900898989008888
      8800888888000000000000000000000000000000000000000000B57373000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B573730000000000000000000000000000000000000000000000
      000042A5FF0042ADFF0042B5FF0039ADFF003994F7001831B5003952DE000000
      0000000000000000000000000000000000000000000000000000000000008A8A
      8A00898989008989890000000000000000008B8B8B0000000000878787000000
      0000000000000000000000000000000000000000000000000000000000008A8A
      8A00898989008989890089898900898989008888880088888800878787000000
      0000000000000000000000000000000000000000000094313100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000094313100000000000000000000000000000000000000
      00000000000042A5FF0042ADFF00000000000000000018109400394ADE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000087878700878787008787870086868600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000087878700878787008787870086868600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000018189C00394ADE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008C6363004242420000000000000000000000
      000000000000000000000000000000000000000000004A637B00BD9494000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A637B00BD9494000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000029ADD60031B5DE0021AD
      D600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008C6363009A666600B9666600BB6868004242420000000000000000000000
      0000000000000000000000000000000000006B9CC600188CE7004A7BA500C694
      9400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006B9CC600188CE7004A7BA500C694
      9400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000029ADD6009CDEEF0084EF
      FF004AC6E70021ADD60018A5C60018A5C60018A5C60000000000000000000000
      00000000000000000000000000000000000000000000000000008C6363009A66
      6600C66A6B00D2686900D2686900C3686900424242009A6666009A6666009A66
      66009A6666009A6666009A666600000000004AB5FF0052B5FF00218CEF004A7B
      A500C69494000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004AB5FF0052B5FF00218CEF004A7B
      A500C69494000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000029ADD60052BDE7009CFF
      FF0094FFFF0073DEF70073DEF70073DEF70073DEF7004AC6E70021ADD60018A5
      C6000000000000000000000000000000000000000000000000009A666600D16D
      6E00D16D6E00D16D6E00CF6C6E00C76A6D0042424200C0797A00DF898A00F293
      9400F5A7A500F5A7A5009A666600000000000000000052B5FF0052B5FF001884
      E7004A7BA500C694940000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000052B5FF0052B5FF001884
      E7004A7BA500C694940000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000029ADD60052BDE700ADFF
      FF008CF7FF008CEFFF008CEFFF008CEFFF0073DEF70073DEF70073DEF7004AC6
      EF0021ADD60000000000000000000000000000000000000000009A666600D16D
      6E00D16D6E00D4707100D2707200CC6E71004242420000960000009600000096
      000000960000F5A7A5009A66660000000000000000000000000052B5FF004AB5
      FF00188CE7004A7BA500BD949400000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000052B5FF004AB5
      FF00188CE7004A7BA500BD949400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000029ADD60029ADD600ADDE
      EF0094F7FF0094F7FF008CEFFF008CEFFF008CEFFF008CEFFF0073DEF70073DE
      F7004AC6EF0000000000000000000000000000000000000000009A666600D572
      7300D5727300D9757600D8767700D07275004242420000960000009600000096
      000000960000F5A7A5009A6666000000000000000000000000000000000052B5
      FF004AB5FF002184DE005A6B730000000000AD7B7300C6A59C00D6B5A500CEA5
      9C000000000000000000000000000000000000000000000000000000000052B5
      FF004AB5FF002184DE005A6B730000000000AD7B7300C6A59C00D6B5A500CEA5
      9C00000000000000000000000000000000000000000029ADD60073DEF70029AD
      D6009CFFFF008CF7FF008CF7FF008CF7FF008CEFFF008CEFFF008CEFFF0073DE
      F70073DEF70018A5C600000000000000000000000000000000009A666600E37D
      7E00E37D7E00E37D7E00E6808100D37476004242420000960000008000000080
      000000800000F5A7A5009A666600000000000000000000000000000000000000
      000052BDFF00B5D6EF00A5948C00B59C8C00F7E7CE00FFFFD600FFFFD600FFFF
      D600E7DEBD00CEADA50000000000000000000000000000000000000000000000
      000052BDFF00B5D6EF00A5948C00B59C8C00F7E7CE00FFFFD600FFFFD600FFFF
      D600E7DEBD00CEADA50000000000000000000000000029ADD60094F7FF0029AD
      D600ADDEEF00A5EFF700A5EFF700A5F7FF008CEFFF008CEFFF008CEFFF0073DE
      F7000073080018A5C600000000000000000000000000000000009A666600F087
      8800E9818200EC969700FADCDC00D8888A004242420000800000008000000064
      000000640000F5A7A5009A666600000000000000000000000000000000000000
      000000000000CEB5B500D6B5A500FFEFC600FFFFD600FFFFD600FFFFD600FFFF
      DE00FFFFEF00F7F7EF00B58C8C00000000000000000000000000000000000000
      000000000000CEB5B500D6B5A500FFEFC600FFFFD600FFFFD600FFFFD600FFFF
      DE00FFFFEF00F7F7EF00B58C8C00000000000000000029ADD6009CFFFF0073DE
      F70029ADD60018A5C60018A5C60018A5C600ADDEEF008CF7FF0084EFFF000073
      08005AE78C000073080018A5C6000000000000000000000000009A666600F087
      8800EE868700F0999A00FFFFFF00DA888A0042424200FACCAA00F7B58400F7B5
      8400F7B58400F5A7A5009A666600000000000000000000000000000000000000
      000000000000C6948C00F7DEB500F7D6A500FFF7CE00FFFFD600B55A1800FFFF
      EF00FFFFF700FFFFFF00DED6BD00000000000000000000000000000000000000
      000000000000C6948C00F7DEB500F7D6A500FFF7CE00FFFFD600FFFFDE00FFFF
      EF00FFFFF700FFFFFF00DED6BD00000000000000000029ADD6009CFFFF0094F7
      FF0073DEF70073DEF70073DEF7006BDEF70029ADD600ADDEEF000073080052D6
      7B0042D66B0031C64A00007308000000000000000000000000009A666600F18B
      8C00F48E8F00F28B8C00F48C8D00DC7F800042424200FACCAA00FBD6BB00FBD6
      BB00FBD6BB00F5A7A5009A666600000000000000000000000000000000000000
      000000000000DEBDA500FFE7AD00F7CE9400FFF7CE00E7D6C600B55A1800E7D6
      C600E7D6C600FFFFEF00F7EFD600C69C94000000000000000000000000000000
      000000000000DEBDA500FFE7AD00F7CE9400E7D6C600E7D6C600E7D6C600E7D6
      C600E7D6C600FFFFEF00F7EFD600C69C94000000000029ADD6009CFFFF0094F7
      FF0094F7FF0094F7FF0094F7FF0073DEF70073DEF70029ADD60018A5C600108C
      210031C64A00109C210018A5C6000000000000000000000000009A666600F18B
      8C00F7909100F7919200F18D8E00E085850042424200FACCAA00FBD6BB00FBD6
      BB00FBD6BB00F5A7A5009A666600000000000000000000000000000000000000
      000000000000E7C6AD00FFDEAD00EFBD8400B55A1800B55A1800B55A1800B55A
      1800B55A1800FFFFDE00F7F7D600C6AD9C000000000000000000000000000000
      000000000000E7C6AD00FFDEAD00EFBD8400B55A1800B55A1800B55A1800B55A
      1800B55A1800FFFFDE00F7F7D600C6AD9C000000000029ADD600C6FFFF0094FF
      FF009CFFFF00D6FFFF00D6FFFF008CEFFF0094EFFF0073DEF70073DEF7000884
      100018AD290008841000000000000000000000000000000000009A666600F18B
      8C00F9949500FA949500F3919200E388890042424200FACCAA00FBD6BB00FBD6
      BB00FBD6BB00F5A7A5009A666600000000000000000000000000000000000000
      000000000000DEBDAD00FFE7B500EFBD8400F7CE9400FFEFC600B55A1800FFEF
      C600FFFFDE00FFFFDE00F7EFD600C6A59C000000000000000000000000000000
      000000000000DEBDAD00FFE7B500EFBD8400F7CE9400FFEFC600FFFFDE00FFFF
      DE00FFFFDE00FFFFDE00F7EFD600C6A59C000000000021ADD6009CDEEF00C6FF
      FF00C6FFFF009CDEEF0018ADD60018A5C60018A5C60018A5C60018A5C600088C
      100008A5180000000000000000000000000000000000000000009A666600F18B
      8C00F9909200FC999A00F9969700E78C8D0042424200FACCAA00FBD6BB00FBD6
      BB00FBD6BB00F5A7A5009A666600000000000000000000000000000000000000
      000000000000C69C9400FFEFC600FFEFC600F7D6A500F7CE9C00B55A1800FFF7
      CE00FFF7D600FFFFD600E7DEBD00000000000000000000000000000000000000
      000000000000C69C9400FFEFC600FFEFC600F7D6A500F7CE9C00F7E7B500FFF7
      CE00FFF7D600FFFFD600E7DEBD0000000000000000000000000031B5DE0029AD
      D60018A5C60018A5C60000000000000000000000000000000000088C100008A5
      18000884100000000000000000000000000000000000000000009A6666009A66
      6600E49A9800F9909200FF9D9E00EB8F900042424200FACCAA00FBD6BB00FBD6
      BB00FBD6BB00F5A7A5009A666600000000000000000000000000000000000000
      00000000000000000000DEC6AD00FFFFFF00FFF7EF00F7CE9400EFBD8400F7CE
      9C00FFE7B500FFF7C600BD9C8C00000000000000000000000000000000000000
      00000000000000000000DEC6AD00FFFFFF00FFF7EF00F7CE9400EFBD8400F7CE
      9C00FFE7B500FFF7C600BD9C8C00000000000000000000000000000000000000
      000000000000000000000000000000730800087B0800088C1000088C1000087B
      0800000000000000000000000000000000000000000000000000000000000000
      00009A666600B0717200D7868700DA888800424242009A6666009A6666009A66
      66009A6666009A6666009A666600000000000000000000000000000000000000
      0000000000000000000000000000D6BDBD00F7EFD600FFEFC600FFE7AD00FFE7
      B500F7DEB500CEAD9C0000000000000000000000000000000000000000000000
      0000000000000000000000000000D6BDBD00F7EFD600FFEFC600FFE7AD00FFE7
      B500F7DEB500CEAD9C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009A6666009A6666004242420000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CEAD9400CEAD9C00DEBDA500DEBD
      A500000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CEAD9400CEAD9C00DEBDA500DEBD
      A50000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000600000000100010000000000000300000000000000000000
      000000000000000000000000FFFFFF008000FFE0FFFF00000000C030F87F0000
      0000A028F03F000000009024F01F000000008822F83F000000008421F87F0000
      00008421FC3F000000008421F83F000000008421F01F000000008421F81F0000
      0000FFE1FC3F000000004411FE7F000000002409FF3F000000001405FE1F0000
      00000C03FE1F0000000007FFFFFF0000FFFFFFFFFFFFFFFFFFFFFC00FFFFFFFF
      FE008000FFFFEFFDFE000000FFFFC7FFFE000000FFFFC3FB80000000FFFFE3F7
      80000001C1FBF1E780000003C3FBF8CF80000003C3FBFC1F80010003C1F3FE3F
      80030003D863FC1F80070FC3FE07F8CF807F0003FFFFE1E780FF8007FFFFC3F3
      81FFF87FFFFFC7FDFFFFFFFFFFFFFFFFFE1FFFFFFFFFFFFFFC0FFE3F000CF9FF
      FC0FF81F0008F6CFFC0FE00F0001F6B7DC0E80070063F6B7E805000300C3F8B7
      F003000101EBFE8FB0030000016BFE3FC00000010023FF7FE00180010067FE3F
      C000C001000FFEBFA001E000000FFC9FF003F000000FFDDFF003F803005FFDDF
      CC0CFC0F003FFDDFFFFFFE3F007FFFFFFFFFFFFFFFFFFFFFFDFFFFFF8400FDFF
      F83FFC1F8400F83FF5CFF0078400F00FEDF3E0038400E003DDFBE0038400C003
      DDFBC0018400C003DDFBC0018400C003DDFBC0018400C003D8FBC0018400C003
      D70BC0018400C003CFF3E0038400C003DFE7E0038400C007E3DFF00787FFE01F
      FC3FFC1F87FFFC3FFFFFFFFF87FFFFFFFFFFF0FFFFFFFFFFBFFDE00FFDFFFDFF
      DFFBC003F83FF83FE0078001F5CFF00FE0070001EDF3E003E0070001CDE3C003
      E0070001D01BC003E0070001DD7BC003E0070001DD7BC003E0070001D87BC003
      E0078003D70BC003E007C007CF73C003E007E00FDF67C007DFFBF01FE35FE01F
      BFFDF99FFC3FFC3FFFFFFF9FFFFFFFFFFFFFFE7F9FFF9FFF8FFFF07F0FFF0FFF
      807FC00107FF07FF800FC00183FF83FF8007C001C1FFC1FF8007C001E10FE10F
      8003C001F003F0038003C001F801F8018001C001F801F8018001C001F800F800
      8001C001F800F8008003C001F800F8008007C001F801F801C3C7C001FC01FC01
      FE0FF001FE03FE03FFFFFC7FFF0FFF0F00000000000000000000000000000000
      000000000000}
  end
  object AsyncTimer: TGLAsyncTimer
    Left = 226
    Top = 110
  end
  object GLSimpleNavigation: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = snViewer
    FormCaption = 'GLSViewer - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 552
    Top = 278
  end
  object ImageListObjects: TImageList
    Left = 560
    Top = 112
    Bitmap = {
      494C010161006800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000009001000001002000000000000090
      0100000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FEFDFF00FFFEFE00FFFFFE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFE00FFFFFE00FBFAFA00F3F3F200F0EEEF00F3F4F400FBFAFC00FFFF
      FE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEFE
      FF00FBFAFB00F4F5F400EDEEEF003D3D3E003D3D3E003D3D3E00EFEFEF00F4F5
      F400F9FAFB00FFFEFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00F2F3F3003D3D3E003D3D3E00B7B8BA00B9BABB00B8B8BA003D3D3E003D3D
      3E00EEEEED00FAF9F900FFFFFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FCFCFB00F1F2
      F2003D3D3E00B9B7B900B8B8BB003D3D3E00B9B9BC003D3D3E00B7B8B900B5B4
      B8003D3D3E00ECEAEB00FAFAF900FEFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F7F7F7003D3D
      3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D
      3E003D3D3E003D3D3E00F2F2F200FEFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFDFD00F6F6F6003D3D
      3E00BABABB003D3D3E00BDBBBE003D3D3E00BDBCBF003D3D3E00BBBBBC003D3D
      3E00B5B6BA003D3D3E00EBEBEB00F9FAFA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FCFBFB003D3D3E00BABB
      BC00BCBBBD00BBBBBE003D3D3E00BCBDBF00BCBDBF00BDBDBF003D3D3E00BBBB
      BC00B8B8BB00B5B5B8003D3D3E00F1F1F0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFFFF00FBFBFA003D3D3E003D3D
      3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D
      3E003D3D3E003D3D3E003D3D3E00EEEFEE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FAFBFA003D3D3E00B6B7
      B900B9B9BB00B8B9BC003D3D3E00BCBCBC00BBBDBC00BAB9BC003D3D3E00B7B7
      B900B6B5B700B1B2B3003D3D3E00F1F1F2000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FCFDFC00EDEEEE003D3D
      3E00B3B3B5003D3D3E00B7B8B8003D3D3E00BABABB003D3D3E00B4B4B5003D3D
      3E00AFB1B2003D3D3E00E7E6E700FAF9F8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F2F3F3003D3D
      3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D3E003D3D
      3E003D3D3E003D3D3E00F0F1F000FEFFFD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F9FBFA00E9EB
      EA003D3D3E00AFAFB100B0B0B1003D3D3E00B1B0B2003D3D3E00ABACAD00ACAB
      AD003D3D3E00E6E8E500FAFAF900FFFEFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FBFB
      FA00EEEDEE003D3D3E003D3D3E00B2B1B300B1B2B200B1B2B2003D3D3E003D3D
      3E00EAEAEA00FBF9F90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FCFBFB00F4F3F300E7E7E7003D3D3E003D3D3E003D3D3E00E6E6E600F1F1
      F200FAFAFA00FEFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEFEFE00FBFAFB00F1F2F200EEEEEF00F4F3F200FBFBFA00FFFE
      FE00FFFFFE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840204000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000BA3F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000F5F5EA00F5F5EA0000000000000000000000
      0000000000000000000000000000000000000000000084020400840204000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000BA3F0000BA3F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EFEFDF00C6C6
      8B00B5B56900B1B16100B1B16100B1B16100B1B16100B1B16100B1B16100B5B5
      6900C1C18200DEDEBD0000000000000000008402040084020400840204008402
      0400840204008402040084020400840204000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000BA3F0000BA3F0000BA3F0000BA
      3F0000BA3F0000BA3F0000BA3F0000BA3F000000000000000000000000000000
      00000000000000000000000000000000000000000000EDEDDA00CCCC9800BFBF
      7E00D9D9B100AEAE5A009898360088882700858530007A7A300085853000AAAA
      5500B1B16100CECE9C0000000000000000008402040084020400840204008402
      0400840204008402040084020400840204008402040000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848284008482840084020400840204008402040084020400848284008482
      84000000000000000000000000000000000000BA3F0000BA3F0000BA3F0000BA
      3F0000BA3F0000BA3F0000BA3F0000BA3F0000BA3F0000000000000000000000
      000000000000000000000000000000000000C8C892009F9F3F00ADAD5900BBBB
      7600EBEBD600B2B263008D8D29006E6E0A00595900003A3A0000242400003535
      0000B5B56900CFCF9F00FAFAF400000000000000000084020400840204000000
      0000000000000000000000000000840204008402040000000000000000000000
      0000000000000000000000000000000000000000000000000000848284008402
      0400840204008402040084020400840204008402040084020400840204008402
      0400840204008482840000000000000000000000000000BA3F0000BA3F000000
      000000000000000000000000000000BA3F0000BA3F0000000000000000000000
      000000000000000000000000000000000000EFEFDF009C9C3B00A6A64B00B9B9
      7100E6E6CD00B7B76D008D8D29006A6A070059590000313100001E1E00007878
      2C00BBBB7700B1B16100CCCC9800FCFCF9000000000000000000840204000000
      0000000000000000000000000000840204008402040000000000000000000000
      0000000000000000000000000000000000000000000084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000848284008482840000000000000000000000000000BA3F000000
      000000000000000000000000000000BA3F0000BA3F0000000000000000000000
      00000000000000000000000000000000000000000000C0C082009F9F3F00B1B1
      6100DDDDBB00C0C081008D8D290067670500444400002A2A00003D3D05005555
      050078781500AAAA5400B1B16100DCDCB7000000000000000000000000000000
      0000848284008482840084020400840204008402040084020400848284008482
      8400000000000000000000000000000000008482840084020400000000008402
      0400000000000000000084020400840204000000000000000000000000008402
      0400000000000000000084020400848284000000000000000000000000000000
      0000959692009596920000BA3F0000BA3F0000BA3F0000BA3F00959692009596
      92000000000000000000000000000000000000000000F2F2E4009F9F3F00AAAA
      5200D1D1A200CECE9B008D8D29006767050044440000242400006E6E18003939
      00004B4B00007E7E1B00B1B16100BBBB76000000000000000000848284008402
      0400840204008402040084020400840204008402040084020400840204008402
      0400840204008482840000000000000000008402040084020400000000008402
      0400000000000000000084020400000000008402040000000000840204000000
      00008402040000000000840204008402040000000000000000009596920000BA
      3F0000BA3F0000BA3F0000BA3F0000BA3F0000BA3F0000BA3F0000BA3F0000BA
      3F0000BA3F009596920000000000000000000000000000000000C4C48700A6A6
      4B00C1C18100D2D2A3008D8D2900636305003131000042420800525202003030
      00003A3A00005757000099993F00B9B971000000000084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008482840084828400000000008402040084020400000000008402
      0400840204000000000084020400840204000000000000000000840204000000
      0000840204000000000084020400840204000000000095969200959692001B3E
      27001B3E27001B3E27001B3E27001B3E27001B3E27001B3E27001B3E27001B3E
      27001B3E27009596920095969200000000000000000000000000DEDEBD00A3A3
      4400B5B56900D8D8B10087872300595900002929000071711C00545400003333
      00002E2E00003E3E00007A7A1D00B1B161008482840084020400000000008402
      0400840204008402040000000000840204008402040084020400000000008402
      0400840204000000000084020400848284008402040084020400000000008402
      0400000000000000000084020400000000008402040000000000840204000000
      0000840204000000000084020400840204009596920000BA3F001B3E27001B3E
      270000BA3F0000BA3F0000BA3F001B3E270000BA3F0000BA3F0000BA3F001B3E
      27001B3E27001B3E270000BA3F00959692000000000000000000E0E0C000B4B4
      6800AAAA5200D1D1A30082821E004C4C0000393901008F8F2C00646403004242
      00002E2E0000343400006A6A1000B9B971008402040084020400000000008402
      0400000000008402040000000000840204000000000084020400000000008402
      0400000000000000000084020400840204008482840084020400000000008402
      0400840204000000000084020400840204000000000000000000000000008402
      04000000000000000000840204008482840000BA3F0000BA3F001B3E27001B3E
      270000BA3F001B3E27001B3E27001B3E270000BA3F001B3E27001B3E270000BA
      3F001B3E27001B3E270000BA3F0000BA3F000000000000000000E2E2C500C8C8
      9100A6A64B00CDCD9A0082821E003E3E00007C7C340098983800757511005858
      00003E3E00002E2E00005E5E1000C1C181008402040084020400000000008402
      0400000000008402040000000000840204000000000000000000000000008402
      0400000000000000000084020400840204000000000084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084828400848284000000000000BA3F0000BA3F001B3E27001B3E
      270000BA3F001B3E27001B3E27001B3E270000BA3F001B3E27001B3E270000BA
      3F001B3E27001B3E270000BA3F0000BA3F000000000000000000E2E2C500CBCB
      9600C0C08100BDBD7A00858524004D4D0E00BBBB75009D9D3D0081811D006868
      0500444400002B2B00007A7A2400E5E5CB008482840084020400000000008402
      0400840204008402040000000000840204008402040084020400000000008402
      0400000000000000000084020400848284000000000000000000848284008402
      0400840204008402040084020400840204008402040084020400840204008402
      0400840204008482840000000000000000009596920000BA3F001B3E27001B3E
      270000BA3F0000BA3F0000BA3F001B3E270000BA3F001B3E27001B3E270000BA
      3F001B3E27001B3E270000BA3F00959692000000000000000000F7F7EF00CCCC
      9700D8D8B000B5B5690085853000A8A86C00BFBF7C00A8A84F00959533007979
      15005959000034340000C4C49300000000000000000084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008482840084828400000000000000000000000000000000000000
      0000848284008482840084020400840204008402040084020400848284008482
      8400000000000000000000000000000000000000000095969200959692001B3E
      27001B3E27001B3E27001B3E27001B3E27001B3E27001B3E27001B3E27001B3E
      27001B3E2700959692009596920000000000000000000000000000000000E3E3
      C600D4D4A900C4C4880095954100D1D1A300C5C58A00B1B161009C9C3B007979
      1500595900008D8D4A0000000000000000000000000000000000848284008402
      0400840204008402040084020400840204008402040084020400840204008402
      0400840204008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009596920000BA
      3F0000BA3F0000BA3F0000BA3F0000BA3F0000BA3F0000BA3F0000BA3F0000BA
      3F0000BA3F009596920000000000000000000000000000000000000000000000
      0000DBDBB600D2D2A400D4D4A900CBCB9600C4C48700B1B161009C9C3B007979
      15009D9D4E00FCFCF90000000000000000000000000000000000000000000000
      0000848284008482840084020400840204008402040084020400848284008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000959692009596920000BA3F0000BA3F0000BA3F0000BA3F00959692009596
      9200000000000000000000000000000000000000000000000000000000000000
      000000000000EFEFDF00D1D1A300BFBF7E00B2B26300A6A64B00A5A55200D5D5
      AC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C4C2C400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C4C2C400000000000000000000000000000000000000
      0000000000008442E4008442E4004442E4004442E4004402E4004402C4004402
      C400000000000000000000000000000000000000000000000000000000000000
      00000000000000000000040204000000000000000000FC020400FC020400FCFE
      0400FC020400FCFE0400FC020400FC0204000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4C2C40004020400C4C2C4000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C4C2C40004020400C4C2C40000000000000000008442E4008442
      E4004442E4004402E4004402E4004402E4004402E4004402E4004402E4004402
      C400000000000000000000000000000000000000000000000000000000000402
      0400040204000402040004020400040204000000000084828400FC020400FC02
      0400FCFE0400FC020400FC020400848284000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C4C2C40004020400C4C2
      C40000000000C4C2C40000000000000000000000000000000000C4C2C4000000
      0000C4C2C40004020400C4C2C40000000000000000004442E4004402E4008462
      6400846264008462640084626400A4A2A400A4A2A4004402E4004402E4004402
      C400000000000000000000000000000000000000000000000000000000000402
      040000000000000000000402040000000000000000000000000084828400FC02
      0400FC020400FC02040084828400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C2C4000402
      0400C4C2C400848284000000000000000000000000000000000084828400C4C2
      C40004020400C4C2C4000000000000000000000000004402C4004402C4004442
      44008462640084626400444244004442440084626400A4A2A4004402E4004402
      C400000000000000000000000000000000000000000000000000000000000402
      0400000000000000000000000000000000000000000000000000000000008482
      8400FC0204008482840000000000000000000000000000000000000000000000
      0000848284008482840084020400840204008402040084020400848284008482
      840000000000000000000000000000000000000000000000000000000000C4C2
      C400040204008482840000000000000000000000000000000000848284000402
      0400C4C2C400000000000000000000000000000000004402C400444244008462
      64008462640044424400040204000402040044424400A4A2A4004402E4004402
      C400000000000000000000000000000000000000000000000000000000000402
      0400000000000000000004828400048284000482840004828400000000000000
      0000000000000000000000000000000000000000000000000000848284008402
      0400840204008402040084020400840204008402040084020400840204008402
      0400840204008482840000000000000000000000000000000000C4C2C4008482
      84008482840004020400FCFE0400FC020400FC020400FCFE0400040204008482
      840084828400C4C2C4000000000000000000000000004402C40084626400A4A2
      A4008462640084626400444244004442440044424400846264004402E4004402
      C400000000000000000000000000000000000000000000000000000000000402
      0400048284000482840004FEFC0004FEFC0004FEFC0004FEFC0004828400FC02
      0400FCFE0400FC020400FC020400FC0204000000000084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008482840084828400000000000000000000000000000000000000
      000000000000FCFE0400FC0204008402040084020400FC020400FCFE04000000
      000000000000000000000000000000000000000000004402C40084626400C4C2
      C400A4A2A400A4A2A400846264004442440044424400846264004402E4004402
      C400000000000000000000000000000000000000000000000000000000000402
      040004FEFC0004FEFC0004020400040204000000000004FEFC0004FEFC000482
      840004028400FCFE0400FC020400FC0204008482840084020400000000008402
      0400840204008402040000000000840204008402040084020400000000008402
      0400840204000000000084020400848284000000000000000000000000000000
      000000000000FC0204008402040004FEFC0004FEFC0084020400FC0204000000
      000000000000000000000000000000000000000000004402C40084626400C4C2
      C400C4C2C400C4C2C400A4A2A40084626400846264004402C4004402C4004402
      C400A4A2A400C4C2C40000000000000000000000000000000000000000000402
      0400000000000000000004020400000000000000000000000000040284000402
      84000402FC00FC020400FC020400000000008402040084020400000000008402
      0400000000008402040000000000840204000000000084020400000000008402
      0400000000000000000084020400840204000000000000000000000000000000
      000000000000FC0204008402040004FEFC0004FEFC0084020400FC0204000000
      000000000000000000000000000000000000000000004402C40084626400A4A2
      A400C4C2C400A4A2A400A4A2A40084626400846264004402E4004402E4004402
      E4004442440084626400A4A2A400C4C2C4000000000000000000000000000402
      04000402840004028400000000000000000004028400040284000402FC000402
      FC000402FC00FC02040000000000000000008402040084020400000000008402
      0400000000008402040000000000840204000000000000000000000000008402
      0400000000000000000084020400840204000000000000000000000000000000
      000000000000FCFE0400FC0204008402040084020400FC020400FCFE04000000
      000000000000000000000000000000000000000000004402C4004402C4008462
      640084626400846264004402E4004442E4008442E4004402E400444244000402
      0400040204004442440084626400A4A2A4000000000000000000000000000000
      00000402FC000402FC0004028400040284000402FC000402FC000402FC000402
      FC00FC0204000000000000000000000000008482840084020400000000008402
      0400840204008402040000000000840204008402040084020400000000008402
      0400000000000000000084020400848284000000000000000000C4C2C4008482
      84008482840004020400FCFE0400FC020400FC020400FCFE0400040204008482
      840084828400C4C2C4000000000000000000000000004402C4004402C4004402
      C4004402E4008442E4008442E4008442E400C4C2C40084626400846264004442
      440044424400040204004442440084626400FC020400FC020400FC020400FCFE
      0400FC0204000402FC000402FC000402FC000402FC000402FC0084C2E40004FE
      FC00000000000000000000000000000000000000000084828400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000848284008482840000000000000000000000000000000000C4C2
      C400040204008482840000000000000000000000000000000000848284000402
      0400C4C2C400000000000000000000000000000000004402C400000000000000
      000000000000000000000000000000000000C4C2C400A4A2A400A4A2A4008462
      640044424400040204000402040084626400FC020400FC020400FCFE0400FCFE
      0400FCFE0400FC020400FC020400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848284008402
      0400840204008402040084020400840204008402040084020400840204008402
      0400840204008482840000000000000000000000000000000000C4C2C4000402
      0400C4C2C400848284000000000000000000000000000000000084828400C4C2
      C40004020400C4C2C40000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4C2C400C4C2C400C4C2C400A4A2
      A4008462640044424400040204004442440000000000FC020400FC020400FCFE
      0400FC020400FC02040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848284008482840084020400840204008402040084020400848284008482
      84000000000000000000000000000000000000000000C4C2C40004020400C4C2
      C40000000000C4C2C40000000000000000000000000000000000C4C2C4000000
      0000C4C2C40004020400C4C2C400000000000000000000000000000000000000
      000000000000000000000000000000000000C4C2C400C4C2C40000000000C4C2
      C400A4A2A4008462640004020400A4A2A4000000000000000000FC020400FC02
      0400FC0204000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4C2C40004020400C4C2C4000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C4C2C40004020400C4C2C4000000000000000000000000000000
      00000000000000000000000000000000000000000000C4C2C400C4C2C400C4C2
      C400A4A2A400846264008462640000000000000000000000000000000000FC02
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C4C2C400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C4C2C400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C2C400A4A2
      A400A4A2A400A4A2A40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FC020400FC020400FC02
      0400FCFE0400FC020400FC020400FC0204000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000040204000000000000000000FC020400FC020400FC02
      0400FCFE0400FC020400FC020400FC0204000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000040204000000000000000000FC020400FC020400FCFE
      0400FC020400FCFE0400FC020400FC0204000000000000000000000000000000
      00000000000000000000040204000000000000000000FC020400FC0204008482
      840084626400A4A2A400FC020400FC0204000000000000000000040204000402
      04000402040004020400040204000402040000000000FC020400FC020400FCFE
      0400FCFE0400FCFE0400FC020400FC0204000000000000000000000000000000
      00000402040004020400040204000000000000000000FC020400FC0204000402
      0400FCFE0400FC020400FC020400FC0204000000000000000000000000000402
      0400040204000402040004020400040204000000000084828400FC020400FC02
      0400FCFE0400FC020400FC020400848284000000000000000000040204000402
      040004020400040204000402040004020400A4A2A40044424400442224004462
      640084828400A4A2A400C4C2C400FC0204000000000000000000040204000000
      0000000000000000000004020400000000000000000000000000FC020400FC02
      0400FCFE0400FC020400FC020400000000000000000000000000040204000402
      04000402040004020400040204000402040000000000FC020400040204000402
      0400FCFE0400FCFE0400FC020400FC0204000000000000000000000000000402
      040000000000000000000402040000000000000000000000000084828400FC02
      0400FC020400FC02040084828400000000000000000000000000040204000000
      0000000000000000000004020400446264004422240044626400A4A2A400C4DE
      C400FCFE0400FC020400FC020400000000000000000000000000040204000000
      000000000000000000000482840004828400048284000482840000000000C442
      6400FC020400FC02040000000000000000000000000000000000040204000000
      0000000000000000000004020400040204000000000004020400FC020400FC02
      0400FCFE0400FC020400FC020400000000000000000000000000000000000402
      0400000000000000000000000000000000000000000000000000000000008482
      8400FC0204008482840000000000000000000000000000000000040204000000
      00000000000000000000444244004422240084626400A4A2A400C4C2C400FC02
      0400FC020400FC02040000000000000000000000000000000000040204000000
      0000048284000482840004FEFC0004FEFC0004FEFC0004FEFC00048284000000
      0000FC0204000000000000000000000000000000000000000000040204000000
      000000000000000000000000000004020400040204000402040000000000FC02
      0400FC020400FC02040000000000000000000000000000000000000000000402
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040204000000
      000000000000442224004422240084626400A4A2A400C4C2C400C4C2C400C4C2
      C400FC0204000000000000000000000000000000000004020400040204000402
      040004FEFC0004FEFC0000000000000000000000000004FEFC0004FEFC000482
      8400040284000000000000000000000000000000000000000000040204000000
      0000000000000000000000000000040204000402040000000000000000000000
      0000FC0204000000000000000000000000000000000000000000000000000402
      04000000000000000000040204000000000000000000FC020400FC020400FC02
      0400FCFE0400FC020400FC020400FC0204000000000004020400040204000402
      0400848284004422240084626400A4A2A400C4C2C400C4C2C400C4C2C4008482
      84008462640084626400A4A2A400000000000000000000000000040204000000
      0000000000000000000000000000000000000000000000000000040284000402
      84000402FC000000000000000000000000000000000004020400040204000402
      0400000000000402040004020400040204000402040000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000402
      04000402040004020400040204000402040000000000FC020400FC020400FCFE
      0400FCFE0400FCFE0400FC020400FC0204000000000000000000040204000000
      0000442224004442440084828400C4C2C400C4C2C40044626400444244004462
      640084828400A4A2A400A4A2A400000000000000000000000000040204000000
      0000000000000000000000000000000000000000000000000000040284000402
      84000402FC000000000000000000000000000000000000000000040204000000
      0000000000000000000000000000040204000402040000000000040204000402
      0400000000000000000000000000000000000000000000000000000000000402
      0400000000000000000004020400000000000000000000000000FC020400FC02
      0400FCFE0400FC020400FC020400000000000000000000000000000000000000
      00004422240084828400A4A2A400446264004442440044626400848284000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000402840004028400000000000000000004028400040284000402FC000402
      FC000402FC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000040204000402040004020400040204000000
      0000000000000000000000000000000000000000000000000000000000000402
      040000000000000000000000000000000000000000000000000000000000FC02
      0400FC020400FC0204000000000000000000FC020400FC020400FC020400FCFE
      04004462640084828400C4C2C400A4A2A4000000000000000000000000000000
      000084828400442224008482840000000000FC020400FC020400FC020400FCFE
      04000402FC000402FC0004028400040284000402FC000402FC000402FC000402
      FC0000000000000000000000000000000000FC020400FC020400FC020400FCFE
      0400FC020400FC020400FC020400040204000402040000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FC020400000000000000000000000000FC020400FC020400FCFE0400FCFE
      0400C4DEC40084828400A4A2A400000000000000000000000000000000004462
      640004020400444244008482840000000000FC020400FC020400FCFE0400FCFE
      0400FCFE04000402FC000402FC000402FC000402FC000402FC0084C2E40004FE
      FC0000000000000000000000000000000000FC020400FC020400FCFE0400FCFE
      0400FCFE0400FC02040004020400040204000402040004020400000000000000
      000000000000000000000000000000000000FC020400FC020400FC020400FCFE
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FC020400FC020400FCFE
      0400FC020400FC02040000000000000000000000000084828400444244004422
      240044626400A4A2A400000000000000000000000000FC020400FC020400FCFE
      0400FC020400FC02040000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FC020400FC020400FCFE
      0400FC0204000402040004620400046204000462040004620400040204000000
      000000000000000000000000000000000000FC020400FC020400FCFE0400FCFE
      0400FCFE0400FC020400FC020400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FC020400FC02
      0400FC020400000000000000000000000000040204004442440084828400A4A2
      A400000000000000000000000000000000000000000000000000FC020400FC02
      0400FC0204000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FC020400FC02
      0400FC0204000402040004620400040204000402040004620400040204000000
      00000000000000000000000000000000000000000000FC020400FC020400FCFE
      0400FC020400FC02040000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FC02
      0400000000000000000000000000000000004442440084828400000000000000
      000000000000000000000000000000000000000000000000000000000000FC02
      0400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FC02
      0400000000000000000004020400040204000402040004020400000000000000
      0000000000000000000000000000000000000000000000000000FC020400FC02
      0400FC0204000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FC02
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084828400048284000402840084828400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000004828400040204000402040004028400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000040204000000000000000000FC020400FC020400FC02
      0400FCFE0400FC020400FC020400FC0204000000000000000000000000000000
      0000000000000000000004028400040204000402040004020400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084626400C4A2A400A4A2A400C4C2C40000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000040204000000000000000000FC020400FC020400FC02
      0400FCFE0400FC020400FC020400FC0204000000000000000000040204000402
      04000402040004020400040204000402040000000000FC020400FC020400FCFE
      0400FCFE0400FCFE0400FC020400FC0204000000000000000000000000000000
      0000000000000000000004028400040204000402040004028400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A4A2A400C4420400C4420400C4624400846244008462640084A26400C4C2
      C400000000000000000000000000000000000000000000000000040204000402
      04000402040004020400040204000402040000000000FC020400FC020400FCFE
      0400FCFE0400FCFE0400FC020400FC0204000000000000000000040204000000
      0000000000000000000004020400000000000000000000000000FC020400FC02
      0400FCFE0400FC020400FC02040000000000000000000000000000000000C4C2
      C40000000000C4C2C40004028400040204000402040004028400C4C2C4000000
      00000000000000000000C4C2C40000000000000000000000000000000000C4C2
      C40084424400FC020400C4A26400C4A26400C4624400C4624400C46244008462
      64008462440084626400C4A26400000000000000000000000000040204000000
      0000000000000000000004020400000000000000000000000000FC020400FC02
      0400FCFE0400FC020400FC020400000000000000000000000000040204000000
      000000000000000000000482840004828400048284000482840000000000C442
      6400FC020400FC020400000000000000000000000000C4C2C400848284000482
      0400048204000482040004828400040204000402040004828400048204000482
      0400048204000482040084828400000000000000000000000000000000008462
      4400C4624400C4A26400C4A26400C4624400C4624400C4A26400C4624400C4A2
      6400F4CAA400C462640084624400846264000000000000000000040204000000
      000000000000000000000000000000000000000000000000000000000000FC02
      0400FC020400FC02040000000000000000000000000000000000040204000000
      0000048284000482840004FEFC0004FEFC0004FEFC0004FEFC00048284000000
      0000FC0204000000000000000000000000000000000004820400848284008482
      8400848284008482840004028400040204000402040004820400848284008482
      840084828400848284008482840000000000000000000000000084626400C462
      4400C4A26400C4624400F4CAA400C4624400C4A26400FC020400F4CAA400C462
      4400C4624400F4CAA400C4420400F4CAA4000000000000000000040204000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FC0204000000000000000000000000000000000004020400040204000402
      040004FEFC0004FEFC0000000000000000000000000004FEFC0004FEFC000482
      840004028400000000000000000000000000C4C2C40004820400848284000482
      0400048204000402840004020400040204000402040004820400048204008482
      8400048204000482040084828400848284000000000084A2640084424400C4A2
      6400C4624400FC020400C4A26400FC020400C4624400F4CAA400C4624400C462
      4400C4624400C4A26400A4A2A400C4C2C4000000000004020400040204000402
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040204000000
      0000000000000000000000000000000000000000000000000000040284000402
      84000402FC000000000000000000000000000000000004820400848284000482
      0400040204000402040004020400048204000402040004020400040204008482
      840004820400048204000482040084828400C4A2A40084624400FC020400F4CA
      A400C4624400FC020400C4A26400C4624400C4A26400C4A26400FC020400C4A2
      6400C442040084626400C4C2C400000000000000000000000000040204000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040204000000
      0000000000000000000000000000000000000000000000000000040284000402
      84000402FC000000000000000000000000008482840084828400848284000482
      8400040204000402040004820400048204000482040004020400040204000482
      040004820400048204000482040084828400C4624400C4420400FC020400C462
      4400C4A26400C4624400FC020400C4A26400C4624400C4624400C4A26400C462
      440084A26400A4A2A40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000402840004028400000000000000000004028400040284000402FC000402
      FC000402FC000000000000000000000000008482840004820400848284000482
      0400048204000482040004820400048204008482840004020400040204000482
      0400048204000482040084828400C4C2C40084626400A4A2A40084A26400C4A2
      A400C4420400C4624400FC020400C4624400FC020400FC020400FC020400C462
      6400A4A2A400000000000000000000000000FC020400FC020400FC020400FCFE
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      000000000000000000000000000000000000FC020400FC020400FC020400FCFE
      04000402FC000402FC0004028400040284000402FC000402FC000402FC000402
      FC00000000000000000000000000000000008482840004820400048204008482
      8400048204008482840084828400048204008482840004820400048204000482
      0400048204000482040084828400000000000000000000000000C4C2C400A4A2
      A400A4A2A4008462640084626400A4A2A400C4624400C4624400C4624400A4A2
      A40000000000000000000000000000000000FC020400FC020400FCFE0400FCFE
      0400FCFE0400FC020400FC020400000000000000000000000000000000000000
      000000000000000000000000000000000000FC020400FC020400FCFE0400FCFE
      0400FCFE04000402FC000402FC000402FC000402FC000402FC0084C2E40004FE
      FC0000000000000000000000000000000000C4C2C40084828400048204000482
      0400048204008482840084828400048204008482840084828400048204000482
      0400048204008482840004820400C4C2C4000000000000000000000000000000
      0000000000000000000000000000C4C2C400A4A2A400A4A2A400846264000000
      00000000000000000000000000000000000000000000FC020400FC020400FCFE
      0400FC020400FC02040000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FC020400FC020400FCFE
      0400FC020400FC02040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084828400848284008482
      8400048204000482040004820400048204008482840084828400048204000482
      04000482040084828400C4C2C400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FC020400FC02
      0400FC0204000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FC020400FC02
      0400FC0204000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4C2C40084828400048204000482
      0400848284000482040004820400848284008482840084828400848284000482
      0400048204008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FC02
      0400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FC02
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C2C4000000
      0000848284008482840084828400C4C2C4008482840084828400C4C2C4008482
      8400848284008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000800000008000000080
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000080000000800000000000000000000000800000008000000080
      0000008000000080000000800000008000000080000000800000008000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000028B0800028B0800028B0800028B
      0800028B0800028B0800028B0800028B0800028B0800028B0800028B0800028B
      0800028B0800028B0800028B0800028B0800028B0800028B0800028B0800028B
      0800028B08000000DC00234D970029539900234D97001641950008427B00028B
      0800028B0800028B0800028B0800028B08000000000000000000000000000000
      0000FBFBFB00DDDDDD00BBBBBB00BBBBBB00C1C1C100EEEEEE00000000000000
      0000E0E0E000D2D2D200D6D6D600000000000000000000000000000000000000
      0000000000000000000000000000CCCECC00CCCECC00F4F6F400000000000000
      0000000000000000000000000000000000000397100003971000039710000397
      1000039710000397100003971000039710000397100003971000039710000397
      1000039710000397100003971000039710000397100003971000039710000397
      10001C4B97002736CA002727E2002929E2002727E2002727E3001B2AC7000F3E
      970003971000039710000397100003971000000000000000000000000000E0E0
      E000AAAAAA00B2B2B2007878780085858500BBBBBB00CCCCCC00D0D0D0005D5D
      5D006666660089898900BBBBBB00000000000000000000000000000000000000
      0000DCDEDC00646264001C1E1C001C1E1C001C1E1C001C1E1C005C5E5C00D4D6
      D40000000000E4E2E400F4F6F4000000000005A2180005A2180005A2180005A2
      18002727E2002727E2002727E2002727E2002727E2002727E2002727E2002727
      E20005A2180005A2180005A2180005A2180005A2180005A2180005A218001D50
      98002939C8001F28CE001C1ED6001B1BD8001C1CD9002828DF002929E0001C2D
      C6000000E00005A2180005A2180005A2180000000000F4F4F400B4B4B4009393
      9300A1A1A100C6C6C6007F7F7F00787878009D9D9D0089898900202020003F3F
      3F006666660086868600C5C5C50000000000000000000000000000000000CCCE
      CC002C2A2C00343234003C3E3C003C3E3C003C3E3C003C3E3C00343234002C2A
      2C006C6E6C00746E74002C2A2C00CCCECC0007AD200007AD200007AD200007AD
      20002727E200151CC7000F19BC000E18BA001015C4001717D2002525DE002727
      E20007AD200007AD200007AD200007AD200007AD200007AD20001D529C00293B
      C9001D26CC00151CC7000F19BC000E18BA001015C4001717D2002525DE002929
      E0000000C3000000EF0007AD200007AD200000000000B2B2B2008F8F8F009A9A
      9A00A4A4A400B1B1B100AAAAAA0086868600828282007B7B7B00787878007878
      7800787878009C9C9C00D2D2D200000000000000000000000000000000004446
      44003C3E3C00444644004C4E4C004C4E4C004C4E4C004C4E4C00444644003C3E
      3C003C3E3C00DCDEDC009C9A9C00B4B6B40009B9280009B9280009B9280009B9
      28002727E2000813B60002169E0001159B000311AE000B14C8001515D2002727
      E20009B9280009B9280009B9280009B9280009B928000000DC00283ACC002828
      DF001517CF000813B60002169E0001159B000311AE000B14C8001515D2002A2A
      DC004949E4000000DC000000F70009B9280000000000D2D2D200939393009D9D
      9D00A7A7A700B1B1B100C1C1C1008C8C8C00898989009D9D9D00888888007F7F
      7F007B7B7B00AAAAAA00DDDDDD00000000000000000000000000CCCECC003C3E
      3C004C4E4C004C4E4C00545654005456540054565400545654004C4E4C004C4E
      4C0054565400B4B6B400CCCECC00000000000AC430000AC430000AC430000AC4
      30002727E200020CAB000014920000178F00011BA600052BC4000B43D5002727
      E2000AC430000AC430000AC430000AC430000AC430000000D0002727E2002727
      DF000F0FCB00020CAB000014920000178F00011BA600052BC4000B43D5002E66
      E9002B48E5000000D0000000FD000AC430000000000000000000A8A8A800A1A1
      A100AEAEAE00B7B7B700CFCFCF00A1A1A100939393006B6B6B002D2D2D008686
      860086868600BBBBBB00EBEBEB00000000000000000000000000BCBEBC004446
      44004C4E4C005C5E5C007C7E7C009C9A9C009C9A9C007C7E7C00646264006C6E
      6C00ACAEAC00C4C2C40000000000000000000CCF38000CCF38000CCF38000CCF
      38002727E200010FB300001DA3000029A300003DB400025DCC000592E3002727
      E2000CCF38000CCF38000CCF38000CCF38000CCF38000000CC002929E2002929
      DF000E11CA00010FB300001DA3000029A300003DB400025DCC000592E3002E9F
      F4002D66EA000000CC000000FE000CCF38000000000000000000CFCFCF00A7A7
      A700AEAEAE00BBBBBB00CCCCCC00C5C5C5009A9A9A00424242002D2D2D009393
      930096969600BBBBBB00F8F8F800000000000000000000000000D4D6D4004C4E
      4C0074767400A4A6A400D4DAD400ECEAEC00ECEAEC00DCDEDC00A4A6A4007476
      74004C4E4C00CCCECC00000000000000000019D8470019D8470019D8470019D8
      47002727E200021FC3000039BE000054C1000077CD0000AAE00005CAF2002727
      E20019D8470019D8470019D8470019D8470019D847000000D0002727E2002727
      DF000404C900021FC3000039BE000054C1000077CD0000AAE00005CAF20027B5
      F8002760EA000000D0000000FE0019D847000000000000000000F4F4F400AAAA
      AA00B4B4B400C3C3C300CECECE00E0E0E000A1A1A100858585004C4C4C009A9A
      9A00A3A3A300C1C1C10000000000000000000000000000000000000000006462
      6400A4A6A400BCBEBC00C4C2C400D4D6D400D4D6D400C4C2C400BCBEBC00A4A6
      A4006462640000000000000000000000000031DD5E0031DD5E0031DD5E0031DD
      5E002727E200033BD2000071D50000AADE0000C6E80000E2F30011D7FB002727
      E20031DD5E0031DD5E0031DD5E0031DD5E0031DD5E000000DC004949E4002828
      DF001111D400033BD2000071D50000AADE0000C6E80000E2F30011D7FB002798
      F1002743E7000000DC000000F70031DD5E00000000000000000000000000C6C6
      C600BBBBBB00C7C7C700D1D1D100E0E0E000C6C6C600A7A7A70099999900A1A1
      A100B1B1B100CFCFCF0000000000000000000000000000000000D4D6D4007476
      7400A4A6A400B4B6B400B4B6B400BCBEBC00BCBEBC00B4B6B400B4B6B400B4B6
      B400B4B6B40000000000000000000000000049E3740049E3740049E3740049E3
      74002727E200144CE3000475E80000AAF20005AEF30011BBF7002595F2002727
      E20049E3740049E3740049E3740049E3740049E374000000EF000000C3007272
      FB000808E000144CE3000475E80000AAF20005AEF30011BBF7002595F2002861
      E8001B1BE2000000EF000000ED0049E37400000000000000000000000000E7E7
      E700BFBFBF00CCCCCC00D6D6D600E4E4E400E0E0E000AEAEAE00AEAEAE00AEAE
      AE00B7B7B700D9D9D90000000000000000000000000000000000A4A6A4008482
      84009C9A9C00ACAEAC00ACAEAC008482840084828400ACAEAC00ACAEAC00B4B6
      B400F4F6F40000000000000000000000000049E3740049E3740049E3740049E3
      74002727E2002844E5002760EB00297EF200277CF000277CEE002861E8002727
      E20049E3740049E3740049E3740049E37400E9916100E99161000000E0001515
      CA007272FB002844E5002760EB00297EF200277CF000277CEE002861E8001C38
      E3000000E0000000F900E9916100E99161000000000000000000000000000000
      0000CACACA00D1D1D100D9D9D900E4E4E400F4F4F400D2D2D200BBBBBB00B4B4
      B400C1C1C100F2F2F20000000000000000000000000000000000747674007C7E
      7C00ECEAEC00BCBEBC00ACAEAC009492940094929400ACAEAC00B4B6B400ECEA
      EC000000000000000000000000000000000049E3740049E3740049E3740049E3
      74002727E2002727E2002727E2002929E2002727E2002727E3001B1BE2000000
      E00049E3740049E3740049E3740049E37400EEA77900EEA77900EEA779000000
      E0000000C3004949E4002727E2002929E2002727E2002727E3001B1BE2000000
      E0000000FE00EEA77900EEA77900EEA779000000000000000000000000000000
      0000E4E4E400D3D3D300E0E0E000EDEDED00DBDBDB00E0E0E000EAEAEA00EDED
      ED00FBFBFB0000000000000000000000000000000000DCDEDC0084828400ECEA
      EC000000000000000000ECEAEC00D4D6D400D4D6D400ECEAEC00000000000000
      00000000000000000000000000000000000049E3740049E3740049E3740049E3
      740049E3740049E3740049E3740049E3740049E3740049E3740049E3740049E3
      740049E3740049E3740049E3740049E37400F4BC9100F4BC9100F4BC9100F4BC
      91000000EF000000DC000000D0000000CC000000D0000000DC000000EF000000
      F900F4BC9100F4BC9100F4BC9100F4BC91000000000000000000000000000000
      0000FBFBFB00E0E0E000D3D3D300B1B1B100B7B7B700CACACA00F1F1F1000000
      00000000000000000000000000000000000000000000E4E2E400ECEAEC000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000049E3740049E3740049E3740049E3
      740049E3740049E3740049E3740049E3740049E3740049E3740049E3740049E3
      740049E3740049E3740049E3740049E37400F9D2A900F9D2A900F9D2A900F9D2
      A900F9D2A9000000F7000000FE000000FE000000FE000000F7000000ED00F9D2
      A900F9D2A900F9D2A900F9D2A900F9D2A9000000000000000000000000000000
      000000000000D6D6D600B2B2B200AAAAAA00CCCCCC00FBFBFB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000049E3740049E3740049E3
      740049E3740049E3740049E3740049E3740049E3740049E3740049E3740049E3
      740049E3740049E3740049E374000000000000000000FFE7C100FFE7C100FFE7
      C100FFE7C100FFE7C100FFE7C100FFE7C100FFE7C100FFE7C100FFE7C100FFE7
      C100FFE7C100FFE7C100FFE7C100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      00000000000000000000000000009FD0BD009FD0BD009FD0BD00B3D0D200B3D0
      D200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F9E1CF00F9E1
      CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF000000000000000000000000000000
      0000000000000000000000000000FF808000FF808000FF808000FF8080000000
      000000000000000000000000000000000000FC02040001A2FF0001A2FF0001A2
      FF000000FF000000FF000000FF00FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400FC0204000000000000000000000000000000
      00000000000077777C00173E1C00121C120004020400160012001C1F12007777
      7C00B3D0D200000000000000000000000000E9AB520038B8F800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F9E1CF00F9E1CF00F9E1CF00F9E1CF000000000000000000000000000000
      00000000000000000000FF808000FF808000FF808000FF808000FF808000C000
      000000000000000000000000000000000000FC02040001A2FF0001A2FF0001A2
      FF000000FF000000FF000000FF00DE616200FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400FC0204000000000038B8F80038B8F80038B8
      F80077777C00173E1C00173E1C00173E1C00173E1C001C1F1200160012000402
      0400173E1C00000000000000000000000000E9AB520038B8F80038B8F8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F9E1CF0000000000F9E1CF00F9E1CF000000000000000000000000000000
      00000000000000000000FF808000FF808000FF808000FF808000FF808000C000
      000000000000000000000000000000000000FC02040001A2FF0001A2FF0001A2
      FF000000FF000000FF000000FF00DE616200DE616200FC020400FC020400FC02
      0400FC020400FC020400FC020400FC02040038B8F80038B8F80038B8F8009FD0
      BD0077777C0077777C0077777C0077777C0038603800386038001C1F1200121C
      120004020400386038000000000000000000E9AB520038B8F80038B8F80038B8
      F800000000000000000000000000000000000000000000000000F9E1CF00F9E1
      CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF000000000000000000000000000000
      000000000000FF808000FF808000FF808000FF808000FF808000C0000000C000
      0000C0000000000000000000000000000000FC0204000000FF000000FF000000
      FF0001A2FF0001A2FF0001A2FF00DE616200DE616200DE616200FC020400FC02
      0400FC020400FC020400FC0204000080000038B8F80038B8F800B3D0D200B3D0
      D200B3D0D200B3D0D200C4C2C400C4C2C400B3728D0077777C0038603800173E
      1C0016001200040204009FD0BD0000000000E9AB520038B8F80038B8F80038B8
      F80038B8F80000000000000000009FD0BD009FD0BD00F9E1CF00F9E1CF00F9E1
      CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF000000000000000000000000000000
      000000000000FF808000FF808000FF808000FF808000FF808000C0000000C000
      0000C0000000000000000000000000000000FC0204000000FF000000FF000000
      FF0001A2FF0001A2FF0001A2FF00DE616200DE616200DE616200DE616200FC02
      0400FC020400FC020400008000000080000038B8F80038B8F800B3D0D200B3D0
      D200F9E1CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF00C4C2C40077777C003860
      38001C1F12000402040077777C0000000000E9AB520038B8F80038B8F80038B8
      F80038B8F80038B8F8009FD0BD009FD0BD00F9E1CF00F9E1CF00F9E1CF00F9E1
      CF00F9E1CF000000000000000000F9E1CF000000000000000000000000000000
      0000FF808000FF808000FF808000FF808000FF808000FF80800000000000C000
      0000C0000000C00000000000000000000000FC0204000000FF000000FF000000
      FF0001A2FF0001A2FF0001A2FF00DE616200DE616200DE61620060A060000080
      00000080000000800000008000000080000038B8F80000D0FF0000D0FF00F9E1
      CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF009FD0BD007777
      7C00173E1C00160012003860380000000000E9AB520038B8F80038B8F80038B8
      F80030B3E90060CDA400F9E1CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF00F9E1
      CF00F9E1CF00000000000000000000000000000000000000000000000000FF80
      8000FF808000FF808000FF808000FF808000FF80800000000000000000000000
      0000C0000000C00000000000000000000000FC020400FC020400DE616200DE61
      6200DE616200DE616200DE616200DE61620060A0600060A0600060A0600060A0
      60000080000000800000008000000080000038B8F80001F0FC0000D0FF0001F0
      FC0001F0FC0001F0FC00F9E1CF00F9E1CF00F9E1CF00F9E1CF00C4C2C4007777
      7C0038603800121C12003860380000000000E9AB520038B8F80038B8F80030B3
      E90060CDA400F9E1CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF00F9E1
      CF009FD0BD000000000000000000000000000000000000000000000000000000
      C0000000C0000000C0000000C000FF808000FF80800000000000000000000000
      000000000000C0000000C000000000000000FCFE0400FCFE0400FCFE0400DEDF
      6200DEDF6200DEDF6200DEDF6200DEDF6200DEDF620060A0600060A0600060A0
      600060A0600000800000008000000080000038B8F80001F0FC0001F0FC0001F0
      FC0001F0FC0001F0FC0001F0FC0001F0FC00F9E1CF00F9E1CF00F9E1CF00B372
      8D00386038001C1F120077777C0000000000E9AB520030B3E900F9E1CF00F9E1
      CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF009FD0
      BD009FD0BD0000000000000000000000000000000000000000008080FF000000
      FF000000FF000703D7000703D7000000C000FF80800000000000000000000000
      00000000000000000000C0000000C0000000FCFE0400FCFE0400FCFE0400FCFE
      0400DEDF6200DEDF6200DEDF6200DEDF620097D7FF0097D7FF006DCE69005687
      9C0056879C0056879C00008000000080000038B8F80001F0FC0001F0FC0001F0
      FC0001F0FC0001F0FC0001F0FC0001F0FC00F9E1CF00F9E1CF00F9E1CF00B372
      8D0077777C00386038000000000000000000E9AB520030B3E900F9E1CF008878
      6C0088786C0088786C0088786C00F9E1CF00F9E1CF00F9E1CF00F9E1CF009FD0
      BD0000000000000000000000000000000000000000008080FF000000FF000000
      FF000000FF000000FF000000FF000703D7000000C00000000000000000000000
      0000000000000000000000000000C0000000FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400DEDF6200DEDF6200DEDF620097D7FF0097D7FF0097D7FF005687
      9C0056879C0056879C00008000000080000038B8F80001F0FC0001F0FC0001F0
      FC0001F0FC0001F0FC0001F0FC0001F0FC0001F0FC00F9E1CF00B3D0D2009FD0
      BD0077777C009FD0BD000000000000000000E9AB520030B3E900F9E1CF008878
      6C0088786C0088786C0088786C00F9E1CF00F9E1CF00F9E1CF0060CDA4009FD0
      BD0000000000000000000000000000000000000000008080FF000000FF0000FF
      FF000000FF000000FF000000FF000703D7000000C00000000000000000000000
      000000000000000000000000000000000000FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400DEDF6200DEDF620097D7FF0097D7FF0097D7FF008C8C
      FF0056879C0056879C00008000000080000038B8F80001F0FC0001F0FC0001F0
      FC0001F0FC0001F0FC0001F0FC0001F0FC0001F0FC00F9E1CF00B3D0D2009FD0
      BD009FD0BD00000000000000000000000000E9AB520030B3E900F9E1CF008878
      6C0088786C0088786C0088786C00F9E1CF00F9E1CF00F9E1CF0038B8F80038B8
      F80000000000000000000000000000000000000000008080FF0000FFFF000000
      000000FFFF000000FF000000FF000000FF000000C00000000000000000000000
      000000000000000000000000000000000000FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400DEDF62008C8CFF008C8CFF008C8CFF0097D7
      FF0097D7FF006DCE6900008000000080000000D0FF0000D0FF0001F0FC0001F0
      FC0001F0FC0001F0FC0001F0FC0001F0FC0001F0FC0001F0FC0000D0FF000000
      000000000000000000000000000000000000E9AB520030B3E900F9E1CF008878
      6C0088786C0088786C0088786C00F9E1CF00F9E1CF0038B8F80038B8F80038B8
      F80038B8F800000000000000000000000000000000008080FF000000FF0000FF
      FF000000FF000000FF000000FF000000FF000000C00000000000000000000000
      00000000000000000000000000000000000080FFFF0080FFFF00FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE04008C8CFF008C8CFF008C8CFF0097D7
      FF0097D7FF0097D7FF00008000000080000030B3E90000D0FF0001F0FC0001F0
      FC0001F0FC0001F0FC0001F0FC0001F0FC0001F0FC0001F0FC0000D0FF0038B8
      F80038B8F80038B8F8000000000000000000E9AB520017BAD400F9E1CF00F9E1
      CF00F9E1CF00F9E1CF00F9E1CF00F9E1CF0038B8F80038B8F80038B8F80038B8
      F80038B8F80038B8F800000000000000000000000000000000008080FF000000
      FF000000FF000000FF000000FF008080FF000000000000000000000000000000
      00000000000000000000000000000000000080FFFF0080FFFF0080FFFF00FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE04008C8CFF008C8CFF008C8CFF0097D7
      FF0097D7FF0097D7FF00FCFE04000080000030B3E90000D0FF0000D0FF0000D0
      FF0001F0FC0001F0FC0001F0FC0001F0FC0001F0FC0001F0FC0038B8F80038B8
      F80038B8F80038B8F8000000000000000000E9AB520017BAD40017BAD40030B3
      E90030B3E90030B3E90030B3E90030B3E90038B8F80038B8F80038B8F80038B8
      F80038B8F80038B8F80038B8F800000000000000000000000000000000008080
      FF008080FF008080FF008080FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080FFFF0080FFFF00FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400000000000000000030B3E90030B3E90038B8
      F80038B8F80038B8F80038B8F80038B8F80038B8F80038B8F80038B8F80038B8
      F80038B8F800000000000000000000000000E9AB5200E9AB5200E9AB5200E9AB
      5200E9AB5200E9AB5200E9AB5200E9AB5200E9AB5200E9AB5200E9AB5200E9AB
      5200E9AB5200E9AB5200E9AB5200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084828400848284000402
      04000402040000000000000000000000000000000000FF404000FF404000FF40
      4000FF404000FF404000FF404000FF404000FF404000FF404000C0000000C000
      0000C0000000C0000000FF4040000000000000000000FC0204004C0000004C00
      00004C0000004C0000004C0000004C0000004C0000004C0000004C0000004C00
      00004C0000004C0000004C0000000000000000000000FF404000FF404000FF40
      4000FF404000FF404000FF404000FF404000FF404000FF404000C0000000C000
      0000C0000000C0000000FF404000000000000000000000000000000000000000
      000000000000000000000000000084828400848284008482840084828400FC02
      0400FC020400000000000000000000000000FF404000C0C0C00038A8A80038A8
      A80038A8A80038A8A80038A8A800580000005800000058000000380000003800
      000038000000380000005800000058000000FC02040001A2FF00005DE90062F2
      F20062F2F20062F2F2000000E90062F2F20062F2F2004C00000062F2F20062F2
      F2004C00000062F2F20062F2F2004C000000FF404000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000FF404000FF404000FF404000C0000000C000
      0000C0000000C0000000FF404000FF4040000000000000000000000000000000
      000000000000000000008482840084828400848284000000000000000000FC02
      0400FC020400000000000000000000000000FF404000C0C0C00038A8A80080FF
      FF0080FFFF0080FFFF0038A8A80080FFFF0080FFFF005800000080FFFF0080FF
      FF003800000080FFFF0080FFFF0058000000FC02040001A2FF00005DE90062F2
      F2000000E90062F2F2000000E90062F2F2004C0000004C0000004C00000062F2
      F2004C00000062F2F2004C0000004C000000FF404000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000FF404000FF404000FF404000C0000000C000
      0000C0000000C0000000FF404000FF4040000000000000000000000000000000
      000084828400848284008482840084828400000000000000000000000000FC02
      0400FC020400000000000000000000000000FF404000C0C0C00038A8A80080FF
      FF005800000080FFFF005800000080FFFF0058000000580000003800000080FF
      FF003800000080FFFF005800000058000000FC02040001A2FF00005DE90062F2
      F2000000E9000000E9000000E90062F2F2003D0000004C00000062F2F2004C00
      00004C00000062F2F2004C0000004C000000FF404000C0C0C000C0C0C000FF40
      4000FF404000FF404000FF404000FF404000FF404000FF404000C0000000C000
      0000C0000000C0000000FF404000FF4040000000000000000000848284008482
      8400848284008482840084828400000000000000000000000000048284008402
      840084028400048284000000000000000000FF404000C0C0C00038A8A80080FF
      FF0058000000580000005800000080FFFF00580000005800000080FFFF003800
      00003800000080FFFF005800000058000000FC0204000000FF000000E9000000
      E9004EE9E9004EE9E900005DE90062F2F2003D0000003D00000062F2F20062F2
      F2004C00000062F2F2004C000000002A0000FF404000C0C0C000C0C0C000FF40
      4000FF404000FF404000FF404000FF404000FF404000FF404000C0000000C000
      0000C0000000C0000000FF404000FF4040000402040004020400040204000402
      0400040204000402040004020400040204000402040004020400840284008402
      840084028400840284000402040004020400FF404000C0C0C00038A8A80038A8
      A80080FFFF0080FFFF005800000080FFFF005800000058A8A80080FFFF0080FF
      FF005848480080FFFF0058A8A80058000000FC0204000000FF000000E9000000
      E900005DE900005DE900005DE9003D0000003D0000003D0000003D0000004C00
      00004C0000004C000000002A0000002A0000FF404000C0C0C000C0C0C000C0C0
      C000C0C0C000FF404000FF404000FF404000FF404000FFC0C000FF808000FF80
      8000FF808000FF808000FFC0C000FF404000FCFEFC00FCFEFC0084828400FCFE
      FC008482840084828400FCFEFC0084828400FCFEFC00FCFEFC00840284008402
      84008402840084028400FCFEFC00FCFEFC00FF404000C0C0C00038A8A80038A8
      A80038A8A8005800000058000000580000005800000058A8A800584848005848
      4800584848005848480058A8A80058000000FC0204000000FF000000FF000000
      FF0001A2FF0001A2FF0001A2FF00DE616200DE616200DE61620060A060000080
      000000800000008000000080000000800000FF404000C0C0C000C0C0C000C0C0
      C000C0C0C000FF404000FF404000FF404000FF404000FFC0C000FF808000FF80
      8000FF808000FF808000FFC0C000FF404000FCFEFC00FCFEFC0084828400FCFE
      FC008482840084828400FCFEFC0084828400FCFEFC00FCFEFC00FCFEFC008402
      840084028400FCFEFC00FCFEFC00FCFEFC00FF404000C0C0C000C0C0C000FF40
      4000FF404000FF404000FF404000FF404000FF404000FFC0C000FF808000C000
      0000C0000000C0000000FF404000FF404000FC020400FC020400DE616200DE61
      6200DE616200DE616200DE616200DE61620060A0600060A0600060A0600060A0
      600000800000008000000080000000800000FF404000C0C0C000C0C0C000FF40
      4000FF404000FF404000FF404000FF404000FF404000FFC0C000FF808000C000
      0000C0000000C0000000FF404000FF404000FCFEFC00FCFEFC00848284008482
      840084828400848284008482840084828400FCFEFC00FCFEFC00FCFEFC000482
      840004828400FCFEFC00FCFEFC00FCFEFC00FF404000C0C0C000C0C0C000FF40
      4000FF404000FF404000FF404000FF404000FF404000FFC0C000FF808000C000
      0000C0000000C0000000FF404000FF404000FCFE0400FCFE0400FCFE0400DEDF
      6200DEDF6200DEDF6200DEDF6200DEDF6200DEDF620060A0600060A0600060A0
      600060A06000008000000080000000800000FF404000C0C0C000C0C0C000FF40
      4000FF404000FF404000FF404000FF404000FF404000FFC0C000FF808000C000
      0000C0000000C0000000FF404000FF404000FCFEFC00FCFEFC00848284008482
      840084828400848284008482840084828400FCFEFC00FCFEFC00FCFEFC000482
      840004828400FCFEFC00FCFEFC00FCFEFC00FF404000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000FF404000FF404000FFC0C000FF808000FF80
      8000FF808000C0000000FF404000FF404000FCFE0400FCFE0400FCFE0400FCFE
      0400DEDF6200DEDF6200DEDF6200DEDF620097D7FF0097D7FF006DCE69005687
      9C0056879C0056879C000080000000800000FF404000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000FF404000FF404000FFC0C000FF808000FF80
      8000FF808000C0000000FF404000FF404000FCFEFC00FCFEFC00848284008482
      840084828400848284008482840084828400FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FF404000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000FF404000FF404000FFC0C000FF808000FF80
      8000FF808000C0000000FF404000FF404000FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400DEDF6200DEDF6200DEDF620097D7FF0097D7FF0097D7FF005687
      9C0056879C0056879C000080000000800000FF404000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000FF404000FF404000FFC0C000FF808000FF80
      8000FF808000C0000000FF404000FF404000FCFEFC00FCFEFC00FCFEFC008482
      840084828400848284008482840084828400FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FF404000FF404000FF404000FF40
      4000FF404000FF404000FF404000FF404000FF404000FFC0C000FF808000C000
      0000C0000000C0000000FF404000FF404000FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400DEDF6200DEDF620097D7FF0097D7FF0097D7FF008C8C
      FF0056879C0056879C000080000000800000FF404000FF404000FF404000FF40
      4000FF404000FF404000FF404000FF404000FF404000FFC0C000FF808000C000
      0000C0000000C0000000FF404000FF404000FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00848284008482840084828400FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FF404000FF404000FF404000C000
      0000C0000000C0000000C0000000C0000000C0000000FF808000FF808000C000
      0000C0000000C0000000FF404000FF404000FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400DEDF62008C8CFF008C8CFF008C8CFF0097D7
      FF0097D7FF006DCE69000080000000800000FF404000FF404000FF404000C000
      0000C0000000C0000000C0000000C0000000C0000000FF808000FF808000C000
      0000C0000000C0000000FF404000FF404000FCFEFC00FCFEFC00FCFEFC008482
      84008482840084828400FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FF404000FF404000FF404000C000
      0000C0000000C0000000C0000000C0000000C0000000FF808000FF808000FF80
      8000FF808000FF808000FFC0C000FF40400080FFFF0080FFFF00FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE04008C8CFF008C8CFF008C8CFF0097D7
      FF0097D7FF0097D7FF000080000000800000FF404000FF404000FF404000C000
      0000C0000000C0000000C0000000C0000000C0000000FF808000FF808000FF80
      8000FF808000FF808000FFC0C000FF404000FCFEFC00FCFEFC00FCFEFC008482
      84008482840084828400FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FF404000FF404000FF404000C000
      0000C0000000FF404000FF404000FF404000FF404000FFC0C000FF808000FF80
      8000FF808000FF808000FFC0C000FF40400080FFFF0080FFFF0080FFFF00FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE04008C8CFF008C8CFF008C8CFF0097D7
      FF0097D7FF0097D7FF00FCFE040000800000FF404000FF404000FF404000C000
      0000C0000000FF404000FF404000FF404000FF404000FFC0C000FF808000FF80
      8000FF808000FF808000FFC0C000FF404000FCFEFC00FCFEFC00FCFEFC008482
      84008482840084828400FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC0000000000FF404000FF404000C000
      0000C0000000FF404000FF404000FF404000FF404000FF404000C0000000C000
      0000C0000000C0000000FF404000000000000000000080FFFF0080FFFF00FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE04000000000000000000FF404000FF404000C000
      0000C0000000FF404000FF404000FF404000FF404000FF404000C0000000C000
      0000C0000000C0000000FF404000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000402040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000402040000000000000000000000000000000000000000000000
      0000000000000000000000000000C4C2C400A4A2A400A4A2A400A4A2A400C4C2
      C400A4A2A400C4C2C400C4C2C400C4C2C4000000000004820400048204000482
      040004820400048204000482040004820400048204000000FF000000FF0000FF
      FF0000FFFF000482040004820400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040204000402040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040204000402040000000000000000000000000000000000000000000000
      00000000000000000000C4C2C400A4A2A400A4A2A400A4A2A400C4C2C400A4A2
      A400C4C2C400C4C2C400C4C2C400C4C2C4000000000004820400048204000482
      040004820400048204000482040004820400048204000000FF000000FF0000FF
      FF0000FFFF000482040004820400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000402
      0400FCFEFC000402040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000402
      0400FCFEFC000402040000000000000000000000000000000000000000000000
      00000000000000000000A4A2A400A4A2A400A4A2A400C4C2C400A4A2A400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C4000000000004820400048204000482
      0400048204000482040004820400048204000482040000FFFF0000FFFF000000
      FF000000FF000482040004820400000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000004020400FCFE
      FC00FCFEFC000402040000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000004020400FCFE
      FC00FCFEFC000402040000000000000000000000000000000000000000000000
      000000000000C4C2C400A4A2A400A4A2A400C4C2C400A4A2A400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C4000000000004820400048204000482
      0400048204000482040004820400048204000482040000FFFF0000FFFF000000
      FF000000FF000482040004820400000000000000000000000000000000000482
      840004828400000000000000000000000000000000000402040004828400FCFE
      FC00FCFEFC000402040000000000000000000000000000000000000000000482
      840004828400000000000000000000000000000000000402040004020400FCFE
      FC00FCFEFC000402040000000000000000000000000000000000000000000000
      0000C4C2C400A4A2A400A4A2A400C4C2C400A4A2A400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400A4A2A4000000000004820400048204000482
      0400048204000482040000FFFF0000FFFF000000FF000000FF00048204000482
      0400048204000482040004820400000000000000000000000000000000000482
      840004828400000000000000000000000000040204000482840004828400FCFE
      FC00FCFEFC000402040000000000000000000000000000000000000000000482
      840004828400000000000000000000000000040204000402040004020400FCFE
      FC00FCFEFC000402040000000000000000000000000000000000000000000000
      0000C4C2C400A4A2A400C4C2C400A4A2A400C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C400A4A2A400A4A2A40000000000FC020400FC020400FC02
      0400FC0204000482040000FFFF0000FFFF000000FF000000FF00048204000482
      0400048204000482040004820400000000000000000000000000040284000402
      840004028400000000000000000004020400FCFEFC0004028400040284000402
      8400FCFEFC000402040000000000000000000000000000000000040284000402
      840004028400000000000000000004020400FCFEFC0004020400040204000402
      0400FCFEFC00040204000000000000000000000000000000000000000000C4C2
      C400A4A2A400A4A2A400A4A2A400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400A4A2A400A4A2A400A4A2A400A4A2A40000000000FC020400FC020400FC02
      0400FC020400FC0204000000FF000000FF0000FFFF0000FFFF00048204000482
      0400048204000482040004820400000000000000000000000000040284000402
      840004028400000000000000000004020400FCFEFC0004028400040284000402
      8400FCFEFC000402040000000000000000000000000000000000040284000402
      840004028400000000000000000004020400FCFEFC0004020400040204000402
      0400FCFEFC00040204000000000000000000000000000000000000000000A4A2
      A400A4A2A400A4A2A400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400A4A2
      A400A4A2A400A4A2A400A4A2A4008482840000000000FC020400FC020400FC02
      0400FCFE0400FC0204000000FF000000FF0000FFFF0000FFFF00FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000040284000402
      840004028400000000000000000004020400FCFEFC0004028400040284000402
      8400FCFEFC000402040000000000000000000000000000000000040284000402
      840004028400000000000000000004020400FCFEFC0004020400040204000402
      0400FCFEFC000402040000000000000000000000000000000000C4C2C4008482
      84008482A4008482A400A4A2A400C4C2C400A4A2A400C4C2C400A4A2A400A4A2
      A400A4A2A400A4A2A400848284008482840000000000FC020400FCFEFC00FC02
      0400FCFE0400FC020400FCFEFC00FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC02040000000000000000000000000000000000FC02
      040000000000000000000000000004020400FCFEFC00FCFEFC00FC020400FCFE
      FC00FCFEFC00040204000000000000000000000000000000000000000000FC02
      040000000000000000000000000004020400FCFEFC00FCFEFC0004020400FCFE
      FC00FCFEFC0004020400000000000000000000000000C4C2C400848284008482
      A4008482A40084A2A4008482A400A4A2A400C4C2C400A4A2A400A4A2A400A4A2
      A400A4A2A4008482840084828400C4C2C40000000000FC020400FC020400FCFE
      0400FCFEFC00FCFE0400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000FC020400FC02
      040000000000000000000000000004020400FCFEFC00FCFEFC00FC020400FC02
      0400FCFEFC000402040000000000000000000000000000000000FC020400FC02
      040000000000000000000000000004020400FCFEFC00FCFEFC00040204000402
      0400FCFEFC00040204000000000000000000000000000482A4000482E4000482
      E40084A2C4008482A40084A2C4008482A400A4A2A400A4A2A400A4A2A400A4A2
      A40084828400C4C2C400000000000000000000000000FCFE0400FCFE0400FCFE
      FC0004FEFC00FCFEFC00FCFE0400FCFE0400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000FC020400FC02
      040000000000000000000000000004020400FCFEFC00FCFEFC00FC020400FC02
      0400040204000000000000000000000000000000000000000000FC020400FC02
      040000000000000000000000000004020400FCFEFC00FCFEFC00040204000402
      040004020400000000000000000000000000000000000482C40004A2E40044A2
      E40044A2E40084A2E4008482A4008482A400A4A2A400A4A2A400A4A2A400C4C2
      C4000000000000000000000000000000000000000000FC020400FC020400FCFE
      0400FCFEFC00FCFE0400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      000000000000000000000000000004020400FCFEFC00FCFEFC00FCFEFC000402
      0400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000004020400FCFEFC00FCFEFC00FCFEFC000402
      0400000000000000000000000000000000000000000044A2C40044C2E40084E2
      E40044C2E40004A2E40084A2A4008482A4008482A400C4C2C400000000000000
      00000000000000000000000000000000000000000000FC020400FCFEFC00FC02
      0400FCFE0400FC020400FCFEFC00FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      000000000000000000000000000004020400FCFEFC00FCFEFC00040204000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000004020400FCFEFC00FCFEFC00040204000000
      000000000000000000000000000000000000000000008482C40044C2E40084E2
      E40044C2E40004A2E4008482A400848284000000000000000000000000000000
      00000000000000000000000000000000000000000000FC020400FC020400FC02
      0400FCFE0400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      000000000000000000000000000004020400FCFEFC0004020400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000004020400FCFEFC0004020400000000000000
      0000000000000000000000000000000000000000000000000000C4C2C40044A2
      C40044A2E4000482C400C4C2C400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000040204000402040000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000040204000402040000000000000000000000
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
      0000000000000000000000000000000000000000000004820400048204000482
      0400048204000482040004820400048204000482040004820400048204000482
      0400048204000482040004820400000000000000000004820400048204000482
      04000482040004820400048204000482040004820400FCFE0400000000000000
      0000FCFE04000482040004820400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000402040004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040004020400040204000000000004820400048204000482
      0400048204000482040004820400FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC0004820400000000000000000004820400048204000482
      040004820400048204000482040004820400048204000000000004FE040004FE
      0400000000000482040004820400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000004020400C4C2C400C4C2C4000402
      0400000000000000000000000000000000000000000004020400000000000000
      0000000000000000000000000000040204000000000004820400048204000482
      0400048204000482040004820400FCFEFC008482840084828400848284008482
      840084828400FCFEFC0004820400000000000000000004820400048204000482
      040004820400048204000482040004820400048204000000000004FE040004FE
      0400000000000482040004820400000000008482840084828400848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      84008482840084828400848284008482840004020400C4C2C400C4C2C4000402
      0400000000000000000000000000000000000000000004020400000000000000
      0000000000000000000000000000040204000000000004820400048204000482
      0400048204000482040004820400FCFEFC008482840084828400848284008482
      840084828400FCFEFC0004820400000000000000000004820400048204000482
      04000482040004820400048204000482040004820400FCFE0400000000000000
      0000FCFE040004820400048204000000000084828400C4C2C400C4C2C400C4C2
      C4008482840000000000848284008482840084828400C4C2C400000000008482
      8400C4C2C400C4C2C400C4C2C4008482840004020400C4C2C400C4C2C4000402
      0400000000000000000000000000000000000000000004020400000000000000
      00000000000000000000000000000402040000000000FC020400048204000482
      0400048204000482040004820400FCFEFC008482840084828400848284008482
      840084828400FCFEFC0004820400000000000000000004820400048204000482
      0400048204000482040004820400048204000482040004820400048204000482
      04000482040004820400048204000000000084828400C4C2C40084828400C4C2
      C40084828400C4C2C40084828400848284008482840000000000C4C2C4008482
      8400C4C2C40084828400C4C2C400848284000402040004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      04000402040004020400040204000402040000000000FC020400FC0204000482
      040004820400048204000402FC000402FC000402FC000402FC000402FC000402
      FC000402FC000402FC000402FC000000000000000000FC020400FC020400FC02
      0400FC0204000482040004820400FCFE0400FCFE040004820400048204000482
      040004820400048204000482040000000000848284008482840084828400C4C2
      C4008482840000000000848284008482840084828400C4C2C400000000008482
      8400C4C2C40084828400848284008482840004020400C4C2C400C4C2C4000402
      0400000000000000000000000000000000000000000004020400000000000000
      00000000000000000000000000000402040000000000FC020400FC020400FC02
      0400FC020400FC020400FC0204000402FC00FCFEFC000402FC00FCFEFC000402
      FC00FCFEFC000402FC00048204000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FCFE0400FCFE040004820400048204000482
      04000482040004820400048204000000000084828400C4C2C40084828400C4C2
      C40084828400C4C2C40084828400848284008482840000000000C4C2C4008482
      8400C4C2C40084828400C4C2C4008482840004020400C4C2C400C4C2C4000402
      0400000000000000000000000000000000000000000004020400000000000000
      00000000000000000000000000000402040000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC0204000402FC00FCFEFC000402FC00FCFE
      FC000402FC00FC020400FC0204000000000000000000FC020400FC020400FC02
      0400FCFE0400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC0204000000000084828400C4C2C400C4C2C400C4C2
      C4008482840000000000848284008482840084828400C4C2C400000000008482
      8400C4C2C400C4C2C400C4C2C4008482840004020400C4C2C400C4C2C4000402
      0400000000000000000000000000000000000000000004020400000000000000
      00000000000000000000000000000402040000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC020400FC0204000402FC00FCFEFC000402
      FC00FC020400FC020400FC0204000000000000000000FC02040000000000FC02
      0400FCFE0400FC02040000000000FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000008482840084828400848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284008482840084828400848284000402040004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040004020400040204000000000004FEFC00FC02040004FE
      FC00FC020400FC020400FC020400FC020400FC020400FC0204000402FC00FC02
      0400FC020400FC020400FC0204000000000000000000FC020400FC020400FCFE
      040000000000FCFE0400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000004020400C4C2C400C4C2C4000402
      0400848284008482840084828400848284008482840004020400848284008482
      8400848284008482840084828400040204000000000004FEFC00FC02040004FE
      FC00FC02040004FEFC00FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC0204000000000000000000FCFE0400FCFE04000000
      000004FEFC0000000000FCFE0400FCFE0400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000004020400C4C2C400C4C2C4000402
      0400848284008482840084828400848284008482840004020400848284008482
      84008482840084828400848284000402040000000000FC02040004FEFC0004FE
      FC0004FEFC00FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC0204000000000000000000FC020400FC020400FCFE
      040000000000FCFE0400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000402040004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040004020400040204000000000004FEFC0004FEFC0004FE
      FC0004FEFC0004FEFC0004FEFC00FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC0204000000000000000000FC02040000000000FC02
      0400FCFE0400FC02040000000000FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004FEFC0004FEFC0004FE
      FC0004FEFC00FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC0204000000000000000000FC020400FC020400FC02
      0400FCFE0400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004020400C4DEC400C4DE
      C400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DE
      C400C4DEC400C4DEC400C4DEC400FCFEFC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004020400FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC000402
      0400FCFEFC0004020400C4DEC400FCFEFC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008482840004020400C4DE
      C400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DE
      C400C4DEC4000000000000000000000000000000000004020400FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC0004020400FCFEFC00C4DEC400FCFEFC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084828400040204000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C4DEC4000000000000000000000000000000000004020400FCFEFC00FCFE
      FC000402040004020400FCFEFC00FCFEFC00040204000402040004020400FCFE
      FC0004020400FCFEFC00C4DEC400FCFEFC000000000000000000000000000402
      0400040204000402040000000000000000000000000004020400040204000402
      0400040204000402040000000000000000000000000000000000000000000402
      0400040204000402040000000000000000000000000004020400040204000402
      0400040204000402040000000000000000000000000084828400040204000000
      0000040204000402040000000000000000000000000004020400040204000000
      0000C4DEC4000000000000000000000000000000000004020400FCFEFC000402
      0400FCFEFC00FCFEFC0004020400FCFEFC0004020400FCFEFC00FCFEFC00FCFE
      FC0004020400FCFEFC00C4DEC400FCFEFC000000000000000000040204000000
      0000000000000000000004020400000000000000000004020400000000000000
      0000000000000000000000000000000000000000000000000000040204000000
      0000000000000000000004020400000000000000000004020400000000000000
      0000000000000000000000000000000000000000000084828400040204000000
      0000040204000402040004020400000000000402040004020400040204000000
      0000C4DEC4000000000000000000000000000000000004020400FCFEFC000402
      0400FCFEFC000402040004020400FCFEFC0004020400FCFEFC00FCFEFC00FCFE
      FC0004020400FCFEFC00C4DEC400FCFEFC000000000004020400000000000000
      0000000000000000000000000000040204000000000004020400000000000000
      0000000000000000000000000000000000000000000004020400000000000000
      0000000000000000000000000000040204000000000004020400000000000000
      0000000000000000000000000000000000000000000084828400040204000000
      0000000000000402040004020400040204000402040004020400000000000000
      0000C4DEC4000000000000000000000000000000000004020400FCFEFC000402
      0400FCFEFC00FCFEFC00FCFEFC00FCFEFC0004020400FCFEFC00FCFEFC00FCFE
      FC0004020400FCFEFC00C4DEC400FCFEFC000000000004020400000000000000
      0000000000000402040004020400040204000000000004020400000000000000
      0000000000000000000000000000000000000000000004020400000000000000
      0000000000000402040004020400040204000000000004020400000000000000
      0000000000000000000000000000000000000000000084828400040204000000
      0000000000000000000004020400040204000402040000000000000000000000
      0000C4DEC4000000000000000000000000000000000004020400FCFEFC000402
      0400FCFEFC00FCFEFC0004020400FCFEFC0004020400FCFEFC00FCFEFC00FCFE
      FC0004020400FCFEFC00C4DEC400FCFEFC000000000004020400000000000000
      0000000000000000000000000000000000000000000004020400000000000000
      0000000000000000000000000000000000000000000004020400000000000000
      0000000000000000000000000000000000000000000004020400000000000000
      0000000000000000000000000000000000000000000084828400040204000000
      0000000000000402040004020400040204000402040004020400000000000000
      0000C4DEC4000000000000000000000000000000000004020400FCFEFC00FCFE
      FC000402040004020400FCFEFC00FCFEFC0004020400FCFEFC00FCFEFC00FCFE
      FC0004020400FCFEFC00C4DEC400FCFEFC000000000004020400000000000000
      0000000000000000000000000000040204000000000004020400000000000000
      0000000000000000000000000000000000000000000004020400000000000000
      0000000000000000000000000000040204000000000004020400000000000000
      0000000000000402FC0000000000000000000000000084828400040204000000
      0000040204000402040004020400000000000402040004020400040204000000
      0000C4DEC4000000000000000000000000000000000004020400FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC0004020400FCFEFC00C4DEC400FCFEFC000000000000000000040204000000
      0000000000000000000004020400000000000000000004020400000000000000
      0000000000000000000000000000000000000000000000000000040204000000
      0000000000000000000004020400000000000000000004020400000000000000
      00000402FC000402FC000402FC00000000000000000084828400040204000000
      0000040204000402040000000000000000000000000004020400040204000000
      0000C4DEC4000000000000000000000000000000000004020400FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC000402
      0400FCFEFC0004020400C4DEC400FCFEFC000000000000000000000000000402
      0400040204000402040000000000000000000000000004020400000000000000
      0000000000000000000000000000000000000000000000000000000000000402
      0400040204000402040000000000000000000000000004020400000000000000
      0000000000000402FC0000000000000000000000000084828400040204000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C4DEC4000000000000000000000000000000000004020400FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00C4DEC400FCFEFC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084828400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400C4DEC4000000000000000000000000000000000004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040004020400FCFEFC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084828400848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004020400040204000000
      0000000000000402040004020400040204000402040000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000402040000000000040204000402
      0400000000000402040000000000000000000000000000000000000000000000
      0000000000000402040004020400040204008482840004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      040004020400040204000402040004020400C4C2C40000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000402040000000000000000000000
      0000000000000402040000000000000000000000000004020400000000000000
      0000000000000402040000000000040204008482840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000402040084828400C4C2C400848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284008482840084828400000000000000000004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040004020400000000000402040000000000000000000402
      0400000000000402040000000000000000000000000004020400040204000000
      0000040204000000000000000000040204008482840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000040204008482840000000000C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C40084828400000000008482840000000000848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284008482840084828400040204000000000004020400040204000000
      0000000000000402040000000000000000000000000004020400000000000402
      0400000000000000000004020400000000008482840000000000000000000000
      0000000000000402040004020400000000000000000004020400040204000402
      0400000000000000000000000000040204008482840000000000C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C40084828400000000008482840000000000C4DEC400C4DE
      C400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DE
      C400C4DEC400C4DEC40084828400040204000000000084828400848284008482
      8400848284008482840084828400848284008482840004020400000000000000
      0000000000000402040000000000000000008482840000000000000000000000
      0000040204000000000000000000040204000000000004020400000000000000
      0000000000000000000000000000040204008482840000000000C4C2C400C4C2
      C400C4C2C4000402040004020400C4C2C400C4C2C40004020400040204000402
      0400C4C2C400C4C2C40084828400000000008482840000000000C4DEC400C4DE
      C400C4DEC4000402040004020400C4DEC400C4DEC40004020400040204000402
      0400C4DEC400C4DEC40084828400040204000000000084828400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C40004020400000000000000
      0000000000000402040000000000000000008482840000000000000000000000
      0000040204000000000004020400040204000000000004020400000000000000
      0000000000000000000000000000040204008482840000000000C4C2C400C4C2
      C40004020400C4C2C400C4C2C40004020400C4C2C40004020400C4C2C400C4C2
      C400C4C2C400C4C2C40084828400000000008482840000000000C4DEC400C4DE
      C40004020400C4DEC400C4DEC40004020400C4DEC40004020400C4DEC400C4DE
      C400C4DEC400C4DEC40084828400040204000000000084828400840204008402
      0400840204008402040084020400840204008402040004020400000000000000
      0000000000000000000004020400000000008482840000000000000000000000
      0000040204000000000000000000000000000000000004020400000000000000
      0000000000000000000000000000040204008482840000000000C4C2C400C4C2
      C40004020400C4C2C4000402040004020400C4C2C40004020400C4C2C400C4C2
      C400C4C2C400C4C2C40084828400000000008482840000000000C4DEC400C4DE
      C40004020400C4DEC4000402040004020400C4DEC40004020400C4DEC400C4DE
      C400C4DEC400C4DEC4008482840004020400000000008482840084020400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C40004020400040204000402
      0400040204000402040004020400040204008482840000000000000000000000
      0000040204000000000000000000040204000000000004020400000000000000
      0000000000000000000000000000040204008482840000000000C4C2C400C4C2
      C40004020400C4C2C400C4C2C400C4C2C400C4C2C40004020400C4C2C400C4C2
      C400C4C2C400C4C2C40084828400000000008482840000000000C4DEC400C4DE
      C40004020400C4DEC400C4DEC400C4DEC400C4DEC40004020400C4DEC400C4DE
      C400C4DEC400C4DEC40084828400040204000000000084828400840204008402
      0400840204008402040084020400840204008402040084020400840204008402
      0400848284000000000000000000000000008482840000000000000000000000
      0000000000000402040004020400000000000000000004020400000000000000
      0000000000000000000000000000040204008482840000000000C4C2C400C4C2
      C40004020400C4C2C400C4C2C40004020400C4C2C40004020400C4C2C400C4C2
      C400C4C2C400C4C2C40084828400000000008482840000000000C4DEC400C4DE
      C40004020400C4DEC400C4DEC40004020400C4DEC40004020400C4DEC400C4DE
      C400C4DEC400C4DEC40084828400040204000000000084828400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400848284000000000000000000000000008482840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000040204008482840000000000C4C2C400C4C2
      C400C4C2C4000402040004020400C4C2C400C4C2C40004020400C4C2C400C4C2
      C400C4C2C400C4C2C40084828400000000008482840000000000C4DEC400C4DE
      C400C4DEC4000402040004020400C4DEC400C4DEC40004020400C4DEC400C4DE
      C400C4DEC400C4DEC40084828400040204000000000084828400C4C2C4008402
      040084020400840204008402040084020400C4C2C400C4C2C400C4C2C400C4C2
      C400848284000000000000000000000000008482840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000040204008482840000000000C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C40084828400000000008482840000000000C4DEC400C4DE
      C400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DEC400C4DE
      C400C4DEC400C4DEC40084828400040204000000000084828400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400848284000000000000000000000000008482840004020400040204000402
      0400840204008402040084020400840204008402040084020400840204008402
      0400040204000402040004020400040204008482840000000000C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C40084828400000000008482840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000040204000000000084828400C4C2C4008402
      040084020400840204008402040084020400840204008402040084020400C4C2
      C400848284000000000000000000000000008482840000000000040204008402
      0400840204008402040084020400840204008402040084020400840204008402
      0400000000000402040000000000040204008482840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C4C2C400000000000000000084828400848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284008482840084828400000000000000000084828400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400848284000000000000000000000000008482840084828400848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284008482840084828400C4C2C4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084828400848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284000000000000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000840204000000
      000000000000FC02040084020400840204000000000084020400000000008402
      040000000000FC0204008402040084020400000000000000000000000000C000
      0000C0000000C000000000000000C0000000C0000000C000000000000000C000
      0000C00000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040204000402
      0400040204000402040004020400040204000402040004020400000000000000
      0000000000000000000000000000000000000000000000000000840204000000
      0000000000008402040000000000000000000000000000000000840204000000
      0000000000008402040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000402040004C2
      2400FCFE0400FCFE0400FCFE0400FCFE0400FCFE04000402040004C224000000
      0000000000000000000000000000000000000000000000000000840204000000
      00000000000084020400FC020400840204000000000084020400000000008402
      0400000000008402040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400000000000000000004020400FCFE
      040004C22400FCFE0400FCFE0400FCFE0400FCFE0400040204000000000004C2
      2400000000000000000000000000000000000000000000000000840204000000
      000000000000FC02040084020400FC0204000000000000000000000000000000
      0000000000008402040084020400000000000000000000000000C0000000C000
      0000C0000000C0000000C0000000C0000000C000000000000000C0000000C000
      0000C0000000C00000000000000000000000C4C2C40004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      040004020400040204000402040004020400000000000000000004020400FCFE
      0400FCFE040004C22400FCFE0400FCFE0400FCFE040004020400000000000000
      000004C224000000000000000000000000000000000084020400840204008402
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000008402040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C40004020400040204000402040004020400040204000402
      040004020400040204000402040004020400000000000000000004020400FCFE
      0400FCFE0400FCFE040004020400040204000402040004020400040204000402
      0400040204000402040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4C2C400C4C2C400C4C2C4000402
      0400040204000402040004020400040204000402040004020400040204000402
      040004020400040204000402040004020400000000000000000004020400FCFE
      0400FCFE0400FCFE040004020400FCFE0400FCFE040004020400FCFE0400FCFE
      0400FCFE04000402040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0000000C0000000C0000000C0000000C0000000C00000000000
      000000000000000000000000000000000000C4C2C40004020400C4C2C4000402
      0400040204000402040004020400040204000402040004020400040204000402
      040004020400040204000402040004020400000000000000000004020400FCFE
      0400FCFE0400FCFE040004020400FCFE0400FCFE040004020400FCFE0400FCFE
      0400FCFE04000402040000000000000000000000000084020400000000008402
      040000000000FC02040084020400FC02040000000000FC02040084020400FC02
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4C2C40004020400C4C2C4000402
      0400C4C2C40004020400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400040204000402040004020400040204000000000000000000040204000402
      0400040204000402040004020400040204000402040004020400FCFE0400FCFE
      0400FCFE04000402040000000000000000000000000084020400000000008402
      0400000000008402040000000000840204000000000084020400C4C2C4008402
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C4000402040004020400040204000402
      04000402040004020400040204000402040000000000000000000000000004C2
      2400000000000000000004020400FCFE0400FCFE0400FCFE040004C22400FCFE
      0400FCFE04000402040000000000000000000000000084020400840204008402
      04000000000084020400000000008402040000000000FC020400840204008402
      040000000000000000000000000000000000000000000000000000FF00000080
      0000008000000080000000800000008000000080000000800000008000000080
      000000800000008000000000000000000000C4C2C40004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040004020400040204000000000000000000000000000000
      000004C224000000000004020400FCFE0400FCFE0400FCFE0400FCFE040004C2
      2400FCFE04000402040000000000000000000000000084020400000000008402
      0400000000000000000000000000000000000000000000000000000000008402
      040000000000000000000000000000000000000000000000000000FF000000C0
      00000000FF000000FF0000C0000000C0000000FFFF0000FFFF0000FFFF0000C0
      000000C00000008000000000000000000000C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C4000000000000000000000000000000
      00000000000004C2240004020400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      040004C224000402040000000000000000000000000084020400000000008402
      0400000000000000000000000000000000000000000000000000000000008402
      040000000000000000000000000000000000000000000000000000FF000000C0
      00000000FF0000C0000000C000000000FF000000FF000000FF000000FF000000
      FF0000C000000080000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000004020400040204000402040004020400040204000402
      0400040204000402040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000C0
      000000C0000000C0000000C0000000C0000000C0000000C0000000C0000000C0
      000000C000000080000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000000000000000
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
      00000000000000000000000000000000000000000000FC020400FCFEFC00FCFE
      FC00FCFEFC0004020400FCFEFC00FCFEFC0004020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C4626400C442440000000000000000000000
      00000000000000000000000000000000000000000000FC020400FCFEFC000402
      0400FC020400FC020400FCFEFC00FC020400FCFEFC0004020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000840204000000
      000000000000FC02040084020400840204000000000084020400000000008402
      040000000000FC02040084020400840204000000000000000000840204000000
      000000000000FC02040084020400840204000000000084020400000000008402
      040000000000FC02040084020400840204000000000000000000000000000000
      000084424400C4828400C4626400C44244008442440084424400C4626400C462
      64000000000000000000000000000000000000000000FC020400FC020400FCFE
      FC0004020400FC020400FCFEFC00FC020400FCFEFC0004020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000840204000000
      0000000000008402040000000000000000000000000000000000840204000000
      0000000000008402040000000000000000000000000000000000840204000000
      0000000000008402040000000000000000000000000000000000840204000000
      000000000000840204000000000000000000000000000000000000000000C442
      4400C4828400C44244008442440084424400844244008442440084424400C442
      4400C482840000000000000000000000000000000000FC020400FC020400FC02
      0400FCFEFC0004020400FCFEFC00FC020400FCFEFC0004020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000840204000000
      00000000000084020400FC020400840204000000000084020400000000008402
      0400000000008402040000000000000000000000000000000000840204000000
      00000000000084020400FC020400840204000000000084020400000000008402
      040000000000840204000000000000000000000000000000000000000000C462
      6400C46264008442440084424400844244008442440084424400844244008442
      440084424400C4828400000000000000000000000000FC020400FCFEFC00FCFE
      FC0004020400FC020400FCFEFC00FCFEFC0004020400FC020400FC020400FC02
      0400FC020400FC02040004828400000000000000000000000000840204000000
      000000000000FC02040084020400FC0204000000000000000000000000000000
      0000000000008402040084020400000000000000000000000000840204000000
      000000000000FC02040084020400FC0204000000000000000000000000000000
      0000000000008402040084020400000000000000000000000000000000008442
      4400844244008442440084424400842224008422240084222400842224008422
      240084222400C4424400000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC020400FC020400FC020400048284000482
      8400048284000482840004828400000000000000000084020400840204008402
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000008402040000000000000000000000000084020400840204008402
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000008402040000000000000000000000000000000000C46264008442
      4400844244008442440084424400442224004422240044222400442224004422
      24004422240084424400000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC0204000482840004828400048284000482
      8400048284000482840004828400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C46264008442
      4400844244008442440044222400442224004422240044222400442224004422
      24004422240044222400000000000000000000000000FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE040004828400048284000482
      8400048284000482840004828400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4424400C482
      8400C4A284008442440084424400442224004422240044222400442224004422
      24004422240044222400000000000000000000000000FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400048284000482
      8400048284000482840004828400000000000000000084020400000000008402
      040000000000FC02040084020400FC02040000000000FC02040084020400FC02
      0400000000000000000000000000000000000000000084020400000000008402
      040000000000FC02040084020400FC02040000000000FC02040084020400FC02
      040000000000000000000000000000000000000000000000000000000000C482
      6400844244008442440084424400442224004422240044222400442224004422
      24004422240044222400000000000000000000000000FCFE0400FCFE040004FE
      FC0004FEFC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400048284000482840004828400000000000000000084020400000000008402
      0400000000008402040000000000840204000000000084020400C4C2C4008402
      0400000000000000000000000000000000000000000084020400000000008402
      0400000000008402040000000000840204000000000084020400C4C2C4008402
      0400000000000000000000000000000000000000000000000000000000004422
      2400442224004422240044222400442224004422240044222400442224004422
      24004422240000000000000000000000000000000000FCFE040004FEFC0004FE
      FC0004FEFC0004FEFC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE040004828400000000000000000084020400840204008402
      04000000000084020400000000008402040000000000FC020400840204008402
      0400000000000000000000000000000000000000000084020400840204008402
      04000000000084020400000000008402040000000000FC020400840204008402
      0400000000000000000000000000000000000000000000000000000000008442
      4400442224004422240044222400442224004422240044222400442224004422
      24004442440000000000000000000000000000000000FCFE040004FEFC0004FE
      FC0004FEFC0004FEFC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400000000000000000084020400000000008402
      0400000000000000000000000000000000000000000000000000000000008402
      0400000000000000000000000000000000000000000084020400000000008402
      0400000000000000000000000000000000000000000000000000000000008402
      0400000000000000000000000000000000000000000000000000000000000000
      0000C4C2C400846264004422240084626400444244008462640044222400C4C2
      C4000000000000000000000000000000000000000000FCFE0400FCFE040004FE
      FC0004FEFC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400000000000000000084020400000000008402
      0400000000000000000000000000000000000000000000000000000000008402
      0400000000000000000000000000000000000000000084020400000000008402
      0400000000000000000000000000000000000000000000000000000000008402
      0400000000000000000000000000000000000000000000000000000000000000
      000000000000F4FAFC00C4DEC400C4DEC40000000000C4C2C400C4C2C4000000
      00000000000000000000000000000000000000000000FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400000000000000000000000000000000000000
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
      0000000000000000000004FEFC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008402040000000000FC02
      0400000000000000000000000000000000000482040000000000840204000000
      000004FEFC00000000000402FC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848284008482
      840084828400848284008482840084828400848284008482840084828400C4C2
      C400000000000000000000000000000000000000000000000000000000000000
      00000402FC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084828400C4C2
      C400048284000482040004828400048284000482840004820400048284008482
      8400C4C2C4000000000000000000000000000402FC0000000000048204000000
      0000000000000000000004FEFC000000000000000000FC020400000000000402
      FC00000000000000000084020400000000000000000084828400848284008482
      8400848284000000000000000000000000000000000000000000000000008482
      8400848284008482840084828400000000000000000084828400848284008482
      8400848284000000000000000000000000000000000000000000000000008482
      8400848284008482840084828400000000000000000000000000848284000482
      8400C4C2C4000482840004820400048284000482040004828400048284000482
      040084828400C4C2C40000000000000000000000000004FEFC00000000000000
      0000000000000000000000000000000000008402040000000000000000000000
      000000000000000000000000000000000000000000008482840004FEFC0004FE
      FC00C4C2C400848284008482840084828400848284008482840084828400C4C2
      C40004FEFC0004FEFC0084828400000000000000000084828400FCFE0400FCFE
      0400C4C2C400848284008482840084828400848284008482840084828400C4C2
      C400FCFE0400FCFE040084828400000000000000000000000000848284000482
      840004828400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C4008482840000000000000000000000000000000000FC0204000000
      0000000000000482040000000000000000000000000000000000000000000000
      000000000000048204000000000000000000000000008482840004FEFC000402
      FC0004FEFC0004FEFC0004FEFC0004FEFC0004FEFC0004FEFC0004FEFC0004FE
      FC00FC02040004FEFC0084828400000000000000000084828400FCFE0400FC02
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FC020400FCFE04008482840000000000000000000000000084828400FCFE
      040004828400C4C2C40004828400048284000482040004828400048204000482
      0400048204008482840000000000000000000000000000000000000000008402
      0400000000000000000000000000FC0204000000000004FEFC00000000008402
      04000000000000000000000000000000000000000000848284000402FC00FC02
      04000402FC0004FEFC000402FC0004FEFC000402FC0004FEFC000402FC0004FE
      FC000402FC00FC02040084828400000000000000000084828400FC020400FC02
      0400FC020400FCFE0400FC020400FCFE0400FC020400FCFE0400FC020400FCFE
      0400FC020400FC0204008482840000000000000000000000000084828400FCFE
      0400FCFE0400C4C2C40004828400048284000482840004828400048284000482
      8400048204008482840000000000000000000000000004820400000000000000
      000000000000000000000402FC00000000000000000000000000000000000000
      0000FC020400000000000402FC00000000000000000084828400FC0204000402
      FC00FC0204000402FC00FC0204000402FC00FC0204000402FC00FC0204000402
      FC00FC0204000402FC0084828400000000000000000084828400FC020400FC02
      0400FC020400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC0204008482840000000000000000000000000084828400FC02
      0400FCFE0400C4C2C400FCFE0400FCFE0400FCFE040004828400048284000482
      8400048284008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C4C2C40084828400FC02
      04000402FC00FC0204000402FC00FC0204000402FC00FC0204000402FC00FC02
      04000402FC0084828400C4C2C4000000000000000000C4C2C40084828400FC02
      0400FC02040084020400FC020400FC020400FC02040084020400FC0204008402
      0400FC02040084828400C4C2C40000000000000000000000000084828400FC02
      0400FC020400C4C2C400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE04008482840000000000000000000000000000000000C4C2C4000000
      00000000000000000000C4C2C400000000000000000000000000C4C2C4000000
      0000C4C2C400000000000000000000000000000000000000000084828400FC02
      0400FC0204000402FC00FC0204000402FC00FC0204000402FC00FC020400FC02
      0400FC0204008482840000000000000000000000000000000000848284008402
      040084020400FC02040084020400FC02040084020400FC020400840204008402
      0400840204008482840000000000000000000000000000000000848284008402
      0400FC020400C4C2C400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC0204008482840000000000000000000000000004020400C4C2C4000000
      00000000000004020400C4C2C400000000000000000004020400C4C2C4000402
      0400C4C2C4000000000000000000000000000000000000000000C4C2C4008482
      8400FC020400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      040084828400C4C2C40000000000000000000000000000000000C4C2C4008482
      8400840204008402040084020400840204008402040084020400840204008402
      040084828400C4C2C40000000000000000000000000000000000848284008402
      040084020400C4C2C400FC020400FC020400C4C2C400C4C2C400FC020400FC02
      0400FC0204008482840000000000000000000000000004020400C4C2C400C4C2
      C4000000000004020400C4C2C400C4C2C4000000000004020400C4C2C4000402
      040000000000000000000000000000000000000000000000000000000000C4C2
      C40084828400FC020400FC020400FC020400FC020400FC020400FC0204008482
      8400C4C2C400000000000000000000000000000000000000000000000000C4C2
      C400848284008402040084020400840204008402040084020400840204008482
      8400C4C2C4000000000000000000000000000000000000000000C4C2C4008482
      840084020400C4C2C40084020400C4C2C400000000000000000084020400FC02
      0400840204008482840000000000000000000000000004020400040204000000
      0000C4C2C4000402040004020400000000000000000000000000040204000000
      0000C4C2C4000000000000000000000000000000000000000000000000000000
      0000C4C2C40084828400FC020400FC020400FC020400FC02040084828400C4C2
      C400000000000000000000000000000000000000000000000000000000000000
      0000C4C2C400848284008402040084020400840204008402040084828400C4C2
      C40000000000000000000000000000000000000000000000000000000000C4C2
      C40084828400C4C2C40084020400840204008402040084020400840204008402
      0400840204008482840000000000000000000000000004020400C4C2C4000402
      04000000000004020400C4C2C400C4C2C400C4C2C40004020400C4C2C4000402
      0400C4C2C4000000000000000000000000000000000000000000000000000000
      0000000000000000000084828400848284008482840084828400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084828400848284008482840084828400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C4C2C4008482840084828400848284008482840084828400848284008482
      8400848284008482840000000000000000000000000004020400040204000000
      0000000000000402040004020400040204000000000004020400000000000402
      0400000000000000000000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040000000000000000000000000000000000FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC0004FEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC0000000000000000000000000000000000840204000000
      000000000000FC02040084020400840204000000000084020400000000008402
      040000000000FC02040084020400840204000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004020400000000000000
      0000040204000000000000000000040204000000000000000000040204000000
      00000000000004020400000000000000000000000000FCFEFC00FCFEFC000402
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00000000000000000000000000840204000000
      0000000000008402040000000000000000000000000000000000840204000000
      0000000000008402040000000000000000000000000084828400848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284008482840000000000000000000000000004020400000000000000
      0000040204000000000000000000040204000000000000000000040204000000
      00000000000004020400000000000000000000000000FCFEFC00FCFEFC00FCFE
      FC00FCFEFC008482840084828400FCFEFC000402FC00FCFEFC00FCFEFC00FCFE
      FC00FC020400FCFEFC00FCFEFC00000000000000000000000000840204000000
      00000000000084020400FC020400840204000000000084020400000000008402
      0400000000008402040000000000000000000000000084828400000000000000
      0000848284000000000000000000848284000000000000000000848284000000
      0000000000008482840000000000000000000000000004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      04000402040004020400000000000000000000000000FCFEFC00FCFEFC00FC02
      0400FCFEFC008482840004020400FCFEFC00FCFEFC00FCFEFC0004FEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00000000000000000000000000840204000000
      000000000000FC02040084020400FC0204000000000000000000000000000000
      0000000000008402040084020400000000000000000000000000848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284008482840084828400000000000000000004020400000000000000
      0000040204000000000000000000040204000000000000000000040204000000
      00000000000004020400000000000000000000000000FCFEFC00FCFEFC00FCFE
      FC00FCFEFC008482840004020400FCFEFC00FCFEFC00FCFEFC00FCFEFC000402
      FC00FCFEFC0004FEFC00FCFEFC00000000000000000084020400840204008402
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000008402040000000000000000000000000000000000848284000000
      0000000000008482840000000000000000008482840000000000000000008482
      8400000000000000000084828400000000000000000004020400000000000000
      0000040204000000000000000000040204000000000000000000040204000000
      00000000000004020400000000000000000000000000FCFEFC0004FEFC00FCFE
      FC00FCFEFC008482840004020400848284008482840084828400FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284008482840084828400848284000000000004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      04000402040004020400000000000000000000000000FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00848284000402040004020400040204000402040084828400FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400000000000000000084828400000000000000000084828400000000000000
      0000848284000000000000000000848284000000000004020400000000000000
      0000040204000000000000000000040204000000000000000000040204000000
      00000000000004020400000000000000000000000000FCFEFC00FCFEFC000402
      FC00FCFEFC008482840004020400FCFEFC00FCFEFC008482840004020400FCFE
      FC00FCFEFC000402FC00FCFEFC00000000000000000084020400000000000000
      000000000000FC0204008402040000000000FC02040084020400840204000000
      0000FC0204008402040084020400000000000000000000000000000000000000
      0000848284008482840084828400848284008482840084828400848284008482
      8400848284008482840084828400848284000000000004020400000000000000
      0000040204000000000000000000040204000000000000000000040204000000
      00000000000004020400000000000000000000000000FCFEFC00FCFEFC00FCFE
      FC00FCFEFC008482840004020400FCFEFC00FCFEFC008482840004020400FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00000000000000000084020400000000000000
      0000000000008402040000000000000000008402040000000000840204000000
      0000840204000000000000000000000000000000000000000000000000000000
      0000040204000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      04000402040004020400000000000000000000000000FCFEFC00FC020400FCFE
      FC00FCFEFC00848284000402040084828400848284008482840004020400FCFE
      FC0004828400FCFEFC00FCFEFC00000000000000000084020400840204000000
      0000000000008402040000000000000000000000000084020400840204000000
      0000840204000000000000000000000000000000000000000000000000000000
      0000040204000000000000000000000000000000000004020400040204000402
      0400040204000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC000402040004020400040204000402040084828400FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00000000000000000084020400000000000000
      0000000000008402040000000000000000008402040084020400FC0204000000
      0000840204008402040000000000000000000000000000000000000000000000
      0000040204000000000000000000000000000000000000000000040204000000
      0000000000000000000000000000000000000000000084828400000000008482
      8400000000000000000084828400000000000000000084828400848284008482
      84000000000000000000000000000000000000000000FCFEFC00FCFEFC0004FE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FC020400FCFEFC00000000000000000084020400840204008402
      0400000000008402040000000000000000000000000000000000000000000000
      0000840204000000000000000000000000000000000000000000000000000402
      0400040204000402040000000000000000000000000000000000000000000402
      0400000000000000000000000000000000000000000000000000848284000000
      0000000000000000000084828400000000000000000000000000848284000000
      00000000000000000000000000000000000000000000FCFEFC000402FC00FCFE
      FC00FCFEFC00FCFEFC00FC020400FCFEFC0004828400FCFEFC00FCFEFC0004FE
      FC00FCFEFC00FCFEFC00FCFEFC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040204000000000000000000000000000000000004020400040204000402
      0400040204000000000000000000000000000000000084828400000000008482
      8400000000008482840000000000848284000000000084828400848284008482
      8400000000000000000000000000000000000000000000000000FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC0000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000402
      0400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004828400040284000482
      8400048284000402840004828400048284000482840004828400040284000482
      8400048284000482840004828400000000000000000000000000000000000402
      0400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000402040004FE040004FE
      040004FE040004020400FC020400FC020400FC0204000402040004FE040004FE
      040004FE0400040204000000000000000000000000000000000084828400FCFE
      FC00FC020400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FCFEFC000402040000000000000000000000000004028400048284000482
      8400040284000482840004828400040284000482840004828400048284000482
      8400048284000482840004828400000000000000000004020400040204008482
      8400040204000402040000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000402040004FE040004FE
      040004FE040004020400FC020400FC020400FC0204000402040004FE040004FE
      040004FE0400040204000000000000000000000000000000000084828400FCFE
      FC00FC020400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FCFEFC000402040000000000000000000000000004828400040284000482
      8400048284000482840004828400048284000482840004828400048284000482
      8400848284000482840004828400000000000000000000000000000000000402
      0400FCFE04008482840084828400848284000000000000000000000000000000
      000004020400000000000000000000000000000000000402040004FE040004FE
      040004FE040004020400FC020400FC020400FC0204000402040004FE040004FE
      040004FE0400040204000000000000000000000000000000000084828400FCFE
      FC00FC020400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FCFEFC000402040000000000000000000000000004828400048284000482
      8400048284000482840004828400048284008482840084828400048284008482
      8400048284008482840004828400000000000000000000000000000000000402
      040084828400FCFE0400FCFE0400FCFE04008482840084828400848284000000
      0000040204000000000000000000000000000000000004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      040004020400040204000000000000000000000000000000000084828400FCFE
      FC00FC020400FC020400FC020400FC020400FC02040004820400048204000482
      0400FCFEFC000402040000000000000000000000000004828400048284000482
      840004828400FC020400FC020400FC0204008482840000000000848284000482
      84008482840084828400FC020400000000000000000000000000000000000000
      000084828400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400040204000402
      0400848284000402040004020400000000000000000004020400FC020400FC02
      0400FC02040004020400FC020400FC020400FC0204000402040004FE040004FE
      040004FE0400040204000000000000000000000000000000000084828400FCFE
      FC00FC020400FC02040004820400048204000482040004820400048204000482
      0400FCFEFC0004020400000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC020400FC02040000000000000000008482
      84000000000000000000FC020400000000000000000000000000000000000000
      00000000000084828400FCFE0400FCFE0400FCFE0400FCFE0400FCFE04008482
      8400040204000000000000000000000000000000000004020400FC020400FC02
      0400FC02040004020400FC020400FC020400FC0204000402040004FE040004FE
      040004FE0400040204000000000000000000000000000000000084828400FCFE
      FC00FCFE0400FCFE0400FCFE0400FCFE04000482040004820400048204000482
      0400FCFEFC0004020400000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC020400FC020400FC020400FC0204000000
      000000000000FC020400FC020400000000000000000000000000000000000000
      00000000000084828400FCFE0400FCFE0400FCFE0400FCFE0400848284000000
      0000040204000000000000000000000000000000000004020400FC020400FC02
      0400FC02040004020400FC020400FC020400FC0204000402040004FE040004FE
      040004FE0400040204000000000000000000000000000000000084828400FCFE
      FC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400048204000482
      0400FCFEFC0004020400000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      0000000000000000000084828400FCFE0400FCFE040084828400000000000000
      0000000000000000000000000000000000000000000004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      040004020400040204000000000000000000000000000000000084828400FCFE
      FC00FCFE040004FEFC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFEFC000402040000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008482840004020400FCFE040084828400000000000000
      000000000000000000000000000000000000000000000402040004FEFC0004FE
      FC0004FEFC00040204000402FC000402FC000402FC0004020400C4C2C400C4C2
      C400C4C2C400040204000000000000000000000000000000000084828400FCFE
      FC0004FEFC0004FEFC0004FEFC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFEFC000402040000000000000000000000000004020400000000000000
      0000040204000402040000000000040204000000000004020400000000000402
      0400000000000402040000000000040204000000000000000000000000000000
      0000000000000000000000000000040204008482840000000000000000000000
      000000000000000000000000000000000000000000000402040004FEFC0004FE
      FC0004FEFC00040204000402FC000402FC000402FC0004020400C4C2C400C4C2
      C400C4C2C400040204000000000000000000000000000000000084828400FCFE
      FC00FCFE040004FEFC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFEFC000402040000000000000000000000000004020400000000000000
      0000040204000000000000000000040204000000000004020400000000000402
      0400000000000402040000000000000000000000000000000000000000000000
      0000000000000402040004020400848284000402040004020400000000000000
      000000000000000000000000000000000000000000000402040004FEFC0004FE
      FC0004FEFC00040204000402FC000402FC000402FC0004020400C4C2C400C4C2
      C400C4C2C400040204000000000000000000000000000000000084828400FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC000402040000000000000000000000000004020400000000000000
      0000040204000402040000000000040204000402040000000000000000000402
      0400040204000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000040204000000000000000000000000000000
      0000000000000000000000000000000000000000000004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040000000000000000000000000000000000848284008482
      8400848284008482840084828400848284008482840084828400848284008482
      8400848284008482840000000000000000000000000004020400000000000000
      0000040204000000000000000000040204000000000004020400000000000402
      0400000000000402040000000000000000000000000000000000000000000000
      0000000000000000000000000000040204000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000402040004020400040204000000
      0000040204000402040000000000040204000402040000000000000000000402
      0400040204000000000000000000000000000000000000000000000000000000
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
      000000000000FCFCFC00F1F1F100EBEBEB00F5F5F500FCFCFC00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000040204000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EAEAEA00B6B6
      B60094949400898989007F7F7F00828282008C8C8C00B1B1B100E3E3E300FCFD
      FC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FBFBFB00E2E2E200D4D4D400CCCC
      CC00CECECE00FBFBFB0000000000000000000000000000000000C4C2C4008482
      8400848284008482840084828400040204008482840084828400848284008482
      8400C4C2C40000000000000000000000000000000000CFCFCF00929292008A8A
      8A007A7A7A00606060004F4F4F005C5C5C00797A79007C7C7C0082828200BDBD
      BD00FAFAFA000000000000000000FDFEFD000000000000000000000000000000
      0000040204000402040000000000000000000000000000000000000000000402
      0400000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FBFBFB00E4E4E400B5B5B5007F7F7F0066666600A9A9
      A900CCCCCC00F1F1F10000000000000000000000000000000000848284000000
      0000000000000000000000000000040204000000000000000000000000000000
      000084828400000000000000000000000000E5E5E500A8A8A800A2A1A2009C9C
      9C0095959500868786006D6D6D005E5D5E00666566007F7F7F007C7C7C007A7A
      7A00AEADAE00F8F8F80000000000000000000000000000000000000000000000
      0000000000000402040000000000000000000000000000000000040204000402
      0400000000000000000000000000000000000000000000000000000000000000
      000000000000F8F8F800A4A4A4003A3A3A002D2D2D0058585800818181009D9D
      9D00CECECE00FBFBFB0000000000000000000000000000000000848284000000
      0000000000000000000000000000040204000000000000000000000000000000
      000084828400000000000000000000000000C5C5C5009A9A9A00808080007575
      750083838300939393008B8C8B007272720067676700737273008E8E8E007D7D
      7D0079797900BBBBBB00FCFCFC00000000000000000000000000000000000000
      0000000000000000000004020400040204000000000004020400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EAEAEA0053535300262626005252520095959500D6D6D600EAEAEA00DDDD
      DD00000000000000000000000000000000000000000000000000848284000000
      0000000000000000000000000000040204000000000000000000000000000000
      000084828400000000000000000000000000B2B2B2007575750068686800AEAD
      AE008989890096969600B4B4B4008D8D8D00747374006D6D6D00858585009897
      98007C7B7C007E7E7E00DCDCDC00FDFEFD000000000000000000000000000000
      0000000000000000000000000000040204000402040004020400000000000000
      000000000000000000000000000000000000000000000000000000000000E0E0
      E000313131002D2D2D0068686800ABABAB00D6D6D600C7C7C700CFCFCF00E7E7
      E700F8F8F800F8F8F80000000000000000000000000000000000000000008482
      8400000000000000000000000000040204000000000000000000000000008482
      840000000000000000000000000000000000A9A9A90070707000A2A1A200E8E8
      E8009999990097979700F3F3F300D0D0D0008D8D8D0074747400767676009C9D
      9C00919191007C7C7C00A3A3A300FBFBFB000000000000000000000000000000
      0000000000000000000000000000040204000402040000000000000000000000
      0000000000000000000000000000000000000000000000000000EEEEEE002828
      28002D2D2D00686868009D9D9D00BEBEBE00E7E7E700EAEAEA00D7D7D700CCCC
      CC00CCCCCC00CCCCCC00E1E1E100000000000000000000000000000000000000
      0000848284000000000000000000040204000000000000000000848284000000
      000000000000000000000000000000000000B5B5B50079797900B0B0B000EEEE
      EE00C1C1C10093939300C6C6C600FCFCFC00CFCFCF0086868600747474008888
      8800B0B0B0008080800081818100EAEAEA000000000000000000000000000000
      0000000000000402040004020400040204000402040000000000000000000000
      0000000000000000000000000000000000000000000000000000717171002D2D
      2D006868680099999900B8B8B800E1E1E100C3C3C30087878700666666006E6E
      6E0094949400C7C7C700CCCCCC00F4F4F4000000000000000000000000000000
      0000848284000000000000000000040204000000000000000000848284000000
      000000000000000000000000000000000000D4D4D40080808000A2A2A200D8D8
      D800EDEDED00A3A3A3009C9C9C00E2E2E20000000000B5B5B5007B7B7B007B7B
      7B00ACADAC009797970080808000CCCCCC000000000000000000000000000000
      0000000000000000000000000000040204000402040000000000040204000402
      04000000000000000000000000000000000000000000EEEEEE00191919004E4E
      4E008F8F8F00B5B5B500B6B6B6005C5C5C00414141005858580087878700A7A7
      A700AFAFAF00AAAAAA00CCCCCC00F4F4F4000000000000000000000000008482
      8400000000000000000000000000040204000000000000000000000000008482
      840000000000000000000000000000000000F5F5F500919191008D8D8D00B9B8
      B900E6E6E600DADADA009D9D9D00A1A1A100E4E4E400EDEDED008C8C8C007676
      76009B9A9B00ADADAD0084848400C4C4C4000000000000000000000000000000
      0000000000000000000000000000040204000402040004020400040204000000
      00000000000000000000000000000000000000000000D6D6D600202020007373
      7300A4A4A4005B5B5B0047474700585858008C8C8C00D9D9D900DDDDDD00C8C8
      C800C5C5C500CECECE00D5D5D500FBFBFB000000000000000000000000000000
      0000848284000000000000000000040204000000000000000000848284000000
      00000000000000000000000000000000000000000000CACACA00838283009696
      9600ACABAC00E9E9E900D0D0D0009D9D9D009B9B9B00C8C8C800A8A8A8007676
      76008C8C8C00BABABA0089898900DBDCDB000000000000000000000000000000
      0000000000000000000000000000040204000402040000000000000000000000
      00000000000000000000000000000000000000000000F8F8F800565656008C8C
      8C00BFBFBF00ABABAB00B2B2B200D6D6D600DDDDDD00D1D1D100757575002626
      260070707000C2C2C200F8F8F800000000000000000000000000000000000000
      0000000000008482840084828400040204008482840084828400000000000000
      00008482840084828400C4C2C4000000000000000000FBFBFB00A4A5A4008383
      8300878887008F8F8F00D8D8D800D8D8D800A3A3A30094949400A0A0A0007B7B
      7B0081818100B7B7B700A8A9A800F7F7F7000000000000000000000000000000
      0000000000000000000004020400040204000402040004020400000000000000
      0000000000000000000000000000000000000000000000000000D1D1D1008484
      8400AEAEAE00C8C8C800CFCFCF00D7D7D700C3C3C300545454000C0C0C003232
      32008E8E8E00F4F4F40000000000000000000000000000000000000000000000
      0000000000000000000000000000040204000000000000000000000000000000
      0000000000000000000084828400000000000000000000000000F2F3F2009898
      98008282820076767600676667008F8F8F00B6B6B600A8A7A800AAAAAA007E7D
      7E007C7B7C00C3C3C300EDEDED00FDFEFD000000000000000000000000000000
      0000000000000402040004620400046204000462040004620400040204000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F4F4F400F8F8F800E7E7E70078787800333333002D2D2D005B5B5B009B9B
      9B00EEEEEE000000000000000000000000000000000000000000000000000000
      0000000000000000000084828400040204008482840000000000000000000000
      0000C4C2C400848284008482840000000000000000000000000000000000EDED
      ED00A1A1A100888988007F7F7F007D7D7D0086868600A4A4A400A6A7A6007777
      77007D7D7D00E3E3E30000000000000000000000000000000000000000000000
      0000000000000402040004620400040204000402040004620400040204000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F8F8F800141414004E4E4E0088888800A1A1A100BEBEBE00F8F8
      F800000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C4C2C40004020400C4C2C40000000000000000000000
      0000848284000000000084828400000000000000000000000000000000000000
      0000F5F5F500B7B7B700979797009D9D9D00A6A5A600A7A7A7008E8E8E006868
      6800A7A6A700FCFCFC0000000000000000000000000000000000000000000000
      0000000000000000000004020400040204000402040004020400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004C4C4C0085858500DDDDDD00EBEBEB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FDFEFD000000
      000000000000FDFEFD00E0E0E000B3B2B300949494007F7F7F007A7A7A00B3B3
      B300F9FAF9000000000000000000FDFEFD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400000000008482840084828400848284008482840000000000000000008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C4C2C400C4C2C400A4A2A400A4A2A400A4A2A400A4A2
      A400C4C2C400C4C2C40000000000000000000000000000000000848284000000
      0000848284000000000000000000000000000000000084828400848284000000
      0000848284000000000000000000000000000000000000000000000000000000
      000000000000C4C2C400848284000402040084828400C4C2C400000000000000
      0000000000000000000000000000000000000000000084828400848284008482
      8400848284008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C4C2C400A4A2A400A4A2A400A4A2A400A4A2A400A4A2A400A4A2A400A4A2
      A400A4A2A400A4A2A400A4A2A400000000000000000084828400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008482840000000000000000000000000000000000000000000402
      04008482840084828400C4C2C400FCFE0400C4C2C40084828400848284008482
      840084828400C4C2C40000000000000000008482840000000000000000000000
      0000000000000000000084828400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C4C2C400A4A2A400A4A2A400848204004442440044424400444244008482
      0400A4A2A400A4A2A400A4A2A400000000000000000084828400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084828400000000000000000000000000000000008482
      8400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400C4C2C4008482840004020400000000008482840000000000000000000402
      0400000000000000000084828400000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C4C2
      C400C4C2C400A4A2A40084820400444244004442440004020400040204000402
      040084820400A4A2A400A4A2A400000000000000000004020400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000004020400000000000000000000000000C4C2C4008482
      8400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE040084828400000000008482840084828400848284000402
      0400848284008482840084828400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C2C400C4C2
      C400C4C2C400A4A2A40084820400444244004442440004020400040204000402
      040084820400A4A2A400A4A2A400000000000000000004020400848284008482
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000848284000402040000000000000000000000000084828400FCFE
      0400FCFE0400FCFE040004020400848284008482840084828400C4C2C400FCFE
      0400FCFE0400FCFE040084828400000000008482840000000000000000000402
      0400000000000000000084828400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C2C400C4C2
      C400C4C2C400A4A2A40084820400444244004442440004020400040204000402
      040084820400A4A2A400A4A2A400000000000000000004020400000000000000
      0000848284008482840084828400848284008482840084828400848284008482
      840084828400000000000402040000000000000000000000000084828400FCFE
      0400FCFE0400FCFE04008482840000000000000000000000000004020400FCFE
      0400FCFE0400C4C2C40084828400000000008482840000000000000000000402
      0400000000000000000000000000848284000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C2C400C4C2
      C400C4C2C400A4A2A40084820400444244004442440004020400040204000402
      040084820400A4A2A400A4A2A400000000000000000004020400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000402040000000000000000000000000084828400FCFE
      0400FCFE0400FCFE04008482840000000000000000000000000084828400FCFE
      0400FCFE040084828400C4C2C400000000008482840000000000000000000000
      0000040204008482840084828400848284008482840084828400000000000000
      0000000000000000000000000000000000000000000000000000C4C2C400C4C2
      C400C4C2C400A4A2A40084820400444244004442440004020400040204000402
      040084820400A4A2A400A4A2A400000000000000000004020400000000008482
      8400000000008482840084828400848284008482840000000000000000008482
      84000000000000000000040204000000000000000000C4C2C40084828400FCFE
      0400FCFE0400FCFE040084828400000000000000000084828400FCFE0400FCFE
      0400FCFE04008482840000000000000000000000000084828400000000008482
      8400000000000402040000000000000000000000000084828400848284008482
      8400000000000000000000000000000000000000000000000000C4C2C400C4C2
      C400C4C2C400A4A2A40084820400444244004442440004020400040204000402
      040084820400A4A2A400A4A2A400000000000000000004020400848284000000
      0000848284000000000000000000000000000000000084828400848284000000
      0000848284000000000004020400000000000000000084828400C4C2C400FCFE
      0400FCFE0400FCFE0400C4C2C4008482840084828400FCFE0400FCFE0400FCFE
      0400FCFE04008482840000000000000000000000000000000000848284008482
      8400000000000000000004020400040204000000000000000000848284000000
      0000848284000000000000000000000000000000000000000000C4C2C400C4C2
      C400C4C2C400A4A2A40084820400444244004442440004020400040204000402
      040084820400A4A2A400A4A2A400000000000000000004020400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008482840004020400000000000000000084828400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE040004020400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE04008482840000000000000000000000000000000000000000008482
      8400000000000000000000000000000000000402040004020400848284000000
      0000000000008482840000000000000000000000000000000000C4C2C400A4A2
      A400A4A2A4008482040084820400848204008482040044424400444244000402
      040084820400A4A2A400A4A2A400000000000000000004020400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000004020400000000000000000004020400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400C4C2
      C400848284000402040000000000000000000000000000000000000000000000
      0000848284008482840084828400848284008482840000000000040204000000
      0000000000008482840000000000000000000000000000000000848204008482
      04008482040044424400A4A2A400C4C2C400C4C2C400A4A2A400848204008482
      040084820400C4C2C40000000000000000000000000084828400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008482840000000000000000000000000084828400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400C4C2C40084828400848284008482
      8400C4C2C4000000000000000000000000000000000000000000000000000000
      0000000000000000000084828400000000000000000084828400848284000402
      0400848284008482840084828400000000000000000000000000848204008482
      04000402040044424400A4A2A400C4C2C400C4C2C400A4A2A400848204008482
      0400C4C2C4000000000000000000000000000000000000000000848284008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000008482840000000000000000000000000000000000000000008482
      8400FCFE0400C4C2C400848284008482840084828400C4C2C400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848284008482840000000000000000000402
      040000000000000000008482840000000000000000000000000000000000A4A2
      A400A4A2A400848204008482040084820400A4A2A400A4A2A400C4C2C4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848284008482840084828400848284008482840084828400848284008482
      8400848284000000000000000000000000000000000000000000000000000000
      00000402040084828400C4C2C400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008482840000000000000000000000
      0000000000000000000084828400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084828400848284008482
      840084828400848284000000000000000000000000000000000000000000D8D8
      D800AEAEAE009B9B9B009E9E9E009E9E9E009B9B9B00B9B9B900E4E4E4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFDFD008F8F8F004343
      4300424242004343430045454500494949004C4C4C004C4C4C00535353009999
      9900EFEFEF000000000000000000000000000402040004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040004020400040204000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C4C2C400C4C2C40084828400173E1C00173E1C00000000000000
      00000000000000000000000000000000000000000000A9A9A900414141005D5D
      5D00616161006161610063636300686868007373730078787800727272005959
      59005D5D5D00C2C2C20000000000000000000402040000000000000000000402
      0400000000000000000004020400000000000000000004020400000000000000
      0000040204000000000000000000040204000000000000000000000000000000
      0000848284008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C4C2C4008482840084828400848284008482840084828400173E1C000000
      000000000000000000000000000000000000F5F5F5005B5B5B00585858006767
      6700696969006B6B6B006E6E6E00797979008F8F8F0098989800888888007878
      7800616161005E5E5E00DFDFDF00000000000402040000000000000000000402
      0400000000000000000004020400000000000000000004020400000000000000
      0000040204000000000000000000040204000000000000000000000000008482
      8400848284008482840004020400000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C4C2
      C40084828400848284008482840084828400C4C2C4008482840084828400173E
      1C0000000000000000000000000000000000CACACA0047474700656565006C6C
      6C006E6E6E0070707000757575008A8A8A00B7B7B700C3C3C3009A9A9A008080
      800073737300636363007E7E7E00FBFBFB000402040004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040004020400040204000000000000000000848284008482
      8400C4C2C4008482840084828400040204000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C2C400C4C2
      C4008482840084828400000000000000000000000000C4C2C40084828400173E
      1C00173E1C00000000000000000000000000A7A7A70047474700696969006D6D
      6D006F6F6F00717171007979790096969600CBCBCB00D6D6D600A0A0A0008181
      8100767676006F6F6F006E6E6E00F1F1F100FC020400FC020400FC0204000402
      0400FC020400FC020400FC020400FC020400FC020400FC02040004020400FC02
      0400FC020400FC020400FC020400FC020400000000000000000000000000C4C2
      C40084828400C4C2C40084828400848284000402040000000000000000000000
      0000040204000000000000000000000000000000000000000000C4C2C4008482
      8400848284000000000000000000000000000000000000000000C4C2C4008482
      8400173E1C000000000000000000000000008F8F8F004B4B4B00696969006E6E
      6E006F6F6F00717171007B7B7B009C9C9C00D4D4D400D3D3D3009E9E9E008383
      83007B7B7B007373730072727200ECECEC00FC020400FC020400C44224000402
      0400FC020400FC020400FC020400FC020400FC020400C442240004020400FC02
      0400FC020400FC020400FC020400FC0204000000000000000000000000000000
      0000C4C2C40084828400C4C2C400848284008482840004020400000000000402
      0400848284000000000000000000000000000000000000000000C4C2C4008482
      8400848284000000000000000000000000000000000000000000C4C2C4008482
      8400173E1C0000000000000000000000000073737300535353006B6B6B007272
      720072727200747474007F7F7F00A9A9A900E7E7E700D5D5D5009B9B9B008181
      8100777777007373730072727200EFEFEF004462640044826400048284000402
      0400FC020400FC020400FC02040044626400448264000482840004020400FC02
      0400FC020400FC02040044626400448264000000000000000000000000000000
      000000000000C4C2C40084828400C4C2C4008482840084828400848284008482
      8400040204000000000000000000000000000000000000000000C4C2C4008482
      8400848284000000000000000000000000000000000000000000C4C2C4008482
      8400173E1C000000000000000000000000006B6B6B005A5A5A006E6E6E007474
      7400777777007A7A7A008B8B8B00C8C8C800FEFEFE00E8E8E8009D9D9D008080
      800077777700717171007B7B7B00F8F8F800C4C2240004828400048284000402
      0400FCFE0400FCFE0400FCFE0400C4C22400048284000482840004020400FCFE
      0400FCFE0400FCFE0400C4C22400048284000000000000000000000000000000
      00000000000000000000C4C2C400848284008482840084828400848284008482
      8400040204000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4C2C400C4C2C400848284008482
      840084828400173E1C00173E1C00000000006D6D6D005F5F5F00747474007A7A
      7A007C7C7C008080800098989800E5E5E50000000000F6F6F600A8A8A8008080
      8000787878006E6E6E008A8A8A0000000000FCFE0400FCFE040084C244000402
      040084E2840004FEFC00FCFE0400FCFE0400FCFE040084C244000402040084E2
      840004FEFC00FCFE0400FCFE0400FCFE04000000000000000000000000000000
      0000000000000000000000000000C4C2C40084828400C4C2C400848284008482
      8400848284000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C4C2C400848284008482
      840084828400173E1C0000000000000000007B7B7B005C5C5C00777777008080
      80007C7C7C008080800094949400DADADA0000000000FBFBFB00ADADAD008181
      81007B7B7B006E6E6E009E9E9E0000000000FCFE0400FCFE0400FCFE04000402
      0400FCFE040084E28400FCFE0400FCFE0400FCFE0400FCFE040004020400FCFE
      040084E28400FCFE0400FCFE0400FCFE04000000000000000000000000000000
      000000000000000000000000000084828400C4C2C40084828400848284008482
      8400848284000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C4C2C400C4C2C4008482
      840084828400173E1C000000000000000000BABABA0053535300707070007B7B
      7B007C7C7C007D7D7D0085858500A2A2A200D2D2D200CFCFCF009A9A9A008282
      8200828282006E6E6E00C2C2C200000000000402040004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040004020400040204000000000000000000000000000000
      0000000000000000000084828400C4C2C400C4C2C400C4C2C40084828400C4C2
      C400848284000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C2C4008482
      8400173E1C00000000000000000000000000FEFEFE00B2B2B2005E5E5E006666
      66007171710073737300757575007A7A7A008282820087878700838383008383
      83007E7E7E0080808000F0F0F000000000000402040000000000000000000402
      0400000000000000000004020400000000000000000004020400000000000000
      0000040204000000000000000000040204000000000000000000000000000000
      000000000000000000000000000000000000C4C2C40084828400000000008482
      8400848284000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C2C4008482
      8400173E1C000000000000000000000000000000000000000000C7C7C7006C6C
      6C0051515100585858005F5F5F0063636300656565006B6B6B00737373007272
      720069696900B6B6B60000000000000000000402040000000000000000000402
      0400000000000000000004020400000000000000000004020400000000000000
      0000040204000000000000000000040204000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848284000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      840000000000000000000000000000000000000000000000000000000000F9F9
      F900BBBBBB007F7F7F00636363005F5F5F006161610065656500717171008F8F
      8F00CBCBCB00FDFDFD0000000000000000000402040004020400040204000402
      0400040204000402040004020400040204000402040004020400040204000402
      0400040204000402040004020400040204000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000F3F3F300F0F0F000F1F1F100F1F1F100F4F4F4000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FEFEFE00EAEAEA00F9F9F90000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F8F8F800C9C9C900AEAEAE00AFAFAF007373730062626200B2B2B200FDFD
      FD00000000000000000000000000000000000000000000000000000000000000
      0000FBFBFB00F4F4F400E1E1E100D1D1D100BEBEBE00A9A9A900A1A1A100A5A5
      A500F8F8F8000000000000000000000000000000000000000000000000000000
      000000000000000000008F8F8F008F8F8F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EEEEEE00B8B8
      B8008888880093939300CDCDCD00C5C5C50097979700979797007B7B7B007D7D
      7D00D4D4D400000000000000000000000000000000000000000000000000E1E1
      E1009A9A9A008F8F8F008787870087878700828282008A8A8A00767676009999
      9900C0C0C0000000000000000000000000000000000000000000000000000000
      00008F8F8F008F8F8F008F8F8F008F8F8F008F8F8F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000989898008181
      81008A8A8A00C9C9C900CFCFCF00CFCFCF00A2A2A20097979700979797009797
      970086868600A7A7A700F1F1F10000000000000000000000000098989800C3C3
      C300C3C3C300C3C3C300C3C3C30082828200C3C3C300C3C3C3002A2A2A008C8C
      8C0099999900DEDEDE00000000000000000000000000000000008F8F8F008F8F
      8F008F8F8F008F8F8F008F8F8F008F8F8F008F8F8F0084828400000000000000
      00000000000000000000000000000000000000000000C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C4008F8F8F00FCFE
      FC00FCFEFC0000000000000000000000000000000000FCFCFC00868686008686
      8600C1C1C100D0D0D000D0D0D000CFCFCF00B7B7B70097979700979797009797
      970097979700969696009A9A9A00000000000000000000000000C3C3C300C3C3
      C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3C300202020003B3B
      3B0099999900A1A1A100F8F8F8000000000000000000C4C2C400C4C2C4008F8F
      8F008F8F8F008F8F8F008F8F8F008F8F8F008F8F8F008F8F8F0084828400FCFE
      FC00FCFEFC00000000000000000000000000FCFEFC00C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C4008F8F8F008F8F
      8F000000000000000000000000000000000000000000EDEDED0082828200B9B9
      B900D0D0D000D0D0D000D0D0D000D0D0D000CBCBCB0099999900979797009797
      9700979797007E7E7E0088888800000000000000000000000000C3C3C300C3C3
      C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3C300828282001A1A1A001313
      13005E5E5E0099999900F1F1F10000000000FCFEFC00C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C4008F8F8F008F8F8F008F8F8F00848284008F8F8F008F8F
      8F0000000000000000000000000000000000FCFEFC00FCFEFC00C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C4008F8F8F008F8F8F007575
      75000000000000000000000000000000000000000000D4D4D400ADADAD00D0D0
      D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000A8A8A800989898009797
      97007E7E7E006161610096969600000000000000000000000000C3C3C300C3C3
      C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3C300131313001313
      13003D3D3D009E9E9E00F1F1F10000000000FCFEFC00FCFEFC00C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400848284008F8F8F008F8F8F007575
      75000000000000000000000000000000000000000000FCFEFC00C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C4008F8F8F00757575007575
      75008F8F8F0000000000000000000000000000000000D4D4D400CCCCCC00CECE
      CE00D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000BDBDBD00989898007979
      79006262620061616100A9A9A900000000000000000000000000C3C3C3007F7F
      7F00C3C3C300C3C3C300C3C3C300C3C3C300C3C3C30087878700101010001010
      10004242420099999900F1F1F1000000000000000000FCFEFC00C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C4008F8F8F00757575007575
      75008F8F8F0000000000000000000000000000000000FCFEFC00FCFEFC00C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C4008F8F8F0084828400757575008482
      84005B5B5B0000000000000000000000000000000000C1C1C100B5B5B500BCBC
      BC00BEBEBE00C0C0C000C3C3C300C5C5C500C8C8C800C8C8C8007B7B7B006262
      62006161610061616100B9B9B900000000000000000000000000C3C3C300C3C3
      C300C3C3C300C3C3C300C3C3C300C3C3C300C3C3C3008A8A8A000D0D0D000C0C
      0C003D3D3D009E9E9E00F1F1F1000000000000000000FCFEFC00FCFEFC00C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C4008F8F8F0084828400757575008482
      84005B5B5B000000000000000000000000000000000000000000FCFEFC00C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C4008F8F8F0075757500848284005B5B
      5B005B5B5B005B5B5B00000000000000000000000000C4C4C40085858500BCBC
      BC00BCBCBC00BCBCBC00BCBCBC00BCBCBC00BDBDBD00A5A5A500747474006464
      64006161610061616100C8C8C8000000000000000000000000009C9C9C00C3C3
      C300C3C3C300C3C3C300C3C3C30088888800C3C3C300C3C3C3001A1A1A000C0C
      0C003A3A3A009E9E9E00F1F1F100000000000000000000000000FCFEFC00C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C4008F8F8F0075757500848284005B5B
      5B005B5B5B005B5B5B0000000000000000000000000000000000FCFEFC00FCFE
      FC00C4C2C400C4C2C400C4C2C4008F8F8F0075757500757575005B5B5B005B5B
      5B000000000000000000000000000000000000000000DADADA0063636300A5A5
      A500BCBCBC00BCBCBC00BCBCBC00BCBCBC00B6B6B6007E7E7E00787878007474
      74006363630061616100D6D6D6000000000000000000000000009F9F9F007777
      770073737300737373006A6A6A006A6A6A006666660060606000535353000909
      09003A3A3A009E9E9E00F1F1F100000000000000000000000000FCFEFC00FCFE
      FC00C4C2C400C4C2C400C4C2C4008F8F8F0075757500757575005B5B5B005B5B
      5B0000000000000000000000000000000000000000000000000000000000FCFE
      FC00C4C2C400C4C2C400C4C2C4008F8F8F00757575008F8F8F00000000000000
      0000000000000000000000000000000000000000000000000000C0C0C0007070
      7000B9B9B900BCBCBC00BCBCBC00BCBCBC008D8D8D0078787800787878007878
      78007575750064646400E6E6E600000000000000000000000000EAEAEA005F5F
      5F006060600066666600606060005B5B5B005555550055555500505050003434
      34003C3C3C0099999900F1F1F10000000000000000000000000000000000FCFE
      FC00C4C2C400C4C2C400C4C2C4008F8F8F00757575008F8F8F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FCFEFC00C4C2C4008F8F8F00848284008F8F8F0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C0008E8E8E00BCBCBC00BCBCBC00A4A4A4007878780078787800787878007878
      78007D7D7D009D9D9D00FBFBFB0000000000000000000000000000000000BBBB
      BB004C4C4C005050500050505000505050004C4C4C0046464600414141003F3F
      3F00696969000000000000000000000000000000000000000000000000000000
      0000FCFEFC00C4C2C4008F8F8F00848284008F8F8F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C4C2C4008F8F8F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C1C1C100ABABAB00B4B4B4007D7D7D00787878008A8A8A00B3B3B300D9D9
      D900FCFCFC000000000000000000000000000000000000000000000000000000
      00007B7B7B003F3F3F00464646005D5D5D007C7C7C009D9D9D00C3C3C300E1E1
      E100FBFBFB000000000000000000000000000000000000000000000000000000
      000000000000C4C2C4008F8F8F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D2D2D200ADADAD00C1C1C100E9E9E900FEFEFE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F2F2F200E7E7E700FBFBFB00000000000000000000000000000000000000
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
      0000E7E7E700D1D1D100CCCCCC00C8C8C800BEBEBE00BEBEBE00C5C5C500CCCC
      CC00D1D1D100E1E1E100FBFBFB00000000000000000000000000000000000000
      000000000000F2F2F200CDCDCD00B8B8B800AEAEAE00AAAAAA00AAAAAA00B2B2
      B200C1C1C100DEDEDE00FBFBFB00000000000000000000000000000000000000
      000000000000FBFBFB00C2C2C2007F7F7F0084848400999999009E9E9E00F8F8
      F800000000000000000000000000000000000000000000000000000000000000
      0000F8F8F800D7D7D700B9B9B900A6A6A600979797008E8E8E008A8A8A00B4B4
      B40000000000000000000000000000000000000000000000000000000000D6D6
      D6009E9E9E00A3A3A300949494008F8F8F008F8F8F0099999900999999009999
      99009999990099999900AEAEAE00000000000000000000000000000000000000
      0000DEDEDE009E9E9E0099999900999999009999990099999900999999009999
      99009999990099999900B1B1B100000000000000000000000000000000000000
      0000000000007D7D7D001D1D1D001A1A1A00262626006E6E6E0094949400BCBC
      BC00FBFBFB000000000000000000000000000000000000000000E1E2E100AAA9
      AA008A8A8A006E6E6E00606060006C6C6C00777777007C7C7C007C7C7C007C7C
      7C00DADADA0000000000000000000000000000000000DADADA00AAAAAA009E9E
      9E00AFAFAF00E7E7E7009A9A9A006F6F6F00505050003B3B3B00262626002020
      20004A4A4A0099999900D8D8D80000000000000000000000000000000000EBEB
      EB00C6C6C600A3A3A30082828200525252003B3B3B0037373700474747007E7E
      7E00999999009999990099999900F8F8F8000000000000000000000000000000
      0000949494004949490042424200262626001313130024242400606060009999
      9900A6A6A6000000000000000000000000000000000000000000C3C2C300BBBA
      BB00BBBBBB009C9C9C007D7D7D007D7D7D007D7D7D007D7C7D007D7C7D007C7C
      7C00908F9000FBFBFB00000000000000000000000000C5C5C5007F7F7F008F8F
      8F00A3A3A300E1E1E1009D9D9D006F6F6F00505050003B3B3B00131313000303
      030032323200AAAAAA00000000000000000000000000FBFBFB00C1C1C100CCCC
      CC00C3C3C300A3A3A3007F7F7F00494949002626260013131300000000000606
      0600727272009999990099999900F8F8F800000000000000000000000000E3E3
      E3006A6A6A006F6F6F0065656500424242003030300010101000202020007E7E
      7E00A3A3A30000000000000000000000000000000000EDEDED00BBBABB00BBBA
      BB00BBBBBB00949494007C7D7C007C7D7C007C7D7C007C7C7C007C7C7C007C7C
      7C007C7C7C00C1C1C100000000000000000000000000000000008C8C8C008686
      86009E9E9E00DADADA00A4A4A4006F6F6F004949490030303000131313000303
      03007E7E7E00DDDDDD00000000000000000000000000F1F1F100B8B8B800CCCC
      CC00C3C3C300A3A3A3007F7F7F00494949002626260013131300000000000606
      06005C5C5C009999990099999900F8F8F800000000000000000000000000D6D6
      D600A3A3A300A3A3A3009E9E9E00777777006060600026262600101010004C4C
      4C00A3A3A30000000000000000000000000000000000C7C7C700BBBBBB00BBBB
      BB00BBBBBB008D8D8D007C7D7C007C7D7C007C7D7C007C7D7C007C7C7C007C7C
      7C007C7C7C0081818100F0F0F000000000000000000000000000CFCFCF007F7F
      7F0094949400CCCCCC00B8B8B8006F6F6F004949490026262600060606003A3A
      3A00B0B0B00000000000000000000000000000000000F1F1F100B4B4B400CCCC
      CC00C3C3C300A3A3A3007F7F7F00494949002626260013131300000000000606
      06005C5C5C009999990099999900F8F8F800000000000000000000000000D6D6
      D600A3A3A300A3A3A3009E9E9E00777777006060600026262600101010004C4C
      4C00A3A3A300000000000000000000000000E7EAEA00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00858585007C7D7C007C7D7C007C7D7C007C7D7C007C7C7C007C7C
      7C007C7C7C007C7C7C00A8A8A800000000000000000000000000FBFBFB009393
      93008F8F8F00B7B7B700BEBEBE006F6F6F00494949001C1C1C00030303008383
      8300D8D8D80000000000000000000000000000000000F1F1F100B4B4B400CCCC
      CC00C3C3C300A3A3A3007F7F7F00494949002626260013131300000000000606
      06005C5C5C009999990099999900F8F8F800000000000000000000000000D6D6
      D600A3A3A300A3A3A3009E9E9E00777777006060600026262600101010004C4C
      4C00A3A3A300000000000000000000000000D7D9DA00BBBBBB00BBBBBB00BBBB
      BB00BABABA007E7E7E007D7D7D007C7D7C007C7D7C007C7D7C007C7D7C007C7C
      7C007C7C7C007C7C7C005D5D5D00DADADA00000000000000000000000000CFCF
      CF0086868600A6A6A600C9C9C9006F6F6F003E3E3E001010100035353500AAAA
      AA000000000000000000000000000000000000000000F1F1F100B4B4B400CCCC
      CC00C3C3C300A3A3A3007F7F7F00494949002626260013131300000000000606
      06005C5C5C009999990099999900F8F8F800000000000000000000000000D6D6
      D600A3A3A300A3A3A3009E9E9E00777777006060600026262600101010004C4C
      4C00A3A3A300000000000000000000000000E4E6E700BBBBBB00BCBBBC00BCBB
      BC00B5B5B5007D7D7D007D7D7D007D7D7D007D7D7D007D7D7D007D7D7D007D7D
      7D007D7C7D006B6B6B0010101000949494000000000000000000000000000000
      00009393930094949400C9C9C9006464640034343400060606007E7E7E00DDDD
      DD000000000000000000000000000000000000000000F1F1F100B8B8B800CCCC
      CC00C3C3C300A3A3A3007F7F7F00494949002626260013131300000000000606
      06005C5C5C009999990099999900F8F8F800000000000000000000000000D6D6
      D600A3A3A300A3A3A3009E9E9E00777777006060600026262600101010004C4C
      4C00A3A3A300000000000000000000000000F1F4F400BCBBBC00BCBBBC00BCBC
      BC00BEBFBE00B3B3B300949494007D7D7D007C7D7C007C7D7C007C7D7C007C7D
      7C00757575001B1B1B0011101100DDDDDD000000000000000000000000000000
      0000CFCFCF008A8A8A00C6C6C600646464002727270036363600B0B0B0000000
      00000000000000000000000000000000000000000000F1F1F100B4B4B400CCCC
      CC00C3C3C300A3A3A3007F7F7F00494949002626260013131300000000000606
      06005C5C5C009999990099999900F8F8F800000000000000000000000000D6D6
      D600A3A3A300A3A3A3009E9E9E00777777006060600026262600101010004C4C
      4C00A3A3A30000000000000000000000000000000000C1C1C100BDBDBD00BFBF
      BF00BFBFBF00BFBFBF00BFBFBF00B5B5B500969696007E7E7E007C7D7C007B7B
      7B00282828000909090082828200000000000000000000000000000000000000
      0000000000009C9C9C00B2B2B200676767001919190083838300E3E3E3000000
      00000000000000000000000000000000000000000000F1F1F100B4B4B4009898
      980081818100636363005050500055555500555555003F3F3F00282828001313
      13005C5C5C0099999900B1B1B10000000000000000000000000000000000D9D9
      D900B7B7B700BFBFBF00C0C0C0008A8A8A00737373003B3B3B00202020004040
      4000AEAEAE0000000000000000000000000000000000DBDBDB00BFC0BF00BFBF
      BF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00B7B7B700989898003C3C
      3C000A090A0029292900F6F6F600000000000000000000000000000000000000
      000000000000D8D8D800A1A1A1006969690049494900B6B6B600000000000000
      00000000000000000000000000000000000000000000F1F1F1005A5A5A005050
      5000505050005050500050505000505050005050500050505000505050005050
      50007C7C7C00DEDEDE000000000000000000000000000000000000000000D9D9
      D900BBBBBB00DEDEDE00F4F4F400A3A3A3008282820049494900262626005D5D
      5D00DDDDDD000000000000000000000000000000000000000000D2D3D200BFBF
      BF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00B3B3B3000C0C
      0C0009090900B3B3B30000000000000000000000000000000000000000000000
      00000000000000000000A5A5A50066666600DADADA00F8F8F800000000000000
      00000000000000000000000000000000000000000000FBFBFB00676767005050
      5000505050005050500050505000505050005050500050505000505050006A6A
      6A00C8C8C8000000000000000000000000000000000000000000000000000000
      0000DADADA00CCCCCC00CCCCCC00B4B4B400999999005B5B5B003B3B3B000000
      0000000000000000000000000000000000000000000000000000FCFCFC00CFD0
      CF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00848584000909
      090080808000FEFEFE0000000000000000000000000000000000000000000000
      00000000000000000000D8D8D800BABABA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FBFBFB00ADAD
      AD00939393006C6C6C005555550062626200858585009D9D9D00C6C6C600F8F8
      F800000000000000000000000000000000000000000000000000000000000000
      000000000000C4C4C400C7C7C700B1B1B100999999005B5B5B00858585000000
      000000000000000000000000000000000000000000000000000000000000FBFB
      FB00CDCDCD00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BEBFBE005A5A5A00A4A4
      A400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFD00F7F7F700F1F1F100ECECEC00E6E6E600E0E0E000D9D9D9000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBFBFB00F4F4F400E1E1E100D1D1D100BEBEBE00A9A9A900A1A1A100A5A5
      A500F8F8F8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000EEEEEE00CFCFCF00B8B8B800B8B8B800C6C6C600E7E7
      E700000000000000000000000000000000000000000000000000000000000000
      0000FCFCFC00E2E3E200C7C7C700BCBDBC00BDBEBD00C9CAC900E6E6E500FEFE
      FE0000000000000000000000000000000000000000000000000000000000E1E1
      E1009A9A9A008F8F8F008787870087878700828282008A8A8A00767676009999
      9900C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBFBFB00C2C2C200898989007F7F7F00848484008F8F8F00999999009E9E
      9E00BCBCBC00F8F8F80000000000000000000000000000000000F9FAF900BBBC
      BB00828482007375730073757300737573007375730073757300737573008789
      8700C3C4C300FCFCFC0000000000000000000000000000000000989898007777
      77007B7B7B007B7B7B007B7B7B008282820086868600868686002A2A2A008C8C
      8C0099999900DEDEDE00000000000000000000000000FCFEFC00FCFEFC00FCFE
      FC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFEFC00FCFE
      FC00FCFEFC000000000000000000000000000000000000000000000000000000
      00007D7D7D001D1D1D00131313001A1A1A00262626003A3A3A006E6E6E009494
      940099999900BCBCBC00FBFBFB000000000000000000EBECEB00898B89007375
      7300737573007375730073757300737573007375730073757300737573007375
      73007375730092949200F2F3F2000000000000000000000000008A8A8A007878
      7800787878007F7F7F007F7F7F00868686008282820086868600202020003B3B
      3B0099999900A1A1A100F8F8F80000000000FCFEFC00C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C4008482840000000000000000000000000000000000FBFBFB006D6D
      6D002626260020202000131313000D0D0D001010100020202000505050007B7B
      7B009494940099999900D1D1D10000000000F8F8F80086888600737573007375
      7300737573007375730073757300737573007375730073757300737573007375
      7300737573007375730092939200FCFCFC0000000000000000008A8A8A007878
      78007F7F7F007F7F7F00828282008282820086868600828282001A1A1A001313
      13005E5E5E0099999900F1F1F10000000000FCFEFC00C4C2C400C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C40084828400C4C2C400000000000000000000000000949494004949
      490049494900424242003B3B3B00262626001313130010101000242424006060
      60006363630099999900A6A6A600FBFBFB00BEBFBE0073757300737573007375
      7300737573007375730073757300737573007375730073757300737573007375
      7300737573007375730073757300CDCECD0000000000000000008E8E8E007B7B
      7B007F7F7F008686860086868600868686008686860086868600131313001313
      13003D3D3D009E9E9E00F1F1F1000000000000000000FCFEFC00C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400C4C2C40084828400848284000000000000000000E3E3E3006A6A6A006F6F
      6F006F6F6F006565650052525200424242003030300016161600101010002020
      20003B3B3B007E7E7E00A3A3A300F1F1F100F1F2F100CECECE00ABACAB00888A
      8800737573007375730073757300737573007375730073757300737573007375
      7300737573007375730073757300A8A9A80000000000000000008E8E8E007F7F
      7F0082828200868686008A8A8A008A8A8A008686860087878700101010001010
      10004242420099999900F1F1F1000000000000000000FCFEFC00C4C2C400C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C400848284008482840084828400C4C2C40000000000C7C7C700878787008F8F
      8F008A8A8A007F7F7F00737373005B5B5B004646460030303000161616001010
      1000202020005C5C5C0099999900EBEBEB000000000000000000000000000000
      0000EEEEEE00CBCCCB00A8A9A800858685007374730073747300737473007374
      7300737473007374730073747300A6A8A6000000000000000000939393008686
      86008A8A8A008A8A8A008A8A8A008F8F8F008F8F8F008A8A8A000D0D0D000C0C
      0C003D3D3D009E9E9E00F1F1F100000000000000000000000000FCFEFC00C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2
      C4008482840084828400848284000000000000000000D6D6D600A3A3A300AEAE
      AE00A3A3A3009E9E9E008A8A8A00777777006060600046464600262626001010
      1000161616004C4C4C00A3A3A300F4F4F4000000000000000000000000000000
      0000000000000000000000000000F7F7F6007274720072747200727472007273
      7200727372007274720072747200CACBCA0000000000000000009C9C9C008F8F
      8F0094949400949494008F8F8F0088888800767676006E6E6E001A1A1A000C0C
      0C003A3A3A009E9E9E00F1F1F100000000000000000000000000FCFEFC00C4C2
      C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C4008482
      84008482840084828400C4C2C4000000000000000000D9D9D900B7B7B700BBBB
      BB00BFBFBF00C0C0C000ABABAB008A8A8A00737373005B5B5B003B3B3B002020
      20001010100040404000AEAEAE00000000000000000000000000000000000000
      0000000000000000000000000000F7F7F6007174710071747100717471007173
      710071737100717471008D8F8D00FBFCFB0000000000000000009F9F9F007777
      770073737300737373006A6A6A006A6A6A006666660060606000535353000909
      09003A3A3A009E9E9E00F1F1F10000000000000000000000000000000000FCFE
      FC00C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C400C4C2C4008482
      84008482840084828400000000000000000000000000D9D9D900BBBBBB00C7C7
      C700DEDEDE00F4F4F400D0D0D000A3A3A300828282006A6A6A00494949002626
      26000D0D0D005D5D5D00DDDDDD00000000000000000000000000000000000000
      0000000000000000000000000000F7F7F6007173710071737100717371007173
      7100717371008E8F8E00F1F1F100000000000000000000000000EAEAEA005F5F
      5F006060600066666600606060005B5B5B005555550055555500505050003434
      34003C3C3C0099999900F1F1F100000000000000000000000000000000000000
      0000FCFEFC008482840084828400848284008482840084828400848284008482
      84008482840000000000000000000000000000000000F4F4F400BCBCBC00CCCC
      CC00DEDEDE00EEEEEE00D8D8D800B1B1B1008F8F8F00777777005B5B5B003B3B
      3B0016161600B5B5B50000000000000000000000000000000000000000000000
      0000000000000000000000000000F7F7F6007173710071737100717371008183
      8100BEBFBE00FAFAFA000000000000000000000000000000000000000000BBBB
      BB004C4C4C005050500050505000505050004C4C4C0046464600414141003F3F
      3F00696969000000000000000000000000000000000000000000000000000000
      0000000000008482840004020400040204000402040004020400040204000402
      0400C4C2C4000000000000000000000000000000000000000000DADADA00C7C7
      C700CCCCCC00CCCCCC00C3C3C300B4B4B400999999007F7F7F005B5B5B003B3B
      3B00757575000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FAFAFA00B7B9B700C5C6C500E0E0E000FDFD
      FD00000000000000000000000000000000000000000000000000000000000000
      00007B7B7B003F3F3F00464646005D5D5D007C7C7C009D9D9D00C3C3C300E1E1
      E100FBFBFB000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D0D0
      D000C4C4C400C7C7C700BBBBBB00B1B1B100999999007F7F7F005B5B5B008585
      8500FBFBFB000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F2F2F200E7E7E700FBFBFB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EAEAEA00C3C3C300ABABAB009A9A9A008A8A8A008C8C8C00C8C8C8000000
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
      0000000000000000000000000000000000000000000004020400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004020400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084828400848284008482
      8400040204000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000402040084828400C4C2C400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000402040004020400FC020400040204000402
      0400000000000000000000000000000000000000000000000000000000000000
      0000848284008482840084828400848284008482840084C2440084C244008482
      8400040204000402040000000000000000000000000000000000000000000402
      040000000000000000000000000084828400C4C2C40084828400848284008482
      840084828400C4C2C40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040204000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004020400FC0204000000
      0000000000000000000000000000000000000000000084828400848284008482
      840084C2440084C2440084C2440084C2440084C2440084C2440084C244008482
      8400040204000402040004020400000000000000000000000000000000008482
      840084828400000000000000000084828400FCFE0400FCFE0400FCFE0400FCFE
      0400C4C2C4008482840004020400000000000000000000000000040204000402
      0400040204000000000000000000000000000000000004020400040204000402
      0400000000000000000000000000000000000000000000000000040204000000
      0000000000000000000000000000000000000000000004020400FC020400FCFE
      040000000000000000000000000000000000000000008482840084C2440084C2
      440084C2440084C2440084C2440084C2440084C2440084C2440084C244008482
      8400040204000000000000000000000000000000000000000000C4C2C4008482
      840084828400C4C2C40000000000C4C2C40084828400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE040084828400000000000000000000000000040204000402
      0400040204000000000000000000000000000402040004020400040204000402
      0400040204000000000000000000000000000000000000000000040204000000
      0000000000000000000000000000000000000000000000000000FCFE0400FC02
      040000000000000000000000000000000000000000008482840084C2440084C2
      440084C2440084C2440084C2440084C2440084C2440084C2440084C244008482
      840000000000000000000000000000000000000000000000000084828400FCFE
      0400C4C2C40084828400000000000000000084828400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE040084828400000000000000000000000000040204000402
      0400040204000000000000000000000000000000000004020400040204000402
      0400000000000000000000000000000000000402040004020400FC0204000402
      040004020400000000000000000000000000000000000000000000000000FC02
      0400FCFE0400000000000000000000000000000000008482840084C2440084C2
      440084C2440084C2440084C2440084C2440084C2440084C2440084C244008482
      840000000000000000000000000000000000000000000000000084828400FCFE
      0400FCFE040084828400C4C2C4000000000004020400FCFE0400FCFE0400FCFE
      0400FCFE0400C4C2C40084828400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040204000000
      000000000000000000000000000000000000000000000000000004020400FCFE
      040000000000000000000000000000000000000000000000000000000000FCFE
      0400FC020400040204000000000000000000000000008482840084C2440084C2
      440084C2440084C2440084C2440084C2440084C2440084C2440084C244008482
      840000000000000000000000000000000000000000000000000084828400FCFE
      0400FCFE0400FCFE04008482840084828400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE040084828400C4C2C400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000004020400FC02
      0400000000000000000000000000000000000000000000000000000000000000
      0000FC020400040204000000000000000000000000008482840084C2440084C2
      440084C2440084C2440084C2440084C2440084C2440084C2440084C244008482
      84000000000000000000000000000000000000000000C4C2C40084828400FCFE
      0400FCFE0400FCFE040004020400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE04008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FC02
      0400000000000000000000000000000000000000000000000000000000000402
      040004020400FC0204000402040004020400000000008482840084C2440084C2
      440084C2440084C2440084C2440084C2440084C2440084C2440084C244008482
      8400000000000000000000000000000000000000000084828400C4C2C400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE04008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FC02
      040004020400000000000000000000000000FCFE0400FC020400FC020400FC02
      0400FCFE0400040204000000000000000000000000008482840084C2440084C2
      440084C2440084C2440084C2440084C2440084C2440084C2440084C244008482
      8400000000000000000000000000000000000000000084828400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE04008482840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040204000000
      000000000000000000000000000000000000000000000000000000000000FCFE
      040004020400FCFE0400FC020400FC020400FC020400FCFE0400000000000000
      000000000000040204000000000000000000000000008482840084C2440084C2
      440084C2440084C2440084C2440084C2440084C2440084828400848284008482
      8400000000000000000000000000000000000000000004020400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400C4C2
      C400848284000402040000000000000000000000000000000000000000000402
      0400000000000000000000000000000000000000000004020400040204000402
      0400000000000000000000000000000000000000000000000000040204000402
      0400FC0204000402040004020400000000000000000000000000000000000000
      000000000000000000000000000000000000000000008482840084C2440084C2
      4400848284008482840084828400848284008482840000000000000000000000
      000000000000000000000000000000000000000000000000000084828400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400C4C2C40084828400848284008482
      8400C4C2C4000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040204000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040204000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084828400848284008482
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008482
      8400FCFE0400C4C2C400848284008482840084828400C4C2C400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040204000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000402040084828400C4C2C400000000000000000000000000000000000000
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
      000000000000000000000000000000000000000000000000000000000000F2F2
      E500CDCD9900CDCD9900DCDCB800F8F8F2000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FCFCF900F5F5EA00E6E6CC00D3D3
      A700D3D3A700E1E1C300EDEDDA0000000000000000000000000000000000D9D9
      B200BDBD7900C7C78D00CDCD9900D7D7AF00E9E9D20000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000000000000000
      00000000000000000000FCFCF900F5F5EA00EAEAD500DADAB400B6B66F00CBCB
      9600CBCB9600CBCB9600D1D1A300000000000000000000000000000000008989
      50004F4F0E00ABAB6400BABA7300CDCD9900CDCD9900D0D09F00E7E7CF00F7F7
      EF00000000000000000000000000000000000000000000000000040204000402
      0400040204000402040004020400040204000402040004020400000000000000
      00000000000000000000000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC02040000000000000000000000000000000000F2F2
      E500E8E8D000E5E5CB00E3E3C700D8D8B100CECE9C00D8D8B000717112006A6A
      100083832A00A2A25C00D1D1A30000000000FEFEFC0000000000F5F5EB00B6B6
      8100D0D0A300A9A95E009C9C54009E9E4900BFBF7D00CDCD9900CDCD9900CDCD
      9900D4D4A900E3E3C500F8F8F2000000000000000000000000000402040004C2
      2400FCFE4400FCFE4400FCFE4400FCFE4400FCFE440004020400040204000000
      00000000000000000000000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FC020400FC020400000000000000000000000000FAFAF400E1E1
      C300CBCB9600C8C89100BCBC7700B9B97300CBCB9600CECE9C00848420008484
      20008484200082821E00D1D1A300000000009C9C69007E7E3400B3B36D00E3E3
      C600BDBD8600B7B77A00A5A554008A8A3400494901006D6D2700B1B16900CACA
      9300CDCD9900CDCD9900D0D09F00F4F4E800000000000000000004020400FCFE
      440004C22400FCFE4400FCFE4400FCFE4400FCFE440004020400FCFE44000402
      04000000000000000000000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC020400FC020400FC020400FC020400FC02
      0400FC020400FCFE0400FC0204000000000000000000FCFCF900BEBE7F009A9A
      4B00989846008383280058580A0065651200BEBE7C00C8C89100969632009191
      2D008D8D290082821E00D1D1A30000000000919142007E7E3800AFAF6400A9A9
      5400BDBD7E00BDBD7D00AFAF610092923500A2A24A00767618003D3D01004242
      070072722D00ACAC6300CDCD9900D1D1A200000000000000000004020400FCFE
      4400FCFE440004C22400FCFE4400FCFE4400FCFE440004020400FCFE4400FCFE
      44000402040000000000000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC020400FCFE0400FC020400FCFE0400FC02
      0400FCFE0400FC020400048284000000000000000000DBDBB500D3D3A6008989
      34003737000037370000626214005A5A0700AFAF5E00BDBD7800969632009696
      3200969632008D8D2900D1D1A30000000000FCFCF800CDCDA600B8B87D009C9C
      54008C8C290086863100AAAA5600D2D2A400BFBF7D00B1B162009F9F43007A7A
      1C00494901003C3C0100A1A14D00CDCD9900000000000000000004020400FCFE
      4400FCFE4400FCFE440004C2240004C2240004C224000402040004C2240004C2
      240004C2240004020400000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FCFE0400FC02040004828400048284000482
      84000482840004828400048284000000000000000000C7C79300C1C18200A7A7
      550082821E0082821E008787230076761D00ACAC5900BDBD78009C9C3B009999
      3600969632008D8D2900D1D1A30000000000CFCFA000CACA9300BBBB83009F9F
      480086862E00636317004C4C01005C5C0400A7A75800B5B56A00C1C18200B3B3
      6500939333005555080042420700BDBD7900000000000000000004020400FCFE
      4400FCFE4400FCFE440004C22400FCFE4400FCFE440004020400FCFE4400FCFE
      4400FCFE440004020400000000000000000000000000FC020400FC020400FC02
      0400FC020400FC020400FC020400FC0204000482840004828400048284000482
      84000482840004828400048284000000000000000000C8C89200939332009494
      3200B7B76D00B7B76D00B7B76D0076761700AFAF5E00BABA7300A3A34400A3A3
      4400A3A344008D8D2900E0E0C1000000000000000000E2E2C500B1B16400B6B6
      6C00B8B87600B8B86F00A0A045008D8D2A00737313005252040089894900C4C4
      8700B0B05F00868623003636000095953E00000000000000000004020400FCFE
      4400FCFE4400FCFE440004C22400FCFE4400FCFE440004020400FCFE4400FCFE
      4400FCFE440004020400000000000000000000000000FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE040004828400048284000482
      84000482840004828400048284000000000000000000DBDBB500666607009595
      3C00DCDCB800DCDCB800D4D4A80069691700B5B56800C7C78D00C2C28300B2B2
      6300A3A344008D8D2C00FAFAF400000000000000000000000000EBEBD600E1E1
      C300DEDEBC00D3D3A500C5C58900B5B56900A5A54A00767617003C3C01007777
      3100BCBC7800A2A247005555080071712C000000000000000000040204000402
      0400040204000402040004020400040204000402040004020400FCFE4400FCFE
      4400FCFE440004020400000000000000000000000000FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400048284000482
      84000482840004828400048284000000000000000000FCFCF9009F9F4A00C1C1
      8200DBDBB600D8D8B1008C8C3C0091913800C2C28300BEBE7C00909032008888
      3000D6D6AD00D8D8B0000000000000000000000000000000000000000000E9E9
      D200E4E4C900E3E3C600D7D7AF00CDCD9900BDBD7A00A6A64E00757515004646
      0000B2B26400A7A74D0067670E008B8B5F000000000000000000000000000402
      0400FCFE4400FCFE440004C22400FCFE4400FCFE4400FCFE440004020400FCFE
      4400FCFE440004020400000000000000000000000000FCFE0400FCFE040004FE
      FC0004FEFC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400048284000482840004828400000000000000000000000000A7A75B009191
      2D00969632006666190091913E00C1C18200C1C18300E0E0C100B2B269009B9B
      4C00F7F7EF000000000000000000000000000000000000000000000000000000
      0000E1E1C300E4E4C900E4E4C900DEDEBC00CDCD9900B8B86F00979738006464
      070093933C00A3A3460063630A00E0E0C3000000000000000000000000000000
      000004020400FCFE440004C22400FCFE4400FCFE4400FCFE4400FCFE44000402
      0400FCFE440004020400000000000000000000000000FCFE040004FEFC0004FE
      FC0004FEFC0004FEFC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE040004828400000000000000000000000000A5A556008484
      20009F9F4000BEBE7E00F7F7EF00DDDDBB00BABA7300ADAD5800E7E7CF00F7F7
      EF00000000000000000000000000000000000000000000000000000000000000
      0000EFEFDF00E1E1C300E6E6CC00F8F8EF00DCDCB900C2C28300A5A548007171
      10008F8F36009A9A3600CCCCA500000000000000000000000000000000000000
      0000000000000402040004C22400FCFE4400FCFE4400FCFE4400FCFE4400FCFE
      44000402040004020400000000000000000000000000FCFE040004FEFC0004FE
      FC0004FEFC0004FEFC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400000000000000000000000000A9A95A00A2A2
      4400C4C48700B2B2640093934A00D3D3A600E5E5CB00FAFAF400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFC00DFDFBF00E3E3C600E3E3C600DCDCB900C7C78D00A6A64B006565
      0700C3C38700F4F4E80000000000000000000000000000000000000000000000
      0000000000000000000004020400040204000402040004020400040204000402
      04000402040004020400000000000000000000000000FCFE0400FCFE040004FE
      FC0004FEFC00FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400000000000000000000000000FAFAF400E3E3
      C700E5E5CA00CCCC9700B2B27100F7F7EF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F7F7EF00DCDCB900DCDCB900D6D6AB00C0C08000979738008181
      3500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE0400FCFE
      0400FCFE0400FCFE0400FCFE0400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000F7F7EF00DEDEBC00CECE9C00C0C07F00C9C99400FCFC
      F800000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000900100000100010000000000800C00000000000000000000
      000000000000000000000000FFFFFF00FC7F000000000000F00F000000000000
      E003000000000000E001000000000000C000000000000000C000000000000000
      8000000000000000800000000000000000000000000000008000000000000000
      8000000000000000C000000000000000C000000000000000E003000000000000
      F003000000000000F807000000000000DFFFFFFFDFFFFE7F9FFFFFFF9FFFC003
      00FFFFFF00FF8003007FF00F007F00019E7FC0039E7F0000DE7F9FF9DE7F8000
      F00F2CECF00F8000C0032D54C003C0009FF924D48001C00022242D540000C000
      2AAC24EC0000C0002AEC9FF90000C000222CC0030000C0019FF9F00F8001E003
      C003FFFFC003F003F00FFFFFF00FF80FFFFFFFFFBFFDF80FFD80FFFF1FF8C00F
      E080FFFF8BD1800FEDC1FFFFC3C3800FEFE3F00FE3C7800FEC3FC003C003800F
      E0009FF9F81F800FE0802224F81F8003EDC12AACF81F8000E3032AECF81F8000
      F007222CC0038000000F9FF9E3C7BF0001FFC003C3C3FF0083FFF00F8BD1FF20
      C7FFFFFF1FF8FF81EFFFFFFFBFFDFFC3FFFFFFFFFFFFFF80FFFFFD80FFFFFD80
      FD80C080F180E080C000DDC1C080EDC1DC01DC23DC81EFE3DC03D017DE23EFFF
      D8078387DE77ED808001DFC7887FE080D001DFC7DE4FEDC1F01FF307FE1FEFE3
      00F1000F007FFFF701E1000F003F01FF838383FF801F01FFC70FC7FFC01F83FF
      EF3FEFFFEC3FC7FFFFFFFFFFFFFFEFFFFC3FFFFFFFFFFFFFFC3FFFFFFFFFFD80
      FC3FF87FFD80C080FC3FF00FC080DDC1E81DE001DDC1DC238001E000DFE3D017
      8001C000DFF78387000080008FFFDFC780000001DFFFDFC700000003FFFFF307
      0000000701FF000F0001C00F01FF000F0000FE1F83FF83FF8001FFFFC7FFC7FF
      0003FFFFEFFFEFFFD003FFFFFFFFFFFF80018001FFFFFFFF00000000F031FE3F
      00000000E001F009000000008001E000000000008001E000000000008001C001
      00000000C001C00300000000C001C00300000000C003E00700000000E003C007
      00000000E003C00700000000F003C00F00000000F0078C3F00000000F01F9FFF
      00000000F83FFFFF80018001FFFFFFFF8001FE0FFFC0FE1F0000F8073FF0FC0F
      000080071FF4FC0F000000030FC0F807000000010600F807000000010006F023
      000000010007E073000000010007E079000000010007C07C00000003000F807E
      00000003000F807F00000007000F907F0000001F0007807F000000030003C0FF
      000000030001E1FF800180070001FFFFFF87800180018001FE07000000000000
      FC67000000000000F0E7000000000000C1C30000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000800180018001FFFFFFFBFFFBFE008001FFF3FFF3FC00
      8001FFE3FFE3FC008001FFC3FFC3F8008001E783E783F0008001E703E703F000
      8001C603C603E0008001C603C603E0008001C603C603C0008001EE03EE038000
      8001CE03CE0380038001CE07CE07800F8001FE0FFE0F803F8001FE1FFE1F80FF
      8001FE3FFE3FC1FFFFFFFE7FFE7FFFFFFFFFFFFFFFFFFFFFFFFFFFFF80018031
      FFFF000080018049FFFF0FBE8001804900000FBE8001803104200FBE80018001
      004000008001800104200FBE8001800100400FBE8001800104200FBE8001A201
      0000000080018801FFFF000080019401FFFF000080018801FFFF00008001A201
      FFFFFFFF80018001FFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFF8000FFFFFFFF
      FFFF8000FFFFFFFF80078000FFFFFFFF9FF78000E383E38393978000DDBFDDBF
      91178000BEBFBEBF98378000B8BFB8BF9C778000BFBFBFBF98378000BEBFBEBB
      91178000DDBFDDB193978000E3BFE3BB9FF78000FFFFFFFF80078000FFFFFFFF
      8007FFFFFFFFFFFFFFFF0000FFFFFFFF987FFFFFFFFFFFFF4BF800007FFFFFFF
      7BBA7FFE000180016B967FFE400140009BAD798E40014000803B76BE40014000
      803B74BE40014000803D77BE40014000800076BE40014000800779BE40014000
      80077FFE4001400080077FFE400140008007000040017FFE8007400A7FFD8001
      800700000000FFFF8007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF113FFFFFFFF
      D8A8E227FFFFC03FDBDBFFFFFFFFC01FD8ABE0210000C02FD8F9C0430000C037
      8FFBFFFF0000C003FFFFFC0F0000C003FFFFF81F0000C003A88FFFFF0000C003
      AA8FE0010000EC038A8FC0010000F403AFEFC0010000F803AFEFC001FFFFFC03
      FFFFC001FFFFFFFFFFFFC003FFFFFFFFFFFFFFFFFFFFFFFFFFFF8001FFFFFFFF
      FE7F8001D8A8D8A8F00F8001DBDBDBDBE0078001D8ABD8ABE0038001D8F9D8F9
      E00380018FFB8FFBC0038001FFFFFFFFC0038001FFFFFFFFC0038001A88FA88F
      E0038001AA8FAA8FE00780018A8F8A8FE0078001AFEFAFEFF00F8001AFEFAFEF
      F89F8001FFFFFFFFFFFFFFFFFFFFFFFFFDFFFFFFFFFFFFFFAF55FFFFFFFFC00F
      F7FFFFFFFFFFC0075DAD87E187E1C003BF7F80018001C003DBFB80018001C003
      EEAF80018001C003BDF580018001C003FFFF80018001C003DDD7C003C003C003
      9987C003C003C003888FE007E007C0C391D7F00FF00FE0038807FC3FFC3FF003
      98AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8003C003
      D8A8FFFFB6DB8001DBDB8003B6DB8001D8ABB6DB80038001D8F9C001B6DB8001
      8FFBDB6DB6DB8001FFFFE00080038001FFFFEDB6B6DB8001B911F000B6DB8001
      BB57F7FF800380019B97F787FFFF8001BB13F7DFAD8F80018BF7E3EFDDDF8001
      FFFFF787AA8FC003FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFF8003FFFF8001
      EFFF8003C003800183FF8003C0038001E0F78003C0038001E0178003C0038041
      F0018003C003806DF8078003C0038019F8178003C0038001FC3F8003C003FFFF
      FC3F8003C003B2AAFE7F8003C003B6ABF83F8003C003B267FEFF8003C003B6AB
      FEFFFFFFFFFF1267FFFFFFFFFFFFFFFFFFFFF83FFFFFFFFFFEFFC00FFFFFFF03
      C0078006F3EFFC03DEF70003FBCFF803DEF70001FCBFF00FDEF70000FE3FE003
      EEEF0000FE7FC001F6DF0000F87FC000F6DF0080FE4F8000EEEF0000FE1F8000
      F6DF8000FE7F8001F8318000FC3FC003FEFDC000F81FF007FC71E003F81FF80F
      FC75F003FC3FFC3FFFFFD806FFFFFFFFFFFFE86FFFFFFFFFFC03D797F83F83FF
      F001BFFBE0037DFFF001BFFDE0016DFFE001BFFDC00101FFC0018FF9C0016DFF
      C001B005C1C16EFFC001BFFDC1C1703FC001A86D8183AB8FC00197958003CCD7
      C001BFF98003EF1BC001BFFD8003F05BC003BFFDC007FD81C007CFFBE03FFE6D
      E01FF007F1FFFF7DFFFFFFFFFFFFFF83E01FFFFFFFFFFFFF80070000FFFFF83F
      80036DB6F3FFF01F00016DB6E1FFE00F00000000C0FFC38700000000E077C7C7
      00000000F027C7C700000000F807C7C700000000FC07FF0100810000FE07FF83
      00810000FE07FF8300010000FC07FFC700016DB6FF27FFC7C0036DB6FFF7FFEF
      E0030000FFFFFFFFFC1FFFFFFFFFFFFFFC7FFFFFFFFFFFFFF00FF007FCFFFFFF
      C007E007F07FFFFFC001C003C03F80078001C0018007000F8001C001000F000F
      8001C001000F80078001C001800780078001C0018007C0038001C001C003C00F
      8001C001C00FE03FC001C001E03FF07FE001E007F07FF9FFF007F007F9FFFFFF
      F83FF1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF001F801F80FF00F
      E001F001F807C0078001E000F007C00380038000E0078003C0038000E0078001
      C0078000E0070001C0078000E0070000E00F8000E0070000F00F8000E0070000
      F01F8000E0078001F81F8001E0078001F83F8003E007C003FC3F8007F01FC003
      FCFFC00FF81FE00FFFFFFFFFFFFFF01FFFFFFFFFFFFFFFFFF007FFFFFC0FF00F
      E007FFFFF003C003C0038007F0018001C0010003C0010000C0010001C0000000
      C001800180000000C00180008000F000C001C0018000FE00C001C0018001FE00
      C001E0038001FE01C001F0078003FE03E007F807C007FE0FF007FFFFE007FFFF
      F1FFFFFFF01FFFFFFFFFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFBFFF87FE3F
      FFFFFE0FF003EE03FFDFFF9F8001E601C78FDF8F8007C201C707DFCF800FC301
      C78F07E7800FC101FFDFCFE3800FC001FFFFCFF3800F8003FFFFEFE0800F8003
      FFFFE703800F8003FFDFE03B800F8003EF8FC1FF807FC007FFDFF7FF8FFFE03F
      FFFFF7FFFFFFF1FFFFFFFFFFFFFFFFFFFFFFE0FFFFFFFFFFFF01E07FFFFF8001
      FC01E00FC03F8001E0014001C01F8001C0010000C00F800180010000C0078001
      80010000C003800180010000C003800180018000C00380018001C000C0038001
      8003E000E0038001C007F000F0038001C00FF001F8038001C03FF003FC038001
      C0FFF80FFFFF8001FFFFFC0FFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object OpenDialog: TOpenDialog
    Left = 994
    Top = 224
  end
  object SaveDialog: TSaveDialog
    Left = 994
    Top = 122
  end
end
