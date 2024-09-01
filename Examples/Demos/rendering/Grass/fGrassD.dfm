object FormGrass: TFormGrass
  Left = 220
  Top = 170
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Grass'
  ClientHeight = 707
  ClientWidth = 1061
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 24
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 861
    Height = 707
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = Camera
    Buffer.BackgroundColor = clBackground
    Buffer.Lighting = False
    FieldOfView = 163.898666381835900000
    PenAsTouch = False
    Align = alClient
    OnClick = GLSceneViewerClick
    TabOrder = 0
  end
  object Panel: TPanel
    Left = 861
    Top = 0
    Width = 200
    Height = 707
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    TabOrder = 1
    object LeafLabel: TLabel
      Left = 14
      Top = 56
      Width = 81
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'LeafLabel'
    end
    object NodeCountLabel: TLabel
      Left = 14
      Top = 126
      Width = 81
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'LeafLabel'
    end
    object NodeHLabel: TLabel
      Left = 14
      Top = 196
      Width = 81
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'LeafLabel'
    end
    object NodeAngleLabel: TLabel
      Left = 14
      Top = 266
      Width = 81
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'LeafLabel'
    end
    object Label1: TLabel
      Left = 14
      Top = 490
      Width = 46
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Label'
    end
    object WindRangeLabel: TLabel
      Left = 14
      Top = 336
      Width = 81
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'LeafLabel'
    end
    object WindPowerLabel: TLabel
      Left = 14
      Top = 406
      Width = 81
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'LeafLabel'
    end
    object Leafbar: TTrackBar
      Left = 14
      Top = 84
      Width = 170
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Max = 10000
      Frequency = 1000
      Position = 1000
      TabOrder = 0
      ThumbLength = 18
      OnChange = LeafbarChange
    end
    object AnimateBox: TCheckBox
      Left = 14
      Top = 14
      Width = 114
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Animation'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object NodeCountBar: TTrackBar
      Left = 14
      Top = 154
      Width = 170
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Min = 1
      Position = 3
      TabOrder = 2
      ThumbLength = 18
      OnChange = LeafbarChange
    end
    object NodeHBar: TTrackBar
      Left = 14
      Top = 224
      Width = 170
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Max = 100
      Frequency = 10
      Position = 10
      TabOrder = 3
      ThumbLength = 18
      OnChange = LeafbarChange
    end
    object NodeAngleBar: TTrackBar
      Left = 14
      Top = 294
      Width = 170
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Max = 90
      Frequency = 9
      Position = 20
      TabOrder = 4
      ThumbLength = 18
      OnChange = LeafbarChange
    end
    object WindRangeBar: TTrackBar
      Left = 14
      Top = 364
      Width = 170
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Max = 1000
      Frequency = 100
      Position = 200
      TabOrder = 5
      ThumbLength = 18
      OnChange = LeafbarChange
    end
    object WindPowerBar: TTrackBar
      Left = 14
      Top = 434
      Width = 170
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Max = 100
      Frequency = 10
      Position = 20
      TabOrder = 6
      ThumbLength = 18
      OnChange = LeafbarChange
    end
  end
  object GLScene: TGLScene
    Left = 8
    Top = 8
    object Terrain: TGLTerrainRenderer
      Material.Texture.Disabled = False
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {0000D0C1000000000000D0410000803F}
      Scale.Coordinates = {CDCC4C3ECDCC4C3E0AD7A33C00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      HeightDataSource = GLBitmapHDS
      TileSize = 128
      TilesPerTexture = 0.300000011920929000
      MaxCLODTriangles = 65535
      ContourWidth = 0
    end
    object Wind1: TGLDummyCube
      Tag = 5
      Position.Coordinates = {000000C000000040000000000000803F}
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
    end
    object Wind2: TGLDummyCube
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
    end
    object Wind3: TGLDummyCube
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
    end
    object DirectOpenGL: TGLDirectOpenGL
      UseBuildList = False
      OnRender = DirectOpenGLRender
      Blend = False
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {0000000000004040000020410000803F}
    end
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    OnProgress = GLCadencerProgress
    Left = 8
    Top = 64
  end
  object GLNavigator: TGLNavigator
    VirtualUp.Coordinates = {000000000000803F0000000000000000}
    MovingObject = Camera
    UseVirtualUp = True
    AutoUpdateObject = True
    Left = 88
    Top = 8
  end
  object GLUserInterface: TGLUserInterface
    MouseSpeed = 20.000000000000000000
    GLNavigator = GLNavigator
    Left = 88
    Top = 64
  end
  object GLBitmapHDS: TGLBitmapHDS
    InfiniteWrap = False
    MaxPoolSize = 0
    Left = 176
    Top = 8
  end
  object GLAsyncTimer: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 240
    Top = 8
  end
  object MatLib: TGLMaterialLibrary
    Left = 176
    Top = 64
  end
end
