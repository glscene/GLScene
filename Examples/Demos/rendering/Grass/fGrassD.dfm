object FormGrass: TFormGrass
  Left = 220
  Top = 170
  Caption = 'Grass'
  ClientHeight = 404
  ClientWidth = 602
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 487
    Height = 404
    Camera = Camera
    Buffer.BackgroundColor = clBackground
    Buffer.Lighting = False
    FieldOfView = 152.194625854492200000
    PenAsTouch = False
    Align = alClient
    OnClick = GLSceneViewerClick
    TabOrder = 0
  end
  object Panel: TPanel
    Left = 487
    Top = 0
    Width = 115
    Height = 404
    Align = alRight
    TabOrder = 1
    object LeafLabel: TLabel
      Left = 8
      Top = 32
      Width = 47
      Height = 13
      Caption = 'LeafLabel'
    end
    object NodeCountLabel: TLabel
      Left = 8
      Top = 72
      Width = 47
      Height = 13
      Caption = 'LeafLabel'
    end
    object NodeHLabel: TLabel
      Left = 8
      Top = 112
      Width = 47
      Height = 13
      Caption = 'LeafLabel'
    end
    object NodeAngleLabel: TLabel
      Left = 8
      Top = 152
      Width = 47
      Height = 13
      Caption = 'LeafLabel'
    end
    object Label1: TLabel
      Left = 8
      Top = 280
      Width = 26
      Height = 13
      Caption = 'Label'
    end
    object WindRangeLabel: TLabel
      Left = 8
      Top = 192
      Width = 47
      Height = 13
      Caption = 'LeafLabel'
    end
    object WindPowerLabel: TLabel
      Left = 8
      Top = 232
      Width = 47
      Height = 13
      Caption = 'LeafLabel'
    end
    object Leafbar: TTrackBar
      Left = 8
      Top = 48
      Width = 97
      Height = 17
      Max = 10000
      Frequency = 1000
      Position = 1000
      TabOrder = 0
      ThumbLength = 10
      OnChange = LeafbarChange
    end
    object AnimateBox: TCheckBox
      Left = 8
      Top = 8
      Width = 65
      Height = 17
      Caption = 'Animation'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object NodeCountBar: TTrackBar
      Left = 8
      Top = 88
      Width = 97
      Height = 17
      Min = 1
      Position = 3
      TabOrder = 2
      ThumbLength = 10
      OnChange = LeafbarChange
    end
    object NodeHBar: TTrackBar
      Left = 8
      Top = 128
      Width = 97
      Height = 17
      Max = 100
      Frequency = 10
      Position = 10
      TabOrder = 3
      ThumbLength = 10
      OnChange = LeafbarChange
    end
    object NodeAngleBar: TTrackBar
      Left = 8
      Top = 168
      Width = 97
      Height = 17
      Max = 90
      Frequency = 9
      Position = 20
      TabOrder = 4
      ThumbLength = 10
      OnChange = LeafbarChange
    end
    object WindRangeBar: TTrackBar
      Left = 8
      Top = 208
      Width = 97
      Height = 17
      Max = 1000
      Frequency = 100
      Position = 200
      TabOrder = 5
      ThumbLength = 10
      OnChange = LeafbarChange
    end
    object WindPowerBar: TTrackBar
      Left = 8
      Top = 248
      Width = 97
      Height = 17
      Max = 100
      Frequency = 10
      Position = 20
      TabOrder = 6
      ThumbLength = 10
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
