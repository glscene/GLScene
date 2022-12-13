object FormActorms3d: TFormActorms3d
  Left = 332
  Top = 110
  Caption = 'Actor MS3D Animation'
  ClientHeight = 714
  ClientWidth = 1028
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 65
    Width = 1028
    Height = 649
    Camera = GLCamera1
    Buffer.BackgroundColor = 3618615
    Buffer.AmbientColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.AntiAliasing = aa4xHQ
    Buffer.ShadeModel = smSmooth
    FieldOfView = 139.411239624023400000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1028
    Height = 65
    Align = alTop
    TabOrder = 1
    object Button2: TButton
      Left = 299
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Next Frame'
      TabOrder = 0
      OnClick = Button2Click
    end
    object btnStartStop: TButton
      Left = 13
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 1
      OnClick = btnStartStopClick
    end
    object Button4: TButton
      Left = 867
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Previous'
      TabOrder = 2
      OnClick = Button4Click
    end
    object aniBox: TComboBox
      Left = 104
      Top = 12
      Width = 177
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnSelect = aniBoxSelect
      Items.Strings = (
        'Dance'
        'Sexy Walk'
        'Cartwheel'
        'Hand Flip'
        'Wave'
        'Sun Salutation'
        'Sit')
    end
    object aniPos: TTrackBar
      Left = 388
      Top = 12
      Width = 473
      Height = 29
      Enabled = False
      TabOrder = 4
      OnChange = aniPosChange
    end
  end
  object GLScene1: TGLScene
    Left = 28
    Top = 84
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 120.000000000000000000
      TargetObject = Chair1
      Position.Coordinates = {000020C100002041000000000000803F}
      Direction.Coordinates = {00000000000000800000803F00000000}
    end
    object GLCamera2: TGLCamera
      DepthOfView = 20.000000000000000000
      FocalLength = 70.000000000000000000
      Position.Coordinates = {0000A0400000A0400000A0400000803F}
      object Light2: TGLLightSource
        Ambient.Color = {6666663F6666663F6666663F0000803F}
        ConstAttenuation = 0.600000023841857900
        Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 80.000000000000000000
        SpotDirection.Coordinates = {00000000000000000000803F00000000}
        SpotExponent = 1.000000000000000000
        object Globus: TGLSphere
          Material.Texture.Disabled = False
          Radius = 0.500000000000000000
        end
      end
    end
    object GLFrameBuffer: TGLFBORenderer
      Width = 512
      Height = 512
      DepthTextureName = 'Shadow'
      ClearOptions = [coDepthBufferClear, coUseBufferBackground]
      Camera = GLCamera2
      RootObject = Root
      EnabledRenderBuffers = []
      BeforeRender = GLFrameBufferBeforeRender
      AfterRender = GLFrameBufferAfterRender
    end
    object Root: TGLDummyCube
      CubeSize = 1.000000000000000000
      object LightSpot: TGLLightSource
        Ambient.Color = {9A99593F9A99593FCDCCCC3D0000803F}
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {9A99593F9A99593FCDCCCC3D0000803F}
        Position.Coordinates = {0000A040000080400000A0400000803F}
        Shining = False
        Specular.Color = {9A99593F9A99593FCDCCCC3D0000803F}
        SpotCutOff = 80.000000000000000000
      end
      object GLDirectOpenGL1: TGLDirectOpenGL
        UseBuildList = False
        OnRender = GLDirectOpenGL1Render
        Blend = False
      end
      object Actor1: TGLActor
        Material.Texture.Disabled = False
        Direction.Coordinates = {2EBD3B34F0AD099D0000803F00000000}
        Position.Coordinates = {000080BF00000000000000000000803F}
        Up.Coordinates = {043D48A70000803F6042CA2600000000}
        Reference = aarSkeleton
        Interval = 8
        Options = []
        OnEndFrameReached = Actor1EndFrameReached
        MaterialLibrary = MatLib
      end
      object GLPlane1: TGLPlane
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Height = 11.000000000000000000
        Width = 11.000000000000000000
        XTiles = 5
        YTiles = 5
        Style = [psTileTexture]
      end
      object Chair1: TGLFreeForm
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
        Direction.Coordinates = {EE8384BE00000000EA46773F00000000}
        Position.Coordinates = {000040BF0000000052B85E3F0000803F}
        TurnAngle = -15.000000000000000000
        MaterialLibrary = MatLib
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 108
    Top = 84
  end
  object GLNavigation: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Form1 - %FPS'
    Options = [snoMouseWheelHandled]
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
    Left = 112
    Top = 144
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 264
    Top = 84
  end
  object GLSArchiveManager1: TGLSArchiveManager
    Archives = <
      item
        Name = 'LibArchive'
      end>
    Left = 276
    Top = 184
  end
  object GLSLShader1: TGLSLShader
    Enabled = False
    OnApply = GLSLShader1Apply
    OnInitialize = GLSLShader1Initialize
    Left = 489
    Top = 111
  end
  object MatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'floor_parquet'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.Disabled = False
      end
      item
        Name = 'Shadow'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Shader = GLSLShader1
      end
      item
        Name = 'LightSpot'
        Tag = 0
        Material.Texture.Disabled = False
        Shader = GLSLShader1
      end
      item
        Name = 'Hair'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLCompositeImage'
        Material.Texture.Image.Width = 256
        Material.Texture.Image.Height = 256
        Material.Texture.Image.Depth = 0
      end
      item
        Name = 'Woman4_skin'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'Chair'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
        Shader = GLSLShader1
      end>
    Left = 235
    Top = 105
  end
end
