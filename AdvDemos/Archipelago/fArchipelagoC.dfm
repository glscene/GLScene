object Form1: TForm1
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  Caption = 'GLScene Archipelago'
  ClientHeight = 526
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 120
  TextHeight = 17
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 700
    Height = 526
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera
    BeforeRender = GLSceneViewerBeforeRender
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.FogEnvironment.FogStart = 500.000000000000000000
    Buffer.FogEnvironment.FogEnd = 1000.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clGray
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.FogEnable = True
    FieldOfView = 148.166763305664100000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object PAProgress: TPanel
    Left = 250
    Top = 210
    Width = 231
    Height = 61
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    BorderWidth = 6
    TabOrder = 1
    Visible = False
    object Label1: TLabel
      Left = 7
      Top = 7
      Width = 217
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Loading && compressing textures'
      ExplicitLeft = 9
      ExplicitTop = 9
      ExplicitWidth = 214
    end
    object ProgressBar: TProgressBar
      Left = 7
      Top = 32
      Width = 217
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Max = 16
      TabOrder = 0
    end
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 40
    Top = 32
    object SkyDome: TGLSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Bands = <
        item
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 25.000000000000000000
          Slices = 9
        end
        item
          StartAngle = 25.000000000000000000
          StopAngle = 90.000000000000000000
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Slices = 9
          Stacks = 4
        end>
      Stars = <>
      Options = [sdoTwinkle]
    end
    object DCCamera: TGLDummyCube
      Position.Coordinates = {0000000000000041000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 75.000000000000000000
        TargetObject = DCCamera
        Position.Coordinates = {000000000000803F000040400000803F}
        Left = 264
        Top = 160
      end
    end
    object TerrainRenderer: TGLTerrainRenderer
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {00002040000020400000003F00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      HeightDataSource = GLCustomHDS1
      TileSize = 32
      TilesPerTexture = 1.000000000000000000
      MaterialLibrary = MaterialLibrary
      CLODPrecision = 5
      OnHeightDataPostRender = TerrainRendererHeightDataPostRender
      ContourWidth = 0
    end
    object DOWake: TGLDirectOpenGL
      OnProgress = DOWakeProgress
      UseBuildList = False
      OnRender = DOWakeRender
      Blend = False
    end
    object FFSailBoat: TGLFreeForm
      Scale.Coordinates = {8FC2753C8FC2753C8FC2753C00000000}
      MaterialLibrary = MLSailBoat
    end
    object LSSun: TGLLightSource
      Ambient.Color = {9A99993E9A99993E9A99993E0000803F}
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
      LightStyle = lsParallel
      Specular.Color = {00000000000000000000000000000000}
      SpotCutOff = 180.000000000000000000
      SpotDirection.Coordinates = {0000803F0000803F0000003F00000000}
    end
    object HTFPS: TGLHUDText
      Position.Coordinates = {000096420000C841000000000000803F}
      BitmapFont = BFSmall
      Rotation = 0.000000000000000000
      ModulateColor.Color = {0000000000000000000000000000803F}
    end
    object HTHelp: TGLHUDText
      Up.Coordinates = {4CB7F8BE05C45F3F0000000000000000}
      BitmapFont = BFLarge
      Rotation = 0.000000000000000000
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 376
    Top = 96
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencerProgress
    Left = 112
    Top = 32
  end
  object MaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'detail'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'detail.jpg'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureFormat = tfLuminance
        Material.Texture.Compression = tcStandard
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {00008042000080420000804200000000}
        Texture2Name = 'detail'
      end
      item
        Name = 'water'
        Tag = 0
        Material.BlendingMode = bmTransparency
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = '035eau.jpg'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.MappingSCoordinates.Coordinates = {CDCC4C3D000000000000000000000000}
        Material.Texture.MappingTCoordinates.Coordinates = {00000000CDCC4C3D0000000000000000}
        Material.Texture.Disabled = False
        Texture2Name = 'water'
      end
      item
        Name = 'wake'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'wake.bmp'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Texture2Name = 'wake'
      end>
    Left = 40
    Top = 96
  end
  object GLHeightTileFileHDS1: TGLHeightTileFileHDS
    HTFFileName = 'Data\\Islands.htf'
    InfiniteWrap = False
    MaxPoolSize = 0
    Left = 200
    Top = 32
  end
  object BFSmall: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 296
    Top = 96
  end
  object GLCustomHDS1: TGLCustomHDS
    MaxPoolSize = 0
    OnStartPreparingData = GLCustomHDS1StartPreparingData
    OnMarkDirtyEvent = GLCustomHDS1MarkDirtyEvent
    Left = 200
    Top = 96
  end
  object MLSailBoat: TGLMaterialLibrary
    TexturePaths = '..\\Data'
    Left = 112
    Top = 96
  end
  object BFLarge: TGLWindowsBitmapFont
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = []
    Left = 296
    Top = 32
  end
end
