object Form1: TForm1
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  Caption = 'Ode Terrain'
  ClientHeight = 547
  ClientWidth = 639
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 639
    Height = 547
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.FogEnvironment.FogStart = 200.000000000000000000
    Buffer.FogEnvironment.FogEnd = 650.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clGray
    Buffer.FogEnable = True
    FieldOfView = 159.279708862304700000
    Align = alClient
    TabOrder = 0
  end
  object GLBitmapHDS1: TGLBitmapHDS
    MaxPoolSize = 0
    Left = 320
    Top = 72
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 32
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      LightStyle = lsParallel
      SpotCutOff = 180.000000000000000000
      SpotDirection.Coordinates = {00000000F4FD34BFF4FD343F00000000}
    end
    object SkyDome1: TGLSkyDome
      Up.Coordinates = {000000000000803F0000008000000000}
      Bands = <
        item
          StartAngle = -5.000000000000000000
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
      object SPMoon: TGLSprite
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.BlendingMode = bmTransparency
        Material.MaterialOptions = [moIgnoreFog]
        Material.Texture.ImageAlpha = tiaSuperBlackTransparent
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
        Position.Coordinates = {00000C430000C842000096420000803F}
        Visible = False
        Width = 30.000000000000000000
        Height = 30.000000000000000000
        Rotation = 0.000000000000000000
      end
      object SPSun: TGLSprite
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.BlendingMode = bmAdditive
        Material.MaterialOptions = [moIgnoreFog]
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureFormat = tfLuminance
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
        Position.Coordinates = {00000C430000C842000096420000803F}
        Width = 60.000000000000000000
        Height = 60.000000000000000000
        Rotation = 0.000000000000000000
      end
    end
    object TerrainRenderer1: TGLTerrainRenderer
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'ground'
      Direction.Coordinates = {09260E19ECD96EB30000803F00000000}
      Scale.Coordinates = {00008040000080400000803E00000000}
      Up.Coordinates = {E146A6250000803FECD96E3300000000}
      HeightDataSource = GLBitmapHDS1
      TileSize = 32
      TilesPerTexture = 1.000000000000000000
      QualityDistance = 150.000000000000000000
      BehavioursData = {
        0458434F4C02010201061154474C4F44454865696768744669656C6402001200
        00000002000200120000000002000500000000006F1283F53F08000005000000
        00000000FA084005000000000000000000000500000000000000000000050000
        0000000000000000050000000000000000000005000000000000000000000500
        0000000000000000000500000000000000000000050000000000000000000005
        000000000000000000000200050000000000000080FF3F080500000000000000
        C000400000803F0200}
    end
    object ODEObjects: TGLDummyCube
      CubeSize = 1.000000000000000000
      object ODERenderPoint: TGLRenderPoint
      end
    end
    object HUDText1: TGLHUDText
      Position.Coordinates = {000096420000C841000000000000803F}
      BitmapFont = BitmapFont1
      Rotation = 0.000000000000000000
    end
    object GLLensFlare: TGLLensFlare
      Size = 100
      Seed = 978
      FlareIsNotOccluded = True
      Position.Coordinates = {9A620252C9B28B51B743BAD10000803F}
      Visible = False
      object GLDummyCube1: TGLDummyCube
        CubeSize = 100.000000000000000000
        VisibleAtRunTime = True
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 650.000000000000000000
      FocalLength = 50.000000000000000000
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Left = 264
      Top = 160
      object ODEDrop: TGLDummyCube
        Position.Coordinates = {0000000000000000000020410000803F}
        CubeSize = 1.000000000000000000
      end
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 224
    Top = 72
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    OnProgress = GLCadencer1Progress
    Left = 128
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'ground'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Emission.Color = {9A99993E9A99993E9A99993E0000803F}
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Compression = tcStandard
        Material.Texture.Disabled = False
        Texture2Name = 'details'
      end
      item
        Name = 'details'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureFormat = tfLuminance
        Material.Texture.Compression = tcStandard
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {00000043000000430000004300000000}
      end>
    Left = 32
    Top = 72
  end
  object BitmapFont1: TGLBitmapFont
    GlyphsIntervalX = 1
    GlyphsIntervalY = 1
    Ranges = <
      item
        StartASCII = ' '
        StopASCII = 'Z'
        StartGlyphIdx = 0
      end>
    CharWidth = 30
    CharHeight = 30
    Left = 320
    Top = 16
  end
  object GLODEManager1: TGLODEManager
    Gravity.Coordinates = {0000000000000000C3F51CC10000803F}
    Solver = osmQuickStep
    Iterations = 3
    MaxContacts = 8
    RenderPoint = ODERenderPoint
    Visible = True
    VisibleAtRunTime = True
    Left = 416
    Top = 16
  end
  object GLNavigator1: TGLNavigator
    VirtualUp.Coordinates = {00000000000000000000803F0000803F}
    MovingObject = GLCamera1
    UseVirtualUp = True
    AutoUpdateObject = True
    Left = 224
    Top = 16
  end
  object GLUserInterface1: TGLUserInterface
    MouseSpeed = 25.000000000000000000
    GLNavigator = GLNavigator1
    GLVertNavigator = GLNavigator1
    Left = 128
    Top = 72
  end
end
