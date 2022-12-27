object Form1: TForm1
  Left = 155
  Top = 121
  Align = alClient
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 405
  ClientWidth = 603
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnKeyPress = FormKeyPress
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 603
    Height = 405
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.FogEnvironment.FogStart = 200.000000000000000000
    Buffer.FogEnvironment.FogEnd = 650.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clGray
    Buffer.FogEnable = True
    Buffer.Lighting = False
    FieldOfView = 152.260620117187500000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLBitmapHDS1: TGLBitmapHDS
    MaxPoolSize = 0
    Left = 208
    Top = 72
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 24
    Top = 16
    object InitialRenderPoint: TGLRenderPoint
    end
    object SkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
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
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000000041000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 650.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = DummyCube1
        Position.Coordinates = {0000A040000020410000C8410000803F}
        Left = 264
        Top = 160
      end
    end
    object TerrainRenderer1: TGLTerrainRenderer
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'ground'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {00008040000080400000803E00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      HeightDataSource = GLBitmapHDS1
      TileSize = 32
      TilesPerTexture = 1.000000000000000000
      QualityDistance = 150.000000000000000000
      ContourWidth = 0
    end
    object HUDText1: TGLHUDText
      Position.Coordinates = {000096420000C841000000000000803F}
      BitmapFont = BitmapFont1
      Rotation = 0.000000000000000000
    end
    object DCSound: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLLensFlare: TGLLensFlare
      Size = 100
      Seed = 978
      FlareIsNotOccluded = True
      PreRenderPoint = InitialRenderPoint
      Position.Coordinates = {9A620252C9B28B51B743BAD10000803F}
      Visible = False
      object GLDummyCube1: TGLDummyCube
        CubeSize = 100.000000000000000000
        VisibleAtRunTime = True
      end
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 496
    Top = 24
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 72
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
    Left = 112
    Top = 16
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
    Left = 112
    Top = 72
  end
  object GLSMBASS1: TGLSMBASS
    Active = True
    MaxChannels = 32
    MasterVolume = 1.000000000000000000
    Listener = GLCamera1
    Sources = <>
    Cadencer = GLCadencer1
    Environment = seForest
    Algorithm3D = algFull
    Left = 296
    Top = 16
    Doppler = 0.000000000000000000
  end
  object TISound: TTimer
    Interval = 3000
    OnTimer = TISoundTimer
    Left = 440
    Top = 24
  end
  object GLSoundLibrary: TGLSoundLibrary
    Samples = <>
    Left = 208
    Top = 16
  end
  object Timer2: TTimer
    Left = 136
    Top = 176
  end
end
