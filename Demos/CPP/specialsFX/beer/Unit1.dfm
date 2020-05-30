object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'GLScene Beer'
  ClientHeight = 386
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 484
    Height = 386
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.AmbientColor.Color = {0000000000000000000000000000803F}
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aa2x
    Buffer.ShadeModel = smSmooth
    FieldOfView = 150.951690673828100000
    Align = alClient
    OnDblClick = GLSceneViewer1DblClick
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    VisibilityCulling = vcHierarchical
    Left = 32
    Top = 16
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {000000C00000003F000040400000803F}
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
      object GLCylinder1: TGLCylinder
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {3333733F6666E63E000000000000003F}
        Material.MaterialOptions = [moNoLighting]
        Position.Coordinates = {00000000CDCC4C3D000000000000803F}
        Scale.Coordinates = {00000040CDCC0C400000004000000000}
        BottomRadius = 0.500000000000000000
        Height = 1.049999952316284000
        Slices = 32
        Stacks = 1
        TopRadius = 0.500000000000000000
        Parts = [cySides, cyBottom]
        EffectsData = {
          0458434F4C02010201061254474C536F75726365504658456666656374020202
          001200000000020002000614474C506F6C79676F6E5046584D616E6167657231
          050000000000000080FF3F020602000900000000CDCCCC3D0000000000000000
          02000900000000666666BF00000000000000000200090000803F9A99993E0000
          803F00000000050000000000CDCCCCFA3F0500000000003333F3FE3F05000000
          00008FC2F5F93F02000201090500000000000000000000080200}
      end
      object GLParticleFXRenderer2: TGLParticleFXRenderer
        ZTest = False
        BlendingMode = bmTransparency
      end
      object GLCylinder2: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moNoLighting]
        Position.Coordinates = {000000000000C03F000000000000803F}
        Scale.Coordinates = {000000400000003F0000004000000000}
        BottomRadius = 0.500000000000000000
        Height = 1.200000047683716000
        Slices = 32
        Stacks = 1
        TopRadius = 0.500000000000000000
        Parts = [cySides, cyTop]
      end
      object GLDummyCube3: TGLDummyCube
        Position.Coordinates = {00000000CDCCEC3F000000000000803F}
        CubeSize = 1.000000000000000000
        EffectsData = {
          0458434F4C02010201061254474C536F75726365504658456666656374020202
          001200000000020002000613474C5065726C696E5046584D616E616765723105
          0000000000000080FF3F02060200080200080200090000803F9A99193E000080
          3F000000000500000000000000000000050000000000000080FF3F0500000000
          000AD7A3F93F02000201090500000000000000000000080200}
      end
      object GLParticleFXRenderer1: TGLParticleFXRenderer
        BlendingMode = bmTransparency
      end
      object GLFreeForm1: TGLFreeForm
        Material.BlendingMode = bmAdditive
        Material.Texture.MappingMode = tmmSphere
        Material.Texture.Disabled = False
      end
    end
    object GLShadowPlane1: TGLShadowPlane
      Material.Texture.Disabled = False
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {0000000085EBB1BF000000000000803F}
      Scale.Coordinates = {00002041000020410000803F00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 1.000000000000000000
      Width = 1.000000000000000000
      ShadowingObject = GLFreeForm1
      ShadowedLight = GLLightSource1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000404000004040000040400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 32
    Top = 72
  end
  object GLPerlinPFXManager1: TGLPerlinPFXManager
    Cadencer = GLCadencer1
    Renderer = GLParticleFXRenderer1
    Friction = 1.000000000000000000
    BlendingMode = bmTransparency
    Smoothness = 1.000000000000000000
    Brightness = 3.000000000000000000
    Gamma = 1.399999976158142000
    NoiseScale = 200
    NoiseAmplitude = 100
    ParticleSize = 0.300000011920929000
    ColorInner.Color = {0000803F0000803F0000803F00000000}
    ColorOuter.Color = {0000803F0000803F0000803F00000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000803F0000803F0000803F}
        ColorOuter.Color = {0000803F0000803F0000803F00000000}
        LifeTime = 0.100000001490116100
        SizeScale = 1.000000000000000000
      end
      item
        ColorInner.Color = {0000803F0000803F0000803F0000803F}
        ColorOuter.Color = {0000803F0000803F0000803F00000000}
        LifeTime = 4.000000000000000000
        SizeScale = 1.000000000000000000
      end
      item
        ColorInner.Color = {0000803F0000803F0000803F00000000}
        ColorOuter.Color = {0000803F0000803F0000803F00000000}
        LifeTime = 5.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 336
    Top = 72
  end
  object GLPolygonPFXManager1: TGLPolygonPFXManager
    Cadencer = GLCadencer1
    Renderer = GLParticleFXRenderer2
    Acceleration.Coordinates = {000000009A99993E0000000000000000}
    Friction = 1.000000000000000000
    BlendingMode = bmTransparency
    NbSides = 5
    ParticleSize = 0.029999999329447750
    ColorInner.Color = {0000803F000000000000000000000000}
    ColorOuter.Color = {0000803F0000803F0000000000000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000000000000000CDCCCC3E}
        ColorOuter.Color = {0000803F0000803F0000000000000000}
        LifeTime = 0.250000000000000000
        SizeScale = 1.000000000000000000
      end
      item
        ColorInner.Color = {0000803F00000000000000009A99193F}
        ColorOuter.Color = {0000803F0000803F0000000000000000}
        LifeTime = 3.500000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 336
    Top = 8
  end
end
