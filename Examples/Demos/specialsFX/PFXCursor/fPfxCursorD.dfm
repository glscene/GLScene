object Form1: TForm1
  Left = 403
  Top = 221
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 903
  ClientWidth = 1346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 168
  TextHeight = 24
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1064
    Height = 903
    Cursor = -1
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clNavy
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    FieldOfView = 167.361404418945300000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 1064
    Top = 0
    Width = 282
    Height = 903
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    TabOrder = 1
    object RadioGroup1: TRadioGroup
      Left = 32
      Top = 406
      Width = 239
      Height = 184
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Fire'
      ItemIndex = 0
      Items.Strings = (
        'Burn'
        'No Burn')
      TabOrder = 0
      OnClick = RadioGroup1Click
    end
    object RadioGroup2: TRadioGroup
      Left = 32
      Top = 112
      Width = 239
      Height = 184
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Perlin'
      ItemIndex = 0
      Items.Strings = (
        'Trail'
        'No Trail')
      TabOrder = 1
      OnClick = RadioGroup2Click
    end
    object stPerlin: TStaticText
      Left = 28
      Top = 56
      Width = 231
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Left mouse button for perlin'
      TabOrder = 2
      OnClick = stPerlinClick
    end
    object stFire: TStaticText
      Left = 28
      Top = 350
      Width = 223
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Right mouse button for fire'
      TabOrder = 3
    end
  end
  object GLScene1: TGLScene
    Left = 224
    Top = 16
    object GLParticleFXRenderer1: TGLParticleFXRenderer
    end
    object Light1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000000000000A040000000400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object cur: TGLHUDSprite
      Material.FrontProperties.Diffuse.Color = {000000000000803F000000000000803F}
      Material.BlendingMode = bmTransparency
      Material.MaterialOptions = [moNoLighting]
      Material.Texture.ImageAlpha = tiaAlphaFromIntensity
      Material.Texture.TextureMode = tmModulate
      Material.Texture.TextureWrap = twNone
      Material.Texture.Disabled = False
      Position.Coordinates = {00C08A4300C08A43000000000000803F}
      Width = 32.000000000000000000
      Height = 32.000000000000000000
      Rotation = 0.000000000000000000
    end
    object dc_cur: TGLDummyCube
      CubeSize = 0.009999999776482582
      VisibleAtRunTime = True
      object dc1: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        EffectsData = {
          0458434F4C02010203061254474C536F75726365504658456666656374020202
          00120000000002000200060470667831050000000000000080FE3F0206020009
          00000000000000BF0000000000000000020008020008050000000000CDCCCCFC
          3F050000000000CDCCCCFC3F0500000000000AD7A3F83F020002000905000000
          000000000000000902000200020202001200000000020002000617474C506F69
          6E744C696768745046584D616E6167657232050000000000000080FD3F020602
          0008020008020008050000000000CDCCCCFD3F050000000000CDCCCCFC3F0500
          000000000AD7A3F83F0200020009050000000000000000000009020002000202
          02001200000000020002000617474C506F696E744C696768745046584D616E61
          67657233050000000000000080FF3F0206020008020008020008050000000000
          000000000005000000000000000000000500000000000AD7A3F73F0200020009
          0500000000000000000000090200}
      end
      object dc2: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        EffectsData = {
          0458434F4C02010201061254474C536F75726365504658456666656374020202
          00120000000002000200060470667832050000000000000080FF3F0206020009
          00000000000000400000000000000000020008020008050000000000000080FE
          3F050000000000CDCCCCFC3F0500000000008FC2F5F83F020102010905000000
          00000000000000090201}
      end
      object dc3: TGLDummyCube
        CubeSize = 1.000000000000000000
        EffectsData = {
          0458434F4C02010201061254474C536F75726365504658456666656374020202
          00120000000002000200060470667833050000000000000080FF3F0206020008
          0200080200080500000000000000800040050000000000CDCCCCFB3F05000000
          00000AD7A3F73F02000200090500000000000000000000090200}
      end
    end
    object rend: TGLParticleFXRenderer
    end
    object GLDummyCube3: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCone1: TGLCone
        Material.FrontProperties.Emission.Color = {CDCC4C3FACC8483E9A99193F0000803F}
        Direction.Coordinates = {000000002EBDBBB3000080BF00000000}
        PitchAngle = 180.000000000000000000
        Position.Coordinates = {000080BF00000000000000000000803F}
        Up.Coordinates = {00000000000080BF2EBDBB3300000000}
        BottomRadius = 0.300000011920929000
        Height = 0.500000000000000000
        object GLDummyCube1: TGLDummyCube
          Position.Coordinates = {000000009A9919BE000000000000803F}
          CubeSize = 1.000000000000000000
          EffectsData = {
            0458434F4C02010201061254474C536F75726365504658456666656374020202
            001200000000020002000613474C5065726C696E5046584D616E616765723105
            0000000000000080FD3F0206020009000000000000803F000000000000000002
            0008020008050000000000000000000005000000000000000000000500000000
            00CDCCCCFB3F02000200090500000000000000000000080200}
        end
      end
      object GLCone2: TGLCone
        Material.FrontProperties.Emission.Color = {0000803FF8FEFE3E000000000000803F}
        Direction.Coordinates = {000000002EBDBBB3000080BF00000000}
        PitchAngle = 180.000000000000000000
        Position.Coordinates = {0000803F00000000000000000000803F}
        Up.Coordinates = {00000000000080BF2EBDBB3300000000}
        BottomRadius = 0.300000011920929000
        Height = 0.500000000000000000
        object GLDummyCube2: TGLDummyCube
          Position.Coordinates = {000000009A9919BE000000000000803F}
          CubeSize = 1.000000000000000000
          EffectsData = {
            0458434F4C02010201060A54474C424669726546580201020012000000000200
            02001200000000}
        end
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube3
      Position.Coordinates = {0000000000000040000080400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'plane'
        Tag = 0
        Material.FrontProperties.Emission.Color = {DFDEDE3EDCDB5B3F9493133F0000803F}
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.ImageAlpha = tiaAlphaFromIntensity
      end>
    Left = 224
    Top = 80
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 312
    Top = 16
  end
  object pfx1: TGLPointLightPFXManager
    Cadencer = GLCadencer1
    Renderer = rend
    Friction = 1.000000000000000000
    ColorMode = scmFade
    ParticleSize = 1.000000000000000000
    ColorInner.Color = {0000803F000000000000803F0000803F}
    ColorOuter.Color = {0000803F00000000000000000000803F}
    LifeColors = <
      item
        LifeTime = 5.000000000000000000
        SizeScale = 2.000000000000000000
      end>
    Left = 312
    Top = 80
  end
  object GLAsyncTimer1: TGLAsyncTimer
    Enabled = True
    Interval = 800
    OnTimer = GLAsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 384
    Top = 16
  end
  object GLPointLightPFXManager2: TGLPointLightPFXManager
    Cadencer = GLCadencer1
    Renderer = rend
    Acceleration.Coordinates = {00000000000040BF0000000000000000}
    Friction = 1.000000000000000000
    ParticleSize = 0.500000000000000000
    ColorInner.Color = {000000000000803F0000803F0000803F}
    ColorOuter.Color = {0000803F0000803F0000803F0000803F}
    LifeColors = <
      item
        LifeTime = 3.000000000000000000
        SizeScale = 0.500000000000000000
        RotateAngle = 0.300000011920929000
      end>
    Left = 224
    Top = 136
  end
  object GLPointLightPFXManager3: TGLPointLightPFXManager
    Cadencer = GLCadencer1
    Renderer = rend
    Friction = 1.000000000000000000
    ColorMode = scmFade
    ParticleSize = 0.500000000000000000
    ColorInner.Color = {ACC8483ECDCC4C3FACC8483E0000803F}
    ColorOuter.Color = {0000803F0000803F000000000000803F}
    LifeColors = <
      item
        LifeTime = 1.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 224
    Top = 208
  end
  object pfx2: TGLCustomSpritePFXManager
    Cadencer = GLCadencer1
    Renderer = rend
    Friction = 1.000000000000000000
    ColorMode = scmFade
    ParticleSize = 0.750000000000000000
    ColorInner.Color = {0000803F1283C03E000000000000803F}
    LifeColors = <
      item
        ColorInner.Color = {0000003F0000003F0000000000000000}
        LifeTime = 0.750000000000000000
        SizeScale = 3.000000000000000000
      end>
    Left = 384
    Top = 80
  end
  object pfx3: TGLCustomSpritePFXManager
    Cadencer = GLCadencer1
    Renderer = rend
    Acceleration.Coordinates = {000000009A9999BE0000000000000000}
    Friction = 1.000000000000000000
    OnPrepareTextureImage = pfx3PrepareTextureImage
    ParticleSize = 0.750000000000000000
    ColorInner.Color = {0000803FAE47E13D7B142E3F0000803F}
    LifeColors = <
      item
        ColorInner.Color = {CDCC4C3FACC8483E9A99193F00000000}
        LifeTime = 0.750000000000000000
        SizeScale = 3.000000000000000000
        RotateAngle = 0.949999988079071000
      end>
    Left = 456
    Top = 80
  end
  object GLPerlinPFXManager1: TGLPerlinPFXManager
    Cadencer = GLCadencer1
    Renderer = GLParticleFXRenderer1
    Friction = 1.000000000000000000
    Smoothness = 1.000000000000000000
    Brightness = 1.000000000000000000
    Gamma = 1.000000000000000000
    ParticleSize = 1.000000000000000000
    ColorInner.Color = {0000803F000000000000803F0000803F}
    LifeColors = <
      item
        LifeTime = 3.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 552
    Top = 80
  end
  object GLFireFXManager1: TGLFireFXManager
    FireDir.Coordinates = {00000000000000BF0000000000000000}
    InitialDir.Coordinates = {00000000000080BF0000000000000000}
    Cadencer = GLCadencer1
    ParticleSize = 0.250000000000000000
    FireDensity = 1.000000000000000000
    FireEvaporation = 0.860000014305114700
    ParticleLife = 5
    FireBurst = 1.000000000000000000
    FireRadius = 0.250000000000000000
    Disabled = False
    Paused = False
    ParticleInterval = 0.100000001490116100
    UseInterval = True
    Left = 672
    Top = 80
  end
end
