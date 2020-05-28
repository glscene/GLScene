object GLSLTestForm: TGLSLTestForm
  Left = 261
  Top = 176
  Caption = 'GLSL Diffuse Specular Shader'
  ClientHeight = 375
  ClientWidth = 544
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 544
    Height = 330
    Camera = Camera
    Buffer.FogEnvironment.FogColor.Color = {ACC8483E9A99193FCDCC4C3F9A99993E}
    Buffer.FogEnvironment.FogStart = 50.000000000000000000
    Buffer.FogEnvironment.FogEnd = 5000.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clBackground
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow, roDebugContext]
    Buffer.AntiAliasing = aaNone
    Buffer.DepthPrecision = dp24bits
    Buffer.ColorDepth = cd24bits
    FieldOfView = 136.997131347656300000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 330
    Width = 544
    Height = 45
    Align = alBottom
    TabOrder = 1
    object LightMovingCheckBox: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Light is Moving'
      Checked = True
      Color = clBtnFace
      ParentColor = False
      State = cbChecked
      TabOrder = 0
    end
    object ShaderEnabledCheckBox: TCheckBox
      Left = 176
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Shader Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = ShaderEnabledCheckBoxClick
    end
    object TurnPitchrollCheckBox: TCheckBox
      Left = 8
      Top = 24
      Width = 137
      Height = 17
      Caption = 'Turn-Pitch-Roll Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object RealisticSpecularCheckBox: TCheckBox
      Left = 336
      Top = 8
      Width = 137
      Height = 17
      Caption = 'Realistic Specular Mode'
      TabOrder = 3
      OnClick = RealisticSpecularCheckBoxClick
    end
    object MultiLightShaderCheckBox: TCheckBox
      Left = 176
      Top = 24
      Width = 113
      Height = 17
      Caption = 'MultiLight Shader'
      TabOrder = 4
      OnClick = MultiLightShaderCheckBoxClick
    end
    object EnableFogCheckBox: TCheckBox
      Left = 336
      Top = 24
      Width = 137
      Height = 17
      Caption = 'Enable fog'
      TabOrder = 5
      OnClick = EnableFogCheckBoxClick
    end
  end
  object Scene: TGLScene
    ObjectsSorting = osNone
    Left = 24
    Top = 16
    object GUICube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLArrowLine1: TGLArrowLine
        Position.Coordinates = {00000000000000000000C8420000803F}
        Scale.Coordinates = {0000A0410000A0410000A04100000000}
        BottomRadius = 0.100000001490116100
        Height = 1.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLXYZGrid1: TGLXYZGrid
        Direction.Coordinates = {00000000000080BF0000000000000000}
        Position.Coordinates = {00000000000000000000A0C10000803F}
        Up.Coordinates = {00000000000000000000803F00000000}
        XSamplingScale.Min = -200.000000000000000000
        XSamplingScale.Max = 200.000000000000000000
        XSamplingScale.Step = 20.000000000000000000
        YSamplingScale.Min = -200.000000000000000000
        YSamplingScale.Max = 200.000000000000000000
        YSamplingScale.Step = 20.000000000000000000
        ZSamplingScale.Min = -200.000000000000000000
        ZSamplingScale.Max = 200.000000000000000000
        ZSamplingScale.Step = 20.000000000000000000
        Parts = [gpX, gpZ]
      end
    end
    object LightCube: TGLDummyCube
      Direction.Coordinates = {0000000000000000000080BF00000000}
      Position.Coordinates = {000096C300000000000096430000803F}
      OnProgress = LightCubeProgress
      CubeSize = 1.000000000000000000
      object Light: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000803F00000000000000000000803F}
        LightStyle = lsOmni
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
      object GLSphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {E6E5653F8180003DCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {F4F3733FEEED6D3F000000000000803F}
        Radius = 10.000000000000000000
      end
    end
    object LightCube2: TGLDummyCube
      Position.Coordinates = {000096430000C842000096430000803F}
      Visible = False
      CubeSize = 1.000000000000000000
      object Light2: TGLLightSource
        Ambient.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {DCD8583FC6BF3F3FDCD8583F0000803F}
        LightStyle = lsParallel
        Shining = False
        Specular.Color = {0000803F0000003F0000003F0000803F}
        SpotCutOff = 180.000000000000000000
        SpotDirection.Coordinates = {0000C842000096430000C84200000000}
      end
      object GLArrowLine2: TGLArrowLine
        Material.FrontProperties.Ambient.Color = {1904763FCDCC4C3ECDCC4C3E0000803F}
        Material.FrontProperties.Diffuse.Color = {E7FB693FCDCC4C3FCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {1904763FCDCC4C3ECDCC4C3E0000803F}
        Direction.Coordinates = {B25F9ABE8B8F67BFB25F9ABE00000000}
        Scale.Coordinates = {00004842000048420000484200000000}
        Up.Coordinates = {F7BC23BF4A51DA3EF7BC23BF00000000}
        BottomRadius = 0.100000001490116100
        Height = 2.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
    end
    object WorldCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Fighter: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Fighter'
        Position.Coordinates = {0000164300000000000000000000803F}
        Up.Coordinates = {00000000000080BF0000000000000000}
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object Teapot: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Fighter'
        Position.Coordinates = {000016C300000000000000000000803F}
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object Sphere_big: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Earth'
        Position.Coordinates = {00000000000016C3000000000000803F}
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object Sphere_little: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Earth'
        Position.Coordinates = {0000000000001643000000000000803F}
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
    end
    object Camera: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 65.000000000000000000
      TargetObject = GLXYZGrid1
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {000016C30000E1430000B4430000803F}
      Direction.Coordinates = {00000000000080BF0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object Cadencer: TGLCadencer
    Scene = Scene
    MaxDeltaTime = 0.020000000000000000
    OnProgress = CadencerProgress
    Left = 24
    Top = 56
  end
  object MaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Fighter'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {BFBEBE3EBBBABA3EBBBABA3E0000803F}
        Material.FrontProperties.Emission.Color = {B1B0B03DB1B0B03DB1B0B03D0000803F}
        Material.FrontProperties.Specular.Color = {8988083E8180003E8988083E0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Earth'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {C1C0403DC1C0403D8180003D0000803F}
        Material.FrontProperties.Emission.Color = {A1A0203DA1A0203DC1C0403D0000803F}
        Material.FrontProperties.Shininess = 128
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end>
    Left = 24
    Top = 88
  end
  object DiffuseSpecularShader: TGLSLDiffuseSpecularShader
    LightPower = 1.000000000000000000
    Left = 56
    Top = 16
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    FormCaption = 'GLSL Diffuse Specular Shader  -   %FPS'
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
    Left = 56
    Top = 56
  end
end
