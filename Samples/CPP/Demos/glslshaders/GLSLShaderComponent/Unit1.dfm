object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'GLSL Shader Component'
  ClientHeight = 409
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 562
    Height = 380
    Camera = Camera
    Buffer.BackgroundColor = clBackground
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aaNone
    Buffer.DepthPrecision = dp24bits
    Buffer.ColorDepth = cd24bits
    FieldOfView = 142.227828979492200000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 380
    Width = 562
    Height = 29
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
    object ShadeEnabledCheckBox: TCheckBox
      Left = 288
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Shader Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = ShadeEnabledCheckBoxClick
    end
    object PitchRollTurnCheckBox: TCheckBox
      Left = 112
      Top = 8
      Width = 137
      Height = 17
      Caption = 'Pitch-Roll-Turn Enabled'
      TabOrder = 2
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
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
      object GLSphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {E6E5653F8180003DCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {F4F3733FEEED6D3F000000000000803F}
        Radius = 10.000000000000000000
      end
    end
    object WorldCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Fighter: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'ShaderMaterial'
        Position.Coordinates = {0000164300000000000000000000803F}
        Up.Coordinates = {00000000000080BF0000008000000000}
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object Teapot: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'ShaderMaterial'
        Position.Coordinates = {000016C300000000000000000000803F}
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object Sphere_big: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'ShaderMaterial'
        Position.Coordinates = {00000000000016C3000000000000803F}
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object Sphere_little: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'ShaderMaterial'
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
      Position.Coordinates = {000048C20000164300007A430000803F}
      Direction.Coordinates = {00000000000080BF0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object Cadencer: TGLCadencer
    Scene = Scene
    MaxDeltaTime = 0.020000000000000000
    OnProgress = CadencerProgress
    Left = 24
    Top = 72
  end
  object MaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Earth'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {BFBEBE3EBBBABA3EBBBABA3E0000803F}
        Material.FrontProperties.Emission.Color = {B1B0B03DB1B0B03DB1B0B03D0000803F}
        Material.FrontProperties.Specular.Color = {8988083E8180003E8988083E0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'ShaderMaterial'
        Tag = 0
        Material.FrontProperties.Emission.Color = {A1A0A03D9190903D8180803D0000803F}
        Material.FrontProperties.Specular.Color = {DEDD5D3FDCDB5B3FDCDB5B3F0000803F}
        Shader = GLSLShader
      end>
    Left = 96
    Top = 16
  end
  object GLSLShader: TGLSLShader
    Enabled = False
    OnApply = GLSLShaderApply
    OnUnApply = GLSLShaderUnApply
    OnInitialize = GLSLShaderInitialize
    Left = 96
    Top = 72
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    FormCaption = 'GLSL Shader Component - %FPS'
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
    Left = 200
    Top = 16
  end
end
