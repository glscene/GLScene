object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'GLSL Bump Shader'
  ClientHeight = 434
  ClientWidth = 566
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 566
    Height = 387
    Camera = Camera
    Buffer.BackgroundColor = clBackground
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aaNone
    Buffer.DepthPrecision = dp24bits
    Buffer.ColorDepth = cd24bits
    FieldOfView = 142.863815307617200000
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 387
    Width = 566
    Height = 47
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
    object RollPitchTurnCheckBox: TCheckBox
      Left = 8
      Top = 24
      Width = 161
      Height = 17
      Caption = 'Roll / Pitch / Turn Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object ShaderEnabledCheckBox: TCheckBox
      Left = 176
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Shader Enabled'
      TabOrder = 2
      OnClick = ShaderEnabledCheckBoxClick
    end
    object MultiLightShaderCheckBox: TCheckBox
      Left = 176
      Top = 24
      Width = 106
      Height = 17
      Caption = 'MultiLight Shader'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = MultiLightShaderCheckBoxClick
    end
    object UseSpecularTextureCheckBox: TCheckBox
      Left = 320
      Top = 8
      Width = 121
      Height = 17
      Caption = 'Use Specular Texture'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = UseSpecularTextureCheckBoxClick
    end
    object UseNormalTextureCheckBox: TCheckBox
      Left = 320
      Top = 24
      Width = 121
      Height = 17
      Caption = 'Use Normal Texture'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = UseNormalTextureCheckBoxClick
    end
    object ShowNotGLSceneObjectsCheckBox: TCheckBox
      Left = 456
      Top = 8
      Width = 185
      Height = 17
      Caption = 'Show not GLScene objects'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = ShowNotGLSceneObjectsCheckBoxClick
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
      Position.Coordinates = {000096C3000096C3000096430000803F}
      OnProgress = LightCubeProgress
      CubeSize = 1.000000000000000000
      object Light: TGLLightSource
        Ambient.Color = {9A99193E9A99193E9A99193E0000803F}
        ConstAttenuation = 1.000000000000000000
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
      Position.Coordinates = {000048430000C8C2000096430000803F}
      Visible = False
      CubeSize = 1.000000000000000000
      object Light2: TGLLightSource
        Ambient.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {DCD8583FC6BF3F3FDCD8583F0000803F}
        Shining = False
        Specular.Color = {0000803F0000003F0000003F0000803F}
        SpotCutOff = 180.000000000000000000
      end
      object GLSphere2: TGLSphere
        Material.FrontProperties.Ambient.Color = {F6F5753FCDCC4C3ECDCC4C3E0000803F}
        Material.FrontProperties.Diffuse.Color = {EAE9693FCDCC4C3FCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {F0EF6F3F00000000000000000000803F}
        Radius = 10.000000000000000000
      end
    end
    object WorldCube: TGLDummyCube
      ObjectsSorting = osRenderBlendedLast
      ShowAxes = True
      CubeSize = 1.000000000000000000
      object Sphere_little: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Earth'
        Position.Coordinates = {0000000000001643000000000000803F}
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
      object Teapot: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Earth'
        Position.Coordinates = {000016C300000000000000000000803F}
        Visible = False
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object Fighter: TGLActor
        Position.Coordinates = {0000164300000000000000000000803F}
        Visible = False
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
        MaterialLibrary = TrinityMatlib
      end
      object GLCube: TGLCube
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Earth'
        Visible = False
        CubeSize = {000096420000964200009642}
      end
      object GLDodecahedron: TGLDodecahedron
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Earth'
        Position.Coordinates = {00001643000016C3000000000000803F}
        Scale.Coordinates = {0000C8420000C8420000C84200000000}
        Visible = False
      end
      object GLSphere: TGLSphere
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Earth'
        Direction.Coordinates = {00000000000080BF0000000000000000}
        Position.Coordinates = {000016C300001643000000000000803F}
        Up.Coordinates = {00000000000000000000803F00000000}
        Visible = False
        Radius = 50.000000000000000000
      end
    end
    object Camera: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 65.000000000000000000
      TargetObject = GLXYZGrid1
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {0000000000004842000048430000803F}
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
  object Timer1: TTimer
    Interval = 650
    OnTimer = Timer1Timer
    Left = 24
    Top = 136
  end
  object MaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Earth'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {E7E6E63EEBEAEA3EEBEAEA3E0000803F}
        Material.FrontProperties.Emission.Color = {F1F0F03DF1F0F03DF1F0F03D0000803F}
        Material.FrontProperties.Specular.Color = {8180003E8180003EE1E0E03D0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Material.MaterialLibrary = MaterialLibrary
      end
      item
        Name = 'EarthNormals'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'EarthHeight'
        Tag = 0
      end
      item
        Name = 'EarthGross'
        Tag = 0
        Material.Texture.Disabled = False
      end>
    Left = 24
    Top = 88
  end
  object MyBumpShader: TGLSLBumpShader
    NormalTextureName = 'EarthNormals'
    SpecularTextureName = 'EarthGross'
    MaterialLibrary = MaterialLibrary
    BumpHeight = 0.500000000000000000
    BumpSmoothness = 300
    SpecularPower = 6.000000000000000000
    SpecularSpread = 1.500000000000000000
    LightPower = 1.000000000000000000
    Left = 56
    Top = 16
  end
  object TrinityMatlib: TGLMaterialLibrary
    Left = 56
    Top = 88
  end
end
