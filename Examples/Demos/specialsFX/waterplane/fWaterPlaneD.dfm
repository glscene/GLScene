object FormWaterPlane: TFormWaterPlane
  Left = 169
  Top = 106
  Margins.Left = 2
  Margins.Top = 2
  Margins.Right = 2
  Margins.Bottom = 2
  Caption = 'Water Plane'
  ClientHeight = 507
  ClientWidth = 683
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 683
    Height = 507
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.FaceCulling = False
    FieldOfView = 166.501647949218800000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    TabOrder = 0
    ExplicitWidth = 555
    ExplicitHeight = 405
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLSphere1: TGLSphere
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'CubeMap'
      Radius = 50.000000000000000000
      Slices = 9
      Stacks = 9
    end
    object DCTarget: TGLDummyCube
      Position.Coordinates = {00000040000000000000803F0000803F}
      CubeSize = 1.000000000000000000
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLHeightField1: TGLHeightField
      Material.Texture.MappingMode = tmmObjectLinear
      Material.Texture.MappingSCoordinates.Coordinates = {CDCC4C3D000000000000000000000000}
      Material.Texture.MappingTCoordinates.Coordinates = {00000000CDCC4C3D0000000000000000}
      Material.Texture.Disabled = False
      Direction.Coordinates = {00000000FFFF7F3F0100003300000000}
      Position.Coordinates = {00000000000080BF000000000000803F}
      Scale.Coordinates = {1F85EB3D1F85EB3D0000003F00000000}
      Up.Coordinates = {2FBD3B3302000033000080BF00000000}
      XSamplingScale.Min = -63.000000000000000000
      XSamplingScale.Max = 63.000000000000000000
      XSamplingScale.Step = 2.000000000000000000
      YSamplingScale.Min = -63.000000000000000000
      YSamplingScale.Max = 63.000000000000000000
      YSamplingScale.Step = 2.000000000000000000
      OnGetHeight = GLHeightField1GetHeight
    end
    object GLWaterPlane1: TGLWaterPlane
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'CubeMap'
      Position.Coordinates = {00000000000080BF000000000000803F}
      Scale.Coordinates = {000070410000803F0000704100000000}
      RainForce = 5000.000000000000000000
      Viscosity = 0.990000009536743200
      Elastic = 10.000000000000000000
      Resolution = 128
      Options = []
      SimulationFrequency = 100.000000000000000000
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000E04000007041000040400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 30.000000000000000000
      TargetObject = DCTarget
      Position.Coordinates = {0000E04000008040000040400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 80
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'CubeMap'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000003F}
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.MappingMode = tmmCubeMapReflection
        Material.Texture.Disabled = False
        Shader = GLUserShader1
      end>
    Left = 368
    Top = 16
  end
  object GLUserShader1: TGLUserShader
    OnDoApply = GLUserShader1DoApply
    OnDoUnApply = GLUserShader1DoUnApply
    Left = 272
    Top = 16
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Water Plane - %FPS'
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
    Left = 176
    Top = 16
  end
end
