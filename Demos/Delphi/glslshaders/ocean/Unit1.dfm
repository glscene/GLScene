object Form1: TForm1
  Left = 63
  Top = 64
  Caption = 'GLSL Ocean'
  ClientHeight = 365
  ClientWidth = 515
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 515
    Height = 365
    Camera = GLCamera
    Buffer.BackgroundColor = clBlack
    Buffer.FaceCulling = False
    Buffer.Lighting = False
    FieldOfView = 161.330001831054700000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 56
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000000400000A040000000000000803F}
      SpotCutOff = 180.000000000000000000
      object GLSphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Visible = False
        Radius = 0.050000000745058060
      end
    end
    object DOInitialize: TGLDirectOpenGL
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      UseBuildList = False
      OnRender = DOInitializeRender
      Blend = False
    end
    object DOOceanPlane: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      OnRender = DOOceanPlaneRender
      Blend = False
    end
    object GLHeightField1: TGLHeightField
      Material.MaterialLibrary = MatLib
      Material.LibMaterialName = 'water'
      XSamplingScale.Min = -100.000000000000000000
      XSamplingScale.Max = 100.000000000000000000
      XSamplingScale.Step = 2.000000000000000000
      YSamplingScale.Min = -100.000000000000000000
      YSamplingScale.Max = 100.000000000000000000
      YSamplingScale.Step = 2.000000000000000000
      OnGetHeight = GLHeightField1GetHeight
    end
    object GLSphere2: TGLSphere
      Material.MaterialLibrary = MatLib
      Material.LibMaterialName = 'cubeMap'
      Up.Coordinates = {00000000000080BF0000008000000000}
      Radius = 150.000000000000000000
    end
    object GLCamera: TGLCamera
      DepthOfView = 200.000000000000000000
      FocalLength = 30.000000000000000000
      SceneScale = 2.000000000000000000
      TargetObject = GLHeightField1
      Position.Coordinates = {0000C8C10000A0C1000040400000803F}
      Direction.Coordinates = {0000803F000000000000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object MatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'water'
        Tag = 0
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureFormat = tfRGB
        Material.Texture.Disabled = False
        Material.Texture.NormalMapScale = 0.050000000745058060
        Shader = GLUserShader1
      end
      item
        Name = 'cubeMap'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLCubeMapImage'
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twNone
        Material.Texture.MappingMode = tmmCubeMapNormal
        Material.Texture.Disabled = False
      end>
    Left = 56
    Top = 72
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 200
    Top = 16
  end
  object GLUserShader1: TGLUserShader
    OnDoApply = GLUserShader1DoApply
    OnDoUnApply = GLUserShader1DoUnApply
    Left = 128
    Top = 72
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Camera = CameraCubeMap
    Width = 128
    Height = 128
    BeforeRender = GLMemoryViewer1BeforeRender
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roDestinationAlpha]
    Left = 296
    Top = 16
  end
  object GLScene2: TGLScene
    Left = 128
    Top = 16
    object GLEarthSkyDome1: TGLEarthSkyDome
      Direction.Coordinates = {FEFF7F27000080A7FFFF7F3F00000000}
      RollAngle = -45.000000000000000000
      Up.Coordinates = {F30435BFF304353FD413D42600000000}
      Bands = <>
      Stars = <>
      SunElevation = 25.000000000000000000
      Turbidity = 75.000000000000000000
      SunDawnColor.Color = {0000803F0000803F0000403F00000000}
      ExtendedOptions = []
      Slices = 48
      Stacks = 24
    end
    object CameraCubeMap: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Direction.Coordinates = {00000000000000800000803F00000000}
    end
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'GLSL Ocean - %FPS'
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
    Left = 296
    Top = 72
  end
end
