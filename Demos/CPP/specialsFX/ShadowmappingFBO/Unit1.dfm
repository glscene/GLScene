object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Shadow Mapping FBO'
  ClientHeight = 541
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 690
    Height = 541
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.BackgroundColor = clGray
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 159.054977416992200000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 136
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      NearPlaneBias = 0.100000001490116100
      TargetObject = SceneRoot
      Position.Coordinates = {0000000000000040000040400000803F}
    end
    object GLCamera2: TGLCamera
      DepthOfView = 10.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = SceneRoot
      Position.Coordinates = {0000004000000040000000400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
        object GLSphere1: TGLSphere
          Material.FrontProperties.Emission.Color = {FCFB7B3FE8E7673F000000000000803F}
          Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
          Radius = 0.500000000000000000
        end
      end
    end
    object LightFBORenderer: TGLFBORenderer
      Width = 1024
      Height = 1024
      DepthTextureName = 'ShadowMap'
      MaterialLibrary = GLMaterialLibrary1
      ClearOptions = [coDepthBufferClear, coUseBufferBackground]
      Camera = GLCamera2
      SceneScaleFactor = 500.000000000000000000
      RootObject = SceneRoot
      EnabledRenderBuffers = []
      PostGenerateMipmap = False
    end
    object SceneRoot: TGLDummyCube
      CubeSize = 1.000000000000000000
      object PrepareShadowMapping: TGLDirectOpenGL
        UseBuildList = False
        OnRender = PrepareShadowMappingRender
        Blend = False
      end
      object GLPlane1: TGLPlane
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Chekers2'
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {00000000333333BF000000000000803F}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Height = 8.000000000000000000
        Width = 8.000000000000000000
        XTiles = 2
        YTiles = 2
      end
      object GLTorus1: TGLTorus
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Chekers'
        MajorRadius = 0.400000005960464500
        MinorRadius = 0.100000001490116100
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object GLCylinder1: TGLCylinder
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'mask'
        Position.Coordinates = {0000403F000000006666663F0000803F}
        Up.Coordinates = {000000000000803F0000008000000000}
        BottomRadius = 0.100000001490116100
        Height = 2.000000000000000000
        Slices = 20
        Stacks = 1
        TopRadius = 0.100000001490116100
      end
      object GLFreeForm1: TGLFreeForm
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'bark'
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {0000803F000000BF000080BF0000803F}
        Up.Coordinates = {0000002800000000000080BF00000000}
        AutoScaling.Coordinates = {0000003F0000003F0000003F0000803F}
      end
    end
    object GLShadowTextureSprite: TGLHUDSprite
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'ShowShadowMap'
      Position.Coordinates = {0000804200008042000000000000803F}
      Width = 128.000000000000000000
      Height = 128.000000000000000000
      Rotation = 0.000000000000000000
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'ShowShadowMap'
        Tag = 0
        Shader = GLSLShader1
      end
      item
        Name = 'ShadowMap'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureWrap = twSeparate
        Material.Texture.TextureWrapS = twClampToBorder
        Material.Texture.TextureWrapT = twClampToBorder
        Material.Texture.TextureWrapR = twClampToBorder
        Material.Texture.TextureFormat = tfExtended
        Material.Texture.TextureFormatEx = tfDEPTH_COMPONENT24
        Material.Texture.BorderColor.Color = {0000803F000000000000000000000000}
        Material.Texture.TextureCompareMode = tcmCompareRtoTexture
      end
      item
        Name = 'mask'
        Tag = 0
        Material.BlendingMode = bmTransparency
        Material.Texture.TextureWrap = twSeparate
        Material.FaceCulling = fcNoCull
        TextureScale.Coordinates = {00004040000040400000A04000000000}
        Shader = GLSLShader2
      end
      item
        Name = 'Chekers'
        Tag = 0
        Shader = GLSLShader2
      end
      item
        Name = 'Chekers2'
        Tag = 0
        Shader = GLSLShader2
      end
      item
        Name = 'Lightspot'
        Tag = 0
        Material.Texture.TextureWrap = twNone
        Shader = GLSLShader2
      end
      item
        Name = 'bark'
        Tag = 0
      end>
    Left = 24
    Top = 184
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    TimeMultiplier = 2.000000000000000000
    OnProgress = GLCadencer1Progress
    Left = 96
    Top = 136
  end
  object Timer1: TTimer
    Left = 96
    Top = 184
  end
  object GLSLShader1: TGLSLShader
    Enabled = False
    OnApply = GLSLShader1Apply
    OnUnApply = GLSLShader1UnApply
    Left = 24
    Top = 232
  end
  object GLSLShader2: TGLSLShader
    Enabled = False
    OnApply = GLSLShader2Apply
    OnInitialize = GLSLShader2Initialize
    Left = 24
    Top = 280
  end
  object GLNavigation: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Shadow Mapping FBO - %FPS'
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
    Left = 96
    Top = 232
  end
end
