object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Multisample Textures'
  ClientHeight = 419
  ClientWidth = 569
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
  object MainViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 569
    Height = 419
    Camera = MainCamera
    BeforeRender = MainViewerBeforeRender
    Buffer.FogEnvironment.FogStart = 10.000000000000000000
    Buffer.FogEnvironment.FogEnd = 100.000000000000000000
    Buffer.BackgroundColor = clGray
    FieldOfView = 133.503829956054700000
    Align = alClient
    TabOrder = 0
  end
  object MainScene: TGLScene
    Left = 24
    Top = 8
    object MainCamera: TGLCamera
      DepthOfView = 1024.000000000000000000
      FocalLength = 90.000000000000000000
      NearPlaneBias = 0.001000000047497451
      TargetObject = SceneObjects
      Position.Coordinates = {0000204100002041000020410000803F}
      object TestLight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object FBOContainer: TGLDummyCube
      CubeSize = 1.000000000000000000
      object MultisampleFBO: TGLFBORenderer
        ColorTextureName = 'MultisampledColor'
        DepthTextureName = 'Depth'
        MaterialLibrary = MainMaterialLibrary
        ClearOptions = [coColorBufferClear, coDepthBufferClear, coUseBufferBackground]
        Camera = MainCamera
        RootObject = SceneObjects
        TargetVisibility = tvFBOOnly
        EnabledRenderBuffers = []
      end
    end
    object SceneObjects: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLSphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.PolygonMode = pmLines
        Radius = 4.000000000000000000
      end
      object GLTorus1: TGLTorus
        Material.FrontProperties.Diffuse.Color = {C6BF3F3FDCD8583FDCD8583F0000803F}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {030080A700000000000080BF00000000}
        MajorRadius = 3.000000000000000000
        MinorRadius = 0.600000023841857900
        Sides = 32
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object GLTorus2: TGLTorus
        Material.FrontProperties.Diffuse.Color = {C6BF3F3FDCD8583FDCD8583F0000803F}
        MajorRadius = 3.000000000000000000
        MinorRadius = 0.600000023841857900
        Sides = 32
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object GLCone1: TGLCone
        Material.FrontProperties.Diffuse.Color = {E4DB5B3FEBE0E03EE4DB5B3F0000803F}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {CDCC4C3E00000000CDCC4C3E0000803F}
        Up.Coordinates = {0000000000000000000080BF00000000}
        BottomRadius = 0.500000000000000000
        Height = 5.000000000000000000
      end
      object GLLines1: TGLLines
        AntiAliased = True
        LineColor.Color = {EBE0E03EE4DB5B3F9A93133F0000803F}
        LinePattern = 255
        LineWidth = 5.000000000000000000
        Nodes = <
          item
            X = 6.000000000000000000
          end
          item
            Z = 6.000000000000000000
          end
          item
            X = -6.000000000000000000
          end
          item
            Z = -6.000000000000000000
          end
          item
            X = 6.000000000000000000
          end>
        Options = []
      end
    end
    object GLScreenQuad: TGLHUDSprite
      Material.MaterialLibrary = MainMaterialLibrary
      Material.LibMaterialName = 'Result'
      Rotation = 0.000000000000000000
    end
    object GLHUDText1: TGLHUDText
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
    end
  end
  object MainCadencer: TGLCadencer
    Scene = MainScene
    OnProgress = MainCadencerProgress
    Left = 192
    Top = 8
  end
  object MainMaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'MultisampledColor'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.Disabled = False
      end
      item
        Name = 'Depth'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.TextureFormat = tfExtended
        Material.Texture.TextureFormatEx = tfDEPTH_COMPONENT32
        Material.Texture.Disabled = False
      end
      item
        Name = 'Result'
        Tag = 0
        Shader = GLSLShader1
      end>
    Left = 104
    Top = 8
  end
  object GLSLShader1: TGLSLShader
    Enabled = False
    FragmentProgram.Code.Strings = (
      '#version 150'
      '#define SamplesCount 4'
      ''
      'in      vec4        TexCoord;'
      'out     vec4        color;'
      'uniform sampler2DMS TexUnit0;'
      'uniform vec2        ViewerSize;'
      ''
      'void main()'
      '{'
      #9'vec4 final = vec4(0.0, 0.0, 0.0, 0.0);'
      #9'int i;'
      #9'for (i=0; i<SamplesCount; i++)'
      #9'{'#9
      
        #9#9'vec4 sample = texelFetch(TexUnit0, ivec2(ViewerSize* TexCoord.' +
        'xy), i);'
      #9#9'final  += sample;'
      #9'}'
      #9'color = final / SamplesCount;'
      '}')
    FragmentProgram.Enabled = True
    VertexProgram.Code.Strings = (
      '#version 150'
      ''
      'in  vec4 FTransform;'
      'out vec4 TexCoord;'
      ''
      'void main()'
      '{'
      #9'gl_Position = FTransform;'
      #9'TexCoord = FTransform * 0.5 + 0.5;'
      '}')
    VertexProgram.Enabled = True
    OnApply = GLSLShader1Apply
    Left = 24
    Top = 120
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = MainViewer
    FormCaption = 'Multisample Texture - %FPS'
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
    Left = 24
    Top = 64
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 104
    Top = 64
  end
end
