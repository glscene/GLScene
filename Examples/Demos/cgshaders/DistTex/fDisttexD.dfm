object Form1: TForm1
  Left = 375
  Top = 237
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Cg Texture Distortion'
  ClientHeight = 1073
  ClientWidth = 1678
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 24
  object viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1678
    Height = 1073
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 169.351211547851600000
    PenAsTouch = False
    Align = alClient
    OnMouseMove = viewerMouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 56
    Top = 24
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {23DB793F39B4483FF0A7463F0000803F}
      Position.Coordinates = {000000C00000A0400000A0400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object backGround: TGLPlane
      Material.MaterialLibrary = matLib
      Material.LibMaterialName = 'backGroundFilter'
      Position.Coordinates = {00000000000000000000A0C00000803F}
      Scale.Coordinates = {000040413333E3400000803F00000000}
      Height = 1.000000000000000000
      Width = 1.000000000000000000
    end
    object GLCube1: TGLCube
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLPlane1: TGLPlane
      Material.MaterialLibrary = matLib
      Material.LibMaterialName = 'filter'
      Position.Coordinates = {0000000000000000CDCC8C3F0000803F}
      Scale.Coordinates = {48E12A400AD703400000004000000000}
      Height = 1.000000000000000000
      Width = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLCube1
      Position.Coordinates = {0000000000000000000000400000803F}
    end
  end
  object matLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'filter'
        Tag = 0
        Shader = filterShader
      end
      item
        Name = 'backGroundFilter'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {000020400000C03F0000803F00000000}
      end>
    Left = 192
    Top = 24
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 20
    OnProgress = GLCadencer1Progress
    Left = 192
    Top = 104
  end
  object filterShader: TCgShader
    VertexProgram.OnApply = filterShaderApplyVP
    FragmentProgram.OnApply = filterShaderApplyFP
    OnApplyVP = filterShaderApplyVP
    OnApplyFP = filterShaderApplyFP
    Left = 56
    Top = 112
  end
end
