object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Render To Texture'
  ClientHeight = 635
  ClientWidth = 1006
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 17
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 876
    Height = 635
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clGray
    FieldOfView = 162.101089477539100000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 876
    Top = 0
    Width = 130
    Height = 635
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 10
      Width = 75
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 160
      Width = 71
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'VSync'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object SB: TRadioGroup
      Left = 8
      Top = 189
      Width = 113
      Height = 111
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Texture size'
      ItemIndex = 2
      Items.Strings = (
        '256x256'
        '512x512'
        '2048x2048')
      TabOrder = 1
      OnClick = SBClick
    end
    object RB: TRadioGroup
      Left = 8
      Top = 41
      Width = 113
      Height = 112
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Texture frame'
      ItemIndex = 0
      Items.Strings = (
        '1:1'
        '1:2'
        '1:10')
      TabOrder = 2
      OnClick = RBClick
    end
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 24
    object GLLightSource1: TGLLightSource
      Ambient.Color = {9A99993E9A99993E9A99993E0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000A0400000A0400000A0400000803F}
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLFBORenderer1: TGLFBORenderer
      Active = False
      Width = 2048
      Height = 2048
      ColorTextureName = 'pong'
      MaterialLibrary = GLMaterialLibrary1
      BackgroundColor.Color = {0000803F00000000000000000000803F}
      ClearOptions = [coColorBufferClear, coDepthBufferClear]
      Camera = GLCamera1
      SceneScaleFactor = 512.000000000000000000
      RootObject = GLCube1
      EnabledRenderBuffers = [erbDepth]
      AfterRender = GLFBORenderer1AfterRender
    end
    object GLFBORenderer2: TGLFBORenderer
      Active = False
      Width = 2048
      Height = 2048
      ColorTextureName = 'ping'
      MaterialLibrary = GLMaterialLibrary1
      BackgroundColor.Color = {0000803F00000000000000000000803F}
      ClearOptions = [coColorBufferClear, coDepthBufferClear]
      Camera = GLCamera1
      SceneScaleFactor = 512.000000000000000000
      RootObject = GLCube1
      EnabledRenderBuffers = [erbDepth]
      AfterRender = GLFBORenderer2AfterRender
    end
    object GLCube1: TGLCube
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'ping'
      CubeSize = {000000400000004000000040}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLCube1
      Position.Coordinates = {0000004000000040000000400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 32
    Top = 72
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'ping'
        Tag = 0
        Material.DepthProperties.DepthClamp = True
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
      end
      item
        Name = 'pong'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
      end>
    Left = 160
    Top = 24
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 160
    Top = 80
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'RenderToTexture - %FPS'
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
    Left = 288
    Top = 24
  end
end
