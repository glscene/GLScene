object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Intensity Mesh'
  ClientHeight = 433
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 554
    Height = 433
    Camera = GLCamera
    Buffer.BackgroundColor = clWhite
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roStereo]
    Buffer.FaceCulling = False
    Buffer.Lighting = False
    FieldOfView = 153.991439819335900000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 554
    Top = 0
    Width = 87
    Height = 433
    Align = alRight
    TabOrder = 1
    DesignSize = (
      87
      433)
    object Label1: TLabel
      Left = 8
      Top = 64
      Width = 62
      Height = 13
      Caption = 'Palette Scale'
    end
    object CBWireFrame: TCheckBox
      Left = 8
      Top = 32
      Width = 73
      Height = 17
      Caption = 'Wireframe'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CBSmooth: TCheckBox
      Left = 8
      Top = 8
      Width = 57
      Height = 17
      Caption = 'Smooth'
      TabOrder = 1
    end
    object TBScale: TTrackBar
      Left = 26
      Top = 88
      Width = 31
      Height = 333
      Anchors = [akLeft, akTop, akBottom]
      Max = 200
      Orientation = trVertical
      PageSize = 10
      Frequency = 10
      Position = 50
      TabOrder = 2
      ThumbLength = 15
      OnChange = TBScaleChange
    end
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 24
    object GLFreeForm: TGLFreeForm
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'Palette'
      Scale.Coordinates = {A69B443BA69B443BA69B443B00000000}
      AutoCentering = [macCenterX, macCenterY, macUseBarycenter]
    end
    object DCTarget: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
      object GLCamera: TGLCamera
        DepthOfView = 500.000000000000000000
        FocalLength = 50.000000000000000000
        SceneScale = 2.000000000000000000
        TargetObject = DCTarget
        Position.Coordinates = {0000A04000002041000020420000803F}
      end
    end
    object HSPalette: TGLHUDSprite
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'Palette'
      Position.Coordinates = {0000964300007041000000000000803F}
      Rotation = 0.000000000000000000
    end
    object HTPaletteLeft: TGLHUDText
      Position.Coordinates = {000002430000E040000000000000803F}
      BitmapFont = GLWindowsBitmapFont
      Text = '0'
      Rotation = 0.000000000000000000
      ModulateColor.Color = {0000000000000000000000000000803F}
    end
    object HTPaletteRight: TGLHUDText
      Position.Coordinates = {0000E6430000E040000000000000803F}
      BitmapFont = GLWindowsBitmapFont
      Text = '100'
      Rotation = 0.000000000000000000
      ModulateColor.Color = {0000000000000000000000000000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Palette'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.BackProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {CFCECE3ECFCECE3EC3C2C23ED9CE773F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.Image.Picture.Data = {
          07544269746D617066000000424D660000000000000036000000280000001000
          0000010000000100180000000000300000000000000000000000000000000000
          0000C200C2FF00FFFC0100FFFF0100FF0100FFFF007FFF0002FBC0C0C0E3E5E5
          E3E5E5E3E5E5E3E5E5E3E5E5E3E5E5E3E5E5}
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureWrap = twSeparate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Shader = GLUserShader
      end>
    Left = 144
    Top = 24
  end
  object GLUserShader: TGLUserShader
    Left = 144
    Top = 80
  end
  object GLWindowsBitmapFont: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    Ranges = <
      item
        StartASCII = '0'
        StopASCII = '9'
        StartGlyphIdx = 0
      end>
    Left = 32
    Top = 80
  end
end
