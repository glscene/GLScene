object Form1: TForm1
  Left = 61
  Top = 69
  Caption = 'Intensity Mesh'
  ClientHeight = 529
  ClientWidth = 786
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 678
    Height = 529
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera
    Buffer.BackgroundColor = clWhite
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roStereo]
    Buffer.FaceCulling = False
    Buffer.Lighting = False
    FieldOfView = 158.590713500976600000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 678
    Top = 0
    Width = 108
    Height = 529
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    TabOrder = 1
    DesignSize = (
      108
      529)
    object Label1: TLabel
      Left = 10
      Top = 80
      Width = 80
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Palette Scale'
    end
    object CBWireFrame: TCheckBox
      Left = 10
      Top = 40
      Width = 91
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Wireframe'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBWireFrameClick
    end
    object CBSmooth: TCheckBox
      Left = 10
      Top = 10
      Width = 71
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Smooth'
      TabOrder = 1
      OnClick = CBSmoothClick
    end
    object TBScale: TTrackBar
      Left = 33
      Top = 110
      Width = 38
      Height = 404
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akBottom]
      Max = 200
      Orientation = trVertical
      PageSize = 10
      Frequency = 10
      Position = 50
      TabOrder = 2
      ThumbLength = 19
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
    OnDoUnApply = GLUserShaderDoUnApply
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
