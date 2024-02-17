object Form1: TForm1
  Left = 192
  Top = 107
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Bunny Bump Shader'
  ClientHeight = 765
  ClientWidth = 998
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 168
  TextHeight = 24
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 100
    Width = 998
    Height = 665
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = Camera
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.BackgroundColor = clBackground
    FieldOfView = 162.896347045898400000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 998
    Height = 100
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 14
      Top = 14
      Width = 124
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Shade Method'
    end
    object Label2: TLabel
      Left = 644
      Top = 14
      Width = 129
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Specular Mode'
    end
    object LabelFPS: TLabel
      Left = 900
      Top = 46
      Width = 36
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'FPS'
    end
    object ComboBox1: TComboBox
      Left = 14
      Top = 42
      Width = 254
      Height = 32
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Per-Vertex'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Per-Vertex')
    end
    object GroupBox1: TGroupBox
      Left = 282
      Top = 18
      Width = 296
      Height = 71
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Lights'
      TabOrder = 1
      object ShapeWhite: TShape
        Left = 56
        Top = 28
        Width = 30
        Height = 30
        Hint = 'White'
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Pen.Width = 2
        OnMouseDown = ShapeMouseDown
      end
      object ShapeRed: TShape
        Left = 154
        Top = 28
        Width = 30
        Height = 30
        Hint = 'Red'
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Brush.Color = clRed
        Pen.Width = 2
        OnMouseDown = ShapeMouseDown
      end
      object ShapeBlue: TShape
        Left = 252
        Top = 28
        Width = 30
        Height = 30
        Hint = 'Blue'
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Brush.Color = clBlue
        Pen.Width = 2
        OnMouseDown = ShapeMouseDown
      end
      object cbWhite: TCheckBox
        Left = 16
        Top = 28
        Width = 30
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CheckBoxClick
      end
      object cbRed: TCheckBox
        Left = 114
        Top = 28
        Width = 30
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 1
        OnClick = CheckBoxClick
      end
      object cbBlue: TCheckBox
        Left = 212
        Top = 28
        Width = 30
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 2
        OnClick = CheckBoxClick
      end
    end
    object CheckBox4: TCheckBox
      Left = 802
      Top = 42
      Width = 85
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Spin'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object ComboBox2: TComboBox
      Left = 649
      Top = 42
      Width = 128
      Height = 32
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'smOff'
      OnChange = ComboBox2Change
      Items.Strings = (
        'smOff'
        'smBlinn'
        'smPhong')
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 72
    object DCLights: TGLDummyCube
      CubeSize = 1.000000000000000000
      object WhiteLight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000404000000040000000000000803F}
        LightStyle = lsOmni
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
      object RedLight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {0000803F00000000000000000000803F}
        Position.Coordinates = {0000C0BF00000040666626C00000803F}
        LightStyle = lsOmni
        Shining = False
        Specular.Color = {0000803F0000003F0000003F0000803F}
        SpotCutOff = 180.000000000000000000
      end
      object BlueLight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {00000000000000000000803F0000803F}
        Position.Coordinates = {0000C0BF00000040666626400000803F}
        LightStyle = lsOmni
        Shining = False
        Specular.Color = {0000003F0000003F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object ffBunny: TGLFreeForm
      Material.FrontProperties.Shininess = 64
      Material.FrontProperties.Specular.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
      Material.MaterialLibrary = MatLib
      AutoCentering = [macCenterX, macCenterY, macCenterZ]
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = ffBunny
      Position.Coordinates = {000080400000803F000000000000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 136
  end
  object MatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Bump'
        Tag = 0
        Material.FrontProperties.Shininess = 64
        Material.FrontProperties.Specular.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
        Material.Texture.Disabled = False
        Shader = GLBumpShader1
      end>
    Left = 136
    Top = 72
  end
  object GLBumpShader1: TGLBumpShader
    BumpMethod = bmDot3TexCombiner
    BumpSpace = bsObject
    BumpOptions = []
    SpecularMode = smOff
    DesignTimeEnabled = False
    ParallaxOffset = 0.039999999105930330
    Left = 136
    Top = 136
  end
  object ColorDialog1: TColorDialog
    Left = 352
    Top = 72
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 248
    Top = 72
  end
end
