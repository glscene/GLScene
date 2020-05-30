object Form1: TForm1
  Left = 387
  Top = 217
  Caption = 'Cg Bomb Shader'
  ClientHeight = 419
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 209
    Top = 0
    Width = 0
    Height = 419
    Color = clBtnShadow
    ParentColor = False
    ExplicitHeight = 397
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 209
    Height = 419
    Align = alLeft
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object ComboBox1: TComboBox
      Left = 16
      Top = 344
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Fire'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Fire'
        'marbles1'
        'marbles2'
        'snow'
        'FighterTexture')
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 224
      Width = 177
      Height = 109
      Caption = 'Objects'
      TabOrder = 1
      object CheckBox1: TCheckBox
        Left = 8
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Space Fighter'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CheckBox1Click
      end
      object CheckBox2: TCheckBox
        Left = 8
        Top = 40
        Width = 97
        Height = 17
        Caption = 'TeePot'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = CheckBox1Click
      end
      object CheckBox3: TCheckBox
        Left = 8
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Sphere'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = CheckBox1Click
      end
      object CheckBox4: TCheckBox
        Left = 8
        Top = 86
        Width = 97
        Height = 17
        Caption = 'Big Shpere'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = CheckBox1Click
      end
    end
    object ShaderEnabledCheckBox: TCheckBox
      Left = 32
      Top = 368
      Width = 97
      Height = 17
      Caption = 'Shader Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = ShaderEnabledCheckBoxClick
    end
    object TrackBar1: TTrackBar
      Left = 24
      Top = 8
      Width = 150
      Height = 25
      Max = 100
      PageSize = 1
      Frequency = 5
      TabOrder = 3
      ThumbLength = 10
      TickMarks = tmTopLeft
      OnChange = TrackBar1Change
    end
    object TrackBar2: TTrackBar
      Left = 24
      Top = 32
      Width = 150
      Height = 25
      Max = 100
      PageSize = 1
      Frequency = 5
      TabOrder = 4
      ThumbLength = 10
      TickMarks = tmTopLeft
      OnChange = TrackBar2Change
    end
    object TrackBar3: TTrackBar
      Left = 24
      Top = 56
      Width = 150
      Height = 25
      Max = 100
      PageSize = 1
      Frequency = 5
      TabOrder = 5
      ThumbLength = 10
      TickMarks = tmTopLeft
      OnChange = TrackBar3Change
    end
    object TrackBar4: TTrackBar
      Left = 24
      Top = 80
      Width = 150
      Height = 25
      Max = 100
      PageSize = 1
      Frequency = 5
      TabOrder = 6
      ThumbLength = 10
      TickMarks = tmTopLeft
      OnChange = TrackBar4Change
    end
    object TrackBar5: TTrackBar
      Left = 24
      Top = 104
      Width = 150
      Height = 25
      Max = 100
      PageSize = 1
      Frequency = 5
      TabOrder = 7
      ThumbLength = 10
      TickMarks = tmTopLeft
      OnChange = TrackBar5Change
    end
    object TrackBar6: TTrackBar
      Left = 24
      Top = 128
      Width = 150
      Height = 25
      Max = 100
      PageSize = 1
      Frequency = 5
      TabOrder = 8
      ThumbLength = 10
      TickMarks = tmTopLeft
      OnChange = TrackBar6Change
    end
    object TrackBar7: TTrackBar
      Left = 24
      Top = 152
      Width = 150
      Height = 25
      Max = 100
      PageSize = 1
      Frequency = 5
      TabOrder = 9
      ThumbLength = 10
      TickMarks = tmTopLeft
      OnChange = TrackBar7Change
    end
    object TrackBar8: TTrackBar
      Left = 24
      Top = 176
      Width = 150
      Height = 25
      Max = 100
      PageSize = 1
      Frequency = 5
      TabOrder = 10
      ThumbLength = 10
      TickMarks = tmTopLeft
      OnChange = TrackBar8Change
    end
    object TrackBar9: TTrackBar
      Left = 24
      Top = 200
      Width = 150
      Height = 25
      Max = 100
      PageSize = 1
      Frequency = 5
      TabOrder = 11
      ThumbLength = 10
      TickMarks = tmTopLeft
      OnChange = TrackBar9Change
    end
  end
  object Panel9: TPanel
    Left = 209
    Top = 0
    Width = 353
    Height = 419
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object GLSceneViewer1: TGLSceneViewer
      Left = 1
      Top = 1
      Width = 351
      Height = 417
      Camera = GLCamera1
      Buffer.AntiAliasing = aa4x
      FieldOfView = 148.195465087890600000
      Align = alClient
      TabOrder = 0
    end
  end
  object GLScene1: TGLScene
    Left = 256
    Top = 64
    object ffSphere1: TGLFreeForm
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {000000C00000003F000000000000803F}
      Scale.Coordinates = {8FC2F53C8FC2F53C8FC2F53C00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      AutoCentering = [macCenterX, macCenterY]
    end
    object ffSphere2: TGLFreeForm
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {000080BF00000000000000400000803F}
      Scale.Coordinates = {8FC2F53C8FC2F53C8FC2F53C00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      AutoCentering = [macCenterX, macCenterY]
    end
    object ffTeapot: TGLFreeForm
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {0000000000000000000000C00000803F}
      Scale.Coordinates = {8FC2F53C8FC2F53C8FC2F53C00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      AutoCentering = [macCenterX, macCenterY]
    end
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {000000003333333F000000000000803F}
      CubeSize = 1.000000000000000000
    end
    object GLActor1: TGLActor
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {000000409A99993F000000000000803F}
      Up.Coordinates = {00000000000000000000803F00000000}
      Interval = 100
    end
    object GLXYZGrid1: TGLXYZGrid
      XSamplingScale.Min = -3.000000000000000000
      XSamplingScale.Max = 3.000000000000000000
      XSamplingScale.Step = 0.100000001490116100
      YSamplingScale.Step = 0.100000001490116100
      ZSamplingScale.Min = -3.000000000000000000
      ZSamplingScale.Max = 3.000000000000000000
      ZSamplingScale.Step = 0.100000001490116100
      Parts = [gpX, gpZ]
    end
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000000000002041000000000000803F}
      LightStyle = lsOmni
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object JustATestCube: TGLCube
      Position.Coordinates = {0000000000000000000000400000803F}
      Visible = False
      CubeSize = {0000803F0000003F0000003F}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {B0AAAA3FF2EE0E40B0AA2A400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {8988083E00000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {DBDADA3ED5D4543EA1A0A03D0000803F}
        Material.FrontProperties.Specular.Color = {EDEC6C3EDDDC5C3ED5D4543E0000803F}
        Material.BlendingMode = bmTransparency
      end
      item
        Name = 'Fire'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'marbles1'
        Tag = 0
        Material.Texture.MappingMode = tmmEyeLinear
        Material.Texture.Disabled = False
      end
      item
        Name = 'marbles2'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'snow'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'FighterTexture'
        Tag = 0
        Material.FrontProperties.Emission.Color = {E1E0E03DC1C0C03DC1C0C03D0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end>
    Left = 416
    Top = 64
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 328
    Top = 64
  end
  object Timer1: TTimer
    Left = 492
    Top = 64
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'GLCgBombShader - %FPS'
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
    Left = 321
    Top = 128
  end
end
