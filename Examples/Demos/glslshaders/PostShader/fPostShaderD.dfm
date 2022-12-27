object PostShaderDemoForm: TPostShaderDemoForm
  Left = 261
  Top = 176
  Caption = 'GLSL Post Shader'
  ClientHeight = 552
  ClientWidth = 785
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 562
    Height = 523
    Camera = Camera
    Buffer.BackgroundColor = clBackground
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aaNone
    Buffer.DepthPrecision = dp24bits
    Buffer.ColorDepth = cd24bits
    FieldOfView = 152.082229614257800000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 510
    ExplicitHeight = 519
  end
  object Panel1: TPanel
    Left = 0
    Top = 523
    Width = 785
    Height = 29
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 507
    ExplicitWidth = 661
    object LightMovingCheckBox: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Light is Moving'
      Checked = True
      Color = clBtnFace
      ParentColor = False
      State = cbChecked
      TabOrder = 0
    end
    object TurnPitchrollCheckBox: TCheckBox
      Left = 112
      Top = 8
      Width = 137
      Height = 17
      Caption = 'Turn-Pitch-Roll Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 562
    Top = 0
    Width = 223
    Height = 523
    Align = alRight
    TabOrder = 2
    ExplicitLeft = 438
    ExplicitHeight = 507
    object Label1: TLabel
      Left = 16
      Top = 8
      Width = 42
      Height = 13
      Caption = 'Shaders:'
    end
    object Label2: TLabel
      Left = 19
      Top = 139
      Width = 48
      Height = 13
      Caption = 'Blur Value'
    end
    object lblBlurValue: TLabel
      Left = 201
      Top = 139
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 19
      Top = 172
      Width = 88
      Height = 13
      Caption = 'Thermal Threshold'
    end
    object lblThermalThreshold: TLabel
      Left = 201
      Top = 172
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 19
      Top = 206
      Width = 80
      Height = 13
      Caption = 'Thermal Intensity'
    end
    object lblThermalIntensity: TLabel
      Left = 201
      Top = 206
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 19
      Top = 274
      Width = 75
      Height = 13
      Caption = 'Night Threshold'
    end
    object lblNight: TLabel
      Left = 201
      Top = 274
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 19
      Top = 308
      Width = 87
      Height = 13
      Caption = 'Night Amplification'
    end
    object lblNightAmplification: TLabel
      Left = 201
      Top = 308
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 19
      Top = 240
      Width = 81
      Height = 13
      Caption = 'Dream Threshold'
    end
    object lblDreamThreshold: TLabel
      Left = 201
      Top = 240
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 19
      Top = 342
      Width = 68
      Height = 13
      Caption = 'Pixelate Width'
    end
    object lblPixelateWidth: TLabel
      Left = 201
      Top = 342
      Width = 6
      Height = 13
      Caption = '1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 19
      Top = 376
      Width = 71
      Height = 13
      Caption = 'Pixelate Height'
    end
    object lblPixelateHeight: TLabel
      Left = 201
      Top = 376
      Width = 6
      Height = 13
      Caption = '1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label10: TLabel
      Left = 19
      Top = 410
      Width = 82
      Height = 13
      Caption = 'Posterize Gamma'
    end
    object lblPosterizeGamma: TLabel
      Left = 201
      Top = 410
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 19
      Top = 444
      Width = 83
      Height = 13
      Caption = 'Posterize NColors'
    end
    object lblPosterizeColors: TLabel
      Left = 201
      Top = 444
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label11: TLabel
      Left = 19
      Top = 478
      Width = 55
      Height = 13
      Caption = 'Frost Rand '
    end
    object lblFrostRand: TLabel
      Left = 201
      Top = 478
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label14: TLabel
      Left = 6
      Top = 557
      Width = 56
      Height = 13
      Caption = 'Frost Factor'
    end
    object lblFrostFactor: TLabel
      Left = 188
      Top = 557
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label13: TLabel
      Left = 6
      Top = 601
      Width = 67
      Height = 13
      Caption = 'Trouble Width'
    end
    object lblTroubleWidth: TLabel
      Left = 188
      Top = 601
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label16: TLabel
      Left = 6
      Top = 640
      Width = 70
      Height = 13
      Caption = 'Trouble Height'
    end
    object lblTroubleHeight: TLabel
      Left = 188
      Top = 640
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label18: TLabel
      Left = 6
      Top = 679
      Width = 60
      Height = 13
      Caption = 'Trouble Freq'
    end
    object lblTroubleFreq: TLabel
      Left = 188
      Top = 679
      Width = 15
      Height = 13
      Caption = '0.0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object ShaderCheckListBox: TCheckListBox
      Left = 8
      Top = 24
      Width = 207
      Height = 98
      ItemHeight = 13
      TabOrder = 0
      OnClick = ShaderCheckListBoxClick
      OnClickCheck = ShaderCheckListBoxClick
    end
    object tbBlurValue: TTrackBar
      Left = 104
      Top = 135
      Width = 97
      Height = 27
      Max = 100
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 1
      TickStyle = tsNone
      OnChange = tbBlurValueChange
    end
    object tbThermalThreshold: TTrackBar
      Left = 104
      Top = 170
      Width = 97
      Height = 26
      Max = 100
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 2
      TickStyle = tsNone
      OnChange = tbThermalThresholdChange
    end
    object tbThermalIntensity: TTrackBar
      Left = 104
      Top = 203
      Width = 97
      Height = 27
      Max = 100
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 3
      TickStyle = tsNone
      OnChange = tbThermalIntensityChange
    end
    object tblNightThreshold: TTrackBar
      Left = 104
      Top = 272
      Width = 97
      Height = 27
      Max = 100
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 4
      TickStyle = tsNone
      OnChange = tblNightThresholdChange
    end
    object tbNightAmplification: TTrackBar
      Left = 104
      Top = 306
      Width = 97
      Height = 28
      Max = 1000
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 5
      TickStyle = tsNone
      OnChange = tbNightAmplificationChange
    end
    object tbDreamThreshold: TTrackBar
      Left = 104
      Top = 238
      Width = 97
      Height = 26
      Max = 200
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 6
      TickStyle = tsNone
      OnChange = tbDreamThresholdChange
    end
    object tbPixelateWidth: TTrackBar
      Left = 104
      Top = 340
      Width = 97
      Height = 27
      Max = 64
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 7
      TickStyle = tsNone
      OnChange = tbPixelateWidthChange
    end
    object tbPixelateHeight: TTrackBar
      Left = 104
      Top = 374
      Width = 97
      Height = 28
      Max = 64
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 8
      TickStyle = tsNone
      OnChange = tbPixelateHeightChange
    end
    object tbPosterizeGamma: TTrackBar
      Left = 104
      Top = 410
      Width = 97
      Height = 26
      Max = 300
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 9
      TickStyle = tsNone
      OnChange = tbPosterizeGammaChange
    end
    object tbPosterizeColors: TTrackBar
      Left = 104
      Top = 443
      Width = 97
      Height = 27
      Max = 255
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 10
      TickStyle = tsNone
      OnChange = tbPosterizeColorsChange
    end
    object tbFrostRand: TTrackBar
      Left = 104
      Top = 478
      Width = 97
      Height = 26
      Max = 500
      Min = 10
      PageSize = 1
      Position = 10
      TabOrder = 11
      TickStyle = tsNone
      OnChange = tbFrostRandChange
    end
    object tbFrostFactor: TTrackBar
      Left = 91
      Top = 557
      Width = 97
      Height = 27
      Max = 250
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 12
      TickStyle = tsNone
      OnChange = tbFrostFactorChange
    end
    object tbTroubleWidth: TTrackBar
      Left = 91
      Top = 601
      Width = 91
      Height = 27
      Max = 64
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 13
      TickStyle = tsNone
      OnChange = tbTroubleWidthChange
    end
    object tbTroubleHeight: TTrackBar
      Left = 91
      Top = 634
      Width = 91
      Height = 27
      Max = 64
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 14
      TickStyle = tsNone
      OnChange = tbTroubleHeightChange
    end
    object tbTroubleFreq: TTrackBar
      Left = 83
      Top = 679
      Width = 99
      Height = 27
      Max = 300
      Min = 1
      PageSize = 1
      Position = 1
      TabOrder = 15
      TickStyle = tsNone
      OnChange = tbTroubleFreqChange
    end
  end
  object Scene: TGLScene
    ObjectsSorting = osNone
    Left = 24
    Top = 16
    object GUICube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLArrowLine1: TGLArrowLine
        Position.Coordinates = {00000000000000000000C8420000803F}
        Scale.Coordinates = {0000A0410000A0410000A04100000000}
        BottomRadius = 0.100000001490116100
        Height = 1.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLXYZGrid1: TGLXYZGrid
        Direction.Coordinates = {00000000000080BF0000000000000000}
        Position.Coordinates = {00000000000000000000A0C10000803F}
        Up.Coordinates = {00000000000000000000803F00000000}
        XSamplingScale.Min = -200.000000000000000000
        XSamplingScale.Max = 200.000000000000000000
        XSamplingScale.Step = 20.000000000000000000
        YSamplingScale.Min = -200.000000000000000000
        YSamplingScale.Max = 200.000000000000000000
        YSamplingScale.Step = 20.000000000000000000
        ZSamplingScale.Min = -200.000000000000000000
        ZSamplingScale.Max = 200.000000000000000000
        ZSamplingScale.Step = 20.000000000000000000
        Parts = [gpX, gpZ]
      end
    end
    object LightCube: TGLDummyCube
      Direction.Coordinates = {0000000000000000000080BF00000000}
      Position.Coordinates = {000096C300000000000096430000803F}
      OnProgress = LightCubeProgress
      CubeSize = 1.000000000000000000
      object Light: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
      object GLSphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {E6E5653F8180003DCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {F4F3733FEEED6D3F000000000000803F}
        Radius = 10.000000000000000000
      end
    end
    object WorldCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Fighter: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Fighter'
        Position.Coordinates = {00001643000000000000A0410000803F}
        Up.Coordinates = {00000000000080BF0000008000000000}
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object Teapot: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Fighter'
        Position.Coordinates = {000016C300000000000000000000803F}
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object Sphere_big: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Earth'
        Position.Coordinates = {00000000000016C3000000000000803F}
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object Sphere_little: TGLActor
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'Earth'
        Position.Coordinates = {0000000000001643000000000000803F}
        Interval = 100
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
    end
    object PostShaderHolder: TGLPostShaderHolder
      TempTextureTarget = ttTextureRect
      Shaders = <>
    end
    object Camera: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 65.000000000000000000
      TargetObject = GLXYZGrid1
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {000048C20000164300007A430000803F}
      Direction.Coordinates = {00000000000080BF0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object Cadencer: TGLCadencer
    Scene = Scene
    MaxDeltaTime = 0.020000000000000000
    OnProgress = CadencerProgress
    Left = 96
    Top = 16
  end
  object MaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Noise'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'Fighter'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {BFBEBE3EBBBABA3EBBBABA3E0000803F}
        Material.FrontProperties.Emission.Color = {B1B0B03DB1B0B03DB1B0B03D0000803F}
        Material.FrontProperties.Specular.Color = {8988083E8180003E8988083E0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Earth'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {E7E6E63EEBEAEA3EEBEAEA3E0000803F}
        Material.FrontProperties.Emission.Color = {F1F0F03DF1F0F03DF1F0F03D0000803F}
        Material.FrontProperties.Specular.Color = {8180003E8180003EE1E0E03D0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Snow'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'FireGrade'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'Mask'
        Tag = 0
        Material.Texture.Disabled = False
        Material.Texture.DepthTextureMode = dtmIntensity
      end>
    Left = 40
    Top = 80
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    FormCaption = 'GLSL Post Shader  - %FPS'
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
    Left = 192
    Top = 16
  end
end
