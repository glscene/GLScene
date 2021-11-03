object FormThor: TFormThor
  Left = 176
  Top = 84
  BorderWidth = 3
  Caption = 'Thor'
  ClientHeight = 591
  ClientWidth = 743
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 743
    Height = 477
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    FieldOfView = 156.319564819335900000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 707
    ExplicitHeight = 459
  end
  object Panel1: TPanel
    Left = 0
    Top = 477
    Width = 743
    Height = 114
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 444
    ExplicitWidth = 683
    object Label1: TLabel
      Left = 419
      Top = 73
      Width = 81
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Camera Distance'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 430
      Top = 35
      Width = 54
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Glow Alpha'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 540
      Top = 35
      Width = 41
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Vibration'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 550
      Top = 0
      Width = 43
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Wildness'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 430
      Top = 0
      Width = 47
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Glow Size'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object DistanceBar: TTrackBar
      Left = 390
      Top = 84
      Width = 144
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 20
      Min = 1
      Position = 5
      SelEnd = 20
      SelStart = 1
      TabOrder = 0
      TabStop = False
      ThumbLength = 13
      OnChange = DistanceBarChange
    end
    object GSbar: TTrackBar
      Left = 390
      Top = 13
      Width = 144
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 50
      Position = 10
      SelEnd = 20
      TabOrder = 1
      TabStop = False
      ThumbLength = 13
      OnChange = GSbarChange
    end
    object GAbar: TTrackBar
      Left = 390
      Top = 48
      Width = 144
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 50
      Position = 15
      SelEnd = 50
      TabOrder = 2
      TabStop = False
      ThumbLength = 13
      OnChange = GAbarChange
    end
    object WildBar: TTrackBar
      Left = 530
      Top = 13
      Width = 144
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 20
      Position = 4
      SelEnd = 20
      TabOrder = 3
      TabStop = False
      ThumbLength = 13
      OnChange = WildBarChange
    end
    object VibBar: TTrackBar
      Left = 530
      Top = 48
      Width = 144
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 20
      SelEnd = 20
      TabOrder = 4
      TabStop = False
      ThumbLength = 13
      OnChange = VibBarChange
    end
    object SpinBox: TCheckBox
      Left = 545
      Top = 91
      Width = 126
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'User Formula(Spin)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object CoreBox: TCheckBox
      Left = 545
      Top = 73
      Width = 61
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Core'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 6
      OnClick = CoreBoxClick
    end
    object Memo1: TMemo
      Left = 3
      Top = 3
      Width = 358
      Height = 108
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Small Fonts'
      Font.Style = []
      Lines.Strings = (
        '              ----------GLThorFX----------'
        
          'This special-effect is mostly for creating lightning-effects.How' +
          'ever, it is '
        
          'very flexible. You can write your own formulas for the movement ' +
          'of the '
        'particles, by using the OnCalcPoint event in the ThorManager.'
        
          'If you turn the Core off, and set Wildness and Vibration to 0, y' +
          'ou get a '
        
          'lightbeam.    If you set the Glow-size small, the Alpha, and Vib' +
          'ration High, '
        'and Core off, you get a sparkling effect.'
        ''
        '')
      ParentFont = False
      TabOrder = 7
    end
    object PauseBox: TCheckBox
      Left = 610
      Top = 73
      Width = 61
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Pause'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      OnClick = PauseBoxClick
    end
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 24
    Top = 8
    object SkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000803F000000800000000000000000}
      Bands = <
        item
          StartAngle = -90.000000000000000000
          StartColor.Color = {0000000000000000000000000000803F}
          StopAngle = -30.000000000000000000
          StopColor.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
          Slices = 9
        end
        item
          StartAngle = -30.000000000000000000
          StartColor.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
          StopAngle = -15.000000000000000000
          StopColor.Color = {9A99993E9A99993E9A99993E0000803F}
          Slices = 9
        end
        item
          StartAngle = -15.000000000000000000
          StartColor.Color = {9A99993E9A99993E9A99993E0000803F}
          StopColor.Color = {B5B4343EE4E4E43EBEBD3D3F0000803F}
          Slices = 9
        end
        item
          StartColor.Color = {B5B4343EE5E4E43EBEBD3D3F0000803F}
          StopAngle = 15.000000000000000000
          StopColor.Color = {B6B5353FD1D0503FEFEE6E3F0000803F}
          Slices = 9
        end
        item
          StartAngle = 15.000000000000000000
          StartColor.Color = {8FC2353F60E5503F17D96E3F0000803F}
          StopAngle = 90.000000000000000000
          StopColor.Color = {B2B1313FC8C7473FF2F1713F0000803F}
          Slices = 9
          Stacks = 3
        end>
      Stars = <>
    end
    object HeightField1: TGLHeightField
      Material.Texture.MinFilter = miNearestMipmapNearest
      Material.Texture.TextureMode = tmModulate
      Material.Texture.Disabled = False
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {00000000000040C0000000000000803F}
      Scale.Coordinates = {00002041000020410000803F00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      XSamplingScale.Min = -1.000000000000000000
      XSamplingScale.Max = 1.000000000000000000
      XSamplingScale.Step = 0.200000002980232200
      YSamplingScale.Min = -1.000000000000000000
      YSamplingScale.Max = 1.000000000000000000
      YSamplingScale.Step = 0.200000002980232200
      Options = [hfoTextureCoordinates]
      OnGetHeight = HeightField1GetHeight
    end
    object Objects: TGLDummyCube
      ObjectsSorting = osRenderBlendedLast
      CubeSize = 1.000000000000000000
      object TargetCube: TGLCube
        Material.FrontProperties.Diffuse.Color = {8180803E8180803E0000803F0000803F}
        Material.FrontProperties.Emission.Color = {00000000000000008180003E0000803F}
        Position.Coordinates = {0000404000000000000000000000803F}
      end
      object ThorCube: TGLCube
        Material.FrontProperties.Diffuse.Color = {8180803E8180803E0000803F0000803F}
        Material.FrontProperties.Emission.Color = {00000000000000008180003E0000803F}
        Position.Coordinates = {000040C000000000000000000000803F}
        EffectsData = {
          0458434F4C02010201060A54474C4254686F7246580201020012000000000200
          02000610474C54686F7246584D616E6167657231}
      end
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
        SpotDirection.Coordinates = {00000000000080BF0000000000000000}
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Objects
      Position.Coordinates = {00004040000000400000A0400000803F}
      Left = 152
      Top = 104
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 24
    Top = 72
  end
  object GLThorFXManager1: TGLThorFXManager
    Target.Coordinates = {0000C040000000000000000000000000}
    Cadencer = GLCadencer1
    Maxpoints = 64
    GlowSize = 0.200000002980232200
    InnerColor.Color = {0000803F0000803F0000803F9A99993E}
    OuterColor.Color = {00000000000000000000803F00000000}
    Disabled = False
    Core = True
    Glow = True
    Wildness = 1.000000000000000000
    OnCalcPoint = GLThorFXManager1CalcPoint
    Left = 128
    Top = 8
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Thor - %FPS'
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
    Left = 128
    Top = 72
  end
end
