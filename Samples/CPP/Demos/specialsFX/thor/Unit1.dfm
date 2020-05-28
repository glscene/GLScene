object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Thor'
  ClientHeight = 500
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 554
    Height = 409
    Camera = GLCamera1
    FieldOfView = 152.521591186523400000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 409
    Width = 554
    Height = 91
    Align = alBottom
    TabOrder = 1
    object Label1: TLabel
      Left = 335
      Top = 58
      Width = 72
      Height = 11
      Caption = 'Camera Distance'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 344
      Top = 28
      Width = 47
      Height = 11
      Caption = 'Glow Alpha'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 432
      Top = 28
      Width = 37
      Height = 11
      Caption = 'Vibration'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 440
      Top = 0
      Width = 37
      Height = 11
      Caption = 'Wildness'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 344
      Top = 0
      Width = 40
      Height = 11
      Caption = 'Glow Size'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object DistanceBar: TTrackBar
      Left = 312
      Top = 67
      Width = 115
      Height = 16
      Max = 20
      Min = 1
      Position = 5
      SelEnd = 20
      SelStart = 1
      TabOrder = 0
      TabStop = False
      ThumbLength = 10
      OnChange = DistanceBarChange
    end
    object GSbar: TTrackBar
      Left = 312
      Top = 10
      Width = 115
      Height = 16
      Max = 50
      Position = 10
      SelEnd = 20
      TabOrder = 1
      TabStop = False
      ThumbLength = 10
      OnChange = GSbarChange
    end
    object GAbar: TTrackBar
      Left = 312
      Top = 38
      Width = 115
      Height = 16
      Max = 50
      Position = 15
      SelEnd = 50
      TabOrder = 2
      TabStop = False
      ThumbLength = 10
      OnChange = GAbarChange
    end
    object WildBar: TTrackBar
      Left = 424
      Top = 10
      Width = 115
      Height = 16
      Max = 20
      Position = 4
      SelEnd = 20
      TabOrder = 3
      TabStop = False
      ThumbLength = 10
      OnChange = WildBarChange
    end
    object VibBar: TTrackBar
      Left = 424
      Top = 38
      Width = 115
      Height = 16
      Max = 20
      SelEnd = 20
      TabOrder = 4
      TabStop = False
      ThumbLength = 10
      OnChange = VibBarChange
    end
    object SpinBox: TCheckBox
      Left = 436
      Top = 73
      Width = 101
      Height = 17
      Caption = 'User Formula(Spin)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object CoreBox: TCheckBox
      Left = 436
      Top = 58
      Width = 49
      Height = 17
      Caption = 'Core'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 6
      OnClick = CoreBoxClick
    end
    object Memo1: TMemo
      Left = 2
      Top = 2
      Width = 311
      Height = 87
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
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
      Left = 488
      Top = 58
      Width = 49
      Height = 17
      Caption = 'Pause'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
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
      Material.Texture.ImageClassName = 'TGLPicFileImage'
      Material.Texture.Image.PictureFileName = '..\\..\\..\\..\\media\\marbletiles.jpg'
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
      object ThorCube: TGLCube
        Material.FrontProperties.Diffuse.Color = {8180803E8180803E0000803F0000803F}
        Material.FrontProperties.Emission.Color = {00000000000000008180003E0000803F}
        Position.Coordinates = {000040C000000000000000000000803F}
        EffectsData = {
          0458434F4C02010201060A54474C4254686F7246580201020012000000000200
          02000610474C54686F7246584D616E6167657231}
      end
      object TargetCube: TGLCube
        Material.FrontProperties.Diffuse.Color = {8180803E8180803E0000803F0000803F}
        Material.FrontProperties.Emission.Color = {00000000000000008180003E0000803F}
        Position.Coordinates = {0000404000000000000000000000803F}
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
