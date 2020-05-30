object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sound Around'
  ClientHeight = 370
  ClientWidth = 514
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
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 73
    Width = 514
    Height = 270
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.AntiAliasing = aaNone
    FieldOfView = 139.353729248046900000
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
    TabOrder = 0
  end
  object TrackBar: TTrackBar
    Left = 0
    Top = 343
    Width = 514
    Height = 27
    Align = alBottom
    Max = 180
    Min = -180
    PageSize = 45
    Frequency = 45
    TabOrder = 1
    ThumbLength = 10
    TickMarks = tmBoth
    OnChange = TrackBarChange
  end
  object TrackBar1: TTrackBar
    Left = 0
    Top = 41
    Width = 514
    Height = 32
    Align = alTop
    Max = 50
    Min = -50
    PageSize = 45
    Frequency = 10
    TabOrder = 2
    ThumbLength = 10
    TickMarks = tmBoth
    OnChange = TrackBar1Change
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 514
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      514
      41)
    object Label1: TLabel
      Left = 10
      Top = 8
      Width = 82
      Height = 13
      Caption = 'Sound Manager :'
    end
    object LabelFPS: TLabel
      Left = 10
      Top = 27
      Width = 18
      Height = 13
      Caption = 'FPS'
    end
    object RBBass: TRadioButton
      Left = 98
      Top = 6
      Width = 57
      Height = 17
      Caption = 'BASS'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RBFMODClick
    end
    object RBFMOD: TRadioButton
      Left = 161
      Top = 6
      Width = 57
      Height = 17
      Caption = 'FMOD'
      TabOrder = 1
      OnClick = RBFMODClick
    end
    object Button1: TButton
      Left = 423
      Top = 2
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Chimes (WAV)'
      TabOrder = 2
      OnClick = Button1Click
    end
    object btnHowl: TButton
      Left = 334
      Top = 2
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Howl (MP3)'
      TabOrder = 3
      OnClick = btnHowlClick
    end
    object RBOpenAL: TRadioButton
      Left = 224
      Top = 6
      Width = 65
      Height = 17
      Caption = 'OpenAL'
      TabOrder = 4
      OnClick = RBFMODClick
    end
  end
  object GLSMFMOD: TGLSMFMOD
    MasterVolume = 1.000000000000000000
    Listener = Mickey
    Sources = <>
    Cadencer = GLCadencer1
    Left = 352
    Top = 80
  end
  object GLSMBASS: TGLSMBASS
    Active = True
    MaxChannels = 32
    MasterVolume = 1.000000000000000000
    Listener = Mickey
    Sources = <>
    Cadencer = GLCadencer1
    Left = 288
    Top = 80
  end
  object GLSMOpenAL: TGLSMOpenAL
    MaxChannels = 32
    MasterVolume = 1.000000000000000000
    Listener = Mickey
    Sources = <>
    Cadencer = GLCadencer1
    Environment = seAuditorium
    Left = 416
    Top = 80
  end
  object GLSoundLibrary: TGLSoundLibrary
    Samples = <>
    Left = 216
    Top = 80
  end
  object GLScene: TGLScene
    Left = 16
    Top = 80
    object DummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Torus1: TGLTorus
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Emission.Color = {000000008180803E8180003F0000803F}
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Position.Coordinates = {00000000000000BF000000000000803F}
        Scale.Coordinates = {0000803F0000803F0000003F00000000}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        MajorRadius = 5.000000000000000000
        MinorRadius = 0.100000001490116100
        Rings = 16
        Sides = 3
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object Mickey: TGLSphere
        Position.Coordinates = {000000000000003F000000000000803F}
        Radius = 0.500000000000000000
        Slices = 9
        Stacks = 9
        object Sphere2: TGLSphere
          Position.Coordinates = {CDCCCC3ECDCC4C3E000000000000803F}
          Radius = 0.300000011920929000
          Slices = 6
          Stacks = 6
        end
        object Sphere3: TGLSphere
          Position.Coordinates = {CDCCCCBECDCC4C3E000000000000803F}
          Radius = 0.300000011920929000
          Slices = 6
          Stacks = 6
        end
        object Cone1: TGLCone
          Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
          Position.Coordinates = {00000000000000000000003F0000803F}
          Up.Coordinates = {00000000000000000000803F00000000}
          BottomRadius = 0.300000011920929000
          Height = 0.500000000000000000
          Slices = 8
          Stacks = 2
          Parts = [coSides]
        end
      end
      object Plane1: TGLPlane
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Position.Coordinates = {00000000000000BF000000000000803F}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        Height = 10.000000000000000000
        Width = 0.500000000000000000
      end
    end
    object Sphere: TGLSphere
      Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      Material.FrontProperties.Emission.Color = {8180003F00000000000000000000803F}
      Position.Coordinates = {000000400000003F000000000000803F}
      OnProgress = SphereProgress
      Radius = 0.500000000000000000
      Slices = 9
      Stacks = 9
      BehavioursData = {
        0458434F4C02010201061054474C42536F756E64456D69747465720200120000
        00000200020002000200050000000000000080FF3F050000000000000080FF3F
        0500000000000000C805400500000000000000B407400500000000000000B407
        400500000000000000000000060E474C536F756E644C696272617279060C6472
        756D6C6F6F702E7761760808020309}
      object Disk1: TGLDisk
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Position.Coordinates = {00000000000080BF000000000000803F}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        OuterRadius = 0.500000000000000000
        Slices = 12
        SweepAngle = 360.000000000000000000
      end
    end
    object GLLightSource: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Plane1
      Position.Coordinates = {000000400000A040000020410000803F}
      Left = 256
      Top = 160
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene
    SleepLength = 1
    Left = 80
    Top = 80
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 144
    Top = 80
  end
end
