object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Skeletal Animation'
  ClientHeight = 434
  ClientWidth = 561
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 454
    Height = 393
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    Buffer.Lighting = False
    FieldOfView = 151.447769165039100000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 454
    Top = 0
    Width = 107
    Height = 393
    Align = alRight
    BevelOuter = bvNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    DesignSize = (
      107
      393)
    object LabelFPS: TLabel
      Left = 32
      Top = 27
      Width = 26
      Height = 16
      Alignment = taCenter
      Anchors = []
      Caption = 'FPS'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 24
    end
    object BULongJump: TButton
      Left = 16
      Top = 152
      Width = 81
      Height = 25
      Caption = 'Long Jump'
      TabOrder = 0
      OnClick = BULongJumpClick
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 240
      Width = 81
      Height = 17
      Caption = 'Skeleton'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object BUHighJump: TButton
      Left = 16
      Top = 192
      Width = 81
      Height = 25
      Caption = 'High Jump'
      TabOrder = 2
      OnClick = BUHighJumpClick
    end
    object RBWalk: TRadioButton
      Left = 16
      Top = 88
      Width = 65
      Height = 17
      Caption = 'Walk'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = RBWalkClick
    end
    object RBRun: TRadioButton
      Left = 16
      Top = 112
      Width = 73
      Height = 17
      Caption = 'Run'
      TabOrder = 4
      OnClick = RBRunClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 393
    Width = 561
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      561
      41)
    object TrackBar1: TTrackBar
      Left = 64
      Top = 0
      Width = 497
      Height = 34
      Anchors = [akLeft, akTop, akRight, akBottom]
      Max = 100
      Position = 50
      TabOrder = 0
      TickMarks = tmBoth
      OnChange = TrackBar1Change
    end
    object CBBlend: TCheckBox
      Left = 8
      Top = 12
      Width = 49
      Height = 17
      Caption = 'Blend'
      TabOrder = 1
      OnClick = CBBlendClick
    end
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 8
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000C8420000C8420000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Actor1: TGLActor
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {0000003F0000003F0000003F00000000}
      Up.Coordinates = {0000000000000080000080BF00000000}
      AnimationMode = aamLoop
      Interval = 100
      OnEndFrameReached = Actor1EndFrameReached
      MaterialLibrary = GLMaterialLibrary1
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object XYZGrid1: TGLXYZGrid
      Position.Coordinates = {00000000000090C1000000000000803F}
      LineColor.Color = {1283803E1283003F1283003F0000803F}
      XSamplingScale.Min = -12.000000000000000000
      XSamplingScale.Max = 12.000000000000000000
      XSamplingScale.Step = 4.000000000000000000
      YSamplingScale.Step = 1.000000000000000000
      ZSamplingScale.Min = -20.000000000000000000
      ZSamplingScale.Max = 20.000000000000000000
      ZSamplingScale.Step = 4.000000000000000000
      Parts = [gpX, gpZ]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {000048420000A0410000A0400000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {000000000000803F0000008000000000}
      Left = 224
      Top = 160
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 160
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 272
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 32
    Top = 64
  end
  object AnimationControler1: TGLAnimationControler
    Left = 160
    Top = 64
  end
end
