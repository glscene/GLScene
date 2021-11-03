object FormSkeletal: TFormSkeletal
  Left = 237
  Top = 105
  Caption = 'Skeletal Animation'
  ClientHeight = 501
  ClientWidth = 644
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 510
    Height = 450
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    Buffer.Lighting = False
    FieldOfView = 154.942382812500000000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 510
    Top = 0
    Width = 134
    Height = 450
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    BevelOuter = bvNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    DesignSize = (
      134
      450)
    object LabelFPS: TLabel
      Left = 40
      Top = 30
      Width = 32
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taCenter
      Anchors = []
      Caption = 'FPS'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object BULongJump: TButton
      Left = 20
      Top = 190
      Width = 101
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Long Jump'
      TabOrder = 0
      OnClick = BULongJumpClick
    end
    object CheckBox1: TCheckBox
      Left = 20
      Top = 300
      Width = 101
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Skeleton'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object BUHighJump: TButton
      Left = 20
      Top = 240
      Width = 101
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'High Jump'
      TabOrder = 2
      OnClick = BUHighJumpClick
    end
    object RBWalk: TRadioButton
      Left = 20
      Top = 110
      Width = 81
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Walk'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = RBWalkClick
    end
    object RBRun: TRadioButton
      Left = 20
      Top = 140
      Width = 91
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Run'
      TabOrder = 4
      OnClick = RBRunClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 450
    Width = 644
    Height = 51
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      644
      51)
    object TrackBar1: TTrackBar
      Left = 80
      Top = 0
      Width = 564
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight, akBottom]
      Max = 100
      Position = 50
      TabOrder = 0
      ThumbLength = 25
      TickMarks = tmBoth
      OnChange = TrackBar1Change
    end
    object CBBlend: TCheckBox
      Left = 10
      Top = 15
      Width = 61
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
