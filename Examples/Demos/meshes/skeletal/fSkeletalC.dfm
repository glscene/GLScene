object FormSkeletal: TFormSkeletal
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Skeletal Animation'
  ClientHeight = 760
  ClientWidth = 982
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 168
  TextHeight = 23
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 795
    Height = 688
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    Buffer.Lighting = False
    FieldOfView = 163.460083007812500000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 795
    Top = 0
    Width = 187
    Height = 688
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    BevelOuter = bvNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -23
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    DesignSize = (
      187
      688)
    object LabelFPS: TLabel
      Left = 56
      Top = 47
      Width = 44
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      Anchors = []
      Caption = 'FPS'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -23
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object BULongJump: TButton
      Left = 28
      Top = 266
      Width = 142
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Long Jump'
      TabOrder = 0
      OnClick = BULongJumpClick
    end
    object CheckBox1: TCheckBox
      Left = 28
      Top = 420
      Width = 142
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Skeleton'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object BUHighJump: TButton
      Left = 28
      Top = 336
      Width = 142
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'High Jump'
      TabOrder = 2
      OnClick = BUHighJumpClick
    end
    object RBWalk: TRadioButton
      Left = 28
      Top = 154
      Width = 114
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Walk'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = RBWalkClick
    end
    object RBRun: TRadioButton
      Left = 28
      Top = 196
      Width = 128
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Run'
      TabOrder = 4
      OnClick = RBRunClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 688
    Width = 982
    Height = 72
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      982
      72)
    object TrackBar1: TTrackBar
      Left = 112
      Top = 0
      Width = 870
      Height = 60
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akLeft, akTop, akRight, akBottom]
      Max = 100
      Position = 50
      TabOrder = 0
      ThumbLength = 35
      TickMarks = tmBoth
      OnChange = TrackBar1Change
    end
    object CBBlend: TCheckBox
      Left = 14
      Top = 21
      Width = 86
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
