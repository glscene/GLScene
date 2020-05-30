object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Subdivide'
  ClientHeight = 380
  ClientWidth = 567
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 450
    Top = 0
    Width = 2
    Height = 380
    Align = alRight
    AutoSize = False
    ExplicitLeft = 382
    ExplicitHeight = 324
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 450
    Height = 380
    Camera = GLCamera1
    Buffer.BackgroundColor = 13619151
    Buffer.FaceCulling = False
    FieldOfView = 150.512878417968800000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 452
    Top = 0
    Width = 115
    Height = 380
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 1
    object LASubdivideTime: TLabel
      Left = 8
      Top = 168
      Width = 97
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'LASubdivideTime'
    end
    object BULoad: TButton
      Left = 16
      Top = 8
      Width = 83
      Height = 25
      Caption = 'Load'
      TabOrder = 0
      OnClick = BULoadClick
    end
    object BUSubdivide: TButton
      Left = 8
      Top = 112
      Width = 99
      Height = 25
      Caption = 'Subdivide'
      Enabled = False
      TabOrder = 1
      OnClick = BUSubdivideClick
    end
    object TrackBar1: TTrackBar
      Left = 8
      Top = 144
      Width = 97
      Height = 17
      Hint = 'Subdivision smoothness'
      Position = 5
      TabOrder = 2
      ThumbLength = 10
    end
    object RBWireFrame: TRadioButton
      Left = 16
      Top = 40
      Width = 81
      Height = 17
      Caption = 'Wireframe'
      TabOrder = 3
      OnClick = RBWireFrameClick
    end
    object RBSolid: TRadioButton
      Left = 16
      Top = 56
      Width = 89
      Height = 17
      Caption = 'Solid'
      Checked = True
      TabOrder = 4
      TabStop = True
      OnClick = RBSolidClick
    end
    object CBAnimate: TCheckBox
      Left = 16
      Top = 80
      Width = 81
      Height = 17
      Caption = 'Animate'
      TabOrder = 5
      OnClick = CBAnimateClick
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 16
    object GLActor1: TGLActor
      Material.Texture.TextureMode = tmReplace
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Interval = 100
      AutoCentering = [macCenterX, macCenterY, macCenterZ]
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLActor1
      Position.Coordinates = {0000A04000008040000040400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 128
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 216
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 72
  end
end
