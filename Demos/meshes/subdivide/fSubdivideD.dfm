object FormSubdivide: TFormSubdivide
  Left = 173
  Top = 100
  BorderWidth = 2
  Caption = 'Subdivide'
  ClientHeight = 511
  ClientWidth = 773
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 626
    Top = 0
    Width = 3
    Height = 511
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    AutoSize = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 626
    Height = 511
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = 13619151
    Buffer.FaceCulling = False
    FieldOfView = 157.854904174804700000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 629
    Top = 0
    Width = 144
    Height = 511
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 1
    object LASubdivideTime: TLabel
      Left = 10
      Top = 210
      Width = 121
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taCenter
      AutoSize = False
      Caption = 'LASubdivideTime'
    end
    object BULoad: TButton
      Left = 20
      Top = 10
      Width = 104
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Load'
      TabOrder = 0
      OnClick = BULoadClick
    end
    object BUSubdivide: TButton
      Left = 10
      Top = 140
      Width = 124
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Subdivide'
      Enabled = False
      TabOrder = 1
      OnClick = BUSubdivideClick
    end
    object TrackBar1: TTrackBar
      Left = 10
      Top = 180
      Width = 121
      Height = 21
      Hint = 'Subdivision smoothness'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Position = 5
      TabOrder = 2
      ThumbLength = 13
    end
    object RBWireFrame: TRadioButton
      Left = 20
      Top = 50
      Width = 101
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Wireframe'
      TabOrder = 3
      OnClick = RBWireFrameClick
    end
    object RBSolid: TRadioButton
      Left = 20
      Top = 70
      Width = 111
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Solid'
      Checked = True
      TabOrder = 4
      TabStop = True
      OnClick = RBSolidClick
    end
    object CBAnimate: TCheckBox
      Left = 20
      Top = 100
      Width = 101
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
