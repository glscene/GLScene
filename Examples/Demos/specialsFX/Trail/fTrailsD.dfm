object FormTrails: TFormTrails
  Left = 222
  Top = 103
  Caption = 'Trails'
  ClientHeight = 551
  ClientWidth = 618
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
    Top = 41
    Width = 618
    Height = 510
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = 4194304
    FieldOfView = 157.812561035156300000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 618
    Height = 41
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Caption = ' '
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 15
      Width = 44
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Speed:'
    end
    object TrackBarSpeed: TTrackBar
      Left = 60
      Top = 5
      Width = 166
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Position = 1
      TabOrder = 0
      ThumbLength = 25
    end
  end
  object GLScene1: TGLScene
    Left = 60
    Top = 36
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00000000E1A22B41000000000000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLSphere1: TGLSphere
      Position.Coordinates = {000000008E2EBA3F8C2EBA3E0000803F}
      Radius = 0.500000000000000000
    end
    object Room: TGLSphere
      Material.PolygonMode = pmLines
      NormalDirection = ndInside
      Radius = 8.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Room
      Position.Coordinates = {CFBD804073312F409AF80A410000803F}
      Direction.Coordinates = {82FABBBE331FC9BE96D857BF00000000}
      Up.Coordinates = {3F9720BE056C6B3FE965B8BE00000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 100
    Top = 36
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 140
    Top = 36
  end
end
