object Form1: TForm1
  Left = 222
  Top = 103
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 494
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 33
    Width = 494
    Height = 408
    Camera = GLCamera1
    Buffer.BackgroundColor = 4194304
    FieldOfView = 152.456802368164100000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 494
    Height = 33
    Align = alTop
    Caption = ' '
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 34
      Height = 13
      Caption = 'Speed:'
    end
    object TrackBarSpeed: TTrackBar
      Left = 48
      Top = 4
      Width = 133
      Height = 25
      Position = 1
      TabOrder = 0
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
