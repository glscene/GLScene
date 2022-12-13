object FormCutoutStar: TFormCutoutStar
  Left = 187
  Top = 153
  Caption = 'Cutout Star'
  ClientHeight = 428
  ClientWidth = 574
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 574
    Height = 428
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 141.372207641601600000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object PanelFPS: TPanel
    Left = 248
    Top = 8
    Width = 89
    Height = 25
    Caption = 'FPS'
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000484200002042000070420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object ExtrusionSolid: TGLExtrusionSolid
      Contours = <>
      Parts = [espOutside, espStartPolygon, espStopPolygon]
      Height = 1.000000000000000000
      MinSmoothAngle = 5.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 75.000000000000000000
      TargetObject = ExtrusionSolid
      Position.Coordinates = {0000C04000000041000020410000803F}
      Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Left = 216
      Top = 152
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 48
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 88
  end
end
