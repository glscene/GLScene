object FormCutoutStar: TFormCutoutStar
  Left = 187
  Top = 153
  Caption = 'Cutout Star'
  ClientHeight = 535
  ClientWidth = 718
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 718
    Height = 535
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 148.675781250000000000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object PanelFPS: TPanel
    Left = 310
    Top = 10
    Width = 111
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
