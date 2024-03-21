object FormColumn: TFormColumn
  Left = 203
  Top = 102
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderWidth = 5
  Caption = 'Column'
  ClientHeight = 665
  ClientWidth = 1036
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 24
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1036
    Height = 635
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 145.035247802734400000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object StaticText1: TStaticText
    Left = 0
    Top = 635
    Width = 1036
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    Alignment = taCenter
    AutoSize = False
    BorderStyle = sbsSingle
    Caption = '???.? FPS'
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object DummyCube1: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000A04100002041000020410000803F}
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 208
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 104
    Top = 8
  end
  object GLFPSMovementManager1: TGLFPSMovementManager
    DisplayTime = 2000
    MovementScale = 4.000000000000000000
    Left = 120
    Top = 160
  end
end
