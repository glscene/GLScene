object FormColumn: TFormColumn
  Left = 203
  Top = 102
  BorderWidth = 5
  Caption = 'Column'
  ClientHeight = 380
  ClientWidth = 592
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
    Width = 592
    Height = 363
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 122.293617248535200000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 468
    ExplicitHeight = 337
  end
  object StaticText1: TStaticText
    Left = 0
    Top = 363
    Width = 592
    Height = 17
    Align = alBottom
    Alignment = taCenter
    AutoSize = False
    BorderStyle = sbsSingle
    Caption = '???.? FPS'
    TabOrder = 1
    ExplicitTop = 324
    ExplicitWidth = 406
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
