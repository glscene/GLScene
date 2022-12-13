object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Column'
  ClientHeight = 419
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 628
    Height = 402
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 127.098167419433600000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 469
    ExplicitHeight = 378
  end
  object StaticText1: TStaticText
    Left = 0
    Top = 402
    Width = 628
    Height = 17
    Align = alBottom
    Alignment = taCenter
    AutoSize = False
    BorderStyle = sbsSingle
    Caption = '???.? FPS'
    TabOrder = 1
    ExplicitTop = 375
    ExplicitWidth = 448
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object DummyCube1: TGLDummyCube
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
end
