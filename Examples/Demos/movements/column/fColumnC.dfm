object FormColumn: TFormColumn
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Column'
  ClientHeight = 733
  ClientWidth = 1113
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 23
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1113
    Height = 704
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 148.281265258789100000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object StaticText1: TStaticText
    Left = 0
    Top = 704
    Width = 1113
    Height = 29
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
