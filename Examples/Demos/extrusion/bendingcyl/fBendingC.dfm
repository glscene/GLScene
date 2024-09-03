object FormBending: TFormBending
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Bending Cylinder'
  ClientHeight = 695
  ClientWidth = 1040
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 168
  TextHeight = 23
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1040
    Height = 695
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 163.624389648437500000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object CBSpline: TCheckBox
    Left = 210
    Top = 14
    Width = 100
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Splines'
    TabOrder = 1
    OnClick = CBSplineClick
  end
  object CBFat: TCheckBox
    Left = 658
    Top = 14
    Width = 100
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Fat/Slim'
    TabOrder = 2
  end
  object PanelFPS: TPanel
    Left = 364
    Top = 14
    Width = 226
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'FPS'
    TabOrder = 3
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 128
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Pipe1: TGLPipe
      Position.Coordinates = {00000000000080BF000000000000803F}
      Nodes = <
        item
        end
        item
          Y = 1.000000000000000000
        end
        item
          X = -1.000000000000000000
          Y = 2.000000000000000000
        end>
      Parts = [ppOutside, ppInside, ppStartDisk, ppStopDisk]
      Radius = 0.100000001490116100
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000803F00004040000080400000803F}
      Left = 160
      Top = 120
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 192
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 96
    Top = 128
  end
end
