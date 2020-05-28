object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Bending Cylinder'
  ClientHeight = 397
  ClientWidth = 586
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 586
    Height = 397
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 151.723861694335900000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object CBSpline: TCheckBox
    Left = 120
    Top = 8
    Width = 57
    Height = 17
    Caption = 'Splines'
    TabOrder = 1
    OnClick = CBSplineClick
  end
  object CBFat: TCheckBox
    Left = 376
    Top = 8
    Width = 57
    Height = 17
    Caption = 'Fat/Slim'
    TabOrder = 2
  end
  object PanelFPS: TPanel
    Left = 208
    Top = 8
    Width = 129
    Height = 17
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
