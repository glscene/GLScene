object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Fading'
  ClientHeight = 413
  ClientWidth = 532
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
    Width = 532
    Height = 413
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 152.777770996093800000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 48
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000048420000C842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000000000008040000020410000803F}
    end
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
      object Torus: TGLTorus
        Direction.Coordinates = {F604B5BEF304353F70C41C3F00000000}
        Position.Coordinates = {000000C0CDCC0C40000000000000803F}
        Up.Coordinates = {F604B53EF304353F70C41CBF00000000}
        OnProgress = SphereProgress
        MajorRadius = 0.699999988079071000
        MinorRadius = 0.200000002980232200
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object Cone: TGLCone
        Direction.Coordinates = {00000000F28384BEEA46773F00000000}
        Position.Coordinates = {0000004000002040000000000000803F}
        Up.Coordinates = {00000000EA46773FF283843E00000000}
        OnProgress = SphereProgress
        BottomRadius = 1.000000000000000000
        Height = 1.500000000000000000
      end
      object Cylinder: TGLCylinder
        Position.Coordinates = {000000C0000080BE000000000000803F}
        OnProgress = SphereProgress
        BottomRadius = 1.000000000000000000
        Height = 1.500000000000000000
        TopRadius = 1.000000000000000000
      end
      object Sphere: TGLSphere
        Position.Coordinates = {00000040000080BE000000000000803F}
        OnProgress = SphereProgress
        Radius = 1.000000000000000000
      end
    end
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 16
    Top = 16
  end
  object TIPickTimer: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TIPickTimerTimer
    Left = 128
    Top = 16
  end
end
