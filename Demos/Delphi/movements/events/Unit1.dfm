object Form1: TForm1
  Left = 209
  Top = 106
  Caption = 'Events'
  ClientHeight = 302
  ClientWidth = 503
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 503
    Height = 302
    Camera = Camera1
    Buffer.BackgroundColor = clSilver
    FieldOfView = 143.357925415039100000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 8
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object Cube1: TGLCube
      Material.FrontProperties.Ambient.Color = {0000803F00000000000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      Material.FrontProperties.Emission.Color = {0000803F00000000000000000000803F}
      Position.Coordinates = {000040C000000000000000000000803F}
      Scale.Coordinates = {0000803F0000A0400000803F00000000}
    end
    object Cube2: TGLCube
      Material.FrontProperties.Emission.Color = {000000000000803F000000000000803F}
      Scale.Coordinates = {0000803F0000A0400000803F00000000}
    end
    object Cube3: TGLCube
      Material.FrontProperties.Emission.Color = {00000000000000000000803F0000803F}
      Position.Coordinates = {0000404000000000000000000000803F}
      Scale.Coordinates = {0000803F0000A0400000803F00000000}
    end
    object Camera1: TGLCamera
      DepthOfView = 1000000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000000000000000000020410000803F}
      Direction.Coordinates = {000080BF000000000000000000000000}
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 440
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 40
    Top = 72
  end
  object GLTimeEventsMGR1: TGLTimeEventsMGR
    Cadencer = GLCadencer1
    FreeEventOnEnd = True
    Events = <
      item
        Name = 'Event0'
        StartTime = 2.000000000000000000
        EndTime = 5.000000000000000000
        EventType = etContinuous
        OnEvent = GLTimeEventsMGR1Events0Event
      end
      item
        Name = 'Event1'
        StartTime = 5.000000000000000000
        EndTime = 10.000000000000000000
        Period = 0.010000000000000000
        EventType = etPeriodic
        OnEvent = GLTimeEventsMGR1Events1Event
      end
      item
        Name = 'Event2'
        StartTime = 10.000000000000000000
        OnEvent = GLTimeEventsMGR1Events2Event
      end
      item
        Name = 'Event3'
        StartTime = 15.000000000000000000
        EndTime = -1.000000000000000000
        Period = 0.500000000000000000
        EventType = etPeriodic
        OnEvent = GLTimeEventsMGR1Events3Event
      end
      item
        Name = 'Event4'
        StartTime = 15.000000000000000000
        EndTime = -1.000000000000000000
        Period = 0.100000000000000000
        EventType = etPeriodic
        OnEvent = GLTimeEventsMGR1Events4Event
      end
      item
        Name = 'Event5'
        StartTime = 15.000000000000000000
        EndTime = -1.000000000000000000
        Period = 0.010000000000000000
        EventType = etPeriodic
        OnEvent = GLTimeEventsMGR1Events5Event
      end>
    Left = 184
    Top = 8
  end
end
