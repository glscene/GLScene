object Form1: TForm1
  Left = 175
  Top = 114
  Caption = 'Screen Saver'
  ClientHeight = 368
  ClientWidth = 528
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 528
    Height = 368
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 149.595184326171900000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      BehavioursData = {
        0458434F4C02010201060B54474C42496E657274696102001200000000020002
        00050000000000000080FF3F02000805000000000000008404C0050000000000
        00009002400500000000000000C0004008020008020008}
      object DummyCube2: TGLDummyCube
        Position.Coordinates = {0000000000000000000040400000803F}
        CubeSize = 1.000000000000000000
        BehavioursData = {
          0458434F4C02010201060B54474C42496E657274696102001200000000020002
          00050000000000000080FF3F0200080500000000000000C00040050000000000
          0000000000050000000000000098034008020008020008}
        object DummyCube3: TGLDummyCube
          Position.Coordinates = {000000000000803F000000000000803F}
          CubeSize = 1.000000000000000000
          BehavioursData = {
            0458434F4C02010201060B54474C42496E657274696102001200000000020002
            00050000000000000080FF3F020008050000000000000080FF3F050000000000
            0000C00040050000000000000080034008020008020008}
          object Torus1: TGLTorus
            MajorRadius = 1.000000000000000000
            MinorRadius = 0.300000011920929000
            Rings = 32
            Sides = 32
            StopAngle = 360.000000000000000000
            Parts = [toSides, toStartDisk, toStopDisk]
            BehavioursData = {
              0458434F4C02010201060B54474C42496E657274696102001200000000020002
              00050000000000000080FF3F0200080500000000000000000000050000000000
              0000D80340050000000000000000000008020008020008}
          end
        end
      end
    end
    object DummyCube4: TGLDummyCube
      CubeSize = 1.000000000000000000
      BehavioursData = {
        0458434F4C02010201060B54474C42496E657274696102001200000000020002
        00050000000000000080FF3F0200080500000000000000A00140050000000000
        00000000000500000000000000C0004008020008020008}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {0000803F00000000000000000000803F}
        Position.Coordinates = {0000704100000000000000000000803F}
        SpotCutOff = 180.000000000000000000
      end
      object GLLightSource2: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {9594943E9594943E0000803F0000803F}
        Position.Coordinates = {0000E0C0000000000000E0400000803F}
        SpotCutOff = 180.000000000000000000
      end
      object GLLightSource3: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {0000803F0000803F000000000000803F}
        Position.Coordinates = {0000E0C0000000000000E0C00000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000204100000000000000000000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    Left = 128
    Top = 8
  end
  object GLScreenSaver1: TGLScreenSaver
    OnPropertiesRequested = GLScreenSaver1PropertiesRequested
    Left = 240
    Top = 8
  end
end
