object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Font Color'
  ClientHeight = 372
  ClientWidth = 525
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 525
    Height = 372
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 149.907211303710900000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000004000000040000000400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Teapot1: TGLTeapot
      Material.FrontProperties.Diffuse.Color = {9D9C1C3FB3B2323F0000803F0000803F}
    end
    object HUDText1: TGLHUDText
      Position.Coordinates = {0000484300008C42000000000000803F}
      BitmapFont = BitmapFont
      Text = 'FADING OUT'
      Rotation = 0.000000000000000000
      Alignment = taCenter
      ModulateColor.Color = {0000803F0000803F0000803F3333333F}
    end
    object HUDText2: TGLHUDText
      Position.Coordinates = {0000484300009143000000000000803F}
      BitmapFont = BitmapFont
      Text = 'THE END'
      Rotation = 0.000000000000000000
      Alignment = taCenter
      ModulateColor.Color = {0000803FF8FEFE3E000000003333333F}
    end
    object HUDText3: TGLHUDText
      Position.Coordinates = {000048430000A041000000000000803F}
      BitmapFont = BitmapFont
      Text = 'RED RED'
      Rotation = 0.000000000000000000
      Alignment = taCenter
      ModulateColor.Color = {0000803F00000000000000000000803F}
    end
    object HUDText4: TGLHUDText
      Position.Coordinates = {0000484300007A43000000000000803F}
      BitmapFont = BitmapFont
      Text = 'TRANSPARENT'
      Rotation = 0.000000000000000000
      Alignment = taCenter
      ModulateColor.Color = {9A99593F9A99593FCDCCCC3DCDCCCC3E}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Teapot1
      Position.Coordinates = {0000803F0000003F0000803F0000803F}
    end
  end
  object BitmapFont: TGLBitmapFont
    GlyphsIntervalX = 7
    GlyphsIntervalY = 0
    Ranges = <
      item
        StartASCII = 'A'
        StopASCII = 'H'
        StartGlyphIdx = 0
      end
      item
        StartASCII = 'I'
        StopASCII = 'P'
        StartGlyphIdx = 8
      end
      item
        StartASCII = 'Q'
        StopASCII = 'X'
        StartGlyphIdx = 16
      end
      item
        StartASCII = '!'
        StopASCII = '!'
        StartGlyphIdx = 27
      end>
    CharWidth = 27
    CharHeight = 32
    HSpace = 0
    Left = 24
    Top = 72
  end
  object GLTimeEventsMGR1: TGLTimeEventsMGR
    Cadencer = GLCadencer1
    Events = <
      item
        Name = 'Event0'
        StartTime = 1.500000000000000000
        EndTime = 3.000000000000000000
        EventType = etContinuous
        OnEvent = GLTimeEventsMGR1Events0Event
      end
      item
        Name = 'Event1'
        OnEvent = GLTimeEventsMGR1Events1Event
      end
      item
        Name = 'Event2'
        StartTime = 2.000000000000000000
        EndTime = 10.000000000000000000
        EventType = etContinuous
        OnEvent = GLTimeEventsMGR1Events2Event
      end>
    Left = 328
    Top = 72
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 328
    Top = 8
  end
end
