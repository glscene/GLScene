object FormBmpFont: TFormBmpFont
  Left = 166
  Top = 102
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Bitmap Font'
  ClientHeight = 662
  ClientWidth = 922
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
    Width = 922
    Height = 662
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 162.819976806640600000
    PenAsTouch = False
    Align = alClient
    OnClick = GLSceneViewer1Click
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 16
    Top = 16
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Teapot1
      Position.Coordinates = {0000A04000004040000080400000803F}
      Left = 240
      Top = 152
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Teapot1: TGLTeapot
      Material.FrontProperties.Diffuse.Color = {ADAC2C3FAAA9293FF0EF6F3F0000803F}
      Direction.Coordinates = {EE83843E00000000EA46773F00000000}
      Scale.Coordinates = {0000A0400000A0400000A04000000000}
    end
    object HUDText1: TGLHUDText
      Position.Coordinates = {0000484200008C42000000000000803F}
      BitmapFont = BitmapFont1
      Text = 'Hello World'
      Rotation = 0.000000000000000000
    end
    object HUDText2: TGLHUDText
      Position.Coordinates = {0000C84200009643000000000000803F}
      BitmapFont = BitmapFont1
      Text = 'Spin'
      Rotation = 0.000000000000000000
      Alignment = taCenter
      Layout = tlCenter
    end
    object HUDText3: TGLHUDText
      Position.Coordinates = {0000AF4300009643000000000000803F}
      BitmapFont = BitmapFont1
      Text = 'Scale'
      Rotation = 0.000000000000000000
      Alignment = taCenter
      Layout = tlCenter
    end
    object HUDTextFPS: TGLHUDText
      Position.Coordinates = {0000C8420000A041000000000000803F}
      BitmapFont = BitmapFont1
      Text = 'FPS'
      Rotation = 0.000000000000000000
    end
  end
  object BitmapFont1: TGLBitmapFont
    GlyphsIntervalX = 1
    GlyphsIntervalY = 1
    Ranges = <
      item
        StartASCII = ' '
        StopASCII = 'Z'
        StartGlyphIdx = 0
      end
      item
        StartASCII = 'a'
        StopASCII = 'z'
        StartGlyphIdx = 33
      end>
    CharWidth = 30
    CharHeight = 30
    HSpace = 3
    VSpace = 6
    Left = 80
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 48
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 80
  end
end
