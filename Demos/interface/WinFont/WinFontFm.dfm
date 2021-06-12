object FormWinFont: TFormWinFont
  Left = 129
  Top = 112
  Caption = 'Windows Font'
  ClientHeight = 383
  ClientWidth = 696
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 696
    Height = 383
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 150.733886718750000000
    PenAsTouch = False
    Align = alClient
    OnClick = GLSceneViewer1Click
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 40
    Top = 64
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
      Position.Coordinates = {0000A0410000C841000000000000803F}
      BitmapFont = WindowsBitmapFont1
      Text = 'Hello World'
      Rotation = 0.000000000000000000
    end
    object HUDText2: TGLHUDText
      Position.Coordinates = {000016430000AF43000000000000803F}
      BitmapFont = WindowsBitmapFont1
      Text = 'Spin'
      Rotation = 0.000000000000000000
      Alignment = taCenter
      Layout = tlCenter
    end
    object HUDText3: TGLHUDText
      Position.Coordinates = {008009440000AF43000000000000803F}
      BitmapFont = WindowsBitmapFont1
      Text = 'Scale'
      Rotation = 0.000000000000000000
      Alignment = taCenter
      Layout = tlCenter
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Teapot1
      Position.Coordinates = {0000A04000004040000080400000803F}
      Left = 240
      Top = 152
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 120
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 120
    Top = 184
  end
  object WindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = '@Arial Unicode MS'
    Font.Style = [fsBold]
    Left = 40
    Top = 192
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 64
    object MIPickFont: TMenuItem
      Caption = 'Pick Font'
      OnClick = MIPickFontClick
    end
    object MIViewTexture: TMenuItem
      Caption = 'View Texture'
      OnClick = MIViewTextureClick
    end
    object MIFPS: TMenuItem
      Caption = 'FPS'
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 112
    Top = 120
  end
end
