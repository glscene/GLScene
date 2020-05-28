object Form1: TForm1
  Left = 233
  Top = 95
  BorderStyle = bsDialog
  Caption = 'Spiral'
  ClientHeight = 357
  ClientWidth = 437
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 25
    Width = 437
    Height = 332
    Camera = GLCamera
    Buffer.BackgroundColor = clBlack
    FieldOfView = 146.474884033203100000
    Align = alClient
    OnDblClick = GLSceneViewerDblClick
    OnMouseMove = GLSceneViewerMouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 437
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 1
    Caption = 'Spiral / PFX Demo'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 1
    object SpeedButton1: TSpeedButton
      Left = 363
      Top = 2
      Width = 73
      Height = 21
      Caption = 'FullScreen'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = GLSceneViewerDblClick
    end
  end
  object GLScene: TGLScene
    Left = 16
    Top = 16
    object DCBase: TGLDummyCube
      CubeSize = 1.000000000000000000
      BehavioursData = {
        0458434F4C02010201060B54474C42496E657274696102001200000000020002
        00050000000000000080FF3F0200080500000000000000F00540050000000000
        0000000000050000000000000000000008020008020008}
      EffectsData = {
        0458434F4C02010201061254474C536F75726365504658456666656374020202
        00120000000002000200060750465852696E67050000000000000080FF3F0206
        0200080200090000000000002040000000000000000002000805000000000000
        0000000005000000000000000000000500000000000000FA0740020002010905
        00000000000000000000080200}
      object DCSrc: TGLDummyCube
        Position.Coordinates = {0000803F00000040000000000000803F}
        CubeSize = 1.000000000000000000
        EffectsData = {
          0458434F4C02010201061254474C536F75726365504658456666656374020202
          00120000000002000200060950465853706972616C050000000000000080FF3F
          020602000900000000CDCC4CBE00000000000000000200080200080500000000
          00CDCCCCFB3F0500000000000000000000050000000000CDCCCCFA3F02000200
          090500000000000000000000080200}
      end
    end
    object PFXRenderer: TGLParticleFXRenderer
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DCBase
      Position.Coordinates = {0000C0400000A040000080400000803F}
    end
  end
  object PFXSpiral: TGLPolygonPFXManager
    Cadencer = GLCadencer
    Renderer = PFXRenderer
    Acceleration.Coordinates = {00000000CDCC4CBE0000000000000000}
    Friction = 1.000000000000000000
    NbSides = 9
    ParticleSize = 0.300000011920929000
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000803F000000000000803F}
        ColorOuter.Color = {00000000000000000000803F00000000}
        LifeTime = 3.000000000000000000
        SizeScale = 1.000000000000000000
      end
      item
        ColorInner.Color = {0AD7A33E48E1FA3E1F85EB3E0000803F}
        ColorOuter.Color = {0000803F000000000000000000000000}
        LifeTime = 6.000000000000000000
        SizeScale = 1.000000000000000000
      end
      item
        LifeTime = 9.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 88
    Top = 16
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    MaxDeltaTime = 0.100000000000000000
    Left = 16
    Top = 56
  end
  object Timer: TTimer
    Interval = 2000
    OnTimer = TimerTimer
    Left = 16
    Top = 96
  end
  object PFXRing: TGLPolygonPFXManager
    Cadencer = GLCadencer
    Renderer = PFXRenderer
    Friction = 1.000000000000000000
    NbSides = 9
    ParticleSize = 0.200000002980232200
    ColorInner.Color = {00000000000000001283203F0000803F}
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000803F0000803F9A99193F}
        LifeTime = 2.500000000000000000
        SizeScale = 1.000000000000000000
      end
      item
        LifeTime = 3.500000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 88
    Top = 56
  end
  object GLFullScreenViewer: TGLFullScreenViewer
    Camera = GLCamera
    Width = 800
    Height = 600
    Buffer.BackgroundColor = clBlack
    ManualRendering = False
    RefreshRate = 0
    OnKeyPress = GLFullScreenViewerKeyPress
    OnDblClick = GLFullScreenViewerDblClick
    Left = 88
    Top = 96
  end
end
