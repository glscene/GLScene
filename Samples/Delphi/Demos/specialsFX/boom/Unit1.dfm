object Form1: TForm1
  Left = 155
  Top = 94
  Caption = 'Boom'
  ClientHeight = 297
  ClientWidth = 519
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
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 519
    Height = 297
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 142.783203125000000000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 35
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 8
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000007041000000000000803F}
      CubeSize = 1.000000000000000000
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Sphere1: TGLSphere
      Radius = 0.500000000000000000
      Slices = 9
      Stacks = 6
      BehavioursData = {
        0458434F4C02010201060B54474C42496E657274696102001200000000020002
        00050000000000000080FF3F0200080500000000000000000000050000000000
        0000000000050000000000000000000009020008020008}
      EffectsData = {
        0458434F4C02010202060A54474C424669726546580201020012000000000200
        020006064669726546580200020102001200000000020002000607536D6F6B65
        4658}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 150.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {000048420000A041000070410000803F}
      Left = 248
      Top = 144
    end
  end
  object FireFX: TGLFireFXManager
    FireDir.Coordinates = {00000000000000000000000000000000}
    InitialDir.Coordinates = {00000000000000000000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 512
    ParticleSize = 0.500000000000000000
    FireDensity = 0.600000023841857900
    FireEvaporation = 0.860000014305114700
    ParticleLife = 1
    FireRadius = 0.500000000000000000
    Disabled = False
    Paused = False
    ParticleInterval = 0.009999999776482582
    UseInterval = True
    Reference = Sphere1
    Left = 64
    Top = 56
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 64
    Top = 136
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 64
    Top = 216
  end
  object SmokeFX: TGLFireFXManager
    FireDir.Coordinates = {000000000000803F0000000000000000}
    InitialDir.Coordinates = {000000000000003F0000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 64
    ParticleSize = 2.000000000000000000
    InnerColor.Color = {0000803E0000803E0000803E0000803F}
    OuterColor.Color = {0000000000000000000000000000803F}
    FireDensity = 0.600000023841857900
    FireEvaporation = 0.860000014305114700
    FireRadius = 1.000000000000000000
    Disabled = True
    Paused = False
    ParticleInterval = 0.070000000298023220
    UseInterval = False
    Reference = Sphere1
    Left = 64
    Top = 88
  end
end
