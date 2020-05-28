object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Whirlwind'
  ClientHeight = 296
  ClientWidth = 469
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
    Width = 469
    Height = 268
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.DepthTest = False
    FieldOfView = 158.863967895507800000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 268
    Width = 469
    Height = 28
    Align = alBottom
    Caption = 'FPS'
    TabOrder = 1
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 120
    Top = 80
  end
  object Timer1: TTimer
    Interval = 10000
    OnTimer = Timer1Timer
    Left = 224
    Top = 16
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 40
    object GLParticles1: TGLParticles
      ObjectsSorting = osNone
      CubeSize = 1.000000000000000000
      ParticlePoolSize = 10
      OnActivateParticle = GLParticles1ActivateParticle
      object DummyCube1: TGLDummyCube
        CubeSize = 1.000000000000000000
        BehavioursData = {
          0458434F4C02010201060B54474C42496E657274696102001200000000020002
          00050000000000000080FF3F0200080500000000000000000000050000000000
          0000000000050000000000000000000008020008020008}
        object Sprite1: TGLSprite
          Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
          Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
          Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
          Width = 0.100000001490116100
          Height = 0.100000001490116100
          Rotation = 0.000000000000000000
        end
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 25.000000000000000000
      TargetObject = GLParticles1
      Position.Coordinates = {0000204100004040000000000000803F}
    end
  end
end
