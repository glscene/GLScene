object Form1: TForm1
  Left = 171
  Top = 95
  Caption = 'Volcano'
  ClientHeight = 315
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 376
    Height = 288
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 141.703720092773400000
    Align = alClient
    TabOrder = 0
  end
  object RadioGroup1: TRadioGroup
    Left = 376
    Top = 0
    Width = 84
    Height = 288
    Align = alRight
    Caption = 'Mode'
    ItemIndex = 1
    Items.Strings = (
      'Sleepy'
      'Gentle'
      'Average'
      'Restless'
      'Angry'
      'Inferno')
    TabOrder = 1
    OnClick = RadioGroup1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 288
    Width = 460
    Height = 27
    Align = alBottom
    Caption = 'FPS'
    TabOrder = 2
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 16
    object DCVolcano: TGLDummyCube
      CubeSize = 1.000000000000000000
      BehavioursData = {
        0458434F4C02010201060B54474C42496E65727469610200060E53696D706C65
        20496E657274696102000200050000000000000080FF3F020008050000000000
        0000B40540050000000000000000000005000000000000000000000902000802
        0008}
      EffectsData = {
        0458434F4C02010201061254474C536F75726365504658456666656374020202
        00120000000002000200060A504658566F6C63616E6F050000000000000080FF
        3F02060200090000000000004040000000000000000002000802000805000000
        0000000080FF3F050000000000CDCCCCFC3F0500000000008FC2F5F83F020002
        00090500000000000000000000080200}
      object Sphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Emission.Color = {00000000000000008180003F0000803F}
        Position.Coordinates = {0000000000000040000000400000803F}
        Radius = 0.300000011920929000
        Slices = 12
        Stacks = 12
        EffectsData = {
          0458434F4C02010201061254474C536F75726365504658456666656374020202
          001200000000020002000607504658426C7565050000000000000080FF3F0206
          020008020008020008050000000000CDCCCCFB3F050000000000CDCCCCFB3F05
          0000000000CDCCCCFB3F02000200090500000000000000000000080200}
      end
    end
    object PFXRenderer: TGLParticleFXRenderer
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000000000000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DCCamera: TGLDummyCube
      Position.Coordinates = {0000000000004040000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 30.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = DCCamera
        Position.Coordinates = {00002041000000410000C0400000803F}
      end
    end
  end
  object PFXVolcano: TGLPolygonPFXManager
    Cadencer = GLCadencer1
    Renderer = PFXRenderer
    Acceleration.Coordinates = {00000000000080BF0000000000000000}
    Friction = 1.000000000000000000
    NbSides = 7
    ParticleSize = 0.250000000000000000
    ColorOuter.Color = {0000803F000000000000000000000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F000000000000000000000000}
        LifeTime = 7.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 96
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 96
    Top = 72
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 248
    Top = 16
  end
  object PFXBlue: TGLPolygonPFXManager
    Cadencer = GLCadencer1
    Renderer = PFXRenderer
    Friction = 1.000000000000000000
    ParticleSize = 0.550000011920929000
    ColorInner.Color = {00000000000000000000803F0000803F}
    ColorOuter.Color = {00000000000000001283203F00000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000803F0000803F00000000}
        LifeTime = 3.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 24
    Top = 72
  end
end
