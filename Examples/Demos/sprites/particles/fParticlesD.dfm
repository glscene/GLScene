object FormParticles: TFormParticles
  Left = 284
  Top = 105
  Caption = 'Particles'
  ClientHeight = 438
  ClientWidth = 727
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 727
    Height = 438
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.DepthTest = False
    FieldOfView = 154.278411865234400000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 8
    object GLParticles1: TGLParticles
      ObjectsSorting = osNone
      Scale.Coordinates = {00000040000000400000004000000000}
      CubeSize = 1.000000000000000000
      OnActivateParticle = GLParticles1ActivateParticle
      BehavioursData = {
        0458434F4C02010201060B54474C42496E657274696102001200000000020002
        00050000000000000080FF3F0200080500000000000000F00340050000000000
        0000000000050000000000000000000008020008020008}
      object Sprite1: TGLSprite
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.BlendingMode = bmAdditive
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureFormat = tfIntensity
        Material.Texture.Disabled = False
        OnProgress = Sprite1Progress
        Width = 1.000000000000000000
        Height = 1.000000000000000000
        Rotation = 0.000000000000000000
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLParticles1
      Position.Coordinates = {0000704100000000000000000000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    Left = 40
    Top = 72
  end
  object Timer1: TTimer
    Interval = 300
    OnTimer = Timer1Timer
    Left = 208
    Top = 8
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Particles - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 256
    Top = 104
  end
end
