object FormCutoutstar: TFormCutoutstar
  Left = 0
  Top = 0
  Caption = 'Cutout Star'
  ClientHeight = 443
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 650
    Height = 443
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 142.587753295898400000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object PanelFPS: TPanel
    Left = 248
    Top = 8
    Width = 89
    Height = 25
    Caption = 'FPS'
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000484200002042000070420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object ExtrusionSolid: TGLExtrusionSolid
      Material.Texture.ImageClassName = 'TGLPicFileImage'
      Contours = <>
      Parts = [espOutside, espStartPolygon, espStopPolygon]
      Height = 1.000000000000000000
      MinSmoothAngle = 5.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 75.000000000000000000
      TargetObject = ExtrusionSolid
      Position.Coordinates = {0000C04000000041000020410000803F}
      Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Left = 216
      Top = 152
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 120
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 48
    Top = 80
  end
end
