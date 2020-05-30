object MainForm: TMainForm
  Left = 214
  Top = 182
  Caption = 'MainForm'
  ClientHeight = 441
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GLSV: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 680
    Height = 441
    Camera = Camera
    FieldOfView = 154.447631835937500000
    Align = alClient
    OnMouseMove = GLSVMouseMove
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object GLScene: TGLScene
    Left = 8
    Top = 8
    object Center: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Light: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000A0410000A041000000000000803F}
        SpotCutOff = 180.000000000000000000
      end
      object Plane: TGLPlane
        Material.FrontProperties.Diffuse.Color = {00000000000000000000000000000000}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Scale.Coordinates = {00007A4400007A440000803F00000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
    end
    object Camera: TGLCamera
      DepthOfView = 200.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Center
      Position.Coordinates = {0000484200004842000000000000803F}
      object Light1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000A0400000A0400000A0400000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object Cadencer: TGLCadencer
    Scene = GLScene
    OnProgress = CadencerProgress
    Left = 40
    Top = 8
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 72
    Top = 8
  end
end
