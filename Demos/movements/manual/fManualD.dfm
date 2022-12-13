object FormManual: TFormManual
  Left = 192
  Top = 101
  Caption = 'Manual'
  ClientHeight = 428
  ClientWidth = 585
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnResize = FormResize
  DesignSize = (
    585
    428)
  TextHeight = 13
  object SceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 585
    Height = 404
    Camera = Camera
    Buffer.BackgroundColor = clSilver
    FieldOfView = 127.324623107910200000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 500
    ExplicitHeight = 356
  end
  object TrackBar: TTrackBar
    Left = 0
    Top = 404
    Width = 585
    Height = 24
    Align = alBottom
    Max = 360
    PageSize = 10
    Frequency = 10
    TabOrder = 1
    ThumbLength = 15
    OnChange = TrackBarChange
    ExplicitTop = 354
    ExplicitWidth = 499
  end
  object CBPlay: TCheckBox
    Left = 230
    Top = 66
    Width = 41
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Play'
    Checked = True
    State = cbChecked
    TabOrder = 2
    ExplicitTop = 16
  end
  object StaticText1: TStaticText
    Left = 16
    Top = 16
    Width = 45
    Height = 17
    BorderStyle = sbsSingle
    Caption = '??? FPS'
    TabOrder = 3
  end
  object Scene: TGLScene
    Left = 120
    Top = 16
    object CubeSun: TGLCube
      Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
    end
    object CubeEarth: TGLCube
      Material.FrontProperties.Diffuse.Color = {8786063F8786063F0000803F0000803F}
      Material.FrontProperties.Emission.Color = {0000000000000000A1A0203F0000803F}
      Position.Coordinates = {0000404000000000000000000000803F}
      CubeSize = {0000003F0000003F0000003F}
    end
    object CubeMoon: TGLCube
      Position.Coordinates = {000040400000803F000000000000803F}
      CubeSize = {CDCC4C3ECDCC4C3ECDCC4C3E}
    end
    object LightSource: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      SpotCutOff = 180.000000000000000000
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = CubeSun
      Position.Coordinates = {000020410000A040000020410000803F}
    end
  end
  object Cadencer: TGLCadencer
    Scene = Scene
    OnProgress = CadencerProgress
    Left = 240
    Top = 16
  end
end
