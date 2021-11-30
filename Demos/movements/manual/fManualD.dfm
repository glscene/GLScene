object FormManual: TFormManual
  Left = 192
  Top = 101
  Caption = 'Manual'
  ClientHeight = 473
  ClientWidth = 624
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnResize = FormResize
  PixelsPerInch = 120
  DesignSize = (
    624
    473)
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 624
    Height = 442
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    FieldOfView = 131.307571411132800000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 561
    ExplicitHeight = 346
  end
  object TrackBar: TTrackBar
    Left = 0
    Top = 442
    Width = 624
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    Max = 360
    PageSize = 10
    Frequency = 10
    TabOrder = 1
    ThumbLength = 19
    OnChange = TrackBarChange
    ExplicitTop = 343
    ExplicitWidth = 561
  end
  object CBPlay: TCheckBox
    Left = 8
    Top = 445
    Width = 51
    Height = 22
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Play'
    Checked = True
    State = cbChecked
    TabOrder = 2
    ExplicitTop = 346
  end
  object StaticText1: TStaticText
    Left = 20
    Top = 20
    Width = 54
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    BorderStyle = sbsSingle
    Caption = '??? FPS'
    TabOrder = 3
  end
  object GLScene1: TGLScene
    Left = 120
    Top = 16
    object Cube1: TGLCube
      Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
    end
    object Cube2: TGLCube
      Material.FrontProperties.Diffuse.Color = {8786063F8786063F0000803F0000803F}
      Material.FrontProperties.Emission.Color = {0000000000000000A1A0203F0000803F}
      Position.Coordinates = {0000404000000000000000000000803F}
      CubeSize = {0000003F0000003F0000003F}
    end
    object Cube3: TGLCube
      Position.Coordinates = {000040400000803F000000000000803F}
      CubeSize = {CDCC4C3ECDCC4C3ECDCC4C3E}
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = Cube1
      Position.Coordinates = {000020410000A040000020410000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 240
    Top = 16
  end
end