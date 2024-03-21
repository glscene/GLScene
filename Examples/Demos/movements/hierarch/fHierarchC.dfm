object FormHierarch: TFormHierarch
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Hierarchy'
  ClientHeight = 774
  ClientWidth = 992
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnResize = FormResize
  PixelsPerInch = 168
  DesignSize = (
    992
    774)
  TextHeight = 23
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 992
    Height = 730
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    FieldOfView = 149.356979370117200000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object TrackBar: TTrackBar
    Left = 0
    Top = 730
    Width = 992
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    Max = 360
    PageSize = 10
    Frequency = 10
    TabOrder = 1
    ThumbLength = 26
    OnChange = TrackBarChange
  end
  object CBPlay: TCheckBox
    Left = 457
    Top = 42
    Width = 72
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akBottom]
    Caption = 'Play'
    TabOrder = 2
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 48
    object Cube1: TGLCube
      Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 2.000000000000000000
      object Cube2: TGLCube
        Material.FrontProperties.Diffuse.Color = {8786063F8786063F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {0000000000000000A1A0203F0000803F}
        Position.Coordinates = {0000404000000000000000000000803F}
        CubeSize = {0000003F0000003F0000003F}
        object DummyCube2: TGLDummyCube
          Direction.Coordinates = {00000000F304353FF304353F00000000}
          Up.Coordinates = {00000000F304353FF30435BF00000000}
          CubeSize = 1.000000000000000000
          object Cube3: TGLCube
            Position.Coordinates = {000000000000803F000000000000803F}
            CubeSize = {CDCC4C3ECDCC4C3ECDCC4C3E}
          end
        end
      end
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
    Left = 16
    Top = 88
  end
end
