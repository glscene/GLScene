object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Hierarchy'
  ClientHeight = 442
  ClientWidth = 567
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnResize = FormResize
  DesignSize = (
    567
    442)
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 567
    Height = 417
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    FieldOfView = 128.753723144531300000
    Align = alClient
    TabOrder = 0
  end
  object TrackBar: TTrackBar
    Left = 0
    Top = 417
    Width = 567
    Height = 25
    Align = alBottom
    Max = 360
    PageSize = 10
    Frequency = 10
    TabOrder = 1
    ThumbLength = 15
    OnChange = TrackBarChange
    ExplicitTop = 388
    ExplicitWidth = 544
  end
  object CBPlay: TCheckBox
    Left = 5
    Top = 390
    Width = 41
    Height = 17
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
