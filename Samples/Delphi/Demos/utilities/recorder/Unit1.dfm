object Form1: TForm1
  Left = 217
  Top = 94
  Caption = 'Hierarchy and AVI recorder'
  ClientHeight = 410
  ClientWidth = 568
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 568
    Height = 379
    Camera = GLCamera1
    Buffer.BackgroundColor = clBtnShadow
    FieldOfView = 124.358375549316400000
    Align = alClient
    TabOrder = 0
  end
  object TrackBar: TTrackBar
    Left = 0
    Top = 379
    Width = 568
    Height = 31
    Align = alBottom
    Max = 360
    PageSize = 10
    Frequency = 10
    TabOrder = 1
    ThumbLength = 15
    OnChange = TrackBarChange
  end
  object StaticText1: TStaticText
    Left = 20
    Top = 20
    Width = 54
    Height = 20
    BorderStyle = sbsSingle
    Caption = '??? FPS'
    TabOrder = 2
  end
  object Button1: TButton
    Left = 199
    Top = 8
    Width = 132
    Height = 31
    Caption = 'Record to AVI...'
    TabOrder = 3
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 56
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
    Left = 120
    Top = 56
  end
  object AVIRecorder1: TGLAVIRecorder
    GLSceneViewer = GLSceneViewer1
    Width = 320
    Height = 200
    Compressor = acShowDialog
    OnPostProcessEvent = AVIRecorder1PostProcessEvent
    Left = 40
    Top = 120
  end
end
