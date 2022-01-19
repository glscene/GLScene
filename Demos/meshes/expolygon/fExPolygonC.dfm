object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Extruded Polygon'
  ClientHeight = 444
  ClientWidth = 590
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 17
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 590
    Height = 444
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = Camera
    FieldOfView = 111.908126831054700000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 32
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000FAC30000FAC3000048440000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000FA430000FAC3000048440000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Container: TGLDummyCube
      CubeSize = 100.000000000000000000
    end
    object CameraTarget: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object Camera: TGLCamera
      DepthOfView = 10000.000000000000000000
      FocalLength = 150.000000000000000000
      TargetObject = CameraTarget
      Position.Coordinates = {0000484300007AC4000048430000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Left = 376
      Top = 240
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'MatSurface'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {6666263F0000003F14AEC73E0000803F}
      end
      item
        Name = 'MatInner'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {C3F5683F5C8F423F6666263F0000803F}
      end>
    Left = 144
    Top = 32
  end
end
