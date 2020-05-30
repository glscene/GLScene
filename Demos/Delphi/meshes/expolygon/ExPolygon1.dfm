object Form1: TForm1
  Left = 256
  Top = 188
  Caption = 'Extruded Polygon'
  ClientHeight = 355
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 512
    Height = 355
    Camera = Camera
    FieldOfView = 99.599670410156250000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
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
