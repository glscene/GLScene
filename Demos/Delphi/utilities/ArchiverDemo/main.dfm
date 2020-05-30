object Form1: TForm1
  Left = 333
  Top = 166
  Caption = 'Archive Loader'
  ClientHeight = 483
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 629
    Height = 483
    Camera = GLCamera
    VSync = vsmSync
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 156.605575561523400000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 128
    Top = 73
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000A0400000A0400000A0400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 0.500000000000000000
          LinearAttenuation = 0.100000001490116100
          Specular.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object GLPlane1: TGLPlane
      Material.FrontProperties.Diffuse.Color = {CFBC3C3EA19E9E3ECFBC3C3E0000803F}
      Material.FrontProperties.Shininess = 8
      Material.FrontProperties.Specular.Color = {6666663F6666663F6666663F0000803F}
      Material.DepthProperties.DepthCompareFunction = cfLess
      Material.Texture.Disabled = False
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      PitchAngle = 90.000000000000000000
      ShowAxes = True
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      Height = 10.000000000000000000
      Width = 10.000000000000000000
      XTiles = 20
      YTiles = 20
      Style = [psTileTexture]
    end
    object GLFreeForm: TGLFreeForm
      Position.Coordinates = {000080BF00000000000000000000803F}
      MaterialLibrary = GLMaterialLibrary1
    end
    object GLFreeForm1: TGLFreeForm
      Position.Coordinates = {0000803F00000000000000000000803F}
      Scale.Coordinates = {0000003F0000003F0000003F00000000}
      MaterialLibrary = GLMaterialLibrary1
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
      end>
    Left = 127
    Top = 128
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Mode = cmApplicationIdle
    OnProgress = GLCadencer1Progress
    Left = 127
    Top = 180
  end
  object GLSArchiveManager1: TGLSArchiveManager
    Archives = <
      item
        Name = 'LibArchive'
      end>
    Left = 127
    Top = 232
  end
end
