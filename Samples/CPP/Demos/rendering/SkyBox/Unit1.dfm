object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sky Box'
  ClientHeight = 460
  ClientWidth = 572
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 572
    Height = 460
    Camera = GLCamera1
    Buffer.BackgroundColor = 7168
    Buffer.AmbientColor.Color = {0000803F0000803F0000803F0000803F}
    FieldOfView = 160.268386840820300000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 24
    object GLSkyBox1: TGLSkyBox
      Direction.Coordinates = {9598A23144F7DFB20000803F00000000}
      Up.Coordinates = {1DB356B30000803FB3FA87B300000000}
      MaterialLibrary = GLMaterialLibrary1
      MatNameTop = 'Top'
      MatNameBottom = 'Bottom'
      MatNameLeft = 'Left'
      MatNameRight = 'Right'
      MatNameFront = 'Front'
      MatNameBack = 'Back'
      MatNameClouds = 'Clouds'
      CloudsPlaneOffset = 0.119999997317791000
      CloudsPlaneSize = 2.000000000000000000
      object GLSphere1: TGLSphere
        Material.MaterialLibrary = GLMaterialLibrary1
        Direction.Coordinates = {18EBC2BE6823FE3E49BA47BF00000000}
        PitchAngle = 86.500000000000000000
        Position.Coordinates = {000000C000008040000080BF0000803F}
        TurnAngle = 10.500000000000000000
        Up.Coordinates = {CE00D4BE9A8D58BF0519ACBE00000000}
        Radius = 1.000000000000000000
        object GLSphere2: TGLSphere
          Material.MaterialLibrary = GLMaterialLibrary1
          Direction.Coordinates = {000000000000803F0000000000000000}
          PitchAngle = -8.000000000000000000
          Position.Coordinates = {0000000000000000000000400000803F}
          TurnAngle = 91.500000000000000000
          Up.Coordinates = {0000000000000000000080BF00000000}
          Radius = 0.300000011920929000
        end
      end
    end
    object GLSkyBox2: TGLSkyBox
      Direction.Coordinates = {00000000000000800000803F00000000}
      MaterialLibrary = GLMaterialLibrary1
      MatNameClouds = 'Clouds'
      CloudsPlaneOffset = 0.100000001490116100
      CloudsPlaneSize = 4.000000000000000000
    end
    object Castle: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCube1: TGLCube
        Material.MaterialLibrary = GLMaterialLibrary1
        Position.Coordinates = {0BD43940FFFF7FC018230B400000803F}
        CubeSize = {0000803F000000420000803F}
        object GLCube2: TGLCube
          Material.MaterialLibrary = GLMaterialLibrary1
          Position.Coordinates = {000040C0000000BFCDCCCC3D0000803F}
          CubeSize = {0000C0400000803F0000803F}
        end
      end
      object GLCube11: TGLCube
        Material.MaterialLibrary = GLMaterialLibrary1
        Direction.Coordinates = {000080BF000000001AE1F7B300000000}
        Position.Coordinates = {F42B46C0FFFF7FC018230B400000803F}
        TurnAngle = -90.000000000000000000
        CubeSize = {0000803F000000420000803F}
        object GLCube21: TGLCube
          Material.MaterialLibrary = GLMaterialLibrary1
          Position.Coordinates = {000040C0000000BFCDCCCC3D0000803F}
          CubeSize = {0000C0400000803F0000803F}
        end
      end
      object GLCube111: TGLCube
        Material.MaterialLibrary = GLMaterialLibrary1
        Direction.Coordinates = {0000803F00000000B28FF03200000000}
        Position.Coordinates = {0CD43940FFFF7FC0E8DC74C00000803F}
        TurnAngle = 90.000000000000000000
        CubeSize = {0000803F000000420000803F}
        object GLCube211: TGLCube
          Material.MaterialLibrary = GLMaterialLibrary1
          Position.Coordinates = {000040C0000000BFCDCCCC3D0000803F}
          CubeSize = {0000C0400000803F0000803F}
        end
      end
      object GLCube112: TGLCube
        Material.MaterialLibrary = GLMaterialLibrary1
        Direction.Coordinates = {647F2B3300000000000080BF00000000}
        Position.Coordinates = {F42B46C0FFFF7FC0E8DC74C00000803F}
        TurnAngle = -180.000000000000000000
        CubeSize = {0000803F000000420000803F}
        object GLCube212: TGLCube
          Material.MaterialLibrary = GLMaterialLibrary1
          Position.Coordinates = {000040C0000000BFCDCCCC3D0000803F}
          CubeSize = {0000C0400000803F0000803F}
        end
      end
    end
    object GLLightSource1: TGLLightSource
      Ambient.Color = {BEC0403FBEC0403FBEC0403F0000803F}
      ConstAttenuation = 1.000000000000000000
      QuadraticAttenuation = 0.000099999997473788
      Position.Coordinates = {000088C100003042C0C6B5420000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
      object GLLensFlare1: TGLLensFlare
        Seed = 1465
        FlareIsNotOccluded = True
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 300.000000000000000000
      FocalLength = 40.000000000000000000
      NearPlaneBias = 0.200000002980232200
      Position.Coordinates = {61EB8D3FD98541C02CCF94C00000803F}
      Direction.Coordinates = {E2CB26BF5D9A063F40FD0B3F00000000}
      Up.Coordinates = {A633CE3EC0C1593F8711ADBE00000000}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 112
    Top = 24
  end
  object GLNavigator1: TGLNavigator
    VirtualUp.Coordinates = {000000000000803F000000000000803F}
    MovingObject = GLCamera1
    UseVirtualUp = True
    AutoUpdateObject = True
    MaxAngle = 90.000000000000000000
    MinAngle = -90.000000000000000000
    Left = 208
    Top = 24
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 32
    Top = 80
  end
  object GLUserInterface1: TGLUserInterface
    MouseSpeed = 16.000000000000000000
    GLNavigator = GLNavigator1
    Left = 296
    Top = 24
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'GLScene SkyBox - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 296
    Top = 88
  end
end
