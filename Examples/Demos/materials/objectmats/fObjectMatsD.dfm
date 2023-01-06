object FormMO: TFormMO
  Left = 0
  Top = 0
  Caption = 'Object Materials'
  ClientHeight = 614
  ClientWidth = 906
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 724
    Height = 614
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 154.310821533203100000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 577
  end
  object Panel1: TPanel
    Left = 724
    Top = 0
    Width = 182
    Height = 614
    Align = alRight
    TabOrder = 1
    ExplicitLeft = 730
    ExplicitTop = -8
    object RadioGroup1: TRadioGroup
      Left = 24
      Top = 120
      Width = 145
      Height = 121
      ItemIndex = 0
      Items.Strings = (
        'GLCube'
        'GLHexahedra'
        'GLPlanes'
        'DirectOpenGLCube')
      TabOrder = 0
      OnClick = RadioGroup1Click
    end
    object chbRotate: TCheckBox
      Left = 32
      Top = 111
      Width = 97
      Height = 17
      Caption = 'Rotate'
      TabOrder = 1
    end
  end
  object GLScene1: TGLScene
    Left = 174
    Top = 95
    object GLCamera1: TGLCamera
      DepthOfView = 10000000000.000000000000000000
      FocalLength = 70.000000000000000000
      TargetObject = dcCubes
      Position.Coordinates = {0000404000004040000040400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLPolygon1: TGLPolygon
        Nodes = <>
      end
      object GLActor1: TGLActor
        Interval = 100
      end
    end
    object dcCubes: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCube1: TGLCube
        Material.MaterialLibrary = GLMatLibCube
        Material.LibMaterialName = 'mtRed'
      end
      object GLHexahedron1: TGLHexahedron
        Material.MaterialLibrary = GLMatLibCube
        Material.LibMaterialName = 'mtOrange'
        Position.Coordinates = {0000000000000040000000000000803F}
        Scale.Coordinates = {0000003F0000003F0000003F00000000}
      end
      object dcPlaneCube: TGLDummyCube
        Position.Coordinates = {00000000000000C0000000000000803F}
        OnProgress = dcPlaneCubeProgress
        CubeSize = 1.000000000000000000
        object GLPlaneFront: TGLPlane
          Material.MaterialLibrary = GLMatLibCube
          Material.LibMaterialName = 'mtRed'
          Position.Coordinates = {00000000000000000000003F0000803F}
          Height = 1.000000000000000000
          Width = 1.000000000000000000
        end
        object GLPlaneBack: TGLPlane
          Material.MaterialLibrary = GLMatLibCube
          Material.LibMaterialName = 'mtGreen'
          Direction.Coordinates = {0000000000000000000080BF00000000}
          Position.Coordinates = {0000000000000000000000BF0000803F}
          Height = 1.000000000000000000
          Width = 1.000000000000000000
        end
        object GLPlaneBottom: TGLPlane
          Material.MaterialLibrary = GLMatLibCube
          Material.LibMaterialName = 'mtBlue'
          Direction.Coordinates = {00000000000080BF0000000000000000}
          Position.Coordinates = {00000000000000BF000000000000803F}
          Up.Coordinates = {00000000000000000000803F00000000}
          Height = 1.000000000000000000
          Width = 1.000000000000000000
        end
        object GLPlaneTop: TGLPlane
          Material.MaterialLibrary = GLMatLibCube
          Material.LibMaterialName = 'mtYellow'
          Direction.Coordinates = {000000000000803F0000000000000000}
          Position.Coordinates = {000000000000003F000000000000803F}
          Up.Coordinates = {0000000000000000000080BF00000000}
          Height = 1.000000000000000000
          Width = 1.000000000000000000
        end
        object GLPlaneRight: TGLPlane
          Material.MaterialLibrary = GLMatLibCube
          Material.LibMaterialName = 'mtOrange'
          Direction.Coordinates = {0000803F000000000000000000000000}
          Position.Coordinates = {0000003F00000000000000000000803F}
          Height = 1.000000000000000000
          Width = 1.000000000000000000
        end
        object GLPlaneLeft: TGLPlane
          Material.MaterialLibrary = GLMatLibCube
          Material.LibMaterialName = 'mtViolet'
          Direction.Coordinates = {000080BF000000000000000000000000}
          Position.Coordinates = {000000BF00000000000000000000803F}
          Height = 1.000000000000000000
          Width = 1.000000000000000000
        end
      end
    end
    object dcSpheres: TGLDummyCube
      Position.Coordinates = {0000000000000000000000400000803F}
      CubeSize = 1.000000000000000000
      object Sphere1: TGLSphere
        Radius = 0.500000000000000000
      end
      object ffSphere: TGLFreeForm
        Position.Coordinates = {000000000000C03F000000000000803F}
      end
      object GLMesh1: TGLMesh
        Material.MaterialLibrary = GLMatLibCube
        Material.LibMaterialName = 'mtRed'
        Position.Coordinates = {0000000000000040000000000000803F}
        Scale.Coordinates = {0000003F0000003F0000003F00000000}
        Visible = False
        Mode = mmTriangleStrip
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 564
    Top = 28
  end
  object GLMatLibCube: TGLMaterialLibrary
    Materials = <
      item
        Name = 'mtRed'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      end
      item
        Name = 'mtGreen'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {000000000000003F000000000000803F}
      end
      item
        Name = 'mtBlue'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
      end
      item
        Name = 'mtYellow'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {9A99193FCDCC4C3FACC8483E0000803F}
      end
      item
        Name = 'mtOrange'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000003F000000000000803F}
      end
      item
        Name = 'mtViolet'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
      end>
    Left = 210
    Top = 208
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Object Materials - %FPS'
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
    Left = 104
    Top = 384
  end
end
