object FormNewtonSimple: TFormNewtonSimple
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Newton Simple Spawn'
  ClientHeight = 858
  ClientWidth = 1299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 168
  TextHeight = 23
  object GLSceneViewer1: TGLSceneViewer
    Left = 184
    Top = 0
    Width = 1115
    Height = 858
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 166.704330444335900000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 184
    Height = 858
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    Caption = ' '
    TabOrder = 1
    object btnAddCube: TButton
      Left = 25
      Top = 14
      Width = 131
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'AddCube'
      TabOrder = 0
      OnClick = btnAddCubeClick
    end
    object btnAddSphere: TButton
      Left = 25
      Top = 68
      Width = 131
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'AddSphere'
      TabOrder = 1
      OnClick = btnAddSphereClick
    end
    object btnAddCone: TButton
      Left = 25
      Top = 123
      Width = 131
      Height = 43
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'AddCone'
      TabOrder = 2
      OnClick = btnAddConeClick
    end
    object btnAddCylinder: TButton
      Left = 25
      Top = 177
      Width = 131
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'AddCylinder'
      TabOrder = 3
      OnClick = btnAddCylinderClick
    end
    object btnAddCapsule: TButton
      Left = 25
      Top = 231
      Width = 131
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'AddCapsule'
      TabOrder = 4
      OnClick = btnAddCapsuleClick
    end
    object btnRemoveAll: TButton
      Left = 25
      Top = 285
      Width = 131
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Remove All'
      TabOrder = 5
      OnClick = btnRemoveAllClick
    end
  end
  object GLScene1: TGLScene
    Left = 128
    Top = 8
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Floor
      Position.Coordinates = {0000000000004040000020C10000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object Floor: TGLCube
      BehavioursData = {
        0458434F4C02010201060C54474C4E47445374617469630200060A4E47442053
        746174696302000201060D474C4E47444D616E61676572310800090500000000
        000AD7A3F83F1200000000}
      CubeSize = {000020410000003F00002041}
    end
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000004040000000000000803F}
      CubeSize = 1.000000000000000000
    end
    object GLResolutionIndependantHUDText1: TGLResolutionIndependantHUDText
      Position.Coordinates = {0000003F6666663F000000000000803F}
      Text = 'Bodycount:=1'
      Rotation = 0.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 216
    Top = 8
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Newton Simple Spawn - %FPS'
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
    Left = 216
    Top = 72
  end
  object GLNGDManager1: TGLNGDManager
    NewtonSurfaceItem = <>
    NewtonSurfacePair = <>
    NewtonJoint = <>
    Left = 472
    Top = 16
  end
end
