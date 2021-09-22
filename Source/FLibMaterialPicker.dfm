object GLLibMaterialPickerForm: TGLLibMaterialPickerForm
  Left = 326
  Top = 157
  BorderStyle = bsDialog
  Caption = 'LibMaterial Picker'
  ClientHeight = 261
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 152
    Top = 8
    Width = 78
    Height = 13
    Caption = 'Material Preview'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 88
    Height = 13
    Caption = 'Available Materials'
  end
  object LBMaterials: TListBox
    Left = 8
    Top = 24
    Width = 137
    Height = 223
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBMaterialsClick
    OnDblClick = LBMaterialsDblClick
    OnKeyPress = LBMaterialsKeyPress
  end
  object BBOk: TBitBtn
    Left = 376
    Top = 24
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object BBCancel: TBitBtn
    Left = 376
    Top = 56
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
  object GLSceneViewer: TGLSceneViewer
    Left = 151
    Top = 54
    Width = 210
    Height = 191
    Camera = Camera
    FieldOfView = 71.620002746582030000
    PenAsTouch = False
    TabOrder = 3
  end
  object cbObject: TComboBox
    Left = 151
    Top = 27
    Width = 90
    Height = 21
    ParentShowHint = False
    ShowHint = False
    TabOrder = 4
    Items.Strings = (
      'Cube'
      'Sphere'
      'Cone'
      'Teapot')
  end
  object ComboBox1: TComboBox
    Left = 247
    Top = 27
    Width = 113
    Height = 21
    TabOrder = 5
    Items.Strings = (
      'on a pattern background'
      'on a white background'
      'on a black background'
      'on a blue background'
      'on a red background'
      'on a green background')
  end
  object GLScene: TGLScene
    ObjectsSorting = osNone
    Left = 56
    Top = 48
    object BackgroundSprite: TGLSprite
      Width = 1.000000000000000000
      Height = 1.000000000000000000
      Rotation = 0.000000000000000000
    end
    object World: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Cube: TGLCube
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'LibMaterial'
      end
      object Sphere: TGLSphere
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'LibMaterial'
        Radius = 0.500000000000000000
      end
      object Cone: TGLCone
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'LibMaterial'
        BottomRadius = 0.500000000000000000
        Height = 1.000000000000000000
      end
      object Teapot: TGLTeapot
        Material.MaterialLibrary = GLMaterialLibrary
        Material.LibMaterialName = 'LibMaterial'
      end
    end
    object Light: TGLDummyCube
      CubeSize = 1.000000000000000000
      object LightSource: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {000040400000A040000020410000803F}
        SpotCutOff = 180.000000000000000000
      end
      object FireSphere: TGLSphere
        Radius = 0.300000011920929000
      end
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 132.365310668945300000
      TargetObject = Cube
      Position.Coordinates = {000000400000A040000020410000803F}
    end
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
      end>
    Left = 56
    Top = 128
  end
end
