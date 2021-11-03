object FormCsg: TFormCsg
  Left = 321
  Top = 128
  Caption = 'Constructive Solid Geometry'
  ClientHeight = 545
  ClientWidth = 693
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 693
    Height = 493
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    Buffer.FaceCulling = False
    FieldOfView = 157.067413330078100000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 493
    Width = 693
    Height = 52
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    TabOrder = 1
    object btnClear: TButton
      Left = 10
      Top = 10
      Width = 61
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Clear'
      TabOrder = 0
      OnClick = btnClearClick
    end
    object btnUnion: TButton
      Left = 79
      Top = 10
      Width = 104
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Union A and B'
      TabOrder = 1
      OnClick = btnUnionClick
    end
    object btnSubtractAB: TButton
      Left = 198
      Top = 8
      Width = 93
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Subtract A-B'
      TabOrder = 2
      OnClick = btnSubtractABClick
    end
    object btnSubtractBA: TButton
      Left = 299
      Top = 10
      Width = 94
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Subtract B-A'
      TabOrder = 3
      OnClick = btnSubtractBAClick
    end
    object btnIntersect: TButton
      Left = 400
      Top = 10
      Width = 121
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Intersection A or B'
      TabOrder = 4
      OnClick = btnIntersectClick
    end
    object CheckBox1: TCheckBox
      Left = 571
      Top = 15
      Width = 122
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Solid Result'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = CheckBox1Click
    end
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 8
    object GLCamera1: TGLCamera
      DepthOfView = 5000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000000000000000000096C30000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLDummyCube1: TGLDummyCube
      Scale.Coordinates = {00000040000000400000004000000000}
      CubeSize = 100.000000000000000000
      object GLFreeForm3: TGLFreeForm
        MaterialLibrary = GLMaterialLibrary1
      end
      object GLFreeForm2: TGLFreeForm
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {CFBC3C3EA19E9E3EA19E9E3E0000803F}
        Material.FrontProperties.Shininess = 32
        Material.FrontProperties.Specular.Color = {BEBEBE3E999F1F3F999F1F3F0000803F}
        Position.Coordinates = {0000004200000000000000000000803F}
        Scale.Coordinates = {00002042000020420000204200000000}
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object GLFreeForm1: TGLFreeForm
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {9484843E9484843EDBDEDE3E0000803F}
        Material.FrontProperties.Shininess = 32
        Material.FrontProperties.Specular.Color = {9A99593F9A99593FCDCCCC3D0000803F}
        Scale.Coordinates = {0000A0420000A0420000A04200000000}
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = '1'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {6666663F6666663F6666663F0000803F}
      end
      item
        Name = '2'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {6666E63E6666E63E6666E63E0000803F}
      end>
    Left = 136
    Top = 8
  end
end
