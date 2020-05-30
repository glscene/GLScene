object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Constructive Solid Geometry'
  ClientHeight = 420
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 569
    Height = 378
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    Buffer.FaceCulling = False
    FieldOfView = 150.363708496093800000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 378
    Width = 569
    Height = 42
    Align = alBottom
    TabOrder = 1
    object ButtonClear: TButton
      Left = 8
      Top = 8
      Width = 49
      Height = 25
      Caption = 'Clear'
      TabOrder = 0
      OnClick = ButtonClearClick
    end
    object Button2: TButton
      Left = 63
      Top = 8
      Width = 83
      Height = 25
      Caption = 'Union A and B'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 156
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Subtract A-B'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 239
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Subtract B-A'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 320
      Top = 8
      Width = 97
      Height = 25
      Caption = 'Intersection A or B'
      TabOrder = 4
      OnClick = Button5Click
    end
    object CheckBox1: TCheckBox
      Left = 457
      Top = 12
      Width = 97
      Height = 17
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
      Up.Coordinates = {000000000000803F0000008000000000}
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
