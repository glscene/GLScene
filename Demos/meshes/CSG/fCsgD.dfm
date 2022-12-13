object FormCsg: TFormCsg
  Left = 321
  Top = 128
  Caption = 'Constructive Solid Geometry'
  ClientHeight = 542
  ClientWidth = 726
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 544
    Height = 542
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    Buffer.FaceCulling = False
    FieldOfView = 159.092758178710900000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object PanelLeft: TPanel
    Left = 544
    Top = 0
    Width = 182
    Height = 542
    Align = alRight
    TabOrder = 1
    object chbSolidResult: TCheckBox
      Left = 48
      Top = 324
      Width = 97
      Height = 17
      Caption = 'Solid Result'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chbSolidResultClick
    end
    object btnReset: TButton
      Left = 48
      Top = 371
      Width = 49
      Height = 25
      Caption = 'Reset'
      TabOrder = 1
      OnClick = btnResetClick
    end
    object gbVisibility: TGroupBox
      Left = 16
      Top = 210
      Width = 153
      Height = 92
      Caption = 'Visibility'
      TabOrder = 2
      object chbA: TCheckBox
        Left = 32
        Top = 18
        Width = 97
        Height = 17
        Caption = 'Object A'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chbClick
      end
      object chbB: TCheckBox
        Left = 32
        Top = 41
        Width = 97
        Height = 17
        Caption = 'Object B'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = chbClick
      end
      object chbC: TCheckBox
        Left = 32
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Result'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = chbClick
      end
    end
    object rgOperation: TRadioGroup
      Left = 16
      Top = 16
      Width = 153
      Height = 169
      Caption = 'Operation'
      ItemIndex = 0
      Items.Strings = (
        'Union A and B'
        'Subtract A-B'
        'Subtract B-A'
        'Intersect A and B')
      TabOrder = 3
      OnClick = rgOperationClick
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
      object FF_A: TGLFreeForm
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {9484843E9484843EDBDEDE3E0000803F}
        Material.FrontProperties.Shininess = 32
        Material.FrontProperties.Specular.Color = {9A99593F9A99593FCDCCCC3D0000803F}
        Scale.Coordinates = {0000A0420000A0420000A04200000000}
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object FF_B: TGLFreeForm
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {CFBC3C3EA19E9E3EA19E9E3E0000803F}
        Material.FrontProperties.Shininess = 32
        Material.FrontProperties.Specular.Color = {BEBEBE3E999F1F3F999F1F3F0000803F}
        Position.Coordinates = {0000004200000000000000000000803F}
        Scale.Coordinates = {00002042000020420000204200000000}
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
      object FF_C: TGLFreeForm
        MaterialLibrary = GLMaterialLibrary1
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
