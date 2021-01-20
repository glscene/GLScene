object FormFog: TFormFog
  Left = 192
  Top = 119
  Caption = 'Fog'
  ClientHeight = 372
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 548
    Height = 235
    Camera = GLCamera1
    FieldOfView = 133.897399902343800000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 235
    Width = 548
    Height = 137
    Align = alBottom
    TabOrder = 1
    object LFogStart: TLabel
      Left = 8
      Top = 68
      Width = 38
      Height = 13
      Caption = 'fog start'
    end
    object LFogEnd: TLabel
      Left = 10
      Top = 103
      Width = 36
      Height = 13
      Caption = 'fog end'
    end
    object LFogColor: TLabel
      Left = 368
      Top = 20
      Width = 41
      Height = 13
      Caption = 'fog color'
    end
    object SFogColor: TShape
      Left = 440
      Top = 17
      Width = 57
      Height = 16
      OnMouseDown = SFogColorMouseDown
    end
    object LFogDensity: TLabel
      Left = 368
      Top = 84
      Width = 144
      Height = 13
      Caption = 'fog density (for fmExp/fmExp2)'
    end
    object CBFogEnable: TCheckBox
      Left = 10
      Top = 26
      Width = 73
      Height = 17
      Caption = 'fog on/off'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBFogEnableClick
    end
    object EFogStart: TEdit
      Left = 66
      Top = 60
      Width = 57
      Height = 21
      TabOrder = 1
      Text = '-30'
      OnChange = EFogStartChange
    end
    object EFogEnd: TEdit
      Left = 66
      Top = 100
      Width = 57
      Height = 21
      TabOrder = 2
      Text = '30'
      OnChange = EFogStartChange
    end
    object RGFogDistance: TRadioGroup
      Left = 129
      Top = 8
      Width = 105
      Height = 73
      Caption = '[ fog mode ]'
      ItemIndex = 0
      Items.Strings = (
        'fdDefault'
        'fdEyePlane'
        'fdEyeRadial')
      TabOrder = 3
      OnClick = RGFogModeClick
    end
    object RGFogMode: TRadioGroup
      Left = 240
      Top = 8
      Width = 105
      Height = 73
      Caption = '[ fog mode ]'
      ItemIndex = 1
      Items.Strings = (
        'fmLinear'
        'fmExp'
        'fmExp2')
      TabOrder = 4
      OnClick = RGFogModeClick
    end
    object GBTexture: TGroupBox
      Left = 129
      Top = 87
      Width = 217
      Height = 41
      Caption = '[ texture ]'
      TabOrder = 5
      object CBTextureEnabled: TCheckBox
        Left = 8
        Top = 16
        Width = 65
        Height = 17
        Caption = 'enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CBTextureEnabledClick
      end
      object CBTextureIgnoreFog: TCheckBox
        Left = 80
        Top = 16
        Width = 73
        Height = 17
        Caption = 'ignore fog'
        TabOrder = 1
        OnClick = CBTextureIgnoreFogClick
      end
    end
    object CBApplyToBackground: TCheckBox
      Left = 424
      Top = 39
      Width = 105
      Height = 17
      Caption = 'background too ?'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = CBApplyToBackgroundClick
    end
    object EFogDensity: TEdit
      Left = 440
      Top = 103
      Width = 57
      Height = 21
      TabOrder = 7
      Text = '100'
      OnChange = EFogStartChange
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000096430000FA430000C8430000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000C04000008040000000410000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    Left = 48
    Top = 16
  end
  object ColorDialog1: TColorDialog
    Left = 80
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 112
    Top = 16
  end
end
