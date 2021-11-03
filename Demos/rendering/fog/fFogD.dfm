object FormFog: TFormFog
  Left = 192
  Top = 119
  Caption = 'Fog'
  ClientHeight = 465
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 685
    Height = 294
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    FieldOfView = 142.429962158203100000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 294
    Width = 685
    Height = 171
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    TabOrder = 1
    object LFogStart: TLabel
      Left = 10
      Top = 85
      Width = 47
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'fog start'
    end
    object LFogEnd: TLabel
      Left = 13
      Top = 129
      Width = 45
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'fog end'
    end
    object LFogColor: TLabel
      Left = 460
      Top = 25
      Width = 52
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'fog color'
    end
    object SFogColor: TShape
      Left = 550
      Top = 21
      Width = 71
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      OnMouseDown = SFogColorMouseDown
    end
    object LFogDensity: TLabel
      Left = 460
      Top = 105
      Width = 179
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'fog density (for fmExp/fmExp2)'
    end
    object CBFogEnable: TCheckBox
      Left = 13
      Top = 33
      Width = 91
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'fog on/off'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBFogEnableClick
    end
    object EFogStart: TEdit
      Left = 83
      Top = 75
      Width = 71
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 1
      Text = '-30'
      OnChange = EFogStartChange
    end
    object EFogEnd: TEdit
      Left = 83
      Top = 125
      Width = 71
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 2
      Text = '30'
      OnChange = EFogStartChange
    end
    object RGFogDistance: TRadioGroup
      Left = 161
      Top = 10
      Width = 132
      Height = 91
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
      Left = 300
      Top = 10
      Width = 131
      Height = 91
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
      Left = 161
      Top = 109
      Width = 272
      Height = 51
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '[ texture ]'
      TabOrder = 5
      object CBTextureEnabled: TCheckBox
        Left = 10
        Top = 20
        Width = 81
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CBTextureEnabledClick
      end
      object CBTextureIgnoreFog: TCheckBox
        Left = 100
        Top = 20
        Width = 91
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'ignore fog'
        TabOrder = 1
        OnClick = CBTextureIgnoreFogClick
      end
    end
    object CBApplyToBackground: TCheckBox
      Left = 530
      Top = 49
      Width = 131
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'background too ?'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = CBApplyToBackgroundClick
    end
    object EFogDensity: TEdit
      Left = 550
      Top = 129
      Width = 71
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
    Left = 88
    Top = 16
  end
  object ColorDialog1: TColorDialog
    Left = 168
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 272
    Top = 16
  end
end
