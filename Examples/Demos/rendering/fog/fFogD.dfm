object FormFog: TFormFog
  Left = 192
  Top = 119
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'Fog'
  ClientHeight = 651
  ClientWidth = 967
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 168
  TextHeight = 24
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 967
    Height = 412
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Camera = GLCamera1
    FieldOfView = 152.714172363281300000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 412
    Width = 967
    Height = 239
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alBottom
    TabOrder = 1
    object LFogStart: TLabel
      Left = 14
      Top = 119
      Width = 64
      Height = 24
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'fog start'
    end
    object LFogEnd: TLabel
      Left = 18
      Top = 181
      Width = 64
      Height = 24
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'fog end'
    end
    object LFogColor: TLabel
      Left = 644
      Top = 35
      Width = 73
      Height = 24
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'fog color'
    end
    object SFogColor: TShape
      Left = 770
      Top = 29
      Width = 99
      Height = 28
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      OnMouseDown = SFogColorMouseDown
    end
    object LFogDensity: TLabel
      Left = 644
      Top = 147
      Width = 256
      Height = 24
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'fog density (for fmExp/fmExp2)'
    end
    object CBFogEnable: TCheckBox
      Left = 18
      Top = 46
      Width = 128
      Height = 30
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'fog on/off'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBFogEnableClick
    end
    object EFogStart: TEdit
      Left = 116
      Top = 105
      Width = 100
      Height = 32
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      TabOrder = 1
      Text = '-30'
      OnChange = EFogStartChange
    end
    object EFogEnd: TEdit
      Left = 116
      Top = 175
      Width = 100
      Height = 32
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      TabOrder = 2
      Text = '30'
      OnChange = EFogStartChange
    end
    object RGFogDistance: TRadioGroup
      Left = 225
      Top = 14
      Width = 185
      Height = 127
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
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
      Left = 420
      Top = 14
      Width = 183
      Height = 127
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
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
      Left = 225
      Top = 153
      Width = 381
      Height = 71
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = '[ texture ]'
      TabOrder = 5
      object CBTextureEnabled: TCheckBox
        Left = 14
        Top = 28
        Width = 113
        Height = 29
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CBTextureEnabledClick
      end
      object CBTextureIgnoreFog: TCheckBox
        Left = 140
        Top = 28
        Width = 127
        Height = 29
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'ignore fog'
        TabOrder = 1
        OnClick = CBTextureIgnoreFogClick
      end
    end
    object CBApplyToBackground: TCheckBox
      Left = 742
      Top = 69
      Width = 183
      Height = 29
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'background too ?'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = CBApplyToBackgroundClick
    end
    object EFogDensity: TEdit
      Left = 770
      Top = 181
      Width = 99
      Height = 32
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
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
