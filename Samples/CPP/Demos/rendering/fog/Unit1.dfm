object Form1: TForm1
  Left = 293
  Top = 145
  Caption = 'Fog'
  ClientHeight = 452
  ClientWidth = 598
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 598
    Height = 312
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = GLCamera1
    FieldOfView = 144.457351684570300000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 312
    Width = 598
    Height = 140
    Align = alBottom
    TabOrder = 1
    object LFogStart: TLabel
      Left = 16
      Top = 49
      Width = 38
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'fog start'
    end
    object LFogEnd: TLabel
      Left = 16
      Top = 81
      Width = 36
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'fog end'
    end
    object SFogColor: TShape
      Left = 480
      Top = 101
      Width = 57
      Height = 16
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      OnMouseDown = SFogColorMouseDown
    end
    object LFogColor: TLabel
      Left = 409
      Top = 104
      Width = 41
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'fog color'
    end
    object LFogDensity: TLabel
      Left = 409
      Top = 43
      Width = 144
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'fog density (for fmExp/fmExp2)'
    end
    object CBFogEnable: TCheckBox
      Left = 16
      Top = 12
      Width = 73
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'fog on/off'
      TabOrder = 0
      OnClick = CBFogEnableClick
    end
    object EFogStart: TEdit
      Left = 80
      Top = 41
      Width = 57
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 1
      Text = '-30'
      OnChange = EFogStartChange
    end
    object EFogEnd: TEdit
      Left = 80
      Top = 72
      Width = 57
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 2
      Text = '30'
      OnChange = EFogStartChange
    end
    object GBTexture: TGroupBox
      Left = 156
      Top = 9
      Width = 232
      Height = 120
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '[ texture ]'
      TabOrder = 3
      object CBTextureEnabled: TCheckBox
        Left = 24
        Top = 15
        Width = 65
        Height = 17
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CBTextureEnabledClick
      end
      object CBTextureIgnoreFog: TCheckBox
        Left = 119
        Top = 13
        Width = 73
        Height = 17
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'ignore fog'
        TabOrder = 1
        OnClick = CBTextureIgnoreFogClick
      end
      object RGFogDistance: TRadioGroup
        Left = 2
        Top = 36
        Width = 105
        Height = 73
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = '[ fog mode ]'
        ItemIndex = 0
        Items.Strings = (
          'fdDefault'
          'fdEyePlane'
          'fdEyeRadial')
        TabOrder = 2
        OnClick = RGFogModeClick
      end
      object RGFogMode: TRadioGroup
        Left = 112
        Top = 40
        Width = 105
        Height = 73
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = '[ fog mode ]'
        ItemIndex = 0
        Items.Strings = (
          'fmLinear'
          'fmExp'
          'fmExp2')
        TabOrder = 3
        OnClick = RGFogModeClick
      end
    end
    object CBApplyToBackground: TCheckBox
      Left = 424
      Top = 12
      Width = 105
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'background too ?'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CBApplyToBackgroundClick
    end
    object EFogDensity: TEdit
      Left = 480
      Top = 68
      Width = 57
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 5
      Text = '100'
      OnChange = EFogStartChange
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 24
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
    Left = 40
    Top = 80
  end
  object ColorDialog1: TColorDialog
    Left = 152
    Top = 24
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 152
    Top = 80
  end
end
