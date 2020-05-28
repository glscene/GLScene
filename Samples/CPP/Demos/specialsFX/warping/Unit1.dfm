object Form1: TForm1
  Left = 239
  Top = 111
  Caption = 'Warping'
  ClientHeight = 365
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 531
    Height = 365
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = GLCamera
    FieldOfView = 179.686050415039100000
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
    TabOrder = 0
    ExplicitWidth = 425
    ExplicitHeight = 272
  end
  object MainMenu1: TMainMenu
    Left = 208
    Top = 16
    object MIFile: TMenuItem
      Caption = 'File'
      object MIOpenImageFile: TMenuItem
        Caption = 'Open image file...'
        ShortCut = 16463
        OnClick = MIOpenImageFileClick
      end
      object MISaveCurrentImage: TMenuItem
        Caption = 'Save current image...'
        ShortCut = 16467
        OnClick = MISaveCurrentImageClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MIExit: TMenuItem
        Caption = 'Exit'
        ShortCut = 32883
        OnClick = MIExitClick
      end
    end
    object MIQuality: TMenuItem
      Caption = 'Quality'
      object N1toomuch1: TMenuItem
        Tag = 2
        Caption = '2 (too much)'
        RadioItem = True
        OnClick = MIQualityOptionClick
      end
      object N4highquality1: TMenuItem
        Tag = 4
        Caption = '4 (high quality)'
        RadioItem = True
        OnClick = MIQualityOptionClick
      end
      object N8mediumquality1: TMenuItem
        Tag = 8
        Caption = '8 (medium quality)'
        RadioItem = True
        OnClick = MIQualityOptionClick
      end
      object N16lowquality1: TMenuItem
        Tag = 16
        Caption = '16 (low quality)'
        Checked = True
        RadioItem = True
        OnClick = MIQualityOptionClick
      end
      object MIQualityOption: TMenuItem
        Tag = 32
        Caption = '32 (super fast)'
        RadioItem = True
        OnClick = MIQualityOptionClick
      end
    end
    object MIRadius: TMenuItem
      Caption = 'Radius'
      object N10small1: TMenuItem
        Tag = 10
        Caption = '10 (small)'
        RadioItem = True
        OnClick = MIRadiusSettingClick
      end
      object N20medium1: TMenuItem
        Tag = 20
        Caption = '20 (medium)'
        Checked = True
        RadioItem = True
        OnClick = MIRadiusSettingClick
      end
      object MIRadiusSetting: TMenuItem
        Tag = 40
        Caption = '40 (large)'
        RadioItem = True
        OnClick = MIRadiusSettingClick
      end
      object N80extra1: TMenuItem
        Tag = 80
        Caption = '80 (extra)'
        RadioItem = True
        OnClick = MIRadiusSettingClick
      end
    end
    object MIEffect: TMenuItem
      Caption = 'Effect'
      object MIZoomEffect: TMenuItem
        Caption = 'Zoom'
        Checked = True
        RadioItem = True
        OnClick = MIZoomEffectClick
      end
      object MISpin: TMenuItem
        Tag = 1
        Caption = 'Spin'
        RadioItem = True
        OnClick = MIZoomEffectClick
      end
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 40
    Top = 80
  end
  object GLScene: TGLScene
    Left = 40
    Top = 16
    object HeightField: TGLHeightField
      Material.Texture.MinFilter = miLinear
      Material.Texture.TextureMode = tmReplace
      Material.Texture.Disabled = False
      Direction.Coordinates = {00000000000000800000803F00000000}
      XSamplingScale.Max = 16.000000000000000000
      XSamplingScale.Step = 8.000000000000000000
      YSamplingScale.Max = 16.000000000000000000
      YSamplingScale.Step = 8.000000000000000000
      Options = [hfoTextureCoordinates, hfoTwoSided]
      OnGetHeight = HeightFieldGetHeight
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 0.500000000000000000
      CameraStyle = csOrthogonal
      Position.Coordinates = {0000803F0000803F000020410000803F}
      Left = 256
      Top = 152
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp|All files (*.*)|*.*'
    Left = 128
    Top = 16
  end
end
