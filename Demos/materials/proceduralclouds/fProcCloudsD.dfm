object FormClouds: TFormClouds
  Left = 339
  Top = 205
  Caption = 'Procedural Clouds'
  ClientHeight = 508
  ClientWidth = 671
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 411
    Height = 508
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = Camera
    AfterRender = GLSceneViewer1AfterRender
    Buffer.BackgroundColor = clBackground
    FieldOfView = 179.442382812500000000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 411
    Top = 0
    Width = 260
    Height = 508
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 1
    object Label2: TLabel
      Left = 20
      Top = 185
      Width = 42
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Format'
    end
    object Label3: TLabel
      Left = 20
      Top = 225
      Width = 80
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Compression'
    end
    object Label5: TLabel
      Left = 20
      Top = 340
      Width = 74
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Render Size'
    end
    object LAUsedMemory: TLabel
      Left = 20
      Top = 285
      Width = 85
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Used Memory'
    end
    object LARGB32: TLabel
      Left = 20
      Top = 265
      Width = 85
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Used Memory'
    end
    object LACompression: TLabel
      Left = 20
      Top = 305
      Width = 85
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Used Memory'
    end
    object Label4: TLabel
      Left = 20
      Top = 120
      Width = 43
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'MinCut:'
    end
    object Label6: TLabel
      Left = 20
      Top = 90
      Width = 68
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Sharpness:'
    end
    object CloudFileOpenBtn: TSpeedButton
      Left = 19
      Top = 450
      Width = 31
      Height = 26
      Hint = 'Load Cloud File'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        33333FFFFFFFFFFFFFFF000000000000000077777777777777770F7777777777
        77707F3F3333333333370F988888888888707F733FFFFFFFF3370F8800000000
        88707F337777777733370F888888888888707F333FFFFFFFF3370F8800000000
        88707F337777777733370F888888888888707F333333333333370F8888888888
        88707F333333333333370FFFFFFFFFFFFFF07FFFFFFFFFFFFFF7000000000000
        0000777777777777777733333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
      OnClick = CloudFileOpenBtnClick
    end
    object MakeAndSaveCloudNoiseFile: TSpeedButton
      Left = 201
      Top = 450
      Width = 50
      Height = 28
      Hint = 'Make And Save Cloud'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000074120000741200001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00077777F07777
        77F0F07777F077777F077F0000000000F077770330000D030777770330000D03
        07777703300000030777FF03333333330FFF000300000003000077030FFFFF03
        077777030FFFFF03077777030FFFFF03077777030FFFFF0F07777F0000000000
        0777F07777F0777770F7077777F07777770F777777F077777770}
      OnClick = MakeAndSaveCloudNoiseFileClick
    end
    object Label61: TLabel
      Left = 223
      Top = 420
      Width = 7
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '0'
    end
    object LabelFPS: TLabel
      Left = 20
      Top = 30
      Width = 26
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'FPS'
    end
    object CBFormat: TComboBox
      Left = 110
      Top = 180
      Width = 131
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Style = csDropDownList
      TabOrder = 0
      OnChange = CBFormatChange
      Items.Strings = (
        'RGB    (24 bits)'
        'RGBA  (32 bits)'
        'RGB    (16 bits)'
        'RGBA  (16 bits)')
    end
    object CBCompression: TComboBox
      Left = 110
      Top = 220
      Width = 131
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Style = csDropDownList
      TabOrder = 1
      OnChange = CBFormatChange
      Items.Strings = (
        'None'
        'Standard'
        'Nicest'
        'Fastest')
    end
    object RBDefault: TRadioButton
      Left = 20
      Top = 358
      Width = 71
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '100 %'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = CBFormatChange
    end
    object RBDouble: TRadioButton
      Left = 99
      Top = 358
      Width = 72
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '200 %'
      TabOrder = 3
      OnClick = CBFormatChange
    end
    object RBQuad: TRadioButton
      Left = 179
      Top = 358
      Width = 72
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '400 %'
      TabOrder = 4
      OnClick = CBFormatChange
    end
    object CheckBox1: TCheckBox
      Left = 160
      Top = 54
      Width = 91
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Animated'
      TabOrder = 5
    end
    object SpinEdit1: TSpinEdit
      Left = 110
      Top = 120
      Width = 131
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      MaxValue = 255
      MinValue = 0
      TabOrder = 6
      Value = 98
      OnChange = CBFormatChange
    end
    object SpinEdit2: TSpinEdit
      Left = 110
      Top = 90
      Width = 131
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      MaxValue = 99
      MinValue = 1
      TabOrder = 7
      Value = 98
      OnChange = CBFormatChange
    end
    object CheckBox2: TCheckBox
      Left = 20
      Top = 54
      Width = 101
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Seamless'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = CBFormatChange
    end
    object TrackBar1: TTrackBar
      Left = 30
      Top = 380
      Width = 188
      Height = 41
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Min = 1
      Position = 2
      TabOrder = 9
      ThumbLength = 25
      OnChange = TrackBar1Change
    end
    object CloudRandomSeedUsedEdit: TEdit
      Left = 139
      Top = 420
      Width = 61
      Height = 24
      Hint = 'Cloud Random Seed'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 10
      Text = '12345'
    end
    object CloudImageSizeUsedEdit: TEdit
      Left = 119
      Top = 330
      Width = 41
      Height = 24
      Hint = 'Cloud Image Size'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 11
      Text = '128'
    end
    object UseCloudFileCB: TCheckBox
      Left = 19
      Top = 424
      Width = 112
      Height = 21
      Hint = 'Use File'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Use Cloud File'
      TabOrder = 12
    end
    object CloudFileUsedEdit: TEdit
      Left = 69
      Top = 450
      Width = 122
      Height = 24
      HelpContext = 50
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 13
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 16
    object Plane: TGLPlane
      Material.FrontProperties.Ambient.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Diffuse.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Emission.Color = {00000000000000000000000000000000}
      Material.BlendingMode = bmAlphaTest50
      Material.Texture.ImageClassName = 'TGLProcTextureNoise'
      Material.Texture.Image.MinCut = 0
      Material.Texture.Image.NoiseSharpness = 0.990000009536743200
      Material.Texture.Image.Seamless = True
      Material.Texture.Image.NoiseRandSeed = 497075363
      Material.Texture.TextureMode = tmReplace
      Material.Texture.Disabled = False
      Scale.Coordinates = {0000C03F0000C03F0000803F00000000}
      Height = 50.000000000000000000
      Width = 50.000000000000000000
      XTiles = 2
      YTiles = 2
      Style = [psTileTexture]
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 1.000000000000000000
      CameraStyle = csOrthogonal
      Position.Coordinates = {0000000000000000000070410000803F}
      Left = 256
      Top = 160
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 64
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 264
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    Left = 116
    Top = 11
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'rnd'
    Filter = 'Cloud base (*.clb)|*.clb'
    Left = 188
    Top = 13
  end
end
