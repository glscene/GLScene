object Form1: TForm1
  Left = 339
  Top = 205
  Caption = 'Procedural Clouds'
  ClientHeight = 406
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 329
    Height = 406
    Camera = GLCamera1
    AfterRender = GLSceneViewer1AfterRender
    Buffer.BackgroundColor = clBackground
    FieldOfView = 179.303405761718800000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 329
    Top = 0
    Width = 208
    Height = 406
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 1
    object Label2: TLabel
      Left = 16
      Top = 148
      Width = 32
      Height = 13
      Caption = 'Format'
    end
    object Label3: TLabel
      Left = 16
      Top = 180
      Width = 60
      Height = 13
      Caption = 'Compression'
    end
    object Label5: TLabel
      Left = 16
      Top = 272
      Width = 58
      Height = 13
      Caption = 'Render Size'
    end
    object LAUsedMemory: TLabel
      Left = 16
      Top = 228
      Width = 65
      Height = 13
      Caption = 'Used Memory'
    end
    object LARGB32: TLabel
      Left = 16
      Top = 212
      Width = 65
      Height = 13
      Caption = 'Used Memory'
    end
    object LACompression: TLabel
      Left = 16
      Top = 244
      Width = 65
      Height = 13
      Caption = 'Used Memory'
    end
    object Label4: TLabel
      Left = 16
      Top = 96
      Width = 36
      Height = 13
      Caption = 'MinCut:'
    end
    object Label6: TLabel
      Left = 16
      Top = 72
      Width = 53
      Height = 13
      Caption = 'Sharpness:'
    end
    object CloudFileOpenBtn: TSpeedButton
      Left = 15
      Top = 360
      Width = 25
      Height = 21
      Hint = 'Load Cloud File'
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
      Left = 161
      Top = 360
      Width = 40
      Height = 22
      Hint = 'Make And Save Cloud'
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
      Left = 178
      Top = 336
      Width = 6
      Height = 13
      Caption = '0'
    end
    object LabelFPS: TLabel
      Left = 16
      Top = 24
      Width = 20
      Height = 13
      Caption = 'FPS'
    end
    object CBFormat: TComboBox
      Left = 88
      Top = 144
      Width = 105
      Height = 21
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
      Left = 88
      Top = 176
      Width = 105
      Height = 21
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
      Left = 16
      Top = 286
      Width = 57
      Height = 17
      Caption = '100 %'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = CBFormatChange
    end
    object RBDouble: TRadioButton
      Left = 79
      Top = 286
      Width = 58
      Height = 17
      Caption = '200 %'
      TabOrder = 3
      OnClick = CBFormatChange
    end
    object RBQuad: TRadioButton
      Left = 143
      Top = 286
      Width = 58
      Height = 17
      Caption = '400 %'
      TabOrder = 4
      OnClick = CBFormatChange
    end
    object CheckBox1: TCheckBox
      Left = 120
      Top = 120
      Width = 73
      Height = 17
      Caption = 'Animated'
      TabOrder = 5
    end
    object SpinEdit1: TSpinEdit
      Left = 88
      Top = 96
      Width = 105
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 6
      Value = 98
      OnChange = CBFormatChange
    end
    object SpinEdit2: TSpinEdit
      Left = 88
      Top = 72
      Width = 105
      Height = 22
      MaxValue = 99
      MinValue = 1
      TabOrder = 7
      Value = 98
      OnChange = CBFormatChange
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 120
      Width = 81
      Height = 17
      Caption = 'Seamless'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = CBFormatChange
    end
    object TrackBar1: TTrackBar
      Left = 24
      Top = 304
      Width = 150
      Height = 33
      Min = 1
      Position = 2
      TabOrder = 9
      OnChange = TrackBar1Change
    end
    object CloudRandomSeedUsedEdit: TEdit
      Left = 111
      Top = 336
      Width = 49
      Height = 21
      Hint = 'Cloud Random Seed'
      TabOrder = 10
      Text = '12345'
    end
    object CloudImageSizeUsedEdit: TEdit
      Left = 95
      Top = 264
      Width = 33
      Height = 21
      Hint = 'Cloud Image Size'
      TabOrder = 11
      Text = '128'
    end
    object UseCloudFileCB: TCheckBox
      Left = 15
      Top = 339
      Width = 90
      Height = 17
      Hint = 'Use File'
      Caption = 'Use Cloud File'
      TabOrder = 12
    end
    object CloudFileUsedEdit: TEdit
      Left = 55
      Top = 360
      Width = 98
      Height = 21
      HelpContext = 50
      TabOrder = 13
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 16
    object GLPlane1: TGLPlane
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
      Height = 50.000000000000000000
      Width = 50.000000000000000000
      XTiles = 2
      YTiles = 2
      Style = [psTileTexture]
    end
    object GLCamera1: TGLCamera
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
