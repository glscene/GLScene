object Form1: TForm1
  Left = 202
  Top = 116
  Caption = 'Procedural Clouds'
  ClientHeight = 337
  ClientWidth = 524
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = CBFormatChange
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 316
    Height = 337
    Camera = GLCamera1
    AfterRender = GLSceneViewer1AfterRender
    FieldOfView = 179.274749755859400000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 316
    Top = 0
    Width = 208
    Height = 337
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 1
    object Label2: TLabel
      Left = 16
      Top = 116
      Width = 32
      Height = 13
      Caption = 'Format'
    end
    object Label3: TLabel
      Left = 16
      Top = 148
      Width = 60
      Height = 13
      Caption = 'Compression'
    end
    object Label5: TLabel
      Left = 16
      Top = 240
      Width = 58
      Height = 13
      Caption = 'Render Size'
    end
    object LAUsedMemory: TLabel
      Left = 16
      Top = 196
      Width = 65
      Height = 13
      Caption = 'Used Memory'
    end
    object LARGB32: TLabel
      Left = 16
      Top = 180
      Width = 65
      Height = 13
      Caption = 'Used Memory'
    end
    object LACompression: TLabel
      Left = 16
      Top = 212
      Width = 65
      Height = 13
      Caption = 'Used Memory'
    end
    object Label4: TLabel
      Left = 16
      Top = 64
      Width = 36
      Height = 13
      Caption = 'MinCut:'
    end
    object Label6: TLabel
      Left = 16
      Top = 40
      Width = 53
      Height = 13
      Caption = 'Sharpness:'
    end
    object CBFormat: TComboBox
      Left = 88
      Top = 112
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
      Top = 144
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
      Left = 88
      Top = 238
      Width = 57
      Height = 17
      Caption = '100 %'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = CBFormatChange
    end
    object RBDouble: TRadioButton
      Left = 87
      Top = 262
      Width = 58
      Height = 17
      Caption = '200 %'
      TabOrder = 3
      OnClick = CBFormatChange
    end
    object RBQuad: TRadioButton
      Left = 87
      Top = 286
      Width = 58
      Height = 17
      Caption = '400 %'
      TabOrder = 4
      OnClick = CBFormatChange
    end
    object CheckBox1: TCheckBox
      Left = 120
      Top = 88
      Width = 73
      Height = 17
      Caption = 'Animated'
      TabOrder = 5
    end
    object SpinEdit1: TSpinEdit
      Left = 88
      Top = 64
      Width = 105
      Height = 22
      MaxValue = 255
      MinValue = 120
      TabOrder = 6
      Value = 120
      OnChange = CBFormatChange
    end
    object SpinEdit2: TSpinEdit
      Left = 88
      Top = 40
      Width = 105
      Height = 22
      MaxValue = 90
      MinValue = 0
      TabOrder = 7
      Value = 90
      OnChange = CBFormatChange
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 88
      Width = 81
      Height = 17
      Caption = 'Seamless'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = CBFormatChange
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 16
    object GLPlane1: TGLPlane
      Material.Texture.ImageClassName = 'TGLProcTextureNoise'
      Material.Texture.Image.MinCut = 0
      Material.Texture.Image.NoiseSharpness = 0.990000009536743200
      Material.Texture.Image.Seamless = False
      Material.Texture.Image.NoiseRandSeed = 371356655
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
    Top = 16
  end
end
