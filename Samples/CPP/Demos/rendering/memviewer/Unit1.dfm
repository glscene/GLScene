object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Memory Viewer'
  ClientHeight = 383
  ClientWidth = 575
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 440
    Height = 383
    Camera = GLCamera1
    AfterRender = GLSceneViewer1AfterRender
    Buffer.BackgroundColor = clGray
    FieldOfView = 103.857528686523400000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 440
    Top = 0
    Width = 135
    Height = 383
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 44
      Top = 8
      Width = 60
      Height = 19
      Caption = 'Options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 33
      Top = 48
      Width = 89
      Height = 13
      Caption = 'Texture framerate'
    end
    object LabelFPS: TLabel
      Left = 48
      Top = 264
      Width = 20
      Height = 14
      Caption = 'FPS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object RB1to1: TRadioButton
      Tag = 1
      Left = 55
      Top = 80
      Width = 41
      Height = 17
      Caption = '1:1'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RB1to1Click
    end
    object RB1to2: TRadioButton
      Tag = 2
      Left = 55
      Top = 112
      Width = 41
      Height = 17
      Caption = '1:2'
      TabOrder = 1
    end
    object RB1to10: TRadioButton
      Tag = 10
      Left = 55
      Top = 144
      Width = 41
      Height = 17
      Caption = '1:10'
      TabOrder = 2
    end
    object CheckBox1: TCheckBox
      Left = 55
      Top = 200
      Width = 57
      Height = 17
      Caption = 'VSync'
      TabOrder = 3
      OnClick = CheckBox1Click
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 232
    Top = 16
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 16
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Cube1: TGLCube
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 150.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000A040000080400000A0400000803F}
    end
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Camera = GLCamera1
    Buffer.BackgroundColor = clRed
    Left = 120
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 80
  end
end
