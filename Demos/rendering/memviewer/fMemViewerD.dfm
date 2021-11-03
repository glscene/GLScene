object FormMemViewer: TFormMemViewer
  Left = 173
  Top = 99
  Caption = 'Memory Viewer'
  ClientHeight = 381
  ClientWidth = 596
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Arial'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 428
    Height = 381
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    AfterRender = GLSceneViewer1AfterRender
    Buffer.BackgroundColor = clGray
    FieldOfView = 103.566101074218800000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 428
    Top = 0
    Width = 168
    Height = 381
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 55
      Top = 10
      Width = 75
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 41
      Top = 60
      Width = 111
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Texture framerate'
    end
    object LabelFPS: TLabel
      Left = 50
      Top = 300
      Width = 27
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'FPS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object RB1to1: TRadioButton
      Tag = 1
      Left = 69
      Top = 100
      Width = 51
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '1:1'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RB1to1Click
    end
    object RB1to2: TRadioButton
      Tag = 2
      Left = 69
      Top = 140
      Width = 51
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '1:2'
      TabOrder = 1
      OnClick = RB1to1Click
    end
    object RB1to10: TRadioButton
      Tag = 10
      Left = 69
      Top = 180
      Width = 51
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '1:10'
      TabOrder = 2
      OnClick = RB1to1Click
    end
    object CheckBox1: TCheckBox
      Left = 69
      Top = 250
      Width = 71
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'VSync'
      TabOrder = 3
      OnClick = CheckBox1Click
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 144
    Top = 16
  end
  object GLScene1: TGLScene
    Left = 16
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
    Left = 72
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 192
    Top = 16
  end
end
