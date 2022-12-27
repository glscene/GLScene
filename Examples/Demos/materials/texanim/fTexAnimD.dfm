object FormTexAnim: TFormTexAnim
  Left = 221
  Top = 109
  Caption = 'Texture Animation'
  ClientHeight = 486
  ClientWidth = 551
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 51
    Width = 551
    Height = 435
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    FieldOfView = 122.226837158203100000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 551
    Height = 51
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    TabOrder = 1
    object LabelFPS: TLabel
      Left = 400
      Top = 16
      Width = 26
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'FPS'
    end
    object Button1: TButton
      Left = 10
      Top = 10
      Width = 171
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Generate Anim Frames'
      TabOrder = 0
      OnClick = Button1Click
    end
    object CBAnimate: TCheckBox
      Left = 220
      Top = 15
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Animate'
      Enabled = False
      TabOrder = 1
    end
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 80
    object Cube1: TGLCube
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00007041000020410000E0400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 120.000000000000000000
      TargetObject = Cube1
      Position.Coordinates = {0000A04000008040000040400000803F}
      Left = 208
      Top = 160
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 40
    Top = 200
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 32
    Top = 136
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 32
    Top = 256
  end
end
