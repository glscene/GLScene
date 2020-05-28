object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Texture Animation'
  ClientHeight = 372
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 41
    Width = 446
    Height = 331
    Camera = GLCamera1
    FieldOfView = 108.110099792480500000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 446
    Height = 41
    Align = alTop
    TabOrder = 1
    object LabelFPS: TLabel
      Left = 320
      Top = 13
      Width = 18
      Height = 13
      Caption = 'FPS'
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 137
      Height = 25
      Caption = 'Generate Anim Frames'
      TabOrder = 0
      OnClick = Button1Click
    end
    object CBAnimate: TCheckBox
      Left = 176
      Top = 12
      Width = 97
      Height = 17
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
