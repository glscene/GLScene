object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Cube Map'
  ClientHeight = 377
  ClientWidth = 477
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 477
    Height = 377
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.BackgroundColor = clTeal
    FieldOfView = 64.285034179687500000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Button1: TButton
    Left = 136
    Top = 8
    Width = 163
    Height = 25
    Caption = 'Apply Cube Environment Map'
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object DummyCube1: TGLDummyCube
      CubeSize = 0.800000011920929000
      VisibleAtRunTime = True
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000E0400000A040000040400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Teapot1: TGLTeapot
      Material.Texture.TextureMode = tmReplace
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 300.000000000000000000
      TargetObject = Teapot1
      Position.Coordinates = {000040400000A0400000E0400000803F}
    end
  end
end
