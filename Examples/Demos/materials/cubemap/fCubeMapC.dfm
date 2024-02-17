object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Cube Map'
  ClientHeight = 711
  ClientWidth = 1001
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 168
  TextHeight = 23
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1001
    Height = 711
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.BackgroundColor = clTeal
    FieldOfView = 99.679176330566410000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object btnApply: TButton
    Left = 420
    Top = 28
    Width = 184
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Apply Cube Map'
    TabOrder = 1
    OnClick = btnApplyClick
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
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
    object Plane1: TGLPlane
      Position.Coordinates = {0000000000000000000000BF0000803F}
      Up.Coordinates = {0000803F000000000000008000000000}
      Height = 1.000000000000000000
      Width = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 300.000000000000000000
      TargetObject = Teapot1
      Position.Coordinates = {000040400000A0400000E0400000803F}
    end
  end
end
