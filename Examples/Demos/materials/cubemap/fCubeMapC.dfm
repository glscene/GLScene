object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Cube Map'
  ClientHeight = 406
  ClientWidth = 572
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 572
    Height = 406
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.BackgroundColor = clTeal
    FieldOfView = 68.169807434082030000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object btnApply: TButton
    Left = 240
    Top = 16
    Width = 105
    Height = 25
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
      Material.Texture.ImageClassName = 'TGLCompositeImage'
      Material.Texture.Image.Width = 256
      Material.Texture.Image.Height = 256
      Material.Texture.Image.Depth = 0
      Material.Texture.TextureMode = tmReplace
    end
    object Plane1: TGLPlane
      Material.Texture.ImageClassName = 'TGLCompositeImage'
      Material.Texture.Image.Width = 256
      Material.Texture.Image.Height = 256
      Material.Texture.Image.Depth = 0
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
