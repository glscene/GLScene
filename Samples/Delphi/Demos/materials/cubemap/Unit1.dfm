object Form1: TForm1
  Left = 135
  Top = 85
  Caption = 'Cube Map'
  ClientHeight = 364
  ClientWidth = 415
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 415
    Height = 364
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.BackgroundColor = clTeal
    FieldOfView = 62.487583160400390000
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
    Left = 8
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
      Material.BackProperties.Ambient.Color = {ADAC2C3FCDCC4C3ECDCC4C3E0000803F}
      Material.BackProperties.Emission.Color = {FBFAFA3EB9B8383FB7B6B63E6ABC343F}
      Material.BackProperties.Shininess = 101
      Material.FrontProperties.Ambient.Color = {A7A6263FCDCC4C3ECDCC4C3E0000803F}
      Material.FrontProperties.Shininess = 46
      Material.FrontProperties.Specular.Color = {0000000000000000000000007F6A1C3F}
      Material.Texture.ImageClassName = 'TGLCubeMapImage'
      Material.Texture.ImageAlpha = tiaAlphaFromIntensity
      Material.Texture.MinFilter = miNearest
      Material.Texture.TextureMode = tmReplace
      Position.Coordinates = {00000000000080BE000000000000803F}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 300.000000000000000000
      TargetObject = Teapot1
      Position.Coordinates = {000040400000A0400000E0400000803F}
    end
  end
end
