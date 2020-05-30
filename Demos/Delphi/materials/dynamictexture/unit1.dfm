object Form1: TForm1
  Left = 407
  Top = 201
  Caption = 'Dynamic Texture'
  ClientHeight = 362
  ClientWidth = 591
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 591
    Height = 362
    Camera = GLCamera1
    Buffer.BackgroundColor = clGray
    FieldOfView = 149.115112304687500000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 16
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCube1: TGLCube
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Anim'
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000404000002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000803F0000803F000040400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Anim'
        Tag = 0
        Material.Texture.TextureMode = tmReplace
      end>
    Left = 160
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 64
    Top = 88
  end
  object Timer1: TTimer
    Interval = 3000
    OnTimer = Timer1Timer
    Left = 496
    Top = 16
  end
end
