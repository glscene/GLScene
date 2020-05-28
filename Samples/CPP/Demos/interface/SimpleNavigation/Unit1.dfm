object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Simple Navigation'
  ClientHeight = 404
  ClientWidth = 533
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 533
    Height = 404
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 152.194625854492200000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 80
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000020410000A0410000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLCube1
      Position.Coordinates = {000080400000A0400000C0400000803F}
    end
    object GLCube1: TGLCube
      Material.FrontProperties.Ambient.Color = {D5D4543EBDBC3C3FA7A6263F0000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 48
    Top = 16
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Simple Navigation - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 16
    Top = 16
  end
end
