object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Path Control'
  ClientHeight = 378
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 497
    Height = 378
    Camera = GLCamera1
    Buffer.BackgroundColor = clAppWorkSpace
    FieldOfView = 150.363708496093800000
    Align = alClient
    TabOrder = 0
  end
  object MoveBtn: TBitBtn
    Left = 0
    Top = 0
    Width = 129
    Height = 25
    Caption = 'Start Movement'
    TabOrder = 1
    OnClick = MoveBtnClick
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 48
    object DummyCube1: TGLDummyCube
      CubeSize = 2.000000000000000000
      VisibleAtRunTime = True
      object Cube2: TGLCube
        Material.FrontProperties.Diffuse.Color = {8786063F8786063F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {0000000000000000A1A0203F0000803F}
        Position.Coordinates = {0000404000000000000000000000803F}
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {B8B7373FB8B7373FB8B7373F0000803F}
      Position.Coordinates = {0000204100000000000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Sphere1: TGLSphere
      Position.Coordinates = {0000004500002042008009450000803F}
      Scale.Coordinates = {0000A0400000A0400000A04000000000}
      Radius = 0.500000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {00000000000000000000A0410000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    TimeMultiplier = 2.000000000000000000
    SleepLength = 10
    Left = 32
    Top = 112
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Path Control - %FPS'
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
    Left = 128
    Top = 112
  end
end
