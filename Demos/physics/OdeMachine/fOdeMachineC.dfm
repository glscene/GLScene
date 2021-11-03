object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ODE Machine'
  ClientHeight = 510
  ClientWidth = 786
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 17
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 786
    Height = 510
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    FieldOfView = 157.812561035156300000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {0000803F00000000000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {00000040000060400000C0400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object Machine: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Wheel: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Position.Coordinates = {000020C000000000000000000000803F}
        BottomRadius = 2.000000000000000000
        Height = 0.500000000000000000
        Slices = 32
        TopRadius = 2.000000000000000000
        object Axle: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
          Position.Coordinates = {00000000000000C0000000000000803F}
          BottomRadius = 0.500000000000000000
          Height = 5.000000000000000000
          TopRadius = 0.500000000000000000
        end
        object Pin1: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
          Position.Coordinates = {000000000000003F0000C0BF0000803F}
          BottomRadius = 0.250000000000000000
          Height = 1.000000000000000000
          TopRadius = 0.250000000000000000
        end
      end
      object Arm: TGLCube
        Material.FrontProperties.Diffuse.Color = {CDCC0C3FEC51B83DEC51B83D0000803F}
        Direction.Coordinates = {4B413AB4000000000000803F00000000}
        Position.Coordinates = {0000003F0000403F0000C0BF0000803F}
        CubeSize = {0000F0400000803E0000403F}
      end
      object Pin2: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Position.Coordinates = {000060400000003F0000C0BF0000803F}
        BottomRadius = 0.250000000000000000
        Height = 1.000000000000000000
        TopRadius = 0.250000000000000000
      end
      object Slider: TGLCube
        Material.FrontProperties.Diffuse.Color = {1F856B3F14AE473F52B81E3F0000803F}
        Position.Coordinates = {00005040000000000000C0BF0000803F}
        CubeSize = {000080400000003F0000803F}
      end
    end
    object ODERenderPoint: TGLRenderPoint
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000204100002041000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
      ModulateColor.Color = {0000000000000000000000000000803F}
    end
  end
  object GLODEManager1: TGLODEManager
    Solver = osmQuickStep
    Iterations = 3
    MaxContacts = 8
    RenderPoint = ODERenderPoint
    Visible = True
    VisibleAtRunTime = False
    Left = 128
    Top = 8
  end
  object GLODEJointList1: TGLODEJointList
    Left = 248
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 64
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Left = 248
    Top = 64
  end
end
