object Form1: TForm1
  Left = 221
  Top = 107
  BorderWidth = 3
  Caption = 'Joystick'
  ClientHeight = 189
  ClientWidth = 262
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 262
    Height = 189
    Camera = GLCamera1
    Buffer.BackgroundColor = clBtnShadow
    FieldOfView = 124.233322143554700000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000A0C000002041000070410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Cube1: TGLCube
      Material.FrontProperties.Diffuse.Color = {A9A5253FB1A8283EB1A8283E0000803F}
      Direction.Coordinates = {D9B35D3FEE83043EE646F7BE00000000}
      Position.Coordinates = {000080BF000000C0000000000000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
      ShowAxes = True
      Up.Coordinates = {B85863B2EA46773FEF83843E00000000}
      CubeSize = {000000400000003F00000040}
      object DummyCube1: TGLDummyCube
        Position.Coordinates = {000000000000803E000000000000803F}
        CubeSize = 1.000000000000000000
        object Cylinder1: TGLCylinder
          Material.FrontProperties.Ambient.Color = {8D8C0C3F8A89093FD1D0D03D0000803F}
          Material.FrontProperties.Diffuse.Color = {F9F8783FE1E0E03DC1C0403C0000803F}
          Position.Coordinates = {000000000000803F000000000000803F}
          BottomRadius = 0.200000002980232200
          Height = 2.000000000000000000
          Stacks = 1
          TopRadius = 0.200000002980232200
        end
      end
    end
    object DummyCube2: TGLDummyCube
      Position.Coordinates = {0000904000000000000000000000803F}
      CubeSize = 1.000000000000000000
      object Sphere1: TGLSphere
        Position.Coordinates = {0000000000004040000000000000803F}
        Radius = 0.500000000000000000
      end
      object Sphere2: TGLSphere
        Position.Coordinates = {000000000000803F000000000000803F}
        Radius = 0.500000000000000000
      end
      object Sphere3: TGLSphere
        Position.Coordinates = {00000000000080BF000000000000803F}
        Radius = 0.500000000000000000
      end
      object Sphere4: TGLSphere
        Position.Coordinates = {00000000000040C0000000000000803F}
        Radius = 0.500000000000000000
      end
    end
    object DummyCube3: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube3
      Position.Coordinates = {00000000000000000000A0410000803F}
    end
  end
  object Joystick1: TGLJoystick
    Capture = True
    JoystickID = jidJoystick1
    Threshold = 500
    OnJoystickButtonChange = Joystick1JoystickButtonChange
    Left = 144
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 64
  end
end
