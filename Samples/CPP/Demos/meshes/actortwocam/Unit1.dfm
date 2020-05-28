object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Actor with Two Cameras'
  ClientHeight = 423
  ClientWidth = 658
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 25
    Width = 658
    Height = 398
    Camera = GLCamera2
    Buffer.BackgroundColor = clGreen
    FieldOfView = 126.639793395996100000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 658
    Height = 25
    Align = alTop
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label3: TLabel
      Left = 352
      Top = 6
      Width = 76
      Height = 14
      Caption = 'F7 Third Person'
    end
    object Label4: TLabel
      Left = 448
      Top = 6
      Width = 83
      Height = 14
      Caption = 'F8 First Person'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 8
      Top = 6
      Width = 324
      Height = 14
      Caption = 
        'Use arrow keys to move, CTRL to strafe , SHIFT to run , ESC to e' +
        'xit'
    end
    object CBMouseLook: TCheckBox
      Left = 552
      Top = 4
      Width = 97
      Height = 17
      Caption = '&Mouse Look'
      TabOrder = 0
      OnClick = CBMouseLookClick
    end
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 40
    Top = 56
    object SkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {E9DC72BF000000009BE8A13E00000000}
      Bands = <
        item
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 15.000000000000000000
        end
        item
          StartAngle = 15.000000000000000000
          StopAngle = 90.000000000000000000
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Stacks = 4
        end>
      Stars = <>
    end
    object Disk1: TGLDisk
      Material.Texture.MinFilter = miLinear
      Material.Texture.Disabled = False
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Loops = 1
      OuterRadius = 80.000000000000000000
      Slices = 7
      SweepAngle = 360.000000000000000000
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000704200003443000000000000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      CubeSize = 1.000000000000000000
      object FreeForm1: TGLFreeForm
        Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {0000803F0000803F000000000000803F}
        Scale.Coordinates = {0AD7A33CCDCCCC3C4260E53C00000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        NormalsOrientation = mnoInvert
      end
    end
    object DummyCube2: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 0.100000001490116100
      object GLCamera2: TGLCamera
        DepthOfView = 500.000000000000000000
        FocalLength = 100.000000000000000000
        Position.Coordinates = {000000000000003F000000000000803F}
        Direction.Coordinates = {00000080000000000000803F00000000}
        Left = 320
        Top = 192
      end
      object Actor1: TGLActor
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.MinFilter = miLinear
        Material.Texture.Disabled = False
        Direction.Coordinates = {000000800000803F0000000000000000}
        Up.Coordinates = {0000803F000000000000000000000000}
        Visible = False
        Interval = 100
        object Actor2: TGLActor
          Material.Texture.MinFilter = miLinear
          Material.Texture.Disabled = False
          Direction.Coordinates = {00000080000000000000803F00000000}
          Interval = 100
        end
      end
      object DummyCube3: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLCamera1: TGLCamera
          DepthOfView = 1000.000000000000000000
          FocalLength = 200.000000000000000000
          TargetObject = DummyCube2
          Position.Coordinates = {00000000000040400000A0C10000803F}
          Direction.Coordinates = {00000000000000800000803F00000000}
        end
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 0
    OnProgress = GLCadencer1Progress
    Left = 112
    Top = 56
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 392
    Top = 56
  end
  object GLNavigator1: TGLNavigator
    VirtualUp.Coordinates = {000000000000803F000000000000803F}
    MovingObject = DummyCube2
    UseVirtualUp = True
    Left = 200
    Top = 56
  end
  object GLUserInterface1: TGLUserInterface
    MouseSpeed = 20.000000000000000000
    GLNavigator = GLNavigator1
    Left = 296
    Top = 56
  end
end
