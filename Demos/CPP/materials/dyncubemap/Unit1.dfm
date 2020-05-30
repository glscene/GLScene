object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Dynamic Cube Map'
  ClientHeight = 329
  ClientWidth = 561
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 25
    Width = 561
    Height = 304
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.BackgroundColor = clBlack
    FieldOfView = 143.583038330078100000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 561
    Height = 25
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    object LabelFPS: TLabel
      Left = 288
      Top = 8
      Width = 18
      Height = 13
      Caption = 'FPS'
    end
    object CBDynamic: TCheckBox
      Left = 8
      Top = 4
      Width = 273
      Height = 17
      Caption = 'Animate and dynamically generate cube map'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 32
    object SkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {E9DC72BF000000009BE8A13E00000000}
      Bands = <
        item
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 15.000000000000000000
          Slices = 6
        end
        item
          StartAngle = 15.000000000000000000
          StopAngle = 90.000000000000000000
          StopColor.Color = {5C8F023F5C8F023F0000803F0000803F}
          Slices = 6
          Stacks = 4
        end
        item
          StartAngle = -90.000000000000000000
          StartColor.Color = {000000001283803E000000000000803F}
          StopAngle = -5.000000000000000000
          StopColor.Color = {000000001283003F000000000000803F}
          Slices = 6
        end
        item
          StartAngle = -5.000000000000000000
          StartColor.Color = {000000001283003F000000000000803F}
          StopColor.Color = {0000803F0000803F0000803F0000803F}
          Slices = 6
        end>
      Stars = <>
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {E5D0623FE5D0623FE5D0623F0000803F}
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Torus1: TGLTorus
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000803F000000000000000000000000}
      MajorRadius = 5.000000000000000000
      MinorRadius = 0.300000011920929000
      Rings = 32
      Sides = 12
      StopAngle = 360.000000000000000000
      Parts = [toSides, toStartDisk, toStopDisk]
      object Cylinder1: TGLCylinder
        Material.FrontProperties.Emission.Color = {00000000000000000000803F0000803F}
        Direction.Coordinates = {000000800000803F0000000000000000}
        Position.Coordinates = {0000A04000000000000000000000803F}
        Up.Coordinates = {00000000000000000000803F00000000}
        BottomRadius = 0.300000011920929000
        Height = 5.000000000000000000
        Slices = 12
        Stacks = 1
        TopRadius = 0.300000011920929000
      end
      object Sphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {1283003F0000803F000000000000803F}
        Material.FrontProperties.Emission.Color = {00000000448B0C3FBA490C3E0000803F}
        Position.Coordinates = {0000A0C000000000000000000000803F}
        Radius = 1.000000000000000000
        Slices = 12
        Stacks = 12
      end
      object Cube1: TGLCube
        Material.FrontProperties.Diffuse.Color = {AE47613EAE47613EAE47613E0000803F}
        Material.FrontProperties.Emission.Color = {1283003F00000000000000000000803F}
        Position.Coordinates = {000000000000A040000000000000803F}
        CubeSize = {0000C03F0000C03F0000C03F}
      end
    end
    object Teapot1: TGLTeapot
      Material.Texture.ImageClassName = 'TGLCubeMapImage'
      Material.Texture.MinFilter = miLinear
      Material.Texture.TextureMode = tmReplace
      Material.Texture.MappingMode = tmmCubeMapReflection
      Direction.Coordinates = {F404353F00000000F204353F00000000}
      Scale.Coordinates = {00000041000000410000004100000000}
      Visible = False
      object CubeMapCamera: TGLCamera
        DepthOfView = 50.000000000000000000
        FocalLength = 25.000000000000000000
        TargetObject = Sphere1
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Torus1
      Position.Coordinates = {0000204100008040000000410000803F}
    end
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Camera = CubeMapCamera
    Width = 128
    Height = 128
    Buffer.BackgroundColor = clMaroon
    Left = 40
    Top = 32
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 20.000000000000000000
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 64
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 40
    Top = 64
  end
end
