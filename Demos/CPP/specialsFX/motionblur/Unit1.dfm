object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Motion Blur'
  ClientHeight = 494
  ClientWidth = 594
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 594
    Height = 459
    Camera = Camera
    VSync = vsmSync
    PostRender = GLSceneViewerPostRender
    Buffer.BackgroundColor = clNavy
    FieldOfView = 155.418640136718800000
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 459
    Width = 594
    Height = 35
    Align = alBottom
    Caption = 'FPS'
    TabOrder = 1
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 8
    Top = 8
    object Light: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000F041000000000000F0410000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
    object Cube: TGLCube
      Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      RollAngle = -15.000000000000000000
      Up.Coordinates = {EF83843EE946773F0000008000000000}
      object Torus: TGLTorus
        Scale.Coordinates = {0000A0400000A0400000A04000000000}
        MajorRadius = 0.400000005960464500
        MinorRadius = 0.100000001490116100
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
    end
    object DummyCube: TGLDummyCube
      Direction.Coordinates = {00000000441DAFBEB28F703F00000000}
      PitchAngle = -10.000000000000000000
      Up.Coordinates = {00008031B28F703F441DAF3E00000000}
      CubeSize = 1.000000000000000000
      object Dodecahedron: TGLDodecahedron
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
        Direction.Coordinates = {000000B3000000B30000803F00000000}
        Position.Coordinates = {0000000000000000000040400000803F}
        RollAngle = 10.000000000000000000
        Up.Coordinates = {D3D031BE5D1C7C3F28A8CF3200000000}
      end
    end
    object HUD: TGLHUDSprite
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F9A99193F}
      Material.BlendingMode = bmTransparency
      Material.MaterialOptions = [moIgnoreFog, moNoLighting]
      Material.Texture.ImageClassName = 'TGLBlankImage'
      Material.Texture.Image.ColorFormat = 6408
      Material.Texture.MagFilter = maNearest
      Material.Texture.MinFilter = miNearest
      Material.Texture.TextureMode = tmModulate
      Material.Texture.Compression = tcNone
      Material.Texture.Disabled = False
      Position.Coordinates = {0000804300008043000000000000803F}
      Width = 512.000000000000000000
      Height = 512.000000000000000000
      Rotation = 0.000000000000000000
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Cube
      Position.Coordinates = {0000C040000000000000803F0000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 1.000000000000000000
    OnProgress = GLCadencer1Progress
    Left = 88
    Top = 8
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 304
    Top = 16
  end
end
