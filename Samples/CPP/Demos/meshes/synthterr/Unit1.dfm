object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Synthetic Terrain'
  ClientHeight = 364
  ClientWidth = 526
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 526
    Height = 364
    Camera = GLCamera1
    VSync = vsmSync
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.FogEnvironment.FogStart = 400.000000000000000000
    Buffer.FogEnvironment.FogEnd = 1200.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clBlack
    Buffer.FogEnable = True
    Buffer.Lighting = False
    FieldOfView = 149.276763916015600000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 40
    Top = 24
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000000041000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 1200.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = DummyCube1
        Position.Coordinates = {0000A040000020410000C8410000803F}
        Left = 264
        Top = 160
      end
    end
    object TerrainRenderer1: TGLTerrainRenderer
      Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {00008040000080400000003F00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      HeightDataSource = GLCustomHDS
      TileSize = 32
      TilesPerTexture = 1.000000000000000000
      QualityDistance = 50.000000000000000000
      CLODPrecision = 20
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 256
    Top = 24
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 152
    Top = 24
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 40
    Top = 88
  end
  object GLCustomHDS: TGLCustomHDS
    MaxPoolSize = 0
    OnStartPreparingData = GLCustomHDSStartPreparingData
    Left = 152
    Top = 88
  end
end
