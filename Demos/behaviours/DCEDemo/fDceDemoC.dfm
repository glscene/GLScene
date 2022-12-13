object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'GLScene Dynamic Collision Engine'
  ClientHeight = 394
  ClientWidth = 593
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  WindowState = wsMaximized
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 593
    Height = 394
    Camera = GLCamera1
    Buffer.FogEnvironment.FogStart = 50.000000000000000000
    Buffer.FogEnvironment.FogEnd = 250.000000000000000000
    Buffer.BackgroundColor = clBlack
    Buffer.FogEnable = True
    FieldOfView = 151.517288208007800000
    PenAsTouch = False
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00000000000048420000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Terrain: TGLTerrainRenderer
      Material.MaterialLibrary = GLMatLib
      HeightDataSource = GLBitmapHDS1
      TilesPerTexture = 1.000000000000000000
      MaterialLibrary = GLMatLib
      ContourWidth = 0
      BehavioursData = {
        0458434F4C02010201060C54474C444345537461746963020102001200000000
        0200060D474C4443454D616E61676572310203020009090F0000A0410F000000
        00020008}
    end
    object Ground: TGLPlane
      Material.FrontProperties.Diffuse.Color = {D3D2D23EC7C6463FC7C6C63E0000803F}
      Material.MaterialLibrary = GLMatLib
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {00000000000010C0000000000000803F}
      Scale.Coordinates = {0000FA430000FA430000803F00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Visible = False
      Height = 1.000000000000000000
      Width = 1.000000000000000000
      XTiles = 50
      YTiles = 50
      Style = []
      BehavioursData = {
        0458434F4C02010201060C54474C444345537461746963020102001200000000
        0200060D474C4443454D616E61676572310201020009080F0000A0410F000000
        00020008}
    end
    object Balls: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object moMushroom: TGLFreeForm
      Material.FrontProperties.Diffuse.Color = {A9A8283F8B8A8A3E9190103E0000803F}
      Position.Coordinates = {00000000000000000000A0C00000803F}
      NormalsOrientation = mnoInvert
      BehavioursData = {
        0458434F4C02010201060C54474C444345537461746963020102001200000000
        0200060D474C4443454D616E61676572310202020009090F0000803F0F000000
        00020008}
    end
    object Mushrooms: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCube1: TGLCube
      Material.FrontProperties.Diffuse.Color = {B1B0B03D9998183F8180003D0000803F}
      Material.MaterialLibrary = GLMatLib
      Direction.Coordinates = {00000000CAA8073FAE19593F00000000}
      PitchAngle = 32.000000000000000000
      Position.Coordinates = {0000804000000000000020410000803F}
      Scale.Coordinates = {0000A041000080400000204100000000}
      Up.Coordinates = {00000000AE19593FCAA807BF00000000}
      BehavioursData = {
        0458434F4C02010201060C54474C444345537461746963020102001200000000
        0200060D474C4443454D616E61676572310201020009090F0000F0410F000000
        00020008}
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object Player: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
      BehavioursData = {
        0458434F4C02010201060D54474C44434544796E616D69630201020012000000
        000200060D474C4443454D616E616765723102000909090F0000003F0F000000
        00020502000200090000803F0000A03F0000803F00000000}
      object GLCamera1: TGLCamera
        DepthOfView = 300.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = Player
        Position.Coordinates = {0000000000000040000040C00000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
      object GLActor1: TGLActor
        Material.MaterialLibrary = GLMatLib
        Interval = 100
      end
      object GLSphere1: TGLSphere
        Material.FrontProperties.Ambient.Color = {0000803F9190903D000000000000803F}
        Material.FrontProperties.Diffuse.Color = {BFBE3E3F00000000000000006DE77B3E}
        Material.FrontProperties.Emission.Color = {0000000000000000A1A0203D0000803F}
        Material.BlendingMode = bmAdditive
        Radius = 1.000000000000000000
      end
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000804000000000000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
    end
    object HelpShadow: TGLHUDText
      Position.Coordinates = {000030410000F841000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
      ModulateColor.Color = {A19E9E3ECFBC3C3ECFBC3C3E0000803F}
    end
    object Help: TGLHUDText
      Position.Coordinates = {000020410000F041000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 120
    Top = 8
  end
  object GLDCEManager1: TGLDCEManager
    Gravity = -30.000000000000000000
    WorldScale = 1.000000000000000000
    MovimentScale = 1.000000000000000000
    StandardiseLayers = ccsDCEStandard
    ManualStep = False
    Left = 248
    Top = 8
  end
  object GLBitmapHDS1: TGLBitmapHDS
    MaxPoolSize = 0
    Left = 24
    Top = 72
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Impact'
    Font.Style = []
    Ranges = <
      item
        StartASCII = ' '
        StopASCII = 'z'
        StartGlyphIdx = 0
      end>
    Left = 248
    Top = 72
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 496
    Top = 16
  end
  object GLMatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
      end>
    TexturePaths = '..\\..\\..\\..\\media\\'
    Left = 120
    Top = 72
  end
end
