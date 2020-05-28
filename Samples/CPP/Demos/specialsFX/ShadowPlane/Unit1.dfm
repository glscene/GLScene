object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Shadow Plane'
  ClientHeight = 442
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 625
    Height = 442
    Camera = GLCamera1
    Buffer.BackgroundColor = 8421440
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 154.503555297851600000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 121
    Height = 57
    TabOrder = 1
    object CBShadows: TCheckBox
      Left = 16
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Shadows'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBShadowsClick
    end
    object CBStencil: TCheckBox
      Left = 16
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Stencil Buffer'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBStencilClick
    end
  end
  object GLScene1: TGLScene
    Left = 304
    Top = 8
    object DCShadowing: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Cube1: TGLCube
        Position.Coordinates = {000000000000003F000000000000803F}
        Up.Coordinates = {00000000FFFF7F3F1AF6953100000000}
        CubeSize = {00000040CDCC4C3E3333333F}
      end
      object Sphere1: TGLSphere
        Position.Coordinates = {000000000000803F0000C0BF0000803F}
        Radius = 0.200000002980232200
      end
      object Torus1: TGLTorus
        Direction.Coordinates = {00000000B28F703F441DAF3E00000000}
        Position.Coordinates = {000000000000803F0000C03F0000803F}
        Up.Coordinates = {00000000441DAF3EB28F70BF00000000}
        MajorRadius = 0.300000011920929000
        MinorRadius = 0.100000001490116100
        Rings = 24
        Sides = 12
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
    end
    object DCLight: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000000000004040000000000000803F}
        SpotCutOff = 180.000000000000000000
        object Sphere2: TGLSphere
          Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
          Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
          Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
          Radius = 0.100000001490116100
          Slices = 8
          Stacks = 8
        end
      end
    end
    object DCCameraTarget: TGLDummyCube
      Position.Coordinates = {000000003333333F000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        SceneScale = 2.000000000000000000
        TargetObject = DCCameraTarget
        Position.Coordinates = {0000A04000008040000040400000803F}
      end
    end
    object GLShadowPlane1: TGLShadowPlane
      Material.MaterialLibrary = GLMaterialLibrary
      Material.LibMaterialName = 'Marble'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 8.000000000000000000
      Width = 4.000000000000000000
      XTiles = 4
      YTiles = 8
      Style = [psTileTexture]
      ShadowingObject = DCShadowing
      ShadowedLight = GLLightSource1
    end
    object GLShadowPlane2: TGLShadowPlane
      Material.MaterialLibrary = GLMaterialLibrary
      Material.LibMaterialName = 'Marble'
      Position.Coordinates = {0000000000000040000080C00000803F}
      Up.Coordinates = {000000000000803F0000008000000000}
      Height = 4.000000000000000000
      Width = 4.000000000000000000
      XTiles = 4
      YTiles = 4
      Style = [psTileTexture]
      ShadowingObject = DCShadowing
      ShadowedLight = GLLightSource1
    end
    object GLShadowPlane3: TGLShadowPlane
      Material.MaterialLibrary = GLMaterialLibrary
      Material.LibMaterialName = 'Marble'
      Direction.Coordinates = {0000803F000000800000000000000000}
      Position.Coordinates = {000000C000000040000000000000803F}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 8.000000000000000000
      Width = 4.000000000000000000
      XTiles = 4
      YTiles = 8
      Style = [psTileTexture]
      ShadowingObject = DCShadowing
      ShadowedLight = GLLightSource1
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 304
    Top = 64
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 392
    Top = 8
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Marble'
        Tag = 0
        Material.FrontProperties.Emission.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'BeigeMarble.jpg'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {0000003F0000003F0000803F00000000}
      end>
    TexturePaths = '..\\..\\..\\..\\media'
    Left = 192
    Top = 8
  end
end
