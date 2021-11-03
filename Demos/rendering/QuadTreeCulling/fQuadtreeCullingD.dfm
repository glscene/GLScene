object frmQuadtreeVisCulling: TfrmQuadtreeVisCulling
  Left = 297
  Top = 155
  Caption = 'Quadtree Visibility Culling'
  ClientHeight = 545
  ClientWidth = 790
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 51
    Width = 790
    Height = 494
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.FogEnvironment.FogStart = 3000.000000000000000000
    Buffer.FogEnvironment.FogEnd = 3950.000000000000000000
    Buffer.BackgroundColor = clWhite
    Buffer.Lighting = False
    FieldOfView = 157.112609863281300000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 280
    Top = 270
    Width = 231
    Height = 61
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 10
      Width = 105
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Generating Trees'
    end
    object ProgressBar1: TProgressBar
      Left = 10
      Top = 30
      Width = 211
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 790
    Height = 51
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    TabOrder = 2
    object Label2: TLabel
      Left = 430
      Top = 11
      Width = 41
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Label2'
    end
    object cbUseQuadtree: TCheckBox
      Left = 20
      Top = 10
      Width = 81
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Quadtree'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbUseExtendedFrustum: TCheckBox
      Left = 120
      Top = 10
      Width = 131
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'E&xtended Frustum'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object cbShowQuadtree: TCheckBox
      Left = 280
      Top = 10
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show &Quadtree'
      TabOrder = 2
      OnClick = cbShowQuadtreeClick
    end
  end
  object GLScene1: TGLScene
    Left = 36
    Top = 56
    object GLSkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
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
    object GLTerrainRenderer1: TGLTerrainRenderer
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = '1'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {00000042000000420000004000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      HeightDataSource = GLBitmapHDS1
      TileSize = 32
      TilesPerTexture = 8.000000000000000000
      ContourWidth = 0
    end
    object queryVisible: TGLDirectOpenGL
      UseBuildList = False
      OnRender = queryVisibleRender
      Blend = False
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      Blend = False
    end
    object trees: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000804000008040000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Text = '0'
      Rotation = 0.000000000000000000
    end
    object tree: TGLSprite
      Material.BlendingMode = bmTransparency
      Material.Texture.ImageAlpha = tiaSuperBlackTransparent
      Material.Texture.TextureMode = tmReplace
      Material.Texture.Disabled = False
      Width = 280.000000000000000000
      Height = 300.000000000000000000
      Rotation = 0.000000000000000000
    end
    object GLDirectOpenGL2: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      OnRender = GLDirectOpenGL2Render
      Blend = False
    end
    object GLSphere1: TGLSphere
      Position.Coordinates = {00000000000000000000FAC30000803F}
      Visible = False
      Radius = 90.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 4000.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {0000000000000000000020410000803F}
    end
  end
  object GLBitmapHDS1: TGLBitmapHDS
    MaxPoolSize = 0
    Left = 220
    Top = 60
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = '1'
        Tag = 0
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
        Texture2Name = '2'
      end
      item
        Name = '2'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end>
    Left = 32
    Top = 156
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 128
    Top = 56
  end
  object GLNavigator1: TGLNavigator
    VirtualUp.Coordinates = {000000000000803F000000000000803F}
    MovingObject = GLCamera1
    UseVirtualUp = True
    AutoUpdateObject = True
    Left = 284
    Top = 152
  end
  object GLUserInterface1: TGLUserInterface
    MouseSpeed = 12.000000000000000000
    GLNavigator = GLNavigator1
    Left = 316
    Top = 64
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 424
    Top = 156
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 160
    Top = 152
  end
end
