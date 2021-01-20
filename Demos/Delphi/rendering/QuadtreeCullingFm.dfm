object frmQuadtreeVisCulling: TfrmQuadtreeVisCulling
  Left = 297
  Top = 155
  Caption = 'Quadtree Visibility Culling'
  ClientHeight = 436
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 41
    Width = 632
    Height = 395
    Camera = GLCamera1
    Buffer.FogEnvironment.FogStart = 3000.000000000000000000
    Buffer.FogEnvironment.FogEnd = 3950.000000000000000000
    Buffer.BackgroundColor = clWhite
    Buffer.Lighting = False
    FieldOfView = 151.586471557617200000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 224
    Top = 216
    Width = 185
    Height = 49
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 82
      Height = 13
      Caption = 'Generating Trees'
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 24
      Width = 169
      Height = 17
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 632
    Height = 41
    Align = alTop
    TabOrder = 2
    object Label2: TLabel
      Left = 344
      Top = 9
      Width = 32
      Height = 13
      Caption = 'Label2'
    end
    object cbUseQuadtree: TCheckBox
      Left = 16
      Top = 8
      Width = 65
      Height = 17
      Caption = 'Quadtree'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbUseExtendedFrustum: TCheckBox
      Left = 96
      Top = 8
      Width = 105
      Height = 17
      Caption = 'E&xtended Frustum'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object cbShowQuadtree: TCheckBox
      Left = 224
      Top = 8
      Width = 97
      Height = 17
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
