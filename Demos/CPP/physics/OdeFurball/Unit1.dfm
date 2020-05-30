object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ODE Fur Ball'
  ClientHeight = 442
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 57
    Width = 753
    Height = 385
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 140.033782958984400000
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 753
    Height = 57
    Align = alTop
    TabOrder = 1
    DesignSize = (
      753
      57)
    object Label1: TLabel
      Left = 327
      Top = 10
      Width = 153
      Height = 13
      Caption = '(Steer with A/D W/S UP/DOWN)'
    end
    object Label_FPS: TLabel
      Left = 623
      Top = 9
      Width = 18
      Height = 13
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akRight]
      Caption = 'FPS'
    end
    object CheckBox_LockBall: TCheckBox
      Left = 8
      Top = 8
      Width = 65
      Height = 17
      Caption = '&Lock Ball'
      TabOrder = 0
    end
    object CheckBox_Inertia: TCheckBox
      Left = 8
      Top = 24
      Width = 65
      Height = 17
      Caption = 'Fur &Inertia'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox_InertiaClick
    end
    object CheckBox_FurGravity: TCheckBox
      Left = 80
      Top = 8
      Width = 81
      Height = 17
      Caption = 'Fur &Gravity'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox_FurGravityClick
    end
    object CheckBox_WindResistence: TCheckBox
      Left = 160
      Top = 8
      Width = 105
      Height = 17
      Caption = '&Wind Resistence'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBox_WindResistenceClick
    end
    object TrackBar_WindForce: TTrackBar
      Left = 160
      Top = 24
      Width = 105
      Height = 17
      Max = 100
      Position = 50
      TabOrder = 4
      ThumbLength = 10
      TickStyle = tsNone
      OnChange = TrackBar_WindForceChange
    end
    object CheckBox_Bald: TCheckBox
      Left = 272
      Top = 8
      Width = 49
      Height = 17
      Caption = '&Bald'
      TabOrder = 5
      OnClick = CheckBox_BaldClick
    end
    object CheckBox_Shadows: TCheckBox
      Left = 496
      Top = 8
      Width = 65
      Height = 17
      Caption = '&Shadows'
      TabOrder = 6
      OnClick = CheckBox_ShadowsClick
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.050000000000000000
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 184
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 128
    object DC_LightHolder: TGLDummyCube
      Up.Coordinates = {000000000000803F0000008000000000}
      OnProgress = DC_LightHolderProgress
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {00000000000000000000A0400000803F}
        SpotCutOff = 180.000000000000000000
        object Sphere1: TGLSphere
          Material.BackProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
          Material.BackProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
          Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
          Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
          Radius = 0.100000001490116100
        end
      end
    end
    object DCShadowCaster: TGLDummyCube
      CubeSize = 1.000000000000000000
      object FurBall: TGLSphere
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.TextureMode = tmModulate
        Position.Coordinates = {000000000000C0BF000040400000803F}
        Radius = 0.250000000000000000
      end
    end
    object GLShadowPlane_Floor: TGLShadowPlane
      Material.BackProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
      Material.FrontProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
      Direction.Coordinates = {00000000B6C8CB3DC2BA7E3F00000000}
      Up.Coordinates = {00000000C2BA7E3FB6C8CBBD00000000}
      Height = 8.000000000000000000
      Width = 8.000000000000000000
      XTiles = 10
      YTiles = 10
      Style = [psTileTexture]
      ShadowingObject = DCShadowCaster
      ShadowOptions = [spoUseStencil]
    end
    object GLShadowPlane_Wall: TGLShadowPlane
      Material.BackProperties.Diffuse.Color = {000000000000803F000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
      Direction.Coordinates = {0000803F000000000000000000000000}
      Position.Coordinates = {000080C0000080400000803F0000803F}
      Up.Coordinates = {0000000000000000FFFF7FBF00000000}
      Height = 4.000000000000000000
      Width = 16.000000000000000000
      XTiles = 10
      YTiles = 10
      Style = [psTileTexture]
      ShadowingObject = DCShadowCaster
      ShadowOptions = [spoUseStencil]
    end
    object GLShadowPlane_Floor2: TGLShadowPlane
      Material.BackProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
      Material.FrontProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
      Direction.Coordinates = {00000000B6C8CBBDC2BA7E3F00000000}
      Position.Coordinates = {000000000000F040000000000000803F}
      Up.Coordinates = {00000000C2BA7E3FB5C8CB3D00000000}
      Height = 10.000000000000000000
      Width = 8.000000000000000000
      XTiles = 10
      YTiles = 10
      Style = [psTileTexture]
      ShadowingObject = DCShadowCaster
      ShadowOptions = [spoUseStencil]
    end
    object GLLines1: TGLLines
      Nodes = <>
      Options = []
    end
    object GLShadowPlane_Wall2: TGLShadowPlane
      Material.BackProperties.Diffuse.Color = {000000000000803F000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {00000000000080C00000803F0000803F}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 4.000000000000000000
      Width = 8.000000000000000000
      XTiles = 10
      YTiles = 10
      Style = [psTileTexture]
      ShadowingObject = DCShadowCaster
      ShadowOptions = [spoUseStencil]
    end
    object GLShadowPlane_Wall3: TGLShadowPlane
      Material.BackProperties.Diffuse.Color = {000000000000803F000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
      Direction.Coordinates = {00000000000080BF0000000000000000}
      Position.Coordinates = {00000000000040410000803F0000803F}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 4.000000000000000000
      Width = 8.000000000000000000
      XTiles = 10
      YTiles = 10
      Style = [psTileTexture]
      ShadowingObject = DCShadowCaster
      ShadowOptions = [spoUseStencil]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 70.000000000000000000
      TargetObject = FurBall
      Position.Coordinates = {0000004100004040000040400000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 128
    Top = 128
  end
end
