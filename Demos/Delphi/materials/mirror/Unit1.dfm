object Form1: TForm1
  Left = 215
  Top = 104
  BorderWidth = 3
  Caption = 'Mirror'
  ClientHeight = 262
  ClientWidth = 478
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 373
    Height = 262
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 138.218353271484400000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 373
    Top = 0
    Width = 105
    Height = 262
    Align = alRight
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 1
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object LabelFPS: TLabel
      Left = 6
      Top = 16
      Width = 26
      Height = 16
      Caption = 'FPS'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object CBOpaque: TCheckBox
      Left = 8
      Top = 56
      Width = 73
      Height = 17
      Caption = 'Opaque'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBOpaqueClick
    end
    object CBStencil: TCheckBox
      Left = 8
      Top = 88
      Width = 89
      Height = 17
      Caption = 'Use Stencil'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBStencilClick
    end
    object CBClearZ: TCheckBox
      Left = 8
      Top = 120
      Width = 90
      Height = 17
      Caption = 'ClearZBuffer'
      TabOrder = 2
      OnClick = CBClearZClick
    end
    object CBPlaneClip: TCheckBox
      Left = 8
      Top = 152
      Width = 80
      Height = 17
      Caption = 'Plane Clip'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CBPlaneClipClick
    end
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000003F0000003F0000003F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000C8420000B4420000A0420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DCNonReflectingStuff: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Cylinder: TGLTorus
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {000000008180003F000000000000803F}
        ObjectsSorting = osNone
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {00000000000080BF000000000000803F}
        Up.Coordinates = {E9DC72BF000000009BE8A13E00000000}
        Hint = '0'
        MajorRadius = 4.000000000000000000
        MinorRadius = 0.500000000000000000
        Rings = 24
        Sides = 12
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
        object Cylinder2: TGLCylinder
          Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
          Material.FrontProperties.Diffuse.Color = {000000008180003F000000000000803F}
          BottomRadius = 0.200000002980232200
          Height = 7.000000000000000000
          Slices = 12
          Stacks = 1
          TopRadius = 0.200000002980232200
        end
      end
    end
    object ReflectingObjects: TGLDummyCube
      ObjectsSorting = osNone
      CubeSize = 1.000000000000000000
      object CylinderThroughMirror: TGLCylinder
        Position.Coordinates = {0000000000000000000000C00000803F}
        Up.Coordinates = {F404353FF204353F0000000000000000}
        BottomRadius = 0.200000002980232200
        Height = 3.000000000000000000
        Slices = 12
        Stacks = 1
        TopRadius = 0.200000002980232200
      end
      object Sphere: TGLSphere
        ObjectsSorting = osNone
        Position.Coordinates = {000000000000803F000000000000803F}
        Radius = 0.500000000000000000
        Slices = 12
        Stacks = 6
        Top = 0
        TopCap = ctCenter
        object Cylinder1: TGLCylinder
          Direction.Coordinates = {000000000000803F2EBD3BB300000000}
          Position.Coordinates = {00000000CDCCCCBD6666663F0000803F}
          Up.Coordinates = {000000002EBD3BB3000080BF00000000}
          BottomRadius = 0.100000001490116100
          Height = 1.000000000000000000
          Slices = 8
          Stacks = 1
          TopRadius = 0.100000001490116100
        end
        object Teapot1: TGLTeapot
          Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
          Position.Coordinates = {000000000000003F000000000000803F}
          Scale.Coordinates = {00000040000000400000004000000000}
        end
      end
    end
    object GLMirror: TGLMirror
      Material.FrontProperties.Ambient.Color = {00000000000000000000803F0000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F9A99993E}
      Material.BlendingMode = bmTransparency
      Material.Texture.ImageClassName = 'TGLBlankImage'
      Material.Texture.Image.Width = 64
      Material.Texture.Image.Height = 64
      Material.Texture.Image.ColorFormat = 6408
      Material.Texture.MagFilter = maNearest
      Material.Texture.MinFilter = miNearest
      Material.Texture.Compression = tcNone
      ObjectsSorting = osNone
      Direction.Coordinates = {000000800000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      MirrorObject = ReflectingObjects
      MirrorOptions = [moUseStencil, moMirrorPlaneClip, moClearZBuffer]
      Height = 6.000000000000000000
      Width = 6.000000000000000000
      Radius = 1.000000000000000000
      object Cadre: TGLExtrusionSolid
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Contours = <
          item
            Nodes = <
              item
                X = 3.099999904632568000
                Y = -3.099999904632568000
              end
              item
                X = 3.099999904632568000
                Y = 3.099999904632568000
              end
              item
                X = -3.099999904632568000
                Y = 3.099999904632568000
              end
              item
                X = -3.099999904632568000
                Y = -3.099999904632568000
              end>
          end
          item
            Nodes = <
              item
                X = 2.900000095367432000
                Y = 2.900000095367432000
              end
              item
                X = -2.900000095367432000
                Y = 2.900000095367432000
              end
              item
                X = -2.900000095367432000
                Y = -2.900000095367432000
              end
              item
                X = 2.900000095367432000
                Y = -2.900000095367432000
              end>
          end>
        Parts = [espOutside, espStartPolygon, espStopPolygon]
        Height = 0.100000001490116100
        MinSmoothAngle = 5.000000000000000000
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = ReflectingObjects
      Position.Coordinates = {0000A0400000C040000010410000803F}
      Left = 192
      Top = 128
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 40
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.050000000000000000
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
end
