object Form1: TForm1
  Left = 73
  Top = 86
  Caption = 'Shadow Volumes'
  ClientHeight = 441
  ClientWidth = 695
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 536
    Height = 441
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = GLCamera
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 154.447631835937500000
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 536
    Top = 0
    Width = 159
    Height = 441
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alRight
    BevelInner = bvLowered
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    DesignSize = (
      159
      441)
    object LabelFPS: TLabel
      Left = 44
      Top = 16
      Width = 29
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'FPS'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 16
      Top = 301
      Width = 108
      Height = 15
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Anchors = [akLeft, akBottom]
      Caption = 'Shadow Resolution'
    end
    object CBShowVolumes: TCheckBox
      Left = 16
      Top = 160
      Width = 113
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Show Volumes'
      TabOrder = 0
      OnClick = CBShowVolumesClick
    end
    object RBZFail: TRadioButton
      Left = 16
      Top = 69
      Width = 113
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Z-Fail (capped)'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = RBZFailClick
    end
    object RBZPass: TRadioButton
      Left = 16
      Top = 90
      Width = 105
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Z-Pass'
      TabOrder = 2
      OnClick = RBZFailClick
    end
    object RBNoShadows: TRadioButton
      Left = 16
      Top = 48
      Width = 105
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'No shadows'
      TabOrder = 3
      OnClick = RBZFailClick
    end
    object RBDarkening: TRadioButton
      Left = 16
      Top = 128
      Width = 129
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Darkening (faked)'
      TabOrder = 4
      OnClick = RBZFailClick
    end
    object CBMainLight: TCheckBox
      Left = 16
      Top = 197
      Width = 97
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Main Light'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = CBMainLightClick
    end
    object CBBlueLight: TCheckBox
      Left = 16
      Top = 218
      Width = 97
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Blue Light'
      TabOrder = 6
      OnClick = CBBlueLightClick
    end
    object CBRedLight: TCheckBox
      Left = 16
      Top = 239
      Width = 97
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Red Light'
      TabOrder = 7
      OnClick = CBRedLightClick
    end
    object ScrollBar_ShadowResolution: TScrollBar
      Left = 16
      Top = 320
      Width = 89
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Anchors = [akLeft, akBottom]
      Max = 21
      Min = 3
      PageSize = 0
      Position = 3
      TabOrder = 8
      OnChange = ScrollBar_ShadowResolutionChange
    end
    object Button_GenerateSilhouette: TButton
      Left = 4
      Top = 272
      Width = 125
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Generate Silhouette'
      TabOrder = 9
      OnClick = Button_GenerateSilhouetteClick
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 16
    object DCLight1Turn: TGLDummyCube
      CubeSize = 1.000000000000000000
      object DCLight1Pitch: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLLightSource1: TGLLightSource
          Ambient.Color = {9A99193F9A99193F9A99193F0000803F}
          ConstAttenuation = 1.000000000000000000
          Diffuse.Color = {9A99193F9A99193F9A99193F0000803F}
          Position.Coordinates = {0000000000008040000000000000803F}
          SpotCutOff = 180.000000000000000000
          object GLSphere1: TGLSphere
            Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
            Material.MaterialOptions = [moNoLighting]
            ShowAxes = True
            Radius = 0.150000005960464500
            Slices = 11
            Stacks = 11
          end
        end
      end
    end
    object DCCamera: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera: TGLCamera
        DepthOfView = 1.000000015047466E30
        FocalLength = 50.000000000000000000
        TargetObject = DCCamera
        CameraStyle = csInfinitePerspective
        Position.Coordinates = {000020410000E0400000A0400000803F}
      end
    end
    object DCLight2: TGLDummyCube
      Direction.Coordinates = {000000006C61D83ECA03683F00000000}
      PitchAngle = 15.000000000000000000
      Up.Coordinates = {00000000CA03683F6B61D8BE00000000}
      CubeSize = 1.000000000000000000
      object GLLightSource2: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {00000000000000000000803F0000803F}
        QuadraticAttenuation = 0.009999999776482582
        Position.Coordinates = {00000000000000000000A0C00000803F}
        LightStyle = lsOmni
        Shining = False
        SpotCutOff = 180.000000000000000000
        object GLSphere2: TGLSphere
          Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
          Material.MaterialOptions = [moNoLighting]
          Radius = 0.150000005960464500
          Slices = 9
          Stacks = 9
        end
      end
    end
    object DCLight3: TGLDummyCube
      Direction.Coordinates = {00000000421DAFBEB28F703F00000000}
      PitchAngle = -15.000000000000000000
      Up.Coordinates = {00000000B28F703F431DAF3E00000000}
      CubeSize = 1.000000000000000000
      object GLLightSource3: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {0000803F00000000000000000000803F}
        QuadraticAttenuation = 0.009999999776482582
        Position.Coordinates = {0000000000008040000000000000803F}
        Shining = False
        SpotCutOff = 180.000000000000000000
        object GLSphere3: TGLSphere
          Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
          Material.MaterialOptions = [moNoLighting]
          Radius = 0.150000005960464500
        end
      end
    end
    object GLShadowVolume: TGLShadowVolume
      Lights = <
        item
          LightSource = GLLightSource1
        end
        item
          LightSource = GLLightSource2
        end
        item
          LightSource = GLLightSource3
        end>
      Occluders = <
        item
          Caster = GLCube1
        end
        item
          Caster = GLCylinder1
        end
        item
          CastingMode = scmParentVisible
          Caster = GLSphere_Shadow
        end>
      Options = [svoCacheSilhouettes, svoScissorClips, svoDesignVisible]
      object GLPlane1: TGLPlane
        Material.FrontProperties.Ambient.Color = {00000000000000001283003F0000803F}
        Position.Coordinates = {000000000000A040000020C10000803F}
        Height = 20.000000000000000000
        Width = 20.000000000000000000
        XTiles = 9
        YTiles = 9
        Style = [psTileTexture]
      end
      object GLPlane2: TGLPlane
        Material.FrontProperties.Ambient.Color = {D7A3703E00000000000000000000803F}
        Direction.Coordinates = {0000803F000000000000000000000000}
        Position.Coordinates = {000020C10000A040000000000000803F}
        Up.Coordinates = {00000000FFFF7F3F0000008000000000}
        Height = 20.000000000000000000
        Width = 20.000000000000000000
        XTiles = 9
        YTiles = 9
        Style = [psTileTexture]
      end
      object GLPlane3: TGLPlane
        Material.FrontProperties.Ambient.Color = {000000001283803E000000000000803F}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {00000000295C9FC0000000000000803F}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Height = 20.000000000000000000
        Width = 20.000000000000000000
        XTiles = 20
        YTiles = 20
        Style = [psTileTexture]
      end
      object GLFreeForm: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Scale.Coordinates = {295C8F3D295C8F3D295C8F3D00000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
        MaterialLibrary = GLMaterialLibrary1
      end
      object GLCube1: TGLCube
        Position.Coordinates = {00004040000000C0000000000000803F}
      end
      object DCSpheres: TGLDummyCube
        CubeSize = 1.000000000000000000
      end
      object GLCylinder1: TGLCylinder
        Direction.Coordinates = {00000000000000800000803F00000000}
        Position.Coordinates = {00004040000080C0000000000000803F}
        Up.Coordinates = {0000803F000000000000000000000000}
        BottomRadius = 0.600000023841857900
        Height = 6.000000000000000000
        TopRadius = 0.800000011920929000
      end
      object GLSphere4: TGLSphere
        Position.Coordinates = {00000000000000000000C0C00000803F}
        Radius = 2.000000000000000000
        object GLSphere_Shadow: TGLSphere
          Visible = False
          Radius = 2.000000000000000000
          Slices = 5
          Stacks = 5
        end
      end
    end
    object GLLines1: TGLLines
      LineWidth = 2.000000000000000000
      Nodes = <>
      NodesAspect = lnaInvisible
      SplineMode = lsmSegments
      Options = []
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Mode = cmApplicationIdle
    OnProgress = GLCadencer1Progress
    Left = 112
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 192
    Top = 24
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 40
    Top = 96
  end
end
