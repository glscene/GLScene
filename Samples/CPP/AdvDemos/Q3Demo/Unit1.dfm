object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'GLScene Quake Actor'
  ClientHeight = 458
  ClientWidth = 783
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 783
    Height = 73
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 72
      Height = 13
      Caption = 'Legs Animation'
    end
    object Label2: TLabel
      Left = 200
      Top = 16
      Width = 77
      Height = 13
      Caption = 'Torso Animaiton'
    end
    object Label3: TLabel
      Left = 380
      Top = 8
      Width = 71
      Height = 13
      Caption = 'Torso direction'
    end
    object Label4: TLabel
      Left = 496
      Top = 8
      Width = 25
      Height = 13
      Caption = 'Head'
    end
    object Label5: TLabel
      Left = 616
      Top = 20
      Width = 19
      Height = 13
      Caption = 'Skin'
    end
    object ComboBox1: TComboBox
      Left = 16
      Top = 32
      Width = 169
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = ComboBox1Change
    end
    object ComboBox2: TComboBox
      Left = 200
      Top = 32
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = ComboBox2Change
    end
    object TrackBar1: TTrackBar
      Left = 372
      Top = 24
      Width = 109
      Height = 21
      Max = 90
      Min = -90
      TabOrder = 2
    end
    object TrackBar2: TTrackBar
      Left = 372
      Top = 48
      Width = 109
      Height = 21
      Max = 90
      Min = -90
      TabOrder = 3
    end
    object TrackBar3: TTrackBar
      Left = 488
      Top = 24
      Width = 109
      Height = 21
      Max = 90
      Min = -90
      TabOrder = 4
    end
    object TrackBar4: TTrackBar
      Left = 488
      Top = 48
      Width = 109
      Height = 21
      Max = 90
      Min = -90
      TabOrder = 5
    end
    object ComboSkin: TComboBox
      Left = 612
      Top = 36
      Width = 85
      Height = 21
      TabOrder = 6
      Text = 'Default'
      OnChange = ComboSkinChange
      Items.Strings = (
        'Default'
        'Red'
        'Blue')
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 73
    Width = 783
    Height = 385
    Camera = GLCamera1
    Buffer.BackgroundColor = clSkyBlue
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 51.398372650146480000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 80
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        Ambient.Color = {0000803F0000803F0000803F0000803F}
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000204100000000000020410000803F}
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
      object GLCamera1: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 400.000000000000000000
        TargetObject = DummyCube1
        Position.Coordinates = {00009041000080410000C0400000803F}
        Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
      end
    end
    object ModelCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Legs: TGLActor
        Interval = 100
        MaterialLibrary = MatLib
        object Torso: TGLActor
          Interval = 100
          MaterialLibrary = MatLib
          object Head: TGLActor
            Interval = 100
            MaterialLibrary = MatLib
          end
          object Weapon: TGLActor
            Interval = 100
            MaterialLibrary = MatLib
            object GunSmoke: TGLDummyCube
              CubeSize = 1.000000000000000000
              EffectsData = {
                0458434F4C02010201061254474C536F75726365504658456666656374020202
                001200000000020002000617474C506F696E744C696768745046584D616E6167
                657231050000000000000080FF3F0206020008020008020008050000000000CD
                CCCCFA3F050000000000CDCCCCFA3F050000000000CDCCCCFA3F020002000905
                00000000000000000000080200}
            end
          end
        end
      end
    end
    object GLShadowPlane1: TGLShadowPlane
      Material.DepthProperties.DepthWrite = False
      Position.Coordinates = {0000000000000000000080BF0000803F}
      Height = 10.000000000000000000
      Width = 10.000000000000000000
      ShadowingObject = ModelCube
      ShadowedLight = GLLightSource1
    end
    object GLParticleFXRenderer1: TGLParticleFXRenderer
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 0
    OnProgress = GLCadencer1Progress
    Left = 120
    Top = 80
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 208
    Top = 80
  end
  object MatLib: TGLMaterialLibrary
    Left = 32
    Top = 144
  end
  object GLPointLightPFXManager1: TGLPointLightPFXManager
    Cadencer = GLCadencer1
    Renderer = GLParticleFXRenderer1
    Acceleration.Coordinates = {0000000000000000CDCC4C3E00000000}
    Friction = 1.000000000000000000
    ParticleSize = 0.200000002980232200
    ColorInner.Color = {0000403F0000403F0000403F0000403F}
    ColorOuter.Color = {0000403F0000403F0000403F00000000}
    LifeColors = <
      item
        LifeTime = 3.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 120
    Top = 144
  end
end
