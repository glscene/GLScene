object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'GLScene Candles'
  ClientHeight = 364
  ClientWidth = 478
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 478
    Height = 335
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 118.324333190918000000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object TrackBar1: TTrackBar
    Left = 0
    Top = 335
    Width = 478
    Height = 29
    Hint = 'Wind'
    Align = alBottom
    Max = 20
    Min = -20
    ParentShowHint = False
    Frequency = 5
    ShowHint = True
    TabOrder = 1
    OnChange = TrackBar1Change
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 16
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
    end
    object RevolutionSolid1: TGLRevolutionSolid
      Material.FrontProperties.Diffuse.Color = {0000803F8180003FC9C8C83E0000803F}
      Direction.Coordinates = {2EBDFBB3000000000000803F00000000}
      Nodes = <
        item
          Y = 0.500000000000000000
        end
        item
          Y = 0.500000000000000000
          Z = 2.000000000000000000
        end
        item
          Y = 0.300000011920929000
          Z = 2.000000000000000000
        end
        item
          Y = -0.500000000000000000
          Z = 2.000000000000000000
        end
        item
          Y = -0.500000000000000000
        end>
      Division = 9
      SplineMode = lsmCubicSpline
      Slices = 19
      object Candle: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {F1F0703FCBCA4A3FCBCA4A3F0000803F}
        Position.Coordinates = {000000000000A03F9A99993F0000803F}
        BottomRadius = 0.200000002980232200
        Height = 1.500000000000000000
        Slices = 12
        Stacks = 1
        TopRadius = 0.200000002980232200
        object Lines1: TGLLines
          Position.Coordinates = {000000006666663F000000000000803F}
          LineColor.Color = {A9A5253FB1A8283EB1A8283E0000803F}
          LineWidth = 2.000000000000000000
          Nodes = <
            item
              Y = -0.200000002980232200
            end
            item
              X = 0.050000000745058060
              Z = 0.050000000745058060
            end
            item
              X = -0.050000000745058060
              Y = 0.100000001490116100
              Z = -0.050000000745058060
            end>
          NodesAspect = lnaInvisible
          Options = []
          EffectsData = {
            0458434F4C02010201060A54474C424669726546580201020012000000000200
            02000610474C4669726546584D616E6167657231}
        end
        object DummyCube2: TGLDummyCube
          Direction.Coordinates = {F204353F00000000F40435BF00000000}
          CubeSize = 1.000000000000000000
          object Plane1: TGLPlane
            Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
            Material.FrontProperties.Diffuse.Color = {0000000000000000000000003333B33E}
            Material.BlendingMode = bmTransparency
            Direction.Coordinates = {000000000000803F2CBD3BB300000000}
            Position.Coordinates = {0000C03FA4703DBF000000000000803F}
            Up.Coordinates = {000000B32FBD3BB3000080BF00000000}
            Height = 0.400000005960464500
            Width = 3.000000000000000000
          end
        end
      end
      object GLProxyObject1: TGLProxyObject
        MasterObject = Candle
        ProxyOptions = [pooEffects, pooObjects]
        Position.Coordinates = {0000803F0000A03F9A9919BF0000803F}
      end
      object GLProxyObject2: TGLProxyObject
        MasterObject = Candle
        ProxyOptions = [pooEffects, pooObjects]
        Position.Coordinates = {000080BF0000A03F9A9919BF0000803F}
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {000070410000E040000040400000803F}
      Left = 192
      Top = 120
    end
  end
  object GLFireFXManager1: TGLFireFXManager
    FireDir.Coordinates = {000000009A99993E0000000000000000}
    InitialDir.Coordinates = {00000000CDCC4C3E0000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 128
    ParticleSize = 0.150000005960464500
    FireDensity = 0.600000023841857900
    FireEvaporation = 0.860000014305114700
    ParticleLife = 2
    FireBurst = 1.000000000000000000
    FireRadius = 0.100000001490116100
    Disabled = False
    Paused = False
    ParticleInterval = 0.039999999105930330
    UseInterval = True
    Left = 120
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 32
    Top = 88
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 376
    Top = 8
  end
end
