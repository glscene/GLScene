object FormFire: TFormFire
  Left = 228
  Top = 98
  Margins.Left = 2
  Margins.Top = 2
  Margins.Right = 2
  Margins.Bottom = 2
  BorderWidth = 3
  Caption = 'GLScene Fire'
  ClientHeight = 569
  ClientWidth = 686
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnMouseWheel = FormMouseWheel
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 686
    Height = 569
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 160.064468383789100000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
    ExplicitWidth = 430
    ExplicitHeight = 348
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 8
    Top = 8
    object Torus1: TGLTorus
      Material.FrontProperties.Ambient.Color = {9A99193E9A99193E9A99193E0000803F}
      MajorRadius = 3.000000000000000000
      MinorRadius = 0.200000002980232200
      Rings = 36
      Sides = 9
      StopAngle = 360.000000000000000000
      Parts = [toSides, toStartDisk, toStopDisk]
      BehavioursData = {
        0458434F4C02010201060B54474C42496E657274696102001200000000020002
        00050000000000000080FF3F0200080500000000000000000000050000000000
        00000000000500000000000000F0034009020008020008}
    end
    object Sphere1: TGLSphere
      Radius = 0.300000011920929000
      Slices = 6
      Stacks = 6
      EffectsData = {
        0458434F4C02010201060A54474C424669726546580201020012000000000200
        02000610474C4669726546584D616E6167657231}
      object GLLightSource2: TGLLightSource
        Ambient.Color = {0000803F0000803F0000803F0000803F}
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {0000803F8180003F000000000000803F}
        Position.Coordinates = {000000000000003F000000000000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Sphere1
      Position.Coordinates = {00000041000000400000A0400000803F}
      Left = 152
      Top = 104
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 40
    Top = 8
  end
  object GLFireFXManager1: TGLFireFXManager
    FireDir.Coordinates = {000000000000803F0000000000000000}
    InitialDir.Coordinates = {00000000000000000000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 96
    ParticleSize = 0.699999988079071000
    FireDensity = 0.500000000000000000
    FireEvaporation = 0.860000014305114700
    FireBurst = 1.000000000000000000
    FireRadius = 0.500000000000000000
    Disabled = False
    Paused = False
    ParticleInterval = 0.009999999776482582
    UseInterval = True
    Left = 72
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 40
  end
end
