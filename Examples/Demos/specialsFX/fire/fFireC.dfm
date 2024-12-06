object FormFire: TFormFire
  Left = 0
  Top = 0
  Caption = 'Fire'
  ClientHeight = 504
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnMouseWheel = FormMouseWheel
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 673
    Height = 504
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 157.555084228515600000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
    ExplicitWidth = 428
    ExplicitHeight = 300
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 24
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
    Left = 96
    Top = 8
  end
  object GLFireFXManager1: TGLFireFXManager
    FireDir.Coordinates = {000000000000803F0000000000000000}
    InitialDir.Coordinates = {00000000000000000000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 96
    ParticleSize = 0.699999988079071000
    OuterColor.Color = {0000803FF628DC3E14AE473F0000803F}
    FireDensity = 0.500000000000000000
    FireEvaporation = 0.860000014305114700
    FireBurst = 1.000000000000000000
    FireRadius = 0.500000000000000000
    Disabled = False
    Paused = False
    ParticleInterval = 0.009999999776482582
    UseInterval = True
    Left = 344
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 24
    Top = 72
  end
end
