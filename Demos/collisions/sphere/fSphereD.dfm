object FormSphere: TFormSphere
  Left = 197
  Top = 104
  Caption = 'Sphere Collision'
  ClientHeight = 318
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 450
    Height = 273
    Camera = GLCamera1
    FieldOfView = 139.764404296875000000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object TrackBar1: TTrackBar
    Left = 0
    Top = 273
    Width = 450
    Height = 45
    Align = alBottom
    Max = 20
    Min = -20
    Frequency = 5
    Position = 20
    TabOrder = 1
    TickMarks = tmBoth
    OnChange = TrackBar1Change
  end
  object Button1: TButton
    Left = 336
    Top = 16
    Width = 91
    Height = 25
    Caption = 'Test collisions'
    TabOrder = 2
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000A0410000A0410000A0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object Sphere1: TGLSphere
      Material.FrontProperties.Diffuse.Color = {8A8F0F3FBEBC3C3F8A8F0F3F0000803F}
      Position.Coordinates = {0000000000000000000000400000803F}
      Radius = 0.500000000000000000
      BehavioursData = {
        0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
        0002000611436F6C6C6973696F6E4D616E616765723102010200}
    end
    object Sphere2: TGLSphere
      Material.FrontProperties.Diffuse.Color = {00000000F8FEFE3E0000803F0000803F}
      Radius = 0.500000000000000000
      BehavioursData = {
        0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
        0002000611436F6C6C6973696F6E4D616E616765723102010200}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000A04000004040000000400000803F}
      Left = 256
      Top = 160
    end
  end
  object CollisionManager1: TGLCollisionManager
    OnCollision = CollisionManager1Collision
    Left = 16
    Top = 72
  end
end
