object Form2: TForm2
  Left = 209
  Top = 124
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'Form2'
  ClientHeight = 277
  ClientWidth = 472
  Color = clBtnShadow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 472
    Height = 277
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 140.299743652343700000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Button1: TButton
    Left = 280
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object Button2: TButton
    Left = 144
    Top = 188
    Width = 97
    Height = 19
    Caption = 'Define Password'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 24
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000048C200004842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object SpaceText4: TGLSpaceText
        Material.FrontProperties.Diffuse.Color = {0000803F0000803FD1D0D03D0000803F}
        Material.FrontProperties.Emission.Color = {14AE073F8FC2F53DD7A3F03E0000803F}
        Position.Coordinates = {0000A0C000008040000000000000803F}
        Scale.Coordinates = {00000040000000400000803F00000000}
        Extrusion = 1.000000000000000000
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Screen Saver')
        allowedDeviation = 1.000000000000000000
        CharacterRange = stcrAlphaNum
      end
      object Torus1: TGLTorus
        Tag = 1
        Direction.Coordinates = {FFFFFF3E71C41C3F71C41C3F00000000}
        Position.Coordinates = {000080C000000000000000000000803F}
        Scale.Coordinates = {00000040000000400000004000000000}
        Up.Coordinates = {EA5EAA32F304353FF30435BF00000000}
        MajorRadius = 1.000000000000000000
        MinorRadius = 0.300000011920929000
        Rings = 8
        Sides = 6
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
        BehavioursData = {
          0458434F4C02010201060B54474C42496E657274696102001200000000020002
          00050000000000000080FF3F0200080500000000000000F00340050000000000
          0000000000050000000000000000000009020008020008}
      end
      object Torus2: TGLTorus
        Tag = 2
        Direction.Coordinates = {00000000F304353FF304353F00000000}
        Position.Coordinates = {0000804000000000000000000000803F}
        Scale.Coordinates = {00000040000000400000004000000000}
        Up.Coordinates = {00000000F304353FF30435BF00000000}
        MajorRadius = 1.000000000000000000
        MinorRadius = 0.300000011920929000
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
        BehavioursData = {
          0458434F4C02010201060B54474C42496E657274696102001200000000020002
          00050000000000000080FF3F0200080500000000000000C80340050000000000
          0000000000050000000000000000000009020008020008}
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {000000000000803F0000A0410000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    Left = 32
    Top = 144
  end
end
