object FormCamera: TFormCamera
  Left = 299
  Top = 129
  BorderWidth = 6
  Caption = 'Camera'
  ClientHeight = 450
  ClientWidth = 602
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  DesignSize = (
    602
    450)
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 602
    Height = 450
    Camera = GLCamera1
    VSync = vsmSync
    Buffer.BackgroundColor = clTeal
    FieldOfView = 154.942382812500000000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
    ExplicitWidth = 509
    ExplicitHeight = 427
  end
  object RadioGroup1: TRadioGroup
    Left = 4
    Top = 378
    Width = 181
    Height = 73
    Anchors = [akLeft, akBottom]
    Caption = 'Style'
    ItemIndex = 0
    Items.Strings = (
      'Perspective'
      'Infinite perspective'
      'Perspective, keep field of view'
      'Custom')
    TabOrder = 1
    OnClick = RadioGroup1Click
    ExplicitTop = 348
  end
  object RadioGroup2: TRadioGroup
    Left = 446
    Top = 394
    Width = 145
    Height = 57
    Anchors = [akRight, akBottom]
    Caption = 'Keep mode'
    ItemIndex = 0
    Items.Strings = (
      'Horizontal'
      'Vertical')
    TabOrder = 2
    OnClick = RadioGroup2Click
    ExplicitLeft = 309
    ExplicitTop = 364
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Teapot1: TGLTeapot
      Material.FrontProperties.Diffuse.Color = {9493133F0000803F9291113F0000803F}
      Direction.Coordinates = {0000803F000000000000000000000000}
      Scale.Coordinates = {00000040000000400000004000000000}
      Up.Coordinates = {00000080000000000000803F00000000}
    end
    object DummyCube1: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
      object GLCamera1: TGLCamera
        DepthOfView = 10.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = DummyCube1
        OnCustomPerspective = GLCamera1CustomPerspective
        Position.Coordinates = {00004040000000400000803F0000803F}
        Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 112
    Top = 8
  end
end
