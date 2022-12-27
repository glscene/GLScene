object FormCamera: TFormCamera
  Left = 299
  Top = 129
  BorderWidth = 6
  Caption = 'Camera'
  ClientHeight = 525
  ClientWidth = 581
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 120
  DesignSize = (
    581
    525)
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 581
    Height = 525
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    VSync = vsmSync
    Buffer.BackgroundColor = clTeal
    FieldOfView = 158.431411743164100000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object RadioGroup1: TRadioGroup
    Left = 5
    Top = 435
    Width = 226
    Height = 91
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
  end
  object RadioGroup2: TRadioGroup
    Left = 386
    Top = 455
    Width = 182
    Height = 71
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Caption = 'Keep mode'
    ItemIndex = 0
    Items.Strings = (
      'Horizontal'
      'Vertical')
    TabOrder = 2
    OnClick = RadioGroup2Click
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
