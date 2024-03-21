object FormSmoothnavi: TFormSmoothnavi
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Smooth Navigator'
  ClientHeight = 739
  ClientWidth = 1108
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 168
  TextHeight = 23
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1108
    Height = 656
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 162.665222167968800000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel3: TPanel
    Left = 0
    Top = 656
    Width = 1108
    Height = 83
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object MouseLookCheckBox: TCheckBox
      Left = 14
      Top = 11
      Width = 508
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Mouse Look Active (toggle by pressing the space bar)'
      TabOrder = 0
      OnClick = MouseLookCheckBoxClick
    end
    object GroupBox2: TGroupBox
      Left = 504
      Top = 2
      Width = 338
      Height = 80
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'FPS'
      TabOrder = 1
      object RadioButton6: TRadioButton
        Left = 14
        Top = 28
        Width = 72
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Max'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioButton6Click
      end
      object RadioButton7: TRadioButton
        Left = 112
        Top = 28
        Width = 88
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Middle'
        TabOrder = 1
        OnClick = RadioButton7Click
      end
      object RadioButton8: TRadioButton
        Left = 238
        Top = 28
        Width = 88
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Low'
        TabOrder = 2
        OnClick = RadioButton8Click
      end
    end
    object GroupBox1: TGroupBox
      Left = 854
      Top = 2
      Width = 226
      Height = 80
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Controls'
      TabOrder = 2
      object Label1: TLabel
        Left = 14
        Top = 23
        Width = 101
        Height = 23
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Fly = WASD'
      end
      object Panel1: TPanel
        Left = 12
        Top = 47
        Width = 200
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Caption = 'Accelerate = Hold Shift'
        TabOrder = 0
      end
    end
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 16
    Top = 8
    object scene: TGLDummyCube
      CubeSize = 500.000000000000000000
      object GLXYZGrid1: TGLXYZGrid
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        XSamplingScale.Min = -1000.000000000000000000
        XSamplingScale.Max = 1000.000000000000000000
        XSamplingScale.Step = 20.000000000000000000
        YSamplingScale.Min = -1000.000000000000000000
        YSamplingScale.Max = 1000.000000000000000000
        YSamplingScale.Step = 20.000000000000000000
        ZSamplingScale.Step = 0.100000001490116100
      end
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
      object GLSphere1: TGLSphere
        Material.FrontProperties.Ambient.Color = {CDCC4C3EC9C8483FCDCC4C3E0000803F}
        Material.FrontProperties.Diffuse.Color = {E0DF5F3FCDCC4C3FCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {AFAEAE3E000000008180803D0000803F}
        Radius = 10.000000000000000000
      end
      object GLArrowLine1: TGLArrowLine
        Material.FrontProperties.Ambient.Color = {0000803FF8FEFE3E000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803FF8FEFE3E000000000000803F}
        Material.FrontProperties.Emission.Color = {0000803FF8FEFE3E000000000000803F}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {0000000000004843000000000000803F}
        Scale.Coordinates = {0000C8420000C8420000C84200000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        BottomRadius = 0.100000001490116100
        Height = 1.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLArrowLine1
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {0000FAC300004843000061C40000803F}
      Direction.Coordinates = {F304353F00000080F304353F00000000}
      Up.Coordinates = {00000000FFFF7F3F0000000000000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 80
    Top = 8
  end
  object FPSTimer: TTimer
    OnTimer = FPSTimerTimer
    Left = 152
    Top = 8
  end
end
