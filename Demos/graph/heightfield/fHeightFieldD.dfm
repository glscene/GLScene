object FormHeightField: TFormHeightField
  Left = 196
  Top = 94
  BorderWidth = 4
  Caption = 'Height Field'
  ClientHeight = 455
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 544
    Height = 455
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 132.543212890625000000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 544
    Top = 0
    Width = 209
    Height = 455
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 60
      Top = 10
      Width = 53
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'X extents'
    end
    object Label2: TLabel
      Left = 60
      Top = 73
      Width = 54
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Y extents'
    end
    object Label3: TLabel
      Left = 70
      Top = 135
      Width = 28
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Step'
    end
    object Label4: TLabel
      Left = 24
      Top = 346
      Width = 70
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Color Mode'
    end
    object LabelFPS: TLabel
      Left = 18
      Top = 450
      Width = 26
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'FPS'
    end
    object TrackBar1: TTrackBar
      Left = 0
      Top = 24
      Width = 161
      Height = 41
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 50
      Position = 10
      TabOrder = 0
      ThumbLength = 13
      TickMarks = tmBoth
      OnChange = TrackBar1Change
    end
    object TrackBar2: TTrackBar
      Left = 0
      Top = 86
      Width = 161
      Height = 42
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 50
      Position = 10
      TabOrder = 1
      ThumbLength = 13
      TickMarks = tmBoth
      OnChange = TrackBar2Change
    end
    object TrackBar3: TTrackBar
      Left = 0
      Top = 150
      Width = 161
      Height = 41
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 160
      Min = 10
      Frequency = 10
      Position = 80
      TabOrder = 2
      ThumbLength = 13
      TickMarks = tmBoth
      OnChange = TrackBar3Change
    end
    object RadioGroup1: TRadioGroup
      Left = 40
      Top = 199
      Width = 101
      Height = 111
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Formula'
      ItemIndex = 0
      Items.Strings = (
        'Formula 1'
        'Formula 2'
        'Dynamic')
      TabOrder = 3
      OnClick = RadioGroup1Click
    end
    object CheckBox1: TCheckBox
      Left = 20
      Top = 318
      Width = 91
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Two-sided'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CheckBox1Click
    end
    object ComboBox1: TComboBox
      Left = 20
      Top = 370
      Width = 101
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Style = csDropDownList
      TabOrder = 5
      OnChange = ComboBox1Change
      Items.Strings = (
        'none'
        'emission'
        'diffuse')
    end
    object CheckBox2: TCheckBox
      Left = 20
      Top = 404
      Width = 81
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Lighting'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = CheckBox2Click
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 24
    object HeightField1: TGLHeightField
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      Scale.Coordinates = {00004040000040400000404000000000}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      XSamplingScale.Min = -1.000000000000000000
      XSamplingScale.Max = 1.000000000000000000
      XSamplingScale.Step = 0.079999998211860660
      YSamplingScale.Min = -1.000000000000000000
      YSamplingScale.Max = 1.000000000000000000
      YSamplingScale.Step = 0.079999998211860660
      object Sphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
        Position.Coordinates = {0000803F0000803F000000000000803F}
        Visible = False
        OnProgress = Sphere1Progress
        Radius = 0.100000001490116100
        Slices = 12
        Stacks = 9
        object Lines1: TGLLines
          Nodes = <
            item
            end
            item
              Z = -1.500000000000000000
            end>
          NodesAspect = lnaInvisible
          Options = []
        end
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000F041000048420000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = HeightField1
      Position.Coordinates = {0000404000008040000000410000803F}
      Left = 208
      Top = 168
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 128
    Top = 24
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    Left = 40
    Top = 80
  end
end
