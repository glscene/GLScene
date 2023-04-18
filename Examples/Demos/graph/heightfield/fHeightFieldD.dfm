object FormHeightField: TFormHeightField
  Left = 196
  Top = 94
  BorderWidth = 4
  Caption = 'Height Field'
  ClientHeight = 440
  ClientWidth = 653
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 486
    Height = 440
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 131.112091064453100000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 486
    Top = 0
    Width = 167
    Height = 440
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 48
      Top = 8
      Width = 44
      Height = 13
      Caption = 'X extents'
    end
    object Label2: TLabel
      Left = 48
      Top = 58
      Width = 44
      Height = 13
      Caption = 'Y extents'
    end
    object Label3: TLabel
      Left = 56
      Top = 108
      Width = 22
      Height = 13
      Caption = 'Step'
    end
    object Label4: TLabel
      Left = 19
      Top = 277
      Width = 54
      Height = 13
      Caption = 'Color Mode'
    end
    object LabelFPS: TLabel
      Left = 14
      Top = 408
      Width = 20
      Height = 13
      Caption = 'FPS'
    end
    object TrackBar1: TTrackBar
      Left = 0
      Top = 19
      Width = 129
      Height = 33
      Max = 50
      Position = 10
      TabOrder = 0
      ThumbLength = 10
      TickMarks = tmBoth
      OnChange = TrackBar1Change
    end
    object TrackBar2: TTrackBar
      Left = 0
      Top = 69
      Width = 129
      Height = 33
      Max = 50
      Position = 10
      TabOrder = 1
      ThumbLength = 10
      TickMarks = tmBoth
      OnChange = TrackBar2Change
    end
    object TrackBar3: TTrackBar
      Left = 0
      Top = 120
      Width = 129
      Height = 33
      Max = 160
      Min = 10
      Frequency = 10
      Position = 80
      TabOrder = 2
      ThumbLength = 10
      TickMarks = tmBoth
      OnChange = TrackBar3Change
    end
    object RadioGroup1: TRadioGroup
      Left = 32
      Top = 159
      Width = 81
      Height = 89
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
      Left = 16
      Top = 254
      Width = 73
      Height = 17
      Caption = 'Two-sided'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CheckBox1Click
    end
    object ComboBox1: TComboBox
      Left = 16
      Top = 296
      Width = 81
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      OnChange = ComboBox1Change
      Items.Strings = (
        'none'
        'emission'
        'diffuse')
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 323
      Width = 65
      Height = 17
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
      Material.FrontProperties.Emission.Color = {0000000000000000000000000000003F}
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
