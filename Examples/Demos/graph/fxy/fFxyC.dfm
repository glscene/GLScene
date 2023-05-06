object FormFxy: TFormFxy
  Left = 192
  Top = 121
  Caption = 'Fxy '
  ClientHeight = 564
  ClientWidth = 786
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 563
    Height = 564
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = Camera
    Buffer.BackgroundColor = clTeal
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 132.112792968750000000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 563
    Top = 0
    Width = 223
    Height = 564
    Align = alRight
    TabOrder = 1
    object LabelX: TLabel
      Left = 34
      Top = 48
      Width = 7
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'X'
    end
    object LabelY: TLabel
      Left = 104
      Top = 48
      Width = 7
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Y'
    end
    object LabelZ: TLabel
      Left = 168
      Top = 48
      Width = 7
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Z'
    end
    object TrackBarY: TTrackBar
      Left = 101
      Top = 71
      Width = 36
      Height = 257
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Min = -10
      Orientation = trVertical
      TabOrder = 0
      OnChange = TrackBarYChange
    end
    object TrackBarX: TTrackBar
      Left = 29
      Top = 71
      Width = 36
      Height = 257
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Min = -10
      Orientation = trVertical
      TabOrder = 1
      OnChange = TrackBarXChange
    end
    object TrackBarZ: TTrackBar
      Left = 165
      Top = 71
      Width = 36
      Height = 257
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Min = -10
      Orientation = trVertical
      TabOrder = 2
      OnChange = TrackBarZChange
    end
    object chbCenter: TCheckBox
      Left = 64
      Top = 11
      Width = 97
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Centered Grids'
      TabOrder = 3
      OnClick = chbCenterClick
    end
    object rgFormula: TRadioGroup
      Left = 16
      Top = 345
      Width = 217
      Height = 105
      Caption = 'Formula'
      ItemIndex = 3
      Items.Strings = (
        'x*y'
        'x*y*z'
        'sin(z*12)/(2*(z*6.28+1))'
        '(pow(x,2)+pow(y,2))*sin(8*atan2(x,y))')
      TabOrder = 4
      OnClick = rgFormulaClick
    end
    object rgPolygonMode: TRadioGroup
      Left = 13
      Top = 464
      Width = 196
      Height = 89
      Caption = 'PolygonMode'
      ItemIndex = 0
      Items.Strings = (
        'pmFill'
        'pmLines'
        'pmPoints')
      TabOrder = 5
      OnClick = rgPolygonModeClick
    end
  end
  object GLScene1: TGLScene
    Left = 80
    Top = 48
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 125.000000000000000000
      TargetObject = HeightField
      Position.Coordinates = {0000404000008040000000410000803F}
    end
    object LightSource: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000F041000048420000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object HeightField: TGLHeightField
      Direction.Coordinates = {0044F4970000803F2EBD3BB300000000}
      Scale.Coordinates = {00000040000000400000004000000000}
      ShowAxes = True
      Up.Coordinates = {0000803FEDAD09A72EBD3BB300000000}
      OnPicked = chbCenterClick
      XSamplingScale.Min = -1.000000000000000000
      XSamplingScale.Max = 1.000000000000000000
      XSamplingScale.Step = 0.019999999552965160
      YSamplingScale.Min = -1.000000000000000000
      YSamplingScale.Max = 1.000000000000000000
      YSamplingScale.Step = 0.019999999552965160
      object XYGrid: TGLXYZGrid
        LineColor.Color = {00000000000000000000803F0000803F}
        XSamplingScale.Min = -1.000000000000000000
        XSamplingScale.Max = 1.000000000000000000
        XSamplingScale.Origin = -1.000000000000000000
        XSamplingScale.Step = 0.200000002980232200
        YSamplingScale.Min = -1.000000000000000000
        YSamplingScale.Max = 1.000000000000000000
        YSamplingScale.Origin = -1.000000000000000000
        YSamplingScale.Step = 0.200000002980232200
        ZSamplingScale.Min = -1.000000000000000000
        ZSamplingScale.Max = 1.000000000000000000
        ZSamplingScale.Origin = -1.000000000000000000
        ZSamplingScale.Step = 0.200000002980232200
      end
      object XZGrid: TGLXYZGrid
        LineColor.Color = {000000000000803F000000000000803F}
        XSamplingScale.Min = -1.000000000000000000
        XSamplingScale.Max = 1.000000000000000000
        XSamplingScale.Origin = -1.000000000000000000
        XSamplingScale.Step = 0.100000001490116100
        YSamplingScale.Min = -1.000000000000000000
        YSamplingScale.Max = 1.000000000000000000
        YSamplingScale.Origin = -1.000000000000000000
        YSamplingScale.Step = 0.100000001490116100
        ZSamplingScale.Min = -1.000000000000000000
        ZSamplingScale.Max = 1.000000000000000000
        ZSamplingScale.Origin = -1.000000000000000000
        ZSamplingScale.Step = 0.100000001490116100
        Parts = [gpX, gpZ]
      end
      object YZGrid: TGLXYZGrid
        LineColor.Color = {0000803F00000000000000000000803F}
        XSamplingScale.Min = -1.000000000000000000
        XSamplingScale.Max = 1.000000000000000000
        XSamplingScale.Origin = -1.000000000000000000
        XSamplingScale.Step = 0.100000001490116100
        YSamplingScale.Min = -1.000000000000000000
        YSamplingScale.Max = 1.000000000000000000
        YSamplingScale.Origin = -1.000000000000000000
        YSamplingScale.Step = 0.100000001490116100
        ZSamplingScale.Min = -1.000000000000000000
        ZSamplingScale.Max = 1.000000000000000000
        ZSamplingScale.Origin = -1.000000000000000000
        ZSamplingScale.Step = 0.100000001490116100
        Parts = [gpY, gpZ]
      end
    end
  end
end
