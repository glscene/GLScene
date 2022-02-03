object Form1: TForm1
  Left = 192
  Top = 121
  Caption = 'Fxy '
  ClientHeight = 670
  ClientWidth = 846
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 120
  TextHeight = 16
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 568
    Height = 670
    Camera = Camera
    Buffer.BackgroundColor = clTeal
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 132.487487792968800000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 568
    Top = 0
    Width = 278
    Height = 670
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 83
      Top = 50
      Width = 101
      Height = 16
      Caption = 'XYZ grid position'
    end
    object TrackBar1: TTrackBar
      Left = 216
      Top = 89
      Width = 45
      Height = 321
      Min = -10
      Orientation = trVertical
      TabOrder = 0
      ThumbLength = 25
      OnChange = TrackBar1Change
    end
    object TrackBar2: TTrackBar
      Left = 36
      Top = 89
      Width = 45
      Height = 321
      Min = -10
      Orientation = trVertical
      TabOrder = 1
      ThumbLength = 25
      OnChange = TrackBar2Change
    end
    object TrackBar3: TTrackBar
      Left = 126
      Top = 89
      Width = 45
      Height = 321
      Min = -10
      Orientation = trVertical
      TabOrder = 2
      ThumbLength = 25
      OnChange = TrackBar3Change
    end
    object CheckBox1: TCheckBox
      Left = 80
      Top = 14
      Width = 121
      Height = 21
      Caption = 'Centered Grids'
      TabOrder = 3
      OnClick = CheckBox1Click
    end
    object RadioGroup1: TRadioGroup
      Left = 20
      Top = 431
      Width = 271
      Height = 132
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Formula'
      ItemIndex = 3
      Items.Strings = (
        'x*y'
        'x*y*z'
        'sin(z*12)/(2*(z*6.28+1))'
        '(pow(x,2)+pow(y,2))*sin(8*atan2(x,y))')
      TabOrder = 4
      OnClick = RadioGroup1Click
    end
  end
  object GLScene1: TGLScene
    Left = 80
    Top = 48
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000F041000048420000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLHeightField1: TGLHeightField
      Direction.Coordinates = {0044F4970000803F2EBD3BB300000000}
      Scale.Coordinates = {00000040000000400000004000000000}
      ShowAxes = True
      Up.Coordinates = {0000803FEDAD09A72EBD3BB300000000}
      OnPicked = CheckBox1Click
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
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 125.000000000000000000
      TargetObject = GLHeightField1
      Position.Coordinates = {0000404000008040000000410000803F}
    end
  end
end
