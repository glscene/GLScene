object FormFxy: TFormFxy
  Left = 179
  Top = 106
  BorderWidth = 4
  Caption = 'Fxy'
  ClientHeight = 576
  ClientWidth = 741
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 530
    Height = 576
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 129.493667602539100000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 530
    Top = 0
    Width = 211
    Height = 576
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 58
      Top = 429
      Width = 103
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'XYZ Grid position'
    end
    object Label2: TLabel
      Left = 28
      Top = 86
      Width = 8
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'X'
    end
    object Label3: TLabel
      Left = 96
      Top = 86
      Width = 9
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Y'
    end
    object Label4: TLabel
      Left = 158
      Top = 86
      Width = 8
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Z'
    end
    object CBCentered: TCheckBox
      Left = 60
      Top = 31
      Width = 121
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Centered grids'
      TabOrder = 0
      OnClick = CBCenteredClick
    end
    object TBXYPosition: TTrackBar
      Left = 143
      Top = 110
      Width = 56
      Height = 311
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Min = -10
      Orientation = trVertical
      TabOrder = 1
      ThumbLength = 25
      TickMarks = tmBoth
      OnChange = TBXYPositionChange
    end
    object TBYZPosition: TTrackBar
      Left = 15
      Top = 110
      Width = 56
      Height = 311
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Min = -10
      Orientation = trVertical
      TabOrder = 2
      ThumbLength = 25
      TickMarks = tmBoth
      OnChange = TBYZPositionChange
    end
    object TBXZPosition: TTrackBar
      Left = 79
      Top = 110
      Width = 56
      Height = 311
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Min = -10
      Orientation = trVertical
      TabOrder = 3
      ThumbLength = 25
      TickMarks = tmBoth
      OnChange = TBXZPositionChange
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000F041000048420000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 125.000000000000000000
      TargetObject = HeightField1
      Position.Coordinates = {0000404000008040000080400000803F}
      Left = 208
      Top = 168
    end
    object HeightField1: TGLHeightField
      Direction.Coordinates = {0044F4970000803F2EBD3BB300000000}
      ShowAxes = True
      Up.Coordinates = {0000803F583DAF262EBD3B3300000000}
      XSamplingScale.Min = -1.000000000000000000
      XSamplingScale.Max = 1.000000000000000000
      XSamplingScale.Step = 0.009999999776482582
      YSamplingScale.Min = -1.000000000000000000
      YSamplingScale.Max = 1.000000000000000000
      YSamplingScale.Step = 0.009999999776482582
      OnGetHeight = HeightField1GetHeight
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
      object XYGrid: TGLXYZGrid
        LineColor.Color = {00000000000000000000803F0000803F}
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
      end
    end
  end
  object GLAsyncTimer1: TGLAsyncTimer
    OnTimer = GLAsyncTimer1Timer
    Left = 125
    Top = 21
  end
end
