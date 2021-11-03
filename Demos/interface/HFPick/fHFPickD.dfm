object FormHFPick: TFormHFPick
  Left = 133
  Top = 72
  Caption = 'Heightfield Pick'
  ClientHeight = 561
  ClientWidth = 721
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 594
    Height = 561
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 159.786010742187500000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 594
    Top = 0
    Width = 127
    Height = 561
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 10
      Width = 108
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Last Coord.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 30
      Top = 40
      Width = 41
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'X   Y'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 10
      Top = 210
      Width = 111
      Height = 181
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 
        'This demo uses a crude method for heightfield picking based on t' +
        'he Z-Buffer. This method can be easily adapted for a variety of ' +
        'objects and 2.5D problems.'
      WordWrap = True
    end
    object RBPaint: TRadioButton
      Left = 20
      Top = 100
      Width = 61
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Paint'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 20
      Top = 140
      Width = 71
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Rotate'
      TabOrder = 1
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 32
    object GLLightSource1: TGLLightSource
      Ambient.Color = {9A99193E9A99193E9A99193E0000803F}
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {9A99593F9A99593F9A99593F0000803F}
      Position.Coordinates = {0000E040000070410000A0400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = HeightField
      Position.Coordinates = {0000A04000008040000040400000803F}
    end
    object HeightField: TGLHeightField
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {9A99193F9A99193F0000003F00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      XSamplingScale.Min = -5.500000000000000000
      XSamplingScale.Max = 5.500000000000000000
      XSamplingScale.Step = 0.200000002980232200
      YSamplingScale.Min = -5.500000000000000000
      YSamplingScale.Max = 5.500000000000000000
      YSamplingScale.Step = 0.200000002980232200
      ColorMode = hfcmAmbientAndDiffuse
      OnGetHeight = HeightFieldGetHeight
    end
  end
end
