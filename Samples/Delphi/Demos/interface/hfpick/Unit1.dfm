object Form1: TForm1
  Left = 133
  Top = 72
  Caption = 'Heightfield Pick'
  ClientHeight = 449
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 475
    Height = 449
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 154.888351440429700000
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 475
    Top = 0
    Width = 102
    Height = 449
    Align = alRight
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 82
      Height = 18
      Caption = 'Last Coord.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 24
      Top = 32
      Width = 32
      Height = 18
      Caption = 'X   Y'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 168
      Width = 89
      Height = 145
      AutoSize = False
      Caption = 
        'This demo uses a crude method for heightfield picking based on t' +
        'he Z-Buffer. This method can be easily adapted for a variety of ' +
        'objects and 2.5D problems.'
      WordWrap = True
    end
    object RBPaint: TRadioButton
      Left = 16
      Top = 80
      Width = 49
      Height = 17
      Caption = 'Paint'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 16
      Top = 112
      Width = 57
      Height = 17
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
