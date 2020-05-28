object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Pawn'
  ClientHeight = 403
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 419
    Height = 403
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 152.128311157226600000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 419
    Top = 0
    Width = 129
    Height = 403
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 23
      Top = 8
      Width = 59
      Height = 18
      Caption = 'Options'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 38
      Top = 149
      Width = 26
      Height = 13
      Caption = 'Slices'
    end
    object Label4: TLabel
      Left = 40
      Top = 205
      Width = 41
      Height = 13
      Caption = 'Divisions'
    end
    object Label2: TLabel
      Left = 44
      Top = 247
      Width = 22
      Height = 13
      Caption = 'Stop'
    end
    object LabelTri: TLabel
      Left = 16
      Top = 312
      Width = 43
      Height = 13
      Caption = 'Triangles'
    end
    object CheckBox1: TCheckBox
      Left = 9
      Top = 49
      Width = 113
      Height = 17
      Caption = 'Spline interpolation'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 9
      Top = 72
      Width = 113
      Height = 17
      Caption = 'Normals smoothing'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 9
      Top = 97
      Width = 113
      Height = 17
      Caption = 'Texture map'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox3Click
    end
    object CheckBox4: TCheckBox
      Left = 9
      Top = 120
      Width = 113
      Height = 17
      Caption = 'Modulate texture'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBox4Click
    end
    object TrackBar2: TTrackBar
      Left = 9
      Top = 168
      Width = 113
      Height = 17
      Max = 64
      Min = 4
      Frequency = 16
      Position = 24
      TabOrder = 4
      ThumbLength = 10
      OnChange = TrackBar2Change
    end
    object TrackBar3: TTrackBar
      Left = 9
      Top = 224
      Width = 113
      Height = 17
      Max = 30
      Min = 1
      Frequency = 10
      Position = 10
      TabOrder = 5
      ThumbLength = 10
      OnChange = TrackBar3Change
    end
    object TrackBar1: TTrackBar
      Left = 9
      Top = 266
      Width = 113
      Height = 17
      Max = 360
      Min = 30
      Frequency = 45
      Position = 360
      TabOrder = 6
      ThumbLength = 10
      OnChange = TrackBar1Change
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object RotationSolid1: TGLRevolutionSolid
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Nodes = <
          item
            Y = 0.899999976158142100
          end
          item
            X = 0.400000005960464500
            Y = 0.800000011920929000
          end
          item
            X = 0.449999988079071000
            Y = 0.500000000000000000
          end
          item
            X = 0.250000000000000000
            Y = 0.300000011920929000
          end
          item
            X = 0.250000000000000000
            Y = -0.100000001490116100
          end
          item
            X = 0.600000023841857900
            Y = -0.500000000000000000
          end
          item
            X = 0.600000023841857900
            Y = -0.899999976158142100
          end
          item
            X = 0.589999973773956300
            Y = -0.949999988079071000
          end
          item
            X = 0.600000023841857900
            Y = -1.000000000000000000
          end
          item
            X = 0.600000023841857900
            Y = -1.000000000000000000
          end
          item
            X = 0.500000000000000000
            Y = -1.000000000000000000
          end
          item
            Y = -1.000000000000000000
          end>
        SplineMode = lsmCubicSpline
        Slices = 24
        Normals = nsSmooth
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000804000000000000000000000803F}
      Left = 208
      Top = 136
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 40
  end
end
