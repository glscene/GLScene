object FormMeshExplosion: TFormMeshExplosion
  Left = 174
  Top = 93
  Caption = 'Mesh Explosion'
  ClientHeight = 523
  ClientWidth = 718
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 718
    Height = 401
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = Camera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 151.994766235351600000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 401
    Width = 718
    Height = 122
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label2: TLabel
      Left = 10
      Top = 70
      Width = 28
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Step'
    end
    object Label1: TLabel
      Left = 10
      Top = 10
      Width = 60
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'MaxSteps'
    end
    object Label3: TLabel
      Left = 530
      Top = 10
      Width = 41
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Speed'
    end
    object CheckOn: TCheckBox
      Left = 610
      Top = 80
      Width = 95
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Explosion'
      TabOrder = 0
      OnClick = CheckOnClick
    end
    object Button1: TButton
      Left = 510
      Top = 80
      Width = 94
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Reset'
      TabOrder = 1
      OnClick = Button1Click
    end
    object StepBar: TProgressBar
      Left = 10
      Top = 90
      Width = 491
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 200
      Smooth = True
      TabOrder = 2
    end
    object MaxStepsBar: TTrackBar
      Left = 0
      Top = 30
      Width = 511
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 200
      TabOrder = 3
      ThumbLength = 19
      OnChange = MaxStepsBarChange
    end
    object SpeedBar: TTrackBar
      Left = 520
      Top = 30
      Width = 151
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 20
      Position = 1
      TabOrder = 4
      ThumbLength = 19
      OnChange = SpeedBarChange
    end
  end
  object GLScene1: TGLScene
    Left = 80
    Top = 16
    object mesh: TGLFreeForm
      Scale.Coordinates = {CDCCCC3ECDCCCC3ECDCCCC3E00000000}
      NormalsOrientation = mnoInvert
      EffectsData = {
        0458434F4C02010201060F54474C424578706C6F73696F6E4658020012000000
        0002000200}
    end
    object Camera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = mesh
      Position.Coordinates = {0000000000004842000000000000803F}
      Direction.Coordinates = {00000000000080BF0000000000000000}
      Up.Coordinates = {E8DC723F000000009BE8A1BE00000000}
      Left = 328
      Top = 216
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
        SpotDirection.Coordinates = {00000000000000000000803F00000000}
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 80
    Top = 72
  end
end
