object FormMeshExplosion: TFormMeshExplosion
  Left = 0
  Top = 0
  Caption = 'Mesh Explosion'
  ClientHeight = 541
  ClientWidth = 716
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 716
    Height = 444
    Camera = Camera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 154.614669799804700000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
    TabOrder = 0
    ExplicitWidth = 588
    ExplicitHeight = 325
  end
  object Panel1: TPanel
    Left = 0
    Top = 444
    Width = 716
    Height = 97
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 324
    ExplicitWidth = 588
    object Label2: TLabel
      Left = 8
      Top = 56
      Width = 22
      Height = 13
      Caption = 'Step'
    end
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 47
      Height = 13
      Caption = 'MaxSteps'
    end
    object Label3: TLabel
      Left = 424
      Top = 8
      Width = 30
      Height = 13
      Caption = 'Speed'
    end
    object CheckOn: TCheckBox
      Left = 488
      Top = 64
      Width = 82
      Height = 17
      Caption = 'Explosion'
      TabOrder = 0
      OnClick = CheckOnClick
    end
    object Button1: TButton
      Left = 408
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Reset'
      TabOrder = 1
      OnClick = Button1Click
    end
    object StepBar: TProgressBar
      Left = 8
      Top = 72
      Width = 393
      Height = 16
      Max = 200
      Smooth = True
      TabOrder = 2
    end
    object MaxStepsBar: TTrackBar
      Left = 1
      Top = 25
      Width = 409
      Height = 25
      Max = 200
      TabOrder = 3
      ThumbLength = 15
      OnChange = MaxStepsBarChange
    end
    object SpeedBar: TTrackBar
      Left = 416
      Top = 24
      Width = 121
      Height = 25
      Max = 20
      Position = 1
      TabOrder = 4
      ThumbLength = 15
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
