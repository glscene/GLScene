object MainForm: TMainForm
  Left = 307
  Top = 542
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Sky Clouds'
  ClientHeight = 788
  ClientWidth = 1100
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 24
  object GLSV: TGLSceneViewer
    Left = 127
    Top = 0
    Width = 973
    Height = 788
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = cam
    FieldOfView = 154.265792846679700000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSVMouseDown
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 127
    Height = 788
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    Alignment = taRightJustify
    TabOrder = 1
    object Label1: TLabel
      Left = 5
      Top = 184
      Width = 79
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'N = Night'
    end
    object Label2: TLabel
      Left = 5
      Top = 84
      Width = 66
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'D = Day'
    end
    object Label3: TLabel
      Left = 4
      Top = 117
      Width = 115
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'C = Weather1'
    end
    object Label4: TLabel
      Left = 4
      Top = 151
      Width = 114
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'S = Weather2'
    end
    object Label5: TLabel
      Left = 28
      Top = 14
      Width = 46
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Keys:'
    end
    object PanelFPS: TPanel
      Left = 5
      Top = 238
      Width = 168
      Height = 72
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'FPS'
      TabOrder = 0
    end
  end
  object GLScene: TGLScene
    Left = 120
    Top = 20
    object SbBackground: TGLSkyBox
      CloudsPlaneOffset = 0.200000002980232200
      CloudsPlaneSize = 32.000000000000000000
      object Moons: TGLDummyCube
        Position.Coordinates = {0000A0C1000020410000A0C10000803F}
        CubeSize = 1.000000000000000000
        object sprMasser: TGLSprite
          Width = 10.000000000000000000
          Height = 10.000000000000000000
          Rotation = 0.000000000000000000
        end
        object sprSecunda: TGLSprite
          Position.Coordinates = {00000000000000000000A0400000803F}
          Width = 5.000000000000000000
          Height = 5.000000000000000000
          Rotation = 0.000000000000000000
        end
      end
      object sprSun: TGLSprite
        Position.Coordinates = {000040C000004040000020C10000803F}
        Width = 5.000000000000000000
        Height = 5.000000000000000000
        Rotation = 0.000000000000000000
      end
    end
    object SbClouds: TGLSkyBox
      Position.Coordinates = {0000A0C1000020410000A0C10000803F}
      CloudsPlaneOffset = 0.200000002980232200
      CloudsPlaneSize = 32.000000000000000000
    end
    object dc_cam: TGLDummyCube
      CubeSize = 1.000000000000000000
      object cam: TGLCamera
        DepthOfView = 500.000000000000000000
        FocalLength = 90.000000000000000000
        TargetObject = dc_cam
        Position.Coordinates = {0000204100000000000020410000803F}
      end
    end
  end
  object Cadencer: TGLCadencer
    Scene = GLScene
    OnProgress = CadencerProgress
    Left = 196
    Top = 20
  end
  object Timer: TGLAsyncTimer
    OnTimer = TimerTimer
    ThreadPriority = tpNormal
    Left = 120
    Top = 108
  end
  object MatLib: TGLMaterialLibrary
    Left = 196
    Top = 108
  end
  object CgBackground: TCgShader
    FragmentProgram.OnApply = CgBackgroundApplyFP
    FragmentProgram.OnUnApply = CgBackgroundUnApplyFP
    OnApplyFP = CgBackgroundApplyFP
    OnUnApplyFP = CgBackgroundUnApplyFP
    Left = 154
    Top = 244
  end
  object CgClouds: TCgShader
    VertexProgram.OnApply = CgCloudsApplyVP
    FragmentProgram.OnApply = CgCloudsApplyFP
    FragmentProgram.OnUnApply = CgCloudsUnApplyFP
    OnApplyVP = CgCloudsApplyVP
    OnApplyFP = CgCloudsApplyFP
    OnUnApplyFP = CgCloudsUnApplyFP
    Left = 258
    Top = 244
  end
  object CgMasser: TCgShader
    FragmentProgram.OnApply = CgMasserApplyFP
    FragmentProgram.OnUnApply = CgMasserUnApplyFP
    OnApplyFP = CgMasserApplyFP
    OnUnApplyFP = CgMasserUnApplyFP
    Left = 354
    Top = 244
  end
  object CgSecunda: TCgShader
    FragmentProgram.OnApply = CgSecundaApplyFP
    FragmentProgram.OnUnApply = CgSecundaUnApplyFP
    OnApplyFP = CgSecundaApplyFP
    OnUnApplyFP = CgSecundaUnApplyFP
    Left = 458
    Top = 244
  end
  object CgSun: TCgShader
    FragmentProgram.OnApply = CgSunApplyFP
    FragmentProgram.OnUnApply = CgSunUnApplyFP
    OnApplyFP = CgSunApplyFP
    OnUnApplyFP = CgSunUnApplyFP
    Left = 554
    Top = 244
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSV
    FormCaption = 'Sky Clouds - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 136
    Top = 224
  end
end
