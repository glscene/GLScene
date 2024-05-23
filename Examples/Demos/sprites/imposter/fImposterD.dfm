object FormImposter: TFormImposter
  Left = 110
  Top = 65
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Imposter'
  ClientHeight = 583
  ClientWidth = 954
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 24
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 697
    Height = 583
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roDestinationAlpha]
    FieldOfView = 152.993667602539100000
    PenAsTouch = False
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 697
    Top = 0
    Width = 257
    Height = 583
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 1
    object LabelTexSize: TLabel
      Left = 14
      Top = 14
      Width = 69
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'TexSize'
    end
    object Label2: TLabel
      Left = 14
      Top = 140
      Width = 105
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Sample Size'
    end
    object LabelFPS: TLabel
      Left = 11
      Top = 210
      Width = 36
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'FPS'
    end
    object CBShowTeapot: TCheckBox
      Left = 14
      Top = 56
      Width = 170
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Show Teapot'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBShowTeapotClick
    end
    object CBShowImposter: TCheckBox
      Left = 14
      Top = 91
      Width = 170
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Show Imposters'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBShowImposterClick
    end
    object CBSampleSize: TComboBox
      Left = 126
      Top = 133
      Width = 86
      Height = 32
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      ItemIndex = 3
      TabOrder = 2
      Text = '64'
      OnChange = CBSampleSizeChange
      Items.Strings = (
        '8'
        '16'
        '32'
        '64'
        '128'
        '256')
    end
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 24
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLSkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Bands = <
        item
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 15.000000000000000000
        end
        item
          StartAngle = 15.000000000000000000
          StopAngle = 90.000000000000000000
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Stacks = 4
        end
        item
          StartAngle = -90.000000000000000000
          StartColor.Color = {0000000000000000000000000000803F}
          StopColor.Color = {0000803F0000803F0000803F0000803F}
        end>
      Stars = <>
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLTeapot1: TGLTeapot
      Scale.Coordinates = {00000040000000400000004000000000}
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00004842000034420000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 200.000000000000000000
      FocalLength = 70.000000000000000000
      TargetObject = GLDirectOpenGL1
      Position.Coordinates = {00004040000000400000A0400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 72
    Top = 24
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 48
    Top = 64
  end
end
