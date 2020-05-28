object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Imposter'
  ClientHeight = 355
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 339
    Height = 355
    Camera = GLCamera1
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roDestinationAlpha]
    FieldOfView = 135.120773315429700000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 339
    Top = 0
    Width = 147
    Height = 355
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 1
    object LabelTexSize: TLabel
      Left = 8
      Top = 8
      Width = 37
      Height = 13
      Caption = 'TexSize'
    end
    object Label2: TLabel
      Left = 8
      Top = 80
      Width = 56
      Height = 13
      Caption = 'Sample Size'
    end
    object LabelFPS: TLabel
      Left = 6
      Top = 120
      Width = 18
      Height = 13
      Caption = 'FPS'
    end
    object CBShowTeapot: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Show Teapot'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBShowTeapotClick
    end
    object CBShowImposter: TCheckBox
      Left = 8
      Top = 52
      Width = 97
      Height = 17
      Caption = 'Show Imposters'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBShowImposterClick
    end
    object CBSampleSize: TComboBox
      Left = 72
      Top = 76
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemIndex = 3
      TabOrder = 2
      Text = '64'
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
