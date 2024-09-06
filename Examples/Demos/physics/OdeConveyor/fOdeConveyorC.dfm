object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'ODE Conveyor'
  ClientHeight = 739
  ClientWidth = 991
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 168
  TextHeight = 23
  object GLSceneViewer1: TGLSceneViewer
    Left = 338
    Top = 0
    Width = 653
    Height = 739
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 162.586807250976600000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 338
    Height = 739
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    Caption = ' '
    TabOrder = 1
    object Label1: TLabel
      Left = 28
      Top = 95
      Width = 170
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Conveyor belt speed'
    end
    object Label2: TLabel
      Left = 28
      Top = 207
      Width = 154
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Friction Coefficient'
    end
    object FrictionFeedback: TLabel
      Left = 124
      Top = 245
      Width = 14
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '='
    end
    object Label3: TLabel
      Left = 28
      Top = 305
      Width = 155
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Conveyor direction'
    end
    object Label4: TLabel
      Left = 32
      Top = 357
      Width = 11
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'X'
    end
    object Label5: TLabel
      Left = 32
      Top = 404
      Width = 11
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Y'
    end
    object Label6: TLabel
      Left = 32
      Top = 452
      Width = 11
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Z'
    end
    object NormZ: TLabel
      Left = 149
      Top = 453
      Width = 11
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Z'
    end
    object NormY: TLabel
      Left = 149
      Top = 406
      Width = 11
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Y'
    end
    object NormX: TLabel
      Left = 149
      Top = 359
      Width = 11
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'X'
    end
    object TrackBarMotionSpeed: TTrackBar
      Left = 14
      Top = 128
      Width = 313
      Height = 79
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Max = 20
      TabOrder = 0
      ThumbLength = 35
      OnChange = TrackBarMotionSpeedChange
    end
    object Friction: TEdit
      Left = 28
      Top = 240
      Width = 86
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 1
      OnChange = FrictionChange
    end
    object FDirX: TEdit
      Left = 53
      Top = 352
      Width = 85
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 2
    end
    object FDirY: TEdit
      Left = 53
      Top = 399
      Width = 85
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 3
    end
    object FDirZ: TEdit
      Left = 53
      Top = 446
      Width = 85
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 4
    end
    object AddODECube: TButton
      Left = 28
      Top = 14
      Width = 158
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Add Cube'
      TabOrder = 5
      OnClick = AddODECubeClick
    end
  end
  object GLScene1: TGLScene
    Left = 208
    Top = 8
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLCube1
      Position.Coordinates = {0000A0400000A040000020C10000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {00000040000000400000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object ConveyorBelt1: TGLCube
      Material.FrontProperties.Diffuse.Color = {C9C8C83EC9C8C83EC9C8C83E0000803F}
      BehavioursData = {
        0458434F4C02010201060C54474C4F44455374617469630200060A4F44452053
        746174696302000200060D474C4F44454D616E61676572310200050000000000
        00000000000822000500000000000000C8054005000000000000000000000500
        0000000000000000000500000000000000000000050000000000000000000005
        0000000000000000000005000000000000000000000500000000000000000000
        0500000000000000000000050000000000000000000002000458434F4C020102
        01061254474C4F4445456C656D656E74506C616E65020102000605506C616E65
        0200020008020008020008050000000000000080FF3F}
      CubeSize = {00000040CDCCCC3D0000A041}
    end
    object GLCube1: TGLCube
      Position.Coordinates = {000000000000803F000000C10000803F}
      BehavioursData = {
        0458434F4C02010201060D54474C4F444544796E616D69630200060B4F444520
        44796E616D696302000200060D474C4F44454D616E6167657231020005000000
        00006F1283F53F08000005000000000000000000000500000000000000000000
        0500000000000000000000050000000000000000000005000000000000000000
        0005000000000000000000000500000000000000000000050000000000000000
        00000500000000000000000000050000000000000000000002010458434F4C02
        010201061054474C4F4445456C656D656E74426F7802000603426F7802000200
        08020008020008050000000000000080FF3F0200050000000000000080FF3F05
        0000000000000080FF3F050000000000000080FF3F09}
    end
    object GLRenderPoint1: TGLRenderPoint
    end
    object SpawnPoint: TGLDummyCube
      Position.Coordinates = {0000000000004040000000C10000803F}
      CubeSize = 1.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 280
    Top = 8
  end
  object GLODEManager1: TGLODEManager
    Gravity.Coordinates = {00000000C3F51CC1000000000000803F}
    OnCollision = GLODEManager1Collision
    Solver = osmDefault
    Iterations = 3
    MaxContacts = 8
    RenderPoint = GLRenderPoint1
    Visible = True
    VisibleAtRunTime = True
    Left = 360
    Top = 8
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Conveyor - %FPS'
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
    Left = 280
    Top = 64
  end
end
