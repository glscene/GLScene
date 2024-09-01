object FormOdeConveyor: TFormOdeConveyor
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Conveyor'
  ClientHeight = 684
  ClientWidth = 1022
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 168
  TextHeight = 23
  object GLSceneViewer1: TGLSceneViewer
    Left = 338
    Top = 0
    Width = 684
    Height = 684
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 163.364700317382800000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 338
    Height = 684
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
      OnChange = FDirChange
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
      OnChange = FDirChange
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
      OnChange = FDirChange
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
    Left = 304
    Top = 24
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
        0458434F4C02010201060C54474C4F4445537461746963020012000000000200
        0200060D474C4F44454D616E616765723102000500000000006F1283F53F0800
        000500000000000000FA08400500000000000000000000050000000000000000
        0000050000000000000000000005000000000000000000000500000000000000
        0000000500000000000000000000050000000000000000000005000000000000
        00000000050000000000000000000002000458434F4C02010201061254474C4F
        4445456C656D656E74506C616E65020102001200000000020002000802000802
        0008050000000000000080FF3F}
      CubeSize = {00000040CDCCCC3D0000A041}
    end
    object GLCube1: TGLCube
      Position.Coordinates = {000000000000803F000000C10000803F}
      BehavioursData = {
        0458434F4C02010201060D54474C4F444544796E616D69630200120000000002
        000200060D474C4F44454D616E616765723102000500000000006F1283F53F08
        00000500000000000000FA084005000000000000000000000500000000000000
        0000000500000000000000000000050000000000000000000005000000000000
        0000000005000000000000000000000500000000000000000000050000000000
        0000000000050000000000000000000002010458434F4C02010201061054474C
        4F4445456C656D656E74426F7802001200000000020002000802000802000805
        0000000000000080FF3F0200050000000000000080FF3F050000000000000080
        FF3F050000000000000080FF3F09}
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
    Left = 488
    Top = 16
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
    Left = 632
    Top = 16
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
    Left = 304
    Top = 96
  end
end
