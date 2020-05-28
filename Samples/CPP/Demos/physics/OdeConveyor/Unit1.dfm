object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ODE Conveyor'
  ClientHeight = 422
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 193
    Top = 0
    Width = 365
    Height = 422
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 149.356979370117200000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 193
    Height = 422
    Align = alLeft
    Caption = ' '
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 54
      Width = 100
      Height = 13
      Caption = 'Conveyor belt speed'
    end
    object Label2: TLabel
      Left = 16
      Top = 118
      Width = 90
      Height = 13
      Caption = 'Friction Coefficient'
    end
    object FrictionFeedback: TLabel
      Left = 71
      Top = 140
      Width = 8
      Height = 13
      Caption = '='
    end
    object Label3: TLabel
      Left = 16
      Top = 174
      Width = 91
      Height = 13
      Caption = 'Conveyor direction'
    end
    object Label4: TLabel
      Left = 18
      Top = 204
      Width = 6
      Height = 13
      Caption = 'X'
    end
    object Label5: TLabel
      Left = 18
      Top = 231
      Width = 6
      Height = 13
      Caption = 'Y'
    end
    object Label6: TLabel
      Left = 18
      Top = 258
      Width = 6
      Height = 13
      Caption = 'Z'
    end
    object NormZ: TLabel
      Left = 85
      Top = 259
      Width = 6
      Height = 13
      Caption = 'Z'
    end
    object NormY: TLabel
      Left = 85
      Top = 232
      Width = 6
      Height = 13
      Caption = 'Y'
    end
    object NormX: TLabel
      Left = 85
      Top = 205
      Width = 6
      Height = 13
      Caption = 'X'
    end
    object TrackBarMotionSpeed: TTrackBar
      Left = 8
      Top = 73
      Width = 179
      Height = 45
      Max = 20
      TabOrder = 0
      OnChange = TrackBarMotionSpeedChange
    end
    object Friction: TEdit
      Left = 16
      Top = 137
      Width = 49
      Height = 21
      TabOrder = 1
      OnChange = FrictionChange
    end
    object FDirX: TEdit
      Left = 30
      Top = 201
      Width = 49
      Height = 21
      TabOrder = 2
    end
    object FDirY: TEdit
      Left = 30
      Top = 228
      Width = 49
      Height = 21
      TabOrder = 3
    end
    object FDirZ: TEdit
      Left = 30
      Top = 255
      Width = 49
      Height = 21
      TabOrder = 4
    end
    object AddODECube: TButton
      Left = 16
      Top = 8
      Width = 90
      Height = 25
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
        01060E544F4445456C656D656E74426F7802000603426F780200020008020008
        020008050000000000000080FF3F020005000000000000008000400500000000
        00CDCCCCFB3F0500000000000000A00340}
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
        010201060E544F4445456C656D656E74426F7802000603426F78020002000802
        0008020008050000000000000080FF3F0200050000000000000080FF3F050000
        000000000080FF3F050000000000000080FF3F09}
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
