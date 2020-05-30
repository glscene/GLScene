object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Box Sphere Intersection'
  ClientHeight = 454
  ClientWidth = 552
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
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 180
    Top = 0
    Width = 372
    Height = 454
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 149.907211303710900000
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 180
    Height = 454
    Align = alLeft
    TabOrder = 1
    object Label5: TLabel
      Left = 10
      Top = 134
      Width = 54
      Height = 13
      Caption = 'Sphere pos'
    end
    object Label3: TLabel
      Left = 16
      Top = 8
      Width = 58
      Height = 13
      Caption = 'Box position'
    end
    object Label4: TLabel
      Left = 96
      Top = 8
      Width = 45
      Height = 13
      Caption = 'Box scale'
    end
    object Label1: TLabel
      Left = 8
      Top = 384
      Width = 34
      Height = 13
      Caption = 'Result:'
    end
    object Label7: TLabel
      Left = 96
      Top = 136
      Width = 66
      Height = 13
      Caption = 'Sphere radius'
    end
    object CheckBox06: TCheckBox
      Left = 8
      Top = 291
      Width = 80
      Height = 17
      Caption = 'Grid'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox04Click
    end
    object CheckBox04: TCheckBox
      Left = 8
      Top = 242
      Width = 80
      Height = 19
      Caption = 'Box visible'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox04Click
    end
    object CheckBox05: TCheckBox
      Left = 8
      Top = 267
      Width = 80
      Height = 17
      Caption = 'Axis'
      TabOrder = 2
      OnClick = CheckBox04Click
    end
    object CheckBox07: TCheckBox
      Left = 8
      Top = 314
      Width = 97
      Height = 17
      Caption = 'Sphere visible '
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBox04Click
    end
    object Edit1: TEdit
      Left = 10
      Top = 25
      Width = 56
      Height = 21
      TabOrder = 4
      Text = '12'
      OnChange = Edit1Change
    end
    object Edit2: TEdit
      Left = 10
      Top = 48
      Width = 56
      Height = 21
      TabOrder = 5
      Text = '9'
      OnChange = Edit1Change
    end
    object Edit3: TEdit
      Left = 10
      Top = 72
      Width = 56
      Height = 21
      TabOrder = 6
      Text = '6'
      OnChange = Edit1Change
    end
    object UpDown1: TUpDown
      Left = 66
      Top = 25
      Width = 20
      Height = 21
      Associate = Edit1
      Min = -100
      Position = 12
      TabOrder = 7
    end
    object UpDown2: TUpDown
      Left = 66
      Top = 48
      Width = 20
      Height = 21
      Associate = Edit2
      Min = -100
      Position = 9
      TabOrder = 8
    end
    object UpDown3: TUpDown
      Left = 66
      Top = 72
      Width = 20
      Height = 21
      Associate = Edit3
      Min = -100
      Position = 6
      TabOrder = 9
    end
    object Edit4: TEdit
      Left = 91
      Top = 24
      Width = 56
      Height = 21
      TabOrder = 10
      Text = '10'
      OnChange = Edit1Change
    end
    object Edit5: TEdit
      Left = 91
      Top = 48
      Width = 56
      Height = 21
      TabOrder = 11
      Text = '10'
      OnChange = Edit1Change
    end
    object Edit6: TEdit
      Left = 91
      Top = 72
      Width = 56
      Height = 21
      TabOrder = 12
      Text = '10'
      OnChange = Edit1Change
    end
    object UpDown4: TUpDown
      Left = 147
      Top = 24
      Width = 20
      Height = 21
      Associate = Edit4
      Position = 10
      TabOrder = 13
    end
    object UpDown5: TUpDown
      Left = 147
      Top = 48
      Width = 20
      Height = 21
      Associate = Edit5
      Position = 10
      TabOrder = 14
    end
    object UpDown6: TUpDown
      Left = 147
      Top = 72
      Width = 20
      Height = 21
      Associate = Edit6
      Position = 10
      TabOrder = 15
    end
    object Edit7: TEdit
      Left = 10
      Top = 152
      Width = 56
      Height = 21
      TabOrder = 16
      Text = '7'
      OnChange = Edit1Change
    end
    object Edit8: TEdit
      Left = 10
      Top = 176
      Width = 56
      Height = 21
      TabOrder = 17
      Text = '-1'
      OnChange = Edit1Change
    end
    object Edit9: TEdit
      Left = 10
      Top = 200
      Width = 56
      Height = 21
      TabOrder = 18
      Text = '6'
      OnChange = Edit1Change
    end
    object UpDown7: TUpDown
      Left = 66
      Top = 152
      Width = 20
      Height = 21
      Associate = Edit7
      Min = -100
      Position = 7
      TabOrder = 19
    end
    object UpDown8: TUpDown
      Left = 66
      Top = 176
      Width = 20
      Height = 21
      Associate = Edit8
      Min = -100
      Position = -1
      TabOrder = 20
    end
    object UpDown9: TUpDown
      Left = 66
      Top = 200
      Width = 20
      Height = 21
      Associate = Edit9
      Min = -100
      Position = 6
      TabOrder = 21
    end
    object Edit10: TEdit
      Left = 93
      Top = 152
      Width = 56
      Height = 21
      TabOrder = 22
      Text = '5'
      OnChange = Edit1Change
    end
    object UpDown10: TUpDown
      Left = 149
      Top = 152
      Width = 20
      Height = 21
      Associate = Edit10
      Position = 5
      TabOrder = 23
    end
    object Button3: TButton
      Left = 40
      Top = 344
      Width = 75
      Height = 25
      Caption = 'Recalc'
      TabOrder = 24
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 11
      Top = 98
      Width = 158
      Height = 25
      Caption = 'Random box rotation'
      Default = True
      TabOrder = 25
      OnClick = Button4Click
    end
  end
  object GLScene: TGLScene
    Left = 216
    Top = 16
    object DCCamTarget: TGLDummyCube
      CubeSize = 0.100000001490116100
      VisibleAtRunTime = True
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00007A4400004844000016440000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000FAC30000C8C3000096C30000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCube1: TGLCube
      Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      Direction.Coordinates = {00000000000000800000803F00000000}
    end
    object DCCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
    end
    object GLSphere1: TGLSphere
      Material.FrontProperties.Diffuse.Color = {000000000000803F000000000000803F}
      Radius = 0.500000000000000000
    end
    object GLSphere2: TGLSphere
      Material.FrontProperties.Diffuse.Color = {000000000000803F000000000000803F}
      Material.PolygonMode = pmLines
      Radius = 0.500000000000000000
    end
    object GLXYZGrid1: TGLXYZGrid
      XSamplingScale.Min = -10.000000000000000000
      XSamplingScale.Max = 10.000000000000000000
      XSamplingScale.Step = 3.000000000000000000
      YSamplingScale.Min = -10.000000000000000000
      YSamplingScale.Max = 10.000000000000000000
      YSamplingScale.Step = 3.000000000000000000
      ZSamplingScale.Step = 0.100000001490116100
    end
    object GLLines1: TGLLines
      Visible = False
      LineWidth = 3.000000000000000000
      Nodes = <
        item
          Color.Color = {0000803F00000000000000000000803F}
        end
        item
          X = 1.000000000000000000
          Color.Color = {0000803F00000000000000000000803F}
        end
        item
          Color.Color = {000000000000003F000000000000803F}
        end
        item
          Y = 1.000000000000000000
          Color.Color = {0AD7A33E48E1FA3E1F85EB3E0000803F}
        end
        item
        end
        item
          Z = 1.000000000000000000
        end>
      NodesAspect = lnaInvisible
      SplineMode = lsmSegments
      Options = [loUseNodeColorForLines]
    end
    object GLLines3: TGLLines
      LineColor.Color = {0000803F0000803F000000000000803F}
      LineWidth = 10.000000000000000000
      Nodes = <>
      NodesAspect = lnaInvisible
      Division = 1
      SplineMode = lsmSegments
      Options = []
    end
    object GLCamera1: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 50.000000000000000000
      NearPlaneBias = 0.100000001490116100
      TargetObject = DCCamTarget
      Position.Coordinates = {0000A04000000040000040400000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Left = 256
      Top = 144
    end
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    OnProgress = GLCadencerProgress
    Left = 280
    Top = 16
  end
end
