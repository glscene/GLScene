object FormBoxSphere: TFormBoxSphere
  Left = 281
  Top = 115
  Caption = 'Box Sphere Intersection'
  ClientHeight = 588
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 120
  TextHeight = 16
  object Viewer: TGLSceneViewer
    Left = 225
    Top = 0
    Width = 475
    Height = 588
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 156.222686767578100000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 225
    Height = 588
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    TabOrder = 1
    object Label5: TLabel
      Left = 13
      Top = 168
      Width = 73
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Sphere pos'
    end
    object Label3: TLabel
      Left = 20
      Top = 10
      Width = 76
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Box position'
    end
    object Label4: TLabel
      Left = 120
      Top = 10
      Width = 60
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Box scale'
    end
    object Label1: TLabel
      Left = 10
      Top = 480
      Width = 44
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Result:'
    end
    object Label7: TLabel
      Left = 120
      Top = 170
      Width = 89
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Sphere radius'
    end
    object CheckBox06: TCheckBox
      Left = 10
      Top = 364
      Width = 100
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Grid'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox04Click
    end
    object CheckBox04: TCheckBox
      Left = 10
      Top = 303
      Width = 100
      Height = 23
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Box visible'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox04Click
    end
    object CheckBox05: TCheckBox
      Left = 10
      Top = 334
      Width = 100
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Axis'
      TabOrder = 2
      OnClick = CheckBox04Click
    end
    object CheckBox07: TCheckBox
      Left = 10
      Top = 393
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Sphere visible '
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBox04Click
    end
    object Edit1: TEdit
      Left = 13
      Top = 31
      Width = 70
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 4
      Text = '12'
      OnChange = Edit1Change
    end
    object Edit2: TEdit
      Left = 13
      Top = 60
      Width = 70
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 5
      Text = '9'
      OnChange = Edit1Change
    end
    object Edit3: TEdit
      Left = 13
      Top = 90
      Width = 70
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 6
      Text = '6'
      OnChange = Edit1Change
    end
    object UpDown1: TUpDown
      Left = 83
      Top = 31
      Width = 27
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = Edit1
      Min = -100
      Position = 12
      TabOrder = 7
    end
    object UpDown2: TUpDown
      Left = 83
      Top = 60
      Width = 27
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = Edit2
      Min = -100
      Position = 9
      TabOrder = 8
    end
    object UpDown3: TUpDown
      Left = 83
      Top = 90
      Width = 27
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = Edit3
      Min = -100
      Position = 6
      TabOrder = 9
    end
    object Edit4: TEdit
      Left = 114
      Top = 30
      Width = 70
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 10
      Text = '10'
      OnChange = Edit1Change
    end
    object Edit5: TEdit
      Left = 114
      Top = 60
      Width = 70
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 11
      Text = '10'
      OnChange = Edit1Change
    end
    object Edit6: TEdit
      Left = 114
      Top = 90
      Width = 70
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 12
      Text = '10'
      OnChange = Edit1Change
    end
    object UpDown4: TUpDown
      Left = 184
      Top = 30
      Width = 27
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = Edit4
      Position = 10
      TabOrder = 13
    end
    object UpDown5: TUpDown
      Left = 184
      Top = 60
      Width = 27
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = Edit5
      Position = 10
      TabOrder = 14
    end
    object UpDown6: TUpDown
      Left = 184
      Top = 90
      Width = 27
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = Edit6
      Position = 10
      TabOrder = 15
    end
    object Edit7: TEdit
      Left = 13
      Top = 190
      Width = 70
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 16
      Text = '7'
      OnChange = Edit1Change
    end
    object Edit8: TEdit
      Left = 13
      Top = 220
      Width = 70
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 17
      Text = '-1'
      OnChange = Edit1Change
    end
    object Edit9: TEdit
      Left = 13
      Top = 250
      Width = 70
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 18
      Text = '6'
      OnChange = Edit1Change
    end
    object UpDown7: TUpDown
      Left = 83
      Top = 190
      Width = 27
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = Edit7
      Min = -100
      Position = 7
      TabOrder = 19
    end
    object UpDown8: TUpDown
      Left = 83
      Top = 220
      Width = 27
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = Edit8
      Min = -100
      Position = -1
      TabOrder = 20
    end
    object UpDown9: TUpDown
      Left = 83
      Top = 250
      Width = 27
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = Edit9
      Min = -100
      Position = 6
      TabOrder = 21
    end
    object Edit10: TEdit
      Left = 116
      Top = 190
      Width = 70
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 22
      Text = '5'
      OnChange = Edit1Change
    end
    object UpDown10: TUpDown
      Left = 186
      Top = 190
      Width = 28
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = Edit10
      Position = 5
      TabOrder = 23
    end
    object Button3: TButton
      Left = 50
      Top = 430
      Width = 94
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Recalc'
      TabOrder = 24
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 14
      Top = 123
      Width = 197
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
