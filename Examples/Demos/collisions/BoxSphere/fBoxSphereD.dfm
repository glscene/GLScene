object FormBoxSphere: TFormBoxSphere
  Left = 281
  Top = 115
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Box Sphere Intersection'
  ClientHeight = 823
  ClientWidth = 1318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 168
  TextHeight = 22
  object Viewer: TGLSceneViewer
    Left = 315
    Top = 0
    Width = 1003
    Height = 823
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 166.144287109375000000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 315
    Height = 823
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 1
    object Label5: TLabel
      Left = 18
      Top = 235
      Width = 99
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Sphere pos'
    end
    object Label3: TLabel
      Left = 28
      Top = 14
      Width = 104
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Box position'
    end
    object Label4: TLabel
      Left = 168
      Top = 14
      Width = 83
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Box scale'
    end
    object Label1: TLabel
      Left = 14
      Top = 672
      Width = 59
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Result:'
    end
    object Label7: TLabel
      Left = 168
      Top = 238
      Width = 118
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Sphere radius'
    end
    object CheckBox06: TCheckBox
      Left = 14
      Top = 509
      Width = 140
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Grid'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox04Click
    end
    object CheckBox04: TCheckBox
      Left = 14
      Top = 424
      Width = 140
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Box visible'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox04Click
    end
    object CheckBox05: TCheckBox
      Left = 14
      Top = 467
      Width = 140
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Axis'
      TabOrder = 2
      OnClick = CheckBox04Click
    end
    object CheckBox07: TCheckBox
      Left = 14
      Top = 550
      Width = 170
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Sphere visible '
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBox04Click
    end
    object Edit1: TEdit
      Left = 18
      Top = 44
      Width = 98
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 4
      Text = '12'
      OnChange = Edit1Change
    end
    object Edit2: TEdit
      Left = 18
      Top = 84
      Width = 98
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 5
      Text = '9'
      OnChange = Edit1Change
    end
    object Edit3: TEdit
      Left = 18
      Top = 126
      Width = 98
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 6
      Text = '6'
      OnChange = Edit1Change
    end
    object UpDown1: TUpDown
      Left = 116
      Top = 44
      Width = 38
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = Edit1
      Min = -100
      Position = 12
      TabOrder = 7
    end
    object UpDown2: TUpDown
      Left = 116
      Top = 84
      Width = 38
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = Edit2
      Min = -100
      Position = 9
      TabOrder = 8
    end
    object UpDown3: TUpDown
      Left = 116
      Top = 126
      Width = 38
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = Edit3
      Min = -100
      Position = 6
      TabOrder = 9
    end
    object Edit4: TEdit
      Left = 159
      Top = 42
      Width = 98
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 10
      Text = '10'
      OnChange = Edit1Change
    end
    object Edit5: TEdit
      Left = 159
      Top = 84
      Width = 98
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 11
      Text = '10'
      OnChange = Edit1Change
    end
    object Edit6: TEdit
      Left = 159
      Top = 126
      Width = 98
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 12
      Text = '10'
      OnChange = Edit1Change
    end
    object UpDown4: TUpDown
      Left = 257
      Top = 42
      Width = 39
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = Edit4
      Position = 10
      TabOrder = 13
    end
    object UpDown5: TUpDown
      Left = 257
      Top = 84
      Width = 39
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = Edit5
      Position = 10
      TabOrder = 14
    end
    object UpDown6: TUpDown
      Left = 257
      Top = 126
      Width = 39
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = Edit6
      Position = 10
      TabOrder = 15
    end
    object Edit7: TEdit
      Left = 18
      Top = 266
      Width = 98
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 16
      Text = '7'
      OnChange = Edit1Change
    end
    object Edit8: TEdit
      Left = 18
      Top = 308
      Width = 98
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 17
      Text = '-1'
      OnChange = Edit1Change
    end
    object Edit9: TEdit
      Left = 18
      Top = 350
      Width = 98
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 18
      Text = '6'
      OnChange = Edit1Change
    end
    object UpDown7: TUpDown
      Left = 116
      Top = 266
      Width = 38
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = Edit7
      Min = -100
      Position = 7
      TabOrder = 19
    end
    object UpDown8: TUpDown
      Left = 116
      Top = 308
      Width = 38
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = Edit8
      Min = -100
      Position = -1
      TabOrder = 20
    end
    object UpDown9: TUpDown
      Left = 116
      Top = 350
      Width = 38
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = Edit9
      Min = -100
      Position = 6
      TabOrder = 21
    end
    object Edit10: TEdit
      Left = 163
      Top = 266
      Width = 98
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 22
      Text = '5'
      OnChange = Edit1Change
    end
    object UpDown10: TUpDown
      Left = 261
      Top = 266
      Width = 38
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Associate = Edit10
      Position = 5
      TabOrder = 23
    end
    object Button3: TButton
      Left = 70
      Top = 602
      Width = 131
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Recalc'
      TabOrder = 24
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 19
      Top = 172
      Width = 277
      Height = 43
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
