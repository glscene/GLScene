object FormGizmo: TFormGizmo
  Left = 211
  Top = 170
  Caption = 'Gizmo'
  ClientHeight = 616
  ClientWidth = 679
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 201
    Top = 0
    Width = 478
    Height = 616
    Camera = Camera
    Buffer.BackgroundColor = clMedGray
    FieldOfView = 156.367706298828100000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
    OnMouseUp = ViewerMouseUp
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 616
    Align = alLeft
    TabOrder = 1
    object Bevel1: TBevel
      Left = 1
      Top = 1
      Width = 199
      Height = 8
      Align = alTop
      Shape = bsTopLine
    end
    object Label1: TLabel
      Left = 7
      Top = 472
      Width = 100
      Height = 13
      Caption = 'Bounding Box Color :'
    end
    object Label2: TLabel
      Left = 7
      Top = 512
      Width = 81
      Height = 13
      Caption = 'Label Infos Color'
    end
    object Label3: TLabel
      Left = 7
      Top = 552
      Width = 69
      Height = 13
      Caption = 'Selected Color'
    end
    object Label4: TLabel
      Left = 23
      Top = 434
      Width = 84
      Height = 13
      Caption = 'Auto Zoom factor'
    end
    object Label5: TLabel
      Left = 47
      Top = 455
      Width = 60
      Height = 13
      Caption = 'Zoom Factor'
    end
    object Label6: TLabel
      Left = 23
      Top = 244
      Width = 59
      Height = 13
      Caption = 'Move Coef :'
    end
    object Label10: TLabel
      Left = 17
      Top = 291
      Width = 58
      Height = 13
      Caption = 'Scale Coef :'
    end
    object Label11: TLabel
      Left = 5
      Top = 221
      Width = 77
      Height = 13
      Caption = 'Gizmo Thickness'
    end
    object Label7: TLabel
      Left = 16
      Top = 269
      Width = 66
      Height = 13
      Caption = 'Rotate Coef :'
    end
    object CheckBox1: TCheckBox
      Tag = 1
      Left = 8
      Top = 80
      Width = 145
      Height = 17
      Caption = 'Exclude Objects in list'
      TabOrder = 0
      OnClick = CheckBox12Click
    end
    object CheckBox2: TCheckBox
      Tag = 2
      Left = 8
      Top = 152
      Width = 97
      Height = 17
      Caption = 'Force Axis'
      TabOrder = 1
      OnClick = CheckBox12Click
    end
    object CBXAxis: TComboBox
      Left = 108
      Top = 148
      Width = 89
      Height = 21
      Enabled = False
      ItemIndex = 0
      TabOrder = 2
      Text = 'None'
      OnChange = CBXAxisChange
      Items.Strings = (
        'None'
        'X'
        'XY'
        'XZ'
        'Y'
        'YZ'
        'Z')
    end
    object CheckBox3: TCheckBox
      Tag = 3
      Left = 8
      Top = 171
      Width = 97
      Height = 17
      Caption = 'Force Operation'
      TabOrder = 3
      OnClick = CheckBox12Click
    end
    object CBXOperation: TComboBox
      Left = 108
      Top = 170
      Width = 89
      Height = 21
      Enabled = False
      ItemIndex = 0
      TabOrder = 4
      Text = 'None'
      OnChange = CBXOperationChange
      Items.Strings = (
        'None'
        'Move'
        'Rotate'
        'Scale')
    end
    object CheckBox4: TCheckBox
      Tag = 4
      Left = 8
      Top = 192
      Width = 137
      Height = 17
      Caption = 'Force Uniform Scale'
      TabOrder = 5
      OnClick = CheckBox12Click
    end
    object CheckBox5: TCheckBox
      Tag = 5
      Left = 8
      Top = 314
      Width = 97
      Height = 17
      Caption = 'Show Axis Label'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = CheckBox12Click
    end
    object CheckBox6: TCheckBox
      Tag = 6
      Left = 8
      Top = 330
      Width = 113
      Height = 17
      Caption = 'Show Infos Label'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = CheckBox12Click
    end
    object CheckBox7: TCheckBox
      Tag = 9
      Left = 32
      Top = 348
      Width = 97
      Height = 17
      Caption = 'Object Name'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = CheckBox12Click
    end
    object CheckBox8: TCheckBox
      Tag = 10
      Left = 32
      Top = 364
      Width = 97
      Height = 17
      Caption = 'Operation'
      Checked = True
      State = cbChecked
      TabOrder = 9
      OnClick = CheckBox12Click
    end
    object CheckBox9: TCheckBox
      Tag = 11
      Left = 32
      Top = 380
      Width = 97
      Height = 17
      Caption = 'Coordonates'
      Checked = True
      State = cbChecked
      TabOrder = 10
      OnClick = CheckBox12Click
    end
    object CheckBox10: TCheckBox
      Tag = 8
      Left = 7
      Top = 418
      Width = 97
      Height = 17
      Caption = 'Auto Zoom'
      Checked = True
      State = cbChecked
      TabOrder = 11
      OnClick = CheckBox12Click
    end
    object CheckBox11: TCheckBox
      Tag = 7
      Left = 7
      Top = 401
      Width = 97
      Height = 17
      Caption = 'No Z-Write'
      Checked = True
      State = cbChecked
      TabOrder = 12
      OnClick = CheckBox12Click
    end
    object ColorBox1: TColorBox
      Left = 4
      Top = 488
      Width = 188
      Height = 22
      DefaultColorColor = clWhite
      Selected = clWhite
      TabOrder = 13
      OnChange = ColorBox1Change
    end
    object ColorBox2: TColorBox
      Tag = 1
      Left = 4
      Top = 526
      Width = 188
      Height = 22
      Selected = clYellow
      TabOrder = 14
      OnChange = ColorBox1Change
    end
    object ColorBox3: TColorBox
      Tag = 2
      Left = 4
      Top = 566
      Width = 188
      Height = 22
      Selected = clYellow
      TabOrder = 15
      OnChange = ColorBox1Change
    end
    object edAutoZoomFactor: TEdit
      Left = 114
      Top = 432
      Width = 73
      Height = 21
      TabOrder = 16
      Text = '5'
      OnChange = edAutoZoomFactorChange
      OnKeyPress = edAutoZoomFactorKeyPress
    end
    object edZoomFactor: TEdit
      Left = 114
      Top = 452
      Width = 73
      Height = 21
      Enabled = False
      TabOrder = 17
      Text = '0.35'
      OnChange = edZoomFactorChange
      OnKeyPress = edAutoZoomFactorKeyPress
    end
    object CheckBox12: TCheckBox
      Left = 8
      Top = 63
      Width = 65
      Height = 17
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 18
      OnClick = CheckBox12Click
    end
    object edMoveCoef: TEdit
      Left = 88
      Top = 240
      Width = 73
      Height = 21
      TabOrder = 19
      Text = '0,1'
      OnChange = edMoveCoefChange
      OnKeyPress = edAutoZoomFactorKeyPress
    end
    object edRotateCoef: TEdit
      Left = 88
      Top = 264
      Width = 73
      Height = 21
      TabOrder = 20
      Text = '1'
      OnChange = edRotateCoefChange
      OnKeyPress = edAutoZoomFactorKeyPress
    end
    object CheckBox13: TCheckBox
      Tag = 12
      Left = 8
      Top = 97
      Width = 97
      Height = 17
      Caption = 'Show Move Part'
      Checked = True
      State = cbChecked
      TabOrder = 21
      OnClick = CheckBox12Click
    end
    object CheckBox14: TCheckBox
      Tag = 13
      Left = 8
      Top = 113
      Width = 113
      Height = 17
      Caption = 'Show Rotate Part'
      Checked = True
      State = cbChecked
      TabOrder = 22
      OnClick = CheckBox12Click
    end
    object CheckBox15: TCheckBox
      Tag = 14
      Left = 8
      Top = 131
      Width = 97
      Height = 17
      Caption = 'Show Scale Part'
      Checked = True
      State = cbChecked
      TabOrder = 23
      OnClick = CheckBox12Click
    end
    object edGizmoThickness: TEdit
      Left = 88
      Top = 216
      Width = 73
      Height = 21
      TabOrder = 24
      Text = '1'
      OnChange = edGizmoThicknessChange
    end
    object OptPickMode: TRadioGroup
      Left = 8
      Top = 15
      Width = 180
      Height = 33
      Caption = 'PickMode'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'PickObj'
        'Raycast')
      TabOrder = 25
      OnClick = OptPickModeClick
    end
  end
  object edScaleCoef: TEdit
    Left = 88
    Top = 287
    Width = 73
    Height = 21
    TabOrder = 2
    Text = '0,1'
    OnChange = edScaleCoefChange
    OnKeyPress = edAutoZoomFactorKeyPress
  end
  object GLScene1: TGLScene
    Left = 248
    Top = 56
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLDodecahedron3: TGLDodecahedron
        object GLArrowLine3: TGLArrowLine
          Material.FrontProperties.Diffuse.Color = {0000803F8180003F8180803E0000803F}
          Position.Coordinates = {00000000000000000000003F0000803F}
          BottomRadius = 0.100000001490116100
          Height = 1.000000000000000000
          TopRadius = 0.100000001490116100
          TopArrowHeadHeight = 0.500000000000000000
          TopArrowHeadRadius = 0.200000002980232200
          BottomArrowHeadHeight = 0.500000000000000000
          BottomArrowHeadRadius = 0.200000002980232200
          object GLArrowLine4: TGLArrowLine
            Material.FrontProperties.Diffuse.Color = {000000000000803F8180803E0000803F}
            Direction.Coordinates = {000000000000803F0000000000000000}
            Position.Coordinates = {000000000000003F0000003F0000803F}
            Up.Coordinates = {0000000000000000000080BF00000000}
            BottomRadius = 0.100000001490116100
            Height = 1.000000000000000000
            TopRadius = 0.100000001490116100
            TopArrowHeadHeight = 0.500000000000000000
            TopArrowHeadRadius = 0.200000002980232200
            BottomArrowHeadHeight = 0.500000000000000000
            BottomArrowHeadRadius = 0.200000002980232200
          end
        end
      end
      object GLCube1: TGLCube
        Position.Coordinates = {0000803F00000000000000000000803F}
        object GLSphere1: TGLSphere
          Position.Coordinates = {0000803F00000000000000000000803F}
          Radius = 0.500000000000000000
        end
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {9A99593F9A99593F9A99593F0000803F}
      Position.Coordinates = {0000803F00000040000040400000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {6666663F6666663F6666663F0000803F}
      Position.Coordinates = {000080BF00000040000000400000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
    object RootGizmo: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000404000004040000040400000803F}
      Direction.Coordinates = {3ACD13BF3ACD13BF3ACD13BF00000000}
      Up.Coordinates = {EC05D1BEEC05513FEC05D1BE00000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 248
    Top = 120
  end
  object WindowsBitmapFont: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 312
    Top = 120
  end
end
