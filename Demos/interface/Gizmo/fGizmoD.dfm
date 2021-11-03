object FormGizmo: TFormGizmo
  Left = 211
  Top = 170
  Caption = 'Gizmo'
  ClientHeight = 770
  ClientWidth = 849
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 17
  object Viewer: TGLSceneViewer
    Left = 251
    Top = 0
    Width = 598
    Height = 770
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = Camera
    Buffer.BackgroundColor = clMedGray
    FieldOfView = 161.013214111328100000
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
    Width = 251
    Height = 770
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    TabOrder = 1
    object Bevel1: TBevel
      Left = 1
      Top = 1
      Width = 249
      Height = 10
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Shape = bsTopLine
    end
    object Label1: TLabel
      Left = 9
      Top = 590
      Width = 131
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Bounding Box Color :'
    end
    object Label2: TLabel
      Left = 9
      Top = 640
      Width = 101
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Label Infos Color'
    end
    object Label3: TLabel
      Left = 9
      Top = 690
      Width = 87
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Selected Color'
    end
    object Label4: TLabel
      Left = 29
      Top = 543
      Width = 109
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Auto Zoom factor'
    end
    object Label5: TLabel
      Left = 59
      Top = 569
      Width = 79
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Zoom Factor'
    end
    object Label6: TLabel
      Left = 29
      Top = 305
      Width = 74
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Move Coef :'
    end
    object Label10: TLabel
      Left = 21
      Top = 364
      Width = 72
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Scale Coef :'
    end
    object Label11: TLabel
      Left = 6
      Top = 276
      Width = 100
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Gizmo Thickness'
    end
    object Label7: TLabel
      Left = 20
      Top = 336
      Width = 82
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Rotate Coef :'
    end
    object CheckBox1: TCheckBox
      Tag = 1
      Left = 10
      Top = 100
      Width = 181
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Exclude Objects in list'
      TabOrder = 0
      OnClick = CheckBox12Click
    end
    object CheckBox2: TCheckBox
      Tag = 2
      Left = 10
      Top = 190
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Force Axis'
      TabOrder = 1
      OnClick = CheckBox12Click
    end
    object CBXAxis: TComboBox
      Left = 135
      Top = 185
      Width = 111
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
      Left = 10
      Top = 214
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Force Operation'
      TabOrder = 3
      OnClick = CheckBox12Click
    end
    object CBXOperation: TComboBox
      Left = 135
      Top = 213
      Width = 111
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
      Left = 10
      Top = 240
      Width = 171
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Force Uniform Scale'
      TabOrder = 5
      OnClick = CheckBox12Click
    end
    object CheckBox5: TCheckBox
      Tag = 5
      Left = 10
      Top = 393
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show Axis Label'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = CheckBox12Click
    end
    object CheckBox6: TCheckBox
      Tag = 6
      Left = 10
      Top = 413
      Width = 141
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show Infos Label'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = CheckBox12Click
    end
    object CheckBox7: TCheckBox
      Tag = 9
      Left = 40
      Top = 435
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Object Name'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = CheckBox12Click
    end
    object CheckBox8: TCheckBox
      Tag = 10
      Left = 40
      Top = 455
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Operation'
      Checked = True
      State = cbChecked
      TabOrder = 9
      OnClick = CheckBox12Click
    end
    object CheckBox9: TCheckBox
      Tag = 11
      Left = 40
      Top = 475
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Coordonates'
      Checked = True
      State = cbChecked
      TabOrder = 10
      OnClick = CheckBox12Click
    end
    object CheckBox10: TCheckBox
      Tag = 8
      Left = 9
      Top = 523
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Auto Zoom'
      Checked = True
      State = cbChecked
      TabOrder = 11
      OnClick = CheckBox12Click
    end
    object CheckBox11: TCheckBox
      Tag = 7
      Left = 9
      Top = 501
      Width = 121
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'No Z-Write'
      Checked = True
      State = cbChecked
      TabOrder = 12
      OnClick = CheckBox12Click
    end
    object ColorBox1: TColorBox
      Left = 5
      Top = 610
      Width = 235
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DefaultColorColor = clWhite
      Selected = clWhite
      ItemHeight = 20
      TabOrder = 13
      OnChange = ColorBox1Change
    end
    object ColorBox2: TColorBox
      Tag = 1
      Left = 5
      Top = 658
      Width = 235
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Selected = clYellow
      ItemHeight = 20
      TabOrder = 14
      OnChange = ColorBox1Change
    end
    object ColorBox3: TColorBox
      Tag = 2
      Left = 5
      Top = 708
      Width = 235
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Selected = clYellow
      ItemHeight = 20
      TabOrder = 15
      OnChange = ColorBox1Change
    end
    object edAutoZoomFactor: TEdit
      Left = 143
      Top = 540
      Width = 91
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 16
      Text = '5'
      OnChange = edAutoZoomFactorChange
      OnKeyPress = edAutoZoomFactorKeyPress
    end
    object edZoomFactor: TEdit
      Left = 143
      Top = 565
      Width = 91
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Enabled = False
      TabOrder = 17
      Text = '0.35'
      OnChange = edZoomFactorChange
      OnKeyPress = edAutoZoomFactorKeyPress
    end
    object CheckBox12: TCheckBox
      Left = 10
      Top = 79
      Width = 81
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 18
      OnClick = CheckBox12Click
    end
    object edMoveCoef: TEdit
      Left = 110
      Top = 300
      Width = 91
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 19
      Text = '0,1'
      OnChange = edMoveCoefChange
      OnKeyPress = edAutoZoomFactorKeyPress
    end
    object edRotateCoef: TEdit
      Left = 110
      Top = 330
      Width = 91
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 20
      Text = '1'
      OnChange = edRotateCoefChange
      OnKeyPress = edAutoZoomFactorKeyPress
    end
    object CheckBox13: TCheckBox
      Tag = 12
      Left = 10
      Top = 121
      Width = 121
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show Move Part'
      Checked = True
      State = cbChecked
      TabOrder = 21
      OnClick = CheckBox12Click
    end
    object CheckBox14: TCheckBox
      Tag = 13
      Left = 10
      Top = 141
      Width = 141
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show Rotate Part'
      Checked = True
      State = cbChecked
      TabOrder = 22
      OnClick = CheckBox12Click
    end
    object CheckBox15: TCheckBox
      Tag = 14
      Left = 10
      Top = 164
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show Scale Part'
      Checked = True
      State = cbChecked
      TabOrder = 23
      OnClick = CheckBox12Click
    end
    object edGizmoThickness: TEdit
      Left = 110
      Top = 270
      Width = 91
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 24
      Text = '1'
      OnChange = edGizmoThicknessChange
    end
    object OptPickMode: TRadioGroup
      Left = 10
      Top = 19
      Width = 225
      Height = 41
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
    Left = 110
    Top = 359
    Width = 91
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
