object FormCgTexture: TFormCgTexture
  Left = 205
  Top = 112
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Cg Multi Texturing'
  ClientHeight = 690
  ClientWidth = 1138
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 168
  TextHeight = 24
  object Splitter1: TSplitter
    Left = 548
    Top = 0
    Width = 5
    Height = 690
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = clBtnShadow
    MinSize = 53
    ParentColor = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 548
    Height = 690
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 2
      Top = 2
      Width = 544
      Height = 686
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ActivePage = TabSheet3
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object TabSheet1: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Vertex Program'
        object Splitter3: TSplitter
          Left = 0
          Top = 471
          Width = 527
          Height = 5
          Cursor = crVSplit
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alBottom
          Color = clBtnShadow
          MinSize = 53
          ParentColor = False
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 527
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object LabelVertProfile: TLabel
            Left = 154
            Top = 9
            Width = 111
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Using profile:'
          end
          object CBVertexProgram: TCheckBox
            Left = 11
            Top = 7
            Width = 103
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Enabled'
            Checked = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -19
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            State = cbChecked
            TabOrder = 0
            OnClick = CBVertexProgramClick
          end
        end
        object Panel11: TPanel
          Left = 0
          Top = 44
          Width = 527
          Height = 427
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          Caption = 'Panel6'
          TabOrder = 1
          object Panel12: TPanel
            Left = 2
            Top = 2
            Width = 523
            Height = 28
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            BevelOuter = bvNone
            Caption = 'Shader Code'
            Color = clBtnHighlight
            TabOrder = 0
          end
          object MemoVertCode: TMemo
            Left = 2
            Top = 30
            Width = 523
            Height = 352
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -19
            Font.Name = 'Lucida Console'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
            OnChange = MemoVertCodeChange
          end
          object Panel13: TPanel
            Left = 2
            Top = 382
            Width = 523
            Height = 43
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              534
              43)
            object ButtonApplyVP: TButton
              Left = 407
              Top = 4
              Width = 119
              Height = 35
              Margins.Left = 5
              Margins.Top = 5
              Margins.Right = 5
              Margins.Bottom = 5
              Anchors = [akTop, akRight]
              Caption = 'Apply'
              Enabled = False
              TabOrder = 0
              OnClick = ButtonApplyVPClick
              ExplicitLeft = 396
            end
          end
        end
        object Panel5: TPanel
          Left = 0
          Top = 490
          Width = 536
          Height = 158
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alBottom
          TabOrder = 2
          ExplicitTop = 476
          ExplicitWidth = 527
          DesignSize = (
            536
            158)
          object Label2: TLabel
            Left = 408
            Top = 9
            Width = 53
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akTop, akRight]
            Caption = 'Show:'
            ExplicitLeft = 399
          end
          object Memo1: TMemo
            Left = 1
            Top = 1
            Width = 387
            Height = 156
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alLeft
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Lucida Console'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
            ExplicitLeft = 2
            ExplicitTop = 2
            ExplicitWidth = 378
            ExplicitHeight = 154
          end
          object Button1: TButton
            Left = 406
            Top = 37
            Width = 112
            Height = 31
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akTop, akRight]
            Caption = 'Params'
            TabOrder = 1
            OnClick = Button1Click
            ExplicitLeft = 397
          end
          object Button4: TButton
            Left = 406
            Top = 74
            Width = 112
            Height = 31
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akTop, akRight]
            Caption = 'Asm'
            TabOrder = 2
            OnClick = Button4Click
            ExplicitLeft = 397
          end
        end
      end
      object TabSheet2: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Fragment Program'
        ImageIndex = 1
        object Splitter2: TSplitter
          Left = 0
          Top = 471
          Width = 527
          Height = 5
          Cursor = crVSplit
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alBottom
          Color = clBtnShadow
          MinSize = 53
          ParentColor = False
        end
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 527
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object LabelFragProfile: TLabel
            Left = 154
            Top = 9
            Width = 111
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Using profile:'
          end
          object CBFragmentProgram: TCheckBox
            Left = 11
            Top = 7
            Width = 117
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Enabled'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = CBFragmentProgramClick
          end
        end
        object Panel6: TPanel
          Left = 0
          Top = 44
          Width = 527
          Height = 427
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          Caption = 'Panel6'
          TabOrder = 1
          object Panel7: TPanel
            Left = 2
            Top = 2
            Width = 523
            Height = 28
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            BevelOuter = bvNone
            Caption = 'Shader Code'
            Color = clBtnHighlight
            TabOrder = 0
          end
          object MemoFragCode: TMemo
            Left = 2
            Top = 30
            Width = 523
            Height = 352
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -19
            Font.Name = 'Lucida Console'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
            OnChange = MemoFragCodeChange
          end
          object Panel3: TPanel
            Left = 2
            Top = 382
            Width = 523
            Height = 43
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              534
              43)
            object ButtonApplyFP: TButton
              Left = 407
              Top = 4
              Width = 119
              Height = 35
              Margins.Left = 5
              Margins.Top = 5
              Margins.Right = 5
              Margins.Bottom = 5
              Anchors = [akTop, akRight]
              Caption = 'Apply'
              Enabled = False
              TabOrder = 0
              OnClick = ButtonApplyFPClick
              ExplicitLeft = 396
            end
          end
        end
        object Panel8: TPanel
          Left = 0
          Top = 490
          Width = 536
          Height = 158
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alBottom
          TabOrder = 2
          ExplicitTop = 476
          ExplicitWidth = 527
          DesignSize = (
            536
            158)
          object Label1: TLabel
            Left = 408
            Top = 9
            Width = 53
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akTop, akRight]
            Caption = 'Show:'
            ExplicitLeft = 399
          end
          object Memo3: TMemo
            Left = 1
            Top = 1
            Width = 387
            Height = 156
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alLeft
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -19
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
            ExplicitLeft = 2
            ExplicitTop = 2
            ExplicitWidth = 378
            ExplicitHeight = 154
          end
          object Button2: TButton
            Left = 406
            Top = 37
            Width = 112
            Height = 31
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akTop, akRight]
            Caption = 'Params'
            TabOrder = 1
            OnClick = Button2Click
            ExplicitLeft = 397
          end
          object Button3: TButton
            Left = 406
            Top = 74
            Width = 112
            Height = 31
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akTop, akRight]
            Caption = 'Asm'
            TabOrder = 2
            OnClick = Button3Click
            ExplicitLeft = 397
          end
        end
      end
      object TabSheet3: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Controls'
        ImageIndex = 2
        DesignSize = (
          536
          648)
        object Label16: TLabel
          Left = 14
          Top = 476
          Width = 490
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Note: NV2x class hardware may not honor negative values'
        end
        object GroupBox1: TGroupBox
          Left = 14
          Top = 70
          Width = 501
          Height = 184
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Anchors = [akLeft, akTop, akRight]
          Caption = ' TexCoord. Shifts '
          TabOrder = 0
          DesignSize = (
            501
            184)
          object Label18: TLabel
            Left = 112
            Top = 151
            Width = 347
            Height = 22
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Alignment = taCenter
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '0'
            ExplicitWidth = 338
          end
          object Label3: TLabel
            Left = 28
            Top = 42
            Width = 46
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Tex 0'
          end
          object Label4: TLabel
            Left = 28
            Top = 70
            Width = 46
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Tex 1'
          end
          object Label5: TLabel
            Left = 28
            Top = 98
            Width = 46
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Tex 2'
          end
          object Label6: TLabel
            Left = 28
            Top = 126
            Width = 46
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Tex 3'
          end
          object Label11: TLabel
            Left = 56
            Top = 42
            Width = 6
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
          end
          object Label12: TLabel
            Left = 98
            Top = 151
            Width = 17
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = '-1'
          end
          object Label14: TLabel
            Left = 450
            Top = 151
            Width = 10
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akTop, akRight]
            Caption = '1'
            ExplicitLeft = 441
          end
          object TrackBar1: TTrackBar
            Left = 98
            Top = 42
            Width = 371
            Height = 26
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 0
            ThumbLength = 21
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar2: TTrackBar
            Left = 98
            Top = 70
            Width = 371
            Height = 26
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 1
            ThumbLength = 21
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar3: TTrackBar
            Left = 98
            Top = 98
            Width = 371
            Height = 26
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 2
            ThumbLength = 21
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar4: TTrackBar
            Left = 98
            Top = 126
            Width = 371
            Height = 26
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 3
            ThumbLength = 21
            TickMarks = tmBoth
            TickStyle = tsNone
          end
        end
        object GroupBox2: TGroupBox
          Left = 14
          Top = 280
          Width = 501
          Height = 184
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Blending Weights '
          TabOrder = 1
          DesignSize = (
            501
            184)
          object Label17: TLabel
            Left = 112
            Top = 151
            Width = 347
            Height = 22
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Alignment = taCenter
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '0'
            ExplicitWidth = 338
          end
          object Label7: TLabel
            Left = 28
            Top = 42
            Width = 46
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Tex 0'
          end
          object Label8: TLabel
            Left = 28
            Top = 70
            Width = 46
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Tex 1'
          end
          object Label9: TLabel
            Left = 28
            Top = 98
            Width = 46
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Tex 2'
          end
          object Label10: TLabel
            Left = 28
            Top = 126
            Width = 46
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Tex 3'
          end
          object Label13: TLabel
            Left = 450
            Top = 151
            Width = 10
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akTop, akRight]
            Caption = '1'
            ExplicitLeft = 441
          end
          object Label15: TLabel
            Left = 98
            Top = 151
            Width = 17
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = '-1'
          end
          object TrackBar5: TTrackBar
            Left = 98
            Top = 42
            Width = 371
            Height = 26
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 75
            TabOrder = 0
            ThumbLength = 21
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar6: TTrackBar
            Left = 98
            Top = 70
            Width = 371
            Height = 26
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 75
            TabOrder = 1
            ThumbLength = 21
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar7: TTrackBar
            Left = 98
            Top = 98
            Width = 371
            Height = 26
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 75
            TabOrder = 2
            ThumbLength = 21
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar8: TTrackBar
            Left = 98
            Top = 126
            Width = 371
            Height = 26
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 65
            TabOrder = 3
            ThumbLength = 21
            TickMarks = tmBoth
            TickStyle = tsNone
          end
        end
        object CheckBox2: TCheckBox
          Left = 25
          Top = 21
          Width = 229
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'CgShader Enabled'
          Checked = True
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          State = cbChecked
          TabOrder = 2
          OnClick = CheckBox2Click
        end
      end
    end
  end
  object Panel9: TPanel
    Left = 553
    Top = 0
    Width = 585
    Height = 690
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -33
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object PanelFPS: TPanel
      Left = 1
      Top = 1
      Width = 583
      Height = 84
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Caption = 'FPS'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -33
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object GLSceneViewer1: TGLSceneViewer
      Left = 1
      Top = 85
      Width = 583
      Height = 604
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Camera = GLCamera1
      Buffer.Lighting = False
      Buffer.AntiAliasing = aa4xHQ
      FieldOfView = 160.533935546875000000
      PenAsTouch = False
      Align = alClient
      OnMouseDown = GLSceneViewer1MouseDown
      OnMouseMove = GLSceneViewer1MouseMove
      TabOrder = 1
    end
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 80
    object GLXYZGrid1: TGLXYZGrid
      Position.Coordinates = {000000006F12033B000000000000803F}
      LineColor.Color = {938C0C3E938E0E3F938C0C3E0000803F}
      XSamplingScale.Min = -2.000000000000000000
      XSamplingScale.Max = 2.000000000000000000
      XSamplingScale.Step = 0.100000001490116100
      YSamplingScale.Step = 0.100000001490116100
      ZSamplingScale.Min = -2.000000000000000000
      ZSamplingScale.Max = 2.000000000000000000
      ZSamplingScale.Step = 0.100000001490116100
      Parts = [gpX, gpZ]
    end
    object GLPlane1: TGLPlane
      Material.MaterialLibrary = GLMatLib
      Material.LibMaterialName = 'LibMaterial'
      Direction.Coordinates = {000000000000803F0000000000000000}
      PitchAngle = 90.000000000000000000
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 1.000000000000000000
      Width = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLPlane1
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {000000000000C03F0000803F0000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 32
    Top = 144
  end
  object CgShader1: TCgShader
    VertexProgram.OnApply = CgShader1ApplyVP
    FragmentProgram.OnApply = CgShader1ApplyFP
    FragmentProgram.OnUnApply = CgShader1UnApplyFP
    OnApplyVP = CgShader1ApplyVP
    OnApplyFP = CgShader1ApplyFP
    OnUnApplyFP = CgShader1UnApplyFP
    OnInitialize = CgShader1Initialize
    Left = 168
    Top = 80
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 164
    Top = 144
  end
  object GLMatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.Texture.Disabled = False
        Shader = CgShader1
      end
      item
        Name = 'LibMaterial1'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'LibMaterial2'
        Tag = 0
      end
      item
        Name = 'LibMaterial3'
        Tag = 0
      end>
    TexturePaths = '..\..\..\Media\'
    Left = 105
    Top = 80
  end
end
