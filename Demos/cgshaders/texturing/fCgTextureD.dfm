object FormCgTexture: TFormCgTexture
  Left = 205
  Top = 112
  Caption = 'Cg Multi Texturing'
  ClientHeight = 401
  ClientWidth = 706
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 391
    Top = 0
    Width = 4
    Height = 401
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Color = clBtnShadow
    ParentColor = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 391
    Height = 401
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 2
      Top = 2
      Width = 387
      Height = 397
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ActivePage = TabSheet3
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object TabSheet1: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Vertex Program'
        object Splitter3: TSplitter
          Left = 0
          Top = 245
          Width = 376
          Height = 4
          Cursor = crVSplit
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alBottom
          Color = clBtnShadow
          ParentColor = False
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 376
          Height = 31
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object LabelVertProfile: TLabel
            Left = 110
            Top = 6
            Width = 78
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Using profile:'
          end
          object CBVertexProgram: TCheckBox
            Left = 8
            Top = 5
            Width = 73
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Enabled'
            Checked = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
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
          Top = 31
          Width = 376
          Height = 214
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          Caption = 'Panel6'
          TabOrder = 1
          object Panel12: TPanel
            Left = 1
            Top = 1
            Width = 374
            Height = 20
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alTop
            BevelOuter = bvNone
            Caption = 'Shader Code'
            Color = clBtnHighlight
            TabOrder = 0
          end
          object MemoVertCode: TMemo
            Left = 1
            Top = 21
            Width = 374
            Height = 160
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
            Font.Name = 'Lucida Console'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
            OnChange = MemoVertCodeChange
          end
          object Panel13: TPanel
            Left = 1
            Top = 181
            Width = 374
            Height = 32
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              377
              32)
            object ButtonApplyVP: TButton
              Left = 283
              Top = 3
              Width = 84
              Height = 25
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Caption = 'Apply'
              Enabled = False
              TabOrder = 0
              OnClick = ButtonApplyVPClick
              ExplicitLeft = 280
            end
          end
        end
        object Panel5: TPanel
          Left = 0
          Top = 253
          Width = 379
          Height = 112
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alBottom
          TabOrder = 2
          ExplicitTop = 249
          ExplicitWidth = 376
          DesignSize = (
            379
            112)
          object Label2: TLabel
            Left = 288
            Top = 6
            Width = 39
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akTop, akRight]
            Caption = 'Show:'
            ExplicitLeft = 285
          end
          object Memo1: TMemo
            Left = 1
            Top = 1
            Width = 273
            Height = 110
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alLeft
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Lucida Console'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
            ExplicitWidth = 270
          end
          object Button1: TButton
            Left = 286
            Top = 26
            Width = 80
            Height = 23
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akTop, akRight]
            Caption = 'Params'
            TabOrder = 1
            OnClick = Button1Click
            ExplicitLeft = 283
          end
          object Button4: TButton
            Left = 286
            Top = 53
            Width = 80
            Height = 22
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akTop, akRight]
            Caption = 'Asm'
            TabOrder = 2
            OnClick = Button4Click
            ExplicitLeft = 283
          end
        end
      end
      object TabSheet2: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Fragment Program'
        ImageIndex = 1
        object Splitter2: TSplitter
          Left = 0
          Top = 245
          Width = 376
          Height = 4
          Cursor = crVSplit
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alBottom
          Color = clBtnShadow
          ParentColor = False
        end
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 376
          Height = 31
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object LabelFragProfile: TLabel
            Left = 110
            Top = 6
            Width = 78
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Using profile:'
          end
          object CBFragmentProgram: TCheckBox
            Left = 8
            Top = 5
            Width = 83
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Enabled'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = CBFragmentProgramClick
          end
        end
        object Panel6: TPanel
          Left = 0
          Top = 31
          Width = 376
          Height = 214
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          Caption = 'Panel6'
          TabOrder = 1
          object Panel7: TPanel
            Left = 1
            Top = 1
            Width = 374
            Height = 20
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alTop
            BevelOuter = bvNone
            Caption = 'Shader Code'
            Color = clBtnHighlight
            TabOrder = 0
          end
          object MemoFragCode: TMemo
            Left = 1
            Top = 21
            Width = 374
            Height = 160
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
            Font.Name = 'Lucida Console'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
            OnChange = MemoFragCodeChange
          end
          object Panel3: TPanel
            Left = 1
            Top = 181
            Width = 374
            Height = 32
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              377
              32)
            object ButtonApplyFP: TButton
              Left = 283
              Top = 3
              Width = 84
              Height = 25
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Anchors = [akTop, akRight]
              Caption = 'Apply'
              Enabled = False
              TabOrder = 0
              OnClick = ButtonApplyFPClick
              ExplicitLeft = 280
            end
          end
        end
        object Panel8: TPanel
          Left = 0
          Top = 253
          Width = 379
          Height = 112
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alBottom
          TabOrder = 2
          ExplicitTop = 249
          ExplicitWidth = 376
          DesignSize = (
            379
            112)
          object Label1: TLabel
            Left = 288
            Top = 6
            Width = 39
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akTop, akRight]
            Caption = 'Show:'
            ExplicitLeft = 285
          end
          object Memo3: TMemo
            Left = 1
            Top = 1
            Width = 273
            Height = 110
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alLeft
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
            ExplicitWidth = 270
          end
          object Button2: TButton
            Left = 286
            Top = 26
            Width = 80
            Height = 23
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akTop, akRight]
            Caption = 'Params'
            TabOrder = 1
            OnClick = Button2Click
            ExplicitLeft = 283
          end
          object Button3: TButton
            Left = 286
            Top = 53
            Width = 80
            Height = 22
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akTop, akRight]
            Caption = 'Asm'
            TabOrder = 2
            OnClick = Button3Click
            ExplicitLeft = 283
          end
        end
      end
      object TabSheet3: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Controls'
        ImageIndex = 2
        DesignSize = (
          379
          365)
        object Label16: TLabel
          Left = 10
          Top = 340
          Width = 359
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Note: NV2x class hardware may not honor negative values'
        end
        object GroupBox1: TGroupBox
          Left = 10
          Top = 50
          Width = 354
          Height = 131
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Caption = ' TexCoord. Shifts '
          TabOrder = 0
          DesignSize = (
            354
            131)
          object Label18: TLabel
            Left = 80
            Top = 108
            Width = 244
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Alignment = taCenter
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '0'
            ExplicitWidth = 241
          end
          object Label3: TLabel
            Left = 20
            Top = 30
            Width = 35
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Tex 0'
          end
          object Label4: TLabel
            Left = 20
            Top = 50
            Width = 35
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Tex 1'
          end
          object Label5: TLabel
            Left = 20
            Top = 70
            Width = 35
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Tex 2'
          end
          object Label6: TLabel
            Left = 20
            Top = 90
            Width = 35
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Tex 3'
          end
          object Label11: TLabel
            Left = 40
            Top = 30
            Width = 4
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
          end
          object Label12: TLabel
            Left = 70
            Top = 108
            Width = 13
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = '-1'
          end
          object Label14: TLabel
            Left = 318
            Top = 108
            Width = 8
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akTop, akRight]
            Caption = '1'
            ExplicitLeft = 315
          end
          object TrackBar1: TTrackBar
            Left = 70
            Top = 30
            Width = 261
            Height = 19
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 0
            ThumbLength = 15
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar2: TTrackBar
            Left = 70
            Top = 50
            Width = 261
            Height = 19
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 1
            ThumbLength = 15
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar3: TTrackBar
            Left = 70
            Top = 70
            Width = 261
            Height = 19
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 2
            ThumbLength = 15
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar4: TTrackBar
            Left = 70
            Top = 90
            Width = 261
            Height = 19
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 3
            ThumbLength = 15
            TickMarks = tmBoth
            TickStyle = tsNone
          end
        end
        object GroupBox2: TGroupBox
          Left = 10
          Top = 200
          Width = 354
          Height = 131
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Blending Weights '
          TabOrder = 1
          DesignSize = (
            354
            131)
          object Label17: TLabel
            Left = 80
            Top = 108
            Width = 244
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Alignment = taCenter
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '0'
            ExplicitWidth = 241
          end
          object Label7: TLabel
            Left = 20
            Top = 30
            Width = 35
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Tex 0'
          end
          object Label8: TLabel
            Left = 20
            Top = 50
            Width = 35
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Tex 1'
          end
          object Label9: TLabel
            Left = 20
            Top = 70
            Width = 35
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Tex 2'
          end
          object Label10: TLabel
            Left = 20
            Top = 90
            Width = 35
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Tex 3'
          end
          object Label13: TLabel
            Left = 318
            Top = 108
            Width = 8
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akTop, akRight]
            Caption = '1'
            ExplicitLeft = 315
          end
          object Label15: TLabel
            Left = 70
            Top = 108
            Width = 13
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = '-1'
          end
          object TrackBar5: TTrackBar
            Left = 70
            Top = 30
            Width = 261
            Height = 19
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 75
            TabOrder = 0
            ThumbLength = 15
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar6: TTrackBar
            Left = 70
            Top = 50
            Width = 261
            Height = 19
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 75
            TabOrder = 1
            ThumbLength = 15
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar7: TTrackBar
            Left = 70
            Top = 70
            Width = 261
            Height = 19
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 75
            TabOrder = 2
            ThumbLength = 15
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar8: TTrackBar
            Left = 70
            Top = 90
            Width = 261
            Height = 19
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 65
            TabOrder = 3
            ThumbLength = 15
            TickMarks = tmBoth
            TickStyle = tsNone
          end
        end
        object CheckBox2: TCheckBox
          Left = 18
          Top = 15
          Width = 163
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'CgShader Enabled'
          Checked = True
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -14
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
    Left = 395
    Top = 0
    Width = 311
    Height = 401
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object PanelFPS: TPanel
      Left = 1
      Top = 1
      Width = 309
      Height = 60
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Caption = 'FPS'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object GLSceneViewer1: TGLSceneViewer
      Left = 1
      Top = 61
      Width = 309
      Height = 339
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Camera = GLCamera1
      Buffer.Lighting = False
      Buffer.AntiAliasing = aa4xHQ
      FieldOfView = 144.134292602539100000
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
