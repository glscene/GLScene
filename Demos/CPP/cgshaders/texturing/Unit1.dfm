object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Cg Multi Texturing'
  ClientHeight = 414
  ClientWidth = 591
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 313
    Top = 0
    Height = 414
    Color = clBtnShadow
    ParentColor = False
    ExplicitTop = -4
    ExplicitHeight = 395
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 313
    Height = 414
    Align = alLeft
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 2
      Top = 2
      Width = 309
      Height = 410
      ActivePage = TabSheet3
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Vertex Program'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Splitter3: TSplitter
          Left = 0
          Top = 289
          Width = 301
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          Color = clBtnShadow
          ParentColor = False
          ExplicitTop = 276
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 301
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object LabelVertProfile: TLabel
            Left = 88
            Top = 5
            Width = 63
            Height = 13
            Caption = 'Using profile:'
          end
          object CBVertexProgram: TCheckBox
            Left = 6
            Top = 4
            Width = 59
            Height = 17
            Caption = 'Enabled'
            Checked = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
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
          Top = 25
          Width = 301
          Height = 264
          Align = alClient
          Caption = 'Panel6'
          TabOrder = 1
          object Panel12: TPanel
            Left = 1
            Top = 1
            Width = 299
            Height = 16
            Align = alTop
            BevelOuter = bvNone
            Caption = 'Shader Code'
            Color = clBtnHighlight
            TabOrder = 0
          end
          object MemoVertCode: TMemo
            Left = 1
            Top = 17
            Width = 299
            Height = 221
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
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
            Top = 238
            Width = 299
            Height = 25
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              299
              25)
            object ButtonApplyVP: TButton
              Left = 224
              Top = 2
              Width = 67
              Height = 20
              Anchors = [akTop, akRight]
              Caption = 'Apply'
              Enabled = False
              TabOrder = 0
              OnClick = ButtonApplyVPClick
            end
          end
        end
        object Panel5: TPanel
          Left = 0
          Top = 292
          Width = 301
          Height = 90
          Align = alBottom
          TabOrder = 2
          DesignSize = (
            301
            90)
          object Label2: TLabel
            Left = 228
            Top = 5
            Width = 30
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Show:'
          end
          object Memo1: TMemo
            Left = 1
            Top = 1
            Width = 216
            Height = 88
            Align = alLeft
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -9
            Font.Name = 'Lucida Console'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
          object Button1: TButton
            Left = 226
            Top = 21
            Width = 64
            Height = 18
            Anchors = [akTop, akRight]
            Caption = 'Params'
            TabOrder = 1
            OnClick = Button1Click
          end
          object Button4: TButton
            Left = 226
            Top = 42
            Width = 64
            Height = 18
            Anchors = [akTop, akRight]
            Caption = 'Asm'
            TabOrder = 2
            OnClick = Button4Click
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Fragment Program'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Splitter2: TSplitter
          Left = 0
          Top = 289
          Width = 301
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          Color = clBtnShadow
          ParentColor = False
          ExplicitTop = 287
        end
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 301
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object LabelFragProfile: TLabel
            Left = 88
            Top = 5
            Width = 63
            Height = 13
            Caption = 'Using profile:'
          end
          object CBFragmentProgram: TCheckBox
            Left = 6
            Top = 4
            Width = 67
            Height = 17
            Caption = 'Enabled'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = CBFragmentProgramClick
          end
        end
        object Panel6: TPanel
          Left = 0
          Top = 25
          Width = 301
          Height = 264
          Align = alClient
          Caption = 'Panel6'
          TabOrder = 1
          object Panel7: TPanel
            Left = 1
            Top = 1
            Width = 299
            Height = 16
            Align = alTop
            BevelOuter = bvNone
            Caption = 'Shader Code'
            Color = clBtnHighlight
            TabOrder = 0
          end
          object MemoFragCode: TMemo
            Left = 1
            Top = 17
            Width = 299
            Height = 221
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
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
            Top = 238
            Width = 299
            Height = 25
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              299
              25)
            object ButtonApplyFP: TButton
              Left = 224
              Top = 2
              Width = 67
              Height = 20
              Anchors = [akTop, akRight]
              Caption = 'Apply'
              Enabled = False
              TabOrder = 0
              OnClick = ButtonApplyFPClick
            end
          end
        end
        object Panel8: TPanel
          Left = 0
          Top = 292
          Width = 301
          Height = 90
          Align = alBottom
          TabOrder = 2
          DesignSize = (
            301
            90)
          object Label1: TLabel
            Left = 228
            Top = 5
            Width = 30
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Show:'
          end
          object Memo3: TMemo
            Left = 1
            Top = 1
            Width = 216
            Height = 88
            Align = alLeft
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
          object Button2: TButton
            Left = 226
            Top = 21
            Width = 64
            Height = 18
            Anchors = [akTop, akRight]
            Caption = 'Params'
            TabOrder = 1
            OnClick = Button2Click
          end
          object Button3: TButton
            Left = 226
            Top = 42
            Width = 64
            Height = 18
            Anchors = [akTop, akRight]
            Caption = 'Asm'
            TabOrder = 2
            OnClick = Button3Click
          end
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Controls'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          301
          382)
        object Label16: TLabel
          Left = 8
          Top = 272
          Width = 282
          Height = 13
          Caption = 'Note: NV2x class hardware may not honor negative values'
        end
        object GroupBox1: TGroupBox
          Left = 8
          Top = 40
          Width = 281
          Height = 105
          Anchors = [akLeft, akTop, akRight]
          Caption = ' TexCoord. Shifts '
          TabOrder = 0
          DesignSize = (
            281
            105)
          object Label18: TLabel
            Left = 64
            Top = 86
            Width = 193
            Height = 13
            Alignment = taCenter
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '0'
          end
          object Label3: TLabel
            Left = 16
            Top = 24
            Width = 27
            Height = 13
            Caption = 'Tex 0'
          end
          object Label4: TLabel
            Left = 16
            Top = 40
            Width = 27
            Height = 13
            Caption = 'Tex 1'
          end
          object Label5: TLabel
            Left = 16
            Top = 56
            Width = 27
            Height = 13
            Caption = 'Tex 2'
          end
          object Label6: TLabel
            Left = 16
            Top = 72
            Width = 27
            Height = 13
            Caption = 'Tex 3'
          end
          object Label11: TLabel
            Left = 32
            Top = 24
            Width = 3
            Height = 13
          end
          object Label12: TLabel
            Left = 56
            Top = 86
            Width = 10
            Height = 13
            Caption = '-1'
          end
          object Label14: TLabel
            Left = 252
            Top = 86
            Width = 6
            Height = 13
            Anchors = [akTop, akRight]
            Caption = '1'
          end
          object TrackBar1: TTrackBar
            Left = 56
            Top = 24
            Width = 206
            Height = 15
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 0
            ThumbLength = 12
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar2: TTrackBar
            Left = 56
            Top = 40
            Width = 206
            Height = 15
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 1
            ThumbLength = 12
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar3: TTrackBar
            Left = 56
            Top = 56
            Width = 206
            Height = 15
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 2
            ThumbLength = 12
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar4: TTrackBar
            Left = 56
            Top = 72
            Width = 206
            Height = 15
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 50
            TabOrder = 3
            ThumbLength = 12
            TickMarks = tmBoth
            TickStyle = tsNone
          end
        end
        object GroupBox2: TGroupBox
          Left = 8
          Top = 160
          Width = 281
          Height = 105
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Blending Weights '
          TabOrder = 1
          DesignSize = (
            281
            105)
          object Label17: TLabel
            Left = 64
            Top = 86
            Width = 193
            Height = 13
            Alignment = taCenter
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = '0'
          end
          object Label7: TLabel
            Left = 16
            Top = 24
            Width = 27
            Height = 13
            Caption = 'Tex 0'
          end
          object Label8: TLabel
            Left = 16
            Top = 40
            Width = 27
            Height = 13
            Caption = 'Tex 1'
          end
          object Label9: TLabel
            Left = 16
            Top = 56
            Width = 27
            Height = 13
            Caption = 'Tex 2'
          end
          object Label10: TLabel
            Left = 16
            Top = 72
            Width = 27
            Height = 13
            Caption = 'Tex 3'
          end
          object Label13: TLabel
            Left = 252
            Top = 86
            Width = 6
            Height = 13
            Anchors = [akTop, akRight]
            Caption = '1'
          end
          object Label15: TLabel
            Left = 56
            Top = 86
            Width = 10
            Height = 13
            Caption = '-1'
          end
          object TrackBar5: TTrackBar
            Left = 56
            Top = 24
            Width = 206
            Height = 15
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 75
            TabOrder = 0
            ThumbLength = 12
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar6: TTrackBar
            Left = 56
            Top = 40
            Width = 206
            Height = 15
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 75
            TabOrder = 1
            ThumbLength = 12
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar7: TTrackBar
            Left = 56
            Top = 56
            Width = 206
            Height = 15
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 75
            TabOrder = 2
            ThumbLength = 12
            TickMarks = tmBoth
            TickStyle = tsNone
          end
          object TrackBar8: TTrackBar
            Left = 56
            Top = 72
            Width = 206
            Height = 15
            Anchors = [akLeft, akTop, akRight]
            Max = 100
            Position = 65
            TabOrder = 3
            ThumbLength = 12
            TickMarks = tmBoth
            TickStyle = tsNone
          end
        end
        object CheckBox2: TCheckBox
          Left = 14
          Top = 12
          Width = 131
          Height = 17
          Caption = 'CgShader Enabled'
          Checked = True
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
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
    Left = 316
    Top = 0
    Width = 275
    Height = 414
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object PanelFPS: TPanel
      Left = 1
      Top = 1
      Width = 273
      Height = 48
      Align = alTop
      Caption = 'FPS'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object GLSceneViewer1: TGLSceneViewer
      Left = 1
      Top = 49
      Width = 273
      Height = 364
      Camera = GLCamera1
      Buffer.Lighting = False
      Buffer.AntiAliasing = aa4xHQ
      FieldOfView = 139.764404296875000000
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
        Material.MaterialLibrary = GLMatLib
        Shader = CgShader1
      end
      item
        Name = 'LibMaterial1'
        Tag = 0
        Material.Texture.Disabled = False
        Material.MaterialLibrary = GLMatLib
      end
      item
        Name = 'LibMaterial2'
        Tag = 0
        Material.MaterialLibrary = GLMatLib
      end
      item
        Name = 'LibMaterial3'
        Tag = 0
        Material.MaterialLibrary = GLMatLib
      end>
    TexturePaths = '..\\..\\..\\..\\media\\'
    Left = 105
    Top = 80
  end
end
