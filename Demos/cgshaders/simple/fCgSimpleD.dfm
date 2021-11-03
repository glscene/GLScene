object FormCgSimple: TFormCgSimple
  Left = 202
  Top = 112
  Caption = 'Cg Simple Shader'
  ClientHeight = 550
  ClientWidth = 939
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 461
    Top = 0
    Width = 4
    Height = 550
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
    Width = 461
    Height = 550
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
      Width = 457
      Height = 546
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ActivePage = TabSheet2
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
          Top = 376
          Width = 446
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
          Width = 446
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
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = CBVertexProgramClick
          end
        end
        object Panel11: TPanel
          Left = 0
          Top = 31
          Width = 446
          Height = 345
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
            Width = 444
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
            Width = 444
            Height = 292
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
            OnChange = MemoVertCodeChange
          end
          object Panel13: TPanel
            Left = 1
            Top = 313
            Width = 444
            Height = 31
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              447
              31)
            object ButtonApplyVP: TButton
              Left = 353
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
              ExplicitLeft = 350
            end
          end
        end
        object Panel5: TPanel
          Left = 0
          Top = 384
          Width = 449
          Height = 130
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alBottom
          TabOrder = 2
          ExplicitTop = 380
          ExplicitWidth = 446
          DesignSize = (
            449
            130)
          object Label2: TLabel
            Left = 358
            Top = 6
            Width = 39
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akTop, akRight]
            Caption = 'Show:'
            ExplicitLeft = 355
          end
          object Memo1: TMemo
            Left = 1
            Top = 1
            Width = 343
            Height = 128
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
            ExplicitWidth = 340
          end
          object Button1: TButton
            Left = 356
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
            ExplicitLeft = 353
          end
          object Button4: TButton
            Left = 356
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
            ExplicitLeft = 353
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
          Top = 380
          Width = 449
          Height = 4
          Cursor = crVSplit
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alBottom
          Color = clBtnShadow
          ParentColor = False
          ExplicitTop = 376
          ExplicitWidth = 446
        end
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 449
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
            TabOrder = 0
            OnClick = CBFragmentProgramClick
          end
        end
        object Panel6: TPanel
          Left = 0
          Top = 31
          Width = 449
          Height = 349
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
            Width = 447
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
            Width = 447
            Height = 296
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
            OnChange = MemoFragCodeChange
          end
          object Panel3: TPanel
            Left = 1
            Top = 317
            Width = 447
            Height = 31
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              447
              31)
            object ButtonApplyFP: TButton
              Left = 353
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
            end
          end
        end
        object Panel8: TPanel
          Left = 0
          Top = 384
          Width = 449
          Height = 130
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alBottom
          TabOrder = 2
          DesignSize = (
            449
            130)
          object Label1: TLabel
            Left = 358
            Top = 6
            Width = 39
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akTop, akRight]
            Caption = 'Show:'
            ExplicitLeft = 355
          end
          object Memo3: TMemo
            Left = 1
            Top = 1
            Width = 343
            Height = 128
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
          end
          object Button2: TButton
            Left = 356
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
          end
          object Button3: TButton
            Left = 356
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
          end
        end
      end
    end
  end
  object Panel9: TPanel
    Left = 465
    Top = 0
    Width = 474
    Height = 550
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
      Width = 472
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
      Width = 472
      Height = 488
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Camera = GLCamera1
      Buffer.AntiAliasing = aa4x
      FieldOfView = 156.075897216796900000
      PenAsTouch = False
      Align = alClient
      OnMouseDown = GLSceneViewer1MouseDown
      OnMouseMove = GLSceneViewer1MouseMove
      TabOrder = 1
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 80
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000000000002041000000000000803F}
      LightStyle = lsOmni
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLFreeForm1: TGLFreeForm
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {8FC2F53C8FC2F53C8FC2F53C00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      AutoCentering = [macCenterX, macCenterY]
    end
    object GLXYZGrid1: TGLXYZGrid
      XSamplingScale.Min = -2.000000000000000000
      XSamplingScale.Max = 2.000000000000000000
      XSamplingScale.Step = 0.100000001490116100
      YSamplingScale.Step = 0.100000001490116100
      ZSamplingScale.Min = -2.000000000000000000
      ZSamplingScale.Max = 2.000000000000000000
      ZSamplingScale.Step = 0.100000001490116100
      Parts = [gpX, gpZ]
    end
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {000000003333333F000000000000803F}
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {0000004000004040000080400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {8988083E00000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {DBDADA3ED5D4543EA1A0A03D0000803F}
        Material.FrontProperties.Shininess = 128
        Material.FrontProperties.Specular.Color = {EDEC6C3EDDDC5C3ED5D4543E0000803F}
        Shader = CgShader1
      end>
    Left = 40
    Top = 136
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 112
    Top = 80
  end
  object CgShader1: TCgShader
    VertexProgram.OnApply = CgShader1ApplyVP
    OnApplyVP = CgShader1ApplyVP
    OnInitialize = CgShader1Initialize
    Left = 184
    Top = 80
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 260
    Top = 80
  end
end
