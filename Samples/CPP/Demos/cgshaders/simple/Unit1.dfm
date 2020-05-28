object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Cg Simple Shader'
  ClientHeight = 422
  ClientWidth = 589
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
    Left = 369
    Top = 0
    Height = 422
    Color = clBtnShadow
    ParentColor = False
    ExplicitTop = -85
    ExplicitHeight = 440
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 369
    Height = 422
    Align = alLeft
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 2
      Top = 2
      Width = 365
      Height = 418
      ActivePage = TabSheet1
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
          Top = 283
          Width = 357
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          Color = clBtnShadow
          ParentColor = False
          ExplicitTop = 318
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 357
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
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = CBVertexProgramClick
          end
        end
        object Panel11: TPanel
          Left = 0
          Top = 25
          Width = 357
          Height = 258
          Align = alClient
          Caption = 'Panel6'
          TabOrder = 1
          object Panel12: TPanel
            Left = 1
            Top = 1
            Width = 355
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
            Width = 355
            Height = 215
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
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
            Top = 232
            Width = 355
            Height = 25
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              355
              25)
            object ButtonApplyVP: TButton
              Left = 280
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
          Top = 286
          Width = 357
          Height = 104
          Align = alBottom
          TabOrder = 2
          DesignSize = (
            357
            104)
          object Label2: TLabel
            Left = 284
            Top = 5
            Width = 30
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Show:'
          end
          object Memo1: TMemo
            Left = 1
            Top = 1
            Width = 272
            Height = 102
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
          object Button1: TButton
            Left = 282
            Top = 21
            Width = 64
            Height = 18
            Anchors = [akTop, akRight]
            Caption = 'Params'
            TabOrder = 1
            OnClick = Button1Click
          end
          object Button4: TButton
            Left = 282
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
          Top = 283
          Width = 357
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          Color = clBtnShadow
          ParentColor = False
          ExplicitTop = 318
        end
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 357
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
            TabOrder = 0
            OnClick = CBFragmentProgramClick
          end
        end
        object Panel6: TPanel
          Left = 0
          Top = 25
          Width = 357
          Height = 258
          Align = alClient
          Caption = 'Panel6'
          TabOrder = 1
          object Panel7: TPanel
            Left = 1
            Top = 1
            Width = 355
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
            Width = 355
            Height = 215
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
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
            Top = 232
            Width = 355
            Height = 25
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              355
              25)
            object ButtonApplyFP: TButton
              Left = 280
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
          Top = 286
          Width = 357
          Height = 104
          Align = alBottom
          TabOrder = 2
          DesignSize = (
            357
            104)
          object Label1: TLabel
            Left = 284
            Top = 5
            Width = 30
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Show:'
          end
          object Memo3: TMemo
            Left = 1
            Top = 1
            Width = 272
            Height = 102
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
            Left = 282
            Top = 21
            Width = 64
            Height = 18
            Anchors = [akTop, akRight]
            Caption = 'Params'
            TabOrder = 1
            OnClick = Button2Click
          end
          object Button3: TButton
            Left = 282
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
    end
  end
  object Panel9: TPanel
    Left = 372
    Top = 0
    Width = 217
    Height = 422
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
      Width = 215
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
      Width = 215
      Height = 372
      Camera = GLCamera1
      Buffer.AntiAliasing = aa4x
      FieldOfView = 130.112182617187500000
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
