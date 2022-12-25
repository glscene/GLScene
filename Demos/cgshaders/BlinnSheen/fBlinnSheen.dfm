object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'Cg Normal/Bump Blinn Shading Demo'
  ClientHeight = 386
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 423
    Height = 386
    Camera = GLCamera1
    Buffer.BackgroundColor = clGray
    FieldOfView = 150.951690673828100000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 320
    Top = 8
    Width = 97
    Height = 17
    Caption = 'BumpShader'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLFreeForm1: TGLFreeForm
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'CgShaderMat'
      BehavioursData = {
        0458434F4C02010201060B54474C42496E657274696102001200000000020002
        00050000000000000080FF3F0200080500000000000000000000050000000000
        0000000000050000000000000000000008020008020008}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {000000000000C040000070410000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object CgBumpShader: TCgShader
    VertexProgram.OnApply = CgBumpShaderApplyVP
    FragmentProgram.OnApply = CgBumpShaderApplyFP
    FragmentProgram.OnUnApply = CgBumpShaderUnApplyFP
    OnApplyVP = CgBumpShaderApplyVP
    OnApplyFP = CgBumpShaderApplyFP
    OnUnApplyFP = CgBumpShaderUnApplyFP
    OnInitialize = CgBumpShaderInitialize
    Left = 112
    Top = 72
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'CgShaderMat'
        Tag = 0
        Material.Texture.Disabled = False
        Shader = CgBumpShader
      end>
    Left = 112
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 72
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 24
    Top = 128
  end
end
