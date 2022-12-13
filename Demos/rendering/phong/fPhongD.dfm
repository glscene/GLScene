object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Phong Shader'
  ClientHeight = 398
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  DesignSize = (
    623
    398)
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 623
    Height = 398
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 151.792068481445300000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
    ExplicitWidth = 504
    ExplicitHeight = 340
  end
  object CheckBox1: TCheckBox
    Left = 466
    Top = 24
    Width = 65
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Phong Shader'
    TabOrder = 1
    OnClick = CheckBox1Click
    ExplicitLeft = 432
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000803F00000040000040400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          Specular.Color = {0000803F0000803F0000803F0000803F}
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object GLTeapot1: TGLTeapot
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'phong'
      Scale.Coordinates = {00004040000040400000404000000000}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'phong'
        Tag = 0
        Material.FrontProperties.Shininess = 16
        Material.FrontProperties.Specular.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
        Shader = GLPhongShader1
      end>
    Left = 40
    Top = 8
  end
  object GLPhongShader1: TGLPhongShader
    Left = 72
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpHigher
    Left = 40
    Top = 40
  end
end
