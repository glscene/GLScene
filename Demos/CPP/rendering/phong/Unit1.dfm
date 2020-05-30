object Form1: TForm1
  Left = 264
  Top = 142
  Caption = 'Phong'
  ClientHeight = 403
  ClientWidth = 479
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    479
    403)
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 479
    Height = 403
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = GLCamera1
    FieldOfView = 152.128311157226600000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 307
    Top = 8
    Width = 65
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akTop, akRight]
    Caption = 'Shader'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object GLScene1: TGLScene
    Left = 32
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
    Left = 32
    Top = 144
  end
  object GLPhongShader1: TGLPhongShader
    Enabled = False
    Left = 32
    Top = 216
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 32
    Top = 72
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpHigher
    Left = 416
    Top = 8
  end
end
