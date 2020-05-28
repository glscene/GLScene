object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MultiMaterial'
  ClientHeight = 410
  ClientWidth = 526
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 526
    Height = 410
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 152.586074829101600000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000803F000040400000A0C00000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object GLCube1: TGLCube
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'MultipassMat'
      CubeSize = {000000400000004000000040}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'MultipassMat'
        Tag = 0
        Shader = GLMultiMaterialShader1
      end>
    Left = 120
    Top = 8
  end
  object GLMaterialLibrary2: TGLMaterialLibrary
    Left = 120
    Top = 64
  end
  object GLMultiMaterialShader1: TGLMultiMaterialShader
    MaterialLibrary = GLMaterialLibrary2
    VisibleAtDesignTime = False
    ShaderStyle = ssReplace
    Left = 240
    Top = 64
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 64
  end
  object GLTexCombineShader1: TGLTexCombineShader
    Combiners.Strings = (
      'Tex0:=PrimaryColor dot3 Tex0;')
    DesignTimeEnabled = False
    Left = 240
    Top = 8
  end
end
