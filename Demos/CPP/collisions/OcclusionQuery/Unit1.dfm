object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Occlusion Query'
  ClientHeight = 447
  ClientWidth = 575
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 41
    Width = 575
    Height = 406
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.BackgroundColor = clBackground
    FieldOfView = 152.326324462890600000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 575
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 112
      Top = 3
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 112
      Top = 22
      Width = 31
      Height = 13
      Caption = 'Label2'
    end
    object Label3: TLabel
      Left = 200
      Top = 6
      Width = 259
      Height = 29
      Caption = 'Test objects are hidden!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LabelFPS: TLabel
      Left = 9
      Top = 22
      Width = 18
      Height = 13
      Caption = 'FPS'
    end
    object CheckBox1: TCheckBox
      Left = 9
      Top = 2
      Width = 97
      Height = 17
      Caption = 'Boolean test'
      Enabled = False
      TabOrder = 0
    end
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 48
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCube1: TGLCube
      end
      object GLCylinder1: TGLCylinder
        Position.Coordinates = {0000803F00000000000000000000803F}
        BottomRadius = 0.500000000000000000
        Height = 1.000000000000000000
        TopRadius = 0.500000000000000000
      end
    end
    object GLDummyCube2: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCube2: TGLCube
      end
    end
    object OGLBeginQueries: TGLDirectOpenGL
      UseBuildList = False
      OnRender = OGLBeginQueriesRender
      Blend = False
    end
    object dcTestObjects: TGLDummyCube
      Position.Coordinates = {0000003F3333333F0000803F0000803F}
      CubeSize = 1.000000000000000000
      object GLTorus1: TGLTorus
        Material.FrontProperties.Ambient.Color = {0000803F00000000000000000000803F}
        MajorRadius = 0.400000005960464500
        MinorRadius = 0.100000001490116100
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object GLCone1: TGLCone
        Material.FrontProperties.Ambient.Color = {0000803F00000000000000000000803F}
        BottomRadius = 0.500000000000000000
        Height = 1.000000000000000000
      end
    end
    object OGLEndQueries: TGLDirectOpenGL
      UseBuildList = False
      OnRender = OGLEndQueriesRender
      Blend = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {00000000000000000000A0400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 88
    Top = 48
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 152
    Top = 48
  end
end
