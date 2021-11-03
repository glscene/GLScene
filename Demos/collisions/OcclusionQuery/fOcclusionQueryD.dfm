object FormOcclusionQuery: TFormOcclusionQuery
  Left = 194
  Top = 119
  Caption = 'Occlusion Query'
  ClientHeight = 551
  ClientWidth = 701
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 17
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 51
    Width = 701
    Height = 500
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 157.380142211914100000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 701
    Height = 51
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 140
      Top = 4
      Width = 39
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 140
      Top = 28
      Width = 39
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Label2'
    end
    object Label3: TLabel
      Left = 250
      Top = 8
      Width = 322
      Height = 36
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Test objects are hidden!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -30
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LabelFPS: TLabel
      Left = 11
      Top = 28
      Width = 23
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'FPS'
    end
    object CheckBox1: TCheckBox
      Left = 11
      Top = 3
      Width = 122
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
