object GLSLFrm: TGLSLFrm
  Left = 368
  Top = 316
  Caption = 'GLSL_Morph'
  ClientHeight = 396
  ClientWidth = 474
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  TextHeight = 13
  object GLViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 474
    Height = 396
    Camera = Cam
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aaNone
    Buffer.DepthPrecision = dp24bits
    Buffer.ColorDepth = cd24bits
    FieldOfView = 143.651763916015600000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLViewerMouseDown
    OnMouseMove = GLViewerMouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 16
    object RenderDirectGL: TGLDirectOpenGL
      UseBuildList = False
      OnRender = RenderDirectGLRender
      Blend = False
    end
    object CamBox: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Cam: TGLCamera
        DepthOfView = 500.000000000000000000
        FocalLength = 65.000000000000000000
        TargetObject = Scene
        CameraStyle = csInfinitePerspective
        Position.Coordinates = {000000000000A040000048430000803F}
        Direction.Coordinates = {00000000000000800000803F00000000}
      end
    end
    object Scene: TGLDummyCube
      Direction.Coordinates = {0000003F00000000D7B35D3F00000000}
      Scale.Coordinates = {00004842000048420000484200000000}
      TurnAngle = 30.000000000000000000
      CubeSize = 1.000000000000000000
      object SceneMesh: TGLFreeForm
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.MaterialLibrary = MatLib
        Visible = False
        MaterialLibrary = MatLib
        LightmapLibrary = MatLib
      end
    end
    object Light: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000704200007042000070420000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    MaxDeltaTime = 0.020000000000000000
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 56
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 650
    OnTimer = Timer1Timer
    Left = 24
    Top = 136
  end
  object MatLib: TGLMaterialLibrary
    Left = 24
    Top = 96
  end
end
