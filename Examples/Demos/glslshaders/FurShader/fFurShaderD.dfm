object FormFur: TFormFur
  Left = 188
  Top = 109
  Caption = 'Fur Shaser'
  ClientHeight = 512
  ClientWidth = 716
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
    Width = 716
    Height = 512
    Camera = Camera
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aaNone
    Buffer.DepthPrecision = dp24bits
    Buffer.ColorDepth = cd24bits
    FieldOfView = 151.506607055664100000
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
    end
    object Scene: TGLDummyCube
      CubeSize = 1.000000000000000000
      object SceneMesh: TGLFreeForm
        Direction.Coordinates = {00000000D7B35DBFFFFFFF3E00000000}
        PitchAngle = -60.000000000000000000
        Up.Coordinates = {00000000FFFFFF3ED7B35D3F00000000}
      end
    end
    object Camera: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 65.000000000000000000
      TargetObject = Scene
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {000040400000803F000016430000803F}
      Direction.Coordinates = {00000000000000800000803F00000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
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
    Materials = <
      item
        Name = 'Fur'
        Tag = 0
        Material.Texture.Compression = tcHighSpeed
        Material.Texture.Disabled = False
        Material.FaceCulling = fcCull
        Material.MaterialLibrary = MatLib
      end
      item
        Name = 'FurColor'
        Tag = 0
        Material.Texture.Compression = tcHighSpeed
        Material.Texture.Disabled = False
        Material.FaceCulling = fcCull
        Material.MaterialLibrary = MatLib
      end>
    Left = 24
    Top = 96
  end
end
