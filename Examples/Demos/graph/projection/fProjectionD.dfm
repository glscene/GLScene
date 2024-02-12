object FormProjection: TFormProjection
  Left = 160
  Top = 79
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Projection'
  ClientHeight = 714
  ClientWidth = 959
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 168
  TextHeight = 24
  object SceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 959
    Height = 714
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera
    Buffer.BackgroundColor = clBlack
    Buffer.FaceCulling = False
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 160.919235229492200000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = SceneViewerMouseDown
    OnMouseMove = SceneViewerMouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 24
    Top = 24
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 60.000000000000000000
      TargetObject = GLDummyCube
      Position.Coordinates = {0000E0400000A040000040400000803F}
    end
    object GLDummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLArrowLine1: TGLArrowLine
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        BottomRadius = 0.100000001490116100
        Height = 1.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000484200004842000000000000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object DirectOpenGL: TGLDirectOpenGL
      UseBuildList = False
      OnRender = DirectOpenGLRender
      Blend = False
    end
    object GLPoints: TGLPoints
      NoZWrite = False
      Static = False
      Size = 5.000000000000000000
      Style = psSmooth
    end
    object GLPlane: TGLPlane
      Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000803F9A99193F}
      Material.BlendingMode = bmTransparency
      Material.MaterialOptions = [moNoLighting]
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Height = 6.000000000000000000
      Width = 6.000000000000000000
      object GLXYZGrid1: TGLXYZGrid
        LineColor.Color = {0000803F0000803F0000803F0000003F}
        XSamplingScale.Min = -3.000000000000000000
        XSamplingScale.Max = 3.000000000000000000
        XSamplingScale.Origin = 1.000000000000000000
        XSamplingScale.Step = 0.500000000000000000
        YSamplingScale.Min = -3.000000000000000000
        YSamplingScale.Max = 3.000000000000000000
        YSamplingScale.Step = 0.500000000000000000
        ZSamplingScale.Min = -3.000000000000000000
        ZSamplingScale.Max = 3.000000000000000000
        ZSamplingScale.Step = 0.100000001490116100
      end
    end
  end
end
