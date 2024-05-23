object FormMeshHit: TFormMeshHit
  Left = 108
  Top = 101
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Mesh Hit'
  ClientHeight = 485
  ClientWidth = 998
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 24
  object Label1: TLabel
    Left = 14
    Top = 14
    Width = 141
    Height = 24
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Orthogonal View'
  end
  object Label2: TLabel
    Left = 490
    Top = 14
    Width = 145
    Height = 24
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Perspective View'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 14
    Top = 42
    Width = 464
    Height = 422
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clGray
    FieldOfView = 174.573181152343800000
    PenAsTouch = False
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLSceneViewer2: TGLSceneViewer
    Left = 499
    Top = 47
    Width = 464
    Height = 422
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera2
    Buffer.BackgroundColor = clGray
    FieldOfView = 153.337402343750000000
    PenAsTouch = False
    OnMouseDown = GLSceneViewer2MouseDown
    OnMouseMove = GLSceneViewer2MouseMove
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 56
    Top = 48
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000048420000704200008C420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object FreeForm1: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
        ShowAxes = True
        Up.Coordinates = {00000000000000000000803F00000000}
        NormalsOrientation = mnoInvert
      end
    end
    object Sphere1: TGLSphere
      Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {E9DC72BF000000009BE8A13E00000000}
      Radius = 0.300000011920929000
      Slices = 6
      Stacks = 6
      object ArrowLine1: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F9A99193F}
        Material.FrontProperties.Emission.Color = {1283803E1283803E000000000000803F}
        Material.BlendingMode = bmTransparency
        Position.Coordinates = {0000000000000000CDCCCC3D0000803F}
        BottomRadius = 0.050000000745058060
        Height = 1.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 10.000000000000000000
      TargetObject = DummyCube1
      CameraStyle = csOrthogonal
      Position.Coordinates = {0000504100004040000010410000803F}
    end
    object GLCamera2: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000204100002041000020410000803F}
    end
  end
end
