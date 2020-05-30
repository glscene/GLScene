object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Mesh Hit'
  ClientHeight = 271
  ClientWidth = 555
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 79
    Height = 13
    Caption = 'Orthogonal View'
  end
  object Label2: TLabel
    Left = 280
    Top = 8
    Width = 81
    Height = 13
    Caption = 'Perspective View'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 24
    Width = 265
    Height = 241
    Camera = GLCamera1
    Buffer.BackgroundColor = clGray
    FieldOfView = 170.512069702148400000
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLSceneViewer2: TGLSceneViewer
    Left = 279
    Top = 22
    Width = 265
    Height = 241
    Camera = GLCamera2
    Buffer.BackgroundColor = clGray
    FieldOfView = 134.929183959960900000
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
