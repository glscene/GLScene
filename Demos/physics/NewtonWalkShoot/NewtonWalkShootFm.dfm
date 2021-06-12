object FormNewtonWalkShoot: TFormNewtonWalkShoot
  Left = 0
  Top = 0
  Caption = 'Newton Walk Carry Shoot'
  ClientHeight = 384
  ClientWidth = 580
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 580
    Height = 384
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 150.806854248046900000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 8
    object Body: TGLTorus
      Pickable = False
      MajorRadius = 0.400000005960464500
      MinorRadius = 0.100000001490116100
      StopAngle = 360.000000000000000000
      Parts = [toSides, toStartDisk, toStopDisk]
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D69630200020112000000000802090500000000000AD7A3F83F1200
        000000020109050000000000CDCCCCFB3F050000000000000080FF3F09050000
        00000000000000000200080200080200095455D5305455D5B000000000000080
        3F020008}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Body
      Position.Coordinates = {0000404000004040000040400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object World: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Beer: TGLFreeForm
      end
      object Mushroom: TGLFreeForm
      end
      object Chair: TGLFreeForm
      end
      object Teapot: TGLFreeForm
      end
      object Map: TGLFreeForm
      end
    end
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000002041000000000000803F}
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
    end
    object GLLines1: TGLLines
      Pickable = False
      Nodes = <>
      NodesAspect = lnaInvisible
      SplineMode = lsmSegments
      Options = [loUseNodeColorForLines]
    end
    object GLHUDCross: TGLHUDSprite
      Pickable = False
      Rotation = 0.000000000000000000
    end
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    ZoomSpeed = 1.100000023841858000
    MoveAroundTargetSpeed = 0.250000000000000000
    FormCaption = 'Newton Walk Carry Shoot - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 48
    Top = 64
  end
  object GLNGDManager1: TGLNGDManager
    Gravity.Coordinates = {00000000004075C40000000000000000}
    NewtonSurfaceItem = <>
    NewtonSurfacePair = <>
    DebugOption.NGDManagerDebugs = [mdShowAABB, mdShowJoint]
    Line = GLLines1
    NewtonJoint = <
      item
        JointType = nj_UpVector
        ParentObject = Body
      end
      item
        JointType = nj_UpVector
        ParentObject = Body
        UPVectorDirection.Coordinates = {0000803F000000000000000000000000}
      end>
    Left = 224
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 136
    Top = 8
  end
end
