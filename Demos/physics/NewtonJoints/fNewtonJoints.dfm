object FormNewtonJoints: TFormNewtonJoints
  Left = 0
  Top = 0
  Caption = 'Newton Joints'
  ClientHeight = 403
  ClientWidth = 575
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 575
    Height = 403
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 152.128311157226600000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 16
    object GLLines1: TGLLines
      Pickable = False
      Nodes = <>
      NodesAspect = lnaInvisible
      SplineMode = lsmSegments
      Options = [loUseNodeColorForLines]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Floor
      Position.Coordinates = {0000000000004040000070410000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object Floor: TGLCube
      Position.Coordinates = {00000000000080BF000000000000803F}
      BehavioursData = {
        0458434F4C02010201060C54474C4E47445374617469630200060A4E47442053
        746174696302000201060D474C4E47444D616E61676572310800080500000000
        000AD7A3F83F1200000000}
      CubeSize = {0000A0410000803F0000A041}
    end
    object Hinge: TGLCube
      Position.Coordinates = {000080C000004040000000000000803F}
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000201060D474C4E47444D616E6167657231080008050000
        00000000000000001200000000020109050000000000CDCCCCFB3F0500000000
        00000080FF3F0905000000000000000000000200080200080200090000000000
        000000000000000000803F020008}
      object GLAbsoluteHUDText1: TGLAbsoluteHUDText
        Text = 'Hinge Joint'
        Rotation = 0.000000000000000000
      end
    end
    object Slider: TGLCube
      Position.Coordinates = {000000C000004040000000000000803F}
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000201060D474C4E47444D616E6167657231080008050000
        00000000000000001200000000020109050000000000CDCCCCFB3F0500000000
        00000080FF3F0905000000000000000000000200080200080200090000000000
        000000000000000000803F020008}
      object GLAbsoluteHUDText2: TGLAbsoluteHUDText
        Text = 'Slider Joint'
        Rotation = 0.000000000000000000
      end
    end
    object Corkscrew: TGLCube
      Position.Coordinates = {0000000000004040000000000000803F}
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000201060D474C4E47444D616E6167657231080008050000
        00000000000000001200000000020109050000000000CDCCCCFB3F0500000000
        00000080FF3F0905000000000000000000000200080200080200090000000000
        000000000000000000803F020008}
      object GLAbsoluteHUDText3: TGLAbsoluteHUDText
        Text = 'Corkscrew Joint'
        Rotation = 0.000000000000000000
      end
    end
    object CustomHinge: TGLCube
      Position.Coordinates = {000000400000C040000000000000803F}
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000201060D474C4E47444D616E6167657231080008050000
        00000000000000001200000000020109050000000000CDCCCCFB3F0500000000
        00000080FF3F0905000000000000000000000200080200080200090000000000
        000000000000000000803F020008}
      CubeSize = {000000400000004000000040}
      object GLAbsoluteHUDText4: TGLAbsoluteHUDText
        Text = 'Custom Hinge with Limits'
        Rotation = 0.000000000000000000
      end
    end
    object CustomSlider: TGLCube
      Position.Coordinates = {0000C0400000C040000000000000803F}
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000201060D474C4E47444D616E6167657231080008050000
        00000000000000001200000000020109050000000000CDCCCCFB3F0500000000
        00000080FF3F0905000000000000000000000200080200080200090000000000
        000000000000000000803F020008}
      CubeSize = {000000400000004000000040}
      object GLAbsoluteHUDText5: TGLAbsoluteHUDText
        Text = 'Custom Slider with Limits'
        Rotation = 0.000000000000000000
      end
    end
    object Universal: TGLCone
      Position.Coordinates = {0000A0C00000803F0000E0400000803F}
      BottomRadius = 0.500000000000000000
      Height = 1.000000000000000000
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000201060D474C4E47444D616E6167657231080008050000
        00000000000000001200000000020109050000000000CDCCCCFB3F0500000000
        00000080FF3F0905000000000000000000000200080200080200092EBD3B3200
        0080BE000000000000803F020008}
      object GLAbsoluteHUDText6: TGLAbsoluteHUDText
        Text = 'Universal Joint'
        Rotation = 0.000000000000000000
      end
    end
    object CustomBall: TGLSphere
      Position.Coordinates = {0000A040000000400000A0400000803F}
      Radius = 1.000000000000000000
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000201060D474C4E47444D616E6167657231080008050000
        00000000000000001200000000020109050000000000CDCCCCFB3F0500000000
        00000080FF3F0905000000000000000000000200080200080200090000000000
        000000000000000000803F020008}
      object GLAbsoluteHUDText8: TGLAbsoluteHUDText
        Text = 'Custom Ball with Limits'
        Rotation = 0.000000000000000000
      end
    end
    object Ball: TGLSphere
      Position.Coordinates = {000000000000803F0000A0400000803F}
      Radius = 0.500000000000000000
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000201060D474C4E47444D616E6167657231080008050000
        00000000000000001200000000020109050000000000CDCCCCFB3F0500000000
        00000080FF3F0905000000000000000000000200080200080200090000000000
        000000000000000000803F020008}
      object GLAbsoluteHUDText7: TGLAbsoluteHUDText
        Text = 'Ball and Socketl Joint'
        Rotation = 0.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 104
    Top = 16
  end
  object GLNGDManager1: TGLNGDManager
    VisibleAtRunTime = True
    NewtonSurfaceItem = <>
    NewtonSurfacePair = <>
    DebugOption.NGDManagerDebugs = [mdShowJoint]
    Line = GLLines1
    NewtonJoint = <
      item
        KinematicControllerOptions.LinearFriction = 75.000000000000000000
        JointType = nj_KinematicController
        CollisionState = True
      end
      item
        HingeOptions.PivotPoint.Coordinates = {000080C000004040000000000000803F}
        HingeOptions.PinDirection.Coordinates = {00000000000040400000000000000000}
        JointType = nj_Hinge
        ParentObject = Floor
        ChildObject = Hinge
        CollisionState = True
      end
      item
        SliderOptions.PivotPoint.Coordinates = {000000C000004040000000000000803F}
        SliderOptions.PinDirection.Coordinates = {00000000000040400000000000000000}
        JointType = nj_Slider
        ParentObject = Floor
        ChildObject = Slider
        CollisionState = True
      end
      item
        CorkscrewOptions.PivotPoint.Coordinates = {0000000000004040000000000000803F}
        CorkscrewOptions.PinDirection.Coordinates = {00000000000040400000000000000000}
        JointType = nj_Corkscrew
        ParentObject = Floor
        ChildObject = Corkscrew
        CollisionState = True
      end
      item
        CustomHingeOptions.PivotPoint.Coordinates = {000000400000C040000000000000803F}
        CustomHingeOptions.PinDirection.Coordinates = {00000000000040400000000000000000}
        CustomHingeOptions.MinAngle = -30.000000000000000000
        CustomHingeOptions.MaxAngle = 30.000000000000000000
        JointType = nj_CustomHinge
        ParentObject = Floor
        ChildObject = CustomHinge
        CollisionState = True
      end
      item
        CustomSliderOptions.PivotPoint.Coordinates = {0000C0400000C040000000000000803F}
        CustomSliderOptions.PinDirection.Coordinates = {00000000000040400000000000000000}
        CustomSliderOptions.MinDistance = -3.000000000000000000
        JointType = nj_CustomSlider
        ParentObject = Floor
        ChildObject = CustomSlider
        CollisionState = True
      end
      item
        UniversalOptions.PivotPoint.Coordinates = {0000A0C00000803F0000E0400000803F}
        UniversalOptions.PinDirection.Coordinates = {00000040000000400000000000000000}
        UniversalOptions.PinDirection2.Coordinates = {00000000000000C00000004000000000}
        JointType = nj_Universal
        ParentObject = Floor
        ChildObject = Universal
        CollisionState = True
      end
      item
        BallAndSocketOptions.PivotPoint.Coordinates = {00000000000040400000A0400000803F}
        JointType = nj_BallAndSocket
        ParentObject = Floor
        ChildObject = Ball
        CollisionState = True
      end
      item
        CustomBallAndSocketOptions.PivotPoint.Coordinates = {0000A040000080400000A0400000803F}
        CustomBallAndSocketOptions.MinTwistAngle = -45.000000000000000000
        CustomBallAndSocketOptions.MaxTwistAngle = 45.000000000000000000
        JointType = nj_CustomBallAndSocket
        ParentObject = Floor
        ChildObject = CustomBall
        CollisionState = True
      end>
    Left = 200
    Top = 16
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Newton Joints - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    OnMouseMove = GLSimpleNavigation1MouseMove
    Left = 40
    Top = 80
  end
end
