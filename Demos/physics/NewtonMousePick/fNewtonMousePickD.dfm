object FormNewtonMousePick: TFormNewtonMousePick
  Left = 0
  Top = 0
  Caption = 'Newton Mouse Pick'
  ClientHeight = 504
  ClientWidth = 716
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 17
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 716
    Height = 504
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 157.555084228515600000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 160
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {0000000000004040000020410000803F}
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
      CubeSize = {000020410000803F00002041}
      object GLCube2: TGLCube
        Position.Coordinates = {000000000000A0400000A0C00000803F}
        BehavioursData = {
          0458434F4C02010201060C54474C4E47445374617469630200060A4E47442053
          746174696302000201060D474C4E47444D616E61676572310800080500000000
          000AD7A3F83F1200000000}
        CubeSize = {00002041000020410000803F}
      end
      object GLCube3: TGLCube
        Position.Coordinates = {0000C0400000A040000080BF0000803F}
        BehavioursData = {
          0458434F4C02010201060C54474C4E47445374617469630200060A4E47442053
          746174696302000201060D474C4E47444D616E61676572310800080500000000
          000AD7A3F83F1200000000}
        CubeSize = {0000803F0000204100002041}
      end
      object GLCube4: TGLCube
        Position.Coordinates = {0000C0C00000A040000080BF0000803F}
        BehavioursData = {
          0458434F4C02010201060C54474C4E47445374617469630200060A4E47442053
          746174696302000201060D474C4E47444D616E61676572310800080500000000
          000AD7A3F83F1200000000}
        CubeSize = {0000803F0000204100002041}
      end
    end
    object GLCube1: TGLCube
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000201060D474C4E47444D616E6167657231080008050000
        00000000000000001200000000020109050000000000CDCCCCFB3F0500000000
        00000080FF3F0905000000000000000000000200080200080200090000000000
        000000000000000000803F020008}
    end
    object GLSphere1: TGLSphere
      Position.Coordinates = {000000C000004040000000000000803F}
      Radius = 0.500000000000000000
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000201060D474C4E47444D616E6167657231080008050000
        00000000000000001200000000020109050000000000CDCCCCFB3F0500000000
        00000080FF3F0905000000000000000000000200080200080200090000000000
        000000000000000000803F020008}
    end
    object GLLines1: TGLLines
      Pickable = False
      Nodes = <>
      NodesAspect = lnaInvisible
      SplineMode = lsmSegments
      Options = [loUseNodeColorForLines]
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 288
  end
  object GLNGDManager1: TGLNGDManager
    VisibleAtRunTime = True
    NewtonSurfaceItem = <>
    NewtonSurfacePair = <>
    DebugOption.NGDManagerDebugs = [mdShowAABB, mdShowJoint]
    Line = GLLines1
    NewtonJoint = <
      item
        KinematicControllerOptions.PickModeLinear = True
        KinematicControllerOptions.AngularFriction = 500.000000000000000000
        JointType = nj_KinematicController
        CollisionState = True
      end>
    Left = 400
  end
end
