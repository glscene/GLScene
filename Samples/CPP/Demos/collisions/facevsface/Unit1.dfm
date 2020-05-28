object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Face vs Face'
  ClientHeight = 445
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 428
    Top = 0
    Width = 4
    Height = 296
    Align = alRight
    ExplicitLeft = 460
    ExplicitHeight = 331
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 121
    Top = 0
    Width = 307
    Height = 296
    Camera = GLCamera2
    Buffer.BackgroundColor = 8404992
    FieldOfView = 40.608947753906250000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 121
    Height = 296
    Align = alLeft
    BevelOuter = bvLowered
    TabOrder = 1
    object Shape1: TShape
      Left = 88
      Top = 10
      Width = 25
      Height = 25
      Shape = stCircle
    end
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 64
      Height = 13
      Caption = 'Collision test:'
    end
    object LATime: TLabel
      Left = 8
      Top = 24
      Width = 73
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'LATime'
    end
    object Label2: TLabel
      Left = 8
      Top = 96
      Width = 102
      Height = 13
      Caption = 'Teapot collision mode'
    end
    object cbCollisionMode: TRadioGroup
      Left = 8
      Top = 112
      Width = 105
      Height = 105
      ItemIndex = 4
      Items.Strings = (
        'cbmPoint'
        'cbmSphere'
        'cbmEllipsoid'
        'cbmCube'
        'cbmFaces')
      TabOrder = 0
      OnClick = cbCollisionModeClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 296
    Width = 577
    Height = 149
    Align = alBottom
    TabOrder = 2
    object StringGrid1: TStringGrid
      Left = 8
      Top = 6
      Width = 553
      Height = 139
      ColCount = 6
      DefaultColWidth = 90
      DefaultRowHeight = 20
      RowCount = 6
      TabOrder = 0
    end
  end
  object Memo1: TMemo
    Left = 432
    Top = 0
    Width = 145
    Height = 296
    Align = alRight
    Lines.Strings = (
      'Memo1')
    TabOrder = 3
  end
  object GLScene1: TGLScene
    Left = 192
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000020C100002041000020C10000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {0000003F0000003F0000003F0000803F}
      object GLCamera2: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 400.000000000000000000
        TargetObject = DummyCube1
        Position.Coordinates = {0000704100002041000070410000803F}
      end
      object txtX: TGLSpaceText
        Position.Coordinates = {0000004000000000000000000000803F}
        Scale.Coordinates = {0000003F0000003F0000003F00000000}
        Extrusion = 0.100000001490116100
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'X')
      end
      object txtY: TGLSpaceText
        Position.Coordinates = {0000000000000040000000000000803F}
        Scale.Coordinates = {0000003F0000003F0000003F00000000}
        Extrusion = 0.100000001490116100
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Y')
      end
      object txtZ: TGLSpaceText
        Direction.Coordinates = {00000000000000800000803F00000000}
        Position.Coordinates = {0000000000000000000000400000803F}
        Scale.Coordinates = {0000003F0000003F0000003F00000000}
        Extrusion = 0.100000001490116100
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Z')
      end
      object TeaPot1: TGLFreeForm
        Direction.Coordinates = {613D73BDD128723F4D43A3BE00000000}
        Position.Coordinates = {00000000000000003333B33E0000803F}
        Scale.Coordinates = {0AD7233C0AD7233C0AD7233C00000000}
        Up.Coordinates = {C9B14FBFC3BE0F3ED548113F00000000}
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102040200}
      end
      object TeaPot2: TGLFreeForm
        Direction.Coordinates = {0DFB4AB20000803F7A829A3200000000}
        Position.Coordinates = {0000000000000000CDCCCCBE0000803F}
        Scale.Coordinates = {0AD7233C0AD7233C0AD7233C00000000}
        Up.Coordinates = {F30435BFD41F8BB3F30435BF00000000}
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102040200}
      end
      object CubePoint1: TGLCube
        Position.Coordinates = {000000000000803F000000000000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102000200}
        CubeSize = {CDCC4C3DCDCC4C3DCDCC4C3D}
      end
      object CubePoint2: TGLCube
        Position.Coordinates = {CDCC4CBE0000803F000000000000803F}
        Scale.Coordinates = {CDCC4C3DCDCC4C3DCDCC4C3D00000000}
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102000200}
      end
      object Cube2: TGLCube
        Material.BackProperties.Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000003F}
        Material.BackProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000003F}
        Material.BackProperties.Emission.Color = {0000000000000000000000000000003F}
        Material.BackProperties.Specular.Color = {0000000000000000000000000000003F}
        Material.FrontProperties.Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3ECDCC4C3E}
        Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000003F}
        Material.FrontProperties.Emission.Color = {0000000000000000000000000000003F}
        Material.BlendingMode = bmTransparency
        Position.Coordinates = {0000803F00000000000080BF0000803F}
        Scale.Coordinates = {0000803F000000400000803F00000000}
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102030200}
        CubeSize = {0000803F0000803F0000003F}
      end
      object Bar: TGLCube
        Material.FrontProperties.Diffuse.Color = {0000803F5839343E5839343E0000803F}
        Position.Coordinates = {0000803F000000000000803F0000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102030200}
        CubeSize = {CDCC4C3DCDCC4C3D00000040}
      end
      object GLSphere1: TGLSphere
        Position.Coordinates = {000000C000000000000000000000803F}
        Radius = 0.500000000000000000
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102010200}
      end
      object GLSphere2: TGLSphere
        Position.Coordinates = {000000C000000000000080BF0000803F}
        Scale.Coordinates = {CDCC4C3FCDCC4C3FCDCC4C3F00000000}
        Radius = 0.300000011920929000
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102010200}
      end
      object GLSphereEllipsoid1: TGLSphere
        Position.Coordinates = {000080BF00000000000000C00000803F}
        Scale.Coordinates = {9A99193F0000803F0000803F00000000}
        Radius = 0.500000000000000000
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102020200}
      end
      object GLSphereEllipsoid2: TGLSphere
        Position.Coordinates = {0000000000000000000000C00000803F}
        Scale.Coordinates = {CDCC4C3FCDCCCC3F3333333F00000000}
        Radius = 0.300000011920929000
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102020200}
      end
      object GLCube1: TGLCube
        Position.Coordinates = {0000803F000080BF000000000000803F}
        Scale.Coordinates = {3333333F3333B33F0000803F00000000}
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102030200}
        CubeSize = {0000803F0000803F6666A63F}
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
    end
    object GLCamera3: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 464
    Top = 8
  end
  object CollisionManager1: TGLCollisionManager
    OnCollision = CollisionManager1Collision
    Left = 192
    Top = 80
  end
end
