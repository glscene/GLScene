object FormObjMaterial: TFormObjMaterial
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Object Material'
  ClientHeight = 775
  ClientWidth = 1310
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 30
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1310
    Height = 700
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = Camera
    Buffer.BackgroundColor = clBlack
    FieldOfView = 157.380142211914100000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 1091
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 700
    Width = 1310
    Height = 75
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    TabOrder = 1
    ExplicitWidth = 1089
    object rgPolyhedra: TRadioGroup
      Left = 1
      Top = 1
      Width = 1308
      Height = 73
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Caption = 'Polyhedra'
      Columns = 6
      ItemIndex = 4
      Items.Strings = (
        'Tetrahedron'
        'Octahedron'
        'Hexahedron'
        'Icosahedron'
        'Dodecahedron'
        'Convexhull')
      TabOrder = 0
      OnClick = rgPolyhedraClick
      ExplicitWidth = 1087
    end
  end
  object chbRotation: TCheckBox
    Left = 602
    Top = 42
    Width = 141
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Rotation'
    TabOrder = 2
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 16
    object Camera: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 70.000000000000000000
      SceneScale = 2.000000000000000000
      TargetObject = dcPolyhedra
      Position.Coordinates = {0000803F0000803F0000803F0000803F}
      object LightSource: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object dcPolyhedra: TGLDummyCube
      Position.Coordinates = {000000000000A040000000000000803F}
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
      object Tetrahedron: TGLTetrahedron
        Visible = False
      end
      object dcPolyTet: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object TetTriangle0: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBlue'
          Nodes = <
            item
              X = 0.500000000000000000
              Y = 0.500000000000000000
              Z = 0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = -0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Y = -0.500000000000000000
              Z = 0.500000000000000000
            end>
        end
        object TetTriangle1: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrRed'
          Nodes = <
            item
              X = -0.500000000000000000
              Y = 0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = -0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = 0.500000000000000000
              Z = 0.500000000000000000
            end>
        end
        object TetTriangle2: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrGreen'
          Nodes = <
            item
              X = -0.500000000000000000
              Y = -0.500000000000000000
              Z = 0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Y = 0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = 0.500000000000000000
              Z = 0.500000000000000000
            end>
        end
        object TetTriangle3: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrViolet'
          Nodes = <
            item
              X = 0.500000000000000000
              Y = -0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Y = 0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Y = -0.500000000000000000
              Z = 0.500000000000000000
            end>
        end
      end
      object Octahedron: TGLOctahedron
        Visible = False
      end
      object dcPolyOct: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object OctTriangle0: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrAquamarine'
          Nodes = <
            item
              X = 0.500000000000000000
            end
            item
              Z = -0.500000000000000000
            end
            item
              Y = 0.500000000000000000
            end>
        end
        object OctTriangle1: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBakersChoc'
          Nodes = <
            item
              X = -0.500000000000000000
            end
            item
              Y = 0.500000000000000000
            end
            item
              Z = 0.500000000000000000
            end>
        end
        object OctTriangle2: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBlueViolet'
          Nodes = <
            item
              X = 0.500000000000000000
            end
            item
              Y = -0.500000000000000000
            end
            item
              Z = 0.500000000000000000
            end>
        end
        object OctTriangle3: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBrass'
          Nodes = <
            item
              X = -0.500000000000000000
            end
            item
              Z = 0.500000000000000000
            end
            item
              Y = -0.500000000000000000
            end>
        end
        object OctTriangle4: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBrightGold'
          Nodes = <
            item
              X = 0.500000000000000000
            end
            item
              Z = 0.500000000000000000
            end
            item
              Y = 0.500000000000000000
            end>
        end
        object OctTriangle5: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrCadetBlue'
          Nodes = <
            item
              X = -0.500000000000000000
            end
            item
              Z = -0.500000000000000000
            end
            item
              Y = 0.500000000000000000
            end>
        end
        object OctTriangle6: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrSkyBlue'
          Nodes = <
            item
              X = 0.500000000000000000
            end
            item
              Z = -0.500000000000000000
            end
            item
              Y = -0.500000000000000000
            end>
        end
        object OctTriangle7: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrCoral'
          Nodes = <
            item
              X = -0.500000000000000000
            end
            item
              Y = -0.500000000000000000
            end
            item
              Z = -0.500000000000000000
            end>
        end
      end
      object Hexahedron: TGLHexahedron
        Visible = False
      end
      object dcPolyHex: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object Quadrangle0: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrGreen'
          Nodes = <
            item
              X = -0.500000000000000000
              Y = -0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = -0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = -0.500000000000000000
              Z = 0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Y = -0.500000000000000000
              Z = 0.500000000000000000
            end>
        end
        object Quadrangle1: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrMidnightBlue'
          Nodes = <
            item
              X = -0.500000000000000000
              Y = -0.500000000000000000
              Z = 0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = -0.500000000000000000
              Z = 0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = 0.500000000000000000
              Z = 0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Y = 0.500000000000000000
              Z = 0.500000000000000000
            end>
        end
        object Quadrangle2: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrViolet'
          Nodes = <
            item
              X = -0.500000000000000000
              Y = 0.500000000000000000
              Z = 0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = 0.500000000000000000
              Z = 0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = 0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Y = 0.500000000000000000
              Z = -0.500000000000000000
            end>
        end
        object Quadrangle3: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrRed'
          Nodes = <
            item
              X = -0.500000000000000000
              Y = 0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = 0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = -0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Y = -0.500000000000000000
              Z = -0.500000000000000000
            end>
        end
        object Quadrangle4: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBronze'
          Nodes = <
            item
              X = -0.500000000000000000
              Y = -0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Y = -0.500000000000000000
              Z = 0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Y = 0.500000000000000000
              Z = 0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Y = 0.500000000000000000
              Z = -0.500000000000000000
            end>
        end
        object Quadrangle5: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrSteelBlue'
          Nodes = <
            item
              X = 0.500000000000000000
              Y = -0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = 0.500000000000000000
              Z = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = 0.500000000000000000
              Z = 0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Y = -0.500000000000000000
              Z = 0.500000000000000000
            end>
        end
      end
      object Icosahedron: TGLIcosahedron
        Visible = False
      end
      object dcPolyIco: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object IcoTriangle0: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBlue'
          Nodes = <
            item
              Y = 0.309017002582550000
              Z = -0.500000000000000000
            end
            item
              X = -0.309017002582550000
              Y = 0.500000000000000000
            end
            item
              X = 0.309017002582550000
              Y = 0.500000000000000000
            end>
        end
        object IcoTriangle1: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrMidnightBlue'
          Nodes = <
            item
              Y = 0.309017002582550000
              Z = 0.500000000000000000
            end
            item
              X = 0.309017002582550000
              Y = 0.500000000000000000
            end
            item
              X = -0.309017002582550000
              Y = 0.500000000000000000
            end>
        end
        object IcoTriangle2: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrViolet'
          Nodes = <
            item
              Y = 0.309017002582550000
              Z = 0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Z = 0.309017002582550000
            end
            item
              Y = -0.309017002582550000
              Z = 0.500000000000000000
            end>
        end
        object IcoTriangle3: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrRed'
          Nodes = <
            item
              Y = 0.309017002582550000
              Z = 0.500000000000000000
            end
            item
              Y = -0.309017002582550000
              Z = 0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Z = 0.309017002582550000
            end>
        end
        object IcoTriangle4: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrOrangeRed'
          Nodes = <
            item
              Y = 0.309017002582550000
              Z = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Z = -0.309017002582550000
            end
            item
              Y = -0.309017002582550000
              Z = -0.500000000000000000
            end>
        end
        object IcoTriangle5: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBronze'
          Nodes = <
            item
              Y = 0.309017002582550000
              Z = -0.500000000000000000
            end
            item
              Y = -0.309017002582550000
              Z = -0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Z = -0.309017002582550000
            end>
        end
        object IcoTriangle6: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrOldGold'
          Nodes = <
            item
              Y = -0.309017002582550000
              Z = 0.500000000000000000
            end
            item
              X = -0.309017002582550000
              Y = -0.500000000000000000
            end
            item
              X = 0.309017002582550000
              Y = -0.500000000000000000
            end>
        end
        object IcoTriangle7: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrGreen'
          Nodes = <
            item
              Y = -0.309017002582550000
              Z = -0.500000000000000000
            end
            item
              X = 0.309017002582550000
              Y = -0.500000000000000000
            end
            item
              X = -0.309017002582550000
              Y = -0.500000000000000000
            end>
        end
        object IcoTriangle8: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrForestGreen'
          Nodes = <
            item
              X = -0.309017002582550000
              Y = 0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Z = -0.309017002582550000
            end
            item
              X = -0.500000000000000000
              Z = 0.309017002582550000
            end>
        end
        object IcoTriangle9: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrSkyBlue'
          Nodes = <
            item
              X = -0.309017002582550000
              Y = -0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Z = 0.309017002582550000
            end
            item
              X = -0.500000000000000000
              Z = -0.309017002582550000
            end>
        end
        object IcoTriangle10: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrSteelBlue'
          Nodes = <
            item
              X = 0.309017002582550000
              Y = 0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Z = 0.309017002582550000
            end
            item
              X = 0.500000000000000000
              Z = -0.309017002582550000
            end>
        end
        object IcoTriangle11: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrAquamarine'
          Nodes = <
            item
              X = 0.309017002582550000
              Y = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Z = -0.309017002582550000
            end
            item
              X = 0.500000000000000000
              Z = 0.309017002582550000
            end>
        end
        object IcoTriangle12: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBakersChoc'
          Nodes = <
            item
              Y = 0.309017002582550000
              Z = 0.500000000000000000
            end
            item
              X = -0.309017002582550000
              Y = 0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Z = 0.309017002582550000
            end>
        end
        object IcoTriangle13: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBlueViolet'
          Nodes = <
            item
              Y = 0.309017002582550000
              Z = 0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Z = 0.309017002582550000
            end
            item
              X = 0.309017002582550000
              Y = 0.500000000000000000
            end>
        end
        object IcoTriangle14: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBrass'
          Nodes = <
            item
              Y = 0.309017002582550000
              Z = -0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Z = -0.309017002582550000
            end
            item
              X = -0.309017002582550000
              Y = 0.500000000000000000
            end>
        end
        object IcoTriangle15: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBrightGold'
          Nodes = <
            item
              Y = 0.309017002582550000
              Z = -0.500000000000000000
            end
            item
              X = 0.309017002582550000
              Y = 0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Z = -0.309017002582550000
            end>
        end
        object IcoTriangle16: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrCadetBlue'
          Nodes = <
            item
              Y = -0.309017002582550000
              Z = -0.500000000000000000
            end
            item
              X = -0.309017002582550000
              Y = -0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Z = -0.309017002582550000
            end>
        end
        object IcoTriangle17: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrMidnightBlue'
          Nodes = <
            item
              Y = -0.309017002582550000
              Z = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Z = -0.309017002582550000
            end
            item
              X = 0.309017002582550000
              Y = -0.500000000000000000
            end>
        end
        object IcoTriangle18: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrCoolCopper'
          Nodes = <
            item
              Y = -0.309017002582550000
              Z = 0.500000000000000000
            end
            item
              X = -0.500000000000000000
              Z = 0.309017002582550000
            end
            item
              X = -0.309017002582550000
              Y = -0.500000000000000000
            end>
        end
        object IcoTriangle19: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrCoral'
          Nodes = <
            item
              Y = -0.309017002582550000
              Z = 0.500000000000000000
            end
            item
              X = 0.309017002582550000
              Y = -0.500000000000000000
            end
            item
              X = 0.500000000000000000
              Z = 0.309017002582550000
            end>
        end
      end
      object Dodecahedron: TGLDodecahedron
        Material.FrontProperties.Diffuse.Color = {9A99593F14AE073FCDCCCC3D0000803F}
        Visible = False
      end
      object dcPolyDod: TGLDummyCube
        CubeSize = 1.000000000000000000
        object Pentagon0: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrBlue'
          Nodes = <
            item
              X = -0.485410183668136600
              Z = 0.185410201549530000
            end
            item
              X = -0.300000011920929000
              Y = -0.300000011920929000
              Z = 0.300000011920929000
            end
            item
              Y = -0.185410201549530000
              Z = 0.485410213470459000
            end
            item
              Y = 0.185410201549530000
              Z = 0.485410213470459000
            end
            item
              X = -0.300000011920929000
              Y = 0.300000011920929000
              Z = 0.300000011920929000
            end>
        end
        object Pentagon1: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrMidnightBlue'
          Nodes = <
            item
              X = -0.485410213470459000
              Z = -0.185410201549530000
            end
            item
              X = -0.300000011920929000
              Y = 0.300000011920929000
              Z = -0.300000011920929000
            end
            item
              Y = 0.185410201549530000
              Z = -0.485410213470459000
            end
            item
              Y = -0.185410201549530000
              Z = -0.485410213470459000
            end
            item
              X = -0.300000011920929000
              Y = -0.300000011920929000
              Z = -0.300000011920929000
            end>
        end
        object Pentagon2: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrViolet'
          Nodes = <
            item
              X = 0.485410213470459000
              Z = -0.185410201549530000
            end
            item
              X = 0.300000011920929000
              Y = -0.300000011920929000
              Z = -0.300000011920929000
            end
            item
              Y = -0.185410201549530000
              Z = -0.485410213470459000
            end
            item
              Y = 0.185410201549530000
              Z = -0.485410213470459000
            end
            item
              X = 0.300000011920929000
              Y = 0.300000011920929000
              Z = -0.300000011920929000
            end>
        end
        object Pentagon3: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrRed'
          Nodes = <
            item
              X = 0.485410213470459000
              Z = 0.185410201549530000
            end
            item
              X = 0.300000011920929000
              Y = 0.300000011920929000
              Z = 0.300000011920929000
            end
            item
              Y = 0.185410201549530000
              Z = 0.485410213470459000
            end
            item
              Y = -0.185410201549530000
              Z = 0.485410213470459000
            end
            item
              X = 0.300000011920929000
              Y = -0.300000011920929000
              Z = 0.300000011920929000
            end>
        end
        object Pentagon4: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrOrangeRed'
          Nodes = <
            item
              X = 0.185410201549530000
              Y = -0.485410213470459000
            end
            item
              X = 0.300000011920929000
              Y = -0.300000011920929000
              Z = -0.300000011920929000
            end
            item
              X = 0.485410213470459000
              Z = -0.185410201549530000
            end
            item
              X = 0.485410213470459000
              Z = 0.185410201549530000
            end
            item
              X = 0.300000011920929000
              Y = -0.300000011920929000
              Z = 0.300000011920929000
            end>
        end
        object Pentagon5: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrOrange'
          Nodes = <
            item
              X = -0.185410201549530000
              Y = -0.485410213470459000
            end
            item
              X = -0.300000011920929000
              Y = -0.300000011920929000
              Z = 0.300000011920929000
            end
            item
              X = -0.485410213470459000
              Z = 0.185410201549530000
            end
            item
              X = -0.485410213470459000
              Z = -0.185410201549530000
            end
            item
              X = -0.300000011920929000
              Y = -0.300000011920929000
              Z = -0.300000011920929000
            end>
        end
        object Pentagon6: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrOrange'
          Nodes = <
            item
              X = -0.185410201549530000
              Y = 0.485410213470459000
            end
            item
              X = -0.300000011920929000
              Y = 0.300000011920929000
              Z = -0.300000011920929000
            end
            item
              X = -0.485410213470459000
              Z = -0.185410201549530000
            end
            item
              X = -0.485410213470459000
              Z = 0.185410201549530000
            end
            item
              X = -0.300000011920929000
              Y = 0.300000011920929000
              Z = 0.300000011920929000
            end>
        end
        object Pentagon7: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrOldGold'
          Nodes = <
            item
              X = 0.185410201549530000
              Y = 0.485410213470459000
            end
            item
              X = 0.300000011920929000
              Y = 0.300000011920929000
              Z = 0.300000011920929000
            end
            item
              X = 0.485410213470459000
              Z = 0.185410201549530000
            end
            item
              X = 0.485410213470459000
              Z = -0.185410201549530000
            end
            item
              X = 0.300000011920929000
              Y = 0.300000011920929000
              Z = -0.300000011920929000
            end>
        end
        object Pentagon8: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrGreen'
          Nodes = <
            item
              Y = 0.185410201549530000
              Z = -0.485410213470459000
            end
            item
              X = -0.300000011920929000
              Y = 0.300000011920929000
              Z = -0.300000011920929000
            end
            item
              X = -0.185410201549530000
              Y = 0.485410213470459000
            end
            item
              X = 0.185410201549530000
              Y = 0.485410213470459000
            end
            item
              X = 0.300000011920929000
              Y = 0.300000011920929000
              Z = -0.300000011920929000
            end>
        end
        object Pentagon9: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrForestGreen'
          Nodes = <
            item
              Y = -0.185410201549530000
              Z = -0.485410213470459000
            end
            item
              X = 0.300000011920929000
              Y = -0.300000011920929000
              Z = -0.300000011920929000
            end
            item
              X = 0.185410201549530000
              Y = -0.485410213470459000
            end
            item
              X = -0.185410201549530000
              Y = -0.485410213470459000
            end
            item
              X = -0.300000011920929000
              Y = -0.300000011920929000
              Z = -0.300000011920929000
            end>
        end
        object Pentagon10: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrSkyBlue'
          Nodes = <
            item
              Y = -0.185410201549530000
              Z = 0.485410213470459000
            end
            item
              X = -0.300000011920929000
              Y = -0.300000011920929000
              Z = 0.300000011920929000
            end
            item
              X = -0.185410201549530000
              Y = -0.485410213470459000
            end
            item
              X = 0.185410201549530000
              Y = -0.485410213470459000
            end
            item
              X = 0.300000011920929000
              Y = -0.300000011920929000
              Z = 0.300000011920929000
            end>
        end
        object Pentagon11: TGLPolygon
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrSteelBlue'
          Nodes = <
            item
              Y = 0.185410201549530000
              Z = 0.485410213470459000
            end
            item
              X = 0.300000011920929000
              Y = 0.300000011920929000
              Z = 0.300000011920929000
            end
            item
              X = 0.185410201549530000
              Y = 0.485410213470459000
            end
            item
              X = -0.185410201549530000
              Y = 0.485410213470459000
            end
            item
              X = -0.300000011920929000
              Y = 0.300000011920929000
              Z = 0.300000011920929000
            end>
        end
      end
      object dcConvexhull: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object meshConvexhull: TGLMesh
          Material.MaterialLibrary = GLMatLibColors
          Material.LibMaterialName = 'clrRed'
          ShowAxes = True
          Mode = mmTriangleStrip
        end
      end
    end
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Polyhedra - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 90
    Top = 176
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 198
    Top = 24
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 924
    Top = 70
  end
  object GLMatLibTextures: TGLMaterialLibrary
    Materials = <
      item
        Name = 'a'
        Tag = 0
        Material.BackProperties.Ambient.Color = {00000000000000000000803F0000803F}
        Material.BackProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.BackProperties.Emission.Color = {00000000000000000000803F0000803F}
        Material.BackProperties.Specular.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Ambient.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Emission.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Specular.Color = {00000000000000000000803F0000803F}
      end
      item
        Name = 'as'
        Tag = 0
        Material.BackProperties.Ambient.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.BackProperties.Diffuse.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.BackProperties.Emission.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.BackProperties.Specular.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.FrontProperties.Ambient.Color = {CFBC3C3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Diffuse.Color = {CFBC3C3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Emission.Color = {CFBC3C3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Specular.Color = {CFBC3C3ECFBC3C3EA19E9E3E0000803F}
      end
      item
        Name = 'b'
        Tag = 0
        Material.BackProperties.Ambient.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.BackProperties.Diffuse.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.BackProperties.Emission.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.BackProperties.Specular.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Ambient.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Diffuse.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Emission.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Specular.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
      end
      item
        Name = 'c'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F00000000000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F00000000000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Ambient.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Emission.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Specular.Color = {0000803F00000000000000000000803F}
      end
      item
        Name = 'cs'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F0000803E000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F0000803E000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F0000803E000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F0000803E000000000000803F}
        Material.FrontProperties.Ambient.Color = {0000803F0000803E000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803E000000000000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000803E000000000000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803E000000000000803F}
      end
      item
        Name = 'd'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F0000003F000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F0000003F000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F0000003F000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F0000003F000000000000803F}
        Material.FrontProperties.Ambient.Color = {CDCC0C3FD7A3F03E295C0F3E0000803F}
        Material.FrontProperties.Diffuse.Color = {CDCC0C3FD7A3F03E295C0F3E0000803F}
        Material.FrontProperties.Emission.Color = {CDCC0C3FD7A3F03E295C0F3E0000803F}
        Material.FrontProperties.Specular.Color = {CDCC0C3FD7A3F03E295C0F3E0000803F}
      end
      item
        Name = 'ds'
        Tag = 0
        Material.BackProperties.Ambient.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.BackProperties.Diffuse.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.BackProperties.Emission.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.BackProperties.Specular.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.FrontProperties.Ambient.Color = {0000803F0000003F000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000003F000000000000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000003F000000000000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000003F000000000000803F}
      end
      item
        Name = 'e'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F0000803F000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F0000803F000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F0000803F000000000000803F}
        Material.FrontProperties.Ambient.Color = {295C4F3F8FC2353F1F856B3E0000803F}
        Material.FrontProperties.Diffuse.Color = {295C4F3F8FC2353F1F856B3E0000803F}
        Material.FrontProperties.Emission.Color = {295C4F3F8FC2353F1F856B3E0000803F}
        Material.FrontProperties.Specular.Color = {295C4F3F8FC2353F1F856B3E0000803F}
      end
      item
        Name = 'f'
        Tag = 0
        Material.BackProperties.Ambient.Color = {000000000000003F000000000000803F}
        Material.BackProperties.Diffuse.Color = {000000000000003F000000000000803F}
        Material.BackProperties.Emission.Color = {000000000000003F000000000000803F}
        Material.BackProperties.Specular.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Ambient.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Diffuse.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Emission.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Specular.Color = {000000000000003F000000000000803F}
      end
      item
        Name = 'fs'
        Tag = 0
        Material.BackProperties.Ambient.Color = {000000000000003F0000003F0000803F}
        Material.BackProperties.Diffuse.Color = {000000000000003F0000003F0000803F}
        Material.BackProperties.Emission.Color = {000000000000003F0000003F0000803F}
        Material.BackProperties.Specular.Color = {000000000000003F0000003F0000803F}
        Material.FrontProperties.Ambient.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.FrontProperties.Diffuse.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.FrontProperties.Emission.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.FrontProperties.Specular.Color = {938C0C3E938E0E3F938C0C3E0000803F}
      end
      item
        Name = 'g'
        Tag = 0
        Material.BackProperties.Ambient.Color = {000000000000803F0000803F0000803F}
        Material.BackProperties.Diffuse.Color = {000000000000803F0000803F0000803F}
        Material.BackProperties.Emission.Color = {000000000000803F0000803F0000803F}
        Material.BackProperties.Specular.Color = {000000000000803F0000803F0000803F}
        Material.FrontProperties.Ambient.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.FrontProperties.Diffuse.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.FrontProperties.Specular.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
      end
      item
        Name = 'gs'
        Tag = 0
        Material.BackProperties.Ambient.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.BackProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.BackProperties.Emission.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.BackProperties.Specular.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.FrontProperties.Ambient.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.FrontProperties.Emission.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.FrontProperties.Specular.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
      end>
    Left = 393
    Top = 174
  end
  object GLMatLibColors: TGLMaterialLibrary
    Materials = <
      item
        Name = 'clrBlue'
        Tag = 0
        Material.BackProperties.Ambient.Color = {00000000000000000000803F0000803F}
        Material.BackProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.BackProperties.Emission.Color = {00000000000000000000803F0000803F}
        Material.BackProperties.Specular.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Ambient.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Specular.Color = {00000000000000000000803F0000803F}
      end
      item
        Name = 'clrMidnightBlue'
        Tag = 0
        Material.BackProperties.Ambient.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.BackProperties.Diffuse.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.BackProperties.Emission.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.BackProperties.Specular.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.FrontProperties.Ambient.Color = {CFBC3C3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Diffuse.Color = {CFBC3C3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Emission.Color = {CFBC3C3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Specular.Color = {CFBC3C3ECFBC3C3EA19E9E3E0000803F}
      end
      item
        Name = 'clrViolet'
        Tag = 0
        Material.BackProperties.Ambient.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.BackProperties.Diffuse.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.BackProperties.Emission.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.BackProperties.Specular.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Ambient.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Diffuse.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Specular.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
      end
      item
        Name = 'clrRed'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F00000000000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F00000000000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Ambient.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Specular.Color = {0000803F00000000000000000000803F}
      end
      item
        Name = 'clrOrangeRed'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F0000803E000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F0000803E000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F0000803E000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F0000803E000000000000803F}
        Material.FrontProperties.Ambient.Color = {0000803F0000803E000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803E000000000000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803E000000000000803F}
      end
      item
        Name = 'clrBronze'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F0000003F000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F0000003F000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F0000003F000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F0000003F000000000000803F}
        Material.FrontProperties.Ambient.Color = {CDCC0C3FD7A3F03E295C0F3E0000803F}
        Material.FrontProperties.Diffuse.Color = {CDCC0C3FD7A3F03E295C0F3E0000803F}
        Material.FrontProperties.Emission.Color = {CDCC0C3FD7A3F03E295C0F3E0000803F}
        Material.FrontProperties.Specular.Color = {CDCC0C3FD7A3F03E295C0F3E0000803F}
      end
      item
        Name = 'clrOrange'
        Tag = 0
        Material.BackProperties.Ambient.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.BackProperties.Diffuse.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.BackProperties.Emission.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.BackProperties.Specular.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.FrontProperties.Ambient.Color = {0000803F0000003F000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000003F000000000000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000003F000000000000803F}
      end
      item
        Name = 'clrOldGold'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F0000803F000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F0000803F000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F0000803F000000000000803F}
        Material.FrontProperties.Ambient.Color = {295C4F3F8FC2353F1F856B3E0000803F}
        Material.FrontProperties.Diffuse.Color = {295C4F3F8FC2353F1F856B3E0000803F}
        Material.FrontProperties.Emission.Color = {295C4F3F8FC2353F1F856B3E0000803F}
        Material.FrontProperties.Specular.Color = {295C4F3F8FC2353F1F856B3E0000803F}
      end
      item
        Name = 'clrGreen'
        Tag = 0
        Material.BackProperties.Ambient.Color = {000000000000003F000000000000803F}
        Material.BackProperties.Diffuse.Color = {000000000000003F000000000000803F}
        Material.BackProperties.Emission.Color = {000000000000003F000000000000803F}
        Material.BackProperties.Specular.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Ambient.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Diffuse.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Emission.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Specular.Color = {000000000000003F000000000000803F}
      end
      item
        Name = 'clrForestGreen'
        Tag = 0
        Material.BackProperties.Ambient.Color = {000000000000003F0000003F0000803F}
        Material.BackProperties.Diffuse.Color = {000000000000003F0000003F0000803F}
        Material.BackProperties.Emission.Color = {000000000000003F0000003F0000803F}
        Material.BackProperties.Specular.Color = {000000000000003F0000003F0000803F}
        Material.FrontProperties.Ambient.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.FrontProperties.Diffuse.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.FrontProperties.Emission.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.FrontProperties.Specular.Color = {938C0C3E938E0E3F938C0C3E0000803F}
      end
      item
        Name = 'clrSkyBlue'
        Tag = 0
        Material.BackProperties.Ambient.Color = {000000000000803F0000803F0000803F}
        Material.BackProperties.Diffuse.Color = {000000000000803F0000803F0000803F}
        Material.BackProperties.Emission.Color = {000000000000803F0000803F0000803F}
        Material.BackProperties.Specular.Color = {000000000000803F0000803F0000803F}
        Material.FrontProperties.Ambient.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.FrontProperties.Diffuse.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.FrontProperties.Specular.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
      end
      item
        Name = 'clrSteelBlue'
        Tag = 0
        Material.BackProperties.Ambient.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.BackProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.BackProperties.Emission.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.BackProperties.Specular.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.FrontProperties.Ambient.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.FrontProperties.Emission.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.FrontProperties.Specular.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
      end
      item
        Name = 'clrAquamarine'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {EBE0E03EE4DB5B3F9A93133F0000803F}
      end
      item
        Name = 'clrBakersChoc'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {EC51B83ECDCC4C3EEC51B83D0000803F}
      end
      item
        Name = 'clrBlueViolet'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {029F1F3FBEBEBE3E999F1F3F0000803F}
      end
      item
        Name = 'clrBrass'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {8FC2353F6666263FB81E853E0000803F}
      end
      item
        Name = 'clrBrightGold'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {9A99593F9A99593FCDCCCC3D0000803F}
      end
      item
        Name = 'clrCadetBlue'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {BEBEBE3E999F1F3F999F1F3F0000803F}
      end
      item
        Name = 'clrCoolCopper'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {9A99593F14AE073FCDCCCC3D0000803F}
      end
      item
        Name = 'clrCoral'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803FF8FEFE3E000000000000803F}
      end>
    Left = 393
    Top = 20
  end
end
