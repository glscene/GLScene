object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Nuts and Bolts'
  ClientHeight = 413
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 577
    Height = 413
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 152.777770996093800000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Bolt: TGLDummyCube
        Position.Coordinates = {000000009A9999BE0000803F0000803F}
        CubeSize = 1.000000000000000000
        object RSBoltHead: TGLRevolutionSolid
          Position.Coordinates = {000000001F856B3F000000000000803F}
          Scale.Coordinates = {9A99193E9A99193E9A99193E00000000}
          Nodes = <
            item
              Y = 1.500000000000000000
            end
            item
              X = 5.900000095367432000
              Y = 1.500000000000000000
            end
            item
              X = 6.000000000000000000
              Y = 1.399999976158142000
            end
            item
              X = 6.000000000000000000
              Y = -1.399999976158142000
            end
            item
              X = 5.900000095367432000
              Y = -1.500000000000000000
            end
            item
              Y = -1.500000000000000000
            end>
          Parts = [rspOutside, rspStartPolygon]
          Slices = 6
          Normals = nsSmooth
        end
        object CYBoltShaft: TGLCylinder
          BottomRadius = 0.500000000000000000
          Height = 1.549999952316284000
          TopRadius = 0.500000000000000000
        end
        object RSBoltThreads: TGLRevolutionSolid
          Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
          Nodes = <
            item
              X = 5.000000000000000000
              Y = 0.500000000000000000
            end
            item
              X = 5.500000000000000000
            end
            item
              X = 5.000000000000000000
              Y = -0.500000000000000000
            end>
          StartAngle = -1800.000000000000000000
          StopAngle = 1800.000000000000000000
          YOffsetPerTurn = 1.500000000000000000
          Slices = 24
        end
      end
      object Nut: TGLDummyCube
        Direction.Coordinates = {F304353FF30435BF33BF0D2800000000}
        Position.Coordinates = {0000000000000000000080BF0000803F}
        Up.Coordinates = {F404353FF304353F64C084B300000000}
        CubeSize = 1.000000000000000000
        object RSNutThreads: TGLRevolutionSolid
          Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
          Nodes = <
            item
              X = 5.500000000000000000
              Y = 0.500000000000000000
            end
            item
              X = 5.000000000000000000
            end
            item
              X = 5.500000000000000000
              Y = -0.500000000000000000
            end>
          Parts = [rspInside, rspStartPolygon, rspStopPolygon]
          StartAngle = -475.000000000000000000
          StopAngle = 460.000000000000000000
          YOffsetPerTurn = 1.450000047683716000
          Slices = 24
        end
        object RSNutPans: TGLRevolutionSolid
          Scale.Coordinates = {9A99193E9A99193E9A99193E00000000}
          Nodes = <
            item
              X = 4.500000000000000000
              Y = 1.500000000000000000
            end
            item
              X = 5.900000095367432000
              Y = 1.500000000000000000
            end
            item
              X = 6.000000000000000000
              Y = 1.399999976158142000
            end
            item
              X = 6.000000000000000000
              Y = -1.399999976158142000
            end
            item
              X = 5.900000095367432000
              Y = -1.500000000000000000
            end
            item
              X = 4.500000000000000000
              Y = -1.500000000000000000
            end>
          Slices = 6
        end
        object Annulus1: TGLAnnulus
          BottomRadius = 0.699999988079071000
          Height = 0.449999988079071000
          BottomInnerRadius = 0.550000011920929000
          TopInnerRadius = 0.550000011920929000
          TopRadius = 0.699999988079071000
          Parts = [anInnerSides, anBottom, anTop]
        end
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000804000000000000000000000803F}
      Left = 208
      Top = 136
    end
  end
end
