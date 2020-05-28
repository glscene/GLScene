object Form1: TForm1
  Left = 217
  Top = 127
  Caption = 'Splines'
  ClientHeight = 320
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 326
    Height = 320
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 162.238677978515600000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLLines1: TGLLines
      LineWidth = 6.000000000000000000
      Nodes = <
        item
          X = -1.000000000000000000
          Y = -1.000000000000000000
        end
        item
          Color.Color = {0000803F000000000000803F0000803F}
        end
        item
          X = 1.000000000000000000
          Y = 1.000000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end>
      NodesAspect = lnaCube
      NodeSize = 0.500000000000000000
      SplineMode = lsmCubicSpline
      Options = [loUseNodeColorForLines]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 25.000000000000000000
      TargetObject = GLLines1
      CameraStyle = csOrthogonal
      Position.Coordinates = {00000000000000000000A0400000803F}
    end
  end
end
