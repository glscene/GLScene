object Form1: TForm1
  Left = 198
  Top = 140
  BorderWidth = 5
  Caption = 'Splines'
  ClientHeight = 385
  ClientWidth = 503
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
    Width = 503
    Height = 385
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 165.200805664062500000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object Lines1: TGLLines
      AntiAliased = True
      LineWidth = 6.000000000000000000
      Nodes = <
        item
          X = -1.000000000000000000
          Y = -1.000000000000000000
        end
        item
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 1.000000000000000000
          Y = 1.000000000000000000
          Color.Color = {0000803F00000000000000000000803F}
        end>
      NodesAspect = lnaCube
      NodeSize = 0.500000000000000000
      SplineMode = lsmCubicSpline
      Options = [loUseNodeColorForLines]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 25.000000000000000000
      TargetObject = Lines1
      CameraStyle = csOrthogonal
      Position.Coordinates = {00000000000000000000A0400000803F}
      Left = 248
      Top = 152
    end
  end
end
