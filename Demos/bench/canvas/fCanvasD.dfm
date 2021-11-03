object FormCanvas: TFormCanvas
  Left = 151
  Top = 105
  BorderStyle = bsDialog
  Caption = 'GLCanvas vs GDI'
  ClientHeight = 411
  ClientWidth = 671
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 120
  TextHeight = 16
  object PaintBox: TPaintBox
    Left = 340
    Top = 81
    Width = 320
    Height = 320
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
  end
  object lbGLCanvas: TLabel
    Left = 10
    Top = 58
    Width = 92
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'GLCanvas: N/A'
  end
  object lbGDI: TLabel
    Left = 340
    Top = 56
    Width = 52
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'GDI: N/A'
  end
  object Bevel1: TBevel
    Left = 334
    Top = 60
    Width = 2
    Height = 341
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
  end
  object BULines: TButton
    Left = 10
    Top = 10
    Width = 91
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '20k Lines'
    TabOrder = 0
    OnClick = BULinesClick
  end
  object BUEllipses: TButton
    Left = 110
    Top = 10
    Width = 91
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '20k Ellipses'
    TabOrder = 1
    OnClick = BUEllipsesClick
  end
  object GLSceneViewer: TGLSceneViewer
    Left = 13
    Top = 81
    Width = 320
    Height = 320
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.Lighting = False
    FieldOfView = 145.291946411132800000
    PenAsTouch = False
    TabOrder = 2
  end
  object RBPenWidth1: TRadioButton
    Left = 540
    Top = 10
    Width = 111
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Pen Width = 1'
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object RBPenWidth2: TRadioButton
    Left = 540
    Top = 30
    Width = 111
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Pen Width = 2'
    TabOrder = 4
  end
  object BUArc: TButton
    Left = 210
    Top = 44
    Width = 91
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '20k Arcs'
    TabOrder = 8
    OnClick = BUArcClick
  end
  object BUPoints: TButton
    Left = 310
    Top = 10
    Width = 91
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '200k Points'
    TabOrder = 5
    OnClick = BUPointsClick
  end
  object BURects: TButton
    Left = 210
    Top = 10
    Width = 91
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '20k Rects'
    TabOrder = 6
    OnClick = BURectsClick
  end
  object BUTextOut: TButton
    Left = 409
    Top = 10
    Width = 91
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '20k TextOut'
    TabOrder = 7
    OnClick = BUTextOutClick
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 72
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
    end
  end
  object WindowsBitmapFont: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Left = 96
    Top = 72
  end
end
