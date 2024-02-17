object FormCanvas: TFormCanvas
  Left = 151
  Top = 105
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsSizeToolWin
  Caption = 'GLCanvas vs GDI'
  ClientHeight = 592
  ClientWidth = 950
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 168
  TextHeight = 24
  object PaintBox: TPaintBox
    Left = 476
    Top = 114
    Width = 448
    Height = 448
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
  end
  object lbGLCanvas: TLabel
    Left = 14
    Top = 81
    Width = 128
    Height = 24
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'GLCanvas: N/A'
  end
  object lbGDI: TLabel
    Left = 476
    Top = 79
    Width = 73
    Height = 24
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'GDI: N/A'
  end
  object Bevel1: TBevel
    Left = 467
    Top = 84
    Width = 4
    Height = 478
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
  end
  object BULines: TButton
    Left = 14
    Top = 14
    Width = 128
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '20k Lines'
    TabOrder = 0
    OnClick = BULinesClick
  end
  object BUEllipses: TButton
    Left = 154
    Top = 14
    Width = 128
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '20k Ellipses'
    TabOrder = 1
    OnClick = BUEllipsesClick
  end
  object GLSceneViewer: TGLSceneViewer
    Left = 18
    Top = 114
    Width = 448
    Height = 448
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.Lighting = False
    FieldOfView = 154.834075927734400000
    PenAsTouch = False
    TabOrder = 2
  end
  object RBPenWidth1: TRadioButton
    Left = 756
    Top = 14
    Width = 156
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Pen Width = 1'
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object RBPenWidth2: TRadioButton
    Left = 756
    Top = 42
    Width = 156
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Pen Width = 2'
    TabOrder = 4
  end
  object BUArc: TButton
    Left = 294
    Top = 61
    Width = 128
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '20k Arcs'
    TabOrder = 8
    OnClick = BUArcClick
  end
  object BUPoints: TButton
    Left = 434
    Top = 14
    Width = 128
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '200k Points'
    TabOrder = 5
    OnClick = BUPointsClick
  end
  object BURects: TButton
    Left = 294
    Top = 14
    Width = 128
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '20k Rects'
    TabOrder = 6
    OnClick = BURectsClick
  end
  object BUTextOut: TButton
    Left = 572
    Top = 14
    Width = 128
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
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
