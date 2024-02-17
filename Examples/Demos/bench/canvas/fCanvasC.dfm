object FormCanvas: TFormCanvas
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Canvas'
  ClientHeight = 642
  ClientWidth = 991
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  PixelsPerInch = 168
  TextHeight = 23
  object Bevel1: TBevel
    Left = 473
    Top = 117
    Width = 3
    Height = 478
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
  end
  object PaintBox: TPaintBox
    Left = 487
    Top = 124
    Width = 448
    Height = 448
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
  end
  object GLSceneViewer: TGLSceneViewer
    Left = 14
    Top = 124
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
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 991
    Height = 114
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    TabOrder = 1
    object LAGLCanvas: TLabel
      Left = 14
      Top = 84
      Width = 125
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'GLCanvas: N/A'
    end
    object LAGDI: TLabel
      Left = 476
      Top = 84
      Width = 77
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'GDI: N/A'
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
      TabOrder = 2
      OnClick = BURectsClick
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
      TabOrder = 3
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
      TabOrder = 4
      OnClick = BUPointsClick
    end
    object BUTextOut: TButton
      Left = 574
      Top = 14
      Width = 128
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '20k TextOut'
      TabOrder = 5
      OnClick = BUTextOutClick
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
      TabOrder = 6
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
      TabOrder = 7
    end
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 88
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
    Left = 136
    Top = 88
  end
end
