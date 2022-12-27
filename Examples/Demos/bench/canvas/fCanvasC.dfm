object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Canvas'
  ClientHeight = 459
  ClientWidth = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  PixelsPerInch = 120
  TextHeight = 17
  object Bevel1: TBevel
    Left = 338
    Top = 84
    Width = 2
    Height = 341
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
  end
  object PaintBox: TPaintBox
    Left = 348
    Top = 89
    Width = 320
    Height = 320
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
  end
  object GLSceneViewer: TGLSceneViewer
    Left = 10
    Top = 89
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
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 698
    Height = 81
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    TabOrder = 1
    object LAGLCanvas: TLabel
      Left = 10
      Top = 60
      Width = 92
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'GLCanvas: N/A'
    end
    object LAGDI: TLabel
      Left = 340
      Top = 60
      Width = 54
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'GDI: N/A'
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
      TabOrder = 2
      OnClick = BURectsClick
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
      TabOrder = 3
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
      TabOrder = 4
      OnClick = BUPointsClick
    end
    object BUTextOut: TButton
      Left = 410
      Top = 10
      Width = 91
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '20k TextOut'
      TabOrder = 5
      OnClick = BUTextOutClick
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
      TabOrder = 6
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
