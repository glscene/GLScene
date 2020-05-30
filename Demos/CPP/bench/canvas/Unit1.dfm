object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Canvas'
  ClientHeight = 367
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 270
    Top = 67
    Width = 2
    Height = 273
  end
  object PaintBox: TPaintBox
    Left = 278
    Top = 71
    Width = 256
    Height = 256
  end
  object GLSceneViewer: TGLSceneViewer
    Left = 8
    Top = 71
    Width = 256
    Height = 256
    Camera = GLCamera1
    Buffer.Lighting = False
    FieldOfView = 137.326278686523400000
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 558
    Height = 65
    Align = alTop
    TabOrder = 1
    object LAGLCanvas: TLabel
      Left = 8
      Top = 48
      Width = 73
      Height = 13
      Caption = 'GLCanvas: N/A'
    end
    object LAGDI: TLabel
      Left = 272
      Top = 48
      Width = 43
      Height = 13
      Caption = 'GDI: N/A'
    end
    object BULines: TButton
      Left = 8
      Top = 8
      Width = 73
      Height = 25
      Caption = '20k Lines'
      TabOrder = 0
      OnClick = BULinesClick
    end
    object BUEllipses: TButton
      Left = 88
      Top = 8
      Width = 73
      Height = 25
      Caption = '20k Ellipses'
      TabOrder = 1
      OnClick = BUEllipsesClick
    end
    object BURects: TButton
      Left = 168
      Top = 8
      Width = 73
      Height = 25
      Caption = '20k Rects'
      TabOrder = 2
      OnClick = BURectsClick
    end
    object BUArc: TButton
      Left = 168
      Top = 35
      Width = 73
      Height = 25
      Caption = '20k Arcs'
      TabOrder = 3
      OnClick = BUArcClick
    end
    object BUPoints: TButton
      Left = 248
      Top = 8
      Width = 73
      Height = 25
      Caption = '200k Points'
      TabOrder = 4
      OnClick = BUPointsClick
    end
    object BUTextOut: TButton
      Left = 328
      Top = 8
      Width = 73
      Height = 25
      Caption = '20k TextOut'
      TabOrder = 5
      OnClick = BUTextOutClick
    end
    object RBPenWidth1: TRadioButton
      Left = 432
      Top = 8
      Width = 89
      Height = 17
      Caption = 'Pen Width = 1'
      Checked = True
      TabOrder = 6
      TabStop = True
    end
    object RBPenWidth2: TRadioButton
      Left = 432
      Top = 24
      Width = 89
      Height = 17
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
