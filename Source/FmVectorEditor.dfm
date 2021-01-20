object GLVectorEditorForm: TGLVectorEditorForm
  Left = 411
  Top = 108
  BorderStyle = bsDialog
  Caption = 'XYZ editor'
  ClientHeight = 171
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 72
    Width = 39
    Height = 16
    Caption = 'X axis'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 104
    Width = 38
    Height = 16
    Caption = 'Y axis'
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 136
    Width = 37
    Height = 16
    Caption = 'Z axis'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object IMx: TImage
    Left = 152
    Top = 72
    Width = 16
    Height = 16
    Picture.Data = {
      07544269746D617076010000424D760100000000000076000000280000002000
      000010000000010004000000000000010000130B0000130B0000100000000000
      0000000000000000800000800000008080008000000080008000808000007F7F
      7F00BFBFBF000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF003333333333333333333333FFFFF3333333333999993333333333F77777FF
      F33333399999999933333337777FF377FF3333993370739993333377FF373F37
      7FF3399993000339993337777F777F3377F3393999707333993337F777373333
      37FF993399933333399377F3777FF333377F993339903333399377F33737FF33
      377F993333707333399377F333377FF3377F993333101933399377F333777FFF
      377F993333000993399377FF3377737FF7733993330009993933373FF3777377
      F7F339993300039999333773FF777F777733339993707339933333773FF7FFF7
      7333333999999999333333377733377733333333399999333333333337777733
      3333}
    Transparent = True
    Visible = False
  end
  object IMy: TImage
    Left = 152
    Top = 104
    Width = 16
    Height = 16
    Picture.Data = {
      07544269746D617076010000424D760100000000000076000000280000002000
      000010000000010004000000000000010000130B0000130B0000100000000000
      0000000000000000800000800000008080008000000080008000808000007F7F
      7F00BFBFBF000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF003333333333333333333333FFFFF3333333333999993333333333F77777FF
      F33333399999999933333337777FF377FF3333993370739993333377FF373F37
      7FF3399993000339993337777F777F3377F3393999707333993337F777373333
      37FF993399933333399377F3777FF333377F993339903333399377F33737FF33
      377F993333707333399377F333377FF3377F993333101933399377F333777FFF
      377F993333000993399377FF3377737FF7733993330009993933373FF3777377
      F7F339993300039999333773FF777F777733339993707339933333773FF7FFF7
      7333333999999999333333377733377733333333399999333333333337777733
      3333}
    Transparent = True
    Visible = False
  end
  object IMz: TImage
    Left = 152
    Top = 136
    Width = 16
    Height = 16
    Picture.Data = {
      07544269746D617076010000424D760100000000000076000000280000002000
      000010000000010004000000000000010000130B0000130B0000100000000000
      0000000000000000800000800000008080008000000080008000808000007F7F
      7F00BFBFBF000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF003333333333333333333333FFFFF3333333333999993333333333F77777FF
      F33333399999999933333337777FF377FF3333993370739993333377FF373F37
      7FF3399993000339993337777F777F3377F3393999707333993337F777373333
      37FF993399933333399377F3777FF333377F993339903333399377F33737FF33
      377F993333707333399377F333377FF3377F993333101933399377F333777FFF
      377F993333000993399377FF3377737FF7733993330009993933373FF3777377
      F7F339993300039999333773FF777F777733339993707339933333773FF7FFF7
      7333333999999999333333377733377733333333399999333333333337777733
      3333}
    Transparent = True
    Visible = False
  end
  object SpeedButton1: TSpeedButton
    Left = 8
    Top = 8
    Width = 33
    Height = 23
    Caption = '+ X'
    Flat = True
    Transparent = False
    OnClick = TBxClick
  end
  object SBmX: TSpeedButton
    Left = 8
    Top = 32
    Width = 33
    Height = 23
    Caption = '- X'
    Flat = True
    Transparent = False
    OnClick = SBmXClick
  end
  object SpeedButton3: TSpeedButton
    Left = 42
    Top = 8
    Width = 33
    Height = 23
    Caption = '+ Y'
    Flat = True
    Transparent = False
    OnClick = TByClick
  end
  object SBmY: TSpeedButton
    Left = 42
    Top = 32
    Width = 33
    Height = 23
    Caption = '- Y'
    Flat = True
    Transparent = False
    OnClick = SBmYClick
  end
  object SpeedButton5: TSpeedButton
    Left = 76
    Top = 8
    Width = 33
    Height = 23
    Caption = '+ Z'
    Flat = True
    Transparent = False
    OnClick = TBzClick
  end
  object SBmZ: TSpeedButton
    Left = 76
    Top = 32
    Width = 33
    Height = 23
    Caption = '- Z'
    Flat = True
    Transparent = False
    OnClick = SBmZClick
  end
  object SpeedButton7: TSpeedButton
    Left = 130
    Top = 8
    Width = 47
    Height = 23
    Caption = '0; 0; 0'
    Flat = True
    Transparent = False
    OnClick = TBnullClick
  end
  object SBUnit: TSpeedButton
    Left = 130
    Top = 32
    Width = 47
    Height = 23
    Caption = '1; 1; 1'
    Flat = True
    Transparent = False
    OnClick = SBUnitClick
  end
  object SpeedButton9: TSpeedButton
    Left = 194
    Top = 8
    Width = 63
    Height = 23
    Caption = 'Normalize'
    Flat = True
    Transparent = False
    OnClick = SpeedButton9Click
  end
  object Bevel1: TBevel
    Left = 8
    Top = 62
    Width = 249
    Height = 5
    Shape = bsTopLine
  end
  object SBInvert: TSpeedButton
    Left = 194
    Top = 32
    Width = 63
    Height = 23
    Caption = 'Invert'
    Flat = True
    Transparent = False
    OnClick = SBInvertClick
  end
  object EDx: TEdit
    Left = 56
    Top = 71
    Width = 89
    Height = 21
    TabOrder = 0
    Text = '0'
    OnChange = EDxChange
  end
  object EDy: TEdit
    Left = 56
    Top = 103
    Width = 89
    Height = 21
    TabOrder = 1
    Text = '0'
    OnChange = EDyChange
  end
  object EDz: TEdit
    Left = 56
    Top = 135
    Width = 89
    Height = 21
    TabOrder = 2
    Text = '0'
    OnChange = EDzChange
  end
  object BBok: TBitBtn
    Left = 184
    Top = 72
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 3
  end
  object BBcancel: TBitBtn
    Left = 184
    Top = 104
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 4
  end
end
