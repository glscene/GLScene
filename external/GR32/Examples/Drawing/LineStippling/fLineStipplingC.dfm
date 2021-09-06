object FormLineStipplingC: TFormLineStipplingC
  Left = 0
  Top = 0
  Caption = 'LineStippling Example'
  ClientHeight = 220
  ClientWidth = 261
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage32
    Left = 8
    Top = 8
    Width = 200
    Height = 200
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
  object ScrollBar: TScrollBar
    Left = 224
    Top = 8
    Width = 16
    Height = 201
    Kind = sbVertical
    Min = 1
    PageSize = 0
    Position = 50
    TabOrder = 1
    OnChange = ScrollBarChange
  end
end
