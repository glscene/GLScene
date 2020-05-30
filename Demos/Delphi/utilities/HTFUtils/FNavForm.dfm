object NavForm: TNavForm
  Left = 276
  Top = 328
  BorderStyle = bsDialog
  Caption = 'NavForm'
  ClientHeight = 192
  ClientWidth = 192
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage32
    Left = 0
    Top = 0
    Width = 192
    Height = 192
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smScale
    TabOrder = 0
    OnMouseDown = ImageMouseDown
  end
end
