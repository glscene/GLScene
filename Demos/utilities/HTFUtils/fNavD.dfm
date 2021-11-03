object NavForm: TNavForm
  Left = 276
  Top = 328
  BorderStyle = bsDialog
  Caption = 'Nav Form'
  ClientHeight = 240
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDesktopCenter
  PixelsPerInch = 120
  TextHeight = 16
  object Image: TImage32
    Left = 0
    Top = 0
    Width = 240
    Height = 240
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smScale
    TabOrder = 0
    OnMouseDown = ImageMouseDown
  end
end
