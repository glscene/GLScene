object FormSprites: TFormSprites
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Sprites'
  ClientHeight = 814
  ClientWidth = 1138
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 168
  TextHeight = 30
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 226
    Height = 814
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 0
    object tvBench: TTreeView
      Left = 1
      Top = 1
      Width = 224
      Height = 812
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Indent = 33
      TabOrder = 0
      Items.NodeData = {
        070500000009540054007200650065004E006F00640065003500000000000000
        00000000FFFFFFFFFFFFFFFF00000000000000000000000000010B4300610074
        0065007200500069006C006C00610072000000410000000000000000000000FF
        FFFFFFFFFFFFFF00000000000000000000000000011146006F0075006E007400
        610069006E005000610072007400690063006C006500730000002F0000000000
        000000000000FFFFFFFFFFFFFFFF00000000000000000000000000010849006D
        0070006F0073007400650072000000350000000000000000000000FFFFFFFFFF
        FFFFFF00000000000000000000000000010B49006D0070006F00730074006500
        72007300460046000000310000000000000000000000FFFFFFFFFFFFFFFF0000
        000000000000000000000001095000610072007400690063006C0065007300}
    end
  end
  object PanelSprites: TPanel
    Left = 226
    Top = 0
    Width = 912
    Height = 814
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 406
    ExplicitTop = 280
    ExplicitWidth = 324
    ExplicitHeight = 72
  end
end
