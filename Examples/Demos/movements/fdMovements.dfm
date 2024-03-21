object FormMovements: TFormMovements
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Movements'
  ClientHeight = 814
  ClientWidth = 1138
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
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
    object tvMovements: TTreeView
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
      OnClick = tvMovementsClick
      Items.NodeData = {
        070900000009540054007200650065004E006F00640065002B00000000000000
        00000000FFFFFFFFFFFFFFFF00000000000000000000000000010663006F006C
        0075006D006E0000002B0000000000000000000000FFFFFFFFFFFFFFFF000000
        0000000000000000000001066500760065006E007400730000002F0000000000
        000000000000FFFFFFFFFFFFFFFF000000000000000000000000000108680069
        0065007200610072006300680000002B0000000000000000000000FFFFFFFFFF
        FFFFFF0000000000000000000000000001066D0061006E00750061006C000000
        2D0000000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000
        0001076F0062006A006D006F007600650000002D0000000000000000000000FF
        FFFFFFFFFFFFFF00000000000000000000000000010770006F0069006E007400
        74006F000000270000000000000000000000FFFFFFFFFFFFFFFF000000000000
        00000000000000010470006F006E0067000000330000000000000000000000FF
        FFFFFFFFFFFFFF00000000000000000000000000010A73006D006F006F007400
        68006E0061007600690000002F0000000000000000000000FFFFFFFFFFFFFFFF
        00000000000000000000000000010874007700650065006E0069006E006700}
    end
  end
  object PageControl: TPageControl
    Left = 226
    Top = 0
    Width = 912
    Height = 814
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tsMovements
    Align = alClient
    TabOrder = 1
    object tsMovements: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Movements'
      TabVisible = False
    end
  end
end
