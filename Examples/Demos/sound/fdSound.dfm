object frmSandbox: TfrmSandbox
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Sound'
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
        00000000FFFFFFFFFFFFFFFF00000000000000000000000000010B53006F0075
        006E006400410072006F0075006E0064000000310000000000000000000000FF
        FFFFFFFFFFFFFF00000000000000000000000000010953006F0075006E006400
        42004100530053000000310000000000000000000000FFFFFFFFFFFFFFFF0000
        0000000000000000000000010953006F0075006E00640046004D004F00440000
        00350000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        0000010B53006F0075006E0064004F00700065006E0041004C00000037000000
        0000000000000000FFFFFFFFFFFFFFFF00000000000000000000000000010C53
        006F0075006E00640057006100760065004F0075007400}
    end
  end
  object PanelSound: TPanel
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
    ExplicitLeft = 434
    ExplicitTop = 252
    ExplicitWidth = 324
    ExplicitHeight = 72
  end
end
