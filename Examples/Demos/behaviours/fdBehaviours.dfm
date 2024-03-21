object FormBehaviours: TFormBehaviours
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Behaviours'
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
    object TreeView: TTreeView
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
        070400000009540054007200650065004E006F00640065002D00000000000000
        00000000FFFFFFFFFFFFFFFF0000000000000000000000000001074400430045
        00640065006D006F000000350000000000000000000000FFFFFFFFFFFFFFFF00
        000000000000000000000000010B4600500053006D006F00760065006D006500
        6E0074000000350000000000000000000000FFFFFFFFFFFFFFFF000000000000
        00000000000000010B500061007400680043006F006E00740072006F006C0000
        002B0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        0000010654006F007200710075006500}
    end
  end
  object PanelBehaviours: TPanel
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
    ExplicitLeft = 392
    ExplicitTop = 196
    ExplicitWidth = 324
    ExplicitHeight = 72
  end
end
