object Form1: TForm1
  Left = 209
  Top = 108
  Caption = 'Key Map'
  ClientHeight = 160
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 10
    Top = 40
    Width = 331
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Alignment = taCenter
    AutoSize = False
    Caption = 'Type key to map to this panel...'
  end
  object Label2: TLabel
    Left = 10
    Top = 10
    Width = 331
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Alignment = taCenter
    AutoSize = False
    Caption = 'Click a panel to choose the key'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object PAUp: TPanel
    Left = 110
    Top = 80
    Width = 131
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'MBUTTON'
    TabOrder = 0
    OnClick = PAUpClick
  end
  object PALeft: TPanel
    Left = 10
    Top = 120
    Width = 121
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'LBUTTON'
    TabOrder = 1
    OnClick = PAUpClick
  end
  object PARight: TPanel
    Left = 220
    Top = 120
    Width = 121
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'RBUTTON'
    TabOrder = 2
    OnClick = PAUpClick
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 8
    Top = 56
  end
end
