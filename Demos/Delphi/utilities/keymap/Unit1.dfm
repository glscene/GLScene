object Form1: TForm1
  Left = 209
  Top = 108
  Caption = 'Key Map'
  ClientHeight = 128
  ClientWidth = 286
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 32
    Width = 265
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Type key to map to this panel...'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 265
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Click a panel to choose the key'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object PAUp: TPanel
    Left = 88
    Top = 64
    Width = 105
    Height = 25
    Caption = 'MBUTTON'
    TabOrder = 0
    OnClick = PAUpClick
  end
  object PALeft: TPanel
    Left = 8
    Top = 96
    Width = 97
    Height = 25
    Caption = 'LBUTTON'
    TabOrder = 1
    OnClick = PAUpClick
  end
  object PARight: TPanel
    Left = 176
    Top = 96
    Width = 97
    Height = 25
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
