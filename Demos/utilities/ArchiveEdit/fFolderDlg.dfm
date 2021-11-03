object FDialog: TFDialog
  Left = 267
  Top = 134
  ActiveControl = Edit1
  BorderStyle = bsDialog
  Caption = 'Create Folder'
  ClientHeight = 134
  ClientWidth = 211
  Color = clBtnFace
  ParentFont = True
  Position = poMainFormCenter
  PixelsPerInch = 120
  TextHeight = 20
  object Bevel1: TBevel
    Left = 10
    Top = 10
    Width = 191
    Height = 71
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 50
    Top = 20
    Width = 153
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Enter folder name:'
  end
  object OKBtn: TButton
    Left = 9
    Top = 95
    Width = 94
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 109
    Top = 95
    Width = 94
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 30
    Top = 40
    Width = 151
    Height = 28
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 2
  end
end
