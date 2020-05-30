object FDialog: TFDialog
  Left = 267
  Top = 134
  ActiveControl = Edit1
  BorderStyle = bsDialog
  Caption = 'Create Folder'
  ClientHeight = 107
  ClientWidth = 169
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 153
    Height = 57
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 40
    Top = 16
    Width = 90
    Height = 13
    Caption = 'Enter folder name:'
  end
  object OKBtn: TButton
    Left = 7
    Top = 76
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 87
    Top = 76
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 24
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 2
  end
end
