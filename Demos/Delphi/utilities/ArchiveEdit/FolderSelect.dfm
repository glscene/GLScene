object FolderSel: TFolderSel
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Folder selection'
  ClientHeight = 341
  ClientWidth = 317
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 297
    Height = 161
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 120
    Top = 16
    Width = 64
    Height = 13
    Caption = 'Select folder:'
  end
  object OKBtn: TButton
    Left = 79
    Top = 308
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 159
    Top = 308
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ShellView: TShellTreeView
    Left = 16
    Top = 32
    Width = 281
    Height = 265
    ObjectTypes = [otFolders]
    Root = 'rfDesktop'
    UseShellImages = True
    AutoRefresh = False
    Indent = 19
    ParentColor = False
    RightClickSelect = True
    ShowRoot = False
    TabOrder = 2
  end
end