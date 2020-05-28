object DXPDirectoryDialog: TDXPDirectoryDialog
  Left = 209
  Top = 83
  BorderStyle = bsDialog
  Caption = 'Select directory'
  ClientHeight = 339
  ClientWidth = 372
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    372
    339)
  PixelsPerInch = 96
  TextHeight = 13
  object ShellTreeView: TShellTreeView
    Left = 4
    Top = 4
    Width = 363
    Height = 296
    ObjectTypes = [otFolders]
    Root = 'rfDesktop'
    UseShellImages = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoRefresh = False
    HideSelection = False
    Indent = 19
    ParentColor = False
    RightClickSelect = True
    ShowRoot = False
    TabOrder = 0
  end
  object BUOk: TButton
    Left = 195
    Top = 307
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object BUCancel: TButton
    Left = 291
    Top = 307
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
