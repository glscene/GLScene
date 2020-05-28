object DXPPathsDialog: TDXPPathsDialog
  Left = 278
  Top = 141
  Width = 465
  Height = 333
  BorderWidth = 4
  Caption = 'Paths Dialog'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 449
    Height = 259
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      449
      259)
    object BBReplace: TBitBtn
      Left = 7
      Top = 229
      Width = 81
      Height = 24
      Anchors = [akLeft, akBottom]
      Caption = 'Replace'
      TabOrder = 0
      OnClick = BBReplaceClick
    end
    object BBAdd: TBitBtn
      Left = 95
      Top = 229
      Width = 81
      Height = 24
      Anchors = [akLeft, akBottom]
      Caption = 'Add'
      TabOrder = 1
      OnClick = BBAddClick
    end
    object BBRemove: TBitBtn
      Left = 183
      Top = 229
      Width = 81
      Height = 24
      Anchors = [akLeft, akBottom]
      Caption = 'Remove'
      TabOrder = 2
      OnClick = BBRemoveClick
    end
    object EDPath: TEdit
      Left = 7
      Top = 201
      Width = 386
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 3
      OnChange = EDPathChange
    end
    object BBPickPath: TBitBtn
      Left = 399
      Top = 201
      Width = 41
      Height = 21
      Anchors = [akRight, akBottom]
      Caption = '...'
      TabOrder = 4
      OnClick = BBPickPathClick
    end
    object BBDown: TBitBtn
      Left = 399
      Top = 80
      Width = 41
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Down'
      TabOrder = 5
      OnClick = BBDownClick
    end
    object BBUp: TBitBtn
      Left = 399
      Top = 32
      Width = 41
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Up'
      TabOrder = 6
      OnClick = BBUpClick
    end
    object LBPaths: TListBox
      Left = 8
      Top = 8
      Width = 385
      Height = 185
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 7
      OnClick = LBPathsClick
      OnKeyPress = LBPathsKeyPress
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 259
    Width = 449
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      449
      37)
    object BUOk: TButton
      Left = 285
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object BUCancel: TButton
      Left = 373
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ActionList: TActionList
    Left = 16
    Top = 16
  end
end
