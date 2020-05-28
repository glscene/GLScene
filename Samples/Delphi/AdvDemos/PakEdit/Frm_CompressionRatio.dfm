object FrmCompressionRatio: TFrmCompressionRatio
  Left = 395
  Top = 211
  BorderStyle = bsToolWindow
  Caption = 'Select compression ratio'
  ClientHeight = 72
  ClientWidth = 181
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BtnForOk: TButton
    Left = 9
    Top = 40
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
    OnClick = BtnForOkClick
  end
  object BtnForCancel: TButton
    Left = 97
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = BtnForCancelClick
  end
  object CbForRatio: TComboBox
    Left = 10
    Top = 8
    Width = 161
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
end
