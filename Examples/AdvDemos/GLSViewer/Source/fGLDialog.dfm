inherited GLDialog: TGLDialog
  BorderIcons = [biSystemMenu]
  Caption = 'GLScene Dialog'
  ClientHeight = 362
  ClientWidth = 484
  StyleElements = [seFont, seClient, seBorder]
  ExplicitWidth = 500
  ExplicitHeight = 401
  TextHeight = 20
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 484
    Height = 35
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 476
  end
  object PanelMiddle: TPanel
    Left = 0
    Top = 35
    Width = 484
    Height = 286
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 476
    ExplicitHeight = 261
    object Memo: TMemo
      Left = 1
      Top = 1
      Width = 482
      Height = 284
      Align = alClient
      Lines.Strings = (
        ''
        ''
        '')
      TabOrder = 0
      ExplicitWidth = 474
      ExplicitHeight = 259
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 321
    Width = 484
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 296
    ExplicitWidth = 476
    object ButtonOK: TButton
      Left = 126
      Top = 6
      Width = 93
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object ButtonCancel: TButton
      Left = 238
      Top = 6
      Width = 93
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object ButtonHelp: TButton
      Left = 337
      Top = 6
      Width = 98
      Height = 25
      Caption = 'Help'
      TabOrder = 2
      OnClick = ButtonHelpClick
    end
  end
end
