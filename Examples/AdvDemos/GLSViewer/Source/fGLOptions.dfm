inherited FormOptions: TFormOptions
  HelpContext = 0
  Caption = 'Options'
  ClientHeight = 313
  ClientWidth = 521
  Font.Height = -21
  StyleElements = [seFont, seClient, seBorder]
  OnClose = FormClose
  ExplicitLeft = 2
  ExplicitTop = 2
  ExplicitWidth = 537
  ExplicitHeight = 352
  TextHeight = 25
  inherited PanelTop: TPanel
    Width = 521
    StyleElements = [seFont, seClient, seBorder]
    ExplicitWidth = 513
  end
  inherited PanelMiddle: TPanel
    Width = 521
    Height = 237
    StyleElements = [seFont, seClient, seBorder]
    ExplicitWidth = 513
    ExplicitHeight = 212
    object Label1: TLabel [0]
      Left = 296
      Top = 33
      Width = 105
      Height = 25
      Caption = 'Background'
    end
    inherited Memo: TMemo
      Width = 519
      Height = 235
      TabOrder = 2
      StyleElements = [seFont, seClient, seBorder]
      ExplicitWidth = 511
      ExplicitHeight = 210
    end
    object CheckBoxAxis: TCheckBox
      Left = 96
      Top = 32
      Width = 113
      Height = 17
      Caption = 'Show Axes'
      TabOrder = 0
      OnClick = CheckBoxAxisClick
    end
    object PanelBackground: TPanel
      Left = 375
      Top = 27
      Width = 25
      Height = 25
      Hint = 'Click to change background colour'
      BevelInner = bvLowered
      BevelOuter = bvLowered
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
      OnClick = PanelBackgroundClick
    end
    object RadioGroupLanguage: TRadioGroup
      Left = 28
      Top = 84
      Width = 463
      Height = 127
      Caption = 'Language'
      Columns = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        'English'
        'Russian'
        'Spanish'
        'Italian')
      ParentFont = False
      TabOrder = 3
      OnClick = RadioGroupLanguageClick
    end
  end
  inherited PanelBottom: TPanel
    Top = 272
    Width = 521
    StyleElements = [seFont, seClient, seBorder]
    ExplicitTop = 247
    ExplicitWidth = 513
    inherited ButtonOK: TButton
      OnClick = ButtonOKClick
    end
    inherited ButtonCancel: TButton
      Left = 252
      Width = 99
      ExplicitLeft = 252
      ExplicitWidth = 99
    end
    inherited ButtonHelp: TButton
      Left = 393
      Top = 5
      ExplicitLeft = 393
      ExplicitTop = 5
    end
  end
end
