inherited FormOptions: TFormOptions
  HelpContext = 0
  Caption = 'Options'
  ClientHeight = 313
  ClientWidth = 460
  Font.Height = -21
  StyleElements = [seFont, seClient, seBorder]
  OnClick = rgLanguageClick
  OnClose = FormClose
  ExplicitLeft = 4
  ExplicitTop = 4
  ExplicitWidth = 476
  ExplicitHeight = 352
  TextHeight = 25
  object rgLanguage: TRadioGroup
    Left = 56
    Top = 98
    Width = 351
    Height = 155
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Language'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'English'
      'Russian')
    TabOrder = 0
    OnClick = rgLanguageClick
  end
  object CheckBoxAxis: TCheckBox
    Left = 56
    Top = 28
    Width = 183
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Axis'
    TabOrder = 1
    OnClick = CheckBoxAxisClick
  end
  object PanelBackground: TPanel
    Left = 294
    Top = 22
    Width = 57
    Height = 40
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = clBlack
    ParentBackground = False
    TabOrder = 2
    OnClick = PanelBackgroundClick
  end
  object ButtonOk: TButton
    Left = 318
    Top = 278
    Width = 75
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Ok'
    TabOrder = 3
    OnClick = ButtonOKClick
  end
end
