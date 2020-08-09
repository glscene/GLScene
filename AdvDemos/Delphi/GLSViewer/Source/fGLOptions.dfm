inherited GLOptions: TGLOptions
  HelpContext = 0
  Caption = 'Options'
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelMiddle: TPanel
    object Label1: TLabel [0]
      Left = 296
      Top = 33
      Width = 73
      Height = 16
      Caption = 'Background'
    end
    inherited Memo: TMemo
      TabOrder = 3
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
    object RadioGroupLanguage: TRadioGroup
      Left = 96
      Top = 88
      Width = 329
      Height = 137
      Caption = 'Language'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'English'
        'Russian'
        'Spanish'
        'French'
        'German'
        'Italian')
      TabOrder = 1
      OnClick = RadioGroupLanguageClick
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
      TabOrder = 2
      OnClick = PanelBackgroundClick
    end
  end
  inherited PanelBottom: TPanel
    inherited ButtonOK: TButton
      OnClick = ButtonOKClick
    end
  end
end
