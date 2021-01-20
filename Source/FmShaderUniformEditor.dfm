object GLShaderUniformEditor: TGLShaderUniformEditor
  Left = 432
  Top = 332
  BorderStyle = bsDialog
  Caption = 'Autofill of uniforms'
  ClientHeight = 344
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Labe1: TLabel
    Left = 8
    Top = 5
    Width = 88
    Height = 13
    Caption = 'Available Uniforms'
  end
  object Label1: TLabel
    Left = 223
    Top = 5
    Width = 78
    Height = 13
    Caption = 'Avaiblable Value'
  end
  object Label2: TLabel
    Left = 223
    Top = 97
    Width = 79
    Height = 13
    Caption = 'Texture Sampler'
  end
  object Label3: TLabel
    Left = 223
    Top = 143
    Width = 176
    Height = 13
    Caption = 'Swizzle of texture color components '
  end
  object Label4: TLabel
    Left = 223
    Top = 51
    Width = 131
    Height = 13
    Caption = 'Texture Image/Attachment'
  end
  object LBUniforms: TListBox
    Left = 8
    Top = 24
    Width = 209
    Height = 313
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBUniformsClick
    OnKeyPress = LBUniformsKeyPress
  end
  object AutoSetBox: TComboBox
    Left = 223
    Top = 24
    Width = 209
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = AutoSetBoxChange
  end
  object SamplerBox: TComboBox
    Left = 223
    Top = 116
    Width = 209
    Height = 21
    Style = csDropDownList
    Enabled = False
    TabOrder = 2
    OnChange = SamplerBoxChange
  end
  object Panel1: TPanel
    Left = 223
    Top = 162
    Width = 209
    Height = 143
    TabOrder = 3
    object RedGroup: TRadioGroup
      Left = 8
      Top = 8
      Width = 50
      Height = 129
      Caption = 'RED'
      Color = clRed
      Items.Strings = (
        'R'
        'G'
        'B'
        'A'
        '0'
        '1')
      ParentColor = False
      TabOrder = 0
      OnClick = ColorGroupClick
    end
    object GreenGroup: TRadioGroup
      Tag = 1
      Left = 55
      Top = 8
      Width = 50
      Height = 129
      Caption = 'GREEN'
      Color = clGreen
      Items.Strings = (
        'R'
        'G'
        'B'
        'A'
        '0'
        '1')
      ParentColor = False
      TabOrder = 1
      OnClick = ColorGroupClick
    end
    object BlueGroup: TRadioGroup
      Tag = 2
      Left = 102
      Top = 8
      Width = 50
      Height = 129
      Caption = 'BLUE'
      Color = clBlue
      Items.Strings = (
        'R'
        'G'
        'B'
        'A'
        '0'
        '1')
      ParentColor = False
      TabOrder = 2
      OnClick = ColorGroupClick
    end
    object AlphaGroup: TRadioGroup
      Tag = 3
      Left = 150
      Top = 8
      Width = 50
      Height = 129
      Caption = 'ALPHA'
      Color = clWhite
      Items.Strings = (
        'R'
        'G'
        'B'
        'A'
        '0'
        '1')
      ParentColor = False
      TabOrder = 3
      OnClick = ColorGroupClick
    end
  end
  object TextureBox: TComboBox
    Left = 223
    Top = 70
    Width = 209
    Height = 21
    Style = csDropDownList
    Enabled = False
    TabOrder = 4
    OnChange = TextureBoxChange
  end
  object Button1: TButton
    Left = 360
    Top = 312
    Width = 73
    Height = 25
    Caption = 'Done'
    ModalResult = 1
    TabOrder = 5
  end
end
