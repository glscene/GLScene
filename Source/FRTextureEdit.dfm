object RTextureEdit: TRTextureEdit
  Left = 0
  Top = 0
  Width = 230
  Height = 187
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  DesignSize = (
    230
    187)
  object Label2: TLabel
    Left = 0
    Top = 1
    Width = 30
    Height = 13
    Caption = 'Image'
  end
  object SBEditImage: TSpeedButton
    Left = 212
    Top = 0
    Width = 17
    Height = 21
    Hint = 'Edit image'
    Anchors = [akTop, akRight]
    Caption = '...'
    OnClick = SBEditImageClick
  end
  object Label3: TLabel
    Left = 0
    Top = 52
    Width = 44
    Height = 13
    Caption = 'MagFilter'
  end
  object Label4: TLabel
    Left = 0
    Top = 76
    Width = 40
    Height = 13
    Caption = 'MinFilter'
  end
  object Label1: TLabel
    Left = 0
    Top = 124
    Width = 67
    Height = 13
    Caption = 'Texture Mode'
  end
  object Label5: TLabel
    Left = 0
    Top = 148
    Width = 67
    Height = 13
    Caption = 'Texture Wrap'
  end
  object Label6: TLabel
    Left = 0
    Top = 28
    Width = 57
    Height = 13
    Caption = 'ImageAlpha'
  end
  object Label7: TLabel
    Left = 0
    Top = 100
    Width = 75
    Height = 13
    Caption = 'Filtering Quality'
  end
  object CBMagFilter: TComboBox
    Left = 75
    Top = 48
    Width = 154
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = CBMagFilterChange
  end
  object CBMinFilter: TComboBox
    Left = 75
    Top = 72
    Width = 154
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = CBMinFilterChange
  end
  object CBTextureMode: TComboBox
    Left = 75
    Top = 120
    Width = 154
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = CBTextureModeChange
  end
  object CBTextureWrap: TComboBox
    Left = 75
    Top = 144
    Width = 154
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = CBTextureWrapChange
  end
  object CBDisabled: TCheckBox
    Left = 0
    Top = 168
    Width = 73
    Height = 17
    Caption = 'Disabled'
    TabOrder = 4
    OnClick = CBDisabledClick
  end
  object CBImageClass: TComboBox
    Left = 75
    Top = 0
    Width = 138
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnChange = CBImageClassChange
  end
  object CBImageAlpha: TComboBox
    Left = 75
    Top = 24
    Width = 154
    Height = 21
    Style = csDropDownList
    TabOrder = 6
    OnChange = CBImageAlphaChange
  end
  object CBFilteringQuality: TComboBox
    Left = 75
    Top = 96
    Width = 154
    Height = 21
    Style = csDropDownList
    TabOrder = 7
    OnChange = CBFilteringQualityChange
  end
end
